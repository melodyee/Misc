(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
(*homographyClustersFromMatches=Function[matches,Module[{tolerance=2,iter},
	iter=Function[inlierOutlier,Module[{goodMatches,outliers=inlierOutlier[[2]]},
		goodMatches=filterMatchesByHomography[Transpose@outliers,tolerance];{Transpose@goodMatches,Complement[outliers,Transpose@goodMatches]}]];
	Transpose/@NestWhileList[iter,{Transpose@allMatches,Transpose@allMatches},((*Print[Dimensions/@#];*)Length[#[[1]]]>5)&][[2;;-2,1]]]];*)
filterMatchesByHomographyCluster=Function[matches,Transpose[Join@@(Transpose/@takeAtMostN[#,4])]&@homographyClustersFromMatches@matches];

parseTimeStamp=Function[fname,ToExpression@StringSplit[StringReplace[fname,".JPG"->""],"_"][[-1]]];
decomposeHomography=Function[{homog,coordIkm},decomposeH[coordIkm.homog.Inverse[coordIkm]]];
reprojectionErrorHomographyLp=Function[{homog,matches,p},1/2(pnorm2[(#[[;;2]]/(10^-10 Sign[#[[-1]]]+#[[-1]])&[homog.Append[#,1]]&/@matches[[1]])-matches[[2]],p]
	+pnorm2[(#[[;;2]]/(10^-10 Sign[#[[-1]]]+#[[-1]])&[Inverse@homog.Append[#,1]]&/@matches[[2]])-matches[[1]],p])];
poseErrorFmatrixLp=Function[{fsIn,nmatches,p},Module[{fs=#/Mean[SingularValueList[#][[;;2]]]&@fsIn},pnorm[MapThread[#2.fs.#&,nmatches],p]]];

homographyFromMatchesMulti=Function[{matches,ikm,maxNumTry},homographyFromMatchesMultiLp[matches,ikm,maxNumTry,2]];
homographyFromMatchesMultiLp=Function[{matches,ikm,maxNumTry,p},Module[{nmatches=Map[ikm.Append[#,1]&,matches,{2}],as,ts,ns,a,t,n,f,r,rtn,costF},
	as=Array[a,3{1,1}];ts=Array[t,3];ns=Array[n,3];
	f[as_,ts_,ns_?(NumericQ@#[[1]]&)]:=With[{h=matrixExpSpecialOrthogonal[as]+Outer[Times,ts,ns]},
		pnorm[MapThread[Normalize[h.#]-Normalize[#2]&,nmatches],p]];
	r=Table[FindMinimum[f[as,ts,ns],Join[variableWithInitial[as,RandomReal[1,Dimensions@as]],
			variableWithInitial[ts,RandomReal[1,Dimensions@ts]],variableWithInitial[ns,RandomReal[1,3]]]]
	,{maxNumTry}];(*Print[SortBy[r,#[[1]]&]];*)
	rtn={matrixExpSpecialOrthogonal@as,ts,ns}/.First[SortBy[r,#[[1]]&]][[2]];
	Inverse[ikm].(rtn[[1]]+Outer[Times,rtn[[2]],rtn[[3]]]).ikm
	]];

(*Plane = {world ns,points}*)
homogOfPlane=Function[{plane,t0,ts,qPair,coord,ikm},Module[{localizePlane=Function[{ns,t},ns/(1+ns.t)]},qtnToH[qPair,ts,localizePlane[plane[[1]],t0],coord,ikm]]];
qtnToH=Function[{qs,ts,ns,coord,ikm},
	Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qs[[2]]]].(IdentityMatrix[3]+Outer[Times,ts,ns]).quaternionToRotationMatrix[qs[[1]]].coord.ikm];
homographyInlierOutliers=Function[{matches,homog,pixelDelta},
		Module[{test=Norm[#[[2]]-(#[[;;2]]/(Sign[#[[-1]]]10^-20+#[[-1]])&[homog.Append[#[[1]],1]])]<pixelDelta&},
	{Select[Transpose@matches,test],Select[Transpose@matches,Not@test@#&]}]];
peelHomographyFromMatches=Function[{qPair,allMatches,leastNum,desiredNum,coord,ikm,pixelDelta,maxIter},
		Module[{matches,homog,bestPartition={{},Transpose@allMatches},bestHomog=IdentityMatrix[3],partition,ts,plane},
	Do[matches=Transpose@RandomSample[Transpose@allMatches,Round@Max[leastNum,Length@Transpose@allMatches /3]];
		(*{ts,plane}=initTranslationAndOnePlaneFromMatches[qPair,matches,coord,ikm];
		homog=homogOfPlane[plane,{0,0,0},ts,qPair,coord,ikm];*)
		homog=homographyFromMatchesCalibrated[matches,ikm];
		partition=homographyInlierOutliers[allMatches,homog,pixelDelta];
		If[Length@partition[[1]]>Length@bestPartition[[1]],bestPartition=partition;bestHomog=homog];
		If[Length@partition[[1]]>=desiredNum,Break[]]
	,{maxIter}];Append[bestPartition,bestHomog]]];
optimizeInlierHPairs=Function[{inlierHPairs,ikm},Module[{homogs,matchGroups,labels,matches,inlierHPairs2},homogs=inlierHPairs[[;;,2]];
	matchGroups=Join@@Table[Transpose@List@#&/@inlierHPairs[[i,1]],{i,Length@inlierHPairs}];
	labels=Table[Ordering[reprojectionErrorHomography[#,matches]&/@homogs,1][[1]],{matches,matchGroups}];
	inlierHPairs2=Select[({#[[;;,1]],homogs[[#[[1,2]]]]}&/@SplitBy[SortBy[Thread@{First@Transpose@#&/@matchGroups,labels},Last],Last]),Length@#[[1]]>=3&];
	{#[[1]],homographyFromMatchesCalibrated[transposeLiers@#[[1]],ikm]}&/@inlierHPairs2]];
evalInlierHPairs=Function[inlierHPairs,Outer[reprojectionErrorHomography,inlierHPairs[[;;,2]],Transpose@List@#&/@Join@@inlierHPairs[[;;,1]],1]];
findInlierHomographyPairs=Function[{qPair,allMatches,pixelDelta,leastNumPoints,maxNumPlanes,maxIter,coord,ikm}
		,Module[{desiredNumPoints=3 leastNumPoints},
	With[{work=Function[partition,peelHomographyFromMatches[qPair,
		transposeLiers@partition[[2]],leastNumPoints,desiredNumPoints,coord,ikm,pixelDelta,maxIter]]},
	Select[optimizeInlierHPairs[#,ikm]&@optimizeInlierHPairs[#,ikm]&@NestWhileList[work
		,{Transpose@allMatches,Transpose@allMatches},Length@#[[1]]>=leastNumPoints&&Length@#[[2]]>=leastNumPoints&,1,maxNumPlanes][[2;;,{1,3}]]
	,Length@#[[1]]>=leastNumPoints&]]]];
initTranslationAndOnePlane=Function[{qPair,imgPair,coord,ikm},
	initTranslationAndOnePlaneFromMatches[
		qPair,ImageCorrespondingPoints[imgPair[[1]],imgPair[[2]],"Transformation"->"Perspective"],coord,ikm]];
initTranslationAndOnePlaneFromMatches=Function[{qPair,matches,coord,ikm},Module[{t,n,ts,ns,r3},
	Clear[t,n];ts=Array[t,3];ns=Array[n,3];
	r3=FindMinimum[reprojectionErrorHomography[homogOfPlane[{ns,{}},{0,0,0},ts,qPair,coord,ikm],matches]
		,Join[variableWithInitial[ts,0 ts],variableWithInitial[ns,1&/@ns]](*,MaxIterations->500*)];
	{ts/.r3[[2]],{ns/.r3[[2]],matches[[2]]}}]];
initTranslationAndPlanes=Function[{qPair,imgPair,coord,ikm},Module[{t},
	initTranslationAndPlanesFromMatchesWithT0Ts[qPair,ImageCorrespondingPoints@@imgPair,{0,0,0},Array[t,3],coord,ikm]]];
initTranslationAndPlanesFromMatchesWithT0Ts=Function[{qPair,matches,t0,ts,coord,ikm},Module[{n,nss,r3,inlierHPairs,i,pixelDelta=1,leastNumPoints=5,homogs},
	Print["findInlierHomographyPairs",inlierHPairs=findInlierHomographyPairs[
		qPair,matches,pixelDelta,leastNumPoints,3,50,coord,ikm];//AbsoluteTiming];
	Clear[n];nss=Array[n,{Length@inlierHPairs,3}];
	Print["inlierHPairs",Dimensions@inlierHPairs];
	homogs=Simplify@Table[homogOfPlane[{nss[[i]],{}},t0,ts,qPair,coord,ikm],{i,Length@inlierHPairs}];
	Print["initTranslationAndPlanesFromMatchesWithT0Ts",
	r3=FindMinimum[Total@Table[reprojectionErrorHomography[homogs[[i]],Transpose@inlierHPairs[[i,1]]],{i,Length@inlierHPairs}](*TODO, deal with inlierHPairs=={}*)
		,Join[If[NumberQ[ts[[1]]],{},variableWithInitial[ts,0 ts]],variableWithInitial[nss,Map[1&,nss,{2}]]]];//AbsoluteTiming];
	{ts/.r3[[2]],Table[With[{ns=nss[[i]]},{ns/.r3[[2]],Transpose[inlierHPairs[[i,1]]][[2]]}],{i,Length@inlierHPairs}]}]];
translationFromPlanes=Function[{t0,qPair,imgPair,planes,coord,ikm},Module[{t,ts,r3,keypointGroups,i,homogs},Clear[t];ts=Array[t,3];
	keypointGroups=Select[Transpose[#],Head[#[[2]]]===List&]&/@imageTrackKeypointGroups[imgPair,planes[[;;,2]]];
	homogs=Simplify@Table[homogOfPlane[planes[[i]],t0,ts,qPair,coord,ikm],{i,Length@planes}];
	Print["translationFromPlanes",
	r3=FindMinimum[Total@Table[
		If[keypointGroups[[i]]=={},0,reprojectionErrorHomography[homogs[[i]],Transpose@keypointGroups[[i]]]]
		,{i,Length@planes}],variableWithInitial[ts,0 ts]];//AbsoluteTiming];
	{ts/.r3[[2]],Table[{planes[[i,1]],If[#=={},{},Transpose[#][[2]]]&@keypointGroups[[i]]},{i,Length@planes}]}]];
updateTranslationAndPlanes=Function[{t0,planes,pixelDelta,leastNumPoints,qPair,imgPair,coord,ikm},
		Module[{homog2,allMatches2,plane2,residuePlanes,t23,expandedPlanes,newPlanes,outliers},
	{t23,residuePlanes}=translationFromPlanes[t0,qPair,imgPair,planes,coord,ikm];
	allMatches2=If[#=={},{{},{}},Transpose@#]&@Select[Transpose[ImageCorrespondingPoints@@imgPair],Not@MemberQ[Join@@residuePlanes[[;;,2]],#[[2]]]&];
	Print["allMatches2",Dimensions@allMatches2];
	{expandedPlanes,outliers}=expandPlanes[allMatches2,residuePlanes,pixelDelta,t0,t23,qPair,coord,ikm];
	(*Print["outliers",Dimensions@outliers];*)
	newPlanes=If[Length@outliers>=leastNumPoints,initTranslationAndPlanesFromMatchesWithT0Ts[qPair,Transpose@outliers,t0,t23,coord,ikm][[2]],{}];
	(*Module[{t,r},r=initTranslationAndPlanesFromMatchesWithT0Ts[qPair,Transpose@outliers,{0,0,0},Array[t,3],coord,ikm];Print["reboot",{r[[1]],Dimensions/@r[[2]]}]];*)
	(*Print[{"before filter","expanded",Dimensions/@expandedPlanes,"new",Dimensions/@newPlanes}];*)
	expandedPlanes=Select[expandedPlanes,Length@#[[2]]>=leastNumPoints&];
	newPlanes=Select[newPlanes,Length@#[[2]]>=leastNumPoints&];
	Print[{"expanded",Dimensions/@expandedPlanes,"new",Dimensions/@newPlanes}];
	{t0+t23,Join[expandedPlanes,newPlanes]}]];
expandPlanes=Function[{allMatches,planes,pixelDelta,t0,ts,qPair,coord,ikm},Module[{work,r,expanded={}},
	work=Function[{matches,plane},Module[{homog,partition},
		homog=homogOfPlane[plane,t0,ts,qPair,coord,ikm];partition=homographyInlierOutliers[matches,homog,pixelDelta];
		AppendTo[expanded,{plane[[1]],Join[plane[[2]],transposeLiers[partition[[1]]][[2]]]}];
		transposeLiers@partition[[2]]]];
	r=Fold[work,allMatches,planes];{expanded,Transpose@r}]];

drawPlane=Function[normal,Translate[Rotate[Polygon[{{-1, -1, 0}, {1, -1, 0}, {1, 1, 0}, {-1, 1, 0}}],{{0, 0, 1}, normal}],Normalize[normal]/Norm[normal]]];
imageTrackKeypoints=Function[{imgPair,keypoints},Module[{nf,matches2=ImageCorrespondingPoints@@imgPair},(*keypoints need be result of ImageKeypoints*)
	nf=Nearest[(#1[[1]]->#1&)/@Transpose[matches2]];Transpose[(With[{nbs=nf[#1]},If[Norm[#1-nbs[[1,1]]]<5,nbs[[1]],{#1,Missing[]}]]&)/@keypoints]]];
imageTrackKeypointGroups=Function[{imgPair,keypointGroups},Module[{nf,matches2=ImageCorrespondingPoints@@imgPair},(*keypoints need be result of ImageKeypoints*)
	nf=Nearest[#[[1]]->#&/@Transpose@matches2];Transpose/@Map[With[{nbs=nf[#]},If[Norm[#-nbs[[1,1]]]<3,nbs[[1]],{#,Missing[]}]]&,keypointGroups,{2}]]];

(*KroneckerProduct[IdentityMatrix[3],List/@ts].ns==KroneckerProduct[List/@ns,IdentityMatrix[3]].ts==vec[Outer[Times,ts,ns]]*)
rankOneVecLeastSquare=Function[{A,B,dim,maxIter},(*Finds a rank-1 matrix X of Dimension dim so that minimizes ||A vec(X)-B||_F^2*)
		Module[{tsns0,tsns,nestF,oldCost,curCost,costF,j},
	costF=Function[tsns,pnorm2[A.vec@Outer[Times,tsns[[1]],tsns[[2]]]-B,2]];
	nestF=Function[tsns,Module[{ns,ts},ns=LeastSquares[A.KroneckerProduct[IdentityMatrix[dim[[2]]],List/@tsns[[1]]],B];
		ts=LeastSquares[A.KroneckerProduct[List/@ns,IdentityMatrix[dim[[1]]]],B];{ts,ns}]];
	tsns0={#[[1,;;,1]]#[[2,1,1]],#[[3,;;,1]]}&@SingularValueDecomposition@Transpose@Partition[LeastSquares[A,B],dim[[1]]];
	oldCost=costF@tsns0;
	Do[tsns=nestF@tsns0;curCost=costF@tsns;(*Print[oldCost];*)If[Abs[curCost-oldCost]<0.0001,(*Print[j];*)Break[]];oldCost=curCost;,{j,maxIter}];
	tsns
	]];
(*SeedRandom[1003];as=RandomReal[1,{10,12}];bs=RandomReal[1,10];
x=rankOneVecLeastSquare[as,bs,{4,3},10]
pnorm2[as.vec@Outer[Times,x[[1]],x[[2]]]-bs,2]
pnorm2[as.LeastSquares[as,bs]-bs,2]*)
rankOneLeastSquare=Function[{A,B,maxIter},(*Finds a rank-1 matrix so that minimizes ||A vec(X)-B||_F^2*)
		Module[{tsns0,tsns,nestF,oldCost,curCost,costF,j},
	costF=Function[tsns,pnorm2[A.Outer[Times,tsns[[1]],tsns[[2]],1]-B,2]];
	nestF=Function[tsns,Module[{ns,ts},ns=Join@@LeastSquares[List/@(A.tsns[[1]]),B];ts=LeastSquares[KroneckerProduct[List/@ns,A],vec@B];{ts,ns}]];
	tsns0={#[[1,;;,1]]#[[2,1,1]],#[[3,;;,1]]}&@SingularValueDecomposition@LeastSquares[A,B];
	oldCost=costF@tsns0;
	Do[tsns=nestF@tsns0;curCost=costF@tsns;(*Print[oldCost];*)If[Abs[curCost-oldCost]<0.0001,(*Print[j];*)Break[]];oldCost=curCost;,{j,maxIter}];
	tsns
	]];
(*SeedRandom[1003];{as,bs}=RandomReal[1,{2,10,3}];
x=rankOneLeastSquare[as,bs,10]
pnorm2[as.Outer[Times,x[[1]],x[[2]]]-bs,2]
pnorm2[as.LeastSquares[as,bs]-bs,2]*)

findTranslationGivenAttitudeAndMatches=Function[{imgPair,qPair,matches},Module[{t,n,ts,ns,r3,ikm,coord,rot,homog2,homog3},
	If[Not@NumberQ[matches[[1,1,1]]],(*aimgs=Append[aimgs,Null];*){0,0,0},
	ts=Array[t,3];ns=Array[n,3];ikm=Inverse@intrinsicParameterMatrix@imgPair[[1]];coord={{0,1,0},{-1,0,0},{0,0,-1}};
	rot=LinearSolve@@(quaternionToRotationMatrix/@Reverse[qPair]);
	r3=FindMinimum[0.01Total[ts^2]+1/Length[matches[[1]]] Total@Flatten@Sqrt[
		reprojectionErrorHomography[Inverse[coord.ikm].(rot+Outer[Times,ts,ns]).coord.ikm,matches]],Flatten@Join[ns,ts]];
	Print[Normalize[quaternionToRotationMatrix[qPair[[1]]].(ns/.r3[[2]])]];
	homog2=Inverse[coord.ikm].(rot).coord.ikm;
	homog3=Inverse[coord.ikm].(rot+Outer[Times,ts,ns]/.r3[[2]]).coord.ikm;
	(*aimgs=Append[aimgs,{imgPair[[1]],imgPair[[2]],anaglyph@imgPair
		,anaglyph@{ImagePerspectiveTransformation[imgPair[[1]],homog2,DataRange->Full],imgPair[[2]]}
		,anaglyph@{ImagePerspectiveTransformation[imgPair[[1]],homog3,DataRange->Full],imgPair[[2]]}}];*)
	rotateByQuaternion[-Sign[(ns/.r3[[2]]).{0,0,-1}] ts/.r3[[2]],qPair[[2]]]]
	]];
findTranslationGivenAttitude=Function[{imgPair,qPair},
	findTranslationGivenAttitudeAndMatches[imgPair,qPair,ImageCorrespondingPoints[imgPair[[1]],imgPair[[2]](*,"Transformation"->"Perspective"*)]]];
crossErrorHomography=Function[{homog,matches},MapThread[skewOmega[Append[#2,1]].homog.Append[#,1]&,matches]];
homographyFromMatchesReprojection=Function[{matches,qPair,coord,ikm,rank},Module[{lm,rm,h,hs,hs2,r},
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	hs=Switch[rank,3,Array[h,3{1,1}],_,Array[h,{2,3,rank}]];hs2=Switch[rank,3,hs,_,hs[[1]].Transpose[hs[[2]]]];
	r=FindMinimum[reprojectionErrorHomography[lm.(IdentityMatrix[3]+hs2).rm,matches],variableWithInitial[hs,0 hs]];
	#/#[[-1,-1]]&[lm.(IdentityMatrix[3]+hs2/.r[[2]]).rm]]];
tsnsFromMatches=Function[{matches,qPair,coord,ikm},(*assume matches are Perspective*)Module[{A},
	A=Join@@MapThread[KroneckerProduct[{quaternionToRotationMatrix[qPair[[1]]].coord.ikm.Append[#,1]},
		skewOmega[quaternionToRotationMatrix[qPair[[2]]].coord.ikm.Append[#2,1]]]&,matches];
	{#[[1]]Norm[#[[2]]],#[[2]]/Norm@#[[2]]}&@rankOneVecLeastSquare[A,-A.vec@IdentityMatrix[3],3{1,1},10]
	]];
homographyFromMatchesTsNsCross=Function[{matches,qPair,coord,ikm},Module[{tsns=tsnsFromMatches[matches,qPair,coord,ikm]},
	Inverse[quaternionToRotationMatrix[qPair[[2]]].coord.ikm].(IdentityMatrix[3]+Outer[Times,tsns[[1]],tsns[[2]]])
		.quaternionToRotationMatrix[qPair[[1]]].coord.ikm]];
homographyFromMatchesTsNsReprojection=Function[{matches,qPair,coord,ikm},Module[{r=initTranslationAndOnePlaneFromMatches[qPair,matches,coord,ikm]},
	homogOfPlane[r[[2]],{0,0,0},r[[1]],qPair,coord,ikm]]];
(*tsns=tsnsFromMatches[matches,{q1,q2},coord,ikm]
IdentityMatrix[3]+Outer[Times,tsns[[1]],tsns[[2]]]
Outer[Times,tsns[[1]],tsns[[2]]]

homogTS=lm.(IdentityMatrix[3]+Outer[Times,tsns[[1]],tsns[[2]]]).rm;
MatrixForm/@{homogTS0,homogTS,homographyFromMatchesCalibrated[matches,ikm]}
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@{homogTS,homogTSN,homogM},2]*)

(*rtnFromHomographyMatches=Function[{matches,ikm},Module[{a,t,n,f,as,ts,ns,r,nmatches},
	as=Array[a,3{1,1}];ts=Array[t,3];ns=Array[n,3];nmatches=Map[ikm.Append[#,1]&,matches,{2}];
	f[as_?(NumericQ@#[[1,1]]&),ts_,ns_]:=Module[{rs=matrixExpSpecialOrthogonal@as},
		(*cost=*)(reprojectionErrorHomographyLp[rs+Outer[Times,ts,ns],matches,2]+poseErrorFmatrixLp[skewOmega[ts].rs,nmatches,2])/Length@matches[[1]]];
	r=FindMinimum[f[as,ts,ns],
		Join[variableWithInitial[as,RandomReal[1,3{1,1}]],variableWithInitial[ts,RandomReal[1,3]],variableWithInitial[ns,RandomReal[1,3]]]];
	Print[{"residual",r[[1]]}];{matrixExpSpecialOrthogonal@as,Norm[ns]ts,Normalize[ns]}/.r[[2]]]];
rtnFromHomographyMatches2=Function[{matches,ikm},Module[{a,t,n,f,as,ts,ns,r,nmatches,hs,homog},
	as=Array[a,3{1,1}];ts=Array[t,3];ns=Array[n,3];nmatches=Map[ikm.Append[#,1]&,matches,{2}];
	homog=homographyFromMatchesL1@matches;hs=#/SingularValueList[#][[2]]&[ikm.homog.Inverse[ikm]];
	f[as_?(NumericQ@#[[1,1]]&),ts_,ns_]:=Module[{rs=matrixExpSpecialOrthogonal@as},
		Plus@@{pnorm[(rs+Outer[Times,ts,ns])-hs,2],poseErrorFmatrixLp[skewOmega[ts].rs,nmatches,2]/Length[matches[[1]]]}];
	r=FindMinimum[f[as,ts,ns],
		Join[variableWithInitial[as,RandomReal[1,3{1,1}]],variableWithInitial[ts,RandomReal[1,3]],variableWithInitial[ns,RandomReal[1,3]]]];
	Print[{"residual",r[[1]]}];{matrixExpSpecialOrthogonal@as,Norm[ns]ts,Normalize[ns]}/.r[[2]]]];
rtnFromHomographyMatches3=Function[{matches,ikm},Module[{a,t,n,f,as,ts,ns,r,nmatches,hs,homog,fs,es},
	as=Array[a,3{1,1}];ts=Array[t,3];ns=Array[n,3];nmatches=Map[ikm.Append[#,1]&,matches,{2}];
	homog=homographyFromMatchesL1@matches;hs=#/SingularValueList[#][[2]]&[ikm.homog.Inverse[ikm]];
	fs=fundamentalMatrixFromMatchesLp[matches,1];es=#/Mean[SingularValueList[#][[;;2]]]&[Transpose[Inverse[ikm]].fs.Inverse[ikm]];
	f[as_?(NumericQ@#[[1,1]]&),ts_,ns_]:=Module[{rs=matrixExpSpecialOrthogonal@as},
		Plus@@{pnorm[(rs+Outer[Times,ts,ns])-hs,2],0.1pnorm[skewOmega[Normalize[ts]].rs-es,2]};];
	r=FindMinimum[f[as,ts,ns],Flatten@{as,ts,ns}];{matrixExpSpecialOrthogonal@as,Norm[ns]ts,Normalize[ns]}/.r[[2]]]];
rtnFromHomographyMatches4=Function[{matches,ikm},Module[{a,n,f,as,ts,ns,r,nmatches,hs,homog,fs,es},
	as=Array[a,3{1,1}];ns=Array[n,3];nmatches=Map[ikm.Append[#,1]&,matches,{2}];
	homog=homographyFromMatchesL1@matches;hs=#/SingularValueList[#][[2]]&[ikm.homog.Inverse[ikm]];
	fs=fundamentalMatrixFromMatchesLp[matches,1];es=#/Mean[SingularValueList[#][[;;2]]]&[Transpose[Inverse[ikm]].fs.Inverse[ikm]];
	ts=First@NullSpace@Transpose@es;
	f[as_?(NumericQ@#[[1,1]]&),ns_]:=Module[{rs=matrixExpSpecialOrthogonal@as},
		Plus@@{pnorm[(rs+Outer[Times,ts,ns])-hs,2],pnorm[skewOmega[Normalize[ts]].rs-es,2]}];
	r=NMinimize[f[as,ns],Flatten@{as,ns}];{matrixExpSpecialOrthogonal@as,Norm[ns]ts,Normalize[ns]}/.r[[2]]]];*)


hsFromHomog=Function[{homog,qPair,coord,ikm},Module[{lm,rm},
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	Inverse[lm].homog.Inverse[rm]-IdentityMatrix[3]]];
homogFromHs=Function[{hs,qPair,coord,ikm},Module[{lm,rm},
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	lm.(IdentityMatrix[3]+hs).rm]];
hsFromImagesCorrelation=Function[{imgPair,qPair,coord,ikm,rank},
	hsFromImagesCorrelationWithInit[imgPair,qPair,coord,ikm,rank,Switch[rank,3,Array[0&,3{1,1}],_,Array[0&,{2,3,rank}]]]];
hsFromImagesCorrelationWithInit=Function[{imgPair,qPair,coord,ikm,rank,init},Module[{lm,rm,h,hs,hs2,r,f,matches},
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	hs=Switch[rank,3,Array[h,3{1,1}],_,Array[h,{2,3,rank}]];hs2=Switch[rank,3,hs,_,hs[[1]].Transpose[hs[[2]]]];
	(*matches=ImageCorrespondingPoints@@imgPair;*)
	f[hs2_?(NumericQ@First@Flatten@#&)]:=correlationErrorHomography[lm.(IdentityMatrix[3]+hs2).rm,imgPair]
			(*+If[Transpose@matches=={},0,reprojectionErrorHomography[lm.(IdentityMatrix[3]+hs2).rm,matches]/Length@Transpose@matches]*);
	r=FindMinimum[f[hs2],variableWithInitial[hs,If[rank==3,init
		,{#[[1,;;,;;rank]].#[[2,;;rank,;;rank]],#[[3,;;,;;rank]]}&@SingularValueDecomposition[init]]]];hs/.r[[2]]]];
homographyFromImagesCorrelation=Function[{imgPair,qPair,coord,ikm,rank},
	homographyFromImagesCorrelationWithInit[imgPair,qPair,coord,ikm,rank,homogFromHs[Array[0&,3{1,1}],qPair,coord,ikm]]];
homographyFromImagesCorrelationWithInit=Function[{imgPairIn,qPair,coord,ikm,rank,init},Module[{lm,rm,h,hs,hs2,f,imgPair},
	imgPair=GaussianFilter[#,Mean@ImageDimensions@imgPairIn[[1]]/(*10*)20]&/@imgPairIn;
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	hs=hsFromImagesCorrelationWithInit[imgPair,qPair,coord,ikm,rank,hsFromHomog[init,qPair,coord,ikm]];
	hs2=Switch[rank,3,hs,_,hs[[1]].Transpose[hs[[2]]]];
	#/#[[-1,-1]]&[lm.(IdentityMatrix[3]+hs2).rm]]];

buildHomography=Function[imgs,Module[{preMatches,matches,rect,homog},
	preMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];rect={{420,20},{600,80}}/2;
	matches=Transpose@Select[Transpose@preMatches,Not@pointInRectangle[#[[1]],rect]&&Not@pointInRectangle[#[[2]],rect]&];
	homog=homographyFromMatchesRansac[matches,imgs,Inverse@intrinsicParameterMatrix@#&/@imgs];
	{homog,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}
	,{Show[imgs[[1]],Graphics@{FaceForm[None],EdgeForm[Yellow],Rectangle@@rect}],MapThread[annotateImageWithPoints,{imgs,matches}]}}]];
buildHomographySimple=Function[imgs,Module[{matches,tr,homog,ikms},
	tr=FindGeometricTransform[imgs[[2]],imgs[[1]],"Transformation"->"Perspective"];homog=TransformationMatrix[tr[[2]]];
	If[NumericQ@tr[[1]],{homog,tr[[1]],ImageResize[#,{320}]&@anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}}]]];
buildHomographySimple2=Function[imgs,Module[{matches,homog(*,simgs=ImageResize[#,ImageDimensions[#]/4]&/@imgs*),ikms},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
	homog=homographyFromMatchesCalibrated[matches,ikms];
	{homog,ImageResize[#,{320}]&@anaglyph@{ImagePerspectiveTransformation[
		imgs[[1]],(*DiagonalMatrix[{1/4,1/4,1}].*)homog(*.DiagonalMatrix[{4,4,1}]*),DataRange->Full],imgs[[2]]}
	,{Null,ImageResize[#,{320}]&/@MapThread[annotateImageWithPoints,{imgs,matches}]}}]];
decomposeSameNormalCalibratedHomographyList=Function[homogsIn,Module[{homogs,normal},(*These homographies should be the same plane*)
	homogs=#/SingularValueList[#][[2]]&/@homogsIn;
	(*normal=SingularValueDecomposition[SingularValueDecomposition[#][[3,;;,2]]&/@homogs][[3,;;,-1]];*)
	normal={0,0,1};
	(*Print[dispRtn@standardizeRtn@#&/@SortBy[decomposeH@#,pnorm2[skewOmega[normal].Normalize[#[[3]]],2]&]&/@homogs];*)
	standardizeRtn@First@SortBy[decomposeH@#,pnorm2[skewOmega[normal].Normalize[#[[3]]],2]&]&/@homogs]];
homographyFromMatchesRansac=Function[{matches,imgs,ikms},
	ransacDriver[randomSampleAtMostN[Transpose@matches,100],If[Length@#>9,homographyFromMatches@Transpose@#,homographyFromMatchesL1@Transpose@#]&
			,reprojectionErrorHomography[#,Transpose@{#2}]&
			,0.5(1- nonzeroRatio@ImageData@ImagePerspectiveTransformation[imgs[[1]],#,DataRange->Full])+correlationErrorHomography[#,imgs]&,
		homographyFromMatchesL1@matches,3,Max[5,Round[0.5 Length@Transpose@matches]],Min[20,Round[0.7 Length@Transpose@matches]],20]];

(*dispRtn/@decomposeSameNormalCalibratedHomographyList[{Inverse[ikm.rs[[1,1]].Inverse[ikm]],ikm.rs[[2,1]].Inverse[ikm]}]*)


unitCircleGroup=Function[{a,b},If[Norm[Abs[a]^2-Abs[b]^2-1]>10^-6,Print["Abs[a]^2-Abs[b]^2=!=1"];Abort[],Function[z,(a z+b)/(Conjugate[b]z+Conjugate[a])]]];
\[Tau]0=0.323;
{mm,m}=Function[\[Tau],Table[With[{z=r(Cos[t]+Sin[t] I)},{Re@#,Im@#}&[unitCircleGroup[Sqrt[1+\[Tau]^2],\[Tau]][z]]],{r,0,1,0.3},{t,0,2Pi+0.1,0.3}]]/@{0,\[Tau]0};
transF=Function[{\[Tau],m},Map[unitCircleGroup[Sqrt[1+\[Tau]^2],\[Tau]][#[[1]]+#[[2]]I]&,m,{2}]];
Import@Export["t.png",Graphics@drawGrid@mm]
Import@Export["t2.png",Graphics@drawGrid@m]
Clear[\[Tau]];
Table[Print[r=NMinimize[pnorm2[transF[\[Tau],m],p],\[Tau](*,MaxIterations->1000*)];//AbsoluteTiming];
	{p,\[Tau]/.r[[2]],pnorm2[#,p]&/@{transF[\[Tau]/.r[[2]],m],transF[-\[Tau]0,m]}},{p,0.4,2.3,0.1}]


Manipulate[Graphics@drawGrid@
    Table[With[{z=r(Cos[t]+Sin[t] I)},{Re@#,Im@#}&[unitCircleGroup[Sqrt[1+\[Tau]^2],\[Tau]][z]]],{r,0,1,0.1},{t,0,2Pi+0.1,0.1}],{\[Tau],0,1}]


Clear[a,t];as=Array[a,3{1,1}];ts=Array[t,3];
r2=FindMinimum[Total@Flatten[((toInHomo[Inverse[ikm].Inverse[coord].Re@MatrixExp[as-Transpose@as].(coord.ikm.Append[#,1]+ts)]&/@matches[[1]])-matches[[2]])^2]
	,Join[variableWithInitial[as,lowerTriangle@Chop@MatrixLog@rot],variableWithInitial[ts,ts/.r[[2]]]]];
em3=With[{as=as/.r2[[2]]},MatrixExp[as-as\[Transpose]]].skewOmega[ts/.r2[[2]]];
Function[em,Total[MapThread[(coord.ikm.Append[#2,1]).em.coord.ikm.Append[#,1]&,matches]^2]]/@{em,em2,em3}


MapThread[Join,{MapIndexed[Prepend[#,#2[[1]]]&,aimgs[[-Length@trans2;;]]],Normalize/@trans2}]//MatrixForm


ListLinePlot[Transpose@Import["/s/"<>case<>"/world_accel.csv"][[;;,#]],PlotRange->All]&/@{{2,3,4},{5,6,7},{8,9,10}}


case="posing_square_v";imgs=Import/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
aimgs={};trans2=Parallelize@Table[findTranslationGivenAttitude[imgs[[{i,i+1}]],qs[[{i,i+1}]]],{i,Length@imgs-1}];


gs=Function[xyzs,{Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}],Graphics@BlendLineWithPoint[xyzs[[;;,;;2]]]}]/@
	{(*Accumulate[trans],*)(*Accumulate[Normalize/@trans],*)Accumulate[Normalize/@trans2]}


case="posing_leftturn3_n5_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];


ListLinePlot@Transpose[Import["/g/t3.csv"][[;;,2;;]]]
With[{xyzs=Import["/g/t3.csv"][[;;,2;;]]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


Import@Export["mathematica.png",#]&@Table[MapThread[annotateImageWithPoints,{imgs[[{i,i+1}]],ImageCorrespondingPoints[imgs[[i]],imgs[[i+1]],"Transformation"->"Perspective"]}]
	,{i,Length@imgs-10,Length@imgs-1}]


case="posing_square_out_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
resize=1/4;
lines=Import["/g/t2.csv"];imgHeight=ImageDimensions[imgs[[1]]][[2]];(*MapIndexed[{#2[[1]],Dimensions@#}&,lines]*)
SortBy[MapIndexed[{#2[[1]],(First@Dimensions@#-2)/4}&,lines],Last]
(*Import@Export["opencv.png",#]&@*)Table[{i,If[Length@lines[[i,2;;-2]]>=2,MapThread[annotateImageWithPoints,{imgs[[{i,i+1}]]
		,Transpose[4 resize rowColumnToXy[#[[2]]+1,#[[1]]+1,4 resize imgHeight]&/@Partition[#,2]&/@
	Partition[#,4]&[N@lines[[i,2;;-2]]]]}]]},{i,(*{38,39}*)1,Length@lines-1}]


(*case="posing_square_out_v";imgs=Import[#,ImageSize->{640,480}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];*)
lines=Import["/g/t2.csv"];imgHeight=ImageDimensions[imgs[[1]]][[2]];Dimensions/@lines
(*aimgs={};*)
trans=Parallelize@Table[findTranslationGivenAttitudeAndMatches[imgs[[{i,i+1}]],qs[[{i,i+1}]],Transpose[1/4rowColumnToXy[#[[2]]+1,#[[1]]+1,4imgHeight]&/@Partition[#,2]&/@
	Partition[#,4]&[lines[[i,2;;-2]]]]],{i,Length@imgs-1}];
(*gs=Function[xyzs,{Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}],Graphics@BlendLineWithPoint[xyzs[[;;,;;2]]]}]/@
	{(*Accumulate[trans],*)(*Accumulate[Normalize/@trans],*)Accumulate[Normalize/@trans]}*)
(*Graphics3D[BlendLineWithPoint@Accumulate[Normalize/@trans],Axes->True,AxesLabel->{"E","N","H"}]*)


MapThread[annotateImageWithPoints,{{i1,i2},Transpose[Partition[#,2]&/@Import["/g/matches.csv"]]}]


Export["/g/matches.csv",Flatten/@Map[Reverse@xyToRowColumn[#[[1]],#[[2]],imgHeight]&,Transpose[matches],{2}]]


Export["/g/matches.csv",Flatten/@Transpose[matches]]


findIndex=Function[i,Module[{j=i-1},While[j>=1&&Length@ImageCorrespondingPoints[imgs[[i]],imgs[[j]]][[1]]>50,j--];Min[i-1,j+1]]];
findIndex[10]


positions=Table[{0,0,0},{Length@imgs}];aimgs={};
Do[With[{j=findIndex[i]},Print[{i,j}];positions[[i]]=positions[[i]]+findTranslationGivenAttitude[imgs[[{i,j}]],qs[[{i,j}]]]],{i,2,(*Length@imgs*)20}]


Magnify[#,0.6]&@{annotateImageWithPoints[i1,rowColumnToXy[#[[2]],#[[1]],imgHeight]&/@Import@"/g/predictions_first.csv"],
annotateImageWithPoints[i2,rowColumnToXy[#[[2]],#[[1]],imgHeight]&/@Import@"/g/predictions_last.csv"]}


(*H cannot be reliably decomposed to (R,t,d) (??)*)
Clear[q,t,n,k];ts=Array[t,3];ns=Array[n,3];qs=Array[q,{2,4}];rot=LinearSolve@@(quaternionToRotationMatrix/@Reverse[{q1,q2}]);
r3=NMinimize[
	Norm[LinearSolve@@(quaternionToRotationMatrix/@Reverse[qs])-rot,"Frobenius"]
	+Norm[ikm.homogM.Inverse[ikm]-k qtnToH[qs,ts,ns,coord,IdentityMatrix[3]],"Frobenius"],Flatten@Join[ts,ns,qs,{k}]];
r3[[1]]
homogMF=#/#[[-1,-1]]&[k qtnToH[qs,ts,ns,coord,ikm]/.r3[[2]]];MatrixForm/@{homogMF,homogM}
rotMF=LinearSolve@@(quaternionToRotationMatrix/@Reverse[qs/.r3[[2]]]);
MatrixForm/@{rotMF,rot}
homogRMF=qtnToH[qs/.r3[[2]],0 ts,0 ns,coord,ikm];
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@{homogR,homogRMF,homog,homogM,homogMF},3]


ListAnimate@imgs[[;;]]


case="posing_leftturn_n5_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
ListLinePlot[Transpose@Import["/s/"<>case<>"/world_accel.csv"][[;;,#]],PlotRange->All]&/@{{2,3,4},{5,6,7},{8,9,10}}
ListAnimate@imgs


case="posing_cross_n5_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
ListLinePlot[Transpose@Import["/s/"<>case<>"/world_accel.csv"][[;;,#]],PlotRange->All]&/@{{2,3,4},{5,6,7},{8,9,10}}
ListAnimate@imgs


ListLinePlot@Transpose[Import["/g/t3.csv"][[;;,2;;]]]
With[{xyzs=Import["/g/t3.csv"][[;;,2;;]]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


ListLinePlot@Transpose[Import["/g/t3.csv"][[;;,2;;]]]
With[{xyzs=Import["/g/t3.csv"][[;;,2;;]]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


ListLinePlot@Transpose@r[[;;,1]]
With[{xyzs=r[[;;,1]]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


Table[{i,Graphics3D[{Opacity[0.5],drawPlane/@r[[i,2,;;,1]]},Axes->True,AxesLabel->{"E","N","H"}]},{i,Length@r}]


aimgs=Parallelize@Table[{i,annotateImageWithPointGroup[imgs[[i+1]],r[[i,2,;;,2]]]},{i,Length@r}];ListAnimate@aimgs


ListLinePlot[Length[Join@@#]&/@r[[;;,2,;;,2]],PlotRange->{0,Automatic}]


With[{work=Function[{arg1,arg2},Module[{t=arg1[[1]],planes=arg1[[2]],i=arg2,r},
	Print@i;Print[r=updateTranslationAndPlanes[t,planes,1,5,qs[[{i,i+1}]],imgs[[{i,i+1}]],coord,ikm];//AbsoluteTiming];r]]},
start=1;
r=FoldList[work,initTranslationAndPlanes[qs[[{start,start+1}]],imgs[[{start,start+1}]],coord,ikm],Range[start+1,Min[Infinity,Length@imgs-1]]]];


tss=Parallelize@Table[Module[{tsns1,tsns2},
	tsns1={#[[1]],#[[2,1]]}&@initTranslationAndOnePlane[qs[[{start,start-1}]],imgs[[{start,start-1}]],coord,ikm];
	tsns2={#[[1]],#[[2,1]]}&@initTranslationAndOnePlane[qs[[{start,start+1}]],imgs[[{start,start+1}]],coord,ikm];
	tsns2[[1]]Norm[tsns2[[2]]]/(Norm[tsns1[[1]]]Norm[tsns1[[2]]])],{start,2,Length@imgs-1}];
With[{xyzs=Accumulate[Exp@Accumulate[Log@Norm@#&/@tss] (Normalize/@tss)]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


With[{work=Function[{arg1,arg2},Module[{t=arg1[[1]],planes=arg1[[2]],i=arg2,r},
	Print@i;Print[r=updateTranslationAndPlanes[t,planes,1,5,qs[[{i,i+1}]],imgs[[{i,i+1}]],coord,ikm];//AbsoluteTiming];r]]},
start=1;
r=FoldList[work,initTranslationAndOnePlane[qs[[{start,start+1}]],imgs[[{start,start+1}]],coord,ikm],Range[start+1,Min[(*Infinity*)2,Length@imgs-1]]]];


planeHs=homogOfPlane[#,{0,0,0},ts,{q1,q2},coord,ikm]&/@planes;MatrixForm/@planeHs
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@planeHs,3]


(*planeFormMatches=Function[{matches,qPair,ts,coord,ikm},(*assume matches are Perspective*)Module[{n,ns,r,lm,rm},
	Clear[n];ns=Array[n,3];
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	r=FindMinimum[reprojectionErrorHomography[lm.(IdentityMatrix[3]+Outer[Times,ts,ns]).rm,matches]
		,If[NumericQ[ts[[1]]],{},variableWithInitial[ts,{0,0,0}]]~Join~variableWithInitial[ns,{1,1,0}]];
	
	]];*)


(*matches2=Transpose[Delete[Transpose[matches],13]];*)
MapThread[annotateImageWithPoints,{{i1,i2},matches2}]
{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[q2]],quaternionToRotationMatrix[q1].coord.ikm};
Clear[t,n];ts=Array[t,3];ns=Array[n,3];
r=FindMinimum[reprojectionErrorHomography[lm.(IdentityMatrix[3]+Outer[Times,ts,ns]).rm,matches2]
	,variableWithInitial[ts,{0,0,0}]~Join~variableWithInitial[ns,{1,1,0}]];
homogTSN=(lm.(IdentityMatrix[3]+Outer[Times,ts,ns]).rm)/.r[[2]];
ts/.r[[2]]
Normalize[ns/.r[[2]]]
(*Transpose[quaternionToRotationMatrix[q1]].Normalize[ns/.r[[2]]]*)


tss=Parallelize@Table[initTranslationAndOnePlane[qs[[{start,start+1}]],imgs[[{start,start+1}]],coord,ikm]
	,{start,2,Length@imgs-1}];
With[{xyzs=Accumulate[Normalize/@tss[[;;,1]]]},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]
tss[[;;,2,1]]


case="posing_cross_n5_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
ListLinePlot[Transpose@Import["/s/"<>case<>"/world_accel.csv"][[;;,#]],PlotRange->All]&/@{{2,3,4},{5,6,7},{8,9,10}}
coord={{0,1,0},{-1,0,0},{0,0,-1}};ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
indices=Range[6];{i1,i2,i3,i4,i5,i6}=imgs[[indices]];{q1,q2,q3,q4,q5,q6}=qs[[indices]];
matches=ImageCorrespondingPoints[i1,i2,"Transformation"->"Perspective"];homogM=homographyFromMatches[matches];
{ts,planes}=initTranslationAndPlanes[{q1,q2},{i1,i2},coord,ikm];plane=planes[[1]];ns=plane[[1]];Dimensions/@planes
homogR=qtnToH[{q1,q2},0 ts,0 ns,coord,ikm];homog=qtnToH[{q1,q2},ts,ns,coord,ikm];
{reprojectionErrorHomography[#,matches]&/@{homogR,homog,homogM},
anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@{homogR,homog,homogM}}
annotateImageWithPointGroup[i2,planes[[;;,2]]]

matches2=If[#=={},{{},{}},Transpose@#]&@Select[Transpose[imageTrackKeypoints[{i2,i3},plane[[2]]]],Head[#[[2]]]===List&];
{t3,planes3}=updateTranslationAndPlanes[ts,planes,1,5,{q2,q3},{i2,i3},coord,ikm];t23=t3-ts;Dimensions/@planes3
annotateImageWithPointGroup[i3,planes3[[;;,2]]]
homogM2=homographyFromMatches[matches2];plane3=planes3[[1]];
homog2=homogOfPlane[plane3,ts,t23,{q2,q3},coord,ikm];homogR2=homogOfPlane[plane3,ts,0 t23,{q2,q3},coord,ikm];
{reprojectionErrorHomography[#,matches2]&/@{homogR2,homog2,homogM2},
anaglyph@{ImagePerspectiveTransformation[i2,#,DataRange->Full],i3}&/@{homogR2,homog2,homogM2}}


normalizeHomoMatrix=#/#[[-1,-1]]&;
case="posing_snake_n5_v";imgs=Import/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
indices={25,26};{i1,i2}=imgs[[indices]];imgHeight=ImageDimensions[imgs[[1]]][[2]];
matches=ImageCorrespondingPoints[i1,i2(*,"Transformation"->"Epipolar"*)];
(*pmatches=MapIndexed[{#[[1]],ImageDimensions[imgs[[#2[[1]]]]][[2]]+1-#[[2]]}&,Transpose[Partition[#,2]&
	/@Transpose[Import["/h/p_matched.csv"]]],{2}];
matches=Transpose@Select[Transpose@pmatches,Norm[#[[1]]-#[[2]]]>50&];*)
Length@matches[[1]]
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];rot=LinearSolve@@(quaternionToRotationMatrix/@Reverse[qs[[indices]]]);
(*homog=findSimilarityHomography[{i2,i1}][[2]];*)
coord={{0,1,0},{-1,0,0},{0,0,-1}};homog2=Inverse[ikm].Inverse[coord].rot.coord.ikm;
Clear[t];ts=Array[t,3];ns=Array[n,3];toInHomo=#[[;;2]]/#[[-1]]&;
r3=FindMinimum[0.0 Total[ts^2]+1/Length[matches[[1]]] Sqrt[
		reprojectionErrorHomography[Inverse[coord.ikm].(rot+Outer[Times,ts,ns]).coord.ikm,matches]
		(*reprojectionErrorHomographyCalibrated[rot+Outer[Times,ts,ns],coord.ikm,matches]*)
	],Flatten@Join[ns,ts]];
homog3=Inverse[coord.ikm].(rot+Outer[Times,ts,ns]/.r3[[2]]).coord.ikm;
-Sign[(ns/.r3[[2]]).{0,0,-1}] Normalize@rotateByQuaternion[ts/.r3[[2]],qs[[indices[[2]]]]]
{(#/.r3[[2]])&/@{ts,ns},Normalize[#/.r3[[2]]]&/@{ts,ns}}
MatrixForm/@{(*homog,*)homog2,homog3}
{i1,i2,anaglyph@imgs[[indices]],anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@{(*homog,*)homog2,homog3}}
(*(*F-matrix method will not work*)
Normalize[First[decomposeEssentialMatrix[Transpose[Inverse[ikm]].fundamentalMatrixFromMatches[matches].Inverse[ikm]]][[;;3,-1]]]*)
(*r=NMinimize[Total@Flatten@Abs[((toInHomo[Inverse[ikm].Inverse[coord].rot.(coord.ikm.Append[#,1]+ts)]&/@matches[[1]])-matches[[2]])],ts];
em2=normalizeHomoMatrix[rot.skewOmega[ts/.r[[2]]]];Normalize[ts/.r[[2]]]
em=normalizeHomoMatrix@Transpose[Partition[SingularValueDecomposition[
	Join@@MapThread[KroneckerProduct[{coord.ikm.Append[#1,1]},coord.ikm.Append[#2,1]]&,matches]][[3,;;,-1]],3]];
Function[em,Total[MapThread[(coord.ikm.Append[#2,1]).em.coord.ikm.Append[#,1]&,matches]^2]]/@{em,em2}
fm=normalizeHomoMatrix[Transpose[coord.ikm].em.coord.ikm];*)
MapThread[annotateImageWithPoints,{{i1,i2},matches}]


posePairToHomography=Function[{posePair,km},Module[{q1,q2,rot,s,m3,m4},{q1,q2}=posePair;
	rot=LinearSolve@@(quaternionToRotationMatrix/@{q2,q1});
	(*s=Max@@imgDim;m3={{0,1,-imgDim[[1]]/2},{-1,0,imgDim[[2]]/2},{0,0,-s}};Inverse[m3].rot.m3*)
	m4={{0,1,0},{-1,0,0},{0,0,-1}};
	km.Inverse[m4].rot.m4.Inverse[km]
	]];
fnames=Table["/g/photo"<>IntegerString[i]<>".jpg",{i,Length@FileNames["/g/photo*.jpg"]}];
imgs=Import/@fnames;imuPoses=Import["/g/sensor.csv"];imuPoses2=Import["/g/sensor2.csv"];imgDim=ImageDimensions@imgs[[1]];
km=intrinsicParameterMatrix@imgs[[1]];
(*m={{1,0,-imgDim[[1]]/4},{0,1,-imgDim[[2]]/4},{0,0,1}};
m2={{0.5,0,0},{0,0.5,0},{0,0,1}}.{{1,0,-1},{0,1,-1},{0,0,1}}.{{1,0,0.5},{0,-1,imgDim[[2]]+0.5},{0,0,1}};
homog=#/#[[-1,-1]]&[Inverse[m.m2].Transpose[Partition[Import["/g/t.csv"][[1]],3]].m.m2];MatrixForm/@{m,m2}*)
Table[
indices={i,i+1};{i1,i2}=imgs[[indices]];
homog2=If[#=!=Null,Last@#]&@findSimilarityHomography@{i2,i1};
{homog3,homog4}=posePairToHomography[#[[indices,2;;]],km]&/@{imuPoses,imuPoses2};
homogs=If[homog2=!=Null,{(*homog,*)homog2,homog3,homog4},{homog3,homog4}];
{MatrixForm/@homogs,Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@homogs]}
,{i,{4}}]


(*fnames=FileNames["/g/photo*.jpg"];imgs=Import/@fnames;imuPoses=Import["/g/sensor.csv"];imgDim=ImageDimensions@imgs[[1]];
qm=quaternionToRotationMatrix@quaternionConjugate@quaternionMul[imuPoses[[3,2;;]],quaternionConjugate[imuPoses[[2,2;;]]]];
m3={{3.2/imgDim[[1]],0,0},{0,3.2/imgDim[[2]],0},{0,0,4.6}};
homog3=Inverse[m3].qm.m3;
MatrixForm/@{qm,homog3}*)


(*tr=findSimilarityHomography[imgs[[{2,1}]]];*)
(*anaglyph@{ImagePerspectiveTransformation[imgs[[1]],tr[[2]],DataRange->Full],imgs[[2]]}*)
(*imgDim=ImageDimensions@imgs[[1]];*)
pts=rowColumnToXy[1+#[[2]],1+#[[1]],imgDim[[2]]]&/@Import["/g/t.csv"][[;;,2;;]];annotateImageWithPoints[i1,pts]
pts2=Most[homog3.Append[#,1]]&/@pts;annotateImageWithPoints[i2,pts2]
Export["/g/predictions.csv",Reverse[#-1]&/@Select[xyToRowColumn[#[[1]],#[[2]],imgDim[[2]]]&/@pts2
	,(*1<=#[[1]]<=imgDim[[1]]&&1<=#[[2]]<=imgDim[[2]]&*)True&]]


imgs=Import[#,ImageSize->640]&/@FileNames["/h/d/video_with_imu/pingpong/*.JPG"][[;;5]];(*ListAnimate@imgs*)
km=intrinsicParameterMatrix@imgs[[1]];


pointsFromEm=Function[{match,projectionM,inverseKm},Module[{tmatch=inverseKm.Append[#,1]&/@match,projs={IdentityMatrix[{3,4}],projectionM}},
		#[[;;3]]/#[[-1]]&[SingularValueDecomposition[#,4][[3,;;,-1]]]&@
			(Join@@Table[{projs[[j,3]]#-projs[[j,1]]&@tmatch[[j,1]],projs[[j,3]]#-projs[[j,2]]&@tmatch[[j,2]]},{j,2}])
	]];
points3dFromEssentialMatrix=Function[{em,km,matches},Module[{trans,tran,projs},
	trans=Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&];
	If[Length@trans==0,{Array[{0,0,0}&,Length@matches[[1]]],IdentityMatrix[{3,4}]}
		,tran=trans[[1]];
		With[{projM=Inverse[tran][[;;3]]},{pointsFromEm[#,projM,Inverse[km]]&/@Transpose@matches,projM}]]
	]];


{points,projM}=points3dFromEssentialMatrix[em,km,matches];


tracks=ImageFeatureTrack[imgs[[;;5]]];
annotateImageWithTracks[imgs[[1]],tracks]


matches=ImageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];
MapThread[annotateImageWithPoints,{imgs[[;;2]],matches}]
fm=fundamentalMatrixFromMatchesLp[matches,1];em=km\[Transpose].fm.km;


blendCameras[Join[30#[[1]],quaternionFromRotationMatrix@#[[2,;;3,;;3]]]&/@aligned[[;;,2]]]


imuPoses=#[[1]]->#[[2;;]]&/@Import["/g/sensor.csv"];
cameras=#[[1]]->{#[[2;;4]],Partition[#[[5;;]],4]}&/@Import["/g/cameras.csv"];
aligned=Thread@{imuPoses[[cameras[[;;,1]],2]],cameras[[;;,2]]};xyzs=aligned[[;;,2,1]];
normalizeByFirst=Function[qs,quaternionMul[quaternionConjugate@qs[[1]],#]&/@qs];
mqs=(*normalizeByFirst/@*)Transpose@Table[{(*{#[[1]],-#[[3]],-#[[2]],-#[[4]]}&@*)aligned[[n,1]]
	,quaternionFromRotationMatrix@aligned[[n,2,2,;;3,;;3]]},{n,Length@aligned}];
Graphics3D[BlendLine[rotateByQuaternion[{-#[[2]],-#[[1]],-#[[3]]},mqs[[1,1]]]&@rotateByQuaternion[#,quaternionConjugate@mqs[[2,1]]]&/@xyzs]
	,Axes->True,AxesLabel->{"x","y","z"},ImageSize->400]
ListLinePlot@Transpose[{-#[[2]],-#[[1]],-#[[3]]}&@Normalize@#&/@MapThread[rotateByQuaternion,{Differences@xyzs,mqs[[2,;;-2]]}]]
(*{ListLinePlot@Transpose@mqs[[1,;;,;;]],ListLinePlot@Transpose@mqs[[2,;;,;;]]}
Function[xyzs,{ListLinePlot@Transpose@xyzs,Graphics3D[BlendLine@xyzs,Axes->True,AxesLabel->{"x","y","z"}]}]/@
	{xyzs,rotateByQuaternion[#,mqs[[1,1]]]&/@(-{#[[2]],#[[1]],#[[3]]}&@rotateByQuaternion[#,quaternionConjugate[mqs[[2,1]]]]&/@xyzs)}*)


(*Galaxy Neuxs, sensor width
Nexus 4, sensor width, 3.2, focal length, 4.6, (*1/4" sensor*)*)


rs=findSimilarityHomography/@Partition[imgs,2,1];
MatrixForm/@rs[[;;,2]]


MatrixForm[(*{{0,1,0},{1,0,0},{0,0,1}}.*)Inverse[m3].#.m3]&/@(Inverse[#[[1]]].#[[2]]&/@Partition[quaternionToRotationMatrix/@imuPoses[[;;,2;;]],2,1])




tracks=Transpose[Partition[#,2]&/@Import["/g/tracks.csv"][[;;,2;;]]];
MapThread[annotateImageWithPoints,{imgs[[;;2]],Map[rowColumnToXy[2#[[2]]+1,2#[[1]]+1,imgDim[[2]]]&,smatches,{2}]}]
MapThread[annotateImageWithPoints,{imgs[[;;2]],Map[rowColumnToXy[#[[2]]+1,#[[1]]+1,imgDim[[2]]]&,tracks,{2}]}]


MatrixForm/@{#/#[[-1,-1]]&@Transpose[Partition[Import["/g/t.csv"][[1]],3]],m.m2.Inverse[m3].Inverse[rot].m3.Inverse[m.m2]}


imgs=Import/@Table["/g/photo"<>IntegerString[i]<>".jpg",{i,Length@FileNames["/g/photo*.jpg"]}];imgDim=ImageDimensions@imgs[[1]];
lines=ImportString[StringReplace[Import["/g/param.dat","Text"]," "->","],"CSV"];
poses=Join[ImportString[StringReplace[FileBaseName[#[[1,1]]],"photo"->""],"CSV"][[1]],#[[5]],quaternionFromRotationMatrix[#[[2;;4]]]]&/@SortBy[Partition[lines[[6;;]],5],#[[1,1]]&];
imuPoses=Import["/g/sensor.csv"];


qs=poses[[;;,1]]/.(#[[1]]->#[[2;;]]&/@imuPoses);
qs2=quaternionConjugate[quaternionMul[quaternionConjugate[qs[[1]]],#]]&/@qs;
ListLinePlot@Transpose[qs]
ListLinePlot@Transpose[qs2]
ListLinePlot@Transpose[quaternionConjugate[#][[{1,3,2,4}]]&/@poses[[;;,5;;8]]]


accFlowss=FoldList[joinTwoFlows,flowss[[1]],flowss[[2;;n]]];
anaglyph@{imgs[[1]],warpByFlow[imgs[[n+1]],accFlowss[[n]]]}




Export["/g/t.csv",N@Transpose[randomSampleAtMostN[tracks,10000],{2,3,1}][[1]]]
Export["/g/t2.csv",N@Transpose[randomSampleAtMostN[tracks,10000],{2,3,1}][[2]]]






tracks=Join@@Transpose[Function[flow,Module[{imgHeight=Dimensions[flow][[1]]},MapIndexed[
	With[{rc=#2+Reverse[#]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]&,flow,{2}]]]/@accFlowss,{3,1,2,4}];tracks//Dimensions
Export["t.csv",Transpose[randomSampleAtMostN[tracks,10000],{2,3,1}]]




ListAnimate@MapThread[annotateImageWithPoints,{imgs[[2;;]],Transpose@randomSampleAtMostN[tracks,100]}]




annotateImageWithTracks[imgs[[1]],randomSampleAtMostN[tracks,100]]




matches=matchesFromFlows@Fold[joinTwoFlows,flowss[[1]],flowss[[2;;n]]];
Function[fm,drawEpipolarLineForFundamentalMatrix[imgs[[n+1]],fm,3]]/@
				{(*fundamentalMatrixFromMatchesLp[matches2,0.5]
				,*)fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@matches,10000]],1]}




n=9;Import@Export["t.png",#]&@anaglyph@{imgs[[1]],warpByFlow[imgs[[n+1]],Fold[joinTwoFlows,flowss[[1]],flowss[[2;;n]]]]}




fnames=Table["/h/d/pano_with_imu/tian_rect_half_slide/iframe/image-015"<>IntegerString[i]<>".JPG",{i,0,9}];
imgs=Import/@fnames
flowss=Parallelize[Transpose[Partition[#,Length@#/2]&/@Import["/h/m/OpticalFlow/output/"<>FileBaseName[#]<>"_flow.csv"],{1,3,2}]&/@fnames];




testOpticalFlow=Function[{fnames,odir},Module[{imgs,flows,dim,imgHeight,m,warpedIm2,matches,matches2},
	imgs=(*imageResizeMaxSize[Import[#],600]&*)Import/@fnames;
	If[0==Run["/h/bin/matlab -nosplash -nodesktop -nojvm -r \"cd('/h/m/OpticalFlow');generate_flow({'"<>fnames[[1]]<>"', '"<>fnames[[2]]<>"'});exit\""],
		flows=Transpose[Partition[#,Length@#/2]&/@Import["/h/m/OpticalFlow/output/"<>FileBaseName[fnames[[1]]]<>"_flow.csv"],{1,3,2}];
		dim=Dimensions@ImageData@imgs[[2]];m=ImageData[imgs[[1]]];imgHeight=ImageDimensions[imgs[[2]]][[2]];
		warpedIm2=warpByFlow[imgs[[2]],flows];	
		Export[odir<>"/"<>FileBaseName[fnames[[2]]]<>"_warped.JPG",anaglyph@{imgs[[1]],warpedIm2}];
		matches2=ImageCorrespondingPoints@@imgs;
		matches=Transpose[Join@@Parallelize@Table[{rowColumnToXy[i,j,imgHeight]
			,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]},{i,dim[[1]]},{j,dim[[2]]}]];
		Export[odir<>"/"<>FileBaseName[fnames[[2]]]<>"_epipolar.JPG",#]&@
			Rasterize[Function[fm,drawEpipolarLineForFundamentalMatrix[imgs[[2]],fm,3]]/@
				{fundamentalMatrixFromMatchesLp[matches2,0.5]
				,fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@matches,10000]],1]}]
	];]];


Parallelize[testOpticalFlow[#,"/h/d/video_with_imu/pingpong/flow/"]&/@(Partition[FileNames["/h/d/video_with_imu/pingpong/*.JPG"],2,1])]


Parallelize[testOpticalFlow[#,"/s/tmp/"<>case<>"/flow"]&/@Partition[Table["/s/tmp/"<>case<>"/"<>IntegerString[n]<>".jpg",{n,Length@timestampedImages}],2,1]]


Import@Export["crab_epipolar.png",#]&@Rasterize[Import[#,ImageSize->400]&/@Table["/s/tmp/crab/flow/"<>IntegerString[i]<>"_epipolar.JPG",{i,2,Length@FileNames["/s/tmp/crab/flow/*_epipolar.JPG"]}]]


Table[{n,Dimensions@Import["/s/tmp/"<>case<>"/p_matched"<>IntegerString[n]<>".csv"]},{n,Length@timestampedImages-1}]


velocities2=Parallelize@Table[flows=Transpose[Partition[#,Length@#/2]&/@Import["/h/m/OpticalFlow/output/"<>IntegerString[n]<>"_flow.csv"],{1,3,2}];
	dim=Dimensions@ImageData@imgs[[2]];m=ImageData[imgs[[1]]];imgHeight=ImageDimensions[imgs[[2]]][[2]];
	matches=Transpose[randomSampleAtMostN[Join@@Parallelize@Table[{rowColumnToXy[i,j,imgHeight]
			,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]},{i,dim[[1]]},{j,dim[[2]]}],10000]];
	motionFromMatches[matches,km,gyrosF[timestampedImages[[n,1]]]]
,{n,Length@timestampedImages-1}];


Export["~/mxs/velocities2.mx",{velocities2}]


ListAnimate[ImageRotate[ImageResize[#,300],-Pi/2]&/@timestampedImages[[90;;,2]]]


Import@Export["t.png",Magnify[#,3]&@Rasterize@{ListLinePlot[MedianFilter[#,1]&/@Transpose[Normalize/@velocities2]],ListLinePlot[MedianFilter[#,5]&/@Transpose[Normalize/@velocities2]]}]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[2]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
n=1;fnames={"/s/tmp/"<>case<>"/"<>IntegerString[n]<>".jpg", "/s/tmp/"<>case<>"/"<>IntegerString[n+1]<>".jpg"};
imgs=ColorConvert[Import@#,"Grayscale"]&/@fnames;
km=intrinsicParameterMatrix@imgs[[1]];tmatches=Map[Inverse[km].Append[#,1]&,matches,{2}];
matches=MapIndexed[{#[[1]],ImageDimensions[imgs[[#2[[1]]]]][[2]]+1-#[[2]]}&,Transpose[Partition[#,2]&
	/@Transpose[Import["/s/tmp/"<>case<>"/p_matched"<>IntegerString[n]<>".csv"]]],{2}];
scaling=400/Mean@ImageDimensions[imgs[[2]]];
Magnify[#,scaling]&@MapThread[annotateImageWithPoints,{imgs,matches}]
motionFromMatches=Function[{matches,intrinsicParameterMatrix,angularVelocity},Module[{km=intrinsicParameterMatrix,\[Omega]=angularVelocity,tmatches,mat},
		tmatches=Map[Inverse[km].Append[#,1]&,matches,{2}];
		mat=MapThread[With[{u=#2-#,x=#[[1]],y=#[[2]]}
			,Last[skewOmega[Append[Most@u-({{x y,-x^2,y},{y^2,-x y,-x}}.\[Omega]),0]].{{-1,0,x},{0,-1,y},{0,0,0}}]]&,tmatches,1];
		If[Length@mat<3,{0,0,0},SingularValueDecomposition[mat,3][[3,;;,-1]]]
	]];
velocities=Parallelize@Table[matches=MapIndexed[{#[[1]],ImageDimensions[imgs[[#2[[1]]]]][[2]]+1-#[[2]]}&,
		Transpose[Partition[#,2]&/@Transpose[Import["/s/tmp/"<>case<>"/p_matched"<>IntegerString[n]<>".csv"]]],{2}];
			motionFromMatches[matches,km,gyrosF[timestampedImages[[n,1]]]]
	,{n,Length@timestampedImages-1}];
procVelocities=Transpose[MedianFilter[#,3]&/@Transpose[Normalize[#-Min[#]]&/@Abs@#]]&;
ListLinePlot[Transpose@procVelocities[velocities]]
g=Function[vels,trans=MapThread[rotateByQuaternion,{vels,Most@qs}];xyzs=Accumulate@Select[trans,NumericQ@#[[1]]&];
	Graphics3D[{BlendLine[xyzs],Point@xyzs},ViewPoint->{0,0,10},Axes->True]]/@
		{procVelocities[velocities],Array[{0,1,0}&,Length@velocities]}
Import@Export["t.png",Rasterize[g]];


flows=Transpose[Partition[#,Length@#/2]&/@Import@"t.csv",{1,3,2}];*)
case="forward";fnames=FileNames["/h/"<>case<>"*.JPG"];
imgs=Import[#(*,ImageSize->320*)]&/@fnames;
Run["/h/bin/matlab -nosplash -nodesktop -nojvm -r \"cd('/h/m/OpticalFlow');generate_flow({'"<>fnames[[1]]<>"', '"<>fnames[[2]]<>"'});exit\""]
flows=Transpose[Partition[#,Length@#/2]&/@Import["/h/m/OpticalFlow/output/"<>FileBaseName[fnames[[1]]]<>"_flow.csv"],{1,3,2}];flows//Dimensions
dim=Dimensions@ImageData@imgs[[2]];m=ImageData[imgs[[1]]];imgHeight=ImageDimensions[imgs[[2]]][[2]];
warpedIm2=Image@Parallelize@Table[ImageValue[imgs[[2]]
	,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]],DataRange->Full],{i,dim[[1]]},{j,dim[[2]]}];
imgs
anaglyph@{imgs[[1]],warpedIm2}
matches2=ImageCorrespondingPoints@@imgs;
matches=Transpose[Join@@Parallelize@Table[{rowColumnToXy[i,j,imgHeight]
	,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]},{i,dim[[1]]},{j,dim[[2]]}]];
nmatches=Transpose@Select[Transpose@matches,Norm[#[[1]]-#[[2]]]<Min@ImageDimensions@imgs[[1]]/10&];
Dimensions/@{matches2,matches,nmatches}
Function[fm,drawEpipolarLineForFundamentalMatrix[imgs[[2]],fm,3]]/@
	{fundamentalMatrixFromMatchesLp[matches2,0.5]
	,fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@matches,10000]],1]
	,fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@nmatches,10000]],1]}
imgs[[1]]
Graphics@dirs
imgs[[2]]*)


epipoles=Parallelize[Function[csv,Module[{flows,matches,imgHeight,dim},
	(*Print[csv];*)
	flows=Transpose[Partition[#,Length@#/2]&/@Import[csv],{1,3,2}];dim=Dimensions@flows;imgHeight=dim[[1]];
	matches=matchesFromFlows@flows;
	First@NullSpace[fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@matches,10000]],1]]
	]]/@FileNames["/h/m/OpticalFlow/output/image*flow.csv"]];


ListAnimate[imgs[[30;;100]]]
ListAnimate[imgs[[210;;280]]]


{epipoles}=Import@"~/mxs/epipoles.mx";
imgs=Import/@FileNames["/h/d/pano_with_imu/tian_rect_half_slide/iframe/*.JPG"];
imgDim=ImageDimensions[imgs[[1]]];
ListLinePlot[MedianFilter[#,3]&/@Transpose[Normalize[#[[;;2]]/#[[-1]]-imgDim/2]&/@epipoles],ImageSize->600]


ListLinePlot[(*MedianFilter[#,1]&/@*)Transpose[Normalize[#[[;;2]]/#[[-1]]-imgDim/2]&/@epipoles],ImageSize->600]


Magnify[Import[#,ImageSize->200]&/@FileNames["/h/d/pano_with_imu/tian_rect_half_slide/flow/*_epipolar.JPG"],3]
ListAnimate[Import[#,ImageSize->600]&/@FileNames["/h/d/pano_with_imu/tian_rect_half_slide/flow/*_epipolar*.JPG"]]
ListAnimate[Import/@Table["/h/d/pano_with_imu/tian_rect_half_slide/iframe/image-0"<>IntegerString[i,10,3]<>".JPG",{i,(*40,90*)210,260}]]


ListLinePlot[MedianFilter[#,3]&/@Transpose[#[[;;2]]/#[[3]]&/@epipoles](*,PlotRange->All*)]
ListLinePlot[First[MedianFilter[#,3]&/@Transpose[#[[;;2]]/#[[3]]&/@epipoles](*,PlotRange->All*)]]
ListLinePlot[Last[MedianFilter[#,3]&/@Transpose[#[[;;2]]/#[[3]]&/@epipoles](*,PlotRange->All*)]]


ListPlot[Normalize[#[[;;2]]][[1]]&/@Transpose[MeanFilter[MedianFilter[#,5],1]&/@Transpose@epipoles]]


imgHeight=ImageDimensions[imgs[[2]]][[2]];
epipoles=Parallelize[Function[csv,Module[{flows,matches,imgHeight,dim},
	(*Print[csv];*)
	flows=Transpose[Partition[#,Length@#/2]&/@Import[csv],{1,3,2}];dim=Dimensions@flows;imgHeight=dim[[1]];
	matches=Transpose[Join@@Table[{rowColumnToXy[i,j,imgHeight]
			,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]},{i,dim[[1]]},{j,dim[[2]]}]];
	First@NullSpace[fundamentalMatrixFromMatchesLp[Transpose[randomSampleAtMostN[Transpose@matches,10000]],1]]
	]]/@FileNames["/h/m/OpticalFlow/output/image*flow.csv"]];
ListLinePlot[MedianFilter[#,3]&/@Transpose@epipoles]


{im1,im2}=imgs[[{1,-2}]];
pts=ImageFeatureTrack[{im1,im2}(*,MaxFeatures->20*)];
Show[ImageAssemble[{im1,im2}],Graphics[{Green,PointSize[.02],MapThread[If[#2===Missing[],{Cyan,Point[#1]},Arrow[{#1,#2+{ImageDimensions[im1][[1]],0}}]]&,pts]}]]
Show[ImageAssemble[{im1,im2}],Graphics[{Green,PointSize[.02],MapThread[Arrow[{#1,#2+{ImageDimensions[im1][[1]],0}}]&,ImageCorrespondingPoints[im1,im2]]}]]


case="tian_rect_half_slide";imuPoses=Import["/h/d/pano_with_imu/"<>case<>"/pose.csv"];
blendCameras@MapThread[Join,{5 imuPoses[[;;,2;;4]],imuPoses[[;;,5;;8]]}]



Magnify[anaglyph@{annotateImageWithPoints[i2,allMatches[[2]]],ImagePerspectiveTransformation[annotateImageWithPoints[i1,allMatches[[1]]],#,DataRange->Full]}&/@rs,3]


mat=Join@@MapThread[KroneckerProduct[{ikm.Append[#1,1]},skewOmega[ikm.Append[#2,1]]]&,matchesB];
{#,pnorm2[#,2]}&[mat.Normalize@vec[rs[[1]]]]
ListLinePlot[mat.Normalize@vec[rs[[1]]]]
{#,pnorm2[#,2]}&[mat.Normalize@vec[rs[[2]]]]
ListLinePlot[mat.Normalize@vec[rs[[2]]]]
crossErrorHomography=Function[{homog,matches},MapThread[skewOmega[Append[#2,1]].homog.Append[#,1]&,matches]];
crossErrorHomography[rs[[1]],matchesA]
crossErrorHomography[rs[[2]],matchesB]
crossErrorHomography[rs[[3]],allMatches]


rs=homographyFromMatchesTsNsReprojection[#,{q1,q2},coord,ikm]&/@{matchesA,matchesB,allMatches};
reprojectionErrorHomography[rs[[1]],matchesA]
reprojectionErrorHomography[rs[[2]],matchesB]
reprojectionErrorHomography[rs[[3]],allMatches]
correlationErrorHomography[#,{i1,i2}]&/@rs





hss=Table[hsFromImagesCorrelation[GaussianFilter[#,Mean@ImageDimensions@i1/10]&/@{i1,i2},{q1,q2},coord,ikm,i],{i,3}]


Table[MatrixForm/@SingularValueDecomposition[IdentityMatrix[3]+SvdApprox[hss[[3]],i]],{i,3}]


qPair={q1,q2};
{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
MatrixForm/@SingularValueDecomposition[hss[[3]]]
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@Table[lm.(IdentityMatrix[3]+SvdApprox[hss[[3]],i]).rm,{i,3}],2]
reprojectionErrorHomography[#,allMatches]&/@Table[lm.(IdentityMatrix[3]+SvdApprox[hss[[3]],i]).rm,{i,3}]


trans=Parallelize@Table[
	homogCorrs=Join@@Table[scaleM=DiagonalMatrix@Append[i{1,1},1];
		scaleM.homographyFromImagesCorrelation[
			ImageResize[#,ImageDimensions[#]/i]&/@imgs[[j;;j+1]],qs[[j;;j+1]],coord,ikm.scaleM,rank].Inverse[scaleM]
	,{i,{4(*,8*)}},{rank,{(*2,*)3}}];
	SingularValueDecomposition[hsFromHomog[homogCorrs[[1]],qs[[j;;j+1]],coord,ikm]][[1,;;,1]]
	,{j,Length@imgs-1}];
With[{xyzs=Accumulate@trans},Graphics3D[BlendLineWithPoint@xyzs,Axes->True,AxesLabel->{"E","N","H"}]]


trans[[;;5]]


Parallelize@Table[
	homogCorrs=Join@@Table[scaleM=DiagonalMatrix@Append[i{1,1},1];
		scaleM.homographyFromImagesCorrelation[
			ImageResize[#,ImageDimensions[#]/i]&/@imgs[[j;;j+1]],qs[[j;;j+1]],coord,ikm.scaleM,rank].Inverse[scaleM]
	,{i,{4,8}},{rank,{2,3}}];
	Magnify[Join[imgs[[j;;j+1]],{anaglyph@#,Correlation@@(Flatten@ImageData@#&/@#)}&@{ImagePerspectiveTransformation[imgs[[j]],#,DataRange->Full],imgs[[j+1]]}&/@homogCorrs],2]
	,{j,Length@imgs-1}]


allMatches=ImageCorrespondingPoints[i1,i2];
homogCorrs=Table[scaleM=DiagonalMatrix@Append[i{1,1},1];
		scaleM.homographyFromImagesCorrelation[ImageResize[#,ImageDimensions[#]/i]&/@{i1,i2},{q1,q2},coord,ikm.scaleM,2].Inverse[scaleM]
	,{i,{1,2,4(*,8,16*)}}]
(*Import@Export["t.png",#]&@Rasterize@*)Magnify[Join[{i1,i2},GaussianFilter[#,Mean@ImageDimensions@i1/10]&/@{i1,i2}
	,anaglyph@{annotateImageWithPoints[ImagePerspectiveTransformation[i1,#,DataRange->Full],allMatches[[1]]]
		,annotateImageWithPoints[i2,allMatches[[2]]]}&/@homogCorrs],2]
reprojectionErrorHomography[#,allMatches]&/@homogCorrs


(*homogCorrs=Table[homographyFromImagesCorrelation[GaussianFilter[#,Mean@ImageDimensions@i1/10]&/@{i1,i2},{q1,q2},coord,ikm,i],{i,3}];
homogCorrs2=Table[homographyFromImagesCorrelation[GaussianFilter[#,Mean@ImageDimensions@i1/10]&/@
	(ImageTransformation[#,If[#[[2]]<120,0.1#,#]&,DataRange->Full]&/@{i1,i2}),{q1,q2},coord,ikm,i],{i,3}];
homogCorrs3=Table[homographyFromImagesCorrelation[GaussianFilter[#,Mean@ImageDimensions@i1/10]&/@
	(ImageTransformation[#,If[#[[2]]>120,0.1#,#]&,DataRange->Full]&/@{i1,i2}),{q1,q2},coord,ikm,i],{i,3}];*)

(*(*Cascading seems no additional benefit.*)
qPair={q1,q2};
{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
homogCorrs={};
i=8;scaleM=DiagonalMatrix@Append[i{1,1},1];
AppendTo[homogCorrs,scaleM.homographyFromImagesCorrelation[
	ImageResize[#,ImageDimensions[#]/i]&/@{i1,i2},{q1,q2},coord,ikm.scaleM,3].Inverse[scaleM]];
hs=hsFromHomog[homogCorrs[[-1]],{q1,q2},coord,ikm];i=4;
scaleM=DiagonalMatrix@Append[i{1,1},1];
AppendTo[homogCorrs,scaleM.homographyFromImagesCorrelationWithInit[
	ImageResize[#,ImageDimensions[#]/i]&/@{i1,i2},{q1,q2},coord,ikm.scaleM,3,homogFromHs[hs,{q1,q2},coord,ikm.scaleM]].Inverse[scaleM]];*)


(*homographyFromImagesCorrelationCascade=Function[{imgPair,qPair,coord,ikm,rank,orders},Module[{homog,lm,rm},
	{lm,rm}={Inverse[coord.ikm].Transpose[quaternionToRotationMatrix[qPair[[2]]]],quaternionToRotationMatrix[qPair[[1]]].coord.ikm};
	homog=homogFromHs[Array[0&,3{1,1}],qPair,coord,ikm];
	Table[scaleM=DiagonalMatrix@Append[order{1,1},1];
		scaleM.homographyFromImagesCorrelation[
			ImageResize[#,ImageDimensions[#]/order]&/@{i1,i2},{q1,q2},coord,ikm.scaleM,3].Inverse[scaleM]
		homographyFromImagesCorrelationWithInit[imgPair,qPair,coord,ikm,rank,homogFromHs[Array[0&,3{1,1}],qPair,coord,ikm]]
	,{order,orders}]
	]];*)


{ts,planes}=initTranslationAndPlanes[{q1,q2},{i1,i2},coord,ikm];
homogPs=homogOfPlane[#,{0,0,0},ts,{q1,q2},coord,ikm]&/@planes;MatrixForm/@homogPs
Outer[reprojectionErrorHomography,homogPs,matchGroups,1]//MatrixForm
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@homogPs,2]


r=SortBy[Table[
pts=Transpose@RandomSample[Transpose@allMatches,5];homogT=homographyFromMatches@pts;
allPts=Transpose[Transpose@pts~Join~homographyInlierOutliers[allMatches,homogT,1][[1]]];
{Length@Transpose@allPts,(*SingularValueList[Append[#,1]&/@#]&*)conditionNumber/@pts
	,Join[MapThread[annotateImageWithPoints,{{i1,i2},pts}],MapThread[annotateImageWithPoints,{{i1,i2},allPts}]]
	,Magnify[anaglyph@{ImagePerspectiveTransformation[i1,homogT,DataRange->Full],i2},2]}
,{10}],-#[[1]]&]


allMatches=ImageCorrespondingPoints[i1,i2];
matchesA=Transpose[Select[Transpose@allMatches,((*Print[#[[1,2]]];*)#[[1,2]]<100)&]];
matchesB=Transpose[Select[Transpose@allMatches,((*Print[#[[1,2]]];*)#[[1,2]]>=100)&]];
matchGroups={allMatches,matches,matchesA,matchesB};homogs=homographyFromMatches/@matchGroups;MatrixForm/@homogs
Outer[reprojectionErrorHomography,homogs,matchGroups,1]//MatrixForm
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@homogs,2]
{MapThread[annotateImageWithPoints,{{i1,i2},allMatches}],MapThread[annotateImageWithPoints,{{i1,i2},matchesA}]
	,MapThread[annotateImageWithPoints,{{i1,i2},matchesB}]}
(*Table[{Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@homogs,2]
	,Table[reprojectionErrorHomography[#,m]&/@homogs,{m,{allMatches,matchesA,matchesB}}]},{homogs,{homogCorrs(*,homogCorrs2,homogCorrs3*)}}]*)

{MatrixForm/@#,Normalize@#[[3]]}&@decomposeHomography[#,coord,ikm]&/@homogs
LinearSolve@@(quaternionToRotationMatrix/@{q2,q1})//MatrixForm

(*(*Points on different planes are kind of linearly separable*)
{rot,t,n}=decomposeHomography[#,coord,ikm]&@homogs[[3]]
Graphics3D[Point[With[{x1=coord.ikm.Append[#[[1]],1],x2=coord.ikm.Append[#[[2]],1]}
	,Normalize@LeastSquares[Outer[Times,skewOmega[x2].t,x1],-skewOmega[x2].rot.x1]]&/@Transpose@#],Axes->True,AxesLabel->{"x","y","z"}]&/@matchGroups*)


case="posing_two_planes3_n5_v";imgs=Import[#,ImageSize->{320,240}]&/@FileNames["/s/"<>case<>"/*.jpg"];qs=Import["/s/"<>case<>"/sensor.csv"][[;;,2;;5]];
ListLinePlot[Transpose@Import["/s/"<>case<>"/world_accel.csv"][[;;,#]],PlotRange->All]&/@{{2,3,4},{5,6,7},{8,9,10}}
coord={{0,1,0},{-1,0,0},{0,0,-1}};ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
indices={1,10}+10;{i1,i2}=imgs[[indices]];{q1,q2}=qs[[indices]];
allMatches=ImageCorrespondingPoints[i1,i2];matches=ImageCorrespondingPoints[i1,i2,"Transformation"->"Perspective"];homogM=homographyFromMatches[matches];
{MapThread[annotateImageWithPoints,{{i1,i2},matches}]
	,MapThread[annotateImageWithPoints,{{i1,i2},Transpose[Complement[Transpose@allMatches,Transpose@matches]]}]}
{ts,planes}=initTranslationAndPlanes[{q1,q2},{i1,i2},coord,ikm];plane=planes[[1]];ns=plane[[1]];Dimensions/@planes
homogR=qtnToH[{q1,q2},0 ts,0 ns,coord,ikm];homog=qtnToH[{q1,q2},ts,ns,coord,ikm];
{reprojectionErrorHomography[#,matches]&/@{homogR,homog,homogM},
anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@{homogR,homog,homogM}}
annotateImageWithPointGroup[i2,planes[[;;,2]]]


ii1=ImagePerspectiveTransformation[i1,homogR,DataRange->Full]
matches=ImageCorrespondingPoints@@{i1,i2};
MapThread[annotateImageWithPoints,{{i1,i2},matches}]
matches2=ImageCorrespondingPoints@@{ii1,i2};
MapThread[annotateImageWithPoints,{{ii1,i2},matches2}]
Dimensions/@{matches,matches2}


MapThread[annotateImageWithPoints,{{i1,i2},matches}]
inlierHPairs0=findInlierHomographyPairs[matches,1,3,3,50,ikm];
inlierHPairs=Nest[optimizeInlierHPairs[#,ikm]&,inlierHPairs0,2];
test=#[[1,1]]<100&;inoutLiers={Select[Transpose@matches,test],Select[Transpose@matches,Not@test@#&]};
inlierHPairs2={#,homographyFromMatchesCalibrated[#,ikm]&@Transpose@#}&/@inoutLiers;
inlierHPairs22=optimizeInlierHPairs[#,ikm]&@optimizeInlierHPairs[#,ikm]&@inlierHPairs2;
costOfinlierHPairsLp=Function[{inlierHPairs,p},Total[reprojectionErrorHomographyLp[#[[2]],transposeLiers@#[[1]],p]&/@inlierHPairs]];
costOfinlierHPairsLp[#,2]&/@{inlierHPairs0,inlierHPairs,inlierHPairs2,inlierHPairs22}
costOfinlierHPairsLp[#,0.3]&/@{inlierHPairs0,inlierHPairs,inlierHPairs2,inlierHPairs22}
{Magnify[annotateImageWithPointGroup[i2,transposeLiers[#[[1]]][[2]]&/@inlierHPairs],2]
,Magnify[annotateImageWithPointGroup[i2,{transposeLiers[Select[Transpose@matches,test]][[2]],transposeLiers[Select[Transpose@matches,Not@test@#&]][[2]]}],2]
,Magnify[annotateImageWithPointGroup[i2,transposeLiers[#[[1]]][[2]]&/@inlierHPairs22],2]}
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@inlierHPairs[[;;,2]],2]
Magnify[anaglyph@{ImagePerspectiveTransformation[i1,#,DataRange->Full],i2}&/@inlierHPairs22[[;;,2]],2]


(*PCA to determine motion direction?*)
(*ListLinePlot@Transpose[SingularValueDecomposition[With[{mean=Mean[#]},#-mean&/@#]][[3,;;,1]]&/@Partition[Import["/s/"<>case<>"/world_accel.csv"][[;;,{8,9,10}]],10]]*)


{r21,t,n}=decomposeHomography[homog,coord,ikm];homog2=#/#[[-1,-1]]&[Inverse[coord.ikm].(r21+Outer[Times,t,n]).coord.ikm];MatrixForm/@{homog,homog2}
reprojectionErrorHomography[#,matches]&/@{homog,homog2}
Magnify[#,2]&@anaglyph@{ImagePerspectiveTransformation[annotateImageWithPoints[i1,matches[[1]]],#,DataRange->Full],annotateImageWithPoints[i2,matches[[2]]]}&/@{homog,homog2}


ransacFindLine2DNormal=Function[{pointsIn,minNumXy,goodNumXy,torrerance,costF,maxIter}
			,Module[{fitModelF,enlargeModelF,globalCostF,nullModel={0,0},x},
		fitModelF=Function[points,(#[[;;-2]]/#[[-1]])&@SingularValueDecomposition[Append[#,1]&/@points][[3,;;,-1]]];
		(*globalCostF=Function[{model,points},-Length@Select[points,costF[model,#]<torrerance&]];*)
		globalCostF=Function[{model,points},Total[costF[model,#]&/@points]];
		ransacDriver[pointsIn,fitModelF,costF,globalCostF,nullModel,torrerance,minNumXy,goodNumXy,maxIter]
	]];
fitModelF=Function[points,(#[[;;-2]]/#[[-1]])&@SingularValueDecomposition[Append[#,1]&/@points][[3,;;,-1]]];
n=2;\[Sigma]=0.1;SeedRandom[1003];inliers={.5,0}+#&/@(#/Max[Norm/@#])&@findOrthogonals[RandomReal[1,n],100,True];as=fitModelF[inliers];noisyInliers=#+RandomReal[\[Sigma]]&/@inliers;
points=Join[RandomReal[{-1,1},{200,n}],noisyInliers];
costF=Function[{model,point},Abs[Append[point,1].Append[model,1]/Norm[model]]];
Graphics[Point@#,Axes->True]&/@{inliers,noisyInliers}
as2=ransacFindLine2DNormal[points,30,80,0.3,costF,100];
{{as,as2},Graphics[{Point@points,Blue,Point[RandomSample[noisyInliers,10]](*,Red,Point[findOrthogonals[as2,100,True]]*)}]}
Graphics[{Opacity[0.5],Point@findOrthogonals[as,30,True],Red,Point@findOrthogonals[as2,30,True]},Axes->True]


(*fitModelF=Function[points,LeastSquares[points,Table[1,{Length@points}]]];*)
n=3;\[Sigma]=0.05;SeedRandom[1003];inliers={.5,-0.3,0}+#&/@(#/Max[Norm/@#])&@findOrthogonals[RandomReal[1,n],100,True];
as=fitPlane[inliers];noisyInliers=#+RandomReal[\[Sigma]{-1,1}]&/@inliers;
points=Join[RandomReal[{-1,1},{200,n}],noisyInliers];
costF=Function[{model,point},Abs[point.model-1]/Norm@model];
Graphics3D[Point@#,Axes->True]&/@{inliers,noisyInliers,points}
ransacFindPlane=Function[{pointsIn,minNumXy,goodNumXy,torrerance,costF,maxIter}
			,Module[{enlargeModelF,globalCostF,nullModel={0,0,0},x},
		globalCostF=Function[{model,points},-Length@Select[points,costF[model,#]<torrerance&]];
		(*globalCostF=Function[{model,points},Total[costF[model,#]&/@points]];*)
		ransacDriver[pointsIn,fitPlane,costF,globalCostF,nullModel,torrerance,minNumXy,goodNumXy,maxIter]
	]];
as2=ransacFindPlane[points,3,100,0.1,costF,300];
{{as,as2},Graphics3D[{Point@points,Blue,Point[RandomSample[noisyInliers,10]](*,Red,Point[findOrthogonals[as2,100,True]]*)}]}
Graphics3D[{Opacity[0.5],Point@findOrthogonals[as,30,True],Red,Point@findOrthogonals[as2,30,True]},Axes->True]


lines=Select[ImageLines[EdgeDetect@i1,"Segmented"->True],Norm[#[[-1]]-#[[1]]]>200&];EdgeDetect@i1
Show[i1,Graphics[{Red,Line/@lines}]]


{smallRgbM,grayM,smallGrayM}=ImageData/@{smallRgb,gray,smallGray};
m=Table[Module[{candids=Outer[List,Range[scale i-scale+1,scale i],Range[scale j-scale+1,scale j]],best},
	best=(Join@@candids)[[First@Ordering[Join@@candids,1,Norm[grayM[[#[[1]],#[[2]]]]-smallGrayM[[i,j]]]&]]];
	Map[If[#==best,smallRgbM[[i,j]],grayM[[#[[1]],#[[2]]]]{1,1,1}]&,candids,{2}]]
	,{i,ImageDimensions[smallRgb][[2]]},{j,ImageDimensions[smallRgb][[1]]}];
Magnify[#,3]&@ImageAssemble@Map[Image,m,{2}]
(*Table[{grayM[[i,j]],grayM[[i,j]],grayM[[i,j]]}
	,{i,ImageDimensions[rgb][[2]]},{j,ImageDimensions[rgb][[1]]}]*)


ms=ImageData/@Rest@ColorSeparate[smallRgb,"LAB"];
(*ms[[1]][[;;10,;;10]]*)
mat=Prepend[Function[m,assemble@Table[Partition[PadRight[{m[[i,j]]},scale^2],scale],{i,ImageDimensions[smallRgb][[2]]},{j,ImageDimensions[smallRgb][[1]]}]]/@ms
	,ImageData@gray];mat//Dimensions


\[CapitalOmega]3=SparseArray@N@With[{\[CapitalOmega]1=assemble@Table[Partition[ReplacePart[Table[0,{scale^2}],RandomInteger[{1,scale^2}]->1],scale]
	,{i,ImageDimensions[smallRgb][[2]]},{j,ImageDimensions[smallRgb][[1]]}]},{\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1}];
LS3=RpcaColorTensor[ImageData@gray,(ImageData/@ColorSeparate[ImageResize[smallRgb,ImageDimensions@rgb]]) \[CapitalOmega]3,\[CapitalOmega]3,100];
Magnify[showTensor3/@LS3,2]


\[CapitalOmega]2=SparseArray@N@Table[assemble@Table[Partition[ReplacePart[Table[0,{scale^2}],RandomInteger[{1,scale^2}]->1],scale]
	,{i,ImageDimensions[smallRgb][[2]]},{j,ImageDimensions[smallRgb][[1]]}],{3}];
LS2=RpcaColorTensor[ImageData@gray,(ImageData/@ColorSeparate[ImageResize[smallRgb,ImageDimensions@rgb]]) \[CapitalOmega]2,\[CapitalOmega]2,100];
Magnify[showTensor3/@LS2,2]


\[CapitalOmega]1=randomSparseTensor[Dimensions@ImageData@gray,0.05];\[CapitalOmega]={\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1};rgb\[CapitalOmega]=\[CapitalOmega](ImageData/@ColorSeparate[rgb]);
With[{scale2= scale},
smallRgb2=ColorCombine[Image/@Transpose[#,{2,3,1}]&@Table[Function[comp,If[#=={},0,Mean@#]&@
		Select[Join@@(comp[[scale2 i-scale2+1;;scale2 i,scale2 j-scale2+1;;scale2 j]]),#!=0&]]/@rgb\[CapitalOmega]
,{i,ImageDimensions[rgb][[2]]/scale2},{j,ImageDimensions[rgb][[1]]/scale2}]]];
smallRgb2
fuseRgbLuminance[smallRgb2,First@ColorSeparate[rgb,"LAB"]]


rgb=Import["t.jpg",ImageSize->400{1,1}];scale=8;smallRgb=ImageResize[rgb,ImageDimensions[rgb]/scale];
{gray,smallGray}=Image@Mean[ImageData/@ColorSeparate[#]]&/@{rgb,smallRgb};
(*{gray,smallGray}=(*ColorConvert[#,"Grayscale"]&*)First@ColorSeparate[#,"LAB"]&/@{rgb,smallRgb};*)
Magnify[#,2]&@{rgb,gray,smallRgb,ImageResize[smallRgb,ImageDimensions[rgb]]}


ms=ImageData/@grays;
MatrixForm/@SingularValueDecomposition[ms[[1]]][[3;;]]
MatrixForm/@SingularValueDecomposition[Join@@{ms[[1]],ms[[1]]}][[3;;]]


sampleHalf=RandomSample[#,Round[Length[#]/2]]&;
ms=ImageData/@grays;(*entropy@*)SingularValueList[#][[;;5]]&/@
	{ms[[1]],ms[[2]],sampleHalf[Join@@{ms[[1]],ms[[2]]}],sampleHalf[Join@@{ms[[1]],ms[[1]]}],sampleHalf[Join@@{ms[[2]],ms[[2]]}]
		,sampleHalf[Join@@{ms[[1]],ms[[3]]}],sampleHalf[Join@@{ms[[2]],ms[[3]]}]}//MatrixForm


colorizeByHistogramTransform=Function[{gray,ref},Module[{limg,lref,aref,bref,l2,radius,neighimg,neighref,nfun,col},
	limg=First@ColorSeparate[gray,"LAB"];{lref,aref,bref}=ColorSeparate[ref,"LAB"];
	l2=HistogramTransform[limg,lref];radius=2;
	{neighimg,neighref}=ColorCombine[{
		MeanFilter[#,radius],
		StandardDeviationFilter[#,radius]}]&/@{l2,lref};
	nfun=Nearest[Flatten[ImageData@neighref,1]->Transpose[Flatten@ImageData@#&/@{aref,bref}]];
	col=Map[First@nfun[#,1]&,ImageData@neighimg,{2}];
	ColorConvert[Image[Join[{ImageData@limg},Transpose[col,{2,3,1}]],Interleaving->False,ColorSpace->"LAB"],"RGB"]
	]];
(*imgs=Import[#,ImageSize->400{1,1}]&/@{"t7.jpg","t8.jpg"};*)
(*imgs=Import[#,ImageSize->400{1,1}]&/@{"t9.jpg","t10.jpg"};*)
imgs=Import[#,ImageSize->400{1,1}]&/@{"t4.jpg","t8.jpg"};
grays=ColorConvert[#,"Gray"]&/@imgs;gray=grays[[1]];
matches=ImageCorrespondingPoints[imgs[[1]],imgs[[2]]];imgDim=ImageDimensions@imgs[[1]];
MapThread[annotateImageWithPoints,{imgs,matches}]
(*colorizeByHistogramTransform[grays[[1]],imgs[[2]]]

scale=20;imgDim=ImageDimensions@imgs[[1]];
grayBlocks=ImagePartition[gray,scale];refBlocks=ImagePartition[imgs[[2]],scale];
positionToBlockId=Function[{position,imgDim,scale},Module[{rc,blockDim},
	rc=xyToRowColumn[position[[1]],position[[2]],imgDim[[2]]];blockDim=Reverse@imgDim/scale;Floor[1+(rc-1)/scale]]];
mapping=Table[pair[[1]]->colorizeByHistogramTransform[grayBlocks[[pair[[1,1]],pair[[1,2]]]],refBlocks[[pair[[2,1]],pair[[2,2]]]]]
	,{pair,Transpose[Map[positionToBlockId[#,imgDim,scale]&,matches,{2}]]}];
bimg1=ImageAssemble@ReplacePart[grayBlocks,mapping]*)


rgbM=ImageData@imgs[[2]];\[CapitalOmega]1=N@SparseArray[xyToRowColumn[#[[1]],#[[2]],imgDim[[2]]]&/@matches[[1]]->1,Reverse@imgDim];\[CapitalOmega]={\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1};
m=ReplacePart[ImageData@ColorConvert[gray,"RGB"],xyToRowColumn[#[[1,1]],#[[1,2]],imgDim[[2]]]->(rgbM[[#[[1]],#[[2]]]]&@xyToRowColumn[#[[2,1]],#[[2,2]],imgDim[[2]]])&
	/@Transpose@matches];
rgb\[CapitalOmega]=Transpose[m,{2,3,1}]\[CapitalOmega];
{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[ImageData@gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.03,30];initS=rgb\[CapitalOmega]-initL;
showTensor3/@{initL,n\[CapitalOmega]}
LS=RpcaColorTensor2WithInit[ImageData@gray,rgb\[CapitalOmega],n\[CapitalOmega],initL,initS,200];showTensor3/@LS


Import@Export["/h/d/colorization/paper_revised/figure/label.pdf",Rasterize[ImageRotate[#,-90Degree]&/@{Binarize@showTensor3[\[CapitalOmega]],showTensor3[LS[[1]]],imgs[[grayIndex]]}]]


twoParts=Function[m,Module[{a,as,f,r},
	as=Array[a,Dimensions@m];
	f[as_?(NumericQ@#[[1,1]]&)]:=schattenNorm[m-as,1]+pnorm[as,1];
	r=NMinimize[f[as],Flatten@as];
	r[[1]]
	]];
Parallelize@Select[Table[ms=RandomReal[1,{2,2,2}];
	twoParts/@{ms[[1]]+ms[[2]],ms[[1]],ms[[2]]},{100}],#[[1]]>#[[2]]+#[[3]]&]


img=ColorConvert[Import["t.jpg",ImageSize->800],"Gray"];
img2=ColorConvert[Rationalize@Show[
	img,Graphics@{Red,Text[Style["abc",Large],ImageDimensions[img]/2]}],"Gray"]


SeedRandom[1003];n=3;m=RandomReal[1,n{1,1}];Clear[a];as=Array[a,n{1,1}];
f[as_?(NumericQ@#[[1,1]]&)]:= 1.2 schattenNorm[as,1]+pnorm[m-as,1];
(*f2[as_?(NumericQ@#[[1,1]]&)]:=-schattenNorm[as,1]+pnorm[m-as,1];*)
(*f2[as_?(NumericQ@#[[1,1]]&)]:=schattenNorm[m-as,1]^2+0.64 pnorm[as,1]^2;*)
f2[as_?(NumericQ@#[[1,1]]&)]:=1. schattenNorm[as,1]^2+pnorm[m-as,1]^2;
r=NMinimize[f[as],Flatten@as];
r2=NMinimize[f2[as](*,variableWithInitial[as,0as]*),Flatten@as];
(*r2=NMinimize[f2[as],Flatten@as];*)
(*r=NMinimize[schattenNorm[as,1]+pnorm[m-as,1],Flatten@as];//AbsoluteTiming*)
First/@{r,r2}
Function[r,{MatrixForm/@{#,m-#}&[as/.r[[2]]],SingularValueList/@{m,as/.r[[2]]}}]/@{r,r2}


SeedRandom[1003];Clear[a,b];n=7;as=Array[a,n{1,1}];bs=Array[b,n{1,1}];m=RandomReal[1,n{1,1}];
(*g=Function[{as,bs},MatrixExp[as-as\[Transpose]].m.MatrixExp[bs-bs\[Transpose]]];*)
g=Function[{as,bs},MatrixExp[lowerTriangle@as-Transpose@lowerTriangle@as].m
	.MatrixExp[lowerTriangle@bs-Transpose@lowerTriangle@bs]];
f[as_?(NumberQ@#[[1,1]]&),bs_]:=pnorm[g[as,bs],1]
(*r=NMinimize[f[as,bs],Flatten@{as,bs}];*)
r=FindMinimum[f[as,bs],
	Select[Join@@(variableWithInitial[#,0#]&/@{as,bs}),#[[1]]=!=0&]
	(*,MaxIterations->300*)];//AbsoluteTiming
g[as/.r[[2]],bs/.r[[2]]]//Chop//Abs//MatrixForm
r2=FindMinimum[f[as,bs],
	Select[Join@@(variableWithInitial[#,0#]&/@{as,bs}),#[[1]]=!=0&]
	,Method->"QuasiNewton"
	,Gradient:>{"FiniteDifference", "DifferenceOrder"->1}];//AbsoluteTiming
g[as/.r2[[2]],bs/.r2[[2]]]//Chop//Abs//MatrixForm
SingularValueList@m


(*Sqrt[(||D-X||_*^2+lambda^2 ||X||_ 1^2)(1+1/lambda^2)] >=||D-X||_*+||X||_ 1>=||D-X||_*+||X||_*>=||D||_*
||D-X||_* = lambda^2 ||X||_ 1*)
(*Import@Export["t.png",ImageCrop@GraphicsRow[Reverse[Import/@FileNames["/tmp/*.png"]],-120,ImageSize->800]]*)


{m,n}={3,5};SeedRandom[1003];Clear[a];as=Array[a,{m,n}];
A={Array[1&,m]};xs=PseudoInverse[A].{Array[1&,n]}+(IdentityMatrix@m-PseudoInverse[A].A).as;(*3xs//Simplify//MatrixForm*)
f[xs_?(NumericQ@#[[1,1]]&)]:=(*schattenNorm[xs,1];*)(*pnorm[xs,1];*)pnorm2[Diagonal[xs.Transpose[xs]]-1,3]
r=NMinimize[(*pnorm[xs,0.3]*)f[xs],Flatten@as];
xs/.r[[2]]//MatrixForm
MatrixForm/@SingularValueDecomposition[xs/.r[[2]]]


MatrixForm/@{rtnss[[2,1,1]].rtnss[[1,1,1]],rtnss[[3,1,1]]}
MatrixForm/@{rtnss[[2,2,1]].rtnss[[1,2,1]],rtnss[[3,2,1]]}
MatrixForm/@{rtnss[[2,1,1]].rtnss[[1,2,1]]}
MatrixForm/@{rtnss[[2,2,1]].rtnss[[1,1,1]]}

Table[Det@{Transpose[rtnss[[1,i,1]]].rtnss[[1,i,2]],rtnss[[3,i,1]].rtnss[[2,i,2]],rtnss[[3,i,2]]},{i,2}]


(*normalizeCalibratedHomography[ikm.hss[[6]].hss[[2]].Inverse[ikm]]
normalizeCalibratedHomography[ikm.hss[[3]].Inverse[ikm]]*)
rtnss={decomposeHomography[#,ikm]&@hss[[2]],decomposeHomography[#,ikm]&@hss[[6]],decomposeHomography[#,ikm]&@hss[[3]]};
Map[dispRtn,rtnss,{2}]


imgs=Import[#,ImageSize->{400}]&/@(*Take[FileNames["/s/df_golden/fountain/*.JPG"],8];*){"/h/t2.jpg","/h/t4.jpg","/h/t7.jpg","/h/t8.jpg"}
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
(*MapThread[annotateImageWithPoints,{imgs,ImageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"]}]*)
pairs=Join@@Table[{i,j},{i,Length@imgs},{j,i+1,Length@imgs}]
matchGroups=Parallelize@Table[imageCorrespondingPoints[imgs[[pair[[1]]]],imgs[[pair[[2]]]],"Transformation"->"Perspective"],{pair,pairs}];
hss=Parallelize[homographyFromMatchesL1@#&/@matchGroups];
(*Import@Export["/g/tmp/homographies.csv",Table[Flatten@{pairs[[i]]-1,Transpose@hss[[i]]},{i,Length@hss}]]*)
Parallelize@Table[homog=homographyFromMatchesL1@matchGroups[[i]];
	anaglyph@{ImagePerspectiveTransformation[imgs[[pairs[[i,1]]]],homog,DataRange->Full],imgs[[pairs[[i,2]]]]},{i,Length@pairs}]
(*The two solutions of decomposeHomography are one with much rotaiton and one with much translation.
For 3-view, the much rotational ones are consistent with each other, and much translation ones are consistent with each other, respectively.
However, only one solution will have a stable plane normal across homographies.
Outer[{MatrixForm@#[[1]],MatrixForm@#2[[1]],MatrixForm[#2[[1]].#[[1]]]}&,decomposeHomography[hss[[1]],ikm],decomposeHomography[hss[[4]],ikm],1]
MatrixForm/@decomposeHomography[hss[[2]],ikm][[;;,1]]
imgs[[{1,2}]]
{ImagePerspectiveTransformation[imgs[[1]],hss[[1]],DataRange->Full,PlotRange->All],imgs[[2]]}
{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikm].decomposeHomography[hss[[1]],ikm][[1,1]].ikm,DataRange->Full,PlotRange->All],imgs[[2]]}
{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikm].decomposeHomography[hss[[1]],ikm][[2,1]].ikm,DataRange->Full,PlotRange->All],imgs[[2]]}
*)


SeedRandom[1003];Clear[a,t,n];ass=Array[a,{Length@imgs,3,3}];tss=Array[t,{Length@imgs,3}];nss=Array[n,{Length@pairs,3}];
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;cnt=0;
Total@Table[reprojectionErrorHomography[hss[[i]],matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,Length@imgs}]
nhss=Table[#/SingularValueList[#][[2]]&[ikms[[pairs[[i,2]]]].hss[[i]].Inverse[ikms[[pairs[[i,1]]]]]],{i,Length@hss}];
(*nhss=#/SingularValueList[#][[2]]&@(ikm.#.Inverse[ikm])&/@hss;*)
(*g=Function[{ass,tss,nss},Module[{ra,rb},{ra,rb}=MatrixExp[#-Transpose[#]&@lowerTriangle@#]&/@ass;
	{ra+Outer[Times,tss[[1]],nss[[1]]],rb+Outer[Times,tss[[2]],nss[[2]]],rb.ra+Outer[Times,rb.tss[[1]]+tss[[2]],nss[[3]]]}]];*)
g=Function[{ass,tss,nss},
	Table[With[{rs=MatrixExp[#-Transpose[#]&@lowerTriangle@#]&@ass[[#]]&/@pairs[[i]]},
		Transpose[rs[[2]]].(IdentityMatrix[3]+Outer[Times,tss[[pairs[[i,2]]]]-tss[[pairs[[i,1]]]],nss[[i]]]).rs[[1]]],{i,Length@hss}]
];
f[ass_?(NumericQ@#[[1,1,1]]&),tss_,nss_]:=Module[{hss2},hss2=g[ass,tss,nss];cost=pnorm2[nhss-hss2,2]];
(*this seem hard to optimize
f[ass_?(NumericQ@#[[1,1,1]]&),tss_,nss_]:=Module[{hss2,res},
	hss2=g[ass,tss,nss];
	res=Total@Table[reprojectionErrorHomography[Inverse[ikm].hss2[[i]].ikm,matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,3}];
	(*cnt++;If[Mod[cnt,30]==1,Print[res]];*)res
	];*)
Dynamic[cost]
r=FindMinimum[f[ass,tss,nss],Flatten@{ass,tss,nss},MaxIterations->500];r[[1]]
(*hss2=Inverse[ikm].#.ikm&/@(g@@({ass,tss,nss}/.r[[2]]))*)
hss2=With[{prehss=g@@({ass,tss,nss}/.r[[2]])},Table[Inverse[ikms[[pairs[[i,2]]]]].prehss[[i]].ikms[[pairs[[i,1]]]],{i,Length@prehss}]];
MatrixForm[#/#[[-1,-1]]]&/@hss
MatrixForm[#/#[[-1,-1]]]&/@hss2
Table[reprojectionErrorHomography[hss[[i]],matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,3}]
Table[reprojectionErrorHomography[hss2[[i]],matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,3}]
Table[homog=hss[[i]];anaglyph@{ImagePerspectiveTransformation[imgs[[pairs[[i,1]]]],homog,DataRange->Full],imgs[[pairs[[i,2]]]]},{i,Min[15,Length@pairs]}]
Table[homog=hss2[[i]];anaglyph@{ImagePerspectiveTransformation[imgs[[pairs[[i,1]]]],homog,DataRange->Full],imgs[[pairs[[i,2]]]]},{i,Min[15,Length@pairs]}]
{Graphics3D@Point[tss/.r[[2]]],imgs}
Graphics3D@BlendLine[tss/.r[[2]]]
rss=matrixExpSpecialOrthogonal@#&/@(ass/.r[[2]]);MatrixForm[Inverse[rss[[1]]].#]&/@rss[[2;;]]


case="thu";rawModels=SplitBy[Import["/g/tmp/"<>case<>"_model_docid.csv"][[;;,2;;-2]],#[[1]]&];
models={"position"->#[[3+9;;3+9+3-1]],"bw_rotation"->quaternionConjugate@quaternionFromRotationMatrix[#[[3;;3+9-1]]],"id"->#[[1]],"docid"->#[[2]]}&/@#&/@rawModels;
docidToModel=Flatten[("docid"/.#)->("id"/.#)+1&/@#&/@models];(*models[[-1]]*)
(*Graphics3D[drawSfmModel@#,Axes->True,AxesLabel->{"x","y","z"},ImageSize->300]&/@models*)


case="thu";rules="0:"<>FileBaseName[#]->Import[#,ImageSize->{400}]&/@FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];
twoviews=randomSampleAtMostN[Import["/g/tmp/"<>case<>"_twoviews.csv"][[;;,2;;-2]],30];(*twoview=twoviews[[1]]*)
twoviewEvals=(Function[twoview,imgs=twoview[[;;2]]/.rules;
	{twoview[[;;2]],
		With[{imgPair={ImageResize[ImageReflect@ImagePerspectiveTransformation[ImageReflect@ImageResize[imgs[[1]],{1600}],Partition[twoview[[3;;3+9-1]],3]
				,DataRange->Full],400]
			,imgs[[2]]}},{correlationError@imgPair,anaglyph@imgPair,imgPair,twoview[[;;2]]/.docidToModel}]}]/@twoviews);
docPairCalibratedH=Select[#[[;;2]]->#[[3+9;;3+9+9-1]]&/@twoviews,NumericQ@#[[2,1]]&];docPairCalibratedH[[1]]
selectedDocidPairs=Select[twoviewEvals,#[[2,1]]<0.7&][[;;,1]];
selecteddDocPairCalibratedH=Select[docPairCalibratedH,MemberQ[selectedDocidPairs,#[[1]]]&];


inlierRules=#[[{2,4}]]->{#[[{6,8}]],N@#[[8]]/#[[6]]}&/@Import["/g/tmp/h_inliers.csv"];
{#[[1]]/.inlierRules,#}&/@Select[twoviewEvals,#[[2,1]]<0.7&]
{#[[1]]/.inlierRules,#}&/@Select[twoviewEvals,#[[2,1]]>0.7&]


Select[twoviewEvals,#[[2,1]]<0.7&]


SeedRandom[1003];Clear[a,t,n,s,f,g];ass=Array[a,{Length@models,3,3}];tss=Array[t,{Length@models,3}];ss=Array[s,{Length@models}];
nss=Array[n,{Length@selecteddDocPairCalibratedH,3}];
g[ass_,tss_,nss_,ss_]:=Module[{docidToRts},
	docidToRts=Flatten@Table[("docid"/.#)->{matrixExpSpecialOrthogonal[ass[[i]]].quaternionToRotationMatrix["bw_rotation"/.#],ss[[i]]("position"/.#)+tss[[i]]}&/@models[[i]]
		,{i,Length@models}];
	Table[With[{rt1=(#[[1,1]]/.docidToRts),rt2=(#[[1,2]]/.docidToRts)},
		Transpose[rt2[[1]]].(IdentityMatrix[3]+Outer[Times,rt2[[2]]-rt1[[2]],nss[[i]]]).rt1[[1]]]&@selecteddDocPairCalibratedH[[i]]
	,{i,Length@selecteddDocPairCalibratedH}]
	]
(*g[RandomReal[1,Dimensions@ass],RandomReal[1,Dimensions@tss],RandomReal[1,Dimensions@nss]]*)
nhss=#/SingularValueList[#][[2]]&@Partition[#,3]&/@selecteddDocPairCalibratedH[[;;,2]];
f[ass_?(NumericQ@#[[1,1,1]]&),tss_,nss_,ss_]:=cost=pnorm2[nhss-g[ass,tss,nss,ss],2]
inits=Join@@{variableWithInitial[ass,0ass],variableWithInitial[tss,0tss],variableWithInitial[nss,{1.,0,0}&/@nss],variableWithInitial[ss,1.&/@ss]};
{r=FindMinimum[f[ass,tss,nss,ss],inits];//AbsoluteTiming,r[[1]]}


imgs=Import[#,ImageSize->{400}]&/@(*{"/h/t.jpg","/h/t3.jpg"};*){"/h/t4.jpg","/h/t7.jpg"};(*{"/h/t9.jpg","/h/t10.jpg"};*)
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
(*epiMatches=Transpose@randomSampleAtMostN[Transpose@imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"],100];
MapThread[annotateImageWithPoints,{imgs,epiMatches}]
allMatches=Transpose@randomSampleAtMostN[Transpose@imageCorrespondingPoints[imgs[[1]],imgs[[2]]],300];
MapThread[annotateImageWithPoints,{imgs,allMatches}]*)
matches=Transpose@randomSampleAtMostN[Transpose@imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"],100];
nmatches=ikm.Append[#,1]&/@#&/@matches;
(*matches=Transpose@Delete[Transpose@matches,2];*)
MapThread[annotateImageWithPoints,{imgs,matches}]
homog=homographyFromMatchesL1@matches;(*homog=homographyFromMatchesMultiLp[matches,ikm,5,1];*)
hs=#/SingularValueList[#][[2]]&[ikm.homog.Inverse[ikm]];{"average reproj error",reprojectionErrorHomography[homog,matches]/Length@matches[[1]]}
anaglyph@{ImagePerspectiveTransformation[annotateImageWithPoints[imgs[[1]],matches[[1]]],homog,DataRange->Full]
	,annotateImageWithPoints[imgs[[2]],matches[[2]]]}


SeedRandom[1003];Clear[a,f];p=1;n=2;
m=RandomReal[1,n{1,1}];
(*m=ImageData@ColorConvert[Import["t.jpg",ImageSize->n{1,1}],"Grayscale"];*)
ass=Array[a,{2,n,n}];
f[ass_?(NumericQ@#[[1,1,1]]&)]:=cost=With[{rss=(*MatrixExp*)cayleyTransform[#-#\[Transpose]]&@#&/@ass},pnorm[rss[[1]].m.rss[[2]].(DiagonalMatrix@Range[n]),p]];
Dynamic@cost
{r=FindMinimum[f[ass],Flatten@ass];//AbsoluteTiming,r[[1]],schattenNorm[m,p]}


imgs=ImageRotate[#,-90Degree]&/@Import["/s/df_golden/fountain/*.JPG",ImageSize->{400}]


{i1,i2,i3,i4}=imgs[[{1,2,3,4}]];ikm=Inverse@intrinsicParameterMatrix[i1];
{ha,hb}=ikm.homographyFromMatchesL1[imageCorrespondingPoints[#[[1]],#[[2]],"Transformation"->"Perspective"]].Inverse[ikm]&/@{{i1,i3},{i2,i4}};
rtna=Last@decomposeH@ha;rtnb=Last@decomposeH@hb;


(*PMVS?*)
imgs=Import[#,ImageSize->400]&/@{"t2.jpg","t4.jpg"};imgDim=ImageDimensions@imgs[[1]];
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
homog=homographyFromMatchesL1@matches;hs=ikm.homog.Inverse[ikm];
{imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}}
rtns=decomposeH@hs;rtn=standardizeRtn@rtns[[2]];radius=25;
(*dispRtn/@rtns*)
(*{hs,dispRtn/@rtns,dispRtn@rtn}*)
Manipulate[pts={{w,h},{w+radius,h},{w+radius,h+radius},{w,h+radius},{w,h}};
	homog2=Inverse[ikm].(rtn[[1]]+outer[Norm[rtn[[3]]]rtn[[2]],{nx,ny,Sqrt[1-nx^2-ny^2]}Exp[-logd]]).ikm;
	pts2=homographyTransform[#,homog2]&/@pts;
	{Show[imgs[[1]],Graphics@{Polygon@pts}],Show[imgs[[2]],Graphics@{Polygon[pts2]}]},{w,1,imgDim[[1]]},{h,1,imgDim[[2]]}
		,{{nx,rtn[[3,1]]},-1,1},{{ny,rtn[[3,2]]},-1,1},{{logd,0},-3,3},ContinuousAction->False]


(*It's not reliable in practice to select among decompositions of homography by normal. Selecting by consistency of rotaiton matrix seems better.*)
(*rtn={randomSpecialOrthogonalMatrix[3],RandomReal[1,3],RandomReal[1,3]};
rtn2={randomSpecialOrthogonalMatrix[3],RandomReal[1,3],rtn[[3]]};
hss={rtn[[1]]+outer[rtn[[2]],rtn[[3]]],rtn2[[1]]+outer[rtn2[[2]],rtn2[[3]]]};
dispRtn/@{rtn,rtn2}//TableForm
dispRtn/@Join@@(decomposeH/@hss)//TableForm*)

imgs=Import[#,ImageSize->{400}]&/@{"/h/t2.jpg","/h/t4.jpg","/h/t7.jpg"}
homogs=TransformationMatrix@Last@FindGeometricTransform[#,imgs[[1]],"Transformation"->"Perspective"]&/@imgs[[2;;]];
{anaglyph@{imgs[[2]],ImagePerspectiveTransformation[imgs[[1]],homogs[[1]],DataRange->Full]}
	,anaglyph@{imgs[[3]],ImagePerspectiveTransformation[imgs[[1]],homogs[[2]],DataRange->Full]}}
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
hss=ikm.#.Inverse[ikm]&/@homogs;
dispRtn/@(Join@@(decomposeH/@hss))//TableForm


Parallelize[Function[fnames,Module[{imgs,homogs,ikm,hss,rtns,img,imgHeight},imgs=Import[#,ImageSize->{400}]&/@fnames;
homogs=TransformationMatrix@Last@FindGeometricTransform[#,imgs[[1]],"Transformation"->"Perspective"]&/@imgs[[2;;]];
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];hss=ikm.#.Inverse[ikm]&/@homogs;rtns=standardizeRtn/@decomposeH@hss[[1]];
Function[normal,img=imgs[[1]];imgHeight=ImageDimensions[img][[2]];
	{imgs[[;;2]],normal,Histogram@Log[1+Norm/@#]}&@(Join@@MapIndexed[(#/normal.#&)[ikm.Append[rowColumnToXy[#2[[1]],#2[[2]],imgHeight],1]]&,ImageData@img,{2}])]/@rtns[[;;,3]]
]]/@{{"/h/t11.jpg","/h/t12.jpg"},{"/h/t9.jpg","/h/t10.jpg"},{"/h/t2.jpg","/h/t4.jpg"},{"/h/t7.jpg","/h/t4.jpg"},{"/h/t8.jpg","/h/t4.jpg"}
	,{"/h/forward1.JPG","/h/forward2.JPG"}}]


imageToPointCloud=Function[{img,normal,ikm},Module[{imgHeight},imgHeight=ImageDimensions[img][[2]];
	Join@@MapIndexed[{RGBColor@@#,Point[(#/normal.#&)[ikm.Append[rowColumnToXy[#2[[1]],#2[[2]],imgHeight],1]]]}&,ImageData@img,{2}]]];
(*matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];*)
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];rtns=standardizeRtn/@decomposeH@hss[[1]];imgs[[;;2]]
colorPoints=RandomSample[imageToPointCloud[imgs[[1]],rtns[[1,3]],ikm],3000];
(*Transition by two decompositions of H*)
(*Table[Table[MatrixForm[#/#[[-1,-1]]]&[Re@MatrixExp[t MatrixLog@rtn[[1]]]+outer[t rtn[[2]],rtn[[3]]]]
,{t,0,1,0.1}],{rtn,rtns}]//Transpose//TableForm*)
Manipulate[{Table[anaglyph@{ImagePerspectiveTransformation[imgs[[1]]
			,Inverse[ikm].(Re@MatrixExp[t MatrixLog@rtn[[1]]]+ outer[t rtn[[2]],rtn[[3]]]).ikm,DataRange->Full]
		,imgs[[2]]},{rtn,rtns}],
	Graphics3D[Join[
			Table[drawCameraWithImage[t rtn[[2]]
				,quaternionFromRotationMatrix@(Re@MatrixExp[t MatrixLog@rtn[[1]]]),imgs[[1]]],{rtn,rtns}],
		{drawCameraWithImage[{0,0,0},{1,0,0,0},imgs[[2]]]},{colorPoints}],Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral",ImageSize->600]}
,{{t,1},0,1,0.02},ContinuousAction->False]
(*Graphics3D[{drawCameraWithImage[{0,0,0},{1,0,0,0},imgs[[1]]]
	,drawCameraWithImage[#[[2]],quaternionFromRotationMatrix@#[[1]],imgs[[2]]]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]&@rtns[[1]]
Graphics3D[{drawCameraWithImage[{0,0,0},{1,0,0,0},imgs[[1]]]
	,drawCameraWithImage[#[[2]],quaternionFromRotationMatrix@#[[1]],imgs[[2]]]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]&@rtns[[2]]*)


rs=Parallelize[{{"docid1","docid2","#h_inlier","#f_inlier"}/.#,{"docid1","docid2"}/.#/.rules,evalTwoViewModel[#,rules]}&/@twoviewModels]


evalTwoViewModel=Function[{twoview,rules},Module[{imgs,ikms,tr,homog},
	imgs={"docid1"/.twoview/.rules,"docid2"/.twoview/.rules};ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
	tr=Quiet@FindGeometricTransform[imgs[[2]],imgs[[1]],"Transformation"->"Perspective"];homog=Quiet@TransformationMatrix[tr[[2]]];
	{anaglyphPadBottom[ImageReflect/@{ImagePerspectiveTransformation[ImageReflect@imgs[[1]],Inverse[ikms[[2]]].("calibrated"/.twoview).ikms[[1]]
			,DataRange->Full]
		,ImageReflect@imgs[[2]]}]
	,If[NumericQ@tr[[1]],anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]}
	]];

twoview=twoviewModels[[1]];evalTwoViewModel[twoview,rules]
imgs={"docid1"/.twoview/.rules,"docid2"/.twoview/.rules};ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
tr=FindGeometricTransform[imgs[[2]],imgs[[1]],"Transformation"->"Perspective"];homog=TransformationMatrix[tr[[2]]];
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}
hs=ikms[[2]].homog.Inverse[ikms[[1]]];
MatrixForm/@{"calibrated"/.twoview,hs}


case="street";(*case="hochiminh";*)
lines=Import["/g/tmp/"<>case<>"_homography.csv"][[;;,2;;-2]];{models,rules}=loadSfmModel["street","sfm"];
twoviewModels=Select[If[Length@#>=48,{"homog"->#/#[[-1,-1]]&@unVec[#[[;;9]],3{1,1}],"docid1"->#[[12]],"docid2"->#[[23]]
		,"R1"->unVec[#[[13;;13+9-1]],3{1,1}],"R2"->unVec[#[[24;;24+9-1]],3{1,1}],"calibrated"->#/SingularValueList[#][[2]]&@unVec[#[[35;;35+9-1]],3{1,1}]
		,"parallax"->#[[52]],"#f_inlier"->#[[46]],"#h_inlier"->#[[48]]}]&/@lines
	,#=!=Null&];
(*#[[1]]->MatrixForm@#[[2]]&/@*)(*twoviewModels[[;;7]]*)

(*rs=Parallelize[Function[twoview,(*=twoviewModels[[1]];*)Module[{calibrated,ikms,imgs,tr,homog,hs},
(*Print[{({"R1","R2"}/.model),"calibrated"/.model}];*)
	imgs={"docid1"/.twoview/.rules,"docid2"/.twoview/.rules};ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
	tr=Quiet@FindGeometricTransform[ImageReflect@imgs[[2]],ImageReflect@imgs[[1]],"Transformation"->"Perspective"];
	If[NumericQ@tr[[1]],
		homog=TransformationMatrix[tr[[2]]];hs=ikms[[2]].homog.Inverse[ikms[[1]]]];
	calibrated="calibrated"/.twoview;
	{{"docid1","docid2","#h_inlier","#f_inlier"}/.twoview
		,MatrixForm/@({"R1","R2","parallax"}/.twoview),(*Det[#/SingularValueList[#][[2]]&@calibrated]-1,*)dispRtn/@Quiet@decomposeH[calibrated]//TableForm
			,If[NumberQ@hs[[1,1]],dispRtn/@Quiet@decomposeH[hs]//TableForm]}]]/@twoviewModels[[;;]]];rs//TableForm*)


SplitBy[SortBy[rs,#[[1,1]]&],#[[1,1]]&]


case="street";
clouds={"docid1"->#[[2]],"docid2"->#[[3]],"points"->Partition[#[[4;;-2]],3]}&/@Import["/g/tmp/"<>case<>"_point_3d.csv"];
rejectedClouds={"docid1"->#[[2]],"docid2"->#[[3]],"points"->Partition[#[[4;;-2]],3]}&/@Import["/g/tmp/"<>case<>"_rejected_3d.csv"];
MapIndexed[{#2[[1]],"docid1","docid2"}/.#&,clouds]


cloudIdx=93;Table[{Thread@{{"docid1","docid2"}/.clouds[[cloudIdx]],{"docid1","docid2"}/.clouds[[cloudIdx]]/.rules}
	,Graphics3D[Point["points"/.clouds[[cloudIdx]]],Axes->True,AxesLabel->{"x","y","z"}]
	,Graphics3D[Point["points"/.rejectedClouds[[cloudIdx]]],Axes->True,AxesLabel->{"x","y","z"}]},{cloudIdx,(*Length@clouds*){cloudIdx}}]
dispRtn/@decomposeH["calibrated"/.twoviewModels[[cloudIdx]]]//TableForm


twoview=twoviewModels[[cloudIdx]]
imgs={"docid1","docid2"}/.twoview/.rules
rtns=standardizeRtn/@decomposeH["calibrated"/.twoview];ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
dispRtn/@rtns//TableForm


loadTwoDecomps=Function[case,{"docid1"->#[[2]],"docid2"->#[[3]],"R1"->unVec[#[[4;;4+9-1]],3{1,1}],"std1"->#[[15]],"R2"->unVec[#[[16;;16+9-1]],3{1,1}]
		,"std2"->#[[16+9+2]]}&/@Import["/g/tmp/"<>case<>"_two_decomp.csv"]];
rawGraphFromTwoDecomps=Function[case,Module[{twoDecomps,docidsF},
	twoDecomps=loadTwoDecomps@case;docidsF={"docid1","docid2"}/.#&;
	Graph[Property[DirectedEdge@@{"docid1"/.#,"docid2"/.#},#[[3;;]]]&@First@#&/@SplitBy[SortBy[twoDecomps,docidsF],docidsF]]]];
badEges={};
graphFromTwoDecomps=Function[case,Module[{twoDecomps,docidsF},
	twoDecomps=loadTwoDecomps@case;docidsF={"docid1","docid2"}/.#&;
	Graph[Join@@(Module[{ids={"docid1"/.#,"docid2"/.#},allRstds={{"R1","std1"},{"R2","std2"}}/.#,rstds},
		rstds=Select[allRstds,#[[2]]<300&];
		If[Length@rstds==1,
		{Property[UndirectedEdge@@ids,
			"R"->If[Order@@ids==1,Identity,Transpose][rstds[[1,1]]]
		]},AppendTo[badEges,DirectedEdge@@ids];{}]]&@First@#&/@SplitBy[SortBy[twoDecomps,docidsF],docidsF])]]];
case="street";twoDecomps=loadTwoDecomps@case;g=graphFromTwoDecomps@case;rawG=rawGraphFromTwoDecomps@case;GraphPlot[rawG,VertexLabeling->True,ImageSize->1600]
GraphPlot[g,VertexLabeling->True,ImageSize->1600]
Export["/g/tmp/"<>case<>"_two_decomps_edges.csv",Flatten@{VertexIndex[g,#]-1&/@({"docid1","docid2"}/.#),{vec["R1"/.#],"std1",vec["R2"/.#],"std2"}/.#}&/@twoDecomps]
Export["/g/tmp/"<>case<>"_two_decomps_docids.csv",VertexList[g]]
(*superStepGraph=Function[{g,vertexTransF},Graph[Table[vertexTransF[g,i],{i,Length@VertexList@g}],EdgeList[g]]];*)


SortBy[With[{ass=ass/.r2[[2]]},
Function[edge,Module[{needTranspose=Not[Order[edge[[1]],edge[[2]]]==1]},
	{unitaryMatrixDistance@@#,MatrixForm[Transpose[#[[1]]].#[[2]]],MatrixForm/@#}&@{matrixExpSpecialOrthogonal[ass[[VertexIndex[g,edge[[2]]]]]]
	,If[needTranspose,Transpose,Identity][PropertyValue[{g,edge},"R"]].matrixExpSpecialOrthogonal[ass[[VertexIndex[g,edge[[1]]]]]]}
]]/@EdgeList[g]],First]


SeedRandom[1003];Clear[a];ass=Array[a,{Length@VertexList[g],3,3}];
unitaryMatrixDistance=Function[{m1,m2},pnorm[MatrixLog[Transpose[m1].m2],1]];
f[ass_?(NumericQ@#[[1,1,1]]&)]:=cost=Total[Function[edge,Module[{needTranspose=Not[Order[edge[[1]],edge[[2]]]==1]},
	unitaryMatrixDistance[matrixExpSpecialOrthogonal[ass[[VertexIndex[g,edge[[2]]]]]]
	,If[needTranspose,Transpose,Identity][PropertyValue[{g,edge},"R"]].matrixExpSpecialOrthogonal[ass[[VertexIndex[g,edge[[1]]]]]]]
]]/@EdgeList[g]];
{f[RandomReal[1,Dimensions@ass]],f[MatrixLog@attitude@#&/@VertexList[g]],Dynamic@cost}
(*{r=FindMinimum[f[ass],Flatten@ass];//AbsoluteTiming,r[[1]]}*)
{r2=FindMinimum[f[ass],variableWithInitial[ass,Chop@MatrixLog@attitude@#&/@VertexList[g]],MaxIterations->30];//AbsoluteTiming,r2[[1]]}
(*ass/.r[[2]]*)


(*node=VertexList[g][[rootIdx]];*)node="0:R_Baw";
outEdges=DirectedEdge[node,#]&/@Complement[VertexOutComponent[rawG,node,1],{node}];
Table[{{edge,#->MatrixForm@PropertyValue[{rawG,edge},#]&/@PropertyList[{rawG,edge}]},MatrixForm[attitude[edge[[2]]].Transpose[attitude[edge[[1]]]]]}
	,{edge,badEges}
	(*,{edge,outEdges}*)
	]
{attitude[node],{#,attitude[#]//MatrixForm}&/@outEdges[[;;,2]]}


root=VertexList[g][[rootIdx]];Select[twoDecomps,("docid1"/.#)==root||("docid2"/.#)==root&]


Clear[attitude];rootIdx=First@Ordering[VertexDegree[g],-1];attitude[VertexList[g][[rootIdx]]]=IdentityMatrix[3];DownValues@attitude
queryEdge=Function[edgeIn,
	Function[edge,{edge,#->MatrixForm@PropertyValue[{rawG,edge},#]&/@PropertyList[{rawG,edge}]}]/@{DirectedEdge@@edgeIn,DirectedEdge[edgeIn[[2]],edgeIn[[1]]]}];
BreadthFirstScan[g,VertexList[g][[rootIdx]],{"FrontierEdge"->Function[edge,Module[{needTranspose=Not[Order[edge[[1]],edge[[2]]]==1]},
(*	rstds=Select[{{PropertyValue[{g,edge},"R1"],PropertyValue[{g,edge},"std1"]},{PropertyValue[{g,edge},"R2"],PropertyValue[{g,edge},"std2"]}},#[[2]]<100&];
	Print[{edge,Length@rstds}];*)
	(*Print[{"query",queryEdge@edge}];*)
	(*Print[{"scan",edge,MatrixForm@If[needTranspose,Transpose,Identity][PropertyValue[{g,edge},"R"]]}];*)
	If[Head[attitude[edge[[2]]]]===attitude,
		attitude[edge[[2]]]=If[needTranspose,Transpose,Identity][PropertyValue[{g,edge},"R"]].attitude[edge[[1]]]];
	If[Head[attitude[edge[[1]]]]===attitude,
		attitude[edge[[1]]]=Transpose[If[needTranspose,Transpose,Identity][PropertyValue[{g,edge},"R"]]].attitude[edge[[2]]]];
	Print[{Order@@edge,needTranspose,edge,MatrixForm@attitude@#&/@edge}];
(*	If[Length@rstds==1(*&&Head[attitude[edge[[1]]]]=!=attitude*),(*Print[{rstds[[1]],edge}];*)
		attitude[edge[[2]]]=If[needTranspose,Transpose,Identity][rstds[[1,1]]].attitude[edge[[1]]];];*)
	]]}]
Length@VertexList@g
(*{Length@#,#}&@DownValues[attitude]*)
{#,MatrixForm@attitude@#}&/@VertexList@g//TableForm


(*g2=superStepGraph[g,Function[{g,i},Module[{v=VertexList[g][[i]]},
	If[i==1,Property[
	]]];
GraphPlot[g2,VertexLabeling->True,ImageSize->1600]*)


i=1;pair=pairs[[i]];anaglyph@{ImagePerspectiveTransformation[imgs[[pair[[1]]]],homogs[[i]],DataRange->Full],imgs[[pair[[2]]]]}
anaglyph@{ImagePerspectiveTransformation[
	Radon[#,ImageDimensions@#]&@imgs[[pair[[1]]]],Transpose@homogs[[i]],DataRange->Full],Radon[#,ImageDimensions@#]&@imgs[[pair[[2]]]]}
ImagePerspectiveTransformation[
	Radon[#,ImageDimensions@#]&@imgs[[pair[[1]]]],Inverse[ikms[[2]]].Transpose[ikms[[2]].homogs[[i]].Inverse[ikms[[1]]]].ikms[[1]],DataRange->Full]
Radon[#,ImageDimensions@#]&@imgs[[pair[[1]]]]
Radon[#,ImageDimensions@#]&@imgs[[pair[[2]]]]


buildTwoViewModel[Import[#,ImageSize->{400}]&/@("/s/df/ttsunami.t/unified_viewer/"<>#<>".JPG"&/@{"61NGvDsVyP0AAAAAASEqdA","770Ov4QHYj4AAAAAASEqcw"})]


(*triangleFromDirections=Function[ts,Module[{k,costF},costF=Norm[ts[[1]]+# ts[[2]]-Norm[ts[[1]]+# ts[[2]]]Normalize[ts[[3]]]]&;
	k=N@Norm[Cross[ts[[1]],ts[[3]]]]/(10^-20+Norm[Cross[ts[[2]],ts[[3]]]]);k=If[costF[-k]<costF[k],-k,k];(*Print[k];*)
	(*Print[{k,ts[[1]]+k ts[[2]],ts[[1]]-k ts[[2]],costF[k],costF[-k],Norm[ts[[1]]-k ts[[2]]]Normalize[ts[[3]]]}];*)
	{{0,0,0},ts[[1]],ts[[1]]+k ts[[2]]}]];(*triangleFromDirections@{{1,0,0},{-1,-1,0},{0,1,0}}*)*)
imgs=Import[#,ImageSize->{400}]&/@(*{"/h/t4.jpg","/h/t7.jpg","/h/t8.jpg"};*)FileNames["/h/d/cbk*.jpeg"];
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;pairs={{1,2},{2,3},{3,1}};
(*matchGroups=Table[imageCorrespondingPoints[imgs[[pair[[1]]]],imgs[[pair[[2]]]],"Transformation"->"Perspective"],{pair,pairs}];*)
matchGroups=threeViewTrack@imgs;
homogs=homographyFromMatches/@matchGroups;
(*homogs=Table[TransformationMatrix@Last@findGeometricTransform[imgs[[pair[[2]]]],imgs[[pair[[1]]]],"Transformation"->"Perspective"],{pair,pairs}];*)
anaglyphs=Table[With[{pair=pairs[[i]]},
	anaglyph@{ImagePerspectiveTransformation[imgs[[pair[[1]]]],homogs[[i]],DataRange->Full],imgs[[pair[[2]]]]}],{i,Length@pairs}];
Print@Join[imgs,anaglyphs];
Print[Join@@Table[MapThread[annotateImageWithPoints,{imgs[[pairs[[i]]]],matchGroups[[i]]}],{i,Length@pairs}]];
hss=#/SingularValueList[#][[2]]&/@Table[With[{pair=pairs[[i]]},ikms[[pair[[2]]]].homogs[[i]].Inverse[ikms[[pair[[1]]]]]],{i,Length@pairs}];
allRtnss=standardizeRtn/@decomposeH@#&/@hss;
rtnss=Table[Select[allRtnss[[i]],(pointSpread[#,matchGroups[[i,1]],ikms[[pairs[[i,1]]]]]<0.9)&],{i,Length@allRtnss}];
tuples=SortBy[Table[rtns=Table[rtnss[[i,tuple[[i]]]],{i,3}];ts=Normalize/@rtns[[;;,2]];
	{tuple,Norm@Rest@quaternionFromRotationMatrix[rtns[[1,1]].rtns[[2,1]].rtns[[3,1]]],
	Cross[Transpose[rtns[[1,1]]].ts[[1]],ts[[3]]].rtns[[3,1]].ts[[2]]},{tuple,Tuples[Range[1,Length@#]&/@rtnss]}],Abs[#[[2]]#[[3]]]&];Print[tuples];
rtns=With[{tuple=tuples[[2,1]]},Table[rtnss[[i,tuple[[i]]]],{i,3}]];Print[dispRtn/@rtns//TableForm];
(*vs=-triangleFromDirections[{Transpose[rtns[[1,1]]].rtns[[1,2]],Transpose[rtns[[1,1]]].Transpose[rtns[[2,1]]].rtns[[2,2]],rtns[[3,2]]}];
qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtns[[1,1]]],rtns[[3,1]]};
pointGroups={(#/(#.rtns[[1,3]]))&[ikms[[1]].Append[#,1]]&/@matchGroups[[1,1]],
	(Transpose[rtns[[1,1]]].#+vs[[2]])&[(#/(#.rtns[[2,3]]))]&[ikms[[2]].Append[#,1]]&/@matchGroups[[2,1]],
	(rtns[[3,1]].#+vs[[3]])&[(#/(#.rtns[[3,3]]))]&[ikms[[3]].Append[#,1]]&/@matchGroups[[3,1]]
	};
Graphics3D[{Riffle[{Blue,Red,Darker@Yellow},Point/@pointGroups]
	,Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,3}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]*)


anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].hss[[1]].ikms[[1]],DataRange->Full],imgs[[2]]}
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[3]]].Inverse[hss[[3]]].ikms[[1]],DataRange->Full],imgs[[3]]}


imgs
dispRtn/@decomposeH@hss[[1]]//TableForm
dispRtn/@decomposeH@Inverse[hss[[3]]]//TableForm
MatrixForm/@SingularValueDecomposition[hss[[1]]-hss[[3]]]


Magnify[anaglyph@{annotateImageWithPoints[imgs[[2]],matchGroups[[1,2]]],annotateImageWithPoints[imgs[[2]],matchGroups[[2,1]]]},2]
Magnify[anaglyph@{annotateImageWithPoints[imgs[[3]],matchGroups[[2,2]]],annotateImageWithPoints[imgs[[3]],matchGroups[[3,1]]]},2]


points=(#/(#.rtns[[1,3]]))&[ikms[[1]].Append[#,1]]&/@matchGroups[[1,1]];
Table[Graphics[Riffle[{Blue,Red,Darker@Yellow},
	Point/@{(#[[;;-2]]/#[[-1]]&[Inverse[ikms[[i]]].Transpose@quaternionToRotationMatrix[qs[[i]]].(#-vs[[i]])]&/@points),matchGroups[[i,1]]}]],{i,{2,3}}]


pointGroups={(#/(#.rtns[[1,3]]))&[ikms[[1]].Append[#,1]]&/@matchGroups[[1,1]],
	(Transpose[rtns[[1,1]]].#+vs[[2]])&[(#/(#.rtns[[2,3]]))]&[ikms[[2]].Append[#,1]]&/@matchGroups[[2,1]],
	(rtns[[3,1]].#+vs[[3]])&[(#/(#.rtns[[3,3]]))]&[ikms[[3]].Append[#,1]]&/@matchGroups[[3,1]]
	};
#[[;;-2]]/#[[-1]]&@(*RandomSample[#]*)First@#&/@pointGroups
#[[;;-2]]/#[[-1]]&@(*RandomSample[#]*)Last@#&/@pointGroups


(*Dimensions/@#&/@NestList[iter,{Transpose@allMatches,Transpose@allMatches},3]*)
rs=With[{matches=Transpose@#},
	(*homog=homographyFromMatches@matches;Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];*)
	matches
	]&/@NestWhileList[iter,{Transpose@allMatches,Transpose@allMatches},Length[#[[1]]]>5&][[2;;-2,1]];
With[{matches=Transpose[Join@@(Transpose/@rs[[;;2]])]},
	homog=homographyFromMatches@matches;Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];
	Print[MapThread[annotateImageWithPoints,{imgs,matches}]]];
(*iter@{{{},{}},Transpose@allMatches}*)


imgs=Import[#,ImageSize->{400}]&/@{"/h/d/cbk2.jpeg","/h/d/cbk.jpeg"}(*{"/h/forward1.jpg","/h/forward2.jpg"}*)(*{"/h/t7.jpg","/h/t4.jpg"}*)
buildTwoViewModel@imgs
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
allMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];

filteredMatches=filterMatchesByHomographyCluster[allMatches];
Print[{"filteredMatches",MapThread[annotateImageWithPoints,{imgs,filteredMatches}]}];
fm=fundamentalMatrixFromMatches@filteredMatches;visualizeFmatrix[fm,imgs[[2]],filteredMatches]

matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];homog=homographyFromMatches@matches;
Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];
Print[{"matches",MapThread[annotateImageWithPoints,{imgs,matches}]}];
hs=ikms[[2]].homog.Inverse[ikms[[1]]];rtns=standardizeRtn/@decomposeH@hs;Print[dispRtn/@rtns//TableForm];
{"filteredMatches","rtns",Table[visualizeFmatrix[Transpose[ikms[[2]]].skewOmega[rtn[[2]]].rtn[[1]].ikms[[1]],imgs[[2]],filteredMatches],{rtn,rtns}]}
{"allMatches","rtns",Table[visualizeFmatrix[Transpose[ikms[[2]]].skewOmega[rtn[[2]]].rtn[[1]].ikms[[1]],imgs[[2]],allMatches],{rtn,rtns}]}
Print[{"allMatches",MapThread[annotateImageWithPoints,{imgs,allMatches}]}];

(*matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];*)
(*matches=filterMatchesByHomography[allMatches,3];
homog=homographyFromMatches@matches;
Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];
Print[MapThread[annotateImageWithPoints,{imgs,matches}]];*)


(*isRtnBetter=Function[{rtns,matchSrc,ikm},
	pointSpread[rtns[[1]],matchSrc,ikm]
	]*)


ikms=gikms;rtn=grtns[[1]];fm=Transpose[ikms[[2]]].skewOmega[rtn[[2]]].rtn[[1]].ikms[[1]];
fFiltered=Transpose@Complement[Select[Transpose@allMatches,Abs[Append[#[[2]],1].fm.Append[#[[1]],1]]<0.01&],Transpose@gmatches];
(*matches2=filterMatchesByHomography[fFiltered,2];*)
matches2=Transpose[Transpose[allMatches][[{80,2,6,14,8,32}]]];
Print[MapThread[annotateImageWithPoints,{imgs,#}]]&/@{allMatches,matches2};
homog2=homographyFromMatches@matches2;
visualizeFmatrix[fm,imgs[[2]],#]&/@{gmatches,allMatches,fFiltered,matches2}
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog2,DataRange->Full],imgs[[2]]}


(*Forward motions is also hard*)
imgs=Import[#,ImageSize->{400}]&/@{"/s/df_golden/desk/EqQVNi5W-aQAAAAAAR92AQ.JPG","/s/df_golden/desk/fqNnvCe3-f4AAAAAAR91_w.JPG"};
	(*{"/s/df_golden/desk/PCxPSRryfVUAAAAAAR-vvQ.JPG","/s/df_golden/desk/fqNnvCe3-f4AAAAAAR91_w.JPG"};*)
allMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];
MapThread[annotateImageWithPoints,{imgs,allMatches}]
buildTwoViewModel[imgs]


(Print@#;buildTwoViewModel[Import[#,ImageSize->{400}]&/@#])&/@{
	{"/s/df_golden/desk/EqQVNi5W-aQAAAAAAR92AQ.JPG","/s/df_golden/desk/fqNnvCe3-f4AAAAAAR91_w.JPG"}
	,{"/s/df_golden/desk/EqQVNi5W-aQAAAAAAR92AQ.JPG","/s/df_golden/desk/PCxPSRryfVUAAAAAAR-vvQ.JPG"}};


(Print@#;buildTwoViewModel[Import[#,ImageSize->{400}]&/@#])&/@Partition[FileNames["/s/df_golden/desk/*.JPG"],2,1]


Map[{dispRtn@#[[1]],#[[2]]}&,weightedRtnss,{2}]


{dispRtn@weightedRtnss[[1,1,1]],weightedRtnss[[1,1,2]]}


(*buildMultiTwoViewModel=Function[imgs,Module[{ikms,matchGroups,homogs,hss,rtns,ptss,vs,qs,imageDim,corners,allMatches,matches,hs,n=Length@imgs-1,rtnss},
	(*allMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];*)
	matchGroups=imageCorrespondingPoints[imgs[[1]],#,"Transformation"->"Perspective"]&/@imgs[[2;;]];
	ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
	homogs=homographyFromMatches/@matchGroups;
	Print[MapThread[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],#,DataRange->Full],#2}]&,{homogs,imgs[[2;;]]}]];
	Print[MapThread[MapThread[annotateImageWithPoints,{{imgs[[1]],#},#2}]&,{imgs[[2;;]],matchGroups}]];
	hss=MapThread[#2.#.Inverse[ikms[[1]]]&,{homogs,ikms[[2;;]]}];
	weightedRtnss=Table[matches=matchGroups[[i]];hs=hss[[i]];
		With[{rtn=fixRtnNormalSignRequirePositiveDepth[#,Join@@matchGroups[[;;,1]],ikms[[1]]]&@standardizeRtn@#},
		{rtn,pointSpread[rtn,matches[[1]],ikms[[1]]]}]&/@decomposeH@hs
	,{i,n}];
	Do[Print@TableForm[Table[Print[{"pointSpread",weightedRtnss[[i,j,2]]}];dispRtn@weightedRtnss[[i,j,1]],{j,2}]],{i,n}];
	rtns=(SortBy[#,Last]&/@weightedRtnss)[[;;,1,1]];Print[rtns];
	(*Graphics3D[rtn=rtns[[i]];
		Table[pts=#/(#.rtn[[3]])&[ikms[[1]].Append[#,1]]&/@matches[[1]];Print[{"pointSpread",pointSpread[rtn,matches[[1]],ikms[[1]]]}];
			Print[{"reprojection error",Mean[Norm/@(matches[[2]]-(#[[;;-2]]/#[[-1]]&[Inverse[ikms[[2]]].(rtn[[1]].#+rtn[[2]])]&/@pts))]}];
		vs={{0,0,0},-rtn[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]]};
		{Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@pts]
			,Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,2,Length@qs}]},{i,n}]
	,Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]*)
(*	Print[TableForm[dispRtn/@#[[1]]]&/@weightedRtnss];
	ptss=Table[#/(#.rtns[[j,3]])&[ikms[[1]].Append[#,1]]&/@matchGroups[[i,1]],{i,n},{j,2}];
	
	Print/@Table[
			pts=#/(#.rtn[[3]])&[ikms[[1]].Append[#,1]]&/@matches[[1]];Print[{"pointSpread",pointSpread[rtn,matches[[1]],ikms[[1]]]}];
			Print[{"reprojection error",Mean[Norm/@(matches[[2]]-(#[[;;-2]]/#[[-1]]&[Inverse[ikms[[2]]].(rtn[[1]].#+rtn[[2]])]&/@pts))]}];
		vs={{0,0,0},-rtn[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]]};
		Graphics3D[{Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@pts]
			,Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,2,Length@qs}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"],{rtn,rtns}]
*)
	]];
buildMultiTwoViewModel@exampleStereoPairs[[1]]*)


imgs=exampleStereoPairs[[1]];putativeMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]]];ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
homog=TransformationMatrix@Last@findGeometricTransform[putativeMatches[[2]],putativeMatches[[1]]];
hs=ikms[[2]].homog.Inverse[ikms[[1]]];rtns=decomposeH@hs;dispRtn/@rtns//TableForm
Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]
Table[anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].rtn[[1]].ikms[[1]],DataRange->Full],imgs[[2]]},{rtn,rtns}]
buildTwoViewModel@imgs
(*rtn=rtns[[1]];matchIdx=4;
MapThread[annotateImageWithPoints,{imgs,Transpose[Transpose[putativeMatches][[{matchIdx}]]]}]
Normalize/@{skewOmega[ikms[[2]].Append[putativeMatches[[2,matchIdx]],1]].rtn[[1]].ikms[[1]].Append[putativeMatches[[1,matchIdx]],1]
	,skewOmega[ikms[[2]].Append[putativeMatches[[1,matchIdx]],1]].rtn[[2]]}*)


case="tsunami";fnames=(*Take[#,3]&@*)FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];allImgs=Import[#,ImageSize->{400}]&/@fnames;
badPairs=Join@@Parallelize@Table[
	imgs=allImgs[[{i,j}]];(*Print[imgs];Print[Length@First@imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"]];*)
	If[Length@First@imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"]>20,
		numRtns=Length@rtnsFromTwoView@imgs;If[numRtns>1,fnames[[{i,j}]],{}],{}]
	,{i,Length@fnames},{j,i+1,Length@fnames}]
(*{#,buildTwoViewModel[Import[#,ImageSize->{400}]&/@#]}&/@RandomSample[Select[badPairs,#!={}&],5]*)


pencil=RandomReal[1,{2,3}];homog=randomSpecialOrthogonalMatrix[3]+outer[RandomReal[1,3],RandomReal[1,3]];pencil2=Inverse@Transpose[homog].#&/@pencil;
(*Normalize/@*){homog.(Cross@@pencil),Cross@@pencil2}


descriptors=ImageKeypoints[#,{"Position","OrientedDescriptor","Scale"}]&/@imgs;nfs=Nearest[(#[[1]]->#)&/@#]&/@descriptors;
pencilSimilarity=Function[{pencil1,pencil2},Module[{pt1,pt2},
	pt1=First[nfs[[1]][pencilIntersection@pencil1]];pt2=First[nfs[[2]][pencilIntersection@pencil2]];
	Correlation@@{pt1[[2]],pt2[[2]]}]];
(*findBestPencilMatches=Function[m,If[Dimensions[m][[1]]>Dimensions[m][[2]],Reverse/@findBestMatches@Transpose@m
	,MapIndexed[{#2[[1]],#}&,First@Ordering[#,-1]&/@m]]];
m=Outer[pencilSimilarity,pencilGroups[[1]],pencilGroups[[2]],1]
pencilMatches={pencilGroups[[1,#[[1]]]],pencilGroups[[2,#[[2]]]]}&/@findBestPencilMatches@m;*)

pencilMatch=pencilMatches[[1]](*{pencilGroups[[1,pencilMatches[[1,1]]]],pencilGroups[[2,pencilMatches[[1,2]]]]}*)
(*pencilMatch={pencilGroups[[1,1]],pencilGroups[[2,1]]};*)
pt1=First[nfs[[1]][pencilIntersection@pencilMatch[[1]]]];pt2=First[nfs[[2]][pencilIntersection@pencilMatch[[2]]]];
{Correlation@@#,ListLinePlot@#}&[{pt1[[2]],pt2[[2]]}]
MapThread[HighlightImage,{imgs,{{pt1[[1]]},{pt2[[1]]}}}]
MapThread[visualizePencil,{pencilMatch,imgs}]


linePixels=Function[{img,line},Module[{r,c,m=ImageData@img,dim,imgHeight,delta},dim=Dimensions@m;imgHeight=dim[[1]];delta=Norm[line[[;;2]]];
	FindInstance[With[{xy=rowColumnToXy[r,c,imgHeight]},Append[xy,1]].line<delta&&1<=r<=dim[[1]]&&1<=c<=dim[[2]],{r,c},Integers,100][[;;,;;,2]]
	]];
line=projectiveLineFromPointPair@lineGroups[[1,1]];
linePixels[imgs[[1]],line]


Export["t.png",Rasterize@{Show[imgs[[1]],Graphics[drawNumberedLines@lineGroups[[1]]]],Show[imgs[[2]],Graphics[drawNumberedLines@lineGroups[[2]]]]}]
Export["t2.png",#]&@Rasterize@{Graphics@{Red,drawNumberedPoints[projective@projectiveLineFromPointPair@#&/@lineGroups[[1]]]}
	,Graphics@{Red,drawNumberedPoints[projective@projectiveLineFromPointPair@#&/@lineGroups[[2]]]}}


linesFromImage=Function[image,
	ImageLines[LaplacianFilter[image,1](*EdgeDetect@image*),.08,.1(*,"Segmented"->True*)(*,"NonMaxSuppression"->True*)(*,Method->"RANSAC"*)]];
pencilsFromImage=Function[image,Module[{longLines=linesFromImage@image},
	Select[Table[projectiveLineFromPointPair/@linePair,{linePair,Subsets[longLines,{2}]}]
		,pointInRectangle[pencilIntersection@#,{{1,1},ImageDimensions@image}]&]]];
imgs=exampleStereoPairs[[10]];(*imgs[[2]]=Import["/h/d/52440-chess-board_1.jpg",ImageSize->{800}];*)
lineGroups={linesFromImage[imgs[[1]]],linesFromImage[imgs[[2]]]};
{Graphics@{Red,drawNumberedPoints[projective@projectiveLineFromPointPair@#&/@lineGroups[[1]]]}
	,Graphics@{Red,drawNumberedPoints[projective@projectiveLineFromPointPair@#&/@lineGroups[[2]]]}}
{Show[imgs[[1]],Graphics[drawNumberedLines@lineGroups[[1]]]],Show[imgs[[2]],Graphics[drawNumberedLines@lineGroups[[2]]]]}
(*matches=Transpose[RandomSample[Transpose@imageCorrespondingPoints[imgs[[1]],imgs[[2]](*,"Transformation"->"Epipolar"*)],100]];
	MapThread[annotateImageWithPoints,{imgs,matches}]*)
(*pencilGroups=pencilsFromImage/@imgs;
MapThread[Function[{pencilGroup,img},visualizePencil[#,img]&/@pencilGroup],{pencilGroups,imgs},1]*)
(*MapThread[visualizePencil,{{pencilGroups[[1,1]],pencilGroups[[2,1]]},imgs}]*)
(*{HighlightImage[imgs[[1]],ImageKeypoints[imgs[[1]]]],HighlightImage[imgs[[2]],ImageKeypoints[imgs[[2]]]]}*)


(*imgs=Import[#,ImageSize->{400}]&/@{"/h/t15.jpg","/h/t16.jpg"};*)
image=imgs[[2]];lines=ImageLines[LaplacianFilter[image,1](*EdgeDetect@image*),.08,.1,"Segmented"->True(*,"NonMaxSuppression"->True*)(*,Method->"RANSAC"*)];
longLines=ImageLines[LaplacianFilter[image,1](*EdgeDetect@image*),.08,.1(*,"Segmented"->True*)(*,"NonMaxSuppression"->True*)(*,Method->"RANSAC"*)];
{Show[image,Graphics[{Thick,Yellow,Line/@lines}]],Show[image,Graphics[drawNumberedLines@longLines]]}


imgs=exampleStereoPairs[[10]];(*imgs=allImgs[[;;2]];*)
Parallelize[Function[image,
(*lines=ImageLines[ImageAdjust@GradientFilter[image,3,"NonMaxSuppression"->True],.09,"Segmented"->True];Show[image,Graphics[{Thick,Yellow,Line/@lines}]]*)
lines=ImageLines[LaplacianFilter[image,1](*EdgeDetect@image*),.08,.1(*,"Segmented"->True*)(*,"NonMaxSuppression"->True*)(*,Method->"RANSAC"*)];
	Show[LaplacianFilter[#,1]&@image,Graphics[{Thick,Yellow,Line/@lines}]]]/@(*Flatten@exampleStereoPairs*)imgs]


preMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]](*,"Transformation"->"Epipolar"*)];
matches=Transpose@Select[Transpose@preMatches,Correlation[First[nfs[[1]][#[[1]]]][[2]],First[nfs[[2]][#[[2]]]][[2]]]>0.8&];
MapThread[annotateImageWithPoints,{imgs,matches}]
(*Table[{Correlation@@#,ListLinePlot@#}&@{First[nfs[[1]][matches[[1,matchIdx]]]][[2]],First[nfs[[2]][matches[[2,matchIdx]]]][[2]]},{matchIdx,Length@matches[[1]]}]*)


pt1=First[nfs[[1]][{220.5,250}]];pt2=First[nfs[[2]][{99.6,231}]];{pt1[[3]],pt2[[3]]}
HighlightImage[imgs[[1]],{{220.5,250},pt1[[1]]}]
HighlightImage[imgs[[2]],{{99.6,231},pt2[[1]]}]
{Correlation@@#,ListLinePlot@#}&[{pt1[[2]],pt2[[2]]}]
{Correlation@@#,ListLinePlot@#}&[{pt1[[2]],First[nfs[[2]][{88,230}]][[2]]}]


dict=StringTake[FileBaseName[#],-2]->#&/@FileNames["/s/df/t"<>"blue_seat52"<>".t/unified_viewer/*.JPG"];
imgs=Import[#,ImageSize->{400}]&/@({(*"RA","RQ"*)"4A","3w"(*"Mw","Nw"*)}/.dict);(*buildTwoViewModel[imgs]*)
(*{ikms,homog,hs,rtns,matches,epiMatches,fms}=twoViewEnvironment@imgs;parallaxLevelHomography[homog,ikms]*)
buildTwoViewModel@imgs


(*{ikms,homog,hs,rtns,matches,epiMatches,fms}=twoViewEnvironment@imgs;
ikms[[1]].Append[#,1]&/@matches[[1]]*)


riemannSphereProject=Function[{xy},Module[{x,y,z},NSolve[x^2+y^2+z^2==1&&x/(1-z)==xy[[1]]&&y/(1-z)==xy[[2]],{x,y,z}][[1,;;,2]]]];
(*riemannSphereProject=Function[{xy},Module[{\[Theta],\[Phi],z=xy[[1]]+I xy[[2]]},\[Theta]=Arg[z];\[Phi]=2ArcCot[Abs@z];{\[Phi],\[Theta]}]];*)
sphericalAngleToXyz=Function[{\[Phi],\[Theta]},{Sin[\[Theta]]Cos[\[Phi]],Sin[\[Theta]]Sin[\[Phi]],Cos[\[Theta]]}];
sphericalXyzToAngle=Function[{x,y,z},Module[{\[Phi],\[Theta]},\[Phi]=ArcTan[x,y];\[Theta]=ArcCos[z];{\[Phi],\[Theta]}]];
Chop@{#,sphericalXyzToAngle@@#,sphericalAngleToXyz@@(sphericalXyzToAngle@@#)}&@riemannSphereProject@#&/@{{0,0},{1,1},{1,0},{0,1}}


SeedRandom[1003];m=Partition[RandomComplex[1+I,4],2];mf=moebiusTransform[m];
(*src=RandomComplex[1+I,30];dst=mf/@src;{rsrc,rdst}=riemannSphereProject@{Re@#,Im@#}&/@#&/@{src,dst};
homog=TransformationMatrix@Last@findGeometricTransform*)


SeedRandom[1004];{trs,rs}=Table[randomSpecialOrthogonalMatrix@3,{2}];{tts,ts}=RandomReal[1,{2,3}];
pts=ts+rs.Append[#,1]&/@RandomReal[1,{30,2}];pts2=tts+trs.#&/@pts;
Graphics3D@{Blue,Point@pts,Red,Point@pts2}
matches={projective/@pts,projective/@pts2};


homog=TransformationMatrix@Last@findGeometricTransform[matches[[2]],matches[[1]],"Transformation"->"Perspective"]
Graphics/@{{Blue,Point[projective[homog.Append[#,1]]&/@matches[[1]]]},{Red,Point@matches[[2]]}}


(*imgs=allImgs[[;;2]];*)imgs=exampleStereoPairs[[1]];
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
homog=homographyFromMatches[matches];hs=ikm.homog.Inverse[ikm];rtns=decomposeH@hs;
{anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]},dispRtn/@rtns//TableForm}
Table[anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].(rtn[[1]]+outer[rtn[[2]],rtn[[3]]]).ikms[[1]],DataRange->Full],imgs[[2]]}
	,{rtn,rtns}]
Table[anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].(rtn[[1]]).ikms[[1]],DataRange->Full],imgs[[2]]}
	,{rtn,rtns}]


anaglyph@{ImagePerspectiveTransformation[allImgs[[1]],Inverse[ikms[[2]]].(rtns[[1,1]]+outer[rtns[[1,2]],rtns[[1,3]]]).ikms[[1]],DataRange->Full],allImgs[[2]]}


imgs={allImgs[[idToIndex["mw"/.nameDict]]],allImgs[[idToIndex["lw"/.nameDict]]]}
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;rtn=rtnFromTwoView@imgs;
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].#.ikms[[1]],DataRange->Full],imgs[[2]]}&/@
	{rtn[[1]]+outer[rtn[[2]],rtn[[3]]],rtn[[1]]
		,Transpose[bodyToWorlds[[idToIndex["mw"/.nameDict]]]].bodyToWorlds[[idToIndex["lw"/.nameDict]]]
		,rots[[2]].Transpose[rots[[1]]](*,Transpose[rots[[2]]].rots[[1]]*)}


MatrixForm/@{Transpose[ToLowerCase[#[[2]]]/.bodyToWorldsFull].(ToLowerCase[#[[1]]]/.bodyToWorldsFull)
	,unVec[#[[3;;3+9-1]],3{1,1}]}&/@Import["/g/tmp/EstimateFirstTwoViews."<>case<>".sfm2.csv"][[;;,2;;]]


(*Calibrate the focal length*)
Clear[f,l];Dynamic@cost
f[l_?NumericQ]:=cost=Module[{ikms=Inverse@intrinsicParameterMatrixWithFocalLength[#,l]&/@allImgs,rtns},
	rtns=Parallelize[rtnFromTwoViewWithFocalLength[#,l]&/@Partition[allImgs,2,1,{1,1}]];
	(*pnorm2[rtns[[;;,1]]-grtns[[;;,1]],2]/Length@rtns+*)pnorm2[Fold[#2.#&,rtns[[1,1]],rtns[[2;;,1]]]-IdentityMatrix[3],2]];
(*{r=FindMinimum[f[l],{{l,37/35}}];//AbsoluteTiming,r[[1]]}
l/.r[[2]]*)


(*case="grassland";rules=FileBaseName[#]->Import[#,ImageSize->{400}]&/@FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];*)
case=(*"zijing"*)"evan_pano2";rulesFull=#->Import[#,ImageSize->{400}]&/@FileNames["/s/df_golden/"<>case<>"/*.JPG"][[;;]];rules=FileBaseName[#[[1]]]->#[[2]]&/@rulesFull;
allImgs=rules[[;;,2]];idToIndex=Position[rules[[;;,1]],#][[1,1]]&;
focalLength=Switch[case,"pole",(*27.65/35*)0.8451,"zijing",(*0.9791*)37/35(*2464/1000*),"grassland",(*27.65/35*) 0.8451,_,4.3/3.2];
ikms=Inverse@intrinsicParameterMatrixWithFocalLength[#,focalLength]&/@allImgs;rtns=Parallelize[rtnFromTwoViewWithFocalLength[#,focalLength]&/@Partition[allImgs,2,1]];
nameDict=StringTake[#[[1]],-2]->#[[1]]&/@rules;
bodyToWorlds=FoldList[#2.#&,IdentityMatrix[3],rtns[[;;,1]]];bodyToWorldsFull=MapThread[Rule,{rulesFull[[;;,1]],bodyToWorlds}];
Graphics3D[{drawCameraWithImageWithFocalLength[Sequence@@#,focalLength]&/@MapThread[Append,{{{0,0,0},quaternionFromRotationMatrix@Transpose@#}&/@bodyToWorlds,allImgs}]
	,Axes->True,AxesLabel->{"x","y","z"}},Lighting->"Neutral",ImageSize->1000]


allImgs=Import[#,ImageSize->{400}]&/@Join[FileNames["/s/df_golden/moon_landing2/image-005*.jpg"]
	,FileNames["/s/df_golden/moon_landing2/image-006*.jpg"]
	,FileNames["/s/df_golden/moon_landing2/image-007*.jpg"]
	(*,FileNames["/s/df_golden/moon_landing2/image-008*.jpg"]
	,FileNames["/s/df_golden/moon_landing2/image-009*.jpg"]
	,FileNames["/s/df_golden/moon_landing2/image-010*.jpg"]
	,FileNames["/s/df_golden/moon_landing2/image-011*.jpg"]*)];
ikms=Inverse@intrinsicParameterMatrix@#&/@allImgs;
rtns=Parallelize[rtnFromTwoView/@Partition[allImgs,2,1]];
Graphics3D[{Opacity[0.3],drawCameraWithImage@@@MapThread[Append,{{{0,0,0},quaternionFromRotationMatrix@Transpose@#}
	(*With[{q=quaternionFromRotationMatrix@Transpose@#},{rotateByQuaternion[{0,0,1},q],q}]*)&/@FoldList[#2.#&,IdentityMatrix[3],rtns[[;;,1]]],allImgs}]
	,Axes->True,AxesLabel->{"x","y","z"}},Lighting->"Neutral",ImageSize->1000,Axes->True,AxesLabel->{"x","y","z"}]


(*Fitting two lines*)
SeedRandom[1003];rs=Table[RotationMatrix[RandomReal[]],{2}];lines=Table[rs[[i]].Append[{#},1]&/@RandomReal[1,100],{i,2}];Graphics[Point/@lines]
Clear[f,l];lss=Array[l,{2,2}];(*Append[Array[x,2],1].(outer@@(Append[#,1]&/@lss)).Append[Array[x,2],1]//Simplify*)
f[lss_?(NumericQ@#[[1,1]]&)]=pnorm[Append[#,1].(outer@@(Append[#,1]&/@lss)).Append[#,1]&/@(Join@@lines),1];
{r=FindMinimum[f[lss],Flatten@lss];//AbsoluteTiming,r[[1]]}
lss/.r[[2]]
projective@First@NullSpace[Append[#,1]&/@#]&/@lines


(*case="fountain";*)case="zijing";rulesFull=#->Import[#,ImageSize->{400}]&/@FileNames["/s/df_golden/"<>case<>"/*.JPG"];rules=FileBaseName[#[[1]]]->#[[2]]&/@rulesFull;
(*case="pole";rules=FileBaseName[#]->Import[#,ImageSize->{400}]&/@FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];*)
focalLength=Switch[case,"pole",(*27.65/35*)0.8451,"zijing",(*0.9791*)37/35(*2464/1000*),"grassland",(*27.65/35*) 0.8451,_,4.3/3.2];
imgs=rules[[;;2,2]];(*tracks=threeViewTrack[rules[[;;3,2]]];*)
(*imgs=exampleStereoPairs[[4]];focalLength=4.6/3.2;*)
(*buildTwoViewModelRotational=Function[imgs,Module[{ikms,matches,homog,hs,rtns,pts,vs,qs,imageDim,corners,allMatches,fm,fFiltered},*)
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	ikms=Inverse@intrinsicParameterMatrixWithFocalLength[#,focalLength]&/@imgs;
	homog=homographyFromMatches@matches;
	(*Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];*)
	Print[MapThread[annotateImageWithPoints,{imgs,matches}]];
	hs=ikms[[2]].homog.Inverse[ikms[[1]]];Print[{"parallax",parallaxLevelHomography[homog,ikms]}];
	rtns=rtnsFromTwoViewWithFocalLength[imgs,{focalLength,focalLength}];
	Print[MapThread[Labeled,{{"Homography","Rotation-1","Rotation-2"},
		anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].#.ikms[[1]],DataRange->Full],imgs[[2]]}&/@Prepend[rtns[[;;,1]],hs]}]];
	rtn=rtnFromTwoViewWithFocalLength[#,{focalLength,focalLength}]&@imgs;
	projs={Inverse[ikms[[1]]].ArrayFlatten@{{IdentityMatrix[3],Table[{0},{3}]}},Inverse[ikms[[2]]].ArrayFlatten@{{rtn[[1]],List/@rtn[[2]]}}};
		pts=Table[projective@triangulatePoint[projs,coords,ikms],{coords,Transpose@matches}];
		vs={{0,0,0},-rtn[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]]};
		Graphics3D[{Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@Select[pts,#[[3]]<100&]],
			Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,2,Length@qs}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]
(*]];*)
(*Import@Export["/g/tmp/triangulate.csv",Join[vec/@N@projs,(*vec/@N@ikms,*)Flatten/@Transpose[matches]]]*)


(*facades of fountain dataset*)
facades=rules[[Complement[Range[28],{1,14,18,20,23}],2]];
buildTwoViewModelSmartWithFocalLength[#,focalLength]&/@Partition[facades,2,1]


imgs=exampleStereoPairs[[4]];
{ikms,homog,hs,rtns,matches,epiMatches,fms}=twoViewEnvironment[imgs];allMatches=imageCorrespondingPoints@@imgs;
rtn=rtnFromTwoView@imgs
(*The learnt (R, t) makes reasonable F-matrix*)
fm=Transpose[ikms[[2]]].(skewOmega[rtn[[2]]].rtn[[1]]).ikms[[1]];visualizeFmatrix[fm,imgs[[2]],matches]
remaining=Transpose@Complement[Transpose@allMatches,Transpose@matches];
Manipulate[Module[{line},line=fm.Append[remaining[[1,idx]],1];
	{annotateImageWithPoints[imgs[[1]],{remaining[[1,idx]]}]
		,annotateImageWithPoints[imgs[[2]],{remaining[[2,idx]],projective[homog.Append[remaining[[1,idx]],1]]}]
		,visualizeEpipolarLine[line,imgs[[2]]]}]
,{idx,1,Length@Transpose@remaining,1}]

matchesOnEpipolarLine=Function[{srcPts,fm,imgs},Module[{descriptors,nfs,line,srcDes,canDes,bestMatch},
	descriptors=ImageKeypoints[#,{"Position","OrientedDescriptor"}]&/@imgs;nfs=Nearest[(#[[1]]->#)&/@#]&/@descriptors;
	Transpose[Join@@Table[line=Normalize[fm.Append[srcPts[[idx]],1]];
		srcDes=First[nfs[[1]][remaining[[1,idx]]]];canDes=Select[descriptors[[2]],Abs[line.Append[#[[1]],1]]<0.08&];
		If[canDes=={},{},bestMatch=First@SortBy[canDes,-Correlation[srcDes[[2]],#[[2]]]&];
			{{srcPts[[idx]],bestMatch[[1]]}}]
	,{idx,Length@srcPts}]]
	]];
(*descriptors=ImageKeypoints[#,{"Position","OrientedDescriptor","Scale"}]&/@imgs;nfs=Nearest[(#[[1]]->#)&/@#]&/@descriptors;
Table[line=Normalize[fm.Append[remaining[[1,idx]],1]];
srcDes=First[nfs[[1]][remaining[[1,idx]]]];canDes=Select[descriptors[[2]],Abs[line.Append[#[[1]],1]]<0.08&];
filtered=takeAtMostN[SortBy[canDes,-Correlation[srcDes[[2]],#[[2]]]&],10];
(*Print[Correlation[srcDes[[2]],#]&/@filtered[[;;,2]]];*)
Prepend[MapThread[annotateImageWithPoints,{imgs,{{srcDes[[1]]},filtered[[;;,1]]}}],annotateImageWithPoints[imgs[[2]],canDes[[;;,1]]]],{idx,30(*Length@remaining[[1]]*)}]*)
epiMatches=matchesOnEpipolarLine[remaining[[1]],fm,imgs];
MapThread[annotateImageWithPoints,{imgs,epiMatches}]


homogs=FoldList[#2.#&,IdentityMatrix[3],rs[[;;,1]]];
MapThread[ImagePerspectiveTransformation[#2,Inverse[#],DataRange->Full]&,{homogs[[;;;;5]],allImgs[[;;;;5]]}]


ttrs=Parallelize@Table[
rtns=decomposeSameNormalCalibratedHomographyList@{Inverse[ikm.rs[[i,1]].Inverse[ikm]],ikm.rs[[i+1,1]].Inverse[ikm]};(*dispRtn/@rtns*)
{{-Transpose[rtns[[1,1]]].rtns[[1,2]],Transpose[rtns[[2,1]]].rtns[[2,2]]},rtns[[1,1]]}
,{i,100,Length@rs-1}];
scaledTtoFirstRs=FoldList[{Norm[#[[1,2]]]/Norm[#2[[1,1]]](#2[[1]]),#[[2]].#2[[2]]}&,ttrs[[1]],ttrs[[2;;]]];
tts=MapThread[Function[t,#2.t]/@#&,{scaledTtoFirstRs[[;;,1]],scaledTtoFirstRs[[;;,2]]}];


ListLinePlot@Transpose[quaternionFromRotationMatrix@#&/@scaledTtoFirstRs[[;;,2]]]


{ListLinePlot[Transpose[Most@tts[[;;,2]]-Rest@tts[[;;,1]]],PlotRange->All],ListLinePlot[Transpose[Join@@tts[[;;]]],PlotRange->All]}
smoothTts=Transpose[MedianFilter[#,1]&/@Transpose[tts[[;;,1]]]];
ListLinePlot[Transpose@smoothTts,PlotRange->All]
Graphics3D[BlendLine[Accumulate[smoothTts]],Axes->True,AxesLabel->{"x","y","z"}]


cnt=1;
trajectory=Accumulate[smoothTts];
(*Dynamic@Refresh[cnt=Mod[cnt,Length@trajectory]+1;*)
Manipulate[
	Magnify[{allImgs[[cnt]],Graphics3D[{BlendLine[trajectory],Blue,Point[trajectory[[cnt]]]},Axes->True,AxesLabel->{"x","y","z"}]},2]
,{cnt,1,Length@trajectory,1}]
	(*,UpdateInterval->1]*)


imgs=allImgs[[34-1+{0,1}+25]];(*imgs=Import/@{"/s/df_golden/moon_landing2/image-0034.jpg","/s/df_golden/moon_landing2/image-0035.jpg"}*)
preMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];rect={{420,20},{600,80}}/2;
matches=Transpose@Select[Transpose@preMatches,Not@pointInRectangle[#[[1]],rect]&&Not@pointInRectangle[#[[2]],rect]&];
homog=homographyFromMatchesL1[matches,imgs,{ikm,ikm}];
MapThread[annotateImageWithPoints,{imgs,matches}]
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}


allImgs=Parallelize[Import[#(*,ImageSize->320*)]&/@FileNames["/s/change3_landing/image-*.jpg"]];
rs=Parallelize@Table[buildHomographySimple[allImgs[[i;;i+1]]],{i,1,Length@allImgs-1}];
(*Export["/h/mxs/moon_landing.mx",{allImgs,rs}];*)
(*Export["/h/mxs/change3.mx",{allImgs,rs}];*)
Export["/tmp/t.html",MapIndexed[{#2[[1]],#}&,rs[[;;,2]]]]


rs=Parallelize@Table[buildHomographySimple[allImgs[[{i,i+4}]]],{i,1,Length@allImgs-4}];
Export["/tmp/t.html",MapIndexed[{#2[[1]],#}&,rs[[;;,2]]]]


{allImgs,rs}=Import@"/h/mxs/change3.mx";ikm=Inverse@intrinsicParameterMatrix[allImgs[[1]]];
(*allImgs=Show[#,Graphics@{Rectangle[{1,1},{105,360}],Polygon[{{300,360},{450,360},{450,180}}]}]&/@allImgs;*)


anaglyph@allImgs[[5;;6]]
buildTwoViewModel@allImgs[[5;;6]]


imgs=allImgs[[75+{0,4}]];(*imgs=Import/@{"/s/df_golden/moon_landing2/image-0034.jpg","/s/df_golden/moon_landing2/image-0035.jpg"}*)
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]]
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
homog=homographyFromMatchesL1[matches,imgs,{ikm,ikm}];
MapThread[annotateImageWithPoints,{imgs,matches}]
anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}


evalPair=Function[pair,anaglyph@{ImagePerspectiveTransformation[imgs[[pair[[1]]]],pair[[3]],DataRange->Full],imgs[[pair[[2]]]]}];
evalPair/@pairs


(*MatrixForm[#/#[[-1,-1]]]&/@hss
MatrixForm[#/#[[-1,-1]]]&/@hss2
Table[reprojectionErrorHomography[hss[[i]],matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,3}]
Table[reprojectionErrorHomography[hss2[[i]],matchGroups[[i]]]/Length[Transpose@matchGroups[[i]]],{i,3}]
Table[homog=hss[[i]];anaglyph@{ImagePerspectiveTransformation[imgs[[pairs[[i,1]]]],homog,DataRange->Full],imgs[[pairs[[i,2]]]]},{i,Min[15,Length@pairs]}]
Table[homog=hss2[[i]];anaglyph@{ImagePerspectiveTransformation[imgs[[pairs[[i,1]]]],homog,DataRange->Full],imgs[[pairs[[i,2]]]]},{i,Min[15,Length@pairs]}]
{Graphics3D@Point[tss/.r[[2]]],imgs}
Graphics3D@BlendLine[tss/.r[[2]]]
rss=matrixExpSpecialOrthogonal@#&/@(ass/.r[[2]]);MatrixForm[Inverse[rss[[1]]].#]&/@rss[[2;;]]*)


dispRtn/@(Join@@(decomposeH@Partition[#,3]&/@homogPairs[[7;;14,3;;]]))//TableForm


homogPairs=Import["/g/tmp/street_twoviews.csv"][[;;,{2,3}~Join~Range[-10,-2]]]


case="street";{models,rules}=loadSfmModel["street","sfm"];


case="street";imgs=If[ImageDimensions[#][[1]]<ImageDimensions[#][[2]],ImageRotate[#,-90Degree],#]&@
	Import[#,ImageSize->{800}]&/@FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];
rs=Join@@(Parallelize@Table[{i,j,Quiet@buildHomographySimple[imgs[[{i,j}]]]},{i,1,Length@imgs},{j,i+1,Length@imgs}]);
(*ImageDimensions/@imgs*)
Export["/h/mxs/colorization_"<>case<>".mx",{imgs,rs}]


case="street";{imgs,rs}=Import["/h/mxs/colorization_"<>case<>".mx"];
ikm=Inverse@intrinsicParameterMatrix@imgs[[1]];
sorted=SortBy[{#,ImageResize[#,{200}]&/@imgs[[#[[1;;2]]]],correlationErrorHomography[#[[3,1]],imgs[[#[[1;;2]]]]]}&/@Select[rs,#[[3]]=!=Null&],Last];
sorted//TableForm
goodRs=Select[sorted,Last@#<0.2&][[;;,1]];
g=Graph[goodRs[[;;,;;2]]];VertexList@g
GraphPlot[g,VertexLabeling->True,ImageSize->1200]


viewSfmModelWithScale["thu","sfm",1(*modelIdx*),1(*scale*)]


idx=8;imgs[[idx]]
Graphics3D[Join@@(drawCameraWithImage@@(Append[With[{b2w=#[[1]]},{8{1,1,1}(Transpose[b2w].#[[2]]),quaternionFromRotationMatrix@b2w}]&@
	standardizeRtn@First@decomposeH[ikm.#[[3,1]].Inverse[ikm]],imgs[[#[[2]]]]])&/@Select[goodRs,#[[1]]==idx&])
,Axes->True,AxesLabel->{"x","y","z"},ImageSize->800,Lighting->"Neutral"]


rules=#[[;;2]]->#[[3;;]]&/@goodRs;
circleRs=First/@{{1,3}/.rules,{3,11}/.rules,{1,11}/.rules}


dispRtn/@decomposeH@circleRs[[1,1]]//TableForm
dispRtn/@decomposeH@circleRs[[2,1]]//TableForm
dispRtn/@decomposeH@circleRs[[3,1]]//TableForm

#/#[[-1,-1]]&[(decomposeH@circleRs[[2,1]])[[1,1]].(decomposeH@circleRs[[1,1]])[[1,1]]]
#/#[[-1,-1]]&[(decomposeH@circleRs[[2,1]])[[2,1]].(decomposeH@circleRs[[1,1]])[[2,1]]]


rtns=(First@decomposeH@(ikm.#.Inverse[ikm])&/@Select[goodRs,#[[1]]==idx&][[;;,3,1]]);dispRtn/@rtns//TableForm


dispRtn/@Join@@(decomposeH@(ikm.#.Inverse[ikm])&/@Select[goodRs,#[[1]]==idx&][[;;,3,1]])//TableForm


exampleStereoPairs[[9]]
buildLaplacianWarp@exampleStereoPairs[[9]]
exampleStereoPairs[[1]]
buildLaplacianWarp@exampleStereoPairs[[1]]


imgs=exampleStereoPairs[[1]]
descriptors=ImageKeypoints[#,{"Position","OrientedDescriptor"}]&/@imgs;nfs=Nearest[(#[[1]]->#)&/@#]&/@descriptors;


SeedRandom[1003];d1=descriptors[[1,;;,2]];d2=descriptors[[2,;;,2]];
Outer[Correlation,d1,d2,1]//MatrixPlot
indices=Sort@RandomSample[Range[Dimensions[d1][[2]]],8];
Outer[Correlation,d1[[;;,indices]],d2[[;;,indices]],1]//MatrixPlot


prefnames=FileNames["/s/df_golden/beihang/*.JPG"][[;;]];fnames=Complement[prefnames,(*prefnames[[{14,15}]]*){}];
imgs=Import[#,ImageSize->{400}]&/@fnames;focalLengths=focalLengthFromImageExif/@fnames;
pts=Standardize[(With[{rule=Import[#,"Exif"]},GeoPositionXYZ@GeoPosition@{FromDMS["GPSLatitude"/.rule],FromDMS["GPSLongitude"/.rule],"GPSAltitude"/.rule}]&/@fnames)[[;;,1]]];
Graphics3D@{Blue,Thick,drawNumberedPoints@pts,Thin,BlendLine@pts}


rtnss=MapThread[rtnsFromTwoViewWithFocalLength,{Partition[imgs[[;;]],2,1],Partition[focalLengths[[;;]],2,1]}];
(*{First,First,Last,}*)


(*rtns=MapThread[rtnFromTwoViewWithFocalLength,{Partition[imgs[[;;]],2,1],Partition[focalLengths[[;;]],2,1]}];*)
rots=MapThread[#[#2[[;;,1]]]&,{ReplacePart[Table[First,{Length[rtnss]}],{3->Last,6->Last,16->Last,17->Last}],rtnss}];
qs=quaternionFromRotationMatrix@Transpose[#]&/@FoldList[#2.#&,quaternionToRotationMatrix[(*{-1,1,1.5,0.5}*){1,0,0,0}],rots];
Graphics3D[{Opacity[0.5],MapThread[drawCameraWithImageWithFocalLength,{pts,qs,imgs,focalLengths}],Red,Thick,drawNumberedPoints@pts}
	,Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral",ImageSize->1000]


focalLengths=focalLengthFromImageExif/@fnames;
i=1;MapThread[(Print[i++];buildTwoViewModelSmartWithFocalLength[#,#2])&,{Partition[imgs[[;;]],2,1],Partition[focalLengths[[;;]],2,1]}]


img=ColorConvert[exampleStereoPairs[[1,1]],"Gray"];ikm=intrinsicParameterMatrix@img;
imgs=Table[ImagePerspectiveTransformation[img,Inverse[ikm].quaternionToRotationMatrix[axisAngleToQuaternion[i{0,0,1}]].ikm,DataRange->Full]
	,{i,0,0.3,0.01}];
m=vec@ImageData@#&/@imgs;m//Dimensions
svs=SingularValueList@m;svs/Total[svs]
Accumulate[svs/Total[svs]]
visualizePowerLaw@Rest@svs
