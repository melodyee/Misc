(* ::Package:: *)

(*SetOptions[$FrontEnd, "ClearEvaluationQueueOnKernelQuit" -> False]*)
<<"~/gdrive/mac_home/t3.m"
parseTimeStamp=Function[fname,ToExpression@StringSplit[StringReplace[fname,".JPG"->""],"_"][[-1]]];


SeedRandom[1003];len=300;n=2;Clear[a,g,f1,f2];as=Array[a,{2,n,n}];
g[as_,m_]:=With[{bs=MatrixExp[nilTrace[as[[1]]+I as[[2]]]]},Transpose[With[{z=#[[1]]+#[[2]]I},
	{Re@#,Im@#}&[(bs[[1,1]]z+bs[[1,2]])/(bs[[2,1]]z+bs[[2,2]])]]&/@Transpose[m]]];
f1[as_?(NumericQ[#[[1,1,1]]]&),m_]:=pnorm[g[as,m],1];
f2[as_?(NumericQ[#[[1,1,1]]]&),m_]:=-pnorm[g[as,m],3];
(*Rotate a square*)norm=RandomReal[1,2];m1=With[{u=RandomReal[1,2]},Transpose[(#+u)/(2+norm.#)&/@Transpose@RandomReal[{-1,1},{n,len}]]];
(*Rotate a ellipse*)
(*m2=With[{t=RandomReal[2Pi],u=Boole[perspective]RandomReal[1,2]},Transpose@Table[(RandomReal[] RotationMatrix[t].{4Cos@#,Sin@#}+u)&@RandomReal[2Pi],{len}]];*)
cnt=0;
Parallelize@Table[Module[{},
	Print[r=NMinimize[f[as,m],Flatten@{as},Method->"DifferentialEvolution"];//AbsoluteTiming];
	cnt+=1;
	(*Import@Export["t"<>IntegerString[cnt]<>".png",Rasterize[*)Graphics[Point@Transpose@#,ImageSize->300,Axes->True]&/@
		{m,g[as/.r[[2]],m]}](*]]*)
,{f,{f1,f2}},{m,{m1(*,m2*)}}]


SeedRandom[1003];len=200;n=2;Clear[a,t];as=Array[a,n{1,1}];ts=Array[t,n];
Table[
h[as_,ts_,m_,\[Theta]_]:=ArrayFlatten[{{MatrixExp[as-Transpose[as]],If[affine,List/@ts,0]},{If[perspective,{{Cos@\[Theta],Sin@\[Theta]}},0],1}}];
g[as_,ts_,m_,\[Theta]_]:=(h[as,ts,m,\[Theta]].Transpose[Append[#,Boole@affine]&/@Transpose@m])[[;;2,;;]];
f1[as_?(NumericQ[#[[1,1]]]&),ts_,m_,\[Theta]_]:=pnorm[g[as,ts,m,\[Theta]],1];
f2[as_?(NumericQ[#[[1,1]]]&),ts_,m_,\[Theta]_]:=-pnorm[g[as,ts,m,\[Theta]],3];
(*Rotate a square*)m1=With[{u=Boole[affine]RandomReal[1,2]},Print[u];Transpose[{1,2}#+u&/@Transpose@RandomReal[{-1,1},{n,len}]]];
(*Rotate a ellipse*)
m2=With[{t=RandomReal[2Pi],u=Boole[affine]RandomReal[1,2]},
	Print[u];Transpose@Table[(RandomReal[] RotationMatrix[t].{4Cos@#,Sin@#}+u)&@RandomReal[2Pi],{len}]];
cnt=0;
Parallelize@Table[Module[{},
	Print[r=NMinimize[f[as,ts,m,\[Theta]],Flatten@{as,If[affine,ts,{}],If[perspective,{\[Theta]},{}]},MaxIterations -> 1000,
		Method->{"DifferentialEvolution","PostProcess" -> {FindMinimum, Method -> "QuasiNewton"}}];//AbsoluteTiming];
	cnt+=1;
	(*Import@Export["t"<>IntegerString[cnt]<>".png",Rasterize[*)Graphics[Point@Transpose@#,ImageSize->300,Axes->True
			,PlotLabel->(h[as,ts,m,\[Theta]]/.r[[2]])]&/@
		{m,g[as/.r[[2]],ts/.r[[2]],m,\[Theta]/.r[[2]]]}](*]]*)
,{f,{f1,f2}},{m,{m1,m2}}]
,{affine,{False(*,True*)}},{perspective,{(*False,*)True}}]


i1=CurrentImage[];keypoints=ImageKeypoints[i1,MaxFeatures->20];rng=ImageDimensions@i1;
Dynamic@Refresh[i2=CurrentImage[];tr=FindGeometricTransform[i2,i1];
	Magnify[#,2]&@MapThread[HighlightImage,{{i1,i2},{keypoints,Select[tr[[2]]/@keypoints,1<=#[[1]]<=rng[[1]]&&1<=#[[2]]<=rng[[2]]&]}}],UpdateInterval->3]


i1=CurrentImage[];keypoints=ImageKeypoints[i1,MaxFeatures->20];
Dynamic[i2=CurrentImage[];
	Magnify[#,2]&@MapThread[HighlightImage,{{i1,i2},ImageFeatureTrack[{i1,i2},keypoints]}]]


(*imgDim=ImageDimensions[imgs[[1]]](*imgDim=50{1,1};*)*)
depth=50;
MatrixForm/@{tm
	,ArrayFlatten@{{tm[[;;2,;;2]],List/@(tm[[;;2,-1]]/depth)},{{tm[[-1,;;2]] depth},1}}
	,Inverse[Inverse[translation].quaternionToRotationMatrix[dqs[[1]]].translation]}


imgs=ColorConvert[Import[#,ImageSize->20{1,1}],"Grayscale"]&/@{"t2.jpg","t7.jpg"};
Graphics@Flatten@Module[{a=g[as,ts]/.r[[2]],m=ImageData[imgs[[1]]]},
	Table[{GrayLevel[m[[i,j]]],
		Module[{ii,jj},{ii,jj}=#[[;;2]]/#[[3]]&[a.{i,j,1,1}];
		Point[{jj,-ii}]]},{i,Dimensions[m][[1]]},{j,Dimensions[m][[2]]}]]


Clear[f,a,h,\[Theta],t];SeedRandom[1003];ts=Array[t,3];as=Array[a,3{1,1}];
hs=Array[h,Dimensions[matches][[2]]];(*inverse height*)
g[as_,ts_]:=Append[MapThread[Append,{MatrixExp[as-Transpose@as],ts}],{0,0,0,1}];
f[hs_?(NumericQ@#[[1]]&),as_,ts_]:=pnorm[MapThread[Most[g[as,ts].Join[#,{1,1}]]-Append[#2,1]/#3&,Append[matches,hs^2]],1];
r=NMinimize[f[hs,as,ts],Flatten@Join[Join[ts,as],hs]];//AbsoluteTiming
MatrixForm/@{as,ts,hs}/.r[[2]]


SeedRandom[1003];
points=Flatten[Table[{i,j,k},{i,3},{j,3},{k,3}],2];Graphics3D@Point@points
cameras={IdentityMatrix[{3,4}],ArrayFlatten@{{MatrixExp[#-Transpose[#]&@RandomReal[1,3{1,1}]],RandomReal[1,{3,1}]}}};
Function[c,Graphics[Point[#[[;;2]]/#[[-1]]&[c.Append[#,1]]&/@points]]]/@cameras


(*bundleAdjust=Function[matches,
	]*)
p=2;Clear[x,a,t,f];xs=Array[x,Dimensions@matches];as=Array[a,3{1,1}];ts=Array[t,3];
prod=Function[{m,x1,x2},Append[x1,1].m.Append[x2,1]];
f[xs_?(NumericQ@#[[1,1,1]]&),as_,ts_,matches_]:=With[{em=skewOmega[ts].MatrixExp[as-as\[Transpose]]},
	Total[Table[prod[em,matches[[2,j]],xs[[1,j]]]/(0.000001+prod[em,{0,0},xs[[1,j]]]),{j,1,Length[xs[[1]]]}]^2]
		+Total[Table[prod[em,xs[[2,j]],matches[[1,j]]]/(0.000001+prod[em,xs[[2,j]],{0,0}]),{j,1,Length[xs[[1]]]}]^2]];
r=(*FindMinimum*)NMinimize[f[xs,as,ts,matches]
	,Flatten@{xs,as,ts}
	(*,Join@@{variableWithInital[xs,matches],variableWithInital[as,Array[0&,Dimensions@as]],variableWithInital[ts,Array[0&,Dimensions@ts]]}*)
	(*,Method->(*"LevenbergMarquardt"*)"QuasiNewton"*)];//AbsoluteTiming


Clear[d,t,m];ts=Array[t,3];ms=Array[m,3{1,1}];ds=Array[d,Length@matches[[1]]];
(*f[ts_,ms_,ds_?(NumericQ@#[[1]]&)]:=pnorm2[matches[[2]]-MapThread[#[[;;2]]/#[[3]]&[MapThread[Append,{skewOmega[ts].ms,ts}].(Append[# #2,1])]&,
	{Append[#,1]&/@matches[[1]],ds}],2];*)
r=NMinimize[pnorm2[matches[[2]]-MapThread[#[[;;2]]/#[[3]]&[MapThread[Append,{skewOmega[ts].ms,ts}].(Append[# #2,1])]&,
	{Append[#,1]&/@matches[[1]],ds^2}],2](*f[ts,ms,ds]*),Flatten@{ts,ms,ds}];//AbsoluteTiming
MatrixForm/@{ts,ms,ds^2}/.r[[2]]


Graphics3D[Riffle[GrayLevel[ImageData[i1][[Round[#[[1]]],Round[#[[2]]]]]]&/@ms[[1]],Point/@MapThread[Append[# #2,-1000 #2]&,{ms[[1]],hs^2}]]/.r[[2]]]


tracks=GatherBy[Import@"/tmp/tracks.csv",First];
MapThread[annotateImageWithPoints,{imgs,tracks[[;;,;;,{3,4}]]/3},1]


ms=matches[[;;,;;]];Clear[h,a,t];hs=Array[h,Dimensions[ms][[2]]];as=Array[a,3{1,1}];ts=Array[t,3];
f[hs_?(NumericQ@#[[1,1]]&),as_,ts_]:=pnorm2[With[{A=MapThread[Append,{MatrixExp[as-Transpose@as],ts}]}
	,MapThread[A.Join[# #2,{#2,1}]&,{ms[[1]],hs^2}]]-(Append[#,1]&/@ms[[2]]),2];
r=NMinimize[f[hs,as,ts],Flatten@{hs,as,ts}];//AbsoluteTiming
(MatrixForm/@{hs^2,MatrixExp[as-Transpose[as]],ts})/.r[[2]]


imgs=ColorConvert[Import[#,ImageSize->200{1,1}],"Grayscale"]&/@{"t2.jpg","t7.jpg"};
Graphics@Flatten@Module[{m=ImageData[imgs[[2]]]},
	Table[{GrayLevel[m[[i,j]]],
		Module[{x,y},{x,y}=(*tr[[2]][{j,-i}];*){j,-i};
		Point[{x,y}]]},{i,Dimensions[m][[1]]},{j,Dimensions[m][[2]]}]]


n=128;dir="/s/kejian_front/";imgs=ImageResize[ColorConvert[ImageResize[Import[dir<>#],n/8{1,1}],"Grayscale"],n{1,1}]&/@{"image-028.jpg","image-029.jpg"}
ms=ImageData/@imgs;
(*m=Join@@(gradient2D@m1);b=Flatten[ms[[2]]-ms[[1]]];c=(n+1.)/2;weights=Join@@Table[Exp[-Norm[{i,j}-{c,c}]],{i,n},{j,n}];*)


imgs=Import/@FileNames["/s/gdesk/image-*.jpg"][[;;]];ms=ImageData@ImageResize[ColorConvert[#,"Grayscale"],100{1,1}]&/@imgs;(*Image/@ms*)
(*fs=MapIndexed[{#2[[1]],#}&,{ImageResize[ImageAdjust@#,100]&@(ImageDifference@@(Image/@#))
	,dispOpticalFlow@@(Image/@#)}&/@Partition[ms,2,1]];//AbsoluteTiming 
fs*)
{m1,m2}=ms[[30;;31]];
flows3=opticalFlowTVL1[m1,m2,1,30];//AbsoluteTiming
{dispFlow@flows3,dispFlowColor@flows3,Image/@{m1,m2}}


imgs=Import/@FileNames["/s/seq3/*"];ms=ImageData@ImageResize[ColorConvert[#,"Grayscale"],100{1,1}]&/@imgs;Image/@ms
{m1,m2}=ms[[;;2]];
flows3=opticalFlowTVL1[m1,m2,1,30];//AbsoluteTiming
Graphics[Flatten@MapIndexed[{Hue[Arg[Plus@@(#{1,I})]/(2Pi),Norm[#],1],Point@#2}&,flows3,{2}]]


ImageCapture[]


dispOpticalFlow[Image@ms[[1]],Image@ms[[2]]]//AbsoluteTiming


old=cur=CurrentImage[];Dynamic[old=cur;cur=CurrentImage[];{cur,dispOpticalFlow[old,cur]}]


m=upperTriangle@RandomReal[3{-1,1},4{1,1}];MatrixForm@m
{Eigenvalues@m,MatrixForm@Transpose@Eigenvectors@m}
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"UT"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]]}//AbsoluteTiming*)
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"Schur"];,MatrixForm@xus[[1]],MatrixForm/@xus[[2]]}//AbsoluteTiming
MatrixForm/@SchurDecomposition[m]


SeedRandom[1003];m=RandomReal[3{-1,1},3{1,1,1}];(*m=N@{{1,3},{0,0}};*)
m=N@{{{{1,0},{0,1}},0 IdentityMatrix@2},{0 IdentityMatrix@2,{{1,0},{0,1}}}};
{
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"SVD"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]](*,SingularValueList@m*)}//AbsoluteTiming,
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"SL"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]](*,Det[m]^(1/Dimensions[m][[1]])*)}//AbsoluteTiming,
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"LDU"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]](*,MatrixForm/@luDecomposition@m*)}//AbsoluteTiming,*)
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"Schur"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]]}//AbsoluteTiming,
}


(*If we use real matrix for EVD, then for {{a+bI,0},{0, a-bI}}, we get {{b,a},{a,-b}} as minimum.*)
(*"UT" decomposes a upper triangular matrix into product of upper unitriangular, diagonal and upper unitriangular.*)
(*XXXXXXXXXXXX: parameterization of unitary/orthogonal should allow 2k Pi I, k\in N, on the diagonal*)
sparseLieDecomposition=Function[{D,mynorm,type},Module[{cplx=containsComplexesQ@D||type=="EVD",as,u,dim,getXus,r,f,g,order,varDims,halfDim
		,unitary,unitriangular,specialLinear,generalLinear},
	unitary=If[cplx,MatrixExp[#-#\[ConjugateTranspose]]&[#[[1]]+I #[[2]]],MatrixExp[#-#\[Transpose]]]&;
	unitriangular=MatrixExp@strictUpperTriangle@If[cplx,#[[1]]+I #[[2]],#]&;
	specialLinear=MatrixExp@nilTrace@If[cplx,#[[1]]+I #[[2]],#]&;generalLinear=MatrixExp@If[cplx,#[[1]]+I #[[2]],#]&;
	dim=Dimensions@D;order=Length@dim;halfDim=If[Mod[order,2]==0,dim[[;;order/2]]];
	varDims=Which[MemberQ[{"SVD","LDU","SL"},type],{order,dim},MemberQ[{"EVD","UT-EVD","Cholesky"},type],{order/2,halfDim}
		,MemberQ[{"Schur","QR","UT"},type],{order,Join@@Transpose@{halfDim,halfDim}}];
	as=MapThread[Array[#,If[cplx,{2,#2,#2},#2{1,1}]]&,{Array[u,varDims[[1]]],varDims[[2]]}];
	g=Function[as,Switch[type,"SVD",unitary/@as,"LDU",Transpose@unitriangular@#&/@as,"SL",specialLinear/@as
		,"EVD",With[{ps=generalLinear/@as},Join[ps,Transpose@Inverse@#&/@ps]]
		,"UT-EVD",With[{ps=unitriangular/@as},Join[ps,Transpose@Inverse@#&/@ps]]
		,"Cholesky",With[{ps=unitriangular/@as},Join[Transpose/@ps,ConjugateTranspose/@ps]]
		,"QR",Join[unitary/@as[[;;order/2]],Transpose@unitriangular@#&/@as[[order/2+1;;]]]
		,"Schur",With[{ps=unitriangular[#[[2]]].unitary[#[[1]]]&/@Partition[as,2]},Join[ps,Transpose@Inverse@#&/@ps]]
		,"UT",Join[unitriangular/@as[[;;order/2]],Transpose@unitriangular@#&/@as[[order/2+1;;]]]
		]];
	getXus=Function[{as,m},Module[{us=g[as]},(*sortXUs*)List[foldXUs[m,us,{}],Inverse/@us]]];
	Clear[f];If[cplx,f[as_?(NumericQ[#[[1,1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]],
		f[as_?(NumericQ[#[[1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]]];
	r=NMinimize[f[as],DeleteDuplicates@Flatten[as](*,MaxIterations->500*)];
	getXus[as/.r[[2]],D]
	]];
SeedRandom[1003];m=RandomReal[3{-1,1},3{1,1}];(*m=N@{{1,1},{0,1}};*)
{
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"SVD"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],SingularValueList@m}//AbsoluteTiming,*)
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"SL"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],Det[m]^(1/Dimensions[m][[1]])}//AbsoluteTiming,*)
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"EVD"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],Eigenvalues@m}//AbsoluteTiming,
(*UT-EVD does not generate sparse result
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"UT-EVD"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],Eigenvalues@m}//AbsoluteTiming,*)
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"LDU"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],MatrixForm/@luDecomposition@m}//AbsoluteTiming,
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"QR"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],MatrixForm/@QRDecomposition@m}//AbsoluteTiming,*)
(*{xus=sparseLieDecomposition[m,pnorm[#,1]&,"Schur"];,MatrixForm/@xus[[1]],MatrixForm/@xus[[2]],MatrixForm/@schurDecomposition@m}//AbsoluteTiming,*)
(*With[{m=m.m\[Transpose]},{xus=sparseLieDecomposition[m,pnorm[#,1]&,"Cholesky"];
	,MatrixForm/@Sqrt@Abs@xus[[1]],MatrixForm/@xus[[2]],MatrixForm/@CholeskyDecomposition@m}]//AbsoluteTiming,
m=RandomComplex[(1+I){-1,1},2{1,1}];
{xus=sparseLieDecomposition[m,pnorm[#,1]&,"SVD"];,MatrixForm/@Abs@xus[[1]],MatrixForm/@xus[[2]],SingularValueList@m}//AbsoluteTiming*)
}


m
q=Transpose@Eigenvectors@m;
Norm[q.DiagonalMatrix@Eigenvalues@m.Inverse[q]-m,"Frobenius"]
qr=QRDecomposition@q;
Norm[q-qr[[1]]\[ConjugateTranspose].qr[[2]],"Frobenius"]
Norm[Inverse[qr[[1]]\[ConjugateTranspose].qr[[2]]]-Inverse[qr[[2]]].qr[[1]],"Frobenius"]
Norm[qr[[1]]\[ConjugateTranspose].qr[[2]].DiagonalMatrix@Eigenvalues@m.Inverse[qr[[2]]].qr[[1]]-m,"Frobenius"]


p=1;Select[Table[u=randomUnitaryMatrix[2];\[CapitalLambda]=DiagonalMatrix@RandomReal[{-1,1},2];
	r=MatrixExp[{{0,RandomReal[{-1,1}]},{0,0}}];c=r.\[CapitalLambda].Inverse[r];
	Append[pnorm2[#,p]&/@{c,u.c.u\[ConjugateTranspose]},MatrixForm/@{r,c,u}],{10}],#[[1]]>#[[2]]&]


q=Transpose@Eigenvectors@m;
q.DiagonalMatrix@Eigenvalues@m.Inverse[q]
qr=QRDecomposition@q;
MatrixForm/@{qr[[1]]\[Transpose].qr[[2]],q}
{qr[[1]]\[Transpose],qr[[2]].DiagonalMatrix@Eigenvalues@m.Inverse[qr[[2]]]}


testNormConcave=Function[f,Module[{xs},xs=RandomReal[10,10];{Total[f/@Abs@xs],f@Norm@xs}]];
testNormConcave/@{-Log[#]&,Log,#^2&,-#^3&,#&}


Plot[Function[d,Nest[Function[x,d-Sign[x]Abs[x]^0.5],d,3]][x],{x,-3,3}(*,PlotRange->All*)]


SeedRandom[1003];m=(*#/(Det[#]^(1/Dimensions[#][[1]]))&@*)RandomReal[10{-1,1},2{1,1}];
d=DiagonalMatrix@Table[GeometricMean@SingularValueList@m,{Length@Dimensions@m}];
{Det[m],MatrixForm@m,MatrixForm@d,MatrixForm/@SingularValueDecomposition@m,GeometricMean@SingularValueList@m}
{xsls=heuristicSSLD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xsls}


c=Inverse[Transpose[xsls[[2,2]]]].Inverse[xsls[[2,1]]];MatrixForm@c
Tr[xsls[[2,1]].d.Transpose[xsls[[2,2]]].c]
Total@Total@d
MatrixForm[xsls[[2,1]].d.Transpose[xsls[[2,2]]]]
MatrixForm@m


selected=Flatten@Table[If[sparses[[i]]!=denses[[i]],{i},{}],{i,Length@denses}];
(*selected={(*1,3,*)4,5,6,7,8(*,9,10,11*)};*)
(*selected=Range[1,14];*)
selected=Range[1,2];
showSelected=Function[selected,
	Graphics3D[Riffle[RGBColor@@@(Join@@(colorss[[selected]])),Point/@Join@@(pointss[[selected]])]
	,Axes->True,ImageSize->800,AxesLabel->{"x","y","z"}]];
showSelected[{1}]
showSelected[{2}]
showSelected[{3}]


{imgs,sparses,denses,cameras(*,stereos*),colorss,pointss,sfms}=Import@"/s/mc/fountain.mx";
(*One image*)Graphics3D[Riffle[RGBColor@@@(colorss[[1]]),Point/@pointss[[1]]],Axes->True,ImageSize->800]
(*All images*)Graphics3D[Riffle[RGBColor@@@(Join@@(colorss[[;;]])),Point/@Join@@(pointss[[;;]])]
	,Axes->True,ImageSize->800,AxesLabel->{"x","y","z"}]


k=1;img=imgs[[k]];dim=Reverse@ImageDimensions[img];camera=cameras[[k]];sparse=sparses[[k]];
projected={{#[[1]],-#[[2]]}/#[[3]],-#[[3]]}&[camera.Append[#,1]]&/@(Join@@pointss);
(*g=Graphics[Point[projected[[;;,1]]],Axes->True,AxesLabel->StringSplit@"x y",AxesOrigin->{0,0}];
f=Nearest[Rule[({0,1200}+#&/@projected[[;;,1]])/4,projected[[;;,2]]]];
m=-Table[First@f[{i,300-j}],{j,1,300,2},{i,1,400,2}];
disp@m*)
dispDepthmap[laplaceDepthmap[img,sparse],img]

(*Artifacts sometimes arise *)
rules=Select[Round[({(*8dim[[1]]+*)-#[[1,2]],#[[1,1]]})/4]->-#[[2]]&/@projected[[;;]],1<=#[[1,1]]<=2dim[[1]]&&1<=#[[1,2]]<=2dim[[2]]&];
m2=SparseArray[rules,2 dim];m2//MatrixPlot

(*The NNZ is only 0.03, we cannot matrix complete.*)
(*rgb=Table[
	SparseArray[Select[MapThread[With[{pt=camera.Append[#2,1]},Round[(*100+50 *)pt[[;;-2]]/pt[[-1]]/4]->#[[i]]]&,{colorss[[1]],pointss[[1]]},1]
	,((*Print[#[[1,;;]]];*)pointInRectangle[#[[1,;;]],{{1,1},{800,800}}])&]],{i,3}];rgb//showTensor3
LS=With[{D=unfoldTensor[rgb,2]},Rpca2[D,SparseArray[SparseArray[unfoldTensor[rgb,2]]["NonzeroPositions"]->1,Dimensions@D],100]];
Image/@LS*)

(*img2=ImageResize[img,Reverse@Dimensions@m2];
m3=laplaceDepthmap[img2,m2];
(*(*a odd bamboo shoot in the middle*)dispDepthmap[m3,img2]*)
dispDepthmap[MedianFilter[m3,5],img2]*)

(*(*Low-rank completion for depthmap. Hard to deal with regions without 3D points*)
d=m2;mask=SparseArray[SparseArray[d]["NonzeroPositions"]->1,Dimensions@d];
r=Rpca2[d,mask,100];//AbsoluteTiming
r[[1]]//Image//ImageAdjust
dispDepthmap[MedianFilter[Partition[r[[1]],Dimensions@m2][[1,1]],3],img2]*)

(*Juxtaposing seems not help
d=ArrayFlatten@{{m2,ImageData@ColorConvert[img2,"Grayscale"]}};
mask=SparseArray[SparseArray[d]["NonzeroPositions"]->1,Dimensions@d];
r=Rpca2[d,mask,100];//AbsoluteTiming*)


l={1,4};{d1,d2}=denses[[l]];{c1,c2}=cameras[[l]];{i1,i2}=ImageData/@imgs[[l]];Image/@{i1,i2}
(*Graphics3D[Flatten[Table[{RGBColor@@i1[[i,j]],Point@pixelToPoint[d1,c1,i,j]},{i,dim[[1]]},{j,dim[[2]]}]
		~Join~Table[{RGBColor@@i2[[i,j]],Point@pixelToPoint[d2,c2,i,j]},{i,dim[[1]]},{j,dim[[2]]}],2],Axes->True,AspectRatio->1,BoxRatios->1]*)


solveDepthCost=Function[{imgs,depthmaps,cameras,\[Lambda]},Module[{dims=Dimensions/@imgs,ii,jj},
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten@Table[
		{ii,jj}=Round/@pointToPixel[cameras[[2]],pixelToPoint[depthmaps[[1]],cameras[[1]],i,j]];
		If[1<=ii<=dims[[2,1]]&&1<=jj<=dims[[2,2]],imgs[[1,i,j]]-imgs[[2,ii,jj]],0],{i,dim[[1]]},{j,dim[[2]]}]
		,totalVariation2D@depthmaps[[1]]}]];
(*solveDepth=Function[{initD,imgs,initDepthmaps,cameras,\[Lambda],maxIter},Module[{oldD=RandomReal[1,Dimensions@initD],D=0 initD,
		\[Theta]=1.,\[Rho]=0.9,dim=Dimensions@initD,A,u=initD},
	(*invA=Map[If[#==0,0,1/#]&,A,{2}];\[CapitalOmega]A=Map[Boole[#!=0]&,A,{2}];*)
	Do[If[Norm[D-oldD,"Frobenius"]/(Times@@dim)<10^-5,Print[j];Break[],(*Print["s",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		D=(dShrinkageHadamard[\[Theta] A2,A u +B]-B) invA+(1-\[CapitalOmega]A)u;(*Print["d",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		u=tvDenoise[D,\[Theta],3];(*Print["u",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		Print[Norm[rawM-D,"Frobenius"]];
	];\[Theta]*=\[Rho];
	,{j,maxIter}];D
	]];*)


f=Nearest[Rule[({0,8dim[[1]]}+#&/@projected[[;;,1]])/8,projected[[;;,2]]]];
m=-Table[First@f[{i,dim[[1]]-j}],{j,1,dim[[1]],2},{i,1,dim[[2]],2}];
dispDepthmap[m,img]


{docids,rawcCameras}=Import["/tmp/"<>#]&/@{"docids.csv","raw_camera.csv"};docids=Flatten[docids[[;;,;;-2]]];


Normalize@rawcCameras[[Position[docids,#][[1,1]]]][[5;;8]]&/@{"0:Cuw","0:Cug","0:CvA"}
rawcCameras[[1;;3]]


(*dir="/s/tfountain.tt/";model="8LuUdJb7iAM";*)
(*dir="/s/tnasa.t/";(*model="c2zV_NJ3d_Q";*)model="models";*)
dir="/s/tstreet.t/";model="GvXZfvQJ1Uo";
denseLines=Import[dir<>"dense.csv"];
{docids,rawcCameras}=Import[dir<>#]&/@{"docids.csv","camera.csv"};docids=Flatten[docids[[;;,;;-2]]];
load=Function[k,
	docid=denseLines[[k,1]];
	fname=StringSplit[docid,":"][[2]]<>".JPG";rawImg=Import[dir<>"unified_viewer/"<>fname];rawDim=Reverse[ImageDimensions@rawImg];
	dim=Quotient[#,8]&/@rawDim;img=ImageResize[rawImg,Reverse[dim]];
	camera=Partition[docid/.Thread@Rule[docids,rawcCameras[[;;,;;-2]]],4];
	(*stereo=Import[dir<>model<>"/"<>docid<>"_stereo_points.ply","VertexData"];*)
	{sparse,dense}=Function[fname,With[{line=Import[dir<>fname][[k]]},
		ImageData[Image@Partition[line[[2;;-2]],2dim[[2]]]][[;;;;2,;;;;2]]]]/@{"sparse.csv","dense.csv"};
	{rawColors,rawPoints}=importPly[dir<>model<>"/"<>docid<>"_depthmap.ply"];
	mask=First/@SparseArray[Flatten[Table[If[EvenQ[i]&&EvenQ[j],sparse[[i/2,j/2]],0]
		,{i,2Dimensions[sparse][[1]]},{j,2Dimensions[sparse][[2]]}]]]["NonzeroPositions"];
	{colors,points}=#[[mask]]&/@{rawColors,rawPoints};
	Print[{docid}~Join~Prepend[ImageAdjust@Image@#&/@{sparse,dense},img]];
	sfm=Import[dir<>model<>"/"<>model<>"_model.ply","VertexData"];
	(*g=Graphics3D[Point@sfm,AxesLabel->StringSplit@"x y z",ViewPoint->{-10,-10,0},ViewVertical->{0,1,0}];
	g2=Graphics3D[Point@stereo,ViewVertical->{-1,0,0},AxesLabel->StringSplit@"x y z",Axes->True,ViewPoint->{-4,-6,0}];
	{g,g2,disp@dense}*)
	{img,sparse,dense,camera(*,stereo*),colors,points,sfm}
	];
{imgs,sparses,denses,cameras(*,stereos*),colorss,pointss,sfms}=Thread@Table[load[i],{i,Length@denseLines}];
(*Export["/s/mc/island.mx",{imgs,sparses,denses,cameras(*,stereos*),colorss,pointss,sfms}]*)

(*projected={{#[[1]],-#[[2]]}/#[[3]],-#[[3]]}&[camera.Append[#,1]]&/@stereo;
g=Graphics[Point[projected[[;;,1]]],Axes->True,AxesLabel->StringSplit@"x y",AxesOrigin->{0,0}];
projected2={{#[[1]],-#[[2]]}/#[[3]],-#[[3]]}&[camera.Append[#,1]]&/@sfm;
visible=Select[projected2,1600>#[[1,1]]>0&&0>#[[1,2]]>-1200&];
g2=Graphics[Point[visible[[;;,1]]],Axes->True,AxesLabel->StringSplit@"x y",AxesOrigin->{0,0}];
{g,g2,Show[g,g2]}*)
(*f=Nearest[Rule[({0,1200}+#&/@projected[[;;,1]])/4,projected[[;;,2]]]];
m=-Table[First@f[{i,300-j}],{j,1,300,2},{i,1,400,2}];
disp@m*)


k=56;sparse=sparses[[k]];img=imgs[[k]];
trueGray=ImageData@ColorConvert[ImageResize[img,Reverse@Dimensions@sparse],"Grayscale"];
mask=SparseArray[SparseArray[sparse]["NonzeroPositions"]->1.,Dimensions@sparse];
{L,S}=Rpca2WithInit[sparse,mask,sparse,0sparse,400];
sparse2=ArrayFlatten@{{trueGray,sparse}};mask2=ArrayFlatten@{{N@Map[1&,trueGray,{2}],Normal@mask}};
{L2,S2}=Partition[#,Dimensions@L][[1,2]]&/@Rpca2WithInit[sparse2,mask2,sparse2,0sparse2,400];
{disp[L,img],disp[L2,img]}


r=Import@Export["~/m/levin_weiss_code/gg.bmp",
	ColorCombine[Image@Downsample[ImageData@#,10]&/@ColorSeparate@Import@"~/gg.bmp"]]
r2=Import@Export["~/m/levin_weiss_code/ll.bmp",
	ColorCombine[Image@Downsample[ImageData@#,10]&/@ColorSeparate@Import@"~/ll.bmp"]]


dim=128{1,1};numIter=400;img=ImageResize[Import@"/h/m/levin_weiss_code/example.bmp"(*ExampleData[{"TestImage", "Apples"}]*),dim];
(*img=Import@"~/m/levin_weiss_code/example.bmp";*)
rawRgb=ImageData/@ColorSeparate[ImageResize[Import["/h/m/levin_weiss_code/example_res.bmp"],dim]];
imgs=ColorSeparate@img;gray=Mean[ImageData/@imgs];
(*{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim,0.5];{rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};*)
imageToLabels=Function[{img,gray},Module[{mask,D=ImageResize[img,Reverse@Dimensions@gray],marked},
	(*D=img;*)
	marked=ColorConvert[ColorCombine[{Image@gray}~Join~ColorSeparate[D,"LAB"][[2;;3]],"LAB"],"RGB"];
	mask=Table[Map[Boole[Mean[#^2]> Mean[#]^2+0.0001]&,ImageData@marked,{2}],{3}];
	{(ImageData/@ColorSeparate[marked]) mask,mask,Mean[ImageData/@ColorSeparate[marked]]}
	]];
{rgb\[CapitalOmega],\[CapitalOmega],gray}=imageToLabels[(*Import@"ll.bmp"*)Import@"~/m/levin_weiss_code/example_marked.bmp"
	,ImageData[ColorSeparate[img,"LAB"][[1]]]];
Print[{"nonzero ratio",N[Length@SparseArray[\[CapitalOmega]]["NonzeroPositions"]/Times@@Dimensions@\[CapitalOmega]]}];
initL=initS=0rgb\[CapitalOmega];
(*{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.05,5];initS=rgb\[CapitalOmega]-initL;{rgb\[CapitalOmega],\[CapitalOmega]}={initL,n\[CapitalOmega]};*)
{showTensor3@\[CapitalOmega],showTensor3@rgb\[CapitalOmega],showTensor3[rgb\[CapitalOmega] \[CapitalOmega]],showTensor3@initL,showTensor3@initS}
Print[{"nonzero ratio",N[Length@SparseArray[\[CapitalOmega]]["NonzeroPositions"]/Times@@Dimensions@\[CapitalOmega]]}];
(*{L4,S4}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming*)
{L1,S1}=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming
{L4,S4}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming
(*{L5,S5}=RpcaColorTensorSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming*)
(*{L5,S5}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L4,S4,numIter,RpcaColorTensorLaplaceWithInit,2{1,1},1];//AbsoluteTiming*)
(*showTensor3@L5*)
Ls={L1,L4(*,L5*)};
showTensor3/@Ls
{"rgb prediction",(pnorm[(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}


SeedRandom[1003];rawImg=Import@"~/t5.jpg";(*rawImg=ExampleData[{"TestImage", "Apples"}];*)
dim=400{1,1};isRgb=True;{useLocalconsistency,weightedLocalconsistency}={False,True};divisor2=4^2;divisor=Sqrt[divisor2]{1,1};
numIter=400;missingRatio=0.9;img=ImageResize[rawImg,Reverse@dim];
(*We may permuate the image but still recover.*)(*img=Image@RotateRight[ImageData@img,dim 3/4];*)
(*ps=FindPermutation[Range@#,RandomSample@Range@#]&/@dim;
img=ColorCombine[Image@Transpose[Permute[Transpose@Permute[ImageData@#,ps[[1]]],ps[[2]]]]&/@ColorSeparate[img]];*)
(*Image[RotateRight[ImageData@ColorCombine[Image@Permute[#,InversePermutation[p]]&/@#],dim 0/4],ImageSize->400]&/@Ls*)
imgs=ColorSeparate@img;
{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim,missingRatio];{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};initL=initS=0rgb\[CapitalOmega];
(*Import@Export["t.png",#]&@Rasterize@{Image@gray, showTensor3@rgb\[CapitalOmega],showTensor3@L6}*)
Print[{"nonzero ratio",N[Length@SparseArray[\[CapitalOmega]]["NonzeroPositions"]/Times@@Dimensions@\[CapitalOmega]]}];
(*Inpaint[\[CapitalOmega] rgb//CombineRGB,Image[(1-\[CapitalOmega])[[;;,;;Dimensions[\[CapitalOmega]][[1]]]]]]*)(*This hopelessly failed.*)
If[useLocalconsistency,
	{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.05,20];initS=rgb\[CapitalOmega]-initL;
	If[weightedLocalconsistency,{rgb\[CapitalOmega],\[CapitalOmega]}={rgb\[CapitalOmega]+(1-\[CapitalOmega])initL,\[CapitalOmega]+0.1((1-\[CapitalOmega]) n\[CapitalOmega])};
		,{rgb\[CapitalOmega],\[CapitalOmega]}={initL,n\[CapitalOmega]};];];
{(*{L,S}=RpcaColorCoarseWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor];//AbsoluteTiming,*)
(*{f=scaleForRpcaColor[#,1/divisor]&,sample=#[[;;,;;;;divisor[[1]],;;;;divisor[[2]]]]&,g=scaleForRpcaColor[#,divisor]&};
Dimensions/@{f@gray,sample@rgb\[CapitalOmega],sample@\[CapitalOmega],f@initL,f@initS}*)
(*(*Different unfolding expreiment*)
(*(Export["t.png",#];#)&@Rasterize@MapThread[Labeled[ImageResize[ColorCombine[Image/@#],400],#2]&,{Ls,{"AAAI","{1,2,3}","{2,3}","{1,3}","{1,2}","{1}","{2}","{3}"}}]*)
*)
(*{L,S}=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L2,S2}=Rpca2PseudoColorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L3,S3}=Rpca2LabWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L4,S4}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L5,S5}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
{L6,S6}=RpcaColor2WithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
(*{L6,S6}=Rpca2LabWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L3,S3}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L3,S3,numIter];//AbsoluteTiming,
{L5,S5}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L3,S3}=Rpca2PseudoColorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L4,S4}=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,Rpca2PseudoColorWithInit,divisor];//AbsoluteTiming,*)
(*{L4,S4}=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor];//AbsoluteTiming,*)
(*{L5,S5}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor,1];//AbsoluteTiming,*)
(*{L4,S4}=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorLaplaceWithInit,divisor];//AbsoluteTiming,*)
(*{L3,S3}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L5,S5}=RpcaColorTensorSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*{L3,S3}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2,3}];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)

(*{L5,S5}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L6,S6}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor,1];//AbsoluteTiming,*)

(*{L,S}=RpcaColorSeparateWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L2,S2}=RpcaColor2WithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L2,S2,numIter];//AbsoluteTiming,*)
(*{L5,S5}=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor];//AbsoluteTiming,*)
(*{L6,S6}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor,1];//AbsoluteTiming,*)
(*{L7,S7}=RpcaColorRedBlackBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L6,S6,numIter,RpcaColorTensorWithInit,2divisor];//AbsoluteTiming,*)
(*{L8,S8}=RpcaColorRedBlackBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L7,S7,numIter,RpcaColorTensorWithInit,2divisor];//AbsoluteTiming,*)
(*{L4,S4}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,{2,divisor2/2},1];//AbsoluteTiming,
{L5,S5}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L4,S4,numIter,RpcaColorTensorWithInit,{divisor2/2,2},1];//AbsoluteTiming,
{L6,S6}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L5,S5,numIter,RpcaColorTensorWithInit,Sqrt[divisor2]{1,1},1];//AbsoluteTiming,*)
}

Ls={(*L,L2,L4,L5,*)L6(*L,L2,L3,L4,L5,L6,L7,L8*)};
{"rgb prediction",(pnorm[(1-raw\[CapitalOmega])(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}
If[!isRgb,ImageResize[Image@gray,400]]
If[isRgb,ImageResize[ImageResize[rawImg,dim],400{1,1}],Image[#,ImageSize->400]&/@imgs]
If[isRgb,Magnify@ImageResize[ColorCombine[Image/@#],400]&/@Ls,Image[#,ImageSize->400]&/@#&/@Ls]





dim=512{1,1};imgs=Image@Mean[ImageData/@ColorSeparate@ImageResize[Import@#,dim]]&/@{"~/t5.jpg","~/t2.jpg","~/t.jpg"};
numIter=400;missingRatio=0.6;isRgb=False;divisor=2{1,1};
{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim,missingRatio];{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};initL=initS=0rgb\[CapitalOmega];
{
(*Different unfolding expreiment*)
{L,S}=RpcaColor2WithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
(*{L2,S2}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,2,3}];//AbsoluteTiming,
{L3,S3}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2,3}];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,3}];//AbsoluteTiming,
{L5,S5}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,2}];//AbsoluteTiming,
{L6,S6}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1}];//AbsoluteTiming,
{L7,S7}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2}];//AbsoluteTiming,
{L8,S8}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{3}];//AbsoluteTiming,*)
(*Export["t.png",Image[#,ImageSize->300]&/@rgb\[CapitalOmega]]*)
(*(Export["t.png",#];#)&@Rasterize@Join[Image[#,ImageSize->300]&/@imgs,Image[#,ImageSize->300]&/@L2]*)
(*(Export["t.png",#];#)&@Rasterize@MapThread[Labeled[ImageResize[ColorCombine[Image/@#],400],#2]&,{Ls,{"AAAI","{1,2,3}","{2,3}","{1,3}","{1,2}","{1}","{2}","{3}"}}]*)

	(*{L,S}=RpcaColorSeparateWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
(*	{L2,S2}=RpcaColor2WithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
    {L3,S3}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L2,S2,numIter];//AbsoluteTiming,
	{L4,S4}=RpcaColorRedBlackBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L2,S2,numIter,RpcaColorTensorWithInit,2divisor];//AbsoluteTiming,
*)	
(*{L4,S4}=RpcaColorTensorLaplace[gray,rgb\[CapitalOmega],\[CapitalOmega],numIter];//AbsoluteTiming,*)
}
Ls={L(*,L2,L3,L4,L5,L6,L7,L8*)};
{"rgb prediction",(pnorm[(1-raw\[CapitalOmega])(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}
If[!isRgb,ImageResize[Image@gray,400]]
If[isRgb,ImageResize[ImageResize[rawImg,dim],400{1,1}],Image[#,ImageSize->400]&/@imgs]
If[isRgb,ImageResize[ColorCombine[Image/@#],400]&/@Ls,Image[#,ImageSize->400]&/@#&/@Ls]

(*(Export["t.png",#];#)&@BarChart[{0.08610168360977238`,0.08349747897986283`,0.09168359098191457`,0.09875961240437145`,0.0943741769561935`,0.08795915068408834`}
	,ChartLegends->{"{1,2,3}","{2,3}","{1,3}","{1,2}","{2}","{3}"},ChartStyle->"DarkRainbow"
	,PlotRange -> {Automatic, {0.07, 0.1}},PlotRangePadding -> {Automatic, 0},PlotRangeClipping -> True]*)
(*Import@Export["t.png",#]&@Rasterize[{Prepend[ImageResize[Image@#,400]&/@rgb\[CapitalOmega],If[!isRgb,ImageResize[Image@gray,400]]],
ImageResize[Image@#,400]&/@L}]*)


l=Table[Exp[-Max[0,x]],{x,-10,10,0.1}];
ListPlot@l
cwd=ContinuousWaveletTransform[l,GaborWavelet[],Automatic];
WaveletScalogram@cwd
g={If[\[Tau]<0,Cos[5\[Tau]],Cos[10\[Tau]]],Exp[-Max[0,\[Tau]]]}[[1]];
f=Integrate[Exp[-Pi(\[Tau]-t)^2-2Pi I \[Omega] \[Tau]]g,{\[Tau],-1.9143,1.9143}];
Table[Abs[f],{\[Omega],-4,4},{t,-6,6}]//MatrixPlot


(*Finding formula*)

{as,bs,ds}=Array[#,{2,2}]&/@{a,b,d};
r=Map[D[Total@Flatten[Abs[ds (Transpose[as].bs)]],#]&,bs,{2}];r//MatrixForm//Simplify
r[[1,1]]
as.(-ds Sign[ds (Transpose[as].bs)])//MatrixForm


robustrecoverPointChargesCostFunciton=Function[{P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2},
	{Total@#,#}&@{Total@Abs@B,\[Lambda] Total@Abs[\[CapitalOmega] S],\[Mu]1/2 Total[(X-M+S)^2],\[Mu]2/2 Total[(P.X-B)^2],Y1.(M-X-S),Y2.(P.X-B)}];
robustrecoverPointCharges=Function[{Data,\[CapitalOmega]in,maxIter}
		,Module[{M=vec@Data,\[CapitalOmega]=vec@\[CapitalOmega]in,B,X,Zs,S,Y1,Y2,\[Lambda]=1.,\[Mu]1,\[Mu]2,\[Rho]=1.01,P,PT,PTP,dim=Dimensions@Data,norm2},
	PTP=PT.P;B=0 M;X=M;S=0 M;Y1=Y2=0 M;P=poissonMatrix@dim;PT=Transpose[P];
	norm2=SingularValueDecomposition[unVec[M,dim],1][[2,1,1]];\[Mu]2=1.25/norm2;\[Mu]1=\[Lambda] \[Mu]2;
	Do[
		(*If[Mod[j,10]==1,Print[MatrixPlot@Partition[#,n]&/@{B,X,S}]];*)
		(*Print[robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]];*)
		X=LinearSolve[SparseArray[\[Mu]1 sparseIdentityMatrix[Times@@dim]+\[Mu]2 PTP],\[Mu]1(M-S)+\[Mu]2 PT.B+Y1-Y2.P];
		(*Do[
			X-=0.0001(\[Mu]1 X+\[Mu]2 PT.(P.X) - (\[Mu]1(M-S)+\[Mu]2 PT.B+Y1-Y2.P));
		,{10}];*)
		(*Print[{"X",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		B=vec@dShrinkage[1/\[Mu]2,unVec[P.X+Y2/\[Mu]2,dim]];
		(*Print[{"B",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		Zs=M-X+Y1/\[Mu]1;S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu]1,Zs]+(1-\[CapitalOmega])Zs;
		(*Print[{"S",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		Y1+=\[Mu]1 (M-X-S);Y2+=\[Mu]2 (P.X-B);
		{\[Mu]1,\[Mu]2}*=\[Rho];
	,{j,maxIter}];
	unVec[#,dim]&/@{B,X}
	]];
recoverPointChargesCostFunction=Function[{P,M,\[CapitalOmega],B,X},{Total@#,#}&@{Norm[P.X-B],Norm[\[CapitalOmega](X-M)]}];
recoverPointCharges=Function[{Data,\[CapitalOmega]in,maxIter},Module[{M=Flatten@Data,\[CapitalOmega]=Flatten@\[CapitalOmega]in,B,X,\[Eta]=1.,\[Lambda]=1.,\[Rho]=1.05,P,PT,n=Dimensions[Data][[1]]},
	B=0 M;X=M;P=poissonMatrix@{n,n};PT=Transpose[P];
	Do[
		(*If[Mod[j,10]==1,Print[MatrixPlot@Partition[#,n]&/@{B,X}]];*)
		(*X=LinearSolve[\[Lambda] \[CapitalOmega]+\[Eta] PT.P,\[Lambda] M+\[Eta] Transpose[P].B];*)
		Do[
			X-=0.0001(\[Lambda] \[CapitalOmega] X+\[Eta] PT.(P.X) - (\[Lambda] M+\[Eta] PT.B));
		,{10}];
		(*Print[{"X",recoverPointChargesCostFunction[P,M,\[CapitalOmega],B,X]}];*)
		B=dShrinkage[1/\[Eta],P.X];
		(*Print[{"B",recoverPointChargesCostFunction[P,M,\[CapitalOmega],B,X]}];*)
		{\[Eta],\[Lambda]}*=\[Rho];
	,{j,maxIter}];
	Partition[#,n]&/@{B,X}
	]];


n=40;
b=Table[0,{n^2}];b[[Floor[n 2/3] n + Floor[n 2/3]]]=1;b[[Floor[n/3] n + Floor[n/3]]]=-1;b[[Floor[n/4] n + Floor[n 4/5]]]=-1;
A=poissonMatrix@{n,n};
x=Partition[LinearSolve[A,b],n];//AbsoluteTiming (*Takes 9.0s on desktop*)
\[CapitalOmega]=Partition[randomSparseTensor[{1,Length@b},0.1][[1]],n];MatrixPlot/@{x,\[CapitalOmega],x \[CapitalOmega]}
{B,X}=recoverPointCharges[x \[CapitalOmega],\[CapitalOmega],30];
Print[MatrixPlot/@{B,X}]
Norm[(1-\[CapitalOmega])(x-X),"Frobenius"]
(*ListPlot3D[Join@@Table[{i,j,Partition[X,n][[i,j]]},{i,n},{j,n}],PlotRange->All]*)
{B2,X2}=robustrecoverPointCharges[x \[CapitalOmega],\[CapitalOmega],50];
Print[MatrixPlot/@{B2,X2}]
Norm[(1-\[CapitalOmega])(x-X2),"Frobenius"]


ClearAll[a];
A=Table[a[i,j],{i,6},{j,4}];
A2=A.Transpose[A]//.{Times[a_,b_]->!Xor[a,b],Plus[a_,b_]->Or[a,b]};
FindInstance[And@@Map[First,Select[Flatten[MapIndexed[{#1,#2}&,A2,{2}],1],#[[2,1]]!=#[[2,2]]&]],Flatten@A,Booleans]


Framed@DynamicModule[{contents={},end={0,0}},
	EventHandler[Graphics[Dynamic[Flatten[Function[x,{
		Hue[x[[3]]],PointSize[x[[4]]],Point[First@x]}]/@(contents=Map[
		If[#[[1,2]]>=0,
		{#[[1]]-#[[2]],#[[2]]+{0,0.001},#[[3]],#[[4]]},{{#[[1,1]],0},{0,-0.9}#[[2]],#[[3]],#[[4]]}]&,
	  contents]),1]],PlotRange->{{0,1},{0,1}}],
	"MouseDown":>(AppendTo[contents,{MousePosition["Graphics"],{0,0},RandomReal[],RandomReal[{0.1,0.3}]}]),
	"MouseMoved":>(end=MousePosition["Graphics"])]]


Framed@DynamicModule[{contents={},start={0,0},end={0,0}},EventHandler[
	Graphics[Dynamic[{Text[contents,{0.5,0.5}],Arrow[{start,end}]}(*~Join~Map[Arrow,Partition[contents,2]]*)],PlotRange->{{0,1},{0,1}}],
	"MouseDown":>AppendTo[contents,start=MousePosition["Graphics"]],
	"MouseUp":>AppendTo[contents,end=MousePosition["Graphics"]],
	"LeftArrowKeyDown":>(end-={0.01,0}),
	"RightArrowKeyDown":>(end+={0.01,0}),
	"MouseMoved":>(end=MousePosition["Graphics"])
	]]


ListLogPlot[Abs@#,PlotRange->All]&/@{data[[1,;;;;100]],data[[2,;;;;100]]}
Dynamic[ImageCompose[EdgeDetect[#,3,.1],ImageResize[#,Scaled[0.4]],{Left,Bottom},{Left,Bottom}]&@CurrentImage[]]


Dynamic[Refresh[ListLogPlot@Abs[Fourier@(# Take[Flatten@GaussianMatrix[{{Length@#/2,0},Length@#/2}],Length@#])&@
	(Tr/@Partition[cap@getBuffer[],10])],UpdateInterval->0.5]]


{i1,i2}=images;tr=FindGeometricTransform@@images;
{w,h}=ImageDimensions[i2];
tmp=ImagePerspectiveTransformation[i2,tr[[2]],DataRange->Full,PlotRange->{{0,First@tr[[2]][{w,0}]},{0,h}}];
ImageCompose[tmp,{i1,.7},Round@({w,h}/2)]


img=ImageResize[#,ImageDimensions@#/4]&@imgs[[1]];
Manipulate[ImageAdd[
	ImageMultiply[img,0.5],ImageMultiply[CornerFilter[img,r]//ImageAdjust,2]],{r,0,10}]


ker=RandomVariate[NormalDistribution[],{#,#}]&;
Manipulate[Image[#,ImageSize->300]&/@Join[ShowFourier2D@Transpose[Fourier/@Transpose[Fourier/@ImageData@#]],{ker[r]//ArrayPlot,#//ImageAdjust}]&@
	ImageConvolve[First@ColorSeparate@img,ker[r]],{r,1,20}]


ShowFourier2D=Image@RotateLeft[#,Floor[Dimensions[#]/2]]&/@{Abs@#,Arg@#}&;
Manipulate[Image[#,ImageSize->300]&/@Prepend[ShowFourier2D@Transpose[Fourier/@Transpose[Fourier/@ImageData@#]],#]&@
	ImageConvolve[First@ColorSeparate@img,GaussianMatrix[r]],{r,0,20}]


Manipulate[Prepend[ShowFourier2D@Transpose[Fourier/@Transpose[Fourier/@ImageData@#]]&@#,#]&@
  Image@Graphics@{Opacity[0.5],Rotate[Rectangle[{0.5,0.5},{0.7,0.7}],d Degree],White,Rectangle[{0,0}]},{d,0,90}]


Manipulate[ker=GaussianMatrix[r];
{ker//MatrixPlot,Image[#,ImageSize->500]&@ImageConvolve[img,ker],
	Image[#,ImageSize->500]&@ImageDeconvolve[ImageConvolve[img,ker],ker]},{r,0,10}]


With[{r=3},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-1,1},PlotStyle->{Opacity[1],Texture[img]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->1000(*,ViewRange->{3,1000}*)]]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-1,1},PlotStyle->{Opacity[1],Texture[Import@"~/Downloads/squashedhead2.jpg"]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,(*ViewPoint->{0,0,2},*)ImageSize->300]]


With[{r=1},
ParametricPlot3D[{r Cos[v] Cos@u,r Cos[v] Sin@u, r Sin@v},{u,0,Pi},{v,0,Pi},PlotStyle->{Opacity[0.8],Texture@ImageRotate[Import@"Downloads/sphere_rama.jpg",-90Degree]},Mesh->None,(*PlotRange->All,*)Lighting->"Neutral"
	,(*ViewPoint->{0,0,2},*)ImageSize->1000(*,ViewPoint->{0,0,0}*),(*TextureCoordinateFunction->({#[[1]],#[[3]]}&),*)Boxed -> False]];


Graphics[Line@Map[#[[;;2]]/#[[3]]&@(RotationMatrix[0,{1,1,1}].(#+{3,3,10}))&,
	Flatten[Table[Insert[#,j,i],{i,3},{j,{-1,1}}]&/@Tuples[{1,-1},2],1],{2}]]


dfTimeMap=Function[fname,Module[{l},l=Import[fname];Print[{fname,l//Length}];SortBy[l[[2;;,{1,-1}]],First]]];
ls=dfTimeMap/@Sort@FileNames["~/Downloads/2013*.csv"];
Thread@ls
Function[l,With[{ts=Last/@l},Sort[ts][[Floor[Length[ts]/2]]]]]/@ls


img=Import@"d/IMG_5319.jpg"
ImageResize[img,800]
With[{method=StringTake[StringSplit[#][[1]],{2,-2}]},Print/@{method,ImageResize[img,800,Resampling->method]}]&/@Import["d/t.csv","Lines"];


csvToTable=Function[fname,{#[[1]],fname->#[[6]]}&/@Select[Import[fname],Length@#>1&&#[[2]]=="success"&]];
fnames=FileNames["d/2013-04*.csv"][[-5;;]];
tbs=csvToTable/@fnames;
l=Select[{#[[1]]}~Join~#[[2;;,2]]&/@JoinTablesByKey[tbs,True],And@@(NumberQ/@#[[2;;]])&];l//TableForm
Print["Mean"];Mean/@N@Rest@Transpose[l]
(Export["d/moremachines.png",#];#)&@ListLinePlot[Transpose@SortBy[l[[;;,2;;]],#[[1]]&],
	AxesLabel->{"instance","time(millisecond)"},PlotLegends->fnames,PlotRange->All]
Print["Quantile[#,0.95]"];Quantile[#,0.95]&/@N@Rest@Transpose[l]
Print["Quantile[#,0.75]"];Quantile[#,0.75]&/@N@Rest@Transpose[l]
Print["Max"];Max/@N@Rest@Transpose[l]


Quantile[#,0.75]&@N@Select[Last/@(JoinTablesByKey[tbs,False][[;;,2]]),NumberQ]


(*Quantile[#,0.75]&*)Max[ToExpression@StringSplit[#,{"tours generatedsuccessfully ","s after published"}][[1,2]]&/@Import@"d/t.csv"]


queries=(First/@Import@"d/queries.csv");queries//Length
timestamped=MapIndexed[{#2[[1]],#}&,queries];timestamped[[;;3]]
queryToReuseTimeDistance={#[[1,2]],Differences@#[[;;,1]]}&/@GatherBy[timestamped,Last];
noReuseQueries=Select[queryToReuseTimeDistance,#[[2]]=={}&][[;;,1]];noReuseQueries//Length
reuseQueries=Select[queryToReuseTimeDistance,#[[2]]!={}&];reuseQueries//Length
weights=Dispatch[Rule@@@Join[{#[[1]],Length@#[[2]]}&/@reuseQueries,{#,-1}&/@noReuse]];
potentialReuses=Flatten[reuseQueries[[;;,2]]];{Length@potentialReuses,N@Length@potentialReuses/Length@queries}
(*With[{DD=HistogramDistribution[potentialReuses]},Plot[CDF[DD,x],{x,0,10000}]]*)
getBestReuseQueries=Function[cacheSize,#[[;;Min[Length@#,cacheSize]]]&@Reverse[SortBy[{#[[1]],Length@#[[2]]}&/@reuseQueries,Last]]];
Function[cacheSize,{"byTimestamp"->N@Length@Select[potentialReuses,#<cacheSize&]/Length@queries,
		"byOracle"->N@Total@getBestReuseQueries[cacheSize][[;;,2]]/Length@queries}]/@
	{100,300,1000,3000,10000,100000}
goodCacheSize=1000;
bestReuseQueries=getBestReuseQueries[goodCacheSize];bestReuseQueries[[;;10,1]]
positives=bestReuseQueries[[;;,1]];positives//Length
negatives=noReuseQueries;negatives//Length
(*positives={#[[1]],Length@Select[#[[2]],#<goodCacheSize&]}&/@reuseQueries;positives//Length
negatives={#,0}&/@noReuseQueries;negatives//Length*)


getFeatures=Function[x,With[{word=ToString@x,chars=StringSplit[ToString@x,""]},
	{StringLength@word,Length@StringSplit[word],Boole@StringMatchQ[word, RegularExpression["[[:digit:]]+"]]}]];
features=getFeatures/@positives[[;;10]];


bestEffortToNumber=With[{n=First@ImportString[#,"List"]},If[NumberQ@n,n,#]]&;
spaceSeparatedTableToCsv=Map[bestEffortToNumber,StringSplit[#]&/@#,{2}]&;
Export["d/t2.csv",spaceSeparatedTableToCsv[Import["d/t.csv","Lines"]]]


medianFillIn=Function[{m,radius},With[{mean=Total@Flatten@m/Length@Flatten@m,dim=Dimensions@m},
	Table[If[m[[i,j]]==0,
			If[Length@#>0,Median[m[[#[[1]],#[[2]]]]&/@#],mean]&@Select[Join@@Table[{i+ii,j+jj},{ii,-radius,radius,1},{jj,-radius,radius,1}],
				(#[[1]]>=1&&#[[1]]<=dim[[1]]&&#[[2]]>=1&&#[[2]]<=dim[[2]]&&m[[#[[1]],#[[2]]]]!=0)&]
			,m[[i,j]]],
		{i,dim[[1]]},{j,dim[[2]]}]]];
medianFillIn[m mask,2]//Image


m=ImageData@img;dm=Dimensions[m];
f1=Clip[Round[#],{1,dm[[1]]}]&;
f2=Clip[Round[#],{1,dm[[2]]}]&;
Table[With[{ii=i/2,jj=j/2},
	m[[f1[ii],f2[jj]]]]
		,{i,2 dm[[1]]},{j,2 dm[[2]]}]//Image
Table[With[{ii=i/2,jj=j/2},
	Mean@{m[[f1[ii],f2[jj]]],m[[f1[ii-1],f2[jj]]],m[[f1[ii+1],f2[jj]]],m[[f1[ii],f2[jj-1]]],m[[f1[ii],f2[jj+1]]]}]
		,{i,2 dm[[1]]},{j,2 dm[[2]]}]//Image
Table[With[{ii=i/2,jj=j/2},
	Median@{m[[f1[ii],f2[jj]]],m[[f1[ii-1],f2[jj]]],m[[f1[ii+1],f2[jj]]],m[[f1[ii],f2[jj-1]]],m[[f1[ii],f2[jj+1]]]}]
		,{i,2 dm[[1]]},{j,2 dm[[2]]}]//Image
Table[With[{ii=i/2,jj=j/2},
	m[[f1[ii],f2[jj]]]]
		,{i,2 dm[[1]]},{j,2 dm[[2]]}]//Image


rawImg=Import["~/t.jpg"];numIter=100;
mask = maskFromString[ExampleData[{"Text", "TaoTehChingChinese"}],15];mask//Image
dim=Dimensions@mask;
img=Image@Mean[ImageData/@ColorSeparate@ImageResize[rawImg,Reverse@dim]];
Print[{L,S}=Rpca2[ImageData[img] mask,mask,numIter];//AbsoluteTiming];
Print[X=tnnRpca[ImageData[img] mask,mask,10,numIter,30,"Apgl"];//AbsoluteTiming];
Norm[(1-mask)(#-ImageData[img]),"Frobenius"]&/@{L,X}


{m,n}={100,200};rank=15;M=RandomVariate[NormalDistribution[],{m,rank}].RandomVariate[NormalDistribution[],{rank,n}];
Function[\[Sigma],
	B=M+\[Sigma] RandomVariate[NormalDistribution[],{m,n}];\[CapitalOmega]=RandomSparseMatrix[m,n,0.7];
	Print[{L,S}=Rpca2[B \[CapitalOmega],\[CapitalOmega],200];//AbsoluteTiming];
	Print[X=tnnRpca[B \[CapitalOmega],\[CapitalOmega],5,200,rank,"Admm"];//AbsoluteTiming];
	Print[X2=tnnRpca[B \[CapitalOmega],\[CapitalOmega],5,200,rank,"Apgl"];//AbsoluteTiming];
	{\[Sigma],Norm[(1-\[CapitalOmega])(#-M),"Frobenius"]&/@{L,X,X2}}]/@{1,0.1,0.01}//MatrixForm


nonNegativeInitialize=Function[{eps,dims},(eps+Abs[#])&/@RandomVariate[NormalDistribution[],dims]];
NmfColor2=Function[{Gray,Rgb,\[CapitalOmega],iter,method,r,\[Mu]},Module[{G=Gray,V=Rgb,Y=SparseArray[\[CapitalOmega]],W,H,K,KK,GK,m,n,residues={},nzs,V2,eps=N[10^-6],debug=PrintTemporary[#]&},
	(*V is m-by-3n. G is gray, V is RGB, \[CapitalOmega] is nonzeros*)
	(*nzs can be zeros*)nzs=Append[{##},V[[##]]]&@@@Y["NonzeroPositions"];{m,n}=Dimensions[G];
	{W,H}=nonNegativeInitialize[eps,#]&/@{{m,r},{r,3n}};K=Join@@Table[IdentityMatrix[n],{3}]/3;KK=K.Transpose[K];GK=G.Transpose[K];
	Module[{recordResidues=Function[{},residues=Append[residues,Norm[W.H-rgb,"Frobenius"]/Norm[rgb,"Frobenius"]]]},
	recordResidues[];
	Do[
	  Switch[method,"F",
			debug[V2=SparseArray[{#,#2}->W[[#]].H[[;;,#2]]&@@@nzs,{m,3n}];//AbsoluteTiming];
			debug[W=W (V.SparseArray[Transpose@H]+\[Mu] GK.Transpose[H]) /
				(eps + V2.SparseArray[Transpose@H]+\[Mu] W.((H.KK).Transpose[H]));//AbsoluteTiming];
			recordResidues[];
			debug[V2=SparseArray[{#,#2}->W[[#]].H[[;;,#2]]&@@@nzs,{m,3n}];//AbsoluteTiming];
			debug[H=H (SparseArray[Transpose@W].V+\[Mu] Transpose[W].GK) /
				(eps + SparseArray[Transpose@W].V2+\[Mu] (Transpose[W].W).(H.KK));//AbsoluteTiming];
			recordResidues[];,
			"KL",
			debug[V2=SparseArray[{#,#2}->#3/(eps+W[[#]].H[[;;,#2]])&@@@nzs,{m,3n}];//AbsoluteTiming];
			debug[W=W (V2.SparseArray[Transpose@H]+\[Mu] GK.Transpose[H]) /
				(eps + Y.SparseArray[Transpose@H]+\[Mu] W.((H.KK).Transpose[H]));//AbsoluteTiming];
			recordResidues[];
			debug[V2=SparseArray[{#,#2}->#3/(eps+W[[#]].H[[;;,#2]])&@@@nzs,{m,3n}];//AbsoluteTiming];
			debug[H=H (SparseArray[Transpose@W].V2+\[Mu] Transpose[W].GK) /
				(eps + SparseArray[Transpose@W].Y+\[Mu] (Transpose[W].W).(H.KK));//AbsoluteTiming];
			recordResidues[];,
			_,Abort[]];
	,{iter}];
	{W,H,residues}
	]]];
testNmfColor2=Function[{rawImg,imageDimensions,missingRatio,numIter,method,rank,\[Mu]},Module[{W,H,residues,gray(*,rgb*),\[CapitalOmega]},
	{gray,rgb,\[CapitalOmega]}=prepareDataRpcaColor[rawImg,imageDimensions,missingRatio];
	Print[{W,H,residues}=NmfColor2[gray,\[CapitalOmega] rgb,\[CapitalOmega],numIter,method,rank,\[Mu]];//AbsoluteTiming];
	Print[residues];
	CombineRGB[W.H]
	]];
n=300;testNmfColor2[Import@"~/t.jpg",{n,n},0.9,100,"KL",Floor[n 0.9],0.5]


nonNegativeInitialize=Function[{eps,dims},(eps+Abs[#])&/@RandomVariate[NormalDistribution[],dims]];
NonNegativeMatrixFactorization=Function[{V,r,niter,method},Module[{m,n,residues={},WH,eps=N[10^-6]},{m,n}=Dimensions[V];
	WH=Nest[Module[{W=#[[1]],H=#[[2]]},
		residues=Append[residues,Norm[V-W.H,"Frobenius"]/Norm[V,"Frobenius"]];
		Switch[method,"KL",W=W (((Total[H,{2}])^-1 #)&/@((V/(W.H)).Transpose[H]));H=H ((Total[Transpose[W],{2}])^-1 (Transpose[W].(V/(W.H))));,
			"F",W=W (V.Transpose@H) / (W.(H.Transpose@H));H=H (Transpose@W.V) / ((Transpose@W.W).H);,
			_,Abort[]];
		{W,H}
	]&,nonNegativeInitialize[eps,#]&/@{{m,r},{r,n}},niter];
	{WH[[1]],WH[[2]],ListPlot[residues,PlotRange->All]}
	]];
dirichletMixture=Function[{topics,corpusSize,showMethod},Module[{numTopic,dim,numVoc},
	{numTopic,numVoc}=Dimensions@topics;dim=Sqrt[numVoc];
	{Array[Append[#,1-Tr@#].topics&@RandomVariate[DirichletDistribution[Array[1&,numTopic]]]&,corpusSize],
		Tooltip[showMethod[#],MatrixForm@Chop@#]&@Partition[#,dim]&}
	]];
testNmf=Function[{topics,corpusSize,rank,maxIter,showMethod},Module[{Y,X,unusedP,showOne,A},
	{A,showOne}=dirichletMixture[topics,corpusSize,showMethod];	
	Print[{Y,X,unusedP}=NonNegativeMatrixFactorization[A,rank,maxIter,"KL"];//AbsoluteTiming];
	Print[Norm[A-Y.X,"Frobenius"]/Norm[A,"Frobenius"]];
	{showOne/@topics,showOne/@X,
		showOne/@Transpose[SingularValueDecomposition[A,Min[numTopic,MatrixRank@A]][[3]]]}
	]];
(*example from Finding scientific topics*)
dim=5;numTopic=10;
topics=255. Join[Table[Flatten@Table[Boole[i==k],{i,dim},{j,dim}],{k,dim}],
	Table[Flatten@Table[Boole[j==k],{i,dim},{j,dim}],{k,dim}]];
(Export["~/t2.jpg",#];#)&@TableForm@testNmf[topics,2000,numTopic,200,MatrixPlot]


img=Import@"~/d/twelve_tatoos.jpg";
idim={801,960};
segments=MapThread[Partition[Table[# i /#2 + 1, {i,0,#2}],2,1]&,{idim,{3,4}}];
dim=30;
imgs=Join@@Map[Image@Mean[ImageData/@ColorSeparate@ImageResize[#,{dim,dim}]]&@
	ImageTake[img,#[[1]],#[[2]]]&,Outer[List,segments[[1]],segments[[2]],1],{2}]


topics=Flatten@ImageData@#&/@imgs;topics//Dimensions
testNmf[topics,4000,Length@topics,400,ImageAdjust@Image[#]&]


dim=3;preM=RandomVariate[NormalDistribution[],{dim,dim}];
M=Transpose[preM].preM;M//MatrixForm
SingularValueList@preM
Sqrt@Eigenvalues@M
Eigenvalues@M
v1=Normalize@RandomReal[1,dim];
history=NestList[Function[x,Module[{v1,v0,b1,w,a,b2,v2,unusedA},
	{v1,v0,b1,unusedA}=x;w=M.v1;a=w.v1;w=w-a v1-b1 v0;b2=Norm@w;v2=w/b2;{v2,v1,b2,a}
	]],{v1,0 v1,0,0},dim]
(*(M.#[[1]]/#[[1]])&/@history*)
V=Transpose[history[[;;-2,1]]]
T=With[{bs=history[[2;;-2,3]]},Normal@SparseArray[{Band[{1,1}]->history[[2;;,4]],
	Band[{2,1}]->bs,Band[{1,2}]->bs},{dim,dim}]];T//MatrixForm
Eigenvalues@T


imgs=ImageResize[Import@#,300]&/@FileNames["/s/island/*.JPG"]


extractPos=Function[{n,matrix},Map[If[Head@#===Missing,0,#[[n]]]&,matrix,{2}]];
tracks=ImageFeatureTrack[imgs];
m=extractPos[1,tracks]~Join~extractPos[2,tracks];m//MatrixForm
\[CapitalOmega]=Map[If[#!=0,1,0]&,m,{2}];
{L,S}=Rpca2[m,\[CapitalOmega],100];
L//MatrixForm
xys=MapThread[List,Partition[L,Length[L]/2],2];xys//MatrixForm
Table[ShowImagePoints[imgs[[i]],xys[[i]],3],{i,Length@imgs}]


m=Mean[ImageData/@ColorSeparate@ImageResize[Import["~/t.jpg"],500]];Image@m
{U,S,V}=SingularValueDecomposition[m,Min@Dimensions@m];
(*U.S.Transpose[V]//Image//ImageAdjust*)
{#,U.DiagonalMatrix[#[Diagonal[S]]].Transpose[V]//Image//ImageAdjust//Magnify[#,4]&}&/@
	{#&,ProductLog[Exp[#]]&,#^3&,MapIndexed[If[#2[[1]]==1,#,0]&,#]&,Sqrt,If[#>1,#-1,0]&/@#&}


Dm=Mean[ImageData/@ColorSeparate[ImageResize[Import@"d/yuanshan.jpg",500]]];
{L,S}=Rpca[Dm,1./Sqrt@Max@Dimensions@Dm,1000];
Magnify[#,4]&@ImageAdjust@Image@#&/@{Dm,L,S}


Export["/s/movielens/ml-100k/u.data.csv",ToExpression/@StringSplit/@Import["/s/movielens/ml-100k/u.data","Lines"]]


ratings=Import["/s/movielens/ml-100k/u.data.csv"];
all=N@SparseArray[#[[;;2]]->#[[3]]&/@ratings];
SeedRandom[1003];trainRatio=4/5;trainRatings=RandomSample[ratings,Floor[Length@ratings trainRatio]];testRatings=Complement[ratings,trainRatings];
{trainM,testM}=N@SparseArray[#[[;;2]]->#[[3]]&/@#,Dimensions@all]&/@{trainRatings,testRatings};
nnzAdd=Function[{trainM,x},SparseArray[trainM["NonzeroPositions"]->(trainM["NonzeroValues"]+x),Dimensions@trainM]];
meanTrainM=Mean@trainM["NonzeroValues"];
(*{trainM,testM}=nnzAdd[#,-meanTrainM]&/@{trainM,testM};meanTrainM=Mean@trainM["NonzeroValues"];*)
evalPredicted=Function[{predicted,testM},Sqrt@Mean[(Part[predicted,#[[1]],#[[2]]]-Part[testM,#[[1]],#[[2]]])^2&/@testM["NonzeroPositions"]]];
evalPredicted[Map[meanTrainM&,testM,{2}],testM]
(*scores={};{L,S}=Rpca2[trainM,SparseArray[trainM["NonzeroPositions"]->1.,Dimensions@trainM],1000];*)


dims=Dimensions@trainM;mean=Mean@trainM["NonzeroValues"];{P,Q}=RandomReal[5,#]&/@dims;
iterF=Function[{trainM,Q,mean},With[{nzs=#["NonzeroValues"]},If[Length@nzs==0,mean,(Total@nzs-Total@Q[[#["NonzeroPositions"][[;;,1]]]])/Length@nzs]]&/@trainM];
Do[
	Print[P=iterF[trainM,Q,mean];//AbsoluteTiming];
	Print@evalPredicted[Outer[Plus,P,Q],testM];
	Print[Q=iterF[Transpose@trainM,P,mean];//AbsoluteTiming];
	Print@evalPredicted[Outer[Plus,P,Q],testM];
	,{10}]
nnzAddMatrix=Function[{sparseM,M},(*Only add to nnz of M1*)
	SparseArray[sparseM["NonzeroPositions"]->(sparseM["NonzeroValues"]+(Part[M,#[[1]],#[[2]]]&/@sparseM["NonzeroPositions"])),Dimensions@sparseM]];
{trainM2,testM2}=nnzAddMatrix[#,-Outer[Plus,P,Q]]&/@{trainM,testM};


(trainM-trainM2)[[;;10,;;10]]//Normal//MatrixForm
trainM[[;;10,;;10]]//Normal//MatrixForm
trainM2[[;;10,;;10]]//Normal//MatrixForm
testM[[;;10,;;10]]//Normal//MatrixForm
testM2[[;;10,;;10]]//Normal//MatrixForm
(trainM.Transpose[trainM])[[;;10,;;10]]//Normal//MatrixForm
(trainM2.Transpose[trainM2])[[;;10,;;10]]//Normal//MatrixForm
P[[;;10]]
Q[[;;10]]
Outer[Plus,P,Q][[;;10,;;10]]


nnzAddMatrix=Function[{sparseM,M},(*Only add to nnz of M1*)
	SparseArray[sparseM["NonzeroPositions"]->(sparseM["NonzeroValues"]+(Part[M,#[[1]],#[[2]]]&/@sparseM["NonzeroPositions"])),Dimensions@sparseM]];
sgdMatrixFactorization=Function[{Vin,\[CapitalOmega],rank,maxIter,\[Lambda],learnRate},Module[{V=N@SparseArray@Vin,Vt,W,H,bW,bH,m,n,r=rank,Err,\[Alpha]=learnRate,mean},
	{m,n}=Dimensions[V];{bW,bH}=Table[0.,{#}]&/@{m,n};mean=Mean[V["NonzeroValues"]];
	{W,H}=RandomReal[1.,#]&/@{{m,r},{r,n}};
	Do[
		Err=nnzAddMatrix[V,-W.H-Outer[Plus,bW,bH]];
		bW+=\[Alpha](Total[Err,{2}]-\[Lambda] bW);(*bW=iterF[nnzAddMatrix[V,-W.H],bH,mean];*)
		(*Print[evalPredicted[W.H+Outer[Plus,bW,bH],testM]];*)
		(*Err=nnzAddMatrix[V,-W.H-Outer[Plus,bW,bH]];*)
		bH+=\[Alpha](Total@Err-\[Lambda] bH);(*bH=iterF[Transpose@nnzAddMatrix[V,-W.H],bW,mean];*)
		(*Print[evalPredicted[W.H+Outer[Plus,bW,bH],testM]];*)
		Err=nnzAddMatrix[V,-W.H-Outer[Plus,bW,bH]];
		W+=\[Alpha](Err.Transpose[H]-\[Lambda] W);
		Print[evalPredicted[W.H+Outer[Plus,bW,bH],testM]];
		Err=nnzAddMatrix[V,-W.H-Outer[Plus,bW,bH]];
		H+=\[Alpha](Transpose[W].Err-\[Lambda] H);
		Print[evalPredicted[W.H+Outer[Plus,bW,bH],testM]];
		\[Alpha]=0.99\[Alpha];
		,{iter,maxIter}];{W,H,bW,bH}
]];
r=sgdMatrixFactorization[trainM,SparseArray[trainM["NonzeroPositions"]->1.,Dimensions@trainM],5,200,1,0.001];
evalPredicted[r[[1]].r[[2]]+Outer[Plus,r[[3]],r[[4]]],testM]


(*SeedRandom[1003];*)
maxIter=30;m=RandomReal[1,{60,800}];numColumns=5;rank=4;
Print["NaiveFirstColumns"];cols=Range@numColumns;Cm=m[[;;,cols]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
Print["SvdApprox"];Norm[m-SvdApprox[m,rank],"Frobenius"]/Norm[m,"Frobenius"]
Print["AdaptiveSampling"];
cols=columnBasedDecompositionAdaptiveSampling[m,rank,numColumns,maxIter];
Cm=m[[;;,cols]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
Print["DualSet"];
{cols,Cm}=columnBasedDecompositionDualSet[m,rank,numColumns,maxIter];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
(*qrCols=Function[{m,k},Module[{Q,R,P},{Q,R,P}=QRDecomposition[m,Pivoting->True];First/@Position[Normal@Transpose[P[[;;,;;k]]],x_/;x!=0]]];*)
(*cols=qrCols[m,17];Cm=m[[;;,cols]];
Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
ListLinePlot[First/@Position[Normal@Transpose[gP],x_/;x!=0],PlotRange->All]*)
Print["Block"];
cols4=columnBasedDecompositionBlock[m,rank,numColumns,10,maxIter];
Cm=m[[;;,cols4]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]


(*name="Tony_Blair";imgs=Import/@FileNames["/s/aligned_faces/lfw_funneled/"<>name<>"/*.jpg"];*)
(*imgs=Import/@FileNames["/s/desk/*.JPG"];*)
imgs=Import/@FileNames["/s/memegens/*.jpg"];
imgSize=50;rank=4;numColumns=4;maxIter=10;
m=Transpose[Flatten@ImageData@ImageResize[ColorSeparate[#][[2]],imgSize{1,1}]&/@imgs];
Print["AdaptiveSampling"];
cols=columnBasedDecompositionAdaptiveSampling[m,rank,numColumns,maxIter];//AbsoluteTiming
(*Norm[mt-SvdApprox[mt,Length@cols],"Frobenius"]/Norm[mt,"Frobenius"]*)
Cm=m[[;;,cols]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
Print["NaiveFirstColumns"];
cols2=Range@numColumns;Cm=m[[;;,cols2]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
Print["Block"];
cols4=columnBasedDecompositionBlock[m,rank,numColumns,2,maxIter];
Cm=m[[;;,cols4]];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]
(*{cols3,Cm}=columnBasedDecompositionDualSet[m,rank,numColumns,maxIter];Norm[m-Cm.PseudoInverse[Cm].m,"Frobenius"]/Norm[m,"Frobenius"]*)
(Export["t.png",Rasterize@#];#)&[imgs[[cols]]]


(*http://www.cs.rpi.edu/~drinep/RandNLA/slides/Boutsidis_RandNLA@FOCS_2012.pdf*)
(*http://arxiv.org/pdf/1103.0995.pdf*)
dualSetSparsification=Function[{U,V,r},(*U is m*n, V is k*n, s is m*)
	Module[{s,k,n,A,As,deltaU,l,\[Phi],\[Lambda],Wt,lb,ub,vtw,jOld=0,t,j},
	{k,n}=Dimensions@V;s=Array[0&,n];A=Array[0&,{k,k}];deltaU=Norm[U,"Frobenius"]^2/(1-Sqrt[k/r]);
	If[r<=k,Throw["r must > k"]];
	{\[Lambda],Wt}=Eigensystem@A;\[Phi]=Function[l,Total[1/(\[Lambda]-l)]];
	Do[(*Print[{"\[Tau]",\[Tau]}];*)l=\[Tau]-Sqrt[r k];
		Do[
			j=Mod[jOld+idx,n]+1;(*Print@{"j",j};*)
			With[{wtv=Wt.V[[;;,j]],vj=V[[;;,j]],uj=U[[;;,j]]},
			vtw=wtv;
			ub=vtw.DiagonalMatrix[(\[Lambda]-(l+1))^-2/(\[Phi][l+1]-\[Phi][l])-(\[Lambda]-(l+1))^-1].wtv;
			lb=Total[uj^2]/deltaU;
			(*Print[{lb,ub}];*)
			If[lb<ub,
				(*Print[{"found j",j}];*)
				t=2/(lb+ub);s[[j]]+=t;A+=t Outer[Times,vj,vj];jOld=j;
				{\[Lambda],Wt}=Eigensystem@A;\[Phi]=Function[l,Total[1/(\[Lambda]-l)]];
(*				With[{S=(1-Sqrt[k/r])/r s},
					Print[{"After",SingularValueList[V.DiagonalMatrix@Sqrt@S],N[1-Sqrt[k/r]],Norm[U.DiagonalMatrix@Sqrt@S,"Frobenius"],Norm[U,"Frobenius"]}]];*)
				Break[];
			]]
		,{idx,0,n-1}]
	,{\[Tau],0,r-1}];
	(1-Sqrt[k/r])/r s
	]];
(*SeedRandom[1003]
n=10;m=10;k=4;r=5;{m,n,k,r}
{U,S,V}=SingularValueDecomposition[RandomReal[1,{n,n}],k];
V1=Transpose[V];V1//Dimensions
Chop[V1.Transpose[V1]]
U=RandomReal[1,{m,n}];U//Dimensions
S=dualSetSparsification[U,V1,r]
{SingularValueList[V1.DiagonalMatrix@Sqrt@S][[k]],N[1-Sqrt[k/r]],Norm[U.DiagonalMatrix@Sqrt@S,"Frobenius"],Norm[U,"Frobenius"]}*)
(*http://tocmirror.cs.tau.ac.il/articles/v002a012/v002a012.pdf*)
columnBasedDecompositionAdaptiveSampling=Function[{Data,rank,numColumns,maxIter},Module[{M=Data,V,weights,Cm,Y},
	V=SingularValueDecomposition[M,rank][[3]];
	weights=Sqrt[Total[#^2]]&/@V;
	(*Print@weights;*)
	SortBy[Table[RandomSample[weights->Range[Dimensions[M][[2]]],numColumns],{maxIter}],
		Norm[M-M[[;;,#]].PseudoInverse[M[[;;,#]]].M,"Frobenius"]&][[1]]
	]];
columnBasedDecompositionDualSet=Function[{Data,rank,numColumns,maxIter},Module[{M=Data,V,weights,Cm,Y,S,C1,C2,E,cols},
	If[4rank>=numColumns,Throw["4rank must < numColumns"]];
	V=SingularValueDecomposition[M,rank][[3]];
	S=dualSetSparsification[M-M.V.Transpose[V],Transpose@V,4 rank];C1=M.DiagonalMatrix[S];E=M-C1.PseudoInverse[C1].M;
	weights=Total[#^2]&/@Transpose[E];
	(*Print@weights;*)
	cols=SortBy[Table[RandomSample[weights->Range[Dimensions[M][[2]]],numColumns-4rank],{maxIter}],
		With[{C=ArrayFlatten@{{M[[;;,#]],C1}}},Norm[M-C.PseudoInverse[C].M,"Frobenius"]]&][[1]];
	{Join[cols,First/@Position[S,x_/;x!=0]],ArrayFlatten@{{M[[;;,cols]],C1}}}
	]];
columnBasedDecompositionBlock=Function[{Data,rank,numColumns,numBlocks,maxIter},Module[{M,k=numColumns,t=numBlocks,m,n,n2,Ms,columns,B},
	{m,n}=Dimensions@Data;
	If[Floor[n/t]<k,Throw["Floor[n/t] must >= k"]];
	n2=t Floor[(n+t-1)/t];M=ArrayFlatten@{{Data,Array[0&,{m,n2-n}]}};
	columns=Flatten@Table[With[{mat=M[[;;,(i-1) n2/t+1;;i n2/t]]},
		With[{pivoting=QRDecomposition[SingularValueDecomposition[mat,k][[3]][[;;,;;k]],Pivoting->True][[3]]},
		Position[Normal@#,1][[1,1]]&/@(Transpose[pivoting][[;;k]])+(i-1) n2/t
		]
	],{i,t}];
	(*Print@columns;*)
	B=M[[;;,columns]];
	columnBasedDecompositionAdaptiveSampling[B,rank,numColumns,maxIter]
	]];


cxApprox=Function[{m,k},Module[{cols,Cm},cols=columnBasedDecompositionAdaptiveSampling[m,k,k,20];Cm=m[[;;,cols]];{Cm,PseudoInverse[Cm].m}]];
cxColsRows=Function[{m,k},Module[{cols,Cm,rows},cols=columnBasedDecompositionAdaptiveSampling[m,k,k,20];Cm=m[[;;,cols]];
	rows=columnBasedDecompositionAdaptiveSampling[Transpose[m],k,k,20];{cols,rows}]];


m=Mean[ImageData/@ColorSeparate[ImageResize[Import@"~/t.jpg",500{1,1}]]];
rank=50;
{Cm,Xm}=cxApprox[m,rank];{Rm,Xm}=Transpose/@cxApprox[Transpose[m],rank];
Um=PseudoInverse[Cm].m.PseudoInverse[Rm];
Cm.Um.Rm//Image
SvdApprox[m,rank]//Image


(*Important columns of a image*)
m.DiagonalMatrix[Sqrt@Total[#^2]&/@SingularValueDecomposition[m,10][[3]]]//Image//ImageAdjust


{cols,rows}=cxColsRows[m,10]
SparseArray[(Join@@Outer[List,rows,cols,1])->1,Dimensions@m]//Image//ImageAdjust


numColumns=Floor[Sqrt[1-missingRatio] Dimensions[gray][[2]]];
cols=columnBasedDecompositionAdaptiveSampling[gray,numColumns,numColumns,20];
\[CapitalOmega]1=N@Normal@SparseArray[Join@@(Function[col,{#,col}&/@RandomSample[#,Floor[Length@# Sqrt[1-missingRatio]]]&@Range[Dimensions[rgb][[1]]]]/@cols)->1,Dimensions@gray];
\[CapitalOmega]=ArrayFlatten@{{\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1}};\[CapitalOmega]//Dimensions


r2=robustPvd[{A},{Map[1&,A,{2}]},r,1000];
MatrixForm/@r2


(*Select[*)Table[n=4;s=DiagonalMatrix@RandomReal[1,n];p=RandomReal[1,n{1,1}];
	{MatrixRank@p,MatrixForm@p,MatrixForm@s,MatrixForm[p.s.Inverse[p]],MatrixForm[p.Inverse[p]],Total@Abs@Flatten[#,1]&/@{p.s.Inverse[p],s}},{1}](*,#[[-1,1]]<#[[-1,2]]&]*)


Select[Table[s=DiagonalMatrix@RandomReal[1,10];m=RandomReal[1,{10,10}];{(*MatrixForm/@{s,Inverse[s],m,s.m.Inverse[s]},*)
	Total@Abs@Flatten[s.m.Inverse[s]],Total@Abs@Flatten[m]},{1000}],#[[-2]]<#[[-1]]&]


m=RandomReal[1000{-1,1},{3,3}];r=SingularValueDecomposition@m;
Select[Table[Total@Abs@Flatten[#]&/@{m,r[[2]]},{10000}],#[[1]]<#[[2]]&]


m=RandomReal[{-3,3},{3,3}];
m={{1,0,0,0},{0,1,0,0},{1,-1,1,0},{1,-1,1,1}};
{s,j}=JordanDecomposition@m;
MatrixForm/@{m,s,j,s.j.Inverse[s]}
Total@Flatten@Abs@#&/@{m,j}


{s,j}=JordanDecomposition@{{a,1/2},{-1/2,a}};
MatrixForm/@{s,j}


m=Normal@SparseArray[{Band[{1,1}]->2,Band[{1,2}]->1},3{1,1}];
s=DiagonalMatrix@{1,1,2};
MatrixForm/@{m,s.m.Inverse[s]}
Total@Flatten@Abs@#&/@{m,s.m.Inverse[s]}


as=Array[a,3{1,1}];m={{2,1,0},{0,2,1},{0,0,1}};
r=NMinimize[Total@Abs@Flatten[as.m.Inverse[as]],Flatten@as]
MatrixForm/@{as/.r[[2]],as.m.Inverse[as]/.r[[2]],as/.r[[2]],Inverse[as/.r[[2]]]}
Total@Abs@Flatten@#&/@{m,as.m.Inverse[as]/.r[[2]]}


Select[Table[q=0.5;{MatrixForm@#,Total[(Abs@Flatten@#)^q],Total[Abs[#]^q&/@SingularValueList[#]]}&@RandomReal[3{-1,1},{5,5}],{1000}],#[[3]]>#[[2]]&]


(*Select[*)Table[q=2;prem=RandomReal[3{-1,1},{5,5}];m=prem(*+Transpose[prem]*);r=Eigensystem@m;P=Transpose[r[[2]]];L=DiagonalMatrix[r[[1]]];
	{m,Total[(Abs@Flatten@#)^q]&@L,Total[(Abs@Flatten@#)^q]&@m},{1}](*, #[[-2]]>#[[-1]]&]*)


Table[n=3;(*q=2;*)r=First@SingularValueDecomposition[RandomReal[3{-1,1},n{1,1}],n];s=DiagonalMatrix@RandomReal[1,n](*{1,0,0}*);
	Total[(Abs@Flatten@#)^q]&(*MatrixForm@Map[Abs[#]^q&,#,{2}]&*)/@{r.s.Transpose[r],s},{q,{0.1,0.3,0.5,1,1.5,2}}]


k=1;m=RandomReal[k{-1,1},{2,2}];
(*l=Table[{p,pnorm[m,p]-pnorm[SingularValueList@m,p]},{p,0.2,2.5,0.001}];l[[-10;;]]
l=Transpose@{l[[2;;,1]],-Differences[l[[;;,2]]]};ListLogPlot[l,Joined->True]*)
p=1.5;
r=SingularValueDecomposition@m;m2=r[[1]].DiagonalMatrix[Diagonal[r[[2]]]^(p-1)].Transpose[r[[3]]];
MatrixForm/@({Abs[m]^(p-1),Abs@m2}/k)
pnorm[#,p]&/@{m,m2,r[[2]]}
{Max@Abs@m,Max@Abs@r[[2]]}


p=1.5;q=p/(p-1);
(*Select[*)Table[m=RandomReal[100{-1,1},{5,3}];r=SingularValueDecomposition[m,3];
	{pnorm[m,p],pnorm[r[[2]],p],pnorm[m,q],pnorm[r[[2]],q],p,q,MatrixForm@m,
		Norm[r[[1]].r[[2]].Transpose[r[[3]]]-m,"Frobenius"],
		Tr[r[[2]]]/pnorm[r[[1]].Transpose[r[[3]]],q],Tr[r[[2]]]/pnorm[IdentityMatrix[Length@r[[1]]],q]},{10}](*,#[[1]]+#[[3]]<#[[2]]+#[[4]]&]*)


(*p=1.5;q=p/(p-1);*)
Select[q=2.1;Table[m=RandomReal[10{-1,1},{5,3}];r=SingularValueDecomposition[m,3];m2=Transpose[m].m;
	a=Abs[m]^q;b=r[[3]].(Abs[r[[2]]]^q).Transpose[r[[3]]];
	{MatrixForm@m,MatrixForm[a],MatrixForm[b],Total@Flatten@a,Tr[Abs[m2]^(q/2)],Tr[b]}
	,{10000}],!(#[[-3]]<=#[[-2]]<=#[[-1]])&]


Select[Table[m=RandomReal[3{-1,1},{5,3}];m2=Transpose[m].m;
r=SingularValueDecomposition[m,3];sm=r[[3]].r[[2]].Transpose[r[[3]]];
(*MatrixForm/@{r[[3]].(r[[2]]^2).Transpose[r[[3]]],m2,sm sm}*)
p=1.;ms={Abs[sm]^p,Tr@MatrixPower[sm,p]};
{Total@Abs@Flatten@ms[[1]],ms[[2]],MatrixForm@Chop@ms[[1]]},{1000}],#[[1]]<#[[2]]&]


n=10;pts=Table[{Re@#,Im@#}&@Exp[2Pi I i/n],{i,n}];Graphics[Point@pts]
pts//MatrixForm


m=Abs@RandomReal[{-1,1},{5,3}];m2=Transpose[m].m;
f=Function[x,Tr[m2^x]-Total[(Eigenvalues@m2)^x]];
Plot[f[x],{x,0.5,3}]


frobeniusTraceNormCost=Function[{W,H,Y,D,\[Mu]},Module[{R=D-Transpose[W].H},
	{#,Total@#}&@Append[{1/2,1/2,\[Mu]/2} (Norm[#,"Frobenius"]^2&/@{W,H,R}),Tr[Transpose[Y].R]]
	]];
frobeniusTraceNorm=Function[{D,rank,maxIter},Module[{W,H,Y,\[Mu]=1.0,\[Rho]=1.05,dim=Dimensions[D]},
	{W,H}=RandomReal[1,#]&/@{{rank,dim[[1]]},{rank,dim[[2]]}};Y=0D;
	Do[
	W=LinearSolve[IdentityMatrix[rank]+\[Mu] H.Transpose[H],H.Transpose[\[Mu] D+Y]];
	(*Print[{"W",frobeniusTraceNormCost[W,H,Y,D,\[Mu]]}];*)
	H=LinearSolve[IdentityMatrix[rank]+\[Mu] W.Transpose[W],W.(\[Mu] D+Y)];
	(*Print[{"H",frobeniusTraceNormCost[W,H,Y,D,\[Mu]]}];*)
	Y+=\[Mu](D-Transpose[W].H);
	\[Mu]*=\[Rho];
	,{maxIter}];
	{W,H}
	]];
robustFrobeniusTraceNormCost=Function[{W,H,Y,D,S,\[CapitalOmega],\[Mu]},Module[{R=D-Transpose[W].H-S},
	{#,Total@#}&@Join[{1/2,1/2,\[Mu]/2} (Norm[#,"Frobenius"]^2&/@{W,H,R}),{Tr[Transpose[Y].R],Total@Flatten@Abs[S \[CapitalOmega]]}]
	]];
robustFrobeniusTraceNorm=Function[{D,\[CapitalOmega],rank,maxIter,\[Lambda]},Module[{W,H,Y,S,Z,\[Mu]=1.0,\[Rho]=1.01,dim=Dimensions[D]},
	{W,H}=RandomReal[1,#]&/@{{rank,dim[[1]]},{rank,dim[[2]]}};Y=S=Z=0D;
	Do[
	Z=D-Transpose[W].H+Y/\[Mu];
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Z]+(1-\[CapitalOmega]) Z;
	If[Mod[j,10]==1,Print[{"S",robustFrobeniusTraceNormCost[W,H,Y,D,S,\[CapitalOmega],\[Mu]]}]];
	W=LinearSolve[IdentityMatrix[rank]+\[Mu] H.Transpose[H],H.Transpose[\[Mu] D-\[Mu] S+Y]];
	If[Mod[j,10]==1,Print[{"W",robustFrobeniusTraceNormCost[W,H,Y,D,S,\[CapitalOmega],\[Mu]]}]];
	H=LinearSolve[IdentityMatrix[rank]+\[Mu] W.Transpose[W],W.(\[Mu] D-\[Mu] S+Y)];
	If[Mod[j,10]==1,Print[{"H",robustFrobeniusTraceNormCost[W,H,Y,D,S,\[CapitalOmega],\[Mu]]}]];
	Y+=\[Mu](D-Transpose[W].H-S);
	\[Mu]*=\[Rho];
	,{j,maxIter}];
	{W,H}
	]];
n=400;m=Mean[ImageData/@ColorSeparate@ImageResize[Import@"t.jpg",n{1,1}]];\[CapitalOmega]=RandomSparseMatrix[n,n,0.95];
r=frobeniusTraceNorm[m,Floor[0.5n],20];
{Total@SingularValueList@m,Mean[Norm[#,"Frobenius"]^2&/@r],
	Norm[m-Transpose[r[[1]]].r[[2]],"Frobenius"]/Norm[m,"Frobenius"]}
r=robustFrobeniusTraceNorm[m \[CapitalOmega],\[CapitalOmega],Floor[0.5n],500,1.];
{Total@SingularValueList@m,Mean[Norm[#,"Frobenius"]^2&/@r],
	Norm[(1-\[CapitalOmega])(m-Transpose[r[[1]]].r[[2]]),"Frobenius"]}
(Export["t.png",Rasterize@#];#)&[Image[#,ImageSize->400]&/@{m,Transpose[r[[1]]].r[[2]]}]


(*Experiment 1, toy data*)
Table[{"forceDiagonal",force,Mean@#,#}&@Table[
	rank=8;
	m=RandomReal[1,2{5,4}];r=SingularValueDecomposition[#,rank]&@m;(*MatrixForm/@r*)
	m2=r[[1]].r[[2]].Transpose[r[[3]]];
	{X,Us}=sparseUnitaryDecomposition[m,rank,20,force];
	(*Print[{"{X,m2,s2}",MatrixForm/@{X,m2,r[[2]]}}];*)
	(*Print[MatrixForm@Chop[Transpose[#].#]&/@Us];*)
	{Norm[foldXUs[X,Us,{}]-m2,"Frobenius"]/Norm[m2,"Frobenius"],
		Abs[Total@Flatten@Abs@X-Total@SingularValueList@m2]/Total@SingularValueList@m2},{1}],{force,{True,False}}]
(*m=RandomReal[1,3{3,2}];{Norm[sShrinkage[0.1,m]-zShrinkage[0.1,m,6,40],"Frobenius"]/Norm[m,"Frobenius"]}
MatrixForm/@{sShrinkage[0.1,m],zShrinkage[0.1,m,6,40],m}*)
Table[Sort@Abs@Flatten@First@sparseUnitaryDecomposition[{{{0,1},{1,0}},{{1,0},{0,0}}},2,30,False],{10}]//MatrixForm
(*Below shows that pseudo-diagonal matrix may not have lowest trace norm and least support.*)
(*xus=sparseUnitaryDecomposition[{{{0,1},{1,0}},{{1,0},{0,1}}},4,30,False];MatrixForm/@xus[[1]]*)


(*Experiment 2, with real data, we can get sparse tensor, but not with random tensor.
Sparsity of the core depends on core matrix size.*)

(*SeedRandom[1003];n=400;m2=RandomReal[1,{3,n,n}];*)
(*n=400;m2=ImageData/@ColorSeparate[ImageResize[Import@"t.jpg",n{1,1}]];*)
n=100;m2=ImageData/@Join@@(ColorSeparate@ImageResize[Import@#,n{1,1}]&/@FileNames["/s/island/*.JPG"]);
{X,Us}=sparseUnitaryDecomposition[m2,Floor[0.5n],10,False];{"frobenius ratio",Total@Flatten[(m2-foldXUs[X,Us,{}])^2]/Total@Flatten[m2^2]}
nnz=N@Length@SparseArray[X]["NonzeroPositions"];
(*MatrixForm/@X*)Print[{"nnz",nnz,"nnzRatio",nnz/Times@@Dimensions@m2}]
l=Reverse@Sort[Abs@Flatten[X]];
ListPlot[Accumulate@l/Total@l,PlotRange->All]
folded=foldXUs[X,Us,{}];
(*ColorCombine[Image[#,ImageSize->400]&/@m2]
ColorCombine[Image[#,ImageSize->400]&/@folded]*)
ColorCombine[Image[#,ImageSize->400]&/@#]&/@Partition[m2,3]
ColorCombine[Image[#,ImageSize->400]&/@#]&/@Partition[folded,3]
Dimensions/@{m2,folded}
ByteCount/@{m2,SparseArray@X,Us}


(*Versus SVD. SUD don't get sparse enough core.*)

m=Mean[ImageData/@ColorSeparate[ImageResize[Import@"t.jpg",400{1,1}]]];l=SingularValueList@m;n=200;m2=SvdApprox[m,n];
(*Manipulate[Image@SvdApprox[m,n],{n,10,400,10}]*)
{X,Us}=sparseUnitaryDecomposition[m,n,50,False];folded=foldXUs[X,Us,{}];
ListPlot[#,PlotRange->All]&/@{Accumulate@l/Total@l,Accumulate@Abs@Flatten@X/Total@Abs@Flatten@X}
{"Frobenius, {SUD, svd}",Norm[#-m,"Frobenius"]&/@{folded,m2}}
{"Trace norm, SUD, svd, orig",Total@Abs@Flatten@#&/@{X,l,m}}
Image@m2
Image@folded


(*versus HOSVD. SUD gets sparser core, but takes many iterations. Core of SUD is not orthogonal, while that of HOSVD is.*)
SeedRandom[1003];
m={{{0.9073`,0.7158`,-0.3698`},{1.7842`,1.697`,0.0151`},{2.1236`,-0.074`,1.4429`}},
	{{0.8924`,-0.4898`,2.4288`},{1.7753`,-1.5077`,4.0337`},{0.6631`,1.9103`,-1.7495`}},
	{{2.1488`,0.3054`,2.3753`},{4.2495`,0.3207`,4.7146`},{1.826`,2.1335`,-0.2716`}}};
(*m=Partition[#,3]&/@m;*){"m",MatrixForm/@m}
{X,Us}=sparseUnitaryDecomposition[m,3,200,False];
{"SUD:X",MatrixForm/@Chop@X}
{"SUD:Us",MatrixForm/@Us}
{X2,Us2}=hosvd[0.01,m,3];
{"HOSVD:X2",MatrixForm/@X2}
{"HOSVD:Us2",MatrixForm/@Us2}
{"Frobenius error ratio",Sqrt@Total[Flatten[foldXUs[#[[1]],#[[2]],{}]-m]^2]&/@{{X,Us},{X2,Us2}}}
{"Trace norm, SUD, hosvd, orig",
	Total@Abs@Flatten@#&/@{X,X2,m},{"Tr as lower bound",Max@Table[Total[Tr/@rotateTensor[m,i,1]],{i,Length@Dimensions@m}]}}
(*ListLogLogPlot[Table[{\[Tau],Sqrt@Total[Flatten[foldXUs[unDShrinkage[\[Tau],dShrinkage[\[Tau],X]],Us,{}]-m]^2]},{\[Tau],{0,0.0001,0.001,0.01,0.1,1,10}}],Joined->True]*)
Outer[Dot,Flatten/@X,Flatten/@X,1]//MatrixForm
Outer[Dot,Flatten/@X2,Flatten/@X2,1]//MatrixForm


m=RandomReal[1,{3,3}];m2=m+Transpose[m];r=SingularValueDecomposition@m2;
{MatrixForm/@{m2,r[[2]]},entropy/@{m2,r[[2]]},Total@Abs@Flatten[#]&/@{m2,r[[2]]},pnorm[#,0.3]&/@{m2,r[[2]]}}


(*\!\(\(||\)\(X\)
\*SubscriptBox[\(||\), \(1\)]\(+\[Mu]\)\(||\)\(1 - D\ U . X . 
\*SuperscriptBox[\(V\), \(T\)]\)
\*SubscriptBox[\(||\), \(1\)]\(\(-\[Mu]\)\ D\ U . X . 
\*SuperscriptBox[\(V\), \(T\)]\nsign \((X)\) + \[Mu]\ \((D\ V^T . U)\)^T\ sign \((1 - D\ U . X . V^T)\) - \[Mu]\ D\ V^T . U\)\)*)


(*\!\(min\  || \(
\*SuperscriptBox[\(E\), \(M - 
\*SuperscriptBox[\(M\), \(T\)]\)] . D . 
\*SuperscriptBox[\(E\), \(N - 
\*SuperscriptBox[\(N\), \(T\)]\)]\)
\*SubscriptBox[\(||\), \(1\)]\)*)
v2={3,-5};s={1,1/3};v=s{Cos[Pi/4],Sin[Pi/4]};
U=N@(#[[1]].Transpose[#[[3]]])&@SingularValueDecomposition@Outer[Times,DiagonalMatrix[s^(-1/2)].v,v2,1];
Graphics[{Blue,Line@Table[s{Cos@t,Sin@t},{t,0,2Pi+0.1,0.1}],Disk[v,0.1],Red,Disk[v.U,0.1],Darker@Yellow,Disk[v2,0.1]},Axes->True,AxesOrigin->{0,0}]


cccpSudCost=Function[{X,Us,\[Mu],p,D},{Total@#,#}&@{pnorm[X,1],-pnorm2[X,p],\[Mu]/2 pnorm2[foldXUs[X,Us,{}]-D,2]}];
cccpSud=Function[{D,rank,p,maxIter,forceDiagonal},Module[{X,
		norm2=Sqrt[Mean@Flatten[D^2]],\[Mu],\[Rho]=1.5,fnorm=Sqrt@Total@Flatten[D^2],
		order=Length@Dimensions@D,Us,diag=If[forceDiagonal,Diagonal,Identity],undiag=If[forceDiagonal,DiagonalMatrix,Identity]},
	If[p<=2,Print["p should >=2"];Abort[]];
	Us=Table[Transpose@Orthogonalize@RandomReal[1,{Min[Dimensions[D][[i]],rank],Dimensions[D][[i]]}],{i,order}];	
	\[Mu]=1.25/norm2;
	X=diag@foldXUs[D,Transpose/@Us,{}];
	Do[
		If[j>1,Print[{j,cccpSudCost[X,Us,\[Mu],p,D]}]];
		(*X=dShrinkage[1/\[Mu],diag[foldXUs[D,Transpose/@Us,{}]+1/\[Mu] (Abs[X]^(p-1))/(pnorm2[X,p]^(1/p-1))]];*)
		X=dShrinkage[1/\[Mu],diag[foldXUs[D,Transpose/@Us,{}]+p/\[Mu] (Abs[X]^(p-1))]];
		(*X=X fnorm/Sqrt@Total@Flatten[X^2];*)
		(*Print[{"X",cccpSudCost[X,Us,\[Mu],p,D]}];*)
		Do[
			Us[[i]]=procrustes[unfoldTensor[D,i].Transpose[unfoldTensor[foldXUs[undiag@X,Us,{i}],i]]];
			(*Print[{"Us",i,cccpSudCost[X,Us,\[Mu],p,D]}];*)
			,{i,order}];
		\[Mu]*=\[Rho];
	,{j,maxIter}];
	sortXUs[undiag@X,Us]
	]];
SeedRandom[1003];
n=10;m=RandomReal[1,n{1,1}];
n=400;m=Mean[ImageData/@ColorSeparate[ImageResize[Import@"t.jpg",n{1,1}]]];
r=SingularValueDecomposition@m;
{X,Us}=cccpSud[m,Floor[0.2n],2.2,10,False];folded=foldXUs[X,Us,{}];
(*{"svd",MatrixForm/@r}
{"sud",MatrixForm/@Prepend[Us,X]}*)
{"Trace norm, svd, sud",Total@Diagonal@r[[2]],pnorm[X,1]}
{"Frobenius ratio",Norm[m-folded,"Frobenius"]/Norm[m,"Frobenius"]}
Image@m
Image@folded
Image@SvdApprox[m,Floor[0.2n]]


(*SeedRandom[1003];*)
m=RandomReal[1,{n,n}];as=RandomReal[1,{n,n}];bs=RandomReal[1,{n,n}];
MatrixForm@(#-Transpose[#])&/@{as,bs}
(*pnorm 3 is much smoother than pnorm 1, but still half convex/concave*)
ListPlot@Table[pnorm[MatrixExp[t(as-Transpose[as])+(1-t)(bs-Transpose[bs])].m,3],{t,0,1,0.01}]


img=Rasterize[Graphics[{Rectangle[{0,0},{16,8}],FaceForm[None],EdgeForm[{White}],Polygon[{{1,4},{3,6},{5,4},{3,2}}]
	,EdgeForm[{White,Dashing[0.03]}],Polygon[{#[[1]]+8,#[[2]]}&/@{{1,4},{3,6},{5,4},{3,2}}]}],Background->None]
Radon[img,Method->"Hough"]//ImageAdjust
Show[ImageMultiply[Graphics@Line[ImageLines@img],0.5],ImageMultiply[img,0.5]]
m=Map[Norm,ImageData[Binarize@img],{2}];(*m//MatrixPlot*)
xys=SparseArray[m]["NonzeroPositions"];Graphics[{Point[xys]}]
SingularValueDecomposition[Prepend[#,1.]&/@xys][[3]]


(*Can QR algorithm be applied to a symmetric 3-tensor?*)
SeedRandom[1003];m=RandomReal[1,15{1,1}];m=m+Transpose[m];{q,r}=QRDecomposition@m;
Eigenvalues@m
l=NestList[
	Function[m,#[[2]].ConjugateTranspose[#[[1]]]&@QRDecomposition@m],m,30];
(*MatrixForm/@l*)
(*Not guaranteed to be monotone.*)
l2=Total@Abs@Flatten@#&/@l;ListLogPlot[l2,PlotRange->All]
Select[Differences@l2,#>0&]


m2=ArrayFlatten@{{0,RandomReal[1,{5,3}]}};
MatrixForm/@SingularValueDecomposition[m2,3]
SingularValueList@m2


SvdApprox=Function[{m,k},If[k>=0,#[[1]].#[[2]].Transpose[#[[3]]]&@SingularValueDecomposition[m,k]
	,Module[{r,n=Min@Dimensions@m},r=SingularValueDecomposition[m,n];r[[1]].DiagonalMatrix@PadLeft[Diagonal[r[[2]]][[k;;-1]],n].Transpose[r[[3]]]]]];
m//MatrixForm
MatrixForm/@SingularValueDecomposition@m
MatrixForm/@SingularValueDecomposition@SvdApprox[m,1]
MatrixForm/@SingularValueDecomposition@SvdApprox[m,-1]


SingularValueDecomposition[If[n>p,SvdApprox[m,p-n]]][[2]]


n=3;p=3;Clear[a,b,f,g];{as,bs}=Array[#,n{1,1}]&/@{a,b};SeedRandom[1003];m=RandomReal[{-1,1},n{1,1}];matrixExp=Re@MatrixExp@#&;
getXus=Function[{as,bs},v=Re@matrixExp[bs-Transpose[bs]];u=Re@matrixExp[as-Transpose[as]];
	sortXUs[Transpose[u].m.v,{u,v}]];
disp=Function[{as,bs},xus=getXus[as,bs];Print[xus[[1]]//Chop//MatrixForm]];
(*approximate matrix exp don't work well untill >=7 order*)
(*matrixExp=Function[m,Module[{a=0m,b=N@IdentityMatrix@Length@m},Do[a+=b/N[k!];b=b.m;,{k,0,7}];a]];*)
vals={};f={pnorm2[#,0.5]&,-pnorm2[#,4]&,pnorm[#,1]-pnorm[#,3]&,pnorm2[#,1]-pnorm2[#,3]&,pnorm2[#,1]&,pnorm2[#,"en"]&,-pnorm2[#,3]&}[[3]];
R=IdentityMatrix[{n,p}];
g[as_?(NumberQ[#[[1,1]]]&),bs_]:=f[Transpose[R].matrixExp[Transpose[as]-as].m.matrixExp[bs-Transpose[bs]].R];
r=NMinimize[g[as,bs],Flatten[as~Join~bs]
	,StepMonitor:>
		(xus=getXus[as,bs];vals=Append[vals,Total@Abs@Flatten@xus[[1]]]),MaxIterations->500];//AbsoluteTiming
{r[[1]],f[SingularValueDecomposition[m][[2]]]}
(*r=FindMinimum[-Total@Flatten[Abs[Re@matrixExp[Transpose[as]-as].m.Re@matrixExp[bs-Transpose[bs]]]^3],Flatten[as~Join~bs]];//AbsoluteTiming*)
With[{as=as/.r[[2]],bs=bs/.r[[2]]},disp[as,bs]];
MatrixForm/@SingularValueDecomposition@m
ListPlot[vals,PlotRange->All]


(*There is a matrice pair in same orbit with ||A||_p<||B||_p but ||A||_q>||B||_q. *)
SeedRandom[1003];
Select[Table[v=RandomReal[1,3{1,1}];
	{#,Boole[#[[1]]>#[[2]]]}&/@Transpose[{pnorm[#,0.5],pnorm[#,1],pnorm[#,1.5]}&/@Table[randomUnitaryMatrix[3].v,{2}]],{10}]
	,Mean[#[[;;,2]]]!=#[[1,2]]&]


SeedRandom[1005];
Table[ms=RandomReal[1,{2,2,2,2}];
	{Table[With[{m1=unfoldTensor[ms[[1]],i],m2=unfoldTensor[ms[[2]],i]},
		{dotProduct[m1,m2],SingularValueList[m1].SingularValueList[m2]}],{i,3}]~Join~{{dotProduct@@ms,pnorm[ms[[1]],2]pnorm[ms[[2]],2]}}
	,Table[ms2=Abs[heuristicSUD[#,p][[1]]]&/@ms;{pnorm[#,1]&/@ms2,Abs[dotProduct@@ms2]},{p,{(*0.3,0.5,*)1,1.5,3,10}}]},{10}]


Needs["NumericalCalculus`"]
m=RandomReal[10,2{1,1}];d=SingularValueDecomposition[m][[2]];
{Plot[pnorm2[m,p]-pnorm2[d,p],{p,0.0001,2.1},PlotRange->All],
Plot[ND[pnorm2[m,x]-pnorm2[d,x],x,p],{p,0.0001,2.1},PlotRange->All],
Plot[ND[pnorm2[m,x]-pnorm2[d,x],{x,2},p],{p,0.0001,2.1},PlotRange->All]}


SeedRandom[1004];m=(*hermitian@*)RandomReal[0.8,2{1,1}];
(*m={{3,2},{0,0}};*)
(*m=N@{{1,0,0},{0,2,1},{0,0,2}}*)
(*m=N@{{1,0,0},{0,2,1},{0,-1,2}}*)
(*{xus=heuristicSUD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xus,SingularValueList@m}*)
(*{xsls=heuristicSSLD[m,1];//AbsoluteTiming,MatrixForm/@Abs@xsls,GeometricMean@SingularValueList@m}*)
(*{xus2=heuristicSymSUD[m,0.5];//AbsoluteTiming,MatrixForm/@Abs@xus2,Abs@Eigenvalues@m,MatrixForm/@SchurDecomposition@N@m}*)
(*{xpus=heuristicEVD[m,0.1];//AbsoluteTiming,MatrixForm/@Abs@xpus,Abs@Eigenvalues@m,SingularValueList@m}*)

(*(*The cores of the best truncated approximations should be compatible.*)
(*SeedRandom[1005];*)m=(*hermitian@*)RandomReal[1,{3,2,2}];
{xus=heuristicSUD[m,1];//AbsoluteTiming,MatrixForm/@Abs@xus,SingularValueList@m}
{txus=heuristicTruncatedSUD[m,1.5,{1,1,1}];//AbsoluteTiming,MatrixForm/@Abs@txus}
{txus2=heuristicTruncatedSUD[m,1.5,2{1,1,1}];//AbsoluteTiming,MatrixForm/@Abs@txus2}
{txus3=heuristicTruncatedSUD[m,1.5,{2,1,2}];//AbsoluteTiming,MatrixForm/@Abs@txus3}*)

m={{{1,0},{0,1}},{{0,1},{1,1}}};(*Can be simplified*)
m={{{1,0},{0,1}},{{0,1},{-1,1}}};(*Leads to same shape, then lowest storng orthogonal rank tensor decomposition not unique.*)
(*m={{{1,1},{1,0}},{{1,0},{0,0}}};
m={{{0,1},{1,0}},{{1,0},{0,0}}};
m={{{0,1},{1,0}},{{0,0},{0,0}}};
m={{{1,0},{0,0}},{{0,0},{0,0}}};
m={{{1,0},{0,0}},{{0,0},{0,1}}};
(*m={{1,2},{1,2}};*)
m={{1,2},{3,4}};
(*Kolda-OTD-draft-3*)m=With[{a={1,0},b={0,1},c={1,1}/Sqrt[2]},TensorProduct@@{a,a,a}+TensorProduct@@{a,b,c}+TensorProduct@@{a,c,b}];*)
(*(*Incorrect Kolda-OTD-draft-4*)m=With[{a={1,0},b={0,1}},3TensorProduct@@{a,b,b}+2TensorProduct@@{b,b,b}+TensorProduct@@{a,a,b}];*)
(*Non-uniqueness of storng orthogonal decomposition*)
(*(*Kolda-OTD-3.3*)m=With[{a={1,0},b={0,1}},3TensorProduct@@{a,b,b}+2TensorProduct@@{b,b,b}+TensorProduct@@{a,a,a}];*)
(*(*Kolda-OTD-draft-8*)vs=IdentityMatrix[4];m=N@Total@MapThread[Times,{{1,0.75,0.7,0.7,0.65,0.65},
	TensorProduct@@{vs[[#[[1]]]],vs[[#[[2]]]],vs[[#[[3]]]]}&/@{{1,1,1},{2,2,2},{1,3,4},{1,4,3},{2,3,4},{2,4,3}}},1];*)
{xus=heuristicSUD[m,1];//AbsoluteTiming,MatrixForm/@Abs@xus}
(*{xus2=heuristicSUD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xus2}*)
(*{xsls=heuristicSSLD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xsls}*)

(*SeedRandom[1006];m=RandomReal[{-1,1},{2,2,2}];
m={{{0,1},{1,0}},{{1,0},{0,0}}};
(*{xus=heuristicSUD[m,0.5];//AbsoluteTiming,MatrixForm/@Abs@xus}
{xus2=heuristicSUD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xus2}
pnorm[#,0.5]&/@{xus[[1]],xus2[[1]]}
pnorm[#,1.5]&/@{xus[[1]],xus2[[1]]}*)
ps={0.5,1.9};
{xsls=heuristicSSLD[m,ps[[1]]];//AbsoluteTiming,MatrixForm/@Abs@xsls}
{xsls2=heuristicSSLD[m,ps[[2]]];//AbsoluteTiming,MatrixForm/@Abs@xsls2}
pnorm[#,ps[[1]]]&/@{xsls[[1]],xsls2[[1]]}
pnorm[#,ps[[2]]]&/@{xsls[[1]],xsls2[[1]]}*)


n=2;rank=3;order=3;ms=Table[TensorProduct@@RandomReal[1,{3,n}],{rank}];m=Total@ms;
{xsls=heuristicSSLD[m,1.5];//AbsoluteTiming,MatrixForm/@Abs@xsls,MatrixForm/@m}


MatrixForm/@{m,foldXUs[xus[[1]],xus[[2]],{}],foldXUs[xus2[[1]],xus2[[2]],{}]}
pnorm[#,1]&/@{m,xus[[1]],xus2[[1]]}
pnorm[#,1.5]&/@{m,xus[[1]],xus2[[1]]}


d=With[{n=2},Table[Boole[Mod[i+k-1,n]==Mod[j,n]],{i,n},{j,n},{k,n}]];
{MatrixForm/@d}
Table[{m=unfoldTensor[d,i];MatrixForm@m,Transpose[m].m//MatrixForm},{i,3}]


MatrixForm/@SingularValueDecomposition[Transpose[m].m]


d=Array[a,{2,4}]m
MatrixForm[(d.Transpose[d])^1.5]
MatrixForm[(Transpose[d].d)^1.5]


Tr[(Transpose[m].m)^1.5]
Tr[(m.Transpose[m])^1.5]


as=Array[a,{2,3}];
Tr[(as\[Transpose].as)^1.5]//Simplify
Tr[(as.as\[Transpose])^1.5]//Simplify


{as,bs}=Array[#,{2,2}]&/@{a,b};
Tr[(as\[Transpose].bs)^1.5]-Tr[(bs\[Transpose].as)^1.5]//Simplify
Tr[(as\[Transpose])^1.5]-Tr[(as)^1.5]//Simplify


tr (USV\[Transpose]\[CapitalLambda]ACB\[Transpose]BCA\[Transpose]\[CapitalLambda]VSU\[Transpose])^op=tr (USV\[Transpose] \[CapitalLambda]AC^2 A\[Transpose]\[CapitalLambda]VSU\[Transpose])^op
(*=tr S^2 V\[Transpose] \[CapitalLambda]AC^2 A\[Transpose]\[CapitalLambda]V*)


(*Kolda-OTD-draft-8*)vs=IdentityMatrix[4];m=N@Total@MapThread[Times,{{1,0.75,0.7,0.7,0.65,0.65},
	TensorProduct@@{vs[[#[[1]]]],vs[[#[[2]]]],vs[[#[[3]]]]}&/@{{1,1,1},{2,2,2},{1,3,4},{1,4,3},{2,3,4},{2,4,3}}},1];
{xus=heuristicSUD[m,1];//AbsoluteTiming,MatrixForm/@Abs@xus}
{txus=heuristicTruncatedSUD[m,1.5,{1,1,1}];//AbsoluteTiming,MatrixForm/@Abs@txus}
{txus2=heuristicTruncatedSUD[m,1.5,{2,2,1}];//AbsoluteTiming,MatrixForm/@Abs@txus2}


pnorm2[foldXUs[#[[1]],#[[2]],{}]-m,2]&/@{xus,txus,txus2}


pnorm2[foldXUs[txus[[1]],txus[[2]],{}]-m,2]
pnorm2[foldXUs[txus2[[1]],txus2[[2]],{}]-m,2]
pnorm2[txus2[[1]],2]
m


N@Table[{p,pnorm2[SortBy[Flatten[#],Abs][[;;]],p]&/@{m,xus[[1]]}},{p,{0.1,0.5,1,1.5,2,2.5,3}}]
Plot[pnorm2[SortBy[Flatten[#],Abs][[;;]],p]&/@{m,xus[[1]]},{p,0.001,3}]


vs=IdentityMatrix[4];m=N@Total@MapThread[Times,{{1,0.75,0.7,0.7,0.65,0.65},
	TensorProduct@@{vs[[#[[1]]]],vs[[#[[2]]]],vs[[#[[3]]]]}&/@{{1,1,1},{2,2,2},{1,3,4},{1,4,3},{2,3,4},{2,4,3}}},1];


(*Generating k-pseudo-diagonal forms*)
n=3;m=Table[Boole[Mod[k-i,n]==Mod[j-1,n]],{i,n},{j,n},{k,n}];MatrixForm/@m
vec[m]


(*No Lp extension for von Neumann trace inequality.*)
Table[d=RandomReal[1,{2,2}];l=DiagonalMatrix@RandomReal[1,2];{pnorm[Diagonal[d.l],0.5],pnorm[SingularValueDecomposition[d][[2]].l,0.5]},{100}]


Function[p,{p,xsls=heuristicSSLD[m,p];//AbsoluteTiming,MatrixForm/@Abs@xsls,pnorm2[#,p]&[xsls[[1]]]}]/@{1.9,1.5,1,0.5,0.2,0.1,0.01}


(*Try solving a small example analytically.*)
g=foldXUs[{{{1,1},{1,0}},{{1,0},{0,0}}},RotationMatrix/@{\[Alpha],\[Beta],\[Gamma]},{}];
f=Simplify@Total[Flatten[Abs@g]^4]
r=Solve[D[f,\[Alpha]]==0&&D[f,\[Beta]]==0&&D[f,\[Gamma]]==0,{\[Alpha],\[Beta],\[Gamma]}]//N


m={{{1,0,0},{0,1,0},{0,0,1}},{{0,1,0},{0,0,1},{1,0,0}},{{0,0,1},{1,0,0},{0,1,0}}}
Import@Export["t.png",Magnify[MatrixForm/@m]]
unfoldTensor[m,1]//MatrixForm
(*#\[Transpose].#&@unfoldTensor[m,1]//MatrixPlot*)


n=4;order=3;Clear[a,vs];vars=Array[a,order];as=Array[#,{2,n,n}]&/@vars;SeedRandom[1005];
(*Then will get different optimum for different p<1, but otherwise same optimum.
When we allow negative, the brute force optimizaiton had hard time to get optimum.*)
m=(*symmetrize@*)RandomComplex[{-3,3}(1+I),Array[n&,order]];
(*m=N@RandomChoice[{0,1},Array[n&,order]];m*)
(*m=N@{{{0,1},{1,1}},{{0,0},{1,0}}};*)
(*m=N@{{{1+Sqrt[2],0},{0,-1}},{{0,1-Sqrt[2]},{1,0}}};*)
(*m=N@{{{1,0},{0,-1}},{{0,1},{1,0}}};(*orthogonal rank 4*)*)
(*m=N@{{{{2,0},{0,-1}},{{0,1},{1,0}}},{{{0,1},{1,0}},{{-1,0},{0,1}}}};(*orthogonal rank 8, replace 2 with 1 is also orank 8.*)*)
(*m=3TensorProduct@@{{1,0},{0,1},{0,1}}+2TensorProduct@@{{0,1},{0,1},{0,1}}+TensorProduct@@{{1,0},{1,0},{1,0}};(*Kolda example*)*)
(*m=N@{{{1,0},{0,1}},{{0,1},{1,0}}};(*orthogonal rank 2*)*)
(*m={{{0,1},{1,0}},{{1,0},{0,1}}};*)(*diagonal*)
(*m={{{0,1},{1,0}},{{1,0},{0,0}}};*)(*non-diagonal*)
(*m=N@{{{2,1},{1,4}},{{1,3},{0,1}}};*)
(*m={{{1,0,0},{0,0,1},{0,1,0}},{{0,1,0},{1,0,0},{0,0,1}},{{0,0,1},{0,1,0},{1,0,0}}};*)
(*D is of (order-2)-diagonal is not optimum, but if D has (order-2)-diagonal core, then this core is optimal among all p?*)
matrixExp=MatrixExp[#]&;
getXus=Function[{as,m},Module[{us},us=matrixExp[#-ConjugateTranspose[#]]&[#[[1]]+I #[[2]]]&/@as;
	sortXUs[foldXUs[m,ConjugateTranspose/@us,{}],us]]];
disp=Function[{as,m},Module[{x=getXus[as,m][[1]]},Print[{Abs@First@sortXUs[x//Chop,{}]//MatrixForm,pnorm2[x,1],
	{"unfold",Total@SingularValueList@Flatten[m,order-2]},
	{"hosvd",pnorm2[hosvd[0.01,m,n][[1]],1]},
	{"sud",pnorm2[sparseUnitaryDecomposition[m,n,200,False][[1]],1]}}]]];
vals={};
r=Module[{xus},NMinimize[(*-pnorm2[getXus[as,m][[1]],3]*)pnorm2[getXus[as,m][[1]],1],Flatten[as]
	,StepMonitor:>
		(xus=getXus[as,m];vals=Append[vals,pnorm2[xus[[1]],1]])]];//AbsoluteTiming
disp[as/.r[[2]],m];
MatrixForm/@(Map[SingularValueDecomposition,m,{order-2}])
ListPlot[vals,PlotRange->All]
r2=NMinimize[pnorm2[getXus[as,m][[1]],0.5],Flatten[as]];//AbsoluteTiming
disp[as/.r2[[2]],m];
(*Following shows that sparseUnitaryDecomposition minimizes trace norm better than NMinimize*)
(*SeedRandom[1003];ListPlot[Table[pnorm2[sparseUnitaryDecomposition[m,n,n,False][[1]],1],{n,10,100}],PlotRange->All]
SeedRandom[1003];ListPlot[Table[pnorm[xus=sparseUnitaryDecomposition[m,n,n,False];foldXUs[xus[[1]],xus[[2]],{}]-m,2]/pnorm[m,2]
	,{n,10,200}],PlotRange->All]*)
ms=getXus[as/.#,m][[1]]&/@{r[[2]],r2[[2]]};
Table[pnorm2[#,p]&/@ms,{p,{0.5,1,1.5,1.9}}]

(*SUD is different from HOSVD: SUD minimizes Lp of core, HOSVD maximizes L2 of diagonal.*)
(*xus=sparseUnitaryDecomposition[m,2,300,False];xus2=hosvd[0.00001,m,2,300];
{MatrixForm/@xus,MatrixForm/@xus2}
{pnorm2[foldXUs[#[[1]],#[[2]],{}]-m,1]&/@{xus,xus2},pnorm[#,1]&/@{xus[[1]],xus2[[1]]},pnorm[Diagonal@Diagonal@#,2]&/@{xus[[1]],xus2[[1]]}}*)


(*Same minima for differnt p for matrix*)(*SeedRandom[1003];ms=Array[m,{2,2,2}];d=RandomReal[5{-1,1},{2,2}];MatrixForm/@d*)
(*Same minima for p>1 for high order tensor, different minima for all p<1*)
SeedRandom[1004];order=4;Clear[m];ms=Array[m,{order,2,2}];d=RandomReal[3{-1,1},Array[2&,order]];MatrixForm/@d
(*SeedRandom[1004];order=6;ms=Array[m,{order,2,2}];d=RandomReal[3{-1,1},Array[2&,order]];MatrixForm/@d*)
(*d=ImageData/@ColorSeparate@ImageResize[Import[#],200{1,1}]&/@FileNames["/s/island/*.JPG"];d//Dimensions
ms=MapIndexed[Table[m[#2[[1]],i,j],{i,#},{j,#}]&,Dimensions@d];*)
mynorm=Function[p,If[p<2,1,-1] pnorm2[#,p]&];
opt=Function[p,
		r=NMinimize[mynorm[p]@foldXUs[d,MatrixExp[#-Transpose@#]&/@ms,{}],Flatten@ms
			(*,MaxIterations->100,Method->"DifferentialEvolution"*)
			,MaxIterations->100,Method->"NelderMead"
			(*,MaxIterations->100,Method->"RandomSearch"*)
		];
		tus=Re@MatrixExp[#-Transpose@#]&/@(ms/.r[[2]]);x=foldXUs[d,tus,{}];
		{x,tus}=sortXUs[x,tus];
		{p,r[[1]],Chop@x}];
sx=opt[0.5][[3]];sx2=opt[2.3][[3]];MatrixForm/@{sx,sx2}
SortBy[Flatten@#,-Abs@#&]&/@{sx,sx2}
Table[{#[[1]],#[[2]],mynorm[p][sx],mynorm[p][sx2],MatrixForm/@#[[3]]}&@opt[p]
	,{p,(*{0.2,0.3,0.5,0.9,1,1.1,1.3,1.5,1.9,2.1,3,5,10}*){0.5,1,10}}]


m=Mean[ImageData/@ColorSeparate[ImageResize[Import@"t.jpg",400{1,1}]]];m2=m[[;;;;2,;;;;2]];
SingularValueList/@{m,2m2}
MatrixPlot/@SingularValueDecomposition@m
MatrixPlot/@SingularValueDecomposition@m2
Image@m
Image@m2


Select[Table[u=First@SingularValueDecomposition@RandomReal[1,3{1,1}];m={{{1,0,0},{0,1,0},{0,0,1}},{{0,1,0},{1,0,0},{0,0,0}},{{0,0,1},{0,0,0},{1,0,0}}};
	{MatrixForm/@m,pnorm2[m,1],pnorm2[foldXUs[m,Transpose/@{u,u,u},{}],1]},{10000}],#[[-2]]>#[[-1]]&]


Clear[x,y,sign,t,s];SeedRandom[1003];n=30;m=RandomReal[1,n{1,1}];m=m+Transpose[m];tmax=1;
(*rate=2;m2=DiagonalMatrix@PadRight[rate SingularValueList[m[[;;;;rate,;;;;rate]]],n];*)
sign=Function[x,x/Sqrt[x^2+10^-6]];
m2=DiagonalMatrix[Reverse[Range[n]^2.]];(*Converges faster than Range[n]*)
s=NDSolve[{y'[x]==lieBracket[y[x],lieBracket[y[x],
	(*(*working for largest eigen value, but oscillates for others*)Abs[y[x]+10^-6]^2*)
	(*Finds eigenvalues very well*)m2
	(*(*working for largest eigen value, but oscillates for others*)-Tanh[10y[x]]*)
	]],y[0]==m},y,{x,0,tmax},MaxSteps->10000];//AbsoluteTiming
(*Chop@MatrixForm[sortXUs[y[tmax]/.s,{}][[1]]]*)
mysingulars=Function[{y,x,s},Abs@Diagonal[First@sortXUs[First[y[x]/.s],{}]]];
(Export["t3.png",#];#)&@Rasterize[{ListLogPlot[{SingularValueList@m,mysingulars[y,tmax,s]},PlotRange->All]
	,ListLogPlot[Transpose@Table[mysingulars[y,x,s],{x,0,tmax,0.1}],Joined->True]},ImageSize->900]

(*Solve for the unitary matrix*)
t=NDSolve[{y'[x]==y[x].lieBracket[
	(*(*oscillates for all*)Abs[Transpose[y[x]].m.y[x]+10^-6]^2*)
	(*Finds eigenvalues very well*)m2
	(*(*oscillates for all*)-Tanh[10Transpose[y[x]].m.y[x]]*)
	,Transpose[y[x]].m.y[x]
	],y[0]==IdentityMatrix@Length@m},y,{x,0,tmax},MaxSteps->10000];//AbsoluteTiming
mysingulars2=Function[{y,x,s},Abs@Diagonal[First@sortXUs[With[{u=First[y[x]/.s]},Transpose[u].m.u],{}]]];
(Export["t4.png",#];#)&@Rasterize[{ListLogPlot[{SingularValueList@m,mysingulars2[y,tmax,t]},PlotRange->All]
	,ListLogPlot[Transpose@Table[mysingulars2[y,x,t],{x,0,tmax,0.1}],Joined->True]},ImageSize->900]


Clear[x,y,sign,u,v,t];SeedRandom[1003];n=30;dim=n{1,1};m=RandomReal[1,dim];m2=Normal@SparseArray[{i_,j_}/;i==j->(n+1-i)^2,Reverse@dim];tmax=1;
s=NDSolve[{u'[t]==0.5(u[t].Transpose[m.v[t].m2].u[t]-m.v[t].m2),v'[t]==0.5(v[t].m2.Transpose[u[t]].m.v[t]-Transpose[m2.Transpose[u[t]].m]),
	u[0]==IdentityMatrix@dim,v[0]==IdentityMatrix@dim[[2]]},{u,v},{t,0,tmax},MaxSteps->10000];//AbsoluteTiming
mysingulars3=Function[{u,v,t,s,m},Abs@Diagonal[First@sortXUs[With[{U=First[u[t]/.s],V=First[v[t]/.s]},Transpose[U].m.V],{}]]];
{Norm[SingularValueList@m-mysingulars3[u,v,tmax,s,m]]/Norm@SingularValueList@m,SingularValueList@m,mysingulars3[u,v,tmax,s,m]}
ListLogPlot[Transpose@Table[mysingulars3[u,v,t,s,m],{t,0,tmax,0.1}],Joined->True]


SeedRandom[1003];n=3;m=RandomReal[1,n{1,1,1}];{X,Us}=sparseUnitaryDecomposition[m,n,100,False];
X2=X;X2[[n,n,n]]=0;m2=foldXUs[X2,Us,{}];l={m,X,foldXUs[X,Us,{}],m2};
MatrixForm/@l
pnorm2[#,2]&/@l
pnorm2[#,1]&/@l
{X[[n,n,n]]^2,pnorm2[m2-m,2]}
Table[MatrixForm[#.Transpose[#]]&@unfoldTensor[X,i],{i,Length@Dimensions@X}]


(*||||_ 1 is not convex on Stiefel Manifold*)
ms=Table[(#-Transpose[#])&@RandomReal[3{-1,1},{4,4}],{2}];
ListPlot[Transpose@Table[{pnorm2[MatrixExp[ms[[1]]],1](1-t)+pnorm2[MatrixExp[ms[[2]]],1]t,pnorm2[MatrixExp[ms[[1]](1-t)+ms[[2]]t],1]}
	,{t,0,1,0.03}],Joined->True,PlotRange->All]


{X,Us}=sparseUnitaryDecomposition[d,50,30,False];folded=foldXUs[X,Us,{}];
pnorm[folded-d,1]/pnorm[d,1]
ListLogPlot[Reverse@SortBy[Flatten@X,Abs],PlotRange->All]


(*Inducing block diagonal, but here we miss another constraint to make Z meaningful*)
Z=Array[z,{3,3}];
r=NMinimize[With[{Z2=Z-DiagonalMatrix@Diagonal@Z},Total@Flatten@Abs[Transpose[Z2].Z2]],Flatten@Z]
Z-DiagonalMatrix@Diagonal@Z/.r[[2]]//MatrixForm


(*a={1,0};b={0,1};ss={3,2,1};
A=Total[ss {TensorProduct[a,b,b],TensorProduct[b,b,b],TensorProduct[a,a,a]}];MatrixForm/@A*)
SeedRandom[1003];
{U,V}=Table[First@SingularValueDecomposition@RandomReal[1,{3,3}],{2}];
A=2TensorProduct@@U+TensorProduct@@V;MatrixForm/@A
Table[{X,Us}=sparseUnitaryDecomposition[A,r,100,False];folded=foldXUs[X,Us,{}];
	{Norm@Flatten[folded-A]/Norm@Flatten@A,MatrixForm/@folded,MatrixForm/@X,pnorm[X,1]},{r,{1,3}}]


X2=0X;X2[[1,1,1]]=X[[1,1,1]];X2[[1,2,2]]=X[[1,2,2]];X2[[2,2,2]]=X[[2,2,2]];X=X2
folded=foldXUs[X,Us,{}];{Norm@Flatten[folded-A]/Norm@Flatten@A,MatrixForm/@folded,MatrixForm/@X,pnorm[X,1]}


p=4;n=4;SeedRandom[1003];m=RandomReal[1,n{1,1}];MatrixForm/@SingularValueDecomposition@m
Clear[a,b,f];{as,bs}=Array[#,n{1,1}]&/@{a,b};
(*nilTrace=ReplacePart[#,{n,n}->-(Tr@#-#[[n,n]])]&;g=MatrixExp[nilTrace@#]&;(*Special Linear group produces diagonals of uniform values.*)*)
g=MatrixExp[#-Transpose[#]][[;;p]]&;(*If not full rank, only capture the least singular values, and also a zero(?)*)
f[as_?(NumericQ[#[[1,1]]]&),bs_]:=Total@Flatten@Abs[g[as].m.Transpose[g[bs]]];
r=NMinimize[f[as,bs],Flatten[as]~Join~Flatten[bs]];//AbsoluteTiming
{ga,gb}=g[#/.r[[2]]]&/@{as,bs};
MatrixForm/@{ga,gb,First@sortXUs[ga.m.Transpose@gb,{}]}


n=80;SeedRandom[1003];m=RandomReal[1,n{1,1}];xus=sparseUnitaryDecomposition[m,n,40,False];
folded=foldXUs[xus[[1]],xus[[2]],{}];x2=foldXUs[m,Transpose/@xus[[2]],{}];
pnorm2[#,1]&/@{SingularValueList@m,xus[[1]],x2,m,folded}
pnorm[#,2]&/@{m,m-folded}


(*n=3;SeedRandom[1003];m=RandomReal[1,n{1,1,1}];*)
n=2;m={{{0,1},{1,0}},{{1,0},{0,0}}};
Clear[u1,u2,u3,f];us=Array[#,{n,n}]&/@{u1,u2,u3};
g=MatrixExp[#-Transpose[#]]&;
f[us_?(NumericQ[#[[1,1,1]]]&)]:=pnorm2[foldXUs[m,g/@us[[;;3]],{}],1];
r=NMinimize[f[us],Flatten[us]];//AbsoluteTiming
gus=g[#/.r[[2]]]&/@us;MatrixForm/@gus
folded=foldXUs[m,gus,{}];Map[MatrixForm,sortXUs[folded,gus],{2}]
pnorm2[#,1]&/@{m,folded}
(*rank-1 approximation*)
Clear[x,f2];
f2[x_,us_?(NumericQ[#[[1,1,1]]]&)]:=pnorm2[m-x TensorProduct@@(First@g[#]&/@us),1];
r2=NMinimize[f2[x,us],Append[Flatten[us],x]];//AbsoluteTiming
gus2=First@g[#/.r2[[2]]]&/@us;Prepend[MatrixForm/@gus2,x/.r2[[2]]]
m2=(x/.r2[[2]]) TensorProduct@@gus2;
(*truncated SUD*)
ListPlot[Table[With[{m2=foldXUs[truncTensor[folded,i],Transpose/@gus,{}]},pnorm2[m-m2,2]],{i,n^3}],PlotRange->All]
(*orth-rank-k approximation*)
k=2;xs=Array[x,Dimensions@m];diag=Normal@SparseArray[{i_,j_,l_}/;i==j==l&&l<=k->1,Dimensions@m];
f4[xs_,us_?(NumericQ[#[[1,1,1]]]&)]:=pnorm2[m-foldXUs[xs diag,g[#]&/@us,{}],1];
r4=NMinimize[f4[xs,us],Join[Flatten[us],Flatten@xs]];//AbsoluteTiming
gus4=g[#/.r4[[2]]]&/@us;Join[MatrixForm/@gus4,MatrixForm/@(xs/.r4[[2]])]
m4=foldXUs[(xs/.r4[[2]]) diag,gus4,{}];
{tm1,tm2,tm3}=foldXUs[truncTensor[folded,#],Transpose/@gus,{}]&/@{1,2,3};
pnorm2[#,2]&/@{m,m2,m4,tm1,tm2,tm3,m-m2,m-m4,m-tm1,m-tm2,m-tm3}


(*n=2;SeedRandom[1003];randomUnitaryMatrix=First@SingularValueDecomposition@RandomReal[1,#{1,1}]&;
m=RandomReal[1,n{1,1,1}];*)
\[Lambda]=0.1;
(*m=foldXUs[Normal@SparseArray[{{1,1,1}->1},n{1,1,1}],Table[randomUnitaryMatrix@n,{n}],{}]*)
Clear[u1,u2,u3,s,f];us=Array[#,{n,n}]&/@{u1,u2,u3,s};
(*nilTrace=ReplacePart[#,{n,n}->-(Tr@#-#[[n,n]])]&;g=MatrixExp[nilTrace@#]&;(*Special Linear group produces diagonals of uniform values.*)*)
g=MatrixExp[#-Transpose[#]]&;
f[us_?(NumericQ[#[[1,1,1]]]&)]:=pnorm2[foldXUs[m-\[Lambda] us[[4]],g/@us[[;;3]],{}],1]+\[Lambda] pnorm2[us[[4]],1];
r=NMinimize[f[us],Flatten[us]];//AbsoluteTiming
gus=Append[g[#/.r[[2]]]&/@us[[;;3]],us[[4]]/.r[[2]]];MatrixForm/@gus
folded=foldXUs[m-\[Lambda] gus[[4]],gus[[;;3]],{}];MatrixForm/@First@sortXUs[folded,{}]
pnorm2[#,1]&/@{m,folded,gus[[4]]}


(*For RGB images, if we shuffle one of them by rows, then the unfolded trace norm will not change in one unfolding?*)
rgbs=ImageData/@ColorSeparate@ImageResize[Import@"t.jpg",100{1,1}];SeedRandom[1003];
columnShuffle=Transpose@RandomSample@Transpose@#&;
{Image@#,Total@SingularValueList@#}&/@
	{Mean@rgbs,columnShuffle@RandomSample@Mean@rgbs,ArrayFlatten@{rgbs},ArrayFlatten@{{RandomSample@rgbs[[1]],rgbs[[2]],rgbs[[3]]}}}


d2=Floor[Dimensions[xus[[1]]]/1.5];
folded2=foldXUs[xus[[1,;;d2[[1]],;;d2[[2]],;;d2[[3]]]],{xus[[2,1,;;,;;d2[[1]]]],xus[[2,2,;;,;;d2[[2]]]],xus[[2,3,;;,;;d2[[3]]]]},{}];
Image/@folded2


Image@#&/@xus[[1]]
MatrixForm@xus[[1,1,;;30,;;30]]
folded=foldXUs[xus[[1]],xus[[2]],{}];
Image/@folded
X2=foldXUs[rgbs,Transpose/@xus[[2]],{}];
Image/@X2
MatrixForm@X2[[1,;;30,;;30]]
{pnorm2[xus[[1]],1],pnorm2[X2,1]}


rgbs=ImageData/@Flatten[ColorSeparate@ImageResize[Import@#,100{1,1}]&/@FileNames["/s/island/*.JPG"]];
rgbs[[1]]//Image


xus=sparseUnitaryDecomposition[rgbs,100,80,False];//AbsoluteTiming


r=SingularValueDecomposition@rgbs[[1]];
Image[Transpose[r[[1]]].#.r[[3]]]&/@rgbs
Chop@MatrixForm[Transpose[r[[1]]].#.r[[3]]]&/@rgbs


ts=RandomReal[1,2];s=DiagonalMatrix@RandomReal[1,3];s
w=RandomReal[{-1,1},3];w
ListPlot[Table[pnorm2[RotationMatrix[ts[[1]](1-t)+ts[[2]]t,w].s,1],{t,0,1,0.01}],Joined->True]


(*SeedRandom[1003];*)n=2;
ms=Table[First@SingularValueDecomposition@RandomReal[1,n{1,1}],{2}];
ms={ms[[1]],ms[[1]].MatrixPower[ms[[2]],1/10]};
s=SingularValueDecomposition[RandomReal[1,n{1,1}]][[2]];
ListPlot[pnorm2[#.s,1]&/@
	Table[MatrixExp[MatrixLog[ms[[1]]](1-t)+MatrixLog[ms[[2]]]t],{t,0,1,0.01}],Joined->True]
MatrixForm/@ms
Eigenvalues/@ms
MatrixForm@MatrixLog@#&/@ms


(*When doing a clustering, we can 
min ||DYY^T-D||_F^2+\[Lambda]||YY^T||_ 1 s.t. Y^T Y=I
*)
Clear[f,as,a];n=3;p=2;as=Array[a,n{1,1}];m=RandomReal[1,{n,2}];\[Lambda]=0.01;(*m corresponds to D^T*)
h=Function[x,MatrixExp[x-Transpose[x]][[;;,;;p]]];
g={pnorm2[Transpose[m].#.Transpose[#]-Transpose[m],2],\[Lambda] pnorm2[#.Transpose[#],1]}&;
f[x_?MatrixQ]:=With[{y=h[x]},Total@g[y]]
r=NMinimize[f[as],Flatten[as]];//AbsoluteTiming
y=h[as/.r[[2]]]
{pnorm2[m,2],g[y],Total@g[y],MatrixForm@y,MatrixForm[y.Transpose[y]],MatrixForm[Transpose[y].y]}
MatrixForm/@{Transpose[m].y.Transpose[y],Transpose@m}
Total@g[h[RandomReal[1,n{1,1}]]]


(*Stiefel-based approach not working*)
SeedRandom[1003];n=50;p=3;m=RandomReal[1,{n,2}];Clear[f];tmax=1;
\[Lambda]=1;c=Array[\[Lambda]&,n{1,1}]-m.Transpose[m];
f[x_?MatrixQ]:=With[{grad=c.x},grad-x.Transpose[x].grad]
x0=RandomReal[1,{n,p}];
s=NDSolve[{
    x'[t]==f[x[t]]
,x[0]==x0},x,{t,0,tmax},MaxSteps->1000];//AbsoluteTiming
getResult=Function[{x,s,t},First[x[t]/.s]];
g=pnorm2[Transpose[m].#.Transpose[#]-Transpose[m],2]+\[Lambda] pnorm2[#.Transpose[#],1]&;
xt=getResult[x,s,tmax];
{g[#](*,#//MatrixForm*)}&/@{x0,xt}
Graphics[Riffle[Point/@m,Switch[Position[#,Max@#][[1,1]],1,Blue,2,Red,3,Darker@Yellow]&/@xt],Axes->True,PlotRange->All]
(*(Export["t.png",#];#)&@ListLogPlot[Transpose@Table[Flatten@getResult[x,s,t],{t,0,tmax,0.1}],Joined->True]
(Export["t2.png",#];#)&@ListLogPlot[Table[Norm[a.getResult[x,s,t]-b,"Frobenius"],{t,0,tmax,0.1}],Joined->True]*)


Select[Table[n=5;m=RandomReal[20{-1,1},n{1,1}];r=SingularValueDecomposition@m;
	pnorm2[#-dShrinkage[1,#],2]&/@{r[[2]],m}
	(*{entropy[r[[2]]],entropy[m]}*),{10000}],#[[1]]>#[[2]]&]


SeedRandom[1003];{a,b,x}=RandomReal[1,{3,5,5}];atb=Transpose[a].b;a2=Transpose[a].a;
MatrixForm/@NestList[#+0.01(a2.#-atb)&,x,50]
LinearSolve[a,b]//MatrixForm


(*SeedRandom[1003];n=10;d=Transpose[#].#&@RandomReal[1,n{1,1}];*)
d=t;
Nm=DiagonalMatrix[Range[n]^1.5];M=0 Nm;
(*matrixExp=Re@MatrixExp@#&;*)
matrixExp=Function[m,Total[NestList[m.#&,IdentityMatrix@n,15] Table[1./i!,{i,0,15}]]];
IdentityMatrix@n+Total@Table[MatrixPower[#,i]/i!,{i,8}]&;
skew=(#-Transpose[#])/2&;\[Alpha]=0.0003/n;
l=NestList[skew[#+\[Alpha] lieBracket[Nm,matrixExp[-#].d.matrixExp[#]]]&,M,500];//AbsoluteTiming
r=ListLogPlot[Transpose[Abs@Diagonal[matrixExp[-#].d.matrixExp[#]]&/@l],PlotRange->All,Joined->True];//AbsoluteTiming
(Export["t.png",#];#)&@r
U=matrixExp@l[[-1]];
(Export["t2.png",#];#)&@
	ListLogPlot[{SingularValueList@d,Reverse@Sort@Diagonal[Transpose[U].d.U]},PlotRange->All,PlotLegends->{"Truth","Approx."}]


SeedRandom[1003];n=70;m=#.Transpose[#]&@RandomReal[1,n{1,1}];(*SingularValueList@m*)
b1=0;v1=Normalize@RandomReal[1,n];v0=0 v1;as={};bs={};
NestList[Module[{b1,v1,v0,w,a,b},{b1,v1,v0}=#;w=m.v1;a=w.v1;w=w-a v1-b1 v0;b=Norm@w;AppendTo[as,a];AppendTo[bs,b];{b,w/b,v1}]&,{b1,v1,v0},n];
bs=Most@bs;
t=SparseArray[{Band[{1,1}]->as,Band[{2,1}]->bs,Band[{1,2}]->bs},{n,n}];
(*SingularValueList@t*)
(*l=CholeskyDecomposition[t];
l//MatrixForm
Sqrt@SingularValueList@t*)


assembleN2=Function[ms,Module[{dim=Dimensions@ms[[1]],n=Sqrt@Length@ms}
	,Table[ms[[n(Mod[i-1,n])+Mod[j-1,n]+1,1+Floor[(i-1)/n],1+Floor[(j-1)/n]]],{i,1,n dim[[1]]},{j,1,n dim[[2]]}]]];
breakToN2=Function[{m,n},Join@@Table[m[[i;;;;n,j;;;;n]],{i,n},{j,n}]];k=4;
rawImg=ImageData@ColorConvert[ImageResize[Import["t.jpg"],dim],"Grayscale"];
imgs=Image/@breakToN2[rawImg,k];numIter=100;missingRatio=0.9;
{gray,rgb,\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim/k,missingRatio];
Print[{L2,S2}=RpcaColor2[gray,rgb \[CapitalOmega],\[CapitalOmega],numIter,k^2];//AbsoluteTiming];
l=Transpose[Partition[#,dim[[2]]/k]&/@L2];
assembleN2[l,k]//Image
ImageResize[gray//Image,k Dimensions@gray]
Norm[(1-\[CapitalOmega])(#-rgb),"Frobenius"]&/@{L2}
Norm[gray-Mean[foldToTensor[#,Prepend[Dimensions@gray,3],2]],"Frobenius"]&/@{L2}


SeedRandom[1003];{n,k}=2{5,15};d=RandomReal[1,n];A=RandomReal[1,{n,k}];Clear[x,f];xs=Array[x,k];
{g,g2}={pnorm2[#,1]&,(*-pnorm2[#,3]&*)pnorm[#,0.5]&};
(*f[xs_]:=0.5Total@Exp@Abs[d-A.xs]+g[xs];*)
f[g_]:=Function[xs,0.5pnorm2[d-A.xs,2]+0.1g[xs]];
r=NMinimize[f[g][xs],xs];
ListPlot[xs/.r[[2]],PlotRange->All]
r12=NMinimize[f[g2][xs],xs];
ListPlot[xs/.r12[[2]],PlotRange->All]
{0.5pnorm2[d-A.(xs/.r[[2]]),2],g[(xs/.r[[2]])]}
Clear[f2];f2[xs_]:=PseudoInverse[A].d+(IdentityMatrix@k-PseudoInverse[A].A).xs;
r2=NMinimize[g@f2[xs],xs];
ListPlot[f2[xs/.r2[[2]]],PlotRange->All]
r22=NMinimize[g2@f2[xs],xs];
ListPlot[f2[xs/.r22[[2]]],PlotRange->All]


(*This also induces sparsity.*)
r3=NMinimize[{-Total[If[#==0,0,Abs[#]^2 Log[Abs@#]]&/@xs],Total[xs^2]==1},xs];
ListPlot[xs/.r3[[2]],PlotRange->All]


SeedRandom[1003];m=(*#.Transpose[#]&@*)RandomReal[1,8{1,1}];
(Export["t.txt",#];#)&@StringJoin@Riffle[Prepend[StringJoin@Riffle[MapIndexed[StringJoin[ToString/@{#2[[1]]-1,":",#}]&,#]," "]&/@m
	,StringJoin[Riffle[ToString/@Dimensions@m," "]]],"\n"];
SingularValueList[Transpose[m].m]
SingularValueList[m]
MatrixForm/@SingularValueDecomposition@m


(*Cross coloring by matching?*)
img=ColorConvert[ImageResize[Import@"t.jpg",dim],"Grayscale"];ref=ImageResize[Import@"t3.jpg",dim];
images={img,ColorConvert[ref,"Grayscale"]};
matches=ImageCorrespondingPoints@@images;matches
MapThread[Show[#1,Graphics[{Yellow,MapIndexed[Inset[#2[[1]],#1]& ,#2]}]]&,{images,matches}]

(*Cross coloring by luminance*)
img=ColorConvert[ImageResize[Import@"t2.jpg",dim],"Grayscale"];ref=ImageResize[Import@"t4.jpg",dim];
limg=First@ColorSeparate[img,"LAB"];
{lref,aref,bref}=ColorSeparate[ref,"LAB"];
l2=HistogramTransform[limg,lref];
ImageHistogram/@{l2,lref}
radius=2;
{neighimg,neighref}=ColorCombine[{
MeanFilter[#,radius],
StandardDeviationFilter[#,radius]}]&/@{l2,lref};
nfun=Nearest[Flatten[ImageData@neighref,1]->Transpose[Flatten@ImageData@#&/@{aref,bref}]];
col=Map[First@nfun[#,1]&,ImageData@neighimg,{2}];
ColorConvert[Image[Join[{ImageData@limg},Transpose[col,{2,3,1}]],Interleaving->False,ColorSpace->"LAB"],"RGB"]


SeedRandom[1003];m=RandomReal[1,{2,2,2,2,2}];MatrixForm/@hosvd[0.001,m,2]
MatrixForm/@sparseUnitaryDecomposition[m,2,200,False]


(*Experiment on limited search space*)
SeedRandom[1003];n=3;order=3;Clear[a,x,f];vars=Array[a,order];as=Array[#,n{1,1}]&/@vars;tau = 0.05; m = RandomReal[1, Array[n&,order]];
matrixExp = Re[MatrixExp[#]]&;
{MatrixForm /@ Map[SingularValueDecomposition,m,{order-2}],MatrixForm/@sparseUnitaryDecomposition[m,n,300,False]}
test = Function[p, Clear[f, f2, g, g2]; xs = Array[x, Array[p&,order]];
     g = Function[{as, ts}, foldXUs[ts,matrixExp[# - Transpose[#]][[;;,;;p]]&/@as,{}]];
	 f[(as_)?(NumericQ[#1[[1,1,1]]] & ), xs_] :=tau*pnorm2[xs, 1] + (1/2)*pnorm2[g[as,xs] - m, 2]; 
     Print[AbsoluteTiming[r = NMinimize[f[as, xs], Flatten[{as, xs}]]; ]]; 
     g2 = Function[{as, ts}, foldXUs[ts,Transpose[matrixExp[# - Transpose[#]][[1 ;; All,1 ;; p]]]&/@as,{}]];
	 f2[(as_)?(NumericQ[#1[[1,1,1]]] & ), xs_] := tau*pnorm2[xs, 1] + (1/2)*pnorm2[g2[as, m] - xs, 2]; 
     Print[AbsoluteTiming[r2 = NMinimize[f2[as, xs], Flatten[{as, xs}]]; ]];
     {MatrixForm[xs /. r[[2]]], MatrixForm /@ (as /. r[[2]]), MatrixForm[xs /. r2[[2]]], MatrixForm /@ (as /. r2[[2]])}];
test[3]
test[2]


(*Suppose p-SUD of D has optimal X^* (p), then let Subscript[k, 1] be max tensor singular value, and Subscript[k, -1] be min tensor singular value,
let X^^ (q) be our suboptimum:
For q<2, ||Subscript[k, 1] X^^ (q)Subsuperscript[||, p, p]>=||Subscript[k, 1] X^* (q)Subsuperscript[||, p, p]>=orank(D)
For q>2, ||X^^ (q)/Subscript[k, -1]Subsuperscript[||, p, p]<=||X^* (q)/Subscript[k, -1]Subsuperscript[||, p, p]<=orank(D)

Do we have Subscript[k, 1]>max singular of unfoldings of D? And similarily for Subscript[k, -1]?*)


Rpca2T=Function[{D,\[CapitalOmega],iter},Module[{norm2=Sqrt[Total@Flatten[D^2]/Total@Flatten[\[CapitalOmega]^2]],
		\[Lambda]=10.,\[Eta]=10.,Y,L,S,\[Mu],\[Rho],Zl,Zs,Us,order=Length@Dimensions@D,rank=Max@Dimensions@D},(*L,S,D are m-by-n. \[CapitalOmega] is nonzeros*)
	Y=0D;\[Mu]=1.25/norm2;\[Rho]=1.01;L=S=0 D;(*Print[{\[Mu],MatrixForm/@D}];*)
	Us=Table[Transpose@Orthogonalize@Transpose@RandomReal[1,{Dimensions[D][[i]],rank}],{i,order}];
	Do[
    Zl=(Y+\[Mu](D-S))/\[Mu];
	(*Print[L=zShrinkage[1/\[Mu],Zl,rank,10];//AbsoluteTiming];*)
	Print[{L,Us}=zShrinkageWithHint[1/\[Mu],Zl,30,Us];//AbsoluteTiming];
	Zs=Y/\[Mu]+D-L;
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];(*If[Mod[j,10]==0,PrintTemporary@Image[L]];*)(*(scores=Append[scores,#];Print[#])&@evalPredicted[L,testM2];*)
	Print[Total[Flatten[(1-mask)(data-L)]^2]];
	,{j,iter}];
	{L,S}]];
SeedRandom[1003];
n=10;data=ImageData/@ColorSeparate@ImageResize[Import@"t.jpg",n];mask=randomSparseTensor[Dimensions@data,0.3];
Image/@(data mask)
{L,S}=Rpca2T[data mask,mask,30];//AbsoluteTiming
ImageResize[ColorCombine[Image/@L],400]
ColorCombine[Image/@S]
Total[Flatten[(1-mask)(data-L)]^2]
