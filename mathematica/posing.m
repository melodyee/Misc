(* ::Package:: *)

case={"pingpong","rect","crab","lego","updown"}[[3]];
imageDir="/h/d/video_with_imu/"<>case<>"/";timestampedImages={parseTimeStamp@FileBaseName[#],Import[#(*,ImageSize->400*)]}&/@FileNames[imageDir<>"/*.JPG"];
imuPoses=Import["/g/"<>case<>"_imu_pose.csv"];
Export["~/mxs/"<>case<>"_ba.mx",{imageDir,timestampedImages,imuPoses}];


dq=quaternionMul[Normalize@Mean[Normalize/@imuPoses[[;;23,9;;12]]],quaternionConjugate@Normalize@Mean[Normalize/@poses[[;;23,5;;8]]]]
blendCameras@MapThread[Join,{5 poses[[;;23,2;;4]],quaternionMul[dq,Normalize@#]&/@poses[[;;23,5;;8]]}]
blendCameras@MapThread[Join,{5 imuPoses[[;;23,2;;4]],imuPoses[[;;23,9;;12]]}]
(*blendCameras@MapThread[Join,{5 imuPoses[[;;23,2;;4]],imuPoses[[;;23,5;;8]]}]*)


case={"rect","crab","lego","pingpong"}[[2]];{imageDir,timestampedImages,imuPoses}=Import["~/mxs/"<>case<>"_ba.mx"];
(*poses=SortBy[Prepend[#[[2;;]],parseTimeStamp@StringSplit[#[[1]],":"][[-1]]]&/@Import["/g/"<>case<>"_poses.csv"],First];*)
(*poses=SortBy[Import["/g/"<>case<>"_poses.csv"],First];*)
blendCameras@MapThread[Join,{5 imuPoses[[;;;;4,2;;4]],imuPoses[[;;;;4,5;;8]]}]
(*(*Magnify[#,10]&@*)blendCameras@MapThread[Join,{5 poses[[;;,2;;4]],
	quaternionMul[quaternionConjugate@Normalize@#,quaternionMul[{Sqrt[2]/2,0,0,Sqrt[2]/2},{0,1,0,0}]]&/@Normalize/@poses[[;;,5;;8]]}]*)


getVelocityFromHomography=Function[{m,dim},Append[m[[;;2,3]]/Max@dim,Log@Det@m[[;;2,;;2]]]];
procImage=Function[img,ImageResize[ColorConvert[img,"Grayscale"],ImageDimensions[img]/2]];
qInit=oq=q={1,0,0,0};doInit=True;translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[CurrentImage[]]},{0,0,1}};
oimg=img=procImage@CurrentImage[];dim=Reverse@ImageDimensions@img;vs=createClosureBuffer[10];vs2=createClosureBuffer[10];
addAndShow=Function[{buf,v},Module[{vs=buf[v]},Magnify[#,2]&@{ListLinePlot[vs[[;;,4]],PlotRange->{Automatic,{0,1.0}},PlotLabel->"Error"]
		,(*ListLinePlot[MedianFilter[#,1]&/@Transpose@vs[[;;,;;3]],PlotRange->{Automatic,{-0.3,0.3}(*Automatic*)},PlotLabel->"Velocity"]*)
		Graphics3D[BlendLine@Accumulate@vs[[;;,;;3]],Axes->True]}]];
Dynamic[oq=q;oimg=img;
	q=getQuaternionAdb[];
	If[NumericQ@q[[1]]
		,If[doInit,doInit=False;qInit=q];img=procImage@CurrentImage[];
		tr=TimeConstrained[FindGeometricTransform[oimg
			,transformImagePhoneImuFaceFrontHeadRight[img,oq,q,ImageDimensions@img]
			,"Transformation"->"Similarity"],0.05,Null];
		tr2=TimeConstrained[FindGeometricTransform[oimg
			,img
			,"Transformation"->"Similarity"],0.05,Null];
	];
	With[{rot=quaternionToRotationMatrix@q},
	{If[NumericQ[tr[[1]]],addAndShow[vs,Append[rot.getVelocityFromHomography[TransformationMatrix@tr[[2]],dim],tr[[1]]]]]
		,If[NumericQ[tr2[[1]]],addAndShow[vs2,Append[rot.getVelocityFromHomography[TransformationMatrix@tr2[[2]],dim],tr2[[1]]]]]}]]


(*For collecting photos annotated with quaternions*)
ImageCapture[RasterSize->{640,480}];
qInit=q={1,0,0,0};doInit=True;imgDim=ImageDimensions@CurrentImage[];Print[imgDim];qsBuf=createClosureBuffer[100];imgsBuf=createClosureBuffer[100];
Dynamic@Module[{img=CurrentImage[],q=getQuaternionAdb[]},
	If[NumericQ@q[[1]],If[doInit,doInit=False;qInit=q];qs=qsBuf[q];imgs=imgsBuf[img];transformImagePhoneImuFaceFrontHeadRight[img,qInit,q]]]


{qs,imgs}=Import@"~/mxs/doodle.mx";(*"~/mxs/wardrobe.mx";*)
dqs=quaternionFromRotationMatrix[LinearSolve@@(quaternionToRotationMatrix/@#)]&/@Partition[qs,2,1];
(*MapThread[{anaglyph@#,anaglyphBrightness@#}&@{#[[1]],transformImagePhoneImuFaceFrontHeadRight[#[[2]],#2[[1]],#2[[2]]]}&
	,{Partition[imgs,2,1][[;;]],Partition[qs,2,1][[;;]]}]*)
trs4=Parallelize[FindGeometricTransform[#[[1]],#[[2]],"Transformation"->(*"Perspective"*)"Similarity"]&/@Partition[imgs,2,1]];
tms4=TransformationMatrix[#[[2]]]&/@trs4;
(*ListLinePlot[
	{ArcCos@First@quaternionFromRotationMatrix@ArrayFlatten@{{#/Sqrt[Det[#]]&[#[[;;2,;;2]]],0},{0,1}}&/@tms4,Join[Array[0&,1],ArcCos@First@#&/@dqs]}
	,PlotRange->All,ImageSize->800]*)
(*strs4=Parallelize@MapThread[FindGeometricTransform[#2[[1]],ImagePerspectiveTransformation[#2[[2]],Inverse[quaternionToRotationMatrix[#]]]
	,"Transformation"->"Perspective"]&,{dqs,Partition[imgs,2,1]}];
stms4=TransformationMatrix[#[[2]]]&/@strs4;*)
w=ImageDimensions[imgs[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
Parallelize@Table[With[{i1=imgs[[i]],i2=imgs[[i+1]]},
	{anaglyph@{i1,i2},anaglyph@{i1,ImagePerspectiveTransformation[i2,Inverse[scaling].tms4[[i]].scaling]}}],{i,1,Length@imgs-1,4}]


(*{i1,i2}=imgs[[15;;16]];*)
Parallelize[Function[pair,Module[{i1,i2,tr,w,scaling,tm},{i1,i2}=pair;
tr=FindGeometricTransform[i1,i2];Print[tr[[1]]];
w=ImageDimensions[imgs[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
tm=Inverse[scaling].TransformationMatrix[tr[[2]]].scaling;
{anaglyph@{i1,i2},anaglyph@{i1,ImagePerspectiveTransformation[i2,tm]}}]]/@Partition[imgs,2,1][[;;;;4]]]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[-1]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
dqs=quaternionFromRotationMatrix[LinearSolve@@(quaternionToRotationMatrix/@#)]&/@Partition[qs,2,1];
imgs=timestampedImages[[;;,2]];translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
w=ImageDimensions[imgs[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
selection={40,41};
tm=Inverse[scaling].TransformationMatrix[FindGeometricTransform[imgs[[selection[[1]]]],imgs[[selection[[2]]]]][[2]]].scaling;
translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
qtm=(*Inverse[translation].*)Inverse[LinearSolve@@(quaternionToRotationMatrix/@qs[[selection]])](*.translation*);
{i1,i2}=imgs[[selection]];
anaglyph@{i1,i2}
anaglyph@{i1,ImagePerspectiveTransformation[i2,tm]}
(*anaglyph@{imgs[[1]],ImagePerspectiveTransformation[imgs[[2]]
	,Inverse[Inverse[translation].quaternionToRotationMatrix[dqs[[1]]].translation]]}*)
anaglyph@{i1,ImagePerspectiveTransformation[i2
	,qtm]}
(*anaglyph@{imgs[[1]],ImagePerspectiveTransformation[imgs[[2]]
	,quaternionToRotationMatrix[dqs[[1]]]]}*)
MatrixForm/@{tm,qtm}


With[{tr=FindGeometricTransform[imgs[[2]],#,"Transformation"->"Perspective"]}
		,{If[NumericQ@#,#]&@tr[[1]],imgs[[2]],#,If[NumericQ[TransformationMatrix[tr[[2]]][[1,1]]],
			anaglyph@{imgs[[2]],ImagePerspectiveTransformation[#,TransformationMatrix@tr[[2]],DataRange->Full]}]}]&/@imgs[[2;;;;10]]


SeedRandom[1003];
n=15;i1=imgs[[n]];
(*Parallelize[With[{tr=findSimilarityHomography[imgs[[{n,#}]]]},
		{#,imgs[[#]]}~Join~If[tr=!=Null,{tr[[1]],anaglyph@{imgs[[n]]
			,ImagePerspectiveTransformation[imgs[[#]],TransformationMatrix@tr[[2]],DataRange->Full]}},{}]]&/@
	Sort@RandomSample[Select[Range[Length@qs],quaternionDistance[qs[[#]],qs[[n]]]<0.1&],10]]*)
neighbors=Parallelize@Select[Sort@randomSampleAtMostN[Select[Range[Length@qs],quaternionDistance[qs[[#]],qs[[n]]]<0.1&],10],
	With[{tr=findSimilarityHomography[imgs[[{n,#}]]]},tr=!=Null&&tr[[1]]>0.01]&]
ImageRotate[#,-Pi/2]&@Image@Mean@Parallelize[With[{tr=findSimilarityHomography[imgs[[{n,#}]]]},
	ImageData@ImagePerspectiveTransformation[imgs[[#]],TransformationMatrix@tr[[2]],DataRange->Full]]&/@neighbors]


ListPlot@Transpose[accelsF/@timestampedImages[[;;,1]]]
translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
(*ListAnimate@*)MapThread[{#,(*ImageReflect[#,Top->Left]*)(*ImageRotate[#,-Pi/2]*)#&@
		ImagePerspectiveTransformation[(*ImageRotate[#,Pi/2]*)(*ImageReflect[#,Left->Top]*)#,Inverse[translation].#2.translation]}&
	,{imgs,LinearSolve@@(quaternionToRotationMatrix/@{qs[[1]],#})&/@qs}][[;;,2]]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[-1]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
dqs=quaternionFromRotationMatrix[LinearSolve@@(quaternionToRotationMatrix/@#)]&/@Partition[qs,2,1];
imgs=timestampedImages[[;;,2]];translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
(*imgs=(*ColorConvert[Import@#,"Graylevel"]&*)Import/@FileNames["/s/tmp/line_dance2/*.jpg"];*)
(*trs2=Parallelize[FindGeometricTransform@@@Partition[imgs,2,1]];
seq3=Parallelize@MapThread[ImagePerspectiveTransformation[#2,#,DataRange->Full]&,{FoldList[#.#2&,IdentityMatrix@3,TransformationMatrix/@trs2[[;;,2]]],imgs}]
Mean[ImageData/@seq3]//Image*)
(*trs4=Parallelize[FindGeometricTransform[#[[1]],#[[2]](*,"Transformation"->"Similarity"*)]&/@Partition[imgs,2,1]];*)
(*seq4=Parallelize@MapThread[ImagePerspectiveTransformation[#2,#,DataRange->Full]&,{FoldList[#.#2&,IdentityMatrix@3,TransformationMatrix/@trs4[[;;,2]]],imgs}]
Mean[ImageData/@seq4]//Image*)
(*trs5=FindGeometricTransform[#[[1]],#[[2]],"Transformation"->"Perspective"]&/@Partition[imgs,2,1];
seq5=MapThread[ImagePerspectiveTransformation[#2,#,DataRange->Full]&,{FoldList[#.#2&,IdentityMatrix@3,TransformationMatrix/@trs5[[;;,2]]],imgs}]
Mean[ImageData/@seq5]//Image*)
(*MatrixForm@TransformationMatrix@#&/@trs2[[;;,2]]
ListLinePlot[Det@TransformationMatrix[#][[;;2,;;2]]&/@trs2[[;;,2]],PlotRange->All]*)
(*Import@Export["t.png",#]&@ImageAssemble@Partition[ImageRotate[#,-Pi/2]&/@seq4[[;;;;4]],8]*)
(*MatrixForm@TransformationMatrix@#&/@trs4[[;;,2]]
ListLinePlot[Det@TransformationMatrix[#][[;;2,;;2]]&/@trs4[[;;,2]],PlotRange->All]*)
(*translationFromImagePairByHomography2=Function[imgPair,Module[{imgs=imgPair,w,tm,tr,scaling},
	w=ImageDimensions[imgs[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
	tr=Quiet@FindGeometricTransform[imgs[[2]],imgs[[1]],"Transformation"->"Perspective"];
	(*Print[tr];*)
	If[Head[tr[[2]]]===TransformationFunction,
		tm=Inverse[scaling].TransformationMatrix[tr[[2]]].scaling;
	(*Print[{tr[[1]],tm//MatrixForm}];*)
		tm[[;;,3]](*-Normalize[Cross@@Transpose[tm[[;;,;;2]]]]*)
		,{0,0,0}]
	]];*)
translationFromImagePairByHomography=Function[imgPair,Module[{w,tm,tr,scaling},
	w=ImageDimensions[imgPair[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
	tr=Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Similarity"];
	If[Head[tr[[2]]]===TransformationFunction,tm=Inverse[scaling].TransformationMatrix[tr[[2]]].scaling;
		tm[[;;2,3]]~Join~{0.5Log@Det[tm[[;;2,;;2]]]},{0,0,0}]
	]];
translationFromImagesByHomography=Function[imgs,Module[{w,ptm,tm,tr,scaling},
	w=ImageDimensions[imgs[[1]]][[1]];scaling=DiagonalMatrix@{w,w,1};
	ptm=Fold[#.If[Head[#2]===TransformationFunction,IdentityMatrix[3],TransformationMatrix@#2[[2]]]&,IdentityMatrix[3]
		,Parallelize[Function[imgPair,Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Similarity"]]
			/@Partition[imgs,2,1]]];
	tm=Inverse[scaling].ptm.scaling;
	tm[[;;2,3]]~Join~{0.5Log@Det[tm[[;;2,;;2]]]}
	]];
(*trans=Parallelize[translationFromImagePairByHomography/@Partition[imgs,2,1]];*)
(*trans=Parallelize[translationFromImagePairByHomography@{imgs[[1]],#}&/@Rest@imgs];
Graphics3D[BlendLine@Transpose[MedianFilter[#,2]&/@Transpose@Select[Thread@{trans,Most@qs},NumericQ@#[[1,1]]&][[;;,1]]](*,PlotRange->All*)]*)
(*ListLinePlot[Norm/@(accelsF/@timestampedImages[[;;,1]])]
ListLinePlot@Transpose[accelsF/@timestampedImages[[;;,1]]]
qGyros=With[{ts=timestampedImages[[;;,1]]},integrateAngularVelocityVerlet[ts 10^-3,gyrosF/@ts][[;;,2]]];*)
(*ListLinePlot[(*#-GaussianFilter[#,10]&@*)Accumulate@Accumulate[#-GaussianFilter[#,10]](*GaussianFilter[#,10]*)&/@
	Transpose@MapThread[rotateByQuaternion[accelsF[#2],#]&,{qGyros,timestampedImages[[;;,1]]}]]*)
(*Graphics3D@BlendLine@Accumulate[(*Normalize/@*)(rotateByQuaternion[Append[#[[1,;;2]],0],#[[2]]]&/@Select[Thread@{trans,Most@qs},NumericQ@#[[1,1]]&])]*)
(*ListLinePlot[trs4[[;;,1]](*,PlotRange->All*),ImageSize->600]
ListLinePlot[Transpose[TransformationMatrix[#][[;;2,3]]&/@trs4[[;;,2]]],PlotRange->All,ImageSize->600]
ListLinePlot[Det@TransformationMatrix[#][[;;2,;;2]]&/@trs4[[;;,2]],PlotRange->All,ImageSize->600]*)


Parallelize[Function[imgPair,Module[{pointPairs=ImageCorrespondingPoints[imgPair[[1]],imgPair[[2]],"Transformation"->"EpiPolar"]},
		{imgPair,Graphics[Arrow/@Transpose@pointPairs],Graphics[Arrow[{{0,0},#[[2]]-#[[1]]}]&/@Transpose@pointPairs]}]]/@
	Partition[imgs,2,1][[;;30]]]


case={"sweep_floor","line_scan","line_dance2","line_dance","line","pingpong"}[[2]];{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
imgs=timestampedImages[[;;,2]];
translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions[imgs[[1]]]},{0,0,1}};
Parallelize@Table[
	Function[img2,Module[{img1=imgs[[n]],tr,timg1},
		tr=FindGeometricTransform[img1,img2,"Transformation"->"Similarity"];
		timg1=ImagePerspectiveTransformation[img2,TransformationMatrix@tr[[2]],DataRange->Full];
			Join[{n,{"tr[[1]]",tr[[1]]},{"corr",Correlation@@(Flatten@ImageData@#&/@{img1,img2})}},ImageRotate[#,-Pi/2]&/@({img1,img2}
				~Join~{anaglyph@{img1,timg1},anaglyphBrightness@{img1,timg1}})]]]/@
	{imgs[[n+1]],ImagePerspectiveTransformation[imgs[[n+1]],Inverse[translation].LinearSolve@@(quaternionToRotationMatrix/@qs[[;;2]]).translation]}
	,{n,Min[15,#]&[Length@timestampedImages-1]}]


Parallelize@Table[timg=ImagePerspectiveTransformation[imgs[[n+1]],Inverse@TransformationMatrix@trs4[[n,2]],DataRange->Full];
		Prepend[ImageRotate[#,-Pi/2]&/@(imgs[[n;;n+1]]~Join~{anaglyph@{imgs[[n]],timg},anaglyphBrightness@{imgs[[n]],timg}}),n]
	,{n,Min[50,#]&@Length@timestampedImages-1}]


oldimg=img=CurrentImage[];timeWindows=Table[{},{10}];
Dynamic@Refresh[
	oldimg=img;img=CurrentImage[];tr=FindGeometricTransform@@(ImageResize[#,100]&/@{oldimg,img});
	If[Head[timeWindows]=!=List,timeWindows=Table[{},{10}];];
	timeWindows=MapThread[Append,{If[Length@#>20,Rest@#,#]&/@timeWindows,Prepend[Flatten[TransformationMatrix[tr[[2]]]],tr[[1]]]}];
	ListLinePlot[#,PlotRange->All]&/@{timeWindows[[1]],timeWindows[[{2,3,5,6}]],timeWindows[[{4,7}]]}
(*ImageSubtract[img,oldimg]*)
(*MatrixForm/@FindGeometricTransform[oldimg,img]*),UpdateInterval->5]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[2]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
imgs=timestampedImages[[;;,2]];
(*R as in E = R\hat{t}*)
translationFromMatchesBase=Function[{matches,km,R},Module[{mat,tmatches=Map[Inverse[km].Append[#,1]&,matches,{2}]},
	mat=Join@@MapThread[KroneckerProduct[{#},#2.R]&,tmatches];
	If[Length@mat>3
		,SingularValueDecomposition[Transpose@{mat[[;;,6]]-mat[[;;,8]],mat[[;;,7]]-mat[[;;,3]],mat[[;;,2]]-mat[[;;,4]]},3][[3,;;,-1]]
		,{0,0,0}]
	]];
trans=Parallelize@Table[translationFromMatchesBase[ImageCorrespondingPoints[imgs[[i]],imgs[[i+1]],"Transformation"->"EpiPolar"]
		,intrinsicParameterMatrix@imgs[[1]]
		,Transpose(*Identity*)[LinearSolve@@(quaternionToRotationMatrix/@{qs[[i]],qs[[i+1]]})]]
,{i,1,Length@qs-1}];
xyzs=Accumulate@Select[trans,NumericQ@#[[1]]&];(*Graphics3D@{BlendLine[xyzs],Point@xyzs}*)
Graphics3D[BlendLine@Accumulate[rotateByQuaternion@@@Select[Thread@{trans,qs[[2;;]]},NumericQ@#[[1,1]]&]],Axes->True]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[4]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
(*n=1;qInit=quaternionToRotationMatrix@qf[timestampedImages[[n,1]]];
translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions@timestampedImages[[1,2]]},{0,0,1}};
seq=(img=ImageRotate[#[[2]],Pi/2](*ImageReflect[#[[2]],Left->Top]*)(*#[[2]]*);
	ImageRotate[#,-Pi/2]&@ImageRotate[#,-Pi/2](*ImageReflect[#,Left->Top]*)&/@
		{img,ImagePerspectiveTransformation[img
				,Inverse[translation].Inverse[qInit].quaternionToRotationMatrix[qf[#[[1]]]].translation]})&/@
					timestampedImages[[;;(*n;;n+10*)(*{80,85,88}*)]];
ListAnimate@seq*)
(*Import@Export["t.png",#]&@Rasterize@Magnify[#,3]&@MatrixForm@seq*)


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[12]];
{gyros,accels,mags}=Table[Prepend[#[[2;;]],#[[1]]/10^6]&/@Import["/h/d/video_with_imu/"<>case<>"/TYPE_"<>typ<>".log","TextSeparator"->" "][[;;,{1,3,4,5}]]
	,{typ,{"GYROSCOPE","ACCELEROMETER","MAGNETIC_FIELD"}}];
{gyrosF,accelsF,magsF}=Interpolation[{#[[1]],#[[2;;]]}&/@#,InterpolationOrder->1]&/@{gyros,accels,mags};
ListLinePlot[Transpose[Rest/@#],PlotRange->All]&/@{gyros,accels,mags}
timestampedImages={ToExpression[StringSplit[FileBaseName@#,"_"][[-1]]]/10^6,Import[#,ImageSize->800]}&/@
	FileNames["/h/d/video_with_imu/"<>case<>"/IMG*"][[;;]];
qf=Interpolation[{#[[1]]/10^6,Prepend[#[[2;;]],Sqrt[1-Total[#[[2;;]]^2]]]}&/@
		Import["/h/d/video_with_imu/"<>case<>"/TYPE_ROTATION_VECTOR.log","TextSeparator"->" "][[;;,{1,3,4,5}]]
	,InterpolationOrder->1];
(*qf=Interpolation[{#[[1]]/10^6,#[[2;;]]}&/@Import["/h/d/video_with_imu/"<>case<>"/t3.txt","CSV"],InterpolationOrder->1];*)
qs=qf/@(timestampedImages[[;;,1]]);
{ListPlot@Transpose@qs
,ListPlot@Transpose[rotateByQuaternion[#[[2;;]],qf[#[[1]]]]&/@accels]
,ListPlot@Transpose[rotateByQuaternion[#[[2;;]],qf[#[[1]]]]&/@mags]
}
Export["/h/mxs/"<>case<>".mx",{qs,timestampedImages,qf,gyrosF,accelsF,magsF}];


(*imgs=ColorConvert[Import[#,ImageSize->800],"Grayscale"]&/@
	Table["/tmp/"<>case<>"/"<>IntegerString[i]<>".jpg",{i,Length@FileNames["/tmp/"<>case<>"/*.jpg"]}];*)
imgs=timestampedImages[[;;,2]];
epipolarOfImagePair=Function[imgPair,Module[{matches,fm2},
	matches=ImageCorrespondingPoints[imgPair[[1]],imgPair[[2]],"Transformation"->"EpiPolar"(*"Perspective"*)];Print[Dimensions@matches];
	fm2=fundamentalMatrixFromMatchesLp[matches,1];
	If[fm2=!=Null,drawEpipolarLineForFundamentalMatrix[imgPair[[2]],fm2,3]]
	]];
(*epipolarOfImagePair[Import/@FileNames["~/slide*.JPG"]]
epipolarOfImagePair[Import/@FileNames["~/forward*.JPG"]]*)
imgs2=Parallelize[(Print@#;epipolarOfImagePair@#)&/@Partition[imgs,2,1][[;;;;4]]];
(*Length@ImageCorrespondingPoints[#[[1]],#[[2]],"Transformation"->"EpiPolar"(*"Perspective"*)][[1]]&/@Partition[pingpongs,2,1]
{14,83,43,30,51,97,27,31,17,55,33,25,31,15,15,28,20,31,77,48,46,62,13,22,19,44,61,9,7,8,9,13,8,26,13,30,8
	,10,8,8,0,16,49,51,50,10,15,12,16,13,22,44,21,30,46,63,29,54,34,24,18,14,17,21,22,18,22,28,47,27,21,45,42
	,20,103,38,37,59,100,152,42,49,53,66,28,48,17,13,21,22,23,9,20,12,27,14,24,19,11,10,12,9,15,7,15,29,23,8,0,0,9,13,0,0,9,26}
*)
(*epipoleOfImagePair=Function[imgPair,Module[{matches,fm2},
	matches=ImageCorrespondingPoints[imgPair[[1]],imgPair[[2]],"Transformation"->"EpiPolar"(*"Perspective"*)];Print[Dimensions@matches];
	fm2=fundamentalMatrixFromMatchesLp[matches,1];
	SingularValueDecomposition[fm2][[3,;;,-1]]]];
epipoles=Parallelize[epipoleOfImagePair/@Partition[imgs,2,1]];*)


useExternalMatch=False;isPortrait=True;
(*imgs=ColorConvert[Import[#,ImageSize->400{1,1}],"Grayscale"]&/@FileNames["/s/saloon/*.jpg"][[-2;;]];*)
(*imgs=Import/@{"/s/gdesk/image-013.jpg","/s/gdesk/image-014.jpg"}*)
(*imgs=ColorSeparate[Import[#,ImageSize->400{1,1}],"HSB"][[1]]&/@{"t2.jpg","t7.jpg"};*)
colorImgs=If[useExternalMatch,Import@#,Import[#,ImageSize->If[isPortrait,400,1000](*{1,1}*)]]&/@{
	(*"/tmp/pingpong/14.jpg","/tmp/pingpong/20.jpg"*)
	(*"/tmp/pingpong/114.jpg","/tmp/pingpong/115.jpg"*)
	(*"t11.jpg","t12.jpg"*)"/s/tmp/orbit_updown/10.jpg", "/s/tmp/orbit_updown/50.jpg"
	(*"~/m/rvctools/vision/images/building2-1.png","~/m/rvctools/vision/images/building2-2.png"*)
	(*"~/m/rvctools/vision/images/eiffel2-1.jpg","~/m/rvctools/vision/images/eiffel2-2.jpg"*)
	(*"~/m/vgg_vision/vgg_examples/chapel00.png","~/m/vgg_vision/vgg_examples/chapel01.png"*)
	(*"t2.jpg","t7.jpg"*)(*,"t8.jpg"*)(*"t.jpg","t3.jpg"*)(*"t2.jpg","t4.jpg"*)(*"t9.jpg","t10.jpg"*)
	(*"/s/kejian_front/image-056.jpg","/s/kejian_front/image-057.jpg"*)(*"~/doc/a.png","~/doc/b.png"*)
	(*"/h/d/workspace/tcv/church01.jpg","/h/d/workspace/tcv/church03.jpg"*)};
imgs=ColorConvert[#,"Grayscale"]&/@colorImgs;km=intrinsicParameterMatrix@imgs[[1]];
{i1,i2(*,i3*)}=imgs;
matches=randomSampleAtMostN[#,500]&/@If[useExternalMatch,MapIndexed[{#[[1]],ImageDimensions[colorImgs[[#2[[1]]]]][[2]]+1-#[[2]]}&
		,Transpose[Partition[#,2]&/@Transpose[Import@"~/t.csv"]],{2}]
	,ImageCorrespondingPoints[colorImgs[[1]],colorImgs[[2]](*,"Transformation"->"EpiPolar"*)(*"Perspective"*)]];
Length@matches[[1]]
scaling=400/Mean@ImageDimensions[imgs[[2]]];
Magnify[#,scaling]&@MapThread[annotateImageWithPoints,{{i1,i2},matches}]
img=imgs[[1]];dim=Dimensions@ImageData@img;
fm=fundamentalMatrixFromMatchesBase[matches];fm2=fundamentalMatrixFromMatchesLp[matches,0.2];
fm3=fundamentalMatrixFromMatchesBaseLp[matches,0.2];
em=Transpose[km].fm2.km;
em2=fundamentalMatrixFromMatchesMinReprojectionErrorEnforceRank[em,matches,km];
Function[em,
	depths=depthFromEssentialMatrix[em,km,matches];
	{reprojectionError[em,km,matches,depths],Graphics3D[Join@@MapThread[If[100>#2>-100,{RGBColor@@#3,Point[Append[#,#2]]},{}]&
	,{matches[[1]],depths,ImageValue[colorImgs[[1]],#]&/@matches[[1]]}],Axes->True]}]/@{em,em2}
Magnify[#,scaling]&@drawEpipolarLineForFundamentalMatrix[imgs[[2]],Inverse[km\[Transpose]].#.Inverse[km],5]&/@{em,em2}


Table[
With[{images=timestampedImages[[{n,n+1,n+2},2]]},Module[{w,h,res},
	{w,h}=ImageDimensions[images[[1]]];
	res=ImageFeatureTrack[images,Flatten[Table[{x,y},{x,.5,w-.5,20},{y,.5,h-.5,20}],1]];
	Magnify@{images,Graphics[{Red,If[FreeQ[#,_Missing],Arrow[{#}]]&/@Transpose[res]}]}]],{n,1,Length@timestampedImages-2,5}]


case={"sweep_floor","crab","orbit_updown","updown","orbit_facade","orbit","rect_slide","line_scan","line_dance2","line_dance","line","pingpong"}[[2]];
{qs,timestampedImages,qf,gyrosF,accelsF,magsF}=Import["/h/mxs/"<>case<>".mx"];
n=1;imgs=ColorConvert[Import@#,"Grayscale"]&/@{"/s/tmp/"<>case<>"/"<>IntegerString[n]<>".jpg", "/s/tmp/"<>case<>"/"<>IntegerString[n+1]<>".jpg"};
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
velocities=Parallelize@Table[matches=MapIndexed[{#[[1]],ImageDimensions[imgs[[#2[[1]]]]][[2]]+1-#[[2]]}&,Transpose[Partition[#,2]&
			/@Transpose[Import["/s/tmp/"<>case<>"/p_matched"<>IntegerString[n]<>".csv"]]],{2}];
		motionFromMatches[matches,km,gyrosF[timestampedImages[[n,1]]]]
	,{n,Length@timestampedImages-1}];
procVelocities=Transpose[MedianFilter[#,3]&/@Transpose[Normalize[#-Min[#]]&/@Abs@#]]&;
ListLinePlot[Transpose@procVelocities[velocities]]
g=Function[vels,trans=MapThread[rotateByQuaternion,{vels,Most@qs}];xyzs=Accumulate@Select[trans,NumericQ@#[[1]]&];
	Graphics3D[{BlendLine[xyzs],Point@xyzs},ViewPoint->{0,0,10},Axes->True]]/@
		{procVelocities[velocities],Array[{0,1,0}&,Length@velocities]}
Import@Export["t.png",Rasterize[g]];


procVelocities=Transpose[MedianFilter[#,3]&/@Transpose[Normalize[#-Min[#]]&/@Abs@#]]&;
Magnify@{ListLinePlot[MedianFilter[#,3]&/@Transpose@velocities,PlotRange->All],ListLinePlot[Transpose@procVelocities[velocities],PlotRange->All]}
g=Function[vels,trans=MapThread[rotateByQuaternion,{vels,Most@qs}];xyzs=Accumulate@Select[trans,NumericQ@#[[1]]&];Graphics3D@{BlendLine[xyzs],Point@xyzs}]/@
	{procVelocities[velocities],Array[{0,1,0}&,Length@velocities]}
