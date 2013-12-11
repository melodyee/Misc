findSimilarityHomographyFromSequence=Function[imgs,Fold[{#[[1]]+#2[[1]],#[[2]].#2[[2]]}&,{0,IdentityMatrix[3]},findSimilarityHomography/@Partition[imgs,2,1]]];
refineImagesWithF=Function[{imgs,f},Module[{trs},
	trs=FoldList[{#[[1]]+If[#2===Null,0,#2[[1]]],#[[2]].If[#2===Null,IdentityMatrix[3],#2[[2]]]}&,{0,IdentityMatrix[3]},
		f/@Partition[imgs,2,1]];
	{trs,MapThread[ImagePerspectiveTransformation[#,#2,DataRange->Full]&,{imgs,trs[[;;,2]]}]}
	]];
findTranslationHomography=Function[imgPair,Module[{tr=Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Translation"]},
	If[NumberQ[tr[[1]]]&&TransformationFunction===Head[tr[[2]]],{tr[[1]],TransformationMatrix[tr[[2]]]}]]];

(*ListAnimate@timestampedImages[[31;;40,2]]*)
(*MatrixForm[LinearSolve@@(quaternionToRotationMatrix/@#)]&/@Partition[imuPoses[[31;;35,5;;8]],2,1]
MatrixForm@Last@findSimilarityHomography@#&/@Partition[imgs,2,1]*)
(*set=Range[31,35];*)(*Good*)
(*set=Range[41,45];*)(*Bad*)
set=Range[81,85];imgs=ImageRotate[#,-Pi/2]&/@timestampedImages[[(*31;;35*)set(*40*),2]];

res=refineImagesWithF[imgs,findSimilarityHomography];res[[1,;;,1]]
nimgs=res[[2]];meanImage@nimgs

qimgs=MapThread[ImagePerspectiveTransformation[#,#2,DataRange->Full]&,
	{imgs,FoldList[#.#2&,IdentityMatrix[2],(LinearSolve@@(quaternionToRotationMatrix/@#))[[;;2,;;2]]&/@Partition[imuPoses[[set,5;;8]],2,1]]}];
res=refineImagesWithF[qimgs,findTranslationHomography];res[[1,;;,1]]
nimgs=res[[2]];meanImage@nimgs
(*Magnify[meanImage/@{imgs,qimgs},2]
findTranslationHomography/@Partition[qimgs,2,1]*)
(*subImages=Transpose[ImagePartition[#,ImageDimensions[#]/5]&/@nimgs,{3,1,2}][[2,2]]
Magnify[{meanImage@subImages,meanImage[refineImagesWithF[subImages,findTranslationHomography][[2]]]},2]*)
(*tr=findSimilarityHomographyFromSequence[imgs];
nimg=ImagePerspectiveTransformation[imgs[[-1]],tr[[2]],DataRange->Full];
anaglyph[{imgs[[1]],nimg}]*)


anaglyph[nimgs]
anaglyph[ImageAssemble/@Transpose[Map[Function[subImages,refineImagesWithF[subImages,findTranslationHomography][[2]]]
	,Transpose[ImagePartition[#,ImageDimensions[#]/4]&/@nimgs,{3,1,2}],{2}],{2,3,1}]]
	
imgs=Import/@FileNames["~/slide*.JPG"];
imgs=Import/@FileNames["~/forward*.JPG"];
sim=Last@findSimilarityHomography[imgs];
(*anaglyph@{imgs[[1]],ImagePerspectiveTransformation[imgs[[2]],sim,DataRange->Full]}*)
res=refineImagesWithF[imgs,findSimilarityHomography];res[[1,;;,1]]
nimgs=res[[2]];meanImage@nimgs
(*imgs=Import/@FileNames["~/forward*.JPG"];sim=Last@findSimilarityHomography[imgs];
anaglyph@{imgs[[1]],ImagePerspectiveTransformation[imgs[[2]],sim,DataRange->Full]}*)


Clear[a,f];as=Array[a,3{1,1}];logSim=MatrixLog@findSimilarityHomography[imgs[[1;;2]]][[2]];
f[as_?(NumericQ@#[[1,1]]&)]:=Total@Abs@Flatten[ImageData@imgs[[1]]-ImageData@ImagePerspectiveTransformation[imgs[[2]],MatrixExp@as,DataRange->Full]]
	+Total@Flatten[(as-logSim)^2];
(*r=FindMinimum[f[as],variableWithInital[as,MatrixLog@findSimilarityHomography[imgs[[1;;2]]][[2]]]];//AbsoluteTiming*)
r=NMinimize[f[as],Flatten@as];//AbsoluteTiming
anaglyph@{imgs[[1]],ImagePerspectiveTransformation[imgs[[2]],MatrixExp[as/.r[[2]]],DataRange->Full]}
