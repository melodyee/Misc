(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
(*Exactly the same as RpcaColorLevinWeissSimpleLaplaceWithInit*)
RpcaColorLevinWeissSimpleLaplaceGrayWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,rgb,dim=Dimensions@Gray,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega][[1]]]].poissonMatrix[Dimensions@Gray]];A=SparseArray[DiagonalMatrix[vec[\[CapitalOmega][[1]]]]+A2];
	rgb=unVec[LinearSolve[A,vec[#]+A2.vec[Gray](*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim]&/@Rgb[[;;]];
	L=(*correctWithGray[Gray,#]&@*)rgb;{L,Rgb-L}]];
RpcaColorShrinkOverLinearWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,rgb,dim=Dimensions@Gray,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega][[1]]]].poissonMatrix[Dimensions@Gray]];A=SparseArray[DiagonalMatrix[vec[\[CapitalOmega][[1]]]]+A2];
	rgb=unVec[shrinkageOverLinear[List/@vec@shrinkage["SVD",#,unVec[vec@#2,dim]]&,0.08,A,List/@(vec[#]+A2.vec[Gray]),30],dim]&/@Rgb[[;;]];
	L=correctWithGray[Gray,#]&@rgb;{L,Rgb-L}]];
imageTiling=Function[img,Module[{m=ImageData@img,dim,dim2,selectClosesFactor},
	selectClosesFactor=Function[{big,target},First@SortBy[Divisors@big,Norm[#-target]&]];
	If[Length@Dimensions@m==3,ColorCombine[imageTiling/@ColorSeparate[img]],
		dim=Dimensions@m;dim2=MapThread[selectClosesFactor,{dim,Sqrt@dim}];(*Print@dim2;Print[Map[vec,Partition[m,dim2],{2}]//Dimensions];*)
		Image[Join@@Map[Flatten,Partition[m,dim2],{2}]]]]];
inverseImageTiling=Function[{img,origDim},Module[{m=ImageData@img,dim2,selectClosesFactor,blockDim},
	selectClosesFactor=Function[{big,target},First@SortBy[Divisors@big,Norm[#-target]&]];
	If[Length@Dimensions@m==3,ColorCombine[inverseImageTiling[#,origDim]&/@ColorSeparate[img]],
		dim2=MapThread[selectClosesFactor,{origDim,Sqrt@origDim}];blockDim=origDim/dim2;
		ImageAssemble@Map[Image,Partition[Transpose@unVec[#,dim2]&/@m,blockDim[[2]]],{2}]
	]]];

homographyFromMatchesRansac=Function[{matches,imgs,ikms},
	ransacDriver[randomSampleAtMostN[Transpose@matches,100],If[Length@#>9,homographyFromMatches@Transpose@#,homographyFromMatchesL1@Transpose@#]&
			,reprojectionErrorHomography[#,Transpose@{#2}]&
			,0.5(1- nonzeroRatio@ImageData@ImagePerspectiveTransformation[imgs[[1]],#,DataRange->Full])+correlationErrorHomography[#,imgs]&,
		homographyFromMatchesL1@matches,3,Max[5,Round[0.5 Length@Transpose@matches]],Min[20,Round[0.7 Length@Transpose@matches]],20]];

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
(*dispRtn/@decomposeSameNormalCalibratedHomographyList[{Inverse[ikm.rs[[1,1]].Inverse[ikm]],ikm.rs[[2,1]].Inverse[ikm]}]*)

RpcaColorLevinWeissLabWithInit=Function[{Luminance,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,rgb,lab,dim=Dimensions@Luminance},
	lab=ColorSeparate[ColorCombine[Image/@Rgb],"LAB"];
	A=laplacianMatrixFromMatrix[Luminance,\[CapitalOmega]in[[1]],1];(*Export["/tmp/A.csv",A];Export["/tmp/Gray.csv",Gray];*)
	rgb=ImageData/@ColorSeparate[ColorCombine[Prepend[Image@Transpose@Partition[
		LinearSolve[A,vec@ImageData[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim[[1]]]&/@lab[[2;;]],Image@Luminance],"LAB"],"RGB"];
	{rgb,0Rgb}]];
RpcaColorLevinWeissOrigWithInit=Function[{gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter},Module[{fnames,partialColored=rgb\[CapitalOmega] \[CapitalOmega]+(1-\[CapitalOmega])Table[gray,{3}]},
	fnames=Table[CreateTemporary[],{4}];MapThread[Export[#,#2,"PNG"]&,{fnames[[;;3]],{Image@gray,ColorCombine[Image/@partialColored],Image@\[CapitalOmega][[1]]}}];
	Run["/h/bin/matlab -nosplash -nodesktop -nojvm -r \"cd('/h/m/levin_weiss_code');colorizeScript({'"<>Riffle[fnames,"','"]<>"'});exit\""];
	{ImageData/@ColorSeparate@Import[fnames[[-1]],"PNG"],0 rgb\[CapitalOmega]}
	(*Map[{#,#,#}&,gray,{2}]+foldToTensor[rgb\[CapitalOmega],{3,Dimensions[rgb\[CapitalOmega]][[1]],Dimensions[rgb\[CapitalOmega]][[2]]/3},2]*)
	]];
testRpcaColorAlgorithms=Function[{dataSets,type,outputDir,noiseF},
	Module[{numIter=400,prefix,L,S,isRgb,imgs,gray,trueGray,rawRgb,raw\[CapitalOmega],rgb,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,t,result,recordResult,totalResult={}
		,missingRatios,imageDimensions,n\[CapitalOmega],partialColored},
	recordResult=Function[{isRgb,nameList,r,rawRgb,raw\[CapitalOmega],gray},Module[{name=StringJoin@Riffle[ToString/@nameList,"-"],fname},
		fname=FileNameJoin[{outputDir,name<>".jpg"}];PrintTemporary[fname];L=r[[2,1]];
		Export[fname,If[isRgb,ImageResize[ColorCombine[Image/@L],400],ImageResize[Image[ArrayFlatten@{L}],400]]];
		AppendTo[totalResult,{"name"->name,"time"->r[[1]],
			"rse"->pnorm[L-rawRgb,2]/pnorm[rawRgb,2],
			"corr"->1-Correlation[Flatten[(1-raw\[CapitalOmega])L],Flatten[(1-raw\[CapitalOmega])rawRgb]],
			"rgb"->(pnorm[(1-raw\[CapitalOmega])(L-rawRgb),2]/Sqrt[Times@@Dimensions@rawRgb]),
			"gray"->(pnorm[Mean[1-raw\[CapitalOmega]](Mean[L]-gray),2]/Sqrt[Times@@Dimensions@gray])}]]];
	Switch[type
		,"unfoldings",
			{missingRatios,imageDimensions}={{(*0.1,0.4,*)(*0.1*)(*0.6,0.8,*)0.95(*,0.95*)(*,0.99*)},{(*16{1,1}*)(*32{1,1},*)(*128{1,1}*)128{1,1}}};
		,"lowratio",
			{missingRatios,imageDimensions}={{0.98(*,0.99*)},{(*32{1,1},*)256{1,1}(*,512{1,1}*)}};
		,"multigrid",
			{missingRatios,imageDimensions}={{0.95(*,0.99*)},{32{1,1},128{1,1}(*,512{1,1}*)}};
		,_,
			{missingRatios,imageDimensions}={{0.6,0.9},{32{1,1}}};
	];
	Do[isRgb="isRgb"/.dataSet;imgs="images"/.dataSet;
		Do[
		{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,imageDimension,missingRatio];
		{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,Map[noiseF[]+#&,rawRgb,{-1}] raw\[CapitalOmega],raw\[CapitalOmega]};partialColored=rgb\[CapitalOmega]+(1-\[CapitalOmega])Table[gray,{3}];
		MapThread[Export,{{"/tmp/orig.png","/tmp/gI.png","/tmp/colorIm.png","/tmp/cI.png"}
			,{ColorCombine[Image/@rgb],Image@gray,Image@\[CapitalOmega][[1]],ColorCombine[Image/@partialColored]}}];
		initL=initS=0rgb\[CapitalOmega];
		prefix={"name"/.dataSet,missingRatio,imageDimension};If[isRgb,trueGray=ImageData@ColorConvert[ImageResize[ColorCombine[imgs],imageDimension],"Gray"]];
		Switch[type
		,"unfoldings",
			result=RpcaColorLevinWeissSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"LlwSimpleLaplace"},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Llw"},result,rawRgb,raw\[CapitalOmega],gray];
(*			result=RpcaColorLevinWeissLabWithInit[ImageData@First@ColorSeparate[ColorCombine[Image/@rgb],"LAB"],rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"LlwLab"},result,rawRgb,raw\[CapitalOmega],gray];*)
			(*Need use trueGray here.*)
(*			result=RpcaColorLevinWeissOrigWithInit[trueGray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			(*result=RpcaColorLevinWeissOrigWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;*)
			recordResult[isRgb,{Sequence@@prefix,"LlwOrig"},result,rawRgb,raw\[CapitalOmega],(*??*)trueGray];*)
			(*result=RpcaColorFourierWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Fourier"},result,rawRgb,raw\[CapitalOmega],gray];*)
(*			result=RpcaColorFourierLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"FourierLaplace"},result,rawRgb,raw\[CapitalOmega],gray];*)
			(*result=RpcaColorHadamardWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Hadamard"},result,rawRgb,raw\[CapitalOmega],gray];*)
			(*result=RpcaColorGrayWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Gray"},result,rawRgb,raw\[CapitalOmega],gray];*)
			result=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2}]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"{2}"},result,rawRgb,raw\[CapitalOmega],gray];
			(*result=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2,3}]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,{2,3}},result,rawRgb,raw\[CapitalOmega],gray];*)
			(*result=Rpca2PseudoColorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Pseudo"},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorSeparateWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Separate"},result,rawRgb,raw\[CapitalOmega],gray];
			Do[
			result=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,subset]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,subset},result,rawRgb,raw\[CapitalOmega],gray];
			,{subset,Select[Subsets[{1,2,3}],#!={}&]}];*)
			(*result=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Laplace"},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorTensorSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"SimpleLaplace"},result,rawRgb,raw\[CapitalOmega],gray];*)
		,"lowratio",
			Print[rawRgb//showTensor3];
			recordResult[isRgb,{Sequence@@prefix,"Pure-gray"},{0,{{gray,gray,gray},rgb\[CapitalOmega]-{gray,gray,gray}}},rawRgb,raw\[CapitalOmega],gray];
			{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.05,20];initS=rgb\[CapitalOmega]-initL;
			Do[
			result=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega]+(1-\[CapitalOmega])initL,\[CapitalOmega]+weight((1-\[CapitalOmega]) n\[CapitalOmega]),initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Weighted",weight},result,rawRgb,raw\[CapitalOmega],gray];
			,{weight,{0,(*0.01,*)0.1,0.5,(*0.8,*)1.0}}];
			result=RpcaColorTensorWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"{2,3}-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorTensorUnfoldingsWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter,{2}]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"{2}-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorGrayWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Gray-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorFourierWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Fourier-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
			(*result=RpcaColorFourierLaplaceWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"FourierLaplace-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];*)
			result=RpcaColorLevinWeissSimpleLaplaceWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"LlwSimpleLaplace-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorLevinWeissWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Llw-Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
		,"block",
			Do[With[{divisor=divisorScale{1,1}},
			If[Mod[imageDimension,2divisorScale]!={0,0},Continue[]];
			result=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"block"},result,rawRgb,raw\[CapitalOmega],gray];
			{L,S}=result[[2]];
			result=RpcaColorRedBlackBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L,S,numIter,RpcaColorTensorWithInit,2divisor]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"redblack"},result,rawRgb,raw\[CapitalOmega],gray];
			],{divisorScale,{1,2,4,8}}];
		,"multigrid",
			Do[With[{divisor=divisorScale{1,1}},
			If[Mod[imageDimension,2divisorScale]!={0,0},Continue[]];
			result=RpcaColorBlockWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"block"},result,rawRgb,raw\[CapitalOmega],gray];
			result=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor,1]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"zigzag"},result,rawRgb,raw\[CapitalOmega],gray];
			result=Rpca2ColorFmgWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit
				,1+Round[N@Log[GeometricMean@divisor]/Log[2]]]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"fmg"},result,rawRgb,raw\[CapitalOmega],gray];
			],{divisorScale,{1,2,4,8}}];
		];
		,{missingRatio,missingRatios},{imageDimension,imageDimensions}];
	,{dataSet,dataSets}];
	totalResult
	]];


(*imgs=Import["/h/t*.jpg",ImageSize->400]
img=imgs[[2]];*)

SeedRandom[1003];dim=128{1,1};{gray,rgb,masks}=prepareDataRpcaColor[ColorSeparate[Import["/h/t11.jpg",ImageSize->dim]],dim,0.9];
(*\[CapitalOmega]=ImageData@Erosion[Image@randomSparseTensor[dim{1,1},0.9],2];masks={\[CapitalOmega],\[CapitalOmega],\[CapitalOmega]};*)
noiseF=(*RandomVariate@LaplaceDistribution[0,0.02]*)0&;
noisyGray=Map[#+noiseF[]&,gray,{-1}];
(*noisyGray=ImageData@ImageResize[ImageResize[#,ImageDimensions[#]/2],ImageDimensions@#]&@Image@gray;*)
noisyRgb\[CapitalOmega]=masks Map[#+noiseF[]&,rgb,{-1}];
{gray//Image,noisyGray//Image,rgb//showTensor3,noisyRgb\[CapitalOmega]//showTensor3}
Dimensions/@{gray,rgb,masks}


n=128;img=ImageResize[ExampleData[{"TestImage","Girl3"}],n{1,1}];img//ImageDimensions
lab=ColorSeparate[img,"LAB"];{img,ImageAdjust/@lab}
lapG=laplacianMatrixFromMatrix[ImageData@lab[[1]],0 ImageData@lab[[1]],1];


dispResiduals=Function[residuals,Module[{distribFs,x},distribFs=CDF[SmoothKernelDistribution@#][x]&/@residuals;
	Plot[distribFs,{x,0,1},PlotRange->All]]];
evalSparsenessSmoothness=Function[{m,lapG},
	fourierResiduals=Join@@Abs[fourier2D@m];hadamardResiduals=Join@@Abs[walshMatrix[7].m.walshMatrix[7]\[Transpose]];
	lapResiduals=Join@@Abs@LaplacianFilter[m,1];lapGResiduals=Abs[lapG.vec[m]];
	sparserResiduals={fourierResiduals,hadamardResiduals};
	{Histogram@Flatten@m,Image@Partition[#,n]&/@sparserResiduals,
		ListLogPlot[Prepend[(Take[#,n]/Total[#]&)@Reverse@Sort@#&/@sparserResiduals,SingularValueList[m]/Total[SingularValueList@m]]
		,PlotRange->All,Joined->True(*,ImageSize->800*)],
	dispResiduals@{lapResiduals,lapGResiduals}(*,ImageSize->800*)(*,Automatic,"CDF"*)}
	];
(*Experiment 1. There exists sparse and smooth structure in data. Even when data is somewhat noisy.*)
Table[Module[{lab2,noisyImage},
	lab2=If[noiseLevel==0,0,0.01RandomVariate[PoissonDistribution[noiseLevel 100],Dimensions@ImageData@#]]+ImageData@#&/@lab;
	noisyImage=ColorCombine[Image/@lab2,"LAB"];
	{noisyImage,peakSignalToNoiseRatio[noisyImage,img],
		evalSparsenessSmoothness[#,7 lapG]&/@Join[{RandomReal[1,Dimensions[ImageData@lab[[1]]]]},lab2]}
],{noiseLevel,{0,0.05,0.1}}]


(*Experiment 2, when gray is noisy, \n abla_G is not better than \n abla ?*)
Table[Module[{gray,lapG},
gray=If[noiseLevel==0,0,0.01RandomVariate[PoissonDistribution[noiseLevel 100],Dimensions@ImageData@#]]+ImageData@#&@lab[[1]];
lapG=laplacianMatrixFromMatrix[gray,0 gray,1];
{ColorCombine[Join[{Image@gray},lab[[2;;]]],"LAB"],peakSignalToNoiseRatio[gray,lab[[1]]],
,evalSparsenessSmoothness[ImageData@#,7lapG][[4]]&/@lab[[2;;]]}
],{noiseLevel,{0,0.05,0.2}}]


orig=Import@"/tmp/orig.png"
Magnify[#,2]&@Table[ImageDifference[orig,ImageResize[r2[[i,2]],ImageDimensions@orig]],{i,2}]
(*Magnify[#,2]&@Table[Histogram[*)ImageHistogram/@{orig,r2[[1,2]],r2[[2,2]]}
Table[{r2[[i,2]],Median[#^2&/@Flatten[ImageData@orig-ImageData@ImageResize[r2[[i,2]],ImageDimensions@orig]]]},{i,2}]


rgbSet={"name"->"rgb","images"->(ColorSeparate@Import@"/h/d/lotsOfFlowers.jpg"(*"/h/d/1600sunflower001.jpg"*)),"isRgb"->True};
separationSet={"name"->"separation","images"->(ColorConvert[Import@#,"Grayscale"]&/@{"~/t5.jpg","~/t2.jpg","~/t.jpg"}),"isRgb"->False};
dataSets={rgbSet(*,separationSet*)};
SeedRandom[1003];r={};
(*r=Join[r,testRpcaColorAlgorithms[dataSets,"lowratio","/tmp",(*RandomVariate[LaplaceDistribution[0,0.1]]&*)0&]];*)
r=Join[r,testRpcaColorAlgorithms[dataSets,"unfoldings","/tmp",(*RandomVariate[LaplaceDistribution[0,0.1]]&*)0&]];
r
r2={Riffle[{"name","time","rse","gray","rgb","corr"},{"name","time","rse","gray","rgb","corr"}/.#],Import["/tmp/"<>("name"/.#)<>".jpg"]}&/@r
(*testRpcaColorAlgorithms[dataSets,"lowratio","/tmp"]*)
(*testRpcaColorAlgorithms[dataSets,"multigrid","/tmp"]*)
(*testRpcaColorAlgorithms[dataSets,"block","/tmp"]*)


evalColorResult=Function[{L,S},With[{scale=1/pnorm[rgb,2]},
	{{"rgb",pnorm[rgb-L,2]scale,"gray",pnorm[Mean[L]-Mean@rgb,2]scale},showTensor3/@{L,S},{"unknown",pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}
	,ImageAdjust@ImageDifference[rgb//showTensor3,L//showTensor3]}]];
Clear[colorLevinWeiss];
colorLevinWeiss[Gray_List,Rgb_List,\[CapitalOmega]in_List,OptionsPattern[{"CorrectGray"->True,"GrayLaplacianRadius"->1,"Method"->"PartialLaplacian"}]]:=
		Module[{A,rgb,dim=Dimensions@Gray,L,\[CapitalOmega]=SparseArray[\[CapitalOmega]in],lapG=laplacianMatrixFromGray[Gray,OptionValue["GrayLaplacianRadius"]],diagMask,\[Lambda]=1
			,complementDiagMask},
	diagMask=sparseDiagonalMatrix[vec[\[CapitalOmega][[1]]]];complementDiagMask=sparseIdentityMatrix[Length@diagMask]-diagMask;
	Switch[OptionValue["Method"]
		,"FullLaplacian",A=vertcat[lapG,\[Lambda] diagMask];rgb=Transpose@Partition[
			LeastSquares[A,Join[0vec[#],\[Lambda] vec[#]],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim[[1]]]&/@Rgb;
		,"PartialLaplacian",A=complementDiagMask.lapG+diagMask;
			rgb=unVec[LinearSolve[A,vec[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim]&/@Rgb
		,"NoGray",A=complementDiagMask.lapG+diagMask;
			rgb=Gray+unVec[LinearSolve[A,vec[#]-diagMask.vec[Gray],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim]&/@Rgb
		,"AddGray",A=complementDiagMask.lapG+diagMask;
			rgb=unVec[LinearSolve[A,vec[#]+complementDiagMask.lapG.vec[Gray],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim]&/@Rgb];
	L=If[OptionValue["CorrectGray"],correctWithGray[Gray,#],#]&@rgb;(*Append[rg,Length[Rgb] Gray-Total@rg]*)
	{L,Rgb-L}];
i=5;{L[i],S[i]}=colorLevinWeiss[noisyGray,noisyRgb\[CapitalOmega],masks];//AbsoluteTiming
evalColorResult[L[i],S[i]]
i=6;{L[i],S[i]}=colorLevinWeiss[noisyGray,noisyRgb\[CapitalOmega],masks,"CorrectGray"->False];//AbsoluteTiming
evalColorResult[L[i],S[i]]
i=7;{L[i],S[i]}=colorLevinWeiss[noisyGray,noisyRgb\[CapitalOmega],masks,"Method"->"NoGray","CorrectGray"->False];//AbsoluteTiming
evalColorResult[L[i],S[i]]
i=9;{L[i],S[i]}=colorLevinWeiss[noisyGray,noisyRgb\[CapitalOmega],masks,"Method"->"AddGray","CorrectGray"->False];//AbsoluteTiming
evalColorResult[L[i],S[i]]


RpcaColorShrinkTVWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,rgb,dim=Dimensions@Gray,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A2=SparseArray[sparseDiagonalMatrix[1-vec[\[CapitalOmega][[1]]]].poissonMatrix[Dimensions@Gray]];A=SparseArray[sparseDiagonalMatrix[vec[\[CapitalOmega][[1]]]]+A2];
	rgb=totalVariationShrinkage[0.02,A,unVec[vec[#]+A2.vec[Gray],dim],50]&/@Rgb[[;;]];
	L=correctWithGray[Gray,#]&@rgb;{L,Rgb-L}]];

(*LLW's result without correction by gray is close to: TotalVariationFilter[gray//Image,0.2]//Magnify[#,2]&*)
Clear[L,S];
(*(*Color info can be represented as a Complex of A+B I where A, B are from LAB*)
L=ImageData/@ColorSeparate@ColorConvert[ColorCombine[Prepend[Image@Re@First@RpcaFourierWithInit[ImageData@#,masks[[1]],0masks[[1]],0masks[[1]],100]&/@ColorSeparate[
	ColorCombine[Image/@(rgb masks)],"LAB"][[2;;]],First@ColorSeparate[ColorCombine[Image/@rgb],"LAB"]],"LAB"],"RGB"];S=0L;
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}

{L,S}=RpcaFourierWithInit[(#[[1]]+I #[[2]]&)[ImageData/@ColorSeparate[ColorCombine[Image/@(rgb masks)],"LAB"][[2;;]]],masks[[1]],0masks[[1]],0masks[[1]],100];
L=ImageData/@ColorSeparate@ColorConvert[ColorCombine[Prepend[Image/@{Re@L,Im@L},First@ColorSeparate[ColorCombine[Image/@rgb],"LAB"]],"LAB"],"RGB"];S=0L;
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}*)

(*(*Working in LAB color space makes things worse*)
L=ImageData/@ColorSeparate@ColorConvert[ColorCombine[Prepend[Image@First@Rpca2[ImageData@#,masks[[1]],100]&/@ColorSeparate[
	ColorCombine[Image/@(rgb masks)],"LAB"][[2;;]],First@ColorSeparate[ColorCombine[Image/@rgb],"LAB"]],"LAB"],"RGB"];S=0L;
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}
L=ImageData/@ColorSeparate@ColorConvert[ColorCombine[Prepend[
	inverseImageTiling[#,dim]&@Image@First@Rpca2[ImageData@imageTiling@#,ImageData@imageTiling@Image@masks[[1]],100]&/@ColorSeparate[
		ColorCombine[Image/@(rgb masks)],"LAB"][[2;;]],First@ColorSeparate[ColorCombine[Image/@rgb],"LAB"]],"LAB"],"RGB"];S=0L;
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}*)
(*{L,S}=RpcaColorGrayWithInit[gray,rgb masks,masks,0masks,0masks,100];//AbsoluteTiming
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}*)
(*{L,S}=RpcaColor2WithInit[gray,rgb masks,masks,L,S,100];//AbsoluteTiming
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}*)
(*i=1;{L[i],S[i]}=RpcaColorFourierWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}*)
i=2;{L[i],S[i]}=RpcaColorLevinWeissWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i]]
i=3;{L[i],S[i]}=RpcaColorLevinWeissSimpleLaplaceWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i]]
i=4;{L[i],S[i]}=RpcaColorShrinkTVWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i]]
(*i=5;{L[i],S[i]}=RpcaColorTensorLaplaceWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i]]*)
(*i=6;{L[i],S[i]}=RpcaColorTensorSimpleLaplaceWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i]]*)
(*(*Post-filtering can hardly improve*)
i=4;L[i]=correctWithGray[gray,ImageData/@ColorSeparate@TotalVariationFilter[ColorCombine[Image/@L[3]],0.03,Method->"Poisson"]];S[i]=0L[i];
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}*)
(*Post-improving RpcaColorLevinWeissSimpleLaplaceWithInit*)
(*i=4;{L[i],S[i]}=RpcaColorFourierWithInit[noisyGray,noisyRgb\[CapitalOmega],masks,L[3],S[3],100];//AbsoluteTiming
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}
{Total[schattenNorm[#,1]&/@#]&/@Array[L,i],pnorm[#,1]&/@Array[S,i]}*)
(*Post-improving RpcaColorLevinWeissSimpleLaplaceWithInit by working in LAB*)
(*lab=ImageData/@ColorSeparate[ColorCombine[Image/@L[3]],"LAB"];(*rgb\[CapitalOmega]Lab=ImageData/@ColorSeparate[ColorCombine[Image/@noisyRgb\[CapitalOmega]],"LAB"];*)
abFiltered=Table[First@Rpca2[lab[[i]],Map[1&,lab[[i]],{-1}],100],{i,2,3}];
i=5;L[i]=ImageData/@ColorSeparate@ColorConvert[ColorCombine[Image/@Prepend[abFiltered,lab[[1]]],"LAB"],"RGB"];S[i]=noisyRgb\[CapitalOmega]-L[i];
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}*)
(*{Total[schattenNorm[#,1]&/@#]&/@Array[L,i],pnorm[#,1]&/@Array[S,i]}*)


(*The manual label example seems trivial: the colorization is good once we apply correctWithGray*) 
Clear[L,S];img=ColorCombine[ColorSeparate[Import["/h/d/Manual.png",ImageSize->{300}],"RGB"]];
gray=ImageData@First@ColorSeparate[ColorConvert[Import["/h/d/original.png",ImageSize->{300}],"RGB"],"Gray"];
rgb=ImageData/@ColorSeparate[Import["/h/d/original.png",ImageSize->{300}],"RGB"];
colorPartMask=Function[img,Map[N@Boole[Norm[#-Table[Mean@#,{3}]]>0.0000001]&,ImageData@img,{2}]];
\[CapitalOmega]1=colorPartMask[img]randomSparseTensor[Reverse@ImageDimensions@img,0.05];masks=\[CapitalOmega]={\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1};{\[CapitalOmega]//showTensor3}
rgb\[CapitalOmega]=(*Transpose[ImageData@img,{2,3,1}]*)correctWithGray[gray,#]&[ImageData/@ColorSeparate@img] \[CapitalOmega];
i=1;{L[i],S[i]}=RpcaColorFourierWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],100];
showTensor3/@{L[i],S[i]}
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}
i=2;{L[i],S[i]}=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],0];//AbsoluteTiming
showTensor3/@{L[i],S[i]}
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}
i=3;{L[i],S[i]}=RpcaColorLevinWeissSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],200];
showTensor3/@{L[i],S[i]}
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}
i=4;{L[i],S[i]}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],100];
showTensor3/@{L[i],S[i]}
{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}


(*Colorization by matching against a colleciton of photos, need be of similar lighting etc., can be from a video*)
Clear[L,S];case="street";grayIndex=1;allFnames=FileNames[(*"/s/df_golden/"<>case<>"/*.JPG"*)"/s/df/t"<>case<>".t/unified_viewer/*.JPG"];
(*{"/h/t2.jpg","/h/d/Trevi_fountain_at_night.jpg","/h/d/url.jpg","/h/d/Trevi-Fountain-Rome-Italy-34.jpg","/h/d/Trevi-Fountain.jpeg"};*)
(*FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];*)
imgSize=400;numIter=100;
imgs=Parallelize[
	If[ImageDimensions[#][[1]]>ImageDimensions[#][[2]],ImageRotate[#,-90Degree],#]&@
		Import[#,ImageSize->{imgSize}]&/@Join[allFnames[[;;grayIndex-1]],allFnames[[grayIndex+1;;]]]];
img=If[ImageDimensions[#][[1]]>ImageDimensions[#][[2]],ImageRotate[#,-90Degree],#]&@Import[#,ImageSize->{imgSize}]&@allFnames[[grayIndex]];
gray=ImageData@ColorConvert[img,"Gray"];rgb=ImageData/@ColorSeparate@img;
getColorPoints=Function[{gray,refImg},Module[{imgDim,matches,rgbM},
	imgDim=ImageDimensions@refImg;rgbM=ImageData@refImg;
	matches=imageCorrespondingPoints[Image@gray,ColorConvert[refImg,"Gray"],"Transformation"->"Epipolar"];
	xyToRowColumn[#[[1,1]],#[[1,2]],imgDim[[2]]]->(rgbM[[#[[1]],#[[2]]]]&@xyToRowColumn[#[[2,1]],#[[2,2]],imgDim[[2]]])&/@Transpose@matches]];
(*procImage[{gray,{}},imgs[[1]]]*)
rules=Join@@(Parallelize[getColorPoints[gray,#]&/@imgs]);rules//Dimensions
\[CapitalOmega]1=SparseArray[rules[[;;,1]]->1,Dimensions@gray];masks=\[CapitalOmega]={\[CapitalOmega]1,\[CapitalOmega]1,\[CapitalOmega]1};{img,\[CapitalOmega]//showTensor3//ImageAdjust}
rgb\[CapitalOmega]=Transpose[ReplacePart[ImageData@ColorConvert[Image@gray,"RGB"],rules],{2,3,1}]\[CapitalOmega];
(*{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[ImageData@gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.03,30];initS=rgb\[CapitalOmega]-initL;
showTensor3/@{initL,n\[CapitalOmega]}
LS=RpcaColor2WithInit[ImageData@gray,rgb\[CapitalOmega],n\[CapitalOmega],initL,initS,200];showTensor3/@LS
Import@Export["t.png",Rasterize[ImageRotate[#,-90Degree]&/@{showTensor3[LS[[1]]],gray,imgs[[grayIndex]]}]]*)
i=1;{{L[i],S[i]}=RpcaColorLevinWeissSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],0];//AbsoluteTiming
,showTensor3/@{L[i],S[i]}
,{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}}
i=2;{{L[i],S[i]}=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],0];//AbsoluteTiming
,showTensor3/@{L[i],S[i]}
,{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}}
{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.05,20];initS=rgb\[CapitalOmega]-initL;
i=3;{{L[i],S[i]}=RpcaColorTensorUnfoldingsWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter,{2}];//AbsoluteTiming
,showTensor3/@{L[i],S[i]}
,{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}}
i=4;{{L[i],S[i]}=RpcaColorTensorUnfoldingsWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter,{2,3}];//AbsoluteTiming
,showTensor3/@{L[i],S[i]}
,{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}}
i=5;{{L[i],S[i]}=RpcaColorFourierWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming
,showTensor3/@{L[i],S[i]}
,{pnorm[rgb-L[i],2]/pnorm[rgb,2],showTensor3/@{L[i],S[i]},pnorm[(rgb-L[i])(1-masks),2]/pnorm[rgb (1-masks),2]
	,ImageAdjust@ImageDifference[rgb//showTensor3,L[i]//showTensor3]}}


(*Variation of label*)
gray=ImageData@First@ColorSeparate[Import@"~/m/levin_weiss_code/example.bmp","LAB"];dim=Dimensions@gray;
markedPre=Import@"~/m/levin_weiss_code/example_marked.bmp";
marked=ImageData/@(ColorSeparate[markedPre,"LAB"][[2;;3]]);
getMask=Function[threshold,N@Map[Boole[Mean[#^2]-Mean[#]^2>threshold]&,ImageData@markedPre,{2}]];
test=Function[mask,
	Print[A=laplacianMatrixFromMatrix[gray,mask,1];//AbsoluteTiming];
	r2=LinearSolve[A,vec[# mask],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}]&/@marked;
	img=Image[Transpose[Prepend[Transpose@Partition[#,dim[[1]]]&/@r2,gray],{3,1,2}],ColorSpace->"LAB"];
	{mask//Image,Image[Transpose[Prepend[# mask&/@marked,gray],{3,1,2}],ColorSpace->"LAB"],img}];
Import@Export["t.png",Rasterize[TableForm[test/@{getMask@0.001,getMask@0.0001,getMask[0.0001]randomSparseTensor[dim,0.01]}]
	,ImageSize->800]]


gray2=ImageData@ColorConvert[Import@"~/m/levin_weiss_code/example.bmp","Grayscale"];
markedRgb=Image[Transpose[Prepend[ImageData/@(ColorSeparate[markedPre,"LAB"][[2;;3]]),gray],{3,1,2}],ColorSpace->"LAB"];
\[CapitalOmega]=Table[getMask[0.0001],{3}];rgb\[CapitalOmega]=3rgbVector (\[CapitalOmega] (ImageData/@ColorSeparate[markedRgb,"RGB"]));
{L,S}=RpcaColorLevinWeissWithInit[gray2,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],0];
{markedRgb,showTensor3[L/(3rgbVector)]}


(*RpcaSimpleLaplaceTraceNormWithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L,\[Tau]=0.1,\[Mu]=30,f},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega]]].poissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix@vec[\[CapitalOmega]]+A2];
	f=Function[xs,LinearSolve[\[Mu] sparseIdentityMatrix[Dimensions[A][[2]]]+A,\[Mu] vec[sShrinkage[\[Tau]/\[Mu],unVec[xs,dim]]]+vec[D]]];
	L=unVec[Nest[f,LinearSolve[A,vec[D]],iter],dim];
	(*L=unVec[LinearSolve[A,vec[D](*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim];*)
	{L,D-L}]];*)

(*RpcaOverLinearWithInit=Function[{D,\[CapitalOmega],initL,initS,iter,unitaryType},Module[{norm2,\[Lambda],\[Eta]=10.,Y,L=initL,S=initS,\[Mu],\[Rho],m,n,Zl,Zs,OL},
	(*SVD: min ||L||_* + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	(*Fourier: min ||FLF^T||_ 1 + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	(*L,S,D are m-by-kn. \[CapitalOmega] is nonzeros*)
	{m,n}=Dimensions[D];\[Lambda]=Min[0.5,100./GeometricMean@{m,n}];norm2=SingularValueDecomposition[D,1][[2,1,1]];
	Y=0D;(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];*)\[Mu]=12.5/norm2;\[Rho]=1.005;
	Do[
    Zl=(Y+\[Mu](D-S))/\[Mu];
	OL=L;
	L=Switch[unitaryType,"SVD",sShrinkage[1/\[Mu],Zl],
		"FourierLaplace",Re@InverseFourier@cShrinkageHadamard[0.001/\[Mu](KroneckerProduct@@(Sqrt[Range[0,#-1]]&/@Dimensions@Zl)),Fourier@Zl],
		"Fourier",Re@InverseFourier@cShrinkage[0.02/\[Mu],Fourier@Zl],
		"Hadamard",With[{padded=padToTwoPower@Zl},Take[unVec[hadamardTransform@dShrinkage[0.02/\[Mu],hadamardTransform@vec@padded],Dimensions@padded],
			Sequence@@Dimensions@Zl]]
	];
	Zs=Y/\[Mu]+D-L;
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
	If[pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];If[Mod[j,30]==0,PrintTemporary@ImageAdjust@Image@Re[L]];(*(scores=Append[scores,#];Print[#])&@evalPredicted[L,testM2];*)
	,{j,iter}];
	{L,S}]];*)


SeedRandom[1003];as=RandomReal[1,{6,10}];bs=RandomReal[1,{6,10}];\[Tau]=0.1;\[Mu]=5;
g=Function[xs,{\[Tau] schattenNorm[xs,1],1/2 pnorm2[as.xs-bs,2]}];
f=Function[xs,LinearSolve[\[Mu] IdentityMatrix[10]+as\[Transpose].as,\[Mu] sShrinkage[\[Tau]/\[Mu],xs]+as\[Transpose].bs]];
g/@NestList[f,LeastSquares[as,bs],100]


(*Testing RPCA*)
(*The error is reduce by 50%! But there are some artifacts*)
k=1;{L,S}=Rpca2WithInit[gray masks[[1]],masks[[1]],0gray,0gray,100];//AbsoluteTiming
Magnify[{pnorm[gray-L,2]/pnorm[gray,2],Image/@{L,S}},k]
{L,S}=ImageData@inverseImageTiling[Image@#,dim]&/@Rpca2WithInit[
	ImageData@imageTiling@Image[gray masks[[1]]],ImageData@imageTiling@Image[masks[[1]]],0gray,0gray,100];//AbsoluteTiming
Magnify[{pnorm[gray-L,2]/pnorm[gray,2],Image/@{L,S}},k]

(*Tiling make it worse by 20%, why?*)
{L,S}=RpcaColor2WithInit[gray,rgb masks,masks,0masks,0masks,100];//AbsoluteTiming
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}
(*Magnify[{imageTiling@Image[gray],imageTiling@ColorCombine[Image/@(rgb masks)],imageTiling@ColorCombine[Image/@(masks)]},2]*)
{L,S}=ImageData/@ColorSeparate@inverseImageTiling[ColorCombine[Image/@#],dim]&/@RpcaColor2WithInit[
	ImageData@imageTiling@Image[gray]
	,ImageData/@ColorSeparate@imageTiling@ColorCombine[Image/@(rgb masks)]
	,ImageData/@ColorSeparate@imageTiling@ColorCombine[Image/@(masks)]
	,0masks,0masks,100];//AbsoluteTiming
{pnorm[rgb-L,2]/pnorm[rgb,2],showTensor3/@{L,S},pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]}


Rpca2LabMayJuxtaposeWithInit=Function[{Luminance,Rgb,\[CapitalOmega]in,initL,initS,iter,juxtapose},Module[{L,S,k,m,n,ab},(*Only complete the non-Gray parts*)
	{k,m,n}=Dimensions@Rgb;ab=ImageData/@ColorSeparate[ColorCombine[Image/@Rgb,"RGB"],"LAB"][[2;;]];
	{L,S}=Rpca2[unfoldTensor[If[juxtapose,Append[#,Luminance],#]&[ab],3],
		unfoldTensor[If[juxtapose,Append[#,Array[1&,Dimensions@Luminance]],#]&@\[CapitalOmega]in[[2;;]],3],iter];
	Print[Dimensions/@{L,S}];
	Print[Image/@Partition[L,Dimensions@Luminance]];
	L=ImageData@ColorConvert[ColorCombine[Prepend[Image/@Partition[L,Dimensions@Luminance][[1,2]],Image@Luminance],"LAB"],"RGB"];
	{L,Rgb-L}]];
