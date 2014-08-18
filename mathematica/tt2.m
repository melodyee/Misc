(* ::Package:: *)

$HistoryLength=1;
<<"~/gdrive/mac_home/t3.m"
showTensor3=Function[m,ImageResize[ColorCombine[Image/@m],400{1,1}]];


gray2=ImageData@ColorConvert[Import@"~/m/levin_weiss_code/example.bmp","Grayscale"];
markedRgb=Image[Transpose[Prepend[ImageData/@(ColorSeparate[markedPre,"LAB"][[2;;3]]),gray],{3,1,2}],ColorSpace->"LAB"];
\[CapitalOmega]=Table[getMask[0.0001],{3}];rgb\[CapitalOmega]=3rgbVector (\[CapitalOmega] (ImageData/@ColorSeparate[markedRgb,"RGB"]));
{L,S}=RpcaColorLevinWeissWithInit[gray2,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],0];
{markedRgb,showTensor3[L/(3rgbVector)]}


gray=ImageData@First@ColorSeparate[Import@"~/m/levin_weiss_code/example.bmp","LAB"];dim=Dimensions@gray;
markedPre=Import@"~/m/levin_weiss_code/example_marked.bmp";
marked=ImageData/@(ColorSeparate[markedPre,"LAB"][[2;;3]]);
(*Tally@Chop@Flatten[ImageData@ColorConvert[marked,"Grayscale"]-ImageData@gray]*)
getMask=Function[threshold,
	N@Map[Boole[Mean[#^2]-Mean[#]^2>threshold]&,ImageData@markedPre,{2}]];
test=Function[mask,
	Print[A=laplacianMatrixFromMatrix[gray,mask,1];//AbsoluteTiming];
	r2=LinearSolve[A,vec[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}]&/@marked;
	img=Image[Transpose[Prepend[Transpose@Partition[#,dim[[1]]]&/@r2,gray],{3,1,2}],ColorSpace->"LAB"];
	{mask//Image,Image[Transpose[Prepend[marked,gray],{3,1,2}],ColorSpace->"LAB"],img}];
(*Import@Export["t.png",*)Rasterize[TableForm[test/@{(*getMask@0.001,*)getMask@0.0001(*,getMask[0.0001]randomSparseTensor[Dimensions@mask,0.8]*)}]
	,ImageSize->800]


dir="/s/tfountain.tt/";model="8LuUdJb7iAM";denseLines=Import[dir<>"dense.csv"][[1]];dim=50{3,4};fname=StringSplit[denseLines[[1]],":"][[2]]<>".JPG";
img=ImageResize[Import[dir<>"unified_viewer/"<>fname],Reverse@dim];
sfm=Import[dir<>model<>"/"<>model<>"_model.ply","VertexData"];
{docids,cameras}=Import[dir<>#]&/@{"docids.csv","camera.csv"};
camera=Partition[denseLines[[1]]/.Thread@Rule[docids[[1,;;-2]],cameras[[;;,;;-2]]],4];
g=Graphics3D[Point@sfm,AxesLabel->StringSplit@"x y z",ViewPoint->{-10,-10,0},ViewVertical->{0,1,0}];
stereo=Import[dir<>model<>"/"<>denseLines[[1]]<>"_stereo_points.ply","VertexData"];
(*{points,colors}=Import[dir<>"8LuUdJb7iAM/"<>denseLines[[1]]<>"_depthmap.ply",#]&/@{"VertexData","VertexColors"};*)
g2=Graphics3D[Point@stereo,ViewVertical->{-1,0,0},AxesLabel->StringSplit@"x y z",Axes->True,ViewPoint->{-4,-6,0}];
{sparse,dense}=Function[fname,With[{line=Import[dir<>fname][[1]]},
	ImageData[Image@Partition[line[[2;;-2]],400]][[;;;;2,;;;;2]]]]/@{"sparse.csv","dense.csv"};
Prepend[ImageAdjust@Image@#&/@{sparse,dense},img]
disp=Function[m,ListPlot3D[N[Join@@MapIndexed[Append[{#2[[2]],-#2[[1]]},-#]&,
		m,{2}]],PlotStyle->Texture[img],Lighting->"Neutral",ImageSize->500,Mesh->None,AxesLabel->StringSplit@"x y z"]];
{g,g2,disp@dense}
(*projected={{#[[1]],-#[[2]]}/#[[3]],-#[[3]]}&[camera.Append[#,1]]&/@stereo;
g=Graphics[Point[projected[[;;,1]]],Axes->True,AxesLabel->StringSplit@"x y",AxesOrigin->{0,0}];
projected2={{#[[1]],-#[[2]]}/#[[3]],-#[[3]]}&[camera.Append[#,1]]&/@sfm;
visible=Select[projected2,1600>#[[1,1]]>0&&0>#[[1,2]]>-1200&];
g2=Graphics[Point[visible[[;;,1]]],Axes->True,AxesLabel->StringSplit@"x y",AxesOrigin->{0,0}];
{g,g2,Show[g,g2]}*)
(*f=Nearest[Rule[({0,1200}+#&/@projected[[;;,1]])/4,projected[[;;,2]]]];
m=-Table[First@f[{i,300-j}],{j,1,300,2},{i,1,400,2}];
disp@m*)


gray=ImageData@First@ColorSeparate[img,"LAB"];
mask=SparseArray[SparseArray[sparse]["NonzeroPositions"]->1.,Dimensions@sparse];
Print[A=laplacianMatrixFromMatrix[gray,mask,1];//AbsoluteTiming];
r=Transpose@Partition[LinearSolve[A,vec[sparse],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],Dimensions[gray][[1]]];
disp@r


trueGray=ImageData@ColorConvert[img,"Grayscale"];mask=SparseArray[SparseArray[sparse]["NonzeroPositions"]->1.,Dimensions@sparse];
{L,S}=Rpca2WithInit[sparse,mask,sparse,0sparse,400];
sparse2=ArrayFlatten@{{trueGray,sparse}};mask2=ArrayFlatten@{{N@Map[1&,trueGray,{2}],Normal@mask}};
{L2,S2}=Partition[#,Dimensions@L][[1,2]]&/@Rpca2WithInit[sparse2,mask2,sparse2,0sparse2,400];
{disp@L,disp@L2}


img=Import@"t.jpg";
Table[{Total@SingularValueList@#/k,Total@Flatten@#/k/k}&@Mean[ImageData/@ColorSeparate@ImageResize[img,k 10{1,1}]],{k,100}]


r=Import@Export["~/m/levin_weiss_code/gg.bmp",
	ColorCombine[Image@Downsample[ImageData@#,10]&/@ColorSeparate@Import@"~/gg.bmp"]]
r2=Import@Export["~/m/levin_weiss_code/ll.bmp",
	ColorCombine[Image@Downsample[ImageData@#,10]&/@ColorSeparate@Import@"~/ll.bmp"]]


img=ExampleData[{"TestImage", "Apples"}];Export["gg.jpg",ColorConvert[img,"Grayscale"]];{img,Import@"gg.jpg",Import@"ll.jpg"}


dim=256{1,1};numIter=400;img=ImageResize[Import@"~/m/levin_weiss_code/example.bmp"(*ExampleData[{"TestImage", "Apples"}]*),dim];
(*img=Import@"~/m/levin_weiss_code/example.bmp";*)
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
{L4,S4}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming
(*{L5,S5}=RpcaColorTensorSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],0rgb\[CapitalOmega],0rgb\[CapitalOmega],numIter];//AbsoluteTiming*)
(*{L5,S5}=Rpca2ColorZigZagWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],L4,S4,numIter,RpcaColorTensorLaplaceWithInit,2{1,1},1];//AbsoluteTiming*)
showTensor3@L4
(*showTensor3@L5*)
Ls={L4(*,L5*)};
{"rgb prediction",(pnorm[(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}


r=ImageResize[Import@#,100]&/@{"/s/s1.jpg","/s/s2.jpg","/s/s3.jpg"};
Export["/s/ss1.jpg",r[[1]]]
Export["/s/ss2.jpg",r[[2]]]
Export["/s/ss3.jpg",r[[3]]]


rgbSet={"name"->"rgb","images"->(ColorSeparate@Import@"t.jpg"),"isRgb"->True};
separationSet={"name"->"separation","images"->(ColorConvert[Import@#,"Grayscale"]&/@{"~/t5.jpg","~/t2.jpg","~/t.jpg"}),"isRgb"->False};
dataSets={rgbSet,separationSet};
testRpcaColorAlgorithms=Function[{dataSets,type,outputDir},
	Module[{numIter=400,prefix,L,S,isRgb,imgs,gray,rawRgb,raw\[CapitalOmega],rgb,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,t,result,recordResult,totalResult={}
		,missingRatios,imageDimensions,n\[CapitalOmega]},
	recordResult=Function[{isRgb,nameList,r,rawRgb,raw\[CapitalOmega],gray},Module[{name=StringJoin@Riffle[ToString/@nameList,"-"],fname},
		fname=FileNameJoin[{outputDir,name<>".jpg"}];Print[fname];L=r[[2,1]];
		Export[fname,If[isRgb,ImageResize[ColorCombine[Image/@L],400],ImageResize[Image[ArrayFlatten@{L}],400]]];
		AppendTo[totalResult,{"name"->name,"time"->r[[1]],
			"rgb"->(pnorm[(1-raw\[CapitalOmega])(L-rawRgb),2]/Sqrt[Times@@Dimensions@rawRgb]),
			"gray"->(pnorm[Mean[1-raw\[CapitalOmega]](Mean[L]-gray),2]/Sqrt[Times@@Dimensions@gray])}]]];
	Switch[type
		,"unfoldings",
			{missingRatios,imageDimensions}={{0.4(*,0.6,0.8,0.9,0.95*)},{32{1,1}(*,128{1,1}*)(*,512{1,1}*)}};
		,"lowratio",
			{missingRatios,imageDimensions}={{0.95(*,0.99*)},{32{1,1},128{1,1}(*,512{1,1}*)}};
		,"multigrid",
			{missingRatios,imageDimensions}={{0.95(*,0.99*)},{32{1,1},128{1,1}(*,512{1,1}*)}};
		,_,
			{missingRatios,imageDimensions}={{0.6,0.9},{32{1,1}}};
	];
	Do[isRgb="isRgb"/.dataSet;imgs="images"/.dataSet;
		Do[
		{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,imageDimension,missingRatio];{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};initL=initS=0rgb\[CapitalOmega];
		prefix={"name"/.dataSet,missingRatio,imageDimension};
		Switch[type
		,"unfoldings",
			result=Rpca2PseudoColorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Pseudo"},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorSeparateWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Separate"},result,rawRgb,raw\[CapitalOmega],gray];
			Do[
			result=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,subset]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,subset},result,rawRgb,raw\[CapitalOmega],gray];
			,{subset,Select[Subsets[{1,2,3}],#!={}&]}];
			result=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Laplace"},result,rawRgb,raw\[CapitalOmega],gray];
			result=RpcaColorTensorSimpleLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"SimpleLaplace"},result,rawRgb,raw\[CapitalOmega],gray];
		,"lowratio",
			{initL,n\[CapitalOmega]}=localColorConsistencyInterpolation[gray,unfoldTensor[rgb\[CapitalOmega],2],unfoldTensor[\[CapitalOmega],2],0.05,20];initS=rgb\[CapitalOmega]-initL;
			Do[
			result=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega]+(1-\[CapitalOmega])initL,\[CapitalOmega]+weight((1-\[CapitalOmega]) n\[CapitalOmega]),initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Weighted",weight},result,rawRgb,raw\[CapitalOmega],gray];
			,{weight,{0,0.01,0.1,0.5,0.8,1.0}}];
			result=RpcaColorTensorWithInit[gray,initL,n\[CapitalOmega],initL,initS,numIter]//AbsoluteTiming;
			recordResult[isRgb,{Sequence@@prefix,"Weight",1},result,rawRgb,raw\[CapitalOmega],gray];
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
SeedRandom[1003];
testRpcaColorAlgorithms[dataSets,"unfoldings","/tmp"]
(*testRpcaColorAlgorithms[dataSets,"lowratio","/tmp"]*)
(*testRpcaColorAlgorithms[dataSets,"multigrid","/tmp"]*)
(*testRpcaColorAlgorithms[dataSets,"block","/tmp"]*)


Rpca2LabWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter}
Rpca2LabWithMayJuxtaposeInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,juxtapose},Module[{L,S,k,m,n},(*Only complete the non-Gray parts*)
	{k,m,n}=Dimensions@Rgb;
	{L,S}=foldToTensor[#,{k-1,m,n},2]&/@
		Rpca2WithInit[
			unfoldTensor[Table[If[juxtapose,ArrayFlatten@{Gray,#},#]&[Rgb[[i]]-Gray],{i,k-1}],2]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{Array[1&,Dimensions@#],#},#]&/@\[CapitalOmega]in[[;;k-1]],2]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{Gray,#},#]&/@initL[[;;k-1]],2]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{Array[0&,Dimensions@#],#},#]&/@initS[[;;k-1]],2],iter];
	L=#+Gray&/@L;AppendTo[L,k Gray-Total@L];Print@Dimensions[L];
	{L,Rgb-L}]];
Rpca2LabUnfoldingsWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{L,S,k,m,n},
	{k,m,n}=Dimensions@Rgb;
	{L,S}=foldToTensor[#,{k-1,m,n},2]&/@
		Rpca2WithInit[unfoldTensor[Table[Rgb[[i]]-Gray,{i,k-1}],2],unfoldTensor[\[CapitalOmega]in[[;;k-1]],2]
			,unfoldTensor[initL[[;;k-1]],2],unfoldTensor[initS[[;;k-1]],2],iter];
	L=#+Gray&/@L;AppendTo[L,k Gray-Total@L];Print@Dimensions[L];
	{L,Rgb-L}]];


SeedRandom[1003];rawImg=Import@"~/t6.jpg";(*rawImg=ExampleData[{"TestImage", "Apples"}];*)
dim=128{1,1};isRgb=True;{useLocalconsistency,weightedLocalconsistency}={False,True};divisor2=4^2;divisor=Sqrt[divisor2]{1,1};
numIter=400;missingRatio=0.9;img=ImageResize[rawImg,Reverse@dim];
(*We may permuate the image but still recover.*)(*img=Image@RotateRight[ImageData@img,dim 3/4];*)
(*img=ColorCombine[Image@Permute[ImageData@#,p]&/@ColorSeparate[img]];*)
(*Image[RotateRight[ImageData@ColorCombine[Image@Permute[#,InversePermutation[p]]&/@#],dim 0/4],ImageSize->400]&/@Ls*)
imgs=ColorSeparate@img;
{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim,missingRatio];{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};initL=initS=0rgb\[CapitalOmega];
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
{L,S}=RpcaColorLevinWeissWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L2,S2}=Rpca2PseudoColorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
(*{L3,S3}=Rpca2LabWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,*)
{L4,S4}=RpcaColorTensorWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L5,S5}=RpcaColorTensorLaplaceWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
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

Ls={L,L2,L4,L5(*L,L2,L3,L4,L5,L6,L7,L8*)};
{"rgb prediction",(pnorm[(1-raw\[CapitalOmega])(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}
If[!isRgb,ImageResize[Image@gray,400]]
If[isRgb,ImageResize[ImageResize[rawImg,dim],400{1,1}],Image[#,ImageSize->400]&/@imgs]
If[isRgb,Magnify@ImageResize[ColorCombine[Image/@#],400]&/@Ls,Image[#,ImageSize->400]&/@#&/@Ls]


dim=512{1,1};imgs=Image@Mean[ImageData/@ColorSeparate@ImageResize[Import@#,dim]]&/@{"~/t5.jpg","~/t2.jpg","~/t.jpg"};
numIter=400;missingRatio=0.9;isRgb=False;divisor=2{1,1};
{gray,rawRgb,raw\[CapitalOmega]}=prepareDataRpcaColor[imgs,dim,missingRatio];{rgb,rgb\[CapitalOmega],\[CapitalOmega]}={rawRgb,rawRgb raw\[CapitalOmega],raw\[CapitalOmega]};initL=initS=0rgb\[CapitalOmega];
{
(*Different unfolding expreiment*)
{L,S}=RpcaColor2WithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter];//AbsoluteTiming,
{L2,S2}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,2,3}];//AbsoluteTiming,
{L3,S3}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2,3}];//AbsoluteTiming,
{L4,S4}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,3}];//AbsoluteTiming,
{L5,S5}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1,2}];//AbsoluteTiming,
{L6,S6}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{1}];//AbsoluteTiming,
{L7,S7}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{2}];//AbsoluteTiming,
{L8,S8}=RpcaColorTensorUnfoldingsWithInit[gray,rgb\[CapitalOmega],\[CapitalOmega],initL,initS,numIter,{3}];//AbsoluteTiming,
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
Ls={L,L2,L3,L4,L5,L6,L7,L8};
{"rgb prediction",(pnorm[(1-raw\[CapitalOmega])(#-rawRgb),2]/Sqrt[Times@@dim])&/@Ls}
{"gray prediction",pnorm[gray-Mean[#],2]/Sqrt[Times@@dim]&/@Ls}
If[!isRgb,ImageResize[Image@gray,400]]
If[isRgb,ImageResize[ImageResize[rawImg,dim],400{1,1}],Image[#,ImageSize->400]&/@imgs]
If[isRgb,ImageResize[ColorCombine[Image/@#],400]&/@Ls,Image[#,ImageSize->400]&/@#&/@Ls]

(*(Export["t.png",#];#)&@BarChart[{0.08610168360977238`,0.08349747897986283`,0.09168359098191457`,0.09875961240437145`,0.0943741769561935`,0.08795915068408834`}
	,ChartLegends->{"{1,2,3}","{2,3}","{1,3}","{1,2}","{2}","{3}"},ChartStyle->"DarkRainbow"
	,PlotRange -> {Automatic, {0.07, 0.1}},PlotRangePadding -> {Automatic, 0},PlotRangeClipping -> True]*)


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
robustrecoverPointCharges=Function[{Data,\[CapitalOmega]in,maxIter},Module[{M=Flatten@Data,\[CapitalOmega]=Flatten@\[CapitalOmega]in,B,X,Zs,S,Y1,Y2,\[Lambda]=1.,\[Mu]1,\[Mu]2,\[Rho]=1.01,P,PT,PTP,n=Dimensions[Data][[1]],norm2},
	PTP=PT.P;B=0 M;X=M;S=0 M;Y1=Y2=0 M;P=poissonMatrix[n];PT=Transpose[P];norm2=SingularValueDecomposition[Partition[M,n],1][[2,1,1]];\[Mu]2=1.25/norm2;\[Mu]1=\[Lambda] \[Mu]2;
	Do[
		(*If[Mod[j,10]==1,Print[MatrixPlot@Partition[#,n]&/@{B,X,S}]];*)
		(*Print[robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]];*)
		X=LinearSolve[\[Mu]1 IdentityMatrix[n^2]+\[Mu]2 PTP,\[Mu]1(M-S)+\[Mu]2 PT.B+Y1-Y2.P];
		(*Do[
			X-=0.0001(\[Mu]1 X+\[Mu]2 PT.(P.X) - (\[Mu]1(M-S)+\[Mu]2 PT.B+Y1-Y2.P));
		,{10}];*)
		(*Print[{"X",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		B=Flatten@dShrinkage[1/\[Mu]2,Partition[P.X+Y2/\[Mu]2,n]];
		(*Print[{"B",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		Zs=M-X+Y1/\[Mu]1;S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu]1,Zs]+(1-\[CapitalOmega])Zs;
		(*Print[{"S",robustrecoverPointChargesCostFunciton[P,M,\[CapitalOmega],\[Lambda],\[Mu]1,\[Mu]2,B,X,S,Y1,Y2]}];*)
		Y1+=\[Mu]1 (M-X-S);Y2+=\[Mu]2 (P.X-B);
		{\[Mu]1,\[Mu]2}*=\[Rho];
	,{j,maxIter}];
	Partition[#,n]&/@{B,X}
	]];
recoverPointChargesCostFunction=Function[{P,M,\[CapitalOmega],B,X},{Total@#,#}&@{Norm[P.X-B],Norm[\[CapitalOmega](X-M)]}];
recoverPointCharges=Function[{Data,\[CapitalOmega]in,maxIter},Module[{M=Flatten@Data,\[CapitalOmega]=Flatten@\[CapitalOmega]in,B,X,\[Eta]=1.,\[Lambda]=1.,\[Rho]=1.05,P,PT,n=Dimensions[Data][[1]]},
	B=0 M;X=M;P=poissonMatrix[n];PT=Transpose[P];
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
A=poissonMatrix[n];
x=Partition[LinearSolve[A,b],n];//AbsoluteTiming (*Takes 9.0s on desktop*)
\[CapitalOmega]=Partition[RandomSparseMatrix[1,Length@b,0.1][[1]],n];MatrixPlot/@{x,\[CapitalOmega],x \[CapitalOmega]}
{B,X}=recoverPointCharges[x \[CapitalOmega],\[CapitalOmega],30];
Print[MatrixPlot/@{B,X}]
Norm[(1-\[CapitalOmega])(x-X),"Frobenius"]
(*ListPlot3D[Join@@Table[{i,j,Partition[X,n][[i,j]]},{i,n},{j,n}],PlotRange->All]*)
{B2,X2}=robustrecoverPointCharges[x \[CapitalOmega],\[CapitalOmega],50];
Print[MatrixPlot/@{B2,X2}]
Norm[(1-\[CapitalOmega])(x-X2),"Frobenius"]


expr={HoldForm[Subscript[OverDot@OverHat@x,t]==
	\[Alpha](Subscript[x,t]-Subscript[OverHat@x,t])],
	HoldForm[Subscript[OverHat@X,s]==\[Alpha]/(s+\[Alpha])Subscript[X,s]]};
expr=Ga[2\[Theta]\[Mu]/\[Sigma]^2,\[Sigma]^2/2/\[Theta]];
expr=HoldForm[Subscript[{{x},{y}},1<=i<=n]]/.{x->Subscript[x,i],y->Subscript[y,i]};
expr=HoldForm[Subscript[{{OverHat[x]},{OverHat[y]}},1<=i<=n]]/.{x->Subscript[x,i],y->Subscript[y,i]};
(*expr=HoldForm[Subscript[S,i]==BracketingBar[
		{{Cos[\[Theta]],-Sin[\[Theta]]},{Sin[\[Theta]],Cos[\[Theta]]}} {{x},{y}}-{{OverHat@x},{OverHat@y}}+OverVector[b]]^2
	]/.{x->Subscript[x,i],y->Subscript[y,i]}*)
(*expr=HoldForm[S==Sum[Subscript[S,i],{i,1,n}]];
expr={HoldForm[D[S,\[Theta]]==0],HoldForm[D[S,OverVector[b]]==0]}*)
expr=HoldForm[Subscript[{{OverTilde[x]},{OverTilde[y]}},1<=i<=n]]/.{x->Subscript[x,i],y->Subscript[y,i]};
expr=HoldForm[{{OverTilde[x]},{OverTilde[y]}}=={{x Cos[i \[Theta]]+y Sin[i \[Theta]]},{y Cos[i \[Theta]]-x Sin[i \[Theta]]}}]/.{x->Subscript[x,i],y->Subscript[y,i]};
expr=HoldForm[\[ScriptCapitalN][\[CapitalDelta]{{OverTilde[x]},{OverTilde[y]}},(Subscript[\[Sigma],i]+Subscript[\[Sigma],i+1])I]];
expr=HoldForm[\[CapitalDelta]{{OverHat[x]},{OverHat[y]}}];
expr=HoldForm[Subscript[OverHat[x],k|k]==(\[ScriptCapitalI]-A)Subscript[OverHat[x],k|k-1]+A Subscript[x,k]
	+Subscript[P,k|k-1]Subsuperscript[H,k,T]Subsuperscript[S,k,-1]Subscript[v,k]];
expr=HoldForm[p[Subscript[z,k]|Subscript[x,k]]];
expr=HoldForm[p[Subscript[x,k]|Subscript[x,k-1]]];
expr=HoldForm[p[Subscript[x,k]|Subscript[z,k]]];
expr=HoldForm[OverDot@OverHat[\[Theta]]==Subscript[OverDot@\[Theta],g]+1/\[Tau] (Subscript[\[Theta],am]-OverHat[\[Theta]])];
expr=HoldForm[OverHat[\[CapitalTheta]]==(\[Tau]s/(1+\[Tau]s))Subscript[\[CapitalTheta],g]+(1/(1+\[Tau]s))Subscript[\[CapitalTheta],am]];
expr={OverDot@OverHat[\[Theta]]==Subscript[OverDot@\[Theta],g]+1/\[Tau] (Subscript[\[Theta],am]-OverHat[\[Theta]])-b,
	OverDot@b==-Subscript[k,b](Subscript[\[Theta],am]-OverHat[\[Theta]])};
expr=HoldForm[OverHat[\[CapitalTheta]]==(s^2/(s^2+s/\[Tau]+Subscript[k,b]))Subscript[\[CapitalTheta],g]+((s/\[Tau]+Subscript[k,b])/(s^2+s/\[Tau]+Subscript[k,b]))Subscript[\[CapitalTheta],am]];
expr=Ga[2\[Theta]\[Mu]/\[Sigma]^2,\[Sigma]^2/2/\[Theta]];
form=TableForm[TraditionalForm/@expr]
Export["~/t.png",form,ImageResolution->200]
(*Export["~/t.png",TraditionalForm[A==Subscript[P, k|k-1] Subsuperscript[H, k, T] Subsuperscript[S, k, -1] Subscript[H, k]],ImageResolution->200]*)
(*A==Subscript[P, k|k-1] Subsuperscript[H, k, T] Subsuperscript[S, k, -1] Subscript[H, k]//TeXForm*)


ClearAll[a];
A=Table[a[i,j],{i,6},{j,4}];
A2=A.Transpose[A]//.{Times[a_,b_]->!Xor[a,b],Plus[a_,b_]->Or[a,b]};
FindInstance[And@@Map[First,Select[Flatten[MapIndexed[{#1,#2}&,A2,{2}],1],#[[2,1]]!=#[[2,2]]&]],Flatten@A,Booleans]


e=HoldForm[D[f,t]=-D[\[Mu] f,x]+D[D f,{x,2}]];
e=HoldForm[Dt@Subscript[X,t]=-D[U,X]Dt@t+Sqrt[2D] Dt@Subscript[W,t]];
e=HoldForm[-D[U,x]f=D[D f,x]];
e=HoldForm[f=1/D E^(-U/D)];
e=HoldForm[Dt@Subscript[X,t]=\[Mu] Dt@t+Sqrt[2D] Dt@Subscript[W,t]];
e=HoldForm[\[Mu]=-D[U,x]];
(*e=HoldForm[Log[1+Abs[x-\[Mu]]]];
e=HoldForm[Integrate[Exp[-U],{x,-Infinity,Infinity}]=1];
e=HoldForm[Proportional[U,Log[1+(x-\[Mu])^2]]];
e=HoldForm[Dt[Subscript[r,t]]=\[Theta](\[Mu]-Subscript[r,t])Dt[t]+\[Sigma] Sqrt[Subscript[r,t]]Dt[Subscript[W,t]]];
e=HoldForm[2Dt[Sqrt@Subscript[r,t]]=\[Theta] (\[Mu]-Subscript[r,t])/Sqrt[Subscript[r,t]] Dt[t]+\[Sigma] Dt[Subscript[W,t]]];
e=HoldForm[x^(2\[Theta]\[Mu])Exp[-\[Theta]/4 x^2]];
*)
f=Function[e,{e,-D[e,x],E^(-e)}];
e={{"\:52bf\:80fd","\:529b","\:5206\:5e03"},f[(x-\[Mu])^2],f[Abs[x-\[Mu]]],f[Log[1+(x-\[Mu])^2]],f[Sqrt[1+(x-\[Mu])^2]-1]};
e//TraditionalForm
e//TeXForm
Export["~/t.png",e//TraditionalForm,ImageResolution->200]


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


H={{2/Sqrt[5],0},{1/Sqrt[5],0},{0,1}};
H2={{1/Sqrt[2],0},{1/Sqrt[2],0},{0,1}};
H//MatrixForm
Transpose[H].H//MatrixForm
Transpose[H2].H2//MatrixForm
H.Transpose[H]//MatrixForm
Tr[H.Transpose[H]]
Tr[H2.Transpose[H2]]


img=ImageTake[Import@"~/Downloads/I_2.gif",{1,-1},{110,300}]
showKeyPoints=Function[{img},
	With[{xy=
		(*ImageKeypoints[img,MaxFeatures->50]*)
		(*ImageKeypoints[img]*)
		First/@Select[Transpose@{ImageKeypoints@img,ImageKeypoints[img,"Strength"]},#[[2]]>0.002&]
		},Show[img,Graphics[{PointSize[Medium],White,Point[xy]}]]]];
showKeyPoints@img
showKeyPoints@ImageRotate[img,10Degree]
showKeyPoints@ImageRotate[img,30Degree]
showKeyPoints@ImageResize[img,Floor[ImageDimensions@img/2]]
showKeyPoints@ImageResize[img,Floor[ImageDimensions@img * 2]]


(*Dynamic[ImageDifference@@CurrentImage[2]]*)


(*Dynamic[showKeyPoints@CurrentImage[]]*)


l=Import["~/t3.csv"][[;;-2,;;-2]]
cols={Sequence@@{StringTake[#,4],StringTake[#,{5,-1}]}&@StringReplace[#[[1]],"\""->""],
	Sequence@@{
		{If[MemberQ[#,"GPS"],"GPS","NOGPS"],
		If[MemberQ[#,"WIFI"],"WIFI","NOWIFI"]}&@StringSplit[StringSplit[#[[2]],"_LOWF"][[1]],"_"],If[StringMatchQ[#[[2]],__~~"LOWF"],"LowF","HighF"]}}&@StringSplit[#,"_HOOK_"]&/@l[[1,2;;]]
rows=l[[2;;,1]]
data=Join@@Table[Flatten@{l[[1+i,1+j]],rows[[i]],cols[[j]]},{i,Length@rows},{j,Length@cols}];data//TableForm
Export["~/t4.csv",Prepend[data,StringSplit["power,device,gps_setting,wifi_setting,gps_hook,wifi_hook,freq",","]]]


imgs=Import/@FileNames["~/Downloads/df_golden/*.JPG"][[;;10]]


imgsD=FileBaseName[#]->ImageResize[Import[#],400]&/@FileNames["~/Downloads/df_golden/*.JPG"];
allImgs=Last/@imgsD;
imgs=(StringSplit[#,":"][[-1]]&/@Import["t3.csv"][[1,;;-2]])/.imgsD;
cameras=Import["t4.csv"][[;;Length@imgs]];


coords={{0, 0, 1}, {0, 1, 1}, {1, 1, 1}, {1, 0, 1}};
Graphics3D@{Opacity[1], Texture[imgs[[1]]],
   Polygon[coords, VertexTextureCoordinates -> coords]}


(*ListPointPlot3D@*)With[{m=Partition[Import["t4.csv"][[1]],4]},Table[m[[;;,;;3]].{i,j,1}-m[[;;,4]],{i,5},{j,8}]]


Function[images,
{i1,i2}=ImageResize[#,500]&/@images;tr=FindGeometricTransform@@{i1,i2};
{w,h}=ImageDimensions[i2];
tmp=ImagePerspectiveTransformation[i2,tr[[2]],DataRange->Full,PlotRange->{{0,First@tr[[2]][{w,0}]},{0,h}}];
ImageCompose[tmp,{i1,.7},Round@({w,h}/2)]
]/@Partition[imgs,2,1]


matches=ImageCorrespondingPoints@@images;
MapThread[Show[#1,Graphics[{Yellow,MapIndexed[Inset[#2[[1]],#1]& ,#2]}]]&,{images,matches}]


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


Manipulate[BilateralFilter[#,\[Sigma],\[Mu]]&@img,{\[Sigma],0,10},{\[Mu],0,1}]


Manipulate[ker=GaussianMatrix[r];
{ker//MatrixPlot,Image[#,ImageSize->500]&@ImageConvolve[img,ker],
	Image[#,ImageSize->500]&@ImageDeconvolve[ImageConvolve[img,ker],ker]},{r,0,10}]


Plot3D[Sin[3x] + Sin[3y], {x, 0, 2.5}, {y, 0, 2.5}, Mesh -> None, 
 Lighting -> "Neutral", 
 PlotStyle -> Texture[images[[1]]]]


bin="/usr/local/google/home/georgezhou/ssd/gumdrop/prof/pipeline_cli.orig"
Parallelize["cpprof -text "<>bin<>" "<>#<>">/tmp/"<>FileBaseName[#]<>".txt"&/@FileNames[FileNameJoin@FileNameSplit[bin][[;;-2]]<>"/pp_*"]]


{{f,0,p0},{0,f,p1},{0,0,1}}.MapThread[Append,{IdentityMatrix[3],-Table[c[i],{i,3}]}]//MatrixForm


img=Import@"~/Downloads/Venedig_Pano_AM.jpg";


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


n=3
(*d[right=1;up=2;left=3;down=4;*)
goto=Table[False,{i,n},{j,n},{d,4}];
(*move[pos,*)
pp=Function[goto,Table[If[Not@goto[[i,j,d]],Rotate[Line[{{j,-i},{j+1,-i}}],d 90Degree,{j+1/2,-i-1/2}]],{i,n},{j,n},{d,4}]];
(*Map[legals,Table[{i,j},{i,n},{j,n}],{2}]/.{1->"right",2->"up",3->"left",4->"down"}//MatrixForm*)
Graphics[pp[goto](*,Axes->True*)]


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


dim=400{1,1};
{gray,rgb,\[CapitalOmega]}=prepareDataRpcaColor[Import["~/d/fountain_depthmap.jpg"],dim,0.999];
CombineRGB[rgb \[CapitalOmega]]
(*color=ArrayFlatten@{ImageData/@ColorSeparate[ImageResize[Import@"~/d/fountain.jpg",dim]]};
Print[LS=Rpca2[ArrayFlatten@{{rgb \[CapitalOmega],color}},ArrayFlatten@{{\[CapitalOmega],Map[1.&,color,{2}]}},100];//AbsoluteTiming];
L=Take[LS[[1]],{1,dim[[1]]},{1,3dim[[2]]}];
CombineRGB@L
Print[{L3,S3}=Rpca2[rgb \[CapitalOmega],\[CapitalOmega],100];//AbsoluteTiming];
CombineRGB@L3
Norm[(1-\[CapitalOmega])(#-rgb),"Frobenius"]/Norm[(1-\[CapitalOmega])rgb,"Frobenius"]&/@{L,L3}*)


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


aitkenAcceleration@(4Accumulate@Table[(-1)^(i+1)/(2i-1),{i,10}])
Nest[aitkenAcceleration,(4Accumulate@Table[(-1)^(i+1)/(2i-1),{i,15}]),2]
Divide@@@Transpose[Accumulate/@Transpose[{Numerator@#,Denominator@#}&/@(4Accumulate@Table[(-1)^(i+1)/(2i-1),{i,10}])]]


(*Solve[xn-xx-(xn-xx)/(xn1-xn)==xn1-xx-(xn1-xx)/(xn2-xn1),{xx}]//Simplify*)
(*newtonAcceleration=Function[l,(#[[2]](-1-#[[2]]+#[[3]])+#[[1]]^2(-#[[2]]+#[[3]])+#[[1]](2#[[2]]^2+#[[3]]-2#[[2]]#[[3]]))
	/(#[[3]]-2#[[2]]+#[[1]])&/@Partition[l,3,1]];*)
(*With[{f=newtonAcceleration},Transpose@{history[[3;;,1]],f@history[[;;,2]],f@history[[;;,3]]}]*)
aitkenAcceleration=Function[l,#[[3]]-(#[[3]]-#[[2]])^2/(#[[3]]-2#[[2]]+#[[1]])&/@Partition[l,3,1]];
testAitken=Function[n,With[{f=Nest[aitkenAcceleration,#,n]&},Transpose@{history[[1+2n;;,1]],f@history[[;;,3]],f@history[[;;,4]]}]];


imgs=ImageResize[Import@#,300]&/@FileNames["/s/island/*.JPG"]


extractPos=Function[{n,matrix},Map[If[Head@#===Missing,0,#[[n]]]&,matrix,{2}]];
tracks=ImageFeatureTrack[imgs];
m=extractPos[1,tracks]~Join~extractPos[2,tracks];m//MatrixForm
\[CapitalOmega]=Map[If[#!=0,1,0]&,m,{2}];
{L,S}=Rpca2[m,\[CapitalOmega],100];
L//MatrixForm
xys=MapThread[List,Partition[L,Length[L]/2],2];xys//MatrixForm
Table[ShowImagePoints[imgs[[i]],xys[[i]],3],{i,Length@imgs}]


ShowImagePoints=Function[{img,points,scale},Show[img,Graphics[Table[{{Yellow,Circle[p,scale]}},{p,points}]]]];
(*ShowImagePoints[imgs[[1]],ImageKeypoints[imgs[[1]],{"Position"},MaxFeatures->100],3]*)


m=Mean[ImageData/@ColorSeparate@ImageResize[Import["~/t.jpg"],500]];Image@m
{U,S,V}=SingularValueDecomposition[m,Min@Dimensions@m];
(*U.S.Transpose[V]//Image//ImageAdjust*)
{#,U.DiagonalMatrix[#[Diagonal[S]]].Transpose[V]//Image//ImageAdjust//Magnify[#,4]&}&/@
	{#&,ProductLog[Exp[#]]&,#^3&,MapIndexed[If[#2[[1]]==1,#,0]&,#]&,Sqrt,If[#>1,#-1,0]&/@#&}


Dm=Mean[ImageData/@ColorSeparate[ImageResize[Import@"d/yuanshan.jpg",500]]];
{L,S}=Rpca[Dm,1./Sqrt@Max@Dimensions@Dm,1000];
Magnify[#,4]&@ImageAdjust@Image@#&/@{Dm,L,S}


Table[Pause[1];CurrentImage[],{20}]
MapIndexed[Export["/s/seq2/"<>IntegerString[#2[[1]]]<>".png",#]&,imgs]


name="Tony_Blair";name="Tony_Blair";
imgs=Import/@FileNames["/s/aligned_faces/lfw_funneled/"<>name<>"/*.jpg"];
n=300;
m=Flatten@ImageData@ImageResize[ColorSeparate[#][[2]],n{1,1}]&/@imgs;
k=Min[10,Length@m];
imageData=Partition[#,n]&/@Transpose[SingularValueDecomposition[m,k][[3,;;,;;k]]];
ImageAdjust@Image[#]&/@(Join@@(Transpose@{imageData,-imageData}))


sentences=Import["~/d/chinese-segment/icwb2-data/training/msr_training.utf8","Lines"];sentences[[1]]
cleaned=Select[If[Length@#>=3&&#[[1]]=="\[OpenCurlyDoubleQuote]",#[[3;;]],#]&@StringSplit[#," "],#!=""&]&/@sentences;(*A lot of sentences starts with double quote, no idea why.*)
lineStates=Function[ln,If[StringLength@#==1,"S",Join@@{{"B"},Table["M",{StringLength@#-2}],{"E"}}]&/@ln];
Thread@{lineStates/@#,#}&@cleaned[[;;3]]
transitionsF=Function[ln,Partition[Flatten@lineStates@ln,2,1]];
transitions=Tally[Join@@(transitionsF/@cleaned)]
dict={"B"->1,"M"->2,"E"->3,"S"->4};
N[#/Total[#]]&/@Normal@SparseArray[#[[1]]->#[[2]]&/@(transitions/.dict)]//MatrixForm


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


gridDim={4,4};prefix="v";
vertices=Join@@Table[{prefix,i,j},{i,gridDim[[1]]},{j,gridDim[[2]]}];
perVertex=4;
createMatrix=Function[{id,dim,vertices},
	Floor[(dim+(perVertex-1){1,1})/perVertex]
	];
createMatrix["m",{10,10},vertices]
(*{"create",{"m",{1,1}},{4,4}}*)


SeedRandom[1003];
hashTable=System`Utilities`HashTable;hashTableAdd=System`Utilities`HashTableAdd;hashTableGet=System`Utilities`HashTableGet;hashTableRemove=System`Utilities`HashTableRemove;hashTableContainsQ=System`Utilities`HashTableContainsQ;
hashTableUpdate=Function[{h,k,v},If[hashTableContainsQ[h,k],hashTableRemove[h,k]];hashTableAdd[h,k,v]];
hashTableAppend=Function[{h,k,v},If[hashTableContainsQ[h,k],With[{ov=hashTableGet[h,k]},hashTableRemove[h,k];hashTableAdd[h,k,Append[ov,v]]],hashTableAdd[h,k,{v}]]];
numVertices=4;prefix="v";blockSize=1000;
buildPlan=Function[{state,cmds},((hashTableGet[state,"vertices"]/.(#[[1,3]]->#&/@GatherBy[#,#[[3]]&]))&/@cmds)];
createMatrix=Function[{state,id,rawDim},Module[{dim,blocks},
		dim=Ceiling[rawDim/blockSize];blocks=Join@@Table[{i,j},{i,dim[[1]]},{j, dim[[2]]}];
		{With[{block=id[#[[2,1]],#[[2,2]]],vertex=#[[1]]},
				hashTableUpdate[hashTableGet[state,"address"],block,vertex];hashTableAppend[hashTableGet[state,"blocks"],id,block];
				{"create",block,vertex}]&/@Transpose@{RandomChoice[hashTableGet[state,"vertices"],Times@@dim],blocks}}
	]];
moveMatrix=Function[{state,blocks},Module[{dim,matrixBlocks=hashTableGet[state,"blocks"],address=hashTableGet[state,"address"],pos},
	(*Print[{#,hashTableGet[address,#]}&/@blocks];*)
	pos=hashTableGet[address,RandomChoice[blocks]];
	(*Print@pos;*)
	With[{blk=#,opos=hashTableGet[address,#]},{"send",blk,opos,pos}]&/@Select[blocks,hashTableGet[address,#]=!=pos&]]
	];
computeComponentWise=Function[{state,dstId,srcVals,srcIds,compute},(*z=ax+y*)Module[{matrixBlocks=hashTableGet[state,"blocks"],address=hashTableGet[state,"address"],cmds},
	cmds=Join@@(moveMatrix[state,#]&/@GatherBy[Union@Flatten[hashTableGet[matrixBlocks,#]&/@Append[srcIds,dstId]],#[[1]]&]);
	(*Print@cmds;*)
	hashTableUpdate[address,#[[2]],#[[4]]]&/@cmds;
	{cmds}~Join~{Union[{"sync","",#}&/@cmds[[;;,4]]]}~Join~
	{{"compute",#,hashTableGet[address,#],compute,srcVals,srcIds}&/@hashTableGet[matrixBlocks,dstId]}
	]];
computeMatrixDot=Function[{state,dstId,srcVals,srcIds},Module[{matrixBlocks=hashTableGet[state,"blocks"],address=hashTableGet[state,"address"],cmds,exprs,
		dstBlks,blks1,blks2},
	(*getDim=Function[{id,val},If[val=="T",Reverse,Identity]@(Length@Union@#&/@{#[[;;,1]],#[[;;,2]]}&@hashTableGet[matrixBlocks,id])];*)
	(*Print[MapThread[getDim,{Append[srcIds,dstId],Append[srcVals,""]}]];*)
	{blks1,blks2,dstBlks}=hashTableGet[matrixBlocks,#]&/@Append[srcIds,dstId];
	exprs=Function[dstBlk,
		dstBlk->(Select[blks1,If[srcVals[[1]]=="T",#[[2]],#[[1]]]==dstBlk[[1]]&]~Join~Select[blks2,If[srcVals[[2]]=="T",#[[1]],#[[2]]]==dstBlk[[2]]&])
		]/@dstBlks;
	cmds=Join@@(moveMatrix[state,Append[#[[2]],#[[1]]]]&/@exprs);
	(*Print@exprs;*)
	hashTableUpdate[address,#[[2]],#[[4]]]&/@cmds;
	(*(Print@MatrixForm@#;#)&[*){cmds}~Join~{Union[{"sync","",#}&/@cmds[[;;,4]]]}~Join~{{"compute",#[[1]],hashTableGet[address,#[[1]]],srcVals,#[[2]],"dot"}&/@exprs}
	]];
computeMatrix=Function[{state,dstId,srcVals,srcIds,compute},Module[{},
		Switch[compute
				,"uniform",{{"compute",#,address@#,{"uniform",srcVals[[;;2]]}}&/@hashTableGet[hashTableGet[state,"blocks"],dstId]}
				,"dot",computeMatrixDot[state,dstId,srcVals,srcIds]
				,_,computeComponentWise[state,dstId,srcVals,srcIds,compute]]
	]];
state=hashTable[];
hashTableUpdate[state,"blocks",hashTable[]];(*blocks of a matrix*)
hashTableUpdate[state,"vertices",prefix[#]&/@Range[numVertices]];
hashTableUpdate[state,"address",hashTable[]];(*block to vertex*)
plan=buildPlan[state,Join@@{
	createMatrix[state,"a",{3020,1100}],
	createMatrix[state,"b",{3020,1100}],
	createMatrix[state,"c",{1100,1100}],
	computeMatrix[state,"a",{0,10},{},"uniform"],
	computeMatrix[state,"a",{0.5},{"a","b"},"zaxy"],
	computeMatrix[state,"c",{"T",""},{"a","b"},"dot"]}];
plan//TraditionalForm


(*dumpMemory=Function[{memory,vertices},/@vertices]*)
execInstruction=Function[{memory,instr,blockSize},
	Switch[instr[[1]]
		,"create",hashTableAdd[hashTableGet[memory,instr[[3]]],instr[[2]],0 IdentityMatrix@blockSize]
		,"send",]]


execPlan=Function[{plan,vertices},Module[{memory=Table[hashTable[],{Length@vertices}],vertexNum=First,indices,blockSize=2},
	indices=vertexNum/@vertices;
	
	/@plan;
	]];
execPlan[plan,hashTableGet[state,"vertices"]]


h = System`Utilities`HashTable[];
System`Utilities`HashTableAdd[h, "a", {3,"a"}];
System`Utilities`HashTableAdd[h, "b", 2];
ToHeldExpression[ToString[h, InputForm]][[1, 2]]


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


robustSvdCostFunction=Function[{\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y},
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten@MapThread[Times,{\[CapitalOmega]s,S}],
		Total@Abs@Flatten@X,
		\[Mu]/2 Total@Flatten@MapThread[Function[{d,x,s},(d-U.x.Transpose[V]-s)^2],{D,X,S}],
		Total@MapThread[Function[{y,d,s,x},Tr[Transpose[y].(d-U.x.Transpose[V]-s)]],{Y,D,S,X}]}];
robustPvd=Function[{Data,\[CapitalOmega]s,rank,maxIter},Module[{D=Data,norm2,Y,\[Mu],\[Rho]=1.01,\[Lambda]=10.,X,S,L,ZS,U,V,n=Length@Data,dim=Dimensions[Data[[1]]],VU,VUT,solveU},
	norm2=First@SingularValueList[Mean@Data,1];Y=0 D;S=0D;\[Mu]=1.25 n/norm2;{U,V}=Transpose@Orthogonalize@Transpose@RandomReal[1,#]&/@{{dim[[1]],rank},{dim[[2]],rank}};
	X=RandomReal[1,{n,rank,rank}];Print@{"\[Mu]",\[Mu]};
	(*Print[Dimensions/@{Data,\[CapitalOmega]s,U,X,V,Y}];*)
	Do[
		L=U.#.Transpose[V]&/@X;
		ZS=MapThread[Function[{Y,D,L},Y/\[Mu]+D-L],{Y,D,L}];
		If[Mod[j,300]==1,Print[robustSvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]]];
		S=MapThread[Function[{\[CapitalOmega],ZS},\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],ZS]+(1-\[CapitalOmega])ZS],{\[CapitalOmega]s,ZS}];
		If[Mod[j,300]==1,Print[{"S",robustSvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		X=MapThread[Function[{D,S,Y},dShrinkage[1/\[Mu],Transpose[U].(D-S+Y/\[Mu]).V]],{D,S,Y}];
		If[Mod[j,300]==1,Print[{"X",robustSvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		U=#[[3]].Transpose[#[[1]]]&@SingularValueDecomposition[#,rank]&@Total@MapThread[Function[{D,S,Y,X},X.Transpose[V].Transpose[D-S+Y/\[Mu]]],{D,S,Y,X}];
		If[Mod[j,300]==1,Print[{"U",robustSvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		V=#[[1]].Transpose[#[[3]]]&@SingularValueDecomposition[#,rank]&@Total@MapThread[Function[{D,S,Y,X},Transpose[D-S+Y/\[Mu]].U.X],{D,S,Y,X}];
		If[Mod[j,300]==1,Print[{"V",robustSvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		Y+=\[Mu] MapThread[Function[{D,X,S},D-U.X.Transpose[V]-S],{D,X,S}];
		\[Mu]*=\[Rho];
		(*Print@MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}];*)
		(*Print[Dimensions/@{U,X,V,S,Y}];*)
		(*Print[MatrixForm/@{U,X,V,S,Y}];*)
	,{j,maxIter}];
	{U,X,V,S}
	]];


SeedRandom[1003];n=400;
data=ImageData/@ColorSeparate@ImageResize[Import["t.jpg"],n{1,1}];\[CapitalOmega]s=RandomSparseMatrix[Dimensions[#][[1]],Dimensions[#][[2]],0.5]&/@data;
Ds=MapThread[Times,{data,\[CapitalOmega]s},1];Image/@Ds
{U,X,V,S}=robustPvd[Ds,\[CapitalOmega]s,Floor[0.1n],1000];//AbsoluteTiming
ImageResize[ColorCombine[Image[U.#.Transpose[V]]&/@X],400]
ImageAdjust@Image[#]&/@S
MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}]


Table[Export["D"<>IntegerString[i]<>".csv",data[[i]]],{i,3}];
Table[Export["O"<>IntegerString[i]<>".csv",\[CapitalOmega]s[[i]]],{i,3}];


{L,S}=Rpca2[ArrayFlatten@{Ds},ArrayFlatten@{\[CapitalOmega]s},100];//AbsoluteTiming
CombineRGB@L


Norm[(1-ArrayFlatten@{\[CapitalOmega]s})(L-ArrayFlatten@{data}),"Frobenius"]^2


Total@MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]^2],{X,data,\[CapitalOmega]s}]


r2=robustPvd[{A},{Map[1&,A,{2}]},r,1000];
MatrixForm/@r2


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


robustSudCostFunction=Function[{\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y},
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten@MapThread[Times,{\[CapitalOmega]s,S}],
		Total@Abs@Flatten@X,
		\[Mu]/2 Total@Flatten[(D-foldXUs[X,Us,{}]-S)^2],
		Flatten[Y].Flatten[D-foldXUs[X,Us,{}]-S]}];
robustSud=Function[{Data,\[CapitalOmega]s,rank,maxIter},Module[{D=Data,norm2,Y,\[Mu],\[Rho]=1.01,\[Lambda]=10.,X,S,L,ZS,Us,n=Length@Data,dim=Dimensions[Data[[1]]]},
	norm2=First@SingularValueList[Mean@Data,1];Y=0 D;S=0D;\[Mu]=1.25 n/norm2;
	Us=Transpose@Orthogonalize@Transpose@RandomReal[1,#]&/@{{3,3},{dim[[1]],rank},{dim[[2]],rank}};
	X=RandomReal[1,{3,rank,rank}];Print@{"\[Mu]",\[Mu]};
	(*Print[Dimensions/@{Data,\[CapitalOmega]s,U,X,V,Y}];*)
	Do[
		L=foldXUs[X,Us,{}];
		ZS=Y/\[Mu]+D-L;
		If[Mod[j,300]==1,Print[robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]]];
		S=\[CapitalOmega]s dShrinkage[\[Lambda]/\[Mu],ZS]+(1-\[CapitalOmega]s)ZS;
		If[Mod[j,300]==1,Print[{"S",robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}]];
		X=dShrinkage[1/\[Mu],foldXUs[D-S+Y/\[Mu],Transpose/@Us,{}]];
		(*(*If[Mod[j,300]==1,*)Print[{"X",robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}];*)
		Do[
			Us[[i]]=procrustes[unfoldTensor[D-S+Y/\[Mu],i].Transpose[unfoldTensor[foldXUs[X,Us,{i}],i]]];
			If[Mod[j,300]==1,Print[{"Us"<>IntegerString@i,robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}]];
		,{i,Length@Us}];
		Y+=\[Mu] (D-foldXUs[X,Us,{}]-S);
		\[Mu]*=\[Rho];
		(*Print@MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}];*)
		(*Print[Dimensions/@{U,X,V,S,Y}];*)
		(*Print[MatrixForm/@Us];*)
		(*Print[MatrixForm/@{X,S,Y}];*)
	,{j,maxIter}];
	{X,Us,S}
	]];


{X2,Us,S2}=robustSud[Ds,\[CapitalOmega]s,Floor[0.1n],1000];//AbsoluteTiming
L=foldXUs[X2,Us,{}];
ImageResize[ColorCombine[Image/@L],400]
ImageAdjust@Image[#]&/@S2
Total@Flatten[((1-\[CapitalOmega]s)(L-data))^2]


SeedRandom[1003];n=400;
data=ImageData/@ColorSeparate@ImageResize[Import["t.jpg"],n{1,1}];\[CapitalOmega]s=RandomSparseMatrix[Dimensions[#][[1]],Dimensions[#][[2]],0.2]&/@data;
Ds=MapThread[Times,{data,\[CapitalOmega]s},1];Image/@Ds
{U,X,V,S}=robustPvd[Ds,\[CapitalOmega]s,Floor[0.1n],1000];//AbsoluteTiming
ImageResize[ColorCombine[Image[U.#.Transpose[V]]&/@X],400]
ImageAdjust@Image[#]&/@S
MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}]
L=U.#.Transpose[V]&/@X;
Total@Flatten[((1-\[CapitalOmega]s)(L-data))^2]


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


\!\(\(||\)\(X\)
\*SubscriptBox[\(||\), \(1\)]\(+\[Mu]\)\(||\)\(1 - D\ U . X . 
\*SuperscriptBox[\(V\), \(T\)]\)
\*SubscriptBox[\(||\), \(1\)]\(\(-\[Mu]\)\ D\ U . X . 
\*SuperscriptBox[\(V\), \(T\)]\)\)
sign(X)+\[Mu] (D V^T.U)^T sign(1-D U.X.V^T)-\[Mu] D V^T.U


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


n=3;order=3;Clear[a];vars=Array[a,order];as=Array[#,n{1,1}]&/@vars;SeedRandom[1003];
m=(*symmetrize@*)RandomReal[1,Array[n&,order]];matrixExp=MatrixExp;
getXus=Function[{as,m},Module[{us},us=Re@matrixExp[#-Transpose[#]]&/@as;sortXUs[foldXUs[m,Transpose/@us,{}],us]]];
disp=Function[{as,m},Module[{x=getXus[as,m][[1]]},Print[{x//Chop//MatrixForm,pnorm2[Flatten@x,1],
	{"unfold",Total@SingularValueList@If[MatrixQ@m,m,Join@@m]},
	{"hosvd",pnorm2[hosvd[0.01,m,n][[1]],1]},
	{"sud",pnorm2[sparseUnitaryDecomposition[m,n,30,False][[1]],1]}}]]];
vals={};
r=Module[{xus},NMinimize[(*-pnorm2[getXus[as,m][[1]],3]*)pnorm2[getXus[as,m][[1]],1],Flatten[as]
	,StepMonitor:>
		(xus=getXus[as,m];vals=Append[vals,pnorm2[xus[[1]],1]])]];//AbsoluteTiming
(*r=FindMinimum[-Total@Flatten[Abs[Re@matrixExp[Transpose[as]-as].m.Re@matrixExp[bs-Transpose[bs]]]^3],Flatten[as~Join~bs]];//AbsoluteTiming*)
disp[as/.r[[2]],m];
MatrixForm/@(SingularValueDecomposition/@m)
ListPlot[vals,PlotRange->All]
(*Following shows that sparseUnitaryDecomposition minimizes trace norm better than NMinimize*)
(*SeedRandom[1003];ListPlot[Table[pnorm2[sparseUnitaryDecomposition[m,n,n,False][[1]],1],{n,10,100}],PlotRange->All]
SeedRandom[1003];ListPlot[Table[pnorm[xus=sparseUnitaryDecomposition[m,n,n,False];foldXUs[xus[[1]],xus[[2]],{}]-m,2]/pnorm[m,2]
	,{n,10,200}],PlotRange->All]*)


m=Mean[ImageData/@ColorSeparate[ImageResize[Import@"t.jpg",400{1,1}]]];m2=m[[;;;;2,;;;;2]];
SingularValueList/@{m,2m2}
MatrixPlot/@SingularValueDecomposition@m
MatrixPlot/@SingularValueDecomposition@m2
Image@m
Image@m2


Select[Table[u=First@SingularValueDecomposition@RandomReal[1,3{1,1}];m={{{1,0,0},{0,1,0},{0,0,1}},{{0,1,0},{1,0,0},{0,0,0}},{{0,0,1},{0,0,0},{1,0,0}}};
	{MatrixForm/@m,pnorm2[m,1],pnorm2[foldXUs[m,Transpose/@{u,u,u},{}],1]},{10000}],#[[-2]]>#[[-1]]&]


Clear[x,y,sign];SeedRandom[1003];n=30;m=RandomReal[1,n{1,1}];m=m+Transpose[m];tmax=1;
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


Clear[x,y,sign,u,v];SeedRandom[1003];n=3;dim=n{1,1};m=RandomReal[1,dim];m2=Normal@SparseArray[{i_,j_}/;i==j->(n+1-i)^2,Reverse@dim];tmax=1;
s=NDSolve[{u'[t]==0.5(u[t].Transpose[m.v[t].m2].u[t]-m.v[t].m2),v'[t]==0.5(v[t].m2.Transpose[u[t]].m.v[t]-Transpose[m2.Transpose[u[t]].m]),
	u[0]==IdentityMatrix@dim,v[0]==IdentityMatrix@dim[[2]]},{u,v},{t,0,tmax},MaxSteps->10000];//AbsoluteTiming
mysingulars3=Function[{u,v,t,s,m},Abs@Diagonal[First@sortXUs[With[{U=First[u[t]/.s],V=First[v[t]/.s]},Transpose[U].m.V],{}]]];
{Norm[SingularValueList@m-mysingulars3[u,v,tmax,s,m]]/Norm@SingularValueList@m,SingularValueList@m,mysingulars3[u,v,tmax,s,m]}
ListLogPlot[Transpose@Table[mysingulars3[u,v,t,s,m],{t,0,tmax,0.1}],Joined->True]


Clear[n,h];{hs,ns}=DiagonalMatrix@Array[#,2]&/@{h,n};lieBracket[hs,lieBracket[hs,ns]]//MatrixForm
(*h=DiagonalMatrix@RandomReal[1,4]*)
h=(*Array[a,{2,2}];*){{1,3},{3,1}};(*h[[1,1]]=a[2,2];*)
MatrixForm/@{h,Sign[h]}
h.Transpose[Sign[h]]-Transpose[h.Transpose[Sign[h]]]//Simplify//TraditionalForm


SeedRandom[1003];array=RandomReal[1,#2]&;n=20;{us,vs,ws}=array[#,n{1,1}]&/@{u,v,w};ss=array[s,n{1,1,1}];
(*MatrixForm/@*)
vec@foldXUs[ss,{us,vs,ws},{}]==KroneckerProduct[ws,KroneckerProduct[vs,us]].vec[ss]


SeedRandom[1003];n=3;m=RandomReal[1,n{1,1,1}];{X,Us}=sparseUnitaryDecomposition[m,n,100,False];
X2=X;X2[[n,n,n]]=0;m2=foldXUs[X2,Us,{}];l={m,X,foldXUs[X,Us,{}],m2};
MatrixForm/@l
pnorm2[#,2]&/@l
pnorm2[#,1]&/@l
{X[[n,n,n]]^2,pnorm2[m2-m,2]}
Table[MatrixForm[#.Transpose[#]]&@unfoldTensor[X,i],{i,Length@Dimensions@X}]


metricTensor=Function[{coords,params},Module[{jacobian},
	jacobian=Outer[D[#,#2]&,coords,params,1];
	Simplify[Transpose[jacobian].jacobian]]];
christoffelSymbol=Function[{coords,params},Module[{g,ig,\[Mu],\[Sigma],\[Nu],\[Lambda]},(*We use 2nd kind*)
	g=metricTensor[coords,params];ig=Inverse[g];
	1/2 Total@Table[ig[[\[Lambda],\[Sigma]]](D[g[[\[Sigma],\[Nu]]],params[[\[Mu]]]]+D[g[[\[Sigma],\[Mu]]],params[[\[Nu]]]]-D[g[[\[Mu],\[Nu]]],params[[\[Sigma]]]]),
		{\[Sigma],Length@params},{\[Lambda],Length@params},{\[Mu],Length@params},{\[Nu],Length@params}]
	]];
geodesicEquation=Function[{coords,params},Module[{chris=christoffelSymbol[coords,params],\[Alpha],\[Beta],\[Gamma],xs=#[t]&/@params,r=#->#[t]&/@params},
	{#,DSolve[#,xs,t]}&@Simplify@Thread[Table[Dt[xs[[\[Alpha]]],{t,2}],{\[Alpha],Length@params}]==
		Total@Total@Table[-(chris[[\[Alpha],\[Beta],\[Gamma]]]/.r)Dt[xs[[\[Beta]]],t]Dt[xs[[\[Gamma]]],t],{\[Beta],Length@params},{\[Gamma],Length@params},{\[Alpha],Length@params}]]]];
riemannCurvatureTensor=Function[{coords,params},Module[{chris,\[Rho],\[Sigma],\[Mu],\[Nu],\[Lambda],n=Length@params},
	chris=christoffelSymbol[coords,params];
	Table[D[chris[[\[Rho],\[Nu],\[Sigma]]],params[[\[Mu]]]]-D[chris[[\[Rho],\[Mu],\[Sigma]]],params[[\[Nu]]]]+
		Total@Table[chris[[\[Rho],\[Mu],\[Lambda]]]chris[[\[Lambda],\[Nu],\[Sigma]]]-chris[[\[Rho],\[Nu],\[Lambda]]]chris[[\[Lambda],\[Mu],\[Sigma]]],{\[Lambda],n}]
		,{\[Rho],n},{\[Sigma],n},{\[Mu],n},{\[Nu],n}]]];
(*sectionalCurvature=Function[{u,v,coords,params},Module[{riemann=riemannCurvatureTensor[coords,params],i,j,k,l,n=Length@params},
	Total[Table[riemann[[l,i,j,k]]v[[i]]u[[j]]v[[k]],{i,n},{j,n},{k,n},{l,n}],3].u]];*)
ricciCurvatureTensor=Function[{coords,params},Module[{riemann=riemannCurvatureTensor[coords,params],i,l,j,\[Lambda],n=Length@params},
	Simplify@Total@Table[riemann[[l,i,l,j]],{l,n},{i,n},{j,n}]]];
ricciScalarCurvature=Function[{coords,params},Module[{ricci=ricciCurvatureTensor[coords,params],ig=Inverse@metricTensor[coords,params]},
	Simplify@Total[ricci ig,2]]];
test=Function[input,
	{input[[1]],{"metric tensor",MatrixForm@metricTensor[input[[2]],input[[3]]]},
		{"christoffel 2nd",MatrixForm/@christoffelSymbol[input[[2]],input[[3]]]},
		{"geodesicEquation",Simplify@geodesicEquation[input[[2]],input[[3]]]},
		{"sectionalCurvature",Simplify@sectionalCurvature[{0,1},{1,0},input[[2]],input[[3]]]},
		{"ricciCurvature",Simplify@ricciCurvatureTensor[input[[2]],input[[3]]]},
		{"scalarCurvature",ricciScalarCurvature[input[[2]],input[[3]]]}}];
Clear[t,r,x,n];
test@{"Cylinder",{Cos[\[Theta]],Sin[\[Theta]],z},{\[Theta],z}}
test@{"Torus",{(a+Cos@u)Cos@v,(a+Cos@u)Sin@v,Sin@u},{u,v}}
test@{"Polar",r{Cos[\[Theta]],Sin[\[Theta]]},{r,\[Theta]}}
test@{"Sphere",r{Sin[\[Theta]]Cos[\[Phi]],Sin[\[Theta]]Sin[\[Phi]],Cos[\[Theta]]},{\[Theta],\[Phi]}}
(*test@{"Minkowsski",{x,y,Sqrt[1+x^2+y^2]},{x,y}}*)
test@{"Hyperboloid",{Cosh[\[Phi]]Cos[\[Theta]],Cosh[\[Phi]]Sin[\[Theta]],Sinh[\[Phi]]},{\[Theta],\[Phi]}}
(*test@{"Hyperboloid n",{Sqrt[Total[{x1,x2,x3}^2]+r^2],x1,x2,x3},{x1,x2,x3}}*)
(*test@{"Poincar\[EAcute] half-plane model",{2x,(-1+x^2+y^2)}/Sqrt[x^2+(1+y)^2],{x,y}}*)
(*Clear[x,y,z];test@{"L^(1/2)",{x^(1/2),y^(1/2)},{x,y}}*)
(*r=NDSolve[{((x^\[Prime])^\[Prime])\[InvisibleApplication](t)+x^\[Prime](t)^2/x[t]==0,((y^\[Prime])^\[Prime])\[InvisibleApplication](t)+y^\[Prime](t)^2/y[t]==0,((z^\[Prime])^\[Prime])\[InvisibleApplication](t)+z^\[Prime](t)^2/z[t]==0,x[0]==1,y[0]==1,z[0]==4,x'[0]==1,y'[0]==1,z'[0]==1},{x[t],y[t],z[t]},{t,0,5}];
l=Table[First[{x[t],y[t],z[t]}/.r],{t,0,5,0.1}];
Graphics3D[Point@l,Axes->True]
ListPlot@Transpose@l*)


(*||||_ 1 is not convex on Stiefel Manifold*)
ms=Table[(#-Transpose[#])&@RandomReal[3{-1,1},{4,4}],{2}];
ListPlot[Transpose@Table[{pnorm2[MatrixExp[ms[[1]]],1](1-t)+pnorm2[MatrixExp[ms[[2]]],1]t,pnorm2[MatrixExp[ms[[1]](1-t)+ms[[2]]t],1]}
	,{t,0,1,0.03}],Joined->True,PlotRange->All]


(*(*Same minima for differnt p for matrix*)SeedRandom[1003];ms=Array[m,{2,2,2}];d=RandomReal[5{-1,1},{2,2}];MatrixForm/@d*)
(*(*Same minima for p<=1, and p>1 respecitively for high order tensor?*)SeedRandom[1004];order=4;ms=Array[m,{order,2,2}];d=RandomReal[3{-1,1},Array[2&,order]];MatrixForm/@d*)
(*SeedRandom[1004];order=6;ms=Array[m,{order,2,2}];d=RandomReal[3{-1,1},Array[2&,order]];MatrixForm/@d*)
d=ImageData/@ColorSeparate@ImageResize[Import[#],200{1,1}]&/@FileNames["/s/island/*.JPG"];d//Dimensions
ms=MapIndexed[Table[m[#2[[1]],i,j],{i,#},{j,#}]&,Dimensions@d];
(*mynorm=Function[p,If[p<2,1,-1] pnorm2[#,p]&];
opt=Function[p,
		r=NMinimize[mynorm[p]@foldXUs[d,MatrixExp[#-Transpose@#]&/@ms,{}],Flatten@ms
			(*,MaxIterations->100,Method->"DifferentialEvolution"*)
			(*,MaxIterations->100,Method->"NelderMead"*)
			,MaxIterations->100,Method->"RandomSearch"
		];
		tus=MatrixExp[#-Transpose@#]&/@(ms/.r[[2]]);x=foldXUs[d,tus,{}];
		{x,tus}=sortXUs[x,tus];
		{p,r[[1]],Chop@x}];
sx=opt[1.][[3]];sx2=opt[2.3][[3]];MatrixForm/@{sx,sx2}
SortBy[Flatten@#,-Abs@#&]&/@{sx,sx2}
Table[{#[[1]],#[[2]],mynorm[p][sx],mynorm[p][sx2],MatrixForm/@#[[3]]}&@opt[p]
	,{p,{0.2,0.3,0.5,0.9,1,1.1,1.3,1.5,1.9,2.1,3,5,10}}]*)


{X,Us}=sparseUnitaryDecomposition[d,50,30,False];folded=foldXUs[X,Us,{}];
pnorm[folded-d,1]/pnorm[d,1]
ListLogPlot[Reverse@SortBy[Flatten@X,Abs],PlotRange->All]


metricTensor=Function[{coords,params},Module[{jacobian},
jacobian=Outer[D[#,#2]&,coords,params,1];
Simplify[Transpose[jacobian].jacobian]]];
Clear[x,y];(*Rosenbrock_function*)f=(1-x)^2+100(y-x^2)^2
(Export["t.png",#];#)&[Plot3D[f,{x,-2,2},{y,-1,3},PlotRange->All,AxesLabel->{"x","y"}]]
(*h=D[f,{{x,y},2}];h//Simplify//MatrixForm
Reduce[h[[1,1]]>=0&&Det[h]>=0,{x,y},Reals]*)
NMinimize[f,{x,y}]
r={x2->(1-x),y2->(y-x^2)};r2={x->1-x2,y->y2+(1-x2)^2};
F=100y2^2+x2^2;F/.r
g=metricTensor[r[[;;,2]],{x,y}]
tmax=10;s=NDSolve[{{x2'[t],y2'[t]}==Evaluate[-Inverse[g].(D[F,{{x2,y2}}])/.r2/.{x2->x2[t],y2->y2[t]}],x2[0]==3,y2[0]==3},{x2[t],y2[t]},{t,0,tmax}];
l=Table[First[{x2[t],y2[t]}/.s],{t,0,tmax,0.1}];
(Export["t2.png",#];#)&[Rasterize@{ListPlot[Transpose@l,PlotRange->All,Joined->True,PlotLabel->"Plot of {x2,y2}"],
ListLogPlot[Transpose@l,PlotRange->All,Joined->True,PlotLabel->"Log plot of {x2,y2}"],
ListLogPlot[F/.Thread[{x2,y2}->#]&/@l,PlotRange->All,PlotLabel->"Minimum",Joined->True]}]


hessian=Function[{f,coords,params},Module[{chris,i,j,k},
	chris=christoffelSymbol[coords,params];
	Table[D[D[f,params[[j]]],params[[i]]]-Total@Table[chris[[k,i,j]]D[f,params[[k]]],
		{k,Length@params}],{i,Length@params},{j,Length@params}]]];
Clear[m,n,\[Theta],\[Sigma],\[Mu]];
{"Bernoulli",hessian[-\[Theta] Log[\[Theta]]-(1-\[Theta])Log[(1-\[Theta])],{\[Theta]},{\[Theta]}]//Simplify}
{"Normal",hessian[1/2 Log[2Pi E \[Sigma]^2],{\[Mu],\[Sigma]},{\[Mu],\[Sigma]}]//Simplify}


(*Inducing block diagonal, but here we miss another constraint to make Z meaningful*)
Z=Array[z,{3,3}];
r=NMinimize[With[{Z2=Z-DiagonalMatrix@Diagonal@Z},Total@Flatten@Abs[Transpose[Z2].Z2]],Flatten@Z]
Z-DiagonalMatrix@Diagonal@Z/.r[[2]]//MatrixForm


m=RandomReal[3{-1,1},3{1,1}];m//MatrixForm
(*Operator norm is log-convex, http://en.wikipedia.org/wiki/Riesz% E2 %80 %93 Thorin_interpolation _theorem*)
l=Table[{p,Log[pnorm[m,p]/pnorm[SingularValueDecomposition[m][[2]],p]]},{p,0.1,3,0.1}]
ListPlot[l,PlotRange->All]


l=Table[{p,pnorm[DiagonalMatrix[{1,2,3}],p]},{p,0.1,3,0.1}]
ListLogPlot[l,PlotRange->All]
l2=Table[{p,pnorm2[DiagonalMatrix[{1,2,3}],p]},{p,0.1,3,0.1}]
ListPlot[l2,PlotRange->All]


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


Reverse@SortBy[Flatten@xus[[1]],Abs]


Image@SvdApprox[rgbs[[1]],Floor[Length[rgbs[[1]]]/2]]


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


k=2;Clear[x,d,n];{xs,ds,ns}=Array[#,k{1,1}]&/@{x,d,n};r=Simplify[Transpose[xs].ds.xs.ns];
{xs,ds,ns}=RandomReal[1,{3,k,k}];xs=MatrixExp[skew[xs]];
vec[Transpose[xs].ds.xs.ns]
vec[ns].KroneckerProduct[Transpose@xs,Transpose@xs].vec[ds]
(*r[[1,1]]
r[[1,2]]
r[[2,1]]
r[[2,2]]*)


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


min ||y-A.x||_ 2^2 + ||x||_ 3/||x||_ 2
min ||y-A.x||_ 2^2 - ||x||_ 3 s.t. \sum x=1


Plot[{2If[x<1,x-x^2/2,1/2],Sqrt@x},{x,0,10}]


m=sShrinkage[1,ImageData@img];
Magnify[Image@#,2]&/@{ImageData@img,m,unSShrinkage[1,m]}


Rpca4=Function[{D,\[CapitalOmega],iter},Module[{norm2,\[Lambda]=10.,\[Eta]=10.,Y,L,S,\[Mu],\[Rho],m,n,Zl,Zs},
	(*min ||L||_ 0 + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S>, approximate by unshrinkage*)
	(*L,S,D are m-by-nn. \[CapitalOmega] is nonzeros*)
	{m,n}=Dimensions[D];norm2=SingularValueDecomposition[D,1][[2,1,1]];
	Y=Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];\[Mu]=1.25/norm2;\[Rho]=1.1;
	L=S=0 D;
	Do[
		Y=0;
    Zl=(Y+\[Mu](D-S))/\[Mu];
	L=(*unSShrinkage[1/\[Mu],*)sShrinkage[1/\[Mu],Zl];
	Zs=Y/\[Mu]+D-L;
	(*S=\[CapitalOmega] unDShrinkage[\[Lambda]/\[Mu],dShrinkage[\[Lambda]/\[Mu],Zs]]+(1-\[CapitalOmega]) Zs;*)
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];If[Mod[j,5]==1,PrintTemporary@Image[L]];
	,{j,iter}];
	{L,S}]];
rawImg=Import["~/t.jpg"];numIter=200;
mask = maskFromString[ExampleData[{"Text", "TaoTehChingChinese"}],15];mask//Image
dim=Dimensions@mask;
img=Image@Mean[ImageData/@ColorSeparate@ImageResize[rawImg,Reverse@dim]];
Print[{L,S}=Rpca2[ImageData[img] mask,mask,numIter];//AbsoluteTiming];
Print[{L2,S2}=Rpca4[ImageData[img] mask,mask,numIter];//AbsoluteTiming];
Norm[(1-mask)(#-ImageData[img]),"Frobenius"]&/@{L,L2}
Image@L
Image@L2


L4=unSShrinkage[0.5/(1.01^100),L];


Norm[(1-mask)(ImageData@img-#),"Frobenius"]&/@{L,L4,(L+L4)/2}


Transpose[Take[SingularValueList@#,MatrixRank@L]&/@{L,L4,ImageData@img}]


\[CapitalOmega]=randomSparseTensor[Dimensions@ImageData@img,0.5];
(*nimg=Image[ImageData[img] \[CapitalOmega],ImageData@img,{2}]];*)
{L,S}=Rpca2[ImageData[img] \[CapitalOmega],\[CapitalOmega],200];
img
(*nimg*)
Image@L
unSShrinkage[1/(1.01^100),L]//Image


(*mixed shrinkage, ||x||_ 1-||x||_ 3*)


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


(*fastIndependentComponentAnalysis=Function[{D,numComponent,maxIter,method},(*method can be Tanh or Normal *)
	Module[{W={},w,wold=0,n,m,g,gprime},{n,m}=Dimensions@D;
	{g,gprime}=Switch[method,"Tanh",{Tanh,1-Tanh[#]^2&},"Normal",# Exp[-#^2/2]&,(1-#^2)Exp[-#^2/2]&];
	Do[
		w=RandomReal[1,n];
		Do[
		If[Norm[w-wold]<0.001,Break[]];
		w=(D.g[w.D]-gprime[w.D])/m;
		,{j,maxIter}];
	,{p,numComponent}];
	W;
	]];*)


SeedRandom[1003];m=RandomReal[1,{2,2,2,2,2}];MatrixForm/@hosvd[0.001,m,2]
MatrixForm/@sparseUnitaryDecomposition[m,2,200,False]



