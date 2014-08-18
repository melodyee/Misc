(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
lowEntropyMask=Function[img,ImageData@ImageApply[1-#&,Dilation[Erosion[Binarize[ImageAdjust@EntropyFilter[img,1],0.8],0],0]]];
skyCharacteristicLine=Function[img,Module[{mask=lowEntropyMask@img,imgHeight=ImageDimensions[img][[2]],weight=2},
	Transpose[{#[[;;,1]],imputeWithMean[weight imgHeight MeanFilter[#[[;;,2]],0]]}]&@
		MapIndexed[{#2[[1]],#}&,(Mean@#(*+0.0001Length@#*))&@Select[#,#!=0&]&/@Transpose[ImageData[ColorSeparate[img,"LAB"][[3]]] mask]]]];
filterNonGray=Function[metaAndImgs,Select[metaAndImgs,Chop[Mean@Flatten[ImageData@ColorSeparate[#[[2]],"LAB"][[3]]],0.0001]!=0&]];
(*baimingAlbum=Import["/h/mxs/panos_"<>"baiming"<>".mx"];
tedAlbum=Import["/h/mxs/panos_"<>"ted"<>".mx"];*)

(*Panorama auto-connect*)
(*panos=Import[#(*,ImageSize->{400}*)]&/@FileNames["/h/panos/*.jpg"];
imgs=ImageResize[#,{400}]&/@{panos[[1]],panos[[2]]};*)
(*matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]]];
Magnify[MapThread[annotateImageWithPoints,{imgs,matches}],2]*)


Image@lowEntropyMask@ImageResize[ColorConvert[Import["/g/tmp/lychen_vietnam_pho.jpg"],"Gray"],{400}]


Image@lowEntropyMask@ColorConvert[Import["/g/tmp/lychen_vietnam_pho.jpg",ImageSize->{400}],"Gray"]
Binarize[EntropyFilter[ColorConvert[Import["/g/tmp/lychen_vietnam_pho.jpg",ImageSize->{400}],"Gray"],1],0.8]


m=ImageData@ImageFilter[((*Print@{N@Entropy@#,#};Abort[];*)Entropy@#)&,ColorConvert[Import["/g/tmp/lychen_vietnam_pho.jpg",ImageSize->{400}],"Gray"],1];m//Image


(*Binarize@*)ImageFilter[entropy,ColorConvert[#,"Gray"],1]&/@exampleStereoPairs[[;;,1]]


(*parseTimestampFromName=Function[fname,StringSplit[FileBaseName@fname,"_"][[2;;3]]];(*parseTimestampFromName@"/h/baiming_panos/PANO_20131214_110406.jpg"*)
user="ted";album=Parallelize@Map[{#,Import[#,ImageSize->{400}]}&,SplitBy[SortBy[{#,parseTimestampFromName@#}&/@FileNames[
	"/h/panos/"<>user<>"/PANO*.jpg"],Last],#[[2,1]]&][[;;,;;,1]],{2}];album//TableForm
Export["/h/mxs/panos_"<>user<>".mx",album]*)


img=imgs[[1]];
sunPts=detectSun[img];//AbsoluteTiming
Show[img,Graphics@{Thick,Red,drawNumberedPoints@sunPts}]
(*sunPt=With[{imgHeight=ImageDimensions[img][[2]]},N@Mean@Flatten[MapIndexed[If[#==1,{rowColumnToXy[#2[[1]],#2[[2]],imgHeight]},{}]&,Map[N@Boole[#==1]&,comps,{2}],{2}],2]]*)

(*rs=Parallelize@Map[{#,detectSun@#[[2]],countMorphologicalComponents@detectSun@#[[2]]}&,baimingAlbum,{2}];
Select[Join@@rs,Mean@Flatten@ImageData[#[[2]]]>0&]*)


panos[[1]]
panos[[2]]
panos[[3]]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-2r,2r},PlotStyle->{Opacity[1],Texture[Import["/g/tmp/cbk.jpg"]]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->800(*,ViewRange->{3,1000}*)]]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-2r,2r},PlotStyle->{Opacity[1],Texture[imgs[[1]]]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->800(*,ViewRange->{3,1000}*)]]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-2r,2r},PlotStyle->{Opacity[1],Texture[panos[[1]]]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->800(*,ViewRange->{3,1000}*)]]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-2r,2r},PlotStyle->{Opacity[1],Texture[panos[[2]]]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->800(*,ViewRange->{3,1000}*)]]


imgs=Import/@{"t.png","t2.png"};matches=imageCorrespondingPoints@@imgs;
MapThread[annotateImageWithPoints,{imgs,Transpose[RandomSample[Transpose[matches],200]]}]


With[{r=1},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-2r,2r},PlotStyle->{Opacity[1],Texture[panos[[3]]]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	,ViewPoint->{0,0.1,0},ImageSize->800(*,ViewRange->{3,1000}*)]]


Clear[f,r,t,t2,z];cnt=0;
(*Dynamic@cimg2*)
Dynamic@{ccorr,crtz}
f[r_,t_?NumericQ,t2_,z_]:=Module[{img2,corr},
	img2=ImageTransformation[ImageForwardTransformation[ImageTransformation[img,cf3[cf2[#,t],z]&],cf[#,r]&],cf2[#,t2]&];
	corr=correlation[vec@ImageData@img3,vec@ImageData@img2];If[Mod[cnt++,20]==0,ccorr=corr;cimg2=img2;crtz={r,t,t2,z}];
	-corr+3(r-1)^2+0.01t^2+0.01t2^2(*+0.5(z-1)^2*)];
r2=NMinimize[f[(*r*)1,t,(*t2*)0,1],{(*r,*)t(*,t2*)(*,z*)}];


LS=Rpca2[mask ImageData[ColorConvert[img,"Gray"]],mask,100];Image@LS[[1]]
Image@LS[[2]]


With[{img=dataM mask//Image},Graphics[RandomSample[Flatten@MapIndexed[If[Abs[#]>0.1,{Arrow[{#[[2]],-#[[1]]}&/@{#2,#2+{Re@#,Im@#}}]},{}]&
	,MapThread[#1 Exp[I #2]&,ImageData/@{GradientFilter[img,1],GradientOrientationFilter[img,1]},2],{2}],200],ImageSize->1000]]


Erosion[mask,1] GradientFilter[dataM mask,1]//Image//ImageAdjust
MatrixPlot[Erosion[mask,1] Map[First,GradientOrientationFilter[dataM mask,3],{2}],ImageSize->600]


img=ImageResize[ImageTake[panos[[3]],{1,1000}],{800}]
(*Dynamic@corr*)
img3=ImageResize[ImageTake[panos[[2]],{1,1000}],{800}]
Manipulate[img2=ImageTransformation[ImageForwardTransformation[ImageTransformation[img,panoChangeYScale[panoChangeAngle[#,t],z]&]
	,panoChangeRadius[#,r]&],panoChangeAngle[#,t2]&];img2,{{r,1},0.5,1.5,0.01},{{t,0},-1,1,0.05},{{t2,0},-1,1,0.05},{{z,1},0.5,1.8,0.05}
,ContentSize->{800}]


img=ImageResize[ImageTake[panos[[1]],{1,1000}],{800}]
img3=ImageResize[ImageTake[panos[[2]],{1,1000}],{800}]
patches=ImagePartition[img,20];ker=patches[[7,18]]
ImageAdjust@ImageCorrelate[img3,ColorConvert[ker,"Gray"],CosineDistance]


verticalAlignImgs=Function[{imgs,shift},GraphicsColumn[{ImageTransformation[imgs[[1]],panoChangeAngle[#,shift]&],imgs[[2]]},Left,-200,ImageSize->400]];
detectSun=Function[img,Module[{comps,sunPt,compWithCount},comps=MorphologicalComponents@Image@Map[N@Boole[Mean[#]>250./255]&,ImageData@ImageResize[img,{400}],{2}];
	If[Length@Tally@Flatten@comps>=2,compWithCount=Reverse[SortBy[Tally@Flatten@comps,Last]][[2]];
		If[compWithCount[[2]]>10,
			List@With[{imgHeight=ImageDimensions[img][[2]]},N@Mean@Flatten[MapIndexed[If[#==1,{
				rowColumnToXy[#2[[1]],#2[[2]],imgHeight]},{}]&,Map[N@Boole[#==compWithCount[[1]]]&,comps,{2}],{2}],2]],{}],{}]]];
alignBySun=Function[imgs,Module[{sunPts},sunPts=detectSun/@imgs;
	If[And@@(Length[#]>=1&/@sunPts),Subtract@@MapThread[#[[1,1]]/ImageDimensions[#2][[1]]&,{sunPts,imgs}]]]];
(*sunPts=detectSun[img];Show[img,Graphics@{Thick,Red,drawNumberedPoints@sunPts}]*)

alignPanoCluster=Function[metaAndImgsIn,Module[{metaAndImgs,panos,debugTab,imgs,lines,weights,corrs,shift,img,mask,dataM,g,desTable,res},
	Print[metaAndImgs={#[[1]],ImageResize[#[[2]],{400}]}&/@metaAndImgsIn;panos=metaAndImgs[[;;,2]];
	imgs=ImageTake[#,{1,Round[ImageDimensions[#][[2]]/2]}]&/@panos;lines=skyCharacteristicLine/@imgs;//AbsoluteTiming];
	Print[desTable=Join[{Labeled[Module[{image=#[[2]]},Show[image,Graphics@{Thick,Red,drawNumberedPoints@detectSun@image}]],#[[1]]]&/@metaAndImgs}
		,{Table[img=imgs[[i]];mask=lowEntropyMask@img;dataM=ImageData[ColorConvert[img,"Gray"]];
			g=Graphics[Line[{#[[1]],8#[[2]]}&/@lines[[i]]](*,ImageSize->400*)];
			GraphicsColumn[{g,Image[dataM mask],ImageResize[panos[[i]],{400}]},Left,-120,ImageSize->{400}]
	,{i,Length@panos}]}];//AbsoluteTiming];
	weights=lines[[;;,;;,2]];
	Print[res=Join[desTable,Table[corrs=ListCorrelate[weights[[indices[[1]]]],weights[[indices[[2]]]],1];shift=First@Ordering[corrs,-1]-1;
		{GraphicsColumn@{ListLinePlot@corrs,ListLinePlot@{RotateRight[weights[[indices[[1]]]],shift],weights[[indices[[2]]]]}}
			,verticalAlignImgs[imgs[[indices]],alignBySun[imgs[[indices]]]],verticalAlignImgs[imgs[[indices]],-shift/Length@lines[[1]]]}
	,{indices,Partition[Range[Length@panos],2,1,1]}]];//AbsoluteTiming];
	res]];
aligned=alignPanoCluster@MapIndexed[{#2,ImageResize[#,{400}]}&,panos,{1}];aligned//TableForm
(*Export["/tmp/t.html",TableForm@aligned]*)
(*alignPanoCluster@baimingAlbum[[25]]*)


rsTed=Join@@Parallelize[alignPanoCluster/@Select[filterNonGray/@tedAlbum,Length@#>1&]];(*rs//TableForm*)
Export["/g/tmp/pano_html/panos_ted.html",rsTed//TableForm]


rs=Join@@Parallelize[alignPanoCluster/@Select[filterNonGray/@baimingAlbum,Length@#>1&]];(*rs//TableForm*)
Export["/g/tmp/pano_html/panos_baiming.html",rs//TableForm]


alignPanoCluster@baimingAlbum[[21]];(*Good*)
alignPanoCluster@baimingAlbum[[18]];
alignPanoCluster@baimingAlbum[[17]];
alignPanoCluster@baimingAlbum[[14]]; (*Works in overcast!*)
alignPanoCluster@baimingAlbum[[10]];
alignPanoCluster@baimingAlbum[[9]];
alignPanoCluster@baimingAlbum[[7]];
alignPanoCluster@baimingAlbum[[6]];
alignPanoCluster@baimingAlbum[[5]];
alignPanoCluster@baimingAlbum[[4]];
alignPanoCluster@baimingAlbum[[2]];(*One bad.*)


alignPanoCluster@baimingAlbum[[20]];(*Bad, cloudy sky.*)
alignPanoCluster@baimingAlbum[[12]];(*Bad, trees.*)


alignPanoCluster@baimingAlbum[[19]]; (*Shiny object.*)


(*Bad data.*)alignPanoCluster@baimingAlbum[[16]];
alignPanoCluster@baimingAlbum[[15]];
alignPanoCluster@baimingAlbum[[8]];
alignPanoCluster@baimingAlbum[[3]];(*Quite differnt time, different place?*)
alignPanoCluster@baimingAlbum[[1]];


width=80;imgs=ColorConvert[#,"Gray"]&@ImageTake[#,{10,Round[ImageDimensions[#][[2]]/2]}]&@Import[StringReplace[#,"file://"->""]
	,ImageSize->{width}]&/@{"file:///h/panos/ted/PANO_20130812_195836.jpg","file:///h/panos/ted/PANO_20130812_200702.jpg"}
m=ImageData@ImageCorrelate[imgs[[1]],imgs[[2]],correlation];m//MatrixPlot
bestCorr=Max@Flatten@m
shift=N[(ImageDimensions[imgs[[1]]][[1]]/2-Position[m,bestCorr][[1,2]])/ImageDimensions[imgs[[1]]][[1]]]
imgs[[1]]
ImageTransformation[imgs[[2]],panoChangeAngle[#,shift]&]





img=ColorConvert[Import@"/g/tmp/flat_vietnam_pho.jpg","Gray"]
imgs=Import/@FileNames["/g/tmp/lychen_vietnam_pho.jpg-*.jpg"]


Export["/g/tmp/t.png",Rasterize@Parallelize@Table[matches=Transpose[randomSampleAtMostN[Transpose@imageCorrespondingPoints[img,image(*,"Transformation"->"Perspective"*)],300]];
	Print[matches//Dimensions];MapThread[annotateImageWithPoints,{{img,image},matches}],{image,imgs[[4;;4]]}]]


m=SparseArray[{Band[{1,1}]->2/3,Band[{2,1}]->1/6,Band[{1,2}]->1/6},{5,5}];m//MatrixForm
n=2;Plot[{f[1],f[2]},{x,-3,3}]
{Integrate[a f[1] b f[2],{x,-3,5}],Integrate[a f[1] b f[1],{x,-3,5}]}
SeedRandom[1003];as=RandomReal[1,5];f=Function[k,Max[0,1-Abs[x-k]^n]];g=Total@MapIndexed[# f[#2[[1]]]&,as]
Plot[g,{x,-2,7}]


(*cubeToLatitudeLongitude=Function[{xy,faceIndex},(*Converts a 2-length cube to LatLng*)
	(* faceIndex from 1 to 6, with last two being up and down, and 1 to 4 rotates around z-axis. xy is in Image coordinate*)
	]*)
panos=Import[#,ImageSize->{400}]&/@FileNames["/h/panos/*.jpg"];


panoToCubeFaces=Function[pano,Module[{w,cf,cf2,cf3},w=ImageDimensions[pano][[1]]/4;
	cf=Compile[{{xy,_Real,1}},With[{xyz=Normalize@{w/2,xy[[1]]-w/2,xy[[2]]-w/2}},{ArcTan[xyz[[2]]/xyz[[1]]]/Pi 2w+w/2,(Pi-ArcCos[xyz[[3]]/Norm[xyz]])/Pi 2w}]];
	cf2=Compile[{{xy,_Real,1}},With[{xyz=Normalize@{-(xy[[2]]-w/2),xy[[1]]-w/2,w/2}},{ArcTan[xyz[[1]],xyz[[2]]]/Pi 2w+2w,(Pi-ArcCos[xyz[[3]]/Norm[xyz]])/Pi 2w}]];
	cf3=Compile[{{xy,_Real,1}},With[{xyz=Normalize@{-(xy[[2]]-w/2),xy[[1]]-w/2,-w/2}},{ArcTan[xyz[[1]],xyz[[2]]]/Pi 2w+2w,(Pi-ArcCos[xyz[[3]]/Norm[xyz]])/Pi 2w}]];
	Join[Table[ImageTransformation[Image[RotateLeft[ImageData@pano,{0,i w,0}]],cf,PlotRange->{{1,w},{1,w}},DataRange->Full],{i,0,3}],
		ImageTransformation[Image[RotateLeft[ImageData@pano,{0,0,0}]],#,PlotRange->{{1,w},{1,w}},DataRange->Full]&/@{cf2,cf3}]]];
fuseImagesByHomography=Function[imgs,Module[{trs},
	trs=Parallelize[TransformationMatrix@Last@FindGeometricTransform[imgs[[1]],#,"Transformation"->"Affine"]&/@imgs[[2;;]]];
	Image@Mean[ImageData/@Prepend[Table[ImagePerspectiveTransformation[imgs[[i+1]],trs[[i]],DataRange->Full],{i,Length@trs}],imgs[[1]]]]]];


panos=Rest[Import[#(*,ImageSize->{400}*)]&/@FileNames["/h/d/R001079*_20140522114*.jpg"]]
panoFaces=Parallelize[panoToCubeFaces/@panos];
faces=fuseImagesByHomography/@Transpose[panoFaces]


img=Import["http://imgs.abduzeedo.com/files/articles/gorgeous-wideangle-photography/2754113949_15f9d2a2ae_z.jpg"];
LaplacianFilter[img,1]


(*imgs=Import/@FileNames["/g/tmp/cbk.jpg*.jpg"]
imgs2=Import/@FileNames["/g/tmp/cbk2.jpg*.jpg"]
matches=imageCorrespondingPoints[imgs[[4]],imgs2[[3]],"Transformation"->"Perspective"];
MapThread[annotateImageWithPoints,{{imgs[[4]],imgs2[[3]]},matches}]*)
homog=homographyFromMatches@matches;
anaglyph@{ImagePerspectiveTransformation[],imgs2[[3]]}


img=ImageResize[#,Scaled[3],Resampling->"Bicubic"]&@Import["/h/d/input_small.png"]
(*ImageDeconvolve[img,GaussianMatrix[1.5]]*)
(*TotalVariationFilter[img,Method->"Laplacian"]*)


Export["/tmp/t.jpg",TotalVariationFilter[Import["/h/panos/ted/R0010495.jpg"],"Method"->"Poisson"]]


rgb=ImageData/@ColorSeparate[ImageResize[ImageTake[Import["/h/Downloads/IMG_20140530_113456.jpg"],{1600,1800},{1800,2000}],Scaled[3]],"CMYK"];
ColorCombine[Image/@{1.5rgb[[1]],rgb[[2]],1.5rgb[[3]],rgb[[4]]},"CMYK"]


rgb=(*ImageResize[*)ColorConvert[Import["/h/infrared/rgb3.jpg"],"Gray"](*,{400}]*)
infra=(*ImageResize[*)ColorConvert[Import["/h/infrared/infrared3_950.jpg"],"Gray"](*,{400}]*)
{StandardDeviationFilter[rgb,1],StandardDeviationFilter[infra,1]}
{LaplacianFilter[rgb,1],LaplacianFilter[infra,1]}
(*matches=ImageCorrespondingPoints[rgb,infra,"Transformation"->"Affine"];homog=homographyFromMatches@matches;
anaglyph@{ImagePerspectiveTransformation[rgb,homog,DataRange->Full],infra}*)


n=3;SeedRandom[1003];Clear[r];rs=Array[r,n{1,1}];m=RandomReal[1,n{1,1}];svd=SingularValueDecomposition@m;qr=QRDecomposition@m;
MatrixForm/@Append[Join[qr,svd],m]
Reverse[SortBy[Table[{pnorm[#,3],MatrixForm@#}&[matrixExpSpecialOrthogonal[RandomReal[1,n{1,1}]].svd[[2]].Transpose[svd[[3]]]],{100000}],First]][[;;10]]


Clear[a];SeedRandom[1003];m=RandomReal[1,{3,1}];as=Array[a,3{1,1}];
{r=NMinimize[pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as];//AbsoluteTiming,r[[1]]}
{r2=FindMinimum[pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as];//AbsoluteTiming,r2[[1]]}
MatrixForm/@{matrixExpSpecialOrthogonal[as].m/.r[[2]],matrixExpSpecialOrthogonal[as].m/.r2[[2]]}


{#,Import[#,ImageSize->{300}],"GPSTimeStamp"/.Import[#,"Exif"]}&/@FileNames["/h/d/PANO*.jpg"]


case="May25";m=Import["/h/panos/"<>case<>".csv"][[;;,;;-2]];dict=MapIndexed[#->#2[[1]]&,Union@@m[[;;,2;;;;2]]];
weights=Normal@SparseArray[Join@@Table[Map[{i,#[[1]]/.dict}->#[[2]]&,Partition[m[[i,2;;]],2]],{i,Length@m}],{Length@m,Length@dict}];
wordWeightDict=Thread[dict[[;;,1]]->(Total/@Transpose@weights)];
dm=(*{"image"->Import[StringReplace[#[[1]],{"May25"->"May25_small","March25"->"March25_small"}]],"labels"->#[[2;;]]}*)
	MapIndexed[With[{ln=m[[#2[[1]]]]},With[{path=StringReplace[ln[[1]],{"May25"->"May25_small","March25"->"March25_small"}]},
		{"path"->path,"image"->Import[path,ImageSize->{400}],"labels"->ln[[2;;]],"vector"->#}]]&
		,Transpose[#/Total[#]^2&/@Transpose@weights],1];
(*dm//MatrixPlot*)
wordCloudFromTally=Function[tallyIn,Module[{tally,range,words,wordsimg,iteration},
	tally = Reverse@SortBy[tallyIn, Last];range = {Min@(Last /@ tally), Max@(Last /@ tally)};
	words = Style[First@#, FontFamily -> "Cracked", FontWeight -> Bold, 
		FontColor -> Hue[RandomReal[], RandomReal[{.5, 1}], RandomReal[{.5, 1}]], FontSize -> Last@Rescale[#, range, {12, 40}]] & /@ tally;
	wordsimg = ImageCrop[Image[Graphics[Text[#]]]] & /@ words;
	iteration[img1_, w_, fun_: (Norm[#1 - #2] &)] :=  Module[{imdil, centre, diff, dimw, padding, padded1, minpos},
		dimw = ImageDimensions[w];  padded1 = ImagePad[img1, {dimw[[1]] {1, 1}, dimw[[2]] {1, 1}}, 1];
		imdil = MaxFilter[Binarize[ColorNegate[padded1], 0.01], Reverse@Floor[dimw/2 + 2]];centre = ImageDimensions[padded1]/2;
		minpos = Reverse@Nearest[Position[Reverse[ImageData[imdil]], 0], Reverse[centre], DistanceFunction -> fun][[1]];
		diff = ImageDimensions[imdil] - dimw;  padding[pos_] := Transpose[{#, diff - #} &@Round[pos - dimw/2]];
		ImagePad[#, (-Min[#] {1, 1 }) & /@ BorderDimensions[#]] &@   ImageMultiply[padded1, ImagePad[w, padding[minpos], 1]]];
	Fold[iteration[##,Norm[{1, 1/2} (#2 - #1)] &]&, wordsimg[[1]], Rest[wordsimg]]]];


(*Table[{i,dm[[i]],{First@Rest@#,Last@#}&@SortBy[dm,CorrelationDistance["vector"/.dm[[i]],"vector"/.#]&]},{i,RandomSample[Range@Length@dm,10]}]*)
(*Export["/h/d/pano-ica-similarity.html",*)Table[Flatten@{"image"/.dm[[i]],"image"/.#&/@Rest[#][[;;5]](*,"image"/.Last@#*)}&@SortBy[dm,CorrelationDistance["vector"/.dm[[i]],"vector"/.#]&]
	,{i,RandomSample[Range@Length@dm,30](*Length@dm*)}]//TableForm(*];*)


(*Map["image"/.#&,*)clusters=FindClusters[Range@Length@dm,10,DistanceFunction->((1-#)&@correlation["vector"/.dm[[#]],"vector"/.dm[[#2]]]&)](*,{2}]//TableForm*);
rows=Map["image"/.dm[[#]]&,clusters,{2}];


Export["/h/d/pano-clusters.html",SortBy[Parallelize@Table[Flatten@{wordCloudFromTally@takeAtMostN[
	Reverse@SortBy[{#[[1]],#[[2]]/(#[[1]]/.wordWeightDict)}&/@Tally@Flatten[("labels"/.#)[[;;;;2]]&/@dm[[clusters[[i]]]]],Last],10],rows[[i]]},{i,Length@rows}],Length]//TableForm]


Export["/h/d/pano-tags.html",Parallelize@Table[Flatten@{Rasterize@tag,"image"/.#&/@Reverse[SortBy[Select[dm,MemberQ["labels"/.#,tag]&],("vector"/.#)[[tag/.dict]]&]]},{tag,tags[[;;]]}]//TableForm]


tags=Sort@dict[[;;,1]];


panoramioTags=First/@(Import@"/h/panos/panoramio-tags.csv")


Export["/tmp/May25.jpg",ImageAssemble@Partition[Import[#,ImageSize->{200,100}]&/@FileNames["/h/panos/May25_small/*.jpg"][[;;]],10]]


Import["http://apius.faceplusplus.com/v2/detection/detect?api_key=DEMO_KEY&api_secret=DEMO_SECRET&url=http%3A%2F%2Fwww.faceplusplus.com%2Fwp-content%2Fthemes%2Ffaceplusplus%2Fassets%2Fimg%2Fdemo%2F1.jpg%3Fv%3D2&attribute=age%2Cgender%2Crace%2Csmiling%2Cpose%2Cglass"]



