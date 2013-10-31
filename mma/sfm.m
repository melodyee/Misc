(*imgs=ColorConvert[Import[#,ImageSize->400{1,1}],"Grayscale"]&/@FileNames["/s/saloon/*.jpg"][[-2;;]];*)
(*imgs=Import/@{"/s/gdesk/image-013.jpg","/s/gdesk/image-014.jpg"}*)
(*imgs=ColorSeparate[Import[#,ImageSize->400{1,1}],"HSB"][[1]]&/@{"t2.jpg","t7.jpg"};*)
colorImgs=Import[#,ImageSize->400{1,1}]&/@{
	(*"~/m/rvctools/vision/images/building2-1.png","~/m/rvctools/vision/images/building2-2.png"*)
	"~/m/rvctools/vision/images/eiffel2-1.jpg","~/m/rvctools/vision/images/eiffel2-2.jpg"
	(*"~/m/vgg_vision/vgg_examples/chapel00.png","~/m/vgg_vision/vgg_examples/chapel01.png"*)
	(*"t2.jpg","t7.jpg"*)(*,"t8.jpg"*)(*"t.jpg","t3.jpg"*)(*"t2.jpg","t4.jpg"*)(*"t9.jpg","t10.jpg"*)
	(*"/s/kejian_front/image-056.jpg","/s/kejian_front/image-057.jpg"*)(*"~/doc/a.png","~/doc/b.png"*)
	(*"/h/d/workspace/tcv/church01.jpg","/h/d/workspace/tcv/church03.jpg"*)};
imgs=ColorConvert[#,"Grayscale"]&/@colorImgs;
annotateImageWithPoints=Function[{img,points},Show[img,Graphics[{Yellow,MapIndexed[Inset[#2[[1]],#1]& ,points]}]]];
annotateImageWithLines=Function[{img,lines,scale},
	Show[img,Graphics[{Yellow,MapIndexed[{Inset[#2[[1]],#1[[1]]],Line[{#[[1]],#[[1]]+(#[[2]]-#[[1]])scale}]}& ,lines]}]]];
{i1,i2(*,i3*)}=imgs;matches=ImageCorrespondingPoints[colorImgs[[1]],colorImgs[[2]],"Transformation"->"EpiPolar"(*"Perspective"*)];
Length@matches[[1]]
(*Import@Export["t.png",#]&@Rasterize@Join[Table[
points = ImageKeypoints[img, {"Position", "Scale", "Orientation", "ContrastSign"},MaxFeatures -> 100];
Show[img,
 Graphics[Table[{{If[p[[4]] == 1, Yellow, Red], 
     Circle[p[[1]], p[[2]]*2.5], 
     Line[{p[[1]], p[[1]] + p[[2]]*2.5*{Cos[p[[3]]], Sin[p[[3]]]}}]}}, {p, 
    points}]]],{img,imgs}],
	MapThread[annotateImageWithPoints,{{i1,i2},matches}]]*)
MapThread[annotateImageWithPoints,{{i1,i2},matches}]
(*Magnify[#,0.5]&@*)annotateImageWithLines[i1,Transpose@matches,3]
(*keypoints=ImageKeypoints[#,{"PixelPosition","Descriptor"}]&/@imgs;
nfs=Nearest[(Rule@@@#)]&/@keypoints;
Import@Export["t2.png",ListLinePlot[First/@{nfs[[1]][matches[[1,1]]],nfs[[1]][matches[[1,2]]],nfs[[2]][matches[[2,1]]]}]]*)
(*tr=FindGeometricTransform[i1,i2(*,"Transformation"->"Perspective",Method->"RANSAC"*)];
{w,h}=ImageDimensions[i2];
tmp=ImagePerspectiveTransformation[i2,tr[[2]],DataRange->Full,PlotRange->{{0,First@tr[[2]][{w,0}]},{0,h}}];
ImageCompose[tmp,{i1,.5},Round@({w,h}/2)]*)
homographyFromMatches=Function[matches,With[{s=Max@Flatten@matches},
	With[{mat=Join@@MapThread[KroneckerProduct[{Append[#,s]},skewOmega@Append[#2,s]]&,matches]},
	#/#[[-1,-1]]&@(# {{1,1,s},{1,1,s},{1/s,1/s,1}})&@Transpose@Partition[SingularValueDecomposition[mat,9][[3,;;,-1]],3]]]];
homographyFromMatchesL1=Function[matches,Module[{s,h,hs,mat,r},
	s=Max@Flatten@matches;mat=Join@@MapThread[KroneckerProduct[{Append[#,s]},skewOmega@Append[#2,s]]&,matches];
	hs=Array[h,9];r=NMinimize[{pnorm[mat.hs,1],pnorm[hs,2]==1},hs];
	#/#[[-1,-1]]&@(# {{1,1,s},{1,1,s},{1/s,1/s,1}})&@Transpose@Partition[#/#[[-1]]&[hs/.r[[2]]],3]]];
homograhyTransform=Function[{pixelPosition,homography},
	#[[;;2]]/#[[3]]&[homography.If[Length@pixelPosition==2,Append[pixelPosition,1],pixelPosition]]];
hg=homographyFromMatchesL1@matches;hg//MatrixForm
hg2=homographyFromMatches@matches;
Function[x,annotateImageWithLines[imgs[[2]],Transpose@{homograhyTransform[#,x]&/@matches[[1]],matches[[2]]},5]]/@{hg,hg2}
img=imgs[[1]];dim=Dimensions@ImageData@img;
With[{hg=hg},Manipulate[Magnify[#,400/dim[[1]]]&/@{Show[img,Graphics@{Yellow,Point[rowColumnToXy[i,j,dim[[1]]]]}]
	,Show[imgs[[2]],Graphics@{Yellow,Arrow[{homograhyTransform[rowColumnToXy[i,j,dim[[1]]],hg]
			,homograhyTransform[Append[rowColumnToXy[i,j,dim[[1]]],1.2],hg]}]}]}
	,{{i,301},1,dim[[1]]},{{j,309},1,dim[[2]]}]]
With[{hg=hg},Show[imgs[[2]],Graphics@Table[{Yellow,Arrow[{homograhyTransform[rowColumnToXy[i,j,dim[[1]]],hg]
			,homograhyTransform[Append[rowColumnToXy[i,j,dim[[1]]],1.2],hg]}]},{i,1,dim[[1]],40},{j,1,dim[[2]],40}]]]
drawEpipolarLine=Function[{img,lineNormal},(*lineNormal dot {x,y,1} = 0*)Module[{dim=Dimensions@ImageData@img},
	Line@If[lineNormal[[2]]==0
		,{{-lineNormal[[3]]/lineNormal[[1]],1},{-lineNormal[[3]]/lineNormal[[1]],dim[[1]]}}
		,Table[{x,-(x lineNormal[[1]]+lineNormal[[3]])/lineNormal[[2]]},{x,{1,dim[[2]]}}]
	]]];
With[{fm=fm},Manipulate[Magnify[#,400/dim[[1]]]&/@{Show[img,Graphics@{Yellow,Point[rowColumnToXy[i,j,dim[[1]]]]}]
	,Show[imgs[[2]],Graphics@{Yellow,drawEpipolarLine[imgs[[2]],fm.Append[{i,j},1]]}]}
	,{{i,301},1,dim[[1]]},{{j,309},1,dim[[2]]}]]
With[{fm=fm},Magnify[#,400/dim[[1]]]&@
	Show[imgs[[2]],Graphics[{Yellow}~Join~Table[drawEpipolarLine[imgs[[2]],fm.Append[{i,j},1]]
	,{i,1,dim[[1]],80},{j,1,dim[[2]],80}]]]]
