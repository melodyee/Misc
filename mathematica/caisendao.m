(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"


img=ColorConvert[Import["t4.jpg",ImageSize->400{1,1}],"Gray"];
l=ImageAdjust/@SortBy[Join@@ImagePartition[img,30],schattenNorm[ImageData@#,1]&]
(*schattenNorm[ImageData@#,1]&/@(Join@@ImagePartition[img,30])*)


Table[qidx=RandomInteger[{1,Length@allImgs}];
{allImgs[[qidx]],Take[#,3]&@Reverse@SortBy[
	Parallelize[{{HistogramTransform[#,allImgs[[qidx]]],Length@First@imageCorrespondingPoints[allImgs[[qidx]],#,"Transformation"->"Homography"]}
		,Length@First@imageCorrespondingPoints[allImgs[[qidx]],HistogramTransform[#,allImgs[[qidx]]],"Transformation"->"Homography"]}&/@allImgs[[Delete[Range@Length@allImgs,qidx]]]],Last]}
,{10}]


Table[qidx=RandomInteger[{1,Length@allImgs}];
{allImgs[[qidx]],Take[#,3]&@Reverse@SortBy[
	Parallelize[{#,Length@First@imageCorrespondingPoints[allImgs[[qidx]],#,"Transformation"->"Homography"]}&/@allImgs[[Delete[Range@Length@allImgs,qidx]]]],Last]}
,{10}]


(*allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*","/h/d/caishendao/biz_india/",2],Not@DirectoryQ@#&];
Export["/h/mxs/india.mx",{allImgs}]*)
(*allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*","/h/d/caishendao/biz_china/",Infinity],Not@DirectoryQ@#&];
Export["/h/mxs/china.mx",{allImgs}]*)


allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*.jpg","/h/d/caishendao/biz_mexico/",Infinity],Not@DirectoryQ@#&];
Export["/h/mxs/mexico.mx",{allImgs}]


allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*.jpg","/h/d/caishendao/biz_mexico/",Infinity],Not@DirectoryQ@#&];
Export["/h/mxs/mexico.mx",{allImgs}]


MapIndexed[{#2[[1]],#}&,allImgs]


imgs=allImgs[[{2,90}]];
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Homography"];
homog=homographyFromMatchesL1@matches;{"#matches",Length@matches[[1]]}
MapThread[annotateImageWithPoints,{imgs[[;;2]],matches}]
anaglyph@{ImagePerspectiveTransformation[annotateImageWithPoints[imgs[[1]],matches[[1]]],homog,DataRange->Full]
	,annotateImageWithPoints[imgs[[2]],matches[[2]]]}


(*allImgs=(*ColorConvert[#,"Gray"]&/@*)Import["/h/d/z*.jpeg",ImageSize->{400}];*)
(*allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*","/h/d/caishendao/biz_india/",2],Not@DirectoryQ@#&];*)
(*allImgs=Import[#,ImageSize->{400}]&/@Select[FileNames["*","/h/d/caishendao/biz_brazil/",2],Not@DirectoryQ@#&];*)
{allImgs}=Import@"/h/mxs/china.mx";
allImgs//Dimensions
imgs=(*{HistogramTransform[#[[1]],#[[2]]],#[[2]]}&@*)allImgs[[{55,56}]];
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
homog=homographyFromMatchesL1@matches;{"#matches",Length@matches[[1]]}
MapThread[annotateImageWithPoints,{imgs[[;;2]],matches}]
anaglyph@{ImagePerspectiveTransformation[annotateImageWithPoints[imgs[[1]],matches[[1]]],homog,DataRange->Full]
	,annotateImageWithPoints[imgs[[2]],matches[[2]]]}


imgs=Import[#,ImageSize->400]&/@
	{"/h/d/caishendao/biz_china/n4/IMG_20140303_133208.jpg","/h/d/caishendao/biz_china/gn/IMG_20140303_133211.jpg"}
	(*{"/h/d/caishendao/biz_china/pn_2/IMG_20140304_064504.jpg","/h/d/caishendao/biz_china/gn_2/IMG_20140304_144510.jpg"}*)
	(*{"/h/d/caishendao/biz_china/n4/IMG_20140303_131810.jpg","/h/d/caishendao/biz_china/n4/IMG_20140303_131916.jpg"};*)
(*ColorConvert[#,"Gray"]&/@imgs*)
ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]](*,"Transformation"->"Perspective"*)];
MapThread[annotateImageWithPoints,{imgs,Transpose@randomSampleAtMostN[Transpose@matches,100]}]


buffer=Array[0&,{180,320}];
getDepthmapAdb=Function[{},Module[{lines,numRow,row},
	lines=Import["!/usr/local/bin/adb logcat -t 500 -d DEPTH:I *:S|grep -v beginning","Lines"];
	Do[{numRow,row}={#[[1]],#[[2;;-2]]}&@ImportString[StringSplit[line,":"][[-1]],"CSV"][[1]];
		(*Print[numRow];*)
		buffer[[numRow + 1]]=row;
	,{line,lines}]
	(*If[lines=!={},
		ImportString[StringSplit[#,": "][[-1]],"CSV"][[1]]&/@lines]*)]];
(*Dynamic@{DateString[],getDepthmapAdb[]}*)


Run@"/usr/local/bin/adb logcat -c";
Dynamic[Refresh[{DateString[],getDepthmapAdb[]},UpdateInterval->1]]


Dynamic[Refresh[getDepthmapAdb[];buffer//Image,UpdateInterval->0.3]]i


(*lines=*)getDepthmapAdb[]


imgs=Import["/s/peanut/IMG*"]


radiusDistort=Function[{w,ikm},Function[xy,Module[{xyd,rd,ru},xyd=#[[;;-2]]/#[[-1]]&[ikm.Append[xy,1]];rd=Norm@xyd;ru=Tan[rd w]/(2 Tan[w/2]);
	#[[;;-2]]/#[[-1]]&[Inverse[ikm].Append[ru/rd xyd,1]]]]];
peanutUndistortBackWideAngle=Function[wimg,Module[{ikm,w,rdF},ikm=Inverse@{{255.595,0,312.693},{0,255.482,241.336},{0,0,1}};w=0.916127;
	rdF=radiusDistort[w,ikm];ImageForwardTransformation[wimg,rdF,DataRange->Full,PlotRange->All]]];
peanutParseColorDepth=Function[img,Module[{img2,img3,cimg,dimg,wimg,m},m=ImageData@img;
	img2=Image@Partition[Flatten@m[[273-55+1;;273,;;,1]],640];dimg=ImageResize[img2,(*ImageDimensions[img2]{3/8,4/3}*){960,588}];
	img3=Image@m[[281;;]];cimg=ImageResize[img3,(*ImageDimensions[img]{3/4,4/3}*){960,588}];
	wimg=ImageResize[#,{640,480}]&@Image[Partition[Flatten@m[[11;;157,;;,1]],640]];
	{cimg,dimg,wimg}]];
{cimg,dimg,wimg}=peanutParseColorDepth[imgs[[1]]]


wimgs=Reverse@Import["/s/peanut/IMG_20140307_08*.jpg",ImageSize->{640,480}];
imgs=peanutUndistortBackWideAngle/@wimgs;
(*Table[Export["/g/pair/"<>IntegerString[i]<>".jpg",peanutUndistortBackWideAngle[wimgs[[i]]]],{i,Length@wimgs}]*)


(*jointImg=Image@ArrayFlatten@{ImageData@ColorConvert[#,"Gray"]&/@imgs};*)
n=250;jointImg=ImageAdjust@Image@ArrayFlatten@{ImageData[ColorConvert[#,"Gray"]][[;;,n;;1148-n]]&/@imgs};
Manipulate[With[{r=3},
ParametricPlot3D[{r Cos@t,r Sin@t,z},{t,0,2Pi},{z,-1,1},PlotStyle->{Opacity[1],Texture[jointImg]},Mesh->None,PlotRange->All,Lighting->"Neutral"
	(*,ViewPoint->{0,0.1,0}*),ImageSize->600(*,ViewRange->{3,1000}*),ViewVector->{{0,0.,0},{0.1Sin[\[Theta]],0.1Cos[\[Theta]],0}}]]
,{\[Theta],-2Pi,2Pi,0.001}]
(*Manipulate[
Rotate[g,\[Theta],{0,1,0},{0,0,0}]
,{\[Theta],-2Pi,2Pi}]*)


Import@"/s/peanut/calibration.xml"


matrixExpUnitLowerTriangular=Function[as,Re@MatrixExp@strictLowerTriangle@as];

SeedRandom[1003];Clear[a];n=2;ass=Array[a,{2,n,n}];m=RandomReal[1,n{1,1}];
{r=NMinimize[pnorm[matrixExpUnitLowerTriangular[ass[[1]]].m.matrixExpUnitLowerTriangular[ass[[2]]],1],Flatten@ass];//AbsoluteTiming,r[[1]]}
MatrixForm[matrixExpUnitLowerTriangular[ass[[1]]].m.matrixExpUnitLowerTriangular[ass[[2]]]/.r[[2]]]
{pnorm[#,1],#}&/@{Eigenvalues@m,SingularValueList@m}


SeedRandom[1003];m=RandomReal[1,3{1,1}];
ult=matrixExpUnitLowerTriangular[RandomReal[1,3{1,1}]];uut=Transpose@Inverse[ult];
MatrixForm/@{ult,m,uut,ult.m.uut}
{pnorm[Eigenvalues@m,1],pnorm[ult.m.uut,1],pnorm[m,1]}



