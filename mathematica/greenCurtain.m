imagePadToImageDim=Function[{img,imgDim,padding},Module[{cur=ImageDimensions@img,left,bottom},
	left=Round[(imgDim[[1]]-cur[[1]])/2];bottom=Round[(imgDim[[2]]-cur[[2]])/2];
	ImagePad[img,{{left,imgDim[[1]]-cur[[1]]-left},{bottom,imgDim[[2]]-cur[[2]]-bottom}},Padding->padding]]];
imgs=ImageRotate[#,90Degree]&/@Import["/home/zsc/w/green_curtain/image-*.jpg"];
(*Image@Map[Boole[Abs[#+0.23]<0.15]&,ImageData@ColorSeparate[#,"LUV"][[3]],{2}]&/@imgs[[;;;;10]]*)
(*Image@Map[Boole[Abs[#-0.25]<0.15]&,ImageData@ColorSeparate[#,"LUV"][[2]],{2}]&/@imgs[[;;;;10]]*)
masks2=Map[Boole[Abs[#+0.23]<0.15]&,ImageData@ColorSeparate[#,"LUV"][[3]],{2}] Map[Boole[Abs[#-0.25]<0.15]&,ImageData@ColorSeparate[#,"LUV"][[2]],{2}]&/@imgs;
masks3=Closing[Opening[Closing[#,3],5],5]&/@masks2;
imgs3=ImageResize[#,{ImageDimensions[imgs[[1]]][[2]]}]&@Image[#,"Byte"]&/@Import["/h/w/green_curtain/Rotating_earth_(huge).gif"];
imgs4=Take[Flatten@Table[imgs3,{Ceiling[Length@imgs/Length@imgs3]}],Length@imgs];
imgs2=Parallelize@MapThread[Image[255ImageData@ImageAdd[imagePadToImageDim[ImageMultiply[Image[1-#2],#],ImageDimensions@#3,0]
	,Image@ImageData[ImageMultiply[imagePadToImageDim[Image@#2,ImageDimensions@#3,1],#3]][[;;,;;,;;3]]],"Byte"]&,{imgs,masks3,imgs4}];

(*imgs3=ImageResize[#,ImageDimensions@imgs2[[1]]]&/@Import@"/h/d/waterfall2.gif";
imgs4=Take[Flatten@Table[imgs3,{Ceiling[Length@imgs/Length@imgs3]}],Length@imgs];
imgs2=Parallelize@MapThread[Image[255ImageData@ImageAdd[ImageMultiply[Image[1-#2],#],ImageMultiply[Image@#2,#3]],"Byte"]&,{imgs,masks3,imgs4}];*)

(*Histogram[ImageData[ColorSeparate[#,"LUV"][[2]]][[5,5]]&/@imgs]
Histogram[ImageData[ColorSeparate[#,"LUV"][[3]]][[5,5]]&/@imgs]*)
