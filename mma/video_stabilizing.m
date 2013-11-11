imgs=(*ColorConvert[Import@#,"Graylevel"]&*)Import/@FileNames["/s/tmp/line_dance2/2*.jpg"];
img=First@imgs;
Import@Export["t.png",Rasterize[(tr=FindGeometricTransform[img,#];ImagePerspectiveTransformation[#,tr[[2]],DataRange->Full])&/@Rest@imgs]]
trs=FindGeometricTransform@@@Partition[imgs,2,1];
trs2=FindGeometricTransform@@@Partition[imgs,2,1];
seq2=MapThread[ImagePerspectiveTransformation[#2,#,DataRange->Full]&,{FoldList[#2.#&,IdentityMatrix@3,TransformationMatrix/@trs2[[;;,2]]],imgs}];
Import@Export["t2.png",Rasterize[seq2]];
Import@Export["t3.png",Mean[ImageData/@seq2]//Image]
