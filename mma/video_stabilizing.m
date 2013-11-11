imgs=(*ColorConvert[Import@#,"Graylevel"]&*)Import/@FileNames["/s/tmp/line_dance2/2*.jpg"];
img=First@imgs;
Import@Export["t.png",Rasterize[(tr=FindGeometricTransform[img,#];ImagePerspectiveTransformation[#,tr[[2]],DataRange->Full])&/@Rest@imgs]]
trs=FindGeometricTransform@@@Partition[imgs,2,1];
trs2=FindGeometricTransform@@@Partition[imgs,2,1];
seq2=MapThread[ImagePerspectiveTransformation[#2,#,DataRange->Full]&,{FoldList[#2.#&,IdentityMatrix@3,TransformationMatrix/@trs2[[;;,2]]],imgs}];
Import@Export["t2.png",Rasterize[seq2]];
Import@Export["t3.png",Mean[ImageData/@seq2]//Image]


(*Blocking approach, need keep killing/starting adb.*)
(*qInit=q={1,0,0,0};doInit=True;imgs={};qs={};
(*Dynamic@Graphics3D[{Opacity[0.3],Cuboid[{-1/2,-1/2,-1/2}],Opacity[1]\

	,Arrow[Tube@{{0,0,0},rotateByQuaternion[{0,0,0.5},q]}],Arrow[Tube@{{\
0,0,0},rotateByQuaternion[{0,0.5,0},q]}]}]*)
Run["/usr/local/bin/adb \
logcat -c"]
str=OpenRead["!/usr/local/bin/adb logcat -s ROTV:I"];cnt=0;
While[cnt<1000,
cnt+=1;
s=Read[str,Record];(*Print[s]*)
	If[And@@(NumericQ/@#),
		q=#;If[doInit,doInit=False;qInit=q];If[Mod[cnt,10]==0,AppendTo[imgs,\
CurrentImage[]];AppendTo[qs,q]];
	]&@ImportString[s,"CSV"][[1,-4;;]];
]
Run["killall -9 adb"]
Close[str]*)
