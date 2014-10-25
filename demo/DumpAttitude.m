<<"~/gdrive/mac_home/t3.m"
Clear[displayPointCloudWithLabels];
displayPointCloudWithLabels[pts_List,labels_]:=Module[{minmax={Min@#,Max@#}&@pts[[;;,3]],tpts},tpts=Transpose[pts[[;;,;;3]]];
	Graphics[MapThread[Inset[Style[#,Hue[#3],FontSize->30],#2,{0,0},100]&,{labels,Developer`ToPackedArray@Transpose[tpts[[;;2]]],0.7((tpts[[3]]-minmax[[1]])/(minmax[[2]]-minmax[[1]]))}],ImageSize->1000]]
(*displayPointCloudWithLabels[pts_List,labels_]:=Module[{minmax={Min@#,Max@#}&@pts[[;;,3]]},
	Graphics[MapThread[Inset[Style[#,Hue[0.7 (#2[[3]]-minmax[[1]])/(minmax[[2]]-minmax[[1]])],FontSize->30],#2[[;;2]],{0,0},100]&,{labels,pts}],ImageSize->1500]];*)

trainFeatures=Developer`ToPackedArray@Import["/mnt/202/d/word2vec-read-only/matrix.csv"];
vocab=First/@Import["/mnt/202/d/word2vec-read-only/vocab2.csv",CharacterEncoding -> "Unicode"];

SeedRandom[1004];X=Standardize[trainFeatures,Mean,1&];svd=SingularValueDecomposition[X,5];V=svd[[3]];
projected=100Standardize[trainFeatures.V];
(*displayPointCloudWithLabels[projected[[indices]],vocab[[indices]]]*)

indices=Range@200(*Length@trainFeatures*);
{pts,labels}={Developer`ToPackedArray[projected[[indices]]],vocab[[indices]]};
Dynamic@(s=StringReplace[#,{"["->"{","]"->"}",":"->","}]&@Import["http://192.168.2.166:1337/"];idqs=Append[#,Sqrt[1-Total[#[[2;;]]^2]]]&@ToExpression@#&/@Partition[Flatten@ToExpression@s,4];
q=SortBy[idqs,First][[1,2;;]];(*Refresh[*)displayPointCloudWithLabels[rotateByQuaternion[#,q]&/@pts[[;;,;;3]],labels](*,UpdateInterval->1]*))
