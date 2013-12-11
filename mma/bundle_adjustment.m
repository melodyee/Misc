problem=ImportString[StringReplace[StringReplace[Import["~/d/problem-49-7776-pre.txt"],"  "->""]," "->","],"CSV"];
{numCameras,numPoints,numObservations}=problem[[1]];
projections=problem[[2;;numObservations+1]];
cameras=Partition[problem[[-9numCameras-3numPoints;;-3numPoints-1,1]],9];points=Partition[problem[[-3numPoints;;,1]],3];
Clear[cam,pt];cams=Array[cam,{numCameras,9}];pts=Array[pt,{numPoints,3}];
(*First/@problem[[numObservations+2;;]]*)
snavelyReprojectionError=Function[{cam,point,observed},Module[{xy,distortion},
	xy=-#[[;;2]]/#[[3]]&@(cam[[4;;6]]+rotateByQuaternionSlow[point,axisAngleToQuaternion[cam[[;;3]]]]);
	distortion=1+xy.xy(cam[[8]]+cam[[9]] xy.xy);
	xy distortion cam[[7]]-observed
	]];
(*With[{camera=cameras[[#[[1]]+1]],point=points[[#[[2]]+1]]},snavelyReprojectionError[camera,point,#[[3;;4]]]]&/@projections[[;;1]]*)
f[cams_?(NumericQ@#[[1,1]]&),pts_]:=Total[#.#&@With[{camera=cams[[#[[1]]+1]],point=pts[[#[[2]]+1]]},snavelyReprojectionError[camera,point,#[[3;;4]]]]&/@projections[[;;]]]
FindMinimum[f[cams,pts],variableWithInital[Flatten@{cams,pts},Flatten@{cameras,points}]]
