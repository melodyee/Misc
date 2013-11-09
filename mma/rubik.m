rotateByQuaternion=Compile[{{v,_Real,1},{q,_Real,1}},
	(*Quat2Matrix[#].{0,1,0} == RotateByQuaternion[{0,1,0},#]*)
	{q[[3]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])-q[[4]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[1]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[2]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	-q[[2]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[1]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[4]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[3]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	q[[1]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[2]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])-q[[3]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[4]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]])}
	/ (Norm[q]^2)
	];
	
SeedRandom[1003];Clear[cubeS,dispCube,rotateCube];
dispCube[cubeS[colors_,positions_]]:=Riffle[colors,Polygon/@(positions/2)]
rotateCube[cubeS[colors_,positions_],axis_,step_]:=Module[{q},(*Print[Mean[Join@@positions],axis,step];*)
	q=Prepend[#,Sqrt[1-Total[#^2]]]&@(Sin[step Pi/4]axis);
	cubeS[colors,Round@Map[rotateByQuaternion[#,q]&,positions,{2}]]]
dispRubik=Function[rubik,Graphics3D[dispCube/@Flatten@rubik,Lighting->"Neutral",Axes->True,AxesLabel->{"x","y","z"}]];
getPositions[cubeS[colors_,positions_]]:=positions;
sortRubik=Function[rubik,Partition[#,3]&/@Partition[SortBy[Flatten@rubik,Mean[Join@@getPositions[#]]&],9]];
rotateRubik=Function[{rubik,q,step},Module[{moving=With[{pos=Position[q,x_/;Abs[x]>0.1][[1,1]]},Join@@Table[Insert[{i,j},q[[pos]]+2,pos],{i,3},{j,3}]]},
	sortRubik@Table[If[MemberQ[moving,{x,y,z}],rotateCube[#,Abs@q,step],#]&@rubik[[x,y,z]],{x,3},{y,3},{z,3}]]];
colors={Blue,Red,Darker@Yellow,White,Pink,Purple};positions=Flatten[Table[Table[Insert[#,z,pos]&/@{{-1,-1},{-1,1},{1,1},{1,-1}},{z,{-1,1}}],{pos,3}],1];
cube=cubeS[colors,positions];
rubik=Table[cubeS[colors,Map[#+{x,y,z}&,positions,{2}]],{x,-2,2,2},{y,-2,2,2},{z,-2,2,2}];(*dispRubik@rotateRubik[rubik,{0,1,0},1]*)
randomRotateRubik=Function[rubik,rotateRubik[rubik,RandomChoice[{-1,1}] Insert[{0,0},1,RandomInteger[{1,3}]],RandomChoice@{-1,1}]];
dispRubik/@NestList[randomRotateRubik,rubik,3]
