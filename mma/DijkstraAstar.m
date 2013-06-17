<< "PriorityQueue.m"
Needs["PriorityQueue`"];
dijkstra=Function[{src,dest,distF,expandF},Module[{dist,previous,queue=MakeQueue[#[[1]]>#2[[1]]&],u,enqueue,dequeue},
	enqueue=EnQueue[queue,{-dist[#],#}]&;
	dequeue=DeQueue[queue][[2]]&;
	dist[src]=0;enqueue[src];previous[src]=src;
	While[Not@EmptyQueue@queue,
		u=dequeue[];(*Print@u;*)
		If[u===dest,Break[]];
		Function[v,
			With[{alt=dist[u]+distF[u,v]},
				If[Head[dist[v]]===dist||alt<dist[v],
					dist[v]=alt;previous[v]=u;enqueue[v]
				]
			]]/@expandF[u];
	];
	If[u===dest,Reverse@Most@FixedPointList[previous[#]&,dest]]
	]];

astar=Function[{src,dest,distF,expandF,heuristicCostF},
	Module[{closed,queue=MakeQueue[#[[1]]>#2[[1]]&],previous,gScore,fScore,enqueue,dequeue,u,cnt=0},
	enqueue=EnQueue[queue,{-fScore[#],#}]&;
	dequeue=DeQueue[queue][[2]]&;
	gScore[src]=0;fScore[src]=gScore[src]+heuristicCostF[src,dest];enqueue[src];previous[src]=src;
	While[Not@EmptyQueue@queue,
		u=dequeue[];
		If[u===dest,Break[]];
		closed[u]=0.;
		Function[v,
			With[{altG=gScore[u]+distF[u,v]},
				If[Head[closed[v]]===closed&&(Head[gScore[v]]===gScore||altG<gScore[v]),
					gScore[v]=altG;previous[v]=u;fScore[v]=gScore[v]+heuristicCostF[v,dest];enqueue[v]
				]]]/@expandF@u;
	];
	If[u===dest,Reverse@Most@FixedPointList[previous[#]&,dest]]
	]];

weights={{1,2}->7,{1,3}->9,{1,6}->14,{2,3}->10,{2,4}->15,{3,4}->11,{3,6}->2,{4,5}->6,{5,6}->9};
fromWeights=Function[weights,Module[{expandF,distF,g,neighbors},
	distF=Function[{u,v},If[NumberQ[#],#,{v,u}/.weights]&[{u,v}/.weights]];
	g=Graph[weights[[;;,1]]];
	neighbors=(#->Complement[VertexList@NeighborhoodGraph[g,#],{#}])&/@VertexList[g];
	expandF=Function[v,v/.neighbors];
	{distF,expandF}]];
{distF,expandF}=fromWeights@weights;
dijkstra[1,5,distF,expandF]
heuristicCostF=0&;
astar[1,5,distF,expandF,heuristicCostF]

(*Eight digit problem, some from Acrush, http://www.cppblog.com/abilitytao/archive/2009/05/22/85139.html *)
disp=(Partition[#,3]//MatrixForm)&;
dest={1,2,3,8,0,4,7,6,5};
src=Nest[RandomChoice[expandF@#]&,dest,1000];
disp/@{src,dest}
arcT=MapIndexed[#->#2[[1]]&,dest];
dist=Table[Abs[Floor[u/3]-Floor[v/3]]+Abs[Mod[u,3]-Mod[v,3]],{u,0,8},{v,0,8}];
countP=If[Head@#===List,Length@First,Length]&@PermutationCycles[src/.arcT][[1]];
{b1,b2}=Position[#,0][[1,1]]&/@{src,dest};
noAnswer=Mod[countP-dist[[b1,b2]],2]!=0 (*incorrect yet*)
expandF=Function[x,With[{n=Sqrt@Length@x,zeroPos=Position[x,0][[1,1]]},Module[{positions},
	positions=#[[1]]+3#[[2]]+1&/@Select[{{#[[1]]+1,#[[2]]},{#[[1]]-1,#[[2]]},{#[[1]],#[[2]]-1},{#[[1]],#[[2]]+1}}&@
			{Mod[#,n],Floor[#/3]}&[zeroPos-1],
		And@@Thread[0<=#<{n,n}]&];
	ReplacePart[x,{zeroPos->x[[#]],#->0}]&/@positions
	]]];
distF=1&;
heuristicCostF=Function[{u,destUnused},Total@MapIndexed[dist[[#2[[1]],#]]&,u/.arcT]];
disp/@astar[src,dest,distF,expandF,heuristicCostF]
