canAgree=#===#2||#===0||#2===0||#==="*"||#2==="*"&;
canAgreeNeighbors=Function[{board,pos,cur},Module[{dim=Dimensions@board,neighborVals},
	neighborVals=board[[Sequence@@#]]&/@Select[pos+#&/@{{-1,0},{1,0},{0,1},{0,-1}},And@@Thread[#<=dim]&&And@@Thread[#>=1]&];
	(neighborVals=!=0 neighborVals)&&(And@@(canAgree[cur,#]&/@neighborVals))
	]];
reduce=Function[l,With[{r=Fold[If[#=!=Null&&#=!=0&&#2=!=0&&(#===#2||#==="*"||#2==="*"),#2]&,l[[1]],l[[2;;]]]},r=!=Null]];
clearLine=Function[{backgroundIn,boardIn,pos,scoreIn},Module[{dim=Dimensions@backgroundIn,background=backgroundIn,board=boardIn,score=scoreIn,row,col,positions},
	row={pos[[1]],#}&/@Range@dim[[2]];col={#,pos[[2]]}&/@Range@dim[[1]];
	positions=Select[{row~Join~col,row, col},reduce[board[[#[[1]],#[[2]]]]&/@#]&];
	If[positions=!={},
		score+=Length@Union@First[positions]-1;
		(background[[#[[1]],#[[2]]]]=1)&/@First[positions];
		(board[[#[[1]],#[[2]]]]=0)&/@First[positions]];
	{background,board,score}]];
(*m={{1,1,1},{0,1,0}};clearLine[0 m, m, {2,2},0]
m={{1,1,1},{0,1,0}};clearLine[0 m, m, {1,2},0]*)
evolve=Function[{state,pos},Module[{background,board,cur,score,thrown,reject=True},
	{background,board,cur,score,thrown}=state;
	If[Or@@Thread[pos==0],
		If[thrown<3,
			reject=False;thrown+=1;
		],
		If[(board[[Sequence@@pos]]===0&&canAgreeNeighbors[board,pos,cur])||cur===0,
			reject=False;board[[Sequence@@pos]]=cur;{background,board,score}=clearLine[background,board,pos,score]
		];
	];
	If[reject,state,
		cur=RandomChoice[Range[0,9]~Join~{"*"}];
		{background,board,cur,score,thrown}]
	]];

show=Function[{background,board,score},Module[{dim=Dimensions@background},
	Graphics[{FaceForm[None],EdgeForm[Black],Rectangle[{0,0},{dim[[2]],-dim[[1]]}]}~Join~
		MapThread[Text[Style[#2,50],{#[[2]]-0.5,-#[[1]]+0.5}]&,{SparseArray[board]["NonzeroPositions"],SparseArray[board]["NonzeroValues"]}]~Join~
		{EdgeForm[None],FaceForm[Green],Opacity[0.3]}~Join~
		(Rectangle[{#[[2]]-1,-#[[1]]}]&/@SparseArray[background]["NonzeroPositions"]),Axes->True,PlotLabel->score]
]];
(*dim={7,7};show[Array[0&,dim],Array[0&,dim],"a"]*)
toPos=Function[{x,y,dim},(*{dim[[1]]-Floor[y],1+Floor[x]}*){1,1}+Floor@{-y,x}];
Module[{dim={7,7}},
DynamicModule[{background=Array[0&,dim],board=Array[If[{##}==Floor[dim/2+1],"*",0]&,dim],score=0,cur="1",thrown=0},
	ClickPane[Dynamic@show[background,board,Style[{score,cur,thrown},20]],
	With[{pos=toPos[#[[1]],#[[2]],dim]},(*Print@pos;*){background,board,cur,score,thrown}=evolve[{background,board,cur,score,thrown},pos]]&]]]
