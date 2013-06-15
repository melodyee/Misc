safeArray=Function[{rules,dim},Normal@If[Select[rules[[;;,1]],Or@@Thread[#>dim]||Or@@Thread[#<1]&]==={},SparseArray[rules,dim]]];
shiftLeft=Function[{l,n},If[n>=0,l[[n+1;;]]~Join~(0 l[[;;n]]),(0 l[[;;-n]])~Join~l[[;;n-1]]]];
rotate=Function[moving,Module[{nnz=SparseArray[moving]["NonzeroPositions"],center,newNnz},
	center=Round[0.5{1,1}+Mean[nnz]];newNnz={-#[[2]],#[[1]]}&[#-center]+center&/@nnz;
	If[Select[newNnz,#[[1]]<=0||#[[2]]<=0||(Or@@Thread[#>Dimensions[moving]])&]=={},
		safeArray[#->1&/@newNnz,Dimensions@moving]]]];
clearRows=Function[static,Module[{residue=Select[static,#!=(1&/@static[[1]])&],n},
	n=Length@static-Length@residue;{Table[0,{n},{Length@static[[1]]}]~Join~residue,n}]];(*m={{1,1},{0,1},{1,0},{1,1}};clearRows@m*)
addNew=Function[{state,shapes},Module[{static,moving,proposal,totalScore,dim},
	{static,moving,totalScore}=state;dim=Dimensions@static;
	proposal=If[SparseArray[moving]["NonzeroPositions"]==={},
		safeArray[({#[[1]],#[[2]]+Floor[dim[[2]]/2]}->1)&/@(SparseArray[RandomChoice[shapes]]["NonzeroPositions"]),dim]];
	{static,If[proposal=!=Null,proposal,moving],totalScore}]];
evolve=Function[{state,action},Module[{proposal,static,moving,score,totalScore,legal},
	{static,moving,totalScore}=state;
	proposal=Switch[action,"fall",shiftLeft[moving,-1],"left",shiftLeft[#,1]&/@moving,"right",shiftLeft[#,-1]&/@moving,"rotate",rotate@moving];
	{static,moving}=If[proposal===Null,{static,moving},
		legal=With[{nnzs=SparseArray[#]["NonzeroPositions"]&/@{static,moving,proposal}},Intersection@@nnzs[[{1,3}]]==={}&&SameQ@@(Length/@nnzs[[2;;3]])];
		If[action==="fall",If[legal,{static,proposal},{static+moving,0 static}],
			If[legal,{static,proposal},{static,moving}]]];
	{static,score}=clearRows@static;
	{static,moving,totalScore+score}
]];

With[{dim={20,10},shapes={{{1,1},{1,1}},{{1,1},{0,1},{0,1}},{{1,1,1},{0,1,0}},{{1,0},{1,1},{0,1}},{{0,1},{1,1},{1,0}},{{1,1,1,1}}}},
Framed@DynamicModule[{state={Array[0&,dim],Array[0&,dim],0},width=dim[[2]],height=dim[[1]],stop=False},
	EventHandler[
			Dynamic[Refresh[
				state=addNew[state,shapes];
				state=evolve[state,"fall"];
				stop=Select[Flatten[Plus@@state[[;;2]]],#>1&]=!={};
				{state[[3]]}~Join~{MatrixPlot[Plus@@state[[;;2]]]},If[stop,None,UpdateInterval->0.3],TrackedSymbols:>{stop}]]
		,PlotRange->{{0,width},{0,height}},
	"LeftArrowKeyDown":>(state=evolve[state,"left"]),
	"RightArrowKeyDown":>(state=evolve[state,"right"]),
	"UpArrowKeyDown":>(state=evolve[state,"rotate"]),
	"DownArrowKeyDown":>(state=Nest[evolve[#,"fall"]&,state,3])]
	]]


(*Tests below*)
movings={
	{{1,1,0},{0,0,0},{0,0,0}},
	{{1,1,0},{0,0,0}},
	{{1,1,1},{0,0,0}},
	{{0,0,0},{1,1,1}},
	Array[0&,{10,5}]
	};
testMoving=Function[moving,(state=evolve[{0 moving, moving,0},#];MatrixForm/@state)&/@{"left","right","fall","rotate"}];
testMoving/@movings
testAddNew=Function[moving,Module[{shapes},
	shapes={{{1,1},{1,1}},{{1,1},{0,1},{0,1}},{{1,1,1},{0,1,0}},{{1,0},{1,1},{0,1}},{{0,1},{1,1},{1,0}},{{1,1,1,1}}};
	state=addNew[{0 moving, moving,0},shapes];MatrixForm/@state]];
testAddNew/@movings

step=Function[{stateIn,action},Module[{shapes,state=stateIn},
	shapes={{{1,1},{1,1}},{{1,1},{0,1},{0,1}},{{1,1,1},{0,1,0}},{{1,0},{1,1},{0,1}},{{0,1},{1,1},{1,0}},{{1,1,1,1}}};
	state=addNew[state,shapes];
	state=evolve[state,action];
	state]];
init=Array[0&,{10,5}];
actions=Riffle[RandomChoice[{"left","right","rotate"(*,"fall"*)},50],"fall"];
Thread@{{MatrixPlot[#[[1]]],MatrixPlot[#[[2]]],#[[3]]}&/@Rest@FoldList[step,{init,init,0},actions],actions}
