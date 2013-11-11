http://rosettacode.org/wiki/Sudoku#Mathematica


(*index=(#[[1]]-1) n+#[[2]]&;
uncertainty=Function[{assign,k},-Length@Union[Select[neighborSets@@k,s[[#[[1]],#[[2]]]]!=0&]]];
(*MatrixForm/@{s,next,Map[uncertainty,MapIndexed[#2&,s,{2}],{2}]}*)
step=Function[input,Module[{assign,next,prevPosS,nextPosS,toCheck},
	{assign,next,prevPosS,nextPosS,toCheck}=input;
(*	gassign=assign;
	gprevPosS=prevPosS;
	gnext=next;*)
	(*Print[{MatrixForm@gassign,MatrixForm@gnext,gprevPosS,nextPosS,gassign[[#[[1]],#[[2]]]]&/@gprevPosS}];*)
	(*Print@Length@nextPosS;*)
	(*Print[Join[MatrixForm/@{assign,next},{prevPosS,nextPosS}]];*)
		With[{k=If[toCheck===Null,First@SortBy[nextPosS,uncertainty[assign,#]&],toCheck]},
			With[{candids=Select[Range[next[[k[[1]],k[[2]]]],n],Not@MemberQ[assign[[#[[1]],#[[2]]]]&/@(neighborSets@@k),#]&,1]},
			If[Length@candids==0
				,If[Length@prevPosS==0,{}
					,{{ReplacePart[assign,k->0],ReplacePart[next,k->1],Most@prevPosS,Prepend[nextPosS,Last@prevPosS],Last@prevPosS}}]
				,(*Print[{k,First[candids]}];*)
				{{ReplacePart[assign,k->First@candids],ReplacePart[next,k->First@candids+1],Append[prevPosS,k],Complement[nextPosS,{k}],Null}}]]]
	]];
(*Block[{$RecursionLimit=10000,$IterationLimit=10000},step[s,next,{},Select[Join@@MapIndexed[#2&,s,{2}],s[[#[[1]],#[[2]]]]==0&]]];*)
input={s,Map[If[#==0,1,10]&,s,{2}],{},Select[Join@@MapIndexed[#2&,s,{2}],s[[#[[1]],#[[2]]]]==0&],Null};
Dynamic[input[[1]]//MatrixForm]
(*Dynamic[{MatrixForm@gnext,gprevPosS,gassign[[#[[1]],#[[2]]]]&/@gprevPosS}]*)
While[Length[input[[4]]]!=0,
	r=step[input];
	If[Length@r==0,Break[]];
	input=r[[1]];
	];*)




n=9;n2=Sqrt@n;
strs={".685...2..9.72......2983...8...32..1.14...69.6..49...8...3692......57.6..7...895."
	,"4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"};
s=ToExpression@Partition[StringSplit[StringReplace[strs[[1]],"."->"0"], ""],n];
init=MapIndexed[If[#==0,Complement[Range[n],s[[#[[1]],#[[2]]]]&/@(Join@@(neighborSets@@#2))],{#}]&,s,{2}];init//MatrixForm
refineWith=Function[{m,k,v},
	With[{m2=ReplacePart[MapIndexed[If[#2[[1]]==k[[1]]||#2[[2]]==k[[2]]||Quotient[#2,n2]==Quotient[k,n2],Complement[#,{v}],#]&,m,{2}],k->{v}]},
		If[checkPartilValid[m2,k],{m2},{}]]];
hasNoDupe=Length@Union[#]==Length[#]&;
neighborSets=Function[{i,j}
	,Complement[#,{{i,j}}]&/@{Table[{i,k},{k,n}],Table[{k,j},{k,n}]
			,(Join@@Table[{ii,jj}+n2{Quotient[i-1,n2],Quotient[j-1,n2]},{ii,n2},{jj,n2}])}
		];
checkPartilValid=Function[{m,k},And@@(hasNoDupe[Select[m[[#[[1]],#[[2]]]]&/@#,Length@#==1&]]&/@neighborSets@@k)];
(*m=refineWith[init,{1,1},3];MatrixForm/@m*)
splitAt=Function[{m,k},Join@@(refineWith[m,k,#]&/@m[[k[[1]],k[[2]]]])];
(*MatrixForm/@splitAt[init,{1,1}]*)
explode=Function[m,With[{lengths=Union[Length/@(Join@@m)]},
	If[Length@lengths==1,(*Print@m;*){}
		,DeleteDuplicates[Flatten[Function[pos,Function[v,(*Print[{pos,v}];*)refineWith[m,pos,v]]/@m[[pos[[1]],pos[[2]]]]]/@
			Position[init,x_/;Length[x]==lengths[[2]]],2]]]
	]];
(*MatrixForm/@Nest[Join@@(explode/@#)&,{init},2]*)



Needs["Combinatorica`"]
n=9;n2=Sqrt@n;
strs={".685...2..9.72......2983...8...32..1.14...69.6..49...8...3692......57.6..7...895."
        ,"4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"};
s=ToExpression@Partition[StringSplit[StringReplace[strs[[1]],"."->"0"], ""],n];
index=(#[[1]]-1) n+#[[2]]&;
neighborSets=Function[{i,j}
        ,Complement[#,{{i,j}}]&/@{Table[{i,k},{k,n}],Table[{k,j},{k,n}]
                        ,(Join@@Table[{ii,jj}+n2{Quotient[i-1,n2],Quotient[j-1,n2]},{ii,n2},{jj,n2}])}];
constraints=(DeleteDuplicates@Flatten@Table[If[index[#]<index@{i,j},{UndirectedEdge[index@{i,j},index@#]},{}]&/@
	Join@@(neighborSets[i,j]),{i,n},{j,n}])/.UndirectedEdge->List;
colornodes=Select[Tuples[Range[101,109],2],#[[1]]!=#[[2]]&];
preassigned=Flatten[MapIndexed[If[#!=0,Function[c,{index@#2,c}]/@Complement[Range[101,109],{100+#}],{}]&,s,{2}],2];
g=FromUnorderedPairs[Join@@{constraints,colornodes,preassigned}];
g
Partition[VertexColoring[g,Algorithm->Optimum],n]//MatrixForm




Clear[a];as=Array[a,Dimensions@s];b=MapThread[If[#==0,#2,#]&,{s,as},2];
FindInstance[And[And@@(1<=#<=n&/@Flatten@as),And@@(Flatten@Table[b[[#[[1]],#[[2]]]]!=b[[i,j]]&/@Join@@neighborSets[i,j],{i,n},{j,n}])],Flatten@as,Integers,1]
