http://rosettacode.org/wiki/Sudoku#Mathematica
(*It turns out this version is still too slow for strs[[2]] below*)
solve[array_] := 
 NestWhile[
  Join @@ Function[newarray, 
      Function[{i, j}, 
        Table[ReplacePart[newarray, 
          Position[newarray, 0, {2}, 1][[1]] -> n], {n, 
          Select[Range@9, 
           FreeQ[newarray[[i]], #] && FreeQ[newarray[[All, j]], #] && 
             FreeQ[Partition[
                newarray, {3, 3}][[Sequence @@ 
                 Quotient[{i, j}, 3, -2]]], #] &]}]] @@ 
       Position[newarray, 0, {2}, 1][[1]]] /@ # &, {array}, ! 
    FreeQ[#, 0] &]


n=9;n2=Sqrt@n;Clear[neighbors];
neighbors[i_,j_]:=neighbors[i,j]={Complement[Table[{i,k},{k,n}],{{i,j}}],Complement[Table[{k,j},{k,n}],{{i,j}}]
                ,Complement[Join@@Table[Quotient[{i,j}-1,n2]n2+{ii,jj},{ii,n2},{jj,n2}],{{i,j}}]};
simplifyOne=Function[{i,j,im},Module[{n=Length@im},If[Length@im[[i,j]]==1,im
                ,ReplacePart[im,{i,j}->Complement[im[[i,j]],Union@@Select[im[[#[[1]],#[[2]]]]&/@Join@@neighbors[i,j],Length@#==1&]]]]]];
simplify=Function[im,Module[{om=im,n=Length@im,oldOm},oldOm=om;Do[om=simplifyOne[i,j,om];,{i,n},{j,n}];If[oldOm==om,om,simplify@om]]];
explodeOne=Function[{m,typ,i},Module[{ln=If[typ=="R",m[[i]],m[[;;,i]]],n=Length@m},
        If[Length@Flatten@ln==n,{m},Select[simplify@
			If[typ=="R",ReplacePart[m,i->List/@#],Transpose@ReplacePart[Transpose@m,i->List/@#]]&/@
				Select[Tuples[ln],Union@#==Range[n]&],Position[#,{}]=={}&]]]];
isSol=Function[m,Length@Flatten@m==Length[m]^2];
(*{searchSpace[im[[#[[1]],#[[2]]]]&/@Tuples[{Range[3]+3Quotient[#-1,3],Range[3]+3Mod[#-1,3]}]]}&/@Range[9]*)
explodeAll=Function[m,Module[{oms={m},oldOms,found,sols,n=Length@m,searchSpace=Times@@(Length/@#)&},
        oldOms=oms;
		While[True,(*Also searching over columns slow one case donw. Just heuristic anyway.*)
			oms=Join@@(Function[im,Module[{typi=SortBy[Select[Join@@{{"R",#,searchSpace[im[[#]]]}&/@Range[n](*,{"C",#,searchSpace[im[[;;,#]]]}&/@Range[n]*)},Last@#!=1&],Last][[1,;;2]]},
				explodeOne[im,typi[[1]],typi[[2]]]]]/@oms);
			sols=Select[oms,isSol,1];If[sols!={},found=True;Break[]]];
        If[found,sols,If[oldOms==oms,oms,Join@@(explodeAll/@oms)]]]];
s="......71...37...8646...8...958.....4....2....7.....391...2...4383...96...21......";
s="4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......";
m=ToExpression@Partition[StringSplit[StringReplace[s,"."->"0"],""],n];(*m//MatrixForm*)
im=simplify@Map[If[#==0,Range@n,{#}]&,m,{2}];(*im//MatrixForm*)
(*MatrixForm/@explodeAll[im]//RuntimeTools`Profile*)
MatrixForm/@explodeAll[im]//AbsoluteTiming


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


s = "400000805
  030000000
  000700000
  020000060
  000080400
  000010000
  000603070
  500200000
  104000000";
m = ToExpression[StringSplit[#, ""] & /@ StringSplit[s, "\n"]];
Clear[x]; xs = Array[x, Dimensions[m]];
board = MapThread[If[# == 0, #2, #] &, {m, xs}, 2]; board // MatrixForm
(*cost=Function[m,Module[{n=Length@m,isFull,isFull2,withinRange},
isFull=(Total[#]-Total@Range@n)^2&
isFull2=(Times@@#-Times@@(Range@n))^2 /Times@@(Range@n)&
withinRange=If[1<=#<=n,0,1]&
Total[isFull@#+isFull2@#(*Total[If[Abs[#]<0.3,1,0]&/@Differences[#]*)&/@(\
Join@@{m,Transpose@m,Flatten/@(Join@@Partition[m,{3,3}])})]
+Total@Flatten@Map[withinRange,m,{2}]
]];
r=NMinimize[cost[board](*,1<=#<=9&/@Flatten@xs}*),Flatten@xs,Method->\
"DifferentialEvolution"];//AbsoluteTiming
r[[1]]
Round[board/.r[[2]]]//MatrixForm*)
