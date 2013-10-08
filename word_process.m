fname="~/abe_temp_data/sentiment/sentiment.csv";ofname="~/t.csv";l=Import@fname;
(*Drop the quotes at start and end.*)
vec=Function[ln,{Boole[ln[[1]]=="pos"],Tally@StringSplit[StringTake[ln[[2]],{3,-3}]," "]}]/@l[[;;]];
words=Union@(First/@(Join@@vec[[;;,2]]));dict=Dispatch@Thread[words->Range@Length@words];
Import@Export[ofname,Prepend[Normal@SparseArray[Rule@@@(#[[2]]/.dict),Length@words],#[[1]]]&/@vec]
(*Import@Export["~/t2.txt",Function[ln,ToString[ToString[#[[1]]]<>" "<>ToString[#[[2]]]&/@SortBy[Prepend[ln[[2]]/.dict,{0,If[ln[[1]]==0,"neg","pos"]}],First]]]/@vec[[;;]]]*)
(*Export["t.txt","@attribute word"<>ToString@#<>" numeric"&/@Range@Length@words]*)



s=OpenRead@"t2.csv";
{train,test}=OpenWrite/@{"train.csv","test.csv"};
While[(r=Read[s,Word])=!=EndOfFile,WriteString[If[RandomReal[]>0.5,train,test],r<>"\n"]];
Close/@{train,test};
