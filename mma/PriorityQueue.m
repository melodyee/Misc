BeginPackage["PriorityQueue`"]
(*A max-queue*)
(*from http://www.mathematica-journal.com/issue/v7i4/maeder/contents/html/Links/index_lnk_1.html*)
MakeQueue::usage="MakeQueue"
EnQueue::usage="EnQueue"
CopyQueue::usage="CopyQueue"
EmptyQueue::usage="EmptyQueue"
TopQueue::usage="TopQueue"
DeQueue::usage="DeQueue"
DeleteQueue::usage="DeleteQueue"

Begin["`Private`"]

SetAttributes[queue, HoldAll]
SetAttributes[array, HoldAllComplete]

makeArray[n_] := array@@Table[Null, {n}]

MakeQueue[pred_:Greater] :=
  Module[{ar,n=0},
    ar = makeArray[2];
    queue[ar, n, pred]
  ]

CopyQueue[queue[a0_,n0_,pred_]] :=
  Module[{ar=a0,n=n0}, queue[ar, n, pred] ]

EnQueue[q:queue[ar_,n_,pred_], val_] :=
  Module[{i,j},
    If[ n == Length[ar], (* extend (double size) *)
        ar = Join[ar, makeArray[Length[ar]]] ];
    n++;
    ar[[n]] = val; i = n;
    While[ True, (* restore heap *)
      j = Floor[i/2];
      If[ j < 1 || pred[ar[[j]], ar[[i]]], Break[] ];
      {ar[[i]], ar[[j]]} = {ar[[j]], ar[[i]]};
      i = j;
    ];
    q
  ]

EmptyQueue[queue[ar_,n_,pred_]] := n == 0

TopQueue[queue[ar_,n_,pred_]] := ar[[1]]

DeQueue[queue[ar_,n_,pred_]] :=
  Module[{i,j,res=ar[[1]]},
    ar[[1]] = ar[[n]]; ar[[n]] = Null; n--; j = 1;
    While[ j <= Floor[n/2], (* restore heap *)
      i = 2j;
      If[ i < n && pred[ar[[i+1]], ar[[i]]], i++ ];
      If[ pred[ar[[i]], ar[[j]]],
          {ar[[i]], ar[[j]]} = {ar[[j]], ar[[i]]} ];
      j = i
    ];
    res
  ]

DeleteQueue[queue[ar_,n_,pred_]] := (ClearAll[ar,n];)

queue/:Normal[q0_queue] :=
  Module[{l={}, q=CopyQueue[q0]},
    While[!EmptyQueue[q], AppendTo[l, TopQueue[q]]; DeQueue[q]];
    DeleteQueue[q];
    l
  ]

Format[q_queue/;EmptyQueue[q]] := PriorityQueue[]
Format[q_queue] := PriorityQueue[TopQueue[q], "..."]

End[]

EndPackage[]
Needs["PriorityQueue`"]
