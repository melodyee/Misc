n=8;next=Array[1&,n];
step=Function[{assign,next,k},(*Print[{assign,next,k}];*)
	Which[k==0,{}
		,k==n+1,{assign}~Join~step[assign,next,k-1]
		,True,With[{candids=Select[Range[n]
				,Not@MemberQ[Join@@{assign[[;;k-1]],assign[[;;k-1]]+Reverse@Range[k-1],assign[[;;k-1]]-Reverse@Range[k-1]},#]&&#>=next[[k]]&,1]},
			If[Length@candids==0,step[assign,ReplacePart[next,k->0],k-1]
				,step[ReplacePart[assign,k->First@candids],ReplacePart[next,k->First@candids+1],k+1]]]
	]];
sols=step[Array[0&,n],next,1];//AbsoluteTiming
Length@sols
Import@Export["t.png",Rasterize[MatrixPlot@SparseArray[MapIndexed[{#2[[1]],#}&,#]->1,n{1,1}]&/@sols]]