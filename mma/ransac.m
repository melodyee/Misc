ransacFindLine=Function[{xys,minNumXy,goodNumXy,torrerance,maxIter},Module[{x,maybeInliners,maybeModel,consensus,param
		,bestModel=Null,bestError=Infinity,bestConsensus,eval},
	eval=Function[ab,pnorm[ab[[1]] #[[1]]+ab[[2]]-#[[2]]&/@xys,1]];
	Do[
		maybeInliners=RandomSample[xys,minNumXy];
		maybeModel=LinearModelFit[maybeInliners,x,x];
		param=Reverse@maybeModel["BestFitParameters"];
		consensus=Select[xys,Abs[#[[2]]-param[[1]]#[[1]]-param[[2]]]<torrerance&];
		If[Length@consensus>goodNumXy,Module[{model=LinearModelFit[maybeInliners,x,x],error},
				error=eval@Reverse@model["BestFitParameters"];
				If[error<bestError,bestModel=model;bestError=error(*Print[bestError];*)]];
		];
	,{maxIter}];If[bestModel===Null,{0,0},Reverse@bestModel["BestFitParameters"]]
	]];
Import@Export["t.png",#]&@Rasterize@Table[SeedRandom[1003];{a,b}=RandomReal[1,2];points=Join[RandomReal[1,{200,2}],xys={#,a #+b+RandomReal[\[Sigma]]}&/@RandomReal[1,100]];
f[\[Theta]_?NumericQ,c_]:=pnorm2[Total[{Cos[\[Theta]],Sin[\[Theta]]}#+c]&/@points,p];
g[\[Theta]_?NumericQ,c_]:=pnorm2[Total[{Cos[\[Theta]],Sin[\[Theta]]}#+c]&/@xys,p];
Print[r=NMinimize[f[\[Theta],c],{\[Theta],c}];//AbsoluteTiming];
(*{#,f@@#,g@@#}&[{\[Theta],c}/.r[[2]]]*)
	img=Image@SparseArray[Clip[#,{1,400}]&/@Round[400 points]->1,400{1,1}];
	{{"\[Sigma]",\[Sigma],"pnorm",p},Graphics@Point[points],Graphics[Prepend[(*Riffle[{Blue,Red,Darker@Yellow},*)Join@@{Text["+",#]&/@points
			,Text["o",#]&/@xys
			,Text["x",#]&/@({#,-(Cot[\[Theta]]#+2c/Sin[\[Theta]])/.r[[2]]}&/@xys[[;;,1]])
			,Text["|",#]&/@With[{rp=ransacFindLine[points,60,100,0.1,200]},{#,rp[[1]]#+rp[[2]]}&/@xys[[;;,1]]]
		}(*]*),Opacity[0.5]]]
	,Radon@img//ImageAdjust
	,Magnify[#,0.5]&@Show[img,Graphics[{Thick,Orange,Line/@ImageLines@img}]]}
,{\[Sigma],{0,0.1,0.3,0.5}},{p,2}]
