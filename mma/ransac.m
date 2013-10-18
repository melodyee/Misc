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

(*findLineCost=Function[{xys,abcs,uvws,\[Theta],\[Lambda]},{Total@#,#}&@{
	pnorm[MapThread[Dot,{Append[#,1]&/@xys,abcs}],1],1/(2\[Theta])pnorm2[uvws-abcs,2],pnorm[Differences@uvws,1]}];
findLine=Function[{xys,\[Lambda],maxIter},Module[{initAbc,abcs,oldAbcs,uvws
		,n=Length@xys,\[Theta]=1,\[Rho]=0.8},
	(*initAbc=With[{ab=SingularValueDecomposition[*)
LeastSquares[Append[#,1]&/@xys,Array[1&,Length@xys]]-{0,0,1}(*RandomReal[1,3]*);
	Print[initAbc];
	uvws=abcs=Array[initAbc&,Length@xys];oldAbcs=0 abcs;
	Do[If[pnorm[abcs-oldAbcs,2]/n<10^-5,Print[j];Break[]
		,oldAbcs=abcs;Print["s",findLineCost[xys,abcs,uvws,\[Theta],\[Lambda]]];
			abcs=MapThread[Function[{uvw,xy},dShrinkageVector[\[Theta],uvw,Append[xy,1],0]],{uvws,xys}];Print["d",findLineCost[xys,abcs,uvws,\[Theta],\[Lambda]]];
			uvws=TotalVariationFilter[abcs,\[Theta] \[Lambda]];Print["tv",findLineCost[xys,abcs,uvws,\[Theta],\[Lambda]]];
			\[Theta]*=\[Rho];];
	,{j,maxIter}];abcs
	]];
SeedRandom[1003];{a,b}=RandomReal[1,2];points=Join[RandomReal[1,{100,2}],xys={#,a #+b}&/@RandomReal[1,100]];ListPlot@points
findLine[points,1,10]*)
