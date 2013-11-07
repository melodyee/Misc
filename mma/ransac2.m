(*fitModelF will return a model. enlargeModelF[maybeModel,points] will do best effort to enlarge the model,
	and return the model and its inliers.
evalModelF will eval the model on whole set of points*)
ransacDriver=Function[{points,fitModelF,costF,nullModel,torrerance,minNumPoints,goodNumPoints,maxIter}
			,Module[{maybeInliners,maybeModel,maybeError,bestModel=Null,bestError=Infinity,largerModel,inLiers,bestInLiers},
        Do[maybeInliners=RandomSample[points,minNumPoints];maybeModel=fitModelF[maybeInliners];
				inLiers=Select[points,costF[maybeModel,#]<torrerance&];largerModel=fitModelF@inLiers;
                If[Length@inLiers>goodNumPoints,Module[{error=Total[costF[largerModel,#]&/@points]},
                      If[error<bestError,bestModel=largerModel;bestError=error(*Print[bestError];*)]];
                ];
        ,{maxIter}];
		If[bestModel===Null,nullModel,bestModel]]];
ransacFindLine2D=Function[{pointsIn,minNumXy,goodNumXy,torrerance,costF}
			,Module[{fitModelF,enlargeModelF,nullModel={0,0},x,maxIter=10},
		fitModelF=Function[points,Reverse[LinearModelFit[points,x,x]["BestFitParameters"]]];
		ransacDriver[pointsIn,fitModelF,costF,nullModel,torrerance,minNumXy,goodNumXy,maxIter]
	]];
(*\[Sigma]=0.1;SeedRandom[1003];{a,b}=RandomReal[1,2];points=Join[RandomReal[1,{200,2}],xys={#,a #+b+RandomReal[\[Sigma]{-1,1}]}&/@RandomReal[1,100]];
costF=Function[{model,point},Abs[point[[2]]-model[[1]]point[[1]]-model[[2]]]];
l={a,b};l2=ransacFindLine2D[points,60,100,0.1,costF];
pointsOneLine=Function[{a,b},Table[{x,a x +b},{x,0,1,0.05}]];
{{l,l2},Graphics[{Point@points,Blue,Point[pointsOneLine@@l],Red,Point[pointsOneLine@@l2]}]}*)
