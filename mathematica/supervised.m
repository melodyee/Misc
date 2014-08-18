(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
{known,unknown}=Import@"/h/mxs/DigitRecognizer.mx";//AbsoluteTiming
SeedRandom[1003];fullSet=Range[Length@known];trainSet=RandomSample[fullSet,Round[4/5 Length@known]];testSet=Complement[fullSet,trainSet];
train=Developer`ToPackedArray@known[[trainSet]];test=Developer`ToPackedArray@known[[testSet]];
digits=SplitBy[SortBy[train,First],First];
trainResponse=Developer`ToPackedArray@train[[;;,1]];
trainResponseOneHot=Developer`ToPackedArray[N@oneHot[#+1,10]&/@train[[;;,1]]];trainFeatures=Append[#,1.]&/@train[[;;,2;;]];
trainResponseSupportVector=N@Map[If[#>=1,1,-1]&,trainResponseOneHot,{2}];testFeatures=Append[#,1.]&/@test[[;;,2;;]];
testResponse=Developer`ToPackedArray@test[[;;,1]];
evalOneHot=Function[{ws2,testFeatures,testResponse},If[Dimensions[ws2][[1]]==Dimensions[testFeatures][[2]]
	,N@Total@MapThread[Boole[#==#2]&,{testResponse,decodeOneHot[#]-1&/@(testFeatures.ws2)}]/Length@testFeatures]];
evalOneHotLogistics=Function[{ws2,testFeatures,testResponse},If[Dimensions[ws2][[1]]==Dimensions[testFeatures][[2]]
	,N@Total@MapThread[Boole[#==#2]&,{testResponse,decodeOneHot[#]-1&/@sigmoid2D[testFeatures.ws2]}]/Length@testFeatures]];
evalOneHotSupportVector=Function[{ws,testFeatures,testResponse},If[Dimensions[ws][[1]]==Dimensions[testFeatures][[2]]
	,N@Total@MapThread[Boole[#==#2]&,{testResponse,decodeOneHot[#]-1&/@Map[Boole[#>=0]&,testFeatures.ws,{2}]}]/Length@testFeatures]];
showImage=Function[v,Partition[v,28]//Image];showImage/@trainFeatures[[;;10]]


mean=Mean[trainFeatures];scale=255 Norm[trainFeatures[[1]]]/Length[trainFeatures[[1]]];
trainFeaturesS=(#-mean&/@trainFeatures)/scale;
testFeaturesS=(#-mean&/@testFeatures)/scale;


(*Export to Weka*)
Export["/Applications/MacPorts/Weka.app/Contents/Resources/train_3000.csv",Prepend[Prepend[#[[2;;]],
	"c"<>IntegerString[Round[#[[1]]]]]&/@train[[;;3000]],Table["f"<>IntegerString[i],{i,Length@train[[1]]}]]]


(*Export to Abe*)
Export["/g/tmp/train.csv",Prepend[#[[2;;]],"c"<>IntegerString[Round[#[[1]]]]]&/@train[[;;]]]
Export["/g/tmp/test.csv",Prepend[#[[2;;]],"c"<>IntegerString[Round[#[[1]]]]]&/@test[[;;]]]


(*PCA visualization of data*)
SeedRandom[1003];X=Standardize[trainFeatures,Mean,1&];svd=SingularValueDecomposition[X,2];V=svd[[3]];
indices=RandomSample[Range@Length@trainFeatures,100];
Graphics[MapThread[Inset[showImage@#,#2,{0,0},100]&,{trainFeatures[[indices]],trainFeatures[[indices]].#}],ImageSize->500]&/@{
	V(*,V.DiagonalMatrix@Abs@Standardize@Diagonal@Transpose[svd[[2]]]*)}


SeedRandom[1003];Y=RandomSample@Join[RandomReal[1,{30,2}],RandomReal[{1,2},{10,2}]];Dynamic[Graphics@{MapThread[Inset,{cW,Y},1],Point@cH}]
kmeans[Y,2,"DebugF"->((Pause[10];cW=#;cH=#2)&),"DebugEveryN"->1];


Dynamic[ImageAdjust@showImage@#&/@cH]
kmeans[trainFeatures[[;;]],10,"DebugF"->((cW=#;cH=#2)&),"DebugEveryN"->20];//AbsoluteTiming
(*clus=FindClusters[trainFeatures,10];//AbsoluteTiming
ImageAdjust@showImage@Mean@#&/@clus*)


(*SVD is equivalent to PMF, both are poor in approximating trainFeatures.*)
svectors=Transpose[SingularValueDecomposition[trainFeatures,10][[3,;;,1;;10]]];ImageAdjust@showImage@#&/@svectors


Dynamic@{cost,(*MatrixPlot@Transpose[cwh[[1]]],*)ImageAdjust@showImage@#&/@Chop@cwh[[2]]}
nonNegativeMatrixFactorization=Function[{V,r,niter,method}
		,Module[{nonNegativeInitialize,m,n,residues={},WH,eps=N[10^-6],wh,vwh,wt,normV2,WtW,wtv,ht},
	{m,n}=Dimensions[V];normV2=pnorm2[V,2];
	nonNegativeInitialize=Function[{eps,dims},pack@Map[(eps+Abs[#])&,RandomVariate[NormalDistribution[],dims],{-1}]];
	WH=Nest[Module[{W=Chop@#[[1]],H=Chop@#[[2]]},cwh=#;
		Switch[method,"KL",Abort[];wh=W.H;vwh=V/(wh+eps);W=W Developer`ToPackedArray[(((Total[H,{2}])^-1 #)&/@(vwh.Transpose[H]))];
				H=H Developer`ToPackedArray[((Total[Transpose[W],{2}])^-1 (Transpose[W].vwh))];
			,"F",W=W (V.Transpose[H])/(eps+W.(H.Transpose[H]));
				wt=Transpose[W];WtW=wt.W;wtv=wt.V;H=H wtv/(eps+WtW.H);ht=Transpose[H];
				residues=Append[residues,cost=Sqrt[matrixFactorizationDistanceSquare[V,W,H,normV2]]];
			,_,Abort[]];
		{W,H}
	]&,nonNegativeInitialize[eps,#]&/@{{m,r},{r,n}},niter];
	{WH[[1]],WH[[2]],ListPlot[residues,PlotRange->All]}
	]];
{ws9,hs9,plot}=nonNegativeMatrixFactorization[trainFeatures[[;;]],10,100,"F"];plot


(*k=10, 261047, no initialization in 180s. k=20 finds all digits, 281184 in 400s.*)
cnt=0;Dynamic@MatrixPlot@cus2t
Dynamic@{ImageAdjust@showImage@#&/@cws,Sqrt@cost}
Clear[nonNegativeMatrixFactorizationGradientDescent];
(*"Hadamard":(U\circ U)(V\circ V),"HalfHadamard",U(V\circ V),"Sigmoid",(S\circ U)(S\circ V),"HalfSigmoid",(S\circ U)(V\circ V)
	,"SigmoidOriginal",(S\circ U)V*) 
(*"SigmoidOriginal" fails to break symmetry.*)
nonNegativeMatrixFactorizationGradientDescent[X_List,k_Integer
		,OptionsPattern[{"Init"->"Random","p"->2,"lambda"->1.,"Method"->"Hadamard","SoftMax"->"Logistic","SoftMaxK"->4}]]:=
	Module[{us,vs,u,v,f,g,r,us2,vs2,us2t,vs2t,Xnorm2=pnorm2[X,2],p=OptionValue["p"],W,H,\[Lambda]=OptionValue["lambda"],b=10,softMax,softMaxJacobian},
	Switch[OptionValue["SoftMax"],"Hadamard",softMax:=softMaxHadamardRows2D[us,OptionValue["SoftMaxK"]];
				softMaxJacobian:=MapThread[softMaxHadamardJacobian[#,OptionValue["SoftMaxK"]].#2&,{us,2(X.vs2t-us2.(vs2.vs2t))},1];
		,"Logistic",softMax:=softMaxRows2D[us,b];softMaxJacobian:=MapThread[softMaxJacobianFromValue[#].#2&,{us2,2(X.vs2t-us2.(vs2.vs2t))},1],_,Abort[]];
	If[Head[OptionValue["Init"]]===List,{W,H}=OptionValue["Init"]
		,{W,H}={RandomReal[1,{Dimensions[X][[1]],k}],RandomReal[1,{k,Dimensions[X][[2]]}]}];
	us=Array[u,{Dimensions[X][[1]],k}];vs=Array[v,{k,Dimensions[X][[2]]}];
	f[us_?(NumericQ@#[[1,1]]&),vs_]:=(Switch[OptionValue["Method"],"Sigmoid",us2=sigmoid2D@us;vs2=sigmoid2D@vs,"Hadamard",us2=us us;vs2=vs vs;
			,"HalfHadamard",us2=us;vs2=vs vs;,"HalfSigmoid",us2=sigmoid2D@us;vs2=vs vs,"SoftmaxHadamard",us2=softMax;vs2=vs vs;
			,"SigmoidOriginal",us2=sigmoid2D@us;vs2=vs,"SoftmaxOriginal",us2=softMax;vs2=vs,_,Abort[]];us2t=Transpose[us2];vs2t=Transpose[vs2];
		With[{c=matrixFactorizationDistanceSquare[X,us2,vs2,Xnorm2]},If[Mod[cnt++,20]==0,cws=vs2;cus2t=us2t;cost=c];
		c+If[p==2,\[Lambda] pnorm2[us,2]+\[Lambda] pnorm2[vs,2],\[Lambda] relativisticNorm2D[us,1]+\[Lambda] relativisticNorm2D[vs,1]]]);
	g[us_?(NumericQ@#[[1,1]]&),vs_]:=-Join[vec[If[MemberQ[{"SoftmaxOriginal","SoftmaxHadamard"},OptionValue["Method"]],softMaxJacobian
			,Switch[OptionValue["Method"],"Sigmoid",(1-us2)us2,"Hadamard",2us,"SigmoidOriginal",(1-us2)us2,"HalfHadamard",1
				,"HalfSigmoid",(1-us2)us2]2(X.vs2t-us2.(vs2.vs2t))]+\[Lambda] If[p==2,2us,gradientRelativisticNorm2D[us,1]]]
		,vec[Switch[OptionValue["Method"],"Sigmoid",(1-vs2)vs2,"SigmoidOriginal",1,"SoftmaxOriginal",1,_,2vs]2(us2t.X-(us2t.us2).vs2)
			+\[Lambda] If[p==2,2vs,gradientRelativisticNorm2D[vs,1]]]];
	Print@{r=FindMinimum[f[us,vs],Join[variableWithInitial[vec[us],vec@Sqrt@W]
		,variableWithInitial[vec[vs],vec@Sqrt@H]],Gradient:>g[us,vs]];//AbsoluteTiming,r[[1]]};
	{With[{rus=us/.Dispatch[r[[2]]]},rus rus],With[{rvs=vs/.Dispatch[r[[2]]]},rvs rvs]}];
k=10;method={"Sigmoid","Hadamard","HalfHadamard","HalfSigmoid","SigmoidOriginal","SoftmaxOriginal","SoftmaxHadamard"}[[2]];
{ws10,hs10}=nonNegativeMatrixFactorizationGradientDescent[trainFeatures[[;;3000]],k(*,{"p"->1}*),"Method"->method,"SoftMax"->"Hadamard"];
Print@{ImageAdjust@showImage@#&/@cws,Sqrt@cost}
(*{ws10,hs10}=nonNegativeMatrixFactorizationGradientDescent[trainFeatures[[;;]],k,
	{"Method"->method,"Init"->{RandomReal[1,{Length@trainFeatures,k}],Last@nonNegativeMatrixFactorizationGradientDescent[trainFeatures[[;;3000]],k,
		{"Method"->method,"Init"->{RandomReal[1,{3000,k}]
			,Last@nonNegativeMatrixFactorizationGradientDescent[trainFeatures[[;;300]],k,{"Method"->method}]}}]}}];*)


(*On 500random cases, 0.856(num=300), 0.908(num=3000), 0.976(num=All)*)(*Nearest Neighbor*)
num=All;nf=Nearest[trainFeatures[[;;num]]->trainResponseOneHot[[;;num]]];//AbsoluteTiming
evalOneHotNearestNeighbor=Function[{nf,testFeatures,testResponse},N@Total@MapThread[Boole[#==#2]&
	,{testResponse,Parallelize[decodeOneHot[First[nf[#]]]-1&/@testFeatures]}]/Length@testFeatures];
indices=RandomSample[Range@Length@testFeatures,500];
evalOneHotNearestNeighbor[nf,testFeatures[[indices]],testResponse[[indices]]]//AbsoluteTiming


(*Linear regression gives 0.24*)
ws=LeastSquares[train[[;;,2;;]],train[[;;,1]]];
N@Total@MapThread[Boole[#==#2]&,{test[[;;,1]],Round[test[[;;,2;;]].ws]}]/Length@test
ListPlot[ws,PlotRange->All]


(*Using OneHot boosts to 0.843 and 0.850 (with bias term))*)
Table[ws2=LeastSquares[featureF/@train[[;;,2;;]],trainResponseOneHot];
	{evalOneHot[ws2,featureF/@test[[;;,2;;]],testResponse],ListPlot[Transpose@ws2,PlotRange->All]}
,{featureF,{Append[#,0.]&,Append[#,1.]&,Append[#,255.]&,Append[#,0.1]&}}]
ImageAdjust@showImage@#&/@Transpose[ws2]


(*0.849, Regularized regression (simple)*)
\[Lambda]=10000;
ws8=LeastSquares[vertcat[trainFeatures,\[Lambda] sparseIdentityMatrix[Dimensions[trainFeatures][[2]]]],
	Join[trainResponseOneHot,Array[0&,{Dimensions[trainFeatures][[2]],Dimensions[trainResponseOneHot][[2]]}]]];
evalOneHot[ws8,testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws8]


(*0.842(p=2,\[Lambda]=0.1num),0.850(p=1,\[Lambda]=0.1num), Regularized regression*)
Clear[f,w];ws3=Array[w,{Dimensions[trainFeatures][[2]],Dimensions[trainResponseOneHot][[2]]}];num=All;
	{\[Lambda],p}={10 Length[trainFeatures[[;;num]]],2};
Dynamic[Append[ImageAdjust@showImage@#&/@Transpose[cws],{cost,evalOneHotLogistics[cws,testFeatures,testResponse]}]]
f[ws_?(NumericQ@#[[1,1]]&)]:=(cws=ws;cost=pnorm2[trainResponseOneHot[[;;num]]-(trainFeatures[[;;num]].ws),2]+\[Lambda] pnorm2[ws,p]);
g[ws_?(NumericQ@#[[1,1]]&)]:=vec[2(AtA.ws3-AtB)]+\[Lambda] If[p==2,2 vec@ws3,vec@Sign@ws3];
ws2=LeastSquares[trainFeatures,trainResponseOneHot];Print[{"f[ws2]",f[ws2]}];
AtA=Transpose[trainFeatures[[;;num]]].trainFeatures[[;;num]];
AtB=Transpose[trainFeatures[[;;num]]].trainResponseOneHot[[;;num]];
{r=FindMinimum[f[ws3],variableWithInitial[vec@ws3,vec@ws2](*vec@ws3*),Gradient:>g[ws3]
	(*,MaxIterations->1000*)];//AbsoluteTiming,r[[1]]}
evalOneHot[ws3/.Dispatch[r[[2]]],testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws3/.Dispatch[r[[2]]]]


(*0.911 in 38s, Fourier domain*)
(*0.912 in 32s, raw + Fourier*)
trainFeaturesFourier=pack[Join[#,FourierDCT@#]&/@trainFeatures];testFeaturesFourier=pack[Join[#,FourierDCT@#]&/@testFeatures];
Dynamic[Append[ImageAdjust@showImage@Re@FourierDCT[#[[Length[#]/2;;]],3]&/@Transpose[cws],{cost,evalOneHotLogistics[cws,testFeaturesFourier,testResponse]}]]
num=All;cnt=0;
ws11=lve[trainFeaturesFourier[[;;num]],trainResponseOneHot[[;;num]]];//AbsoluteTiming


(*Logistic Regression*)
(*SoftMax: 0.79 (num=300), 0.88 (num=3000), 0.926 in 39s. Need initialization*)
(*Logit: 0.79 (num=300), 0.88 (num=3000), 0.911 in 20s. Don't need initialization.*)
cnt=0;Dynamic[Append[ImageAdjust@showImage@#&/@Transpose[cws],{cost,evalOneHotLogistics[cws,testFeatures,testResponse]}]]
Clear[logisticSolve];
logisticSolve[A_List,B_List,OptionsPattern[{"Init"->"Random","Method"->"SoftMax","MaxIterations"->100,"DebugEveryN"->20,"DebugF"->(Null&)}]]:=
	Module[{f,g,w,ws,r,\[Lambda]=Length@A,SAX,es,At=Transpose@A,b=5
		,useSoftMax},ws=Array[w,{Dimensions[A][[2]],Dimensions[B][[2]]}];useSoftMax=OptionValue["Method"]==="SoftMax";
	f[ws_?(NumericQ@#[[1,1]]&)]:=(SAX=If[useSoftMax,softMaxRows2D[A.ws,b],sigmoid2D[A.ws]];es=SAX-B;With[{c=pnorm2[es,2]+\[Lambda] pnorm2[ws,2]}
		,If[Mod[cnt++,20]==0,cws=ws;cost=c];c]);
	g[ws_?(NumericQ@#[[1,1]]&)]:=(2 vec[At.If[useSoftMax,MapThread[softMaxJacobianFromValue[#2].#&,{es,SAX},1],SAX (1-SAX) es]+\[Lambda] ws]);
	r=FindMinimum[f[ws](*Method is "QuasiNewton"*)
		,If[Head[OptionValue["Init"]]===List,variableWithInitial[vec@ws,vec[OptionValue["Init"]]]
			,variableWithRandomInitial[vec@ws]],Gradient:>g[ws](*,PrecisionGoal->Infinity*)
		,MaxIterations->OptionValue["MaxIterations"]];
	cws=ws/.Dispatch[r[[2]]];(*Print[evalOneHotLogistics[cws,testFeatures,testResponse]];*)
	ws/.Dispatch[r[[2]]]];
num=All;(*ws12=logisticSolve[trainFeatures[[;;num]],trainResponseOneHot[[;;num]]];//AbsoluteTiming*)
(*With[{trainFeatures=trainFeaturesS,testFeatures=testFeaturesS},*)
Table[Print[ws12=logisticSolve[trainFeatures[[;;num]],trainResponseOneHot[[;;num]]
	,{"Method"->method,"Init"->logisticSolve[trainFeatures[[;;3000]],trainResponseOneHot[[;;3000]]
		,{"Method"->method,"Init"->logisticSolve[trainFeatures[[;;300]],trainResponseOneHot[[;;300]],{"Method"->method}]}]}];//AbsoluteTiming];
{method,evalOneHotLogistics[ws12,testFeatures,testResponse]},{method,{"SoftMax","Logit"}}]


num=All;ws12=logisticSolve[ Normalize/@trainFeatures[[;;num]],trainResponseOneHot[[;;num]]];//AbsoluteTiming
evalOneHotLogistics[ws12, Normalize/@testFeatures,testResponse]


num=All;ws12=logisticSolve[ Standardize/@trainFeatures[[;;num]],trainResponseOneHot[[;;num]]];//AbsoluteTiming
evalOneHotLogistics[ws12, Standardize/@testFeatures,testResponse]


(*Bagging of Logistic only gives 0.838(optimal weighting), 0.737(no weighting). Maybe we need random choice of features?*)
num=Length@trainFeatures;
wss=Parallelize@Table[num=Length@trainFeatures;indices=Sort@RandomSample[Range@num,Round[num/5]];logisticSolve[
	trainFeatures[[indices]],trainResponseOneHot[[indices]]],{20}];
X=vec/@(sigmoid2D[trainFeatures.#]&/@wss);
evalOneHotLogisticsMulti=Function[{wss,testFeatures,testResponse,as},If[Dimensions[wss[[1]]][[1]]==Dimensions[testFeatures][[2]]
	,N@Total@MapThread[Boole[#==#2]&,{testResponse,decodeOneHot[#]-1&/@(as.(sigmoid2D[testFeatures.#]&/@wss))}]/Length@testFeatures]];
as=Normalize@LeastSquares[pack@Join[Transpose[X],400N@IdentityMatrix[Length[X]]],Join[vec[trainResponseOneHot],Table[0.,{Length@X}]]]
evalOneHotLogistics[#,trainFeatures,trainResponse]&/@wss
evalOneHotLogisticsMulti[wss,trainFeatures,trainResponse,#]&/@{as,Normalize[1&/@as]}
evalOneHotLogistics[#,testFeatures,testResponse]&/@wss
evalOneHotLogisticsMulti[wss,testFeatures,testResponse,#]&/@{as,Normalize[1&/@as]}


(*0.88 (num=3000), 0.898*)
Dynamic[Append[ImageAdjust@showImage@#&/@Transpose[cws],{cost,evalOneHotLogistics[cws,testFeatures,testResponse]}]]
softHingeSolve=Function[{A,B},softHingeSolveWithInit[A,B,LeastSquares[A,B]]];
softHingeSolveWithInit=Function[{A,B,ws2},Module[{f,g,w,ws,r,es,\[Lambda]=10 Length@A,aws,At=Transpose[A]},ws=Array[w,{Dimensions[A][[2]],Dimensions[B][[2]]}];
	f[ws_?(NumericQ@#[[1,1]]&)]:=(cws=ws;aws=A.ws;es=softHinge2D[aws,1]-B;cost=pnorm2[es,2]+\[Lambda] pnorm2[ws,2]);
	g[ws_?(NumericQ@#[[1,1]]&)]:=2vec[At.(sigmoid2D[aws] es)+\[Lambda] ws];
	r=FindMinimum[f[ws],variableWithInitial[vec@ws,vec[ws2]](*variableWithRandomInitial[vec@ws]*),Gradient:>g[ws]];
	Print[r[[1]]];ws/.Dispatch[r[[2]]]]];
evalOneHotSoftHinge=Function[{ws2,testFeatures,testResponse},N@Total@MapThread[Boole[#==#2]&
	,{testResponse,decodeOneHot[#]-1&/@softHinge2D[testFeatures.ws2,1]}]/Length@testFeatures];
ws7=softHingeSolveWithInit[trainFeatures[[;;]],trainResponseOneHot[[;;]]
	,softHingeSolve[trainFeatures[[;;3000]],trainResponseOneHot[[;;3000]]]];
evalOneHotSoftHinge[ws7,testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws7]


(*0.810, SVM with Exp[-y f[x]] objective?.*)
Clear[supportVector];
supportVector[xs_List,ys_List,OptionsPattern[{"lambda"->Null,"Loss"->"L2"}]]:=Module[{w,f,g,ws,r,ws2,xsws,xswsys,xst=Transpose[xs],cf,\[Lambda]},
	\[Lambda]=If[NumericQ@OptionValue["lambda"],Length@xs,OptionValue["lambda"]];
	cf=Switch[OptionValue["Loss"],"L2",Compile[{{xsws,_Real,2},{yss,_Real,2}},MapThread[If[#1 #2>1,0.`,#1-#2]&,{xsws,yss},2],CompilationTarget:>"C"]
		,"Exp",Compile[{{xxs,_Real,2},{wws,_Real,2},{yss,_Real,2}},MapThread[If[#1 #2>1,0.`,#1-#2]&,{xsws,yss},2],CompilationTarget:>"C"],_,Abort[]];
	ws=Array[w,{Dimensions[xs][[2]],Dimensions[ys][[2]]}];ws2=LeastSquares[xs,ys];
	f[ws_?(NumberQ[#1[[1,1]]]&)]:=(xsws=xs.ws;xswsys=cf[xsws,ys];pnorm2[xswsys,2]+\[Lambda] pnorm2[ws,2]);
	g[ws_?(NumberQ[#1[[1,1]]]&)]:=2 vec[xst.xswsys+\[Lambda] ws];
	{AbsoluteTiming[r=FindMinimum[f[ws],variableWithInitial[vec[ws],vec[ws2]],Gradient:>g[ws]];],r[[1]]};ws/. Dispatch[r[[2]]]];


(*0.810, SVM least square.*)
ws5=supportVectorLeastSquare[trainFeatures,trainResponseSupportVector,Length@trainFeatures];
evalOneHotSupportVector[ws5,testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws5]


(*not working. SVM dual form*)
Dynamic[Append[ImageAdjust@showImage@#&/@Transpose[cws],{cost,evalOneHotSupportVector[cws,testFeatures,testResponse]}]]
num=1000;
Clear[a,f,g];X=trainFeatures[[;;num]];Y=trainResponseSupportVector[[;;num]];as=Array[a,Dimensions@Y];Xt=Transpose[X];
	f[as_?(NumberQ@#[[1,1]]&)]:=(ws=Xt.(Y as);cws=ws;cost={0.5 pnorm2[ws,2],-Total@Flatten[as],pnorm2[as,2]};Plus@@cost[[;;2]]);
	g[as_?(NumberQ@#[[1,1]]&)]:=vec[Y (X.ws)-1];
r=FindMinimum[(*Prepend[Join@@Map[0<=#<=1&,as,{2}],f[as]]*) f[as],vec@as,Gradient:>g[as],MaxIterations->1000];


(*0.67(num=300), 0.74(num=3000), 0.799(num=All)*)
supportVectorRelativistic=Function[{xs,ys,\[Lambda]},supportVectorRelativisticWithInit[xs,ys,\[Lambda],LeastSquares[xs,ys]]];
supportVectorRelativisticWithInit=Function[{xs,ys,\[Lambda],ws2},(*\[Lambda] is different from C as it penalizes L2 of deviation.*)
		Module[{w,f,g,ws,r,xsws,xswsys,xst=Transpose[xs],cf,eps=10^(-6)},
	cf=Compile[{{xsws,_Real,2},{yss,_Real,2}},MapThread[If[# #2>1,0.,(#-#2)]&,{xsws,yss},2]];
	ws=Array[w,{Dimensions[xs][[2]],Dimensions[ys][[2]]}];
	f[ws_?(NumberQ@#[[1,1]]&)]:=(cws=ws;xsws=xs.ws;xswsys=cf[xsws,ys];cost=relativisticNorm2D[xswsys,eps]+\[Lambda] pnorm2[ws,2]);
	g[ws_?(NumberQ@#[[1,1]]&)]:=vec[xst.gradientRelativisticNorm2D[xswsys,eps]+2 \[Lambda] ws];
	Print@{r=FindMinimum[f[ws],variableWithInitial[vec@ws,vec@ws2],Gradient:>g[ws]];//AbsoluteTiming,r[[1]]};
	Print[evalOneHotSupportVector[cws,testFeatures,testResponse]];ws/.Dispatch[r[[2]]]]];
Dynamic[Append[ImageAdjust@showImage@#&/@Transpose[cws],{cost,evalOneHotSupportVector[cws,testFeatures,testResponse]}]]
\[Lambda]=1;
ws9=supportVectorRelativistic[trainFeatures[[;;]],trainResponseSupportVector[[;;]],\[Lambda] Length@trainFeatures,
		supportVectorRelativistic[trainFeatures[[;;3000]],trainResponseSupportVector[[;;3000]],\[Lambda] 3000,
			supportVectorRelativistic[trainFeatures[[;;300]],trainResponseSupportVector[[;;300]],\[Lambda] 300]]];
evalOneHotSupportVector[ws9,testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws9]


(*0.838, Rank constrainted least square.*)
Y=trainResponseOneHot[[;;]];X=trainFeatures[[;;]];
Clear[f,u,v];k=10;us=Array[u,{Dimensions[X][[2]],k}];vs=Array[v,{k,Dimensions[Y][[2]]}];Dynamic@cost
f[us_?(NumericQ@#[[1,1]]&),vs_]:=cost=pnorm2[X.us.vs-Y,2]+\[Lambda] pnorm2[us,2]+\[Lambda] pnorm2[vs,2];
g[us_?(NumericQ@#[[1,1]]&),vs_]:=2Join[vec[XtX.us.vs.Transpose[vs]-XtY.Transpose[vs]+\[Lambda] us],vec[Transpose[us].XtX.us.vs-Transpose[us].XtY+\[Lambda] vs]];
ws2=LeastSquares[X,Y];\[Lambda]=10 Length[X];
{ius,ivs}=({#[[1]].#[[2]],Transpose@#[[3]]}&@SingularValueDecomposition[ws2,k]);
evalOneHot[ws2,testFeatures,testResponse]
evalOneHot[ius.ivs,testFeatures,testResponse]
XtX=Transpose[X].X;XtY=Transpose[X].Y;
{r=FindMinimum[f[us,vs],Join[variableWithInitial[vec@us,vec@ius],variableWithInitial[vec@vs,vec@ivs]]
	(*Join[variableWithInitial[vec@us,RandomReal[1,Dimensions@vec@us]],variableWithInitial[vec@vs,RandomReal[1,Dimensions@vec@vs]]]*)
	,Gradient:>g[us,vs]];//AbsoluteTiming,r[[1]]}
ws5=(us/.Dispatch[r[[2]]]).(vs/.Dispatch[r[[2]]]);
evalOneHot[ws5,testFeatures,testResponse]
ImageAdjust@showImage@#&/@Transpose[ws5]


(*Logistic: k=10, 0.52 (with 3000), 0.79*)
(*Logistic: k=30, 0.80 (with 3000), 0.897 takes 100s*)
(*SoftMax: k=30, 0.909 takes 117s*)
(*Logistic: k=100, 0.89 (with 3000), 0.929 takes 340s.*)
(*SoftMax: k=100, 0.934 takes 353s.*)
(*Logistic: k=300, 0.908 (with 3000), 0.9407, takes 907s.*)
(*SoftMax: k=300, 0.952, takes 995s.*)
(*SoftMax: k=500, 0.9558, takes 2473s.*)
evalOneHotTwoLayerNeural=Function[{ws,us,testFeatures,testResponse},N@Total@MapThread[Boole[#==#2]&
	,{testResponse,decodeOneHot[#]-1&/@sigmoid2D[sigmoid2D[testFeatures.ws].us]}]/Length@testFeatures];
Clear[twoLayerNeuralNetwork];cnt=0;
twoLayerNeuralNetwork[xs_List,ys_List,k_Integer,OptionsPattern[{"Init"->"Random","Method"->"Logistic","MaxIterations"->100}]]:=
	Module[{ws,us,w,u,f,g,r,ss,rs,es,rpe,xst=Transpose@xs,\[Lambda]=0.001 Length@xs,useSoftMax=OptionValue["Method"]==="SoftMax",b=1000},
	ws=Array[w,{Dimensions[xs][[2]],k}];us=Array[u,{k,Dimensions[ys][[2]]}];
	f[ws_?(NumericQ@#[[1,1]]&),us_]:=(cws=ws;cus=us;ss=sigmoid2D[xs.ws];
		rs=If[useSoftMax,softMaxRows2D[ss.us,b],sigmoid2D[ss.us]];es=rs-ys;rpe=If[useSoftMax,
				MapThread[softMaxJacobianFromValue[#2].#&,{es,rs},1],(rs (1-rs)) es];
			With[{c=pnorm2[es,2]+\[Lambda] pnorm2[ws,2]+\[Lambda] pnorm2[us,2]},If[Mod[cnt++,20]==0,cws=ws;cost=c];c]);
	g[ws_?(NumericQ@#[[1,1]]&),us_]:=2 Join[vec[xst.(ss (1-ss)(rpe.Transpose[us]))+\[Lambda] ws],vec[Transpose[ss].rpe+\[Lambda] us]];
	{r=FindMinimum[f[ws,us],If[Head[OptionValue["Init"]]===List
		,Join[variableWithInitial[ws,OptionValue["Init"][[1]]],variableWithInitial[us,OptionValue["Init"][[2]]]],
			Join[variableWithInitial[ws,RandomReal[{-1,1},Dimensions@ws]],variableWithInitial[us,RandomReal[{-1,1},Dimensions@us]]]]
		,Gradient:>g[ws,us],MaxIterations->OptionValue["MaxIterations"]];//AbsoluteTiming,r[[1]]};
	(*Print[evalOneHotTwoLayerNeural[cws,cus,testFeatures,testResponse]];*)
	{ws/.Dispatch[r[[2]]],us/.Dispatch[r[[2]]]}];
(*(*We can learn XOR now.*)Table[X=Append[#,1]&/@Tuples[{0,1},2];Y={0,1,1,0};
	{ws,us}=twoLayerNeuralNetwork[X,{#,1-#}&/@Y,3,"Method"->"SoftMax"];evalOneHotTwoLayerNeural[ws,us,X,Y],{10}]*)
Dynamic@{ImageAdjust@showImage@#&/@Transpose@cws,cost,evalOneHotTwoLayerNeural[cws,cus,testFeatures,testResponse]}
k=20;(*method="SoftMax";*)method="Logistic";
(*{ws,us}=twoLayerNeuralNetwork[trainFeatures[[;;3000]],trainResponseOneHot[[;;3000]],k,{"Method"->method}];//AbsoluteTiming*)
{ws,us}=twoLayerNeuralNetwork[trainFeatures[[;;]],trainResponseOneHot[[;;]],k,
	{"Method"->method,"Init"->twoLayerNeuralNetwork[trainFeatures[[;;3000]],trainResponseOneHot[[;;3000]],k,
		{"Method"->method,"Init"->twoLayerNeuralNetwork[trainFeatures[[;;300]],trainResponseOneHot[[;;300]],k,
			{"Method"->method}]}]}];//AbsoluteTiming
evalOneHotTwoLayerNeural[ws,us,testFeatures,testResponse]
ImageResize[ImageAdjust@Image@Partition[#,Floor@Sqrt@k],50]&/@Transpose[us]


(*Clear[neuralNetwork];
neuralNetwork[trainFeatures[[;;300]],trainResponseOneHot[[;;300]],"Layers"->{{"Logistic",30},{"SoftMax",Null}}];
neuralNetwork[xs_List,ys_List,OptionsPattern[{"Init"->"Random"}]]:=
	Module[{wss,w,f,g,es,xst=Transpose@xs,\[Lambda]=0.01 Length@xs,b=1000,layers=OptionValue["Layers"],layerDims},
	layerDims=Partition[Join[{Dimensions[xs][[2]]},layers[[;;-2,2]],{Dimensions[ys][[2]]}],2,1];wss=Array[w,#]&/@layerDims;
	f[ws_?(NumericQ@#[[1,1]]&),us_]:=(cws=wss[[1]];cus=us;ss=sigmoid2D[xs.ws];
		rs=If[useSoftMax,softMaxRows2D[ss.us,b],sigmoid2D[ss.us]];es=rs-ys;rpe=If[useSoftMax,
				MapThread[softMaxJacobianFromValue[#2].#&,{es,rs},1],(rs (1-rs)) es];
			With[{c=pnorm2[es,2]+\[Lambda] pnorm2[ws,2]+\[Lambda] pnorm2[us,2]},If[Mod[cnt++,20]==0,cws=ws;cost=c];c]);
	g[ws_?(NumericQ@#[[1,1]]&),us_]:=2 Join[vec[xst.(ss (1-ss)(rpe.Transpose[us]))+\[Lambda] ws],vec[Transpose[ss].rpe+\[Lambda] us]];
	{r=FindMinimum[f[ws,us],If[Head[OptionValue["Init"]]===List
		,Join[variableWithInitial[ws,OptionValue["Init"][[1]]],variableWithInitial[us,OptionValue["Init"][[2]]]],
			Join[variableWithInitial[ws,RandomReal[{-1,1},Dimensions@ws]],variableWithInitial[us,RandomReal[{-1,1},Dimensions@us]]]]
		,Gradient:>g[ws,us](*,MaxIterations->1000*)];//AbsoluteTiming,r[[1]]};
	(*Print[evalOneHotTwoLayerNeural[cws,cus,testFeatures,testResponse]];*)
	{ws/.Dispatch[r[[2]]],us/.Dispatch[r[[2]]]}];*)


(*Auto encoder has output=input*)
Dynamic@{ImageAdjust@showImage@#&/@Transpose@cws,Sqrt@cost(*,evalOneHotTwoLayerNeural[cws,cus,testFeatures,testResponse]*)}
k=10;
{ws,us,error}=twoLayerNeuralNetwork[trainFeatures[[;;]],trainFeatures[[;;]],k,
	{"Init"->twoLayerNeuralNetwork[trainFeatures[[;;3000]],trainFeatures[[;;3000]],k,
		{"Init"->twoLayerNeuralNetwork[trainFeatures[[;;300]],trainFeatures[[;;300]],k]}]}];//AbsoluteTiming


Dynamic@{ImageAdjust@showImage@#&/@Transpose[cws],cost}
denseRpca2UnitaryWithInit=Function[{D,initL,initS,iter,k},Module[{norm2,\[Lambda],\[Eta]=10.,Y,L=initL,S=initS,\[Mu],\[Rho],m,n,Zl,Zs,OL,svd},
	(*SVD: min ||L||_* + \[Lambda]||S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	{m,n}=Dimensions[D];\[Lambda]=Min[0.5,100./GeometricMean@{m,n}];norm2=Norm[D];
	Y=0D;(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];*)\[Mu]=12.5/norm2;\[Rho]=1.01;
	Do[Zl=Y/\[Mu]+D-S;OL=L;svd=SingularValueDecomposition[Zl,k];cws=svd[[3]];
	L=svd[[1]].dShrinkage[1/\[Mu],svd[[2]]].Transpose[svd[[3]]];
	Zs=Y/\[Mu]+D-L;
	S=dShrinkage[\[Lambda]/\[Mu],Zs];
    Y=Y+\[Mu](D-L-S);cost={pnorm[D-L-S,2],pnorm[#,2]&@S,Total@Diagonal@svd[[2]]}/norm2;
	\[Mu]=\[Rho] \[Mu];
	,{j,iter}];
	{L,S}]];
X=trainFeatures[[;;3000]];
LS=denseRpca2UnitaryWithInit[X,X,0X,300,20];


(*NaiveBayes 0.844*)
priors=Log@N@Standardize[SortBy[Tally[train[[;;,1]]],First][[;;,2]],0&,Total];
Print[{#[[1,1]],{ImageAdjust@showImage@Mean@#,ImageAdjust@showImage@StandardDeviation@#}&@#[[;;,2;;]]
	}&/@SplitBy[SortBy[train[[;;]],First],First]];
stats=({Mean@#,StandardDeviation@#}&@#[[;;,2;;]]&/@SplitBy[SortBy[train[[;;]],First],First]);
meanStd=Mean@Flatten@stats[[;;,2]];
jf=Function[{testFeature,stats},(Total@Flatten[-(Most@testFeature-#[[1]])^2/(meanStd/5+#[[2]])]&/@stats)+priors];
predictions=First@Ordering[jf[#,stats],-1]-1&/@testFeatures;
N@Total@MapThread[Boole[#==#2]&,{testResponse,predictions}]/Length@testFeatures


(*With[{K=Transpose[X].X},
	Table[indices=Sort@RandomSample[Range@Length@X,k];pnorm2[Transpose[X[[indices]]].X[[indices]]-K,2]
,{k,{10,100,1000,10000}}]]*)


SeedRandom[1003];x=RandomReal[1,20];f=Function[x,((1+x x)-1)/(x.x)];
(*ListLinePlot[Rest@NestList[f,x,10],PlotRange->All]
ListLinePlot[Rest@NestList[f,100 x,10],PlotRange->All]
ListLinePlot[First/@Rest@NestList[softMaxRows2D[#,10]&,{x},10],PlotRange->All]
ListLinePlot[First/@Rest@NestList[softMaxRows2D[#,10]&,{100x},10],PlotRange->All]
ListLinePlot[First/@Rest@NestList[chop2D@{softMaxRows2D[#,10][[1]]#[[1]]}&,{x},10],PlotRange->All]*)
ListLinePlot[First/@Rest@NestList[(*chop2D@*){softMaxRows2D[#,10][[1]]#[[1]]}&,{100x},1],PlotRange->All]


powerDot=Function[p,Compile[{{x,_Real,1},{y,_Real,1}},Dot[Sign[#]Power[Abs[#],p/2]&/@x,Sign[#]Power[Abs[#],p/2]&/@y],CompilationTarget:>"C"]];
powerDot[2][Range[3],Range[3]]
Dot[Range[3],Range[3]]
powerDot[1][Range[3],Range[3]]
powerDot[3][Range[3],Range[3]]


(*poly kernel cannot separate radius =(1,3,5)*)
colorPoints=Flatten[Table[{Switch[r,1,Blue,4,Red,_,Green],{5r Cos@t,5r Sin@t,z}},{r,{1,4,16}},{t,0,7/4 Pi,0.1},{z,0,1,0.3}],2];
g1=Graphics3D[{#[[1]],Point@#[[2]]}&/@colorPoints];
X=Standardize[colorPoints[[;;,2]],Mean,1&];V=SingularValueDecomposition[X,2][[3]];
g2=Graphics[MapThread[{#,Point@#2}&,{colorPoints[[;;,1]],X.V}],ImageSize->500];

principalComponentsProjectionF=Function[{X,numComponent,kernel},Module[{preK,K,U},preK=Outer[kernel,X,X,1];
	K=Transpose[Standardize[Transpose@Standardize[preK,Mean,1.&],Mean,1.&]];U=SingularValueDecomposition[K,numComponent][[1]];Function[x,(kernel[x,#]&/@X).U]]];
principalComponentsProjections=Function[{X,numComponent,kernel},Module[{preK,K,U},preK=Outer[kernel,X,X,1];
	K=Transpose[Standardize[Transpose@Standardize[preK,Mean,1.&],Mean,1.&]];U=SingularValueDecomposition[K,numComponent][[1]];K.U]];
gs=Table[f=principalComponentsProjectionF[X,2,kernel];
{Graphics[MapThread[{#,Point@#2}&,{colorPoints[[;;,1]],principalComponentsProjections[Standardize[X,Mean,1&],2,kernel]}],ImageSize->500],
Graphics[MapThread[{#,Point@#2}&,{colorPoints[[;;,1]],f/@X}],ImageSize->500]}
,{kernel,{Exp[-Norm[#-#2]^2/200.]&,(#.#2+1)^2&,powerDot[1],powerDot[3],Dot}}];
Join[{g1,g2},gs]


Clear[robustPca];
robustPca[D_List,\[CapitalOmega]_List,OptionsPattern[{"Method"->"SVD","Init"->"Zero","MaxIterations"->100,"mu"->12.5,"rho"->1.005}]]:=
	Module[{dim=Dimensions@D,norm2,\[Lambda],\[Eta]=10.,Y,L,S,\[Mu],\[Rho],m,n,Zl,Zs,OL,unitaryType=OptionValue["Method"]},(*L,S,D are m-by-kn. \[CapitalOmega] is nonzeros*)
	(*SVD: min ||L||_* + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)(*Fourier: min ||FLF^T||_ 1 + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	Switch[OptionValue["Init"],"Zero",L=0D;S=0D;,"Random",L=RandomReal[1,dim];S=RandomReal[1,dim];,_,{L,S}=OptionValue["Init"]];
	{m,n}=Dimensions[D];\[Lambda]=Min[0.5,100./GeometricMean@{m,n}];norm2=SingularValueDecomposition[D,1][[2,1,1]];
	Y=0D;(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];*)\[Mu]=OptionValue["mu"]/norm2;\[Rho]=OptionValue["rho"];
	Do[Zl=Y/\[Mu]+D-S;OL=L;
	L=Switch[unitaryType,"SVD",sShrinkage[1/\[Mu],Zl],
		"FourierLaplace",Re@InverseFourier@cShrinkageHadamard[0.001/\[Mu](KroneckerProduct@@(Sqrt[Range[0,#-1]]&/@Dimensions@Zl)),Fourier@Zl],
		"Fourier",Re@InverseFourier@cShrinkage[0.02/\[Mu],Fourier@Zl],
		"Hadamard",With[{padded=padToTwoPower@Zl},Take[unVec[hadamardTransform@dShrinkage[0.02/\[Mu],hadamardTransform@vec@padded],Dimensions@padded],
			Sequence@@Dimensions@Zl]]];
	Zs=Y/\[Mu]+D-L;S=\[CapitalOmega] cShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
	If[pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];If[Mod[j,30]==0,PrintTemporary@ImageAdjust@Image@Re[L]];(*(scores=Append[scores,#];Print[#])&@evalPredicted[L,testM2];*)
	,{j,OptionValue["MaxIterations"]}];
	{L,S}];


ms=Table[randomOrthogonalMatrix[30],{2}];interM=Function[a,Re@MatrixExp[(1-a) MatrixLog@ms[[1]]+a MatrixLog@ms[[2]]]];
ListLogLogPlot@SingularValueList[vec/@Join[{ms[[1]],ms[[2]]},Table[interM[a],{a,0,1,0.2}]]]
ListLogLogPlot@SingularValueList[vec/@Join[{ms[[1]],ms[[2]]},Table[interM[a],{a,0,1,0.1}]]]
Manipulate[(*Image[#,ImageSize->{200}]&*)
	With[{mms={ms[[1]],interM[a],ms[[2]]}},{SingularValueList[vec/@mms],MatrixPlot/@mms}],{a,0,1,0.01}]


ImageAdjust@showImage@#&/@Transpose[SingularValueDecomposition[digits[[1]]][[3]]]


visualizePowerLaw[#/First[#]]&@SingularValueList@#&/@digits
visualizePowerLaw[#/First[#]]&@SingularValueList@#&/@Partition[train[[]],Length@digits[[1]]]


m=digits[[1,;;500]];mask=Developer`ToPackedArray@N@Normal@randomSparseTensor[Dimensions@m,0.5];
LS=robustPca[mask m,mask];


(*Norm[mask m - m,"Frobenius"]/Norm[m,"Frobenius"]
Norm[LS[[1]] - m,"Frobenius"]/Norm[m,"Frobenius"]*)
Image/@{m,mask m,LS[[1]],LS[[2]]}


ListLogLogPlot[{SingularValueList@LS[[1]],SingularValueList@m}]


mask=randomSparseTensor[Dimensions@digits[[1]],0.5];
ListLogLogPlot[SingularValueList@#&/@{digits[[1]] mask,digits[[1]]}]


ListLogLogPlot[Parallelize[SingularValueList@#&/@digits],Joined->True]
ListLogLogPlot[Parallelize[SingularValueList@#&/@Partition[train[[]],Length@digits[[1]]]],Joined->True]
ListLogLogPlot[Join[Parallelize[SingularValueList@#&/@digits],Parallelize[SingularValueList@#&/@Partition[train[[]],Length@digits[[1]]]]],Joined->True]


m=ImageData@ColorConvert[exampleStereoPairs[[1,1]],"Gray"];
visualizePowerLaw@SingularValueList[m]
visualizePowerLaw@SingularValueList[Fourier@m]
Fourier@m//Abs//Image


LS=robustPca[Abs@Fourier@m,Map[1&,m,{2}]];Image@Abs@#&/@LS


ListLogLogPlot[#/First[#]&/@{SingularValueList[LS[[1]]],SingularValueList@m},Joined->True]


ListLogLogPlot[{SingularValueList@LS2[[1]],SingularValueList@hm2},Joined->True]


hadamardTransform2D[LS2[[1]]]//Image//ImageAdjust
hadamardTransform2D[LS2[[2]]]//Image


m2=ImageData@ImageResize[Image@m,256{1,1}];
hadamardTransform2D=Function[m,Transpose[hadamardTransform/@Transpose[hadamardTransform/@m]]];
hm2=hadamardTransform2D@m2;hm2//Image
LS2=robustPca[hm2,Map[1&,m2,{2}]];
Image/@LS2


n=5;SeedRandom[1003];Clear[a];ass=Array[a,{2,n,n}];m=RandomReal[1,n{1,1}];dm=N@DiagonalMatrix[Reverse@Range@n];
{r=FindMinimum[Tr[dm.matrixExpSpecialOrthogonal[ass[[1]]].m.matrixExpSpecialOrthogonal[ass[[2]]]],Flatten@ass];//AbsoluteTiming,r[[1]]}
MatrixForm[matrixExpSpecialOrthogonal[ass[[1]]/.r[[2]]].m.matrixExpSpecialOrthogonal[ass[[2]]]/.r[[2]]]
MatrixForm/@SingularValueDecomposition[m]


ListLogLogPlot[{1.7 SingularValueList@RandomVariate[UniformDistribution[{-1,1}],400{1,1}]
	(*,0.7SingularValueList@RandomVariate[CauchyDistribution[0,1],400{1,1}]*)
	,3.6SingularValueList@RandomVariate[BetaDistribution[1,1],400{1,1}]
	,SingularValueList@RandomVariate[GammaDistribution[1,1],400{1,1}]
	,0.7SingularValueList@RandomVariate[LaplaceDistribution[0,1],400{1,1}]
	,SingularValueList@RandomVariate[NormalDistribution[],400{1,1}]},Joined->True]


n=4;SeedRandom[10003];as=Array[a,n{1,1}];m=RandomReal[1,n{1,1}];
{r=NMinimize[pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as];//AbsoluteTiming,r[[1]]}
MatrixForm[matrixExpSpecialOrthogonal[as/.r[[2]]].m]
MatrixForm/@SingularValueDecomposition[m]
MatrixForm/@QRDecomposition[m]


m=ImageData@ColorConvert[exampleStereoPairs[[1,1]],"Gray"];
ImageAdjust@Image@#&/@QRDecomposition@m
ImageAdjust@Image@#&/@SingularValueDecomposition@m


standardizeImage=Function[img,Module[{m=ImageData@img},
	Image@RotateLeft[m,Round[Total[Join@@MapIndexed[#2 #&,m,{2}]]/Total[Flatten@m]]-Round[Dimensions[m]/2]]]];
m=Partition[digits[[1,1,2;;]],28];(*{Image@m,standardizeImage@Image@m}*)
Parallelize[Flatten@ImageData@Identity@showImage@#&/@digits[[1]]]//MatrixPlot
Parallelize[Flatten@ImageData@standardizeImage@showImage@#&/@digits[[1]]]//MatrixPlot


MatrixPlot@digits[[2]]
Image@digits[[2]]
(*showImage/@digits[[1]]*)


Image/@QRDecomposition@digits[[1]]
MatrixPlot@#&/@SingularValueDecomposition@digits[[1]]


Function[img,{img,ImageCorrelate[img,img]//ImageAdjust,showFourier2D@Fourier@ImageData@ColorConvert[img,"Gray"]}]/@exampleStereoPairs[[;;,1]]


v=Flatten@ImageData@ColorConvert[ImageResize[exampleStereoPairs[[1,1]],{30,30}],"Gray"];
Table[RotateLeft[v,i],{i,Length@v}]//Dimensions
ListPlot[Rest@SingularValueList@Table[RotateLeft[v,i],{i,Length@v}],PlotRange->All]


img=ColorConvert[ImageResize[exampleStereoPairs[[1,1]],300{1,1}],"Gray"];


smoothed=ImageResize[ImageResize[img,ImageDimensions[img]/4],ImageDimensions[img]];
ImageDifference[img,smoothed]//ImageAdjust
LaplacianFilter[img,1]
LaplacianFilter[smoothed,1]
smoothed
GaussianFilter[img,3]
ImageAdd[smoothed,LaplacianFilter[img,1]]


filtered=GaussianFilter[img,5]
(Print@#;Print@ImageDeconvolve[filtered,GaussianMatrix[5],Method->#])&/@{"DampedLS","Tikhonov","TSVD","Wiener","Hybrid","SteepestDescent","RichardsonLucy"}
ImageDeconvolve[#,(*GaussianMatrix[5]*)BoxMatrix[3]/49,Method->"TotalVariation"]&@filtered
