(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
showImage=Image[#,ImageSize->{400}]&;


normF=Function[img2,{Total@Flatten@Abs[ImageData@LaplacianFilter[img2,1]],Total@Flatten@Abs@Fourier@ImageData@img2,schattenNorm[ImageData@img2,1]}[[1]]];
ListLinePlot[
	Table[img2=If[doMasking,ImageMultiply[mask,#]&,Identity]@ImageRotate[gray,t];{t,
		normF[img2]},{t,0,Pi,0.02}]
	,PlotRange->All,AxesLabel->{"angle","nuclear norm"}]


n=100;img=Binarize@ColorNegate@ImageResize[ImagePad[Rasterize[Style["a",n]],n/2{{1,1},{1,1}},Padding->1],n{1,1}];gray=ColorConvert[img,"Gray"];doMasking=True;
mask=ColorConvert[ImageResize[Image@Graphics[{EdgeForm[White],FaceForm[White],Disk[n/2{1,1},n/2]},Background->Black],n{1,1}],"Gray"];
Manipulate[img2=If[doMasking,ImageMultiply[mask,#]&,Identity]@ImageRotate[gray,t];
Labeled[img2,"nuclear norm="<>ToString[schattenNorm[ImageData@img2,1]]],{t,0,Pi}]
vals=Table[img2=If[doMasking,ImageMultiply[mask,#]&,Identity]@ImageRotate[gray,t];{t,schattenNorm[ImageData@img2,1]},{t,0,Pi,0.02}];
Import@Export["t.png",#]&@ListLinePlot[
	vals
	,AxesLabel->{"angle","nuclear norm"}]
Table[t,{t,0,Pi,0.02}][[Ordering[vals[[;;,2]],1][[1]]]]


normalizeByRotationalHomography=Function[{pts,fun},Module[{a,as,h,f,r},Clear[a];as=Array[a,3{1,1}];
	h[as_]:=With[{homog=matrixExpSpecialOrthogonal@Clip[as,0.03{-1,1}]},transformByHomography[#,homog]&/@pts];
	f[as_?(NumberQ@#[[1,1]]&)]:=fun[h[as]];
	r=NMinimize[f[as],Flatten@as];h[as/.r[[2]]]]];
SeedRandom[1005];
Transpose@Parallelize@Table[m=SparseArray[1-(Rasterize[i//Magnify[#,3]&]//Binarize//ImageData)];h=Re@MatrixExp[0.03MatrixLog@randomSpecialOrthogonalMatrix[3]];
pts0=Standardize[m["NonzeroPositions"],Mean,1&];pts=transformByHomography[#,h]&/@pts0;
(*Graphics[{Blue,Point@pts,Red,Point@Standardize@PrincipalComponents@pts,Green,Point@normalizeByRotationalHomography[pts,pnorm[#,3]&]}]*)
{Graphics[{Blue,Point@pts}],Graphics[{Red,Point@Standardize@PrincipalComponents@pts}]
	,Graphics[{Green,Point@normalizeByRotationalHomography[pts,pnorm[#,3]&]}]},{5},{i,{"\:5929\:5730","\:4e0b","\:4e8b"}}]



(*Order is important in normalization*)
MatrixForm/@QRDecomposition[MatrixExp[strictUpperTriangle[RandomReal[1,2{1,1}]]].randomOrthogonalMatrix[2]]


(*We can learn the Lie group from examples*)
SeedRandom[1003];mat=NullSpace@Table[m=RandomReal[1,3{1,1}];mm=m-Transpose[m](*m-DiagonalMatrix[Diagonal[m]]*);vec@mm,{100}];mat//Chop//MatrixForm

bases=unVec[#,3{1,1}]&/@NullSpace@mat;MatrixForm/@bases
nm=Total[bases RandomReal[1,Length@bases]];nm//MatrixForm
Tr[nm]
nm+Transpose[nm]//Chop





(*Change under different Missing Ratio*)
m=imageFs[[1]][300];Table[ImageAdjust@Image[m randomSparseTensor[Dimensions@m,nnzRatio]],{nnzRatio,{0.01,0.05,0.1,0.2,0.5,0.9,0.95}}]
ListLogLogPlot[Table[SingularValueList[m randomSparseTensor[Dimensions@m,nnzRatio]],{nnzRatio,{0.01,0.05,0.1,0.2,0.5,0.9,0.95}}]
	,Sequence@@options,PlotLabel->"SVD"]


{lines,rating}=movieLens100k[];mask=Normal@N@Map[Boole[#>=1]&,rating[[;;,;;]],{2}];
sortedRating=Transpose[SortBy[Transpose@SortBy[Normal@rating,zeroCount],zeroCount]];
Table[ListLogLogPlot@SingularValueList@standardizeTensor@Take[sortedRating,{1,n},{1,n}],{n,{30,100,300,900}}]


(*a is near 0, b is around 0.8imageSize, c is around 1*)
Table[svs=SingularValueList[standardizeTensor[imageF[30]]][[;;20]];rule=FindFit[svs,a+b x^(-c),{a,b,c},x];
	ListLogLogPlot[{svs,Table[(a+b x^(-c))/.rule,{x,Length@svs}]},PlotLabel->rule,ImageSize->300],{imageF,imageFs}]


(*c is 0.4 - 0.7*)wm=walshMatrix[8];
Table[m=standardizeTensor@ImageData@ColorConvert[ImageResize[img,256{1,1}],"Gray"];
wvs=Reverse[Sort[Abs@Flatten[wm.m.wm]]][[;;30]];
rule=FindFit[Reverse[Sort[Abs@Flatten[wm.m.wm]]][[;;30]],b x^(-c),{b,c},x];
{img,ListLogLogPlot[wvs,Sequence@@(options/.((ImageSize->sz_)->ImageSize->300)),PlotLabel->"Walsh"]
	,ListLogLogPlot[{wvs,Table[(b x^(-c))/.rule,{x,Length@wvs}]
			,Table[(wvs[[1]] x^(-0.8))/.rule,{x,1,Length@wvs}]},PlotLabel->rule,ImageSize->300]},{img,Join[Import/@{"/h/t.jpg","/h/t5.jpg","/h/t15.jpg"}
		,exampleStereoPairs[[;;,1]]]}]


(*Uniform and Normal noise are easy to distinguish, but Cauchy noise is quite not distinguishable.*)
SeedRandom[1003];
randLowRankUF=Function[{n,ratio},standardizeTensor[RandomReal[1,{n,Round[n ratio]}].RandomReal[1,{Round[n ratio],n}]]];
randUniF=Function[n,standardizeTensor@randomOrthogonalMatrix@n];
randUF=Function[n,standardizeTensor@RandomReal[1,n{1,1}]];
randNF=Function[n,standardizeTensor@RandomVariate[NormalDistribution[],n{1,1}]];
randLF=Function[n,standardizeTensor@RandomVariate[LaplaceDistribution[0,1],n{1,1}]];
randCF=Function[n,standardizeTensor@RandomVariate[CauchyDistribution[0,1],n{1,1}]];
imgs=exampleStereoPairs[[;;,1]];
imageFs=Function[img,Function[n,standardizeTensor@ImageData@ColorConvert[ImageResize[img,n{1,1}],"Gray"]]]/@imgs;
fs=Join[{randLowRankUF[#,0.1]&,randLowRankUF[#,0.5]&,randUniF,randUF,randNF,randLF,randCF},imageFs];
labelGroups={{"LowRank-0.1","LowRank-0.5"},{"Unitary","Uniform","Normal","Laplace","Cauchy"},Table["Image-"<>IntegerString[i],{i,Length@imgs}]};
labels=Flatten@labelGroups;plotStyles=plotStylesForLabelGroups@labelGroups;
plotStyles=plotStylesForLabelGroups@labelGroups;
legends=LineLegend[Take[plotStyles,Length@labels],labels];
options={(*PlotRange->{Automatic,{10^-4,1000}},*)PlotLegends->legends,ImageSize->800,Joined->True,PlotStyle -> plotStyles};
With[{plot=ListLogLogPlot},Table[{plot[Table[m=f[n];SingularValueList@m,{f,fs}],Sequence@@options,PlotLabel->"SVD"]
	,plot[Table[m=f[n];Reverse[Sort[Abs@Flatten@Fourier[m]]][[;;n]],{f,fs}],Sequence@@options,PlotLabel->"Fourier"]
	,plot[Table[m=f[n];Reverse@Sort@Abs@Diagonal@Last@luDecomposition@m,{f,fs}],Sequence@@options,PlotLabel->"LU"]
	,plot[Table[m=f[n];Reverse@Sort@Abs@Diagonal@Last@QRDecomposition@m,{f,fs}],Sequence@@options,PlotLabel->"QR"]
	}
,{n,{(*90,*)300}}]]


Clear[f,x,t,s];SeedRandom[1003];n=3;m=RandomReal[1,n{1,1}];tmax=1;ws=Outer[Abs@Subtract[#,#2]&,Range@n,Range@n,1];
f[xs_?(NumericQ@#[[1,1,1]]&)]:=
	With[{u=xs[[1]],v=xs[[2]]},{(ws Transpose[m.v]).Transpose[ws Transpose[u].m.v],Transpose[ws (Transpose[u].m)].(ws Transpose[u].m.v)}];
s=NDSolve[{x'[t]==f[x[t]],x[0]==Table[randomSpecialOrthogonalMatrix@n,{2}]},x,{t,0,tmax},MaxSteps->10000];//AbsoluteTiming
ListPlot@Transpose@Table[Flatten@First[x[t]/.s],{t,0,tmax,0.1}]


(*Tensor*)(*Weighted L2 also sparsifies like L1, but converges fast*)
SeedRandom[1001];Clear[a,f,g];k=2;n=5;p=2;ass=Array[a,{k,n,n}];m=RandomReal[1,Table[n,{k}]];
(*ws=Table[Abs[i-j]+Abs[j-k],{i,n},{j,n},{k,n}];*)ws=Table[Abs[i-j],{i,n},{j,n}];
(*ws=Array[n&,n{1,1}]-DiagonalMatrix@Reverse[Range[n]];(*We can force off-diagonals be zero, but cannot sort diagonals.*)*)
(*ws=Array[1&,n{1,1}]-IdentityMatrix[n];*)(*Seems also works, but converges slower.*)
(*ws=Array[1&,n{1,1}];*)
g=Function[ass,foldXUs[m,(*matrixExpSpecialOrthogonal*)Re@MatrixExp@nilTrace[#]&/@ass,{}]];
f[ass_?(NumericQ@#[[1,1,1]]&)]:=cost=pnorm2[ws g[ass],p];
Dynamic@cost
{r=FindMinimum[f[ass],Flatten@ass,MaxIterations->1000];//AbsoluteTiming,r[[1]]}
MatrixForm/@g[ass/.r[[2]]]
(*SingularValueList[m]*)
(*pnorm2[ws SingularValueDecomposition[m][[2]],p]*)


(*Weighted L2 also sparsifies like L1, but converges fast*)
SeedRandom[1001];Clear[a,f,g];n=5;p=2;ass=Array[a,{2,n,n}];m=RandomReal[1,n{1,1}];(*ws=outer[Range[n],Array[1&,n]];(*This works in the direction, but not exact.*)*)
ws=Outer[Abs@Subtract[#,#2]&,Range@n,Range@n,1];
(*ws=Array[n&,n{1,1}]-DiagonalMatrix@Reverse[Range[n]];(*We can force off-diagonals be zero, but cannot sort diagonals.*)*)
(*ws=Array[1&,n{1,1}]-IdentityMatrix[n];*)(*Seems also works, but converges slower.*)
(*ws=Array[1&,n{1,1}];*)
g=Function[ass,(matrixExpSpecialOrthogonal[ass[[1]]].m.matrixExpSpecialOrthogonal[ass[[2]]])];
f[ass_?(NumericQ@#[[1,1,1]]&)]:=cost=pnorm2[ws g[ass],p];
Dynamic@cost
{r=FindMinimum[f[ass],Flatten@ass,MaxIterations->1000];//AbsoluteTiming,r[[1]]}
{Reverse@SortBy[Diagonal@g[ass/.r[[2]]],Abs],Diagonal@g[ass/.r[[2]]]}
SingularValueList[m]
pnorm2[ws SingularValueDecomposition[m][[2]],p]


With[{xs=xs/.r[[2]]},{f[xs],Flatten[as.xs.ns-xs.symmetrize[Transpose[xs].as.xs.ns]]}]
With[{xs=RandomReal[1,Dimensions@xs]},{f[xs],Flatten[as.xs.ns-xs.symmetrize[Transpose[xs].as.xs.ns]]}]


SeedRandom[1003];xs=randomSpecialOrthogonalMatrix@n;\[Alpha]=0.01;xs1=procrustes[xs-\[Alpha] (as.xs.ns-xs.symmetrize[Transpose[xs].as.xs.ns])];
xs2=procrustes[xs-\[Alpha] skew[as.xs.ns]];
Function[xs,Tr[Transpose[xs].as.xs.ns]]/@{xs,xs1,xs2}


With[{xs=randomSpecialOrthogonalMatrix@n},
MatrixForm/@{xs.Transpose[xs],#.Transpose[#]&[xs+0.01(as.xs.ns-xs.symmetrize[Transpose[xs].as.xs.ns])],#.Transpose[#]&[xs+0.01skew[as.xs.ns]]}]


Clear[x,t,s];SeedRandom[1003];n=3;as=symmetrize@RandomReal[1,n{1,1}];tmax=1;ns=DiagonalMatrix[Reverse[Range[n]^2.]];
s=NDSolve[{x'[t]==2(as.x[t].ns-x[t].symmetrize[Transpose[x[t]].as.x[t].ns]),x[0]==randomSpecialOrthogonalMatrix@n},x,{t,0,tmax},MaxSteps->10000];//AbsoluteTiming
ListPlot@Transpose@Table[Flatten@First[x[t]/.s],{t,0,tmax,0.1}]


SeedRandom[1003];Clear[f,x];n=3;
(*as=symmetrize@RandomReal[1,n{1,1}];*)
as=Import@"/tmp/as.csv";
xs=Array[x,n{1,1}];ns=N@DiagonalMatrix@Reverse@Range[n];
f[xs_?(NumericQ@#[[1,1]]&)]:=-Tr[Transpose[xs].as.xs.ns];
{r=FindMinimum[{f[xs],Transpose[xs].xs==IdentityMatrix[n]},Flatten@xs
	,Gradient:>Flatten@skew[as.xs.ns]
	(*Gradient:>Flatten[as.xs.ns-xs.symmetrize[Transpose[xs].as.xs.ns]]*)];//AbsoluteTiming,r[[1]]}
xs/.r[[2]]


(*Eval the spectrum over many images*)
Parallelize@Table[Module[{data=ImageData@ColorConvert[ImageResize[exampleStereoPairs[[i,1]],64{1,1}],"Gray"]
	,wms,power},
	wms=walshMatrix[Log[2,#]]&/@Dimensions[data];
	power=Join@@MapIndexed[{Log[Times@@#2],Log@Abs@#}&,wms[[1]].data.wms[[2]],{2}];
	(*Print[Image@data];*){Image@data,ListPlot[power(*,PlotRange->All*)]}]
,{i,Length@exampleStereoPairs}]


RpcaKroneckerWithInit=Function[{regularizer,regularizerGradient,D,\[CapitalOmega]in,initL,initS,iter},Module[{dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L,r,f,g,x,xs,s\[CapitalOmega]},
	xs=Array[x,Dimensions@D];s\[CapitalOmega]=Sqrt[\[CapitalOmega]];
	f[xs_?(NumericQ@#[[1,1]]&)]:=regularizer[xs]+pnorm2[s\[CapitalOmega] (xs-D),2];
	g[xs_?(NumericQ@#[[1,1]]&)]:=vec[regularizerGradient[xs]]+vec[2\[CapitalOmega](xs-D)];
	r=FindMinimum[f[xs],vec@xs,Gradient:>g[xs]];L=xs/.Dispatch[r[[2]]];{L,D-L}]];
Clear[RpcaSimpleLaplaceFull];
RpcaSimpleLaplaceFull[data\[CapitalOmega]_,\[CapitalOmega]_,OptionsPattern[{"MaxIterations"->100,"lambda"->0.2,"Type"->"Balanced"}]]:=
		Module[{dim,regularizer,regularizerGradient,grads,lap,\[Lambda],useCircular=OptionValue["Type"]==="Circular"},
	\[Lambda]=N@OptionValue["lambda"];dim=Dimensions@data\[CapitalOmega];grads=N@If[useCircular,circularGradientMatrix1D,balancedGradientMatrix1D][#]&/@dim;
	lap=If[useCircular,circularPoissonMatrix,balancedPoissonMatrix][dim];
	regularizer=\[Lambda] (pnorm2[grads[[1]].#,2]+pnorm2[#.grads[[2]],2])&;regularizerGradient=\[Lambda] lap.vec[#]&;
	RpcaKroneckerWithInit[regularizer,regularizerGradient,data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],OptionValue["MaxIterations"]]];
(*i=10;{L[i],S[i]}=RpcaSimpleLaplaceFull[data\[CapitalOmega],\[CapitalOmega]];evalResult["RpcaSimpleLaplaceFull-Balanced",L[i],S[i]]
i=11;{L[i],S[i]}=RpcaSimpleLaplaceFull[data\[CapitalOmega],\[CapitalOmega],"Type"->"Circular"];evalResult["RpcaSimpleLaplaceFull-Circular",L[i],S[i]]*)

(*Parallelize@Table[With[{D=data\[CapitalOmega]},\[CapitalLambda]s=DiagonalMatrix@Power[Range[#],c]&/@Dimensions[D];wms=walshMatrix[Log[2,#]]&/@Dimensions[D];\[CapitalLambda]2s=#^2&/@\[CapitalLambda]s;\[Mu]=0.001;];
i=10;{{L[i],S[i]}=RpcaKroneckerWithInit[\[Mu]/2 pnorm2[Abs[\[CapitalLambda]s[[1]].wms[[1]].#.wms[[2]].\[CapitalLambda]s[[2]]],2]&
		,\[Mu] wms[[1]].\[CapitalLambda]2s[[1]].wms[[1]].#.wms[[2]].\[CapitalLambda]2s[[2]].wms[[2]]&,data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],maxIter];//AbsoluteTiming
	,{pnorm[((1-\[CapitalOmega])(L[i]-data))^2,2],Image@L[i],ImageAdjust@Image@S[i]}},{c,{0.2,0.4,0.6,0.8}}]*)
(*fm=Fourier[];*)

RpcaSimpleLaplaceFullCircularFractional[data\[CapitalOmega]_,\[CapitalOmega]_,OptionsPattern[{"MaxIterations"->100,"lambda"->0.2,"alpha"->1,"p"->2}]]:=
		Module[{dim,regularizer,regularizerGradient,grads,lap,\[Lambda],\[Alpha],\[CapitalLambda]2s,\[CapitalLambda]s,ess,useL2Loss=Switch[OptionValue["p"],2,True,1,False,_,Abort[]]},
	\[Lambda]=N@OptionValue["lambda"];dim=Dimensions@data\[CapitalOmega];\[Alpha]=N@OptionValue["alpha"];
	\[CapitalLambda]2s=N@Power[diagonalizedCircularLaplacian1D@#,\[Alpha]]&/@dim;\[CapitalLambda]s=N@Power[diagonalizedCircularLaplacian1D@#,\[Alpha]/2]&/@dim;
	regularizer=(ess={\[CapitalLambda]s[[1]].Fourier[#]&/@Transpose[#],\[CapitalLambda]s[[2]].Fourier[#]&/@#};\[Lambda] If[useL2Loss,pnorm2[ess,2],Plus@@(relativisticNorm2D[Abs[#],1]&/@ess)])&;
	regularizerGradient=\[Lambda] Re@vec[If[useL2Loss,(Transpose[InverseFourier[\[CapitalLambda]s[[1]].#]&/@ess[[1]]])+(InverseFourier[\[CapitalLambda]s[[2]].#]&/@ess[[2]])
		,gradientRelativisticNorm2D[Transpose[InverseFourier[\[CapitalLambda]s[[1]].#]&/@ess[[1]]],1]
			+gradientRelativisticNorm2D[InverseFourier[\[CapitalLambda]s[[2]].#]&/@ess[[2]],1]]]&;
	RpcaKroneckerWithInit[regularizer,regularizerGradient,data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],OptionValue["MaxIterations"]]];

Table[++i;Print[{L[i],S[i]}=RpcaSimpleLaplaceFullCircularFractional[data\[CapitalOmega],\[CapitalOmega],"alpha"->\[Alpha]];//AbsoluteTiming];
	evalResult[{"RpcaSimpleLaplaceFullCircularFractional","\[Alpha]",\[Alpha],"i",i},L[i],S[i]],{\[Alpha],{(*0.2*)(*0.5,0.8,1,*)(*1.1,1.2,*)2(*,3,4*)}}]


evalResult["",(*fixWithObserved[\[CapitalOmega],data\[CapitalOmega],*)borderMask[dim] L[5]+(1-borderMask[dim])L[12],S[12]]
evalResult["",fixWithObserved[\[CapitalOmega],data\[CapitalOmega],borderMask[dim] L[5]+(1-borderMask[dim])L[12]],S[12]]


RpcaSparseLaplaceWithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L,S,\[Eta]=1.,\[Rho]=1.1,\[Lambda]=1.,\[CapitalOmega]c},
	\[CapitalOmega]c=DiagonalMatrix[1-vec[\[CapitalOmega]]];S=\[CapitalOmega]c.(vec@RandomReal[1,Dimensions@D]);
	A2=SparseArray[\[CapitalOmega]c.balancedPoissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix@vec[\[CapitalOmega]]+A2];
	Do[L=LeastSquares[A,vec[D]+S];Print[Max@Abs@Histogram[Flatten[A2.L]]];S=dShrinkage[\[Lambda]/\[Eta],A2.L];\[Eta]*=\[Rho];
	,{j,iter}];L=unVec[L,dim];{L,D-L}]];
i=10;{L[i],S[i]}=RpcaSparseLaplaceWithInit[data\[CapitalOmega],\[CapitalOmega],0data\[CapitalOmega],0data\[CapitalOmega],10];//AbsoluteTiming
evalResult["RpcaSparseLaplaceWithInit",L[i],S[i]]


evalResult=Function[{tag,L,S},{pnorm2[((1-\[CapitalOmega])(L-data)),2]/pnorm[1-\[CapitalOmega],1],pnorm2[(\[CapitalOmega](L-data)),2]/pnorm[\[CapitalOmega],1],Image@L,ImageAdjust@Image@S,tag}];
fixWithObserved=Function[{\[CapitalOmega],data\[CapitalOmega],L},(1-\[CapitalOmega])L+data\[CapitalOmega]];
(*leastSquaresL1=Function[{as,bs,maxIter},Module[{f,apb,\[Tau]=0.1},apb=LeastSquares[as,bs,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}];
	f=Function[xs,SparseArray[apb+LeastSquares[as,dShrinkage[\[Tau],as.xs-bs],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}]]];
	Nest[f,apb,maxIter]]];
RpcaSimpleLaplaceL1WithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega]]].poissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix[vec[\[CapitalOmega]]]+A2];
	L=unVec[leastSquaresL1[A,vec[D],iter(*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim];
	{L,D-L}]];*)
RpcaSimpleLaplaceFullWithInitLeastSquare=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	(*We will get artifacts if we don't enforce over borders.*)
	A2=SparseArray[(*sparseDiagonalMatrix[vec[1-borderMask@dim]].*)balancedPoissonMatrix[dim]];
	A=SparseArray@vertcat[A2,sparseDiagonalMatrix@vec[\[CapitalOmega]]];
	L=unVec[LeastSquares[A,Join[0vec[D],vec[D]](*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim];
	{L,D-L}]];
RpcaSimpleLaplaceWithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega]]].balancedPoissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix@vec[\[CapitalOmega]]+A2];
	L=unVec[LinearSolve[A,vec[D](*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim];
	{L,D-L}]];
RpcaSimpleLaplaceTraceNormWithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L,\[Tau]=0.1,\[Mu]=30,f},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega]]].balancedPoissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix@vec[\[CapitalOmega]]+A2];
	f=Function[xs,LinearSolve[\[Mu] sparseIdentityMatrix[Dimensions[A][[2]]]+A,\[Mu] vec[sShrinkage[\[Tau]/\[Mu],unVec[xs,dim]]]+vec[D]]];
	L=unVec[Nest[f,LinearSolve[A,vec[D]],iter],dim];
	(*L=unVec[LinearSolve[A,vec[D](*,Method->{"Krylov"(*,Preconditioner->"ILU0"*)}*)],dim];*)
	{L,D-L}]];
RpcaSimpleLaplaceTraceNormAdmmWithInit=Function[{D,\[CapitalOmega]in,initL,initS,iter},Module[{dim=Dimensions@D,L},
	RpcaShrinkOverLinearAdmmWithInit[List/@vec@sShrinkage[#,unVec[vec@#2,dim]]&,D,\[CapitalOmega]in,initL,initS,iter]]];
RpcaShrinkOverLinearAdmmWithInit=Function[{shrinkageOp,D,\[CapitalOmega]in,initL,initS,iter},Module[{A,A2,dim=Dimensions@D,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L,\[Tau]=0.1},
	A2=SparseArray[DiagonalMatrix[1-vec[\[CapitalOmega]]].balancedPoissonMatrix[Dimensions@D]];A=SparseArray[sparseDiagonalMatrix@vec[\[CapitalOmega]]+A2];
	L=unVec[vec@shrinkageOverLinearAdmm[shrinkageOp,\[Tau],A,List/@vec[D],iter],dim];{L,D-L}]];

(*Comparing Rpca Pvd Sud*)
SeedRandom[1003];n=128;Clear[L,S];data=ImageData@ColorConvert[ImageResize[Import["/h/t4.jpg"],n{1,1}],"Gray"];maxIter=50;
\[CapitalOmega]=randomSparseTensor[Dimensions[data],0.2];data\[CapitalOmega]=data \[CapitalOmega];{data\[CapitalOmega]//Image}

(*i=1;{U,X,V,S[i]}=robustPvd[Ds,\[CapitalOmega]s,Floor[0.1n],500];//AbsoluteTiming
L[i]=U.#.Transpose[V]&/@X;
{pnorm[((1-\[CapitalOmega]s)(L[i]-data))^2,2],showTensor3@L[i],ImageAdjust@showTensor3@S[i]}

i=2;{X2,Us2,S[i]}=robustSud[Ds,\[CapitalOmega]s,Floor[0.1n],1000];//AbsoluteTiming
L[i]=foldXUs[X2,Us2,{}];
{pnorm[((1-\[CapitalOmega]s)(L[i]-data))^2,2],showTensor3@L[i],ImageAdjust@showTensor3@S[i]}*)

i=3;{L[i],S[i]}=Rpca2[data\[CapitalOmega],\[CapitalOmega],maxIter];//AbsoluteTiming
evalResult["Rpca2",L[i],S[i]]

(*i=4;{L[i],S[i]}=Function[m,foldToTensor[m,{3,Dimensions[m][[1]],Dimensions[m][[2]]/3},2]]/@
	RpcaFourierWithInit[data\[CapitalOmega],\[CapitalOmega],maxIter];//AbsoluteTiming
{pnorm[((1-\[CapitalOmega])(L[i]-data))^2,2],Image@L[i],ImageAdjust@Image@S[i]}*)

i=5;{L[i],S[i]}=RpcaSimpleLaplaceWithInit[data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],0];//AbsoluteTiming
evalResult["RpcaSimpleLaplaceWithInit",L[i],S[i]]

(*i=6;{L[i],S[i]}=Function[m,foldToTensor[m,{3,Dimensions[m][[1]],Dimensions[m][[2]]/3},2]]/@
	RpcaSimpleLaplaceTraceNormWithInit[ArrayFlatten@{Ds},ArrayFlatten@{\[CapitalOmega]s},0ArrayFlatten@{\[CapitalOmega]s},0ArrayFlatten@{\[CapitalOmega]s},50];//AbsoluteTiming
{pnorm[((1-\[CapitalOmega]s)(L[i]-data))^2,2],showTensor3@L[i],ImageAdjust@showTensor3@S[i]}*)

i=7;{L[i],S[i]}=RpcaSimpleLaplaceTraceNormAdmmWithInit[data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],maxIter];//AbsoluteTiming
evalResult["RpcaSimpleLaplaceTraceNormAdmmWithInit",L[i],S[i]]

(*i=8;{L[i],S[i]}=RpcaSimpleLaplaceFullWithInit[data\[CapitalOmega],\[CapitalOmega],0\[CapitalOmega],0\[CapitalOmega],0];//AbsoluteTiming
{pnorm[((1-\[CapitalOmega])(L[i]-data))^2,2],Image@L[i],ImageAdjust@Image@S[i]}*)

i=9;{L[i],S[i]}=RpcaMeanFilterWithInit[data\[CapitalOmega],\[CapitalOmega],RandomReal[1,Dimensions@\[CapitalOmega]],0\[CapitalOmega],maxIter];//AbsoluteTiming
evalResult["RpcaMeanFilterWithInit",L[i],S[i]]


(*SeedRandom[1003];*)n=2;ys=(*randomSpecialOrthogonalMatrix@n*)RandomReal[1,n{1,1}];Clear[x,f];xs=Array[x,n{1,1}];ys
weights=Range[n];\[Tau]=0.1;
f[xs_?(NumericQ@#[[1,1]]&)]:=\[Tau] weightedSchattenNorm[xs,1,weights]+ 1/2 pnorm2[xs-ys,2];
r=NMinimize[f[xs],vec@xs,MaxIterations->500];
rs={xs/.r[[2]],ys,#[[1]].DiagonalMatrix[Diagonal[#[[2]]]-\[Tau] weights].Transpose[#[[3]]]&@SingularValueDecomposition@ys};
{f[#],MatrixForm@#}&/@rs
(*DiagonalMatrix[Range[n]].SingularValueList[ys].DiagonalMatrix[Range[n]]*)


SeedRandom[1003];Clear[a];as=Array[a,2{1,1}];ys=RandomReal[1,2{1,1}];
Map[D[vec[#].vec[#]&[ys-cayleyTransform[as-Transpose[as]]],#]&,as,{2}]//Simplify//MatrixForm


Plot3D[Max[0,1-Abs[8 x]]Max[0,1-Abs[8 y]],{x,-1,1},{y,-1,1},PlotRange->All]


SeedRandom[1003];num=5;
n=num+2;pulses=Table[Max[0,1-Abs[(n-1)/2 (x-(-1+2(i-1)/(n-1)))]],{i,2,n-1}];
Plot[pulses,{x,-1,1},PlotRange->All]
legendres=N@Table[LegendreP[i,x],{i,0,30}];
coeffM=Parallelize[N@Table[(i+1/2)Integrate[# LegendreP[i,x],{x,-1,1}],{i,0,Length@legendres-1}]&/@pulses];
Plot[{pulses[[1]],Simplify[legendres.coeffM[[1]]]},{x,-1,1}]


order=8;m=ImageData@ColorConvert[ImageResize[exampleStereoPairs[[1,1]],(2^order){1,1}],"Gray"];
Histogram[Flatten@m]


svs=SingularValueList@m;visualizePowerLaw[svs]
fCoeffs=Reverse[Sort[Flatten[Abs@Fourier[m]]]][[2;;50]];visualizePowerLaw[fCoeffs]
wm=walshMatrix[order];
wCoeffs=Reverse[Sort@Flatten[Abs[wm.m.wm]]][[2;;50]];visualizePowerLaw[wCoeffs]


(*maxRelativisticNorm2D=Compile[{{xs,_Real,2},{eps,_Real}},Module[{eps2=eps^2},Max/@Map[Sqrt[eps2+#^2]-eps&,xs,{2}]],CompilationTarget:>"C"];
gradientMax=Compile[{{xs,_Real,1},{eps,_Real}},With[{idx=First@Ordering[Abs[xs],-1],eps2=eps^2},xs[[idx]]/Sqrt[eps2+xs[[idx]]^2] oneHot[idx,Length@xs]]];

Clear[x,f,g];n=2;SeedRandom[1003];as=RandomReal[1,n];xs=Array[x,n];eps=10^-10;
f[xs_?(NumericQ@#[[1]]&)]:=First@maxRelativisticNorm2D[{xs-as},eps];
g[xs_?(NumericQ@#[[1]]&)]:=gradientMax[xs-as,eps];
r=FindMinimum[f[xs],xs,Gradient:>g[xs]];
{r[[1]],as,xs/.r[[2]]}*)


lab=ColorSeparate[exampleStereoPairs[[2,1]],"LAB"];
svd=SingularValueDecomposition[ImageData@lab[[1]]];
Image[#,ImageSize->400]&/@(Transpose[svd[[1]]].ImageData[#].svd[[3]]&/@lab)
MatrixPlot@Abs@Fourier@ImageData@#&/@lab
Table[MatrixPlot@outer[svd[[1,;;,k]],svd[[3,;;,k]]],{k,10}]


img=ColorConvert[exampleStereoPairs[[1,1]],"Gray"];(*img=lab[[2]];*)
radius=1;center=(((2radius+1)^2)-1)/2+1;
m=Prepend[Join[#[[;;center-1]],#[[center+1;;]]]&@Flatten[#],Flatten[#][[center]]]&/@Reap[ImageFilter[(Sow@#;Mean[#])&,img,radius]][[2,1]];
ws=LeastSquares[m[[;;,2;;]],m[[;;,1]]];ws
Log[(Norm[m[[;;,1]]-m[[;;,2;;]].ws])/Length[m]]
{img,ImageAdjust@Image@unVec[m[[;;,1]]-m[[;;,2;;]].ws,Dimensions@ImageData@img]}
(*(Norm[m[[;;,1]]-m[[;;,2;;]].{-0.25,0.5,-0.25,0.5,0.5,-0.25,0.5,-0.25}])/Length[m]*)


img
ImageAdjust@ImageFilter[{-0.25,0.5,-0.25,0.5,-1,0.5,-0.25,0.5,-0.25}.vec[#]&,img,1]
ImageAdjust@ImageFilter[({0,1,0,1,-4,1,0,1,0}/4).vec[#]&,img,1]


SeedRandom[1003];n=100;\[CapitalOmega]=randomSparseTensor[n{1,1},0.8];
m=outer[RandomReal[1,n],RandomReal[1,n]]\[CapitalOmega] ;Image[#,ImageSize->400]&/@{m,\[CapitalOmega]}
LS=robustPca[m,Map[1&,m,{2}],"mu"->1.25];
ImageAdjust@Image[#,ImageSize->400]&/@LS


rotateValue=Function[m,Module[{a,as},as=Array[a,Dimensions[m][[1]]{1,1}];FindMinimum[
	pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as][[1]]]];


m=RandomReal[1,2{1,1}];svd=SingularValueDecomposition@m;
{rotateValue@m,pnorm[svd[[2]].svd[[3]],1],pnorm[svd[[2]],1]}


(*SeedRandom[1003];*)
Clear[a];dim={3,3};as=Array[a,dim[[1]]{1,1}];
Table[m=RandomReal[1,dim];
	Join[{r=NMinimize[pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as];//AbsoluteTiming,r[[1]],pnorm[Last@QRDecomposition@m,1],rotateValue[m]}
		,MatrixForm/@Join[{matrixExpSpecialOrthogonal[as/.r[[2]]].m},QRDecomposition@m]],{10}]//TableForm


SeedRandom[1003];m=RandomReal[1,2{1,1}];
Table[r=NMinimize[If[p>2,-1,1]pnorm[RotationMatrix[\[Theta]].m,p],\[Theta]];{r[[1]],MatrixForm[RotationMatrix[\[Theta]/.r[[2]]].m]},{p,{1,2.5,3}}]


SeedRandom[1003];orbitvalue=rotateValue(*Total@Abs@Diagonal@Last@QRDecomposition@#&;*)
vals=Table[ass=RandomReal[1,{2,2,2}];{MatrixForm/@ass,orbitvalue[ass[[1]]]+orbitvalue[ass[[2]]]-2orbitvalue[(ass[[1]]+ass[[2]])/2]},{20}];
Select[vals,#[[2]]>0&]
Select[vals,#[[2]]<0&]


(*SeedRandom[1003];*)
exps=Parallelize@Table[xys=RandomReal[1,{50,3}];pos=RandomReal[1,{100,3}];
pvs=Table[m=#/pnorm[#,2]&@Outer[pnorm2[#-#2,pow]&,xys,pos,1];
	vals=SingularValueList@m;{pow,(*pnorm2[m,2],m//MatrixPlot,*)Total@vals(*,ListLogLogPlot[vals]*)},{pow,{1,1.5,1.9,2,2.1,2.5,3}}];
SortBy[pvs,#[[2]]&],{1000}];
exps[[;;,1]]


SeedRandom[1003];
(*Parallelize@*)Table[
ms=RandomReal[1,{2,2,2}];
normLike=Function[m,Module[{r,a,as},as=Array[a,2{1,1}];r=FindMinimum[pnorm[matrixExpSpecialOrthogonal[as].m,1],Flatten@as];{r[[1]],matrixExpSpecialOrthogonal[as/.r[[2]]].m}]];
(*Print[normLike[ms[[1]]]];*)
normLike[ms[[1]]][[1]]+normLike[ms[[2]]][[1]]-normLike[ms[[1]]+ms[[2]]][[1]],{50}]


MatrixForm/@Append[ms,Plus@@ms]
{normLike[ms[[1]]][[1]],normLike[ms[[2]]][[1]],normLike[ms[[1]]+ms[[2]]][[1]]}
schattenNorm[#,1]&/@Append[ms,Plus@@ms]


Clear[rotateSparse];
rotateSparse[ms_List,OptionsPattern[{"MaxIterations"->100,"lambda"->0}]]:=Module[{\[Lambda]=OptionValue["lambda"],maxIter=OptionValue["MaxIterations"]
		,a,as,rs,r,zs,xs,\[Mu]=1,\[Rho]=2},
	as=Array[a,Length[ms]{1,1}];zs=0ms;
	Do[r=FindMaximum[pnorm[matrixExpSpecialOrthogonal[as].xs,3],Flatten@as];rs=matrixExpSpecialOrthogonal[as/.r[[2]]];
	Transpose[rs].(ms-zs)
	,{j,maxIter}];
	];


pShrinkage=Function[{\[Tau],A,p},Module[{x,f,g,ms=A,r,xs},xs=Array[x,Dimensions@A];
	f[xs_?(NumericQ[#[[1,1]]]&)]:=pnorm[xs,p]+1/2pnorm2[xs-ms,2];g[xs_?(NumericQ[#[[1,1]]]&)]:=vec[xs^(p-2)Sign[xs]/pnorm[xs,p]^(p-1)+(xs-ms)];
	r=FindMinimum[f[xs],vec@xs,Gradient:>g[xs]];xs/.Dispatch[r[[2]]]
	]];
pShrinkageRotated=Function[{\[Tau],A,p},Module[{f,ms=A,r,a,as,xs,rs},as=Array[a,Dimensions[A][[2]]{1,1}];
	f[as_?(NumericQ@#[[1,1]]&)]:=(rs=matrixExpSpecialOrthogonal[as];xs=pShrinkage[\[Tau],A.rs,p];\[Tau] pnorm[xs,p]+1/2pnorm2[xs-A.rs,2]);
	r=NMaximize[f[as],Flatten@as];{rs,xs.Transpose[rs]}
	]];
pShrinkageRotated3D=Function[{\[Tau],A,p},Module[{f,ms=A,r,a,as,xs,rs},as=Array[a,Dimensions[A][[2]]];
	f[as_?(NumericQ@#[[1]]&)]:=(rs=MatrixExp@skewOmega[as];xs=pShrinkage[\[Tau],A.rs,p];\[Tau] pnorm[xs,p]+1/2pnorm2[xs-A.rs,2]);
	r=FindMaximum[f[as],Flatten@as];{rs,xs.Transpose[rs]}
	]];


SeedRandom[1003];n=400;
(*origM=(RandomReal[1,{n,3}].RandomReal[1,{3,n}]);*)
origM=ImageData[ImageResize[ColorConvert[Import["/h/t5.jpg"],"Gray"],n{1,1}]];
\[CapitalOmega]=randomSparseTensor[Dimensions@origM,0.5];
m=origM \[CapitalOmega];
Image[#,ImageSize->400]&/@{origM,\[CapitalOmega],origM \[CapitalOmega]}
LS=additiveDecomposition[m,\[CapitalOmega]];
Image[#,ImageSize->400]&/@{LS[[1]],-LS[[2]]}
LS2=Rpca2[m,\[CapitalOmega],500];Image[#,ImageSize->400]&/@{LS2[[1]],-LS2[[2]]}


SeedRandom[1003];ms=RandomReal[{-1,1},{3,3}];rot={{Cos@t,-Sin@t,0},{Sin@t,Cos@t,0},{0,0,1}};pnorm[ms.rot,3]//Simplify
Plot[pnorm[ms.rot,3],{t,0,2Pi}]
{r=FindMaximum[pnorm[ms.rot,3],t];//AbsoluteTiming,r[[1]]}
Graphics3D@Point[ms.(rot/.r[[2]])]
{r=NMaximize[pnorm[ms.rot,3],t];//AbsoluteTiming,r[[1]]}
rot/.r[[2]]
Graphics3D@Point[ms.(rot/.r[[2]])]


oms={{-1,-1},{1,-1},{1,1},{2,1},{0,3},{-2,1},{-1,1},{-1,-1}};ms=oms.{{1,a},{0,1}}/.{a->0.3};Graphics@Line@ms
{r=FindMinimum[Total@Flatten@Power[ms.{{1,a},{0,1}},8](*,Gradient:>D[Total@Flatten@Power[ms.{{1,a},{0,1}},4]*),a];//AbsoluteTiming,r[[1]]}
a/.r[[2]]
Graphics@Line[ms.({{1,a},{0,1}}/.r[[2]])]


oms={{-1,-1},{1,-1},{1,1},{2,1},{0,3},{-2,1},{-1,1},{-1,-1}};ms=oms.{{s,0},{0,1/s}}/.{s->0.3};Graphics@Line@ms
{r=FindMinimum[pnorm[ms.{{s,0},{0,1/s}},1](*,Gradient:>D[Total@Flatten@Power[ms.{{1,a},{0,1}},4]*),s];//AbsoluteTiming,r[[1]]}
s/.r[[2]]
Graphics@Line[ms.({{s,0},{0,1/s}}/.r[[2]])]


SeedRandom[1003];ms=RandomSample[ExampleData[{"Geometry3D","StanfordBunny"},"VertexData"],8000];ListSurfacePlot3D[ms,MaxPlotPoints->50]
mss=Table[ms.MatrixExp[nilTrace@RandomReal[0.3{-1,1},Dimensions[ms][[2]]{1,1}]],{5}];
normalized=Parallelize[(r=lieNormalize[#,"power"->3(*,"Group"->"Rotation"*)];ListSurfacePlot3D[Standardize[r[[2]]],MaxPlotPoints->50])&/@mss];
bunnies={ListSurfacePlot3D[#,MaxPlotPoints->50]&/@mss,normalized};bunnies//TableForm
(*Export["/h/d/bunnies.pdf",Rasterize[TableForm@bunnies,ImageSize->1000]]*)


ms=RandomSample[Import["http://exampledata.wolfram.com/dinosaur.ply.gz","VertexData"],500];
mss=Table[ms.MatrixExp[nilTrace@RandomReal[0.3{-1,1},Dimensions[ms][[2]]{1,1}]],{5}];
normalized=Parallelize[(r=lieNormalize[#,"power"->3(*,"Group"->"Rotation"*)];Graphics3D[{Red,Point[Standardize[r[[2]]]]},Axes->True])&/@mss];
(*Export["/s/workspace/sld/figure/sparse_linear_3d.pdf",*)Identity[TableForm@{Graphics3D@{Blue,Point[#]}&/@mss,normalized}](*]*)


ms=extractPointCloud@Rasterize[(*"\:5f20_"*)"STOP"(*"|"*),ImageSize->200];SeedRandom[1003];
mss=Table[ms.MatrixExp[nilTrace@RandomReal[1.{-1,1},Dimensions[ms][[2]]{1,1}]].randomOrthogonalMatrix[2],{5}];
normalized=Parallelize[(r=lieNormalize[#,"power"->3(*,"Group"->"Rotation"*)];Graphics[{Red,Point[fourDirectionNormalize@Standardize[r[[2]]]]},Axes->True])&/@mss];
(*Export["/s/workspace/sld/figure/sparse_linear_3d.pdf",*)Identity[TableForm@{Graphics@{Blue,Point[#]}&/@mss,normalized}](*]*)


(*Observations: special linear group is better than rotation group even if only rotation is involved. Order of transform also does not matter.*)
figs=Import["/h/brown_shape/99db/pngs/*.png"];selected=figs[[{1,3,8,16,30,48,60}]];
SeedRandom[1003];
Table[ms=extractPointCloud@selected[[i]];
(*ms=extractPointCloud@Rasterize[(*"\:5f20_"*)"STOP"(*"|"*),ImageSize->200];*)
(*mss=Table[ms.With[{\[Theta]=RandomReal[2Pi]},{{Cos[\[Theta]],-Sin[\[Theta]]},{Sin[\[Theta]],Cos[\[Theta]]}}].With[{a=RandomReal[3{-1,1}]},{{1,a},{0,1}}],{5}];*)
(*mss=Table[ms.With[{a=RandomReal[3{-1,1}]},{{1,a},{0,1}}].With[{\[Theta]=RandomReal[2Pi]},{{Cos[\[Theta]],-Sin[\[Theta]]},{Sin[\[Theta]],Cos[\[Theta]]}}],{5}];*)
mss=Table[ms.MatrixExp[nilTrace@RandomReal[{-1,1},Dimensions[ms][[2]]{1,1}]],{5}];
pcaNormalized=Graphics@{Green,Point@PrincipalComponents[#]}&/@N@mss;
rotationNormalized=Identity[(r=lieNormalize[#,"power"->3,"Group"->"Rotation"];
	{(*r[[1]],*)Graphics[{Purple,Point[fourDirectionNormalize@Standardize[r[[2]]]]},Axes->True]})&/@mss];
normalized=Identity[(r=lieNormalize[#,"power"->3(*,"Group"->"Rotation"*)];
	{(*r[[1]],*)Graphics[{Red,Point[fourDirectionNormalize@Standardize[r[[2]]]]},Axes->True]})&/@mss];
(*Export["/s/workspace/sld/figure/sparse_linear.pdf",*)
g=Rasterize[TableForm@MapThread[Prepend,{{Graphics@{Blue,Point[#]}&/@mss,pcaNormalized,rotationNormalized,normalized}
	,Style[#,{17,Bold}]&/@{"Distorted","PCA","GOO_SO","GOO_SL"}}],ImageSize->2000];
Export["/h/normalizing_"<>IntegerString[i]<>".pdf",g]
(*(*Sufficient but necessary for Transpose[mn].mn to be diagonal*)mn=lieNormalize[mss[[1]],"power"->3][[2]];Transpose[mn].mn*)
,{i,{1,5,6}}]


StringJoin@Table["0",{5-Length@IntegerString@#}]<>IntegerString[#]&[5]
"http://venge.net/graydon/talks/mkc/html/mgp00049.jpg"


(*Different sampling rate*)
SeedRandom[1003];
(*Parallelize@Do[mss=Table[extractPointCloud[(*Rasterize[(*"\:5f20_"*)"STOP"(*"|"*),ImageSize->sz]*)
		ImageResize[selected[[i]],sz]].MatrixExp[nilTrace@RandomReal[1,Dimensions[ms][[2]]{1,1}]]
	,{sz,20{2,3,4,6,8}}];*)
Parallelize@Do[mss=Table[RandomSample[extractPointCloud[(*Rasterize[(*"\:5f20_"*)"STOP"(*"|"*),ImageSize->sz]*)
		selected[[i]]],sz].MatrixExp[nilTrace@RandomReal[{-1,1},Dimensions[ms][[2]]{1,1}]]
	,{sz,20{3,4,6,8,10,12}}];
normalized=Identity[(r=lieNormalize[#,"power"->3(*,"Group"->"Rotation"*)];
	Graphics[{Red,Point[fourDirectionNormalize@Standardize[r[[2]]]]},Axes->True])&/@mss];
(*Export["/s/workspace/sld/figure/sparse_linear.pdf",*)g=Rasterize[TableForm@MapThread[Prepend,{{Graphics@{Blue,Point[#]}&/@mss,normalized
	,Style["#point="<>IntegerString[#],{17,Bold}]&/@(20{3,4,6,8,10,12})},Style[#,{17,Bold}]&/@{"Distorted","GOO_SL",""}}],ImageSize->2000];(*]*)
Export["/h/sampling_rate_"<>IntegerString[i]<>".pdf",g],{i,Length@selected}]


(*Limited search range*)
ms=extractPointCloud@Rasterize[(*"\:5f20_"*)"STOP"(*"|"*),ImageSize->200];
SeedRandom[1003];mss=Table[ms.RotationMatrix[RandomReal[1]],{5}];
Graphics[Point/@mss]
Clear[t];
Parallelize[Function[m,{r=NMinimize[{pnorm[m.RotationMatrix[-t],3],0<=t<=1},t,Method->"RandomSearch"];//AbsoluteTiming
	,r[[1]],Graphics[Point[m.RotationMatrix[-t/.r[[2]]]]]}]/@mss]


showImage@#&/@trainFeatures[[;;50]]
Parallelize[{Graphics[{Blue,Point@#}],Graphics[{Red,Point[lieNormalize[#,"power"->3][[2]]]}]}&/@(
	N@extractPointCloud@ImageResize[showImage@#,100{1,1},Resampling->"Nearest"]&/@trainFeatures[[;;50]])]


(*SeedRandom[1003];*)m=RandomReal[1,{10,2}];
Graphics@Line@#&/@{
	fourDirectionNormalize[m],fourDirectionNormalize[m.RotationMatrix[90Degree]]
	,fourDirectionNormalize[m.RotationMatrix[180Degree]],fourDirectionNormalize[m.RotationMatrix[270Degree]]}


Parallelize@Table[Run["cd /h/d/butterfly;/opt/local/bin/wget http://www.lems.brown.edu/~dmc/LinktoImages/Butterflies/Butterfly-a"<>IntegerString[i,10,3]<>".gif"],{i,100}]


n=2;SeedRandom[1003];MatrixForm/@{matrixExpSpecialOrthogonal[RandomReal[1,n{1,1}]],MatrixExp@strictUpperTriangle@RandomReal[1,n{1,1}]}
qrs=Table[matrixExpSpecialOrthogonal[RandomReal[1,n{1,1}]].MatrixExp@strictUpperTriangle@RandomReal[1,n{1,1}],{2}];
MatrixForm/@QRDecomposition[qrs[[1]].qrs[[2]]]


SeedRandom[1003];n=5;qs=matrixExpSpecialOrthogonal@RandomReal[1,n{1,1}];ms=qs.(upperTriangle@RandomReal[1,n{1,1}]).Transpose[qs];MatrixForm@qs
MatrixForm/@SchurDecomposition[ms]
Clear[a,u];as=Array[a,n{1,1}];us=Array[u,n{1,1}];
{r=FindMinimum[pnorm[strictLowerTriangle[matrixExpSpecialOrthogonal[as].ms.Transpose[matrixExpSpecialOrthogonal[as]]],1.5]
	,Flatten@{as,us}];//AbsoluteTiming,r[[1]]}
MatrixForm[matrixExpSpecialOrthogonal[as/.r[[2]]].ms.Transpose[
	matrixExpSpecialOrthogonal[as/.r[[2]]]]]


unitCircleGroup=Function[{a,b},If[Norm[Abs[a]^2-Abs[b]^2-1]>10^-6,Print["Abs[a]^2-Abs[b]^2=!=1"];Abort[],Function[z,(a z+b)/(Conjugate[b]z+Conjugate[a])]]];
\[Tau]0=0.323;
{mm,m}=Function[\[Tau],Table[With[{z=r(Cos[t]+Sin[t] I)},{Re@#,Im@#}&[unitCircleGroup[Sqrt[1+\[Tau]^2],\[Tau]][z]]],{r,0,1,0.3},{t,0,2Pi+0.1,0.3}]]/@{0,\[Tau]0};
transF=Function[{\[Tau],m},Map[unitCircleGroup[Sqrt[1+\[Tau]^2],\[Tau]][#[[1]]+#[[2]]I]&,m,{2}]];
Import@Export["t.png",Graphics@drawGrid@mm]
Import@Export["t2.png",Graphics@drawGrid@m]
Clear[\[Tau]];
Table[Print[r=NMinimize[pnorm2[transF[\[Tau],m],p],\[Tau](*,MaxIterations->1000*)];//AbsoluteTiming];
    {p,\[Tau]/.r[[2]],pnorm2[#,p]&/@{transF[\[Tau]/.r[[2]],m],transF[-\[Tau]0,m]}},{p,0.4,2.3,0.3}]


SeedRandom[1003];m=Standardize@RandomReal[1,{10,2}];svd=SingularValueDecomposition[m];svd1=svd[[1,;;,;;2]];
pnorm[#,3]&/@{svd1 GeometricMean[Diagonal[svd[[2]]]],m,svd[[1]],Last@lieNormalize@m}
pnorm[#,1]&/@{svd1 GeometricMean[Diagonal[svd[[2]]]],m,svd[[1]],Last@lieNormalize@m}
pnorm[#,1]&/@{svd1 GeometricMean[Diagonal[svd[[2]]]],m,svd[[1]],Last@lieNormalize[m,"power"->1]}


NMinimize[pnorm[(svd1 GeometricMean[Diagonal[svd[[2]]]]).DiagonalMatrix@{s,1/s}.RotationMatrix[\[Theta]],3],{s,\[Theta]}]
NMinimize[pnorm[(svd1 GeometricMean[Diagonal[svd[[2]]]]).DiagonalMatrix@{s,1/s}.RotationMatrix[\[Theta]],1],{s,\[Theta]}]


With[{m=svd1},{FindMinimum[pnorm[a m[[;;,1]]+m[[;;,2]],1],a],FindMinimum[pnorm[a m[[;;,1]]+m[[;;,2]],2],a]}]


(*SeedRandom[1003];*)
rm=RotationMatrix[a];(*rm={{(1-a^2)/(1+a^2),-2a/(1+a^2)},{2a/(1+a^2),(1-a^2)/(1+a^2)}};*)
m=RandomReal[1,{2,2}];
(*Total[m.rm]
D[Total@Power[m.rm,1],a]//Simplify
D[Total@Power[m.rm,1],{a,2}]//Simplify*)
Plot[pnorm2[m.rm,1],{a,-5,5}]
(*Total@Power[m.rm,3]//Simplify
D[Total@Power[m.rm,3],a]//Simplify
D[Total@Power[m.rm,3],{a,2}]//Simplify*)
Plot[pnorm2[m.rm,3],{a,-5,5}]


n=3;SeedRandom[1003];m=RandomReal[1,n{1,1}];(*m//TeXForm*)(*Export["/tmp/m.csv",m]*)
(*TeXForm/@SingularValueDecomposition@m
Clear[u,v];us=Array[u,3];vs=Array[v,3];
{r=NMinimize[pnorm[MatrixExp[skewOmega[-us]].m.MatrixExp[skewOmega[vs]],1],Flatten@{us,vs}];//AbsoluteTiming,r[[1]]}
uvs=MatrixExp@skewOmega@#&/@{us/.r[[2]],vs/.r[[2]]};TeXForm/@uvs
core=Transpose[uvs[[1]]].m.uvs[[2]];core//MatrixForm
core//ScientificForm//TeXForm*)

(*m=With[{P=MatrixExp@RandomReal[1,3{1,1}]},P.DiagonalMatrix@RandomReal[{-1,1},3].Inverse[P]];
evd=Eigensystem@m;{evd[[1]],evd[[2]]//MatrixForm}
Clear[a,f];
as=Array[a,{2,n,n}];
f[as_?(NumericQ@#[[1,1,1]]&)]:=With[{P=MatrixExp@nilTrace[as[[1]]+as[[2]]I]},pnorm[Inverse[P].m.P,1.8]];
(*as=Array[a,{n,n}];
f[as_?(NumericQ@#[[1,1]]&)]:=With[{P=MatrixExp@nilTrace[as]},pnorm[Inverse[P].m.P,1.5]];*)
{r=NMinimize[f[as],Flatten@{as},MaxIterations->1000];//AbsoluteTiming,r[[1]]}

MatrixForm@With[{P=MatrixExp@nilTrace[as[[1]]+as[[2]]I/.r[[2]]]},Inverse[P].m.P]
evd[[2]]//TeXForm
MatrixExp@nilTrace[as[[1]]+as[[2]]I/.r[[2]]]//TeXForm*)

(*MatrixForm@With[{P=MatrixExp@nilTrace[as/.r[[2]]]},Inverse[P].m.P]
evd[[2]]//TeXForm*)

(*qdr={Transpose[#[[1]]],DiagonalMatrix@Diagonal@#[[2]],Inverse[DiagonalMatrix@Diagonal@#[[2]]].#[[2]]}&@QRDecomposition@m;
MatrixForm/@qdr
TeXForm/@qdr
Clear[u,v];us=Array[u,3];vs=Array[v,3];
{r=NMinimize[pnorm[MatrixExp[skewOmega[-us]].m.Inverse@{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}},1],Flatten@{us,vs}];//AbsoluteTiming,r[[1]]}
uvs={MatrixExp[skewOmega[us/.r[[2]]]],{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}}/.r[[2]]};TeXForm/@uvs
core=Transpose[uvs[[1]]].m.Inverse[uvs[[2]]];core//MatrixForm
core//ScientificForm//TeXForm
MatrixForm/@{m,qdr[[1]].qdr[[2]].qdr[[3]],uvs[[1]].core.uvs[[2]]}
Print[Norm[uvs[[1]].core.uvs[[2]]-m,"Frobenius"]//ScientificForm//TeXForm]
Print[Norm[uvs[[2]]-qdr[[3]],"Frobenius"]//TeXForm]*)

(*{MatrixForm/@lduDecomposition@m,pnorm[lduDecomposition[m][[2]],1]}*)
(*Cholesky*)m=Transpose[m].m;chol=CholeskyDecomposition@m;Print[chol//TeXForm];Print[chol//MatrixForm];Print[TeXForm@ConjugateTranspose@chol];
(*TeXForm@ScientificForm@#&/@luNoPivotDecompomposition@m
Clear[u,v];us=Array[u,3];vs=Array[v,3];
(*{r=NMinimize[pnorm[Inverse@{{1,u[1],u[2]},{0,1,u[3]},{0,0,1}}.m.Inverse@{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}},1],Flatten@{us,vs}];//AbsoluteTiming,r[[1]]}
uvs={{{1,u[1],u[2]},{0,1,u[3]},{0,0,1}}/.r[[2]],{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}}/.r[[2]]};TeXForm/@uvs*)
{r=NMinimize[pnorm[strictLowerTriangle[Inverse[Transpose@{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}}].m],1],Flatten@{us,vs}
	,MaxIterations->10000,Method->"DifferentialEvolution"];//AbsoluteTiming,r[[1]]}
uvs={Transpose@{{1,u[1],u[2]},{0,1,u[3]},{0,0,1}}/.r[[2]],Transpose@{{1,v[1],v[2]},{0,1,v[3]},{0,0,1}}/.r[[2]]};
Print@TeXForm@ScientificForm@uvs[[2]]
core=(*Inverse[uvs[[1]]].*)Inverse[uvs[[2]]].m;core//MatrixForm
core//ScientificForm//TeXForm
Print[uvs[[2]].DiagonalMatrix@Power[Diagonal@core,1/2]//TeXForm]*)

(*m=SvdApprox[m,2]+10^-12 IdentityMatrix[Length@m]
Abs[Power[Det[m],1/n]]n
Abs[Power[Det[m],1/n]]IdentityMatrix[3]//TeXForm
Clear[u,v];us=Array[u,n{1,1}];vs=Array[v,n{1,1}];
{r=NMinimize[pnorm[MatrixExp[-nilTrace@us].m.MatrixExp[-nilTrace@vs],1],Flatten@{us,vs},Method->"NelderMead"];//AbsoluteTiming,r[[1]]}
uvs={MatrixExp[nilTrace[us/.r[[2]]]],MatrixExp[nilTrace[vs/.r[[2]]]]};TeXForm/@uvs
core=Inverse[uvs[[1]]].m.Inverse[uvs[[2]]];core//MatrixForm
core//ScientificForm//TeXForm*)

(*schur=SchurDecomposition[m,RealBlockDiagonalForm->False]
MatrixForm/@schur
Print[Re[schur[[1]]]//ScientificForm//TeXForm]
Print[Im[schur[[1]]]//ScientificForm//TeXForm]
Print[Re@schur[[2]]//ScientificForm//TeXForm]
Print[Im@schur[[2]]//ScientificForm//TeXForm]
Clear[u,v];us=Array[u,3];vs=Array[v,3];
{r=NMinimize[pnorm[strictLowerTriangle[Transpose[MatrixExp@skewOmega[us+I vs]].m.MatrixExp@skewOmega[us+I vs]],1],Flatten@{us,vs}
	(*,MaxIterations->10000,Method->"DifferentialEvolution"*)];//AbsoluteTiming,r[[1]]}
Print[MatrixExp@skewOmega[(us+I vs)/.r[[2]]]//Re//TeXForm]
Print[MatrixExp@skewOmega[(us+I vs)/.r[[2]]]//Im//TeXForm]
core=Transpose[MatrixExp@skewOmega@((us+I vs)/.r[[2]])].m.MatrixExp@skewOmega@((us+I vs)/.r[[2]]);core//MatrixForm
Print[core//Re//ScientificForm//TeXForm]
Print[core//Im//ScientificForm//TeXForm]*)


(*polygonArea=Function[polygon,0.5Abs@Total[Det@#&/@Partition[polygon,2,1,{1,1}]]];*)
ParallelNeeds["ComputationalGeometry`"];
polygonArea=((*Print[{ConvexHullArea@#,#}];*)ComputationalGeometry`ConvexHullArea@#)&;
(*homog=matrixExpSpecialOrthogonal[RandomReal[1,3{1,1}]]+outer[RandomReal[1,3],RandomReal[1,3]];
SeedRandom[1003];
polygon=#/Sqrt@polygonArea[#]&[RandomReal[1,{5,2}]];
polygon2=#/Sqrt@polygonArea[#]&[homographyTransform[#,homog]&/@polygon];
polygonArea/@{polygon,polygon2}
Graphics@{Blue,Line@polygon,Red,Line@polygon2}*)
Parallelize@
Table[polygon=#/Sqrt@polygonArea[#]&[RandomSample[mss[[i]],100]];
Clear[a,f,g,t,n,b];as=Array[a,3];ts=Array[t,3];ns=Array[n,3];bs=Array[b,2{1,1}];
g=Function[{as,ts,ns,bs},With[{homog=MatrixExp@skewOmega@as(*+outer[ts,ns]*)},(#/Sqrt@polygonArea[#]&[homographyTransform[#,homog]&/@polygon])(*.MatrixExp@nilTrace@bs*)]];
f[as_?(NumericQ@#[[1]]&),ts_,ns_,bs_]:=pnorm[g[as,ts,ns,bs],5];
{f[0as,0ts,0ns,0bs],{r=NMinimize[f[as,ts,ns,bs],Flatten@{as,ts,ns,bs}];//AbsoluteTiming,r[[1]]},
	Graphics@{Blue,Point@polygon,Red,Point@g[as/.r[[2]],ts/.r[[2]],ns/.r[[2]],bs/.r[[2]]]}},{i,Length@mss}]


normalizeByRotationalHomography=Function[{pts,fun},Module[{a,as,h,f,r},Clear[a];as=Array[a,3{1,1}];
	h[as_]:=With[{homog=matrixExpSpecialOrthogonal@as},transformByHomography[#,homog]&/@pts];
	f[as_?(NumberQ@#[[1,1]]&)]:=fun[h[as]];
	r=NMinimize[f[as],Flatten@as];h[as/.r[[2]]]]];
SeedRandom[1005];
Transpose@Parallelize@Table[m=SparseArray[1-(Rasterize[i//Magnify[#,3]&]//Binarize//ImageData)];h=Re@MatrixExp[0.1MatrixLog@randomSpecialOrthogonalMatrix[3]];
pts0=Standardize[m["NonzeroPositions"],Mean,1&];pts=transformByHomography[#,h]&/@pts0;
(*Graphics[{Blue,Point@pts,Red,Point@Standardize@PrincipalComponents@pts,Green,Point@normalizeByRotationalHomography[pts,pnorm[#,3]&]}]*)
{Graphics[{Blue,Point@pts}],Graphics[{Red,Point@Standardize@PrincipalComponents@pts}]
	,Graphics[{Green,Point@normalizeByRotationalHomography[pts,pnorm[#,3]&]}]},{5},{i,5}]


SeedRandom[1003];n=7;(*starts to fail when n>=8*)
pointSets=Table[rot=Re@MatrixExp[0.1 MatrixLog@randomSpecialOrthogonalMatrix[3]];
	transformByHomography[#,rot]&/@(Flatten[Table[{i-(n+1)/2,j-(n+1)/2},{i,n},{j,n}],1]),{5}];
Import@Export["t.png",#]&@GraphicsGrid[Transpose@Parallelize[
	Function[pts,{Graphics@{Blue,Point@pts},Graphics@{Red,Point@Standardize@PrincipalComponents[pts]}
		,Graphics@{Green,Point@normalizeByRotationalHomography[pts,pnorm2[#,3]&]}}]/@pointSets],Frame->All]


SeedRandom[1003];
mnorm=Function[m,Module[{r,a,f,ass},ass=Array[a,{3,3}];
	Last@{r=NMinimize[pnorm[foldXUs[m,MatrixExp@skewOmega@#&/@ass,{}],1],Flatten@ass];//AbsoluteTiming,r[[1]]}
]];
Parallelize@Table[ms=RandomReal[1,{2,3,3,3}];Plus@@(mnorm/@ms)-mnorm[Plus@@ms],{100}]


Clear[a,b];as=Array[a,3];bs=Array[b,3];SeedRandom[1003];m=RandomReal[1,3{1,1}];
{r=NMinimize[pnorm[MatrixExp@skewOmega@as.m.MatrixExp@skewOmega@bs,3],Flatten@{as,bs}];//AbsoluteTiming,r[[1]]}
MatrixExp@skewOmega[as/.r[[2]]].m.MatrixExp@skewOmega[bs/.r[[2]]]//MatrixForm
MatrixForm/@SingularValueDecomposition@m


(*{r=NMinimize[-Tr@Abs[Transpose@{{1,a[1],a[2]},{0,1,a[3]},{0,0,1}}.m.{{1,b[1],b[2]},{0,1,b[3]},{0,0,1}}]
	,Flatten@{as,bs}];//AbsoluteTiming,r[[1]]}
Transpose@{{1,a[1],a[2]},{0,1,a[3]},{0,0,1}}.m.{{1,b[1],b[2]},{0,1,b[3]},{0,0,1}}/.r[[2]]//MatrixForm*)


SeedRandom[1003];Clear[a,f];m=RandomReal[1,3{1,1,1}];ass=Array[a,3{1,1}];
{r=NMinimize[pnorm[foldXUs[m,MatrixExp[skewOmega[#]]&/@ass,{}],1],Flatten@ass];//AbsoluteTiming,r[[1]]}
f[as_?(NumericQ@#[[1]]&)]:=Total[schattenNorm[#,1]&/@(m.MatrixExp@skewOmega@as)];
{r=NMinimize[f[ass[[1]]],Flatten@ass];//AbsoluteTiming,r[[1]]}
schattenNorm[Join@@m,1]


n=300;SeedRandom[1003];m=RandomReal[1,{3,n,n}];
f[as_?(NumericQ@#[[1]]&)]:=Total[schattenNorm[#,1]&/@(MatrixExp[skewOmega@as].m)];
{r=NMinimize[f[ass[[1]]],Flatten@ass];//AbsoluteTiming,r[[1]]}
schattenNorm[Join@@m,1]


?RotateLeft


m=ImageData@ColorConvert[Import["/s/t7.jpg",ImageSize->{200,200}],"Gray"];rk=30;d=15;
#[[1,;;,;;rk]].#[[2,;;rk,;;rk]].Transpose@#[[3,;;,;;rk]]&@SingularValueDecomposition[m]//Image//ImageAdjust
(#[[1,;;,;;rk]].#[[2,;;rk,;;rk]].Transpose@#[[3,;;,;;rk]]&@SingularValueDecomposition[
	Join@@(With[{n=Length@#},Table[RotateLeft[#,i],{i,-d,d}]]&/@m)])[[1+d;;;;1+2d]]//Image//ImageAdjust


	nzRatio=0.3;noise=0;scale=481;img=Import["/h/t2.jpg",ImageSize->{scale}];
	dim=Round[N[scale]#/Max@#&@Reverse@ImageDimensions@img];
	SeedRandom[1003];{gray,rgb,masks}=prepareDataRpcaColor[ColorSeparate[ImageResize[img,Reverse@dim]],dim,1-nzRatio];
    noiseF=If[noise==0,0,RandomVariate@NormalDistribution[0,noise]]&;
(*\[CapitalOmega]=ImageData@Erosion[Image@randomSparseTensor[dim{1,1},0.9],2];masks={\[CapitalOmega],\[CapitalOmega],\[CapitalOmega]};*)
	noisyGray=Map[#+noiseF[]&,gray,{-1}];
	noisyRgb\[CapitalOmega]=masks Map[#+noiseF[]&,rgb,{-1}];
	(*Print@{pnorm[rgb-{gray,gray,gray},2]/pnorm[rgb,2],gray//Image,noisyGray//Image,rgb//showTensor3,noisyRgb\[CapitalOmega]//showTensor3};*)
	Print[{Dimensions/@{gray,rgb,masks},nzRatio,noise}];


mask=unfoldTensor[masks,2];
Table[{meth,foldToTensor[Rpca2UnitaryWithInit[unfoldTensor[noisyRgb\[CapitalOmega],2],mask,0mask,0mask,100,meth][[1]],Dimensions@masks,2]//showTensor3}
	,{meth,{"Fourier","Gray","FourierLaplace","Hadamard"}}]


pts0={#[[2]],-#[[1]]}&/@Position[ImageData@ColorNegate@Binarize@Rasterize[Style["STOP",FontFamily->"Times",FontSize->40]],1,{2}];
Parallelize@Table[rotM=RotationMatrix[RandomReal[{-1,1}Pi]];pts=pts0.rotM;
r=lieNormalize[pts,(*"Group"->"Rotation",*)"Method"->"Local","power"->1];
{Graphics@Point@pts,Graphics@Point@r[[2]]},{20}]



