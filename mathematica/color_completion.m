(* ::Package:: *)

<<"~/gdrive/mac_home/t3.m"
evalColorResult=Function[{L,S,rgb,masks},With[{scale=1/pnorm[rgb,2]},
	{"rgb"->pnorm[rgb-L,2]scale,"gray"->pnorm[Mean[L]-Mean@rgb,2]scale,"LS"->showTensor3/@{L,S}
		,"unknown"->pnorm[(rgb-L)(1-masks),2]/pnorm[rgb (1-masks),2]
	(*,"residue"->ImageAdjust@ImageDifference[rgb//showTensor3,L//showTensor3]*)}]];
RpcaTensorWithInit=Function[{D,\[CapitalOmega],initL,initS,iter},RpcaTensorUnfoldingsWithInit[D,\[CapitalOmega],initL,initS,iter,{2,3}]];
RpcaTensorUnfoldingsWithInit=Function[{D,\[CapitalOmega],initL,initS,iter,unfoldings},
	(*min \sum_i||L_i||_* + \[Lambda] order||\[CapitalOmega] S||_ 1 + \[Mu]1/2 order||D-X-S||_F^2+\[Mu]2/2\sum_i||L_i-X||_F^2 *)
	Module[{dim,norm2,\[Lambda],\[Eta]=0.,OL,Y1,Y2,L,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs},
	(*S,D,\[CapitalOmega],X,Y1 are {k,m,n}, L,Y2 are {order,k,m,n}. D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	(*unfoldings=Select[Range@Length@dim,dim[[#]]>=GeometricMean@dim&];*)L=Table[initL,{Length@unfoldings}];
	Y1=0D(*Sign[D]/Max[norm2,Mean[Norm[#,Infinity]&/@D]/\[Lambda]]*);Y2=Table[Y1,{Length@unfoldings}];\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	Ikn=sparseIdentityMatrix[k n];
	Do[
    X=foldToTensor[Transpose@LinearSolve[(\[Mu]1+\[Mu]2) Ikn,Transpose[unfoldTensor[Mean[-Y2+\[Mu]2 L]+\[Mu]1(D-S)+Y1,2]]],dim,2];
    Zl=(#+\[Mu]2 X)/\[Mu]2&/@Y2;
	OL=L;
	L=Table[foldToTensor[sShrinkage[1/\[Mu]2,unfoldTensor[Zl[[i]],unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
	Zs=Y1/\[Mu]1+D-X;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/(0.000001+pnorm[L,2])<0.0005,Break[]];
    Y1+=\[Mu]1(D-X-S);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,30]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{X,S}]];


scale=512;nzRatio=0.3;noise=0;
dim=Round[N[scale]#/Max@#&@Reverse@ImageDimensions@img];
	SeedRandom[1003];{gray,rgb,masks}=prepareDataRpcaColor[ColorSeparate[ImageResize[img,Reverse@dim]],dim,1-nzRatio];
	masks=SparseArray[N@Normal@masks];
    noiseF=If[noise==0,0,RandomVariate@NormalDistribution[0,noise]]&;
(*\[CapitalOmega]=ImageData@Erosion[Image@randomSparseTensor[dim{1,1},0.9],2];masks={\[CapitalOmega],\[CapitalOmega],\[CapitalOmega]};*)
	noisyGray=Map[#+noiseF[]&,gray,{-1}];
	noisyRgb\[CapitalOmega]=masks Map[#+noiseF[]&,rgb,{-1}];


RpcaTripleMatricesWithInit=Function[{D,\[CapitalOmega],initL,initS,iter}(*Assume D is made of three matrices*)
		,Module[{norm2,\[Lambda],\[Eta]=10.,Y,L=initL,S=initS,\[Mu],\[Rho],m,n,Zl,Zs,OL,a,as,us,r,f,as0={0.,0,0}},as=Array[a,3];
	(*: \inf_{\U\in\UG, \L+\S=\D} \sum_i\|(\U\times_1 \L)_i\|_* + \[Lambda]||\Omega\circ S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	(*L,S,D are 3-by-m-by-n. \[CapitalOmega] is nonzeros*)
	{m,n}=Rest@Dimensions[D];\[Lambda]=Min[0.5,100./GeometricMean@{m,n}];norm2=SingularValueDecomposition[unfoldTensor[D,2],1][[2,1,1]];
	Y=0D;(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];*)\[Mu]=12.5/norm2;\[Rho]=1.005;
	f[as_?(NumericQ@#[[1]]&)]:=Total[schattenNorm[#,1]&/@(MatrixExp[skewOmega[as]].L)];
	Do[r=FindMinimum[f[as],variableWithInitial[as,as0]];as0=as/.r[[2]];
	us=N@MatrixExp[skewOmega[as0]];(*Print[us];*)
	Zl=Y/\[Mu]+D-S;OL=L;
	L=Transpose[us].(sShrinkage[1/\[Mu],#]&/@(us.Zl));
	Zs=Y/\[Mu]+D-L;
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
	If[pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];If[Mod[j,30]==0,printTemporary@ImageAdjust@Image@Re[L]];(*(scores=Append[scores,#];Print[#])&@evalPredicted[L,testM2];*)
	,{j,iter}];
	{L,S}]];
i=1;{L[i],S[i]}=RpcaTripleMatricesWithInit[noisyRgb\[CapitalOmega],masks,0masks,0masks,100];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]


mask=unfoldTensor[masks,3];
i=1;{L[i],S[i]}=foldToTensor[#,Dimensions@masks,3]&/@Rpca2WithInit[unfoldTensor[noisyRgb\[CapitalOmega],3],mask,0mask,0mask,100];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]
i=1;{L[i],S[i]}=foldToTensor[#,Dimensions@masks,3]&/@RpcaFourierWithInit[unfoldTensor[noisyRgb\[CapitalOmega],3],mask,0mask,0mask,100];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]
i=1;{L[i],S[i]}=foldToTensor[#,Dimensions@masks,3]&/@RpcaMeanFilterWithInit[unfoldTensor[noisyRgb\[CapitalOmega],3],mask,0mask,0mask,100];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]
i=1;{L[i],S[i]}=foldToTensor[#,Dimensions@masks,3]&/@RpcaFourierLaplaceWithInit[unfoldTensor[noisyRgb\[CapitalOmega],3],mask,0mask,0mask,100];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]
i=2;{L[i],S[i]}=RpcaTensorUnfoldingsWithInit[noisyRgb\[CapitalOmega],masks,0masks,0masks,100,{2,3}];//AbsoluteTiming
evalColorResult[L[i],S[i],rgb,masks]



