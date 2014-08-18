(* ::Package:: *)

BeginPackage["Sensor`"]
CenteringMatrix=Table[If[i==j,1-1/#,-1/#],{i,#},{j,#}]&;
CenterizeKernelMatrix=Function[K,With[{ones=Array[1./Length@K&,{Length@K,Length@K}]},K-ones.K-K.ones+ones.K.ones]];
DistanceMatrixToGramMatrix=-0.5CenteringMatrix[Length@#].#.CenteringMatrix[Length@#]&;
GramMatrixToVectors=Function[{XtX,k},Transpose[Sqrt[DiagonalMatrix[Eigenvalues[XtX,k]]].Eigenvectors[XtX,k]]];
ClassicMultidimensionalScaling=Function[{d,k},GramMatrixToVectors[DistanceMatrixToGramMatrix@d,k]];
GramMatrixToDistanceMatrix=Function[{g,k},Outer[Total[(#-#2)^2]&,#,#,1]&[GramMatrixToVectors[g,k]]];
FindUnimodularTransformationMatrix=Function[{src,target},(*src, target are k-by-n, result R is used as R.src-target*)
	#[[3]].Transpose[#[[1]]]&@SingularValueDecomposition[Transpose@Standardize[Transpose@src,Mean,1&].Standardize[Transpose@target,Mean,1&]]];
AtMost=If[Length@#<#2,#,#[[;;#2]]]&;(*AtMost[#,3]&/@{Range@2,Range@10}*)
FlattenAssocList=Function[{alist,n},Normal@SparseArray[Rule@@@alist,{n}]];(*FlattenAssocList[{{2,3},{4,5}},10]*)
GramSchmidt=Function[{v,u},v-Projection[v,u]];
ToZOrder=Function[{n,n2},Module[{l,l2,m},
	{l,l2}=IntegerDigits[#,2]&/@{n,n2};m=Max[Length/@{l,l2}];
	FromDigits[Flatten[Thread@{PadLeft[l,m],PadLeft[l2,m]}],2]]];
(*With[{n=8},Rotate[Graphics[Line/@Partition[#[[;;2]]&/@SortBy[Join@@Table[{i,j,ToZOrder[i-1,j-1]},{i,n},{j,n}],Last],2,1]],-90Degree]]*)
HilbertCurveN2XY=Function[{n,x0,y0},Module[{rx,ry,s=Floor[n/2],d=0,x=x0,y=y0},
	While[s>=1,
		rx=Boole[BitAnd[x,s]>=1];ry=Boole[BitAnd[y,s]>=1];d+=s s BitXor[3rx,ry];
		(*Print@{rx,ry,s,d};*)
		If[ry==0,If[rx==1,x=s-1-x;y=s-1-y];{x,y}={y,x}];
		s=Floor[s/2]];
	d]];
(*With[{n=4},Graphics[Line/@Partition[#[[;;2]]&/@SortBy[Join@@Table[{i-1,j-1,HilbertCurveN2XY[n,i-1,j-1]},{i,n},{j,n}],Last],2,1]]]*)
PcaVectors=Function[m,SingularValueDecomposition[Standardize[N@m,Mean,1&]][[3]]];
(*pts=RandomVariate[MultinormalDistribution[{3,4},{{10,3},{3,8}}],1000];Graphics[{Point/@pts,Red}~Join~(Line@{Mean@pts+10#,Mean@pts-10#}&/@PcaVectors[pts][[;;1]]),Axes->True]*)
Cross2=Dot[{0,0,1},Cross[Append[#,1],Append[#2,1]]]&;(*Cross2@@@{{{1,0},{1,0}},{{1,0},{0,0}}}*)(*Cross2@@@{{{1,0},{0,1}},{{1,0},{1,1}}}*)
(*GaussianWindow=Function[{n, sigma},Map[Exp[-(#-(n+1)/2)^2/sigma^2/2]&,Range@n]];*)
PowerSpectrum=Function[{seq,alpha},
	Module[{L=Length[seq],alphaN=alpha Length[seq]},
	Map[Abs[#]^2/alphaN&,Fourier[Join[GaussianWindow[L, (L-1)/4]seq,
		Table[0,{alphaN-L}]]][[;;Floor[((alphaN-1)/2)+1]]]]]];
MainFrequency=Function[{seq,alpha},
	Module[{spectrum=PowerSpectrum[seq,alpha]},
		N[Ordering[spectrum][[-1]]/alpha]
	]];
(*FindMainComponent=Function[{seq,alpha},
	Module[{L=Length[seq],alphaN=alpha Length[seq],spectrum,k},
	spectrum=Fourier[Join[GaussianWindow[L, (L-1)/4]seq, Table[0,{alphaN-L}]]];
	k=Ordering[spectrum][[-1]];
	InverseFourier[Table[If[i==k||i==alphaN+1-k,spectrum[[k]],0],{i,1,alphaN}]][[1;;L]]
	]];*)
RemoveComponentNear=Function[{seq,alpha,bounds},
	Module[{L=Length[seq],alphaN=alpha Length[seq],spectrum,lb,k},
	spectrum=Map[Abs,Fourier[Join[GaussianWindow[L, (L-1)/4]seq, Table[0,{alphaN-L}]]]];
	(*k=bounds[[1]]+Ordering[spectrum[[bounds[[1]];;bounds[[2]]]]][[-1]]-1;*)
	k=Ordering[spectrum][[-1]];
	Print[k];
	Print[spectrum[[k-10;;k+10]]];
	spectrum=Fourier[Join[seq, Table[0,{alphaN-L}]]];
	InverseFourier[Map[If[#==k||#==alphaN+1-k,Print[#];0,spectrum[[#]]]&,Range@alphaN]][[1;;L]]
	]];
Resample=Function[{l,dt,window},
	With[{f=Function[x,If[x<l[[-1]][[1]],
		Interpolation[l,InterpolationOrder->1][x],l[[-1]][[2]]]]},
	Map[f[l[[1]][[1]]+(#-1)*dt]&,Range[window/dt]]
	]];
AverageByTimestamp = Function[l, Map[Mean,SplitBy[l,#[[4]]&]]];
PickOut = Function[{txyzt,typ},
	Transpose[AverageByTimestamp[Map[{#[[2]],#[[3]],N[#[[4]]],#[[5]],#[[7]],#[[8]]}&,
		Select[txyzt,#[[1]]==typ&]]]]];
LoadFile = Function[{fname,n},Take[ReadList[fname,
	{Word,Number,Number,Number,Number,Number,Word,Word}],n]];
LoadData = Function[{fname, n, typ}, PickOut[LoadFile[fname, n], typ]];
ProcData=Function[{xyzt},
	Module[{m=Median@Differences[xyzt[[4]]]},
	Join[xyzt[[1;;3]],{Accumulate@Prepend[Map[If[#>10m,m,#]&,Differences@xyzt[[4]]],First[xyzt[[4]]]]}]
	]];
(*AngularVelocityToQuaternion=Compile[{{w,_Real,1}},
	Module[{phi,wx,wy,wz},
		{wx,wy,wz}=w;
		phi = Norm[{wx,wy,wz}];
		If[phi==0,{1,0,0,0},
		{Cos[phi/2],Sin[phi/2]wx/phi,Sin[phi/2]wy/phi,Sin[phi/2]wz/phi}
	]]];*)
(*IntegrateAngularVelocity2=Compile[{{ts, _Integer,1},{wxyzs,_Real,2}},
	FoldList[QuaternionMul[#2,#1]&,{1,0,0,0},MapThread[AngularVelocityToQuaternion[#1/10^9 #2]&,{Differences[ts],Most@wxyzs}]]];*)
QuaternionMul=Compile[{{q,_Real,1},{q2,_Real,1}},
	{q[[1]] q2[[1]]-q[[2]] q2[[2]]-q[[3]] q2[[3]]-q[[4]] q2[[4]],q[[2]] q2[[1]]+q[[1]] q2[[2]]-q[[4]] q2[[3]]+q[[3]] q2[[4]],
	q[[3]] q2[[1]]+q[[4]] q2[[2]]+q[[1]] q2[[3]]-q[[2]] q2[[4]],q[[4]] q2[[1]]-q[[3]] q2[[2]]+q[[2]] q2[[3]]+q[[1]] q2[[4]]}];
RotateByQuaternion=Compile[{{v,_Real,1},{q,_Real,1}},
	(*Quat2Matrix[#].{0,1,0} == RotateByQuaternion[{0,1,0},#]*)
	{q[[3]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])-q[[4]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[1]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[2]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	-q[[2]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[1]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[4]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[3]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	q[[1]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[2]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])-q[[3]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[4]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]])}
	/ (Norm[q]^2)
	];
QuaternionConjugate=Compile[{{q,_Real,1}},{q[[1]],-q[[2]],-q[[3]],-q[[4]]}];
(*ListPlot@Transpose@Map[Quat2Matrix[#].{0,1,0}-RotateByQuaternion[{0,1,0},#]&,Table[Table[RandomVariate[NormalDistribution[]],{i,4}],{j,1000}]]*)
QuaternionDerivative=Compile[{{q,_Real,1},{w,_Real,1}},
	{1/2 (-q[[2]] w[[1]]-q[[3]] w[[2]]-q[[4]] w[[3]]),1/2 (q[[1]] w[[1]]-q[[4]] w[[2]]+q[[3]] w[[3]]),1/2 (q[[4]] w[[1]]+q[[1]] w[[2]]-q[[2]] w[[3]]),1/2 (-q[[3]] w[[1]]+q[[2]] w[[2]]+q[[1]] w[[3]])}];
SkewOmega=Compile[{{w,_Real,1}},
	With[{p=w[[1]],q=w[[2]],r=w[[3]]},
		{{0,-r,q},{r,0,-p},{-q,p,0}}
	]];
SoraToMatrix=Compile[{{w,_Real,1}},
	With[{c=Cos@Norm[w],s=Sin@Norm[w],v=Normalize[w]},
	{{c+v[[1]]v[[1]](1-c),v[[1]]v[[2]](1-c)-v[[3]]s,v[[1]]v[[3]](1-c)+v[[2]]s},
	{v[[2]]v[[1]](1-c)+v[[3]]s,c+v[[2]]v[[2]](1-c),v[[2]]v[[3]](1-c)-v[[1]]s},
	{v[[3]]v[[1]](1-c)-v[[2]]s,v[[3]]v[[2]](1-c)+v[[1]]s,c+v[[3]]v[[3]](1-c)}}
	]];
(*AngularVelocityToMatrix=Compile[{{w,_Real,1},{dt,_Real}},
	With[{m=IdentityMatrix[3]-dt SkewOmega[w]},
	m/(Norm[m,2]^(1/Length[m]))
	]];*)
IntegrateAngularVelocityVerlet=Compile[{{ts, _Integer,1},{wxyzs,_Real,2}},
	MapThread[{#2,#1}&,
		{FoldList[Normalize[#1+QuaternionDerivative[#1,#2[[1]]]#2[[2]]]&,{1,0,0,0},MapThread[{#1,#2}&,{Most[wxyzs],Differences[ts]/10^9}]],ts}]];
IntegrateAngularVelocity=Compile[{{ts, _Integer,1},{wxyzs,_Real,2}},
	MapThread[{#2,#1}&,
		{FoldList[QuaternionMul,{1,0,0,0},MapThread[Normalize[Matrix2Quat@SoraToMatrix[#1 #2]]&,{Most[wxyzs],Differences[ts]/10^9}]],ts}]];
IntegrateAngularVelocityWithBias=Compile[{{ts, _Integer,1},{wxyzs,_Real,2},{bias,_Real,2}},
	Module[{biasF=Interpolation[bias,InterpolationOrder->1]},
		MapThread[{#2,#1}&,
		{FoldList[Normalize[#1+QuaternionDerivative[#1,#2[[1]]-biasF[#2[[3]]]]#2[[2]]]&,{1,0,0,0},
			MapThread[{#1,#2,#3}&,{Most[wxyzs],Differences[ts]/10^9,Most[ts]}]],ts}]
		]];
LearnBias=Function[{ts,wxyzs,xyzs,kB},
	With[{xs=Map[Normalize,xyzs],kX=Sqrt[2kB]},
	FoldList[
		With[{w=-#1[[2]]-kX Cross[#1[[1]],#2[[1]]],dt=#2[[3]]/10^9},
			{Normalize[#1[[1]]-Cross[#2[[2]]+w,#1[[1]]] dt],#1[[2]]+kB Cross[#1[[1]],#2[[1]]] dt}]&,
		{First[xs],{0,0,0}},Transpose[{Most[xs],Most[wxyzs],Differences[ts]}]]
	]];
(*xbqs=LearnBias[ts,wxyzs,xyzs,0.01];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbqs]]
ListPlot[Transpose@Map[#[[1]]&,xbqs],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,xbqs],PlotRange->All]*)
LearnBias2=Function[{ts,wxyzs,mags,xyzs,kB},
	With[{xs=Map[Normalize,xyzs],kX=Sqrt[2kB],kZ=Sqrt[2kB]},
	FoldList[
		With[{wX=-#1[[2]]-kX Cross[#1[[1]],#2[[1]]],wZ=-#1[[2]]-kZ Cross[#1[[3]],#2[[4]]],dt=#2[[3]]/10^9},
			{Normalize[#1[[1]]-Cross[#2[[2]]+wX,#1[[1]]] dt],#1[[2]]+kB(Cross[#1[[1]],#2[[1]]]+Cross[#1[[3]],#2[[4]]])dt,
				Normalize[#1[[3]]-Cross[#2[[2]]+wZ,#1[[3]]] dt]}]&,
		{First[xs],{0,0,0},Normalize[First[mags]-Projection[First[mags],First[xs]]]},
			Transpose[{Most[xs],Most[wxyzs],Differences[ts],Most[MapThread[Normalize[#1-Projection[#1,#2]]&,{mags,xs}]]}]]
	]];
ComplementByQuaternions=Function[{xyzt,qf},
	Module[{l=Transpose[xyzt[[1;;4]]]},
	Map[{#[[4]],RotateByQuaternion[#[[1;;3]],qf[#[[4]]]]}&,l]
	]];
GetGravityDir=Function[{xyzt,wxyzt,threshold},
	Module[{qf = Interpolation@IntegrateAngularVelocity[wxyzt],cxyzt,wxyztf},
	cxyzt=ComplementByQuaternions[xyzt,qf];
	wxyztf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[wxyzt]],InterpolationOrder->1];
	Mean[Flatten[Map[If[Norm[wxyztf[#[[1]]]]>threshold,{Normalize[#[[2]]]},{}]&,cxyzt],1]]
	]];
RemoveGravity=Function[{xyzt,wxyzt},
	Module[{l=Transpose[xyzt[[1;;4]]]},
		With[{qf = Interpolation@IntegrateAngularVelocity[wxyzt]},
		Map[#[[1;;3]]-RotateByQuaternion[First[l][[1;;3]],Conjugate[qf[#[[4]]]]]&,l]
	]]
	];
FindMoving=Function[{xyzt, threshold, window},
	Map[{Mean[#][[2]],
		If[Variance[Map[First,#]]>threshold,1,0]}&,
			Partition[Map[{Norm[#[[1;;3]]],#[[4]]}&,Transpose[xyzt[[1;;4]]]],window]]];
IntegrateQuaternion=Function[{isMovingF,qs},
	Module[{ts,vs},
		ts=Map[First,qs];vs=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,qs];
	Transpose[{Rest[ts],Accumulate[Most[Map[isMovingF[#]&,ts] vs] Differences[ts]/10^9]}]
	]];
GetXYZTrace=Function[{fname, movingThreshold, movingWindow},
	Module[{xyzt=LoadData[fname,All,"Acc"],wxyzt=LoadData[fname,All,"Gyo"],
			qs,isMovingF},
		qs=IntegrateAngularVelocity[wxyzt];
		isMovingF=Interpolation@FindMoving[
			xyzt, movingThreshold, movingWindow];
		IntegrateQuaternion[isMovingF,qs]
	]];
ProjectToHorizontal=Function[{txyzs,gravityDir},
	Module[{mat = RotationMatrix[{gravityDir,{0,0,1}}]},
	Map[{#[[1]],(mat.#[[2]])[[1;;2]]}&,txyzs]
	]];
GetSwinging = Function[l,MapThread[VectorAngle,{Most[l],Rest[l]}]];
PrintXYTraceTo=Function[{ifname, ofname, opicture},
		Module[{txyzs,gravityDir,xyzt,wxyzt,pxyzs},
		xyzt=LoadData[ifname,All,"Acc"];
		wxyzt=LoadData[ifname,All,"Gyo"];
		txyzs=GetXYZTrace[ifname, 0.4, 15];
		gravityDir=GetGravityDir[xyzt,wxyzt,0.3];
		pxyzs=ProjectToHorizontal[txyzs,gravityDir];
		Export[ofname,Map[{Round[#[[1]]], #[[2]][[1]], #[[2]][[2]]}&,
			pxyzs]];
		Export[opicture,ListPlot[
			Map[{#[[2]][[1]], #[[2]][[2]]}&,pxyzs],Joined->True]];
	]];
SimplifyLines = Function[{xys, deltaTheta, deltaThreshold},
   Accumulate[
    Map[Total, 
     SplitBy[Select[Differences[xys], Norm[#] > deltaThreshold &], 
      Floor[ArcTan[#[[1]], #[[2]]]/deltaTheta] &]]]
   ];
LowPassFilter=Function[{l,w,init},FoldList[(1-1/w)#1+1/w#2&,init,l]];
HighPassFilter=Function[{l,w},l-GaussianFilter[l,w]];
BandPassFilter=Function[{l,w1,w2},
	GaussianFilter[l-GaussianFilter[l,w2],w1]
	];
VectorFilter=Function[{f,l,w},Transpose@Map[f[#,w]&,Transpose@l]];
(*l1=Table[Sin[2Pi x],{x,-3,3,0.01}];
l2=Table[Sin[5Pi x],{x,-3,3,0.01}];
l=l1+l2;
ListPlot@{l1,l2,GaussianFilter[l,18],HighPassFilter[l,18]}*)
UniqueRotationMatrix=(*Compile[{{u1,_Real,1},{u2,_Real,1},{v1,_Real,1},{v2,_Real,1}},*)
	Function[{u1,u2,v1,v2},
		Transpose[PseudoInverse[Map[Normalize,{u1,u2,Cross[u1,u2]}]].
			Map[Normalize,{v1,v2,Cross[v1,v2]}]]
	];
(*EstimateGyroBiasFromAccel=Function[{xyzt,wxyztf},
	Module[{gs=Transpose[xyzt[[1;;3]]],ts=xyzt[[4]]},
		With[{gdirs=MapThread[Normalize[Cross[#1,#2]]&,{Most[gs],Rest[gs]}]},
		PseudoInverse[gdirs].
			(MapThread[Dot,{gdirs,Map[wxyztf,Most[ts]]}]-MapThread[Norm[#1]/#2&,{Differences[gs],Differences[ts]}])
	]]];*)
Matrix2Quat= Compile[{{M,_Real,2}},
	Module[{s,x,y,z},
	s=Sqrt[M[[1]][[1]]+M[[2]][[2]]+M[[3]][[3]]+1]/2;
	x=(M[[3]][[2]]-M[[2]][[3]])/4/s;
	y=(M[[1]][[3]]-M[[3]][[1]])/4/s;
	z=(M[[2]][[1]]-M[[1]][[2]])/4/s;
	Normalize[Map[Re,{s,x,y,z}]]
	]];
Matrix2Quat2=Function[M,
	Module[{a2=0.25(M[[1,1]]+M[[2,2]]+M[[3,3]]+1),
			b2=0.25(M[[1,1]]-M[[2,2]]-M[[3,3]]+1),
			c2=0.25(-M[[1,1]]+M[[2,2]]-M[[3,3]]+1),
			d2=0.25(-M[[1,1]]-M[[2,2]]+M[[3,3]]+1),
			ab=0.25(M[[3,2]]-M[[2,3]]),
			ac=0.25(M[[1,3]]-M[[3,1]]),
			ad=0.25(M[[2,1]]-M[[1,2]]),
			cd=0.25(M[[3,2]]+M[[2,3]]),
			db=0.25(M[[1,3]]+M[[3,1]]),
			bc=0.25(M[[2,1]]+M[[1,2]]),m2},
		m2=Max[a2,b2,c2,d2];
		Switch[m2,a2,{a2,ab,ac,ad}/Sqrt@a2,b2,{ab,b2,bc,db}/Sqrt@b2,c2,{ac,bc,c2,cd}/Sqrt@c2,d2,{ad,db,cd,d2}/Sqrt@d2]
	]];
Quat2Matrix= Function[{q},
	With[{q2=Normalize[q]},
	Module[{s=q2[[1]],vx=q2[[2]],vy=q2[[3]],vz=q2[[4]]},
	{{1-2*vy*vy-2*vz*vz,2*vx*vy-2*s*vz,2*vx*vz+2*s*vy},
	{2*vx*vy+2*s*vz,1-2*vx*vx-2*vz*vz,2*vy*vz-2*s*vx},
	{2*vx*vz-2*s*vy,2*vy*vz+2*s*vx,1-2*vx*vx-2*vy*vy}}
	]]];
Quat2Omega=Function[{qin},
	With[{q=Normalize[qin]},
	If[q[[1]]==1,{0,0,0},
		2ArcCos[q[[1]]] List@@q[[2;;4]] / Sqrt[1-q[[1]]^2]]
	]];
Rodrigues=Function[{w},
	Module[{k=Normalize[w],t=Norm[w]},
	IdentityMatrix[3]+Sin[t]SkewOmega[k]+(1-Cos[t])SkewOmega[k].SkewOmega[k]]
	];
VectorPairToAngularVelocity=Function[{v1,v2,v3,v4},
	Module[{q,w},
	q=Normalize[Matrix2Quat[UniqueRotationMatrix[v1,v2,v3,v4]]];
	w=2ArcCos[q[[1]]];
	w Normalize[{q[[2]],q[[3]],q[[4]]}]
	]];
GyroBiasFromAccel=Function[{i1,i2,vs,ts},
	Module[{v1=vs[[i1]],v2=vs[[i2]],v3=vs[[2i2-i1]]},
	VectorPairToAngularVelocity[v1,v2,v2,v3]/(ts[[i2]]-ts[[i1]]) 10^9
	]];
RotateAndSmooth=Function[{xyzt,qf,window},
	Module[{qg},
	qg=Map[{#[[4]],RotateByQuaternion[#[[1;;3]],
		qf[#[[4]]]]}&,Transpose@xyzt[[1;;4]]];
	Interpolation@MapThread[{#1,#2}&,
			{Map[First,qg],Transpose@Map[MedianFilter[#,window]&,Transpose[Map[#[[2]]&,qg]]]}]
	]];
(*GyroBiasFromAccelMag=Function[{i1,i2,xyzt,mag,qf,window,n},
	Module[{v1,v2,v3,v4,smag,qmag,qg,
		t1=xyzt[[4]][[i1]],t2=xyzt[[4]][[i2]]},
	smag=Transpose@Select[Transpose[mag[[1;;4]]],t1<=#[[4]]<=t2&];
	qg=RotateAndSmooth[Map[Take[#,{i1,i2}]&,xyzt],qf,window];
	qmag=RotateAndSmooth[smag,qf,window];
	Table[];
	]];*)
FitArc=Function[xyzs,
	Module[{r,n,center,radius,v1,v2},
	r=Map[Mean,Transpose@xyzs];
	n=Eigenvectors[Covariance[xyzs,xyzs],-1][[1]];
	center=PseudoInverse[2MapThread[#1-#2&,{Rest[xyzs],Most[xyzs]}]].MapThread[#1.#1-#2.#2&,{Rest[xyzs],Most[xyzs]}];
	radius=Mean@Map[Norm[#-center]&,xyzs];
	v1=GramSchmidt[xyzs[[1]]-center,n];
	v2=GramSchmidt[xyzs[[-1]]-center,n];
	{center,Abs@VectorAngle[v1,v2] Normalize@Cross[v1,v2]}
	]];
(*arc=Table[{Cos[t]-1,1,Sin[t]},{t,0,3Pi/4,0.1}]
Graphics3D@Line@arc
FitArc[arc]*)
Clamp=Function[{a},Function[x,Sign[x]Log[1+a Abs[x]]/a]];
SequenceAngularVelocity=Function[{l,stride},
	Table[FitArc[l[[x-stride;;x+stride]]][[2]]/(2stride+1),{x,stride+1,Length[l]-stride}]
	];
PointTorsion=Function[{p1,p2,p3,p4},
	Module[{b3,b2,t3,t2,n3,n2},
		If[Norm[p4-p3]Norm[p3-p2]Norm[p4-p2]Norm[p3-p1]==0,0,
		n3=Normalize[p4+p2-2p3];
		n2=Normalize[p3+p1-2p2];
		t3=Normalize[p4-p2];
		t2=Normalize[p3-p1];
		b3=Cross[t3,n3];
		b2=Cross[t2,n2];
		VectorAngle[b3,b2]/Norm[p3-p2]]
	]];
SequenceTorsion=Function[{l,stride},
	Table[PointTorsion[l[[x-2stride]],l[[x-stride]],l[[x]],l[[x+stride]]],{x,2stride+1,Length[l]-stride}]
	];
PointCurvature=Compile[{{p1,_Real,1},{p2,_Real,1},{p3,_Real,1}},
	With[{l=Norm[p2-p1]},
	If[l==0,VectorAngle[{1},{1}],VectorAngle[p2-p1,p3-p2]/l]
	]];
SequenceCurvature=Function[{l,stride},
	Table[PointCurvature[l[[x-stride]],l[[x]],l[[x+stride]]],{x,stride+1,Length[l]-stride}]
	];
(*r=1;h=0.03;k=r/(r^2+h^2);t=h/(r^2+h^2);
M={{0,k,0},{-k,0,t},{0,-t,0}};s=0.1;
Ms=NestList[(IdentityMatrix[3]+s M).#&,IdentityMatrix[3],1000];
Ts=Map[Normalize[#.{s-s^3k^2/6,s^2k/2,s^3k t/6}]&,Ms];
Graphics3D@Line@Accumulate@Ts*)
(*Graphics3D@Line@Accumulate@Map[#.{1,0,0}&,Table[MatrixExp[M t],{t,1,100,0.1}]]*)
TrajectoryFromCurvatureTorsion=Compile[{{ts,_Real,1},{curvatures,_Real,1},{torsions,_Real,1}},
	Accumulate@Map[Normalize[#.{1,0,0}]&,FoldList[#1.#2&,IdentityMatrix[3],MapThread[MatrixExp[#3{{0,#1,0},{-#1,0,#2},{0,-#2,0}}]&,{Most@curvatures,Most@torsions,Differences[ts]}]]]
	];
(*Graphics3D@Line@TrajectoryFromCurvatureTorsion[ts/10^9,Map[1&,ts],Map[0.03&,ts]]*)
(*CurvatureTorsionFromTrajectoryByFeedback=Compile[{{ts,_Integer,1},{xyzs,_Real,2},{kK,_Real},{kT,_Real}},
	FoldList[
		With[{dt=#2[[1]]/10^9},
			{#1[[1]]+,#1[[2]]+,}]&
		,{0,0,{1,0,0},{0,1,0}},MapThread[{#1,Normalize[#2],Normalize[#3]}&,{Differences@ts,Differences@xyzs,Differences@Differences@Prepend[xyzs,{0,0,0}]}]
	];*)
(*stride=50;Graphics3D@Line@TrajectoryFromCurvatureTorsion[ts[[;;-3stride-1]]/10^9,20SequenceCurvature[Accumulate@dirs,stride][[;;-stride-1]],20 SequenceTorsion[Accumulate@dirs,stride]]*)
intrinsicCurve3D[ {\[Kappa]_, \[Tau]_}, {s_, smin_, smax_}, {s0_, x0_, t0_, n0_}] :=
  Module[ {x, t, n, b},
   x /. First @
     NDSolve[
      {
       x'[s] == t[s],                               x[s0] == x0,
       t'[s] == \[Kappa] n[s],                           t[s0] == t0,
       n'[s] == \[Tau] b[s] - \[Kappa] t[s],         n[s0] == n0,
       b'[s] == -\[Tau] n[s],
       b[s0] == t0\[Cross]n0
       },
      {x, t, n, b},
      {s, smin, smax}
      ]
   ];
intrinsicCurve3D[ {\[Kappa]_, \[Tau]_}, {s_, smin_, smax_}] :=
  intrinsicCurve3D[{\[Kappa], \[Tau]}, {s, smin, smax}, {0, {0, 0, 0}, {1, 0, 0}, {0, 1, 0}}];
(*ParametricPlot3D[
 Evaluate@intrinsicCurve3D[ {1.3, 0.5 Sin[s]}, {s, 0, 4 \[Pi]}][s], {s, 0, 4 Pi}]
ParametricPlot3D[Evaluate@intrinsicCurve3D[ {0.1, 1}, {s, 0, 4 \[Pi]}][s], {s, 0, 4 Pi}]
*)
SingleLatLongToXY=Compile[{{latlng,_Real,1},{scaling,_Real}},
	6367000.0{latlng[[2]]scaling,latlng[[1]]}Pi/180.(10^-7)];
LatLongToXY=Compile[{{latlngs,_Real,2}},
	With[{scaling=Cos@Mean[latlngs[[All,1]]Pi/180/10^7]},
	Map[SingleLatLongToXY[#,scaling]&,latlngs]
	]];
ChopFile=Function[{fname,ratio1,ratio2},
	Module[{ofname="/tmp/"<>FileBaseName@fname<>"_part.txt",l},
	l=StringSplit[Import[fname],"\n"];
	Export[ofname,l[[Floor[Length@l ratio1];;Floor[Length@l ratio2]]]]
	]];
InsightToWalk=Function[{fname,ofname},
	Module[{xyzt,wxyzt,mag,rot,l3,g},
	{xyzt,wxyzt,mag,rot}=LoadData3[fname,#]&/@{";accel",";gyro",";compass",";rotation_vector"};
	g=Function[{xyzt,tag},
		{#[[1]],StringJoin[Riffle[{tag}~Join~(Function[x,ToString[x,InputForm]]/@#[[{2,3,4,1}]])," "]]}&/@xyzt];
	l3=Join@@{g[xyzt,"Acc"],g[wxyzt,"Gyo"],
		g[mag,"Compass"],g[rot,"Rot"]};
	Export[ofname,StringJoin[Riffle[{#[[2]],"0","walk",fname}," "]]&/@SortBy[l3,First]]
	]]
WalkToInsight=Function[{fname,ofname},
	Module[{g,xyzt,wxyzt,mags,rots,l3},
	g=Function[{xyzt,tag},
		{ToString[#[[1]]],tag<>StringJoin[Riffle[Map[Function[x,ToString[x,InputForm]],#[[2;;]]]," "]]}&/@xyzt];
	{xyzt,wxyzt,mags,rots}=Map[{#[[4]]}~Join~#[[1;;3]]&,
		Transpose[LoadData[fname,All,#][[1;;4]]]]&/@{"Acc","Gyo","Compass","Rot"};
	l3=Join@@{g[xyzt,";accel;"],g[wxyzt,";gyro;"],
		g[mags,";compass;"],g[rots,";rotation_vector;"]};
	Export[ofname,#[[1]]<>#[[2]]&/@SortBy[l3,First]]]]
CorrectStaticBias=Function[fname,
	Module[{marker,xyzt,wxyzt,mags,ofname,g,gyroBias,cwxyzt,l3},
	marker=LoadMarker3[fname];
	{xyzt,wxyzt,mags}=LoadData3[fname,#]&/@{";accel",";gyro",";compass"};
	ofname="~/"<>FileBaseName[fname]<>".corr.txt";
	g=Function[{xyzt,tag},
		{ToString[#[[1]]],tag<>StringJoin[Riffle[Map[Function[x,ToString[x,InputForm]],#[[2;;]]]," "]]}&/@xyzt];
	With[{n=30},
		gyroBias=Mean[Join@@Select[Partition[wxyzt[[All,2;;4]],n],Tr[Norm/@#]<0.03n&]]]
	If[Head[gyroBias]===List,
		cwxyzt=Map[{#[[1]]}~Join~(#[[2;;4]]-gyroBias)&,wxyzt];
	l3=Join@@{g[xyzt,";accel;"],g[cwxyzt,";gyro/static_corrected_bias;"],
		g[mags,";compass;"],g[marker,";latLngE7Marker;"]};
	Export[ofname,#[[1]]<>#[[2]]&/@SortBy[l3,First]]]]]
TrajectoryFromRotationVector=Function[fname,
	Module[{rot,rots,res,xys,ts},
	rot=LoadData2[fname,";rotation_vector"];
	{xys,ts}=LoadXYs[fname,"--ontg_use_peak"];
	rots=Interpolation[{#[[1]],#[[2;;4]]}&/@rot,InterpolationOrder->1]/@ts;
	res=Map[(*Function[x,Normalize[{x[[1]],x[[2]],0}]]@*)
		RotateByQuaternion[{0,1,0},{Re@Sqrt[1-#.#],#[[1]],#[[2]],#[[3]]}]&,rots];
	{fname,Accumulate@res}]]
TestTrajectoryMethods=Function[fname,
	Module[{xys,ts,rot,rots,res},
	{xys,ts}=LoadXYs[fname,"--ontg_use_peak"];
	rot=LoadData2[fname,";rotation_vector"];
	If[rot=!={},
		rots=Interpolation[{#[[1]],#[[2;;4]]}&/@rot,InterpolationOrder->1]/@ts;
		res=Map[RotateByQuaternion[{0,1,0},{Re@Sqrt[1-#.#],#[[1]],#[[2]],#[[3]]}]&,rots];];
	{{"rv",If[rot=!={},BlendPlot@Accumulate@res[[All,1;;2]]]},{"mncf",BlendPlot@xys},
		{"insight",{xys,ts}=LoadXYsGeneral[fname,"--generateMethod=Insight"];BlendPlot@xys},
		{"insightNoMag",{xys,ts}=LoadXYsGeneral[fname,"--generateMethod=InsightNoMag"];BlendPlot@xys},
		{"xins",{xys,ts}=LoadXYsGeneral[fname,"--generateMethod=Xins"];BlendPlot@xys}}]]
LoadMarker=Function[fname,
	With[{l=FindList[fname,";latLngE7Marker"]},
		N@ToExpression[{ToExpression[#[[1]]]10.^(-9)}~Join~{StringSplit[#[[3]]]}]&/@
			StringSplit[Fold[If[StringMatchQ[#2,__~~"CANCEL_LAST_MARKER"],#1[[;;-2]],Append[#1,#2]]&,{},l],";"]]];
InsightFileTimestamp=ReadList[#,Number,1][[1]]&;
LoadData2=Function[{fname,tag},
	Module[{l=FindList[fname,tag]},
		Developer`ToPackedArray@N@Map[{ToExpression[#[[1]]]10.^(-9)}~Join~ToExpression[
			StringSplit[StringReplace[#[[3]],"E"->"*10^"]," "][[;;]]]&,StringSplit[l,";"]]]];
(*LoadFile2=Function[fname,
	Module[{xyzt,wxyzt,mags,markers,
	];*)
Sample=Function[{xyzt,ts},
	Module[{f=Interpolation[{#[[1]],#[[2;;4]]}&/@(First/@SplitBy[xyzt,First]),InterpolationOrder->1]},
		f/@ts]];
(*ListLinePlot[Transpose@xyzt[[All,2;;4]],MaxPlotPoints->5000]
ListLinePlot[Transpose@wxyzt[[All,2;;4]],MaxPlotPoints->5000]*)
Mac2Long=FromDigits[StringReplace[#,":"->""],16]&;
Long2Mac=StringJoin@Riffle[StringJoin@@@Partition[PadLeft[StringSplit[IntegerString[#,16],""],12,"0"],2],":"]&;
LoadWifi=Function[fname,
	Module[{parse=Function[sigs,{ToLowerCase@If[StringCount[#,":"]>0,#,
		Long2Mac@ToExpression@#]&@ToString@#[[1]],ToExpression[#[[3]]]}&@StringSplit[#,","]&/@StringSplit[sigs]]},
	Select[({ToExpression[#[[1]]]10.^(-9),parse[#[[3]]]}&@StringSplit[#,";",All]&)/@FindList[fname,"wifi"],
		Last@#!={}&]]];
LoadMacs=Function[fname,Union[Join@@(#[[2,All,1]]&/@LoadWifi[fname])]];
LoadData3=Function[{fname,tag},
	Module[{l=FindList[fname,tag]},
		Map[ToExpression[
			{#[[1]]}~Join~StringSplit[StringReplace[#[[3]],"E"->"*10^"]," "][[;;3]]]&,StringSplit[l,";"]]]];
LoadMarker3=Function[fname,(*specialized for file format conversion*)
	With[{l=FindList[fname,";latLngE7Marker"]},
		ToExpression[{#[[1]]}~Join~StringSplit[#[[3]]]]&/@
			StringSplit[Fold[If[StringMatchQ[#2,__~~"CANCEL_LAST_MARKER"],#1[[;;-2]],Append[#1,#2]]&,{},l],";"]]];
LoadXYs=Function[{fname,flag},
	Module[{res,ts},
	res=Import["!~/bin/LocalEvaluator_deploy.jar --surveyFile="<>AbsoluteFileName[fname]
			<>" --generateMethod=MNCF "<>flag<>" --printPoints --printRawTracePoints","Table"];
	ts=10.^-9 res[[All,3]];
	{res[[All,1;;2]],ts}]];
LoadXYsNoMag=Function[{fname,flag},LoadXYs[fname,flag<>" "<>"--ontgFilter1MagGain=0 --ontgFilter2MagGain=0"]];
LoadNcfXYs=Function[{fname,flag},
	Module[{res,ts},
	res=Import["!~/bin/LocalEvaluator_deploy.jar --surveyFile="<>AbsoluteFileName[fname]
			<>" --generateMethod=NCF "<>flag<>" --printPoints --printRawTracePoints","Table"];
	ts=10.^-9 res[[All,3]];
	{res[[All,1;;2]],ts}]];
LoadXYs2=Function[{fname,flag},
	Module[{xys,ts},
	{xys,ts}=LoadXYs[fname,flag];
	MapThread[Prepend,{xys,ts}]]];
LoadXYf=Function[{fname,flag},
	Module[{xys,ts},
	{xys,ts}=LoadXYs[fname,flag];
	Interpolation[MapThread[List,{ts,xys}],InterpolationOrder->1]
	]];
LoadXYsGeneral=Function[{fname,flag},
	Module[{res,ts,dt=1},
		res=Import["!~/bin/LocalEvaluator_deploy.jar --surveyFile="<>AbsoluteFileName[fname]
			<>" "<>flag<>" --printPoints --printRawTracePoints","Table"];
	ts=Table[i,{i,res[[1,3]]10.^(-9),res[[-1,3]]10.^(-9),dt}];
	{Interpolation[MapThread[List,{res[[All,3]]10.^-9,res[[All,1;;2]]}],InterpolationOrder->1]/@ts,ts}]];
BuildDictionaryPair=Function[l,{Dispatch@Thread[#->Range@Length@#],Dispatch@Thread[Range@Length@#->#]}&[Union@l]];
MacsToIds=Function[wifis,Dispatch@MapIndexed[#->#2[[1]]&,Union@@wifis[[;;,2,;;,1]]]];
WifisToMatrix=Function[{wifis,mapping},FlattenAssocList[#,Length@mapping[[1]]]&/@(wifis[[;;,2]]/.mapping)];
PlotWifiMatrices=Function[ms,(Join@@Riffle[ms,Table[10,{Length@ms},{3},{Length@ms[[1,1]]}]])//Transpose//MatrixPlot];
BuildWifiModel=Function[{fname},
	Module[{xys,ts,xyf,flag="--ontg_use_peak",res,dt=1,vectorizeRSS,posmap,wifimap,wifis,mapping},
	{xys,ts}=LoadXYs[fname,flag];
	xyf=Interpolation[MapThread[List,{ts,xys}],InterpolationOrder->1];
	wifis=LoadWifi[fname];
	mapping=MacsToIds@wifis;
	vectorizeRSS=Function[rss,PadRight[#,Length@mapping]&@Normal@SparseArray[Select[((#[[1]]/.mapping)->#[[2]]&)/@rss,Head[#[[1]]]==Integer&]]];
	wifimap=Map[{xyf@#[[1]],vectorizeRSS@#[[2]]}&,wifis];
	posmap=Rule[#2,#]&@@@wifimap;
	{ts,xys,xyf,wifis,wifimap,posmap,vectorizeRSS}
	]];
BoundingBox={Min/@#,Max/@#}&@Transpose@#&;(*Graphics[{Opacity[0.1],Rectangle@@BoundingBox@markers[[1]]}]*)
BlendPoints=Function[{xys},With[{n=Length@xys},Riffle[Table[Hue[0.7-0.7i/n],{i,n}],Point/@xys]]];
BlendLine=Function[{xys},With[{n=Length@xys},Riffle[Table[Hue[0.7-0.7i/n],{i,n}],Line/@Partition[xys,2,1]]]];
BlendRedLine=Function[{xys},With[{n=Length@xys},Riffle[Table[RGBColor[N@i/n,0,0],{i,n}],Point/@xys]]];
BlendBlueLine=Function[{xys},With[{n=Length@xys},Riffle[Table[RGBColor[0,0,N@i/n],{i,n}],Point/@xys]]];
RainbowLine=Function[xys,Riffle[Line/@Partition[xys,2,1],{Red,Darker@Yellow,Blue,Green,Cyan,Blue,Purple}]];
BlendPlot[xys_,opts___]:=
	Module[{c=0},
	ListLinePlot[xys,PlotRange->All,ColorFunction->Function[{x,y},ColorData["Rainbow"][c++;N[c]/Length[xys]]],opts]];
MatrixContourPlot[m_,opts___]:=
	With[{f=Interpolation[Join@@MapIndexed[Append[#2,#]&,m,{2}],InterpolationOrder->1]},ContourPlot[f[x,y],{x,1,Dimensions[m][[1]]},{y,1,Dimensions[m][[2]]},opts]];
ConditionalFramed=Function[{l,f},If[f[#,l],Framed@#,#]&/@l];(*ConditionalFramed[Range[100],PrimeQ@#&]*)
MatrixListPlot3D[m_,opts___]:=ListPlot3D[Join@@MapIndexed[Append[#2,#]&,m,{2}],opts];
HausdorffDistance=Function[{pts,pts2,distFun},Module[{ptsNf=Nearest[pts],pts2Nf=Nearest[pts2]},
	0.5(Mean[distFun[First@ptsNf@#,#]&/@pts2]+Mean[distFun[First@pts2Nf@#,#]&/@pts])]];
TestWifiModel=Function[{fname,nf,posmap,vectorizeRSS},
	Module[{wifis2,xys2,ts2,predicted,pxys2},
		wifis2=LoadWifi[fname];
		{xys2,ts2}=LoadXYs[fname,"--ontg_use_peak"];
		predicted=nf[vectorizeRSS[#]][[1]]/.posmap&/@(Last/@wifis2);
		pxys2=Interpolation[Transpose@{First/@wifis2,predicted},InterpolationOrder->1]/@ts2;
		{Mean[Norm/@(pxys2-xys2)],(*Histogram[Norm/@(pxys2-xys2)]*),BlendPlot@xys2,BlendPlot[predicted],BlendPlot[Transpose[MedianFilter[#,3]&/@Transpose[predicted]]]}]];
LoadMarkerXY=Function[fname,
	Module[{marker=LoadMarker[fname]},
	MapThread[Prepend,{LatLongToXY[Last/@marker],First/@marker}]
	]];
LoadMarkerXYf=Function[fname,
	Module[{xyts=LoadMarkerXY@fname},
	SafeFirstOrderInterpolation[MapThread[List,{First/@xyts,Rest/@xyts}]]
	]];
LoadUncorrectedGyro=Function[{fname},
	Module[{l=Select[FindList[fname,";gyro"], !StringMatchQ[#,__~~"Corrected"~~__]&]},
		Developer`ToPackedArray@N@Map[{ToExpression[#[[1]]]10.^(-9)}~Join~ToExpression[
			StringSplit[StringReplace[#[[3]],"E"->"*10^"]," "][[;;3]]]&,StringSplit[l,";"]]]];
LoadQuaternionF=Function[fname,
	Module[{wxyzt,ts,qqs},
	wxyzt=LoadUncorrectedGyro[fname];
	qqs=Quiet@IntegrateAngularVelocity[wxyzt[[All,1]],10.^9wxyzt[[All,2;;4]]];
	Interpolation[qqs,InterpolationOrder->1]
	]];
LoadNcfQuaternionF=Function[{fname,flag},
	Module[{tfname=$TemporaryDirectory<>"/"<>ToString@Unique["NcfAttitude"]<>".csv",ts},
	Import["!~/bin/LocalEvaluator_deploy.jar --surveyFile="<>AbsoluteFileName[fname]<>" --oftg_attitude_dump_path="<>tfname
			<>" --generateMethod=NCF "<>flag,"Table"];
	Interpolation[{10.^-9#[[1]],#[[2;;]]}&/@Import[tfname],InterpolationOrder->1]]];
FitEllipsoid=Function[xyzs,
	Module[{x0,y0,z0,r,b,c,r2,rules},
	{r2,rules}=NMinimize[{Sum[Abs[Sqrt[(xyzs[[i,1]]-x0)^2+(xyzs[[i,2]]-y0)^2/b^2+(xyzs[[i,3]]-z0)^2/c^2]-r],{i,Length@xyzs}]},{x0,y0,z0,r,b,c}];
	{(r/.rules)^2,rules,ListLinePlot[(#[[1]]-x0)^2+(#[[2]]-y0)^2/b^2+(#[[3]]-z0)^2/c^2/.rules&/@xyzs]}]];
L1Minimization=Function[{A,b}, Module[
  {B, c, Aall, ball, x, lb, AT, m, n},
  {m, n} = Dimensions[A];
  AT = Transpose[A];
  B = SparseArray[{{i_, i_} -> 1}, {m, m}];
  Aall = Join[Transpose[Join[B, -AT]], Transpose[Join[B, AT]]];
  ball = Join[-b, b];
  c = Join[Table[1, {m}], Table[0, {n}]];
  lb = Table[-Infinity, {m + n}];
  x = LinearProgramming[c, Aall, ball, lb];
  x = Drop[x, m]
  ]];
BuildFineMarkerF=Function[{zts,markers},Module[{stepsf,stepToMarkerF},
	stepsf=Interpolation[Transpose@{zts,Range@Length@zts},InterpolationOrder->1];
	stepToMarkerF=Interpolation[MapThread[List,{Quiet[stepsf/@First/@markers],Last/@markers}],InterpolationOrder->1];
	Interpolation[MapThread[List,{zts,Quiet[stepToMarkerF/@stepsf/@zts]}],InterpolationOrder->1]
	]];
LoadMagModel2 = Function[{fname},
    Module[{prefix = "~/dumpsaves/", mx, zmags, zxys, zaxyzs, zts, mxyf, marker,wifis,qs,ncfqs},
    	mx = prefix <> FileBaseName@fname <> ".mx";
    	If[FileExistsQ[mx], {zmags, zxys, zaxyzs, zts, mxyf,wifis,qs,ncfqs} = Import[mx],
            {zmags, zxys, zaxyzs, zts,marker,wifis,qs,ncfqs} = BuildMagModel2[fname];
            mxyf = If[FindList[fname,";latLngE7Marker",1]==={},Interpolation[MapThread[List,{zts,zxys}],InterpolationOrder->1],
				BuildFineMarkerF[zts,marker]];
     		Export[mx, {zmags, zxys, zaxyzs, zts, mxyf,wifis,qs,ncfqs}, "MX"]];
    	{zmags, zxys, zaxyzs, zts, mxyf,wifis,qs,ncfqs}
    	]];
BuildMagModel2=Function[fname,
	Module[{mxyf,dxys,dts,ts,mags,sig,d,xys,ts2,axyzs,wifis,qs,ncfqs},
		{xys,ts2}=LoadXYs[fname,""];mxyf=Interpolation[MapThread[List,{ts2,xys}],InterpolationOrder->1];
		dxys=Norm/@Differences@xys;wifis=LoadWifi@fname;
		dts=Interpolation[MapThread[List,{Prepend[Accumulate@dxys,0],ts2}],InterpolationOrder->1];
		ts=Table[dts[d],{d,0,Tr[dxys],0.1}];
		{mags,axyzs}=((Interpolation[MapThread[List,{First/@#,Rest/@#}],InterpolationOrder->1]&@
			LoadData2[fname,#])/@ts)&/@{";compass",";accel"};
		ncfqs=LoadNcfQuaternionF[fname,""]/@ts;
		qs=LoadQuaternionF[fname]/@ts;
		{mags,mxyf/@ts,axyzs,ts,LoadMarker@fname,wifis,qs,ncfqs}]];
BuildMagModel=Function[fname,
	Module[{mags,xys,axyzs,ts,marker},
		{mags,xys,axyzs,ts,marker}=BuildMagModel2@fname;
		{Norm/@mags,xys}]];
SafeFirstOrderInterpolation=Interpolation[Last/@SplitBy[#,First],InterpolationOrder->1]&;
BuildMagMarkerModel2=Function[fname,
	Module[{mxyf=LoadMarkerXYf@fname,mxy=LoadMarkerXY[fname],dxys,dts,ts,mags,sig,d,xys,axyzs},
		xys=Rest/@mxy;
		dxys=Norm/@Differences@xys;
		dts=SafeFirstOrderInterpolation[MapThread[List,{Prepend[Accumulate@dxys,0],First/@mxy}]];
		ts=Table[dts[d],{d,0,Tr[dxys],0.1}];
		{mags,axyzs}=((Interpolation[MapThread[List,{First/@#,Rest/@#}],InterpolationOrder->1]&@LoadData2[fname,#])/@ts)&/@{";compass",";accel"};
		{mags,mxyf/@ts,axyzs,ts}]];
BuildMagMarkerModel=Function[fname,
	Module[{mags,mxys,ts},
	{mags,mxys,ts}=BuildMagMarkerModel2@fname;
	{Norm/@mags,mxys}]];
TestMagModel=Function[{fname,sig2,xys2},
	Module[{sig,xys,r2},
	{sig,xys}=BuildMagModel@fname;
	Function[ssig,
		r2=Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1];
		xys2[[Ordering[r2][[-1]]]]
		]/@Partition[sig,90,30]
	]];
PlotLabeledTrace=Function[fname,
	Module[{sig,xys,pxys},
	{sig,xys}=BuildMagModel@fname;
	pxys=MapIndexed[{#2[[1]],#1}&,#]&@xys;
	Image[#,ImageSize->700]&@Graphics[Text[ToString[#[[1]]],#[[2]]]&/@(First/@Partition[pxys,10]),Axes->True]
	]];
PlotLabeledMarkerTrace=Function[fname,
	Module[{mag,xys,pxys},
	{mag,xys}=BuildMagMarkerModel@fname;
	pxys=MapIndexed[{#2[[1]],#1}&,#]&@xys;
	Image[#,ImageSize->1000]&@Graphics[Text[ToString[#[[1]]],#[[2]]]&/@(First/@Partition[pxys,45]),Axes->True]
	]];
ShowMagSignature=Function[fname,
	Module[{mags,xys,ts},
	mags=LoadData2[fname,";compass"];
	{xys,ts}=LoadXYs[fname,""];
	{fname,Labeled[ListLinePlot[Norm/@mags[[All,2;;4]]],"Mag-Norm"],Labeled[ListLinePlot[Transpose@mags[[All,2;;4]]],"Mag-XYZ"],Labeled[ListLinePlot@xys,"Trajectory"]}
	]];
InducedTrace=Function[{sxys,xys},
	Module[{nf=Nearest[xys->Automatic]},
	nf[#][[1]]&/@sxys
	]];
LoadPressure=Function[fname,ToExpression[StringSplit[#,";"][[-1]]]&/@FindList[fname,";pressure"]];
PlotMagneticVectors=Function[{fname,size,mean,scale},
  Module[{mag, xys, ts, rot, rots, res, mags},
    mag = LoadData2[fname, ";compass"];
    {xys, ts} = LoadXYs[fname, "--ontg_use_peak"];
    mags = Interpolation[Map[{#[[1]], #[[2 ;;]]} &, mag], InterpolationOrder -> 1] /@ ts;
    rot = LoadData2[fname, ";rotation_vector"];
    rots = Interpolation[{#[[1]], #[[2 ;; 4]]} & /@ rot,InterpolationOrder -> 1] /@ ts;
    res = MapThread[RotateByQuaternion[#2, {Re@Sqrt[1 - #.#], #[[1]], #[[2]], #[[3]]}] &, {rots, mags}];
    Image[#,ImageSize->size]&@Graphics[Arrow/@MapThread[List, {xys, xys + scale (#-mean&/@res[[All, 1 ;; 2]])}],PlotLabel->fname]]];
FireMatch=Function[{orig,end},{Darker@Yellow,Line[{orig,end}],Red,Point[end]}];
PlotBearingInPosition=Function[{ncfqs2,mxys2,n},
	Graphics[{Opacity[0.1],Line@Standardize[LatLongToXY[mxys2],Mean,1&],Opacity[1]}~Join~(
		Join@@MapThread[FireMatch[#,#+5#2]&,
		{Standardize[LatLongToXY[mxys2[[;;;;n]]],Mean,1&],Most@RotateByQuaternion[{0,1,0},#]&/@ncfqs2[[;;;;n]]}])]];
(*Image[#,ImageSize->2000]&@PlotBearingInPosition[ncfqs2,mxys2,100]*)
SegmentDistance = 
  Function[{p1, p2}, 
   Min[Norm[First@p1 - First@p2] + Norm[Last@p1 - Last@p2], 
    Norm[First@p1 - Last@p2] + Norm[Last@p1 - First@p2]]];
SvdApprox=Function[{m,k},
	Module[{u,w,v},
	{u,w,v}=SingularValueDecomposition[m,k];
	u.w.Transpose[v]]];
RelativisticCenter =
  Function[l, 
   Quiet[FindMinimum[Tr[Sqrt[1 + (# - x)^2] & /@ l], {x, Median[l]}, 
      Method -> "PrincipalAxis"]][[2, 1, 2]]];
BuildWifiMarkerModel=Function[{fname},
	Module[{xys,ts,xyf,res,dt=1,vectorizeRSS,posmap,wifimap,wifis,mapping},
	{xys,ts}={Rest/@#,First/@#}&@LoadMarkerXY[fname];
	xyf=Interpolation[MapThread[List,{ts,xys}],InterpolationOrder->1];
	wifis=LoadWifi[fname];
	mapping=MapIndexed[#1->#2[[1]]&,Union[First/@Join@@Last/@wifis]];
	vectorizeRSS=Function[rss,PadRight[#,Length@mapping]&@Normal@SparseArray[Select[((#[[1]]/.mapping)->#[[2]]&)/@rss,Head[#[[1]]]==Integer&]]];
	wifimap=Map[{xyf@#[[1]],vectorizeRSS@#[[2]]}&,wifis];
	posmap=Rule[#2,#]&@@@wifimap;
	{ts,xys,xyf,wifis,wifimap,posmap,vectorizeRSS}
	]];
DynamicTimeWarp=Function[{sig,sig2,dist},
	Module[{cost,n=Length@sig,m=Length@sig2,pre},
		Do[cost[0,i]=Infinity,{i,m}];
		Do[cost[i,0]=Infinity,{i,n}];
		cost[0,0]=0;
		pre[{0,0}]={0,0};
		Do[With[{min=Min[cost[i-1,j],cost[i,j-1],cost[i-1,j-1]]},
			cost[i,j]=dist[sig[[i]],sig2[[j]]]+min;
			Switch[min,cost[i-1,j-1],pre[{i,j}]={i-1,j-1},
				cost[i,j-1],pre[{i,j}]={i,j-1},
				cost[i-1,j],pre[{i,j}]={i-1,j}]],{i,n},{j,m}];
		{cost[n,m],Drop[Reverse@FixedPointList[pre,Length/@{sig,sig2}],2]}
	]];
DynamicTimeWarpWindow=Function[{sig,sig2,dist,w2},
	Module[{cost,n=Length@sig,m=Length@sig2,pre,w},
		w=Max[w2,Abs[n-m]];
		cost[_,_]=Infinity;
		cost[0,0]=0;
		pre[{0,0}]={0,0};
		Do[With[{min=Min[cost[i-1,j],cost[i,j-1],cost[i-1,j-1]]},
			cost[i,j]=dist[sig[[i]],sig2[[j]]]+min;
			Switch[min,cost[i-1,j],pre[{i,j}]={i-1,j},
				cost[i,j-1],pre[{i,j}]={i,j-1},
				cost[i-1,j-1],pre[{i,j}]={i-1,j-1}]],{i,n},{j,Max[1,i-w],Min[m,i+w]}];
		{cost[n,m],Drop[Reverse@FixedPointList[pre,Length/@{sig,sig2}],2]}
	]];
CreateClosureBuffer=Function[n,Module[{lst={}},Function[l,
	lst=If[Length@lst<n,Append[lst,l],Append[lst[[-n+1;;]],l]]]]];
(*v={0,0,0};buf3=CreateClosureBuffer[100];
Dynamic[ListPlot[Norm/@buf3[v]]]
Dynamic[ListPlot[Transpose@buf3[v]]]
Dynamic[ListLogPlot[Abs@Fourier[Norm/@buf3[v]]]]
*)

(*Why is the result negated?*)
(*HilbertTransform=Function[u,InverseFourierTransform[-I Sign[w] FourierTransform[u,t,w],w,t]];
HilbertTransform[Cos[t]]
HilbertTransform[Sin[t]]
HilbertTransform[1/(1+t^2)]
HilbertTransform@HilbertTransform[Cos[t]]*)
(*Method below from WIKI seems broken*)
(*ListConvolve[Table[If[Mod[n,2]==1,0,2./Pi/(n-1)],{n,Length@l}],l]*)

(*Not working*)
(*HarmonicConjugate=Function[u,
	Module[{v},
	DSolve[{D[v[x,y],x]==-D[u,y],D[v[x,y],y]==D[u,x]},v[x,y],{x,y}]]];
HarmonicConjugate[Exp[x]Sin[y]]
HarmonicConjugate[Cos[x+I y]]
HarmonicConjugate[x/(x^2+y^2)]*)
SoftThreshold = 
  Function[{\[Epsilon], x}, 
   If[x > \[Epsilon], x - \[Epsilon], 
    If[x < -\[Epsilon], x + \[Epsilon], 0]], Listable];
SoftThresholdApprox = Function[{\[Epsilon], M, n},
   	Module[{U, S, V},
    	{U, S, V} = 
     SingularValueDecomposition[M, n, Tolerance -> 10^-2];
    	U.SoftThreshold[\[Epsilon], S].Transpose[V]
    	]];
ShowFourier2D=Image@RotateLeft[#,Floor[Dimensions[#]/2]]&/@{Abs@#,Arg@#}&;
Fourier2D = Fourier /@ Transpose[Fourier /@ Transpose[#]] &;
InverseFourier2D = InverseFourier /@ Transpose[InverseFourier /@ Transpose[#]] &;
ListConvolve2D=Function[{m,m2},InverseFourier2D[Sqrt[Length@m] Fourier2D[m] Fourier2D[m2]]];
Rpca=Function[{D,\[Lambda],iter},
	(*n2=3n*)
	Module[{Y1,L,S,norm2,\[Mu]1,\[Rho]=1.5,m=Dimensions[D][[1]],n2=Dimensions[D][[2]],Zs,sv=10,svp,U,SS,V,Z,
			dnorm=Norm[D,"Frobenius"]},
		L=S=Table[0.,{m},{n2}];
		norm2=SingularValueDecomposition[D,1][[2,1,1]];
		Y1=D/Max[norm2,Norm[Flatten@D,Infinity]/\[Lambda]];
		\[Mu]1=1.25/norm2;
	Do[
		Zs=Y1/\[Mu]1+D-L;
		S=SoftThreshold[\[Lambda]/\[Mu]1,Zs];
		{U,SS,V}=SingularValueDecomposition[(Y1+\[Mu]1(D-S))/\[Mu]1,Min[sv,m,n2]];
		svp=Max[1,Length@Select[Diagonal[SS],#>1/\[Mu]1&]];
		sv=If[svp<sv,Min[svp+1,n2],Min[svp+Round[0.05n2],n2]];
		L=U[[All,;;svp]].DiagonalMatrix[Diagonal[SS][[;;svp]]-1/\[Mu]1].Transpose[V[[All,;;svp]]];
		Z=D-L-S;
		Y1+=\[Mu]1 Z;
		\[Mu]1=Min[\[Mu]1 \[Rho],10^7];
		If[Norm[Z,"Frobenius"]/dnorm < 10^-7,Print[i];Break[]];
		,{i,iter}];
	{L,S}
	]];
RpcaMatrixComplete=Function[{D,\[CapitalOmega],\[Lambda],iter},(*D is the observable part*)
	(*n2=3n*)
	Module[{Y1,L,S,norm2,\[Mu]1,\[Rho]=1.5,m=Dimensions[D][[1]],n2=Dimensions[D][[2]],Zs,sv=10,svp,U,SS,V,
			dnorm=Norm[D,"Frobenius"]},
		L=S=Table[0.,{m},{n2}];
		norm2=SingularValueDecomposition[D,1][[2,1,1]];
		Y1=D/Max[norm2,Norm[Flatten@D,Infinity]/\[Lambda]];
		\[Mu]1=1.25/norm2;
	Do[
		Zs=Y1/\[Mu]1+D-L;
		S=\[CapitalOmega] SoftThreshold[\[Lambda]/\[Mu]1,Zs]+(1-\[CapitalOmega])Zs;
		{U,SS,V}=SingularValueDecomposition[(Y1+\[Mu]1(D-S))/\[Mu]1,Min[sv,m,n2]];
		svp=Max[1,Length@Select[Diagonal[SS],#>1/\[Mu]1&]];
		sv=If[svp<sv,Min[svp+1,n2],Min[svp+Round[0.05n2],n2]];
		L=U[[All,;;svp]].DiagonalMatrix[Diagonal[SS][[;;svp]]-1/\[Mu]1].Transpose[V[[All,;;svp]]];
		Y1+=\[Mu]1(D-L-S);
		\[Mu]1=Min[\[Mu]1 \[Rho],10^7];
		If[Norm[Z,"Frobenius"]/dnorm < 10^-7,Print[i];Break[]];
		,{i,iter}];
	{L,S}
	]];
MatrixComplete09=Function[{D,\[CapitalOmega],iter},
	(*n2=3n*)
	Module[{Y1,L,\[Mu]1=1.,\[Rho],m=Dimensions[D][[1]],n2=Dimensions[D][[2]],Zs,norm2,sv=5,svn,svp,U,SS,V,Z,
			tol=10^-7,dnorm=Norm[D,"Frobenius"]},
		\[Rho]=1.1+2.5 Length@Flatten@\[CapitalOmega]/m/n2;
		norm2=SingularValueDecomposition[D,1][[2,1,1]];
		\[Mu]1=0.3/norm2;
		L=S=Table[0.,{m},{n2}];
		Y1=Table[0.,{m},{n2}];
	Do[
		Z=If[i==1,D,Z+Y1/\[Mu]1];
		{U,SS,V}=SingularValueDecomposition[L+Z,Min[sv,m,n2],Tolerance->Min[0.1tol,0.01/\[Mu]1]];
		svp=svn=Min[sv,Max[1,Length@Select[Diagonal[SS],#>1/\[Mu]1&]]];
		sv=If[svp<sv,Min[svp+1,n2],Min[svp+10,n2]];
		L=U[[All,;;svp]].DiagonalMatrix[Diagonal[SS][[;;svp]]-1/\[Mu]1].Transpose[V[[All,;;svp]]];
		Z=D-\[CapitalOmega] L;
		Y1+=\[Mu]1 Z;
		\[Mu]1*=\[Rho];
		If[Norm[Z,"Frobenius"]/dnorm < tol,Print[i];Break[]];
		,{i,iter}];
	L
	]];
RpcaColor=Function[{W,D,\[CapitalOmega],\[Lambda],\[Eta],iter},
	(*n2=3n*)
	Module[{Y1,Y2,L,S,X,norm2,\[Mu]1,\[Mu]2,\[Rho]=1.5,m=Dimensions[D][[1]],n2=Dimensions[D][[2]],T,Zs,sv=10,svp,U,SS,V,
			dnorm=Norm[D,"Frobenius"]},
		X=L=S=Table[0.,{m},{n2}];
		T=Transpose@ArrayFlatten[1./3{{IdentityMatrix[n2/3],IdentityMatrix[n2/3],IdentityMatrix[n2/3]}}];
		norm2=SingularValueDecomposition[D,1][[2,1,1]];
		Y1=Y2=Sign[D]/Max[norm2,Norm[Flatten@D,Infinity]/\[Lambda]];
		\[Mu]1=\[Mu]2=1.25/norm2;
	Do[
		Zs=Y1/\[Mu]1+D-L;
		S=\[CapitalOmega] SoftThreshold[\[Lambda]/\[Mu]1,Zs]+(1-\[CapitalOmega])Zs;
		{U,SS,V}=SingularValueDecomposition[(Y1+Y2+\[Mu]1(D-S)+\[Mu]2 X)/(\[Mu]1+\[Mu]2),Min[sv,m,n2]];
		svp=Max[1,Length@Select[Diagonal[SS],#>1/(\[Mu]1+\[Mu]2)&]];
		sv=If[svp<sv,Min[svp+1,n2],Min[svp+Round[0.05n2],n2]];
		L=U[[All,;;svp]].DiagonalMatrix[Diagonal[SS][[;;svp]]-1/(\[Mu]1+\[Mu]2)].Transpose[V[[All,;;svp]]];
		X=Transpose@LinearSolve[Transpose[(\[Eta] T.Transpose[T]+\[Mu]2 IdentityMatrix[n2])],Transpose[\[Eta] W.Transpose[T]-Y2+\[Mu]2 L]];
		Y1+=\[Mu]1(D-L-S);
		Y2+=\[Mu]2(X-L);
		\[Mu]1=Min[\[Mu]1 \[Rho],10^7];\[Mu]2=Min[\[Mu]2 \[Rho],10^7];
		If[Norm[Z,"Frobenius"]/dnorm < 10^-7,Print[i];Break[]];
		,{i,iter}];
	{L,S}
	]];
FactorizeByGravityYPointingDirs=Function[{vec,rawGrav,r},
	Module[{g,x,grav,xdirs,f=Compile[{{v1,_Real},{v2,_Real},{v3,_Real},{a,_Real},{b,_Real},{c,_Real}},
		With[{tnorm=Abs[a b]^2+Abs[b c]^2+Abs[a^2+c^2]^2},If[tnorm==0,0,(-a b v1+a^2 v2+c (c v2-b v3))/Sqrt[tnorm]]]]},
	grav=Normalize/@GaussianFilter[rawGrav,{{r,0}}];
	g=MapThread[Dot,{vec,grav}];
	xdirs=Normalize@{#[[3]],0,-#[[1]]}&/@grav;
	x=MapThread[Dot,{vec,xdirs}];
	Transpose@{x,MapThread[f[#[[1]],#[[2]],#[[3]],#2[[1]],#2[[2]],#2[[3]]]&,{vec,grav}],g}
	]];
CompensatedSquareErrorOneDim=Function[{xs,ys,\[Lambda]},
	Module[{ker=Table[1.,{Length@xs}],n=Length@xs},
	Mean[xs^2]+\[Lambda] Mean[xs]^2+
		(ListCorrelate[ker,ys^2]-2ListCorrelate[xs,ys])/n+
			\[Lambda] (ListCorrelate[ker,ys]/n)^2-2\[Lambda] Mean[xs]ListCorrelate[ker,ys]/n
		]];
CompensatedErrorLiftToMultiDim=Function[{oneDim},
	Function[{needle,hay,\[Lambda]},
		Total[oneDim[#[[1]],#[[2]],\[Lambda]]&/@Transpose@{Transpose@needle,Transpose@hay}]]
	];
CompensatedSquareError=CompensatedErrorLiftToMultiDim[CompensatedSquareErrorOneDim];
CompensatedQuadErrorOneDim=Function[{xs,ys,\[Lambda]},
	Module[{ker=Table[1.,{Length@xs}],n=Length@xs},
	Sqrt[Mean[xs^4]+(-4ListCorrelate[xs^3,ys]+6ListCorrelate[xs^2,ys^2]-4ListCorrelate[xs,ys^3]+ListCorrelate[ker,ys^4])/n]
		]];
CompensatedQuadError=CompensatedErrorLiftToMultiDim[CompensatedQuadErrorOneDim];
CompensatedCorrelationErrorOneDim=Function[{xs,ys,\[Lambda]},
	Module[{ker=Table[1.,{Length@xs}],n=Length@xs,ey},
	ey=ListCorrelate[ker,ys]/n;
	200(1-(ListCorrelate[xs,ys]/n-Mean[xs]ey)/Sqrt[(ListCorrelate[ker,ys^2]/n-ey^2)(Mean[xs^2]-Mean[xs]^2)])
	]];
CompensatedCorrelationError=CompensatedErrorLiftToMultiDim@CompensatedCorrelationErrorOneDim;
CompensatedAbsoluteErrorOneDim=Function[{xs,ys,\[Lambda]},
	ListCorrelate[xs,ys,{1,-1},0,Abs[#-#2]&,Plus]/Length@xs
	];
CompensatedAbsoluteError=CompensatedErrorLiftToMultiDim@CompensatedAbsoluteErrorOneDim;
DilateSignal = Function[{xs, \[Lambda]},
   	Interpolation[MapIndexed[{\[Lambda] First@#2, #} &, xs], 
     InterpolationOrder -> 1] /@ Range[Floor[\[Lambda] Length@xs]]
   	];
(*ListPlot[DilateSignal[Range[6],0.8],PlotRange->All]*)
NanoToDateList=DateList[AbsoluteTime[{1970,1,1,0,0,0}]+#/10.^9]&
(*LatLongDistance = Function[{latlng, latlng2},
   	Module[{dlatlng = Sin[0.5 (latlng2 - latlng) Pi/180./10^7], x},
    	x = #[[1]]^2 + #[[2]]^2 Cos[latlng[[1]] Pi/180./10^7] Cos[
          latlng2[[1]] Pi/180./10^7] &@dlatlng;
    	6367000.0 2 ArcTan[Sqrt[Max[0., 1. - x]], Sqrt[x]]
    ]];(*{LatLongDistance[10^7FromDMS/@{{39,59,33.08},{116,19,27.17}},10^7FromDMS/@{{39,59,31.71},{116,19,28.61}}],54.5}*)*)
LatLongDistance=GeoDistance[# 10^-7,#2 10^-7]&;
LatLongToTileURL=With[{lngToIndex=Function[{lng, zoom},Module[{alng,blng,a,b},
		{alng,blng}=First[{a,b}/.Solve[{a*(-180)+b==0,a(180)+b==1},{a,b}]];Floor[(alng*lng+blng)2^zoom]]],
	mercator=Function[lat,Log[Abs[Sec[lat*Degree]+Tan[lat*Degree]]]],
	indicesToTileURL=Function[{x,y,zoom},"http://mt0.google.com/vt/x="<>ToString[x]<>"&y="<>
		ToString[y]<>"&z="<>ToString[zoom]]},
	With[{latToIndex=Function[{lat,zoom},Module[{a,b,alat,blat},
		{alat,blat}=First[{a,b}/.Solve[{a*mercator[85.0511287798066]+b==0,a*mercator[-85.0511287798066]+b==1},{a,b}]];
		Floor[(alat*mercator[lat]+blat)2^zoom]]]},
	Function[{latlng,zoom},
		indicesToTileURL[lngToIndex[latlng[[2]],zoom],latToIndex[latlng[[1]],zoom],zoom]
	]]];
(*Import@LatLongToTileURL[FindGeoLocation[],13]*)
ResampleWithRate = Function[{sigs, k},(*return Floor[\[Kappa] Length@sigs] many samples*)
   Module[{f = 
      Quiet@Interpolation[
        MapThread[List, {Range[0, Length@sigs - 1], sigs}], 
        InterpolationOrder -> 1], dt, newL = Floor[k Length@sigs]},
    		If[Length@sigs == 1, Table[sigs[[1]], {newL}],
     			dt = (newL - 1)/(Length@sigs - 1);
     			If[newL == 1, sigs[[;; 1]], 
      f /@ ((# - 1)/dt & /@ Range[newL])]]]];
LocalMinimaIndices=Function[{l,r},Module[{n=Length@l},(*Slow*)
	Reap[Scan[If[And@@(Function[k,#==k||l[[k]]>l[[#]]||l[[k]]==l[[#]]&&#<k]/@Select[Range[#-r,#+r],(#>=1&&#<=n)&]),Sow[#]]&,Range@n]][[2,1]]]];
MeanAngle=Compile[{{x,_Real,1}}, Arg[Mean[Exp[I x]]]];(*http://en.wikipedia.org/wiki/Mean_of_circular _quantities*)
(*MeanAngle /@ ({{350, 10}, {90, 180, 270, 360}, {10, 20, 30}}Degree)*)
(*ParallelExport=Function[{ofname,gs},
	Module[{tfnames=Table[$TemporaryDirectory<>"/"<>ToString@Unique["ParallelExport"]<>"."<>FileExtension@ofname,{Length@gs}]},
	Parallelize@MapThread[Export,{tfnames,gs}];
	Export[ofname,Import/@tfnames]]];*)
SimpleTokenizer=Function[s,ToLowerCase@StringReplace[s,Thread[{"+","-","*","/","\\",".",",","!",":",";","'","\"","[","]","(",")","@","#","$","%","^","&","_","=","{","}","<",">","?","`"}->""]]];
JoinTablesByKey=Function[{tbs,doInner},
	Module[{nils=Array[""&,Length@First@#-1]&/@tbs,keys=If[doInner,Intersection@@(#[[;;,1]]&/@tbs),Union@@(#[[;;,1]]&/@tbs)],dispatches=Dispatch@Thread[#[[;;,1]]->#[[;;,2;;]]]&/@tbs},
	Table[Prepend[Join@@(Table[If[Head[key/.#]===List,key/.#,nils[[i]]]&@dispatches[[i]],{i,Length@dispatches}]),key],{key,keys}]]];
NonzeroMask=N@SparseArray[SparseArray[#]["NonzeroPositions"]->1,Dimensions@#]&;
RandomSparseMatrix=Function[{m,n,nnzRatio},N@SparseArray[(RandomSample[Join@@Table[{i,j},{i,m},{j,n}],Floor[m n nnzRatio]])->1,{m,n}]];
(*SplitToRuns@{1,2,3,-1,0,4}=={{1,2,3},{-1,0,4}}
SplitToRuns@{1,2,3,2,-1,0,4}=={{1,2,3},{3,2,-1},{-1,0,4}}
SplitToRuns@{1,2,3,-1,0,4,4,3,4,5}=={{1,2,3},{-1,0,4,4},{3,4,5}}*)
SplitToRunsBy=Function[{l,f},Append[#[[;;,1]],#[[-1,-1]]]&/@Select[SplitBy[Partition[l,2,1],f[#[[1]],#[[2]]]&],Length@#>1&]];
SplitToRuns=SplitToRunsBy[#,#<=#2&]&;
DropWhile=Drop[#,Length@TakeWhile[#,#2]]&;
(*DropWhile[{1,2,3,5,1,2},#<5&]=={5,1,2}*)
SplitHeaderBy=Join@@@Partition[DropWhile[SplitBy[#,#2],With[{f=#2},Not[f[#[[1]]]]&]],2]&;
(*la={1,2,3,1,2,5,1}
SplitHeaderBy[la,#==1&]=={{1,2,3},{1,2,5}}
SplitHeaderBy[la,#==2&]=={{2,3,1},{2,5,1}}
SplitHeaderBy[la,#==5&]=={{5,1}}*)
CsvToJson=Function[With[{header=#[[1]]},Thread[header->#]&/@#[[2;;]]]];
ShowImagePoints=Function[{img,points,scale},Show[img,Graphics[Table[{{Yellow,Circle[p,scale]}},{p,points}]]]];
EndPackage[ ]
