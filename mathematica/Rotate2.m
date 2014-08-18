(* ::Package:: *)

Needs["Quaternions`"]
$HistoryLength=1;
<< "/home/georgezhou/git_math/google3/experimental/users/georgezhou/mathematica/Sensor2.m"


xyzt=ProcData@LoadData["~/walk1.txt",All,"Acc"];
wxyzt=ProcData@LoadData["~/walk1.txt",All,"Gyo"];
mag=ProcData@LoadData["~/walk1.txt",All,"Compass"];
xyztf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[xyzt[[1;;4]]]],InterpolationOrder->1];
wxyztf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[wxyzt[[1;;4]]]],InterpolationOrder->1];
magf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[mag[[1;;4]]]],InterpolationOrder->1];
isMoving=FindMoving[xyzt,0.4,15];isMovingF=Interpolation@isMoving;
ts=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.05 10^9}];
dt=N[Median[Differences@ts]/10^9]
xyzs=Map[xyztf,ts];wxyzs=Map[wxyztf,ts];mags=Map[magf,ts];
qqs=IntegrateAngularVelocity[wxyzt[[4]],Transpose@wxyzt[[1;;3]]];qf=Interpolation[qqs,InterpolationOrder->1];qs=Map[qf,ts];
movings=Map[Max[0,Min[1,N@isMovingF@#]]&,wxyzt[[4]]];
ts2=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.5 10^9}];


l=GaussianFilter[Take[Map[Norm,Transpose@xyzt[[1;;3]]],All],10];
(*ListPlot@l*)
(*Differences@l;*)
(*Normal@CrossingDetect@Differences@l;*)
Total@Normal@CrossingDetect@Differences@l/4.


f=Function[{xs,theta,kappa},
	FoldList[Function[{s,x},{s[[1]]+x-s[[2]],(1-kappa)(s[[2]]+theta (s[[1]]+x-s[[2]]))+kappa x}],{0,0},xs]]


ListLinePlot[Map[Norm,Transpose[xyzt[[1;;3]]]][[1000;;1200]],MaxPlotPoints->1000]


ListLinePlot[Map[#[[1]]&,f[Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.3,0.8]],0.3,0.8]][[1000;;1200]],MaxPlotPoints->1000]


ListLinePlot[Map[#[[1]]&,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.9]][[1000;;1200]],MaxPlotPoints->1000]


LocalExtrema=Function[{l,w,sign},Map[SortBy[#,Function[x,sign x[[2]]]][[-1]]&,Partition[l,w]]];
l=MapThread[{#1,Norm@#2}&,{xyzt[[4]],Transpose@xyzt[[1;;3]]}];
w=20;
ListLinePlot[{l,LocalExtrema[l,w,1],LocalExtrema[l,w,-1]}]
f1=Interpolation[LocalExtrema[l,w,1],InterpolationOrder->3];
f2=Interpolation[LocalExtrema[l,w,-1],InterpolationOrder->3];
l2=l-Map[(f1@First[#]+f2@First[#])/2.&,l];
{Image[ListLinePlot[l],ImageSize->800],Image[ListLinePlot[l2],ImageSize->800],Image[ListLinePlot[LocalExtrema[l,w,1]],ImageSize->800],Image[ListLinePlot[LocalExtrema[l,w,-1]],ImageSize->800]}
ListLinePlot[l2[[1000;;2000]]]
ListLinePlot[l[[1000;;2000]]]


ListLinePlot[Flatten[LocalMaxima@MapThread[{#1,Norm@#2}&,{xyzt[[4]],Transpose@xyzt[[1;;3]]}],1]]


ListLinePlot@l[[1000;;1500]]


l=Map[Norm,Transpose[xyzt[[1;;3]]]];
l2=GaussianFilter[Function[x,x-GaussianFilter[x,10]][l],5]-0.8;
ListLinePlot@l2
Total@CrossingDetect@l2/2.


ListLinePlot[Function[x,GaussianFilter[x-GaussianFilter[x,10],5]-0.8][Map[Norm,Transpose[xyzt[[1;;3]]]]][[1000;;2000]],MaxPlotPoints->1000]


Show[ListLinePlot[{MapThread[{#1,#2}&,{xyzt[[4]],Map[Norm,Transpose[xyzt[[1;;3]]]]}]}],ListPlot[Map[{First@#,6}&,Import["~/t.csv"]]]]


DoG=Function[{xs,ts},
	Module[{w=2 10^8/Median[Differences@ts]},
	GaussianFilter[Differences@GaussianFilter[xs,w],w]
	]];
(*Image[ListLinePlot[DoG[Map[Norm,Transpose[xyzt[[1;;3]]]],xyzt[[4]]],MaxPlotPoints->2000],ImageSize->2000]
Image[ListLinePlot[DoG[DoG[Map[Norm,Transpose[xyzt[[1;;3]]]],xyzt[[4]]],xyzt[[4]]],MaxPlotPoints->2000],ImageSize->2000]
Image[ListLinePlot[DoG[DoG[DoG[Map[Norm,Transpose[xyzt[[1;;3]]]],xyzt[[4]]],xyzt[[4]]],xyzt[[4]]],MaxPlotPoints->2000],ImageSize->2000]*)
CountStepPeak=Function[xyzt,
	Module[{w},
	w=2 10^8/Median[Differences@xyzt[[4]]];
	(*Total@CrossingDetect@Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.8]]/2.*)
	(*Total@CrossingDetect@Function[x,GaussianFilter[x-GaussianFilter[x,10],5]-0.8][Map[Norm,Transpose[xyzt[[1;;3]]]]]/2.*)
	Total@CrossingDetect@Function[x,
		Module[{l2=GaussianFilter[Differences@GaussianFilter[x,w],w]},
		(*Module[{l2=GaussianFilter[ListCorrelate[{1/280,-4/105,1/5,-4/5,0,4/5,-1/5,4/105,-1/280},GaussianFilter[x,w]],w]},*)
			l2-0.6Sqrt@Variance@l2]][Map[Norm,Transpose[xyzt[[1;;3]]]]]/2.
	]];
xyzt=LoadData["/usr/local/google/users/georgezhou/step/walk5.txt",All,"Acc"];
(*xyzt=LoadData["~/doc/sensor/step/walk5.txt",All,"Acc"];*)
CountStepPeak[xyzt]
(*l=Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.8]];*)
l=Map[Norm,Transpose[xyzt[[1;;3]]]];
w=2 10^8/Median[Differences@xyzt[[4]]];
image1=Image[ListLinePlot[
			l,
			(*Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.8]],*)
				MaxPlotPoints->1000,PlotRange->All],ImageSize->1000]
l2=GaussianFilter[Differences@GaussianFilter[l,w],w];
image2=Image[ListLinePlot[
			l2,
			(*Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.8]],*)
				MaxPlotPoints->1000,PlotRange->All],ImageSize->1000]
Sqrt[Variance[l2]]
Total[CrossingDetect[l2-0.8Sqrt[Variance[l2]]]]/2.


g=Function[fname,
	Module[{xyzt},
		xyzt=LoadData[fname,All,"Acc"];
		{CountStepPeak[xyzt],Image[ListLinePlot[
			Map[Norm,Transpose[xyzt[[1;;3]]]],
			(*Map[First,f[Map[Norm,Transpose[xyzt[[1;;3]]]],0.2,0.8]],*)
				MaxPlotPoints->1000,PlotRange->All],ImageSize->400]}
	]];
l={38, 34, 32, 43, 40, 45, 43, 39, 35,0};
Table[{l[[i-4]],g["~/doc/sensor/step/walk"<>IntegerString[i]<>".txt"]},{i,5,13}]
l={125,133,144,141,139,131,142,140,148,140,133,141,138,141,141,130,141,140,138,142,127,139,133,140,142,130,139,139,129,139,141,140,142,129,140,142};
res=Table[{i,l[[i]],g["/usr/local/google/users/georgezhou/step/walk"<>IntegerString[i]<>".txt"]},{i,36}]
Mean@Map[Abs,(Map[#[[2]]&,res]-Map[#[[3,1]]&,res])/Map[#[[2]]&,res]]
Median@Map[Abs,(Map[#[[2]]&,res]-Map[#[[3,1]]&,res])/Map[#[[2]]&,res]]


wxyztf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[wxyzt[[1;;4]]]],InterpolationOrder->1];


markers=ProcData[LoadData["~/walk1.txt",All,"Marker"]];
Graphics[Line[LatLongToXY@Transpose@markers[[1;;2]]],Axes->True]
markerf=Interpolation[MapThread[{#1,#2}&,{markers[[4]],LatLongToXY@Transpose@markers[[1;;2]]}],InterpolationOrder->1];
(*Export["~/www/t.png",Graphics[Line[LatLongToXY@Transpose@markers[[1;;2]]],Axes->True]]*)
(*Export["~/EPA_truth.png",Graphics[Line[Transpose[{markers[[2]],markers[[1]]}]],Axes->True]]*)


(*gps=ProcData@LoadData["~/walk1.txt",All,"Gps"];
ListPlot@Transpose[{gps[[2]],gps[[1]]}]*)


(*rott=N@Map[ToExpression/@{#[[2]],#[[3]],#[[4]],StringDrop[StringDrop[#[[5]],2],-1]}&,Select[Import["~/t.txt","Table"],First[#]=="Rot:"&]];*)
rott=ProcData@LoadData["~/walk1.txt",All,"Rot"];
rotf=Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[rott[[1;;4]]]],InterpolationOrder->1];
rots=Map[rotf,ts];
res=Map[RotateByQuaternion[{0,1,0},{Re@Sqrt[1-#.#],#[[1]],#[[2]],#[[3]]}]&,Transpose[rott[[1;;3]]]];
Graphics@Line@Map[#[[1;;2]]&,Accumulate@MapThread[#1 #2 #3&,
	{Most@res,Differences[rott[[4]]]/10^9,Most[Map[isMovingF, rott[[4]]]]}]]
Graphics3D[Line@Accumulate@MapThread[#1 #2 #3&,
	{Most@res,Differences[rott[[4]]]/10^9,Most[Map[isMovingF, rott[[4]]]]}],Axes->True]


oris=ProcData@LoadData["~/walk1.txt",All,"Ori"];
Graphics3D@Line@Accumulate[
	Most[Map[{Sin[#[[1]]Pi/180],Cos[#[[1]]Pi/180]Cos[#[[2]]Pi/180],Cos[#[[1]]Pi/180]Sin[#[[2]]Pi/180]}&,Transpose@oris[[1;;3]]]] 
	Differences[oris[[4]]]
	Most[Map[isMovingF,oris[[4]]]]]


xbms3=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.5,0.0001,{0,0,0},1];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms3]];Print[gyroBias];
ListPlot[Transpose@Map[#[[2]]&,xbms3],PlotRange->All,MaxPlotPoints->5000]
oris2=TraceFromXBMS[xbms3[[All,1;;3]]];
ListPlot[Transpose@MapThread[#1.#2&,{oris2,xyzs}],MaxPlotPoints->5000]
ListPlot[Transpose@MapThread[#1.#2&,{oris2,mags}],MaxPlotPoints->5000]
seq=MapThread[#1 #2&,{Map[#.{0,1,0}&,oris2],Map[isMovingF,ts]}];
Graphics3D[Line@Accumulate@seq,Axes->True]


(*ListPlot[Map[GaussianFilter[#,50]&,Transpose@MapThread[Normalize@Matrix2Quat[PseudoInverse[#1].#2]&,{Partition[xyzs,20],Partition[mags,20]}]],Joined->True]*)


angles=MapThread[VectorAngle,{mags,xyzs}];
ListPlot@Transpose@MapThread[#1 #2&,{Map[If[Abs[#]>0.2,0,1]&,angles-Median[angles]],Transpose@Map[GaussianFilter[#,500]&,Transpose@qmags]}]


qxyzs=MapThread[RotateByQuaternion,{xyzs,qs}];
qmags=MapThread[RotateByQuaternion,{mags,qs}];
ListPlot[Transpose@qxyzs,MaxPlotPoints->5000]
ListPlot[Transpose@qmags,MaxPlotPoints->5000]
ListPlot[MapThread[VectorAngle,{qxyzs,qmags}],MaxPlotPoints->5000]
(*d1=MapThread[VectorAngle,{qxyzs,qmags}];
ListPlot[{d1,GaussianFilter[d1,300],MedianFilter[d1,300]},MaxPlotPoints->5000]*)


(*Manipulate[*)BlendPlot[Transpose[Function[l,
	Accumulate[Join@@(Accumulate[Standardize[#,Mean,1&]]&/@Partition[l,10])]
	]/@(Transpose@Accumulate[qxyzs[[All,1;;2]]])]](*,{c,3,100,1}]*)


Image[#,ImageSize->2000]&@ListLinePlot@Transpose@Accumulate[qxyzs[[All,1;;2]]]


Image[#,ImageSize->2000]&@ListLinePlot[Join@@{Transpose@qxyzs,0.3Transpose@mags}]


ListLinePlot@Transpose@Accumulate@qxyzs[[All,1;;2]]
l=Transpose@Accumulate@Accumulate@Transpose[(#-GaussianFilter[#,10])&/@Transpose[qxyzs]];
ListLinePlot@l[[1;;2]]
{ListLinePlot@Transpose@Accumulate@Transpose[(#-GaussianFilter[#,10])&/@Transpose[qxyzs]],ListLinePlot@Transpose@Accumulate@Standardize[qxyzs,Mean,1&],ListLinePlot@l}
Function[l,
	Module[{a,b,l2,x},
	{a,b}=Last/@FindFit[#,a+b x,{a,b},x]&@l;
	l2=l-Table[a+b x,{x,1,Length[l]}];
	{ListLinePlot[l2],ListLinePlot[Accumulate@l2]}
	]]/@l
l2=Function[l,
	Module[{a,b,l2,x},
	{a,b}=Last/@FindFit[#,a+b x,{a,b},x]&@l;
	l2=l-Table[a+b x,{x,1,Length[l]}];
	(*{ListLinePlot[l2],ListLinePlot[Accumulate@l2]}*)
	Accumulate@l2]]/@l;
ListLinePlot[l2[[1;;2]]]
BlendPlot[Transpose[l2[[1;;2]]]]


(*(*dwd = DiscreteWaveletTransform[d1, SymletWavelet[4], 8];*)
dwd = DiscreteWaveletTransform[d1, DaubechiesWavelet[4], 8];
WaveletListPlot[dwd, PlotLayout -> "CommonXAxis",MaxPlotPoints->5000]
rdwd1 = WaveletMapIndexed[(#*0.0) &, dwd, {___, 1}];
ListLinePlot[InverseWaveletTransform[rdwd1], PlotLabel -> "filtered coarse data",MaxPlotPoints->5000]*)


(*ListPlot[Transpose[MapThread[Normalize[Prepend[1/2#1 #2,1]][[2;;4]]&,{Most[wxyzs],Differences[ts]/10^9}]-MapThread[Normalize[Matrix2Quat@SoraToMatrix[#1 #2]][[2;;4]]&,{Most[wxyzs],Differences[ts]/10^9}]]
,PlotRange->All]*)


ListPlot@Map[GaussianFilter[#,200]&,Transpose@qmags[[;;10000]]]


ListPlot@Map[MedianFilter[#,2000]&,Transpose@qmags[[;;10000]]]


(*Graphics3D@Line@Map[Normalize,qxyzs]
Graphics3D@Line@Map[Normalize,qmags]*)


smoothQxyzs=Transpose@Map[GaussianFilter[#,100]&,Transpose@qxyzs];
smoothQmags=Transpose@Map[GaussianFilter[#,200]&,Transpose@qmags];
sxyzs=MapThread[RotateByQuaternion[#1,List@@Conjugate[Quaternion@@#2]]&,{smoothQxyzs,qs}];
smags=MapThread[RotateByQuaternion[#1,List@@Conjugate[Quaternion@@#2]]&,{smoothQmags,qs}];
ListPlot@Transpose@smoothQxyzs
ListPlot@Transpose@smoothQmags
(*ListPlot@Transpose@sxyzs
ListPlot@Transpose@smags*)
(*Graphics3D@Line@Map[Normalize,smoothQxyzs]
Graphics3D@Line@Map[Normalize,smoothQmags]*)


(*If[Norm[VectorAngle[#1[[1]],#1[[3]]]-medianAngle]>0.4,0,1]*)


TraceFromXBMS=Function[xbms2,
		Map[UniqueRotationMatrix[#1[[1]],GramSchmidt[#1[[3]],#1[[1]]],{0,0,1},{0,1,0}]&,xbms2]
	];
LearnBiasSora2WithInit=Compile[{{ts,_Integer,1},{wxyzs,_Real,2},{mags,_Real,2},{xyzs,_Real,2},{kB,_Real},{kB2,_Real},
		{initAntiGravityBiasMagNorth,_Real,2},{maxB,_Real}},
	With[{xs=Map[Normalize,xyzs],kX=Sqrt[2kB],kZ=Sqrt[2kB2],
		medianAngle=Median@MapThread[VectorAngle,{mags,xyzs}]},
	FoldList[With[{wX=-#1[[2]]-kX Cross[#1[[1]],#2[[1]]],
				wZ=-#1[[2]]-(kZ Cross[#1[[3]],#2[[4]]]),
				dt=#2[[3]]/10^9},
			{SoraToMatrix[(-#2[[2]]-wX)dt].#1[[1]],
				Function[{oldX, x}, If[Norm[x] > maxB, oldX, x]][
					#1[[2]],#1[[2]]+(kB Cross[#1[[1]],#2[[1]]]+kB2 Cross[#1[[3]],#2[[4]]])Abs[dt]],
				SoraToMatrix[(-#2[[2]]-wZ)dt].#1[[3]],
				Cross[#1[[1]],#2[[1]]],
				Cross[#1[[3]],#2[[4]]]}]&,
		Join[initAntiGravityBiasMagNorth,{{0,0,0},{0,0,0}}],
			Transpose[{Rest[xs],Rest[wxyzs],Differences[ts],Rest[MapThread[Normalize[#1-Projection[#1,#2]]&,{mags,xs}]]}]]
	]];
LearnBiasSora2=Function[{ts,wxyzs,mags,xyzs,kB,kB2},LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,
	{Normalize@First[xyzs],{0,0,0},Normalize@GramSchmidt[First@mags,First@xyzs]},0.05]];
LearnBiasSora2B=Function[{ts,wxyzs,mags,xyzs,kB,kB2},LearnBiasSora2WithInitBackward[ts,wxyzs,mags,xyzs,kB,kB2,{{0,0,1},{0,0,0},{0,1,0}},0.05]];
LearnBiasSora2WithInitBackward=Function[{ts,wxyzs,mags,xyzs,kB,kB2,initAntiGravityBiasMagNorth,maxB},
	Map[{-#[[1]],#[[2]],-#[[3]]}&,Reverse@LearnBiasSora2WithInit[Reverse@ts,Reverse@wxyzs,Reverse@mags,Reverse@xyzs,kB,kB2,
		{-initAntiGravityBiasMagNorth[[1]],initAntiGravityBiasMagNorth[[2]],-initAntiGravityBiasMagNorth[[3]]},maxB]]
	];
LearnBiasSora2BurnIn=Function[{ts,wxyzs,mags,xyzs,kB,kB2},
	Module[{xbms,xbms2,xbms3,xbms4,maxB=0.05,k=Floor[0.01 Length[ts]]},
	xbms=LearnBiasSora2WithInitBackward[ts[[;;k]],wxyzs[[;;k]],mags[[;;k]],xyzs[[;;k]],kB,kB2,{{0,0,1},{0,0,0},{0,1,0}},maxB];
	LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,xbms[[1,;;]],maxB]
	]];
LearnBiasSora2FBExp=Function[{ts,wxyzs,mags,xyzs,kB,kB2},
	Module[{xbms,xbms2,xbms3,xbms4,maxB=0.05},
	xbms=LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,{{0,0,1},{0,0,0},{0,1,0}},maxB];
	xbms2=LearnBiasSora2WithInitBackward[ts,wxyzs,mags,xyzs,kB,kB2,xbms[[-1,;;]],maxB];
	xbms3=LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,xbms2[[-1,;;]],maxB];
	xbms4=LearnBiasSora2WithInitBackward[ts,wxyzs,mags,xyzs,kB,kB2,xbms3[[-1,;;]],maxB];
	{xbms,xbms2,xbms3,xbms4}
	]];
LearnBiasSora2FB=Function[{ts,wxyzs,mags,xyzs,kB,kB2},
	Module[{xbms,xbms2,xbms3,xbms4,maxB=0.05},
	xbms=LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,{{0,0,1},{0,0,0},{0,1,0}},maxB];
	xbms2=LearnBiasSora2WithInitBackward[ts,wxyzs,mags,xyzs,kB,kB2,xbms[[-1,;;]],maxB];
	xbms3=LearnBiasSora2WithInit[ts,wxyzs,mags,xyzs,kB,kB2,xbms2[[-1,;;]],maxB];
	xbms4=LearnBiasSora2WithInitBackward[ts,wxyzs,mags,xyzs,kB,kB2,xbms3[[-1,;;]],maxB];
	MapThread[Map[Mean,Transpose[{#2,#3,#4}]]&,{xbms,xbms2,xbms3,xbms4}]
	]];
LearnBiasSora3WithInit=Compile[{{ts,_Integer,1},{wxyzs,_Real,2},{mags,_Real,2},{xyzs,_Real,2},{kB,_Real},{kB2,_Real},
		{initAntiGravityBiasMagNorth,_Real,2},{meanB,_Real,1},{concentration,_Real}},
	With[{xs=Map[Normalize,xyzs],kX=Sqrt[2kB],kZ=Sqrt[2kB2]},
	FoldList[With[{wX=-#1[[2]]-kX Cross[#1[[1]],#2[[1]]],
				wZ=-#1[[2]]-kZ Cross[#1[[3]],#2[[4]]],
				dt=#2[[3]]/10^9},
			{SoraToMatrix[(-#2[[2]]-wX)dt].#1[[1]],
				#1[[2]]+(concentration (Function[x,x][meanB-#1[[2]]])+ kB Cross[#1[[1]],#2[[1]]]+kB2 Cross[#1[[3]],#2[[4]]])Abs[dt],
				SoraToMatrix[(-#2[[2]]-wZ)dt].#1[[3]],
				Cross[#1[[1]],#2[[1]]],
				Cross[#1[[3]],#2[[4]]]}]&,
		Join[initAntiGravityBiasMagNorth,{{0,0,0},{0,0,0}}],
			Transpose[{Rest[xs],Rest[wxyzs],Differences[ts],Most[MapThread[Normalize[#1-Projection[#1,#2]]&,{mags,xs}]]}]]
	]];
LearnBiasSora3=Function[{ts,wxyzs,mags,xyzs,kB,kB2,meanB,concentration},LearnBiasSora3WithInit[ts,wxyzs,mags,xyzs,kB,kB2,
	{Normalize@First[xyzs],{0,0,0},Normalize@GramSchmidt[First@mags,First@xyzs]},meanB,concentration]];


(*{xbms,xbms2,xbms3,xbms4}=LearnBiasSora2FBExp[ts,wxyzs,mags,xyzs,0.05,0.001];*)


(*ListPlot[Transpose@Map[#[[1]]&,xbms],PlotRange->All]
ListPlot[Transpose@Map[#[[1]]&,xbms2],PlotRange->All]
ListPlot[Transpose@Map[#[[1]]&,xbms3],PlotRange->All]
ListPlot[Transpose@Map[#[[1]]&,xbms4],PlotRange->All]
ListPlot[Transpose@MapThread[Median[{#1[[1]],#2[[1]],#3[[1]],#4[[1]]}]&,{xbms,xbms2,xbms3,xbms4}],PlotRange->All]*)


(*ListPlot[Transpose@Map[#[[3]]&,xbms],PlotRange->All]
ListPlot[Transpose@Map[#[[3]]&,xbms2],PlotRange->All]
ListPlot[Transpose@Map[#[[3]]&,xbms3],PlotRange->All]
ListPlot[Transpose@MapThread[Mean[{#1[[3]],#2[[3]],#3[[3]]}]&,{xbms,xbms2,xbms3}],PlotRange->All]*)


(*ListPlot[Transpose@Map[#[[2]]&,xbms],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,xbms2],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,xbms3],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,xbms4],PlotRange->All]
(*ListPlot[Transpose@MapThread[Median[{#2[[2]],#3[[2]],#4[[2]]}]&,{xbms,xbms2,xbms3,xbms4}],PlotRange->All]*)*)


xbms3=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.005,0.0001,{0,0,0},0.0001];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms3]];Print[gyroBias];
ListPlot[Transpose@Map[#[[2]]&,xbms3],PlotRange->All,MaxPlotPoints->5000]
oris2=TraceFromXBMS[xbms3[[All,1;;3]]];
ListPlot[Transpose@MapThread[#1.#2&,{oris2,xyzs}],MaxPlotPoints->5000]
ListPlot[Transpose@MapThread[#1.#2&,{oris2,mags}],MaxPlotPoints->5000]
seq=MapThread[#1 #2&,{Map[#.{0,1,0}&,oris2],Map[isMovingF,ts]}];
Graphics3D[Line@Accumulate@seq,Axes->True]
xbms3=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.05,0.02,{0,0,0},0.0001];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms3]];Print[gyroBias];
ListPlot[Transpose@Map[#[[2]]&,xbms3],PlotRange->All,MaxPlotPoints->5000]
oris2=TraceFromXBMS[xbms3[[All,1;;3]]];
ListPlot[Transpose@MapThread[#1.#2&,{oris2,xyzs}],MaxPlotPoints->5000]
ListPlot[Transpose@MapThread[#1.#2&,{oris2,mags}],MaxPlotPoints->5000]
seq=MapThread[#1 #2&,{Map[#.{0,1,0}&,oris2],Map[isMovingF,ts]}];
Graphics3D[Line@Accumulate@seq,Axes->True]
xbms3=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.005,0.0001,gyroBias,0.1];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms3]];Print[gyroBias];
ListPlot[Transpose@Map[#[[1]]&,xbms3],PlotRange->All,MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[2]]&,xbms3],PlotRange->All,MaxPlotPoints->5000]
ListPlot[Transpose@Map[Normalize[#[[3]]]&,xbms3],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[4]]&,xbms3],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[5]]&,xbms3],MaxPlotPoints->5000]
(*cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]}]];*)
(*cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],Map[{#1,gyroBias}&,ts]];
seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
Graphics3D[Line@Accumulate@seq,Axes->True]*)
oris2=TraceFromXBMS[xbms3[[All,1;;3]]];
ListPlot[Transpose@MapThread[#1.#2&,{oris2,xyzs}],MaxPlotPoints->5000]
ListPlot[Transpose@MapThread[#1.#2&,{oris2,mags}],MaxPlotPoints->5000]
seq=MapThread[#1 #2&,{Map[#.{0,1,0}&,oris2],Map[isMovingF,ts]}];
Graphics3D[Line@Accumulate@seq,Axes->True]
(*Graphics3D[Line@Accumulate[MapThread[Normalize@GramSchmidt[#1,#2]&,{xbms2[[All,3]],xbms2[[All,1]]}] Map[isMovingF,ts]],Axes->True]*)


Export["~/pic/gyro_bias_smooth.png",ListPlot[Transpose@Map[#[[2]]&,xbms2],PlotRange->All,MaxPlotPoints->5000]]


xbms2=LearnBiasSora2BurnIn[ts,wxyzs,mags,xyzs,0.005,0.0002];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms2]];Print[gyroBias];
ListPlot[Transpose@Map[#[[1]]&,xbms2],PlotRange->All,MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[2]]&,xbms2],PlotRange->All,MaxPlotPoints->5000]
ListPlot[Transpose@Map[Normalize[#[[3]]]&,xbms2],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[4]]&,xbms2],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[5]]&,xbms2],MaxPlotPoints->5000]
(*cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]}]];*)
cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],Map[{#1,gyroBias}&,ts]];
seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
Graphics3D[Line@Accumulate@seq,Axes->True]


seqf=Interpolation[MapThread[{#1,#2}&,{wxyzt[[4]],seq}],InterpolationOrder->1];seqs=seqf/@ts;
Graphics3D@Line@Accumulate@MapThread[GramSchmidt,{seqs,xyzs}]


cqqf=Interpolation[cqqs,InterpolationOrder->1];cqs=cqqf/@ts;
ListPlot@Transpose@MapThread[RotateByQuaternion,{xyzs,cqs}]
ListPlot@Transpose@MapThread[RotateByQuaternion,{mags,cqs}]


oris2=TraceFromXBMS[xbms2[[;;,1;;3]]];
ListPlot@Transpose@MapThread[#1.#2&,{oris2,xyzs}]
ListPlot@Transpose@MapThread[#1.#2&,{oris2,mags}]
Graphics3D@Line@Accumulate[Map[#.{0,1,0}&,oris2] Map[isMovingF,ts]]


(*Graphics3D@Line@MapThread[Normalize[#1.#2]&,{oris2,xyzs}]
Graphics3D@Line@MapThread[Normalize[#1.#2]&,{oris2,mags}]*)


sxbms=LearnBiasSora2BurnIn[ts,wxyzs,smags,xyzs,0.05,0.001];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,sxbms]];Print[gyroBias];
ListPlot[Transpose@Map[#[[1]]&,sxbms],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,sxbms],PlotRange->All]
ListPlot[Transpose@Map[Normalize[#[[3]]]&,sxbms]]
cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,sxbms]]}]];
seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
Graphics3D@Line@Accumulate@seq


ReconstructTrace["~/walk2.txt"]
ReconstructTrace["~/walk3.txt"]
ReconstructTrace["~/walk4.txt"]
ReconstructTrace["~/walk5.txt"]
ReconstructTrace["~/walk6.txt"]
ReconstructTrace["~/walk7.txt"]


ReconstructTrace3=Function[{fname},
		With[{xyzt=ProcData@LoadData[fname,All,"Acc"],wxyzt=ProcData@LoadData[fname,All,"Gyo"],mag=ProcData@LoadData[fname,All,"Compass"],
			ToFunction=Function[xyzt,Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[xyzt[[1;;4]]]],InterpolationOrder->1]]},
		Module[{ts=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.05 10^9}],wxyztf,xyztf,magf,wxyzs,mags,xyzs,xbms2,isMoving,qmags,qxyzs,smags,sxyzs,oris2,seq,movings,isMovingF,gyroBias},
		{xyztf,wxyztf,magf}=Map[ToFunction[#[[1;;4]]]&,{xyzt,wxyzt,mag}];
		{xyzs,wxyzs,mags}=Map[Map[#,ts]&,{xyztf,wxyztf,magf}];
		isMoving=FindMoving[xyzt,0.4,15];isMovingF=Interpolation@isMoving;
		movings=Map[Max[0,Min[1,N@isMovingF@#]]&,ts];
		xbms2=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.05,0.02,{0,0,0},0];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms2]];Print[gyroBias];
		xbms2=LearnBiasSora3[ts,wxyzs,mags,xyzs,0.005,0.0001,gyroBias,0.1];gyroBias=Map[Median[Take[#,All]]&,Transpose@Map[#[[2]]&,xbms2]];Print[gyroBias];
		Graphics3D[Line@Accumulate[MapThread[Normalize@GramSchmidt[#1,#2]&,{xbms2[[All,3]],xbms2[[All,1]]}] movings],Axes->True]
	]]];


ReconstructTrace3["~/walk2.txt"]


ReconstructTrace3["~/walk12.txt"]


Table[ReconstructTrace3["~/walk"<>IntegerString[i]<>".txt"],{i,2,20}]


(*xbs=LearnBias[ts,wxyzs,xyzs,0.01];
ListPlot[Transpose@Map[#[[1]]&,xbs],PlotRange->All]
ListPlot[Transpose@Map[#[[2]]&,xbs],PlotRange->All]
cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbs]]}]];
seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
Graphics3D@Line@Accumulate@seq*)


(*IntegrateAngularVelocityWithBiasAndCorr=Compile[{{ts, _Integer,1},{wxyzs,_Real,2},{bias,_Real,2},{xyzs,_Real,2},{kB,_Real}},
	Module[{biasF=Interpolation[bias,InterpolationOrder->1],kX=Sqrt[2kB],error,dt},
		MapThread[{#2,#1[[1,;;]]}&,
		{FoldList[
			(error=Cross[RotateByQuaternion[{0,0,1},#1[[1]]],#2[[4]]];dt=#2[[2]];
			{Normalize[#1[[1]]+QuaternionDerivative[#1[[1]],#2[[1]]-biasF[#2[[3]]]-#1[[2]]-kX error]#2[[2]]],#1[[2]]+kB error dt})&,
			{{1,0,0,0},{0,0,0}},
			MapThread[{#1,#2,#3,#4}&,{Most[wxyzs],Differences[ts]/10^9,Most[ts],Most[xyzs]}]],ts}]
		]];*)


(*ReconstructTrace5=Function[{fname},
		With[{xyzt=LoadData[fname,All,"Acc"],wxyzt=LoadData[fname,All,"Gyo"],mag=LoadData[fname,All,"Compass"],
			ToFunction=Function[xyzt,Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[xyzt[[1;;4]]]],InterpolationOrder->1]]},
		Module[{ts=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.05 10^9}],wxyztf,xyztf,magf,wxyzs,mags,xyzs,xbms2,isMoving,qmags,qxyzs,smags,sxyzs,cqqs,seq,movings,isMovingF},
		{xyztf,wxyztf,magf}=Map[ToFunction[#[[1;;4]]]&,{xyzt,wxyzt,mag}];
		{xyzs,wxyzs,mags}=Map[Map[#,ts]&,{xyztf,wxyztf,magf}];
		movings=Map[Max[0,Min[1,N@isMovingF@#]]&,wxyzt[[4]]];
		xbms2=LearnBias2[ts,wxyzs,mags,xyzs,0.05];
		cqqs=IntegrateAngularVelocityWithBiasAndCorr[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]}],
			xyztf/@wxyzt[[4]],0.0001];
		isMoving=FindMoving[xyzt,0.4,15];isMovingF=Interpolation@isMoving;
		seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
		Graphics3D@Line@Accumulate@seq
	]]];*)


(*LearnBiasQuat=Function[{ts,wxyzs,mags,xyzs,kB},
	With[{xs=Map[Normalize,xyzs],kX=Sqrt[2kB],kZ=Sqrt[2kB]},
	Map[{RotateByQuaternion[{0,0,1},#[[1]]],#[[2]],RotateByQuaternion[{0,1,0},#[[3]]]}&,FoldList[
		With[{wX=-#1[[2]]-kX Cross[RotateByQuaternion[{0,0,1},#1[[1]]],#2[[1]]],
				wZ=-#1[[2]]-kZ Cross[RotateByQuaternion[{0,1,0},#1[[3]]],#2[[4]]],dt=#2[[3]]/10^9},
			{Normalize[#1[[1]]+QuaternionDerivative[#1[[1]],-#2[[2]]-wX]dt],
				#1[[2]]+kB(Cross[RotateByQuaternion[{0,0,1},#1[[1]]],#2[[1]]]+Cross[RotateByQuaternion[{0,1,0},#1[[3]]],#2[[4]]])dt,
				Normalize[#1[[3]]+QuaternionDerivative[#1[[3]],-#2[[2]]-wZ]dt]}]&,
		{{1,0,0,0},{0,0,0},{1,0,0,0}},
			Transpose[{Most[xs],Most[wxyzs],Differences[ts],Most[MapThread[Normalize[#1-Projection[#1,#2]]&,{mags,xs}]]}]]
	,{1}]]];*)


(*LearnBiasSora2NTimes=Function[{ts,wxyzs,mags,xyzs,kB,kB2,n},
	Module[{xbms2,b={0,0,0}},
		Do[(xbms2=LearnBiasSora2WithBias[ts,wxyzs,mags,xyzs,kB,kB2,b];
			b=Median/@Transpose@xbms2[[;;,2]];Print[b]),{i,n}];
		xbms2
	]];*)


ReconstructTrace=Function[{fname},
		With[{xyzt=ProcData@LoadData[fname,All,"Acc"],wxyzt=ProcData@LoadData[fname,All,"Gyo"],mag=ProcData@LoadData[fname,All,"Compass"],
			ToFunction=Function[xyzt,Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[xyzt[[1;;4]]]],InterpolationOrder->1]]},
		Module[{ts=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.05 10^9}],wxyztf,xyztf,magf,wxyzs,mags,xyzs,xbms2,isMoving,qmags,qxyzs,smags,sxyzs,cqqs,seq,movings,isMovingF},
		{xyztf,wxyztf,magf}=Map[ToFunction[#[[1;;4]]]&,{xyzt,wxyzt,mag}];
		{xyzs,wxyzs,mags}=Map[Map[#,ts]&,{xyztf,wxyztf,magf}];
		movings=Map[Max[0,Min[1,N@isMovingF@#]]&,wxyzt[[4]]];
		xbms2=LearnBiasSora2BurnIn[ts,wxyzs,mags,xyzs,0.05,0.01];
		cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]}]];
		isMoving=FindMoving[xyzt,0.4,15];isMovingF=Interpolation@isMoving;
		seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
		Graphics3D@Line@Accumulate@seq
	]]];


(*ReconstructTrace6["~/walk2.txt"]
ReconstructTrace6["~/walk3.txt"]
ReconstructTrace6["~/walk4.txt"]*)


ListPlot@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]


(*ReconstructTrace6["~/walk13.txt"]
ReconstructTrace6["~/walk14.txt"]
ReconstructTrace6["~/walk15.txt"]
ReconstructTrace6["~/walk16.txt"]
ReconstructTrace6["~/walk17.txt"]
ReconstructTrace6["~/walk18.txt"]
ReconstructTrace6["~/walk19.txt"]
ReconstructTrace6["~/walk20.txt"]*)


ReconstructTrace6["~/walk21.txt"]
ReconstructTrace6["~/walk22.txt"]


ReconstructTrace2["~/walk13.txt"]
ReconstructTrace2["~/walk14.txt"]
ReconstructTrace2["~/walk15.txt"]
ReconstructTrace2["~/walk16.txt"]
ReconstructTrace2["~/walk17.txt"]
ReconstructTrace2["~/walk18.txt"]
ReconstructTrace2["~/walk19.txt"]
ReconstructTrace2["~/walk20.txt"]


ReconstructTraceSmoothMags=Function[{fname},
		With[{xyzt=ProcData@LoadData[fname,All,"Acc"],wxyzt=ProcData@LoadData[fname,All,"Gyo"],mag=ProcData@LoadData[fname,All,"Compass"],
			ToFunction=Function[xyzt,Interpolation[Map[{#[[4]],#[[1;;3]]}&,Transpose[xyzt[[1;;4]]]],InterpolationOrder->1]]},
		Module[{ts=Table[i,{i,xyzt[[4]][[10]],xyzt[[4]][[-10]],0.05 10^9}],wxyztf,xyztf,magf,wxyzs,mags,xyzs,xbms2,isMoving,qmags,smoothQmags,smags,sxyzs,cqqs,seq,movings,isMovingF,qqs,qf,qs},
		{xyztf,wxyztf,magf}=Map[ToFunction[#[[1;;4]]]&,{xyzt,wxyzt,mag}];
		{xyzs,wxyzs,mags}=Map[Map[#,ts]&,{xyztf,wxyztf,magf}];
		qqs=IntegrateAngularVelocity[wxyzt[[4]],Transpose@wxyzt[[1;;3]]];qf=Interpolation[qqs,InterpolationOrder->1];qs=Map[qf,ts];
		qmags=MapThread[RotateByQuaternion,{mags,qs}];
		smoothQmags=Transpose@Map[GaussianFilter[MedianFilter[#,2000],100]&,Transpose@qmags];
		smags=MapThread[RotateByQuaternion[#1,List@@Conjugate[Quaternion@@#2]]&,{smoothQmags,qs}];
		movings=Map[Max[0,Min[1,N@isMovingF@#]]&,wxyzt[[4]]];
		xbms2=LearnBiasSora2[ts,wxyzs,smags,xyzs,0.05,0.02];
		cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[2]]&,xbms2]]}]];
		isMoving=FindMoving[xyzt,0.4,15];isMovingF=Interpolation@isMoving;
		seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
		Graphics3D@Line@Accumulate@seq
	]]];


ReconstructTraceSmoothMags["~/walk13.txt"]
ReconstructTraceSmoothMags["~/walk14.txt"]
ReconstructTraceSmoothMags["~/walk15.txt"]
ReconstructTraceSmoothMags["~/walk16.txt"]
ReconstructTraceSmoothMags["~/walk17.txt"]
ReconstructTraceSmoothMags["~/walk18.txt"]
ReconstructTraceSmoothMags["~/walk19.txt"]
ReconstructTraceSmoothMags["~/walk20.txt"]


ReconstructTraceSmoothMags["~/walk21.txt"]
ReconstructTraceSmoothMags["~/walk22.txt"]


full=Table[Sin[x-Pi],{x,0,5Pi,0.1}];
sample=Table[Sin[x-Pi]Exp[-(x-Pi)^2],{x,0,5Pi,0.1}];
sample2=Table[Sin[x-2Pi]Exp[-(x-2Pi)^2],{x,0,5Pi,0.1}];
sample3=Table[Sin[x-3Pi]Exp[-(x-3Pi)^2],{x,0,5Pi,0.1}];


z=(Fourier[full]Fourier[Reverse@sample])[[Ordering[Fourier[full]Fourier[Reverse@sample],-1,Abs[#1]<Abs[#2]&]]];{Norm[z],Arg[z]Length[sample]/2/Pi}


z=(Fourier[full]Fourier[Reverse@sample2])[[Ordering[Fourier[full]Fourier[Reverse@sample2],-1,Abs[#1]<Abs[#2]&]]];{Norm[z],Arg[z]Length[sample]/2/Pi}


z=(Fourier[full]Fourier[Reverse@sample3])[[Ordering[Fourier[full]Fourier[Reverse@sample3],-1,Abs[#1]<Abs[#2]&]]];{Norm[z],Arg[z]Length[sample]/2/Pi}


ListPlot[Map[Abs,Fourier[full]Fourier[Reverse@sample]],PlotRange->All]


ListPlot[{full,sample,sample2,sample3},PlotRange->All]


ListPlot@InverseFourier[Fourier[full]Fourier[Reverse@sample3]]


sample=Flatten[Table[{x,y,If[Norm[{x,y}-{10,20}]<3,1,0]},{x,0,100},{y,0,100}],1];
full=Flatten[Table[{x,y,Sin[x/10]Sin[y/10]},{x,0,100},{y,0,100}],1];


ListPlot3D@sample


ListPlot3D@full


coeffs=InverseFourier[Fourier[Partition[Map[#[[3]]&,sample],Floor[Sqrt[Length[sample]]]]] Fourier[Partition[Map[#[[3]]&,full],Floor[Sqrt[Length[full]]]]]]


ListPlot3D@MapThread[{#1[[1]],#1[[2]],Re[#2]}&,{Flatten[Table[{x,y},{x,0,100},{y,0,100}],1],Flatten[coeffs,1]}]


PositiveSemiDefiniteMatrixQ=Compile[{{m,_Real,2}},
	(*Only works for hermitian matrix*)
	Fold[And,True,Map[Abs[Im[#]]<10^-6&&Re[#]>=0&,Eigenvalues[m]]]
	];
(*q is B->I*)
KalmanPredict=Function[{w,dt,P,q,biasGyro},
	With[{Q=ArrayFlatten@Map[IdentityMatrix[3] #&,{{10^-7dt ,-0.5 10^-8 dt^2},{-0.5 10^-8 dt^2,10^-8 dt}},{2}]},
	Module[{trueW,Phi00,Phi10,Phi,wx,theta,ilwe,P2},
	trueW=w-biasGyro;
	theta=Norm[trueW]dt;
	wx=SkewOmega[trueW];
	ilwe=1/Norm[trueW];
	Phi00=IdentityMatrix[3]-Sin[theta]wx ilwe+(1-Cos[theta])wx.wx ilwe ilwe;
	Phi10=-IdentityMatrix[3]dt+(1-Cos[theta]) ilwe^2 wx-(theta-Sin[theta]) ilwe^3 wx.wx;
	Phi=ArrayFlatten[{{Phi00,Phi10},{0,IdentityMatrix[3]}}];
	(*Check positive semi-definite*)
	P2=Function[x,x]
	(*P2=Function[x,If[PositiveSemiDefiniteMatrixQ[x[[1;;3,1;;3]]]&&PositiveSemiDefiniteMatrixQ[x[[4;;6,4;;6]]],x,0 x]]*)
		[Phi.P.Transpose[Phi]+Q];
	{P2,Normalize[q+QuaternionDerivative[q,trueW dt]],biasGyro}
	]]];
KalmanUpdate=Function[{observed,axis,sigma,P,q,biasGyro},
	Module[{bb,L,innovation,K,R,LtSi,P2},
	bb=RotateByQuaternion[axis,QuaternionConjugate@q];
	L=SkewOmega[bb];
	innovation=Normalize[observed]-bb;
	R=sigma^2 IdentityMatrix[3];
	LtSi=Transpose[L].PseudoInverse[L.P[[1;;3,1;;3]].Transpose[L]+R];
	K={P[[1;;3,1;;3]].LtSi,P[[4;;6,1;;3]].LtSi};
	(*Check positive semi-definite*)
	(*P2=Function[x,If[PositiveSemiDefiniteMatrixQ[x[[1;;3,1;;3]]]&&PositiveSemiDefiniteMatrixQ[x[[4;;6,4;;6]]],x,0 x]]*)
	P2=Function[x,x]
		[ArrayFlatten[{{P[[1;;3,1;;3]]-K[[1]].L.P[[1;;3,1;;3]],
		P[[1;;3,4;;6]]-K[[1]].L.P[[1;;3,4;;6]]},
		{Transpose[P[[1;;3,4;;6]]-K[[1]].L.P[[1;;3,4;;6]]],
		P[[4;;6,4;;6]]-K[[2]].L.P[[1;;3,4;;6]]}}]];
	{P2,Normalize[q+QuaternionDerivative[q,K[[1]].innovation]],biasGyro+K[[2]].innovation,innovation}
	]];
KalmanFilter=Function[{wxyzs,mags,xyzs,ts},
	Module[{P2,b2,q,biasGyro,innovationM={0,0,0},innovationA={0,0,0},north,up,east},
	FoldList[
		(q=#1[[2]];biasGyro=#1[[3]];
		{P2,q,biasGyro}=KalmanPredict[#2[[1]],#2[[4]],#1[[1]],q,biasGyro];
		up=Transpose[Quat2Matrix[q]].{0,0,1};
		east=Cross[#2[[2]],up];
		north=Cross[up,east];
		{P2,q,biasGyro,innovationM}=KalmanUpdate[north,{0,1,0},0.5/Norm[north],P2,q,biasGyro];
		{P2,q,biasGyro,innovationA}=KalmanUpdate[#2[[3]],{0,0,1},0.05/Norm[#2[[3]]],P2,q,biasGyro];
		{P2,q,biasGyro,innovationM,innovationA}
		)&
		,{0 IdentityMatrix[6],{1,0,0,0},{0,0,0},{0,0,0},{0,0,0}},MapThread[{#1,#2,#3,#4}&,{Rest@wxyzs,Rest@mags,Rest@xyzs,Differences@ts/10^9}]]
	]];


QuaternionKsi=Compile[{{q,_Real,1}},
	{{-q[[2]],-q[[3]],-q[[4]]},{q[[1]],-q[[4]],q[[3]]},{q[[4]],q[[1]],-q[[2]]},{-q[[3]],q[[2]],q[[1]]}}
	];


k=-1;
res=KalmanFilter[wxyzs[[1;;k]],mags[[1;;k]],xyzs[[1;;k]],ts[[1;;k]]];
ListPlot[Transpose@Map[#[[3]]&,res],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[4]]&,res],MaxPlotPoints->5000]
ListPlot[Transpose@Map[#[[5]]&,res],MaxPlotPoints->5000]
Graphics3D@Line@Accumulate@MapThread[#1 #2 #3&,
	{Most@Map[RotateByQuaternion[{0,1,0},#[[2]]]&,res],Differences[ts]/10^9,Most[Map[isMovingF, ts]]}]
cqqs=IntegrateAngularVelocityWithBias[wxyzt[[4]],Transpose@wxyzt[[1;;3]],MapThread[{#1,#2}&,{ts,Transpose@Map[MedianFilter[#,3000]&,Transpose@Map[#[[3]]&,res]]}]];
seq=Map[RotateByQuaternion[{0,1,0},#[[2]]]&,cqqs] movings;
Graphics3D@Line@Accumulate@seq
