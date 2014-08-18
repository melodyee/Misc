(* ::Package:: *)

<< "~/git_math/google3/experimental/users/georgezhou/mathematica/Sensor2.m"
$HistoryLength=1;


r=Parallelize@Map[{#,TestTrajectoryMethods@#}&,FileNames["BEJ_KEJ_5_georgezhou*"]];


Manipulate[ListLinePlot[Mean[(Abs/@Fourier[#])&/@Partition[Norm/@wxyzt[[All,2;;4]],c]],PlotRange->All],{c,30,500,1}]


ListLinePlot@Transpose[(ToExpression/@StringSplit[#,";"]&/@Import["BSPMElectrodes.csv"])[[All,1,2;;4]]]


fname="/tmp/georgezhou-Rectangle-Swing.txt";(*fname="t.txt";*)
xyzt=LoadData2[fname,";accel"];
mags=LoadData2[fname,";compass"];
ts=xyzt[[All,1]];
qf=LoadQuaternionF[fname];
qs=Map[qf,ts];
qxyzs=MapThread[RotateByQuaternion,{xyzt[[All,2;;4]],qs}];
(*Select[Partition[xyzt[[All,1]],2,1],#[[1]]>#[[2]]&]
Select[Partition[wxyzt[[All,1]],2,1],#[[1]]>#[[2]]&]
Select[Partition[mags[[All,1]],2,1],#[[1]]>#[[2]]&]*)
{ListLinePlot@Transpose[wxyzt[[All,2;;4]]],ListLinePlot@Transpose[xyzt[[All,2;;4]]],ListLinePlot@Transpose[mags[[All,2;;4]]],ListLinePlot@Transpose[qxyzs]}
BlendPlot@Accumulate[SingularValueDecomposition[Standardize[#,Mean,1&]][[3,3]]&/@Partition[qxyzs,100,10]][[All,;;2]]
ListLinePlot@Transpose[qxyzs]


AccelPCA=Function[fname,
	Module[{xyzt,wxyzt,ts,qqs,qxyzs,qf,qs},
xyzt=LoadData2[fname,";accel"];
wxyzt=LoadData2[fname,";gyro"];
ts=xyzt[[All,1]];
qqs=IntegrateAngularVelocity[wxyzt[[All,1]],10.^9wxyzt[[All,2;;4]]];qf=Interpolation[qqs,InterpolationOrder->1];qs=Map[qf,ts];
qxyzs=MapThread[RotateByQuaternion,{xyzt[[All,2;;4]],qs}];
{fname,ListLinePlot@Accumulate[SingularValueDecomposition[Standardize[#,Mean,1&]][[3,3]]&/@Partition[qxyzs,200,10]][[All,;;2]]}]];


AccelPCA/@FileNames["/tmp/*-Rectangle-Swing.txt"]


l=Table[Max[0,100-(x-30)^2]+80RandomVariate[NormalDistribution[]],{x,0,100}];
l2=Table[Max[0,100-(x-10)^2],{x,0,20}];
ListLinePlot[#,PlotRange->All]&/@{l,l2}
ListLinePlot[Correlation[l2,#]&/@Partition[l,Length@l2,1]]


(*c=3300;len=300;*)
c=600;len=300;
(*c=2000;len=500;*)
(*c=1500;len=500;*)
(*c=12500;len=1000;*)
(*c=8000;len=500;*)
ssig=sig[[c;;c+len]];sts=mags[[c;;c+len,1]];sxys=xyf/@sts;
r2=(*Correlation[ssig,#]*)(1-CosineDistance[ssig,#])&/@Partition[sig2,Length@ssig,1];
r={Labeled[ListLinePlot[{sig,sig2}],"Mag-Norm"],(*ListLinePlot@ssig,*)
	Labeled[ListLinePlot[r2,PlotRange->All],"Correlation"]}
(*cs=Ordering[Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1]][[-1;;]];*)
cs=Reverse[First/@(SortBy[{#,r2[[#]]}&/@(1+Flatten@MapIndexed[If[#==1,{#2[[1]]},{}]&,CrossingDetect@Differences@r2]),Last][[-10;;]])]
Function[c2,
	sts2=mags2[[c2;;c2+len,1]];
	sxys2=xyf2/@sts2;
	{Labeled[ListLinePlot[{Table[0,{c2}]~Join~ssig,sig2}],"Match"],Labeled[ListPlot[{sxys,First/@Partition[xys,5]},PlotRange->All],"Matched-part-1"],
Labeled[ListPlot[{sxys2,First/@Partition[xys2,5]},PlotRange->All],"Matched-part-2"]}]/@cs


Graphics[Line/@Reap[Outer[If[Norm[#-#2]<2&&Norm[#-#2]>0.000001,Sow[{#,#2}]]&,mxys,mxys,1]][[2,1]]]


xys={-3,0}+#&/@xys;
ListLinePlot@{xys,xys2}


(*len=300;
Histogram@Table[Correlation[sig[[c;;c+len]],sig2[[With[{r=Random[Integer,{1,Length[xys2]-len}]},Range[r,r+len]]]]],{c,Length[xys]-len}]
Histogram@Table[Correlation[sig[[c;;c+len]],sig2[[InducedTrace[xys[[c;;c+len]],xys2]]]],{c,Length[xys]-len}]*)


ListLinePlot@Table[Correlation[sig[[c;;c+len]],sig2[[InducedTrace[xys[[c;;c+len]],xys2]]]],{c,Length[xys]-len}]


c=150;len=50;
ssig=sig[[c;;c+len]];sxys=xys[[c;;c+len]];
{ListLinePlot@{sxys,xys2[[InducedTrace[sxys,xys2]]]},BlendPlot/@{sxys,xys2[[InducedTrace[sxys,xys2]]]}}
Correlation[sig[[c;;c+len]],sig2[[InducedTrace[xys[[c;;c+len]],xys2]]]]
ListLinePlot@{ssig,sig2[[InducedTrace[xys[[c;;c+len]],xys2]]]}


c=1500;len=100;
c=650;len=50;
ssig=sig[[c;;c+len]];sxys=xys[[c;;c+len]];
r2=(*Correlation[ssig,#]*)(1-CosineDistance[ssig,#])&/@Partition[sig2,Length@ssig,1];
r={Labeled[ListLinePlot[{sig,sig2},MaxPlotPoints->5000],"Mag-Norm"],(*ListLinePlot@ssig,*)
	Labeled[ListLinePlot[r2,PlotRange->All,MaxPlotPoints->5000],"Correlation"]}
(*cs=Ordering[Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1]][[-1;;]];*)
cs=Reverse[First/@(SortBy[{#,r2[[#]]}&/@(1+Flatten@MapIndexed[If[#==1,{#2[[1]]},{}]&,CrossingDetect@Differences@r2]),Last][[-10;;]])]
Function[c2,
	sxys2=xys2[[c2;;c2+len]];
	Image[#,ImageSize->300]&/@{ListLinePlot[{Table[0,{c2}]~Join~ssig,sig2},MaxPlotPoints->5000],ListPlot[{sxys,First/@Partition[xys,10]}(*,MaxPlotPoints->5000*),PlotRange->All],
		ListPlot[{sxys2,First/@Partition[xys2,10]}(*,MaxPlotPoints->5000*),PlotRange->All]}]/@cs


(*(*c=2000;len=500;*)
(*c=1500;len=500;*)
c=42500;len=1000;
(*c=8000;len=500;*)
{mxyf,mxyf2}=LoadMarkerXYf2/@{fname,modelfname};
{xys,xys2}={mxyf/@(First/@mags),mxyf2/@(First/@mags2)};
ssig=sig[[c;;c+len]];sts=mags[[c;;c+len,1]];sxys=mxyf/@sts;
r2=Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1];
r={Labeled[ListLinePlot[{sig,sig2},MaxPlotPoints->5000],"Mag-Norm"],(*ListLinePlot@ssig,*)
	Labeled[ListLinePlot[r2,PlotRange->All,MaxPlotPoints->5000],"Correlation"]}
(*cs=Ordering[Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1]][[-1;;]];*)
cs=Reverse[First/@(SortBy[{#,r2[[#]]}&/@(1+Flatten@MapIndexed[If[#==1,{#2[[1]]},{}]&,CrossingDetect@Differences@r2]),Last][[-10;;]])]
Function[c2,
	sts2=mags2[[c2;;c2+len,1]];
	sxys2=mxyf2/@sts2;
	{Labeled[ListLinePlot[{Table[0,{c2}]~Join~ssig,sig2},MaxPlotPoints->5000],"Match"],Labeled[ListPlot[{sxys,First/@Partition[xys,500]}(*,MaxPlotPoints->5000*),PlotRange->All],"Matched-part-1"],
Labeled[ListPlot[{sxys2,First/@Partition[xys2,500]}(*,MaxPlotPoints->5000*),PlotRange->All],"Matched-part-2"]}]/@cs*)


fname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul6.txt";
(*fname="BEJ_KEJ_5_georgezhou_Jul5_S.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul10_swing.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_S_2.txt";
fname="BEJ_KEJ_5_georgezhou_Jul6.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul5_S_2.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul11.txt";modelfname="PEK_google_KEJ_5_baohj.txt";*)
(*modelfname2="PEK_google_KEJ_5_baohj.txt";*)
(*fname="41_2.txt";modelfname="41_3.txt";*)
(*fname="ABQ.txt";modelfname="ABQ_2.txt";*)
(*fname="PEK_google_KEJ_5_fishywang.txt";modelfname="PEK_google_KEJ_5_baohj.txt";*)
{ListLinePlot[LatLongToXY[Last/@LoadMarker[#]]&/@{fname,modelfname}]}~Join~(BlendPlot/@(LatLongToXY[Last/@LoadMarker[#]]&/@{fname,modelfname}))
ListLinePlot@LoadXYs[#,""][[1]]&/@{fname,modelfname}
{mags,mags2}=LoadData2[#,";compass"]&/@{fname,modelfname};
{ListLinePlot[Transpose[Rest/@mags],MaxPlotPoints->5000],ListLinePlot[Transpose[Rest/@mags2],MaxPlotPoints->5000]}
{xys,xys2}=LoadXYs[#,""][[1]]&/@{fname,modelfname};
{xyf,xyf2}=LoadXYf[#,""]&/@{fname,modelfname};
{sig,sig2}=Norm/@Rest(*Last*)/@#&/@{mags,mags2};


(*Function[fname,
	Module[{xys,ts,magf,mags},
	{xys,ts}=LoadXYs[#,""]&@fname;
	mags=LoadData2[#,";compass"]&@fname;
	magf=Interpolation[MapThread[List,{First/@#,Rest/@#}],InterpolationOrder->1]&@mags;
	{fname,BubbleChart[First/@Partition[MapThread[Append,{xys,Norm/@(magf/@ts)}],3]]}]]/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]]*)


Needs["PlotLegends`"]
r=Labeled[ListLinePlot[{LoadXYs[#,"--ontg_filter1_mag_gain=0 --ontg_filter2_mag_gain=0"][[1]],LoadXYs[#,""][[1]]},PlotLegend->{"NoMag","Mag"}],#]&/@FileNames@"BEJ_TUSPARK_georgezhou_*"
Export["t.png",GraphicsGrid[Partition[Rest[r],3],ImageSize->700]]


(*{#,ListLinePlot[{LoadXYs[#,"--ontg_filter1_mag_gain=0 --ontg_filter2_mag_gain=0"][[1]],LoadXYs[#,""][[1]]}]}&/@FileNames@"BEJ_KEJ_5_georgezhou_Jul*"*)


modelfname="BEJ_KEJ_5_georgezhou_Jul13_S_cyclic.txt";(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_survey_3_1.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_6.txt";*)(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_S_2.txt";*)(*modelfname="PEK_google_KEJ_5_fishywang.txt";*)
(*modelfname="ABQ_2.txt";*)
modelfname="BEJ_KEJ_5_georgezhou_Jul13_cyclic_2_2.txt";
mag2=LoadData2[modelfname,";compass"];
(*qf2=LoadQuaternionF[modelfname];*)
(*xyf2=LoadXYf[modelfname,""];
xys2=xyf2/@mag2[[;;,1]];
qmags2=RotateByQuaternion[#[[2;;4]],qf2[#[[1]]]]&/@mag2;*)
(*ListLinePlot@xys2*)
ListLinePlot@Transpose[Rest/@LoadData2[modelfname,";accel"]]
(*ListLinePlot@Transpose@qmags2*)


LoadMarkerXYf2=Function[fname,
	Module[{marker=LoadMarker[fname]},
	Interpolation[MapThread[List,{First/@marker,Standardize[LatLongToXY[Last/@marker],Mean,1&]}],InterpolationOrder->1]
	]];


{rmag,rmag2}=(First/@Partition[#,10])&/@{mag,mag2};
{mxyf,mxyf2}=LoadMarkerXYf2/@{fname,modelfname};
Image[#,ImageSize->300]&/@Table[ListContourPlot@MapThread[Append,{mxyf/@(First/@rmag),(i@#)&/@rmag}],{i,{Norm@#[[2;;3]]&,Last,Norm[Rest@#]&}}]
Image[#,ImageSize->300]&/@Table[ListContourPlot@MapThread[Append,{mxyf2/@(First/@rmag2),i@#&/@rmag2}],{i,{Norm@#[[2;;3]]&,Last,Norm[Rest@#]&}}]
ListLinePlot[{mxyf/@(First/@rmag),mxyf2/@(First/@rmag2)}]


(*{rmag,rmag2}=(First/@Partition[#,10])&/@{mag,mag2};
{xyf,xyf2}=LoadXYf[#,""]&/@{fname,modelfname};
Image[#,ImageSize->300]&/@Table[ListContourPlot@MapThread[Append,{xyf/@(First/@rmag),(i@#)&/@rmag}],{i,{Norm@#[[2;;3]]&,Last,Norm[Rest@#]&}}]
Image[#,ImageSize->300]&/@Table[ListContourPlot@MapThread[Append,{xyf2/@(First/@rmag2),i@#&/@rmag2}],{i,{Norm@#[[2;;3]]&,Last,Norm[Rest@#]&}}]
ListLinePlot[{xyf/@(First/@rmag),xyf2/@(First/@rmag2)}]*)


(*Function[fname,
	Module[{mag=LoadData2[fname,";compass"],xyf},
	xyf=LoadXYf[#,""]&@fname;
	{fname,Image[#,ImageSize->300]&/@Join[Table[ListContourPlot@MapThread[Append,{xyf/@(First/@mag),(i@#)&/@mag}],{i,{Norm@#[[2;;3]]&,Last,Norm[Rest@#]&}}],
		{BlendPlot[xyf/@(First/@mag)],ListLinePlot@Transpose[Rest/@LoadData2[fname,";accel"]],ListLinePlot@Transpose[Rest/@mag]}]}
	]]/@{"BEJ_KEJ_5_georgezhou_Jul13_cyclic_2_1.txt","BEJ_KEJ_5_georgezhou_Jul13_cyclic_2_2.txt",
		"BEJ_KEJ_5_georgezhou_Jul13_cyclic_3_1.txt","BEJ_KEJ_5_georgezhou_Jul13_cyclic_3_2.txt",
		"BEJ_KEJ_5_georgezhou_Jul13_cyclic_4_1.txt","BEJ_KEJ_5_georgezhou_Jul13_cyclic_4_2.txt"}//MatrixForm*)


Export["t.png",#]&@Graphics3D@Line[Normalize[Rest@#]&/@LoadData2["in_place_rotate_nexus_s_sensors_2.txt",";compass"]]


FitEllipsoid2=Function[xyzs,
	Module[{x0,y0,z0,r,b,c,xy,yz,xz,r2,rules},
	{r2,rules}=NMinimize[{Sum[Abs[Sqrt[(xyzs[[i,1]]-x0)^2+
		xy(xyzs[[i,1]]-x0)(xyzs[[i,2]]-y0)/b+(xyzs[[i,2]]-y0)^2/b^2+
		yz(xyzs[[i,2]]-y0)(xyzs[[i,3]]-z0)/b/c+(xyzs[[i,3]]-z0)^2/c^2+
		xz(xyzs[[i,1]]-x0)(xyzs[[i,3]]-z0)/c]-r],{i,Length@xyzs}],0.9<b<1.1,0.9<c<1.1,-0.1<xy<0.1,-0.1<xz<0.1,-0.1<yz<0.1},{x0,y0,z0,r,b,c,xy,yz,xz}];
	{(r/.rules)^2,rules,ListLinePlot[(#[[1]]-x0)^2+xy(#[[1]]-x0)(#[[2]]-y0)/b+yz(#[[2]]-y0)(#[[3]]-z0)/b/c+xz(#[[1]]-x0)(#[[3]]-z0)/c+(#[[2]]-y0)^2/b^2+(#[[3]]-z0)^2/c^2/.rules&/@xyzs]}]];


r=Map[Function[fname,
	Module[{mag=LoadData2[fname,";compass"],xyzt=LoadData2[fname,";accel"]},
	{fname,FitEllipsoid[Rest/@mag],FitEllipsoid[Rest/@xyzt]}]],FileNames["in_place_rotate_*nexus*"]]


Join[{#[[1]]},Last/@#[[2,2]]]&/@r//MatrixForm


fname="t.txt";
qf=LoadQuaternionF[fname];
mag=LoadData2[fname,";compass"];
xyzt=LoadData2[fname,";accel"];
(*xyf=LoadXYf[fname,""];xys=xyf/@mag[[;;,1]];*)
qmags=RotateByQuaternion[#[[2;;4]],qf[#[[1]]]]&/@mag;
(*ListLinePlot@xys*)


fname="BEJ_KEJ_5_georgezhou_Jul13_cyclic.txt";(*fname="PEK_google_KEJ_5_hongjibao.txt";*)(*fname="PEK_google_KEJ_5_baohj.txt";*)
fname="BEJ_KEJ_5_georgezhou_Jul5_5.txt";(*fname="BEJ_KEJ_5_georgezhou_Jul5_S.txt";*)
(*fname="ABQ.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul13_cyclic_2_1.txt";*)
mag=LoadData2[fname,";compass"];
(*qf=LoadQuaternionF[fname];*)
(*xyf=LoadXYf[fname,""];xys=xyf/@mag[[;;,1]];*)
(*qmags=RotateByQuaternion[#[[2;;4]],qf[#[[1]]]]&/@mag;*)
(*xyzt=LoadData2[fname,";accel"];*)
(*qxyzs=RotateByQuaternion[#[[2;;4]],qf[#[[1]]]]&/@xyzt;*)
(*ListLinePlot@xys*)
(*ListLinePlot@Transpose@qmags[[;;]]*)
ListLinePlot@Transpose[Rest/@LoadData2[fname,";accel"]]
(*ListLinePlot@Transpose@qxyzs*)


(*Function[fname,
	Module[{mag=LoadData2[fname,";compass"],xyf=LoadXYf[#,""]&@fname},
	{fname,ListLinePlot[xyf/@(First/@mag)],ListLinePlot[Accumulate@mag[[All,3]]],
	ListLinePlot[Accumulate@mag[[All,2]]],ListLinePlot[Accumulate@mag[[All,4]]]}]]/@FileNames["BEJ_KEJ_5_georgezhou_Jul13_cyclic*.txt"]*)


(*Function[fname,
	Module[{xyf=LoadXYf[#,""]&@fname,mag=LoadData2[fname,";compass"]},
	{fname,ListLinePlot[xyf/@(First/@mag)],ListLinePlot@mag[[All,3]],ListLinePlot[Accumulate@mag[[All,3]]]}
	]]/@FileNames["BEJ_KEJ_5_georgezhou_Jul*.txt"]*)


fname="BEJ_KEJ_5_georgezhou_Jul5_5.txt";fname2="BEJ_KEJ_5_georgezhou_Jul5_6.txt";
{mag,mag2}=LoadData2[#,";compass"]&/@{fname,fname2};
{xyf,xyf2}=LoadXYf[#,""]&/@{fname,fname2};
ListLinePlot[{xyf/@(First/@mag),xyf2/@(First/@mag2)}]
ListLinePlot[#[[All,3]]&/@{mag,mag2}]
ListLinePlot[Accumulate@#[[All,3]]&/@{mag,mag2}]


fname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul6.txt";
fname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul16_1_1.txt";
(*fname="BEJ_KEJ_4_georgezhou_Jul17_1_2_shirt.txt";modelfname="BEJ_KEJ_4_georgezhou_Jul17_1_1.txt";
fname="BEJ_KEJ_4_georgezhou_Jul17_2_2_shirt.txt";modelfname="BEJ_KEJ_4_georgezhou_Jul17_1_1.txt";*)
{{sig,xys},{sig2,xys2}}=BuildMagModel/@{fname,modelfname};
ListLinePlot@{xys,xys2}
BlendPlot/@{xys,xys2}


Export["t.png",#]&@Image[#,ImageSize->Large]&@
	ListLinePlot[{First@#,Norm@Rest@#}&/@LoadData2[#,";compass"]&/@{fname,modelfname},PlotRange->All]


c=60;len=30;
ssig=sig[[c;;c+len]];sxys=xys[[c;;c+len]];
r2=Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1];
r={Labeled[ListLinePlot[{sig,sig2},MaxPlotPoints->5000],"Mag-Norm"],(*ListLinePlot@ssig,*)
	Labeled[ListLinePlot[r2,PlotRange->All,MaxPlotPoints->5000],"Correlation"]}
(*cs=Ordering[Abs@Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1]][[-1;;]];*)
cs=Reverse[First/@(SortBy[{#,r2[[#]]}&/@(1+Flatten@MapIndexed[If[#==1,{#2[[1]]},{}]&,CrossingDetect@Differences@r2]),Last][[-5;;]])]
Function[c2,
	sxys2=xys2[[c2;;c2+len]];
	Image[#,ImageSize->300]&/@{ListLinePlot[{Table[0,{c2}]~Join~ssig,sig2},MaxPlotPoints->5000],ListPlot[{sxys,First/@Partition[xys,2]}(*,MaxPlotPoints->5000*),PlotRange->All],
		ListPlot[{sxys2,First/@Partition[xys2,2]}(*,MaxPlotPoints->5000*),PlotRange->All]}]/@cs


(*l=Function[ssig,
		Module[{r2,cs},
		r2=Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1];
		cs=Reverse[Ordering[r2][[-10;;]]];
		MapThread[List,{cs,r2[[cs]]}]
		]]/@Partition[sig,30,10];
l2=FoldList[Function[{la,lb},Reverse@normalize@SortBy[{First[#],Last[#]+Mean[Function[b,10/(0.1+Norm[xys2[[First[b]]]-xys2[[First[#]]]])Last[b]]/@la]}&/@lb,Last]]
	,normalize@First[l],Rest[l]];
l[[;;10]]//MatrixForm
l2[[;;10]]//MatrixForm
BlendPlot@MedianFilter[xys2[[#[[1,1]]&/@l2]],{1,0}]*)


normalize=Function[l,
	MapThread[List,{First/@l,Last/@l /Total[Last/@l]}]];
TestMagModel2=Function[{fname,sig2,xys2},
	Module[{sig,xys,l,l2},
	{sig,xys}=BuildMagModel@fname;
	l=Function[ssig,
		Module[{r2,cs},
		r2=Correlation[ssig,#]&/@Partition[sig2,Length@ssig,1];
		cs=Reverse[Ordering[r2][[-10;;]]];
		MapThread[List,{cs,r2[[cs]]}]
		]]/@Partition[sig,30,10];
	l2=FoldList[Function[{la,lb},Reverse@normalize@SortBy[{First[#],Last[#]+Mean[Function[b,10/(0.1+Norm[xys2[[First[b]]]-xys2[[First[#]]]])Last[b]]/@la]}&/@lb,Last]]
		,normalize@First[l],Rest[l]];
	xys2[[#[[1,1]]&/@l2]]
	]];


Function[fname,{fname,BlendPlot@LoadXYs[fname,""][[1]],BlendPlot@MedianFilter[TestMagModel2[fname,sig,xys],{3,0}]}]/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]]


modelfname="BEJ_KEJ_5_georgezhou_Jul6.txt";
{sig,xys}=BuildMagModel@modelfname;
Function[fname,{fname,BlendPlot@LoadXYs[fname,""][[1]],BlendPlot@MedianFilter[TestMagModel[fname,sig,xys],{3,0}]}]/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]]


hongjibaoConnect={{1,10},{6,17},{9,11},{20,12},{25,12},{23,26},{26,36},{34,37},{39,47},{39,52},{44,50},{53,66},{58,91},{59,69},{82,85},{84,88},{84,75},{70,109},{68,91},{100,102},{100,103},{100,105},{105,113},{95,98}};
Jul5Survey2Connect={{4,104},{40,142},{52,129},{155,280},{166,295},{166,314},{184,331},{340,751},{556,655},{397,544}};


{ListPlot@sig2[[634;;655]],ListPlot@Reverse@sig2[[556;;577]]}
{ListLinePlot@sig[[97;;130]],ListLinePlot@Join[sig2[[142;;157]],Reverse@sig2[[250;;283]]]}


nonIntersectingPath=Function[{d,vertex,n,limit},
	Nest[Function[paths,
	Flatten[Function[path,With[{v=Last@path},path~Append~#&/@Complement[If[Head[v/.d]===Integer,{},v/.d]~Join~If[v<limit,{v+1},{}]~Join~If[v>1,{v-1},{}],path]]]/@paths,1]],{{vertex}},n-1]];
(*ListLinePlot@xys3[[#]]&/@nonIntersectingPath[d,40,50,Length@xys3]
ListLinePlot[xys3[[#]]&/@nonIntersectingPath[d,40,50,Length@xys3]]*)
similarity=Function[{sa,sb},
	Module[{s,s2},If[Length@sa<Length@sb,{s,s2}={sb,sa},{s,s2}={sa,sb}];
	Module[{sf=Interpolation[s,InterpolationOrder->1],i,ratio=N[Length@s]/Length@s2},1-CorrelationDistance[s2,sf/@(ratio Range@Length@s2)]]]];
{similarity[Range[10],Range[3]],similarity[Range[10],RandomVariate[NormalDistribution[],10]],similarity[Range[10],Reverse@Range[3]]}


fishyConnect={{1,126},{46,166},{91,281},{106,141},{301,766},{721,616},{320,596},{351,826},{876,896},{906,1221},{1256,1281},{1296,1361},{1296,1236},{1371,1476},{1406,1056},{1046,1531},{1566,966},{1601,1621},{1521,1066}};
(*pair={1,50};ratio=1.2;*)
pair={131,186};ratio=1.1;
ssig=sig2[[pair[[1]];;pair[[-1]]]];sxys=xys2[[pair[[1]];;pair[[-1]]]];
d=Dispatch[Rule[#[[1,1]],Last/@#]&/@GatherBy[#~Join~(Reverse/@#),First]]&@fishyConnect;
candids=Function[n,Union[Flatten[nonIntersectingPath[d,#,n,Length@xys3]&/@Range@Length@xys3,1]]]@Floor[ratio Length@ssig];//AbsoluteTiming
(*candids=Partition[nodes,Length@ssig,1];*)
r2=Reverse@SortBy[{#,similarity[ssig,sig3[[#]]]+0.2similarity[First/@sxys,First/@xys3[[#]]]+0.2similarity[Last/@sxys,Last/@xys3[[#]]]}&/@candids,Last];//AbsoluteTiming
r=r2[[;;5]];
ListPlot[{First/@Partition[xys3,10]}~Join~(xys3[[First@#]]&/@r)]
Image[#,ImageSize->200]&/@(ListPlot[{First/@Partition[xys3,10]}~Join~{xys3[[First@#]]}]&/@r)
{ListLinePlot@sxys}~Join~(ListLinePlot[xys3[[First@#]]]&/@r)
{ListLinePlot@ssig}~Join~({ListLinePlot[sig3[[First@#]]],Last@#}&/@r)


fname="BEJ_KEJ_5_georgezhou_Jul16_1_2.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul5_survey_2.txt";
{{sig,xys},{sig2,xys2}}=BuildMagModel/@{fname,modelfname};
fname3="PEK_google_KEJ_5_fishywang.txt";{sig3,xys3}=BuildMagMarkerModel@fname3;
fname4="PEK_google_KEJ_5_hongjibao.txt";{sig4,xys4}=BuildMagMarkerModel@fname4;
fname5="PEK_google_KEJ_5_baohj.txt";{sig5,xys5}=BuildMagMarkerModel@fname5;
{PlotLabeledTrace@fname,PlotLabeledTrace@modelfname,PlotLabeledMarkerTrace@fname3,PlotLabeledMarkerTrace@fname4,PlotLabeledMarkerTrace@fname5}


Export["t.png",#]&@GraphicsGrid@Partition[#,4]&[Join@@{ListLinePlot/@{sig[[3 46;;3 66]],sig2[[3 66;;3 91]],sig3[[3 1;;3 26]],sig4[[3 1;;3 31]]},
	Graphics@Line@#&/@{xys[[3 46;;3 66]],xys2[[3 66;;3 91]],xys3[[3 1;;3 26]],xys4[[3 1;;3 31]]},
	ListLinePlot/@{Join@@{sig[[3 91;;3 106]],sig[[3 151;;3 166]]},sig2[[3 131;;3 186]],sig3[[3 261;;3 351]],Join@@{sig4[[3 96;;3 111]],sig4[[3 231;;3 251]],sig4[[3 346;;3 391]]}},
	Graphics@Line@#&/@{Join@@{xys[[3 91;;3 106]],xys[[3 151;;3 166]]},xys2[[3 131;;3 186]],xys3[[3 261;;3 351]],Join@@{xys4[[3 96;;3 111]],xys4[[3 231;;3 251]],xys4[[3 346;;3 391]]}}}]


ListLinePlot/@{sig[[3 201;;3 231]],sig2[[3 391;;3 431]],sig3[[3 1211;;3 1256]],sig4[[3 981;;3 1021]],sig5[[3 1551;;3 1636]]}
Graphics@Line@#&/@{xys[[3 201;;3 231]],xys2[[3 391;;3 431]],xys3[[3 1211;;3 1256]],xys4[[3 981;;3 1021]],xys5[[3 1551;;3 1636]]}
ListLinePlot/@{sig[[3 46;;3 66]],sig2[[3 66;;3 91]],sig3[[3 1;;3 26]],sig4[[3 1;;3 31]],Reverse@sig5[[3 106;;3 131]]}
Graphics@Line@#&/@{xys[[3 46;;3 66]],xys2[[3 66;;3 91]],xys3[[3 1;;3 26]],xys4[[3 1;;3 31]],Reverse@xys5[[3 106;;3 131]]}
ListLinePlot/@{Join@@{sig[[3 66;;3 71]],sig[[3 1;;3 11]]},Join[sig2[[3 91;;3 106]],sig2[[3 1;;3 16]]],sig3[[3 31;;3 61]],sig4[[3 31;;3 51]],Reverse@sig5[[3 76;;3 104]]}
Graphics@Line@#&/@{Join@@{xys[[3 66;;3 71]],xys[[3 1;;3 11]]},Join[xys2[[3 91;;3 106]],xys2[[3 1;;3 16]]],xys3[[3 31;;3 61]],xys4[[3 31;;3 51]],Reverse@xys5[[3 76;;3 104]]}
ListLinePlot/@{Join@@{sig[[3 91;;3 106]],sig[[3 151;;3 166]]},sig2[[3 131;;3 186]],sig3[[3 261;;3 351]],
	Join@@{sig4[[3 96;;3 111]],sig4[[3 231;;3 251]],sig4[[3 346;;3 391]]},Join@@{sig5[[3 16;;3 31]],sig5[[3 261;;3 286]],sig5[[3 411;;3 461]]}}
Graphics@Line@#&/@{Join@@{xys[[3 91;;3 106]],xys[[3 151;;3 166]]},xys2[[3 131;;3 186]],xys3[[3 261;;3 351]],
	Join@@{xys4[[3 96;;3 111]],xys4[[3 231;;3 251]],xys4[[3 346;;3 391]]},Join@@{xys5[[3 16;;3 31]],xys5[[3 261;;3 286]],xys5[[3 411;;3 461]]}}


Export["t.png",#]&@GraphicsGrid[#,ImageSize->Large]&@Partition[#,2]&[Join@@{Graphics@Line@#&/@{xys6[[3 701;;3 766]],xys7[[3 426;;3 491]]},ListLinePlot/@(Transpose/@{sig6[[3 701;;3 766]],sig7[[3 426;;3 491]]}),
	Graphics@Line@#&/@{xys6[[3 1526;;3 1681]],xys7[[3 906;;3 1051]]},ListLinePlot/@(Transpose/@{sig6[[3 1526;;3 1681]],sig7[[3 906;;3 1051]]})}]


fname6="ABQ.txt";fname7="ABQ_2.txt";fname8="ABQ_3.txt";
{sig6,xys6,axyzs6}=BuildMagMarkerModel2@fname6;{sig7,xys7,axyzs7}=BuildMagMarkerModel2@fname7;{sig8,xys8,axyzs8}=BuildMagMarkerModel2@fname8;
PlotLabeledMarkerTrace/@{fname6,fname7,fname8}
ListLinePlot/@(Transpose/@{sig6[[3 701;;3 766]],sig7[[3 426;;3 491]],sig8[[3 451;;3 496]]})
ListLinePlot/@(Transpose/@{sig6[[3 1526;;3 1681]],sig7[[3 906;;3 1051]],Reverse@sig8[[3 1121;;3 1251]]})


(*fname9="1250_survey.txt";fname10="1250_survey_2.txt";fname11="1250_survey_3.txt";
{sig9,xys9}=BuildMagMarkerModel@fname9;{sig10,xys10}=BuildMagMarkerModel@fname10;{sig11,xys11}=BuildMagMarkerModel@fname11;
PlotLabeledMarkerTrace/@{fname9,fname10,fname11}*)
(*ListLinePlot/@{Join@@{sig9[[2116;;2166]],sig9[[1441;;1481]]},Reverse@sig10[[686;;786]],Join@@{Reverse@sig11[[871;;921]],sig11[[1101;;1136]]}}
ListLinePlot/@{sig9[[1836;;1906]],Join@@{Reverse@sig10[[1226;;1236]],sig10[[876;;936]]},Join@@{Reverse@sig11[[1061;;1076]],Reverse@sig11[[831;;866]]}}*)


fname12="BEJ_KEJ_5_georgezhou_Jul5_6.txt";fname13="BEJ_KEJ_5_georgezhou_Jul11.txt";fname14="BEJ_KEJ_5_georgezhou_Jul5_5.txt";
{{sig12,xys12},{sig13,xys13},{sig14,xys14}}=BuildMagModel/@{fname12,fname13,fname14};
PlotLabeledTrace/@{fname12,fname13,fname14}


ListLinePlot/@{sig2[[86;;126]],Reverse@sig3[[576;;606]]}
ListLinePlot/@{sig2[[301;;336]],Reverse@sig3[[301;;326]]}
ListLinePlot/@{sig2[[136;;166]],sig3[[431;;456]]}
ListLinePlot/@{sig2[[86;;166]],Reverse@sig3[[541;;606]]}
ListLinePlot/@{sig2[[266;;336]],Join@@{Reverse@sig3[[416;;456]],Reverse@sig3[[306;;326]]}}


shapeWeight=0.2;
Jul56Connect={{1,91},{31,121},{41,111},{166,241},{206,281},{296,606},{326,416},{426,571},{471,496},{496,511},{471,516}};
{{sig3,xys3},{sig2,xys2}}={{sig12,xys12},{sig13,xys13}};
indices=Join@@{Range[86,126](*,Range[32,86]*)};
indices=Join@@{Range[301,336](*,Range[32,86]*)};
indices=Join@@{Range[136,166](*,Range[32,86]*)};
indices=Join@@{Range[81,166](*,Range[32,86]*)};
indices=Join@@{Range[266,336](*,Range[32,86]*)};
ratio=1.;
ssig=sig2[[indices]];sxys=xys2[[indices]];
d=Dispatch[Rule[#[[1,1]],Last/@#]&/@GatherBy[#~Join~(Reverse/@#),First]]&@Jul56Connect;
candids=Function[n,Union[Flatten[nonIntersectingPath[d,#,n,Length@xys3]&/@Range@Length@xys3,1]]]@Floor[ratio Length@ssig];//AbsoluteTiming
(*candids=Partition[nodes,Length@ssig,1];*)
r2=Reverse@SortBy[{#,similarity[ssig,sig3[[#]]]+shapeWeight(similarity[First/@sxys,First/@xys3[[#]]]+similarity[Last/@sxys,Last/@xys3[[#]]])}&/@candids,Last];//AbsoluteTiming
r=r2[[;;5]];
ListPlot[{First/@Partition[xys3,10]}~Join~(xys3[[First@#]]&/@r)]
Image[#,ImageSize->200]&/@(ListPlot[{First/@Partition[xys3,10]}~Join~{xys3[[First@#]]}]&/@r)
{ListLinePlot@sxys}~Join~(ListLinePlot[xys3[[First@#]]]&/@r)
{ListLinePlot@ssig}~Join~({ListLinePlot[sig3[[First@#]]],Last@#}&/@r)


fname2="BEJ_KEJ_5_georgezhou_Jul5_8.txt";


ListLinePlot[Rest/@LoadXYs2[#,""]]&/@{fname,fname2}


ListLinePlot[GaussianFilter[Norm/@(Rest/@LoadData2[#,";accel"]),100]]&/@{fname,fname2}
ListLinePlot[GaussianFilter[Norm/@(Rest/@LoadData2[#,";compass"]),100]]&/@{fname,fname2}
ListLinePlot[GaussianFilter[Norm/@(Rest/@LoadData2[#,";gyro"]),100]]&/@{fname,fname2}


(*PlotMarkerMagneticVectors = Function[fname,
   	Module[{mag, xys, ts, rot, rots, res, mags},
    	mag = LoadData2[fname, ";compass"];
    	{xys, ts} = {Rest/@#,First/@#}&@LoadMarkerXY[fname];
    	mags = Interpolation[Map[{#[[1]], #[[2 ;;]]} &, mag], InterpolationOrder -> 1] /@ ts;
    	Graphics[Arrow/@MapThread[List, {xys, xys + 0.05 (#-{3,20}&/@mags[[All, 1 ;; 2]])}],PlotLabel->fname]]];*)


PlotMagneticVectors[#,500,{3,20},0.3]&/@FileNames["BEJ*TUSPARK*"]


r=PlotMagneticVectors[#,500,{3,20},0.15]&/@FileNames["BEJ*cyclic*"]


PlotMagneticVectors[#,1000,{3,20},0.3]&/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*cross*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*swing*"]]


PlotRvMarkerMagneticVectors[#,500,{30,40},0.2,30]&/@{fname3,fname5}


PlotRvMarkerMagneticVectors[#,1500,{30,40},1,60]&/@{fname6,fname7,fname8}


BuildMagRvMarkerModel=Function[fname,
	Module[{mxyf=LoadMarkerXYf@fname,mxy=LoadMarkerXY[fname],dxys,dts,ts,mags,sig,d,xys,accels},
		xys=Rest/@mxy;
		dxys=Norm/@Differences@xys;
		dts=SafeFirstOrderInterpolation[MapThread[List,{Prepend[Accumulate@dxys,0],First/@mxy}]];
		ts=Table[dts[d],{d,0,Tr[dxys],0.1}];
		{mags,accels}=((Interpolation[MapThread[List,{First/@#,Rest/@#}],InterpolationOrder->1]&@LoadData2[fname,#])/@ts)&/@{";compass",";accel"};
		{MapThread[{Norm[#-Projection[#,#2]],Norm@Projection[#,#2]}&,{mags,accels}],mxyf/@ts}]];
PlotRvMarkerMagneticVectors=Function[{fname,size,mean,scale,step},
  Module[{xys, sigs},
	{sigs,xys}=(First/@Partition[#,step])&/@BuildMagRvMarkerModel[fname];
    Image[#,ImageSize->size]&@Graphics[Arrow/@MapThread[List, {xys, xys + scale (#-mean&/@sigs)}],PlotLabel->fname,Axes->True]]];
BuildMagRvModel=Function[fname,
	Module[{mxyf=LoadXYf[fname,""],dxys,dts,ts,sigs,sig,d,xyt,accels,mags,rots},
		xyt=LoadXYs2[fname,""];
		dxys=Norm/@Differences@(Rest/@xyt);
		dts=Interpolation[MapThread[List,{Prepend[Accumulate@dxys,0],First/@xyt}],InterpolationOrder->1];
		ts=Table[dts[d],{d,0,Tr[dxys],0.1}];
		{mags,rots}=
			((Interpolation[MapThread[List,{First/@#,Rest/@#}],InterpolationOrder->1]&@LoadData2[fname,#])/@ts)&/@{";compass",";rotation_vector"};
        sigs= MapThread[RotateByQuaternion[#2, {Re@Sqrt[1 - #.#], #[[1]], #[[2]], #[[3]]}] &, {rots, mags}];
		{sigs,mxyf/@ts}]];
TestMagModel2=Function[{fname,sig2,xys2,len},
	Module[{sig,xys,r2,n=3 len},
	{sig,xys}=BuildMagModel@fname;
	Function[ssig,
		r2=ListCorrelate[Standardize[ssig],sig2]/n/Sqrt[(MovingAverage[sig2^2,n]-MovingAverage[sig2,n]^2)];
		xys2[[Ordering[r2][[-1]]]]
		]/@Partition[sig,n,len]
	]];
TestMagRvModel=Function[{fname,sigs2,xys2,len},
	Module[{sigs,sig,xys,r2,n=3 len},
	{sigs,xys}=BuildMagRvModel@fname;
	r2=MapThread[Function[ssig,ListCorrelate[Standardize[ssig],#2]/n/Sqrt[(MovingAverage[#2^2,n]-MovingAverage[#2,n]^2)]]/@Partition[#,n,len]&
		,Transpose/@{sigs,sigs2}];
	xys2[[Ordering[#][[-1]]&/@Product[r2[[i]],{i,2,3}]]]
	]];
ShowMagRvSignature=Function[fname,
	Module[{sigs,xys},
	{sigs,xys}=BuildMagRvModel[fname];
	{fname,Labeled[ListLinePlot[Transpose@sigs],"Mag-World-XYZ"],Labeled[ListLinePlot@xys,"Trajectory"]}
	]];
PlotRvMagneticVectors=Function[{fname,size,mean,scale,step},
  Module[{xys, sigs},
	{sigs,xys}=(First/@Partition[#,step])&/@BuildMagRvModel[fname];
    Image[#,ImageSize->size]&@Graphics[Arrow/@MapThread[List, {xys, xys + scale (#-mean&/@sigs[[All, 2;;]])}],PlotLabel->fname,Axes->True]]];

PointJul6={{1,251},191,{426,76},{131,366,536},{},{},746,{626,836},1620,1576,{},{1116,1376},1191,1306,1251};
PointJul6T3={221,166,46,{106,336},386,436,{},531,1614,1561,1191,{1081,811},891,951,1011};
PointJul11T2={{},{},{31,1221},{86,1176},{},{},{},{},{271,1006},966,876,{471,751},551,691,631};
PointJul5T8={216,156,{391,41},{106,331,481},541,601,661,{},{736,1452},776,{},{961,1206},1096,1141,1036};
(*ps={86,736,181,826,901,2206,1921,1771,2486,2891,2746,3766,3946,4111,4036};
fname3="PEK_google_KEJ_5_fishywang.txt";
PlotLabeledMarkerTrace@fname3
{sig3,xys3}=BuildMagMarkerModel@fname3;
ListLinePlot@xys3[[ps]]
groundTruthPoints=xys3[[ps]];*)
groundTruthPoints={{9.903264657744307`*^6,4.444167641569013`*^6},{9.903273773604896`*^6,4.444167918101424`*^6},{9.903264191637766`*^6,4.44417704887206`*^6},{9.903273638401376`*^6,4.444176892157127`*^6},{9.903273424496207`*^6,4.444184325254759`*^6},{9.903264191701485`*^6,4.444184534658874`*^6},{9.903264095923662`*^6,4.444195417485811`*^6},{9.903273153534709`*^6,4.444189780759675`*^6},{9.903275857859774`*^6,4.444199481982081`*^6},{9.90327569257166`*^6,4.44420541862263`*^6},{9.903289749450989`*^6,4.444205972781347`*^6},{9.90330399053517`*^6,4.444202626699201`*^6},{9.903303683762511`*^6,4.44421427956324`*^6},{9.903299997603271`*^6,4.444208838716636`*^6},{9.903307442349177`*^6,4.444209472129783`*^6}};
correctionTable={"BEJ_KEJ_5_georgezhou_Jul6.txt"->PointJul6,"BEJ_KEJ_5_georgezhou_Jul6_3.txt"->PointJul6T3,"BEJ_KEJ_5_georgezhou_Jul5_8.txt"->PointJul5T8,
	"BEJ_KEJ_5_georgezhou_Jul11_2.txt"->PointJul11T2};
CorrXYs=Function[fname,
	Module[{sigs,xys,d,l,split},
	{sigs,xys}=BuildMagRvModel[fname];
	d=Dispatch[Join@@MapThread[Function[x,x->#2]/@#&,{If[Head@#===List,#,{#}]&/@(fname/.correctionTable),groundTruthPoints}]];
	l={#,Boole[Head[#/.d]===Integer]}&/@Range@Length@xys;
	split=If[Tr[Last/@#]<Length[Last/@#]-2,
		With[{pos=Position[#,{_,0}][[2,1]]},
			Join[{First/@#[[;;pos]]},#0[#[[pos;;]]]]],{First/@#}]&;
	((Last[FindGeometricTransform[First@#,Last@#]&@
		Transpose@Select[MapThread[List,{#/.d,xys[[#]]}],Head@First@#===List&]]/@xys[[#]])&/@split@l)]];


PlotRvMagneticVectors[#,500,{20,-40},0.2,10]&/@FileNames["BEJ*cyclic*"]


PlotRvMagneticVectors[#,500,{20,-40},0.2,10]&/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*cross*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*swing*"]]


Map[ShowMagRvSignature,FileNames["BEJ_KEJ_5_georgezhou_Jul5*"]]


ListLinePlot[Join@@@(CorrXYs/@{"BEJ_KEJ_5_georgezhou_Jul6.txt","BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"})]
ListLinePlot/@(CorrXYs/@{"BEJ_KEJ_5_georgezhou_Jul6.txt","BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"})


modelfname="BEJ_KEJ_5_georgezhou_Jul6.txt";
{sigs,xys}=BuildMagModel@modelfname;
xys=Append[Join@@(Most/@#),#[[-1,-1]]]&@CorrXYs[modelfname,PointJul6,groundTruthPoints];


r=TestMagModel2[#,sigs,xys,30]&@"BEJ_KEJ_5_georgezhou_Jul11_2.txt";
BlendPlot@MedianFilter[r,{3,0}]


Function[fname,{fname,BlendPlot@LoadXYs[fname,""][[1]],BlendPlot@MedianFilter[TestMagModel2[fname,sigs,xys,30],{3,0}]}]/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"}


modelfname
{sigs2,xys2}=BuildMagRvModel@modelfname;


BlendPlot@MedianFilter[TestMagRvModel[#,sigs2,xys2,30],{3,0}]&/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"}


{sigs,xys}=BuildMagRvModel@"BEJ_KEJ_5_georgezhou_Jul6_3.txt";


len=30;n=3 len;
r=MapThread[Function[ssig,ListCorrelate[Standardize[ssig],#2]/n/Sqrt[(MovingAverage[#2^2,n]-MovingAverage[#2,n]^2)]]/@Partition[#,n,len]&
	,Transpose/@{sigs,sigs2}];


BlendPlot@MedianFilter[xys2[[Ordering[#][[-1]]&/@Product[r[[i]],{i,2,3}]]],{3,0}]


{ts,xys,xyf,wifis,wifimap,posmap,vectorizeRSS}=BuildWifiModel[AbsoluteFileName@modelfname];
WifiDistance2=Compile[{{v,_Real,1},{v2,_Real,1}},
	1/(1+Tr[MapThread[If[#1==0||#2==0,0,5./(5+Abs[#1-#2])]&,{v,v2}]]/Tr[MapThread[Boole[#1!=0||#2!=0]&,{v,v2}]])];
(*{MatrixPlot@Outer[EuclideanDistance,First/@wifimap,First/@wifimap,1],MatrixPlot@Outer[WifiDistance,Last/@wifimap,Last/@wifimap,1]}*)
nf=Nearest[Last/@wifimap,DistanceFunction->WifiDistance2];


Map[If[Head[#]===Graphics,Image[#,ImageSize->200],#]&,
	Parallelize@Map[{#}~Join~TestWifiModel[#,nf,posmap,vectorizeRSS]&,{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"}],{2}]//MatrixForm


{{sigs,xys},{sigs2,xys2}}=BuildMagModel/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul11_2.txt"};


r=EvalPF/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul26_1_2.txt"}
(*/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]]]*)


FastMapCorrelation=Compile[{{klist,_Real,1},{list,_Real,1}},
	With[{n=Length@klist},
	ListCorrelate[Standardize[klist],list]/N[n]/Sqrt[(MovingAverage[list^2.,n]-MovingAverage[list,n]^2.)]
	]];


EvalPF=Function[fname,
	Module[{sigs,xys,sigs2,xys2,mx,numP,sigmaZ,sigsf,proc,r,randomPick,res,r2,r3,f},
{{sigs,xys},{sigs2,xys2}}=BuildMagModel/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt",fname};
f={#&,GaussianFilter[#,10]&,Differences@GaussianFilter[#,10]&}[[1]];
mx=Length@sigs;
numP=500;
sigmaZ=1;
sigsf=Interpolation[f@sigs,InterpolationOrder->1];
randomPick=Function[n,Transpose@{RandomVariate[UniformDistribution[{1,mx}],n],Table[1./numP,{n}]}];
proc=Function[{state,obs},
	Module[{state2,tw},
	state2={#[[1]]+RandomVariate[NormalDistribution[1,1]],#[[2]]Exp[(*-(obs-sigsf[#[[1]]])^2/sigmaZ^2*)-Abs[obs-sigsf[#[[1]]]]/sigmaZ]}&/@state;
	tw=Tr[Last/@state2];
	state2={#[[1]],#[[2]]/tw}&/@state2;
	If[Norm[Last/@state2]^(-2)<numP/2,
		(*Join[*){#,1./numP}&/@RandomChoice[Rule[Last/@state2,First/@state2],numP](*,randomPick[numP/10]]*)
		,state2]
	]];
res=FoldList[proc,randomPick@numP,f@sigs2];
(*r=SortBy[#,Last][[-1]]&/@res;*)
r2=Mean[(#[[2]] xys[[Clip[Round@#[[1]],{1,Length@xys}]]])&/@#]&/@res;
r3=Tr[(#[[2]] #[[1]])&/@#]&/@res;
{fname,(*ListLinePlot[First/@r],*)BlendPlot/@{r3,r2,(*xys[[Clip[(Round@First@#)&/@r,{1,Length@xys}]]],*)xys2}(*,ListLinePlot[Last/@r]*)}]];

EvalCorrelationPF2=Function[{sigs,xys,sigs2,xys2},
	Module[{mx,r,k=80,ra,i},
	mx=Length@sigs-k+1;
	ra=N@Range[mx];
	r=Reap[Fold[Function[{state,obs},
		Module[{state2},
		Sow[Tr[ra state]];
		state2=Normalize[#,Tr]&@ListCorrelate[{0.2,0.6,0.2},state ((1.+#)/2.&/@FastMapCorrelation[obs,sigs]),-1];
		If[1./Tr[state2^2.]<mx/2,
			Normalize[#,Tr]&@PadRight[Normal[SparseArray[Rule@@@(Tally@RandomChoice[state2->Range@Length@state2,Length@state2])]],Length@state2,0],state2]]]
			,N@Table[1./mx,{mx}],Partition[sigs2,k,1]]][[2,1]];
	r]];
EvalPF2=Function[{sigs,xys,sigs2,xys2},
	Module[{mx=Length@sigs,r,ra,i,state2},
	r=Reap[Fold[Function[{state,obs},
		Sow[Ordering[state][[-1]],1];
		state2=Normalize[#,Tr]&@ListCorrelate[(*{0.1,0.2,0.4,0.2,0.1}*)(*{0.05,0.05,0.4,0.1,0.4}*)
			(*{0.1,0.4,0.,0.4,0.1},state ((1./(1.+Norm[obs-#]^2.))&/@sigs),-3];*)
			{0.2,0.8,0.}(*{0.1,0.2,0.6,0.1}*),state ((1./(1.+Norm[obs-#]^2.))&/@sigs),-1];
		Sow[state2,2];
		If[1./Tr[state2^2.]<mx/2,
			Normalize[#,Tr]&@PadRight[Normal[SparseArray[Rule@@@(Tally@Join[RandomSample[Range[mx],Floor[mx/100]],
				RandomChoice[state2->Range@mx,mx]])]],mx,0],state2]]
			,N@Table[1./mx,{mx}],sigs2]][[2]];
	r]];
EvalPF3=Function[{sigs,xys,sigs2,xys2},
	Module[{mx=Length@sigs,r,ra,i,state2},
	r=Reap[Fold[Function[{state,obs},
		Sow[Ordering[state][[-1]],1];
		state2=Normalize[#,Tr]&@ListCorrelate[(*{0.1,0.2,0.4,0.2,0.1}*)(*{0.05,0.05,0.4,0.1,0.4}*)
			(*{0.1,0.4,0.,0.4,0.1},state ((1./(1.+Norm[obs-#]^2.))&/@sigs),-3];*)
			{0.2,0.8,0.}(*{0.1,0.2,0.6,0.1}*),state (Exp[-Norm[Normalize@obs-Normalize@#]^2.] (*(1./(1.+Norm[obs-#]^2.))*)&/@sigs),-1];
		Sow[state2,2];
		If[1./Tr[state2^2.]<mx/2,
			Normalize[#,Tr]&@PadRight[Normal[SparseArray[Rule@@@(Tally@Join[RandomSample[Range[mx],Floor[mx/100]],
				RandomChoice[state2->Range@mx,mx]])]],mx,0],state2]]
			,N@Table[1./mx,{mx}],sigs2]][[2]];
	r]];
EvalPFDual=Function[{sigs,xys,sigs2,xys2},
	Module[{mx=2 Length@sigs,r,ra,i,state2,post,ls,rs,cross=0.1},
	r=Reap[Fold[Function[{state,obs},
		Sow[Ordering[state][[-1]],1];
		post=(1./(1.+Norm[obs-#]^2.))&/@sigs;
		ls=ListCorrelate[{0.2,0.3,0.5,0.1,0.0},state[[1;;mx/2]] post,-2];
		rs=ListCorrelate[{0.1,0.5,0.3,0.2},state[[mx/2+1;;]] post,1];
		state2=Normalize[#,Tr]&@Join[(1-cross)ls+cross rs,cross ls+(1-cross) rs];
		Sow[state2,2];
		If[1./Tr[state2^2.]<mx/2,
			Normalize[#,Tr]&@PadRight[Normal[SparseArray[Rule@@@(Tally@Join[RandomSample[Range[mx],Floor[mx/2/100]],
				RandomChoice[state2->Range@mx,mx]])]],mx,0],state2]]
			,N@Table[1./mx,{mx}],sigs2]][[2]];
	r]];
BuildDiffMagModel2=Function[{fname,n},
	Module[{mxyf=LoadXYf[fname,""],dxys,dts,ts,mag,mags,sig,d,xys,ts2,axyz,axyzs},
		{xys,ts2}=LoadXYs[fname,""];
		dxys=Norm/@Differences@xys;
		dts=Interpolation[MapThread[List,{Prepend[Accumulate@dxys,0],ts2}],InterpolationOrder->1];
		ts=Table[dts[d],{d,0,Tr[dxys],0.1}];
		mag=LoadData2[fname,";compass"];axyz=LoadData2[fname,";accel"];
		axyz=Transpose@Join[{First/@axyz},FoldList[(1.-10./n)#+10./n #2&,First@#,Rest@#]&/@Transpose[Rest/@axyz]];
		mag=Transpose@Join[{First/@mag},Transpose[Rest/@mag]-(FoldList[(1.-1./n)#+1./n #2&,First@#,Rest@#]&/@Transpose[Rest/@mag])];
		{mags,axyzs}=((Interpolation[MapThread[List,{First/@#,Rest/@#}]]&@#)/@ts)&/@{mag,axyz};
		{mags,mxyf/@ts,axyzs}]];
BuildDiffMagModel=Function[{fname,n},
	Module[{mags,xys,axyzs},
	{mags,xys,axyzs}=BuildDiffMagModel2[fname,n];
	{Norm/@mags,xys}]];	
TestEvalPF=Function[{modelfname,fname,f},
	Module[{r,r2,sigs,xys,sigs2,xys2},
	{{sigs,xys},{sigs2,xys2}}=BuildDiffMagModel[#,500]&/@{modelfname,fname};
	r2=MatrixPlot@Outer[Exp[-(#-#2)^2]&,sigs,sigs2,1];
	r=f[sigs,xys,sigs2,xys2];
	{r2,MatrixPlot@r[[2]],ListLinePlot@r[[1]]}]];
TestEvalPF2=Function[{modelfname,fname,f,n},
	Module[{r,r2,mags,xys,mags2,xys2,axyzs,axyzs2},
	{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildDiffMagModel2[#,n]&/@{modelfname,fname};
	r2=MatrixPlot@Outer[Exp[-Norm[#-#2]^2]&,First/@Partition[mags,5],First/@Partition[mags2,5],1];
	r=f[mags,xys,mags2,xys2];
	{r2,MatrixPlot@r[[2]],ListLinePlot@r[[1]]}]];


ListCorrelate[{0.2,0.3,0.5,0.1,0.0},{aa,bb,cc,dd,ee,ff,gg,hh},-2]
ListCorrelate[{0.1,0.5,0.3,0.2},{aa,bb,cc,dd,ee,ff,gg,hh},1]
ListCorrelate[{0.1,0.2,0.6,0.1},{aa,bb,cc,dd,ee,ff,gg,hh},-1]


TestEvalPF2["BEJ_KEJ_5_georgezhou_Jul30_1_dual_2.txt",#,EvalPF2,500]&/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt","BEJ_KEJ_5_georgezhou_Jul6_2.txt","BEJ_KEJ_5_georgezhou_Jul6_3.txt",
	"BEJ_KEJ_5_georgezhou_Jul6.txt","BEJ_KEJ_5_georgezhou_Jul5_5.txt","BEJ_KEJ_5_georgezhou_Jul5_6.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt"}


TestEvalPF2["BEJ_KEJ_5_georgezhou_Jul30_1_dual_2.txt",#,EvalPF2,500]&/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt","BEJ_KEJ_5_georgezhou_Jul6_2.txt","BEJ_KEJ_5_georgezhou_Jul6_3.txt",
	"BEJ_KEJ_5_georgezhou_Jul6.txt","BEJ_KEJ_5_georgezhou_Jul5_5.txt","BEJ_KEJ_5_georgezhou_Jul5_6.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt"}


TestEvalPF2["BEJ_KEJ_5_georgezhou_Jul30_1_dual_2.txt",#,EvalPF2,50]&/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt","BEJ_KEJ_5_georgezhou_Jul6_2.txt","BEJ_KEJ_5_georgezhou_Jul6_3.txt",
	"BEJ_KEJ_5_georgezhou_Jul6.txt","BEJ_KEJ_5_georgezhou_Jul5_5.txt","BEJ_KEJ_5_georgezhou_Jul5_6.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt"}


labels={"-26day","-6day","-2day","0day"};
r2=BuildMagModel2/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul26_1.txt","BEJ_KEJ_5_georgezhou_Jul30_1_dual.txt","BEJ_KEJ_5_georgezhou_Aug1_2_dual.txt"};
Dimensions/@r2


r={r2[[1]],r2[[2]],#[[;;1550]]&/@r2[[3]],#[[;;1400]]&/@r2[[4]]};


Norm@Mean@#[[1]]&/@r


Export["t.png",#]&@GraphicsRow[MapIndexed[ListPlot[Norm/@#[[1]],PlotLabel->labels[[#2[[1]]]]]&,r],ImageSize->1000]


Export["t.png",#]&@GraphicsRow[MapIndexed[ListPlot[Transpose@#[[1]],PlotLabel->labels[[#2[[1]]]]]&,r],ImageSize->1000]


Export["t.png",#]&@GraphicsRow[MapIndexed[BlendPlot[#[[2]],PlotLabel->labels[[#2[[1]]]]]&,r],ImageSize->1000]


(*Try shirt*)
{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildDiffMagModel2[#,500]&/@{modelfname,fname};//AbsoluteTiming
ListLinePlot[(Norm/@MapThread[Projection,#])&/@{{mags,axyzs},{mags2,axyzs2}},PlotRange->All]
{sigs,sigs2}=(Norm/@MapThread[(#-Projection[#,#2])&,#])&/@{{mags,axyzs},{mags2,axyzs2}};
MatrixPlot@Outer[Exp[-Norm[#-#2]^2]&,sigs,sigs2,1]


(*modelfname="BEJ_KEJ_5_georgezhou_Aug1_2_dual.txt";*)
modelfname="BEJ_KEJ_5_georgezhou_Jul26_1.txt";
(*modelfname="BEJ_KEJ_5_georgezhou_Jul30_1_dual_2.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul6_2.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_6.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_survey_3_1.txt";*)
(*modelfname="BEJ_KEJ_5_georgezhou_Jul5_8.txt";*)
fname="BEJ_KEJ_5_georgezhou_Jul6_3.txt";
(*fname="PEK_google_KEJ_5_fishywang.txt";*)
(*fname="PEK_google_KEJ_5_baohj.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul6_2.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul30_1_dual.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_3.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_5.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_8.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_6.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_survey_3_2.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_survey.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul5_shirt.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul10_swing.txt";*)
(*fname="BEJ_KEJ_5_georgezhou_Jul26_1_2.txt";*)
(*{{sigs,xys},{sigs2,xys2}}=BuildMagModel/@{modelfname,fname};//AbsoluteTiming*)
(*{{sigs,xys},{sigs2,xys2}}=BuildDiffMagModel[#,500]&/@{modelfname,fname};//AbsoluteTiming*)
(*{Mean@sigs-Mean@sigs2,Median@sigs-Median@sigs2,Image[#,ImageSize->500]&@ListLinePlot[{sigs,sigs2},PlotRange->All]}*)
(*r=MatrixPlot@Outer[Exp[-(#-#2)^2]&,sigs,sigs2,1]*)
(*Map[Function[i,{{{sigs,xys},{sigs2,xys2}}=BuildDiffMagModel[#,i]&/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt",fname};//AbsoluteTiming,
	Mean@sigs-Mean@sigs2,Median@sigs-Median@sigs2,ListLinePlot[{sigs,sigs2},PlotRange->All]}],
	{10,20,50,100,200,500,1000,2000,5000}]*)
{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildDiffMagModel2[#,500]&/@{modelfname,fname};//AbsoluteTiming
Image[#,ImageSize->Large]&/@{(*MatrixPlot@Outer[Exp[-Norm[{Norm@#[[;;2]]-Norm@#2[[;;2]],#[[3]]-#2[[3]]}]^2.]&,mags,mags2,1],*)MatrixPlot@Outer[Exp[-(Norm[#-#2])^2]&,mags,mags2,1]}
Image[#,ImageSize->Large]&/@{ListLinePlot@Transpose@mags,ListLinePlot@Transpose@mags2,ListLinePlot@{xys,xys2}}


Histogram@Select[ToExpression[First@#]&/@Import["t2.txt","Table"],NumberQ@#&&#>0.01&]


Histogram@Select[ToExpression[First@#]&/@Import["t.txt","Table"],NumberQ@#&&#>0.01&]


(*It seems the gravity-dir magnetic collected with two directions are different*)
{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildMagModel2[#]&/@{modelfname,fname};//AbsoluteTiming
(*Image[#,ImageSize->Large]&@ListLinePlot[GaussianFilter[#,100]&/@-{mags[[All,3]],Reverse@mags[[All,3]]}]
Image[#,ImageSize->Large]&@ListLinePlot[GaussianFilter[#,100]&/@{Norm/@MapThread[Projection,{mags,axyzs}],Reverse[Norm/@MapThread[Projection,{mags2,axyzs2}]]}]*)


RotationMatrix[ArcSin[Cross[{1,0,0},{0,-1,0}][[3]]],{0,1,0}]


RotationMatrix[Pi/4,{0,1,0}]


proc3=Function[{mags,axyzs},
	Module[{m3,m1,m2},
		m3=MapThread[Projection,{mags,axyzs}];
		#[[2]]&/@mags-m3
	]]


proc2=Normalize@Most@#&/@#&;
r3=Outer[Exp[-(Norm[#-#2])^2]&,proc2@mags,proc2@mags2,1];
MatrixPlot@r3


Image[#,ImageSize->Large]&@ListLinePlot@#&/@{Transpose@mags,Transpose@mags2}


proc=Transpose[#-GaussianFilter[#,500]&/@Transpose[#]]&;
r=Outer[1./(1.+(Norm[#-#2])^2)&,proc@mags,proc@mags2,1];
r2=Outer[Exp[-(Norm[#-#2])^2]&,Normalize/@mags,Normalize/@mags2,1];
Image[#,ImageSize->Large]&@MatrixPlot@#&/@{r,r2}


col=1;
(*{sigs,sigs2}=#-GaussianFilter[#,100]&/@{mags[[;;Floor[Length@mags/2],col]],-Reverse@mags[[Floor[Length@mags/2];;,col]]};*)
{sigs,sigs2}=(Norm/@(#-GaussianFilter[#,500]))&/@{mags[[All,col]],mags2[[All,col]]};
ListLinePlot@{sigs,sigs2}
MatrixPlot@Outer[Exp[-(#-#2)^2]&,sigs,sigs2,1]


c=All;
(*r=EvalPFDual[Norm/@mags[[;;c]],xys,Norm/@mags2[[;;c]],xys2];*)
(*r=EvalPF3[mags[[;;c]],xys,mags2[[;;c]],xys2];*)
r=EvalPF2[mags[[;;c]],xys,mags2[[;;c]],xys2];
MatrixPlot@r[[2]]
ListLinePlot/@{r[[1]],{xys,xys2}}
pos=xys[[r[[1]]]];
Graphics@Riffle[Point/@pos,ColorData["Rainbow"]/@(1/Length@pos (Range@Length@pos))]


Export["t.png",Graphics@Riffle[Point/@pos,ColorData["Rainbow"]/@(1/Length@pos (Range@Length@pos))]]


c=All;
(*r=EvalPFDual[Norm/@mags[[;;c]],xys,Norm/@mags2[[;;c]],xys2];*)
r=EvalPF3[mags[[;;c]],xys,mags2[[;;c]],xys2];
(*r=EvalPF2[sigs[[;;c]],xys,sigs2[[;;c]],xys2];*)
MatrixPlot@Transpose@r[[2]]
ListLinePlot/@{r[[1]],{xys,xys2}}
pos=xys[[r[[1]]]];
Graphics@Riffle[Point/@pos,ColorData["Rainbow"]/@(1/Length@pos (Range@Length@pos))]
r2=r;


(*With[{pos=MedianFilter[pos,{10,0}]},
	Graphics@Riffle[Point/@pos,ColorData["Rainbow"]/@(1/Length@pos (Range@Length@pos))]]*)


a=sigs[[;;Floor[Length@xys/2]]];
b=Reverse@sigs[[Floor[Length@xys/2];;]];
c=sigs2[[;;Floor[Length@xys2/2]]];
d=Reverse@sigs2[[Floor[Length@xys2/2];;]];
Export["t.png",GraphicsGrid[Partition[{ListLinePlot[{a,b},PlotLabel->"a,b"],ListLinePlot[{c,d},PlotLabel->"c,d"],
	ListLinePlot[{a,c},PlotLabel->"a,c"],ListLinePlot[{b,d},PlotLabel->"b,d"]},2],ImageSize->800]]
Needs["PlotLegends`"]
Export["t.png",ListLinePlot[Join@@{{xys[[;;Floor[Length@xys/2]]],Reverse@xys[[Floor[Length@xys/2];;]]},{xys2[[;;Floor[Length@xys2/2]]],Reverse@xys2[[Floor[Length@xys2/2];;]]}},PlotLegend->{"a","b","c","d"}, LegendPosition -> {1.1, -0.4}],ImageSize->800]


Export["t.png",#]&@GraphicsRow@{ListPlot[xys,PlotLabel->"INS"],ListPlot[xys[[Floor/@r[[1]]]],PlotLabel->"Mag-Sig"]}


EvalCorrelationPF=Function[fname,
	Module[{sigs,xys,sigs2,xys2,mx,numP,sigmaZ,sigsf,proc,r,randomPick,res,r2,r3,f},
{{sigs,xys},{sigs2,xys2}}=BuildMagModel/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt",fname};
(*{sigs,sigs2}=Take[#,100]&/@{sigs,sigs2};*)
{sigs,sigs2}=Partition[#,5,1]&/@{sigs,sigs2};
f={#&,GaussianFilter[#,10]&,Differences@GaussianFilter[#,10]&}[[1]];
mx=Length@sigs;
numP=500;
sigmaZ=1;
sigsf=Interpolation[MapIndexed[{#2[[1]],#}&,f@sigs],InterpolationOrder->1];
randomPick=Function[n,Transpose@{RandomVariate[UniformDistribution[{1,mx}],n],Table[1./numP,{n}]}];
proc=Function[{state,obs},
	Module[{state2,tw},
	state2={#[[1]]+RandomVariate[NormalDistribution[1,1]],#[[2]] Abs@Correlation[obs,sigsf[#[[1]]]]}&/@state;
	(*state2=Transpose@{state[[All,1]]+RandomVariate[NormalDistribution[1,1],Length@state],
		state[[All,2]] Abs[FastMapCorrelation[obs,sigsf[#[[1]]]&/@state]]};*)
	tw=Tr[Last/@state2];
	state2={#[[1]],#[[2]]/tw}&/@state2;
	If[Norm[Last/@state2]^(-2)<numP/2,
		(*Join[*){#,1./numP}&/@RandomChoice[Rule[Last/@state2,First/@state2],numP](*,randomPick[numP/10]]*)
		,state2]
	]];
res=FoldList[proc,randomPick@numP,f@sigs2];
(*r=SortBy[#,Last][[-1]]&/@res;*)
r2=Mean[(#[[2]] xys[[Clip[Round@#[[1]],{1,Length@xys}]]])&/@#]&/@res;
r3=Tr[(#[[2]] #[[1]])&/@#]&/@res;
{fname,(*ListLinePlot[First/@r],*)BlendPlot/@{r3,r2,(*xys[[Clip[(Round@First@#)&/@r,{1,Length@xys}]]],*)xys2}(*,ListLinePlot[Last/@r]*)}]];


EvalCorrelationPF/@{"BEJ_KEJ_5_georgezhou_Jul6_2.txt","BEJ_KEJ_5_georgezhou_Jul5_8.txt"}


r=EvalCorrelationPF/@{"BEJ_KEJ_5_georgezhou_Jul6_3.txt","BEJ_KEJ_5_georgezhou_Jul26_1_2.txt"}
(*/@Complement[
		FileNames["BEJ_KEJ_5_georgezhou_Jul*"],FileNames["BEJ_KEJ_5_georgezhou_Jul*cyclic*"]~Join~FileNames["BEJ_KEJ_5_georgezhou_Jul*short*"]]]*)


r=EvalCorrelationPF/@{"BEJ_KEJ_5_georgezhou_Jul5_survey_3_1.txt","BEJ_KEJ_5_georgezhou_Jul5_survey_3_2.txt"}


{{sigs,xys},{sigs2,xys2}}=BuildMagModel/@{"BEJ_KEJ_5_georgezhou_Jul26_1.txt",(*"BEJ_KEJ_5_georgezhou_Jul26_1_2.txt"*)"BEJ_KEJ_5_georgezhou_Jul6_3.txt"};


ListLinePlot@{xys,xys2}


n=50;ssig=sigs[[20;;20+n-1]];
ListLinePlot[ListCorrelate[Standardize@ssig,sigs2]/n/Sqrt[(MovingAverage[sigs2^2,n]-MovingAverage[sigs2,n]^2)]]


ListLinePlot[{sigs,sigs2},PlotRange->All]
Image[#,ImageSize->800]&@ListLinePlot[Differences@GaussianFilter[#,5]&/@{sigs,sigs2}]


((Image[#,ImageSize->500])&@ListLinePlot@#)&/@(Differences@GaussianFilter[#,10]&/@{sigs,sigs2})


Export["t.png", #] &@r


c=100;ListLinePlot[{#-GaussianFilter[#,c]&@sigs,#-GaussianFilter[#,c]&@sigs2},PlotRange->All]


c=100;r=MatrixPlot@Outer[Exp[-(#-#2)^2]&,Drop[#,c-1]-MovingAverage[#,c]&@sigs,Drop[#,c-1]-MovingAverage[#,c]&@sigs2,1]


c=100;r=MatrixPlot@Outer[Exp[-(#-#2)^2]&,#-GaussianFilter[#,c]&@sigs,#-GaussianFilter[#,c]&@sigs2,1]


r=MatrixPlot@Outer[Exp[-(#-#2)^2]&,sigs,sigs2,1]


MatrixPlot@Outer[Correlation,Partition[sigs,5,2],Partition[sigs2,5,2],1]


MatrixPlot@Outer[CosineDistance[#,#2]&,Partition[sigs,30,10],Partition[sigs2,30,10],1]


r={Labeled[Image[#,ImageSize->300]&@MatrixPlot@Outer[Correlation,Partition[sigs,80,10],Partition[sigs2,80,10],1],"Window=80"],
	Labeled[Image[#,ImageSize->300]&@MatrixPlot@Outer[Correlation,Partition[sigs,50,10],Partition[sigs2,50,10],1],"Window=50"],
	Labeled[Image[#,ImageSize->300]&@MatrixPlot@Outer[Correlation,Partition[sigs,20,10],Partition[sigs2,20,10],1],"Window=20"]}


Image[#,ImageSize->300]&/@{MatrixPlot@Outer[Correlation,Partition[sigs,30,10],Partition[sigs2,30,10],1],MatrixPlot@Outer[Correlation,Partition[sigs,20,10],Partition[sigs2,20,10],1]}


Image[#,ImageSize->300]&/@{MatrixPlot@Outer[CosineDistance,Partition[sigs,80,10],Partition[sigs2,80,10],1],MatrixPlot@Outer[CosineDistance,Partition[sigs,50,10],Partition[sigs2,50,10],1]}


GraphicsRow[Image[#,ImageSize->800]&/@{ListPlot@l,ListLinePlot[{sig[[First/@l]],sig2[[Last/@l]]},PlotRange->All]},ImageSize->1000]


GraphicsRow[Image[#,ImageSize->500]&/@{Show[ListPlot[First/@Partition[xys,10]],ListLinePlot[xys[[#]]&/@Select[Map[First,SplitBy[l,Last],{2}],Length@#>30&]]],
	Show[ListPlot[First/@Partition[xys2,10]],ListLinePlot[xys2[[#]]&/@Select[Map[Last,SplitBy[l,First],{2}],Length@#>30&]]]},ImageSize->1000]


Graphics[BlendPlot[xys,PlotLabel->"BEJ_KEJ_5_georgezhou_Jul26_1.txt"]]


Export["t.png", #] &@GraphicsRow[{Graphics[ListPlot[xys,PlotLabel->"BEJ_KEJ_5_georgezhou_Jul26_1.txt"]],
	Graphics[ListPlot[xys2,PlotLabel->"BEJ_KEJ_5_georgezhou_Jul6_3.txt"]]},ImageSize->1000]


Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{xys,Norm/@MapThread[Projection,{mags,axyzs}]}])]
Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{xys2,Norm/@MapThread[Projection,{mags2,axyzs2}]}])]


Image[#,ImageSize->Large]&/@
(*Image[#,ImageSize->1000]&@Show@*)(Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{#[[2]],Norm/@#[[1]]}])]&/@(BuildMagMarkerModel2/@FileNames["ABQ3_*.txt"]))


Image[#,ImageSize->Large]&/@
(*Image[#,ImageSize->1000]&@Show@*)(Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{#[[2]],Norm/@#[[1]]}])]&/@(BuildMagMarkerModel2/@FileNames["ABQ2_*.txt"]))


Image[#,ImageSize->Large]&/@
(*Image[#,ImageSize->1000]&@Show@*)(Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{#[[2]],Norm/@#[[1]]}])]&/@(BuildMagModel2/@FileNames["BEJ_KEJ_5_georgezhou_Jul*.txt"]))


Image[#,ImageSize->1000]&/@
(*Image[#,ImageSize->1000]&@Show@*){Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{xys,Norm/@mags}])],
	Graphics[Join@@({Hue[(#[[3]]-20)/100],Point[#[[1;;2]]]}&/@MapThread[Append,{xys2,Norm/@mags2}])]}


{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildMagMarkerModel2[#]&/@{modelfname,fname};//AbsoluteTiming
Dimensions/@{mags,mags2}


ListPlot[Transpose[axyzs[[;;;;5]]]]
ListPlot@Transpose[axyzs2[[;;;;5]]]


ListPlot@Transpose[MapThread[Projection,{mags,axyzs}][[;;;;5]]]
ListPlot@Transpose[MapThread[Projection,{mags2,axyzs2}][[;;;;5]]]


{{mags,xys,axyzs},{mags2,xys2,axyzs2}}=BuildMagModel2[#]&/@{modelfname,fname};//AbsoluteTiming
Dimensions/@{mags,mags2}


ListPlot@Transpose@MapThread[Projection,{mags,axyzs}]
ListPlot@Transpose@MapThread[Projection,{mags2,axyzs2}]


ListPlot@Transpose@axyzs
ListPlot@Transpose@axyzs2


MatrixPlot@Outer[Exp[-Norm[#-#2]]&,Accumulate@(#-GaussianFilter[#,100])&@Transpose[mags][[2]],Accumulate@(#-GaussianFilter[#,100])&@Transpose[mags2][[2]],1]


ListPlot@{Accumulate@(#-GaussianFilter[#,100])&@Transpose[mags][[2]],Accumulate@(#-GaussianFilter[#,100])&@Transpose[mags2][[2]]}


ListPlot@Transpose@mags
ListPlot@Transpose@mags2


Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[#[[1;;2]].#2[[;;2]]]]&,Standardize[mags[[;;;;2]],Median,1&],Standardize[mags2[[;;;;2]],Median,1&],1]


Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[#.#2]]&,Standardize[mags[[;;;;2]],Median,1&],Standardize[mags2[[;;;;2]],Median,1&],1]


Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[Norm@#-Norm@#2]^2.]&,Standardize[mags[[;;;;2]],Median,1&],Standardize[mags2[[;;;;2]],Median,1&],1]


Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[#-#2]]&,Standardize[mags[[;;;;2]],Median,1&],Standardize[mags2[[;;;;2]],Median,1&],1]


ImageConvolve[#,{{1,0,0},{0,0,0},{0,0,1}}]&@
(*ImageConvolve[#,{{1,0,0},{0,0,0},{0,0,1}}]&@
ImageConvolve[#,{{1,1,1},{0,0,0},{-1,-1,-1}}]&@
ImageConvolve[#,{{1,0,-1},{1,0,-1},{1,0,-1}}]&@
*)	(*ImageConvolve[#,{{0,0,-1},{0,0,0},{1,0,0}}]&@*)
		r//ImageAdjust


r=MatrixPlot[Outer[Exp[-Norm[#-#2]]^2.&,mags[[;;;;5]],mags2[[;;;;5]],1],Frame->False]


factorize=Function[{vec,grav},
	Module[{g=Projection[vec,grav],x},
	x=Projection[vec,Cross[{0,1,0},grav]];
	Norm/@{x,vec-g-x,g}
	]];


r=MatrixPlot[Outer[Exp[-Norm[#-#2]]^2.&,
	MapThread[factorize,{mags[[;;;;5]],axyzs[[;;;;5]]}],MapThread[factorize,{mags2[[;;;;5]],axyzs2[[;;;;5]]}],1],Frame->False]


r=MatrixPlot[Outer[Exp[-Norm[#-#2]]^2.&,
	Norm/@MapThread[#-Projection[#,#2]&,{mags[[;;;;5]],axyzs[[;;;;5]]}],Norm/@MapThread[#-Projection[#,#2]&,{mags2[[;;;;5]],axyzs2[[;;;;5]]}],1],Frame->False]


r=MatrixPlot[Outer[Exp[-Norm[#-#2]]^2.&,
	Norm/@MapThread[Projection,{mags[[;;;;5]],axyzs[[;;;;5]]}],Norm/@MapThread[Projection,{mags2[[;;;;5]],axyzs2[[;;;;5]]}],1],Frame->False]


r=MatrixPlot[Outer[Exp[-Norm[#-#2]]^2.&,mags[[;;;;5]],mags2[[;;;;5]],1],Frame->False]


r3=EvalPF3[mags[[;;;;5]],xys[[;;;;5]],mags2[[;;;;5]],xys2[[;;;;5]]];


MatrixPlot@Transpose@r3[[2]]


r=Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[#-#2]]^2.&,mags[[;;;;5]],mags2[[;;;;5]],1]


r=Image[#,ImageSize->Large]&@MatrixPlot@Outer[Exp[-Norm[#-#2]]^2.&,Standardize[mags[[;;;;5]],Mean,1&],Standardize[mags2[[;;;;5]],Mean,1&],1]


l=Import["~/20news-bydate/matlab/test.data","Table"];
d=Dispatch@MapIndexed[First@#2->First@#&,Import["~/20news-bydate/matlab/test.label","Table"]];


commons=Select[Tally[#[[2]]&/@l],#[[2]]>1000&];
words=Dispatch[First@#->0&/@commons];
l2=Select[l,(#[[2]]/.words)==0&];
Length@l2
wordsNum=Dispatch[Thread[#->Range@Length@#]&@commons];


ar=SparseArray[{#[[1]]/.d,#[[2]]/.wordsNum}->#[[3]]&/@l2]
ar2=N@Normal@ar;//AbsoluteTiming
Dimensions@ar2


{L,S}=Rpca[ar2,0.08,100];//AbsoluteTiming


MatrixPlot@ar2(*//ImageAdjust*)
MatrixPlot@L(*//ImageAdjust*)
MatrixPlot@S
MatrixRank@L
MatrixPlot[#.Transpose[#]]&@(#/Tr[#]&/@S)


l=Import/@FileNames["~/pic/sequence/t*.png"][[;;40]];
l2=First@ColorSeparate@#&/@l
{L,S}=Rpca[Flatten@ImageData@#&/@l2[[;;20]],1./200,100];//AbsoluteTiming


ImageAdjust@ImageDifference[l2[[1]],#]&/@l2
ImageAdjust@Image@Partition[#,320]&/@S


ImageAdjust@Image@Partition[Orthogonalize[Append[L,Flatten@ImageData@#]][[-1]],320]&/@l2


fname="BEJ_KEJ_5_georgezhou_Jul26_1.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul6_2.txt";
{{mags,xys,axyzs,ts},{mags2,xys2,axyzs2,ts2}}=BuildMagModel2[#]&/@{fname,modelfname};//AbsoluteTiming
vec=FactorizeByGravityYPointingDirs[mags, axyzs, 10];vec2=FactorizeByGravityYPointingDirs[mags2, axyzs2, 10];
(*ListPlot@Transpose@vec
ListPlot@Transpose@vec2*)
mxyf=LoadXYf[fname,""];mxyf2=LoadXYf[modelfname,""];
vecf=Interpolation[MapThread[List,{ts,vec}],InterpolationOrder->1];
vecf2=Interpolation[MapThread[List,{ts2,vec2}],InterpolationOrder->1];


fname="ABQ3_1.txt";modelfname="ABQ3_6.txt";
fname="~/zsc/LAS_terminal3_1_ereanos_2.txt";modelfname="~/zsc/LAS_terminal3_1_willbaker_2.txt";
{{mags,xys,axyzs,ts},{mags2,xys2,axyzs2,ts2}}=BuildMagMarkerModel2[#]&/@{fname,modelfname};//AbsoluteTiming
vec=FactorizeByGravityYPointingDirs[mags, axyzs, 10];vec2=FactorizeByGravityYPointingDirs[mags2, axyzs2, 10];
mxyf=LoadMarkerXYf[fname];mxyf2=LoadMarkerXYf[modelfname];
vecf=Interpolation[MapThread[List,{ts,vec}],InterpolationOrder->1];
vecf2=Interpolation[MapThread[List,{ts2,vec2}],InterpolationOrder->1];


fname="BEJ_KEJ_5_georgezhou_Jul26_1.txt";modelfname="BEJ_KEJ_5_georgezhou_Jul6_2.txt";
{{mags,xys,axyzs,ts},{mags2,xys2,axyzs2,ts2}}=BuildMagModel2[#]&/@{fname,modelfname};//AbsoluteTiming
{mxyf,mxyf2}=LoadXYf[#,""]&/@{fname,modelfname};
vec=FactorizeByGravityYPointingDirs[mags, axyzs, 10];vec2=FactorizeByGravityYPointingDirs[mags2, axyzs2, 10];
vecf=Interpolation[MapThread[List,{ts,vec}],InterpolationOrder->1];
vecf2=Interpolation[MapThread[List,{ts2,vec2}],InterpolationOrder->1];


Graphics[Line/@Partition[xys,150],Axes->True]
(*Graphics[Line[mxyf/@#]&/@timestamps[[All,1]],Axes->True]*)


matched=Select[Import["/tmp/matched.csv"(*"/tmp/matched_ABQ3_1_7.csv"*)],#[[3]]!=-1&];
timestamps={Select[ts,Function[x,#[[3]]<x 10^9<#[[4]]]],Select[ts2,Function[x,#[[1]]<x 10^9<#[[2]]]]}&/@matched;
MapThread[List,{MapThread[List,{Graphics[{Point/@#[[1]],Red,Point/@#[[2]]},Axes->True]&/@({mxyf/@#[[1]],mxyf2/@#[[2]]}&/@timestamps),
	(ListPlot@Transpose@#&/@{vecf/@#[[1]],vecf2/@#[[2]]})&/@timestamps}],matched[[All,5;;]]}]


L1MatchMagSig2D=Function[{xs,ys},
	With[{\[Lambda]1=50,\[Lambda]2=0.5,a0=1,b0={0,0},k=Length@xs},Module[{a,b1,b2},
		FindMinimum[Mean[Norm/@(#-{b1,b2}&/@(ys-a xs))]+\[Lambda]1((a-a0)^2)(*-Log[a]*)+\[Lambda]2 Norm[{b1,b2}-b0]^2,{a,b1,b2}]]]];
rxs=mxyf[#[[3]]10^-9]&/@matched;rys=mxyf2[#[[1]]10^-9]&/@matched;
xs=MedianFilter[rxs,{3,0}];ys=rys;
r=L1MatchMagSig2D[xs,ys]
{ListLinePlot@{xys,xys2},ListLinePlot@{#+r[[2,2;;,2]]&/@(r[[2,1,2]] xys),xys2}}


thetas=MapThread[Arg[(#2[[1]]+#2[[2]]I)/(#[[1]]+#[[2]]I)]&,{Most@Differences@xys,Rest@Differences@xys}];
thetas2=GaussianFilter[thetas,20];
{ListLinePlot[thetas2,PlotRange->All],ListPlot@Accumulate[0.1{Cos@#,Sin@#}&/@Accumulate@thetas2]}
points=First/@Select[MapIndexed[Prepend[#,First@#2]&,Partition[thetas2,3,1]],#[[3]]>#[[2]]&&#[[3]]>#[[4]]&]-1
Graphics@{Red,Point/@xys[[points]],Opacity[0.05],Black,Point/@xys}


n=Length@xys;
A1=Table[Boole[i>=j],{i,n},{j,n}];
A=N[A1.A1];
Export["xs.csv",xys[[All,1]]];
Export["ys.csv",xys[[All,2]]];
Export["a.csv",A];


localMaxima=Function[thetas2,First/@Select[MapIndexed[Prepend[#,First@#2]&,Partition[thetas2,3,1]],#[[3]]>#[[2]]&&#[[3]]>#[[4]](*&&#[[3]]>0.001*)&]];


l=1-Abs@Correlation[#[[;;,1]],#[[;;,2]]]&/@Partition[xys,15,2];
ListPlot[l,PlotRange->All]
points=2First/@Select[MapIndexed[{First@#2,#}&,l],#[[2]]>0.1&];
Graphics@{Red,Point/@xys[[points]],Opacity[0.05],Black,Point/@xys}


Graphics@{Red,Point/@xys[[Join[localMaxima@xsp,localMaxima@ysp]]],Opacity[0.05],Black,Point/@xys}


l=Partition[xys,50];ListPlot@l


deltas=Last@#-First@#&/@l;


ListLinePlot[Accumulate@deltas]


xsp=First/@Import["xsp.csv"];
ysp=First/@Import["ysp.csv"];
ListPlot@Accumulate@Accumulate@Transpose[{xsp,ysp}]


matched[[;;,1]]


idx=Nearest[ts->Automatic,# 10^-9][[1]]&/@matched[[;;,3]]
idx2=Nearest[ts2->Automatic,# 10^-9][[1]]&/@matched[[;;,1]]
ListLinePlot@{xys,xys2,xys[[idx]],xys2[[idx2]]}


r=FindGeometricTransform[xys[[idx]],xys2[[idx2]],"Transformation"->"Rigid"]
ListLinePlot@{xys[[idx]],r[[2]]/@xys2[[idx2]]}
ListPlot@{xys,r[[2]]/@xys2}


matched=Select[Import["matched_KEJ_5_Jul26_1_Jul6_2.csv"(*"/tmp/matched_ABQ3_1_7.csv"*)],#[[3]]!=-1&&#[[5]]<40&];
timestamps={Select[ts,Function[x,#[[3]]<x 10^9<#[[4]]]],Select[ts2,Function[x,#[[1]]<x 10^9<#[[2]]]]}&/@matched;
MapThread[List,{MapThread[List,{Graphics[{Point/@#[[1]],Red,Point/@#[[2]],Black,Opacity[0.05],Point/@xys,Point/@xys2},Axes->True]&/@({mxyf/@#[[1]],mxyf2/@#[[2]]}&/@timestamps),
	(ListPlot@Transpose@#&/@{vecf/@#[[1]],vecf2/@#[[2]]})&/@timestamps}],matched[[All,5;;]]}]


fname="ABQ3_1.txt";modelfname="ABQ3_2.txt";
{{mags,xys,axyzs,ts},{mags2,xys2,axyzs2,ts2}}=BuildMagModel2[#]&/@{fname,modelfname};//AbsoluteTiming
{qf,qf2}=LoadQuaternionF/@{fname,modelfname};//AbsoluteTiming
{qs,qs2}={qf/@ts,qf2/@ts2};


{qmags,qmags2}=MapThread[RotateByQuaternion,#]&/@{{mags,qs},{mags2,qs2}};//AbsoluteTiming
{qaxyzs,qaxyzs2}=MapThread[RotateByQuaternion,#]&/@{{axyzs,qs},{axyzs2,qs2}};//AbsoluteTiming
{qqmags,qqmags2}=MapThread[RotateByQuaternion[#,QuaternionConjugate[#2]]&,#]&/@{{Standardize[qmags,Mean,1&],qs},{Standardize[qmags2,Mean,1&],qs2}};//AbsoluteTiming


ListPlot@Transpose@#&/@{qaxyzs,qaxyzs2}
ListPlot@Transpose@#&/@{mags,mags2}
ListPlot@Transpose@#&/@{qmags,qmags2}
ListPlot@Transpose@#&/@{qqmags,qqmags2}


findAllMatch=Function[{span,needle,hay,\[Lambda]},
	Module[{orderFn=CompensatedSquareError (*CompensatedOrderingError*)(*CompensatedAbsoluteError*)},
	With[{span2=Span[#[[1]],#[[1]]+Abs[span[[2]]-span[[1]]]]},
		{span2,#[[2]]}]&/@
			MapIndexed[{First@#2,#}&,orderFn[needle[[span]],hay,\[Lambda]]]]];
testMatchSeq=Function[{sigs,sigs2,mxyf,zts,mxyf2,zts2,hitBonus,hitRange,stride,\[Lambda]},
	Module[{l2},
	l2=Rest@FoldList[
	Function[{lastSpanCost,range},
		Module[{span2=Span[First@range,Last@range],lastSpan=lastSpanCost[[1]]},
		SortBy[{#[[1]],span2,#[[2]]+If[lastSpan=!=Span[0,0]&&Abs[#[[1,1]]-lastSpan[[1]]]<hitRange,hitBonus,0]}&/@
				findAllMatch[span2,sigs2,sigs,\[Lambda]],Last][[1]]
	]],{Span[0,0],0},Partition[Range@Length@sigs2,stride]];
	Module[{span=#[[1]],span2=#[[2]],cost=#[[3]]},{{LatLongDistance[First@#,Last@#]&@(mxyf/@zts[[span]]),
				Mean[MapThread[LatLongDistance,{mxyf/@zts[[span]],mxyf2/@zts2[[span2]]}]],cost},
			ListPlot@{Norm/@sigs[[span]],Norm/@sigs2[[span2]]},ListPlot/@Transpose[{Transpose@sigs[[span]],Transpose@sigs2[[span2]]}],
			Graphics[{Point/@(Reverse/@(mxyf/@zts[[span]])),Red,Point/@(Reverse/@(mxyf2/@zts2[[span2]]))}]}]&/@l2
	]];
findMatch=Function[{span,needle,hay,\[Lambda]},
	Module[{orderFn=(*CompensatedCorrelationError*)CompensatedSquareError(*CompensatedOrderingError*)(*CompensatedAbsoluteError*)(*CompensatedQuadError*)},
	With[{span2=Span[#,#+Abs[span[[2]]-span[[1]]]]},
		{span2,orderFn[needle[[span]],hay[[span2]],\[Lambda]][[1]]}]&@
			Ordering[orderFn[needle[[span]],hay,\[Lambda]]][[1]]]];
testMatch=Function[{sigs,sigs2,mxyf,zts,mxyf2,zts2,stride,\[Lambda]},
	Module[{span,span2=Span[First@#,Last@#],cost
			(*,rspan2=Span[Last@#,First@#,-1],rspan,isReverse,rcost,rsigs2={-#[[1]],-#[[2]],#[[3]]}&/@sigs2*)},
		{span,cost}=findMatch[span2,sigs2,sigs,\[Lambda]];
		(*{span,cost}=findMatch[span2,{Norm@#[[;;2]],#[[3]]}&/@sigs2,{Norm@#[[;;2]],#[[3]]}&/@sigs,\[Lambda]];*)
		(*{rspan,rcost}=findMatch[rspan2,rsigs2,sigs,\[Lambda]];isReverse=rcost<cost;If[isReverse,{span,cost}={rspan,rcost}];*)
		{{LatLongDistance[First@#,Last@#]&@(mxyf/@zts[[span]]),
				Mean[MapThread[LatLongDistance,{mxyf/@zts[[span]],mxyf2/@zts2[[span2]]}]],cost},
			ListPlot@{Norm/@sigs[[span]],Norm/@sigs2[[span2]]},ListPlot/@Transpose[{Transpose@sigs[[span]],Transpose@sigs2[[span2]]}],
			Graphics[{(*Opacity[0.2],*)Point/@(Reverse/@(mxyf/@zts[[span]])),Red,Point/@(Reverse/@(mxyf2/@zts2[[span2]]))},Axes->True]}
	]&/@Partition[Range@Length@sigs2,stride(*,Floor[stride/4]*)]];
testMatchSpanCost=Function[{sigs,sigs2,stride,\[Lambda]},
	Module[{span,span2=Span[First@#,Last@#],cost
			(*rspan2=Span[Last@#,First@#,-1],rspan,isReverse,rcost,rsigs2={-#[[1]],-#[[2]],#[[3]]}&/@sigs2*)},
		{span,cost}=findMatch[span2,sigs2,sigs,\[Lambda]]
	]&/@Partition[Range@Length@sigs2,stride(*,Floor[stride/4]*)]];
lessThanPortion=Function[{r,th},N@Length@Select[r[[All,1,2]],#<th&]/Length@r[[All,1,2]]];
testFileSingleModel=Function[{modelfname,fname,w,\[Lambda]},
	Module[{zmags,zxys,zaxyzs,zts,mxyf,mxyf2,zmags2,zxys2,zaxyzs2,zts2,sigs,sigs2,f,r},
		{{zmags,zxys,zaxyzs,zts,mxyf},{zmags2,zxys2,zaxyzs2,zts2,mxyf2}}=LoadMagModel2[#]&/@{modelfname,fname};
		sigs=FactorizeByGravityYPointingDirs[zmags,zaxyzs,10];
		sigs2=FactorizeByGravityYPointingDirs[zmags2,zaxyzs2,10];
		r=testMatch[sigs,sigs2,mxyf,zts,mxyf2,zts2,w,\[Lambda]];
		f=lessThanPortion[r,#]&/@{5,10,20};
		{f,r}]];
testFileMultiModel=Function[{fname,modelfnames,w,\[Lambda]},
	Module[{r=SortBy[#,Function[x,x[[1,3]]]][[1]]&/@Transpose[testFileSingleModel[#,fname,w,\[Lambda]][[2]]&/@modelfnames]},
	r]];
precisionRecallMatrix=Function[{th,r4},
	Module[{all={#}&/@Range@Length@r4,
		p=Position[r4,x_/;ListQ[x]&&x[[1,3]]<th,1],
		gp=Position[r4,x_/;ListQ[x]&&x[[1,2]]<5,1]},
			Length/@{Intersection[p,gp],Intersection[p,Complement[all,gp]],
				Intersection[Complement[all,p],gp],Intersection[Complement[all,p],Complement[all,gp]]}]];
(*r=testFileMultiModel[fname,modelfnames,150,0.];//AbsoluteTiming
{#,calcPrecisionRecall@#}&@precisionRecallMatrix[r,100]
lessThanPortion[r,#]&/@{5,10,20}
ListPlot[r[[All,1,2;;]],PlotRange->All]*)
