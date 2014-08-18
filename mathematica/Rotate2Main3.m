(* ::Package:: *)

<< "~/git_math/google3/experimental/users/georgezhou/mathematica/Sensor2.m"
$HistoryLength=1;
lns=Import@"/home/georgezhou/model_decay/levelNames.csv";nameMapping=Dispatch[Rule@@@lns];
lts=Import@"model_decay/level-timestamps.csv";
ageMapping=With[{r=Parallelize[
	With[{now=DateString[]},{#[[1]],DateDifference[NanoToDateList@Max[1000000 Rest@#],now]}]&/@ (Select[lts,Length@#>1&]),DistributedContexts->All]},
		Dispatch[Rule@@@(r/.Dispatch[Rule@@@lns])]];
AggregateWifis=Function[{wifis,k},{Mean[First/@#],{#[[1,1]],Mean[Last/@#]}&/@GatherBy[Join@@(Last/@#),First]}&/@Partition[wifis,k]];


(*lts=Select[Import@"t.csv",StringQ@#[[1]]&&NumberQ@#[[2]]&&NumberQ@#[[3]]&];
l={#[[1,1]]/.nameMapping,DateDifference[NanoToDateList[10^6 Max[#[[;;,2]]]],now]}&/@GatherBy[Select[Import["t.csv"],#[[3]]==" APPROVED"&],First];
Histogram@l[[;;,2]]
l2={#[[1]]/.nameMapping,1-N@#[[3]]/#[[2]]}&/@glsApStats;
Export["t.png",ListPlot[Select[JoinTablesByKey[{l,l2}],StringQ@#[[1]]&&NumberQ@#[[2]]&&NumberQ@#[[3]]&][[;;,2;;]],PlotRange->All]]*)


physical=Import@"model_decay/level-mac-stat-Jan-28.csv";


Export["t.csv",{#[[1]]/.nameMapping,"http://insight-ops.sandbox.google.com/insight/admin/level?k="<>#[[1]],#[[2]],#[[3]],1-N@#[[3]]/#[[2]],#[[4]],#[[5]]}&/@physical]


1-N@#[[3]]/#[[2]]&/@physical


(({Union@#[[;;,2]],Length@#}&/@GatherBy[Import@"/tmp/coverage.csv",First])/.nameMapping)[[;;30]]//TableForm


(*Export["model_decay/taggedLevels.csv",Union[StringTake[#,StringLength@"0x80dd2bd8ca1447dd:0xdb3e02c083e472bd"]&/@Flatten@Import@"/home/georgezhou/model_decay/taggedSurveys.csv"]]*)
tagged=Import@"model_decay/tagged-levels-survey-stats-broken_models_2012_12.csv";
untagged=Import@"model_decay/untagged-levels-survey-stats-broken_models_2012_12.csv";
edgeStats=Import["~/model_decay/edges-survey-stats-broken_models_2012_12.csv"];
glsApStats=Import@"~/model_decay/3-physical-level-mac-stats.csv";
apStats=Import@"~/model_decay/2-survey-stats-broken_models_2012_12.csv";
levellatlngs=Import@"model_decay/level_latlng.csv";
sensorCollects=Import@"model_decay/sensorcollects-level-coverage.csv";


sensorCollectsD=Dispatch[Rule@@@sensorCollects];
Export["t.csv",Prepend[Select[JoinTablesByKey[{{#[[1]]/.nameMapping,"http://insight-ops.sandbox.google.com/insight/admin/level?k="<>#[[1]],1-N@#[[3]]/#[[2]],#[[1]]/.sensorCollectsD,chopLevel[#[[1]]/.nameMapping]/.ncoverage}&/@glsApStats,
	levellatlngs},True],NumberQ[#[[4]]]&&NumberQ[#[[5]]]&],StringSplit@"level link missing_ratio coverage max_coverage lat lng"]]


Export["t.csv",Prepend[Select[JoinTablesByKey[{{#[[1]]/.nameMapping,"http://insight-ops.sandbox.google.com/insight/admin/level?k="<>#[[1]],1-N@#[[3]]/#[[2]]}&/@glsApStats,
	levellatlngs}],NumberQ@#[[-1]]&&NumberQ@#[[3]]&],StringSplit@"level link missingratio lat lng"]]


l=Mean/@Transpose@#&/@GatherBy[{#[[1]],1-N@#[[3]]/#[[2]]}&/@Select[glsApStats/.nameMapping/.ageMapping,NumberQ@#[[1]]&],Floor[#[[1]]/10]&]
Export["t.png",ListPlot[l,PlotRange->All,ImageSize->400,AxesLabel->{"averageSurveyAge","missingRatio"}]]


Export["t.png",ListPlot[Import["t.csv","Table"],PlotRange->All,ImageSize->500,AxesLabel->{"edgeMissingRatio","deltaMeanError"}]]


l={#[[5]],#[[6]],1-N@#[[3]]/#[[2]],#[[1]]}&/@(Join@@@Select[GatherBy[Join[glsApStats/.nameMapping,levellatlngs],First],Length@#>1&]);
Export["t.png",Graphics[Join@@{Flatten[{Blend[{Green,Red},#[[3]]],Point@#[[{2,1}]]}&/@l],{Black,Opacity[0.1]},Map[Polygon[Reverse/@#]&,coords,{2}]},ImageSize->1000]]


Export["t.csv",Prepend[{#[[1]],#[[2]],#[[3]],1-N@#[[3]]/#[[2]]}&/@glsApStats,StringSplit@"level	#physical_ap	#remaining_physical_ap	physical_ap_missing_ratio"]]


Export["t.csv",Transpose[{Most@#[[1]],Accumulate@#[[2]]/N@Total@#[[2]]}&[HistogramList[1-N@#[[3]]/#[[2]]&/@glsApStats,{0.01}]]]]


Export["t.csv",Select[glsApStats/.nameMapping,MemberQ[l,#[[1]]]&]]


GatherBy[Join[List/@l,edgeStats/.nameMapping],First]


Rest/@(Join@@@Select[GatherBy[Join[List/@l,edgeStats/.nameMapping],First],Length@#>1&])//TableForm


Export["t.csv",edgeStats/.nameMapping]


r=SortBy[Flatten/@Select[GatherBy[Join@@(levelStats/@{tagged,untagged,edgeStats}),First],Length@#>2&]/.nameMapping,#[[2]]&]//TableForm


Export["t.png",Graphics[Flatten[{Blend[{Green,Red},#[[3]]/600],Point@#[[;;2]]}&/@({#[[2]],#[[3]],#[[1]]/.nameMapping/.ageMapping}&/@r2)],Axes->True,AxesLabel->{"OldMeanError","NewMeanError"},AxesOrigin->{0,0},ImageSize->500]]
Export["t2.png",ListPlot[{#[[1]]/.nameMapping/.ageMapping,#[[3]]-#[[2]]}&/@r2,AxesLabel->{"SurveyAge","DeltaMeanError"},AxesOrigin->{0,0},ImageSize->500]]


Export["t.png",Graphics[Flatten[{Blend[{Green,Red},#[[3]]],Point@#[[;;2]]}&/@r2[[;;,2;;4]]],Axes->True,AxesLabel->{"OldMeanError","NewMeanError"},AxesOrigin->{0,0},ImageSize->500]]
(*for edge missing ratio*)(*Export["t.png",Graphics[Flatten[{Blend[{Green,Red},#[[3]]],Point@#[[;;2]]}&/@r2[[;;,{2,3,5}]]],Axes->True,AxesLabel->{"OldMeanError","NewMeanError"},AxesOrigin->{0,0},ImageSize->500]]*)


Export["t2.png",ListPlot[Transpose@{r2[[;;,4]],r2[[;;,3]]-r2[[;;,2]]},AxesLabel->{"MissingRatio","DeltaMeanError"}]]


r3={Total@#[[3;;5]],Total@#[[8;;10]],(1-N@#[[17]]/#[[16]])}&/@Select[r,NumberQ@#[[3]]&];


r4=Select[{#[[1]],1-N@#[[3]]/#[[2]]}&/@(Select[glsApStats/.nameMapping,StringTake[#[[1]],3]=="JPN"&]/.nameMapping/.ageMapping),NumberQ@#[[1]]&&NumberQ@#[[2]]&];
ListPlot@r4
ListPlot[{Mean@#[[;;,1]],Mean@#[[;;,2]]}&/@GatherBy[r4,Floor[#[[1]],40]&]]


Graphics[Flatten[{Hue[0.7#[[3]]],Point@#[[;;2]]}&/@r3],Axes->True,AxesLabel->{"OldNullRatio","NewNullRatio"}]


Export["t.png",Plot[CDF[HistogramDistribution[1-N@#[[3]]/#[[2]]&/@glsApStats],x],{x,0,1},AxesOrigin->{0,0},AxesLabel->{"PhysicalMissingRatio","AccumulatedProbability"}]]


r=Flatten/@Select[GatherBy[Join@@{levelStats@untagged,levelStats@tagged,edgeStats,glsApStats,apStats},First],Length@#>4&];
approxMean=#[[1]](1-Total@#[[2;;4]])+50 Total@#[[2;;4]]&;
r2=Select[SortBy[Flatten@{#[[1]],approxMean[#[[2;;5]]],approxMean[#[[7;;10]]](*,Sqrt[1-N@#[[13]]/#[[12]]],Sqrt[1-N@#[[14]]/#[[12]]]*),1-N@#[[17]]/#[[16]],1-N@#[[20]]/#[[19]],1-N@#[[21]]/#[[19]]}&/@r,#[[3]]&],NumberQ@#[[2]]&];r2//MatrixForm


levelStats={#[[1,1]],Mean@#[[;;,2;;]]}&/@GatherBy[Prepend[#[[2;;]],StringTake[#[[1]],37]]&/@#,First]&;
r=Flatten/@Select[GatherBy[Join@@(levelStats/@{tagged,untagged}),First],Length@#>1&];
(*r//MatrixForm*)
(*(*Export["t.csv",*)Join@@{#[[;;3]],{#[[5]]-#[[10]]},#[[4;;]]}&[Join@@{#[[;;4]],{If[Total@#[[6;;8]]>0.7,50,#[[5]]]},#[[6;;]]}]&/@(Join@@@Select[GatherBy[Join[l3[[;;,{1,2,4}]],SortBy[Select[r,Total@#[[-3;;]]<0.3&],#[[2]]&]/.nameMapping],First],Length@#>1&])//MatrixForm*)


l=Flatten[Import@"~/model_decay/taggedSurveys.csv"];l2=Import@"~/model_decay/taggedsurveys-level-mac-stats.csv";l3=Rest@Import@"model_decay/to-resurvey.csv";l4=Import@"~/model_decay/survey-stats-broken_models_2012_12.csv";
r=Select[l2,MemberQ[StringTake[#,StringLength@"0x80dd2bd8ca1447dd:0xdb3e02c083e472bd"]&/@l,#[[1]]]&]/.nameMapping;
Export["t.csv",Join@@@Select[GatherBy[Join[l3[[;;,{1,2,4}]],SortBy[Select[r,Total@#[[-3;;]]<0.3&],#[[2]]&]/.nameMapping],First],Length@#>1&]]


Export["t.csv",SortBy[Join[{#[[1]],#[[2]],1-N@#[[3]]/#[[2]]},#[[4;;]]]&/@(Join@@@Select[GatherBy[Join[l2,l4],StringTake[#[[1]],StringLength@"0x479009f6552b0ad5:0x3d46376bb9318450"]&]/.nameMapping,Length@#>1&]),#[[3]]&]]


Export["t.png",ListLinePlot[Transpose@l4[[;;,{3,-1}]],PlotLegends->{"PhysicalMissingRatioSurveys","PhysicalMissingRatioGls"}]]
Correlation@@Transpose@ll4[[;;,{3,-1}]]
Export["t.csv",ll4]


ll4=SortBy[{#[[1]],#[[2]],1-N@#[[3]]/#[[2]],#[[5]],#[[6]],#[[7]]}&/@(Join@@@Select[GatherBy[Join[r,l3],First],Length@#>1&]),Last];ll4//MatrixForm


rtrainm//MatrixPlot


spans=With[{third=Floor[Length@rtrainm/3],third2=Floor[Length@rtrainm 2/3],mid=Floor[Length@rtrainm/2]},{1;;10,third;;third+10,mid;;mid+10,third2;;third2+10,-10;;-1}];
Graphics@Join[Riffle[{Blue,Red,Darker@Yellow,Green,Purple},Line[mxyfs[[1]]/@train[[#,1]]]&/@spans],{Opacity[0.1],Red,Line[mxyfs[[1]]/@train[[;;,1]]]}]
(rtrainm[[#]]//TableForm)&/@spans


{wifiss,mxyfs}=Transpose[LoadMagModel2[#][[{6,5}]]&/@fnames[[{17,18},-1]]];
mapping=MacsToIds[Join@@wifiss];
{rtrainm,rtestm}=WifisToMatrix[#,mapping]&/@wifiss;
{train,test}=wifiss;
mxyss=MapThread[Quiet[#/@#2]&,{mxyfs,#[[;;,1]]&/@wifiss}];
Graphics@BlendLine@#&/@mxyss


?SingularValueDecomposition


SingularValueDecomposition[N@(rtrainm/.{0->0.00001}),10][[1]]//MatrixPlot


r=NonNegativeMatrixFactorizationKL[N@(rtrainm/.{0->0.00001}),10,300];


Outer[Total[(#-#2)^2]&,#,#,1]&@mxyss[[1,;;]]//MatrixPlot


#.Transpose@#&@SingularValueDecomposition[N@(rtrainm/.{0->0.00001}),10][[1]]//MatrixPlot


#.Transpose@#&@Chop[r[[1,;;]],0.01]//MatrixPlot


Chop[r[[1,;;]],0.01]//MatrixPlot


Chop[r[[1]].r[[2]],0.1]//MatrixPlot
rtrainm//MatrixPlot


Norm[rtrainm-r[[1]].r[[2]],"Frobenius"]/Norm[rtrainm,"Frobenius"]


r[[-1]]


Median@Select[Flatten[mask dsm],#>0&]


z2m=Median@Select[Flatten[mask dsm],#>0&]/Median@Select[Flatten[mask dsz],#>0&];ndsz=z2m dsz;
Graphics[BlendPoints@ClassicMultidimensionalScaling[#,2],Axes->True]&/@{ndsz,dsm,mask ndsz+(1-mask)dsm}


{s1,s2}=Select[#,#>0&]&/@{Flatten[mask dsm],Flatten[mask dsz]};
N@SpearmanRankCorrelation[s1,s2]
Correlation[s1,s2]


Histogram@Flatten[mask dsm]
Histogram@Flatten[mask ndsz]


(mask DistanceMatrixToGramMatrix@dsz)+(1-mask)DistanceMatrixToGramMatrix@dsm//MatrixPlot


DistanceMatrixToGramMatrix@dsm//MatrixPlot
DistanceMatrixToGramMatrix@dsz//MatrixPlot


DistanceMatrixToGramMatrix[dsm][[;;20,;;20]]//MatrixPlot
DistanceMatrixToGramMatrix@dsm[[;;20,;;20]]//MatrixPlot


DistanceMatrixToGramMatrix[dsm][[;;100,;;100]]//MatrixPlot
DistanceMatrixToGramMatrix@dsm[[;;100,;;100]]//MatrixPlot


{mask dsm//MatrixPlot,mask dsz//MatrixPlot}
mask ndsz+(1-mask)dsm//MatrixPlot


dsz//MatrixPlot
SvdApprox[mask dsz,4]//MatrixPlot


SvdApprox[mask dsz+(1-mask)dsm,4]//MatrixPlot


Omega=mask;Dm=dsz Omega;lambda=3;


M=Dm;M//MatrixPlot
err=Norm[M-SvdApprox[M,4],"Frobenius"]^2+lambda Norm[Omega M-Dm,"Frobenius"]^2
M=Dm+(1-Omega)(-err PseudoInverse[2(M-SvdApprox[M,4])+2lambda Omega (Omega M-Dm)]);
M//MatrixPlot
err=Norm[M-SvdApprox[M,4],"Frobenius"]^2+lambda Norm[Omega M-Dm,"Frobenius"]^2


SingularValueList@dsm
SingularValueList[Chop@Exp[-dsm],10]


gsm=DistanceMatrixToGramMatrix@dsm;gsm//MatrixPlot
SingularValueList@gsm


k=250;m=10;n=50;X1=dsm[[k;;k+n,k;;k+n]];
Export["t.csv",BandMatrix[Length@X1,m] dsz[[k;;k+n,k;;k+n]]+ (1-BandMatrix[Length@X1,n-m])X1];{Import@"t.csv"//MatrixPlot}


X2=GramMatrixToDistanceMatrix[Import@"t2.csv",2];X3=GramMatrixToDistanceMatrix[Import@"t3.csv",2];
{X1//MatrixPlot,X2//MatrixPlot,X3//MatrixPlot}
{Norm[X1-X2,"Frobenius"]/Norm[X1,"Frobenius"],Norm[X1-X3,"Frobenius"]/Norm[X1,"Frobenius"]}
Graphics[{Opacity[0.5],Blue,Line[ClassicMultidimensionalScaling[X1,2]],Red,Line[ClassicMultidimensionalScaling[X2,2]],Darker@Yellow,Line[ClassicMultidimensionalScaling[X3,2]],Green,Line[ClassicMultidimensionalScaling[dsz[[k;;k+n,k;;k+n]],2]]},Axes->True]


X1=gsm;
Export["t.csv",BandMatrix[Length@X1,5] X1];{Import@"t.csv"//MatrixPlot}


X2=Import@"t2.csv";
{X1//MatrixPlot,X2//MatrixPlot}
Norm[X1-X2,"Frobenius"]/Norm[X1,"Frobenius"]


BandMatrix=Function[{n,k},Table[If[Abs[i-j]<k,1,0],{i,n},{j,n}]];(*BandMatrix[10,3]//MatrixForm*)
UpperBandMatrix=Function[{n,k},Table[If[i-j<k,1,0],{i,n},{j,n}]];
awts=awifis[[;;,1]];amxys=1.4Standardize[LatLongToXY[mxyf/@awts],Mean,1&];azxys=Standardize[Interpolation[Thread@{zts,zxys},InterpolationOrder->1]/@awts,Mean,1&];ListPlot@{amxys,azxys}
mask=BandMatrix[Length@dsz,100];dsz=Outer[Norm[#-#2]^2&,#,#,1]&@azxys;dsm=Outer[Norm[#-#2]^2&,#,#,1]&@amxys;{dsz//MatrixPlot,dsm//MatrixPlot}


Omega=BandMatrix[Length@dsm,100];Dm=dsm Omega;Dm//MatrixPlot
{L,S}=RpcaMatrixComplete[Dm,Omega,0.05,200];
L//MatrixPlot
SingularValueList@L


WifiDistance=Compile[{{v,_Real,1},{v2,_Real,1}},1./Max[0.0001,Mean@MapThread[If[#1==0||#2==0,0,1./(10+Abs[#1-#2])]&,{v,v2}]]];
WifiDistance2=Compile[{{v,_Real,1},{v2,_Real,1}},1-Tr[MapThread[If[#1==0||#2==0,0,5./(5+Abs[#1-#2])]&,{v,v2}]]/Tr[MapThread[Boole[#1!=0||#2!=0]&,{v,v2}]]];
fname="2011.txt";
{zmags,zxys,zaxyzs,zts,mxyf,wifis,qs,ncfqs}=LoadMagModel2[fname];markerf=LoadMarkerXYf@fname;mxys=Standardize[Quiet[markerf/@(First/@wifis)],Mean,1&];Graphics[BlendLine[mxys],Axes->True]
mapping=MacsToIds[wifis];
train=SortBy[RandomSample[wifis,Floor[Length@wifis/2]],First];
test=SortBy[Complement[wifis,train],First];
{rtrainm,rtestm}=WifisToMatrix[#,mapping]&/@{train,test};
{trainm,testm}=Prepend[#,1]&/@{rtrainm,rtestm};
(*step=5;
{trainm,testm}=Prepend[#,1]&/@(With[{n0=Length@Select[#,#==0&],n=Length@#},If[n==n0,0,N@Tr@#/(n-n0)]]&/@Transpose[#]&/@Partition[WifisToMatrix[#,mapping],step])&/@{train,test};*)
awifis=AggregateWifis[wifis,5];am=WifisToMatrix[#,mapping]&@awifis;
dw=Outer[WifiDistance,#,#,1]&@am;dw2=Outer[WifiDistance2,#,#,1]&@am;dm=Outer[Norm[#-#2]^2&,#,#,1]&[Quiet[markerf@First@#&/@awifis]];
MatrixPlot/@{dw,dw2,dm}


Export["t.csv",dm]


Export["t.txt",StringJoin@Riffle[StringJoin@Riffle[ToString/@#," "]&/@dm,"\n"]]


Graphics[BlendPoints@ClassicMultidimensionalScaling[dm,2]]
Graphics[BlendPoints@ClassicMultidimensionalScaling[dw2,2]]


ListPlot3D@Select[Append@@@Thread@{Standardize[Quiet[markerf/@awifis[[;;,1]]],Mean,1&],am[[;;,8]]},#[[-1]]!=0&]


ListPointPlot3D@Select[Append@@@Thread@{Quiet[markerf/@awifis[[;;,1]]],am[[;;,8]]},#[[-1]]!=0&]


Correlation[Flatten@#,Flatten@dm]&/@{dw,dw2}
Needs["MultivariateStatistics`"]
N@SpearmanRankCorrelation[Flatten@#,Flatten@dm]&/@{dw,dw2}


kw2=Outer[WifiDistance2,#,#,1]&[Quiet[markerf@First@#&/@awifis]];K=kw2;
Kc=CenterizeKernelMatrix@K;Alpha=Eigenvectors@Kc;Graphics[BlendPoints[K.Transpose[Alpha[[;;2]]]],Axes->True]


km=Outer[Exp[-Norm[#-#2]^2/50]&,#,#,1]&[Quiet[markerf@First@#&/@awifis]];K=km;
Kc=CenterizeKernelMatrix@K;Alpha=Eigenvectors@Kc;Graphics[BlendPoints[K.Transpose[Alpha[[;;2]]]],Axes->True]


gt=Quiet[markerf/@test[[;;,1]]];
Mean@MapThread[Norm[#-#2]&,{gt,#}]&/@{xys,xys2}
Mean@MapThread[Norm[#-#2]&,{gt,MedianFilter[#,{3,0}]}]&/@{xys,xys2}


WifiDistance=Compile[{{v,_Real,1},{v2,_Real,1}},1./Max[0.0001,Mean@MapThread[If[#1==0||#2==0,0,1./(10+Abs[#1-#2])]&,{v,v2}]]];
nf=Nearest[Thread[rtrainm->Quiet[markerf/@train[[;;,1]]]],DistanceFunction->WifiDistance];
xys=First@nf@#&/@rtestm;
{Graphics@BlendLine[xys],Graphics@BlendLine[MedianFilter[xys,{3,0}]]}


WifiDistance2=Compile[{{v,_Real,1},{v2,_Real,1}},1/(1+Tr[MapThread[If[#1==0||#2==0,0,5./(5+Abs[#1-#2])]&,{v,v2}]]/Tr[MapThread[Boole[#1!=0||#2!=0]&,{v,v2}]])];
nf2=Nearest[Thread[rtrainm->Quiet[markerf/@train[[;;,1]]]],DistanceFunction->WifiDistance2];
xys2=First@nf2@#&/@rtestm;
{Graphics@BlendLine[xys2],Graphics@BlendLine[MedianFilter[xys2,{3,0}]]}


Transpose@SortBy[Transpose@rtrainm,Tr]//MatrixPlot


Graphics@{BlendLine@MedianFilter[L[[;;Length@trainm,-2;;]],{0,0}],Line@m[[;;Length@trainm,-2;;]]}


Graphics@{BlendLine@MedianFilter[L[[Length@trainm;;,-2;;]],{3,0}],Line@m[[;;Length@trainm,-2;;]]}


m=ArrayFlatten@{{trainm,Standardize[Quiet[markerf/@(First/@train[[;;;;step]])],Mean,1&]},{testm,0}};
Omega=Array[If[Length@trainm<#<=Length@m&&#2>=Length@m[[1]]-2,0,1]&,Dimensions@m];
(*Omega=N@Map[Boole[#!=0]&,m,{2}];*)
{L,S}=RpcaMatrixComplete[N@m,Omega,0.05,100];
MatrixPlot/@{L,S}
MatrixRank@L


Clear[mu,mu2,sigma,sigma2];
Integrate[Min[PDF[NormalDistribution[mu,sigma],x],PDF[NormalDistribution[mu2,sigma2]],x],{x,-Infinity,Infinity}]


Export["t.png",Plot[CDF[HistogramDistribution[r3[[;;,-1]]],x],{x,0,1},PlotLabel->"Missing ratio CDF",PlotRange->All]]
Export["t2.png",Plot[CDF[HistogramDistribution[r5[[;;,-1]]],x],{x,0,1},PlotLabel->"Physical missing ratio CDF",PlotRange->All]]


Export["t.png",Histogram[r3[[;;,-1]],PlotLabel->"Missing ratio histogram"]]
Export["t2.png",Histogram[r5[[;;,-1]],PlotLabel->"Physical missing ratio histogram"]]


loadMissingRatio=Reverse@SortBy[Append[{#[[1]],#[[2]],#[[2]]-#[[3]]},1.-N@#[[3]]/#[[2]]]&/@(Import[#]/.nameMapping),Last]&;
filterMtv=Select[#,StringMatchQ[#[[1]],"MTV_"~~__]&]&;


(*JoinTablesByKey@{Table[Prepend[Range[3],i],{i,2}],Table[Prepend[Reverse@Range[3],i],{i,{1,3}}]}//TableForm*)
coverage=Import@"model_decay/sensorcollects-level-coverage.csv"/.nameMapping;
combined=loadMissingRatio@"model_decay/combined-level-mac-stats.csv";
physical=loadMissingRatio@"model_decay/2-physical-level-mac-stats.csv";
manual=Append[Prepend[ToExpression/@#[[2;;-2]],#[[1]]],#[[-1]]]&@StringSplit[#[[1]]]&/@Import["model_decay/manual_test_Nov_7.csv"];
chopLevel=StringJoin@Riffle[Most@StringSplit[#,"_"],"_"]&;
ncoverage=Dispatch[Rule@@@({#[[1,1]],Tr@#[[;;,2]]}&/@GatherBy[{chopLevel@#[[1]],#[[2]]}&/@coverage,First])];


N@Length@Select[#,#>30&]/Length@#&@Select[(chopLevel[#[[1]]]/.ncoverage)&/@physical,NumberQ]


Export["t4.csv",SortBy[Select[Select[JoinTablesByKey@{physical,coverage,List@@@ageMapping[[1]]},#[[4]]!=""&&#[[2]]>10&],(chopLevel[#[[1]]]/.ncoverage)>100&],#[[4]]&]//Reverse]


l=ReadList["model_decay/macs/insight-macs.csv",String];l2=ReadList["model_decay/macs/aptable-macs.csv",String];


Last/@Select[StringSplit[#[[1]],":"]&/@nameMapping[[1]],#[[1]]=="0x89c259a94d883d35"&]


Function[fprint,Select[{#,"0x"<>IntegerString[BitXor@@Append[#,FromDigits["d50903a5572b3c5a",16]],16]}&/@
	(Subsets[FromDigits[StringReplace[#,"0x"->""],16]&/@(Last/@Select[StringSplit[#[[1]],":"]&/@nameMapping[[1]],#[[1]]=="0x89c259a94d883d35"&])]),#[[-1]]==fprint&]]/@
		{"0x29f3e6daa782d51e","0x9503fd849e4b050a","0xb151ef827ab1c87f"}//MatrixForm


Select[nameMapping[[1]],StringSplit[#[[1]],":"][[1]]=="0x0d4231d06aa8ff2f"&][[;;,1]](*/.nameMapping*)


("0x89c259a94d883d35:0x"<>IntegerString[#,16]&/@{13005739383191491624,7230788854756340773,4196989457356396872,8568393330350896852,16327313184314277241,11225779848423320847,11936713218252121783})(*/.nameMapping*)//Sort//TableForm


Select[nameMapping[[1]],StringSplit[#[[1]],":"][[1]]=="0x89c259a94d883d35"&][[;;,1]]/.nameMapping//Sort//TableForm


SortBy[Join@@@Select[GatherBy[r~Join~r3,First],Length@#>1&],Last]//TableForm


Parallelize[Run/@Flatten@Table[{"fileutil cat /cns/vc-d/home/insight/ttl=15d/lbs/run39/"<>i<>"_missing-macs.csv* >~/model_decay/"<>i<>"_missing-macs.csv",
	"codex --sstable=\"%s,%s\n\" /cns/vc-d/home/insight/ttl=15d/lbs/run39/"<>i<>"_level-mac-stats.sstable* >~/model_decay/"<>i<>"_level-mac-stats.csv"},
{i,{"7_15__8_15","8_15__9_15","9_15__10_15","10_15__11_15"}}]]


rlmss=Select[#/.nameMapping,StringMatchQ[#[[1]],"MTV_"~~__]&]&/@lmss;


Export["t.csv",Prepend[First/@(StringSplit/@ImportString[s,"CSV"]),{"level","Ap","MAp","ratio","triggered"}]]


Join[#[[;;2]],#[[2]]-#[[3;;]]]~Join~{Min[#[[2]]-#[[3;;]]]/#[[2]]//N}&/@SortBy[#[[{1,2,3,6,9,12}]]&/@(Join@@@Thread[Sort/@rlmss]),1-Max@#[[3;;]]/#[[2]]&]//Reverse//TableForm


ListLinePlot[#[[{2,3,6,9,12}]]&/@(Join@@@Thread[Sort/@rlmss])]


rlmss=Select[#/.nameMapping,StringMatchQ[#[[1]],"MTV_google"~~__]&]&/@lmss;
N[Mean/@Transpose[#[[{2,3,6,9,12}]]&/@(Join@@@Thread[Sort/@rlmss])]]
N[Median/@Transpose[#[[{2,3,6,9,12}]]&/@(Join@@@Thread[Sort/@rlmss])]]
N[Median/@Transpose[#[[{2,3,6,9,12}]]&/@(Join@@@Thread[Sort/@rlmss])]]


missings=First/@Import@#&/@Table["model_decay/"<>i<>"_missing-macs.csv",{i,{"7_15__8_15","8_15__9_15","9_15__10_15","10_15__11_15"}}];
lmss=Import/@Table["model_decay/"<>i<>"_level-mac-stats.csv",{i,{"7_15__8_15","8_15__9_15","9_15__10_15","10_15__11_15"}}];


SortBy[Select[{#[[1]],#[[2]],#[[2]]-#[[3]],#[[5]],#[[6]]}&/@(Join@@@GatherBy[l~Join~mrs,First]),StringMatchQ[#[[1]],"MTV_google"~~__]&],First]//TableForm


{#[[1]],#[[2]],#[[2]]-#[[3]],#[[5]],#[[6]]}&/@(Join@@@GatherBy[l~Join~mrs,First])//#[[;;100]]&//TableForm


fnames=JPNfnames[[;;,-1]];
wifiss=Parallelize[LoadWifi/@fnames];
mapping=MacsToIds[Join@@wifiss];
ms=WifisToMatrix[#,mapping]&/@wifiss;
markerfs=LoadMarkerXYf/@fnames;
(*poss=MapThread[Standardize[#,Mean,1&]&@Quiet[#/@#2[[;;,1]]]&,Thread@{markerfs,wifiss}];*)


fnames=modelfiles@JPNfnames[[-1,-1]];
wifiss=Parallelize[LoadWifi/@fnames];
mapping=MacsToIds[Join@@wifiss];
ms=WifisToMatrix[#,mapping]&/@wifiss;
markerfs=LoadMarkerXYf/@fnames;
poss=Table[Standardize[markerfs[[i]]/@wifiss[[i,;;,1]],Mean,1&],{i,Length@markerfs}];


<< ComputationalGeometry`
ParallelNeeds@"ComputationalGeometry`"


i=1;
(*BubbleChart[{#[[1]],#[[2]],#[[3]]+100}&/@]*)
m=Select[ArrayFlatten@{{m,pos}},#[[i]]!=0&][[;;,{-2,-1,i}]]
ListPlot3D@m


Parallelize[Table[Function[{m,pos},Module[{polys},
	polys=Polygon@#[[ConvexHull@#]]&/@Select[Table[Select[ArrayFlatten@{{m,pos}},#[[i]]!=0&][[;;,-2;;]],{i,Length@m[[1]]}],#!={}&];
	Graphics[Prepend[Riffle[polys,Array[Hue[0.7#/Length@polys]&,Length@polys]],Opacity[0.2]]]]][ms[[i]],poss[[i]]],{i,Length@ms}],DistributedContexts->All]


polys=Polygon@#[[ConvexHull@#]]&/@Select[Table[Select[ArrayFlatten@{{m,pos}},#[[i]]!=0&][[;;,-2;;]],{i,Length@m[[1]]}],#!={}&];
Graphics[Prepend[Riffle[polys,Array[Hue[0.7#/Length@polys]&,Length@polys]],Opacity[0.2]]]


Graphics[Point/@Parallelize[Table[With[{ps=FindClusters[Thread[pos->Range@Length@pos],300][[i]]},Mean@pos[[ps]]],{i,10}],DistributedContexts->All],Axes->True]


Parallelize@Table[With[{ps=FindClusters[Thread[pos->Range@Length@pos],300][[i]]},Plus@@@(Transpose@Map[Boole[#!=0]&,m[[ps]],{2}])],{i,10}]//MatrixPlot


Parallelize@Table[With[{ps=FindClusters[Thread[pos->Range@Length@pos],300][[i]]},{Graphics[Line@Standardize[pos[[ps]],Mean,1&],Axes->True],m[[ps]]//MatrixPlot}],{i,10}]


Graphics/@{BlendLine@Extract[pos,Parallelize[Nearest[N[m]->Automatic,#,1]&/@N@m2[[;;300]],DistributedContexts->All]],BlendLine@pos2[[;;300]]}


Graphics@BlendLine@MedianFilter[Transpose@{xs,ys},{3,0}]


ListPlot[m.PseudoInverse[N@m].pos[[;;,1]]-pos[[;;,1]],PlotRange->All]
ListPlot[m.PseudoInverse[N@m].pos[[;;,2]]-pos[[;;,2]],PlotRange->All]


m=ms[[1]];pos=poss[[1]];m2=ms[[2]];pos2=poss[[2]];
xs=m2.LeastSquares[m,pos[[;;,1]]];r=xs-pos2[[;;,1]];
{Histogram@r,Mean@Abs@r}
ys=m2.LeastSquares[m,pos[[;;,2]]];r2=ys-pos2[[;;,2]];
{Histogram@r2,Mean@Abs@r2}


ListPlot[LeastSquares[m,pos[[;;,1]]],PlotRange->All]
ListPlot[LeastSquares[m,pos[[;;,2]]],PlotRange->All]


Image[#,ImageSize->1000]&@Graphics[MapThread[Arrow[{#,#2}]&,{Transpose[{xs,ys}],pos2}][[;;;;20]]]


Graphics@BlendLine[Transpose[{xs,ys}][[;;;;10]]]
Graphics@BlendLine@pos2


Dimensions/@ms
nms=(N@(#/.{0->-100})&/@ms);
f=Function[{m,m2},Mean@Abs[m[[;;,;;-2]].LeastSquares[m2[[;;,;;-2]],m2[[;;,-1]]]-m[[;;,-1]]]];
Outer[f,#,#,1]&@nms//MatrixForm


f2=Function[{m,m2,col},Module[{full=Range@Length@m[[1]],beta,r},beta=LeastSquares[m2[[;;,Complement[full,{col}]]],m2[[;;,col]]];
	r=m[[;;,Complement[full,{col}]]].beta-m[[;;,col]];Mean@Abs@r]];


l=nms[[2,;;,15]];
ListLinePlot@{l,If[Abs@#<10,0,#]&/@Prepend[Differences@l,0]-100}


ListLinePlot@{nms[[2,;;,15]],nms[[2,;;,Complement[Range@Length@nms[[1,1]],{15}]]].LeastSquares[nms[[1,;;,Complement[Range@Length@nms[[1,1]],{15}]]],nms[[1,;;,15]]]}
ListLinePlot/@{nms[[2,;;,15]],nms[[2,;;,Complement[Range@Length@nms[[1,1]],{15}]]].LeastSquares[nms[[1,;;,Complement[Range@Length@nms[[1,1]],{15}]]],nms[[1,;;,15]]]}


SortBy[Table[{ListPlot@nms[[1]][[;;,i]],f2[nms[[2]],nms[[1]],i]},{i,Length@nms[[1,1]]}],-#[[-1]]&]


(*m=N@ms[[1]]/.{0->-100};m2=N@ms[[1]]/.{0->-100};*)
m=nms[[1]];m2=nms[[2]];full=Range@Length@m[[1]];col=RandomChoice[full]
beta=LeastSquares[m2[[;;,Complement[full,{col}]]],m2[[;;,col]]];
r=m[[;;,Complement[full,{col}]]].beta-m[[;;,col]];
{Mean@Abs@r,Sqrt@Mean[r r]}
{Histogram@beta,Histogram@r}


Export["t.csv",Prepend[ArrayFlatten@{{m,pos}},Array["ap"<>ToString@#&,Length@m[[1]]]~Join~{"lng","lat"}]]
Export["t2.csv",Prepend[ArrayFlatten@{{m/.{0->-100},pos}},Array["ap"<>ToString@#&,Length@m[[1]]]~Join~{"lng","lat"}]]


l={"43_1"->"0x808fba0275af85b7:0xf8be8e7c556d0b93","44_1"->"0x808fb9ff55393439:0xb38b01824805c20a",
"45_1"->"0x808fb9ff37878169:0x41a65699fca52339","GWC1"->"0x808fba1b17feb01b:0xbbac582372b6a08a",
"LAX_paseo_colorado"->"0x80c2c36f6d82d72f:0xea2599898d18527a","WAS_smithsonian_nationalmuseum_americanhistory_1"->"0x89b7b799226ed8f9:0xa120561a0bc2686b"};


Image[#,ImageSize->Large]&@Histogram@#&/@Transpose@stats3[[;;,2;;]]


(lv\[Function]Select[stats,#[[1]]==lv&])/@(Last/@l)//MatrixForm
(lv\[Function]Select[stats2,#[[1]]==lv&])/@(Last/@l)//MatrixForm


r=Select[Reverse[SortBy[{Hyperlink[#[[1]]/.nameMapping,"https://insight-ops.sandbox.google.com/insight/admin/level?k="<>#[[1]]],N@#[[4]]/#[[2]],Sequence@@#[[2;;]]}&
	/@Select[stats3,#[[5]]>10&],#[[2]]&]],StringMatchQ[#[[1,1]],"MTV_google"~~__]&];r//TableForm


Export["model_decay/lost_macs_stats.txt",StringSplit[#," "][[1]]&/@Import["model_decay/lost_macs_stats.txt","Lines"]]


lost=First/@Import@"/home/georgezhou/model_decay/lost-macs-gls.csv";
dlost=Dispatch[#->0&/@lost];
plms=Import@"/home/georgezhou/model_decay/insight-level-macs.csv";
lms=plms/.Dispatch[Rule@@@lns];
glms=Select[lms,StringMatchQ[#[[1]],"MTV_google"~~__]&];
(*Reverse@SortBy[{#[[1]],Length@#[[2;;]]}&/@lms,Last]*)
jaccard=(N@Length@Intersection[#,#2]/Length@Union[#,#2]&);
stripPrefix=StringJoin[Riffle[StringSplit[#,"_"][[3;;]],"_"]]&;
(*stripPrefix2=StringJoin[Riffle[StringSplit[#,"_"][[2;;]],"_"]]&;*)
mrs=Append[#,N@#[[3]]/#[[2]]]&/@(With[{s=#[[2;;-2]]},{#[[1]],Length@s,Length@Select[s/.dlost,#==0&]}]&/@lms);
gmrs=Select[mrs,StringMatchQ[#[[1]],"MTV_google"~~__]&];
rmrs=Select[mrs,(#[[1]]/.ageMapping)<365&];


l=Select[Join@@Parallelize[Outer[{#[[1]],#2[[1]],EditDistance[#[[1]],#2[[1]]],jaccard[#[[2;;]],#2[[2;;]]]}&,glms,glms,1],DistributedContexts->All],#[[1]]!=#[[2]]&];l[[1]]


Export["t.png",#]&@Image[#,ImageSize->Large]&@ListPlot[Map[#[[2;;3]]&,GatherBy[Select[Import["model_decay/google-level-survey.csv","Table"],Length@#>4&],Last],{2}]
	,AxesOrigin->{0,0},AxesLabel->{"#unique mac","#unique mac lost"},PlotMarkers->(Style[#,20]&/@{"-","+"})(*,PlotLegend->{"Not triggered","Triggered"}*)]
Export["t2.png",Plot[CDF[EmpiricalDistribution[rmrs[[;;,4]]],x],{x,0,1},AxesOrigin->{0,0},AxesLabel->{"missing-ratio","CDF"}]]


Export["t.png",#]&@ListPlot[{#[[1]],#[[4]]}&/@(gmrs/.ageMapping),PlotRange->All,AxesOrigin->{0,0},PlotLabel->"Google levels",AxesLabel->{"#day since survey","missing-ratio"}]
Export["t2.png",#]&@ListPlot[{#[[1]],#[[4]]}&/@(mrs/.ageMapping),PlotRange->All,AxesOrigin->{0,0},PlotLabel->"All levels",AxesLabel->{"#day since survey","missing-ratio"}]


{Length@Select[rmrs[[;;,4]],#>0.15&],Length@rmrs[[;;,4]]}


olist=ImportString[#,"CSV"]&@""


Reverse@SortBy[Select[mrs,StringMatchQ[#[[1]],"MTV_google"~~__]&],N@#[[3]]/#[[2]]&]//TableForm


Image[#,ImageSize->1000]&@GraphPlot[{#[[1]]->#[[2]],#[[3]]}&/@Select[Join@@Outer[{stripPrefix@#[[1]],stripPrefix@#2[[1]],
		If[#==#2||Hash@#[[1]]>Hash@#2[[1]],0,jaccard[#[[2;;]],#2[[2;;]]]]}&,glms,glms,1],#[[3]]>0.05&]
	,VertexLabeling->True(*,PackingMethod->"LayeredTop"*)]


Export["t.png",#]&@ListPlot[stats[[;;,2;;3]],PlotLabel->"GlsNonGps",AxesLabel->{"Model MAC number","Trace MAC number"},PlotRange->All]
Export["t2.png",#]&@ListPlot[stats2[[;;,2;;3]],PlotLabel->"GlsGps",AxesLabel->{"Model MAC number","Trace MAC number"},PlotRange->All]


stats3=Prepend[#[[2;;]],StringReplace[StringJoin@StringSplit[#[[1]]][[2;;]],"stat-"->""]]&/@Import["t.txt","CSV"];


stats=Append[#,"gls"]&/@Import@"model_decay/lost_macs_stats.csv";stats[[1]]
stats2=Append[#,"gls_gps"]&/@Import@"model_decay/lost_macs_stats_gls_gps.csv";stats2[[1]]


StringJoin@{"gqui /cns/vc-d/home/insight/ttl=15d/lbs/run37/insight-level-to-lbs-traces.sstable-* where \""
  ,StringJoin@Riffle["key_ = '"<>#<>"'"&/@Import["model_decay/levelkeys.txt","Lines"]," or "]
  ,"\" --outfile=sstable:/home/georgezhou/model_decay/lbs_traces.sstable"}


StringJoin@{"gqui /cns/vc-d/home/insight/ttl=15d/lbs/run38/insight-level-to-gls-observations.sstable* where \""
  ,StringJoin@Riffle["key_ = '"<>#<>"'"&/@Import["model_decay/levelkeys.txt","Lines"]," or "]
  ,"\" --outfile=sstable:/home/georgezhou/model_decay/gls_traces.sstable"}


(*fname="/tmp/t.csv";*)
{axyzs,wxyzs,gpss}=LoadData2[fname,#]&/@{";accel",";gyro",";gps"};wifis=LoadWifi@fname;
axyzs[[-1,1]]-axyzs[[1,1]]
{ListPlot@Transpose@axyzs[[;;,2;;]],ListPlot@Transpose@wxyzs[[;;,2;;]],ListPlot@Transpose@GaussianFilter[axyzs[[;;,2;;]],{{30,0}}],Graphics@Line@LoadXYsNoMag[fname,""][[1]],
	Graphics@BlendLine@LatLongToXY@gpss[[;;,2;;3]]}


Graphics3D@Line@Accumulate[RotateByQuaternion[{0,1,0},#]&/@LoadQuaternionF@fname/@wxyzs[[;;,1]]]


gpsss=Select[#,Last@#<30000&]&@LoadData2[#,";gps"]&/@fnames;


ExportGpsToKml=Function[{fname,gpsss},Module[{lines=Line[Reverse/@#[[;;,2;;3]]10^-7]&/@gpsss},
	Export[fname, "Data" -> {{"LayerName" -> "ShortestTour", "Geometry" -> lines}}, {"KML", "Rules"}]]];


ExportGpsToKml["tour.kml",gpsss[[;;]]]


Graphics@{Line[{{-1,1},{-1,-1},{1,-1},{1,1},{-1,1}}],Inset[img,{0,0}]}


fnames=FileNames["robintraces/*-*-*.txt"];


Graphics@Line[Reverse/@LoadData2[fname,";gps"][[;;,2;;3]]]


NanoToDateList@ToExpression[StringSplit[ReadList[#,String,1],";"][[1,1]]]&/@fnames//Sort


fnames=FileNames["/tmp/GWC1/*.txt"];fname=fnames[[1]];


LoadQuaternionFWithBias=Function[fname,
	Module[{wxyzt,ts,qqs,bias},
	wxyzt=LoadUncorrectedGyro[fname];
	bias=LoadData2[#,";bias_gyro"][[1,2;;]]&@fname;
	qqs=Quiet@IntegrateAngularVelocityWithBias[wxyzt[[All,1]],10.^9wxyzt[[All,2;;4]],Thread@{wxyzt[[All,1]],10.^9 bias&/@wxyzt}];
	Interpolation[qqs,InterpolationOrder->1]
	]];


BubbleChart@#[[;;,2;;4]]&@gpsss[[5]]
Graphics@BlendLine@#[[;;,2;;3]]&@gpsss[[5]]


ListPlot@Transpose@LoadData2[#,";accel"][[;;,2;;]]&/@fnames[[;;5]]
ListPlot@Transpose@GaussianFilter[LoadData2[#,";accel"][[;;,2;;]],{{100,0}}]&/@fnames[[;;5]]


BubbleChart@#[[;;,2;;4]]&/@gpsss[[;;5]]
Graphics[BlendPoints[#[[;;,2;;3]]]]&/@gpsss[[;;5]]
Graphics@BlendLine@MedianFilter[#[[;;,2;;3]],{3,0}]&/@gpsss[[;;5]]
Graphics@BlendLine@LoadXYs[#,""][[1]]&/@fnames[[;;5]]


fname=fnames[[3]];
gpss=LoadData2[#,";gps"]&@fname;
qf=LoadQuaternionF@fname;bqf=LoadQuaternionFWithBias@fname;
ts=First/@gpss;
dirs=RotateByQuaternion[{0,1,0},#]&/@(qf/@ts);
bdirs=RotateByQuaternion[{0,1,0},#]&/@(bqf/@ts);
axyzs=LoadData2[#,";accel"]&@fname;
ListPlot@Transpose@axyzs[[;;,2;;]]
ListPlot[Norm/@Differences@gpss[[;;,2;;3]]]
ListPlot@Transpose@Differences@gpss[[;;,2;;3]]
ListPlot@Transpose@dirs
ListPlot@Transpose@bdirs


Graphics@BlendPoints[LatLongToXY[LoadData2[#,";gps"][[;;,2;;3]]]]&/@fnames


Graphics@BlendLine@LoadXYs[#,""][[1]]&/@fnames
Graphics@BlendLine@LoadNcfXYs[#,""][[1]]&/@fnames
Graphics@BlendLine@LoadXYsNoMag[#,""][[1]]&/@fnames


fnames=FileNames["/tmp/GWC1/*.txt"];fname=fnames[[1]];


fnames=FileNames["/tmp/B44/*.txt"];fname=fnames[[1]];


Complement[smacs,mapping[[1,;;,1]]]//Length


gpsss=Parallelize[LoadData2[#,";gps"]&/@fnames];
wifiss=Parallelize[LoadWifi/@fnames];
mapping=MacsToIds[Join@@wifiss];
ms=WifisToMatrix[#,mapping]&/@wifiss;


smacs=Union[Join@@(Parallelize[LoadMacs/@FileNames["/tmp/GWC1/MTV_google_GWC1_1/*.txt"]])];{smacs//Length,smacs[[1]]}


smacs=Union[Join@@(Parallelize[LoadMacs/@FileNames["/tmp/B44/MTV_google_44_1/*.txt"]])];{smacs//Length,smacs[[1]]}


ms//PlotWifiMatrices//Image[#,ImageSize->1000]&


line=gpsss[[1]][[;;,2;;3]]10^-7;


g=Graphics[Line@line,
	PlotRange->{{center[[1]]-1000scale,center[[1]]+1000scale},{center[[2]]-1000scale,center[[2]]+1000scale}}]


ImageCompose[g,{img,0.5},center,{0,0}]


Graphics[{Opacity[0.2],Line[{-{800,800},{800,800}}],{Inset@img,0.5}}]


37 25 21.78
27 25 02.51


122 05 07.20
122 04 41.52


LatLongToXY2=GeoGridPosition[#,"Mercator"][[1]]&@GeoPosition@#&/@#&;


Graphics[Line@LatLongToXY2@line,Axes->True]
Graphics[Line@LatLongToXY@line,Axes->True]


GeoGridPosition[GeoPosition@Reverse@#,"Mercator"]&/@line


Graphics@Line@LatLongToXY@line


Graphics[Polygon[
  Map[ GeoGridPosition[ GeoPosition[#], "Mercator"][[1]] & , 
   coords, {2}]]]


center={37.42,-122.0815165};zoom=17;sizes={1000,1000};
img=Import[StringJoin@{"http://maps.googleapis.com/maps/api/staticmap?center=",
	ToString[center[[1]]],",",ToString@center[[2]],"&zoom=",ToString@zoom,"&size=",ToString@sizes[[1]],"x",ToString@sizes[[2]],"&sensor=false"}];


Image[#,ImageSize->1000]&@Graphics[BlendPoints[#[[;;,{3,2}]]&/@gpsss],Axes->True]


Image[#,ImageSize->1000]&@Graphics[BlendPoints[#[[;;,{3,2}]]&/@gpsss],Axes->True]


Image[#,ImageSize->1000]&@Graphics@BlendLine[#[[;;,2;;3]]&/@gpsss]


LoadMagModel2@"zsc/JPN_tok_m_machida_jorna_2_mokumura_survey.txt";


Parallelize[(Print@FileBaseName@#;Print@{FileBaseName@#,First@AbsoluteTiming@LoadMagModel2@#})&/@Union@Flatten[modelfiles/@Last/@fnames[[;;]]],DistributedContexts->All]//AbsoluteTiming


gs=Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];
	Magnify@Image@ListPlot[Transpose@#,AxesLabel->{"n",FileBaseName@fname}]&/@{MapThread[RotateByQuaternion,{zaxyzs2[[;;;;10]],qs2[[;;;;10]]}],MapThread[RotateByQuaternion,{zaxyzs2[[;;;;10]],ncfqs2[[;;;;10]]}]}
	]]/@Union@Flatten[modelfiles/@Last/@fnames[[;;]]],DistributedContexts->All];//AbsoluteTiming
Export["t.pdf",gs//TableForm];//AbsoluteTiming


(*Detect corrupted lines in input data*)(*grep metadata_ /home/georgezhou/zsc/*.txt >~/t2.txt*)
rs = Parallelize@  Map[{#, Import["!grep -n metadata_ " <> #, "Table"]} &,    FileNames[fname(*"~/zsc/*.txt"*)]];MatrixForm@rs[[1,2]]
(*FindList["t2.txt", {"compass", "gyro/MPL",    "accel/MPL Accel"}] // MatrixForm*)


(#//Transpose//MatrixPlot//Image[#,ImageSize->Large]&)&/@wmss


SparseSolve=Function[{m,y,k,leftMultiplier,valueOp},(*Like PseudoInverse[m].y, but sparse*)Module[{v,v2,nonNils},
	v=PseudoInverse@m.y;
	nonNils=Position[v,x_/;valueOp[x]>=Sort[valueOp/@v][[-k]]];
	v2=PseudoInverse@Transpose@Extract[Transpose@m,nonNils].y;
	leftMultiplier.FlattenAssocList[MapThread[{#[[1]],#2}&,{nonNils,v2}],Length@v]
	]];


Chop@wms2[[1]]
v=PseudoInverse@Transpose[Join@@wmss].wms2[[1]];
Chop[Transpose[Join@@wmss].v]
ListPlot[v,PlotRange->All]
v2=SparseSolve[Transpose[Append[#,1]&/@(Join@@wmss)],Append[wms2[[1]],1],10,IdentityMatrix[Length[Join@@wmss]],Abs];
Chop[Transpose[Join@@wmss].v2]
Tr@v2


testWifiPosVariates=Function[{mxys2,wmss,wms2,latlongs,featureNum,featureOp,valueOp},Module[{op2,op2mxys2,mean=Mean[Join@@latlongs]},
	op2=SparseSolve[Transpose[featureOp/@(Join@@wmss)],featureOp@#,featureNum,Transpose[Standardize[Join@@latlongs,Mean,1&]],valueOp]+mean&;
	op2mxys2=Parallelize[op2/@wms2];
	Graphics@{Opacity[0.1],BlendLine@mxys2,Opacity[1],BlendLine@MedianFilter[op2mxys2,{2,0}]}]];


r=Table[Print@{featureNum,featureOp,valueOp,testWifiPosVariates[mxys2,wmss,wms2,latlongs,featureNum,featureOp,valueOp]},
	{featureNum,{1,3,10}},{featureOp,{#&,Append[#,1]&}},{valueOp,{#&,Abs}}]


mapping=MacsToIds[Join@@Prepend[wifiss,wifis2]];
mss=(WifisToMatrix[#,mapping]/.{0->-100})&/@wifiss;
ms2=(WifisToMatrix[#,mapping]/.{0->-100})&@wifis2;
trans=Map[(#+100)/20. &,#,{2}]&;mss=trans/@mss;ms2=trans@ms2;
timeAggregate=Function[{ms2,wifis2},{#[[;;,1]],#[[;;,2;;]]}&@(Mean/@SplitBy[MapThread[Prepend,{N@ms2,wifis2[[;;,1]]}],Round[#[[1]]/3]&])];
{wts2,wms2}=timeAggregate[ms2,wifis2];
{wtss,wmss}=Thread@MapThread[timeAggregate,{mss,wifiss}];
latlongs=MapThread[Quiet[#/@#2]&,{mxyfs,wtss}];
mean=Mean[Join@@latlongs];
op=Transpose[Standardize[Join@@latlongs,Mean,1&]].PseudoInverse@Transpose[Join@@wmss].#+mean&;//AbsoluteTiming
opmxys2=Parallelize[op/@wms2,DistributedContexts->All];//AbsoluteTiming
Graphics@{Opacity[0.1],BlendLine@mxys2,Opacity[1],BlendLine@MedianFilter[opmxys2,{2,0}]}
featureNum=1;featureOp=Append[#,1]&;valueOp=#&;
op2=SparseSolve[Transpose[featureOp/@(Join@@wmss)],featureOp@#,featureNum,Transpose[Standardize[Join@@latlongs,Mean,1&]],valueOp]+mean&;//AbsoluteTiming
op2mxys2=Parallelize[op2/@wms2];//AbsoluteTiming
Graphics@{Opacity[0.1],BlendLine@mxys2,Opacity[1],BlendLine@MedianFilter[op2mxys2,{2,0}]}


Graphics[Arrow/@Thread@{wmxys2[[;;;;5]],opmxys2[[;;;;5]]}]


wmxys2=Quiet[mxyf2/@wts2];
{Mean@#,Median@#}&@MapThread[LatLongDistance,{wmxys2,MedianFilter[opmxys2,{2,0}]}]
{Mean@#,Median@#}&@MapThread[LatLongDistance,{wmxys2,opmxys2}]
{Mean@#,Median@#}&@MapThread[LatLongDistance,{wmxys2,op2mxys2}]


calcPrecisionRecall=N@{#[[1]]/(#[[1]]+#[[2]]),#[[1]]/(#[[1]]+#[[3]])}&;
disectFilename=Function[fname,If[#[[-1]]=="survey",#[[;;-2]],#]&@If[DigitQ@#[[-1]],#[[;;-2]],#]&@StringSplit[FileBaseName[fname],"_"]];(*disectFilename@"~/zsc/ZRH_terminale_CH_1_shaunvann_2.txt"*)
modelfiles=Select[FileNames["~/zsc/"<>StringJoin[Riffle[Most@disectFilename@#,"_"]]&@#<>"*"],Function[x,Not@StringMatchQ[x,__~~Last@disectFilename@#~~__]]]&;(*modelfiles@"~/zsc/ZRH_terminale_CH_1_shaunvann_2.txt"*)
sortByModelFileSize=Function[fnames,First/@Reverse@SortBy[{#,Tr[FileByteCount/@Prepend[modelfiles@#,#]]}&/@fnames,Last]];
precisionRecallMatrixOfMatches=Function[{selectedBests,th,thMarker},
	With[{tp=Length@Select[selectedBests,#[[12]]<th&&#[[6]]<thMarker&],fp=Length@Select[selectedBests,#[[12]]<th&&#[[6]]>thMarker&],
	fn=Length@Select[selectedBests,#[[12]]>th&&#[[6]]<thMarker&]},{tp,fp,fn}]];
ReweightedSquareError=Function[{ssigs,ssigs2},With[{ws=Append[1/(1+Norm@#)&/@Differences@GaussianFilter[ssigs,{{5,0}}],1]},Mean[(#.#&/@(ssigs-ssigs2))ws/Mean[ws]]]];
compositeError=Function[{needle,hay,\[Lambda]},
	Module[{corr=CompensatedCorrelationError[needle,hay,0],quad=CompensatedQuadError[needle,hay,0],
		square0=CompensatedSquareError[needle,hay,0]},266(1-1/(1+E^(-2.5888365424778983`+ 0.007784292698789694` corr-0.00900154930304511` quad+0.022940588890972072` square0)))]];
compositeError2=Function[{needle,hay,\[Lambda]},
	Module[{corr=CompensatedCorrelationError[needle,hay,0],quad=CompensatedQuadError[needle,hay,0],
		square0=CompensatedSquareError[needle,hay,0]},266(1-1/(1+E^(-3.3705+0.0114 corr-0.0111 quad+0.0223 square0)))]];
toIndexed=MapIndexed[Prepend[#,#2[[1]]]&,#]&;
dispMatch=Function[m,
	Module[{mxys=mxyss[[m[[1]],m[[4]]]],sigs=sigss[[m[[1]],m[[4]]]],mxys2=mxys2s[[m[[2]],m[[3]]]],sigs2=sigs2s[[m[[2]],m[[3]]]]},
	{m,Framed[Labeled[Min[First@CompensatedSquareError[sigs2,sigs,0],First@CompensatedSquareError[{-#[[1]],-#[[2]],#[[3]]}&/@Reverse@sigs2,sigs,0]],"Var[x-y]"],FrameStyle->Red],
		Framed[Labeled[Mean@MapThread[LatLongDistance,{mxys,mxys2}],"Mean-Marker"],FrameStyle->Blue],
		Labeled[ReweightedSquareError[sigs,sigs2],"ReweightedSquare"],
		Labeled[First@CompensatedQuadError[sigs2,sigs,0],\[Sqrt]"Quad"],#^2&@Tr@Mean[Abs/@(sigs2-sigs)],Labeled[First@CompensatedCorrelationError[sigs2,sigs,0],"Corr"],
		Labeled[First@compositeError[sigs2,sigs,0],"logistic"],Labeled[First@compositeError2[sigs2,sigs,0],"logistic-2"],
		(*Norm[sigs2,"Frobenius"]/Length@sigs2,Norm[sigs,"Frobenius"]/Length@sigs,*)
		First@CompensatedSquareErrorOneDim[Norm/@sigs2,Norm/@sigs,0],
	FileBaseName@modelfnames[[m[[1]]]],Graphics[{Opacity[0.2],BlendBlueLine@mxys2,BlendRedLine@mxys},Axes->True],
	Show@{Graphics@{Blue,Line[Reverse/@mxys2]},Graphics@{Red,Line[Reverse/@mxys],Axes->True},g2,gs[[m[[1]]]]},
	ListPlot[{Norm/@sigs2,Norm/@sigs},PlotRange->{Automatic,{10,70}}],
	ListPlot[#,PlotRange->{Automatic,{-60,60}}]&/@Transpose[Transpose/@{sigs2,sigs}]}]];
pickWifis=Function[{wifis,lowerUpper},Union@@Select[wifis,lowerUpper[[1]]<=First@#<=lowerUpper[[-1]]&][[;;,2,;;,1]]];
wifiSetDistance=With[{inter=Length@Intersection[#,#2],union=Length@Union[#,#2]},-200. If[union==0,0,inter/union]]&;
fnames={{1,"~/zsc/COP_steenstrom_fields_copenhagen_DK_1_erea_survey.txt"},{2,"~/zsc/COP_steenstrom_fields_copenhagen_DK_1_mbaron_survey.txt"},{3,"~/zsc/HVI_steenstrom_hvidovre_DK_1_ajeer_survey.txt"},
{4,"~/zsc/HVI_steenstrom_hvidovre_DK_1_hheer.txt"},{5,"~/zsc/JPN_kng_m_cial_tsurumi_1_jsuzuki_survey.txt"},{6,"~/zsc/JPN_kng_m_cial_tsurumi_1_yino_survey.txt"},{7,"~/zsc/JPN_osk_d_hankyu_mens_5_yakita_survey.txt"},
{8,"~/zsc/JPN_osk_d_hankyu_mens_5_yiwama_survey.txt"},{9,"~/zsc/JPN_osk_m_herbis_3_yiwama_survey.txt"},{10,"~/zsc/JPN_osk_m_herbis_3_yyuto_survey.txt"},{11,"~/zsc/JPN_stm_d_marui_omiya_3_domine_survey.txt"},
{12,"~/zsc/JPN_stm_d_marui_omiya_3_ershimada_survey.txt"},{13,"~/zsc/JPN_stm_d_marui_omiya_6_domine_survey.txt"},{14,"~/zsc/JPN_stm_d_marui_omiya_6_hyazaki_survey.txt"},{15,"~/zsc/JPN_stm_d_marui_omiya_7_domine_survey.txt"},
{16,"~/zsc/JPN_stm_d_marui_omiya_7_kykobayashi_survey.txt"},{17,"~/zsc/JPN_tok_m_divercity_P_7_jsuzuki_survey.txt"},{18,"~/zsc/JPN_tok_m_divercity_P_7_takaike.txt"},{19,"~/zsc/JPN_tok_m_ginza9_1_tchiba_survey.txt"},
{20,"~/zsc/JPN_tok_m_ginza9_1_tkosaka_survey.txt"},{21,"~/zsc/JPN_tok_m_machida_jorna_2_eiwamoto_survey.txt"},{22,"~/zsc/JPN_tok_m_machida_jorna_2_katsuyam_survey.txt"},{23,"~/zsc/LAS_terminal3_1_ereanos_2.txt"},
{24,"~/zsc/LAS_terminal3_1_willbaker_2.txt"},{25,"~/zsc/LIE_unibail_carre_senart_Lieusaint_Cedex_FR_1_erea_survey.txt"},{26,"~/zsc/LIE_unibail_carre_senart_Lieusaint_Cedex_FR_1_hsongue_survey.txt"},
{27,"~/zsc/MAR_orange_canebiere_Marseille_FR_1_malberdi_survey.txt"},{28,"~/zsc/MAR_orange_canebiere_Marseille_FR_1_natashar_survey.txt"},{29,"~/zsc/MTV_chm_1401_nshorelineboulevard_1_phsmith.txt"},
{30,"~/zsc/MTV_chm_1401_nshorelineboulevard_1_toddbeier.txt"},{31,"~/zsc/NEW_bestbuy_1280_lexingtonave_1_dtempleton_survey.txt"},{32,"~/zsc/NEW_bestbuy_1280_lexingtonave_1_scolopmontero_survey.txt"},
{33,"~/zsc/ORF_mainterminal_garage_2_cwhitestone_survey.txt"},{34,"~/zsc/ORF_mainterminal_garage_2_dawessling_survey.txt"},{35,"~/zsc/SEA_main_concourse_2_doerre.txt"},{36,"~/zsc/SEA_main_concourse_2_lebarnes.txt"},
{37,"~/zsc/WAS_smithsonian_nationalmuseum_of_naturalhistory_0_rgroover_2.txt"},{38,"~/zsc/WAS_smithsonian_nationalmuseum_of_naturalhistory_0_trossiter.txt"},{39,"~/zsc/WAS_smithsonian_zoo_panda_plaza_1_djose_survey.txt"},
{40,"~/zsc/WAS_smithsonian_zoo_panda_plaza_1_lbersing.txt"},{41,"~/zsc/ZRH_terminale_CH_1_nhalai_survey.txt"},{42,"~/zsc/ZRH_terminale_CH_1_shaunvann_2.txt"}};
JPNfnames=Select[fnames,StringMatchQ[#[[2]],__~~"JPN"~~__]&];
testFileMagMatch=Function[{fname,useScales,windowLength},
	Module[{modelfnames=modelfiles@fname,zmags2,zxys2,zaxyzs2,zts2,mxyf2,zmagss,zxyss,zaxyzss,ztss,mxyfs,mxys2,mxyss,sigs2,qsigs2,sigss,qsigss,
		scales,toNeedles,sigs2s,qsigs2s,mxys2s,ars,selectedBests,zxys2s,wifis2,wifiss,qss,qs2,ncfqss,ncfqs2},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=Quiet[mxyf2/@zts2];
	{zmagss,zxyss,zaxyzss,ztss,mxyfs,wifiss,qss,ncfqss}=Transpose@Map[LoadMagModel2,modelfnames];mxyss=MapThread[Quiet@Map[#,#2]&,{mxyfs,ztss}];
	sigs2=FactorizeByGravityYPointingDirs[zmags2,zaxyzs2,10];sigss=MapThread[FactorizeByGravityYPointingDirs[#,#2,10]&,{zmagss,zaxyzss}];
	qsigs2=MapThread[RotateByQuaternion,{zmags2,ncfqs2}];qsigss=MapThread[MapThread[RotateByQuaternion,{#,#2}]&,{zmagss,ncfqss}];
	scales=If[useScales,Table[Floor[windowLength k],{k,0.7,1.4,0.05}],{windowLength}];
	toNeedles=If[useScales,Table[ResampleWithRate[#,k],{k,0.7,1.4,0.05}],List@#]&/@Partition[#,windowLength]&;
	{sigs2s,qsigs2s,mxys2s,zxys2s}=toNeedles/@{sigs2,qsigs2,mxys2,zxys2};
	ars=Flatten[(*Parallelize@*)Table[Table[With[{haySpan=#[[1]];;#[[1]]+scales[[scaleIndex]]-1,cost=#[[2]]},With[{ssigs=sigss[[hayIndex,haySpan]],ssigs2=sigs2s[[needleIndex,scaleIndex]]},
			{hayIndex,needleIndex,scaleIndex,haySpan,cost,Min@MapThread[LatLongDistance,{mxyss[[hayIndex,haySpan]],mxys2s[[needleIndex,scaleIndex]]}]
				,(*7*)First@CompensatedSquareError[ssigs,ssigs2,-1],First@CompensatedQuadError[ssigs,ssigs2,0],First@CompensatedCorrelationError[ssigs,ssigs2,0]
				,(*10*)ReweightedSquareError[ssigs,ssigs2],(*11*)First@compositeError[ssigs,ssigs2,0]
				,(*12*)Min[First@compositeError2[ssigs,ssigs2,0],First@compositeError2[ssigs,{-#[[1]],-#[[2]],#[[3]]}&/@Reverse@ssigs2,0]]
				,(*13*)wifiSetDistance[pickWifis[wifiss[[hayIndex]],ztss[[hayIndex,haySpan]]],pickWifis[wifis2,Partition[zts2,windowLength][[needleIndex]]]]
				,(*14*)First@CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex,haySpan]],0]
				,(*15*)HausdorffDistance[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex,haySpan]],Dot[#-#2,#-#2]&]
				,(*16*)HausdorffDistance[sigs2s[[needleIndex,scaleIndex]],sigss[[hayIndex,haySpan]],Dot[#-#2,#-#2]&]}
			]]&@With[{min=Min[#]},{Position[#,min][[1,1]],min}]&@
				(*CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]*)
				MapThread[Min,{CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]
					,CompensatedSquareError[{-#[[1]],-#[[2]],#[[3]]}&/@Reverse@qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]}],{hayIndex,Length@sigss}]
	,{needleIndex,Length@sigs2s},{scaleIndex,Length@scales}],2];
	{Select[(SortBy[#,#[[12]]&][[1]]&/@GatherBy[ars,#[[2]]&]),#[[5]]<800&],mxyss,mxys2,mxys2s,zxys2,zxys2s}
]];


testWifiMatch=Function[{fname,windowLength},Module[{modelfnames=modelfiles@fname,zmags2,zxys2,zaxyzs2,zts2,mxyf2,zmagss,zxyss,zaxyzss,ztss,mxyfs,mxys2,mxyss,sigs2,sigss,
		scales,toNeedles,sigs2s,mxys2s,ars,selectedBests,zxys2s,wifis2,wifiss,zts2s,useScales=False},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2}=LoadMagModel2[fname];mxys2=Quiet[mxyf2/@zts2];
	{zmagss,zxyss,zaxyzss,ztss,mxyfs,wifiss}=Transpose@Map[LoadMagModel2,modelfnames];mxyss=MapThread[Quiet@Map[#,#2]&,{mxyfs,ztss}];
	sigs2=zmags2;sigss=zmagss;
	scales=If[useScales,Table[Floor[windowLength k],{k,0.7,1.4,0.05}],{windowLength}];
	toNeedles=If[useScales,Table[ResampleWithRate[#,k],{k,0.7,1.4,0.05}],List@#]&/@Partition[#,windowLength]&;
	{sigs2s,mxys2s,zxys2s,zts2s}=toNeedles/@{sigs2,mxys2,zxys2,zts2};
	Print[Dimensions/@{sigs2s,mxys2s,zxys2s,zts2s}];
	ars=Flatten[(*Parallelize@*)Table[Table[With[{haySpan=windowLength (#[[1]]-1)+1;;windowLength (#[[1]]-1)+1+scales[[scaleIndex]]-1,cost=#[[2]]},With[{ssigs=sigss[[hayIndex,haySpan]],ssigs2=sigs2s[[needleIndex,scaleIndex]]},
			{hayIndex,needleIndex,scaleIndex,haySpan,cost,Min@MapThread[LatLongDistance,{mxyss[[hayIndex,haySpan]],mxys2s[[needleIndex,scaleIndex]]}]
				,(*7*)First@CompensatedSquareError[ssigs,ssigs2,-1],First@CompensatedQuadError[ssigs,ssigs2,0],First@CompensatedCorrelationError[ssigs,ssigs2,0],
				(*10*)ReweightedSquareError[ssigs,ssigs2],(*11*)First@compositeError[ssigs,ssigs2,0],
				(*12*)(*First@compositeError2[ssigs,ssigs2,0]*)Min[First@compositeError2[ssigs,ssigs2,0],First@compositeError2[ssigs,{-#[[1]],-#[[2]],#[[3]]}&/@ssigs2,0]]
				,(*13*)wifiSetDistance[pickWifis[wifiss[[hayIndex]],ztss[[hayIndex,haySpan]]],pickWifis[wifis2,Partition[zts2,windowLength][[needleIndex]]]]}
			]]&@With[{min=Min[#]},{Position[#,min][[1,1]],min}]&@(wifiSetDistance[pickWifis[wifiss[[hayIndex]],#],pickWifis[wifis2,zts2s[[needleIndex,scaleIndex]]]]&/@Partition[ztss[[hayIndex]],windowLength]),{hayIndex,Length@sigss}]
	,{needleIndex,Length@sigs2s},{scaleIndex,Length@scales}],2];
	SortBy[#,#[[5]]&][[1]]&/@GatherBy[ars,#[[2]]&]
	]];
Dynamic[{hayIndex,needleIndex}]
selectedBests=testWifiMatch[fname,150];//AbsoluteTiming
precisionRecallMatrixOfMatches2=Function[{selectedBests,th,thMarker},
	With[{tp=Length@Select[selectedBests,#[[5]]<th&&#[[6]]<thMarker&],fp=Length@Select[selectedBests,#[[5]]<th&&#[[6]]>thMarker&],fn=Length@Select[selectedBests,#[[5]]>th&&#[[6]]<thMarker&]},{tp,fp,fn}]];
{calcPrecisionRecall@#,Append[#,Length@sigs2s-Tr@#]}&@precisionRecallMatrixOfMatches2[selectedBests,(*100*)-100,5]


(*model=LogitModelFit[data,features,features];*)
features={square0,square1,quad,corr,reweighted(*,composite,composite2*)};fields={"AIC", "BIC", "LikelihoodRatioIndex", "PearsonChiSquare", "BestFit"};
sub=Table[Join[{i}, LogitModelFit[data, i, features][fields]], {i, Subsets[features]}];
Grid[Join[{Prepend[fields,"Model"]}, SortBy[sub, -#[[2]] &]]]
model=LogitModelFit[data,SortBy[sub,-#[[2]]&][[-1,1]],features]


r=Function[th,
	Module[{pre=Boole[#<th]&/@data[[;;,7]],p=data[[;;,-1]],tp},tp=Count[p pre,1];
	{calcPrecisionRecall@#,#}&@{tp,Count[pre,1]-tp,Count[p,1]-tp}]]/@{10,15,25,50,75,100,125,150};r//Grid
Export["t.png",Binarize@Rasterize[Prepend[Flatten/@r,{"precision","recall","TP","FP","FN"}]//TableForm]]


testsuite=JPNfnames[[;;,-1]];testsuite=fnames[[;;,-1]];
windowLength=150;needleCounts=Parallelize[Length@Partition[First@LoadMagModel2@#,windowLength]&/@testsuite];//AbsoluteTiming
aars=Parallelize[Function[fname,((*Print@FileBaseName@fname;*)(Print@{FileBaseName@fname,#[[1]]};#[[2,1]])&@AbsoluteTiming@testFileMagMatch[fname,False,windowLength])]/@testsuite,DistributedContexts->All];//AbsoluteTiming
Export["t2.csv",Prepend[Prepend[#[[7;;]],#[[5]]]~Join~{(*Boole[*)#[[6]]<5(*]*)}&/@Join@@aars,
	{"square_0","square_-1","quad","corr","reweighted","composite","composite2","wifi","world_square_0","world_hausdorff","hausdorff","<5"}]]
data=Prepend[#[[7;;]],#[[5]]]~Join~{Boole[#[[6]]<5]}&/@Join@@aars;data[[1]]


(*Relation of time with file sizes*)(*r={#[[2,1]],Tr[FileBaseName/@modelfiles@#[[2,1]]/.szs],#[[1]]}&/@rs;r//MatrixForm
ListPlot[Transpose@{r[[;;,2]]10^-7,r[[;;,3]]}]*)


ors=rs;ors//TableForm


rs-ors//TableForm


rs=MapThread[{FileBaseName@#2,#3}~Join~(#~Join~calcPrecisionRecall@#&@precisionRecallMatrixOfMatches[#,50,5]&@#)&,{aars,testsuite,needleCounts}];
{calcPrecisionRecall@#~Join~#&@(Tr/@Transpose@rs[[;;,3;;5]]),calcPrecisionRecall@#~Join~#&@(Tr/@Transpose@#[[;;,3;;5]])&@Select[rs,StringMatchQ[#[[1]],"JPN"~~__]&]}
{N@Tr@rs[[;;,3]]/Tr@rs[[;;,2]],N@rs[[;;,3]]/rs[[;;,2]]}
Prepend[#,First@First@Position[FileBaseName/@Last/@fnames,#[[1]]]]&/@rs//TableForm
Export["t.png",Binarize@Rasterize[Prepend[SortBy[rs,First],{"level","#needle","TP","FP","FN","precision","recall"}]//TableForm]]


pickWifis=Function[{wifis,lowerUpper},Union@@Select[wifis,lowerUpper[[1]]<=First@#<=lowerUpper[[-1]]&][[;;,2,;;,1]]];
wifis2=LoadWifi@fname;wifiss=LoadWifi/@modelfnames;
Function[m,Length@Intersection[#,#2]/Length@Union[#,#2]&[pickWifis[wifiss[[m[[1]]]],ztss[[m[[1]],m[[4]]]]],pickWifis[wifis2,Partition[zts2,windowLength][[m[[2]]]]]]]/@selectedBests


toWifiSigNeedles=Function[fname,Module[{wifis,intervalNumbering},
	wifis=LoadWifi@fname;intervalNumbering=Interpolation[Join@@MapIndexed[{{First@#,First@#2},{Last@#,First@#2}}&,{First@#,Last@#}&/@Partition[zts2,150]],InterpolationOrder->1];
	Union@@@GatherBy[Select[{#,intervalNumbering@First@#}&/@wifis,Floor[#[[2]]]==#[[2]]&],Last][[;;,;;,1,2,;;,1]]]];
wifisigs2=toWifiSigNeedles@fname;
MatrixPlot@Outer[Length@Intersection[#,#2]/Length@Union[#,#2]&,wifisigs2,wifisigs2,1]


fileTimestamp=ReadList[#,Number,1][[1]]&;


Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,sigs2,positions,positions2,positions3,qmags2,ncfqmags2},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	(*positions=Position[Tr@Variance@#&/@Partition[zmags2,10],x_/;x>80];
	qmags2=MapThread[RotateByQuaternion,{zmags2,ncfqs2}];
	positions2=Position[Tr@Variance@#&/@Partition[qmags2,10],x_/;x>80];
	ncfqmags2=MapThread[RotateByQuaternion,{zmags2,ncfqs2}];*)
	(*positions3=Position[Tr@Variance@#&/@Partition[ncfqmags2,10],x_/;x>200];*)
	(*positions3=Position[Tr[MedianDeviation[#]^2]&/@Partition[ncfqmags2,10],x_/;x>200];*)
	(*positions3=Position[Total[Norm/@(#-GaussianFilter[#,100])&/@Transpose[RotateByQuaternion[{0,1,-1},#]&/@qs2]],x_/;x>2.5];*)
	positions3=Position[Differences[Arg[Complex@@#]&/@Differences@mxys2],x_/;x>4];
	Graphics[{Opacity[0.1],(*Blend*)Line@mxys2,Opacity[1],
		(*Red,Sequence[Point/@Extract[mxys2,positions]],*)
		Red,Sequence[Point/@Extract[mxys2,positions3]]}]]]/@JPNfnames[[;;,-1]]](*modelfiles@JPNfnames[[-1,-1]]]*)


qmags2=MapThread[RotateByQuaternion,{zmags2,qs2}];
ListPlot@Transpose@qmags2
ncfqmags2=MapThread[RotateByQuaternion,{zmags2,ncfqs2}];
ListPlot@Transpose@ncfqmags2


PlotBearingInPosition=Function[{ncfqs2,mxys2,n},
	Graphics[{Opacity[0.1],Line@Standardize[LatLongToXY[mxys2],Mean,1&],Opacity[1]}~Join~(
		Join@@MapThread[FireMatch[#,#+5#2]&,
		{Standardize[LatLongToXY[mxys2[[;;;;n]]],Mean,1&],Most@RotateByQuaternion[{0,1,0},#]&/@ncfqs2[[;;;;n]]}])]];


ListPlot[Total[Norm/@(#-GaussianFilter[#,100])&/@Transpose[RotateByQuaternion[{0,1,-1},#]&/@qs2]],PlotRange->All]


ListLinePlot[Arg[Complex@@#]&/@Differences@mxys2,PlotRange->All]
ListLinePlot[Differences[GaussianFilter[Arg[Complex@@#]&/@Differences@mxys2,10]],PlotRange->All]


Image[#,ImageSize->1000]&@Graphics[Point/@mxys2]


toClusterCenters=#[[Floor[(1+Length@#)/2]]]&/@SplitBy[#,Round[First@#/10]&]&;


Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,sigs2,positions,positions2,positions3,qmags2,ncfqmags2,n=20},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	Image[#,ImageSize->Large]&@Graphics@MapThread[FireMatch[#,#+4#2]&,
		{(Most@mxys2)[[;;;;n]],(MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,qs2}])[[;;;;n]]}]
	]]/@(*JPNfnames[[;;2,-1]]*)modelfiles@fnames[[16,-1]]]


Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,sigs2,positions,positions2,positions3,qmags2,ncfqmags2,n=20},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	Image[#,ImageSize->Large]&@Graphics@MapThread[FireMatch[#,#+4#2]&,
		{(Most@mxys2)[[;;;;n]],(MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}])[[;;;;n]]}]
	]]/@(*JPNfnames[[;;2,-1]]*)modelfiles@fnames[[16,-1]]]


Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,sigs2,positions,positions2,positions3,qmags2,ncfqmags2,n=20},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	Image[#,ImageSize->Large]&@Graphics@MapThread[FireMatch[#,#+4#2]&,
		{(Most@mxys2)[[;;;;n]],({Re@#,Im@#}&/@(Complex@@@(Most/@Most@zmags2) (Normalize@Conjugate@#&/@(Complex@@@(Differences@mxys2)))))[[;;;;n]]}]
	]]/@(*JPNfnames[[;;2,-1]]*)modelfiles@fnames[[16,-1]]]


(*positions3=Position[Differences[GaussianFilter[Arg[Complex@@#]&/@Differences@mxys2,10]],x_/;x>0.05];*)
positions3=toClusterCenters@Position[Tr[SingularValueList[#][[3;;]]]&/@Partition[qs2[[;;]],10,1],x_/;x>0.1];
Image[#,ImageSize->1000]&@Graphics[{Opacity[0.3],BlendLine@mxys2,Opacity[1],Red,Sequence[Point/@Extract[mxys2,positions3]]}]


ListPlot[Boole[#>0.2]&/@(Tr[SingularValueList[#][[3;;]]]&/@Partition[qs2[[;;]],10,1]),PlotRange->All]


ListPlot[Tr[SingularValueList[#][[3;;]]]&/@Partition[qs2[[;;]],10,1],PlotRange->All]


ListLinePlot[Transpose[SingularValueList/@Partition[qs2[[;;]],10]][[3;;]],PlotRange->All]


ListPlot@Transpose[RotateByQuaternion[{0,1,-1},#]&/@qs2[[1;;1000]]]


part=1;;1000
ListPlot@Transpose[RotateByQuaternion[{0,1,-1},#]&/@ncfqs2[[part]]]
ListPlot@Transpose[RotateByQuaternion[{0,1,-1},#]&/@qs2[[part]]]
ListPlot@Transpose[zmags2[[part]]]


PlotBearingInPosition[ncfqs2,mxys2,50]
Image[#,ImageSize->Large]&/@Table[PlotBearingInPosition[ncfqss[[i]],mxyss[[i]],20],{i,Length@mxyss}]


Image[#,ImageSize->1000]&@PlotBearingInPosition[ncfqs2,mxys2,20]


Image[#,ImageSize->1000]&@Graphics[FireMatch@@@Partition[mxys2[[;;;;20]],2]]


ListPlot[Tr[MedianDeviation[#]^2]&/@Partition[zmags2,10],PlotRange->All]


ListPlot[Tr@StandardDeviation@#&/@Partition[zmags2,10],PlotRange->All]
ListPlot[Tr@Variance@#&/@Partition[qmags2,10],PlotRange->All]
ListPlot[Tr@Variance@#&/@Partition[ncfqmags2,10],PlotRange->All]


Parallelize[BubbleChart@MapThread[Append,{Standardize[Quiet[mxyf2/@wifis2[[;;,1]]],Mean,1&],-0.01#}]&/@Transpose@ms[[1,;;,;;]]]


ListLinePlot/@Transpose[ms[[1]]/.{0->-100}]


mapping=MacsToIds[Join@@Prepend[wifiss,wifis2]];
ms=WifisToMatrix[#,mapping]&/@Prepend[wifiss,wifis2];
(Join@@Riffle[ms,Table[10,{Length@ms},{3},{Length@ms[[1,1]]}]])//Transpose//MatrixPlot//Image[#,ImageSize->1000]&
booleanize2=Map[Boole[#!=0]&,#,{2}]&;
booleanize=Map[Boole[#!=0]&,#,{1}]&;
booleanize@Total[booleanize2@#]&/@ms//MatrixPlot
Total[booleanize2@#]&/@ms//MatrixForm


useScales=False;windowLength=150;
fname=Last@fnames[[16]];(*fname="~/zsc/JPN_tok_m_machida_jorna_2_katsuyam_survey.txt";*)(*fname=Last@JPNfnames[[9]];*)(*fname="~/zsc/ZRH_terminale_CH_1_dcarrelli_survey.txt";*)modelfnames=modelfiles@fname;{fname,modelfnames}
{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=Quiet[mxyf2/@zts2];
{zmagss,zxyss,zaxyzss,ztss,mxyfs,wifiss,qss,ncfqss}=Transpose@Map[LoadMagModel2,modelfnames];mxyss=MapThread[Quiet@Map[#,#2]&,{mxyfs,ztss}];
g2=Graphics[{Blue,Opacity[0.1],Line[Reverse/@mxys2]}];gs=Parallelize@Map[Graphics[{Red,Opacity[0.1],Line[Reverse/@#]},Axes->True]&,mxyss];//AbsoluteTiming
Prepend[MapIndexed[{First@#2,#}&,Graphics[BlendLine@LatLongToXY[#[[;;;;50]]],Axes->True]&/@mxyss],Graphics[BlendLine@LatLongToXY[mxys2[[;;;;50]]],Axes->True]]
{Graphics@BlendLine@zxys2,ListPlot@Transpose@MapThread[RotateByQuaternion,{zaxyzs2[[;;;;10]],ncfqs2[[;;;;10]]}],ListPlot[Transpose@MapThread[RotateByQuaternion,{zmags2[[;;;;10]],ncfqs2[[;;;;10]]}],AxesLabel->{"n","mag"}]}
{qsigs2=MapThread[RotateByQuaternion,{zmags2,ncfqs2}];//AbsoluteTiming,qsigss=Parallelize@MapThread[MapThread[RotateByQuaternion,{#,#2}]&,{zmagss,ncfqss}];//AbsoluteTiming}
{sigs2=FactorizeByGravityYPointingDirs[zmags2,zaxyzs2,10];//AbsoluteTiming,sigss=Parallelize@MapThread[FactorizeByGravityYPointingDirs[#,#2,10]&,{zmagss,zaxyzss}];//AbsoluteTiming}
scales=If[useScales,Table[Floor[windowLength k],{k,0.7,1.4,0.05}],{windowLength}];toNeedles=If[useScales,Table[ResampleWithRate[#,k],{k,0.7,1.4,0.05}],List@#]&/@Partition[#,windowLength]&;
{sigs2s,qsigs2s,mxys2s,zxys2s}=toNeedles/@{sigs2,qsigs2,mxys2,zxys2};
ars=Flatten[Parallelize[Table[Table[With[{haySpan=#[[1]];;#[[1]]+scales[[scaleIndex]]-1,cost=#[[2]]},With[{ssigs=sigss[[hayIndex,haySpan]],ssigs2=sigs2s[[needleIndex,scaleIndex]]},
			{hayIndex,needleIndex,scaleIndex,haySpan,cost,Min@MapThread[LatLongDistance,{mxyss[[hayIndex,haySpan]],mxys2s[[needleIndex,scaleIndex]]}]
				(*HausdorffDistance[mxyss[[hayIndex,haySpan]],mxys2s[[needleIndex,scaleIndex]],LatLongDistance]*)
				,(*7*)First@CompensatedSquareError[ssigs,ssigs2,-1],First@CompensatedQuadError[ssigs,ssigs2,0],First@CompensatedCorrelationError[ssigs,ssigs2,0],
				(*10*)ReweightedSquareError[ssigs,ssigs2],(*11*)First@compositeError[ssigs,ssigs2,0]
				,(*12*)(*First@compositeError2[ssigs,ssigs2,0]*)Min[First@compositeError2[ssigs,ssigs2,0],First@compositeError2[ssigs,{-#[[1]],-#[[2]],#[[3]]}&/@Reverse@ssigs2,0]]
				,(*13*)wifiSetDistance[pickWifis[wifiss[[hayIndex]],ztss[[hayIndex,haySpan]]],pickWifis[wifis2,Partition[zts2,windowLength][[needleIndex]]]]
				,(*14*)First@CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex,haySpan]],-1]
				,(*15*)HausdorffDistance[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex,haySpan]],Dot[#-#2,#-#2]&]
				,(*16*)HausdorffDistance[sigs2s[[needleIndex,scaleIndex]],sigss[[hayIndex,haySpan]],Dot[#-#2,#-#2]&]}
			]]&@With[{min=Min[#]},{Position[#,min][[1,1]],min}]&@
				(*CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]*)
				MapThread[Min,{CompensatedSquareError[qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]
					,CompensatedSquareError[{-#[[1]],-#[[2]],#[[3]]}&/@Reverse@qsigs2s[[needleIndex,scaleIndex]],qsigss[[hayIndex]],0]}],{hayIndex,Length@sigss}]
	,{needleIndex,Length@sigs2s},{scaleIndex,Length@scales}],DistributedContexts->All],2];//AbsoluteTiming
selectedBests=Select[(SortBy[#,#[[12]]&][[1]]&/@GatherBy[ars,#[[2]]&]),#[[5]]<800&];
{calcPrecisionRecall@#,Append[#,Length@sigs2s-Tr@#]}&@precisionRecallMatrixOfMatches[selectedBests,(*100*)50,5]


dispMatch/@Select[selectedBests,(#[[12]]<50||#[[5]]<50)&&#[[6]]>5&]


dispMatch/@Select[selectedBests,#[[12]]<50&&#[[6]]>5&][[;;]]


dispMatch/@Select[selectedBests,#[[12]]>50&&#[[6]]<5&][[;;]]


matches=Select[selectedBests,#[[12]]<50&&#[[6]]>5&];Dimensions@matches
(*MapIndexed[{First@#2,#}&,dispMatch/@matches]*)


(*g=GraphicsColumn@{Graphics[BlendLine@Standardize[LatLongToXY@mxys2,Mean,1&],PlotLabel->Style["GroundTruth",Larger],Axes->True,PlotRange->{{-25,25},{-35,25}}],
	Graphics[BlendLine@Standardize[zxys2,Mean,1&],PlotLabel->Style["DeadReckoning",Larger],Axes->True,PlotRange->{{-25,25},{-35,25}}]}
Export["t.png",g(*,ImageSize->300*)];*)


checkMatch=Function[{m,sigss,mxyss,sigs2s,mxys2s},{mxyss[[m[[1]],m[[4]]]],sigss[[m[[1]],m[[4]]]],mxys2s[[m[[2]],m[[3]]]],sigs2s[[m[[2]],m[[3]]]]}];
{k=12,matches[[k]],matches[[k,5]]}
dispMatch@matches[[k]]
{smxys,ssigs,smxys2,ssigs2}=checkMatch[matches[[k]],sigss,mxyss,sigs2s,mxys2s];
{Graphics[{Blue,Line@smxys,Red,Line@smxys2}],ListPlot[{Norm/@(ssigs-ssigs2),Norm/@ssigs-Norm/@ssigs2}],ListPlot[(Abs/@Transpose[ssigs-ssigs2]),PlotRange->{Automatic,{0,20}}]}~Join~
	(ListPlot[#,PlotRange->{Automatic,{-40,40}}]&/@Transpose[Transpose/@{ssigs2,ssigs}])
{Mean[Norm/@(ssigs-ssigs2)],Mean@Abs[Norm/@ssigs-Norm/@ssigs2]}


ls[[;;,2]]
ListLinePlot@{ls[[;;,2]],Cos[ls[[;;,2]]]}
FoldList[If[Abs[#2-#]<Abs[#2+Pi-#],If[Abs[#2-#]<Abs[#2-Pi-#],#2,#2-Pi],#2+Pi]&,0,ls[[;;,2]]]
ListLinePlot[FoldList[If[Abs[#2-#]<Abs[#2+Pi-#],If[Abs[#2-#]<Abs[#2-Pi-#],#2,#2-Pi],#2+Pi]&,0,ls[[;;,2]]],PlotRange->All]


ls=trans@#[[2]]&/@Select[selectedBests,#[[12]]<50&];
ListLinePlot[ls[[;;,2]],PlotRange->All,PlotMarkers->"x"]
ListLinePlot[ls[[;;,3]],PlotRange->All,PlotMarkers->"x"]
ListLinePlot[ls[[;;,4]],PlotRange->All,PlotMarkers->"x"]


magMatches2=Parallelize[Prepend[Image[#,ImageSize->Medium]&/@testFileMagMatch2[#,False,150],#]&/@fnames[[;;,-1]],DistributedContexts->All];//AbsoluteTiming
Export["t.pdf",magMatches2//TableForm]//AbsoluteTiming


RotationMatrix2dToScaleArg=Function[mat,With[{scale=Power[Det[mat],1./Length@mat]},{scale,ArcTan@@{#[[1]],-#[[2]]}&@First[mat/scale]}]];
TransformationMatrix2dToScaleArgBias=Function[mat,With[{scale=Sqrt@Det@mat[[1;;2,1;;2]]},{scale,ArcTan@@{#[[1]],-#[[2]]}&@First[mat[[1;;2,1;;2]]/scale],mat[[;;2,3]]}]];
ScaleArgBiasToTransformationMatrix=Function[{scale,arg,bias},Transpose[Transpose[scale RotationMatrix[arg]]~Join~{bias}]];
ScaleArgBiasToTransformationFunction=Function[trans,With[{m=ScaleArgBiasToTransformationMatrix[trans[[1]],trans[[2]],trans[[3;;4]]]},Function[x,m.Append[x,1]]]];
FindScaleArgBias2D=Function[{pts,pts2},(*transformation from pts2 to pts*)
	Module[{d2,d,r},{d2,d}=First@Transpose@Last@SingularValueDecomposition[Standardize[#,Mean,1&],1]&/@{pts2,pts};
	r=UniqueRotationMatrix[Append[d2,0],{0,0,1.},Append[d,0],{0,0,1.}];
	Append[RotationMatrix2dToScaleArg@r,Mean@pts(*-Most[r.Append[Mean@pts2,0]]*)]
	]];
testFileMagMatch2=Function[{fname,useScales,windowLength},
	Module[{selectedBests,positives,mxyss,mxys2,mxys2s,zxys2,zxys2s,scaling,mgs,mgs2,mags,l,trans,zgs,known,g,g2,pos},
	{selectedBests,mxyss,mxys2,mxys2s,zxys2,zxys2s}=testFileMagMatch[fname,useScales,windowLength];
	positives=Select[selectedBests,#[[12]]<50&];
	scaling=Cos[Mean[(mxyss[[1]][[All,1]] \[Pi])/(180 10000000)]];
	mgs2=SingleLatLongToXY[#,scaling]&/@mxys2s[[#[[2]],#[[3]]]]&/@positives;
	mgs=SingleLatLongToXY[#,scaling]&/@#&/@(mxyss[[#[[1]],#[[4]]]]&/@positives);
	l=Function[m,{m[[2]],Flatten@FindScaleArgBias2D[SingleLatLongToXY[#,scaling]&/@mxyss[[m[[1]],m[[4]]]],zxys2s[[m[[2]],m[[3]]]]]}]/@positives;
	trans=Interpolation[l,InterpolationOrder->1];
	zgs=ScaleArgBiasToTransformationFunction@trans[#[[2]]]/@Standardize[zxys2s[[#[[2]],#[[3]]]],Mean,1&]&/@positives;
	known=#[[2]]&/@Select[selectedBests,#[[12]]<50&];pos=MapThread[Rule,{known,zgs}];
	g=Join@@({Hue[0.7(1-#[[2]]/Length@sigs2s)],Line[ScaleArgBiasToTransformationFunction@trans[#[[2]]]/@Standardize[zxys2s[[#[[2]],#[[3]]]],Mean,1&]]}&/@positives);
	g2=Join@@(With[{xys=Join@@zxys2s[[#[[2;;-2]],1]]},
		{Hue[0.7(1-#[[1]]/Length@zxys2s)],Line[Last@FindGeometricTransform[{Last[#[[1]]/.pos],First[#[[-1]]/.pos]},{First@xys,Last@xys},"Transformation"->"Rigid"]/@xys]}]&/@
		Select[Range@@@Partition[known,2,1],Length@#>=3&]);
	{Graphics[BlendLine[SingleLatLongToXY[#,scaling]&/@mxys2],Axes->True,PlotLabel->"GroundTruth"],Graphics[BlendLine@zxys2,PlotLabel->"DeadReckoning"],
		Graphics[g,Axes->True,PlotLabel->"Matched"],Graphics[g2,Axes->True,PlotLabel->"Extrapolated"],Show@{Graphics[g,Axes->True,PlotLabel->"Matched+Extrapolated"],Graphics@g2}}
]];


scaling=Cos[Mean[(mxyss[[1]][[All,1]] \[Pi])/(180 10000000)]];
mgs2=SingleLatLongToXY[#,scaling]&/@mxys2s[[#[[2]],#[[3]]]]&/@Select[selectedBests,#[[12]]<50&];
mgs=SingleLatLongToXY[#,scaling]&/@#&/@(mxyss[[#[[1]],#[[4]]]]&/@Select[selectedBests,#[[12]]<50&]);
l=Function[m,{m[[2]],Flatten@FindScaleArgBias2D[SingleLatLongToXY[#,scaling]&/@mxyss[[m[[1]],m[[4]]]],zxys2s[[m[[2]],m[[3]]]]]}]/@Select[selectedBests,#[[12]]<50&];
trans=Interpolation[l,InterpolationOrder->1];
zgs=ScaleArgBiasToTransformationFunction@trans[#[[2]]]/@Standardize[zxys2s[[#[[2]],#[[3]]]],Mean,1&]&/@Select[selectedBests,#[[12]]<50&];
(*Thread@{Graphics/@Thread@{(*Opacity[0.5],*)Blue,Line/@mgs2,Red,Line/@mgs,Darker@Yellow,Line/@zgs},#[[2]]&/@Select[selectedBests,#[[12]]<50&]}*)
known=#[[2]]&/@Select[selectedBests,#[[12]]<50&];pos=MapThread[Rule,{known,zgs}];
g=Graphics[Join@@({Hue[0.7(1-#[[2]]/Length@sigs2s)],Line[ScaleArgBiasToTransformationFunction@trans[#[[2]]]/@Standardize[zxys2s[[#[[2]],#[[3]]]],Mean,1&]]}&/@Select[selectedBests,#[[12]]<50&]),Axes->True];
g2=Graphics[Join@@(With[{xys=Join@@zxys2s[[#[[2;;-2]],1]]},
	{Hue[0.7(1-#[[1]]/Length@sigs2s)],Line[Last@FindGeometricTransform[{Last[#[[1]]/.pos],First[#[[-1]]/.pos]},{First@xys,Last@xys},"Transformation"->"Rigid"]/@xys]}]&/@
	Select[Range@@@Partition[known,2,1],Length@#>=3&]),Axes->True];
{Graphics[BlendLine[SingleLatLongToXY[#,scaling]&/@mxys2],Axes->True],Graphics@BlendLine@zxys2,g,g2,Show@{g,g2}}


(*mgs2=SingleLatLongToXY[#,scaling]&/@mxys2s[[#,1]]&/@Range@Length@mxys2s;
zgs=MapIndexed[ScaleArgBiasToTransformationFunction@trans[First@#2]/@Standardize[#,Mean,1&]&,zxys2s];
known=#[[2]]&/@Select[selectedBests,#[[12]]<50&];
r=MapIndexed[{If[MemberQ[known,#],Labeled[Framed[#],"Known"],#]&@First@#2,#}&,Graphics/@Thread@{(*Opacity[0.5],*)Blue,Line/@mgs2,(*Red,mgs,*)Darker@Yellow,Line/@zgs}]*)


(*BlendPlot[Join@@MapThread[#[#2]&,{MapThread[Last@FindGeometricTransform[#,#2,"Transformation"->"Similarity"]&,{(Reverse/@mxyf/@zts)[[#]]&/@First/@r7,Partition[zxys2,150]}],Partition[zxys2,150]}]]*)
(*Graphics[Point/@zxys2]
Graphics[Point@mxyf2@#&/@zts2]*)
(*pos=Position[r,x_/;ListQ[x]&&x[[1,3]]<300,1];
ListPlot@Extract[Partition[zxys2,150],pos]
Extract[r,pos]*)


xs=Range[30];hs=Range[10];zys=ListConvolve[hs,xs];
{Length@zys,ListPlot@zys}


NN=20;M=Length@hs;Nx=Length@xs;fftN=Function[{xs,N},Fourier[PadRight[xs,N]]];
Hs=fftN[hs,NN];ys=Table[0,{Nx+NN-M}];
Do[Module[{il=Min[i+NN-1,Nx]},
	ys[[i;;i+NN-M]]=InverseFourier[fftN[xs[[i;;il]],NN]Hs][[M;;NN]]],{i,Nx}];
{ListPlot@ys,ListPlot@ys[[;;Length@zys]],Length@ys}


testFileJava=Function[{fname,modelfname},
	First@Import["!~/bin/main_deploy.jar --input_trace_filename="<>AbsoluteFileName[fname]<>
		" --input_trace_filename2="<>AbsoluteFileName[modelfname]<>" --outputFilename=/dev/null --task=doBatchMatch --stepDetector=com.google.mobile.dmall.sensor.step.FrequencyStepDetector "<>
		"--mag_sig_match_threshold=4000 --batch_match_window_span=150 2>/dev/null","Table"]];
N[#[[;;-2]]/#[[-1]]]&@testFileJava["ABQ3_3.txt","ABQ3_5.txt"]


Parallelize@Map[Outer[If[FileByteCount[#]>FileByteCount[#2],testFileJava[#,#2,150,0.]]&,#,#,1]&@(First/@#)&,GatherBy[l,#[[2]]&][[;;2]]]


testStepCounter=Function[fname,
	Module[{zmags,zxys,zaxyzs,zts,mxyf,mxys,zxys2},
		{zmags,zxys,zaxyzs,zts,mxyf}=LoadMagModel2[fname];
		mxys=Standardize[LatLongToXY[mxyf/@zts],Mean,1&];
		zxys2=Interpolation[MapThread[List,Reverse@LoadXYs[fname,"--ontg_use_peak"]],InterpolationOrder->1]/@zts;
		{Graphics[{Line[zxys],Red,Line[mxys]}],
			ListPlot@{Accumulate[Norm/@Differences@zxys],Accumulate[Norm/@Differences@zxys2],Accumulate[Norm/@Differences@mxys]}}]];
r=Parallelize[Map[Flatten@{#,testStepCounter@#}&,Last/@fnames[[;;]]],DistributedContexts->All];//AbsoluteTiming
Export["t.png",(*Rasterize@*)Prepend[r,{"","","Freq,Peak,Marker"}]//TableForm]


fname=fnames[[19,-1]](*;fname="41.txt"*);fname="~/zsc/JPN_stm_d_marui_omiya_3_yujirok.txt";fname="~/zsc/JPN_tok_m_machida_jorna_2_katsuyam_survey.txt"
{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=Quiet[mxyf2/@zts2];//AbsoluteTiming
{Graphics@BlendLine@LatLongToXY@mxys2,Graphics@BlendLine@zxys2,ListPlot[Transpose@MapThread[RotateByQuaternion,{zaxyzs2[[;;;;10]],qs2[[;;;;10]]}]]}
dqs2=QuaternionMul[QuaternionConjugate@#[[1]],#[[2]]]&/@Partition[qs2,2,1];


rqs2=FoldList[QuaternionMul,{1,0,0,0},dqs2];
{ListPlot[Transpose@qs2],ListPlot[Transpose@dqs2],ListPlot[Transpose@rqs2],ListPlot[Transpose@MapThread[RotateByQuaternion,{zaxyzs2,qs2}]]}
tqf2=LoadNcfQuaternionF[fname,"--oftg_filter1_mag_gain=0.0 --oftg_filter2_mag_gain=0.0"];tqs2=tqf2/@zts2;
Append[{ListPlot[Transpose@tqs2],ListPlot[Transpose@MapThread[RotateByQuaternion,{zaxyzs2,tqs2}]]},Graphics@BlendLine@First@LoadNcfXYs[fname,"--oftg_filter1_mag_gain=0.0 --oftg_filter2_mag_gain=0.0"]]


With[{xys=nzxys2},
Manipulate[Graphics@BlendLine[{Re@#,Im@#}&/@Accumulate[Array[Exp[I # t/Length@xys]&,Length@xys-1] Complex@@@Differences@xys]],{t,-2Pi,2Pi,0.1}]]


rdqs2=QuaternionMul[QuaternionConjugate@#[[1]],#[[2]]]&/@Partition[Reverse@qs2,2,1];
kB=0.05;kX=Sqrt[2 kB];
init={Last@qs2,{0,0,0},{0,0,1}};
(*concentration=0;*)
r=Module[{nq},
	FoldList[With[{q=#[[1]],bias=#[[2]],antiGrav=#[[3]],dq=#2[[1]],accel=#2[[2]],dt=Max[-1.,#2[[3]]]},
	With[{edq=(*QuaternionConjugate@*)Matrix2Quat@SoraToMatrix[(bias-kX Cross[antiGrav,accel])dt]},
		(nq=QuaternionMul[edq,QuaternionMul[q,dq]];
			{nq,
			bias+kB Cross[antiGrav,accel]dt,
			RotateByQuaternion[{0,0,1},nq]})]]&
	,init,Thread@{rdqs2,Normalize/@Rest@Reverse@zaxyzs2,Differences@Reverse@zts2}]];
nqs=-Reverse@r[[;;,1]];biases=Reverse@r[[;;,2]];
Image[#,ImageSize->Medium]&/@{ListPlot[Transpose@nqs,AxesLabel->{"n","attitude"}],ListPlot[Transpose@biases,AxesLabel->{"n","bias"}],ListPlot[Transpose@zaxyzs2,AxesLabel->{"n","raw-gravity"}],
	ListPlot[Transpose@Reverse@r[[;;,3]],AxesLabel->{"n","rotated-gravity"}],ListPlot[Transpose@MapThread[RotateByQuaternion,{zaxyzs2,nqs}],AxesLabel->{"n","rotated-gravity2"}]}
nmags2=MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,nqs}];smoothed=GaussianFilter[nmags2,{{1000,0}}];
nzxyzs2=Accumulate[RotateByQuaternion[{0,1,0},#]&/@nqs];nzxys2=Most/@nzxyzs2;
snmags2={Re@#,Im@#}&/@(Normalize@Conjugate[Complex@@@smoothed] Complex@@@nmags2);
ListPlot/@{Transpose@smoothed,Transpose@snmags2,Transpose[Normalize/@snmags2]}
{Graphics@BlendLine@LatLongToXY@mxys2,Graphics3D@BlendLine[nzxyzs2],Graphics[BlendLine@nzxys2,Axes->True],Graphics@BlendLine[{Re@#,Im@#}&/@Accumulate[I Rest@Normalize@Conjugate[Complex@@@smoothed] Complex@@@Differences@nzxys2]]}


kB=0.05;kX=Sqrt[2 kB];
init={First@qs2,{0,0,0},{0,0,1}};
(*concentration=0;*)
r=Module[{nq},
	FoldList[With[{q=#[[1]],bias=#[[2]],antiGrav=#[[3]],dq=#2[[1]],accel=#2[[2]],dt=Min[1.,#2[[3]]]},
	With[{edq=(*QuaternionConjugate@*)Matrix2Quat@SoraToMatrix[(bias+kX Cross[antiGrav,accel])dt]},
		(nq=QuaternionMul[edq,QuaternionMul[q,dq]];
			{nq,
			bias+kB Cross[antiGrav,accel]dt,
			RotateByQuaternion[{0,0,1},nq]})]]&
	,init,Thread@{dqs2,Normalize/@Rest@zaxyzs2,Differences@zts2}]];
nqs=r[[;;,1]];biases=r[[;;,2]];
Image[#,ImageSize->Medium]&/@{ListPlot[Transpose@nqs,AxesLabel->{"n","attitude"}],ListPlot[Transpose@biases,AxesLabel->{"n","bias"}],ListPlot[Transpose@zaxyzs2,AxesLabel->{"n","raw-gravity"}],
	ListPlot[Transpose@r[[;;,3]],AxesLabel->{"n","rotated-gravity"}],ListPlot[Transpose@MapThread[RotateByQuaternion,{zaxyzs2,nqs}],AxesLabel->{"n","rotated-gravity2"}]}
nmags2=MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,nqs}];smoothed=GaussianFilter[nmags2,{{1000,0}}];
nzxyzs2=Accumulate[RotateByQuaternion[{0,1,0},#]&/@nqs];nzxys2=Most/@nzxyzs2;
snmags2={Re@#,Im@#}&/@(Normalize@Conjugate[Complex@@@smoothed] Complex@@@nmags2);
ListPlot/@{Transpose@smoothed,Transpose@snmags2,Transpose[Normalize/@snmags2]}
{Graphics@BlendLine@LatLongToXY@mxys2,Graphics3D@BlendLine[nzxyzs2],Graphics[BlendLine@nzxys2,Axes->True],Graphics@BlendLine[{Re@#,Im@#}&/@Accumulate[I Rest@Normalize@Conjugate[Complex@@@smoothed] Complex@@@Differences@nzxys2]]}


cs=Normalize[Plus@@({1,I}#)]&/@Differences@nzxys2;dcs=Divide[#[[2]],#[[1]]]&/@Partition[cs,2,1];
(*Note we starts with I.*)(*Chop[FoldList[Times,First@cs,dcs]-cs]*)
nmags2=MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,nqs}];
kB2=0.0;kZ=Sqrt[2 kB2];
r=Module[{nc},
	FoldList[With[{c=#[[1]],bias=#[[2]],magDir=#[[3]],dc=#2[[1]],mag=#2[[2]],dt=#2[[3]]},
	With[{edc=Exp[(bias+kZ Cross2[magDir,mag])dt I]},
		(nc=Normalize[c dc/edc];
			{nc,
			bias+kB2 Cross2[magDir,mag]Abs[dt],
			{Re@#,Im@#}&@(nc)})]]&
	,{First@cs,0,{0,1}},Thread@{dcs,(Normalize/@nmags2)[[3;;]],Rest@Differences@zts2}]];
Image[#,ImageSize->Medium]&/@{ListPlot[Transpose[{Re@#,Im@#}&/@r[[;;,1]]],AxesLabel->{"n","dir"}],
	ListPlot[r[[;;,2]],AxesLabel->{"n","bias"}],ListPlot[Transpose@nmags2,AxesLabel->{"n","raw-mag"}],ListPlot[Transpose@r[[;;,3]],AxesLabel->{"n","rotated-magDir"}],
	ListPlot[Transpose[{Re@#,Im@#}&/@((Plus@@({1,I}#)&/@Rest@nmags2) r[[;;,1]])],AxesLabel->{"n","rotated-magDir2"}]}
Graphics@Line[{Re@#,Im@#}&/@Accumulate@r[[;;,1]]]


(*Try making trajectories sparse*)
fromArgs={Re@#,Im@#}&/@Accumulate[Exp[I #]&/@#]&;
gs=Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,cs,dcs},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	cs=Complex@@@Differences@zxys2;dcs=#[[2]]/#[[1]]&/@Partition[cs,2,1];
	{fname,Graphics[BlendLine[mxys2]],Graphics[BlendLine[zxys2]](*Graphics@Line[fromArgs@Accumulate[Arg/@dcs]],*)
	,Graphics@BlendLine[fromArgs[Arg@First@cs+Accumulate[If[Abs[#]<0.05,0,#]&/@Arg[dcs]]]]
	,Graphics@BlendLine[fromArgs[Arg@First@cs+Accumulate[If[Abs[#]<0.1,0,#]&/@Arg[dcs]]]]
	(*,Graphics@BlendLine[fromArgs[Arg@First@cs+Accumulate[If[Abs[#]<0.2,0,#]&/@Arg[dcs]]]]*)
	,Graphics@BlendLine[fromArgs[Arg@First@cs+Accumulate[SoftThreshold[0.1,#]&/@Arg[dcs]]]]
	(*Image@Graphics@Line[fromArgs@MeanShiftFilter[Accumulate[Arg/@dcs],100,1]]*)
	(*Graphics@BlendLine[fromArgs@MedianFilter[Accumulate[Arg/@dcs],10]],*)
	(*Graphics@BlendLine[fromArgs@MedianFilter[#-GaussianFilter[#,{{800,0}}],5]&@Accumulate[Arg@dcs]]*)}
	]]/@JPNfnames[[;;,-1]],DistributedContexts->All];//AbsoluteTiming
(*Export["t.pdf",gs//TableForm];//AbsoluteTiming*)


MemoryConstrained[gs=Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,cs,dcs,nmxys2,nzxys2},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	{nmxys2,nzxys2}={ToNormParam[Standardize[LatLongToXY@mxys2,Mean,1&],Length@zts2],ToNormParam[zxys2,Length@zts2]};
	Prepend[Image/@Join[Graphics@BlendLine@#&/@{LatLongToXY@mxys2,zxys2},
		{ListPlot[Accumulate@Arg[Complex@@@Differences@#]&/@{nmxys2,nzxys2},AxesLabel->{"n","Arg"}],ListPlot[Norm/@Differences@#&/@{nmxys2,nzxys2},AxesLabel->{"n","stride"}]}],fname]
	]]/@JPNfnames[[;;,-1]],DistributedContexts->All];,10^9];//AbsoluteTiming


ToNormParam=Function[{xys,numPoints},Module[{norms=Norm/@Differences@xys,tf},tf=SafeFirstOrderInterpolation@Thread@{Prepend[Accumulate@norms,0],xys};Table[tf@i,{i,1,Tr@norms,(Tr@norms-1)/numPoints}]]];
TrajDistance=Function[{traj,traj2},Mean[Abs/@(Subtract@@@Transpose[Arg[Complex@@@Differences@ToNormParam[#,Length@traj]]&/@{traj,traj2}])]];
TrajHausdorffDistance=Function[{traj,traj2},HausdorffDistance[#[[1]],#[[2]],Norm[#-#2]&]&[MapIndexed[{First@#2,#}&,Arg[Complex@@@Differences@ToNormParam[#,Length@traj]]]&/@{traj,traj2}]];
magRegularize=Function[{zmags2,ncfqs2,nzxys2,n},Module[{dcs=GaussianFilter[MapThread[Normalize@Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}],{{n,0}}]},
	Accumulate[{Re@#,Im@#}&/@(Conjugate[-I Complex@@@dcs] Complex@@@Differences@nzxys2)]]];
magSelfRegularize=Function[{zmags2,ncfqs2,n},Module[{dcs=GaussianFilter[MapThread[Normalize@Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}],{{n,0}}]},
	{Re@#,Im@#}&/@(Conjugate[-I Complex@@@dcs] Complex@@@MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}])]];


Graphics@BlendLine@#&/@{LatLongToXY@mxys2,zxys2}
{nmxys2,nzxys2}={ToNormParam[Standardize[LatLongToXY@mxys2,Mean,1&],Length@mxys2],ToNormParam[zxys2,Length@mxys2]};
{ListPlot[Norm/@(Subtract@@@Partition[#,2,1])&/@{nmxys2,nzxys2},AxesLabel->{"n","stride"}],ListPlot@Transpose@MapThread[RotateByQuaternion,{zmags2,ncfqs2}]}
(*ListPlot@Transpose@GaussianFilter[MapThread[RotateByQuaternion,{zmags2,ncfqs2}]*)


MemoryConstrained[gs=Parallelize[Function[fname,Module[{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2,mxys2,cs,dcs,nmxys2,nzxys2,nnzxys2,nnnzxys2,n4zxys2,ranges={100,150,300}},
	{zmags2,zxys2,zaxyzs2,zts2,mxyf2,wifis2,qs2,ncfqs2}=LoadMagModel2[fname];mxys2=mxyf2/@zts2;
	{nmxys2,nzxys2}={ToNormParam[Standardize[LatLongToXY@mxys2,Mean,1&],Length@zts2],ToNormParam[zxys2,Length@zts2]};
	{nnzxys2,nnnzxys2,n4zxys2}=magRegularize[zmags2,ncfqs2,nzxys2,#]&/@ranges;
	{fname,Sequence@@(Image@Graphics@BlendLine@#&/@{nmxys2,nzxys2,nnzxys2,nnnzxys2,n4zxys2}),Sequence@@(Join[ConditionalFramed[TrajDistance[nmxys2,#]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2},#==Min[#2]&]
		,ConditionalFramed[TrajHausdorffDistance[nmxys2,#]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2},#==Min[#2]&]])
		,Sequence@@(Image@ListPlot@Transpose@magSelfRegularize[zmags2[[;;;;3]],ncfqs2[[;;;;3]],#]&/@ranges)
		(*,Sequence@@(ListPlot[Accumulate/@{Arg[Complex@@@Differences@nmxys2],Arg[Complex@@@Differences@#]}]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2})*)}
	]]/@JPNfnames[[;;,-1]],DistributedContexts->All];,2 10^9];//AbsoluteTiming


Export["t2.pdf",gs//TableForm];//AbsoluteTiming


{nmxys2,nzxys2}={ToNormParam[Standardize[LatLongToXY@mxys2,Mean,1&],Length@zts2],ToNormParam[zxys2,Length@zts2]};
nnzxys2=magRegularize[zmags2,ncfqs2,nzxys2,300];nnnzxys2=magRegularize[zmags2,ncfqs2,nzxys2,500];n4zxys2=magRegularize[zmags2,ncfqs2,nzxys2,1000];
{fname,Sequence@@(Image@Graphics@BlendLine@#&/@{nmxys2,nzxys2,nnzxys2,nnnzxys2,n4zxys2}),Sequence@@(Join[ConditionalFramed[TrajDistance[nmxys2,#]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2},#==Min[#2]&]
		,ConditionalFramed[TrajHausdorffDistance[nmxys2,#]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2},#==Min[#2]&]])
		,Sequence@@(Image@ListPlot@Transpose@magSelfRegularize[zmags2[[;;;;3]],ncfqs2[[;;;;3]],#]&/@{300,500,1000})
		(*,Sequence@@(ListPlot[Accumulate/@{Arg[Complex@@@Differences@nmxys2],Arg[Complex@@@Differences@#]}]&/@{nzxys2,nnzxys2,nnnzxys2,n4zxys2})*)}


Manipulate[Graphics@BlendLine@magRegularize[zmags2,ncfqs2,nzxys2,c],{c,30,300,10}]


n=50;dcs=GaussianFilter[MapThread[Normalize@Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}],{{n,0}}];
{ListPlot@Transpose@dcs,ListPlot@Arg[Complex@@@dcs]}
ListPlot@Transpose@MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}]
ListPlot@Transpose[{Re@#,Im@#}&/@(Conjugate[-I Complex@@@dcs] Complex@@@MapThread[Most@RotateByQuaternion[#,#2]&,{zmags2,ncfqs2}])]
Graphics@BlendLine@magRegularize[zmags2,ncfqs2,nzxys2,n]



