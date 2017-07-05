(* ::Package:: *)

printTemporary=(#;)&;
resampleList = Function[{xys, xs}, Module[{f, oxs},
        Assert[Max[xs] == 1];
        Assert[Min[xs] == 0];
        f = Interpolation[xys, InterpolationOrder -> 1];
        oxs = xys[[;; , 1]];
        f /@ (xs Max[oxs])
    ]];
qdrDecomposition=Function[m,Module[{qr=QRDecomposition[m]},{ConjugateTranspose@qr[[1]], DiagonalMatrix[Diagonal[qr[[2]]]], LinearSolve[DiagonalMatrix[Diagonal[qr[[2]]]],qr[[2]]]}]];
matrixContourPlot3D=Function[m,Module[{xyzs=Join@@MapIndexed[Append[{#2[[2]],-#2[[1]]},#]&,m,{2}]},{ListContourPlot[xyzs],ListPlot3D@xyzs}]];
jacobian=Function[{ys,xs},Table[D[y,#]&/@xs,{y,ys}]];
structuredEdge=Function[img,Module[{tfname=CreateTemporary[]<>".jpg",ofname=CreateTemporary[]<>".jpg",img2},Export[tfname,img];
	Run["LD_LIBRARY_PATH=/home/zsc/bin/opencv/lib /home/zsc/bin/ocr_main.bin --task detect_edge --input "<>tfname<>" --input2 /home/zsc/d/chars74k/edge_model.yml --output "<>ofname];
	img2=Import@ofname;DeleteFile[{tfname,ofname}];img2]];
integerStringFixedLength=Function[{i,n},StringJoin[Append[Table["0",{Max[0,n-StringLength@#]}],#]]&@IntegerString[i]];
vandeMonde=Function[{v,n},Transpose@Table[v^j,{j,0,n-1}]];
klDivergence=Compile[{{p,_Real,1},{q,_Real,1}},Tr@Table[If[p[[i]]==0,0,p[[i]](Log[p[[i]]]-Log[q[[i]]])],{i,Length@q}]];
dropWhile=Function[{lst,crit},Module[{i=1},Do[If[Not@crit[lst[[i]]],Break[],i+=1],{Length@lst}];lst[[i;;]]]];
importGb2312=Function[fname,Module[{tmp=CreateTemporary[]},Run["/usr/bin/iconv -f gb2312 -t utf8 "<>fname<>" -o "<>tmp];Import[tmp,"Lines",CharacterEncoding->"UTF-8"]]];
shoelaceFormula=Function[pts,0.5 Abs[pts[[;;,1]].RotateLeft[pts[[;;,2]],1]-pts[[;;,2]].RotateLeft[pts[[;;,1]],1]]];
eigenDecomposition=Function[m,{Transpose[#[[2]]],DiagonalMatrix[#[[1]]],Transpose@Inverse@#[[2]]}&@Eigensystem@m];
matrixPowerF=Function[m,With[{es=Eigensystem@m},Function[a,Transpose@LinearSolve[es[[2]],DiagonalMatrix[Power[es[[1]],a]].es[[2]]]]]];
imageCropToMultipleDivisorSize=Function[{img,divisor},ImageTake[img,Sequence@@Reverse@Transpose[{{1,1},divisor Floor[ImageDimensions[img]/divisor]}]]];
standardizeMatrixAdaptive=Function[{m,radius},Module[{ker,r=2radius+1,mu,std},ker=N@ConstantArray[1,r{1,1}]/r/r;
	mu=ListConvolve[ker,m,{1,-1},0][[radius+1;;-radius-1,radius+1;;-radius-1]];std=0.0001+Sqrt[ListConvolve[ker,(m-mu)^2,{1,-1},0][[radius+1;;-radius-1,radius+1;;-radius-1]]];
	(m-mu)/std]];
columnBasedDecompositionAdaptiveSampling=Function[{Data,rank,numColumns,maxIter},Module[{M=Data,V,weights,Cm,Y},
	V=SingularValueDecomposition[M,Min[Min@@Dimensions@Data,rank]][[3]];
	weights=Sqrt[Total[#^2]]&/@V;
	(*Print@weights;*)
	SortBy[Table[RandomSample[weights->Range[Dimensions[M][[2]]],numColumns],{maxIter}],Norm[M-M[[;;,#]].LeastSquares[M[[;;,#]],M],"Frobenius"]&][[1]]
	]];
cxApprox=Function[{m,k},Module[{cols,Cm},cols=columnBasedDecompositionAdaptiveSampling[m,k,k,4];Cm=m[[;;,cols]];{Cm,LeastSquares[Cm,m]}]];
cxColsRows=Function[{m,k},Module[{cols,Cm,rows},cols=columnBasedDecompositionAdaptiveSampling[m,k,k,4];Cm=m[[;;,cols]];
	rows=columnBasedDecompositionAdaptiveSampling[Transpose[m],k,k,4];{cols,rows}]];
imagePadToImageDim=Function[{img,imgDim,padding},Module[{cur=ImageDimensions@img,left,bottom},
	left=Round[(imgDim[[1]]-cur[[1]])/2];bottom=Round[(imgDim[[2]]-cur[[2]])/2];
	ImagePad[img,{{left,imgDim[[1]]-cur[[1]]-left},{bottom,imgDim[[2]]-cur[[2]]-bottom}},Padding->padding]]];
fullWidthToHalfWidth=Function[ch,If[ListQ@#&&Length[#]>0&&NumberQ@#[[1]]&&65281<=#[[1]]<=65374,FromCharacterCode[#[[1]]-65248],ch]&@ToCharacterCode@ch];
fourDirectionNormalize=Function[oms,Module[{weights},
	(*weights=N@Table[i^2 j,{i,Dimensions[oms][[1]]},{j,Dimensions[oms][[2]]}];*)
	oms.First[SortBy[Table[RotationMatrix[90i Degree],{i,0,3}],pnorm[Map[Max[#,0]&,Standardize[oms].#,{2}],2]&]]]];
Clear[lieNormalize];
lieNormalize[oms_List,OptionsPattern[{"power"->3,"Polarity"->"Min","Method"->"Global"(*,"Weighted"->True*),"Group"->"SpecialLinear"}]]:=
		Module[{a,as,ms=Standardize[oms,Mean,1&],r,global=OptionValue["Method"]==="Global",p(*,weights*),transformed},
	as=Array[a,Dimensions[ms][[2]]{1,1}];
	p=OptionValue["power"];(*weights=N@Table[If[OptionValue["Weighted"],i^2 j,1],{i,Dimensions[oms][[1]]},{j,Dimensions[oms][[2]]}];*)
	transformed=Switch[Dimensions[ms][[2]]
	,2,((*weights*)ms).Switch[OptionValue@"Group",
		"SpecialLinear",{{a[2,1],0},{0,1/a[2,1]}}.{{1,a[1,2]},{0,1}}.With[{\[Theta]=a[1,1]},{{Cos[\[Theta]],-Sin[\[Theta]]},{Sin[\[Theta]],Cos[\[Theta]]}}],
		"Rotation",With[{\[Theta]=a[1,1]},{{Cos[\[Theta]],-Sin[\[Theta]]},{Sin[\[Theta]],Cos[\[Theta]]}}],
		"ShearingSqueezing",With[{d=Exp@a[1,1],s=a[1,2]},{{d,0},{s,1/d}}],
		(*"Moebius",pnorm2[((*weights*)ms),p],*)
		_,Print["(1) Group NYI."];Abort[]]
	,_,If[OptionValue["Group"]!="SpecialLinear",Print["(2) Group NYI."];Abort[]];ms.MatrixExp[nilTrace@as]];
	r=Switch[OptionValue["Polarity"],"Max",If[global,NMaximize,FindMaximum],"Min",If[global,NMinimize,FindMinimum],_,Abort[]][
		pnorm2[transformed,p],Flatten@as];
	{r[[1]],transformed/.r[[2]]}
	];
extractPointCloud=Function[img,Position[Round@ImageData@Binarize@LaplacianFilter[Binarize@img,1],1]];
fontlist=Function[{},Sort[FE`Evaluate[FEPrivate`GetPopupList["MenuListFonts"]]]];
qlDecomposition=Function[m,Module[{id=N@Reverse@IdentityMatrix[Length@m]},{id.#[[1]].id,id.#[[2]].id}&@QRDecomposition[id.m.id]]];
imageToPointCloud=Function[img,Module[{m=ImageData@ImageResize[img,{100}],dispF,dim},dim=Dimensions[m][[;;2]];
	Graphics3D[Flatten@MapIndexed[If[ListQ@#,{RGBColor@@#,Point@Append[#2/dim,Mean@#]},{GrayLevel@#,Point@Append[#2/dim,#]}]&,m,{2}]]]];
luNoPivotDecompomposition[a_] := Module[ {L, U, n, m, k, j, p},U = a;n = Length[ a ];L = IdentityMatrix[n];							                                                              
  For[ k = 1, k <= n-1, k = k+1,  For[ j = k+1, j <= n, j = j+1, m = U[[j,k]] / U[[k, k]];L[[j,k]] = m;
    For[ p = k, p <= n, p = p+1, U[[j,p]] = U[[j,p]] - m*U[[k,p]] ]]];{L, U}]
Clear[kmeans];
kmeans[Y_List,k_Integer,OptionsPattern[{"MaxIterations"->100,"DebugEveryN"->20,"DebugF"->(Null&)}]]:=
		Module[{oldW,W,H,maxIter=OptionValue["MaxIterations"],norms,n=Length@Y},
	H=RandomSample[Y,k];W=Table[1,{n}];
	Do[norms=MapThread[Dot,{H,H},1];oldW=W;W=First@Ordering[#+norms,1]&/@(-2Y.Transpose[H]);(*Pause[1];Print["."];*)
		If[W===oldW,Break[]];H=Table[Mean@Extract[Y,Position[W,i]],{i,k}];If[Mod[j-1,OptionValue["DebugEveryN"]]==0,OptionValue["DebugF"][W,H];];
	,{j,maxIter}];W];(*kmeans[Y,10,"DebugF"->((cW=#;cH=#2)&),"DebugEveryN"->1];*)
checkEq=Function[{message,expected,actual},If[expected=!=actual,Print[{message,expected,actual}];Abort[]]];
countMorphologicalComponents=Function[img,Length@Union@Flatten@MorphologicalComponents@img];
imputeWithMean=Function[line,Module[{mask,n=Length@line,\[CapitalOmega],\[CapitalOmega]c},mask=N@Boole[NumericQ@#]&/@line;\[CapitalOmega]=sparseDiagonalMatrix[mask];
	\[CapitalOmega]c=sparseDiagonalMatrix[1-mask];LeastSquares[\[CapitalOmega]+\[CapitalOmega]c.(laplacianMatrix1D[n]+N@SparseArray[{{1,1}->-1,{n,n}->-1},n{1,1}]),line mask]]];
(*Clear[imputeWithMean];
imputeWithMean[line_List,OptionsPattern[{"MaxIterations"->300}]]:=With[{mask=Boole[NumericQ@#]&/@line},RpcaMeanFilter[{line mask},{mask},OptionValue["MaxIterations"]][[1,1]]];*)
visualizeColumnColors=Function[img,Module[{imgHeight=ImageDimensions[img][[2]],cp},cp=Mean/@Transpose@ImageData@img;
	Show[img,Graphics[Riffle[{Red,Green,Blue},Line/@Map[{1,imgHeight}#&
	,(ListLinePlot@Transpose[cp])[[1,2,1,3;;,3,1]],{2}]]],ImageSize->800]]];
panoChangeRadius=Compile[{{xy,_Real,1},{r,_Real}},(*Keep width/2 place unchanged.*){xy[[1]]-ArcSin[(1-r)Sin[xy[[1]](2Pi)]],xy[[2]]}];
panoChangeAngle=Compile[{{xy,_Real,1},{t,_Real}},(*Rotate to left.*){Mod[xy[[1]]+t-0.000001,1]+0.000001,xy[[2]]}];
panoChangeYScale=Compile[{{xy,_Real,1},{z,_Real}},{xy[[1]],z xy[[2]]}];
correlation=Function[{l1,l2},With[{corr=Quiet@Correlation[l1,l2]},If[NumberQ@corr,corr,-1]]];
softMaxHadamardJacobian=Compile[{{xs,_Real,1},{k,_Integer}},With[{xsk1=xs^(k-1)},With[{xsk=xsk1 xs},With[{txsk=Total[xsk]},With[{nxsk=xsk/txsk},
	With[{nxsk1=xsk1/txsk},k(DiagonalMatrix[nxsk1]-outer[nxsk1,nxsk])]]]]],CompilationTarget:>"C"];(*softMaxJacobianFromExp@RandomReal[1,10]*)
softMaxHadamardRows2D=Compile[{{xs,_Real,2},{k,_Integer}},With[{exs=#^k},exs/Total[exs]]&/@xs,CompilationTarget:>"C"];
softMaxJacobianFromValue=Compile[{{nexs,_Real,1}},Table[If[i==j,nexs[[i]](1-nexs[[j]]),-nexs[[i]]nexs[[j]]]
	,{i,Length@nexs},{j,Length@nexs}],CompilationTarget:>"C"];(*softMaxJacobianFromExp@RandomReal[1,10]*)
softMaxRows2D=Compile[{{xs,_Real,2},{b,_Real}},With[{exs=Exp[b(#-Max@#)]},1+(exs/Total@exs)-1]&/@xs,CompilationTarget:>"C"];
centeringMatrix=Function[n,pack[IdentityMatrix[n]-Array[1.&,{n,n}]/n]];
matrixFactorizationDistanceSquare=Function[{V,W,H,normV2},Module[{Wt=Transpose@W},(*Norm[V-W.H,"Frobeinius"]*)
	Plus@@{normV2,Total@Flatten[(Wt.W) (H.Transpose[H])],-2Total@Flatten[H (Wt.V)]}]];
chop2D=Compile[{{m,_Real,2}},Map[If[-10^-10<#<10^-10,0.,#]&,m,{2}],CompilationTarget:>"C"];
pack=Developer`ToPackedArray;
softHinge2D=Compile[{{xs,_Real,2},{invEps,_Real}},With[{eps=1/invEps},(*Plot[Flatten@softHinge2D[{{x}},1],{x,-5,5.}]*)
	Map[If[#>0,#+Log[1+eps Exp[-#]]-Log[eps],Log[eps+Exp[#]]-Log[eps]]&,xs,{-1}]],CompilationTarget:>"C"];
relativisticNorm2D=Compile[{{xs,_Real,2},{eps,_Real}},Module[{eps2=eps^2},Total@Flatten[Map[Sqrt[eps2+#^2]-eps&,xs,{2}]]]
	,CompilationTarget:>"C"];
gradientRelativisticNorm2D=Compile[{{xs,_Real,2},{eps,_Real}},Module[{eps2=eps^2},Map[#/Sqrt[eps2+#^2]&,xs,{2}]]
	,CompilationTarget:>"C"];
relativisticSigmoidAndGradient2D=Compile[{{xs,_Real,2}},
	Transpose[Map[With[{s1=Sqrt[1+(#+2)^2],s2=Sqrt[1+(#-2)^2]},{0.5+0.125(s1-s2),0.125((2+#)/s1+(2-#)/s2)}]&,xs,{2}],{2,3,1}]
,CompilationTarget:>"C"];
(*Plot[{sigmoid2D[{{x}}],0.5+0.125(relativisticNorm2D[{{x+2}},1]-relativisticNorm2D[{{x-2}},1])
	,0.5+0.125(Sqrt[1+(x+2)^2]-Sqrt[1+(x-2)^2])},{x,-5,5}]*)
parseGpx=Function[fname,Module[{rule,latlngs,timestamps,points,indices,interp},rule=Import[fname,"Data"];
	points=("Geometry"/.rule)[[1,1,1,1]];indices=Sort@Union[{1},RandomSample[Range[Length@points],100],{Length@points}];
	Print@Graphics[BlendLineWithPoint[points[[indices]]]];
	latlngs=("Geometry"/.rule[[1]])[[1,1,1]];timestamps=("PointTimestamp"/.("LabeledData"/.rule[[1]]))[[1,1]];
	interp=Interpolation@MapThread[{AbsoluteTime[#],#2}&,{timestamps,latlngs}];
	{interp,points,latlngs,timestamps,rule}]];
timestampFromImageExif=First@ImportString[StringReplace["DateTime"/.Import[#,"Exif"],{":"->","," "->","}],"CSV"]&;
focalLengthFromImageExif=Function[fname,With[{rule=Import[#,"Exif"]},("FocalLengthIn35mmFilm"/.rule)/35]&@fname];
buildLaplacianWarp=Function[imgs,Module[{scale,dim,indices,matches,\[Lambda]=1,pm,sp,sols,b,timg,interp},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	scale=4;dim=Round[ImageDimensions@imgs[[1]]/scale];indices=dim[[1]](#[[2]]-1)+#[[1]]&/@Round[matches[[1]]/scale];
	pm=poissonMatrix[dim];sp=SparseArray[({#,#}&/@indices)->1,(Times@@dim){1,1}];
	Print[sols=Table[b=SparseArray[indices->(#[[idx]]&/@(matches[[2]]/scale)),(Times@@dim)];
		(*Cannot use Array[0&,Times@@dim] below as it will shrink the image.*)
		LeastSquares[N@vertcat[pm,\[Lambda] sp],N@Join[pm.vec[Table[{i,j}[[idx]],{i,dim[[1]]},{j,dim[[2]]}]](*Array[0&,Times@@dim]*),\[Lambda] b]]
	,{idx,2}];//AbsoluteTiming];
	Print[Graphics[drawGrid@Transpose[unVec[#,dim]&/@sols,{3,1,2}],ImageSize->400]];
	interp=Interpolation[Join@@MapThread[List,{Table[{i,j},{i,dim[[1]]},{j,dim[[2]]}],Transpose[unVec[#,dim]&/@sols,{3,1,2}]},2]];
	timg=ImageForwardTransformation[imgs[[1]],4interp@@(((#-{1,1})/4))+{1,1}&,DataRange->Full];
	Print[timg];
	Print[anaglyph@{timg,imgs[[2]]}];
	Print[anaglyph@{imgs[[1]],imgs[[2]]}];
	]];
circularLaplacianMatrix1D=Function[n,ReplacePart[laplacianMatrix1D[n],{{1,n}->-1,{n,1}->-1}]];
diagonalizedCircularLaplacian1D=Function[n,sparseDiagonalMatrix[Prepend[2+Re[Power[-1,#/n]+Power[-1,(2n-#)/n]]&/@
	If[EvenQ@n,Join[Reverse[#],{0},#]&[Range[2,n-2,2]],Join[Reverse[#],#]&[Range[1,n-2,2]]],0]]];
fitPowerLaw=Function[l,Module[{x,c},FindFit[l/l[[1]],x^(-c),{c},x]][[1,2]]];(*{fitPowerLaw[Range[10]^(-3)],fitPowerLaw[Range[10]^(3)]}*)
visualizePowerLaw=Function[l,Module[{c=fitPowerLaw@l},{c,ListLogLogPlot@#,ListPlot[#,PlotRange->All]}&@{l,l[[1]](Range[Length@l]^(-c))}]];
visualizeHomography=Function[{homog,imgs},anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}];
standardizeTensor[m_,f1_:Mean,f2_:StandardDeviation]:=ArrayReshape[Standardize[Flatten@m,f1,f2],Dimensions@m];
plotStylesForLabelGroups=Function[groups,Module[{groupColors={Blue,Red,Green,Purple},
		perGroupStyles=Join@@Outer[Composition,{#&,{#,Dashed}&,{#,DotDashed}&,{#,Dotted}&},{#&,{#,Thick}&(*,{#,Thin}&*),Darker(*,Lighter*)},1]},
	(*If[Length@#==1,#[[1]],#]&/@Join@@*)Join@@MapIndexed[(perGroupStyles[[#2[[2]]]])[groupColors[[#2[[1]]]]]&,groups,{2}]]];
takeAtMostN=Function[{l,n},If[Length@l>=n,l[[;;n]],l]];
(*hingeLoss=Compile[{{ts,_Real,1},{xs,_Real,1}},Total@MapThread[Max[0,1-# #2]&,{ts,xs}]];*)
supportVectorLeastSquare=Function[{xs,ys,\[Lambda]},(*\[Lambda] is different from 1/C as it penalizes L2 of deviation.*)
		Module[{w,f,g,ws,r,ws2,xsws,xswsys,xst=Transpose[xs],cf},
	cf=Compile[{{xsws,_Real,2},{yss,_Real,2}},MapThread[If[# #2>1,0.,(#-#2)]&,{xsws,yss},2],CompilationTarget:>"C"];
	ws=Array[w,{Dimensions[xs][[2]],Dimensions[ys][[2]]}];ws2=LeastSquares[xs,ys];
	f[ws_?(NumberQ@#[[1,1]]&)]:=(xsws=xs.ws;xswsys=cf[xsws,ys];pnorm2[xswsys,2]+\[Lambda] pnorm2[ws,2]);
	g[ws_?(NumberQ@#[[1,1]]&)]:=2vec[xst.xswsys+\[Lambda] ws];
	{r=FindMinimum[f[ws],variableWithInitial[vec@ws,vec@ws2],Gradient:>g[ws]];//AbsoluteTiming,r[[1]]};ws/.Dispatch[r[[2]]]]];
(*sigmoid=Function[x,1/(1+Exp[-x])];*)
(*sigmoid2D=Compile[{{xs,_Real,2}},Map[If[#>0,1/(1+Exp[-#]),With[{e=Exp[#]},e/(1+e)]]&,xs,{2}],CompilationTarget:>"C"];*)
sigmoid2D=Compile[{{xs,_Real,2}},Map[If[#>0,1/(1+Exp[-#]),With[{e1=1+Exp[#]},1-1/e1]]&,xs,{2}],CompilationTarget:>"C"];
decodeOneHot=Ordering[Abs[#],-1][[1]]&;
oneHot=Function[{i,n},ReplacePart[Table[0,{n}],Round[i]->1]];
essentialMatrixToFundamentalMatrix=Function[{em,ikms},Transpose[ikms[[2]]].em.ikms[[1]]];
fundamentalMatrixToEssentialMatrix=Function[{fm,ikms},Inverse[Transpose[ikms[[2]]]].fm.Inverse[ikms[[1]]]];
imageGradient=Function[img,MapThread[# Exp[I #2]&,ImageData/@{GradientFilter[img,1],GradientOrientationFilter[img,1]},2]];
projectiveLineFromPointPair=Function[pair,Cross@@(Append[#,1]&/@pair)];
calibratedProjectiveLineFromPointPair=Function[{pair,ikm},Cross@@(ikm.Append[#1,1]&)/@pair];
pencilIntersection=Function[pencil,projective[Cross@@pencil]];
visualizeEpipolarLine=Function[{line,img},visualizePencil[{line},img]];
visualizePencil=Function[{pencil,image},Show[image,ContourPlot[Evaluate[{x,y,1}.#==0&/@pencil],{x,0,ImageDimensions[image][[1]]},{y,0,ImageDimensions[image][[2]]}
	,ContourStyle->{{Thick,Blue},{Thick,Red}}]]];
drawNumberedLines=Function[lines,{Thick,Yellow,Line/@lines,Red,MapIndexed[Inset[#2[[1]],#1+RandomReal[10{-1,1}]]&,Mean/@lines]}];
crossRatioQuadruple=Function[quadruple,Module[{zs=Plus@@(#{1,I})&/@quadruple},(zs[[1]]-zs[[3]])(zs[[2]]-zs[[4]])/((zs[[2]]-zs[[3]])(zs[[1]]-zs[[4]]))]];
projective=#[[;;-2]]/#[[-1]]&;
transposeLiers=If[#=={},{{},{}},Transpose@#]&;
lineSimple=Function[{xy1,xy2},Module[{delta=xy2-xy1,gradient,dir},If[Abs[delta[[2]]]>Abs[delta[[1]]],Reverse/@lineSimple[Reverse@xy1,Reverse@xy2],
	gradient=delta[[2]]/delta[[1]];dir=If[xy2[[1]]>xy1[[1]],1,-1];Table[{i,Round[xy1[[2]]+(i-xy1[[1]]) gradient]},{i,Range[xy1[[1]],xy2[[1]],dir]}]]]];
(*line={{2,5},{10,2}};Graphics[{Line@line,Point[lineSimple@@line]},Axes->True]*)
twoViewEnvironment=Function[imgs,Module[{ikms,rtns,hs,homog,matches,epiMatches,fms},
	rtns=rtnsFromTwoView[imgs];hs=With[{rtn=rtns[[1]]},rtn[[1]]+outer[rtn[[2]],rtn[[3]]]];
	ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;(*homog=Inverse[ikms[[2]]].hs.ikms[[1]];*)
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];homog=homographyFromMatches@matches;
	epiMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];
	Print[anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}];
	fms=Table[Transpose[ikms[[2]]].skewOmega[Normalize@rtn[[2]]].rtn[[1]].ikms[[1]],{rtn,rtns}];
	{ikms,homog,hs,rtns,matches,epiMatches,fms}]];
traceNormOrientPhoto=Function[imgIn,Module[{doMasking=True,mask,n=Min[50,Min@ImageDimensions@imgIn],img,gray,img2,optT,allTs,f,t,r},
	img=ImageResize[imgIn,n{1,1}];
	mask=ColorConvert[Image[Graphics[{EdgeForm[White],FaceForm[White],Disk[n/2{1,1},n/2]},Background->Black],ImageSize->n{1,1}],"Gray"];
	gray=If[doMasking,ImageMultiply[mask,#]&,Identity]@ColorConvert[img,"Gray"];
	f[t_?NumericQ]:=schattenNorm[ImageData@ImageRotate[gray,t],1];
	r=NMinimize[f[t],t];{r,ImageRotate[gray,t/.r[[2]]],ImageRotate[imgIn,t/.r[[2]]]}]];
(*exampleStereoPairs=Map[Import[#,ImageSize->{400}]&,{{"/h/t11.jpg","/h/t12.jpg"},{"/h/t14.jpg","/h/t13.jpg"},{"/h/t13.jpg","/h/t14.jpg"}
	,{"/h/t2.jpg","/h/t4.jpg"},{"/h/t4.jpg","/h/t7.jpg"},{"/h/t7.jpg","/h/t8.jpg"},{"/h/t9.jpg","/h/t10.jpg"},{"/h/forward1.jpg","/h/forward2.jpg"}
	,{"/h/slide1.jpg","/h/slide2.jpg"},{"/h/t15.jpg","/h/t16.jpg"}},{2}];*)

breakARGB={BitAnd[BitShiftRight[#,24],255],BitAnd[BitShiftRight[#,16],255],BitAnd[BitShiftRight[#,8],255],BitAnd[#,255]}/255&;
skew=Function[m,(m-m\[Transpose])/2];
borderMask=Function[dim,SparseArray[#->1&/@
		Join[Join@@Table[{{i,1},{i,dim[[2]]}},{i,dim[[1]]}],Join@@Table[{{1,j},{dim[[1]],j}},{j,dim[[2]]}]],dim]];
vertcat=Function[{m,m2},Module[{n=Dimensions[m][[1]]},If[Head@m===SparseArray&&Head@m2===SparseArray,
		SparseArray[Join[MapThread[Rule,{m["NonzeroPositions"],m["NonzeroValues"]}]
			,{#[[1,1]]+n,#[[1,2]]}->#[[2]]&/@MapThread[Rule,{m2["NonzeroPositions"],m2["NonzeroValues"]}]],{n,0}+Dimensions[m2]]
	,ArrayFlatten@{{m},{m2}}]]];
horzcat=Function[{m,m2},Module[{n=Dimensions[m][[1]]},If[Head@m===SparseArray&&Head@m2===SparseArray,
		SparseArray[Join[MapThread[Rule,{m["NonzeroPositions"],m["NonzeroValues"]}]
			,{#[[1,1]],#[[1,2]]+n}->#[[2]]&/@MapThread[Rule,{m2["NonzeroPositions"],m2["NonzeroValues"]}]],{0,n}+Dimensions[m2]]
	,ArrayFlatten@{{m,m2}}]]];
filterMatchesByGivenHomography=Function[{matches,homog,tolerance},transposeLiers@Extract[Transpose@matches,Position[Boole[Norm[#]<tolerance]&/@
		(((#[[;;-2]]/#[[-1]])&[homog.Append[#[[1]],1]]&/@Transpose@matches)-matches[[2]]),1]]];
filterMatchesByHomography=Function[{matches,tolerance},Module[{homog},
	homog=TransformationMatrix@Last@findGeometricTransform[matches[[2]],matches[[1]],"Transformation"->"Perspective"];
	filterMatchesByGivenHomography[matches,homog,tolerance]]];
pointSpread=Function[{rtn,matchSrc,ikm},#[[1]]/Total[#[[;;2]]]&@SingularValueList[Standardize[(#/(#.rtn[[3]]))&[ikm.Append[#,1]]&/@matchSrc,Mean,1&]]];
fixRtnNormalSignRequirePositiveDepth=Function[{rtn,matchSrc,ikm},If[And@@(rtn[[3]].ikm.Append[#,1]<0&/@matchSrc),{rtn[[1]],-rtn[[2]],-rtn[[3]]},rtn]];
rtnsFromTwoViewWithFocalLength=Function[{imgs,focalLengths},Module[{matches,ikms,homog,hs,rtns,parallax},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	ikms=MapThread[Inverse@intrinsicParameterMatrixWithFocalLength[#,#2]&,{imgs,focalLengths}];
	homog=homographyFromMatches@matches;hs=ikms[[2]].homog.Inverse[ikms[[1]]];
	rtns=fixRtnNormalSignRequirePositiveDepth[#,matches[[1]],ikms[[1]]]&@standardizeRtn@#&/@decomposeH@hs;
	parallax=Norm[rtns[[1,2]]]Norm[rtns[[1,3]]];
	SortBy[rtns,If[parallax>0.6,pointSpread[#,matches[[1]],ikms[[1]]]&,reprojectionErrorHomography[Inverse[ikms[[2]]].#[[1]].ikms[[1]],matches]&]]]];
rtnFromTwoViewWithFocalLength=Function[{imgs,focalLengths},First@rtnsFromTwoViewWithFocalLength[imgs,focalLengths]];
rtnsFromTwoView=Function[imgs,rtnsFromTwoViewWithFocalLength[imgs,{4.6/3.2,4.6/3.2}]];
rtnFromTwoView=Function[imgs,First@rtnsFromTwoView[imgs]];
buildTwoViewModel=Function[imgs,Module[{ikms,matches,homog,hs,rtns,pts,vs,qs,imageDim,corners,allMatches,fm,fFiltered},
(*imgs=Import[#,ImageSize->{400}]&/@{"/h/t2.jpg","/h/t4.jpg"}(*{"/h/t4.jpg","/h/t7.jpg"};*)*)
	(*allMatches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Epipolar"];*)
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	ikms=Inverse@intrinsicParameterMatrix@#&/@imgs;
	homog=homographyFromMatches@matches;
	Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];
	Print[MapThread[annotateImageWithPoints,{imgs,matches}]];
	hs=ikms[[2]].homog.Inverse[ikms[[1]]];
	rtns=fixRtnNormalSignRequirePositiveDepth[#,matches[[1]],ikms[[1]]]&@standardizeRtn@#&/@decomposeH@hs;Print[dispRtn/@rtns//TableForm];
	Print[{"parallax",parallaxLevelHomography[homog,ikms]}];
	Print[MapThread[Labeled,{{"Homography","Rotation-1","Rotation-2"},
		anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].#.ikms[[1]],DataRange->Full],imgs[[2]]}&/@Prepend[rtns[[;;,1]],hs]}]];
	Print/@Table[(*fm=Transpose[ikms[[2]]].skewOmega[rtn[[2]]].rtn[[1]].ikms[[1]];fFiltered=Transpose@Select[Transpose@allMatches,Abs[Append[#[[2]],1].fm.Append[#[[1]],1]]<0.01&];*)
		pts=#/(#.rtn[[3]])&[ikms[[1]].Append[#,1]]&/@matches[[1]];Print[{"pointSpread",pointSpread[rtn,matches[[1]],ikms[[1]]]}];
		Print[{"reprojection error",Mean[Norm/@(matches[[2]]-(#[[;;-2]]/#[[-1]]&[Inverse[ikms[[2]]].(rtn[[1]].#(*+rtn[[2]]*))]&/@pts))]}];
		vs={{0,0,0},-rtn[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]]};
		Graphics3D[{Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@pts]
			,Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,2,Length@qs}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"],{rtn,rtns}];]];
triangulatePoint=Function[{projs,coords,ikms},Module[{a,as,f,mat,(*point4d,*)r},as=Array[a,3];
	mat=Join@@MapThread[skewOmega[Normalize[#.Append[#2,1]]].#.#3&,{ikms,coords,projs}];
	(*Or point4d={a[1],a[2],1/Abs[a[3]],1}*)
	f[as_?(NumericQ@#[[1]]&)]:=With[{point4d={a[1]Abs[a[3]],a[2]Abs[a[3]],1,Abs[a[3]]}},
		pnorm2[mat.point4d,2]/pnorm2[point4d,2]+Max[0,-projs[[1]].point4d]+Max[0,-projs[[2]].point4d]];
	r=Quiet@FindMinimum[f[as],as];{a[1]Abs[a[3]],a[2]Abs[a[3]],1,Abs[a[3]]}/.r[[2]]]];
(*triangulatePoint=Function[{projs,coords,ikms},Module[{a,as,f,mat,point4d,r},as=Array[a,3];
	mat=Join@@MapThread[skewOmega[Normalize[#.Append[#2,1]]].#.#3&,{ikms,coords,projs}];
	point4d={a[1]Abs[a[3]],a[2]Abs[a[3]],1,Abs[a[3]]};(*Or point4d={a[1],a[2],1/Abs[a[3]],1}*)
	f=Function[as,pnorm2[mat.point4d,2]/pnorm2[point4d,2]];r=Quiet@FindMinimum[f[as],as];point4d/.r[[2]]]];*)
buildTwoViewModelSmartWithFocalLength=Function[{imgs,focalLengths},
		Module[{ikms,matches,homog,hs,rtns,pts,vs,qs,imageDim,corners,allMatches,rtn,projs},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	ikms=MapThread[Inverse@intrinsicParameterMatrixWithFocalLength[#,#2]&,{imgs,focalLengths}];
	homog=homographyFromMatches@matches;
	Print[Append[imgs,anaglyph@{ImagePerspectiveTransformation[imgs[[1]],homog,DataRange->Full],imgs[[2]]}]];
	Print[MapThread[annotateImageWithPoints,{imgs,matches}]];
	hs=ikms[[2]].homog.Inverse[ikms[[1]]];Print[{"parallax",parallaxLevelHomography[homog,ikms]}];
	rtns=rtnsFromTwoViewWithFocalLength[imgs,focalLengths];
	Print[MapThread[Labeled,{{"Homography","Rotation-1","Rotation-2"},
		anaglyph@{ImagePerspectiveTransformation[imgs[[1]],Inverse[ikms[[2]]].#.ikms[[1]],DataRange->Full],imgs[[2]]}&/@Prepend[rtns[[;;,1]],hs]}]];
	Print@Table[
	projs={Inverse[ikms[[1]]].ArrayFlatten@{{IdentityMatrix[3],Table[{0},{3}]}},Inverse[ikms[[2]]].ArrayFlatten@{{rtn[[1]],List/@rtn[[2]]}}};
	pts=Table[projective@triangulatePoint[projs,coords,ikms],{coords,Transpose@matches}];
	vs={{0,0,0},-rtn[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]]};
	Graphics3D[{Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@pts]
			,Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,2,Length@qs}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]
	,{rtn,rtns}]]];
poseFromProjections=Function[putativeMatches,Module[{a,t,f,as,ts,r},as=Array[a,3];ts=Array[t,3];
	f[as_?(NumericQ@#[[1]]&),ts_]:=Module[{R=Re[MatrixExp@skewOmega@as]},
		pnorm[(projective/@putativeMatches[[1]])- (#[[;;-2]]/(10^-6 Sign[#[[-1]]]+#[[-1]])&[R.#+ts]&/@putativeMatches[[2]]),1]];
	r=FindMinimum[f[as,ts],Flatten@{as,ts}];(*Print[r[[1]]];*){Re[MatrixExp@skewOmega[as/.r[[2]]]],ts/.r[[2]]}]];
buildThreeViewModelRotationalWithFocalLength=Function[{imgs,focalLengths},
		Module[{ikms,homog,hs,rtns,pts,vs,qs,tracks,allMatches,rtn,projs,pose},
	ikms=MapThread[Inverse@intrinsicParameterMatrixWithFocalLength[#,#2]&,{imgs,focalLengths}];
	rtn=rtnFromTwoViewWithFocalLength[#,focalLengths]&@imgs;
	projs={Inverse[ikms[[1]]].ArrayFlatten@{{IdentityMatrix[3],Table[{0},{3}]}},Inverse[ikms[[2]]].ArrayFlatten@{{rtn[[1]],List/@rtn[[2]]}}};
	tracks=threeViewTrack@imgs;pts=Table[projective@triangulatePoint[projs,coords,ikms[[;;2]]],{coords,Transpose@tracks[[1;;2]]}];
	pose=poseFromProjections@Transpose@MapThread[{ikms[[3]].Append[#,1],#2}&,{tracks[[3]],pts},1];Print@pose;
	vs={{0,0,0},-rtn[[2]],-pose[[2]]};qs=quaternionFromRotationMatrix/@{IdentityMatrix[3],Transpose[rtn[[1]]],Transpose[pose[[1]]]};
	Graphics3D[{Opacity[0.5],(*Riffle[RGBColor@@PixelValue[imgs[[1]],#]&/@matches[[1]],Point/@pts]
	,*)Table[drawCameraWithImage[vs[[i]],qs[[i]],imgs[[i]]],{i,1,3}]},Axes->True,AxesLabel->{"x","y","z"},Lighting->"Neutral"]]];
threeViewTrack=Function[imgs,Module[{matches,allmatches2,nf,rnf,matches2},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];
	allmatches2=imageCorrespondingPoints[imgs[[2]],imgs[[3]](*,"Transformation"->"Perspective"*)];
	nf=Nearest[Rule@@@Transpose[allmatches2]];matches2=filterMatchesByHomography[Transpose[{#,First@nf@#}&/@matches[[2]]],3];
	rnf=Nearest[Rule@@@Transpose[Reverse@matches]];Transpose[{First@rnf[#[[1]]],#[[1]],#[[2]]}&/@Transpose@matches2]]];
findGeometricTransform=If[NumericQ@#[[1]],#,{Infinity,TransformationFunction@IdentityMatrix[3]}]&@(FindGeometricTransform[##])&;
(* \( \min_X \tau (\| G_ 1 X\|_ 1 + \| G_ 2 X\|_ 1)  + \| A \operatorname{vec} X - \operatorname{vec} Y \| \) *)
totalVariationShrinkage=Function[{\[Tau],A,Y,maxIter},Module[{AtA=SparseArray[A\[Transpose].A],AtY,vecY=vec@Y,G1,G2,dim=Dimensions@Y,f,\[Mu]=100\[Tau]+10^-20,OX,X
		,Gs,GtGs},AtY=A\[Transpose].vecY;X=LinearSolve[A,vecY];
	G1=SparseArray@KroneckerProduct[gradientMatrix1D[dim[[1]]],sparseIdentityMatrix[dim[[2]]]];
	G2=SparseArray@KroneckerProduct[sparseIdentityMatrix[dim[[1]]],gradientMatrix1D[dim[[2]]]];Gs={G1,G2};GtGs=#\[Transpose].#&/@Gs;
	f=Function[X,LinearSolve[SparseArray[AtA+\[Mu] (Plus@@GtGs)],SparseArray[AtY+\[Mu] (Plus@@(#\[Transpose].dShrinkage[\[Tau]/\[Mu],#.X]&/@Gs))]]];
	Do[OX=X;X=f[X];If[pnorm[X-OX,2]/Times@@dim<0.00001,Print[{"totalVariationShrinkage #iter",j}];Break[]];
	,{j,maxIter}];unVec[vec@X,dim]]];
sparseDiagonalMatrix=Function[diag,SparseArray[{Band[{1,1}]->diag},Length@diag {1,1}]];
sparseIdentityMatrix=Function[n,If[NumericQ@n,SparseArray[{Band[{1,1}]->1},n{1,1}],SparseArray[{Band[{1,1}]->1},n]]];
linearSolveTwoSided=Function[{A,B,C},(*Solves A X B = C*)Transpose@LinearSolve[Transpose@B,Transpose@LinearSolve[A,C]]];
linearSolveTwoSidedMask=Function[{A,B,C,\[CapitalOmega]},(*Solves \[CapitalOmega] o (A X B - C)= 0*)linearSolveTwoSided[A,B,\[CapitalOmega] C]];
leastSquaresTwoSided=Function[{A,B,C},(*Solves A X B = C*)Transpose@LeastSquares[Transpose@B,Transpose@LeastSquares[A,C]]];
leastSquaresTwoSidedMask=Function[{A,B,C,\[CapitalOmega]},(*Solves \[CapitalOmega] o (A X B - C) = 0*)leastSquaresTwoSided[A,B,\[CapitalOmega] C]];
padToTwoPower=Function[m,PadRight[m,2^Ceiling[Log[2,#]]&/@Dimensions@m]];
radiusDistort=Function[{w,ikm},Function[xy,Module[{xyd,rd,ru},xyd=#[[;;-2]]/#[[-1]]&[ikm.Append[xy,1]];rd=Norm@xyd;ru=Tan[rd w]/(2 Tan[w/2]);
	#[[;;-2]]/#[[-1]]&[Inverse[ikm].Append[ru/rd xyd,1]]]]];
hadamardTransform=Function[v,Module[{n=Log[2,Length@v],x},If[Head@n=!=Integer,Print["Length need be power of 2."];Abort[]];
	x=First@foldToTensor[{v},Prepend[Table[2,{n}],1],1];vec@foldXUs[x,Table[hadamardMatrix[1],{n}],{}]]];
pointInRectangle=Function[{pt,rec},rec[[2,1]]>=pt[[1]]>=rec[[1,1]]&&rec[[2,2]]>=pt[[2]]>=rec[[1,2]]];
fourierNorm=Function[{m,p},pnorm[Fourier@m,p]];
nonzeroMask=Function[arr,Developer`ToPackedArray@Map[If[#==0.,0.,1.]&,arr,{-1}]];
nonzeroRatio=Function[arr,nonzeroCount[arr]/(Times@@Dimensions@arr)];
nonzeroCount=Function[m,Length[SparseArray[m]["NonzeroValues"]]];
zeroCount=Function[m,Times@@(Dimensions@m)-nonzeroCount[m]];
outer=Outer[Times,##]&;
kyfanShrink=Function[{\[Tau],k,mIn},Module[{svd=SingularValueDecomposition@mIn,m},m=vec@svd[[2]];
	svd[[1]].unVec[ReplacePart[m,#->dShrinkage[\[Tau],m[[#]]]&/@Ordering[m,-k]],Dimensions@mIn].svd[[3]]\[ConjugateTranspose]]];
tnnShrink=Function[{\[Tau],k,mIn},Module[{svd=SingularValueDecomposition@mIn,m},m=Diagonal@svd[[2]];
	svd[[1]].PadRight[DiagonalMatrix[ReplacePart[m,#->dShrinkage[\[Tau],m[[#]]]&/@Ordering[m,k]]],Dimensions@svd[[2]]].svd[[3]]\[ConjugateTranspose]]];
grayCode=Function[n,Module[{lst=Prepend[IntegerDigits[n,2],0]},Table[BitXor[lst[[i-1]],lst[[i]]],{i,2,Length@lst}]]];
householderTransformation=Function[a,IdentityMatrix[Length@a]-2Outer[Times,a,a]/(a.a)];
hadamardMatrix=With[{m=N@{{1,1},{1,-1}}/Sqrt[2]},Function[n,If[n==1,m,KroneckerProduct[m,hadamardMatrix[n-1]]]]];
walshMatrix=With[{walshOrdering=Function[{k,order},Total[Table[2^(i-1),{i,order}] PadLeft[grayCode[k],order]]]},
	Function[order,hadamardMatrix[order][[Table[1+walshOrdering[i,order],{i,0,2^order-1}]]]]];
alternativeCalibratedHomographyDecompostion=With[{alternativeDecompostionT=Function[{R,t,n},{R-2Outer[Times,t,t.R]/(t.t),t,n+2t.R/(t.t)}],
	alternativeDecompostionN=Function[{R,t,n},{R-2Outer[Times,R.n,n]/(n.n),t+2R.n/(n.n),n}]},
	alternativeDecompostionT@@(alternativeDecompostionN[##])&];
decomposeH=Function[{hIn},Module[{h=hIn,t,n,a,ts,ns,as,r,rtn,rtn2},
	h=h/SingularValueList[h][[2]];{ts,ns}=Array[#,3]&/@{t,n};as=Array[a,3{1,1}];
	r=Quiet@FindMinimum[pnorm2[matrixExpSpecialOrthogonal@as+Outer[Times,ts,ns]-h,2],Flatten@{ts,ns,as}];
	rtn={matrixExpSpecialOrthogonal[as],Normalize@ts,Norm[ts]ns}/.r[[2]];
	rtn2=alternativeCalibratedHomographyDecompostion@@rtn;SortBy[standardizeRtn/@{rtn,rtn2},Norm[Cross[#[[3]],{0,0,1}]]&]]];
dispRtn=Function[rtn,{MatrixForm[rtn[[1]]+Outer[Times,rtn[[2]],rtn[[3]]]]
	,{{2ArcCos[#[[1]]],Normalize@#[[2;;]]}&@quaternionFromRotationMatrix@rtn[[1]],SingularValueList@rtn[[1]],Det@rtn[[1]]},Normalize@rtn[[2]],Normalize@rtn[[3]],Norm@rtn[[2]] Norm@rtn[[3]]}];
standardizeRtn=Function[rtn,With[{sign=Sign[rtn[[3,First@Ordering[Abs@rtn[[3]],-1]]]]},
	{rtn[[1]],sign Norm[rtn[[3]]]rtn[[2]],sign Normalize@rtn[[3]]}]];
normalizeCalibratedHomography=Function[homog,homog/SingularValueList[homog][[2]]];
parallaxLevelHomography=Function[{homog,ikms},Norm[#[[2]]]Norm[#[[3]]]&@First@decomposeH[ikms[[2]].homog.Inverse[ikms[[1]]]]];
parallaxLevelTwoView=Norm[#[[2]]]Norm[#[[3]]]&@rtnFromTwoView@#&
(*(*Why not accurate?*)Function[imgs,{ikms,homog,hs,rtns,matches,epiMatches,fms}=twoViewEnvironment@imgs;parallaxLevelHomography[homog,ikms]]/@exampleStereoPairs*)
matchStick=Function[pts,{Yellow,Line[pts],Red,Point[pts[[2]]]}];
visualizeFmatrix=Function[{fm,img2,matches},Module[{arrows},arrows=MapThread[arrowToClosetPointOnLine,{matches[[2]],fm.Append[#,1]&/@matches[[1]]}];
	Show[img2,Graphics@MapIndexed[{Yellow,Inset[#2[[1]],#[[1]]],Line@#,Red,Point@#[[2]]}&,arrows]]]];
arrowToClosetPointOnLine=Function[{point,line},Module[{pt=If[Length@point==3,point[[;;-2]]/point[[-1]],point]},
	{pt,pt-(Append[pt,1].line)line[[;;2]]/pnorm2[line[[;;2]],2]}]];
(*line={1,2,-1};Manipulate[Show[ContourPlot[line.{x,y,1}==0,{x,-3,3},{y,-3,3}],
	Graphics[{Arrow[arrowToClosetPointOnLine[{x,y},line]]},Axes->True]],{x,-3,3},{y,-3,3}]*)
pointLineDistance=Function[{point,line},Module[{pt=If[Length@point==3,point,Append[point,1]]},Abs[line.pt/(#.#&@line[[;;2]])]]];
peakSignalToNoiseRatio=Function[{img1,img2},Module[{mats=If[Head@#===Image,ImageData@#,Identity@#]&/@{img1,img2},mse},
	mse=pnorm2[mats[[1]]-mats[[2]],2]/(Times@@(Dimensions[mats[[1]]][[;;2]]));If[mse==0,Infinity,10. Log[10,1/mse]]]];
normalizeByRotationalHomography=Function[{pts,fun},Module[{a,as,h,f,r},as=Array[a,3{1,1}];
	h[as_]:=With[{homog=matrixExpSpecialOrthogonal@as},transformByHomography[#,homog]&/@pts];
	f[as_?(NumberQ@#[[1,1]]&)]:=fun[h[as]];
	r=NMinimize[f[as],Flatten@as];h[as/.r[[2]]]]];
transformByHomography=Function[{pt,homog},#[[;;-2]]/#[[-1]]&[homog.Append[pt,1]]];
moebiusTransform=Function[m,(#[[1]]/#[[2]]&)[m.{#,1}]&];
matrixExpSpecialOrthogonal=Function[as,Re@MatrixExp[#-#\[Transpose]&@lowerTriangle@as]];
(*Conforms to http://en.wikipedia.org/wiki/Cayley_transform rather than http://en.wikipedia.org/wiki/Rotation_matrix# Skew_parameters _via _Cayley .27s_formula*)
cayleyTransform=Function[m,With[{id=IdentityMatrix@First@Dimensions@m},LinearSolve[id+m,id-m]]];
complexCayleyTransform=Function[m,With[{id=IdentityMatrix@First@Dimensions@m},LinearSolve[I id+m,m-I id]]];
inversecComplexCayleyTransform=Function[m,With[{id=IdentityMatrix@First@Dimensions@m},I LinearSolve[id-m,m+id]]];
BlendLine=Function[{xys},With[{n=Length@xys},Riffle[Table[Hue[0.7-0.7i/n],{i,n}],Line/@Partition[xys,2,1]]]];
BlendLineWithPoint=Function[{xys},With[{n=Length@xys},{Point@xys,Riffle[Table[Hue[0.7-0.7i/n],{i,n}],Line/@Partition[xys,2,1]]}]];
multiViewer = 
  TableForm@{{"3d", "Side", "Front", "Top"}, 
    {Show[#, ViewPoint -> {1, -1, 1}], 
     Show[#, ViewPoint -> {\[Infinity], 0, 0}], 
     Show[#, ViewPoint -> {0, -\[Infinity], 0}], 
     Show[#, ViewPoint -> {0, 0, \[Infinity]}]}, 
    {"", 
     Show[#, ViewPoint -> Right], 
     Show[#, ViewPoint -> Front], 
     Show[#, ViewPoint -> Above]}} &;
(*balancedKeypoints=Function[{img,scale},
	Flatten[MapIndexed[Function[e,{scale (#2-1)+e[[1]],e[[2]]}]/@ImageKeypoints[#,{"PixelPosition","Descriptor"},MaxFeatures->1]&,
		ImagePartition[#,scale],{2}],2]&@img];*)
findOrthogonals=Function[{as,numPoints,isProjective},Module[{x,xs},
	xs=Array[x,Length@as];N@FindInstance[Join[as,If[isProjective,{1},{}]].Join[xs,If[isProjective,{1},{}]]==0,xs,Reals,numPoints][[;;,;;,2]]]];
meanImage=Image@Mean[ImageData/@#]&;
(*drawCamera=Function[{v,q},Module[{points},
	points=rotateByQuaternion[#,q]+v&/@{{0,0,0},{-1,-2,-1},{1,-2,-1},{1,2,-1},{-1,2,-1},{-1,-2,-1},{0,0,0},{1,-2,-1},{0,0,0},{1,2,-1},{0,0,0},{-1,2,-1}};
	{FaceForm[None],EdgeForm[Green],Polygon[points]}]];*)
drawCameraWithImage=Function[{v,q,imgIn},drawCameraWithImageWithFocalLength[v,q,imgIn,4.6/3.2]];
drawCameraWithImageWithFocalLength=Function[{v,q,imgIn,focalLength},Module[{points,facePoints,img,transf=rotateByQuaternion[#,q]+v&,wh,depth=1},
	If[imgIn=!=None,img=ImageResize[imgIn,{320}];
		wh=Most[Inverse@intrinsicParameterMatrixWithFocalLength[img,focalLength].Append[ImageDimensions[img],1]];,wh={1,2}/2];
	facePoints={{-wh[[1]],wh[[2]],depth},{wh[[1]],wh[[2]],depth}
		,{wh[[1]],-wh[[2]],depth}, {-wh[[1]],-wh[[2]],depth},{-wh[[1]],wh[[2]],depth}};
	points={{0,0,0}}~Join~facePoints~Join~{{0,0,0},{wh[[1]],-wh[[2]],depth},{0,0,0},{wh[[1]],wh[[2]],depth},{0,0,0},{-wh[[1]],wh[[2]],depth}};
	{FaceForm[],EdgeForm[Green],Polygon[transf/@points]}~Join~If[imgIn===None,{},{FaceForm[White],Texture[img]
		,Polygon[transf/@facePoints,VertexTextureCoordinates->{{0,1},{1,1},{1,0},{0,0},{0,1}}]}]]];
drawCamera=Function[{v,q},drawCameraWithImage[v,q,None]];
blendCameras=Function[poses,Module[{num=Length@poses},
	Graphics3D[Join@@Table[{FaceForm[None],EdgeForm[Hue[0.7-0.7n/num]],drawCamera[poses[[n,1;;3]],poses[[n,4;;7]]][[-1]]},{n,num}],Axes->True,AxesLabel->{"x","y","z"}]
	]];
drawGrid=Function[m,Flatten[(*Line/@Partition[#,2,1]&*)BlendLine/@Join[m,Transpose@m]]];
conditionNumber=Max@#/Min@#&[SingularValueList@#]&;
randomSampleAtMostN=Function[{set,n},If[Length@set<n,set,RandomSample[set,n]]];
axisAngleToQuaternion=Function[axisAngle,Module[{angle=Norm@axisAngle},Prepend[Sin[angle/2] Normalize@axisAngle,Cos[angle/2]]]];
axisAngleFromQuaternion=Function[q,Module[{angle=2 ArcTan[q[[1]],Norm@q[[2;;]]]},angle Normalize[q[[2;;]]]]];
quadrticAssignmentExperiment=Function[{},
	n=3;SeedRandom[1003];{W,M}=RandomReal[1,{2,n,n}];as=Array[a,n{1,1}];\[Lambda]=1;
	f[as_?(NumericQ[#[[1,1]]]&)]:=With[{U=MatrixExp[as-as\[Transpose]]},Tr[W.Abs[U].M.Abs[U]\[Transpose]]+\[Lambda] pnorm[U,1]];
	Print[r=NMinimize[f[as],Flatten@as];//AbsoluteTiming];
	U=MatrixExp[as-as\[Transpose]]/.r[[2]];
	Print[MatrixForm/@{W,M,U,W.Abs[U].M.Abs[U]\[Transpose]}];
];

findSimilarityHomography=Function[imgPair,Module[{tr=Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Similarity"]},
	If[NumberQ[tr[[1]]]&&TransformationFunction===Head[tr[[2]]],{tr[[1]],TransformationMatrix[tr[[2]]]}]]];
findPerspectiveHomography=Function[imgPair,Module[{tr=Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Perspective"]},
	If[NumberQ[tr[[1]]]&&TransformationFunction===Head[tr[[2]]],{tr[[1]],TransformationMatrix[tr[[2]]]}]]];


quaternionDistance=Function[{q1,q2},Sqrt[1-quaternionFromRotationMatrix[LinearSolve@@(quaternionToRotationMatrix/@{q1,q2})][[1]]^2]];
(*quaternionDistance=Function[{q1,q2},2ArcCos@quaternionMul[quaternionConjugate[q1],q2]];*)
(*rotateByQuaternion[v,q] always equal to quaternionToRotationMatrix[q].v, and not equal to v.quaternionToRotationMatrix[q].
When q is unit quaternion, rotateByQuaternion[v,q] is equal to Rest@quaternionMul[quaternionMul[q,Prepend[v,0]],quaternionConjugate[q]]*)
rotateByQuaternion=Compile[{{v,_Real,1},{q,_Real,1}},
	(*Quat2Matrix[#].{0,1,0} == RotateByQuaternion[{0,1,0},#]*)
	{q[[3]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])-q[[4]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[1]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[2]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	-q[[2]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[1]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])+q[[4]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[3]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]]),
	q[[1]] (-q[[3]] v[[1]]+q[[2]] v[[2]]+q[[1]] v[[3]])+q[[2]] (q[[4]] v[[1]]+q[[1]] v[[2]]-q[[2]] v[[3]])-q[[3]] (q[[1]] v[[1]]-q[[4]] v[[2]]+q[[3]] v[[3]])-q[[4]] (-q[[2]] v[[1]]-q[[3]] v[[2]]-q[[4]] v[[3]])}
	/ (Norm[q]^2)
	];
rotateByQuaternionSlow=Function[{v,q},quaternionToRotationMatrix[q].v];
expPureQuaternion=Function[w,(*Length@w==3*)If[w=={0,0,0},{1,0,0,0},With[{mod=Norm@w},Prepend[Sin@mod w/mod,Cos@mod]]]];
quaternionMul=Compile[{{q,_Real,1},{q2,_Real,1}},
	{q[[1]] q2[[1]]-q[[2]] q2[[2]]-q[[3]] q2[[3]]-q[[4]] q2[[4]],q[[2]] q2[[1]]+q[[1]] q2[[2]]-q[[4]] q2[[3]]+q[[3]] q2[[4]],
	q[[3]] q2[[1]]+q[[4]] q2[[2]]+q[[1]] q2[[3]]-q[[2]] q2[[4]],q[[4]] q2[[1]]-q[[3]] q2[[2]]+q[[2]] q2[[3]]+q[[1]] q2[[4]]}];
quaternionConjugate=Compile[{{q,_Real,1}},{q[[1]],-q[[2]],-q[[3]],-q[[4]]}];
quaternionToRotationMatrix=Function[qin,With[{q=Normalize@qin},
	With[{s = q[[1]],vx=q[[2]],vy=q[[3]],vz=q[[4]]},Partition[{
        1 - 2 * vy * vy - 2 * vz * vz,
        2 * vx * vy - 2 * s * vz,
        2 * vx * vz + 2 * s * vy,
        2 * vx * vy + 2 * s * vz,
        1 - 2 * vx * vx - 2 * vz * vz,
        2 * vy * vz - 2 * s * vx,
        2 * vx * vz - 2 * s * vy,
        2 * vy * vz + 2 * s * vx,
        1 - 2 * vx * vx - 2 * vy * vy},3]
	]]];
(*SeedRandom[1003];q=RandomReal[1,4];v=RandomReal[1,3];
rs={rotateByQuaternion[v,q],quaternionToRotationMatrix[q].v,v.quaternionToRotationMatrix[q]
	,Rest@quaternionMul[quaternionMul[q,Prepend[v,0]],quaternionConjugate[q]],};MatrixForm/@rs
{rs[[1]]==rs[[2]],rs[[2]]==rs[[3]]}*)
quaternionDerivative=Compile[{{q,_Real,1},{w,_Real,1}},
	{1/2 (-q[[2]] w[[1]]-q[[3]] w[[2]]-q[[4]] w[[3]]),1/2 (q[[1]] w[[1]]-q[[4]] w[[2]]+q[[3]] w[[3]]),1/2 (q[[4]] w[[1]]+q[[1]] w[[2]]-q[[2]] w[[3]]),1/2 (-q[[3]] w[[1]]+q[[2]] w[[2]]+q[[1]] w[[3]])}];
(*These are equal:*)
(*SeedRandom[1003];\[Omega]=RandomReal[1,3];dt=0.001;
{MatrixExp[skewOmega[\[Omega]] dt],rotateByQuaternion[v,expPureQuaternion[w dt]],
	quaternionToRotationMatrix[{1,0,0,0}+quaternionDerivative[{1,0,0,0},\[Omega]] dt]}*)

quaternionFromRotationMatrix = Function[mIn, 
     Module[{m = Flatten@mIn, a2, b2, c2, d2, ab, ac, ad, cd, db, bc, m2, a, b, c, d},
          a2 = 0.25 * (1 + m[[1]] + m[[5]] + m[[9]]);
          b2 = 0.25 * (1 + m[[1]] - m[[5]] - m[[9]]);
          c2 = 0.25 * (1 - m[[1]] + m[[5]] - m[[9]]);
          d2 = 0.25 * (1 - m[[1]] - m[[5]] + m[[9]]);
          ab = 0.25 * (m[[8]] - m[[6]]);
          ac = 0.25 * (m[[3]] - m[[7]]);
          ad = 0.25 * (m[[4]] - m[[2]]);
          cd = 0.25 * (m[[8]] + m[[6]]);
          db = 0.25 * (m[[3]] + m[[7]]);
          bc = 0.25 * (m[[4]] + m[[2]]);
          m2 = Max@@{a2, b2, c2, d2};
          Which[m2 == 0,a = b = c = d = 0;
            ,a2 == m2,
              a = Sqrt[a2];
              b = ab / a;
              c = ac / a;
              d = ad / a;
            ,b2 == m2,
              b = Sqrt[b2];
              a = ab / b;
              c = bc / b;
              d = db / b;
            ,c2 == m2,
              c = Sqrt[c2];
              a = ac / c;
              b = bc / c;
              d = cd / c;
            , True,
              d = Sqrt[d2];
              a = ad / d;
              b = db / d;
              c = cd / d;
            ];
           {a, b, c, d}
        ]];
(*Max@@Abs@Table[q=Normalize@RandomReal[1,4];Norm[q-quaternionFromRotationMatrix@quaternionToRotationMatrix@q],{10000}]*)
integrateAngularVelocityVerlet=Function[{ts,wxyzs},
	MapThread[{#2,#1}&,
		{FoldList[Normalize[#1+quaternionDerivative[#1,#2[[1]]]#2[[2]]]&,{1,0,0,0},MapThread[{#1,#2}&,{Most[wxyzs],Differences[ts]}]],ts}]];
variableWithRandomInitial=Function[vs,variableWithInitial[vec@vs,RandomReal[1,Dimensions@vec@vs]]];
variableWithInitial=Function[{xs,inits},MapThread[List,{vec@xs,vec@inits},1]];
rowColumnToXy=Function[{row,column,height},{column-1/2,height-row-1/2}];
xyToRowColumn=Function[{x,y,height},Round@{height-y+1/2,x+1/2}];
pixelCoordToImageCoord=Function[{pixelCoord,imgDim},(pixelCoord-{1,1})imgDim/(imgDim-{1,1})];
(*skewOmega=Compile[{{w,_Real,1}},With[{p=w[[1]],q=w[[2]],r=w[[3]]},{{0,-r,q},{r,0,-p},{-q,p,0}}]];*)
skewOmega=-Normal@HodgeDual@#&;
inverseSkewOmega=Function[m,1/2{m[[3,2]]-m[[2,3]],m[[1,3]]-m[[3,1]],m[[2,1]]-m[[1,2]]}];
(*fourier2D=Function[m,Transpose[Fourier/@Transpose[Fourier/@m]]];*)
(*inverseFourier2D=Function[m,Transpose[InverseFourier/@Transpose[InverseFourier/@m]]];*)
phaseCorrelation=Function[{img1,img2},Module[{g1,g2},
	{g1,g2}=Fourier@ImageData@#&/@{img1,img2};InverseFourier@Map[Normalize,g1 Conjugate[g2],{2}]
	]];
rotateFourier=Function[m,RotateRight[m,Round[Dimensions[m]/2]]];
lieBracket=#.#2-#2.#&;
antiDiagonalMatrix=Reverse@DiagonalMatrix@#&;
antiDiagonal=Diagonal@Reverse@#&;
kroneckerTranspose=Function[{A,dims},Module[{perfectShuffle=Function[{AA,p,q},AA[[Join@@Table[Range[i,p q,q],{i,q}]]]]},
	Transpose[perfectShuffle[Transpose[perfectShuffle[A,dims[[1,1]],dims[[2,1]]]],dims[[1,2]],dims[[2,2]]]]]];
nilTrace=ReplacePart[#,{-1,-1}->-(Tr@#-#[[-1,-1]])]&;
schurComplement=Function[{m,k},m[[;;k,;;k]]-m[[;;k,k+1;;]].LinearSolve[m[[k+1;;,k+1;;]],m[[k+1;;,;;k]]]];
hermitian=(#+ConjugateTranspose[#])/2&;
skewHermitian=(#-ConjugateTranspose[#])/2&;
strictLowerTriangle=Function[m,m Table[Boole[i>j],{i,Dimensions[m][[1]]},{j,Dimensions[m][[2]]}]];
lowerTriangle=Function[m,m Table[Boole[i>=j],{i,Dimensions[m][[1]]},{j,Dimensions[m][[2]]}]];
upperTriangle=Transpose[lowerTriangle@Transpose@#]&;
strictUpperTriangle=Transpose[strictLowerTriangle@Transpose@#]&;
polarDecomposition={#[[1]].Transpose[#[[3]]],#[[3]].#[[2]].Transpose[#[[3]]]}&@SingularValueDecomposition@#&;
rgbVector=(*{0.2992707708537881`,0.587031571460754`,0.11375064544931468`};*){0.299,0.587,0.114};
skewTDistribution=Function[{\[Lambda],\[Nu]},
	With[{m=Gamma[(\[Nu]-1)/2]Sqrt[\[Nu]-2](\[Lambda]-1/\[Lambda])/(Gamma[\[Nu]/2]Sqrt[Pi])},With[{s=Sqrt[\[Lambda]^2+\[Lambda]^-2-1-m^2]},
	Function[z,2\[Lambda] s Gamma[(\[Nu]+1)/2]/((1+\[Lambda]^2)Gamma[\[Nu]/2]Sqrt[Pi(\[Nu]-2)])(1+\[Lambda]^(-2Sign[z+m/s])(m+s z)^2/(\[Nu]-2))^(-(\[Nu]+1)/2)]]]];
assemble=Function[ms,ImageData@ImageAssemble[Map[Image,ms,{Length@Dimensions@ms-2}]]];
(*imageResizeMaxSize=Function[{img,size},ImageResize[img,Round[ImageDimensions[img] size/Max@ImageDimensions[img]]]];*)
blockingImageResize=Function[{img,dim},Module[{window=Array[1.&,dim]},ColorCombine[Image@ArrayFlatten@Map[# window&,ImageData@#,{2}]&/@ColorSeparate@img]]];
gaussianTransform=Function[{g,x,s},Module[{z},Simplify[With[{g2=g/. x->Sqrt[z]},-((Sqrt[2 \[Pi] s] InverseLaplaceTransform[g2,z,1/(2 s)])/(2 s^2))],{s>=0}]]]
chessboard=Function[{dim,divisor},With[{dim2=dim/divisor},
	KroneckerProduct[Table[Boole[0==Mod[i+j,2]],{i,divisor[[1]]},{j,divisor[[2]]}],Array[1&,dim2]]]];
lieBracketDistance=Function[{m,m2},Sqrt@Tr[#.Transpose[#]]&[m.Transpose[m2]-m2.Transpose[m]]];(*||AB\[Transpose]-BA\[Transpose]||_F, another non-equal def is ||A\[Transpose]B-B\[Transpose]A||_F*)
matrixGeometricMean=#.MatrixPower[Inverse[#].#2,1/2]&;
symmetrize=Function[t,Mean@Table[Transpose[t,p],{p,Permutations@Range@Length@Dimensions@t}]];
(*symmetrize@{{{1,2},{3,4}},{{5,6},{7,8}}}//MatrixForm*)
laplacianMatrix1D=Function[n,SparseArray[{Band[{1,1}]->2,Band[{1,2}]->-1,Band[{2,1}]->-1},{n,n}]];
(*There is little difference at (-1,-1) entry: laplacianMatrix1D[6]-(#.#\[Transpose]&@gradientMatrix1D[6])//MatrixForm*)
gradientMatrix1D=Function[n,SparseArray[{Band[{1,1}]->1,Band[{1,2}]->-1},{n,n}]];
balancedGradientMatrix1D=Function[n,SparseArray[{Band[{1,1}]->1,Band[{1,2}]->-1},{n,n}]-SparseArray[{{n,n}->1},{n,n}]];
normalizedLaplacianMatrixFromAdjacencyMatrix=Function[A,Module[{iD},iD=sparseDiagonalMatrix[If[#==0,1,#^(-0.5)]&/@N@Total@A];
	sparseIdentityMatrix@Length@A-iD.A.iD]];
laplacianMatrixFromAdjacencyMatrix=Function[A,Module[{D},D=N@Total@A;sparseDiagonalMatrix@D-SparseArray[A]]];
lyapunovSolveSymmetricTranspose=Function[{A,B,C},(*Solves A.X+X\[Transpose]B==C, when A,B symmetric*)If[Transpose[A]!=A||Transpose[B]!=B,Abort[]];
	LyapunovSolve[A,B,(C+C\[Transpose])/2]+LyapunovSolve[A,B,(C-C\[Transpose])/2]];
lyapunovSolveTwoSided=Function[{A,B,C,D,E},(*Solves A.X+X.B+C.X.D==E*)Module[{W},
	W=SparseArray[KroneckerProduct[sparseIdentityMatrix@Length@B,SparseArray@A]
			+KroneckerProduct[SparseArray[B]\[Transpose],sparseIdentityMatrix@Length@A]]+KroneckerProduct[SparseArray[D]\[Transpose],SparseArray@C];
	Transpose@ArrayReshape[LinearSolve[W,vec@E,Method->{"Krylov",Method -> "ConjugateGradient",Preconditioner->"ILU0"}]
		,{Dimensions[B][[1]],Dimensions[A][[2]]}]
	]];
grayscaleDistanceSimilarityMatrix=Function[{m,\[Tau],radius},Module[{i,j,dim=Dimensions@m,eps=0.0001,r},
	r=Reap[Do[Do[If[{i,j}!={ii,jj},With[{difference=2 Abs[m[[ii,jj]]-m[[i,j]]]/(eps+m[[ii,jj]]+m[[i,j]]),distance=Abs[ii-i]+Abs[jj-j]},
		If[difference<\[Tau],With[{weight=Exp[-difference^2 distance]},If[weight>0.1,Sow[{dim[[1]](j-1)+i,dim[[1]](jj-1)+ii}]]]]]]
		,{ii,Max[i-radius,1],Min[i+radius,dim[[1]]]},{jj,Max[j-radius,1],Min[j+radius,dim[[2]]]}],{i,dim[[1]]},{j,dim[[2]]}]][[2,1]];
	SparseArray[r->1.,Times@@dim{1,1}]]];
circularDifferenceMatrix=Function[n,Module[{m=Normal@SparseArray[{Band[{1,1}]->1,Band[{1,2}]->-1},n{1,1}]},m[[n,1]]=-1;m]];
circularGradientMatrix1D=circularDifferenceMatrix;
truncTensor=Function[{d,k},d ArrayReshape[Normal@SparseArray[Thread[Ordering[Abs@Flatten@d][[-k;;]]->1],Length@Flatten@d],Dimensions@d]]
(*http://mathematica.stackexchange.com/questions/341/implementing-discrete-and-continuous-hilbert-transforms*)
hilbertTransform=Function[{f,u,t}, Module[{fp = FourierParameters -> {1, -1}, x},
    FullSimplify@InverseFourierTransform[-I (2 HeavisideTheta[x] - 1) FourierTransform[f, u, x, fp],x, t, fp]]];
hilbert=Function[data, Module[{fopts = FourierParameters -> {1, -1}, e, n},
   e = Boole[EvenQ[n=Length[data]]]; 
   Im[InverseFourier[Fourier[data, fopts] PadRight[ArrayPad[ConstantArray[2, Quotient[n, 2] - e], {1, e}, 1], n],fopts]]]];
differenceArgs=Function[l,If[#<=-Pi,#+2Pi,If[#>Pi,#-2Pi,#]]&/@Differences@l];
(*ContourPlot[Evaluate[Table[Exp[x]Sin[y]==i,{i,-3,3}]~Join~Table[-Exp[x]Cos[y]==i,{i,-3,3}]],{x,-3,3},{y,-3,3}]
ContourPlot[Evaluate[Table[(*Exp[x]*)1/(1+y^2)==i,{i,-3,3}]~Join~Table[(*Exp[x]*)y/(1+y^2)==i,{i,-3,3}]],{x,-3,3},{y,-3,3}]*)
(*hilbertTransform[#, v, w] & /@ {Sin[v], Cos[v], 1/(1 + v^2), Sinc[v], DiracDelta[v]}*)
randomSpecialOrthogonalMatrix=matrixExpSpecialOrthogonal@RandomReal[1,#{1,1}]&;
randomOrthogonalMatrix=First@SingularValueDecomposition@RandomReal[1,#{1,1}]&;
randomUnitaryMatrix=First@SingularValueDecomposition@RandomComplex[{-1-I,1+I},#{1,1}]&;
kroneckerSum=KroneckerProduct[#,IdentityMatrix@Length@#2]+KroneckerProduct[IdentityMatrix@Length@#,#2]&;
vec=Function[t,Flatten@Transpose[t,Reverse@Range[Length@Dimensions@t]]];
unVec=Function[{v,dim},checkEq["unVec, Times@@dim\[NotEqual]Length@v",Times@@dim,Length@v];Transpose@Partition[v,dim[[1]]]];
blockVec=Function[{t,k},Module[{f,n=Length[Dimensions[t]]},vec[Map[f@@#&,t,{n-k}]]/.(f->List)]];
vanLoanRearrangement=Function[{m,dim2},vec/@blockVec[Partition[m,dim2],2]];
kroneckerDecomposition=Function[{m,dim2},{Transpose@Partition[#[[1]],First[Dimensions[m]/dim2]],Transpose@Partition[#[[2]],dim2[[1]]]}&@
	(Sqrt[#[[2,1,1]]]{First@Transpose@#[[1]],First@Transpose@#[[3]]}&@SingularValueDecomposition[vanLoanRearrangement[m,dim2],1])];
kroneckerDecompositionOrthogonal=Function[{rs,n},Module[{f},
	f=Function[bscs,Module[{cs=procrustesKronecker[rs,bscs[[1]]]},{procrustesKroneckerDual[rs,cs],cs}]];
	FixedPoint[f,{IdentityMatrix[n],IdentityMatrix[Length[rs]/n]},10]]];
dotProduct=vec[#].vec[#2]&;
(*ms=Table[RandomReal[1,{3,3}],{2}];vec[ms[[1]].ms[[2]]]==KroneckerProduct[IdentityMatrix[3],ms[[1]]].vec[ms[[2]]]==KroneckerProduct[Transpose@ms[[2]],IdentityMatrix[3]].vec[ms[[1]]]*)
(*randomSparseTensorStrict=Function[{dim,nnzRatio},Module[{num=Floor[nnzRatio Times@@dim],taken},
	While[Length@DownValues[taken]<num,With[{cand=Table[RandomInteger[{1,dim[[i]]}],{i,Length@dim}]},taken[cand]=0.]];
	SparseArray[DownValues[taken][[;;,1,1,1]]->1.,dim]]];*)
(*randomSparseTensor=Function[{dim,nnzRatio},Module[{num=Floor[nnzRatio Times@@dim]},
	SparseArray[Table[Table[RandomInteger[{1,dim[[i]]}],{i,Length@dim}],{2 num}][[;;num]]->1.,dim]]];*)
randomSparseTensor=Function[{dim,nnzRatio},Module[{totalNum=Times@@dim,num,rules},num=Round[nnzRatio totalNum];
	rules=(1+{Quotient[#,dim[[2]]],Mod[#,dim[[2]]]})->1&/@RandomSample[Range[0,totalNum-1],num];
	SparseArray[rules,dim]]];
(*randomSparseTensor[{2,2,2},0.5]//Normal//MatrixForm*)
rotateTensor=Function[{T,m,n},(*rotate m-th level of T will appear as n-th level*)Transpose[T,Insert[Delete[Range@Length@Dimensions@T,n],n,m]]];
(*m=RandomReal[1,{2,3,4}];{rotateTensor[m,1,1]==rotateTensor[m,2,2]==rotateTensor[m,3,3],rotateTensor[m,1,2]==rotateTensor[m,2,1]==Transpose[m]}
{Dimensions@m,Dimensions@rotateTensor[m,1,3]}*)
(*tensorDot=Function[{T1,n1,T2,n2},rotateTensor[T1,n1,Length@Dimensions@T1].rotateTensor[T2,n2,1]];*)
matrixDotTensor=Function[{m,T,k},rotateTensor[m.rotateTensor[T,k,1],1,k]];
(*ms=RandomReal[1,#]&/@{{3,2},{2,3}};{ms[[1]].ms[[2]]==tensorDot[ms[[1]],2,ms[[2]],1],ms[[2]].ms[[1]]==Transpose[tensorDot[ms[[1]],1,ms[[2]],2]]}*)
foldXUs=Function[{X,Us,skips},Last@Fold[Function[{state,U},With[{i=state[[1]],P=state[[2]]},{i+1,If[MemberQ[skips,i],P,matrixDotTensor[U,P,i]]}]],{1,X},Us]];
(*m=RandomReal[1,{5,4}];r=SingularValueDecomposition[#,3]&@m;foldXUs[r[[2]],{r[[1]],r[[3]]},{}]==r[[1]].r[[2]].Transpose[r[[3]]]*)
sortXUs=Function[{X,Us},Module[{order=Length@Dimensions@X,t=X,tUs={},p},
	Do[ t=rotateTensor[t,j,1];p=FindPermutation[-Total@Flatten@Abs@#&/@t];t=rotateTensor[Permute[t,InversePermutation@p],1,j];
	If[Us!={},tUs=Append[tUs,ConjugateTranspose[Permute[ConjugateTranspose[Us[[j]]],InversePermutation@p]]]];
	,{j,order}];{t,tUs}
	]];
procrustes=Function[X,With[{r=SingularValueDecomposition[X,Min@Dimensions@X]},r[[1]].ConjugateTranspose[r[[3]]]]];
procrustesKronecker=Function[{A,B},(*Sovles \min_{U\in O(n)}\|A-B\otimes U\|_F*)
	procrustes@Total[MapThread[Times,{Partition[A,Dimensions@A/Dimensions@B],B},Length@Dimensions@B],Length@Dimensions@B]];
procrustesKroneckerDual=Function[{A,C},(*\( \min_{U\in O(n)}\|A-U\otimes C\|_F \)*)
	procrustesKronecker[kroneckerTranspose[A,{Dimensions@A/Dimensions@C,Dimensions@C}],C]];
sparseUnitaryDecompositionCost=Function[{D,X,Us,Y,\[Mu]},With[{DL=D-foldXUs[X,Us,{}]},{Total@#,#}&@{Total@Abs@Flatten@X,\[Mu]/2 Total@Flatten[DL^2],Flatten[Y].Flatten[DL]}]];
sparseUnitaryDecomposition=Function[{D,rank,maxIter,forceDiagonal},Module[{X,Y,Z,\[Mu],
		norm2=Sqrt[Mean[Abs[D]^2]],\[Rho]=1.01,
		order=Length@Dimensions@D,Us,diag=If[forceDiagonal,Diagonal,Identity],undiag=If[forceDiagonal,DiagonalMatrix,Identity]},
	Us=Table[ConjugateTranspose@Orthogonalize@RandomReal[1,{Min[Dimensions[D][[i]],rank],Dimensions[D][[i]]}],{i,order}];
	(*Us=Table[Transpose@Orthogonalize@IdentityMatrix[{Min[Dimensions[D][[i]],rank],Dimensions[D][[i]]}],{i,order}];*)
	\[Mu]=12.5/norm2;Y=Sign[D]/norm2;X=diag@foldXUs[D,ConjugateTranspose/@Us,{}];
	Do[
		(*If[j>1,Print[{IntegerString@j,sparseUnitaryDecompositionCost2[D,X,Us,Y,\[Mu]]}]];*)
		X=dShrinkage[1/\[Mu],diag@foldXUs[D+Y/\[Mu],ConjugateTranspose/@Us,{}]];
		(*X=diag[foldXUs[D+Y/\[Mu],Transpose/@Us,{}]/(1+1/\[Mu]((undiag@X)^2+10^-6)^((0.5-2)/2))];*)
		(*Print[{"X",sparseUnitaryDecompositionCost2[D,X,Us,Y,\[Mu]]}];*)
		Do[
			Us[[i]]=procrustes[unfoldTensor[D+Y/\[Mu],i].ConjugateTranspose[unfoldTensor[foldXUs[undiag@X,Us,{i}],i]]];
			(*Print[{"Us"<>IntegerString@i,sparseUnitaryDecompositionCost2[D,X,Us,Y,\[Mu]]}];*)
			,{i,order}];
(*		Z=foldXUs[undiag@X,Us,{}];
(*		With[{i=2},Print[MatrixForm/@{matrixDotTensor[Transpose[Us[[i]]],Z,i],foldXUs[undiag@X,Us,{i}]}]];Abort[];*)
		Do[
			Us[[i]]=procrustes[unfoldTensor[D+Y/\[Mu],i].Transpose[unfoldTensor[matrixDotTensor[Transpose[Us[[i]]],Z,i],i]]];
			(*Print[{"Us"<>IntegerString@i,sparseUnitaryDecompositionCost2[D,X,Us,Y,\[Mu]]}];*)
			,{i,order}];*)
		Y+=\[Mu](D-foldXUs[undiag@X,Us,{}]);\[Mu]*=\[Rho];
	,{j,maxIter}];
	sortXUs[undiag@X,Us]
	]];
zShrinkage=Function[{\[Tau],A,rank,maxIter},Module[{X,Us},
	{X,Us}=sparseUnitaryDecomposition[A,rank,maxIter,False];{foldXUs[dShrinkage[\[Tau],X],Us,{}],Us}]];
trimToRank=Function[{m,rank},(*only works for real matrix*)
	#[[1]].DiagonalMatrix@PadRight[Diagonal[#[[2]]][[;;Min[rank,Min@Dimensions@m]]],Length@#[[2]]].Transpose[#[[3]]]&@
		SingularValueDecomposition@m];
(*pseudoMatrixPlot=Function[moving,
	Graphics[{FaceForm@None,EdgeForm@Black,Rectangle[{1,-Dimensions[moving][[1]]},{1+Dimensions[moving][[2]],0}],FaceForm@Green}
		~Join~(Rectangle@toCoord@#&/@SparseArray[moving]["NonzeroPositions"]),Axes->True]];*)
pnorm=Function[{m,p},If[p==="en",entropy@Flatten@Abs@m,If[Length@Dimensions[m]==2&&p==2,Norm[m,"Frobenius"]
	,If[p<1,Total[Power[Flatten@Abs@m,p]]^(1/p),Norm[Flatten@m,p]]]]];
pnorm2=Function[{m,p},If[p=="en",pnorm[m,p],pnorm[m,p]^p]];
entropy=Total[If[#==0,0,-#Log[#]]&/@Abs@Flatten[#]]&;
schattenNorm=Function[{m,p},pnorm[SingularValueList[m],p]];
weightedSchattenNorm=Function[{m,p,weights},(*Is a norm when weights decreasing*)
	checkEq["weightedSchattenNorm",1,Length@Dimensions@weights];pnorm[SingularValueList[m] weights,p]];
vonNeumannEntropy=entropy[SingularValueList[#1]]&;
(*equivalently: vonNeumannEntropy=-Tr[#.MatrixLog[#]]&;*)
(*N[entropy/@{{3,0,0},{3,0},{3,0,0,0},{2,1},{1.5,1.5},{0,3}}];
Select[Table[With[{x=DiagonalMatrix@RandomReal[10{-1,1},2],m=First@SingularValueDecomposition@RandomReal[{-1,1},2{1,1}]},
	{x,m,Norm[x,1]<Norm[m.x,1],entropy@Flatten@x<entropy@Flatten[m.x]}],{10}],(Not[#[[-2]]]||Not[#[[-1]]])&]*)
until=Function[{cond,cmd},While[True,Unevaluated@cmd;If[cond,Return[]]],{HoldAll}];
simplexVolume=Function[vertices,Det[(#-First@vertices)&/@Rest@vertices]];
(*vertices=RandomReal[1,{3,2}];Graphics[{FaceForm[None],EdgeForm[Green],Rectangle[],Polygon@vertices,Text[simplexVolume@vertices]}]*)
SvdApprox=#[[1]].#[[2]].Transpose[#[[3]]]&@SingularValueDecomposition[#,#2]&;
poissonMatrix=Function[dim,With[{dim2=Times@@dim,m=dim[[1]]}
	,pack@N@SparseArray[{Band[{1,1}]->4,Band[{2,1},{dim2,dim2-1}]->Append[Table[-1,{m-1}],0]
		,Band[{1,2},{dim2-1,dim2}]->Append[Table[-1,{m-1}],0],Band[{1+m,1}]->-1,Band[{1,1+m}]->-1},dim2{1,1}]]];
balancedLaplacianMatrix1D=Function[n,SparseArray[{Band[{1,1}]->2,Band[{1,2}]->-1,Band[{2,1}]->-1},{n,n}]-SparseArray[{{1,1}->1,{n,n}->1},{n,n}]];
balancedPoissonMatrix=Function[dim,kroneckerSum[balancedLaplacianMatrix1D[dim[[2]]],balancedLaplacianMatrix1D[dim[[1]]]]];
circularPoissonMatrix=Function[dim,kroneckerSum[circularLaplacianMatrix1D[dim[[2]]],circularLaplacianMatrix1D[dim[[1]]]]];
(*The larger the image, the lower sampling rate we need. Then we can juxtopose images for lower sampling rate?*)
CombineRGB=Function[m,With[{n=Dimensions[m][[2]]},ColorCombine[Image/@Transpose[Map[Partition[#,n/3]&,m],{2,1,3}]]]];
unDShrinkage=Function[{\[Tau],A},Map[If[#==0,0,Sign[#](Abs[#]+\[Tau])]&,A,{-1}]];
dShrinkage=Function[{\[Tau],A},
	If[NumericQ@\[Tau],Map[Sign[#]Max[Abs[#]-\[Tau],0]&,A,{Length@Dimensions@A}],dShrinkageHadamard[\[Tau],A]]];
dShrinkageHadamard=Function[{\[Tau]m,m},Sign[m]Map[Max[0.,#]&,Abs[m]-\[Tau]m,{Length@Dimensions@m}]];
(*dShrinkage=Compile[{{\[Tau],_Real},{A,_Real,2}},Map[Sign[#]Max[Abs[#]-\[Tau],0]&,A,{2}]];*)
cShrinkage=Function[{\[Tau],A},Map[If[Abs[#]>\[Tau],# (Abs[#]-\[Tau])/Abs[#],0]&,A,{Length[Dimensions[A]]}]];
cShrinkageHadamard=Function[{\[Tau]m,A},MapThread[If[Abs[#]>#2,# (Abs[#]-#2)/Abs[#],0]&,{A,\[Tau]m},Length[Dimensions[A]]]];
(*Cannot generalize to argmin_X 1/2 ||X-Y||_F^2+\[Tau]|AX+B|*)
dShrinkageVector=Function[{\[Tau],y,a,b},(*argmin_x 1/2 ||x-y||_ 2^2+\[Tau]|a^T x+b|, where "a" is vector and b is scalar.*)
	With[{ayb=a.y+b,ta2=\[Tau] a.a},Which[a.a==0,y,ayb>ta2,y-\[Tau] a,ayb<-ta2,y+\[Tau] a,True,-b a/(a.a)]]];
sShrinkage=Function[{\[Tau],A},Module[{U,S,V},{U,S,V}=SingularValueDecomposition[A,Min@Dimensions@A];U.dShrinkage[\[Tau],S].Transpose[V]]];
unSShrinkage=Function[{\[Tau],A},Module[{U,S,V},{U,S,V}=SingularValueDecomposition[A,MatrixRank@A];U.unDShrinkage[\[Tau],S].Transpose[V]]];
(*sShrinkage=Compile[{{\[Tau],_Real},{A,_Real,2}},(#[[1]].dShrinkage[\[Tau],#[[2]]].#[[3]])&@SingularValueDecomposition[A,Min@Dimensions@A]];*)
(* \(\min_X \tau\|X\|_a + \frac12 \|A X - B\|_F^2 \) *)
shrinkageOverLinear=Function[{shrinkageOp,\[Tau],AtA,AtB,maxIter},Module[{\[Mu]=30\[Tau]+10^-20,j,f,OX,X=RandomReal[1,{Dimensions[AtA][[2]],Dimensions[AtB][[2]]}]},
	f=Function[X,shrinkageOp[\[Tau]/\[Mu],LinearSolve[SparseArray[AtA+\[Mu] sparseIdentityMatrix[Length[AtA]]],SparseArray[\[Mu] X + AtB]]]];
	Do[OX=X;X=f[X];If[pnorm[X-OX,2]/Times@@Dimensions@X<0.00001,Print[{"shrinkageOverLinear #iter",j}];Break[]];
	,{j,maxIter}];X]];
(* \(\min_X \tau\|X\|_a + \frac{\mu}{2} \| X - Z \|_F^2 + <Y, X - Z> + \frac12 \|A X - B\|_F^2 \) *)
shrinkageOverLinearAdmm=Function[{shrinkageOp,\[Tau],AtA,AtB,maxIter},Module[{\[Mu],j,f,OX,X,Y,Z,\[Rho]=1.01,norm2},
	Z=LinearSolve[AtA,AtB];X=RandomReal[1,{Dimensions[AtA][[2]],Dimensions[AtB][[2]]}];Y=0X;norm2=SingularValueDecomposition[Z,1][[2,1,1]];\[Mu]=12.5/norm2;
	Do[(*Print[MatrixForm/@{j,X,Y,Z}];*)(*Print[Dimensions@X];*)
		OX=X;X=shrinkageOp[\[Tau]/\[Mu],Z-Y/\[Mu]];Z=LinearSolve[SparseArray[AtA+\[Mu] sparseIdentityMatrix[Length[AtA]]],SparseArray[\[Mu] X + AtB + Y]];
		If[pnorm[X-OX,2]/Times@@Dimensions@X<0.00001,Print[{"shrinkageOverLinearAdmm #iter",j}];Break[]];Y+=\[Mu](X-Z);\[Mu]*=\[Rho];
	,{j,maxIter}];X]];
shrinkage=Function[{typ,\[Tau],m},Switch[typ,"SVD",sShrinkage[\[Tau],m],"Fourier",Re@InverseFourier@cShrinkage[\[Tau],Fourier@m],
	"L1",dShrinkageHadamard[\[Tau],m],_,Print["Unknown type:"<>typ,Abort[];]]];
RpcaMeanFilter=Function[{D,\[CapitalOmega],iter},RpcaMeanFilterWithInit[D,\[CapitalOmega],0D,0D,iter]];
RpcaMeanFilterWithInit=Function[{D,\[CapitalOmega],initL,initS,iter},Module[{f,L},f=Function[xs,(1-\[CapitalOmega]) MeanFilter[xs,1] + D];L=Nest[f,initL,iter];{L,D-L}]];
Rpca2=Function[{D,\[CapitalOmega],iter},Rpca2WithInit[D,\[CapitalOmega],0D,0D,iter]];
Rpca2WithInit=Function[{D,\[CapitalOmega],initL,initS,iter},Rpca2UnitaryWithInit[D,\[CapitalOmega],initL,initS,iter,"SVD"]];
RpcaFourierWithInit=Function[{D,\[CapitalOmega],initL,initS,iter},Rpca2UnitaryWithInit[D,\[CapitalOmega],initL,initS,iter,"Fourier"]];
RpcaFourierLaplaceWithInit=Function[{D,\[CapitalOmega],initL,initS,iter},Rpca2UnitaryWithInit[D,\[CapitalOmega],initL,initS,iter,"FourierLaplace"]];
RpcaHadamardWithInit=Function[{D,\[CapitalOmega],initL,initS,iter},Rpca2UnitaryWithInit[D,\[CapitalOmega],initL,initS,iter,"Hadamard"]];
Rpca2UnitaryWithInit=Function[{D,\[CapitalOmega],initL,initS,iter,unitaryType},Module[{norm2,\[Lambda],\[Eta]=10.,Y,L=initL,S=initS,\[Mu],\[Rho],m,n,Zl,Zs,OL},
	(*SVD: min ||L||_* + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	(*Fourier: min ||FLF^T||_ 1 + \[Lambda]||\[CapitalOmega] S||_ 1+\[Mu]/2||D-L-S||_F^2+<Y,D-L-S> *)
	(*L,S,D are m-by-kn. \[CapitalOmega] is nonzeros*)
	{m,n}=Dimensions[D];\[Lambda]=Min[0.5,100./GeometricMean@{m,n}];norm2=SingularValueDecomposition[D,1][[2,1,1]];
	Y=0D;(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]];*)\[Mu]=12.5/norm2;\[Rho]=1.005;
	Do[Zl=Y/\[Mu]+D-S;OL=L;
	L=Switch[unitaryType,"SVD",sShrinkage[1/\[Mu],Zl],
		"FourierLaplace",Re@InverseFourier@cShrinkageHadamard[0.001/\[Mu](KroneckerProduct@@(Sqrt[Range[0,#-1]]&/@Dimensions@Zl)),Fourier@Zl],
		"Fourier",Re@InverseFourier@cShrinkage[0.02/\[Mu],Fourier@Zl],
		"Hadamard",With[{padded=padToTwoPower@Zl},Take[unVec[hadamardTransform@dShrinkage[0.02/\[Mu],hadamardTransform@vec@padded],Dimensions@padded],
			Sequence@@Dimensions@Zl]]
	];
	Zs=Y/\[Mu]+D-L;
	S=\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],Zs]+(1-\[CapitalOmega]) Zs;
	If[pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
    Y=Y+\[Mu](D-L-S);
	\[Mu]=\[Rho] \[Mu];If[Mod[j,30]==0,printTemporary@ImageAdjust@Image@Re[L]];(*(scores=Append[scores,#];Print[#])&@evalPredicted[L,testM2];*)
	,{j,iter}];
	{L,S}]];
(*Juxtaposing doesn't work.*)
Rpca2LabWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Rpca2LabMayJuxtaposeWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,True]];
Rpca2LabMayJuxtaposeWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,juxtapose},
	Module[{L,S,k,m,n,ab,coeffMatrix},(*Only complete the non-Gray parts*)
	{k,m,n}=Dimensions@Rgb;
	coeffMatrix=Function[k,Table[PadRight[Flatten@{Array[0&,i-1],{1,-1}},k],{i,k-2}]~Join~{Table[If[i==k,k-1,-1],{i,k}]}][Length@Rgb];
	ab=coeffMatrix.Rgb;Print[Dimensions@ab];
	(*Print[Dimensions/@{*)
	Print[Image/@{unfoldTensor[If[juxtapose,ArrayFlatten@{{Gray,#}},#]&/@ab,3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Array[1&,Dimensions@#],#}},#]&/@\[CapitalOmega]in[[;;k-1]],3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Gray,#}},#]&/@initL[[;;k-1]],3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Array[0&,Dimensions@#],#}},#]&/@initS[[;;k-1]],3]}];
	{L,S}=foldToTensor[#,{k-1,m,n},3]&/@
		Rpca2WithInit[unfoldTensor[If[juxtapose,ArrayFlatten@{{Gray,#}},#]&/@ab,3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Array[1&,Dimensions@#],#}},#]&/@\[CapitalOmega]in[[;;k-1]],3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Gray,#}},#]&/@initL[[;;k-1]],3]
			,unfoldTensor[If[juxtapose,ArrayFlatten@{{Array[0&,Dimensions@#],#}},#]&/@initS[[;;k-1]],3],iter];
	L=PseudoInverse[Prepend[coeffMatrix,Array[1/k&,k]]].Prepend[Partition[#,Dimensions@Gray][[1,2]]&/@L,Gray];
	{L,Rgb-L}]];
correctWithGray=Function[{Gray,L},L+Table[Gray-Mean[L],{Length@L}]];
Rpca2PseudoColor=Function[{Gray,Rgb,\[CapitalOmega]in,iter},Rpca2PseudoColorWithInit[Gray,Rgb,\[CapitalOmega]in,0Rgb,0Rgb,iter]];
Rpca2PseudoColorWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{L,S},
	{L,S}=foldToTensor[#,Dimensions@Rgb,2]&/@
		Rpca2WithInit[unfoldTensor[Rgb,2],unfoldTensor[\[CapitalOmega]in,2],unfoldTensor[initL,2],unfoldTensor[initS,2],iter];
	{correctWithGray[Gray,#]&@L,S}]];
Rpca=Function[{D,\[Lambda],iter},
	Module[{Y,L,S,norm2,\[Mu],\[Rho]=1.02,m=Dimensions[D][[1]],n=Dimensions[D][[2]],Zs,U,SS,V,Z,dnorm=Norm[D,"Frobenius"]},
		L=S=Table[0.,{m},{n}];norm2=SingularValueDecomposition[D,1][[2,1,1]];Y=Sign[D]/Max[norm2,Norm[Flatten@D,Infinity]/\[Lambda]];\[Mu]=1.25/norm2;
	Do[
		Zs=Y/\[Mu]+D-L;
		S=dShrinkage[\[Lambda]/\[Mu],Zs];
		L=sShrinkage[1/\[Mu],(Y+\[Mu](D-S))/\[Mu]];
		Z=D-L-S;
		Y=Y+\[Mu] Z;
		\[Mu]=\[Mu] \[Rho];
		If[Norm[Z,"Frobenius"]/dnorm < 10^-7,Break[]];
		,{i,iter}];
	{L,S}
	]];
maskFromString=Function[{str,textSize},
	ImageData@Binarize@Image@Graphics@
		Style[Text@StringJoin@Riffle[Partition[StringSplit[StringTake[str,600],""],30],"\n"],textSize]];
recoverFromMask=Function[{rawImg,mask,numIter},Module[{dim,img,L,S},
	dim=Dimensions@mask;
	img=Image@Mean[ImageData/@ColorSeparate@ImageResize[rawImg,Reverse@dim]];
	Print[{L,S}=Rpca2[ImageData[img] mask,mask,numIter];//AbsoluteTiming];
	{img,(*mask//Image,*)ImageData[img] mask//Image,Image[L]}]];
laplacianMatrixFromGray=Function[{gray,radius},Module[{dim=Dimensions@gray,rules},
	rules=Reap[Do[Sow[{i+(j-1)dim[[1]],i+(j-1)dim[[1]]}->1.(*001*)];
		Module[{neighbors=Select[{{i-1,j},{i+1,j},{i,j-1},{i,j+1}},pointInRectangle[#,{{1,1},dim}]&],gvals,tval=gray[[i,j]],csig,mgv},
		gvals=gray[[#[[1]],#[[2]]]]&/@neighbors;
		csig=Max[0.000002,Max[0.6 Mean[(#-Mean[#])^2&@Append[gvals,tval]],-Min[(gvals-tval)^2]/Log[0.01]]];
		MapThread[Sow[{i+(j-1)dim[[1]],#[[1]]+(#[[2]]-1)dim[[1]]}->#2]&
			,{neighbors,-#/Total@#&@Exp[-(gvals-tval)^2/csig]}];
		];
	,{i,dim[[1]]},{j,dim[[2]]}]][[2,1]];SparseArray[rules,dim[[1]]dim[[2]]{1,1}]]];
laplacianMatrixFromMatrix=Function[{gray,mask,radius},sparseDiagonalMatrix[1-vec[mask]].laplacianMatrixFromGray[gray,radius]+sparseDiagonalMatrix[vec[mask]]];
RpcaColorLevinWeissSimpleLaplaceWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,rgb,dim=Dimensions@Gray,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,L},
	A=SparseArray[sparseDiagonalMatrix[vec[\[CapitalOmega][[1]]]]+sparseDiagonalMatrix[1.-vec[\[CapitalOmega][[1]]]].poissonMatrix[Dimensions@Gray]];
	rgb=Transpose@Partition[LinearSolve[A,vec[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim[[1]]]&/@Rgb[[;;]];
	L=correctWithGray[Gray,#]&@rgb;{L,Rgb-L}]];
RpcaColorLevinWeissSimpleLaplaceNoCorrectionWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},(*LeastSquares cannot ensure agreeing with Gray*)
		Module[{A,rgb,dim=Dimensions@Gray,\[CapitalOmega]=SparseArray@\[CapitalOmega]in,vecRgb},
	A=SparseArray[DiagonalMatrix[vec[\[CapitalOmega][[1]]]]+DiagonalMatrix[1-vec[\[CapitalOmega][[1]]]].poissonMatrix[Dimensions@Gray]];
	vecRgb=LeastSquares[Join[KroneckerProduct[IdentityMatrix[3],A],SparseArray[1/3 KroneckerProduct[{{1,1,1}},IdentityMatrix@Length@A]]]
		,Join[Join@@(vec/@Rgb),vec@Gray],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}];
	{(*correctWithGray[Gray,#]&@*)First@foldToTensor[{vecRgb},Prepend[Dimensions@Rgb,1],1],0Rgb}]];
RpcaColorLevinWeissWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,rgb,dim=Dimensions@Gray,L},
	A=laplacianMatrixFromMatrix[Gray,\[CapitalOmega]in[[1]],1];(*Export["/tmp/A.csv",A];Export["/tmp/Gray.csv",Gray];*)
	rgb=Transpose@Partition[LinearSolve[A,vec[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim[[1]]]&/@Rgb[[;;]];
	(*Can't use k Gray - rg as will introduce blue artifacts.*)L=correctWithGray[Gray,#]&@rgb;(*Append[rg,Length[Rgb] Gray-Total@rg]*)
	{L,Rgb-L}]];
RpcaColorLevinWeissLabWithInit=Function[{Luminance,Rgb,\[CapitalOmega]in,initL,initS,iter},Module[{A,rgb,lab,dim=Dimensions@Luminance},
	lab=ColorSeparate[ColorCombine[Image/@Rgb],"LAB"];
	A=laplacianMatrixFromMatrix[Luminance,\[CapitalOmega]in[[1]],1];(*Export["/tmp/A.csv",A];Export["/tmp/Gray.csv",Gray];*)
	rgb=ImageData/@ColorSeparate[ColorCombine[Prepend[Image@Transpose@Partition[
		LinearSolve[A,vec@ImageData[#],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],dim[[1]]]&/@lab[[2;;]],Image@Luminance],"LAB"],"RGB"];
	{rgb,0Rgb}]];
RpcaColorSeparate=Function[{Gray,D,\[CapitalOmega],iter},RpcaColorSeparateWithInit[Gray,D,\[CapitalOmega],0D,0D,iter]];
RpcaColorSeparateWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter},
	(*\sum_i||L_i||_*+\sum_i||\[CapitalOmega]_i S_i||_ 1+\[Mu]1/2\sum_i||D_i-L_i-S_i||_F^2+\[Mu]2/2||L_i-k W||_F^2*)
	Module[{W=Gray,norm2,\[Lambda]
		,Y1,Y2,L=initL,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,m,n,Zl,Zs,OL},
	(*L,S,D,Y1,\[CapitalOmega] are k-by-m-by-n, Y2,W are m-by-n. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	{k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[SingularValueDecomposition[#,1][[2,1,1]]&/@D];
	Y2=0W;Y1=0D;(*Sign[D]/Max[norm2,Norm[Join@@D,Infinity]/\[Lambda]];*)\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	Do[
	OL=L;
	Zl=(Y1+\[Mu]1(D-S))/(\[Mu]1+\[Mu]2);
	Do[
		L[[i]]=sShrinkage[1/(\[Mu]1+\[Mu]2),Zl[[i]]+\[Mu]2 (k W-Total@L+L[[i]])/(\[Mu]1+\[Mu]2)];
	,{i,k}];
	Zs=Y1/\[Mu]1+D-L;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
	Y1=Y1+\[Mu]1(D-L-S);
	Y2=Y2+\[Mu]2(k W-Total@L);
	{\[Mu]1,\[Mu]2}*=\[Rho];If[Mod[j,30]==1,printTemporary[Image/@L]];
	,{j,iter}];
	{correctWithGray[Gray,#]&@L,S}]];
RpcaColor2=Function[{Gray,Rgb,\[CapitalOmega]in,iter},RpcaColor2WithInit[Gray,Rgb,\[CapitalOmega]in,0 Rgb,0 Rgb,iter]];
RpcaColor2WithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"SVD"]];
RpcaColorFourierWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"Fourier"]];
RpcaColorFourierLaplaceWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"FourierLaplace"]];
RpcaColorHadamardWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"Hadamard"]];
RpcaColorHadamardLaplaceWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"HadamardLaplace"]];
RpcaColorGrayWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter},RpcaColorUnitaryWithInit[Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,"Gray"]];
RpcaColorUnitaryWithInit=Function[{Gray,Rgb,\[CapitalOmega]in,initL,initS,iter,unitaryType},
	Module[{W=Gray,D=unfoldTensor[Rgb,2],\[CapitalOmega]=unfoldTensor[\[CapitalOmega]in,2],norm2,\[Lambda],k,dim,\[Eta]
		,Y1,Y2,L=unfoldTensor[initL,2],S=unfoldTensor[initS,2],X,\[Mu]1,\[Mu]2,\[Rho],T,T2,Ikn,WT,m,n,Zl,Zs,OL,svdW},
	(*L,S,D are m-by-kn. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	(*||L||_*+\lambda||\[CapitalOmega] S||_ 1+\[Mu]1/2||D-L-S||_F^2+\[Mu]2/2||L-X||_F^2+\[Eta]/2||X T-W||_F^2*)
	dim={k,m,n}=Dimensions[Rgb];\[Lambda]=Min[0.5,10./GeometricMean@{m,n}];norm2=SingularValueDecomposition[D,1][[2,1,1]];
	Y1=Y2=0D(*Sign[D]/Max[norm2,Norm[D,Infinity]/\[Lambda]]*);\[Eta]=\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.01;
	svdW={#[[1]],#[[2]],KroneckerProduct[IdentityMatrix[3],#[[3]]]}&@SingularValueDecomposition[W];
	X=L;T=Join@@Table[N@IdentityMatrix[n],{k}]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
(*	Print@{"L",schattenNorm[L,1],"\[CapitalOmega] S",pnorm[\[CapitalOmega] S,1],"D-L-S",pnorm[D-L-S,2],"L-X",pnorm[L-X,2],"XT-W",pnorm[X.T-W,2]
		,"rse",pnorm[Flatten@rgb-Flatten@foldToTensor[X,dim,2],2]/pnorm[Flatten@rgb,2]
		,"corr-rse",pnorm[Flatten@rgb-Flatten@correctWithGray[Gray,#]&@foldToTensor[X,dim,2],2]/pnorm[Flatten@rgb,2]};
	Print[{"{\[Lambda],\[Mu]1,\[Mu]2,\[Eta]}",{\[Lambda],\[Mu]1,\[Mu]2,\[Eta]}}];
	Print[L//Image];*)
    Zl=(Y1+Y2+\[Mu]1(D-S)+\[Mu]2 X)/(\[Mu]1+\[Mu]2);
	OL=L;L=Switch[unitaryType
		,"Fourier",Re@InverseFourier@cShrinkage[0.02/(\[Mu]1+\[Mu]2),Fourier@Zl]
		,"FourierLaplace",Re@InverseFourier@cShrinkageHadamard[0.001/(\[Mu]1+\[Mu]2)(KroneckerProduct@@(Sqrt[Range[0,#-1]]&/@Dimensions@Zl)),Fourier@Zl]
		(*,"FourierDCT",Re@FourierDCT[#,3]&@cShrinkage[0.01/(\[Mu]1+\[Mu]2),FourierDCT@Zl]*)
		,"Hadamard",With[{padded=padToTwoPower@Zl},Take[unVec[hadamardTransform@dShrinkage[0.02/(\[Mu]1+\[Mu]2),hadamardTransform@vec@padded],Dimensions@padded],
			Sequence@@Dimensions@Zl]]
		,"HadamardLaplace",With[{padded=padToTwoPower@Zl},Take[unVec[hadamardTransform@
			dShrinkageHadamard[0.0005/(\[Mu]1+\[Mu]2)vec[KroneckerProduct@@(Sqrt[Range[0,#-1]]&/@Dimensions@padded)]
				,hadamardTransform@vec@padded],Dimensions@padded],Sequence@@Dimensions@Zl]]
		(*,"Gray",ArrayFlatten[Map[svdW[[1]].#.Transpose[svdW[[3]]]&
			,dShrinkage[0.2/(\[Mu]1+\[Mu]2),Map[#.svdW[[3]]&,Partition[Transpose[svdW[[1]]].Zl,Dimensions@svdW[[3]]],{2}]],{2}],2]*)
		,"Gray",svdW[[1]].dShrinkage[0.2/(\[Mu]1+\[Mu]2),Transpose[svdW[[1]]].Zl.svdW[[3]]].Transpose[svdW[[3]]]
		,"SVD",sShrinkage[1/(\[Mu]1+\[Mu]2),Zl]];
	Zs=Y1/\[Mu]1+D-L;S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[Norm[L-OL,"Frobenius"]/Norm[L,"Frobenius"]<0.0005,Break[]];
	(*Print[{Norm[L-OL,"Frobenius"]/Norm[L,"Frobenius"],Norm[L-OL,"Frobenius"],Norm[L,"Frobenius"],Norm[(1-\[CapitalOmega])(L-rgb),"Frobenius"]}];*)
    X=Transpose@LinearSolve[SparseArray[(\[Eta] T2+\[Mu]2 Ikn)],Transpose[\[Eta] WT-Y2+\[Mu]2 L]];
    Y1=Y1+\[Mu]1(D-L-S);Y2=Y2+\[Mu]2(X-L);{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];
	If[Mod[j,30]==1,printTemporary[Image/@foldToTensor[X,dim,2]]];
	,{j,iter}];
	{correctWithGray[Gray,#]&@foldToTensor[X,dim,2],foldToTensor[S,dim,2]}]];
prepareDataRpcaColor=Function[{rawImgs,dim,missingRatio},Module[{\[CapitalOmega],imgs},
	imgs=ImageData@ImageResize[#,Reverse@dim]&/@rawImgs;\[CapitalOmega]=randomSparseTensor[{dim[[1]],dim[[2]]},1-missingRatio];
	{Mean[imgs],imgs,Table[\[CapitalOmega],{Length@imgs}]}]];
localColorConsistency=Compile[{{Gray,_Real,2},{Rgb,_Real,2},{\[CapitalOmega],_Real,2},{\[Tau],_Real},{radius,_Integer}},
	Module[{G=0 Rgb,eps=0.01,weight,m=Dimensions[Gray][[1]],n=Dimensions[Gray][[2]]},
	Table[
		If[\[CapitalOmega][[i,j]]==0,
			Table[
				If[{ii,jj}!={i,j}&&\[CapitalOmega][[ii,jj]]==1,With[{difference=2 Abs[Gray[[ii,jj]]-Gray[[i,j]]]/(eps+Gray[[ii,jj]]+Gray[[i,j]]),distance=Abs[ii-i]+Abs[jj-j]},
					If[difference<\[Tau],
						weight=Exp[-difference^2 distance];
						{G[[i,j]],G[[i,j+n]],G[[i,j+2n]]}+=weight{Rgb[[ii,jj]],Rgb[[ii,jj+n]],Rgb[[ii,jj+2n]]};
					]
				]]
			,{ii,Max[i-radius,1],Min[i+radius,m]},{jj,Max[j-radius,1],Min[j+radius,n]}];
			With[{newGray=(G[[i,j]]+G[[i,j+n]]+G[[i,j+2n]])/3},
				If[newGray!=0,
					{G[[i,j]],G[[i,j+n]],G[[i,j+2n]]}*=(Gray[[i,j]]/newGray);
				]
			]
		]
		,{i,m},{j,n}];
		G
	],CompilationTarget:>"C"];
localColorConsistencyInterpolation=Function[{gray,rgb,\[CapitalOmega],diffenceThreshold,radius},Module[{nRgb,nOmega
		,dim=Prepend[Dimensions@gray,Dimensions[rgb][[2]]/Dimensions[gray][[2]]]},
	nRgb=localColorConsistency[gray,rgb \[CapitalOmega],Normal@\[CapitalOmega],diffenceThreshold,radius];
	nOmega=SparseArray[SparseArray[nRgb]["NonzeroPositions"]->1,Dimensions@nRgb];
	(*Print[{nRgb//Image,nOmega nRgb//Image,CombineRGB[nRgb]}];*)
	{foldToTensor[nRgb,dim,2],foldToTensor[nOmega,dim,2]}]];
unfoldTensor=Function[{t,n},Module[{dim=Dimensions@t},Flatten/@Transpose[t,Insert[Range[2,Length@dim],1,n]]]];
foldToTensor=Function[{m,dim,n},Module[{ranks=Delete[Range@Length@dim,n]},Transpose[ArrayReshape[#,dim[[ranks]]]&/@m,Prepend[ranks,n]]]];
(*t=RandomReal[1,{3,4,5,6}];dim=Dimensions@t;m=unfoldTensor[t,3];t2=foldToTensor[m,dim,3];Print[Dimensions/@{t,m,t2}];Print[t==t2];*)
RpcaColorTensor2=Function[{Gray,D,\[CapitalOmega],iter},RpcaColorTensor2WithInit[Gray,D,\[CapitalOmega],0D,0D,iter]];
RpcaColorTensor2WithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter},
	(*min \sum_i||L_i||_* + \[Lambda] order||\[CapitalOmega] S||_ 1 + \[Mu]1/2\sum_i||D-L_i-S||_F^2+\[Mu]2/2\sum_i||L_i-X||_F^2+\[Eta] order/2||XT-W||_F^2 *)
	Module[{W=Gray,dim,norm2,\[Lambda],\[Eta]=10.
		,OL,Y1,Y2,L,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs,unfoldings},
	(*S,D,\[CapitalOmega],X are {k,m,n}, L,Y1,Y2 are {order,k,m,n}. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	unfoldings=Select[Range@Length@dim,dim[[#]]>=GeometricMean@dim&];L=Table[initL,{Length@unfoldings}];
	Y1=Y2=Table[(*Sign[D]/Max[norm2,Mean[Norm[#,Infinity]&/@D]/\[Lambda]]*)0D,{Length@unfoldings}];\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	T=Flatten[Table[N@IdentityMatrix[n],{k}],1]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
    X=foldToTensor[Transpose@LinearSolve[SparseArray[(\[Eta] T2+\[Mu]2 Ikn)],Transpose[\[Eta] WT+unfoldTensor[Mean[-Y2+\[Mu]2 L],2]]],dim,2];
    Zl=(#+\[Mu]1(D-S)+\[Mu]2 X)/(\[Mu]1+\[Mu]2)&/@(Y1+Y2);
	OL=L;
	L=Table[foldToTensor[sShrinkage[1/(\[Mu]1+\[Mu]2),unfoldTensor[Zl[[i]],unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
(*	Zs=MapThread[Function[{Y1,L},Y1/\[Mu]1+D-L],{Y1,L}];
	S=Table[With[{\[CapitalOmega]i=unfoldTensor[\[CapitalOmega],i],Zsi=unfoldTensor[Zs[[i]],i]},foldToTensor[\[CapitalOmega]i dShrinkage[\[Lambda]/\[Mu]1,Zsi]+(1-\[CapitalOmega]i) Zsi,dim,i]],{i,tensorRank}];*)
	Zs=Mean[Y1]/\[Mu]1+D-Mean@L;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/pnorm[L,2]<0.0005,Break[]];
    Y1+=(\[Mu]1(D-#-S)&/@L);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,30]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{correctWithGray[Gray,#]&@X,S}]];
RpcaColorTensor=Function[{Gray,D,\[CapitalOmega],iter},RpcaColorTensorWithInit[Gray,D,\[CapitalOmega],0D,0D,iter]];
RpcaColorTensorWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter},RpcaColorTensorUnfoldingsWithInit[Gray,D,\[CapitalOmega],initL,initS,iter,{2,3}]];
RpcaColorTensorUnfoldingsWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter,unfoldings},
	(*min \sum_i||L_i||_* + \[Lambda] order||\[CapitalOmega] S||_ 1 + \[Mu]1/2 order||D-X-S||_F^2+\[Mu]2/2\sum_i||L_i-X||_F^2+\[Eta] order/2||XT-W||_F^2 *)
	Module[{W=Gray,dim,norm2,\[Lambda],\[Eta]=10.
		,OL,Y1,Y2,L,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs},
	(*S,D,\[CapitalOmega],X,Y1 are {k,m,n}, L,Y2 are {order,k,m,n}. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	(*unfoldings=Select[Range@Length@dim,dim[[#]]>=GeometricMean@dim&];*)L=Table[initL,{Length@unfoldings}];
	Y1=0D(*Sign[D]/Max[norm2,Mean[Norm[#,Infinity]&/@D]/\[Lambda]]*);Y2=Table[Y1,{Length@unfoldings}];\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	T=Flatten[Table[N@IdentityMatrix[n],{k}],1]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
	(*Print[{"{T2,Ikn,WT,unfoldTensor[Mean[-Y2+\[Mu]2 L],2]}",Dimensions/@{T2,Ikn,WT,unfoldTensor[Mean[-Y2+\[Mu]2 L],2]}}];*)
	(*If[Mod[j,10]==0,Print[{"before",Norm[unfoldTensor[X,2].T-W,"Frobenius"]}]];*)
    X=foldToTensor[Transpose@LinearSolve[SparseArray[(\[Eta] T2+(\[Mu]1+\[Mu]2) Ikn)],Transpose[\[Eta] WT+unfoldTensor[Mean[-Y2+\[Mu]2 L]+\[Mu]1(D-S)+Y1,2]]],dim,2];
	(*If[Mod[j,10]==0,Print[{"after",Norm[unfoldTensor[X,2].T-W,"Frobenius"]}]];*)
    Zl=(#+\[Mu]2 X)/\[Mu]2&/@Y2;
	OL=L;
	L=Table[foldToTensor[sShrinkage[1/\[Mu]2,unfoldTensor[Zl[[i]],unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
	Zs=Y1/\[Mu]1+D-X;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/(0.000001+pnorm[L,2])<0.0005,Break[]];
	(*Print[{"{Zl,L,Zs,S}",Dimensions/@{Zl,L,Zs,S}}];*)
    Y1+=\[Mu]1(D-X-S);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,30]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{correctWithGray[Gray,#]&@X,S}]];
(*RpcaColorTensorUnfoldingsWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter,unfoldings},
	(*min \sum_i||L_i||_* + \[Lambda]||\[CapitalOmega] S||_ 1 + \[Mu]1/2||D-L-S||_F^2+\[Mu]2/2||L-X||_F^2+\[Eta]/2||XT-W||_F^2 *)
	Module[{W=Gray,dim,norm2,\[Lambda],\[Eta]=10.,OL,Y1,Y2,L=initL,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs},
	(*S,D,\[CapitalOmega],X,Y1,Y2,L are {k,m,n}. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	Y2=Y1=0D;\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	T=Flatten[Table[N@IdentityMatrix[n],{k}],1]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
	(*Print[{"{T2,Ikn,WT,unfoldTensor[Mean[-Y2+\[Mu]2 L],2]}",Dimensions/@{T2,Ikn,WT,unfoldTensor[Mean[-Y2+\[Mu]2 L],2]}}];*)
	(*If[Mod[j,10]==0,Print[{"before",Norm[unfoldTensor[X,2].T-W,"Frobenius"]}]];*)
    X=foldToTensor[Transpose@LinearSolve[SparseArray[(\[Eta] T2+\[Mu]2 Ikn)],Transpose[\[Eta] WT+unfoldTensor[-Y2+\[Mu]2 L,2]]],dim,2];
	(*If[Mod[j,10]==0,Print[{"after",Norm[unfoldTensor[X,2].T-W,"Frobenius"]}]];*)
    Zl=(Y1+Y2+\[Mu]1(D-S)+\[Mu]2 X)/(\[Mu]1+\[Mu]2);
	OL=L;
(*XXX: This step cannot ensure optimality*)
	L=Table[foldToTensor[sShrinkage[1/\[Mu]2,unfoldTensor[Zl,unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
	Zs=Y1/\[Mu]1+D-X;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/(0.000001+pnorm[L,2])<0.0005,Break[]];
	(*Print[{"{Zl,L,Zs,S}",Dimensions/@{Zl,L,Zs,S}}];*)
    Y1+=\[Mu]1(D-X-S);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,30]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{correctWithGray[Gray,#]&@X,S}]];*)
RpcaColorTensorLaplace=Function[{Gray,D,\[CapitalOmega],iter},RpcaColorTensorLaplaceWithInit[Gray,D,\[CapitalOmega],0D,0D,iter]];
RpcaColorTensorLaplaceWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter},
	(*min \sum_i||L_i||_* + \[Lambda] order||\[CapitalOmega] S||_ 1 + \[Mu]1/2 order||D-X-S||_F^2+\[Mu]2/2\sum_i||L_i-X||_F^2+\[Eta] order/2||XT-W||_F^2+\[Gamma]||\[Del]X||_F^2 *)
	Module[{W=Gray,dim,norm2,\[Lambda],\[Eta]=10.
		,OL,Y1,Y2,L,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs,unfoldings,simMatrix,lapMatrix,lapMatrix2},
	(*S,D,\[CapitalOmega],X,Y1 are {k,m,n}, L,Y2 are {order,k,m,n}. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	unfoldings=Select[Range@Length@dim,dim[[#]]>=GeometricMean@dim&];L=Table[initL,{Length@unfoldings}];
	simMatrix=grayscaleDistanceSimilarityMatrix[Gray,0.05,1];
	lapMatrix=SparseArray[#/Tr@#&@laplacianMatrixFromAdjacencyMatrix@simMatrix];
	lapMatrix2=SparseArray[10000. SparseArray[KroneckerProduct[sparseIdentityMatrix[k],lapMatrix]]];
	Y1=0D(*Sign[D]/Max[norm2,Mean[Norm[#,Infinity]&/@D]/\[Lambda]]*);Y2=Table[Y1,{Length@unfoldings}];\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	T=Flatten[Table[N@IdentityMatrix[n],{k}],1]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
	X=foldToTensor[Transpose@Partition[LinearSolve[
		SparseArray[SparseArray@KroneckerProduct[SparseArray@Transpose[(\[Eta] T2+(\[Mu]1+\[Mu]2) Ikn)],sparseIdentityMatrix@m]
			+lapMatrix2]
		,vec[\[Eta] WT+unfoldTensor[Mean[-Y2+\[Mu]2 L]+\[Mu]1(D-S)+Y1,2]]
			,Method->{"Krylov",Method -> "ConjugateGradient",Preconditioner->"ILU0"(*,Tolerance->0.001*)}
		],m],dim,2];
    Zl=(#+\[Mu]2 X)/\[Mu]2&/@Y2;
	OL=L;
	L=Table[foldToTensor[sShrinkage[1/\[Mu]2,unfoldTensor[Zl[[i]],unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
	Zs=Y1/\[Mu]1+D-X;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/(0.000001+pnorm[L,2])<0.0005,Break[]];
    Y1+=\[Mu]1(D-X-S);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,20]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{correctWithGray[Gray,#]&@X,S}]];
RpcaColorTensorSimpleLaplaceWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter},
	(*min \sum_i||L_i||_* + \[Lambda] order||\[CapitalOmega] S||_ 1 + \[Mu]1/2 order||D-X-S||_F^2+\[Mu]2/2\sum_i||L_i-X||_F^2+\[Eta] order/2||XT-W||_F^2+\[Gamma]||\[Del]X||_F^2 *)
	Module[{W=Gray,dim,norm2,\[Lambda],\[Eta]=10.
		,OL,Y1,Y2,L,S=initS,X,\[Mu]1,\[Mu]2,\[Rho],k,T,T2,Ikn,WT,m,n,Zl,Zs,unfoldings,lapMatrix1,lapMatrix2,lapMatrixWeight=1.},
	(*S,D,\[CapitalOmega],X,Y1 are {k,m,n}, L,Y2 are {order,k,m,n}. W is gray, D is RGB, \[CapitalOmega] is nonzeros*)
	dim={k,m,n}=Dimensions[D];\[Lambda]=100./GeometricMean@{m,n};norm2=Mean[First@SingularValueList[#,1]&/@D];
	unfoldings=Select[Range@Length@dim,dim[[#]]>=GeometricMean@dim&];L=Table[initL,{Length@unfoldings}];
	lapMatrix1=lapMatrixWeight N@laplacianMatrix1D[m];
	lapMatrix2=lapMatrixWeight KroneckerProduct[IdentityMatrix[k],N@laplacianMatrix1D[n]];
	Y1=0D(*Sign[D]/Max[norm2,Mean[Norm[#,Infinity]&/@D]/\[Lambda]]*);Y2=Table[Y1,{Length@unfoldings}];\[Mu]1=\[Mu]2=12.5/norm2;\[Rho]=1.005;X=0 D;
	T=Flatten[Table[N@IdentityMatrix[n],{k}],1]/k;T2=T.Transpose[T];Ikn=IdentityMatrix[k n];WT=W.Transpose[T];
	Do[
	X=foldToTensor[lyapunovSolveTwoSided[0 IdentityMatrix[m],SparseArray[(\[Eta] T2+(\[Mu]1+\[Mu]2) Ikn)],lapMatrix1,lapMatrix2
			,\[Eta] WT+unfoldTensor[Mean[-Y2+\[Mu]2 L]+\[Mu]1(D-S)+Y1,2]],dim,2];
    Zl=(#+\[Mu]2 X)/\[Mu]2&/@Y2;
	OL=L;
	L=Table[foldToTensor[sShrinkage[1/\[Mu]2,unfoldTensor[Zl[[i]],unfoldings[[i]]]],dim,unfoldings[[i]]],{i,Length@unfoldings}];
	Zs=Y1/\[Mu]1+D-X;
	S=dShrinkage[\[Lambda]/\[Mu]1 Normal@\[CapitalOmega],Zs];
	If[j>1&&pnorm[L-OL,2]/(0.000001+pnorm[L,2])<0.0005,Break[]];
    Y1+=\[Mu]1(D-X-S);Y2+=(\[Mu]2(X-#)&/@L);
	{\[Mu]1,\[Mu]2,\[Eta]}*=\[Rho];If[Mod[j,20]==1,printTemporary@ColorCombine[Image/@X]];(*Ls=Append[Ls,L];*)
	,{j,iter}];
	{correctWithGray[Gray,#]&@X,S}]];
partitionMatrices=Function[{ms,k},Module[{dim=Dimensions@ms,dim2},
	If[Mod[dim[[-2;;]],k]!={0,0},Print["partitionMatrices Mod[dim[[-2;;]],k]\[NotEqual]{0,0}"];Abort[]];
	dim2=dim[[-2;;]]/k;Map[Partition[#,dim2]&,ms,{Length@dim-2}]]];
partitionFoRpcaColor=Function[{ms,k},Switch[Length@Dimensions@ms,2,partitionMatrices[ms,k],3,Transpose[partitionMatrices[ms,k],{3,1,2}]]];
assembleForRpcaColor=Function[ms,Switch[Length@Dimensions@ms,4,assemble@ms,5,assemble/@Transpose[ms,{2,3,1}]]];
(*MatrixForm@Map[Image,#,{-3}]&/@{partitionFoRpcaColor[gray,2],gray,assembleForRpcaColor@partitionFoRpcaColor[gray,2]}
MatrixForm@Map[Image,#,{-3}]&/@{partitionFoRpcaColor[rgb,2],rgb,assembleForRpcaColor@partitionFoRpcaColor[rgb,2]}*)
scaleForRpcaColor=Function[{ms,divisor},Map[ImageData@ImageResize[Image@#,Reverse[Dimensions[#] divisor]]&,ms,{-3}]];
RpcaColorBlock=Function[{Gray,Rgb,\[CapitalOmega]in,maxIter,RpcaColorFn,divisor},
	RpcaColorBlockWithInit[Gray,Rgb,\[CapitalOmega]in,0Rgb,0Rgb,maxIter,Function[{gray,D,\[CapitalOmega],initL,initS,iter},RpcaColorFn[gray,D,\[CapitalOmega],iter]],divisor]];
RpcaColorBlockWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter,RpcaColorWithInitFn,divisor},
	If[GeometricMean@divisor<=1,RpcaColorWithInitFn[Gray,D,\[CapitalOmega],initL,initS,iter],Module[{LSS},
	LSS=MapThread[RpcaColorWithInitFn[#,#2,#3,#4,#5,iter]&,partitionFoRpcaColor[#,divisor]&/@{Gray,D,\[CapitalOmega],initL,initS},2];
	{correctWithGray[Gray,#]&@assembleForRpcaColor@LSS[[;;,;;,1]],assembleForRpcaColor@LSS[[;;,;;,2]]}
	]]];
RpcaColorCoarse=Function[{Gray,Rgb,\[CapitalOmega]in,maxIter,RpcaColorFn,divisor},
	RpcaColorCoarseWithInit[Gray,Rgb,\[CapitalOmega]in,0Rgb,0Rgb,maxIter,Function[{gray,D,\[CapitalOmega],initL,initS,iter},RpcaColorFn[gray,D,\[CapitalOmega],iter]],divisor]];
RpcaColorCoarseWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter,RpcaColorWithInitFn,divisor},
	Module[{LS,f=scaleForRpcaColor[#,1/Reverse@divisor]&,sample=#[[;;,;;;;divisor[[1]],;;;;divisor[[2]]]]&,g=scaleForRpcaColor[#,divisor]&},
	LS=RpcaColorWithInitFn[f@Gray,sample@D,sample@\[CapitalOmega],f@initL,f@initS,iter];
	{correctWithGray[Gray,#]&@LS[[1]],LS[[2]]}
	]];
Rpca2ColorZigZagWithInit=Function[{Gray,Rgb,\[CapitalOmega],initL,initS,maxIter,RpcaColorWithInitFn,divisor,numNCycles},
	Module[{L=initL,S=initS},
	Do[{L,S}=RpcaColorCoarseWithInit[Gray,Rgb,\[CapitalOmega],L,S,maxIter,RpcaColorWithInitFn,GeometricMean@divisor{1,1}];
	{L,S}=RpcaColorBlockWithInit[Gray,Rgb,\[CapitalOmega],L,S,maxIter,RpcaColorWithInitFn,divisor];
	,{j,numNCycles}];
	{correctWithGray[Gray,#]&@L,S}]];
Rpca2ColorFmgWithInit=Function[{GrayIn,RgbIn,\[CapitalOmega]in,initL,initS,maxIter,RpcaColorWithInitFn,numNCycles},
	Module[{L,S,level,Gray,Rgb,\[CapitalOmega],scale,top=2^(numNCycles-1),divisor},
	Fold[Function[{state,newLevel},
		{L,S,level}=state;
		divisor=top/newLevel;Gray=scaleForRpcaColor[GrayIn,1/divisor];
		{Rgb,\[CapitalOmega]}=#[[;;,;;;;divisor,;;;;divisor]]&/@{RgbIn,\[CapitalOmega]in};
		{L,S}=scaleForRpcaColor[#,newLevel/level]&/@{L,S};
		{L,S}=RpcaColorBlockWithInit[Gray,Rgb,\[CapitalOmega],L,S,maxIter,RpcaColorWithInitFn,newLevel{1,1}];
		{L,S,newLevel}
	],{initL,initS,top},Flatten@Table[{Table[2^(j-i),{i,1,j}],Table[2^(i-1),{i,2,j}]},{j,numNCycles}]];
	{correctWithGray[GrayIn,#]&@L,S}]];
partitionRedBlackFoRpcaColor=Function[{m,divisor},Module[{ms=partitionFoRpcaColor[m,2divisor],dim=2divisor},
	Table[assembleForRpcaColor@Outer[ms[[#,#2]]&,Select[i+{-1,0,1},dim[[1]]>=#>0&],Select[j+{-1,0,1},dim[[2]]>=#>0&],1]
	,{i,dim[[1]]},{j,dim[[2]]}]]];
n\[CapitalOmega]RedBlackFoRpcaColor=Function[{totalDim,divisor},Module[{dim=2divisor},
	Table[KroneckerProduct[Outer[Boole[{#,#2}!={i,j}]&,Select[i+{-1,0,1},dim[[1]]>=#>0&],Select[j+{-1,0,1},dim[[2]]>=#>0&],1]
		,Array[1&,totalDim/dim]]
	,{i,dim[[1]]},{j,dim[[2]]}]]];
assembleRedBalckForRpcaColor=Function[{ms,divisor,totalDim},With[{dim=Dimensions@ms,dim2=totalDim/divisor/2},
	assembleForRpcaColor@Table[Partition[#,dim2][[If[i==1,1,2],If[j==1,1,2]]]&/@ms[[i,j]],{i,dim[[1]]},{j,dim[[2]]}]]];
RpcaColorRedBlackBlockWithInit=Function[{Gray,D,\[CapitalOmega],initL,initS,iter,RpcaColorWithInitFn,divisor},
	If[GeometricMean@divisor<=1,RpcaColorWithInitFn[Gray,D,\[CapitalOmega],initL,initS,iter],Module[{LSS,LS},
	LSS=MapThread[With[{n\[CapitalOmega]=Table[#6,{Length@D}]},RpcaColorWithInitFn[#,#2(1-n\[CapitalOmega])+n\[CapitalOmega] #4,#3(1-n\[CapitalOmega])+n\[CapitalOmega],#4,#5,iter]]&,
		Append[partitionRedBlackFoRpcaColor[#,divisor]&/@{Gray,D,\[CapitalOmega],initL,initS},n\[CapitalOmega]RedBlackFoRpcaColor[Dimensions@Gray,divisor]],2];
	LS=assembleRedBalckForRpcaColor[#,divisor,Dimensions@Gray]/@{LSS[[;;,;;,1]],LSS[[;;,;;,2]]};
	{correctWithGray[Gray,#]&@LS[[1]],LS[[2]]}
	]]];
(* It seems not sufficient to use seams. *)
(*seamsRpcaColor=Function[{dim,divisor},Boole@Table[Or@@(MemberQ[{0,1,2,3,4},Mod[#,dim[[1]]/divisor]]&/@{i,j}),{i,dim[[1]]},{j,dim[[2]]}]];
seams=Table[seamsRpcaColor[dim,divisor],{Length@rgb\[CapitalOmega]}];
n\[CapitalOmega]=\[CapitalOmega](1-seams)+seams;Image/@n\[CapitalOmega]
nrgb\[CapitalOmega]=n\[CapitalOmega] L4;Image/@nrgb\[CapitalOmega]
{L5,S5}=Rpca2ColorZigZagWithInit[gray,nrgb\[CapitalOmega],n\[CapitalOmega],initL,initS,numIter,RpcaColorTensorWithInit,divisor,1];//AbsoluteTiming*)
truncatedNuclearNorm=Function[{X,U,V,W,\[Lambda],\[CapitalOmega],D},{#,Plus@@#}&@{Total@SingularValueList@X,-Tr[Transpose[U].W.V],\[Lambda]/2 Norm[X-W,"Frobenius"]^2}];
tnnRpcaStep2Apgl=Function[{D,\[CapitalOmega],U,V,iter},Module[{X=D,nX,Y=D,t=1.,nt,\[Lambda]=0.04,norm2},
	norm2=SingularValueDecomposition[D,1][[2,1,1]];Y=Sign[D]/Max[norm2,Norm[Flatten@D,Infinity]];
	Do[
		nX=sShrinkage[t,Y+t(U.Transpose[V]-\[Lambda](\[CapitalOmega] Y-D))];nt=(1+Sqrt[1+4t^2])/2;Y=nX+(t-1)/nt (nX-X);t=nt;
		If[Norm[nX-X,"Frobenius"]<0.1,Break[]];X=nX;
		(*Print[{MatrixForm@nX,t}];*)
		(*Print[{"in-after-1",truncatedNuclearNorm[X,U,V,X,\[Lambda],\[CapitalOmega],D]}];*)
	,{iter}];X
	]];
tnnRpcaStep2Admm=Function[{D,\[CapitalOmega],U,V,iter},Module[{X=0D,W=D,Y=D,\[Mu]=1,\[Rho]=1.01,oldX=RandomReal[1,Dimensions@D],norm2},
	norm2=SingularValueDecomposition[D,1][[2,1,1]];Y=Sign[D]/Max[norm2,Norm[Flatten@D,Infinity]];\[Mu]=1.25/norm2;
	Do[
		(*Print[{1/\[Mu],(W-1/\[Mu] Y)//MatrixPlot}];*)
		(*Print[{"in-before",truncatedNuclearNorm[X,U,V,W,\[Mu],\[CapitalOmega],D]}];
		Print[Tr[Transpose[Y].(X-W)]];*)
		X=sShrinkage[1/\[Mu],W-1/\[Mu] Y];
		(*Print[Tr[Transpose[Y].(X-W)]];
		Print[{"in-after-1",truncatedNuclearNorm[X,U,V,W,\[Mu],\[CapitalOmega],D]}];*)
		W=X+1/\[Mu](U.Transpose[V]+Y);
		(*Print[{"in-after-2",truncatedNuclearNorm[X,U,V,W,\[Mu],\[CapitalOmega],D]}];*)
		W=(1-\[CapitalOmega])W+D;
		(*Print[{"in-after-3",truncatedNuclearNorm[X,U,V,W,\[Mu],\[CapitalOmega],D]}];*)
		Y=Y+\[Mu] (X-W);
		\[Mu]*=\[Rho];
		(*Print[MatrixPlot/@{oldX,X}];*)
		(*Print[{"Norm[oldX-X,\"Frobenius\"]",Norm[oldX-X,"Frobenius"]}];*)
		If[Norm[oldX-X,"Frobenius"]<0.1,Break[]];oldX=X;
	,{iter}];
	X]];
tnnRpca=Function[{D,\[CapitalOmega],iter,iterStep2,rank,method},Module[{X=D,U,S,V,oldX=RandomReal[1,Dimensions@D]},
	Do[
		{U,S,V}=SingularValueDecomposition[X,rank];
		(*Print[{"out-before",truncatedNuclearNorm[X,U,V,X,1,\[CapitalOmega],D]}];*)
		X=Switch[method,"Apgl",tnnRpcaStep2Apgl,"Admm",tnnRpcaStep2Admm][D,\[CapitalOmega],U,V,iterStep2];
		(*If[Norm[oldX-X,"Frobenius"]<0.01,Break[]];oldX=X;*)
	,{iter}];X
]];
testTnnRpca=Function[{rawImg,mask,numIter,numIterSubstep,rank},Module[{dim,img,L},
	dim=Dimensions@mask;
	img=Image@Mean[ImageData/@ColorSeparate@ImageResize[rawImg,Reverse@dim]];
	Print[L=tnnRpca[ImageData[img] mask,mask,numIter,numIterSubstep,rank];//AbsoluteTiming];
	{img,(*mask//Image,*)ImageData[img] mask//Image,Image[L]}]];
(*tnnRpcaColor=Function[{Gray,Rgb,\[CapitalOmega],iter,iterStep2,rank},Module[{W=Gray,X=Rgb,U,S,V,T,Tfnorm,WTplus,m,n,\[Lambda]=1},
	m=Dimensions[Rgb][[1]];n=Dimensions[Rgb][[2]]/3;
	T=Join@@Table[IdentityMatrix[n],{3}]/3.;Tfnorm=Norm[T,"Frobenius"]^2;WTplus=W.PseudoInverse[T];
	Do[
	X=(X+WTplus \[Lambda] Tfnorm)/(1+\[Lambda] Tfnorm);
	{U,S,V}=SingularValueDecomposition[X,rank];
	Print["out-before"];Print[truncatedNuclearNorm[X,U,V,1,\[CapitalOmega],Rgb]];
	X=tnnRpcaStep2Admm[Rgb,\[CapitalOmega],U,V,iterStep2];
	Print["out-after"];Print[truncatedNuclearNorm[X,U,V,1,\[CapitalOmega],Rgb]];
	printTemporary[CombineRGB[X]];
	,{iter}];X
]];*)
(*testTnnRpca[Import@"~/t.jpg",mask,100,100,100]*)
nmfAdmmCostFunction=Function[{W,H,D,\[Mu]},\[Mu]/2 SparseNonZeroFrobeniusResidue[D,W,H]^2];
nmfAdmm=Function[{Din,\[CapitalOmega]in,r,niter,method},
	Module[{D=N@SparseArray@Din,Y,m,n,residues={},W,H,nzs,V2,\[CapitalOmega]=N@SparseArray@\[CapitalOmega]in,eps=N[10^-6],debug=printTemporary[#]&
		,\[Mu],norm2=SingularValueDecomposition[Din,1][[2,1,1]],\[Rho],V},
	If[Complement[D["NonzeroPositions"],\[CapitalOmega]["NonzeroPositions"]]!={},Throw["\[CapitalOmega]in not cotinaing Din"]];
	{m,n}=Dimensions[D];{W,H}=RandomReal[{eps,1},#]&/@{{m,r},{r,n}};\[Mu]=1.25/norm2;\[Rho]=1.05;V=Y=0D;nzs=\[CapitalOmega]["NonzeroPositions"];
	Do[
		debug[residues=Append[residues,nmfAdmmCostFunction[W,H,D,\[Mu]]];//AbsoluteTiming];V=D+Y/\[Mu];
		(*Print[MatrixPlot/@{V,W,Y}];*)
		Switch[method
			,"F",
			debug[V2=SparseArray[{##}->W[[#]].H[[;;,#2]]&@@@nzs,{m,n}];//AbsoluteTiming];
			debug[W=W (V.SparseArray[Transpose@H]) / (eps + V2.SparseArray[Transpose@H]);//AbsoluteTiming];
			debug[residues=Append[residues,{"W",nmfAdmmCostFunction[W,H,D,\[Mu]]}];//AbsoluteTiming];
			debug[V2=SparseArray[{##}->W[[#]].H[[;;,#2]]&@@@nzs,{m,n}];//AbsoluteTiming];
			debug[H=H (SparseArray[Transpose@W].V) / (eps + SparseArray[Transpose@W].V2);//AbsoluteTiming];
			debug[residues=Append[residues,{"H",nmfAdmmCostFunction[W,H,D,\[Mu]]}];//AbsoluteTiming];
			,"KL",
			debug[V2=SparseArray[{##}->V[[##]]/(eps+W[[#]].H[[;;,#2]])&@@@nzs,{m,n}];//AbsoluteTiming];
			debug[W=W (V2.SparseArray[Transpose@H]) / (eps + \[CapitalOmega].SparseArray[Transpose@H]);//AbsoluteTiming];
			debug[residues=Append[residues,{"W",nmfAdmmCostFunction[W,H,D,\[Mu]]}];//AbsoluteTiming];
			debug[V2=SparseArray[{##}->V[[##]]/(eps+W[[#]].H[[;;,#2]])&@@@nzs,{m,n}];//AbsoluteTiming];
			debug[H=H (SparseArray[Transpose@W].V2) / (eps + SparseArray[Transpose@W].\[CapitalOmega]);//AbsoluteTiming];
			debug[residues=Append[residues,{"H",nmfAdmmCostFunction[W,H,D,\[Mu]]}];//AbsoluteTiming];,
			_,Abort[]];
		debug[V2=SparseArray[({##}->D[[#,#2]]-W[[#]].H[[;;,#2]])&@@@nzs,{m,n}];//AbsoluteTiming];
		Y+=\[Mu] V2;\[Mu]*=\[Rho];
	,{niter}];
	debug[residues=Append[residues,nmfAdmmCostFunction[W,H,V,\[Mu]]];//AbsoluteTiming];
	Print@residues;
	{W,H(*,ListLogPlot[Rest@residues,PlotRange->All]*)}
	]];
rnmfCostFunction=Function[{W,H,S,D,\[Mu],\[CapitalOmega]},{Total@#,#}&@{Total[Flatten@Abs[\[CapitalOmega] S]],\[Mu]/2 Total@Flatten[(D-S-W.H)^2]}];
Rnmf=Function[{Din,\[CapitalOmega],r,niter,method},
	Module[{D=N@Din,S,Y,m,n,residues={},W,H,eps=If[method=="KL",N[10^-6],0],debug=printTemporary[#]&,\[Mu],norm2=SingularValueDecomposition[Din,1][[2,1,1]],\[Rho],V,V2,Zs},
	If[Complement[SparseArray[D]["NonzeroPositions"],SparseArray[\[CapitalOmega]]["NonzeroPositions"]]!={},Throw["\[CapitalOmega]in not cotinaing Din"]];
	{m,n}=Dimensions[D];{W,H}=RandomReal[{eps,1},#]&/@{{m,r},{r,n}};\[Mu]=125/norm2;\[Rho]=1.05;V=S=Y=0D;
	Do[
		debug[residues=Append[residues,rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]];//AbsoluteTiming];
		V=D-S+Y/\[Mu];
		(*Print[MatrixPlot/@{V,W,S,Y}];*)
		Switch[method
			,"F",
			debug[W=W (V.Transpose@H) / (eps + W.(H.Transpose@H));//AbsoluteTiming];
			debug[residues=Append[residues,{"W",rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]}];//AbsoluteTiming];
			debug[H=H (Transpose@W.V) / (eps + Transpose@W.W.H);//AbsoluteTiming];
			debug[residues=Append[residues,{"H",rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]}];//AbsoluteTiming];
			,"KL",
			debug[W=W (((eps)V/(eps+W.H)).Transpose@H + eps) / (eps + \[CapitalOmega].Transpose@H);//AbsoluteTiming];
			debug[residues=Append[residues,{"W",rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]}];//AbsoluteTiming];
			debug[H=H (Transpose@W.((eps+V)/(eps+W.H)) + eps) / (eps + SparseArray[Transpose@W].\[CapitalOmega]);//AbsoluteTiming];
			debug[residues=Append[residues,{"H",rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]}];//AbsoluteTiming];,
			_,Abort[]];
		V2=D-W.H;Zs=V2+Y/\[Mu];
		(*Print[{1/\[Mu],MatrixForm[V2+Y/\[Mu]]}];*)
		S=\[CapitalOmega] dShrinkage[1/\[Mu],Zs]+(1-\[CapitalOmega])Zs;
		debug[residues=Append[residues,{"S",rnmfCostFunction[W,H,S,D,\[Mu],\[CapitalOmega]]}];//AbsoluteTiming];
		Y+=\[Mu](V2-S);
		\[Mu]*=\[Rho];
	,{niter}];
	debug[residues=Append[residues,rnmfCostFunction[W,H,S,V,\[Mu],\[CapitalOmega]]];//AbsoluteTiming];
	(*Print@residues;*)
	{W,H(*,ListLogPlot[Rest@residues,PlotRange->All]*)}
	]];
(*{W,H}=Abs@RandomReal[{0.01,1},#]&/@{{30,rank},{rank,20}};WH=W.H;\[CapitalOmega]in=RandomSparseMatrix[30,20,0.8];S=0.1 RandomReal[1,Dimensions@WH];
r=nmfAdmm[\[CapitalOmega]in (WH+S),\[CapitalOmega]in,rank,30,"KL"];
Norm[(1-\[CapitalOmega]in)(r[[1]].r[[2]]-WH),"Frobenius"]/Norm[WH,"Frobenius"]*)
truncatedSvd=Function[{\[Tau],D,rank},Module[{n,U,S,V},{U,S,V}=SingularValueDecomposition[D,rank];
	n=Length@Select[Diagonal[S],Abs[#]>\[Tau]&];{U[[;;,;;n]],S[[;;n,;;n]],V[[;;,;;n]]}]];
hosvd=Function[{\[Tau],D,rank},Module[{order=Length@Dimensions@D,dim=Dimensions@D,Us},
	Us=Table[First@truncatedSvd[\[Tau],#,Min[rank,dim[[i]]]]&@unfoldTensor[D,i],{i,order}];
	{foldXUs[D,ConjugateTranspose/@Us,{}],Us}]];
fenchelDual=Function[{f,t},Module[{x},Integrate[(InverseFunction@Function[x,Evaluate@D[f[x],x]])[t],t]]];
fitEllipsoid=Function[xyzs,
  Module[{x0,y0,z0,r,b,c,r2,rules},
  {r2,rules}=NMinimize[{Sum[Abs[Sqrt[(xyzs[[i,1]]-x0)^2+(xyzs[[i,2]]-y0)^2/b^2+(xyzs[[i,3]]-z0)^2/c^2]-r],{i,Length@xyzs}]},{x0,y0,z0,r,b,c}];
  {(r/.rules)^2,rules,ListLinePlot[(#[[1]]-x0)^2+(#[[2]]-y0)^2/b^2+(#[[3]]-z0)^2/c^2/.rules&/@xyzs]}]];
fitPlane=Function[pts,Module[{normal},normal=SingularValueDecomposition[With[{m=Mean[pts]},#-m&/@pts]][[3,;;,-1]];normal/Mean[pts.normal]]];
findMaxima=Compile[{{x,_Real,1}},Module[{n, u},
  n = Flatten@Position[Differences[Boole[# > 0]&/@Differences[x]], _?(# < 0 &)];
  u = Flatten@Position[x[[n + 1]] - x[[n]], _?(# > 0 &)];
  n[[u]] + 1],{{_Position,_Real,2}},CompilationTarget:>"C"];

empiricalModeDecomposition=Function[xIn,
 Module[{xt = xIn, imf={}, x1, x2, s1, s2, sd,getSpline,isIMF,isMonotonic},
  isMonotonic=Function[x, Length[findMaxima[x]] Length[findMaxima[-x]] == 0];
  getSpline=Function[x,Module[{n=Length@x, p=findMaxima@x},
	Interpolation[Transpose[{Flatten[{0, p, n + 1}], Flatten[{0, x[[p]], 0}]}]][Range[n]]]];
  isIMF=Function[x,Module[{u1= Count[Most[x] Rest[x], _?Negative], u2= Length[findMaxima[x]] + Length[findMaxima[-x]]}, 
	Abs[u1 - u2] <= 1]];
  While[ !isMonotonic[xt],
   x1 = xt;
   sd = Infinity;
   While[ (sd > 0.1) || ! isIMF[x1],
    s1 = getSpline[x1];
    s2 = -getSpline[-x1];
    x2 = x1 - (s1 + s2)/2;
    sd = Total[(x1 - x2)^2]/Total[x1^2];
    x1 = x2;
   ];          
   AppendTo[imf, x1];
   xt = xt - x1;
  ];
  Append[imf, xt]
]];
(*http://mathematica.stackexchange.com/questions/19812/fastest-way-to-test-if-a-numerical-array-has-complex-elements*)
containsComplexesQ[expr_]:=MemberQ[
  expr /. {
     l_List /; Developer`PackedArrayQ[l, Complex] &&
                Return[True, containsComplexesQ] -> Null, 
     l_List /; Developer`PackedArrayQ[l] -> Null
    }, _Complex, -1
  ];
importPly=Function[fname,Module[{l,colors,points},
	l=Select[Import[fname,"Table"],Length@#==6(*&&Total@Abs@#[[;;3]]<20*)&];points=l[[;;,;;3]];colors=l[[;;,4;;]]/255.;{colors,points}]];
dispDepthmap=Function[{m,img},ListPlot3D[N[Join@@MapIndexed[Append[{#2[[2]],-#2[[1]]},-#]&,
	m,{2}]],PlotStyle->Texture[img],Lighting->"Neutral",ImageSize->500,Mesh->None,AxesLabel->StringSplit@"x y z"
	,BoxRatios->Append[Reverse@Dimensions[m]/Max@Dimensions[m],0.4]]];
(*imgs=ImageResize[Import["/s/tnasa.t/unified_viewer/"<>#],200{1,1}]&/@{"LKA.JPG","LKQ.JPG"}
depths=ImageResize[Import["/tmp/yes/"<>#],200{1,1}]&/@{"0:LKA_depthmap.jpg","0:LKQ_depthmap.jpg"}
dispDepthmap[colorDepthmapToM[depths[[1]]],imgs[[1]]]
dispDepthmap[colorDepthmapToM[depths[[2]]],imgs[[2]]]*)
pixelToPoint=Function[{depthmap,camera,i,j},LinearSolve[camera[[;;,;;3]],{i,j,1}depthmap[[i,j]]-camera[[;;,4]]]];
(*Graphics3D[Flatten[Table[{RGBColor@@i1[[i,j]],Point@pixelToPoint[d1,c1,i,j]},{i,dim[[1]]},{j,dim[[2]]}],2],Axes->True,AspectRatio->1,BoxRatios->1]*)
pointToPixel=Function[{camera,pt},#[[;;2]]/#[[3]]&[camera.Append[pt,1.]]];
(*Tally@Chop@Flatten@Table[pointToPixel[c1,pixelToPoint[d1,c1,i,j]]-{i,j},{i,dim[[1]]},{j,dim[[2]]}]*)
(*dim=Dimensions@i1;*)(*Graphics@Point[Join@@Table[pointToPixel[c2,pixelToPoint[d1,c1,i,j]],{i,dim[[1]]},{j,dim[[2]]}]]*)
colorDepthmapToM=Function[depthmap,Map[If[#[[1]]<#[[3]],#[[2]]/2,(#[[1]]+1)/2]&,ImageData[depthmap],{2}]];
laplaceDepthmap=Function[{img,sparse},Module[{gray,mask,A},
	gray=ImageData@First@ColorSeparate[img,"LAB"];
	mask=SparseArray[SparseArray[sparse]["NonzeroPositions"]->1.,Dimensions@sparse];
	Print[A=laplacianMatrixFromMatrix[gray,mask,1];//AbsoluteTiming];
	Transpose@Partition[LinearSolve[A,vec[sparse],Method->{"Krylov"(*,Preconditioner->"ILU0"*)}],Dimensions[gray][[1]]]]];
(*QR and Schur as limiting case?*)
(*n=3;p=1;m=RandomReal[3{-1,1},n{1,1}];{u,c}=SchurDecomposition@m;qr=QRDecomposition@m;
(*MatrixForm/@{c,heuristicSymSUD[m,p][[1]]}*)
as=Array[a,n{1,1}];f[as_]:=MatrixExp[as-Transpose[as]].m;
r=NMinimize[pnorm2[f[as],p],Flatten@as];
MatrixForm/@{qr[[2]],f[as/.r[[2]]]}*)
schurDecomposition=Function[m,Module[{qr=QRDecomposition@Transpose@Eigenvectors@m,arg},(*This forces real, so core is block diagonal*)
	arg=Exp[I Arg@Flatten[qr[[1]]][[1]]];{(qr[[1]]/arg)\[ConjugateTranspose],qr[[2]].DiagonalMatrix@Eigenvalues@m.Inverse[qr[[2]]]}]];
luDecomposition=Function[m,Module[{r=LUDecomposition@m,l,u,lu,p,dim=Dimensions@m},(*L is not necessarily lower-triangular!*)
	lu=r[[1]];p=r[[2]];l=strictLowerTriangle[lu][[;;dim[[1]],;;dim[[1]]]]+IdentityMatrix[dim[[1]]];u=upperTriangle@lu;{Permute[l,p],u}]];
lduDecomposition=Function[m,Module[{r=luDecomposition@m},
	{r[[1]],DiagonalMatrix@Diagonal@r[[2]],Inverse[DiagonalMatrix@Diagonal@r[[2]]].r[[2]]}]];
(*requires input to be square and has at least super-diagonal nonzeros, or the core will be all zero.*)
(*Geometric concave f may not find eigenvalues: Log[1 + Abs@Log[1 + Abs@Log[1 + Exp@x]]]*)
heuristicSSLD=Function[{D,p},Module[{cplx=containsComplexesQ@D,as,u,dim,(*sparse special-linear decomposition*)
		mynorm=Switch[p,{1,3},pnorm2[#,1]+pnorm2[#,3]&,_,pnorm2[#,p]&],getXus,r,f,g},
	dim=Dimensions@D;as=MapThread[Array[#,If[cplx,{2,#2,#2},#2{1,1}]]&,{Array[u,Length@dim],dim}];
	g=If[cplx,MatrixExp[nilTrace[#[[1]]+I #[[2]]]]&/@#&,MatrixExp[nilTrace[#]]&/@#&];
	getXus=Function[{as,m},Module[{us=g[as]},
		sortXUs[foldXUs[m,us,{}],Inverse/@us]]];
	If[cplx,f[as_?(NumericQ[#[[1,1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]],
		f[as_?(NumericQ[#[[1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]]];
	r=NMinimize[f[as],Flatten[as],MaxIterations->500];
	getXus[as/.r[[2]],D]
	]];
heuristicSUD=Function[{D,p},Module[{cplx=containsComplexesQ@D,as,u,dim,
		mynorm=Switch[p,{1,3},pnorm2[#,1]-pnorm2[#,3]&,_,If[NumericQ[p]&&p>2,-1,1]pnorm2[#,p]&],getXus,r,f,g},
	dim=Dimensions@D;as=MapThread[Array[#,If[cplx,{2,#2,#2},#2{1,1}]]&,{Array[u,Length@dim],dim}];
	g=If[cplx,MatrixExp[#-ConjugateTranspose[#]]&[#[[1]]+I #[[2]]]&/@#&,MatrixExp[#-Transpose[#]]&/@#&];
	getXus=Function[{as,m},Module[{us=g[as]},
		sortXUs[foldXUs[m,us,{}],If[cplx,ConjugateTranspose,Transpose]/@us]]];
	If[cplx,f[as_?(NumericQ[#[[1,1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]],
		f[as_?(NumericQ[#[[1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]]];
	r=NMinimize[f[as],Flatten[as],MaxIterations->500];
	getXus[as/.r[[2]],D]
	]];
heuristicSymSUD=Function[{D,p},Module[{cplx=containsComplexesQ@D,as,u,dim,
		mynorm=Switch[p,{1,3},pnorm2[#,1]-pnorm2[#,3]&,_,If[NumericQ[p]&&p>2,-1,1]pnorm2[#,p]&],getXus,r,f,g},
	dim=Dimensions@D;If[#!=dim[[1]],Print["Non symmetric."];Abort[]]&/@dim;
	as=Array[u,If[cplx,{2,#,#},#{1,1}]]&[dim[[1]]];
	g=If[cplx,
		Table[MatrixExp[#-ConjugateTranspose[#]]&[#[[1]]+I #[[2]]]&[#],{Length@dim}]&,
		Table[MatrixExp[#-Transpose[#]]&[#],{Length@dim}]&];
	getXus=Function[{as,m},Module[{us=g[as]},
		{foldXUs[m,us,{}],If[cplx,ConjugateTranspose,Transpose]/@us}]];
	If[cplx,f[as_?(NumericQ[#[[1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]],
		f[as_?(NumericQ[#[[1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]]];
	r=NMinimize[f[as],Flatten[as],MaxIterations->500];
	getXus[as/.r[[2]],D]
	]];
heuristicTruncatedSUD=Function[{D,p,shape},(*First get best L2 approx of D with shape constraint, then SUD on the approx.*)
	Module[{cplx=containsComplexesQ@D,as,u,xs,x,dim,getXus,r,f,g,limitShape,xus},
	dim=Dimensions@D;as=MapThread[Array[#,If[cplx,{2,#2,#2},#2{1,1}]]&,{Array[u,Length@dim],dim}];
	xs=Array[x,shape];limitShape=#[[;;,;;#2]]&;
	g=If[cplx,MapThread[limitShape[MatrixExp[#-ConjugateTranspose[#]]&[#[[1]]+I #[[2]]],#2]&,{#,shape}]&
		,MapThread[limitShape[MatrixExp[#-Transpose[#]],#2]&,{#,shape}]&];
	getXus=Function[{as,xs},Module[{us=g[as]},sortXUs[xs,us]]];
	If[cplx,f[as_?(NumericQ[#[[1,1,1,1]]]&),xs_]:=pnorm2[D-foldXUs[xs,g[as],{}],2],
		f[as_?(NumericQ[#[[1,1,1]]]&),xs_]:=pnorm2[D-foldXUs[xs,g[as],{}],2]];
	r=NMinimize[f[as,xs],Flatten@Join[as,xs]];
	xus=getXus[as/.r[[2]],xs/.r[[2]]];
	heuristicSUD[foldXUs[xus[[1]],xus[[2]],{}],p]
	]];
heuristicEVD=Function[{D,p},Module[{cplx=containsComplexesQ@D,as,u,dim,
		mynorm=Switch[p,{1,3},pnorm2[#,1]+pnorm2[#,3]&,_,pnorm2[#,p]&],getXus,r,f,g,half=#[[;;Length[#]/2]]&},
	dim=Dimensions@D;as=MapThread[Array[#,If[cplx,{2,#2,#2},#2{1,1}]]&,{Array[u,Length@dim/2],half@dim}];
	g=If[cplx,MatrixExp[Join[#,Transpose@Inverse@#&/@#]&[#[[1]]+I #[[2]]]&/@#]&
		,Join[#,Transpose@Inverse@#&/@#]&[MatrixExp[#]&/@#]&];
	getXus=Function[{as,m},Module[{us=g[as]},{foldXUs[m,us,{}],Inverse/@us}]];
	If[cplx,f[as_?(NumericQ[#[[1,1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]],
		f[as_?(NumericQ[#[[1,1,1]]]&)]:=mynorm[foldXUs[D,g[as],{}]]];
	r=NMinimize[f[as],Flatten[as],MaxIterations->300];
	getXus[as/.r[[2]],D]
	]];
diamondForm=Function[{n,order},If[order==2,DiagonalMatrix@RandomReal[{1,2},n],Table[RotateRight[diamondForm[n,order-1],i-1],{i,n}]]];
(*d=diamondForm[2,3]
(MatrixForm/@{#.#\[Transpose]})&/@Table[unfoldTensor[d,i],{i,3}]*)
(*(MatrixForm/@{#.#\[Transpose],#\[Transpose].#,#,MatrixExp@#})&[Join@@@(Join@@d)]*)
(*Select[Table[{Tr[KroneckerProduct[randomUnitaryMatrix[2],randomUnitaryMatrix[2]].Flatten[d,{{1,2},{3(*,4*)}}]
	.(*KroneckerProduct[randomUnitaryMatrix[3],randomUnitaryMatrix[3]]*)randomUnitaryMatrix[2].Transpose@Flatten[d2,{{1,2},{3(*,4*)}}]],
	Total@Abs@Flatten[d d2]},{100000}],#[[1]]>#[[2]]&]*)
gradient2D=Function[m,MapThread[List,{Transpose[Differences@Append[#,0]&/@Transpose@m],Differences@Append[#,0]&/@m},2]];
gradient2DBackward=Function[m,MapThread[List,{Transpose[Differences@Prepend[#,0]&/@Transpose@m],Differences@Prepend[#,0]&/@m},2]];
divergence2D=Function[p,Differences@Prepend[#,0]&/@p[[;;,;;,2]]+Transpose[Differences@Prepend[#,0]&/@Transpose@p[[;;,;;,1]]]];
totalVariation2D=Function[m,Total@Abs@Flatten[Total[gradient2D[m],{3}]]];
tvDenoise=Function[{D,\[Theta],maxIter},Module[{oldp=RandomReal[1,Append[Dimensions@D,2]],n=Times@@Dimensions@D,p=Array[0&,Append[Dimensions@D,2]]
		,\[Tau]=1./9},
	Do[If[Norm@Flatten[p-oldp]/n<10^-6,Print[j];Break[],
		oldp=p;(*p=MapThread[(#+#2)/(1+Norm[#2])&,{p,\[Tau] grad[div[p]-D/\[Theta]]},2];*)
		Module[{gradd,down,sum},
			gradd=\[Tau] gradient2D[divergence2D[p]-D/\[Theta]];down=1+Map[Norm,gradd,{2}];sum=p+gradd;
			p=MapThread[List,{sum[[;;,;;,1]]/down,sum[[;;,;;,2]]/down},2]];
		]
		(*Print[Norm[rawM-(D-\[Theta] divergence2D[p]),"Frobenius"]];*)
		(*Print[Total@Abs@Flatten[divergence2D[p]-D/\[Theta]]];*)
	,{j,maxIter}];D-\[Theta] divergence2D[p]
	]];
weightedLeastSquares=Function[{m,b,weights},LinearModelFit[{m,b},IncludeConstantBasis->False,Weights->weights]["BestFitParameters"]];
iterativelyLeastSquareLp=Function[{m,b,p,maxIter},Module[{x=LeastSquares[m,b],weights,oldX=0},
	Do[If[Mean[(oldX-x)^2]<0.0001,Break[]];
		oldX=x;
		x=weightedLeastSquares[m,b,(0.000001+Abs[b-m.x])^(p-2)];
	,{maxIter}];x
	]];
geometricMean=Function[{pts,maxIter},(*Weiszfeld's algorithm*)Module[{y=Median@pts,weights},
	Do[(*weights=If[#==0,0,1/#]&[Norm[#-y]]&/@pts;*)(*weights=If[Abs[#]<0.0001,10000,1/#]&[Norm[#-y]]&/@pts;*)
		weights=1/(0.00001+Norm[#-y])&/@pts;y=Total[MapThread[Times,{weights,pts}]]/Total@weights;
	,{maxIter}];y
]];
totalVariationL1Cost=Function[{D,A,B,\[Lambda],\[Theta],u},(*inf_ {D} \sum \[Lambda] |A o D+B|+1/(2\[Theta])\sum (D-u)^2+\sum |\grad u|*)
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten[A D+B],1/(2\[Theta])Total@Flatten[(D-u)^2],totalVariation2D@u}];
totalVariationL1=Function[{initD,A,B,\[Lambda],maxIter},Module[{oldD=RandomReal[1,Dimensions@A],D=0 A,\[Theta]=1.,\[Rho]=0.9,dim=Dimensions@A,A2=A^2,u=initD,invA,\[CapitalOmega]A},
	invA=Map[If[#==0,0,1/#]&,A,{2}];\[CapitalOmega]A=Map[Boole[#!=0]&,A,{2}];
	Do[If[Norm[D-oldD,"Frobenius"]/(Times@@dim)<10^-5,Print[j];Break[],(*Print["s",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		D=(dShrinkageHadamard[\[Theta] A2,A u +B]-B) invA+(1-\[CapitalOmega]A)u;(*Print["d",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		u=tvDenoise[D,\[Theta],3];(*Print["u",totalVariationL1Cost[D,A,B,\[Lambda],\[Theta],u]];*)
		(*Print[Norm[rawM-D,"Frobenius"]];*)
	];\[Theta]*=\[Rho];
	,{j,maxIter}];D
	]];
(*SeedRandom[1003];rawM=ImageData@ColorConvert[ImageResize[Import@"t.jpg",100{1,1}],"Grayscale"];noise=randomSparseTensor[Dimensions@rawM,0.1];
\[CapitalOmega]=Normal@Map[Boole[#!=0]&,noise,{2}];m=rawM+noise;m//Image
m2=totalVariationL1[m,Array[-1&,Dimensions@m](*(1-\[CapitalOmega])*),m,0.1,60];//AbsoluteTiming
ImageResize[m2//Image,400]*)
warpByFlow=Function[{img2,flow},Module[{imgHeight,dim=Reverse@ImageDimensions@img2},imgHeight=dim[[1]];Image@Table[ImageValue[img2
	,With[{rc={i,j}+Reverse@flow[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]],DataRange->Full],{i,dim[[1]]},{j,dim[[2]]}]]];
joinTwoFlows=Function[{flows1,flows2},Module[{dim=Dimensions[flows1],interp=Interpolation[Join@@MapIndexed[{#2,#}&,flows2,{2}],InterpolationOrder->1]},
	Quiet@Table[flows1[[i,j]]+interp@@({i,j}+Reverse@flows1[[i,j]]),{i,dim[[1]]},{j,dim[[2]]}]]];
matchesFromFlows=Function[flows,Module[{dim=Dimensions[flows],imgHeight},imgHeight=dim[[1]];
	Transpose[Join@@Table[{rowColumnToXy[i,j,imgHeight]
		,With[{rc={i,j}+Reverse@flows[[i,j]]},rowColumnToXy[rc[[1]],rc[[2]],imgHeight]]},{i,dim[[1]]},{j,dim[[2]]}]]]];
dispFlow=Function[flows,ListVectorPlot[Reverse/@Transpose@flows,ImageSize->200,VectorScale->0.1]];
dispFlowColor=Function[flows,Graphics[Flatten@MapIndexed[{Hue[Arg[Plus@@(#{1,I})]/(2Pi),Norm[#],1],Point@#2}&,flows,{2}]]];
dispOpticalFlow=Function[{old,cur},Module[{ms,m1,m2,flows},
	ms=ImageData@(*ImageResize[*)ImageResize[ColorConvert[#,"Grayscale"],20(*{1,1}*)](*,40{1,1}]*)&/@{old,cur};{m1,m2}=ms;
	If[Norm[m1-m2,"Frobenius"]>0.01,
		(*flows=opticalFlow[m1,m2,Exp[-Norm@{#,#2}]&,1];*)
		(*flows=opticalFlowTikhonov[m1,m2,0.1,0.05];*)
		flows=opticalFlowTVL1[m1,m2,1,30];dispFlow@flows]
		]];
opticalFlow=Function[{m1,m2,weightFn,radius},Module[{dim=Dimensions@m1,mm1,mm2,weights},
	If[radius<1,Print["bad radius"];Abort[]];weights=Join@@Table[weightFn[i-radius,j-radius],{i,1,1+2radius-1},{j,1+2radius-1}];
	Table[
		mm1=m1[[i-radius;;i+radius,j-radius;;j+radius]];mm2=m2[[i-radius;;i+radius,j-radius;;j+radius]];
		1/2Plus@@MapThread[
			Function[{grad,extract},weightedLeastSquares[Join@@extract[extract/@grad],-Flatten@extract[extract/@(mm2-mm1)],weights]]
			,{{gradient2D@mm1,gradient2DBackward@mm1},{Most,Rest}},1]
		,{i,1+radius,dim[[1]]+1-2radius},{j,1+radius,dim[[2]]+1-2radius}]
	]];
opticalFlowTikhonov=Function[{mm1,mm2,\[Alpha],\[Beta]},Module[{m,ix,iy,it,p,flows},(*\[Alpha] Laplace and \[Beta] to zero*)
	1/2Plus@@MapThread[Function[{grad,extract,pad},
		m=Join@@(extract[extract/@grad]);
		{ix,iy}=DiagonalMatrix@SparseArray@#&/@{m[[;;,1]],m[[;;,2]]};it=Flatten@extract[extract/@(mm2-mm1)];
		p=\[Alpha]/4 poissonMatrix[Dimensions@mm1-1]+\[Beta] sparseIdentityMatrix[Times@@(Dimensions@mm1-1)];
		flows=ArrayPad[#,pad]&@Transpose[Partition[#,Dimensions[mm1][[2]]-1]&/@Reverse@Partition[#,Length@#/2]&@
			LinearSolve[SparseArray@ArrayFlatten@{{ix.ix+p,ix.iy},{ix.iy,iy.iy+p}},-Join[ix.it,iy.it]
				,Method->{"Krylov",Method -> "ConjugateGradient",Preconditioner->"ILU0"}],{3,1,2}];
		flows]
	,{{gradient2D@mm1,gradient2DBackward@mm1},{Most,Rest},{{{0,1},{0,1},{0,0}},{{1,0},{1,0},{0,0}}}},1]
	]];
opticalFlowTVL1Cost=Function[{ixy,it,uv,xy,\[Theta],\[Tau]},
	{Total@#,#}&@{Total@Flatten@MapThread[Function[{pxy,pixy,pit},Abs[pixy.pxy+pit]],{xy,ixy,it},2]
		,1/(2\[Theta])pnorm2[uv-xy,2],\[Tau] pnorm[gradient2D@uv[[;;,;;,1]],1],\[Tau] pnorm[gradient2D@uv[[;;,;;,2]],1]}];
opticalFlowTVL1=Function[{mm1,mm2,\[Tau],maxIter},Module[{it},Module[{\[Rho]=0.7,\[Theta]=1.,xy,uv,ixy,oldxy=RandomReal[1,Append[Dimensions@mm1-1,2]]},
	1/2Plus@@MapThread[Function[{grad,extract,pad},
		ixy=extract[extract/@grad];it=extract[extract/@(mm2-mm1)];
		uv=xy=extract[extract/@opticalFlowTikhonov[mm1,mm2,0.1,0.0]];
		(*uv=xy=RandomReal[1,Append[Dimensions@mm1-1,2]];*)
		Do[If[pnorm[xy-oldxy,2]/(Times@@Dimensions@mm1)<10^-5,(*Print[j];*)Break[]
			,oldxy=xy;(*Print["s",opticalFlowTVL1Cost[ixy,it,uv,xy,\[Theta],\[Tau]]];*)
			uv=TotalVariationFilter[xy,\[Theta] \[Tau]];(*Print["tv",opticalFlowTVL1Cost[ixy,it,uv,xy,\[Theta],\[Tau]]];*)
			xy=MapThread[Function[{pxy,pixy,pit},dShrinkageVector[\[Theta],pxy,pixy,pit]],{uv,ixy,it},2];
			(*Print["sh",opticalFlowTVL1Cost[ixy,it,uv,xy,\[Theta],\[Tau]]];*)];\[Theta]*=\[Rho];
		,{j,maxIter}];
		ArrayPad[#,pad]&@xy]
	,{{gradient2D@mm1,gradient2DBackward@mm1},{Most,Rest},{{{0,1},{0,1},{0,0}},{{1,0},{1,0},{0,0}}}},1]
	]]];
iterativelyLeastRayleighQuotientLp=Function[{dataMatrix,p,maxIter},Module[{m=dataMatrix,x,oldX=0,weights,n=Dimensions[dataMatrix][[2]]},
	(*covariance = dataMatrix\[ConjugateTranspose].dataMatrix*)
	If[Length@m>n
		,x=SingularValueDecomposition[m,n][[3,;;,-1]];
			Do[If[Mean[(oldX-x)^2]<0.0001,Break[]];
				oldX=x;weights=(0.000001+Abs[m.x])^(p-2);
				x=SingularValueDecomposition[weights m,n][[3,;;,-1]];
			,{maxIter}];x
		,Null(*Return Null if faile*)]
	]];
opencvDetect=Function[{image,detectType},Module[{fnames,imgHeight,imgs,rules,boxes},imgHeight=ImageDimensions[image][[2]];imgs={image};
	rules=StringReplace[FileBaseName@#,"haarcascade_"->""]->#&/@FileNames["/h/d/opencv-2.4.8/data/haarcascades/*.xml"];
	fnames=Table[CreateTemporary[],{Length@imgs+1}];MapThread[Export[#,#2,"JPG"]&,{fnames[[;;1]],imgs}];
	If[(detectType/.rules)==detectType,Print[{"Bad detectType",rules[[;;,1]]}]];
	Import["!/h/bin/opencv_matcher --task=detect "<>fnames[[1]]<>" --output_fname="<>fnames[[2]]<>" --face_cascade_name="<>(detectType/.rules),"Lines"];
	boxes=N@rowColumnToXy[#[[2]],#[[1]],imgHeight]&/@{#[[;;2]],#[[;;2]]+#[[3;;4]]}&/@Import[fnames[[2]],"CSV"];
	{"boxes"->boxes,"image"->Show[image, Graphics[{EdgeForm[{Cyan, Thick}], Opacity[0], Rectangle @@@ boxes}]]}
	]];
(*image=ExampleData[{"TestImage", "Girl3"}];r=opencvDetect[image,"frontalface_default"];{"image"/.r}*)

Clear[imageCorrespondingPoints];
imageCorrespondingPoints[image1_,image2_,opts_:{"Transformation"->"none"}]:=Module[{fnames,imgs={image1,image2},imgHeights},
	imgHeights=ImageDimensions[#][[2]]&/@imgs;
	fnames=Table[CreateTemporary[],{Length@imgs+1}];MapThread[Export[#,#2,"JPG"]&,{fnames[[;;2]],imgs}];
	If[Not@MemberQ[{"Perspective","Epipolar","none"},"Transformation"/.opts],Print[{"Transformation",opts}];Abort[]];
	Import["!/h/bin/opencv_matcher "<>fnames[[1]]<>" "<>fnames[[2]]<>" --output_fname="<>fnames[[3]]<>" --ransac_type="<>("Transformation"/.opts),"Lines"];
	N@Transpose[{rowColumnToXy[#[[1,2]]+1,#[[1,1]]+1,imgHeights[[1]]],
		rowColumnToXy[#[[2,2]]+1,#[[2,1]]+1,imgHeights[[2]]]}&@Partition[#,2]&/@Import@fnames[[3]]]
	];
imageCorrespondingPointsHomography=Function[{img,img2},Module[{imgs={img,img2},matches,homog,descriptors,nf,matches2},
	matches=imageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"Perspective"];homog=homographyFromMatches@matches;
	descriptors=ImageKeypoints[#,{"Position","OrientedDescriptor"}]&/@imgs;nf=Nearest[(#[[1]]->#)&/@descriptors[[2]]];
	matches2=Transpose@Join[Transpose@matches,Transpose@filterMatchesByGivenHomography[transposeLiers@Select[Module[{prexy2=#[[;;-2]]/#[[-1]]&[
		homog.Append[#[[1]],1]],xy=#[[1]],descriptor=#[[2]],xy2,descriptor2},
	{xy2,descriptor2}=First@nf[prexy2,1];If[Correlation[descriptor2,descriptor]>0.9,{xy,xy2},{}]
	]&/@descriptors[[1]],#=!={}&],homog,3]];matches2]];
drawNumberedPoints=MapIndexed[Inset[#2[[1]],#1]&,#]&;
annotateImageWithPoints=Function[{img,points},Show[img,Graphics[{Thick,Yellow,drawNumberedPoints@points}]]];
annotateImageWithPointGroup=Function[{img,pointGroup},
	Show[img,Graphics[Join@@MapIndexed[{Hue[0.7-0.7#2[[1]]/10],MapIndexed[Inset[#2[[1]],#]& ,#]}&,pointGroup]]]];
annotateImageWithLines=Function[{img,lines,scale},
	Show[img,Graphics[{Yellow,MapIndexed[{Inset[#2[[1]],#1[[1]]],Line[{#[[1]],#[[1]]+(#[[2]]-#[[1]])scale}]}& ,lines]}]]];
annotateImageWithTracks=Function[{img,tracks},Show[img,Graphics[BlendLine/@Transpose@tracks]]];
projectiveWhitenMatrix=Function[xys,Module[{ss,ts},
	ts=-Mean@#&/@Transpose@xys;ss=1/StandardDeviation@#&/@Transpose@xys;{{ss[[1]],0,ss[[1]] ts[[1]]},{0,ss[[2]],ss[[2]] ts[[2]]},{0,0,1}}]];
fundamentalMatrixFromMatchesBaseLp=Function[{matches,p},
	Module[{mat=Join@@MapThread[KroneckerProduct[{Append[#,1]},Append[#2,1]]&,matches],r},
		r=iterativelyLeastRayleighQuotientLp[mat,p,10];
		If[r===Null,Null,trimToRank[#/#[[-1,-1]],2]&@Transpose@Partition[r,3]]]];
fundamentalMatrixFromMatchesLp=Function[{matches,p},Module[{ts,tmatches,tfm},
	ts=projectiveWhitenMatrix/@matches;tmatches=Table[#[[;;2]]/#[[-1]]&[ts[[i]].Append[#,1]]&/@matches[[i]],{i,Length@matches}];
	tfm=fundamentalMatrixFromMatchesBaseLp[tmatches,p];
	If[tfm===Null,Null,trimToRank[#/#[[-1,-1]],2]&[Transpose[ts[[2]]].tfm.ts[[1]]]]]];
fundamentalMatrixFromMatchesBase=fundamentalMatrixFromMatchesBaseLp[#,2]&;
fundamentalMatrixFromMatches=fundamentalMatrixFromMatchesLp[#,2]&;
homogenousTransformationFromImagePair=Function[imgs,Module[{km,matches,em,fm},
	km=intrinsicParameterMatrix@imgs[[1]];
	matches=ImageCorrespondingPoints[imgs[[1]],imgs[[2]],"Transformation"->"EpiPolar"];
	fm=fundamentalMatrixFromMatchesLp[matches,1];em=Transpose[km].fm.km;
	Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&][[1]]
]];
drawEpipolarLine=Function[{img,lineNormal},(*lineNormal dot {x,y,1} = 0*)Module[{dim=Dimensions@ImageData@img},
	Line@If[lineNormal[[2]]==0
		,{{-lineNormal[[3]]/lineNormal[[1]],1},{-lineNormal[[3]]/lineNormal[[1]],dim[[1]]}}
		,Table[{x,-(x lineNormal[[1]]+lineNormal[[3]])/lineNormal[[2]]},{x,{1,dim[[2]]}}]
	]]];
drawEpipolarLineForFundamentalMatrix=Function[{img2,fm,n},Module[{dim=Reverse@ImageDimensions@img2,imgHeight},
	imgHeight=dim[[1]];
	Show[img2,Graphics[{Yellow}~Join~Table[drawEpipolarLine[img2,fm.Append[With[{rc={i,j}},rowColumnToXy[i,j,imgHeight]],1]]
		,{i,1,dim[[1]],Round[dim[[1]]/n]},{j,1,dim[[2]],Round[dim[[2]]/n]}]]]
	]];
correlationErrorHomography=Function[{h,imgPair},correlationError@{ImagePerspectiveTransformation[imgPair[[1]],h,DataRange->Full],imgPair[[2]]}];
correlationError=Function[imgPair,Module[{mask,ms=ImageData/@padToSameSize@imgPair},mask=Times@@(nonzeroMask/@ms);
	If[NumericQ@#,#,1]&[1-Correlation[Flatten[mask ms[[1]]],Flatten[mask ms[[2]]]]]]];
homographyFromMatches=Function[matches,With[{s=Max@Flatten@matches},
	With[{mat=Join@@MapThread[KroneckerProduct[{Append[#,s]},skewOmega@Append[#2,s]]&,matches]},
	#/#[[-1,-1]]&@(# {{1,1,s},{1,1,s},{1/s,1/s,1}})&@Transpose@Partition[SingularValueDecomposition[mat,9][[3,;;,-1]],3]]]];
fixSignHomography=Function[{homog,matches},
	Sign[Total@MapThread[Dot,{homog.Append[#,1]&/@matches[[1]],Append[#,1]&/@matches[[2]]}]]homog];
homographyFromMatchesL1=Function[matches,Module[{s,h,hs,mat,r},
	s=Max@Flatten@matches;mat=Join@@MapThread[KroneckerProduct[{Append[#,s]},skewOmega@Append[#2,s]]&,matches];
	hs=Array[h,9];r=NMinimize[{pnorm[mat.hs,1],pnorm[hs,2]==1},hs];
	fixSignHomography[#,matches]&@#/#[[-1,-1]]&@(# {{1,1,s},{1,1,s},{1/s,1/s,1}})&@Transpose@Partition[#/#[[-1]]&[hs/.r[[2]]],3]]];
homographyFromMatchesCalibratedIncludeThird=Function[{matches,ikm},
	With[{mat=Join@@MapThread[KroneckerProduct[{ikm.Append[#,1]},skewOmega[ikm.Append[#2,1]]]&,matches]},
	fixSignHomography[#,matches]&@
		#/#[[-1,-1]]&@(Inverse[ikm].Transpose@Partition[SingularValueDecomposition[mat,9][[3,;;,-1]],3].ikm)]];
homographyFromMatchesCalibrated=Function[{matches,ikmsIn},(*It's common to exclude the third.*)Module[{ikms},
	If[Length@Dimensions@ikmsIn==2,ikms={ikmsIn,ikmsIn},ikms=ikmsIn];
	With[{mat=Join@@MapThread[KroneckerProduct[{ikms[[1]].Append[#,1]},{{1,0,0},{0,1,0}}.skewOmega[ikms[[2]].Append[#2,1]]]&,matches]},
	fixSignHomography[#,matches]&@
		#/#[[-1,-1]]&@(Inverse[ikms[[2]]].Transpose@Partition[SingularValueDecomposition[mat,9][[3,;;,-1]],3].ikms[[1]])]]];
homographyTransform=Function[{pixelPosition,homography},#[[;;2]]/#[[3]]&[homography.Append[pixelPosition,1]]];
(*Decomposes as es=rs.skewOmega[ts]*)
decomposeEssentialMatrix=Function[em,Module[{u,s,v},
	{u,s,v}=SingularValueDecomposition[em];If[Det[v]<0,{v,s}=-{v,s}];If[Det[u]<0,{u,s}=-{u,s}];
	Table[Inverse@ArrayFlatten@{{u.rot.v\[Transpose],List/@inverseSkewOmega[u.rot.s.u\[Transpose]]},{0,1}}
		,{rot,{{{0,-1,0},{1,0,0},{0,0,1}},{{0,1,0},{-1,0,0},{0,0,1}}}}]]];
(*SeedRandom[1003];ts=Normalize@RandomReal[1,3];rs=randomSpecialOrthogonalMatrix[3];es=rs.skewOmega[ts];
{{MatrixForm@#[[;;3,;;3]],Normalize@#[[;;3,4]]}&/@decomposeEssentialMatrix@es,dispRtn@{rs,ts,ts}}*)
decomposeEssentialMatrixTR=Function[em,Module[{ts,rs},rs=First@Select[decomposeEssentialMatrix[-Transpose[em]][[;;,;;3,;;3]],Det@#>0&];
	ts=Normalize@First@NullSpace@Transpose@em;{{ts,rs},-{ts,householderTransformation[ts].rs}}]];

intrinsicParameterMatrix=Function[img,(*Use Nexus 4's*)intrinsicParameterMatrixWithFocalLength[img,4.6/3.2]];
intrinsicParameterMatrixWithFocalLength=Function[{img,focalLength},
	With[{f=focalLength Max@ImageDimensions[img]},{{f,0,(ImageDimensions[img][[1]])/2},{0,f,(ImageDimensions[img][[2]]-1)/2},{0,0,1}}]];
fundamentalMatrixFromMatchesMinReprojectionError=Function[{initEm,matchesIn,kmIn},Module[{f,e,es,r,em},
	es=Array[e,3{1,1}];em=trimToRank[initEm,2];
	f[em_?(NumericQ@#[[1,1]]&),km_,matches_]:=reprojectionError[em,km,matches,depthFromEssentialMatrix[em,km,matches]];
	r=FindMinimum[f[es,kmIn,matchesIn],variableWithInitial[es,initEm]];
	es/.r[[2]]
	]];
(*depthFromEssentialMatrixOld=Function[{em,km,matches}
		,Module[{trans,tran,projs,tmatches=With[{ikm=Inverse@km},Map[ikm.Append[#,1]&,matches,{2}]]},
	trans=Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&];
	If[Length@trans==0,Array[0&,Length@matches[[1]]]
		,tran=trans[[1]];
		projs={IdentityMatrix[{3,4}],Inverse[tran][[;;3]]};
		#[[3]]/#[[-1]]&[SingularValueDecomposition[#,4][[3,;;,-1]]]&/@
			Transpose[Join@@Table[{projs[[j,3]]#-projs[[j,1]]&/@tmatches[[j,;;,1]],projs[[j,3]]#-projs[[j,2]]&
				/@tmatches[[j,;;,2]]},{j,2}]]
	]]];*)
depthFromEm=Function[{match,projectionM,inverseKm},Module[{tmatch=inverseKm.Append[#,1]&/@match,projs={IdentityMatrix[{3,4}],projectionM}},
		#[[3]]/#[[-1]]&[SingularValueDecomposition[#,4][[3,;;,-1]]]&@
			(Join@@Table[{projs[[j,3]]#-projs[[j,1]]&@tmatch[[j,1]],projs[[j,3]]#-projs[[j,2]]&@tmatch[[j,2]]},{j,2}])
	]];
depthFromEssentialMatrix=Function[{em,km,matches},Module[{trans,tran,projs},
	trans=Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&];
	If[Length@trans==0,{Array[0&,Length@matches[[1]]],IdentityMatrix[{3,4}]}
		,tran=trans[[1]];
		With[{projM=Inverse[tran][[;;3]]},{depthFromEm[#,projM,Inverse[km]]&/@Transpose@matches,projM}]]
	]];
fundamentalMatrixFromMatchesMinReprojectionErrorEnforceRank=Function[{initEm,matchesIn,kmIn}
		,Module[{t,f0,f,ts,a,as,r,r2,initTs,initAs,em},
	em=trimToRank[initEm,2];ts=Array[t,3];as=Array[a,3{1,1}];initTs=First@NullSpace@Transpose@em;
	r=NMinimize[Norm[MatrixExp[as-as\[Transpose]]-PseudoInverse[skewOmega@initTs].em,"Frobenius"],Flatten@as];
	initAs=as/.r[[2]];
	f[as_?(NumericQ@#[[1,1]]&),ts_,km_,matches_]:=With[{em2=skewOmega[ts].(MatrixExp[as-as\[Transpose]])},
		reprojectionError[em2,km,matches,depthFromEssentialMatrix[em2,km,matches]]];
	r2=FindMinimum[f[as,ts,kmIn,matchesIn],variableWithInitial[as,initAs]~Join~variableWithInitial[ts,initTs]];
	Re[skewOmega[ts].MatrixExp[as-as\[Transpose]]/.r2[[2]]]
	]];

findSimilarityHomographyFromSequence=Function[imgs,Fold[{#[[1]]+#2[[1]],#[[2]].#2[[2]]}&,{0,IdentityMatrix[3]},findSimilarityHomography/@Partition[imgs,2,1]]];
refineImagesWithF=Function[{imgs,f},Module[{trs},
	trs=FoldList[{#[[1]]+If[#2===Null,0,#2[[1]]],#[[2]].If[#2===Null,IdentityMatrix[3],#2[[2]]]}&,{0,IdentityMatrix[3]},
		f/@Partition[imgs,2,1]];
	{trs,MapThread[ImagePerspectiveTransformation[#,#2,DataRange->Full]&,{imgs,trs[[;;,2]]}]}
	]];
findTranslationHomography=Function[imgPair,Module[{tr=Quiet@FindGeometricTransform[imgPair[[1]],imgPair[[2]],"Transformation"->"Translation"]},
	If[NumberQ[tr[[1]]]&&TransformationFunction===Head[tr[[2]]],{tr[[1]],TransformationMatrix[tr[[2]]]}]]];

(*fitModelF will return a model. enlargeModelF[maybeModel,points] will do best effort to enlarge the model,
	and return the model and its inliers.
evalModelF will eval the model on whole set of points*)
ransacDriver=Function[{points,fitModelF,costF,globalCostF,nullModel,torrerance,minNumPoints,goodNumPoints,maxIter}
			,Module[{maybeInliners,maybeModel,maybeError,bestModel=Null,bestError=Infinity,largerModel,inLiers,bestInLiers},
        Do[maybeInliners=RandomSample[points,minNumPoints];maybeModel=fitModelF[maybeInliners];
				inLiers=Select[points,costF[maybeModel,#]<torrerance&];
                If[Length@inLiers>goodNumPoints,largerModel=fitModelF@inLiers;
					Module[{error=globalCostF[largerModel,points]},
                      If[error<bestError,bestModel=largerModel;bestError=error(*Print[bestError];*)]];
                ];
        ,{maxIter}];
		If[bestModel===Null,nullModel,bestModel]]];
ransacFindLine2D=Function[{pointsIn,minNumXy,goodNumXy,torrerance,costF}
			,Module[{fitModelF,enlargeModelF,globalCostF,nullModel={0,0},x,maxIter=10},
		fitModelF=Function[points,Reverse[LinearModelFit[points,x,x]["BestFitParameters"]]];
		(*globalCostF=Function[{model,points},-Length@Select[points,costF[model,#]<torrerance&]];*)
		globalCostF=Function[{model,points},Total[costF[model,#]&/@points]];
		ransacDriver[pointsIn,fitModelF,costF,globalCostF,nullModel,torrerance,minNumXy,goodNumXy,maxIter]
	]];
(*\[Sigma]=0.1;SeedRandom[1003];{a,b}=RandomReal[1,2];points=Join[RandomReal[1,{200,2}],xys={#,a #+b+RandomReal[\[Sigma]{-1,1}]}&/@RandomReal[1,100]];
costF=Function[{model,point},Abs[point[[2]]-model[[1]]point[[1]]-model[[2]]]];
l={a,b};l2=ransacFindLine2D[points,60,100,0.1,costF];
pointsOneLine=Function[{a,b},Table[{x,a x +b},{x,0,1,0.05}]];
{{l,l2},Graphics[{Point@points,Blue,Point[pointsOneLine@@l],Red,Point[pointsOneLine@@l2]}]}*)

(*reprojectionError=Function[{essentialMatrix,intrinsicParameterMatrix,matches,depths}
			,Module[{em=essentialMatrix,km=intrinsicParameterMatrix,trans,tran,projs,points},
		trans=Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&];
		If[Length@trans==0,1000000
			,tran=trans[[1]];
			projs={IdentityMatrix[{3,4}],Inverse[tran][[;;3]]};
			points=Append[#,1]&/@((Inverse[km].Append[#,1]&/@matches[[1]]) depths);
	(*Wrong here*)		Norm[Join[matches[[1]]-(#[[;;2]]/#[[-1]]&/@(km.projs[[1]].#&/@points))
				,matches[[2]]-(#[[;;2]]/#[[-1]]&/@(km.projs[[2]].#&/@points))],"Frobenius"]/Length@matches[[1]]]
	]];*)
reprojectionErrorSingleMatchF=Function[{match,essentialMatrix,intrinsicParameterMatrix,projectionM,inverseKm}
		,Module[{depth=depthFromEm[match,projectionM,inverseKm],km=intrinsicParameterMatrix,point},
		point=Append[#,1]&@((inverseKm.Append[#,1]&@match[[1]]) depth);
		Norm[Join[match[[1]]-(#[[;;2]]/#[[-1]]&@(km.IdentityMatrix[{3,4}].point))
				,match[[2]]-(#[[;;2]]/#[[-1]]&@(km.projectionM.point))],"Frobenius"]
	]];
reprojectionError=Function[{essentialMatrix,intrinsicParameterMatrix,matches,depths}
			,Module[{em=essentialMatrix,km=intrinsicParameterMatrix,trans,points},
		trans=Select[decomposeEssentialMatrix[em],(#.{0,0,10,1})[[3]]>0&];
		If[Length@trans==0,1000000
			,Mean[reprojectionErrorSingleMatchF[#,em,km,Inverse[trans[[1]]][[;;3]],Inverse[km]]&/@Transpose@matches]
	]]];
reprojectionErrorHomography=Function[{homog,matches},1/2 (pnorm2[(#[[;;2]]/(10^-6 Sign[#[[-1]]]+#[[-1]])&[homog.Append[#,1]]&/@matches[[1]])-matches[[2]],2]
		+pnorm2[(#[[;;2]]/(10^-6 Sign[#[[-1]]]+#[[-1]])&[Inverse@homog.Append[#,1]]&/@matches[[2]])-matches[[1]],2])];
(*reprojectionErrorHomographyWeighted=Function[{homog,matches,imgDims},Module[{pps=imgDims/2},
	1/2 (pnorm2[MapThread[(#[[;;2]]/(10^-6 Sign[#[[-1]]]+#[[-1]])&[homog.Append[#,1]]- #2)Sqrt[Norm[#-pps[[1]]]Norm[#2-pps[[2]]]]&,matches,1],2]
		+pnorm2[MapThread[(#[[;;2]]/(10^-6 Sign[#[[-1]]]+#[[-1]])&[homog.Append[#,1]]- #2)Sqrt[Norm[#-pps[[1]]]Norm[#2-pps[[2]]]]&,Reverse@matches,1],2])]];*)
crossErrorHomography=Function[{homog,matches},pnorm2[MapThread[skewOmega[Append[#2,1]].homog.Append[#,1]&,matches],2]];
(*reprojectionErrorHomographyCalibrated=Function[{trans,ikm,matches},Total@Flatten[#^2&[(#/(10^-6 Sign[#[[-1]]]+#[[-1]])&[trans.ikm.Append[#,1]]&/@matches[[1]])-(ikm.Append[#,1]&/@matches[[2]])]
	+#^2&[(#/(10^-6 Sign[#[[-1]]]+#[[-1]])&[Inverse[trans].ikm.Append[#,1]]&/@matches[[2]])-(ikm.Append[#,1]&/@matches[[1]])]]];*)
padToSameSizeBottom=Function[imgs,Module[{dim2=MapThread[Max,ImageDimensions/@imgs]},
	Function[img,ImagePad[img,{{0,#[[1]]},{#[[2]],0}}&@(dim2-ImageDimensions[img])]]/@imgs]];
padToSameSize=Function[imgs,Module[{dim2=MapThread[Max,ImageDimensions/@imgs]},
	Function[img,ImagePad[img,{{0,#[[1]]},{0,#[[2]]}}&@(dim2-ImageDimensions[img])]]/@imgs]];
anaglyphPadBottom=Function[imgs,ColorCombine@Switch[Length@#,2,{#[[1]],#[[2]],#[[2]]},_,Take[#,3]]&[
	ColorConvert[#,"Grayscale"]&/@padToSameSizeBottom[imgs]]];
anaglyph=Function[imgs,ColorCombine@Switch[Length@#,2,{#[[1]],#[[2]],#[[2]]},_,Take[#,3]]&[
	ColorConvert[#,"Grayscale"]&/@padToSameSize[imgs]]];
anaglyphBrightness=Function[imagePair,ImageCompose[#[[1]],{#[[2]],0.7}]&@padToSameSize@imagePair];
highlightFaceInImage=Function[image,With[{features=FindFaces@image},
	Show[image, Graphics[{EdgeForm[{Cyan, Thick}], Opacity[0], Rectangle @@@ features}]]]];
createClosureBuffer=Function[n,Module[{lst={}},Function[l,lst=If[Head@lst=!=List,{}
		,If[Length@lst<n,Append[lst,l],Append[lst[[-n+1;;]],l]]]]]];
(*buf=createClosureBuffer[10];
buf[{1,0,0}] to add and get the current value*)
imageExifOrientation="Orientation"/.("Exif"/.(MetaInformation/.Options[Import[#,"ImageWithExif"], MetaInformation]))&;
(*eulerAnglesToRotationMatrix=RotationMatrix[-#[[3]],{0,0,1}].RotationMatrix[-#[[2]],{0,1,0}].RotationMatrix[-#[[1]],{1,0,0}]&;
(*Not so simple, confer https://truesculpt.googlecode.com/hg-history/38000e9dfece971460473d5788c235fbbe82f31b/Doc/rotation_matrix_to _euler.pdf
*)eulerAnglesFromRotationMatrix=Function[m,{-ArcTan[m[[3,3]],m[[3,2]]],ArcSin[m[[3,1]]],-ArcTan[m[[1,1]],m[[2,1]]]}];
(*m=eulerAnglesToRotationMatrix[{\[Phi],\[Theta],\[Psi]}];m//MatrixForm*)
(*Euler angles are bad representations.*)
(*(*Max@Abs@*)Table[w=Mod[RandomReal[3{-1,1},3],Pi];{Norm[#[[1]]-#[[2]]],#}&@{w,eulerAnglesFromRotationMatrix@eulerAnglesToRotationMatrix@w},{10}]*)*)
twoLineIntersection=Function[lines,Module[{x,y},
	Solve[And@@(Function[line,Det[Prepend[Append[#,1]&/@line,{x,y,1}]]==0]/@lines),{x,y}][[1,;;,2]]]];
slopeAngle=Arg[Plus@@((#[[2]]-#[[1]]){1,I})]&;
imageTakeRatios=Function[{img,m},Module[{dim=Dimensions@ImageData@img},
	ImageTake[img,Round/@(dim[[1]]m[[1]]),Round/@(dim[[2]]m[[2]])]]];
imageTakeCenterRatio=Function[{img,ratio},imageTakeRatios[img,{{(1-ratio)/2,(1+ratio)/2},{(1-ratio)/2,(1+ratio)/2}}]];
fuseRgbLuminance=Function[{smallRgb,luminance},
	ColorCombine[Prepend[Rest@ColorSeparate[ImageResize[smallRgb,ImageDimensions@luminance],"LAB"],luminance],"LAB"]];


showTensor3=Function[m,ImageResize[ColorCombine[Image/@m],{400}]];
showFourier2D =Image@RotateLeft[#, Floor[Dimensions[#]/2]] & /@ {Abs@#, Arg@#} &;
getQuaternionAdb=Function[{},Run@"/usr/local/bin/adb logcat -c";
	ImportString[StringSplit[Import["!/usr/local/bin/adb logcat -t 1 -s ROTV:I","Lines"],": "][[-1,2]],"CSV"][[1,-4;;]]];
transformImagePhoneImuFaceFrontHeadRight=Function[{img,oldQuaternion,quaternion},Module[{translation},
		translation={{1,0,-1/2},{0,1,-1/2 #[[2]]/#[[1]]&@ImageDimensions@img},{0,0,1}};
		ImageReflect[#,Top->Left]&@ImagePerspectiveTransformation[ImageReflect[img,Left->Top]
				,Inverse[translation].(LinearSolve@@(quaternionToRotationMatrix/@{oldQuaternion,quaternion})).translation]
	]];

drawSfmModel=Function[model,Join@@(drawCamera[5("position"/.#),("bw_rotation"/.#)]&/@model)];
loadSfmModel=Function[{case,tag},Module[{fname="/h/mxs/"<>"loadSfmModel."<>case<>"."<>tag<>".mx",result},
	If[FileExistsQ[fname],Import[fname],result=generateSfmModel[case,tag];Export[fname,result];result]]];
generateSfmModel=Function[{case,tag},Module[{rules,cameras,docids,cameraRules},
	rules="0:"<>FileBaseName[#]->Import[#,ImageSize->{400}]&/@FileNames["/s/df/t"<>case<>".t/unified_viewer/*.JPG"];
	cameras=Import["/g/df/t"<>case<>"."<>tag<>"/camera.csv"];docids=Import["/g/df/t"<>case<>"."<>tag<>"/docids.csv"];
	cameraRules=#[[1]]->{"position"->#[[2+9;;2+9+3-1]],"docid"->#[[1]]
		,"bw_rotation"->quaternionConjugate@quaternionFromRotationMatrix[#[[2;;2+9-1]]]}&/@cameras;
	{docids[[;;,;;-2]]/.cameraRules,rules}
	]];
viewSfmModel=Function[{case,tag,modelIdx},Module[{models,rules,xyzs,model,ranges,selected,g,scale},
	{models,rules}=loadSfmModel[case,tag];model=models[[modelIdx]];
	scale=1;xyzs=scale("position"/.#)&/@model;ranges={Min@#,Max@#}&/@Transpose@xyzs;
(*Magnify@*)Manipulate[
	scale=Exp@logScale;
	xyzs=scale("position"/.#)&/@model;ranges={Min@#,Max@#}&/@Transpose@xyzs;
	selected=Select[#,Head["docid"/.#/.rules]=!=String&]&@
		Select[model,With[{pos=scale "position"/.#},xmin<=pos[[1]]<=xmax&&ymin<=pos[[2]]<=ymax&&zmin<=pos[[3]]<=zmax]&];
	g=Graphics3D[{drawCameraWithImage[scale("position"/.#),("bw_rotation"/.#),(("docid"/.#)/.rules)]&/@
		selected},Lighting->"Neutral",ImageSize->1200,Axes->True,AxesLabel->{"x","y","z"},PlotRange->((Mean/@ranges)+(Standardize[#,Mean,0.2&]&/@ranges))]
	,{xmin,ranges[[1,1]],ranges[[1,2]]},{{xmax,ranges[[1,2]]},ranges[[1,1]],ranges[[1,2]]}
	,{ymin,ranges[[2,1]],ranges[[2,2]]},{{ymax,ranges[[2,2]]},ranges[[2,1]],ranges[[2,2]]}
	,{zmin,ranges[[3,1]],ranges[[3,2]]},{{zmax,ranges[[3,2]]},ranges[[3,1]],ranges[[3,2]]}
	,{{logScale,0},-4,4,0.01}
	,ContinuousAction->False]]];
viewSfmModelWithScale=Function[{case,tag,modelIdx,scale},Module[{models,rules,xyzs,model,ranges,selected,g(*,scale*)},
	{models,rules}=loadSfmModel[case,tag];model=models[[modelIdx]];
	xyzs=scale("position"/.#)&/@model;ranges={Min@#,Max@#}&/@Transpose@xyzs;
(*Magnify@*)Manipulate[
	(*scale=Exp@logScale;
	xyzs=scale("position"/.#)&/@model;ranges={Min@#,Max@#}&/@Transpose@xyzs;*)
	selected=Select[#,Head["docid"/.#/.rules]=!=String&]&@
		Select[model,With[{pos=scale "position"/.#},xmin<=pos[[1]]<=xmax&&ymin<=pos[[2]]<=ymax&&zmin<=pos[[3]]<=zmax]&];
	g=Graphics3D[{drawCameraWithImage[scale("position"/.#),("bw_rotation"/.#),(("docid"/.#)/.rules)]&/@
		selected},Lighting->"Neutral",ImageSize->1200,Axes->True,AxesLabel->{"x","y","z"},PlotRange->((Mean/@ranges)+(Standardize[#,Mean,0.2&]&/@ranges))]
	,{xmin,ranges[[1,1]],ranges[[1,2]]},{{xmax,ranges[[1,2]]},ranges[[1,1]],ranges[[1,2]]}
	,{ymin,ranges[[2,1]],ranges[[2,2]]},{{ymax,ranges[[2,2]]},ranges[[2,1]],ranges[[2,2]]}
	,{zmin,ranges[[3,1]],ranges[[3,2]]},{{zmax,ranges[[3,2]]},ranges[[3,1]],ranges[[3,2]]}
	(*,{{logScale,0},-4,4,0.01}*)
	,ContinuousAction->False]]];


robustPvdCostFunction=Function[{\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y},
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten@MapThread[Times,{\[CapitalOmega]s,S}],
		Total@Abs@Flatten@X,
		\[Mu]/2 Total@Flatten@MapThread[Function[{d,x,s},(d-U.x.Transpose[V]-s)^2],{D,X,S}],
		Total@MapThread[Function[{y,d,s,x},Tr[Transpose[y].(d-U.x.Transpose[V]-s)]],{Y,D,S,X}]}];
robustPvd=Function[{Data,\[CapitalOmega]s,rank,maxIter},Module[{D=Data,norm2,Y,\[Mu],\[Rho]=1.01,\[Lambda]=10.,X,S,L,ZS,U,V,n=Length@Data,dim=Dimensions[Data[[1]]],VU,VUT,solveU},
	norm2=First@SingularValueList[Mean@Data,1];Y=0 D;S=0D;\[Mu]=1.25 n/norm2;{U,V}=Transpose@Orthogonalize@Transpose@RandomReal[1,#]&/@{{dim[[1]],rank},{dim[[2]],rank}};
	X=RandomReal[1,{n,rank,rank}];Print@{"\[Mu]",\[Mu]};
	(*Print[Dimensions/@{Data,\[CapitalOmega]s,U,X,V,Y}];*)
	Do[
		L=U.#.Transpose[V]&/@X;
		ZS=MapThread[Function[{Y,D,L},Y/\[Mu]+D-L],{Y,D,L}];
		If[Mod[j,300]==1,Print[robustPvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]]];
		S=MapThread[Function[{\[CapitalOmega],ZS},\[CapitalOmega] dShrinkage[\[Lambda]/\[Mu],ZS]+(1-\[CapitalOmega])ZS],{\[CapitalOmega]s,ZS}];
		If[Mod[j,300]==1,Print[{"S",robustPvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		X=MapThread[Function[{D,S,Y},dShrinkage[1/\[Mu],Transpose[U].(D-S+Y/\[Mu]).V]],{D,S,Y}];
		If[Mod[j,300]==1,Print[{"X",robustPvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		U=#[[3]].Transpose[#[[1]]]&@SingularValueDecomposition[#,rank]&@Total@MapThread[Function[{D,S,Y,X},X.Transpose[V].Transpose[D-S+Y/\[Mu]]],{D,S,Y,X}];
		If[Mod[j,300]==1,Print[{"U",robustPvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		V=#[[1]].Transpose[#[[3]]]&@SingularValueDecomposition[#,rank]&@Total@MapThread[Function[{D,S,Y,X},Transpose[D-S+Y/\[Mu]].U.X],{D,S,Y,X}];
		If[Mod[j,300]==1,Print[{"V",robustPvdCostFunction[\[CapitalOmega]s,U,V,X,S,D,\[Lambda],\[Mu],Y]}]];
		Y+=\[Mu] MapThread[Function[{D,X,S},D-U.X.Transpose[V]-S],{D,X,S}];
		\[Mu]*=\[Rho];
		(*Print@MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}];*)
		(*Print[Dimensions/@{U,X,V,S,Y}];*)
		(*Print[MatrixForm/@{U,X,V,S,Y}];*)
	,{j,maxIter}];
	{U,X,V,S}
	]];
robustSudCostFunction=Function[{\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y},
	{Total@#,#}&@{\[Lambda] Total@Abs@Flatten@MapThread[Times,{\[CapitalOmega]s,S}],
		Total@Abs@Flatten@X,
		\[Mu]/2 Total@Flatten[(D-foldXUs[X,Us,{}]-S)^2],
		Flatten[Y].Flatten[D-foldXUs[X,Us,{}]-S]}];
robustSud=Function[{Data,\[CapitalOmega]s,rank,maxIter},Module[{D=Data,norm2,Y,\[Mu],\[Rho]=1.01,\[Lambda]=10.,X,S,L,ZS,Us,n=Length@Data,dim=Dimensions[Data[[1]]]},
	norm2=First@SingularValueList[Mean@Data,1];Y=0 D;S=0D;\[Mu]=1.25 n/norm2;
	Us=Transpose@Orthogonalize@Transpose@RandomReal[1,#]&/@{{3,3},{dim[[1]],rank},{dim[[2]],rank}};
	X=RandomReal[1,{3,rank,rank}];Print@{"\[Mu]",\[Mu]};
	(*Print[Dimensions/@{Data,\[CapitalOmega]s,U,X,V,Y}];*)
	Do[
		L=foldXUs[X,Us,{}];
		ZS=Y/\[Mu]+D-L;
		If[Mod[j,300]==1,Print[robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]]];
		S=\[CapitalOmega]s dShrinkage[\[Lambda]/\[Mu],ZS]+(1-\[CapitalOmega]s)ZS;
		If[Mod[j,300]==1,Print[{"S",robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}]];
		X=dShrinkage[1/\[Mu],foldXUs[D-S+Y/\[Mu],Transpose/@Us,{}]];
		(*(*If[Mod[j,300]==1,*)Print[{"X",robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}];*)
		Do[
			Us[[i]]=procrustes[unfoldTensor[D-S+Y/\[Mu],i].Transpose[unfoldTensor[foldXUs[X,Us,{i}],i]]];
			If[Mod[j,300]==1,Print[{"Us"<>IntegerString@i,robustSudCostFunction[\[CapitalOmega]s,Us,X,S,D,\[Lambda],\[Mu],Y]}]];
		,{i,Length@Us}];
		Y+=\[Mu] (D-foldXUs[X,Us,{}]-S);
		\[Mu]*=\[Rho];
		(*Print@MapThread[Function[{X,D,\[CapitalOmega]},Norm[(1-\[CapitalOmega])(U.X.Transpose[V]-D),"Frobenius"]],{X,data,\[CapitalOmega]s}];*)
		(*Print[Dimensions/@{U,X,V,S,Y}];*)
		(*Print[MatrixForm/@Us];*)
		(*Print[MatrixForm/@{X,S,Y}];*)
	,{j,maxIter}];
	{X,Us,S}
	]];
minimizeL1OverLinear=Function[{A,B,C,maxIter},Module[{Z,Y=0A,OX,X,\[Mu],BtB=B\[Transpose].B,CCt=C.C\[Transpose],\[Rho]=1.01},(*Solves \min_X \| A + B X C \|_ 1*)
	\[Mu]=12.5/SingularValueDecomposition[A,1][[2,1,1]];X=Array[0&,{Dimensions[B][[2]],Dimensions[C][[1]]}];
	Do[
	Z=dShrinkage[1/\[Mu],A+B.X.C-Y/\[Mu]];
	OX=X;
	X=leastSquaresTwoSided[\[Mu] BtB,CCt,-(\[Mu] B\[Transpose].(A-Z-Y/\[Mu]).C\[Transpose])];
	If[pnorm[X-OX,2]/(pnorm[A,2]+pnorm[X,2])<0.0005,Break[]];
	Y+=\[Mu] (Z-(A+B.X.C));
	\[Mu]*=\[Rho];
	,{maxIter}];
	X
	]];
minimizeL1OverLinearAndMask=Function[{A,B,C,\[CapitalOmega],maxIter},Module[{Z,Y=0A,OX,X,\[Mu],BtB=B\[Transpose].B,CCt=C.C\[Transpose],\[Rho]=1.01},(*Solves \min_X \| A + B (\[CapitalOmega] o X) C \|_ 1*)
	\[Mu]=12.5/SingularValueDecomposition[A,1][[2,1,1]];X=Array[0&,{Dimensions[B][[2]],Dimensions[C][[1]]}];
	Do[
	Z=dShrinkage[1/\[Mu],A+B.(\[CapitalOmega] X).C-Y/\[Mu]];
	OX=X;
	X=leastSquaresTwoSidedMask[\[Mu] BtB,CCt,-(\[Mu] B\[Transpose].(A-Z-Y/\[Mu]).C\[Transpose]),\[CapitalOmega]];
	If[pnorm[\[CapitalOmega](X-OX),2]/(pnorm[A,2]+pnorm[\[CapitalOmega] X,2])<0.0005,Break[]];
	cost=pnorm[A+B.(\[CapitalOmega] X).C,1];
	Y+=\[Mu] (Z-(A+B.(\[CapitalOmega] X).C));
	\[Mu]*=\[Rho];
	,{maxIter}];
	X
	]];


movieLens100k=Function[{},Import@"/h/mxs/MovieLens100k.mx"];


(*(*We can't handle polar decomposition directly. The result below sometimes equal QR, but sometimes not. By wiki it should be SVD.*)
SeedRandom[1003];Clear[a,b,f,g];n=3;{as,bs}=Array[#,n{1,1}]&/@{a,b};m=RandomReal[3{-1,1},n{1,1}];
g[as_,bs_]:=Inverse[MatrixExp[strictLowerTriangle@bs]].MatrixExp[as-Transpose@as].m.Inverse@Transpose[MatrixExp[strictLowerTriangle@bs]];
f[as_?(NumericQ[#[[1,1]]]&),bs_]:=pnorm[g[as,bs],1];
r=NMinimize[f[as,bs],Flatten@{as,bs},MaxIterations->500];//AbsoluteTiming
MatrixExp/@{MatrixExp[(as/.r[[2]])-Transpose[as/.r[[2]]]],MatrixExp[strictLowerTriangle[bs/.r[[2]]]]}
pnorm[#,1]&/@{g[as/.r[[2]],bs/.r[[2]]],SingularValueList@m,Eigenvalues@m,Diagonal@Last@QRDecomposition[m]}
MatrixForm/@{g[as/.r[[2]],bs/.r[[2]]],SingularValueList@m,Abs@Eigenvalues@m,Diagonal@Last@QRDecomposition[m]}*)


(*laplacianMatrixFromMatrix2=Compile[{{gray,_Real,2},{mask,_Real,2},{radius,_Integer}},
	Module[{dim=Dimensions@mask,indices=Most@{{0,0}},vals=Most@{0.}},
	Do[If[mask[[i,j]]==1,AppendTo[indices,{i dim[[2]]+j,i dim[[2]]+j}];AppendTo[vals,1.];
		,Module[{neighbors=Select[Flatten[Table[{ii,jj}
				,{ii,Max[1,i-radius],Min[dim[[1]],i+radius]}
				,{jj,Max[1,j-radius],Min[dim[[2]],j+radius]}],1],#!={i,j}&]
			,gvals,tval=gray[[i,j]],csig,mgv,weights},
		gvals=gray[[#[[1]],#[[2]]]]&/@neighbors;
		csig=Max[0.000002,Max[0.6 Variance@Append[gvals,tval],-Min[(gvals-tval)^2]/Log[0.01]]];
		(*Print[gvals];Abort[];*)
		Do[AppendTo[indices,{i dim[[2]]+j,neighbors[[k,1]]dim[[2]]+neighbors[[k,2]]}]
			,{k,Length@neighbors}];
		(*AppendTo[indices,{i dim[[2]]+j,#[[1]]dim[[2]]+#[[2]]}]&/@neighbors;*)
		weights=(-#/Total@#&@Exp[-(gvals-tval)^2/csig]);
		Do[AppendTo[vals,weights[[k]]],{k,Length@weights}]
		(*AppendTo[vals,#]&/@(-#/Total@#&@Exp[-(gvals-tval)^2/csig]);*)
		]];
	,{i,dim[[1]]},{j,dim[[2]]}];
	SparseArray[indices->vals,dim[[1]]dim[[2]]{1,1}]],{{_Insert,_Integer,2}}];*)
