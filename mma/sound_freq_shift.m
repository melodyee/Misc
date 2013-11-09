snd=SystemDialogInput["RecordSound"];EmitSound@snd
symShiftList=Function[{lst,n}
	,Prepend[Module[{hn=Round[Length[#]/2],middle},
		middle=If[Mod[Length[#],2]==0,{},{#[[hn+1]]}];
		Join@@{#,middle,Conjugate@Reverse@#}&@If[n>=0,PadLeft[#[[;;hn-n]],hn],PadRight[#[[-n+1;;hn]],hn]]]&@Rest@lst,First@lst]];
Import@Export["t.png",Rasterize[Function[shift,
	snd3=Re@InverseFourier[symShiftList[Fourier@snd[[1,1,1]],shift]];
	EmitSound@SampledSoundList[{snd3},32000];ListPlot[snd3,PlotRange->All]]/@{-2000,-1000,0,1000,3000}]]
