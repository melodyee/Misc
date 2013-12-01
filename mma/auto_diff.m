Clear[dual];
dual/:Plus[dual[x_],dual[y_]]:= dual[x+y];
dual/:Times[dual[x_],dual[y_]]:= dual[{x[[1]]y[[1]],y[[1]]x[[2]]+x[[1]]y[[2]]}];
dual/:Power[dual[x_],-1]:= dual[{1/x[[1]],-x[[2]]x[[1]]^-2}];
dual/:Sin[dual[x_]]:= dual[{Sin[x[[1]]],x[[2]]Cos[x[[1]]]}];
dual/:f_[dual[x_]]:= Module[{z},dual[{f[x[[1]]],x[[2]]D[f[z],z]/.(z->x[[1]])}]];
dual[{3,4}]+dual[{1,2}]
dual[{3,4}]dual[{1,2}]
1/dual[{2,1}]
dual[{3,4}]/dual[{2,1}]
Cos@dual[{2,1}]
Sin@dual[{2,1}]
Exp@dual[{2,1}]
Log@dual[{2,1}]
