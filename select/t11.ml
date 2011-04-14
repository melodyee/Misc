open Linda
open ExtChar
open ExtString
open ExtList
let isDirective s =
	let s' = (strip s) in
	startsWith s' "." && isLowerCaseLetter (get s' 1)
let isComment s =
	let s' = (strip s) in
	startsWith s' "#"
 
let preProcess l = filter (fun s -> strip s <> "") l
		
(*let readDirectiveComment = span (fun s -> isDirective s || isComment s)*)
let token needle hay = startsWith (lstrip hay) needle
(*let parse l =                                                      *)
(*	let header,sections = span (fun s -> not (token ".section")) l in*)



