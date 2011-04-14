type module_ = toplevel list * string
and toplevel =
  | FunctionDecl of function_
  | GlobalDecl of global
  | SymbolDecl of symbol

and linkage =
  | LPrivate 
  | LLinkerPrivate (* removed by the linker *)
  | LInternal (* the 'static' keyword in C. *)
  | LAvailableExternally (* allowed to be discarded at will. *)
  | LLinkOnce (* allowed to be discarded. *)
  | LWeak
  (* may not be discarded.*)
  (*  "weak" in C source code. *)
  | LCommon (* "int X;" at global scope *)
  | LAppending (* only be applied to global variables of pointer to array type. *)
  | LExternWeak (* weak until linked, becomes null if not linked*)
  | LLinkOnceOdr (* one definition rule *)
  | LWeakOdr (* one definition rule *)
  | LDefault (* externaly visible *)



