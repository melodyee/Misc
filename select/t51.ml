module Mips = struct
  open Printf
  type coProcessor = 
    [ `C0 | `C1 ]
  let showCoProcessor :coProcessor -> string = function
    | `C0 -> "c0"
    | `C1 -> "c1"  
  type reg =
    [ `Hi
    | `Lo
    | `Fcc of int
    | `FReg of int
    | `IReg of int
    | `CopReg of (coProcessor * int)
    ]
  type ireg = [ `IReg of int ]
  type freg = [ `FReg of int]
  type greg = [ `IReg of int | `FReg of int]
  
  let regCategory :reg -> string= function
    | `Hi
    | `Lo -> "hilo"
    | `Fcc _ -> "fcc"
    | `FReg _ -> "float"
    | `IReg i -> 
      if i = 28 then "gp" else
        if i = 29 then "stack" else "pointer"  
    | `CopReg (cop,i) -> showCoProcessor cop

  let ireg i = assert (0<=i && i<=31);`IReg i
  let freg i = assert (0<=i && i<=31);`FReg i
  let copReg cop i = `CopReg (cop,i)
  let showReg = function
    | `Hi -> "$hi"
    | `Lo -> "$lo"
    | `Fcc i -> sprintf "$fcc%d" i
    | `FReg i -> sprintf "$f%d" i
    | `IReg i -> sprintf "$%d" i
    | `CopReg (cop,i) -> sprintf "$%d" i
    
  type immed =
    | Immed of int64
    | Label of string
    | AsmOp of string * string
  type addr = immed * ireg
  let showImmed = function
    | Immed i -> Int64.to_string i
    | Label s -> s
    | AsmOp(a, b) -> sprintf "%s(%s)" a b
  let showAddr :addr -> string = fun (i, r) -> 
    sprintf "%s(%s)" (showImmed i) (showReg r)
  
  type cmpOp =
    [ `EQ | `NE | `LE | `LT | `GE | `GT |
    `F | `UN | `UEQ | `OLT | `ULT | `OLE | `ULE | `SF |
    `NGLE | `SEQ | `NGL | `NGE | `NGT
    ]
  let showCmpOp :cmpOp -> string= function
    | `EQ -> "eq"
    | `NE -> "ne"
    | `LE -> "le"
    | `LT -> "lt"
    | `GE -> "ge"
    | `GT -> "gt"
    | `F -> "f"
    | `UN -> "un"
    | `UEQ -> "ueq"
    | `OLT -> "olt"
    | `ULT -> "ult"
    | `OLE -> "ole"
    | `ULE -> "ule"
    | `SF -> "sf"
    | `NGLE -> "ngle"
    | `SEQ -> "seq"
    | `NGL -> "ngl"
    | `NGE -> "nge"
    | `NGT -> "ngt"
  type typ =
    [ `V of int 
    | `F of int
    | `I of int
    | `U of int
    | `PS 
    ]
  type ityp = [`I of int | `U of int]
  type ftyp = [`V of int | `F of int | `PS ] (* residing in freg *)
  let showTyp :typ -> string= function
    | `V i -> sprintf "V%d" i
    | `F i -> sprintf "F%d" i
    | `I i -> sprintf "I%d" i
    | `U i -> sprintf "U%d" i
    | `PS -> "PS"
  let toLoadStoreMod :typ -> string = function
    | `I i -> 
      List.assoc i [8,"b";16,"h";32,"w";64,"d"]
    | `U i ->
      List.assoc i [8,"bu";16,"hu";32,"wu";64,"d"]
    | `F i ->
      List.assoc i [32,".s";64,".d"]
    | `PS -> ".d"
    | `V _ -> ".d"
  let toBinOpMod :typ -> string -> string = fun t s ->
    match t with
      | `I 64 -> "d"^s
      | `U 64 -> "d"^s^"u"
      | `I _ -> s
      | `U _ -> s^"u"
      | `F 64 -> s^".d"
      | `F _ -> s^".s"
      | `V _ -> failwith "toBinOpMod"
      | `PS -> s^".ps" 
  type ternOp =
    [ `Madd | `Msub | `Nmadd | `Nmsub ]
  let showTernOp :ternOp -> string = function
    | `Madd -> "madd"
    | `Msub -> "msub"
    | `Nmadd -> "nmadd"
    | `Nmsub -> "nmsub"
  type binOp =
    [ `And | `Or | `Xor | `Nor
    | `Slt
    | `Add | `Sub
    | `Mul | `Div | `Mod
    | `Sll | `Srl | `Sra ]

  let showBinOp :binOp -> string= function
    | `And -> "and"
    | `Or -> "or"
    | `Xor -> "xor"
    | `Nor -> "nor"
    | `Slt -> "slt"
    | `Add -> "add"
    | `Sub -> "sub"
    | `Mul -> "mul"
    | `Div -> "div"
    | `Mod -> "mod"
    | `Sll -> "sll"
    | `Sra -> "sra"
    | `Srl -> "srl"

  type uOp =
    [ `Lu
    | `Sqrt
    | `Neg
    | `Abs
    | `Round
    | `Trunc
    | `Ceil
    | `Floor
    | `Cvt
    | `Mov ]
  
  let showUOp :uOp -> string= function
    | `Lu -> "lu"
    | `Sqrt -> "sqrt"
    | `Neg -> "neg"
    | `Abs -> "abs"
    | `Round -> "round"
    | `Trunc -> "trunc"
    | `Ceil -> "ceil"
    | `Floor -> "floor"
    | `Cvt -> "cvt"
    | `Mov -> "mov"
  let toCmovMod r m = match r with
    | `IReg _ -> begin match m with
      | `N -> "n"
      | `Z -> "z"
      end
    | `Fcc 0 -> begin match m with
      | `N -> "f"
      | `Z -> "t"
      end
    | _ -> failwith "toCmovMod"
  type instr =
    [ `Load of (typ * reg * addr)
    | `Store of (typ * reg * addr)
    | `CMov of ( [`N | `Z] * reg * reg * [ `Fcc of int | `IReg of int])
    | `BOp of typ * binOp * reg * reg * reg
    | `BOpI of typ * binOp * reg * reg * immed
    | `Mul of typ * ireg * ireg
    | `Div of typ * ireg * ireg
    | `UOp of typ * uOp * reg * reg
    | `UOpI of typ * uOp * reg * immed
    | `Sync
    | `Break
    | `Nop
(*    | `BOp of *)
(*    | `Mov of *)
(*    | BOp of (binop * reg * reg)   *)
(*    | BOpI of (binop * reg * immed)*)
(*    | UOp of (uop * reg)           *)
(*    | UOpI of (uop * immed)        *)
(*    | Special of string            *)
(*    | `Attr of (modifier list * instr)*)
    ]
  let showInstr :instr -> string = function
    | `Load (typ,reg,addr) ->
      sprintf "l%s %s,%s" (toLoadStoreMod typ) (showReg reg) (showAddr addr)
    | `Store (typ,reg,addr) ->
      sprintf "s%s %s,%s" (toLoadStoreMod typ) (showReg reg) (showAddr addr)
    | `CMov (m,r,r',r'') ->
      sprintf "mov%s %s,%s,%s" (toCmovMod r'' m) (showReg r) (showReg r') (showReg r'')
    | `BOp (typ,bop,r,r',r'') ->
      sprintf "%s %s,%s,%s" (toBinOpMod typ (showBinOp bop)) (showReg r) (showReg r') (showReg r'')
    | `BOpI (typ,bop,r,r',i) ->
      sprintf "%s %s,%s,%s" (toBinOpMod typ (showBinOp bop)) (showReg r) (showReg r') (showImmed i)
    | `UOp (typ,uop,r,r') ->
      sprintf "%s %s,%s" (toBinOpMod typ (showUOp uop)) (showReg r) (showReg r')
    | `UOpI (typ,uop,r,i) ->
      sprintf "%s %s,%s" (toBinOpMod typ (showUOp uop)) (showReg r) (showImmed i)
    | `Mul (typ,r,r') ->
      sprintf "%s %s,%s" (toBinOpMod typ "mul") (showReg r) (showReg r')
    | `Div (typ,r,r') ->
      sprintf "%s %s,%s" (toBinOpMod typ "div") (showReg r) (showReg r')
    | `Sync -> "sync"
    | `Break -> "break"
    | `Nop -> "nop"

end  