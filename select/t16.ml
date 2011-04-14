open Linda
(*open ExtArray*)
open Printf

(*open ExtList*)
(*open ListMonad*)

module Mips = struct
  type reg = int
  let numNamedReg reg = match ExtString.to_list reg with
    | 'a'::'t'::[] -> 1 
    | 'v'::(('0'|'1') as x)::[] -> ExtChar.digitToInt x + 2 
    | 'a'::(('0'..'7') as x)::[] -> ExtChar.digitToInt x + 4
    | 't'::(('4'..'7') as x)::[] -> ExtChar.digitToInt x + 8
    | 's'::(('0'..'7') as x)::[] -> ExtChar.digitToInt x + 16
    | 't'::(('8'|'9') as x)::[] -> ExtChar.digitToInt x + 16
    | 'k'::'t'::(('0'|'1') as x)::[] -> ExtChar.digitToInt x + 26
    | 'g'::'p'::[] -> 28
    | 's'::'p'::[] -> 29
    | 's'::'8'::[] -> 30
    | 'f'::'p'::[] -> 30
    | _ -> failwith "numNamedReg"
  type immed = 
    | Immed of int 
    | Label of string
    | AsmOp of string * string  
  type addr = immed * reg
  type cmpOp =
    [ `EQ | `NE | `LE | `LT | `GE | `GT |
    `F | `UN | `UEQ | `OLT | `ULT | `OLE | `ULE | `SF |
    `NGLE | `SEQ | `NGL | `NGE | `NGT
    ]
  let showCmpOp = function
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
  type ternop =
    | And | Or | Xor | Nor
    | Slt
    | Add | Sub
    | Mulg | Divg | Modg
    | FMul | FDiv
    | Sll | Srl | Sra
    | Movz | Movn
    | B of cmpOp
    | Madd | Msub | Nmadd | Nmsub

  
  let showTernOp = function
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Nor -> "nor"
    | Slt -> "slt"
    | Add -> "add"
    | Sub -> "sub"
    | FMul -> "mul"
    | FDiv -> "div"
    | Mulg -> "mult"
    | Divg -> "div"
    | Modg -> "mod"
    | Sll -> "sll"
    | Sra -> "sra"
    | Srl -> "srl"
    | Movz -> "movz"
    | Movn -> "movn"
    | B c -> "b" ^ showCmpOp c
    | Madd -> "madd"
    | Msub -> "msub"
    | Nmadd -> "nmadd"
    | Nmsub -> "nmsub"
  type binop =
    | Mul | Div
    | Lu
    | Mfc0 | Mtc0 | Mfc1 | Mtc1
    | Jalr
    | Bz of cmpOp
    | T of cmpOp
    | Sqrt
    | Neg
    | Abs
    | Round
    | Trunc
    | Ceil
    | Floor
    | Cvt
    | Mov
    | C of cmpOp
  
  let showBinop = function
    | Mul -> "mult"
    | Div -> "div"
    | Lu -> "lu"
    | Mfc0 -> "mfc0"
    | Mtc0 -> "mtc0"
    | Mfc1 -> "mfc1"
    | Mtc1 -> "mtc1"
    | Jalr -> "jalr"
    | T c -> "t" ^ showCmpOp c
    | Bz c -> "b" ^ showCmpOp c ^ "z"
    | Sqrt -> "sqrt"
    | Neg -> "neg"
    | Abs -> "abs"
    | Round -> "round"
    | Trunc -> "trunc"
    | Ceil -> "ceil"
    | Floor -> "floor"
    | Cvt -> "cvt"
    | C cmpOp -> "c." ^ showCmpOp cmpOp
    | Mov -> "mov"
  type uop =
    | J
    | Jr
    | Mflo | Mfhi | Mtlo | Mthi
    | Bc1t | Bc1f
  let showUop = function
    | J -> "j"
    | Jr -> "jr"
    | Mflo -> "mflo"
    | Mfhi -> "mfhi"
    | Mtlo -> "mtlo"
    | Mthi -> "mthi"
    | Bc1t -> "bc1t"
    | Bc1f -> "bc1f"
  type modifier =
    [ `FD | `FS | `PS
    | `B | `H | `W | `D
    | `C
    | `U
    | `L | `R
    | `AL
    | `G
    | `V | `S32
    ]
  
  let showMod = function
    | `B -> "b"
    | `H -> "h"
    | `W -> "w"
    | `D -> "d"
    | `L -> "l"
    | `R -> "r"
    | `G -> ".g"
    | `AL -> "al"
    | `FD -> ".d"
    | `FS -> ".s"
    | `C -> "c"
    | `PS -> "ps"
    | `S32 -> "32"
    | `U -> "u"
    | `V -> "v"
  type instr =
    | Load of (reg * addr)
    | Store of (reg * addr)
    | TOp of (ternop * reg * reg * reg)
    | TOpI of (ternop * reg * reg * immed)
    | BOp of (binop * reg * reg)
    | BOpI of (binop * reg * immed)
    | UOp of (uop * reg)
    | UOpI of (uop * immed)
    | Special of string
    | Attr of (modifier list * instr)


  exception IllegalInstruction of string
  let showIReg r = sprintf "$%d" r
  let showFReg r = sprintf "$f%d" r
  let showReg l r =
    if List.mem `FS l || List.mem `FD l || List.mem `PS l then showFReg r else
      showIReg r
  let showImmed = function
    | Immed i -> string_of_int i
    | Label s -> s
    | AsmOp(a,b) -> sprintf "%s(%s)" a b
  let showAddr (i, r) = sprintf "%s(%s)" (showImmed i) (showIReg r)
  let showMods l =
    String.concat "" (List.map showMod l)
  let showCvtMods l =
    String.concat "" @$ List.map ((fun s -> if s.[0]='.' then s else "."^s)*@
        (fun m -> if m =`D then ".l" else showMod m)) l
  let showInstr =
    let pre l = if List.mem `D l then "d" else "" in
    function
    | Attr(l, Load(r, a)) ->
        sprintf "l%s %s,%s" (showMods l) (showReg l r) (showAddr a)
    | Attr(l, Store(r, a)) ->
        sprintf "s%s %s,%s" (showMods l) (showReg l r) (showAddr a)
    | Attr(l, TOpI(top, r, r', i)) ->
      let isImmed = begin match top with
        | B _
        | Sll | Sra | Srl -> ""
        | _ -> "i"
        end in 
        pre l ^ sprintf "%s%s%s %s,%s,%s" (showTernOp top) isImmed (showMods (List.filter ((<>) `D) l)) (showReg l r) (showReg l r') (showImmed i)
    | Attr(l, TOp(top, r, r', r'')) ->
        pre l ^ sprintf "%s%s %s,%s,%s" (showTernOp top) (showMods (List.filter ((<>) `D) l)) (showReg l r) (showReg l r') (showReg l r'')
    | Attr(l, BOp((Mtc1|Mfc1|Mtc0|Mfc0) as bop, r, r')) ->
        pre l ^ sprintf "%s %s,%s" (showBinop bop) (showIReg r) (showFReg r')
    | Attr(l, BOp((Cvt|Trunc|Round|Ceil|Floor) as bop, r, r')) ->
        sprintf "%s%s %s,%s" (showBinop bop) (showCvtMods l) (showFReg r) (showFReg r')        
    | Attr(l, BOp(Jalr, r, r')) ->
        sprintf "jalr %s,%s" (showIReg r) (showIReg r')        
    | Attr(l, BOp(bop, r, r')) ->
        pre l ^ sprintf "%s%s %s,%s" (showBinop bop) (showMods (List.filter ((<>) `D) l)) (showReg l r) (showReg l r')
    | Attr(l, BOpI(bop, r, i)) ->
      let isImmed = begin match bop with
        | Bz _ -> ""
        | _ -> "i"
        end in       
        pre l ^ sprintf "%s%s%s %s,%s" (showBinop bop) isImmed (showMods (List.filter ((<>) `D) l)) (showReg l r) (showImmed i)
    | Attr(l, UOp(op, r)) ->
        pre l ^ sprintf "%s%s %s" (showUop op) (showMods (List.filter ((<>) `D) l)) (showReg l r)
    | Attr(l, UOpI(op, i)) ->
        pre l ^ sprintf "%s%s %s" (showUop op) (showMods (List.filter ((<>) `D) l)) (showImmed i)
    | Special s -> s
    | _ -> ""

(*  type preg =                                                       *)
(*    | Hi                                                            *)
(*    | Lo                                                            *)
(*    | FReg of int                                                   *)
(*    | IReg of int                                                   *)
(*  let isFloat l = List.mem `FS l || List.mem `FD l || List.mem `PS l*)
(*  let determine l l' =                                              *)
(*    List.map (fun r -> if isFloat l then FReg r else IReg r) l'     *)
(*  let rec reads = function                                          *)
(*    | Load (_,(_,r')) -> [IReg r']                                  *)
(*    | Attr(l,Store (r,(_,r'))) -> determine l [r] @ [IReg r']       *)
(*    | TOp (_,_,r',r'') -> [r';r'']                                  *)
(*    | TOpI (_,_,r,_) -> [r]                                         *)
(*    | BOp ((Mul|Div|C _),r,r') -> [r;r']                            *)
(*    | BOp ((Mtc0|Mtc1),r,_) -> [r]                                  *)
(*    | BOp (_,_,r') -> [r']                                          *)
(*    | BOpI ((T _|Bz _),r,_) -> [r]                                  *)
(*    | BOpI (Lu,_,_) -> []                                           *)
(*    | UOp(J,_) -> []                                                *)
(*    | UOp(Mfhi,_) -> [Hi]                                           *)
(*    | UOp(Mflo,_) -> [Lo]                                           *)
(*    | Attr(l,i) -> reads i                                          *)
(*    | UOp ->*)
  let parseLoadStore = function
    | "lb", r, i, r' -> Attr([`B], Load(r, (i, r')))
    | "lbu", r, i, r' -> Attr([`B;`U], Load(r, (i, r')))
    | "lh", r, i, r' -> Attr([`H], Load(r, (i, r')))
    | "lhu", r, i, r' -> Attr([`H;`U], Load(r, (i, r')))
    | "lw", r, i, r' -> Attr([`W], Load(r, (i, r')))
    | "lwu", r, i, r' -> Attr([`W;`U], Load(r, (i, r')))
    | "lwl", r, i, r' -> Attr([`W;`L], Load(r, (i, r')))
    | "lwr", r, i, r' -> Attr([`W;`R], Load(r, (i, r')))
    | "ld", r, i, r' -> Attr([`D], Load(r, (i, r')))
    | "ldl", r, i, r' -> Attr([`D;`L], Load(r, (i, r')))
    | "ldr", r, i, r' -> Attr([`D;`R], Load(r, (i, r')))
    | "ll", r, i, r' -> Attr([`L], Load(r, (i, r')))
    | "lld", r, i, r' -> Attr([`L;`D], Load(r, (i, r')))
    | "l.s", r, i, r' -> Attr([`FS], Load(r, (i, r')))
    | "l.d", r, i, r' -> Attr([`FD], Load(r, (i, r')))
    | "lwc1", r, i, r' -> Attr([`FS], Load(r, (i, r')))
    | "ldc1", r, i, r' -> Attr([`FD], Load(r, (i, r')))
    | "sb", r, i, r' -> Attr([`B], Store(r, (i, r')))
    | "sh", r, i, r' -> Attr([`H], Store(r, (i, r')))
    | "sw", r, i, r' -> Attr([`W], Store(r, (i, r')))
    | "swl", r, i, r' -> Attr([`W;`L], Store(r, (i, r')))
    | "swr", r, i, r' -> Attr([`W;`R], Store(r, (i, r')))
    | "sd", r, i, r' -> Attr([`D], Store(r, (i, r')))
    | "sdl", r, i, r' -> Attr([`D;`L], Store(r, (i, r')))
    | "sdr", r, i, r' -> Attr([`D;`R], Store(r, (i, r')))
    | "sc", r, i, r' -> Attr([`C], Store(r, (i, r')))
    | "scd", r, i, r' -> Attr([`C;`D], Store(r, (i, r')))
    | "s.s", r, i, r' -> Attr([`FS], Store(r, (i, r')))
    | "s.d", r, i, r' -> Attr([`FD], Store(r, (i, r')))
    | "swc1", r, i, r' -> Attr([`FS], Store(r, (i, r')))
    | "sdc1", r, i, r' -> Attr([`FD], Store(r, (i, r')))
    | x,r,i,r' -> raise @$ IllegalInstruction (sprintf "%s %d,%s,%d" x r (showImmed i) r')
  let parseTOpI = function
    | "addi", r, r', i -> Attr([], TOpI(Add, r, r', i))
    | "daddi", r, r', i -> Attr([`D], TOpI(Add, r, r', i))
    | "addiu", r, r', i -> Attr([`U], TOpI(Add, r, r', i))
    | "daddiu", r, r', i -> Attr([`D;`U], TOpI(Add, r, r', i))
    | "slti", r, r', i -> Attr([], TOpI(Slt, r, r', i))
    | "sltiu", r, r', i -> Attr([`U], TOpI(Slt, r, r', i))
    | "andi", r, r', i -> Attr([], TOpI(And, r, r', i))
    | "ori", r, r', i -> Attr([], TOpI(Or, r, r', i))
    | "xori", r, r', i -> Attr([], TOpI(Xor, r, r', i))
    | "beq", r, r', i -> Attr ([], TOpI(B `EQ, r, r', i))
    | "bne", r, r', i -> Attr ([], TOpI(B `NE, r, r', i))
    | "beql", r, r', i -> Attr ([`L], TOpI(B `EQ, r, r', i))
    | "bnel", r, r', i -> Attr ([`L], TOpI(B `NE, r, r', i))
    | "sll", r, r', i -> Attr([], TOpI(Sll, r, r', i))
    | "srl", r, r', i -> Attr([], TOpI(Srl, r, r', i))
    | "sra", r, r', i -> Attr([], TOpI(Sra, r, r', i))
    | "dsll", r, r', i -> Attr([`D], TOpI(Sll, r, r', i))
    | "dsrl", r, r', i -> Attr([`D], TOpI(Srl, r, r', i))
    | "dsra", r, r', i -> Attr([`D], TOpI(Sra, r, r', i))
    | "dsll32", r, r', i -> Attr([`D;`S32], TOpI(Sll, r, r', i))
    | "dsrl32", r, r', i -> Attr([`D;`S32], TOpI(Srl, r, r', i))
    | "dsra32", r, r', i -> Attr([`D;`S32], TOpI(Sra, r, r', i))
    | x,r,r',i -> raise @$ IllegalInstruction (sprintf "%s %d,%d,%s" x r r' (showImmed i))
  let parseTOp = function
    | "add", r, r', i -> Attr([], TOp(Add, r, r', i))
    | "dadd", r, r', i -> Attr([`D], TOp(Add, r, r', i))
    | "addu", r, r', i -> Attr([`U], TOp(Add, r, r', i))
    | "daddu", r, r', i -> Attr([`D;`U], TOp(Add, r, r', i))
    | "sub", r, r', i -> Attr([], TOp(Sub, r, r', i))
    | "dsub", r, r', i -> Attr([`D], TOp(Sub, r, r', i))
    | "subu", r, r', i -> Attr([`U], TOp(Sub, r, r', i))
    | "dsubu", r, r', i -> Attr([`D;`U], TOp(Sub, r, r', i))
    | "slt", r, r', i -> Attr([], TOp(Slt, r, r', i))
    | "sltu", r, r', i -> Attr([`U], TOp(Slt, r, r', i))
    | "and", r, r', i -> Attr([], TOp(And, r, r', i))
    | "or", r, r', i -> Attr([], TOp(Or, r, r', i))
    | "xor", r, r', i -> Attr([], TOp(Xor, r, r', i))
    | "nor", r, r', i -> Attr([], TOp(Nor, r, r', i))
    | "mult.g", r, r', i -> Attr([`G], TOp(Mulg, r, r', i))
    | "dmult.g", r, r', i -> Attr([`D;`G], TOp(Mulg, r, r', i))
    | "multu.g", r, r', i -> Attr([`U;`G], TOp(Mulg, r, r', i))
    | "dmultu.g", r, r', i -> Attr([`D;`U;`G], TOp(Mulg, r, r', i))
    | "div.g", r, r', i -> Attr([`G], TOp(Divg, r, r', i))
    | ("div"|"divu"), r, r', i when r = 0 -> Attr([], BOp(Div, r', i))
    | "ddiv.g", r, r', i -> Attr([`D;`G], TOp(Divg, r, r', i))
    | "divu.g", r, r', i -> Attr([`U;`G], TOp(Divg, r, r', i))
    | "ddivu.g", r, r', i -> Attr([`D;`U;`G], TOp(Divg, r, r', i))
    | "mod.g", r, r', i -> Attr([`G], TOp(Modg, r, r', i))
    | "dmod.g", r, r', i -> Attr([`D;`G], TOp(Modg, r, r', i))
    | "modu.g", r, r', i -> Attr([`U;`G], TOp(Modg, r, r', i))
    | "dmodu.g", r, r', i -> Attr([`D;`U;`G], TOp(Modg, r, r', i))
    | "sllv", r, r', i -> Attr([`V], TOp(Sll, r, r', i))
    | "srlv", r, r', i -> Attr([`V], TOp(Srl, r, r', i))
    | "srav", r, r', i -> Attr([`V], TOp(Sra, r, r', i))
    | "dsllv", r, r', i -> Attr([`D;`V], TOp(Sll, r, r', i))
    | "dsrlv", r, r', i -> Attr([`D;`V], TOp(Srl, r, r', i))
    | "dsrav", r, r', i -> Attr([`D;`V], TOp(Sra, r, r', i))
    | "movz", r, r', r'' -> Attr([], TOp(Movz, r, r', r''))
    | "movn", r, r', r'' -> Attr([], TOp(Movn, r, r', r''))
    | "add.s", r, r', r'' -> Attr([`FS], TOp(Add, r, r', r''))
    | "add.d", r, r', r'' -> Attr([`FD], TOp(Add, r, r', r''))
    | "add.ps", r, r', r'' -> Attr([`PS], TOp(Add, r, r', r''))
    | "sub.s", r, r', r'' -> Attr([`FS], TOp(Sub, r, r', r''))
    | "sub.d", r, r', r'' -> Attr([`FD], TOp(Sub, r, r', r''))
    | "sub.ps", r, r', r'' -> Attr([`PS], TOp(Sub, r, r', r''))
    | "mul.s", r, r', r'' -> Attr([`FS], TOp(FMul, r, r', r''))
    | "mul.d", r, r', r'' -> Attr([`FD], TOp(FMul, r, r', r''))
    | "mul.ps", r, r', r'' -> Attr([`PS], TOp(FMul, r, r', r''))
    | "div.s", r, r', r'' -> Attr([`FS], TOp(FDiv, r, r', r''))
    | "div.d", r, r', r'' -> Attr([`FD], TOp(FDiv, r, r', r''))
    | "div.ps", r, r', r'' -> Attr([`PS], TOp(FDiv, r, r', r''))
    | "madd.s", r, r', r'' -> Attr([`FS], TOp(Madd, r, r', r''))
    | "madd.d", r, r', r'' -> Attr([`FD], TOp(Madd, r, r', r''))
    | "madd.ps", r, r', r'' -> Attr([`PS], TOp(Madd, r, r', r''))
    | "msub.s", r, r', r'' -> Attr([`FS], TOp(Msub, r, r', r''))
    | "msub.d", r, r', r'' -> Attr([`FD], TOp(Msub, r, r', r''))
    | "msub.ps", r, r', r'' -> Attr([`PS], TOp(Msub, r, r', r''))
    | "nmadd.s", r, r', r'' -> Attr([`FS], TOp(Nmadd, r, r', r''))
    | "nmadd.d", r, r', r'' -> Attr([`FD], TOp(Nmadd, r, r', r''))
    | "nmadd.ps", r, r', r'' -> Attr([`PS], TOp(Nmadd, r, r', r''))
    | "nmsub.s", r, r', r'' -> Attr([`FS], TOp(Nmsub, r, r', r''))
    | "nmsub.d", r, r', r'' -> Attr([`FD], TOp(Nmsub, r, r', r''))
    | "nmsub.ps", r, r', r'' -> Attr([`PS], TOp(Nmsub, r, r', r''))
    | x,r,r',r'' -> raise @$ IllegalInstruction (sprintf "%s %d,%d,%d" x r r' r'')
  let parseBOpI = function
    | "lui", r, i -> Attr([], BOpI(Lu, r, i))
    | "tgei", r, r' -> Attr([], BOpI(T `GE, r, r'))
    | "tgeiu", r, r' -> Attr([`U], BOpI(T `GE, r, r'))
    | "tlti", r, r' -> Attr([], BOpI(T `LT, r, r'))
    | "tltiu", r, r' -> Attr([`U], BOpI(T `LT, r, r'))
    | "teqi", r, r' -> Attr([], BOpI(T `EQ, r, r'))
    | "tnei", r, r' -> Attr([], BOpI(T `NE, r, r'))
    | "blez", r, i -> Attr ([], BOpI(Bz `LE, r, i))
    | "bgtz", r, i -> Attr ([], BOpI(Bz `GT, r, i))
    | "bltz", r, i -> Attr ([], BOpI(Bz `LT, r, i))
    | "bgez", r, i -> Attr ([], BOpI(Bz `GE, r, i))
    | "bltzal", r, i -> Attr ([`AL], BOpI(Bz `LT, r, i))
    | "bgezal", r, i -> Attr ([`AL], BOpI(Bz `GE, r, i))
    | "blezl", r, i -> Attr ([`L], BOpI(Bz `LE, r, i))
    | "bgtzl", r, i -> Attr ([`L], BOpI(Bz `GT, r, i))
    | "bltzl", r, i -> Attr ([`L], BOpI(Bz `LT, r, i))
    | "bgezl", r, i -> Attr ([`L], BOpI(Bz `GE, r, i))
    | "bltzall", r, i -> Attr ([`AL;`L], BOpI(Bz `LT, r, i))
    | "bgezall", r, i -> Attr ([`AL;`L], BOpI(Bz `GE, r, i))
    | x,r,i -> raise @$ IllegalInstruction (sprintf "%s %d,%s" x r (showImmed i))
  let parseBOp = function
    | "mult", r, r' -> Attr([], BOp(Mul, r, r'))
    | "dmult", r, r' -> Attr([`D], BOp(Mul, r, r'))
    | "multu", r, r' -> Attr([`U], BOp(Mul, r, r'))
    | "dmultu", r, r' -> Attr([`D;`U], BOp(Mul, r, r'))
    | "div", r, r' -> Attr([], BOp(Div, r, r'))
    | "ddiv", r, r' -> Attr([`D], BOp(Div, r, r'))
    | "divu", r, r' -> Attr([`U], BOp(Div, r, r'))
    | "ddivu", r, r' -> Attr([`D;`U], BOp(Div, r, r'))
    | "jalr", r, r' -> Attr([`AL], BOp(Jalr, r, r'))
    | "tge", r, r' -> Attr([], BOp(T `GE, r, r'))
    | "tgeu", r, r' -> Attr([`U], BOp(T `GE, r, r'))
    | "tlt", r, r' -> Attr([], BOp(T `LT, r, r'))
    | "tltu", r, r' -> Attr([`U], BOp(T `LT, r, r'))
    | "teq", r, r' -> Attr([], BOp(T `EQ, r, r'))
    | "tne", r, r' -> Attr([], BOp(T `NE, r, r'))
    | "mfc0", r, r' -> Attr([], BOp(Mfc0, r, r'))
    | "dmfc0", r, r' -> Attr([`D], BOp(Mfc0, r, r'))
    | "mtc0", r, r' -> Attr([], BOp(Mtc0, r, r'))
    | "dmtc0", r, r' -> Attr([`D], BOp(Mtc0, r, r'))
    | "mfc1", r, r' -> Attr([`FS], BOp(Mfc1, r, r'))
    | "dmfc1", r, r' -> Attr([`D;`FD], BOp(Mfc1, r, r'))
    | "mtc1", r, r' -> Attr([`FS], BOp(Mtc1, r, r'))
    | "dmtc1", r, r' -> Attr([`D;`FD], BOp(Mtc1, r, r'))
    | "sqrt.s", r, r' -> Attr([`FS], BOp(Sqrt, r, r'))
    | "sqrt.d", r, r' -> Attr([`FD], BOp(Sqrt, r, r'))
    | "neg.s", r, r' -> Attr([`FS], BOp(Neg, r, r'))
    | "neg.d", r, r' -> Attr([`FD], BOp(Neg, r, r'))
    | "neg.ps", r, r' -> Attr([`PS], BOp(Neg, r, r'))
    | "abs.s", r, r' -> Attr([`FS], BOp(Abs, r, r'))
    | "abs.d", r, r' -> Attr([`FD], BOp(Abs, r, r'))
    | "abs.ps", r, r' -> Attr([`PS], BOp(Abs, r, r'))
    | "round.l.s", r, r' -> Attr([`L;`FS], BOp(Round, r, r'))
    | "round.l.d", r, r' -> Attr([`L;`FD], BOp(Round, r, r'))
    | "round.w.s", r, r' -> Attr([`W;`FS], BOp(Round, r, r'))
    | "round.w.d", r, r' -> Attr([`W;`FD], BOp(Round, r, r'))
    | "trunc.l.s", r, r' -> Attr([`L;`FS], BOp(Trunc, r, r'))
    | "trunc.l.d", r, r' -> Attr([`L;`FD], BOp(Trunc, r, r'))
    | "trunc.w.s", r, r' -> Attr([`W;`FS], BOp(Trunc, r, r'))
    | "trunc.w.d", r, r' -> Attr([`W;`FD], BOp(Trunc, r, r'))
    | "ceil.l.s", r, r' -> Attr([`L;`FS], BOp(Ceil, r, r'))
    | "ceil.l.d", r, r' -> Attr([`L;`FD], BOp(Ceil, r, r'))
    | "ceil.w.s", r, r' -> Attr([`W;`FS], BOp(Ceil, r, r'))
    | "ceil.w.d", r, r' -> Attr([`W;`FD], BOp(Ceil, r, r'))
    | "floor.l.s", r, r' -> Attr([`L;`FS], BOp(Floor, r, r'))
    | "floor.l.d", r, r' -> Attr([`L;`FD], BOp(Floor, r, r'))
    | "floor.w.s", r, r' -> Attr([`W;`FS], BOp(Floor, r, r'))
    | "floor.w.d", r, r' -> Attr([`W;`FD], BOp(Floor, r, r'))
    | "cvt.s.l", r, r' -> Attr([`FS;`D], BOp(Cvt, r, r'))
    | "cvt.s.w", r, r' -> Attr([`FS;`W], BOp(Cvt, r, r'))
    | "cvt.s.d", r, r' -> Attr([`FS;`FD], BOp(Cvt, r, r'))
    | "cvt.d.l", r, r' -> Attr([`FD;`D], BOp(Cvt, r, r'))
    | "cvt.d.w", r, r' -> Attr([`FD;`W], BOp(Cvt, r, r'))
    | "cvt.d.s", r, r' -> Attr([`FD;`FS], BOp(Cvt, r, r'))
    | "cvt.l.s", r, r' -> Attr([`D;`FS], BOp(Cvt, r, r'))
    | "cvt.l.d", r, r' -> Attr([`D;`FD], BOp(Cvt, r, r'))
    | "cvt.w.s", r, r' -> Attr([`W;`FS], BOp(Cvt, r, r'))
    | "cvt.w.d", r, r' -> Attr([`W;`FD], BOp(Cvt, r, r'))
    | "c.f.s", r, r' -> Attr([`FS], BOp(C `F, r, r'))
    | "c.f.d", r, r' -> Attr([`FD], BOp(C `F, r, r'))
    | "c.f.ps", r, r' -> Attr([`PS], BOp(C `F, r, r'))
    | "c.sf.s", r, r' -> Attr([`FS], BOp(C `SF, r, r'))
    | "c.sf.d", r, r' -> Attr([`FD], BOp(C `SF, r, r'))
    | "c.sf.ps", r, r' -> Attr([`PS], BOp(C `SF, r, r'))
    | "c.un.s", r, r' -> Attr([`FS], BOp(C `UN, r, r'))
    | "c.un.d", r, r' -> Attr([`FD], BOp(C `UN, r, r'))
    | "c.un.ps", r, r' -> Attr([`PS], BOp(C `UN, r, r'))
    | "c.ngle.s", r, r' -> Attr([`FS], BOp(C `NGLE, r, r'))
    | "c.ngle.d", r, r' -> Attr([`FD], BOp(C `NGLE, r, r'))
    | "c.ngle.ps", r, r' -> Attr([`PS], BOp(C `NGLE, r, r'))
    | "c.eq.s", r, r' -> Attr([`FS], BOp(C `EQ, r, r'))
    | "c.eq.d", r, r' -> Attr([`FD], BOp(C `EQ, r, r'))
    | "c.eq.ps", r, r' -> Attr([`PS], BOp(C `EQ, r, r'))
    | "c.seq.s", r, r' -> Attr([`FS], BOp(C `SEQ, r, r'))
    | "c.seq.d", r, r' -> Attr([`FD], BOp(C `SEQ, r, r'))
    | "c.seq.ps", r, r' -> Attr([`PS], BOp(C `SEQ, r, r'))
    | "c.ueq.s", r, r' -> Attr([`FS], BOp(C `UEQ, r, r'))
    | "c.ueq.d", r, r' -> Attr([`FD], BOp(C `UEQ, r, r'))
    | "c.ueq.ps", r, r' -> Attr([`PS], BOp(C `UEQ, r, r'))
    | "c.ngl.s", r, r' -> Attr([`FS], BOp(C `NGL, r, r'))
    | "c.ngl.d", r, r' -> Attr([`FD], BOp(C `NGL, r, r'))
    | "c.ngl.ps", r, r' -> Attr([`PS], BOp(C `NGL, r, r'))
    | "c.olt.s", r, r' -> Attr([`FS], BOp(C `OLT, r, r'))
    | "c.olt.d", r, r' -> Attr([`FD], BOp(C `OLT, r, r'))
    | "c.olt.ps", r, r' -> Attr([`PS], BOp(C `OLT, r, r'))
    | "c.lt.s", r, r' -> Attr([`FS], BOp(C `LT, r, r'))
    | "c.lt.d", r, r' -> Attr([`FD], BOp(C `LT, r, r'))
    | "c.lt.ps", r, r' -> Attr([`PS], BOp(C `LT, r, r'))
    | "c.ult.s", r, r' -> Attr([`FS], BOp(C `ULT, r, r'))
    | "c.ult.d", r, r' -> Attr([`FD], BOp(C `ULT, r, r'))
    | "c.ult.ps", r, r' -> Attr([`PS], BOp(C `ULT, r, r'))
    | "c.nge.s", r, r' -> Attr([`FS], BOp(C `NGE, r, r'))
    | "c.nge.d", r, r' -> Attr([`FD], BOp(C `NGE, r, r'))
    | "c.nge.ps", r, r' -> Attr([`PS], BOp(C `NGE, r, r'))
    | "c.ole.s", r, r' -> Attr([`FS], BOp(C `OLE, r, r'))
    | "c.ole.d", r, r' -> Attr([`FD], BOp(C `OLE, r, r'))
    | "c.ole.ps", r, r' -> Attr([`PS], BOp(C `OLE, r, r'))
    | "c.le.s", r, r' -> Attr([`FS], BOp(C `LE, r, r'))
    | "c.le.d", r, r' -> Attr([`FD], BOp(C `LE, r, r'))
    | "c.le.ps", r, r' -> Attr([`PS], BOp(C `LE, r, r'))
    | "c.ule.s", r, r' -> Attr([`FS], BOp(C `ULE, r, r'))
    | "c.ule.d", r, r' -> Attr([`FD], BOp(C `ULE, r, r'))
    | "c.ule.ps", r, r' -> Attr([`PS], BOp(C `ULE, r, r'))
    | "c.ngt.s", r, r' -> Attr([`FS], BOp(C `NGT, r, r'))
    | "c.ngt.d", r, r' -> Attr([`FD], BOp(C `NGT, r, r'))
    | "c.ngt.ps", r, r' -> Attr([`PS], BOp(C `NGT, r, r'))
    | "mov.s", r, r' -> Attr([`FS], BOp(Mov, r, r'))
    | "mov.d", r, r' -> Attr([`FD], BOp(Mov, r, r'))
    | "mov.ps", r, r' -> Attr([`PS], BOp(Mov, r, r'))
    | x,r,r' -> raise @$ IllegalInstruction (sprintf "%s %d,%d" x r r')
  let parseUOp = function
    | "mfhi", r -> Attr([], UOp(Mfhi, r))
    | "mthi", r -> Attr([], UOp(Mthi, r))
    | "mflo", r -> Attr([], UOp(Mflo, r))
    | "mtlo", r -> Attr([], UOp(Mtlo, r))
    | "jr", r -> Attr([], UOp(Jr, r))
    | x,r -> raise @$ IllegalInstruction (sprintf "%s %d" x r )
  let parseUOpI = function
    | "j", i -> Attr([], UOpI(J, i))
    | "jal", i -> Attr([`AL], UOpI(J, i))
    | "bc1f", i -> Attr([], UOpI(Bc1f, i))
    | "bc1t", i -> Attr([], UOpI(Bc1t, i))
    | "bc1fl", i -> Attr([`L], UOpI(Bc1f, i))
    | "bc1tl", i -> Attr([`L], UOpI(Bc1t, i))
    | x,i -> raise @$ IllegalInstruction (sprintf "%s %s" x (showImmed i))
  let parseMisc = function
    | "sync" -> Special "sync"
    | "syscall" -> Special "syscall"
    | "break" -> Special "break"
    | "nop" -> Special "nop"
    | x -> raise @$ IllegalInstruction (x)
end

module type LEXER = sig
  type token
end

module AsmLexer = struct
  type token = string
  open ExtString
  let tokenize s =
    lines s |>
    List.filter (not *@ (fun s ->
              isBlankString s ||
              startsWith s "#" ||
              startsWith s ".loc"
        ) *@ strip )
    |> List.map (List.hd *@ flip split "#")
end
module InstrLexer = struct
  open ExtChar
  open ExtString
  type token = string
  let tokenize s =
    let keywords = ['$';'(';')';',';' ';'-';'\t'] in
    List.filter (not*@isBlankString) @$ List.map of_list @$ 
        ExtList.isolate (flip List.mem keywords) (flip List.mem keywords) @$
            to_list s
end

module Parse(L: LEXER) = struct
  open Either
  open EitherMonad
  type t = L.token list ->
    (L.token list , L.token list * L.token list) Either.t
  let fmap f t =
    fun s ->
        fmap (fun (a, b) -> (f a, b)) (t s)
  let (<|>) p p' s =
    match p s with
    | Left _ -> p' s
    | x -> x
  let mplus = (<|>)
  let mzero x = return ([], x)
  let zeroOrOne p =
    mzero <|> (fun s -> p s >>= fun (a', s') -> return ([a'], s'))
  let rec zeroOrMore p = (oneOrMore p) <|> mzero
  and oneOrMore p s =
    p s >>= fun (a, s') ->
        zeroOrMore p s' >>= fun (a', s'') ->
            return (a:: a', s'')
  let bind p p' =
    fun s ->
        p s >>= fun (_, s') ->
            p' s'
  let drop p = bind p mzero
  let pSat f = function
    | [] -> fail []
    | x:: xs -> if f x then return (x, xs) else fail (x:: xs)
  let pTakeWhile f l =
    let l',l'' = ExtList.span f l in 
    return (l',l'')
  let pDropWhile f l = pTakeWhile (not*@f) l
  let pLit l s = pSat ((=) l) s
  let pAny l s = pSat (flip List.mem l) s
  let pFirst = function
    | [] -> fail []
    | x::xs -> return (x,xs)
    
  let rec oneOrMoreWith sep p s =
    p s >>= fun (a, s') ->
        pLit sep s' >>= fun(_, s'') ->
            zeroOrMoreWith sep p s'' >>= fun (a', s''') ->
                return (a:: a', s''')
  and zeroOrMoreWith sep p = oneOrMoreWith sep p <|> mzero
  let pThen f p p' s =
    p s >>= fun (a,s') ->
      p' s' >>= fun (a',s'') ->
        return (f a a',s'')
  let pThen3 f p p' p'' s =
    p s >>= fun (a,s') ->
      p' s' >>= fun (a',s'') ->
        p'' s'' >>= fun (a'',s''') ->
          return (f a a' a'',s''')
  let pThen4 f p p' p'' p''' s =
    p s >>= fun (a,s') ->
      p' s' >>= fun (a',s'') ->
        p'' s'' >>= fun (a'',s''') ->
          p''' s''' >>= fun (a''',s'''') ->
            return (f a a' a'' a''',s'''')
  let rec sequence ms s = match ms with
    | [] -> return([], s)
    | m:: ms' ->
        m s >>= fun (a, s') ->
            sequence ms' s' >>= fun (as', s'') ->
                return (a:: as', s'')
  open ExtString
  let pDigit = pSat (ExtChar.isDigit *@hd)
  let pPositive = fmap (String.concat "") @$ oneOrMore pDigit
  let pNumber =
    pThen (fun _ s -> neg @$ int_of_string s) (pLit "-") pPositive
    <|> fmap int_of_string pPositive
end

module MipsInstrParse = struct
  module P = Parse(InstrLexer)
  open P
  open ExtString
  let pImmed =
    pThen3 (fun a b _ -> Mips.AsmOp(a, String.concat "" b))
      (pAny ["%got_disp";"%gp_rel";"%hi";"%lo";"%call16"])
      (P.bind (pLit "(") (pTakeWhile ((<>) ")"))) (pLit ")")
    <|> P.fmap (fun x -> Mips.Immed x) pNumber
    <|> pThen (fun a b -> Mips.Label(a^b)) (pLit "$")
      (pSat (fun s -> Logic.either (flip List.mem ['_';'.']) ExtChar.isAlpha (ExtString.hd s)))
    <|> P.fmap (fun x -> Mips.Label x) 
        (pSat (fun s -> Logic.either (flip List.mem ['_';'.']) ExtChar.isAlpha (ExtString.hd s))
      )
  let pReg =
    P.bind (pLit "$")
      (pNumber
        <|> P.fmap (int_of_string *@tl) @$ pSat (fun s -> hd s ='f')
        <|> P.fmap Mips.numNamedReg @$ 
            pSat (fun s -> try ignore @$ Mips.numNamedReg s;true with _ -> false) )
  let pOp = pFirst
  let pInstr =
    pThen4 (curry4 Mips.parseLoadStore)
      pOp pReg (P.bind (pLit ",") pImmed) (P.bind (pLit "(") pReg)
    <|> pThen4 (curry4 Mips.parseTOp)
      pOp pReg (P.bind (pLit ",") pReg) (P.bind (pLit ",") pReg)
    <|> pThen4 (curry4 Mips.parseTOpI)
      pOp pReg (P.bind (pLit ",") pReg) (P.bind (pLit ",") pImmed)
    <|> pThen3 (curry3 Mips.parseBOp) pOp pReg (P.bind (pLit ",") pReg)
    <|> pThen3 (curry3 Mips.parseBOpI) pOp pReg (P.bind (pLit ",") pImmed)
    <|> pThen (curry Mips.parseUOpI) pOp pImmed
    <|> pThen (curry Mips.parseUOp) pOp pReg
    <|> P.fmap Mips.parseMisc pOp
  open Either
  let parse l = match pInstr l with
    | Left l -> failwith @$ sprintf "Left %s\n" @$ show_stringlist l
    | Right(i,rest) -> i
end
(*module MipsAsm = struct                                                         *)
(*  module P = Parse(AsmLexer)                                                    *)
(*  open P                                                                        *)
(*  open ExtString                                                                *)
(*  type file = directive list * section list                                     *)
(*  and directive = string                                                        *)
(*  and section = directive list * programUnit list                               *)
(*  and programUnit = directive list * (basicBlock) list * directive list         *)
(*  and basicBlock = line list                                                    *)
(*  and line = Dir of directive | Instr of Mips.instr                             *)
(*  and label = Mips.immed                                                        *)
(*  let pToken s = pSat (flip startsWith s)                                       *)
(*  let pDirective = pFirst                                                       *)
(*  let pProgramUnit =                                                            *)
(*    pThen3 (fun a b c -> a,b,c)                                                 *)
(*        (zeroOrMore pDirective) (zeroOrMore pBasicBlock) (zeroOrMore pDirective)*)
(*  let pSection =                                                                *)
(*    pThen3 (fun a b c -> a::b,c)                                                *)
(*        (pToken ".section") (zeroOrMore pDirective) (zeroOrMore pProgramUnit)   *)
(*  let pFile = pThen pair (zeroOrMore pDirective) (zeroOrMore pSection)          *)
(*  let showLable = Mips.showImmed                                                *)
(*  let showDirective = id                                                        *)
(*  let showLine = function                                                       *)
(*    | Dir d -> showDirective d                                                  *)
(*    | Instr i -> Mips.showInstr i                                               *)
(*  let showBasicBlock l = show_list ~sep:"\n" showLine l                         *)
(*  let showProgramUnit (l,l',l'') =                                              *)
(*    show_list ~sep:"\n" showDirective l ^                                       *)
(*    (show_list ~sep:"\n" showBasicBlock @$ List.map snd l') ^                   *)
(*    show_list ~sep:"\n" showDirective l''                                       *)
(*  let showSection (l,l') =                                                      *)
(*    show_list ~sep:"\n" showDirective l ^                                       *)
(*    show_list ~sep:"\n" showProgramUnit l'                                      *)
(*  let showFile (l,l') =                                                         *)
(*    show_list ~sep:"\n" showDirective l ^                                       *)
(*    show_list ~sep:"\n" showSection l'                                          *)
(*end                                                                             *)
(*module MipsAsmParse = struct            *)
(*  module P = Parse(AsmLexer)            *)
(*  open P                                *)
(*  open ExtString                        *)
(*  let parseFile =                       *)
(*       P.fmap pSat (flip startsWith ".")*)
(*end                                     *)

(*open MipsInstrParse*)


(*open Either                                                             *)
(*let () =                                                                *)
(*  let l' = ["daddu $3,$1,$2"                                            *)
(*    ;"daddiu $3,$1,-2"                                                  *)
(*    ;"mult $2, $3"                                                      *)
(*    ;"lbu $0,-23($sp)"                                                  *)
(*    ;"lbu $0,%got_disp($Lt41)($sp)"                                     *)
(*    ;"c.lt.d $f2,$f0"                                                   *)
(*    ;"bc1f $Lt_14_1024"                                                 *)
(*    ;"ldc1 $f15,%gp_rel(kons_)($gp)"                                    *)
(*    ] in                                                                *)
(*  let l'' = AsmLexer.tokenize @$ ExtUnix.readAllFile Sys.argv.(1) in    *)
(*  List.iter (fun str ->                                                 *)
(*		let tokens = InstrLexer.tokenize str in                             *)
(*		printf "tokens: %s\n" @$ show_stringlist tokens;                    *)
(*		let r = MipsInstrParse.pInstr tokens in                              *)
(*    match r with                                                        *)
(*      | Left l -> printf "Left %s\n" @$ show_stringlist l               *)
(*      | Right(i,rest) ->                                                *)
(*        printf "Right %s\n" (Mips.showInstr i)                          *)
(*    ) l'                                                                *)