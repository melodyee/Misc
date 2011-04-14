open Printf

module Mips = struct
  type reg = int
  type immed = int
  type addr = immed * reg  
  type cmpOp =
    [ `EQ | `NE | `LE | `LT | `GE | `GT |
			`F  | `UN | `UEQ | `OLT | `ULT | `OLE | `ULE | `SF |
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
    | Sll | Srl | Sra
    | B of cmpOp | Bz of cmpOp
		| Madd | Msub | Nmadd | Nmsub

  let showTernOp = function
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Nor -> "nor"
    | Slt -> "slt"
    | Add -> "add"
    | Sub -> "sub"
    | Mulg -> "mult"
    | Divg -> "div"
    | Modg -> "mod"
    | Sll -> "sll"
    | Sra -> "sra"
    | Srl -> "srl"
    | B c -> "b" ^ showCmpOp c
    | Bz c -> "b" ^ showCmpOp c ^ "z"
		| Madd -> "madd"
		| Msub -> "msub"
		| Nmadd -> "nmadd"
		| Nmsub -> "nmsub"
  type binop =
    | Mul | Div
    | Lui
    | Mfc0 | Mtc0 | Mfc1 | Mtc1
		| Jalr
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
		| Lui -> "lui"
    | Mfc0 -> "mfc0"
    | Mtc0 -> "mtc0"
		| Mfc1 -> "mfc1"
		| Mtc1 -> "mtc1"
		| Jalr -> "jalr"
		| T c -> "t" ^ showCmpOp c
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
    | Mflo |Mfhi | Mtlo | Mthi
    | Bc1t | Bc1f

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
    | `G -> "g"
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
	exception IllegalInstruction
	let showIReg r = sprintf "$%d" r
	let showFReg r = sprintf "$f%d" r	
	let showReg l r =
		if List.mem `FS l || List.mem `FD l || List.mem `PS l then showFReg r else
			showIReg r    
	let showAddr (i,r) = sprintf "%d(%s)" i (showIReg r)
	let showMods l = 
		String.concat "" (List.map showMod l)
	
	let showInstr = function
		| Attr(l,Load(r,a)) ->
			sprintf "l%s %s,%s" (showMods l) (showReg l r) (showAddr a)
		| Attr(l,Store(r,a)) ->
			sprintf "s%s %s,%s" (showMods l) (showReg l r) (showAddr a)
		| Attr(l,TOpI(top,r,r',i)) ->
			let pre = if List.mem `D l then "d" else "" in 
			pre ^ sprintf "%s %s,%s,%d" (showMods (List.filter ((<>) `D) l)) (showReg l r) (showReg l r') i
	let parseLoadStore = function
		| "lb",r,i,r' -> Attr([`B],Load(r,(i,r')))
		| "lbu",r,i,r' -> Attr([`B;`U],Load(r,(i,r')))
		| "lh",r,i,r' -> Attr([`H],Load(r,(i,r')))
		| "lhu",r,i,r' -> Attr([`H;`U],Load(r,(i,r')))
		| "lw",r,i,r' -> Attr([`W],Load(r,(i,r')))
		| "lwu",r,i,r' -> Attr([`W;`U],Load(r,(i,r')))
		| "lwl",r,i,r' -> Attr([`W;`L],Load(r,(i,r')))
		| "lwr",r,i,r' -> Attr([`W;`R],Load(r,(i,r')))
		| "ld",r,i,r' -> Attr([`D],Load(r,(i,r')))
		| "ldl",r,i,r' -> Attr([`D;`L],Load(r,(i,r')))
		| "ldr",r,i,r' -> Attr([`D;`R],Load(r,(i,r')))
		| "ll",r,i,r' -> Attr([`L],Load(r,(i,r')))
		| "lld",r,i,r' -> Attr([`L;`D],Load(r,(i,r')))
		| "l.s",r,i,r' -> Attr([`FS],Load(r,(i,r')))
		| "l.d",r,i,r' -> Attr([`FD],Load(r,(i,r')))
		| "lwc1",r,i,r' -> Attr([`FS],Load(r,(i,r')))
		| "ldc1",r,i,r' -> Attr([`FD],Load(r,(i,r')))
		| "sb",r,i,r' -> Attr([`B],Store(r,(i,r')))
		| "sh",r,i,r' -> Attr([`H],Store(r,(i,r')))
		| "sw",r,i,r' -> Attr([`W],Store(r,(i,r')))
		| "swl",r,i,r' -> Attr([`W;`L],Store(r,(i,r')))
		| "swr",r,i,r' -> Attr([`W;`R],Store(r,(i,r')))
		| "sd",r,i,r' -> Attr([`D],Store(r,(i,r')))
		| "sdl",r,i,r' -> Attr([`D;`L],Store(r,(i,r')))
		| "sdr",r,i,r' -> Attr([`D;`R],Store(r,(i,r')))
		| "sc",r,i,r' -> Attr([`C],Store(r,(i,r')))
		| "scd",r,i,r' -> Attr([`C;`D],Store(r,(i,r')))
		| "s.s",r,i,r' -> Attr([`FS],Store(r,(i,r')))
		| "s.d",r,i,r' -> Attr([`FD],Store(r,(i,r')))
		| "swc1",r,i,r' -> Attr([`FS],Store(r,(i,r')))
		| "sdc1",r,i,r' -> Attr([`FD],Store(r,(i,r')))
		| _ -> raise IllegalInstruction
	let parseTOpI = function
		| "addi",r,r',i -> Attr([],TOpI(Add,r,r',i))
		| "daddi",r,r',i -> Attr([`D],TOpI(Add,r,r',i))
		| "addiu",r,r',i -> Attr([`U],TOpI(Add,r,r',i))
		| "daddiu",r,r',i -> Attr([`D;`U],TOpI(Add,r,r',i))
		| "slti",r,r',i -> Attr([],TOpI(Slt,r,r',i))
		| "sltiu",r,r',i -> Attr([`U],TOpI(Slt,r,r',i))
		| "andi",r,r',i -> Attr([],TOpI(And,r,r',i))
		| "ori",r,r',i -> Attr([],TOpI(Or,r,r',i))
		| "xori",r,r',i -> Attr([],TOpI(Xor,r,r',i))
		| "beq",r,r',i -> Attr ([],TOpI(B `EQ,r,r',i))
		| "bne",r,r',i -> Attr ([],TOpI(B `NE,r,r',i))
		| "blez",r,r',i -> Attr ([],TOpI(Bz `LE,r,r',i))
		| "bgtz",r,r',i -> Attr ([],TOpI(Bz `GT,r,r',i))
		| "bltz",r,r',i -> Attr ([],TOpI(Bz `LT,r,r',i))
		| "bgez",r,r',i -> Attr ([],TOpI(Bz `GE,r,r',i))
		| "bltzal",r,r',i -> Attr ([`AL],TOpI(Bz `LT,r,r',i))
		| "bgezal",r,r',i -> Attr ([`AL],TOpI(Bz `GE,r,r',i))
		| "beql",r,r',i -> Attr ([`L],TOpI(B `EQ,r,r',i))
		| "bnel",r,r',i -> Attr ([`L],TOpI(B `NE,r,r',i))
		| "blezl",r,r',i -> Attr ([`L],TOpI(Bz `LE,r,r',i))
		| "bgtzl",r,r',i -> Attr ([`L],TOpI(Bz `GT,r,r',i))
		| "bltzl",r,r',i -> Attr ([`L],TOpI(Bz `LT,r,r',i))
		| "bgezl",r,r',i -> Attr ([`L],TOpI(Bz `GE,r,r',i))
		| "bltzall",r,r',i -> Attr ([`AL;`L],TOpI(Bz `LT,r,r',i))
		| "bgezall",r,r',i -> Attr ([`AL;`L],TOpI(Bz `GE,r,r',i))
		| "sll",r,r',i -> Attr([],TOpI(Sll,r,r',i))
		| "srl",r,r',i -> Attr([],TOpI(Srl,r,r',i))
		| "sra",r,r',i -> Attr([],TOpI(Sra,r,r',i))
		| "dsll",r,r',i -> Attr([`D],TOpI(Sll,r,r',i))
		| "dsrl",r,r',i -> Attr([`D],TOpI(Srl,r,r',i))
		| "dsra",r,r',i -> Attr([`D],TOpI(Sra,r,r',i))
		| "dsll32",r,r',i -> Attr([`D;`S32],TOpI(Sll,r,r',i))
		| "dsrl32",r,r',i -> Attr([`D;`S32],TOpI(Srl,r,r',i))
		| "dsra32",r,r',i -> Attr([`D;`S32],TOpI(Sra,r,r',i))
		| _ -> raise IllegalInstruction
	let parseTOp = function
		| "add",r,r',i -> Attr([],TOp(Add,r,r',i))
		| "dadd",r,r',i -> Attr([`D],TOp(Add,r,r',i))
		| "addu",r,r',i -> Attr([`U],TOp(Add,r,r',i))
		| "daddu",r,r',i -> Attr([`D;`U],TOp(Add,r,r',i))
		| "sub",r,r',i -> Attr([],TOp(Sub,r,r',i))
		| "dsub",r,r',i -> Attr([`D],TOp(Sub,r,r',i))
		| "subu",r,r',i -> Attr([`U],TOp(Sub,r,r',i))
		| "dsubu",r,r',i -> Attr([`D;`U],TOp(Sub,r,r',i))
		| "slt",r,r',i -> Attr([],TOp(Slt,r,r',i))
		| "sltu",r,r',i -> Attr([`U],TOp(Slt,r,r',i))
		| "and",r,r',i -> Attr([],TOp(And,r,r',i))
		| "or",r,r',i -> Attr([],TOp(Or,r,r',i))
		| "xor",r,r',i -> Attr([],TOp(Xor,r,r',i))
		| "nor",r,r',i -> Attr([],TOp(Nor,r,r',i))
		| "multg",r,r',i -> Attr([],TOp(Mulg,r,r',i))
		| "dmultg",r,r',i -> Attr([`D],TOp(Mulg,r,r',i))
		| "multug",r,r',i -> Attr([`U],TOp(Mulg,r,r',i))
		| "dmultug",r,r',i -> Attr([`D;`U],TOp(Mulg,r,r',i))
		| "divg",r,r',i -> Attr([],TOp(Divg,r,r',i))
		| "ddivg",r,r',i -> Attr([`D],TOp(Divg,r,r',i))
		| "divug",r,r',i -> Attr([`U],TOp(Divg,r,r',i))
		| "ddivug",r,r',i -> Attr([`D;`U],TOp(Divg,r,r',i))
		| "modg",r,r',i -> Attr([],TOp(Modg,r,r',i))
		| "dmodg",r,r',i -> Attr([`D],TOp(Modg,r,r',i))
		| "modug",r,r',i -> Attr([`U],TOp(Modg,r,r',i))
		| "dmodug",r,r',i -> Attr([`D;`U],TOp(Modg,r,r',i))
		| "sllv",r,r',i -> Attr([`V],TOp(Sll,r,r',i))
		| "srlv",r,r',i -> Attr([`V],TOp(Srl,r,r',i))
		| "srav",r,r',i -> Attr([`V],TOp(Sra,r,r',i))
		| "dsllv",r,r',i -> Attr([`D;`V],TOp(Sll,r,r',i))
		| "dsrlv",r,r',i -> Attr([`D;`V],TOp(Srl,r,r',i))
		| "dsrav",r,r',i -> Attr([`D;`V],TOp(Sra,r,r',i))
		| "add.s",r,r',r'' -> Attr([`FS],TOp(Add,r,r',r''))
		| "add.d",r,r',r'' -> Attr([`FD],TOp(Add,r,r',r''))
		| "add.ps",r,r',r'' -> Attr([`PS],TOp(Add,r,r',r''))
		| "sub.s",r,r',r'' -> Attr([`FS],TOp(Sub,r,r',r''))
		| "sub.d",r,r',r'' -> Attr([`FD],TOp(Sub,r,r',r''))
		| "sub.ps",r,r',r'' -> Attr([`PS],TOp(Sub,r,r',r''))
		| "mul.s",r,r',r'' -> Attr([`FS],TOp(Mulg,r,r',r''))
		| "mul.d",r,r',r'' -> Attr([`FD],TOp(Mulg,r,r',r''))
		| "mul.ps",r,r',r'' -> Attr([`PS],TOp(Mulg,r,r',r''))
		| "div.s",r,r',r'' -> Attr([`FS],TOp(Divg,r,r',r''))
		| "div.d",r,r',r'' -> Attr([`FD],TOp(Divg,r,r',r''))
		| "div.ps",r,r',r'' -> Attr([`PS],TOp(Divg,r,r',r''))
		| "madd.s",r,r',r'' -> Attr([`FS],TOp(Madd,r,r',r''))
		| "madd.d",r,r',r'' -> Attr([`FD],TOp(Madd,r,r',r''))
		| "madd.ps",r,r',r'' -> Attr([`PS],TOp(Madd,r,r',r''))
		| "msub.s",r,r',r'' -> Attr([`FS],TOp(Msub,r,r',r''))
		| "msub.d",r,r',r'' -> Attr([`FD],TOp(Msub,r,r',r''))
		| "msub.ps",r,r',r'' -> Attr([`PS],TOp(Msub,r,r',r''))
		| "nmadd.s",r,r',r'' -> Attr([`FS],TOp(Nmadd,r,r',r''))
		| "nmadd.d",r,r',r'' -> Attr([`FD],TOp(Nmadd,r,r',r''))
		| "nmadd.ps",r,r',r'' -> Attr([`PS],TOp(Nmadd,r,r',r''))
		| "nmsub.s",r,r',r'' -> Attr([`FS],TOp(Nmsub,r,r',r''))
		| "nmsub.d",r,r',r'' -> Attr([`FD],TOp(Nmsub,r,r',r''))
		| "nmsub.ps",r,r',r'' -> Attr([`PS],TOp(Nmsub,r,r',r''))
		| _ -> raise IllegalInstruction
	let parseBOpI = function
		| "lui",r,i -> Attr([],BOpI(Lui,r,i))
		| "tgei",r,r' -> Attr([],BOpI(T `GE,r,r'))
		| "tgeiu",r,r' -> Attr([`U],BOpI(T `GE,r,r'))
		| "tlti",r,r' -> Attr([],BOpI(T `LT,r,r'))
		| "tltiu",r,r' -> Attr([`U],BOpI(T `LT,r,r'))
		| "teqi",r,r' -> Attr([],BOpI(T `EQ,r,r'))
		| "tnei",r,r' -> Attr([],BOpI(T `NE,r,r'))
		| _ -> raise IllegalInstruction
	let parseBOp = function
		| "mult",r,r' -> Attr([],BOp(Mul,r,r'))
		| "dmult",r,r' -> Attr([`D],BOp(Mul,r,r'))
		| "multu",r,r' -> Attr([`U],BOp(Mul,r,r'))
		| "dmultu",r,r' -> Attr([`D;`U],BOp(Mul,r,r'))
		| "div",r,r' -> Attr([],BOp(Div,r,r'))
		| "ddiv",r,r' -> Attr([`D],BOp(Div,r,r'))
		| "divu",r,r' -> Attr([`U],BOp(Div,r,r'))
		| "ddivu",r,r' -> Attr([`D;`U],BOp(Div,r,r'))
		| "jalr",r,r' -> Attr([`AL],BOp(Jalr,r,r'))
		| "tge",r,r' -> Attr([],BOp(T `GE,r,r'))
		| "tgeu",r,r' -> Attr([`U],BOp(T `GE,r,r'))
		| "tlt",r,r' -> Attr([],BOp(T `LT,r,r'))
		| "tltu",r,r' -> Attr([`U],BOp(T `LT,r,r'))
		| "teq",r,r' -> Attr([],BOp(T `EQ,r,r'))
		| "tne",r,r' -> Attr([],BOp(T `NE,r,r'))
		| "mfc0",r,r' -> Attr([],BOp(Mfc0,r,r'))
		| "dmfc0",r,r' -> Attr([`D],BOp(Mfc0,r,r'))
		| "mtc0",r,r' -> Attr([],BOp(Mtc0,r,r'))
		| "dmtc0",r,r' -> Attr([`D],BOp(Mtc0,r,r'))
		| "mfc1",r,r' -> Attr([`FS],BOp(Mfc1,r,r'))
		| "dmfc1",r,r' -> Attr([`FD],BOp(Mfc1,r,r'))
		| "mtc1",r,r' -> Attr([`FS],BOp(Mtc1,r,r'))
		| "dmtc1",r,r' -> Attr([`FD],BOp(Mtc1,r,r'))
		| "sqrt.s",r,r' -> Attr([`FS],BOp(Sqrt,r,r'))
		| "sqrt.d",r,r' -> Attr([`FD],BOp(Sqrt,r,r'))
		| "neg.s",r,r' -> Attr([`FS],BOp(Neg,r,r'))
		| "neg.d",r,r' -> Attr([`FD],BOp(Neg,r,r'))
		| "neg.ps",r,r' -> Attr([`PS],BOp(Neg,r,r'))
		| "abs.s",r,r' -> Attr([`FS],BOp(Abs,r,r'))
		| "abs.d",r,r' -> Attr([`FD],BOp(Abs,r,r'))
		| "abs.ps",r,r' -> Attr([`PS],BOp(Abs,r,r'))
		| "round.l.s",r,r' -> Attr([`L;`FS],BOp(Round,r,r'))
		| "round.l.d",r,r' -> Attr([`L;`FD],BOp(Round,r,r'))
		| "round.w.s",r,r' -> Attr([`W;`FS],BOp(Round,r,r'))
		| "round.w.d",r,r' -> Attr([`W;`FD],BOp(Round,r,r'))
		| "trunc.l.s",r,r' -> Attr([`L;`FS],BOp(Trunc,r,r'))
		| "trunc.l.d",r,r' -> Attr([`L;`FD],BOp(Trunc,r,r'))
		| "trunc.w.s",r,r' -> Attr([`W;`FS],BOp(Trunc,r,r'))
		| "trunc.w.d",r,r' -> Attr([`W;`FD],BOp(Trunc,r,r'))
		| "ceil.l.s",r,r' -> Attr([`L;`FS],BOp(Ceil,r,r'))
		| "ceil.l.d",r,r' -> Attr([`L;`FD],BOp(Ceil,r,r'))
		| "ceil.w.s",r,r' -> Attr([`W;`FS],BOp(Ceil,r,r'))
		| "ceil.w.d",r,r' -> Attr([`W;`FD],BOp(Ceil,r,r'))
		| "floor.l.s",r,r' -> Attr([`L;`FS],BOp(Floor,r,r'))
		| "floor.l.d",r,r' -> Attr([`L;`FD],BOp(Floor,r,r'))
		| "floor.w.s",r,r' -> Attr([`W;`FS],BOp(Floor,r,r'))
		| "floor.w.d",r,r' -> Attr([`W;`FD],BOp(Floor,r,r'))
		| "cvt.s.l",r,r' -> Attr([`FS;`D],BOp(Cvt,r,r'))
		| "cvt.s.w",r,r' -> Attr([`FS;`W],BOp(Cvt,r,r'))
		| "cvt.s.d",r,r' -> Attr([`FS;`FD],BOp(Cvt,r,r'))
		| "cvt.d.l",r,r' -> Attr([`FD;`D],BOp(Cvt,r,r'))
		| "cvt.d.w",r,r' -> Attr([`FD;`W],BOp(Cvt,r,r'))
		| "cvt.d.s",r,r' -> Attr([`FD;`FS],BOp(Cvt,r,r'))
		| "cvt.l.s",r,r' -> Attr([`D;`FS],BOp(Cvt,r,r'))
		| "cvt.l.d",r,r' -> Attr([`D;`FD],BOp(Cvt,r,r'))
		| "cvt.w.s",r,r' -> Attr([`W;`FS],BOp(Cvt,r,r'))
		| "cvt.w.d",r,r' -> Attr([`W;`FD],BOp(Cvt,r,r'))
		| "c.f.s",r,r' -> Attr([`FS],BOp(C `F,r,r'))
		| "c.f.d",r,r' -> Attr([`FD],BOp(C `F,r,r'))
		| "c.f.ps",r,r' -> Attr([`PS],BOp(C `F,r,r'))
		| "c.sf.s",r,r' -> Attr([`FS],BOp(C `SF,r,r'))
		| "c.sf.d",r,r' -> Attr([`FD],BOp(C `SF,r,r'))
		| "c.sf.ps",r,r' -> Attr([`PS],BOp(C `SF,r,r'))
		| "c.un.s",r,r' -> Attr([`FS],BOp(C `UN,r,r'))
		| "c.un.d",r,r' -> Attr([`FD],BOp(C `UN,r,r'))
		| "c.un.ps",r,r' -> Attr([`PS],BOp(C `UN,r,r'))
		| "c.ngle.s",r,r' -> Attr([`FS],BOp(C `NGLE,r,r'))
		| "c.ngle.d",r,r' -> Attr([`FD],BOp(C `NGLE,r,r'))
		| "c.ngle.ps",r,r' -> Attr([`PS],BOp(C `NGLE,r,r'))
		| "c.eq.s",r,r' -> Attr([`FS],BOp(C `EQ,r,r'))
		| "c.eq.d",r,r' -> Attr([`FD],BOp(C `EQ,r,r'))
		| "c.eq.ps",r,r' -> Attr([`PS],BOp(C `EQ,r,r'))
		| "c.seq.s",r,r' -> Attr([`FS],BOp(C `SEQ,r,r'))
		| "c.seq.d",r,r' -> Attr([`FD],BOp(C `SEQ,r,r'))
		| "c.seq.ps",r,r' -> Attr([`PS],BOp(C `SEQ,r,r'))
		| "c.ueq.s",r,r' -> Attr([`FS],BOp(C `UEQ,r,r'))
		| "c.ueq.d",r,r' -> Attr([`FD],BOp(C `UEQ,r,r'))
		| "c.ueq.ps",r,r' -> Attr([`PS],BOp(C `UEQ,r,r'))
		| "c.ngl.s",r,r' -> Attr([`FS],BOp(C `NGL,r,r'))
		| "c.ngl.d",r,r' -> Attr([`FD],BOp(C `NGL,r,r'))
		| "c.ngl.ps",r,r' -> Attr([`PS],BOp(C `NGL,r,r'))
		| "c.olt.s",r,r' -> Attr([`FS],BOp(C `OLT,r,r'))
		| "c.olt.d",r,r' -> Attr([`FD],BOp(C `OLT,r,r'))
		| "c.olt.ps",r,r' -> Attr([`PS],BOp(C `OLT,r,r'))
		| "c.lt.s",r,r' -> Attr([`FS],BOp(C `LT,r,r'))
		| "c.lt.d",r,r' -> Attr([`FD],BOp(C `LT,r,r'))
		| "c.lt.ps",r,r' -> Attr([`PS],BOp(C `LT,r,r'))
		| "c.ult.s",r,r' -> Attr([`FS],BOp(C `ULT,r,r'))
		| "c.ult.d",r,r' -> Attr([`FD],BOp(C `ULT,r,r'))
		| "c.ult.ps",r,r' -> Attr([`PS],BOp(C `ULT,r,r'))
		| "c.nge.s",r,r' -> Attr([`FS],BOp(C `NGE,r,r'))
		| "c.nge.d",r,r' -> Attr([`FD],BOp(C `NGE,r,r'))
		| "c.nge.ps",r,r' -> Attr([`PS],BOp(C `NGE,r,r'))
		| "c.ole.s",r,r' -> Attr([`FS],BOp(C `OLE,r,r'))
		| "c.ole.d",r,r' -> Attr([`FD],BOp(C `OLE,r,r'))
		| "c.ole.ps",r,r' -> Attr([`PS],BOp(C `OLE,r,r'))
		| "c.le.s",r,r' -> Attr([`FS],BOp(C `LE,r,r'))
		| "c.le.d",r,r' -> Attr([`FD],BOp(C `LE,r,r'))
		| "c.le.ps",r,r' -> Attr([`PS],BOp(C `LE,r,r'))
		| "c.ule.s",r,r' -> Attr([`FS],BOp(C `ULE,r,r'))
		| "c.ule.d",r,r' -> Attr([`FD],BOp(C `ULE,r,r'))
		| "c.ule.ps",r,r' -> Attr([`PS],BOp(C `ULE,r,r'))
		| "c.ngt.s",r,r' -> Attr([`FS],BOp(C `NGT,r,r'))
		| "c.ngt.d",r,r' -> Attr([`FD],BOp(C `NGT,r,r'))
		| "c.ngt.ps",r,r' -> Attr([`PS],BOp(C `NGT,r,r'))
		| "mov.ps",r,r' -> Attr([`PS],BOp(Mov,r,r'))
		| _ -> raise IllegalInstruction
	let parseUOp = function
		| "mfhi",r -> Attr([],UOp(Mfhi,r))
		| "mthi",r -> Attr([],UOp(Mthi,r))
		| "mflo",r -> Attr([],UOp(Mflo,r))
		| "mtlo",r -> Attr([],UOp(Mtlo,r))
		| "jr",r -> Attr([],UOp(Jr,r))
		| _ -> raise IllegalInstruction
	let parseUOpI = function
		| "j",i -> Attr([],UOpI(J,i))
		| "jal",i -> Attr([`AL],UOpI(J,i))
		| "bc1f",i -> Attr([],UOpI(Bc1f,i))
		| "bc1t",i -> Attr([],UOpI(Bc1t,i))
		| "bc1fl",i -> Attr([`L],UOpI(Bc1f,i))
		| "bc1tl",i -> Attr([`L],UOpI(Bc1t,i))
		| _ -> raise IllegalInstruction
	let parseMisc = function
		| "sync" -> Special "sync"
		| "syscall" -> Special "syscall"
		| "break" -> Special "break"
		| _ -> raise IllegalInstruction

  open Linda.MaybeMonad
  let lift f s =
    try Some (f s) with IllegalInstruction -> None

  let (<+>) p p' =
    fun s ->
      match p s with
        | None -> p' s
        | x -> x
  let parse0 = lift parseMisc
  let parse1 = lift parseUOpI <+> lift parseUOp
  let parse2 = lift parseBOp <+> lift parseBOpI
  let parse3 = lift parseTOp <+> lift parseTOpI <+> lift parseLoadStore
end