
module Cpu = struct
  let inc = 2
  open Int32
  let regs : t array = Array.init 8 (fun _ -> zero)
  let read r = if r==0 then zero else regs.(r)
  let write r v = if r!=0 then regs.(r) <- v 
  let mem : t array = Array.init 4096 (fun _ -> zero)
  let pc : int ref = ref 0
  type reg = int
  type instr =
    | Add of reg * reg * reg
    | Sub of reg * reg * reg
    | And of reg * reg * reg
    | Or of reg * reg * reg
    | Not of reg * reg
    | SL of reg * int * reg
    | SR of reg * int * reg
    | SRU of reg * int * reg
    | Addi of reg * int * reg
    | Ld of reg * int * reg
    | St of reg * int * reg
    | Bz of reg * int
    | Bgt of reg * int
    | Ble of reg * int
   let interpret instr = match instr with
    | Add (r1,r2,r3) -> write r1 regs.(r2) regs.(r3)
    | Sub (r1,r2,r3) -> regs.(r1) <- sub regs.(r2) regs.(r3)
    | And (r1,r2,r3) -> regs.(r1) <- logand regs.(r2) regs.(r3)
    | Or (r1,r2,r3) -> regs.(r1) <- logor regs.(r2) regs.(r3)
    | Not (r1,r2) -> regs.(r1) <- lognot regs.(r2)
    | SL (r1,i,r3) -> regs.(r1) <- shift_left regs.(r3) i
    | SR (r1,i,r3) -> regs.(r1) <- shift_right regs.(r3) i
    | SRU (r1,i,r3) -> regs.(r1) <- shift_right_logical regs.(r3) i
    | Addi (r1,i,r3) -> regs.(r1) <- add (of_int i) regs.(r3)
    | Ld (r1,i,r3) -> regs.(r1) <- mem.(i+to_int regs.(r3))
    | St (r1,i,r3) -> mem.(i+to_int regs.(r3)) <- regs.(r1)
    | Bz (r1,i) -> if regs.(r1) == zero then pc := !pc + i else pc := !pc + inc
    | Bgt (r1,i) -> if regs.(r1) > zero then pc := !pc + i else pc := !pc + inc
    | Ble (r1,i) -> if regs.(r1) <= zero then pc := !pc + i else pc := !pc + inc
  end
  