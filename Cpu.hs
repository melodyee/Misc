
module Cpu where
import Data.Word
import Data.Bits
import qualified Data.BitString.BigEndian as BS
import qualified Data.ByteString as B
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
type Reg = Word16
type Imm = Word16
data Op = Add | Sub | And | Or | Not | Sl | Sr | Sru deriving (Eq,Show,Read,Enum) 
-- data Opi = Addi | Ld | St | Bz | Bgt | Ble deriving (Eq,Show)
data Cond = Z | Gt | Le deriving (Eq,Show,Read,Enum)
data Instr = 
          Arith Op Reg Reg Reg
        | Addi Reg Reg Imm
        | Ld Reg Reg Imm
        | St Reg Reg Imm
        | B Cond Reg Imm
        deriving (Eq,Show,Read)

encode :: Instr -> Word16
encode (Arith op rd rs1 rs2) =
        fromIntegral (fromEnum op) `shiftL` 12 .|.
        rd `shiftL` 9 .|. rs1 `shiftL` 6 .|. rs2 `shiftL` 3
encode (Addi rd rs1 imm) =
        9 `shiftL` 12 .|.
        rd `shiftL` 9 .|. rs1 `shiftL` 6 .|. (imm .&. 63)
encode (Ld rd rs1 imm) =
        10 `shiftL` 12 .|.
        rd `shiftL` 9 .|. rs1 `shiftL` 6 .|. (imm .&. 63)
encode (St rd rs1 imm) =
        11 `shiftL` 12 .|.
        rd `shiftL` 9 .|. rs1 `shiftL` 6 .|. (imm .&. 63)
encode (B cond rs1 imm) =
        12 `shiftL` 12 .|.
        (fromIntegral.fromEnum$ cond) `shiftL` 9 .|. rs1 `shiftL` 6 .|. imm

toList :: Word16 -> [Int]
toList instr = map fromEnum$ BS.toList . BS.bitString . B.pack $ 
        map fromIntegral [instr `shiftR` 8,instr .&. 255]

myshow :: [Instr] -> String
myshow = unlines.map (compact.toList.encode)
        where compact = foldl1 (++).map show

mywrite k v = M.insert k v
myread r m = maybe (error $ "myread "++show r) id (M.lookup r m) :: Word16

data Cpu = Cpu {
         mem :: M.Map Word16 Word16,
         regs :: M.Map Word16 Word16,
         pc :: Word16
        } deriving (Eq,Show,Read)
        
readReg :: Word16 -> State Cpu Word16
readReg r = do
        cpu <- get
        return (if r>=8 then error ("reg "++show r) else 
                if r==0 then 0 else myread r (regs cpu))
writeReg :: Word16 -> Word16 -> State Cpu ()
writeReg r v = do
        cpu <- get
        put cpu {regs = mywrite r v (regs cpu)}  
         
readMem a = do
        cpu <- get
        return (if a>=4096 then error ("mem "++show a) else myread a (mem cpu))

writeMem r v = do
        cpu <- get
        put cpu {mem = mywrite r v (mem cpu)}

incPc inc = do
        cpu <- get
        put cpu {pc = pc cpu +inc }

interpretOps :: Op -> Word16 -> Word16 -> Word16
interpretOps op = maybe (error (show op)) id (lookup op [
        (Add,(+)),(Sub,(-)),
        (And, (.&.)),(Or,(.|.)), 
        (Not,\x y -> complement x), (Sl,\x y -> shiftL x (fromIntegral y)),
        (Sr, \x y -> fromIntegral (shiftR (fromIntegral x) (fromIntegral y) :: Int)), 
        (Sru, \x y -> shiftR x (fromIntegral y))])        

interpretCond cond = case cond of
        Z -> (==0)
        Gt -> not.lez
        Le -> lez
        where lez x = ((x .&. 32768) == 32768) || x==0 
delta = 1
interpret :: Instr -> State Cpu ()
interpret (Arith op rd rs1 rs2) = do
       r1 <- readReg rs1
       r2 <- readReg rs2
       writeReg rd ((interpretOps op) r1 r2)
       incPc delta
interpret (Addi rd rs1 imm) = do
        r1 <- readReg rs1
        writeReg rd (r1 + imm)
        incPc delta
interpret (Ld rd rs1 imm) = do
        r1 <- readReg rs1
        v <- readMem (fromIntegral (r1 + imm))
        writeReg rd v
        incPc delta
interpret (St rd rs1 imm) = do
        r1 <- readReg rs1
        v <- readReg rd
        writeMem (fromIntegral (r1 + imm)) v
        incPc delta
        
interpret (B cond rs1 imm) = do
        r1 <- readReg rs1
        incPc (if (interpretCond cond) r1 then imm else delta)
       
exec :: State Cpu Cpu 
exec = do
        cpu <- get
        let instr = maybe (error ("wrong address: "++show (pc cpu))) id (M.lookup (pc cpu) rom)
        interpret instr
        cpu <- get
        return cpu

initCpu = Cpu {
        mem = M.empty,
        regs = M.empty,
        pc = 0} 

a @> b = b - a

instrs = [
         Addi 1 0 37
        ,Addi 2 0 (-3)
        ,Arith And 3 0 0
        ,Arith And 4 0 0
        ,B Gt 2 (l5 @> l2) --5
        ,nop
        ,Addi 4 0 1
        ,Arith Sub 2 0 2
        ,B Z 2 (l3 @> l1) -- 3
        ,nop
        ,Arith Add 3 3 1 -- 2
        ,Addi 2 2 (-1)
        ,B Z 0 (l6 @> l3) --6
        ,nop
        ,B Z 4 (l1 @> l4) --1
        ,nop
        ,Arith Sub 3 0 3
        ,B Z 0 (l4 @> l4) -- 4
        ,nop
        ] where nop = Arith Sl 0 0 0
                (l1,l2,l3,l4,l5,l6) = (14,10,8,17,4,12)
rom = M.fromList $ zip [0..] instrs  

main :: IO ()       
main = putStrLn.show.take 100 $ evalState (sequence (repeat exec)) initCpu  