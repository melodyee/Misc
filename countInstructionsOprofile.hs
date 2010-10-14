-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | Count instructions of each category and memory traffic (not cached) based on
-- | Oprofile statistics
--
-----------------------------------------------------------------------------

module Main (
main

) where
import qualified Data.Char as C
import Text.Regex.Posix ( (=~) )
import Data.Char (toLower)
import qualified Safe as S
import Data.List (intercalate, (\\), isPrefixOf, unzip4, unzip5)

parseLog :: FilePath -> IO ([(String,Integer)])
parseLog fn = do
    s <- readFile fn
    let l = tail.dropWhile (not.isPrefixOf "samples") $ lines s
    return $ map line l
    where line ln =
            (last l,read (l!!2) * 500000)
            where l = words ln

infoOp :: String -> (Integer,Integer,Integer,Integer) -- Load LoadBytes Store StoreBytes
infoOp s = let (a,b) = maybe (0,0) (\x -> (1,x)) $ tellBy loads s in
            let (c,d) = maybe (0,0) (\x -> (1,x)) $ tellBy stores s in
            (a,b,c,d)
    where loads = [("ld",8),("ldc1",8),("l.d",8),("lw",4),("lwu",4),("lwc1",4),("lwl",4),("lwr",4),("l.w",4),("lh",2),("lhu",2),("lbu",1),("lb",1)]
          stores = [("sd",8),("sdc1",8),("s.d",8),("sw",4),("swu",4),("swc1",4),("swl",4),("swr",4),("s.w",4),("sh",2),("sb",1)]
          tellBy tab s = Prelude.lookup s tab

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn e l =
    let (l',rest) = span (/=e) l in
    l' : splitOn e (dropWhile ((==)e) rest)

info :: [String] -> (Integer,Integer,Integer,Integer) -- Load LoadBytes Store StoreBytes
info l = let (l1,l2,l3,l4) = unzip4 $ map (infoOp.getOp) l in (sum l1,sum l2,sum l3,sum l4)
    where getOp s = map toLower $ unwords $take 1 $ words ((splitOn ':' s) !! 2)

data Bbinfo = Bbinfo {
        bblabel :: String,
        commits :: Integer,
        len :: Integer,
        loads :: Integer,
        loadbytes :: Integer,
        stores :: Integer,
        storebytes :: Integer
    } deriving (Show,Eq)

myread s = S.readNote s s

parseAnnotAss :: FilePath -> IO ([Bbinfo])
parseAnnotAss fn = do
    s <- readFile fn
    return $ work [] $ lines s
    where
        work :: [Bbinfo] -> [String] -> [Bbinfo]
        work acc lns =
            if null lns then acc else
            let l = dropWhile (not.(flip (=~) "<.*>:")) lns in
            let label_line = head l in
            let commits = myread.last.init.init.words $ label_line in
            let label = tail.init.init $ label_line =~ "<.*>:" in
            let (body,rest) = span (not.isEnd) (tail l) in
            let (a,b,c,d) = info body in
            let bb = Bbinfo { bblabel = label, commits = commits, len = fromIntegral (length body),
                loads = a,  loadbytes = b, stores = c, storebytes = d} in
            work (bb:acc) rest
            where isEnd = (==":").unwords.words

hw3_8 spec = do
    print $ "[tot]" ++ spec
    l <- parseAnnotAss $ "/home/zsc/prof_339/"++spec++".s"
    print l
    let (a,b,c,d,e) = unzip5.map account $ l
    putStrLn $ "[tot] instructions = " ++ show (fromIntegral (sum a*500000) :: Float)
    putStrLn $ "[tot] load instructions = " ++ show (sum b*500000)
    putStrLn $ "[tot] load bytes (data) = " ++ show (sum c*500000)
    putStrLn $ "[tot] store instructions = " ++ show (sum d*500000)
    putStrLn $ "[tot] store bytes (data) = " ++ show (sum e*500000)
    where
        account bb = (cnt, conv (loads bb), conv (loadbytes bb), conv (stores bb), conv (storebytes bb))
            where cnt = commits bb
                  freq = fromIntegral cnt/fromIntegral (len bb) :: Float
                  conv n = freq * fromIntegral n

main = do
    sequence_ . map hw3_8 $ words "art gap gcc gzip mcf perlbmk"
