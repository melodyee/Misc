module OProfile (
Bbinfo (..),
parseAnnotAss
) where
import qualified Data.Char as C
import Text.Regex.Posix ( (=~) )
import Data.Char (toLower, isNumber)
import qualified Safe as S
import Data.List (intercalate, (\\), isPrefixOf, unzip4, unzip5)
import Data.List.Split (wordsBy)
import Numeric (readHex)

data Bbinfo = Bbinfo {
        bblabel :: String,
        addr :: Integer,
        commits :: Integer,
        len :: Integer,
        target :: [Integer]
    } deriving (Show,Eq)

myread s = S.readNote s s
myreadHex :: String -> Integer
myreadHex s = case readHex (unwords.words $ s) of
        [] -> error $ "#" ++ s ++ "#"
        l -> fst $ head l

chopBy :: (a -> Bool) -> [a] -> [[a]]
chopBy p [] = []
chopBy p l =
    let (l1,l2) = span p l in
    let (l3,l4) = span (not.p) l2 in
    (l1++l3):chopBy p l4
chopBy' p l = chopBy p $ dropWhile (not.p) l

parseAnnotAss :: FilePath -> IO ([Bbinfo])
parseAnnotAss fn = do
    s <- readFile fn
    return $ concatMap work $ chopBy' (flip (=~) "<.*>:") $ lines $ s
    where
        work :: [String] -> [Bbinfo]
        work [] = []
        work (label_line:body) =
            let commits = myread.last.init.init.words $ label_line in
            let label = tail.init.init $ label_line =~ "<.*>:" in
            let addr = myreadHex.head.words $ label_line in
            let real_body = filter isInstrLine body in
            if null real_body then [] else
            let fallthrough = (+4). myreadHex $ wordsBy (==':') (last real_body) !! 1 in
            let bb = Bbinfo { bblabel = label, commits = commits, 
                len = fromIntegral (length real_body),
                addr = addr, target = fallthrough:concatMap getTarget real_body} in
            [bb]
            where isInstrLine ln = case wordsBy (==':') ln of
                        [_,a,_] -> isNumber.head.unwords.words $ a
                        _ -> False
                  getTarget ln = target.unwords.words $ ((wordsBy (==':') ln) !! 2)
                  target (x:instr) | x`elem`"bj" = map myreadHex$ filter (isNumber.head) $ wordsBy (`elem` " ,\t") instr
                  target _ = []