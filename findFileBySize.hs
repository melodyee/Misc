import System.IO
import Data.Tree
import System.Directory
import qualified Control.Exception as E
import System.IO.Unsafe
import System.FilePath ((</>))
import System.Environment

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = E.handle handler $
  E.bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
  where 
    handler :: E.SomeException -> IO (Maybe Integer)
    handler _ = return Nothing

-- list :: FilePath -> Tree (FilePath, Maybe Integer)
--list d = unfoldTree (\d -> ((d,size d),aux d)) d
--    where aux d = unsafePerformIO $ getSubs d >>= return.(filter (flip notElem [".",".."]))
--          getSubs d = do
--            b <- doesDirectoryExist d
--            if b then getDirectoryContents d else return []
--          size d = unsafePerformIO $ getFileSize d
--          --doesFileExist d >>= \b -> return $ if b then Just (getFileSize d) else Nothing

list :: FilePath -> Tree (FilePath, Integer)
list d = 
    let forest = map list (aux d) in
    if forest == [] then
        Node {
            rootLabel = (d, size d),
            subForest = forest
        }
    else
        Node {
            rootLabel = (d,sum $ map (snd.rootLabel) forest),
            subForest = forest
        }
    where aux d = unsafePerformIO $ getSubs d >>= return.map (\x -> d </> x).(filter (flip notElem [".",".."]))
          getSubs d = do
            b <- doesDirectoryExist d
            if b then getDirectoryContents d else return []
          size d = maybe 0 id $ unsafePerformIO $ getFileSize d
          --doesFileExist d >>= \b -> return $ if b then Just (getFileSize d) else Nothing

--count = 

trim :: (Integer -> Bool) -> Tree (FilePath, Integer) -> Tree (FilePath, Integer)
trim p t = if p.snd $ root 
        then Node { rootLabel = root, subForest = map (trim p) $ filter (p.snd.rootLabel) (subForest t)}
        else Node { rootLabel = rootLabel t, subForest = []} 
    where root = rootLabel t    

main = do 
    args <- getArgs
    let l = list $ args!!0
    putStrLn $ unlines $ map (\(a,b) -> show b ++ "\t" ++ a) $ flatten $ trim (>(read$ args!!1)) l
