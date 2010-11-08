import qualified Data.Map as M
import Control.Monad
import Control.Monad.State
import Data.List(partition, foldl')
import Data.Maybe(catMaybes, fromJust)

import qualified Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Tree
import OProfile

graphFromBbinfos :: [Bbinfo] -> Gr (String,Float) String 
graphFromBbinfos bbinfos = G.insEdges edges' (G.insNodes nodes' G.empty)
        where m = foldl' (\m i -> M.insert (addr i) (bblabel i) m) M.empty bbinfos
              nodes' = zip [0..] nodes
              edges' = map (\(n,n',l) -> (aux n,aux n',l)) edges
                        where aux n = fromJust (M.lookup n numberm)
              numberm = M.fromList $ zip (map fst nodes) [0..]
              (nodes,edges) = proc [] [] bbinfos
              freq bbinfo = fromIntegral (commits bbinfo) / fromIntegral (len bbinfo)
              proc :: [(String,Float)] -> [[(String,String,String)]] -> [Bbinfo] -> ([(String,Float)],[(String,String,String)])
              proc nodes edges [] = (nodes,concat edges)
              proc nodes edges (bbinfo:xs) = proc ((label,freq bbinfo):nodes) (newedges:edges) xs
                        where label = bblabel bbinfo
                              newedges = map (\lab -> (label, lab, "")) $ catMaybes $ map (`M.lookup` m) $ target bbinfo

testGraph :: Gr (String, Float) String
testGraph = G.mkGraph [(0,("a",10)),(1,("b",10)),(2,("c",7))] edges 
        where edges = [(0,1,""),(0,2,""),(2,1,"")]

collect :: Num a => Gr (c, a) b -> [Equation (c,c) a]
collect g = concatMap collect_node (G.nodes g)
        where label n = fromJust (G.lab g n)
              --collect_node :: G.Node -> [Equation (c,c) a]
              collect_node node =
                let (s,f) = label node in
                filter (not.null.snd ) $ [(f,map ((,) s .fst . label) (G.suc g node))]
                                ++ [(f,map (flip (,) s .fst . label) (G.pre g node))]

type Equation a b = (b, [a])

solve :: (Ord a, Num b) => [Equation a b] -> M.Map a b 
solve l = snd $ myfix solveOne (l,M.empty)
        where solveOne :: (Ord a,Num b) => ([Equation a b], M.Map a b) -> ([Equation a b], M.Map a b)
              solveOne (l,m) = runState (step l) m

step :: (Ord a, Num b) => [Equation a b] -> State (M.Map a b) [Equation a b]
step l = liftM concat $ sequence (map stepOne l)
        where   stepOne :: (Ord a, Num b) => Equation a b -> State (M.Map a b) [Equation a b]
                stepOne (lhs,rhs) = do
                        m <- get
                        let (known,unknown) = partition (flip M.member m) rhs
                        let lhs' = lhs - sum (catMaybes $ map (flip M.lookup m) known)
                        case unknown of
                                [] -> return []
                                [x] -> do 
                                        put $ M.insert x lhs' m
                                        return []
                                _ ->   return [(lhs', unknown)]

myfix :: Eq a => (a -> a) -> a -> a 
myfix f x = last $ map snd $ takeWhile (\(a,b) -> (a/=b)) $ zip l (tail l)
        where l = iterate f x

test :: [Equation String Float]
test = [(10,["AB","CB"]),(10,["AB","AC"]),(7,["AC"])]

main = do
        infos <- parseAnnotAss "/home/zsc/prof_339/gap.s"
        -- /home/zsc/prof_339/gap/gap.ipakeep/5.s"
        -- /home/zsc/prof_339/gzip.s
        let g = graphFromBbinfos infos
        putStrLn $ show $ g
        putStrLn $ show $ solve $ collect g
        -- putStrLn $ show $ infos
        --putStrLn $ show $ solve $ collect $ testGraph
        --putStrLn $ show $ solve test