{-
  MC346 - Projeto 1 - Haskell

  Alunos:
    Gabriel Henrique Rosa Oswaldo - 172185
    Bruno Rosano Laporte Ambrosio - 195141
-}

import qualified Data.List as DL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)


-- Definição dos tipos utilizados
type Node = String
type Pred = Node
type Dist = Float
type Weight = Float
type Visited = Bool
type NodeInfo = (Dist, Pred, Weight, Visited)
type DNode = (Node, NodeInfo)
type DNodeMap = Map Node NodeInfo
type Edge = (Node, Weight)
type Graph = Map Node [Edge]


-- Algoritmo de Dijkstra, e métodos auxiliares, para achar 
-- a árvore de caminhos mínimos em um grafo não direcionado 
-- com pesos não negativos.
dijkstra :: Graph -> Node -> DNodeMap
dijkstra graph start = dijkstra' queue Map.empty
  where queue = initSingleSrc graph start
        dijkstra' q tree
          | Map.size tree == Map.size graph = tree
          | otherwise = dijkstra' newQ $ Map.insert u uInfo tree
          where nodeU = extractMinQ q
                u = fst nodeU
                uInfo = snd nodeU
                newQ = relax graph nodeU $ deleteMinQ q u

initSingleSrc :: Graph -> Node -> DNodeMap
initSingleSrc graph start = Map.fromList $ Prelude.map initSingleSrc' $ Map.keys graph
  where initSingleSrc' node = if (node == start) 
                              then (node, (0, "nil", 0, False))
                              else (node, (1/0, "nil", 0, False))

extractMinQ :: DNodeMap -> DNode
extractMinQ q = DL.minimumBy (\(_,(du, _, _, _)) (_,(dv, _, _, _)) -> compare du dv) $ notVisited $ Map.toList q
  where notVisited = Prelude.filter (\(_,(_, _, _, e)) -> e == False)

deleteMinQ :: DNodeMap -> Node -> DNodeMap
deleteMinQ q minKey = Map.insert minKey (softDelete $ Map.elemAt (getMaybe $ Map.lookupIndex minKey q) q) q
  where softDelete (_, (du, pred, wuv, _)) = (du, pred, wuv, True)

relax :: Graph -> DNode -> DNodeMap -> DNodeMap
relax graph node queue = relax' adj queue
  where u = fst node
        du = fst' $ snd node
        fst' (a,_,_,_) = a
        adj = getMaybe $ Map.lookup u graph
        relax' [] q = q
        relax' ((v, wuv):es) q
          | dv > du + wuv = relax' es $ Map.insert v (du + wuv, u, wuv, visited) q
          | otherwise = relax' es q
          where nodeV = getMaybe $ Map.lookup v q
                dv = fst' nodeV
                visited = (\(_,_,_,d) -> d) nodeV


-- Métodos auxiliares
getMaybe :: Maybe a -> a
getMaybe = fromMaybe (error "Erro ao extrair valor maybe")

showResult :: DNodeMap -> IO ()
showResult tree = printf "%.2f\n" $ Map.foldl (\acc (_, _, wuv, _) -> acc + wuv) 0 tree


-- Setup do grafo utilizando um Map
buildGraph :: [[String]] -> Graph
buildGraph l = Map.fromListWith (++) $ Prelude.map (\[a, b, c] -> (a, [(b, (/) 1 $ read c :: Float)])) l


-- Método para processar o input
processInput :: String -> ([[String]], String)
processInput input = (getLines brokenInput, head $ last brokenInput)
  where brokenInput = breakInput input
        breakInput inpt = Prelude.map words $ lines inpt
        getLines l = addLines $ DL.takeWhile (not . DL.null) l
        addLines l = l ++ (Prelude.map (\[a, b, c] -> [b, a, c]) l)


-- Main
main :: IO ()
main = do
  input <- getContents
  let processedInput = processInput input
  let startNode = snd processedInput
  let graph = buildGraph $ fst processedInput
  showResult $ dijkstra graph startNode
