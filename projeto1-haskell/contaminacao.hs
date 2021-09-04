import Data.List as DL
import Data.Map.Strict as Map


-- Setup

setup :: [[String]] -> Map String [(String, String)]
setup l = Map.fromListWith (++) $ Prelude.map (\[a, b, c] -> (a, [(b, c)])) l


-- Métodos para processar o input

getLines :: [[String]] -> [[String]]
getLines l = addLines $ DL.takeWhile (not . DL.null) l
  where addLines l = l ++ (Prelude.map (\[a, b, c] -> [b, a, c]) l)

breakInput :: String -> [[String]]
breakInput input = Prelude.map words $ lines input

processInput :: String -> ([[String]], String)
processInput input = let brokenInput = breakInput input
                  in (getLines brokenInput, head $ last brokenInput)


-- Main

main :: IO ()
main = do
  input <- getContents
  putStrLn (show $ setup $ fst $ processInput input)
