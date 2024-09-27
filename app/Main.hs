module Main (main) where

import System.IO (readFile, writeFile)
import Data.List (delete)

type Position = (Int, Int)
type Path = [Position]

-- DFS para encontrar la ruta desde el enemigo hasta el jugador
dfs :: Position -> Position -> [Position] -> Path -> Path
dfs start goal walls visited
    | start == goal = [start]
    | null validNeighbors = []  -- Si no hay vecinos válidos, devuelve una ruta vacía
    | otherwise = case filter (not . null) possiblePaths of
        (p:_) -> start : p
        [] -> []
  where
    validNeighbors = [next | next <- neighbors start, next `notElem` visited, next `notElem` walls]
    possiblePaths = [dfs next goal walls (next:visited) | next <- validNeighbors]

-- Vecinos posibles (arriba, abajo, izquierda, derecha)
neighbors :: Position -> [Position]
neighbors (x, y) = filter validNeighbor [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where
    validNeighbor (nx, ny) = nx >= 0 && nx < 20 && ny >= 0 && ny < 15  -- Asumiendo un mapa de 20x15

-- Leer la posición del jugador, enemigo y los muros desde un archivo
readInput :: IO (Maybe (Position, Position, [Position]))
readInput = do
    contents <- readFile "./app/input.txt"
    let ls = lines contents
    case ls of
        [positions, walls] -> do
            let [ex, ey, px, py] = map read (words positions)
            let wallsList = [(read x, read y) | (x:y:_) <- chunkPairs (words walls)]
            return $ Just ((ex, ey), (px, py), wallsList)
        _ -> return Nothing

-- Escribir la ruta calculada en un archivo
writeOutput :: Path -> IO ()
writeOutput path = if null path 
                   then writeFile "./app/output.txt" "No path found"
                   else writeFile "./app/output.txt" (unwords (map show path))

-- Divide una lista en pares
chunkPairs :: [a] -> [[a]]
chunkPairs [] = []
chunkPairs (x:y:xs) = [x, y] : chunkPairs xs
chunkPairs _ = error "La lista debe contener un número par de elementos"

main :: IO ()
main = do
    input <- readInput
    case input of
        Just (enemyPos, playerPos, walls) -> do
            let path = dfs enemyPos playerPos walls []
            writeOutput path
        Nothing -> putStrLn "Error: Input file format is incorrect."