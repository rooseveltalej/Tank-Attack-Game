module Main (main) where

import System.IO (readFile, writeFile)
import Data.List (delete)

type Position = (Int, Int)
type Path = [Position]

-- DFS para encontrar la ruta desde el enemigo hasta el jugador
dfs :: Position -> Position -> [Position] -> Path -> Path
dfs start goal walls visited
    | start == goal = [start]
    | otherwise = start : concat [dfs next goal walls (next:visited) | next <- neighbors start, next `notElem` visited, next `notElem` walls]

-- Vecinos posibles (arriba, abajo, izquierda, derecha)
neighbors :: Position -> [Position]
neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- Leer la posición del jugador y los muros desde un archivo
readInput :: IO (Maybe (Position, Position, [Position]))
readInput = do
    contents <- readFile "C:\\Users\\Usuario\\Desktop\\Juego\\app\\input.txt"
    let positions = map read (words contents)
    case positions of
        [ex, ey, px, py] -> do
            let walls = [(1,1), (1,2), (2,2)] -- Ejemplo de muros
            return $ Just ((ex, ey), (px, py), walls)
        _ -> return Nothing

-- Escribir la ruta calculada en un archivo
writeOutput :: Path -> IO ()
writeOutput path = writeFile "C:\\Users\\Usuario\\Desktop\\Juego\\app\\output.txt" (unwords (map show path))

main :: IO ()
main = do
    input <- readInput
    case input of
        Just (enemyPos, playerPos, walls) -> do
            let path = dfs enemyPos playerPos walls []
            writeOutput path
        Nothing -> putStrLn "Error: Input file format is incorrect."