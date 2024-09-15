import System.Random (randomRIO)
import Data.List (find)

-- Definición de tipos
type Pos = (Int, Int)
data Cell = Empty | UserTank | EnemyTank | Barrier deriving (Eq, Show)
type Board = [[Cell]]

-- Tamaño del tablero
boardSize :: (Int, Int)
boardSize = (10, 10)

-- Inicializa el tablero
initBoard :: IO Board
initBoard = do
    let barriers = [(2, 2), (3, 4), (5, 5)] -- Posiciones fijas de barreras
    userTankPos <- return (5, 0) -- Posición fija del tanque del usuario
    enemyTankPositions <- mapM (\_ -> randomPos boardSize) [1..3] -- 3 tanques enemigos
    let board = [ [ if (x, y) `elem` barriers then Barrier else Empty | x <- [0..snd boardSize - 1] ] | y <- [0..fst boardSize - 1] ]
    let boardWithUserTank = placeCells board [(userTankPos, UserTank)]
    return $ placeCells boardWithUserTank (zip enemyTankPositions (repeat EnemyTank))


-- Coloca celdas en el tablero
placeCells :: Board -> [(Pos, Cell)] -> Board
placeCells board [] = board
placeCells board ((pos, cell):xs) = placeCells (replaceCell board pos cell) xs

-- Reemplaza una celda en el tablero
replaceCell :: Board -> Pos -> Cell -> Board
replaceCell board (x, y) cell = 
    take y board ++ [take x (board !! y) ++ [cell] ++ drop (x + 1) (board !! y)] ++ drop (y + 1) board

-- Obtiene una posición aleatoria en el tablero
randomPos :: (Int, Int) -> IO Pos
randomPos (width, height) = do
    x <- randomRIO (0, width - 1)
    y <- randomRIO (0, height - 1)
    return (x, y)

-- Algoritmo de búsqueda en profundidad (DFS) para encontrar el camino
dfs :: Board -> Pos -> Pos -> [Pos]
dfs board start goal = dfs' [start] [] where
    dfs' [] _ = []
    dfs' (current:stack) visited
        | current == goal = [current]
        | otherwise = 
            let neighbors = filter (`notElem` visited) (getNeighbors current)
                newStack = neighbors ++ stack
                newVisited = current : visited
            in case find (\p -> p == goal) neighbors of
                Just p -> current : dfs' [p] newVisited
                Nothing -> dfs' newStack newVisited
    getNeighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)] -- Movimientos posibles

-- Main
main :: IO ()
main = do
    board <- initBoard
    putStrLn "Initial board:"
    mapM_ print board
    let userPos = (5, 0)
    let enemyPos = [(2, 2), (3, 4), (5, 5)] -- Posiciones de ejemplo para enemigos
    mapM_ (\pos -> print (pos, dfs board pos userPos)) enemyPos
