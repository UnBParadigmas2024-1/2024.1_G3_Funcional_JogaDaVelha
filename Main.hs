import Data.List (intercalate)

-- Definicao dos tipos de dados
data Player = White | Black deriving (Eq, Show)
data Piece = Empty | Pawn Player deriving (Eq, Show)
type Board = [[Piece]]

-- Printa o tabuleiro
printBoard :: Board -> IO ()
printBoard board = do
    mapM_ printRowIndexed $ zip [8,7..1] board
    putStrLn "   a b c d e f g h"
    where
        printRowIndexed (i, row) = do
            putStr $ show i ++ " |" -- Adiciona uma barra vertical antes de cada linha
            putStrLn $ intercalate "|" $ map showPiece row ++ [""]
        showPiece Empty      = " "
        showPiece (Pawn White) = "W"
        showPiece (Pawn Black) = "B"

-- Inicia o jogo
startMatch :: Board -> Player -> IO ()
startMatch board player = do
    printBoard board
    putStrLn $ "\nJogo iniciado para as pecas: " ++ show player

-- Define o tabuleiro inicial
fullBoard :: Board
fullBoard = [[pieceAt r c | c <- [0..7]] | r <- [0..7]]
  where
    pieceAt r c
      | (r + c) `mod` 2 == 0 && r < 3 = Pawn Black
      | (r + c) `mod` 2 == 0 && r > 4 = Pawn White
      | otherwise = Empty

-- Retirar os comentarios a seguir apos os membros terem visto essas outras opcoes!
-- Conforme pesquisas realizadas, seguem as versoes iniciais da funcao (fullBoard) para o preenchimento inicial do tabuleiro

{-  Versao 1

fullBoard = [
    [Empty, Pawn Black, Empty, Pawn Black, Empty, Pawn Black, Empty, Pawn Black],
    [Pawn Black, Empty, Pawn Black, Empty, Pawn Black, Empty, Pawn Black, Empty],
    [Empty, Pawn Black, Empty, Pawn Black, Empty, Pawn Black, Empty, Pawn Black],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
    [Pawn White, Empty, Pawn White, Empty, Pawn White, Empty, Pawn White, Empty],
    [Empty, Pawn White, Empty, Pawn White, Empty, Pawn White, Empty, Pawn White],
    [Pawn White, Empty, Pawn White, Empty, Pawn White, Empty, Pawn White, Empty]
    ]
-}

{-  Versao 2

fullBoard :: Board
fullBoard = [
    [if (r + c) `mod` 2 == 0 
        then if r < 3 
                then Pawn Black 
                else if r > 4 
                        then Pawn White 
                        else Empty 
        else Empty 
    | c <- [0..7]] 
    | r <- [0..7]]
-}

-- Funcao main
main :: IO ()
main = startMatch fullBoard White
