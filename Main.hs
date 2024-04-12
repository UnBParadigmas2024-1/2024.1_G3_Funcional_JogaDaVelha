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

-- Função para validar uma posição no tabuleiro
isValidPosition :: String -> Bool
isValidPosition [c, r] = c `elem` ['a'..'h'] && r `elem` ['1'..'8']
isValidPosition _ = False

-- Função para verificar se o movimento é válido
isValidMove :: (Int, Int) -> (Int, Int) -> Board -> Player -> Bool
isValidMove (x1, y1) (x2, y2) board player =
    isValidPosition [toEnum (y1 + 97), toEnum (x1 + 49)] &&
    isValidPosition [toEnum (y2 + 97), toEnum (x2 + 49)] &&
    abs (x2 - x1) == 1 && abs (y2 - y1) == 1 &&
    isEmptyOrEnemyPiece (x2, y2) board player &&
    isOwnPiece (x1, y1) board player
    where
        isEmptyOrEnemyPiece (x, y) board player = case board !! x !! y of
            Empty -> True
            Pawn p -> p /= player
        isOwnPiece (x, y) board player = case board !! x !! y of
            Pawn p -> p == player
            _ -> False


-- Função para ler uma posição válida
readValidPosition :: IO String
readValidPosition = do
    pos <- getLine
    if isValidPosition pos
        then return pos
        else do
            putStrLn "Posição inválida. Tente novamente:"
            readValidPosition 

-- Inicia o jogo
startMatch :: Board -> Player -> IO ()
startMatch board player = do
    printBoard board
    putStrLn $ "\nJogo iniciado para as peças: "  ++ show player ++ " \nEscolha a posiçao da peça para movimentar: "
    startPos <- readValidPosition
    putStrLn $ "Você selecionou a posição: " ++ startPos
    putStrLn $ "Agora selecione o movimento para a peça "++ startPos ++ "->: " 
    endPos <- readValidPosition
    putStrLn $ "Movimento: " ++ startPos ++ " -> " ++ endPos
    let (x1, y1) = coordsFromPosition startPos
        (x2, y2) = coordsFromPosition endPos
    if isValidMove (x1, y1) (x2, y2) board player
        then putStrLn "Movimento válido!"
        else putStrLn "Movimento inválido."

-- Converte posição para coordenadas
coordsFromPosition :: String -> (Int, Int)
coordsFromPosition [c, r] = (fromEnum r - 49, fromEnum c - 97)

-- Define o tabuleiro inicial
fullBoard :: Board
fullBoard = [[pieceAt r c | c <- [0..7]] | r <- [0..7]]
  where
    pieceAt r c
      | even (r + c) && r < 3 = Pawn Black
      | even (r + c) && r > 4 = Pawn White
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
