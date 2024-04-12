import Data.List (intercalate)

-- Definicao dos tipos de dados
data Player = White | Black deriving (Eq, Show)

data Piece = Empty | Pawn Player deriving (Eq, Show)

type Board = [[Piece]]

-- Printa o tabuleiro
printBoard :: Board -> IO ()
printBoard board = do
  mapM_ printRowIndexed $ zip [8, 7 .. 1] board
  putStrLn "   a b c d e f g h"
  where
    printRowIndexed (i, row) = do
      putStr $ show i ++ " |" -- Adiciona uma barra vertical antes de cada linha
      putStrLn $ intercalate "|" $ map showPiece row ++ [""]
    showPiece Empty = " "
    showPiece (Pawn White) = "W"
    showPiece (Pawn Black) = "B"

-- Função para validar uma posição no tabuleiro
isValidPosition :: String -> Bool
isValidPosition [c, r] = c `elem` ['a' .. 'h'] && r `elem` ['1' .. '8']
isValidPosition _ = False

-- Converte posição para coordenadas
coordsFromPosition :: String -> (Int, Int)
coordsFromPosition [c, r] = (fromEnum r - 49, fromEnum c - 97)

-- Função para verificar se o movimento é válido
isValidMove :: (Int, Int) -> (Int, Int) -> Board -> Player -> Bool
isValidMove (x1, y1) (x2, y2) board player =
  isValidPosition [toEnum (y1 + 97), toEnum (x1 + 49)]
    && isValidPosition [toEnum (y2 + 97), toEnum (x2 + 49)]
    && abs (x2 - x1) == 1
    && abs (y2 - y1) == 1
    && isEmpty (x2, y2) board -- Movimento diagonal de uma casa
    && isOwnPiece (x1, y1) board player -- Posição final vazia
    -- Peça pertence ao jogador atual

isEmpty :: (Int, Int) -> Board -> Bool
isEmpty (x, y) board = case board !! x !! y of
  Empty -> True
  _ -> False

isOwnPiece :: (Int, Int) -> Board -> Player -> Bool
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
  moveLoop board player

moveLoop :: Board -> Player -> IO ()
moveLoop board player = do
  printBoard board
  putStrLn $ "\nJogador " ++ show player ++ " escolha a peça para movimentar: "
  startPos <- readValidPosition
  putStrLn $ "Você selecionou a posição: " ++ startPos
  putStrLn $ "Agora selecione o movimento para a peça " ++ startPos ++ "->: "
  endPos <- readValidPosition
  putStrLn $ "Movimento: " ++ startPos ++ " -> " ++ endPos
  let (x1, y1) = coordsFromPosition startPos
  let (x2, y2) = coordsFromPosition endPos
  if isValidMove (x1, y1) (x2, y2) board player
    then putStrLn "Movimento válido!"
    else do
      putStrLn "\nMovimento inválido."
      putStrLn "\n\n------------------\n\n"
      moveLoop board player

-- Define o tabuleiro inicial
fullBoard :: Board
fullBoard = [[pieceAt r c | c <- [0 .. 7]] | r <- [0 .. 7]]
  where
    pieceAt r c
      | even (r + c) && r < 3 = Pawn Black
      | even (r + c) && r > 4 = Pawn White
      | otherwise = Empty

-- Funcao main
main :: IO ()
main = startMatch fullBoard White
