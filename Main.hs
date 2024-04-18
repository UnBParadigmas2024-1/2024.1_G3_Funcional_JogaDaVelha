import Data.List (intercalate)
import Data.Char (chr, ord)


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
isValidPosition :: (Char, Char) -> Bool
isValidPosition (c, r) = c `elem` ['a' .. 'h'] && r `elem` ['1' .. '8']

-- Converte uma posição em coordenadas
coordsFromPosition :: String -> (Int, Int)
coordsFromPosition [c, r] = (fromEnum r - 49, fromEnum c - 97)
coordsFromPosition _ = error "A posição deve ser uma string de dois caracteres: coluna e linha."

-- Função para verificar se o movimento é válido
isValidMove :: (Int, Int) -> (Int, Int) -> Board -> Player -> Bool
isValidMove (x1, y1) (x2, y2) board player =
  isValidPosition (c1, c3) && isValidPosition (c2, c4) &&
  abs (x2 - x1) == 1 && abs (y2 - y1) == 1 &&
  isEmpty (x2, y2) board &&
  isOwnPiece (x1, y1) board player
  where
    c1 = chr (x1 + 97)
    c2 = chr (x2 + 97)
    c3 = chr (y1 + 49)
    c4 = chr (y2 + 49)

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
  let [char, numStr] = words pos
  let column1 = head char
  let column2 = head numStr
  if isValidPosition (column1, column2)
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
  let [char, numStr] = words startPos
  let column1 = head char
  let column2 = head numStr
  let x1 = fromEnum column1 - 97
  let y1 = fromEnum column2 - 49
  print x1
  print y1
  let [char, numStr] = words endPos
  let column1 = head char
  let column2 = head numStr
  let x2 = fromEnum column1 - 97
  let y2 = fromEnum column2 - 49
  print x2
  print y2
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