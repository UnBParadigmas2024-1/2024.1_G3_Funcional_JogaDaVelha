import Data.List
import Data.Maybe
import Control.Exception

-- Define os tipos de dados para o jogador e o tabuleiro
data Player = X | O deriving (Eq, Show)
type Board = [[Maybe Player]]

-- Função principal para iniciar o jogo
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Jogo da Velha!\n"
    putStrLn "Jogador X começa!"
    playGame initialBoard X

-- Tabuleiro inicial vazio
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

-- Exibe o tabuleiro
printBoard :: Board -> IO ()
printBoard board = do
    putStrLn "\n  1 | 2 | 3 "
    putStrLn "------------"
    mapM_ putStrLn (zipWith printRow [1..] board)
    putStrLn "------------"

-- Imprime linhas do tabuleiro
printRow :: Int -> [Maybe Player] -> String
printRow i row = show i ++ " " ++ intercalate " | " (map showRow row)

-- Imprime conteúdo das casas do tabuleiro
showRow :: Maybe Player -> String
showRow Nothing = " "
showRow (Just player) = show player

-- Função para jogar o jogo
playGame :: Board -> Player -> IO ()
playGame board player = do
    putStrLn $ "\nTabuleiro atual:"
    printBoard board
    putStrLn $ "\nJogador " ++ show player ++ ", sua vez. Por favor, escolha uma coluna e linha (de 1 a 3), separadas por espaço:"
    move <- getLine
    let parseMove = do
            let [colStr, rowStr] = words move
                row = read rowStr - 1
                col = read colStr - 1
            if validMove board row col
                then return (row, col)
                else throw InvalidMoveException
    (row, col) <- catch parseMove (\e -> handleInvalidMove e board player)
    let newBoard = updateBoard board row col player
    if checkWin newBoard player
        then do
            printBoard newBoard
            putStrLn $ "\nJogador " ++ show player ++ " venceu!"
        else if fullBoard newBoard
            then do
                printBoard newBoard                
                putStrLn "O jogo terminou em empate."
            else playGame newBoard (nextPlayer player)

-- Verifica se o movimento é válido
validMove :: Board -> Int -> Int -> Bool
validMove board row col = row >= 0 && row < 3 && col >= 0 && col < 3 && isEmptyCell board row col

-- Verifica se a célula está vazia
isEmptyCell :: Board -> Int -> Int -> Bool
isEmptyCell board row col = isNothing $ (board !! row) !! col

-- Atualiza o tabuleiro com o movimento do jogador
updateBoard :: Board -> Int -> Int -> Player -> Board
updateBoard board row col player = 
    let (rowBefore, currentRow:rowAfter) = splitAt row board
        newRow = take col currentRow ++ [Just player] ++ drop (col + 1) currentRow
    in rowBefore ++ [newRow] ++ rowAfter

-- Verifica se há um vencedor
checkWin :: Board -> Player -> Bool
checkWin board player =
    any (\row -> all (== Just player) row) (rows ++ cols ++ diags)
  where
    rows = board
    cols = transpose board
    diags = [[(board !! i) !! i | i <- [0..2]], [(board !! i) !! (2 - i) | i <- [0..2]]]

-- Verifica se o tabuleiro está cheio
fullBoard :: Board -> Bool
fullBoard = all (all isJust)

-- Retorna o próximo jogador
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

-- Exceção para movimento inválido
data InvalidMoveException = InvalidMoveException deriving Show

instance Exception InvalidMoveException

-- Manipula movimento inválido
handleInvalidMove :: InvalidMoveException -> Board -> Player -> IO (Int, Int)
handleInvalidMove _ board player = do
    putStrLn "\nMovimento inválido. Por favor, tente novamente!"
    playGame board player
    return (0, 0)  -- Retornando um par de coordenadas fictícias para satisfazer o tipo IO (Int, Int)
