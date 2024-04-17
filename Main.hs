import Data.List
import Data.Maybe

-- Define os tipos de dados para o jogador e o tabuleiro
data Player = X | O deriving (Eq, Show)
type Board = [[Maybe Player]]

-- Função principal para iniciar o jogo
main :: IO ()
main = do
    putStrLn "Bem-vindo ao Jogo da Velha!"
    putStrLn "Jogador X começa!"
    playGame initialBoard X

-- Tabuleiro inicial vazio
initialBoard :: Board
initialBoard = replicate 3 (replicate 3 Nothing)

-- Função para exibir o tabuleiro
printBoard :: Board -> IO ()
printBoard = mapM_ (putStrLn . intercalate " | " . map showRow)
  where
    showRow :: Maybe Player -> String
    showRow Nothing = " "
    showRow (Just player) = show player

-- Função para jogar o jogo
playGame :: Board -> Player -> IO ()
playGame board player = do
    putStrLn $ "\nTabuleiro atual:"
    printBoard board
    putStrLn $ "\nJogador " ++ show player ++ ", sua vez. Por favor, escolha uma linha e coluna (de 0 a 2), separadas por espaço:"
    move <- getLine
    let [row, col] = map read (words move)
    if validMove board row col
        then do
            let newBoard = updateBoard board row col player
            if checkWin newBoard player
                then putStrLn $ "\nJogador " ++ show player ++ " venceu!"
                else if fullBoard newBoard
                    then putStrLn "O jogo terminou em empate."
                    else playGame newBoard (nextPlayer player)
        else do
            putStrLn "Movimento inválido. Por favor, tente novamente."
            playGame board player

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
