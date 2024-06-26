
Paradigmas:
 - Funcional


# Jogo Da Velha

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo (de acordo com a Planilha de Divisão dos Grupos)**: 03<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 17/0007413  |  Caio Santos |
| 21/1031664  |  Catlen Cleane Ferreira de Oliveira |
| 20/2023681  |  Gabriel da Silva Rosa |
| 21/2008197  |  João Pedro Alves Machado |
| 19/0032821  |  Lorenzo Santos |
| 21/1043718  |  Paulo Victor Fonseca Sousa |
| 19/0020521  |  Valderson Pontes da Silva Junior |
| 190038900  |  Victor de Souza Cabral |

## Sobre 
Esse projeto visa criar um jogo da velha utilizando a linguagem Haskell, uma linguagem  de programação funcional. O jogo da velha, embora seja muito antigo, se popularizou na Inglaterra no século 19, e as regras do jogo são simples. Duas pessoas jogam alternadamente, preenchendo cada um dos espaços vazios. Cada participante deve usar um símbolo (X ou O). Vence o jogador que conseguir formar primeiro uma linha com três símbolos iguais, seja ela na horizontal, vertical ou diagonal
## Screenshots

Tela inicial:

![Inicio do jogo](/assets/Screenshot1.PNG)

Inicio do jogo

![Inicio do jogo](/assets/screenshot2.PNG)

Final do jogo

![Inicio do jogo](/assets/screenshot3.PNG)

## Instalação 
**Linguagens**:  Haskell<br>
**Tecnologias**: GHC (Glasgow Haskell Compiler) e Stack <br>

É necessário um compilador de Haskell, Sendo recomendado o encontrado [aqui](https://www.haskell.org/downloads/). Caso seja utilizado esse link, para rodar um código utilizando o compilador, use esse [manual de primeiros passos](https://www.haskell.org/ghcup/steps/) para rodar o projeto

## **Uso** 
Para utilizar o projeto do jogo da velha , siga os seguintes passos
### Passos Iniciais
* 1- Verifique se o GHC está Instalado:
Digite ghc --version para verificar se o GHC está instalado. Você deverá ver a versão do GHC se ele estiver instalado corretamente.
* 2- Abra o terminal no seu sistema operacional.
* 3-Navegue até o Diretório do Projeto:
* 4- Use o comando cd para navegar até o diretório onde você salvou o arquivo do projeto. Por exemplo:
cd caminho/para/seu/projeto
* 5-Compile o Código:Compile o código com o GHC usando o comando:ghc -o JogoDaVelha Main.hs

* 6-Execute o Jogo:
Após a compilação, execute o jogo com o comando:
./JogoDaVelha

### Jogando
O jogo começará e o primeiro jogador (X) será solicitado a fazer uma jogada.
Você deve inserir a coluna e a linha desejadas, separadas por um espaço. Por exemplo, para colocar na primeira coluna e na primeira linha, digite 1 1.

Continue seguindo as instruções no terminal, alternando entre os jogadores até que o jogo termine com um vencedor ou um empate.

O jogo anunciará o vencedor ou se o jogo terminou em empate e então terminará a execução.

## Vídeo

Link para o vídeo: https://www.youtube.com/watch?v=1aiKBMgHhHc

Arquivo também presente no repositório.

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
| Nome do Membro                          | Contribuição | Significância da Contribuição para o Projeto |
| --------------------------------------- | ------------ | -------------------------------------------- |
| Caio Santos                             |              |                                    |
| Catlen Cleane Ferreira de Oliveira      |Desenvolvimento de Tratamento de Exceções| Excelente                                    |
| Gabriel da Silva Rosa                   | Desenvolvimento da lógica do jogo da velha e modificação dos participantes do readme                               | Excelente                                    |
| João Pedro Alves Machado                | Criação de validações e modelagem na primeira versao do trabalho (jogo de damas), discussão e participação em grupo           | Excelente                                    |
| Lorenzo Santos                          | Melhoria na visualização, apresentação e lógica do tabuleiro e tratamento de execução do código em empates                   | Excelente                                    |
| Paulo Victor Fonseca Sousa              | Aprimoramento e ajuste das funções "play game", "continue game", e "handleInvalidMove", criação do readme          | Excelente                                       |
| Valderson Pontes da Silva Junior        |              |                                   |
| Victor de Souza Cabral                  | Melhoria na visualização, apresentação e lógica do tabuleiro, entrada de dados e mensagens guias                   | Excelente                                    |

## Outros 

LIÇÕES APRENDIDAS:

- Desenvolvimento da linguagem Haskell: Aprendemos os principais conceitos em sala de aula e ao desenvolver o jogo nos deparamos com algumas questões, e assim tivemos a oportunidade de nos apronfundar mais no assunto.
- Modelagem de Dados: Aprendemos a representar o tabuleiro, executar as jogadas, verificação de jogadas inválidas e tratamento de exceções, através de funções.

PERCEPÇÕES

- O haskell é uma linguaguem muito interessante de se aprender, facilita muitas coisas que geralmente estamos acostumados e dificulta em outras, abordadando os problemas de uma maneira única.
- Ao desenvolver, muitas vezes descobrimos que o que parecia difícil era mais simples do que imaginávamos, enquanto em outras ocasiões subestimamos a complexidade de certos problemas.

CONTRIBUIÇÕES E FRAGILIDADES:

- No início escolhemos como tema o jogo de Damas, mas tivemos muita dificuldade durante o desenvolvimento do código no movimento das peças e regras do jogo, e devido ao tempo que tinhamos disponível para entrega decidimos mudar para o jogo da Velha.

TRABALHOS FUTUROS:

- Desenvolvimento de uma interface para o jogo. Seria necessário fazer algumas adaptações na lógica do código, mas é bem possível implementar uma interface para esse jogo (conforme observado nas fontes a seguir).
- Uma ideia interessante seria colocar um menu, na qual seria possível escolher entre o jogo sendo apresentado pelo terminal ou pela interface.

## Fontes
https://github.com/haskell-game/fungen
https://youtu.be/VxLvaHpAK-U?si=ZnWa6YIc7reyycBO
https://youtu.be/gCVMkKgs3uQ?si=lmxzhyY83AoMypmQ

Material de estudo:

https://www.youtube.com/watch?v=5tCN7Q4qK9Q&list=PLfdR3_dt2rbctdMHwZG2h4aKGROujEG3j
