import Control.Exception
import System.IO
import System.IO.Error
import System.Process
import Data.List
import Data.Function

main = do
  putStrLn "Vamos jogar? Digite lig4 no console"

-- definição dos tipos dos dados
type Jogadores = [Jogador]
type Nome = String
type Vitorias = Int
type Empates = Int
type Derrotas = Int
type VezDoPrimeiro = Bool
type Senha = String
type Tabela = [Char]
data Jogador = Jogador Nome Senha Vitorias Empates Derrotas deriving (Show, Read)


-- Função que recebe uma String e retorna uma IO String
getString :: String -> IO String
getString str = do
 putStr str
 res <- getLine
 return res


-- Função que inicia o programa
lig4 :: IO ()
lig4 = do
 arq <- openFile "Jogadores.txt" ReadMode; -- abre o arquivo para leitura
 dados <- hGetLine arq; -- ler o conteúdo do arquivo
 hClose arq; -- fecha o arquivo
 menu (read dados); -- passa os dados para a função menu
 return ()


-- Função que exibe o Menu Interativo
menu :: Jogadores -> IO Jogadores
menu dados = do
  putStrLn "\n-------------------------------- VAMOS JOGAR LIG4 !!! --------------------------------"
  putStrLn "\nDigite 1 para fazer o seu cadastro"
  putStrLn "Digite 2 para jogar"
  putStrLn "Digite 3 para visualizar o ranking"
  putStrLn "Digite 4 para alterar a sua senha"
  putStrLn "Digite 5 para deletar o seu cadastro"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  op <- getChar
  getChar -- descarta o Enter
  executarOpcao dados op


-- Função para manipular a opção escolhida pelo usuário
executarOpcao :: Jogadores -> Char -> IO Jogadores
executarOpcao dados '1' = cadastrarJogador dados
executarOpcao dados '2' = defineJogadores dados
executarOpcao dados '3' = do
 putStrLn "\nRanking dos jogadores:"
 putStrLn "Vitória vale 3 pontos, empate vale 1 e derrota -1:"
 if null dados then do
  putStrLn "Não há jogadores cadastrados!"
  menu dados

 else do
  -- a função ordenar ordena crescentemente pelo Geral
  putStrLn "\nJogador | Geral  Jogos  Vitórias  Empates  Derrotas"
  exibirRanking (reverse (ordenar dados))
  putStr "\nPressione <Enter> para voltar ao menu..."
  getChar
  menu dados

executarOpcao dados '0' = do
 putStrLn "Até logo!\n"
 return dados

executarOpcao dados '4' = do 
 nome <- getString "\nDigite seu nome: "
 senha <- getString "Digite sua senha: "

 if (checaSenha dados nome senha) then do
  nova_senha <- getString "\nDigite sua nova senha: "
 
  -- abre o arquivo para escrita
  arq <- openFile "Jogadores.txt" WriteMode 
  hPutStrLn arq (show (alterarSenha dados nome senha nova_senha))
  hClose arq -- fecha o arquivo

  -- abre o arquivo para leitura
  arq_leitura <- openFile "Jogadores.txt" ReadMode
  dados_atualizados <- hGetLine arq_leitura
  hClose arq_leitura

  putStrLn "\nSenha atualizada com sucesso!"
  menu (read dados_atualizados)

 else do
  putStrLn "\nNome ou senha incorretos!"
  menu dados 

executarOpcao dados '5' = do 
 nome <- getString "\nDigite seu nome: "
 senha <- getString "Digite sua senha: "

 if (checaSenha dados nome senha) then do
 
  -- abre o arquivo para escrita
  arq <- openFile "Jogadores.txt" WriteMode 
  hPutStrLn arq (show (deletaJogador dados nome senha))
  hClose arq -- fecha o arquivo

  -- abre o arquivo para leitura
  arq_leitura <- openFile "Jogadores.txt" ReadMode
  dados_atualizados <- hGetLine arq_leitura
  hClose arq_leitura

  putStrLn "\nJogador deletado com sucesso!"
  menu (read dados_atualizados)

 else do
  putStrLn "\nNome ou senha incorretos!"
  menu dados 

executarOpcao dados _ = do
 putStrLn "\nOpção inválida! Tente novamente..."
 putStr "\nPressione <Enter> para voltar ao menu..."
 getChar
 menu dados



-- Função que realiza o cadastro de um novo jogador
cadastrarJogador :: Jogadores -> IO Jogadores
cadastrarJogador dados = do
 nome <- getString "\nDigite um nome de usuário: "
 
 if existeJogador dados nome then do
  putStrLn "\nEsse nome já existe, escolha outro."
  putStr "\nPressione <Enter> para continuar..."
  getChar
  menu dados
 else do
  senha <- getString "\nDigite sua senha: "
  conf_senha <- getString "\nConfirme a sua senha: "
  
  if senha /= conf_senha then do
   putStrLn "\nSua confirmação deve ser igual à original!"
   menu dados
  else do
   arq <- openFile "Jogadores.txt" WriteMode -- abre o arquivo para escrita
   hPutStrLn arq (show ((Jogador nome senha 0 0 0):dados))
   hClose arq -- fecha o arquivo
   putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso.")
   putStr "\nPressione <Enter> para continuar..."
   getChar
   menu ((Jogador nome senha 0 0 0):dados) -- retorna a nova lista para o menu


-- Função que tira um jogador do arquivo .txt
deletaJogador :: Jogadores -> String -> String -> Jogadores
deletaJogador [] _ _ = []
deletaJogador ((Jogador nome s v e d):xs) n senha
 | nome == n && s == senha = (deletaJogador xs n senha)
 | otherwise = (Jogador nome s v e d):(deletaJogador xs n senha)

-- Função que altera a senha de um jogador
alterarSenha :: Jogadores -> String -> String -> String -> Jogadores
alterarSenha [] _ _ _ = []
alterarSenha ((Jogador nome s v e d):xs) n senha ns 
 | nome == n && s == senha = (Jogador nome ns v e d) : (alterarSenha xs n senha ns)
 | otherwise = (Jogador nome s v e d):(alterarSenha xs n senha ns)


-- Função que exibe o ranking dos jogadores cadastrados
exibirRanking :: Jogadores -> IO ()
exibirRanking [] = return ()
exibirRanking (x:xs) = do
 putStrLn (n ++ "        " ++ show (3*v + e - d) ++ "        " ++ show (v + e + d) ++ "        " ++ show (v) ++ "        " ++ show (e) ++ "        " ++ show (d))
 exibirRanking xs
 where
  n = obterNome x
  v = obterVitorias x
  e = obterEmpates x
  d = obterDerrotas x
 

-- Função que define os jogadores que irão jogar 
defineJogadores :: Jogadores -> IO Jogadores
defineJogadores dados = do
 jogador1 <- getString "\nDigite o nome do primeiro jogador: "

 -- testa se o jogador1 existe
 if not (existeJogador dados jogador1) then do
  putStrLn "\nEsse jogador não existe!"
  putStr "\nPressione <Enter> para continuar..."
  getChar -- descarta o Enter
  menu dados

 else do
  --putStrLn "\nOlá, " ++ jogador1 ++"!"
  senha1 <- getString "Digite sua senha: "
  if not (checaSenha dados jogador1 senha1) then do
   putStrLn "\nSenha incorreta!"
   menu dados
  else do
   jogador2 <- getString "\nDigite o nome do segundo jogador: "
   if not (existeJogador dados jogador2) then do
    putStrLn "\nEsse jogador não existe!"
    putStr "\nPressione <Enter> para continuar..."
    getChar -- descarta o Enter
    menu dados
   else do
    --putStrLn "\nOlá, " ++ jogador2 ++"!"
    senha2 <- getString "Digite sua senha: "
    if not (checaSenha dados jogador2 senha2) then do
     putStrLn "\nSenha incorreta!"
     menu dados
    else do
     -- se chegou aqui, é porque os dois jogadores existem
     novoJogo dados jogador1 jogador2


-- Função que inicia um novo jogo
novoJogo :: Jogadores -> Nome -> Nome -> IO Jogadores
novoJogo dados jogador1 jogador2 = do
 putStrLn ("\nIniciando o jogo \"" ++ jogador1 ++ " vs " ++ jogador2 ++ "\" ... ")
 putStrLn ("\n" ++ jogador1 ++ " será o \'1\' e " ++ jogador2 ++ " será o \'2\'. Vamos lá!!")
 novaRodada dados ['-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-'] jogador1 jogador2 True

-- Função que inicia uma nova rodada do jogo
novaRodada :: Jogadores -> Tabela -> Nome -> Nome -> VezDoPrimeiro -> IO Jogadores
novaRodada dados tabela jogador1 jogador2 vez = do
 -- imprime o tabuleiro
 putStrLn "Tabuleiro"
 putStrLn ("\n\n" ++ "                              " ++
          (show (tabela !! 12)) ++ " | " ++ (show (tabela !! 13)) ++ " | " ++ (show (tabela !! 14)) ++ " | " ++ (show (tabela !! 15)) ++ "\n                              -----------------------\n" ++ "                              " ++
          (show (tabela !! 8)) ++ " | " ++ (show (tabela !! 9)) ++ " | " ++ (show (tabela !! 10)) ++ " | " ++ (show (tabela !! 11)) ++ "\n                              -----------------------\n" ++ "                              " ++
          (show (tabela !! 4)) ++ " | " ++ (show (tabela !! 5)) ++ " | " ++ (show (tabela !! 6)) ++ " | " ++ (show (tabela !! 7)) ++ "\n                              -----------------------\n" ++ "                              " ++
          (show (tabela !! 0)) ++ " | " ++ (show (tabela !! 1)) ++ " | " ++ (show (tabela !! 2)) ++ " | " ++ (show (tabela !! 3)) ++ "\n                              -----------------------\n" ++ "                              " ++ "\n                     Coluna    1     2     3     4\n" )

 -- verifica se o jogador1 venceu
 if (venceu tabela '1') then do
  putStrLn ("Parábens " ++ jogador1 ++ "! Você venceu!!")

  -- abre o arquivo para escrita para atualizá-lo
  arq_escrita <- openFile "Jogadores.txt" WriteMode
  hPutStrLn arq_escrita (show (atualizaVitoriaDerrota dados jogador1 jogador2))
  hClose arq_escrita

  -- abre o arquivo para leitura
  arq_leitura <- openFile "Jogadores.txt" ReadMode
  dados_atualizados <- hGetLine arq_leitura
  hClose arq_leitura

  putStr "\nPressione <Enter> para voltar ao menu..."
  getChar
  menu (read dados_atualizados)

 -- verifica se o jogador2 venceu
 else if (venceu tabela '2') then do
   putStrLn ("Parábens " ++ jogador2 ++ "! Você venceu!!")

   -- abre o arquivo para escrita para atualizá-lo
   arq_escrita <- openFile "Jogadores.txt" WriteMode
   hPutStrLn arq_escrita (show (atualizaVitoriaDerrota dados jogador2 jogador1))
   hClose arq_escrita

   -- abre o arquivo para leitura
   arq_leitura <- openFile "Jogadores.txt" ReadMode
   dados_atualizados <- hGetLine arq_leitura
   hClose arq_leitura

   putStr "\nPressione <Enter> para voltar ao menu..."
   getChar
   menu (read dados_atualizados)

 -- verifica se houve empate
 else if (null [x | x <- tabela, not (x == '1' || x == '2')]) then do
  
  -- abre o arquivo para escrita para atualizá-lo
  arq_escrita <- openFile "Jogadores.txt" WriteMode
  hPutStrLn arq_escrita (show (atualizaEmpate dados jogador1 jogador2))
  hClose arq_escrita

  -- abre o arquivo para leitura
  arq_leitura <- openFile "Jogadores.txt" ReadMode
  dados_atualizados <- hGetLine arq_leitura
  hClose arq_leitura
  putStrLn ("Deu empate!")
  putStr "\nPressione <Enter> para voltar ao menu..."
  getChar
  menu (read dados_atualizados)

 -- verifica se a vez é do jogador1
 else if (vez) then do
  putStr (jogador1 ++ ", é a sua vez! Escolha a coluna que quer colocar a próxima peça! ")
  op <- getChar
  getChar -- descarta o Enter

  -- testa se a opção é válida
  if not (elem op ['1', '2', '3', '4']) then do
   putStrLn "\nEssa opção NÃO é válida, tente novamente..."
   -- como foi opção inválida, então ainda é a vez do jogador1
   novaRodada dados tabela jogador1 jogador2 True
  
  else if ((op == '1' && not(tabela !! 12 == '-')) || (op == '2' && not(tabela !! 13 == '-')) || (op == '3' && not(tabela !! 14 == '-')) || (op == '4' && not(tabela !! 15 == '-'))) then do
   putStrLn "\nEsta coluna já foi totalmente preenchida, escolha outra opção..."
   novaRodada dados tabela jogador1 jogador2 True
       
  else
   -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
   -- a nova tabela será o retorno da função obterNovoTabuleiro
   novaRodada dados (obterNovoTabuleiro tabela '1' op 1) jogador1 jogador2 False

 else do
  putStr (jogador2 ++ ", é a sua vez! Escolha a coluna que quer colocar a próxima peça! ")
  op <- getChar
  getChar -- descarta o Enter
  if not (elem op ['1', '2', '3', '4']) then do
   putStrLn "\nEssa opção NÃO é válida, tente novamente..."

   -- como foi opção inválida, então ainda é a vez do jogador2
   novaRodada dados tabela jogador1 jogador2 False

  else if ((op == '1' && not(tabela !! 12 == '-')) || (op == '2' && not(tabela !! 13 == '-')) || (op == '3' && not(tabela !! 14 == '-')) || (op == '4' && not(tabela !! 15 == '-'))) then do
   putStrLn "\nEsta coluna já foi totalmente preenchida, escolha outra opção..."
   novaRodada dados tabela jogador1 jogador2 False
  else
   -- se caiu aqui é porque a opção é válida e ainda NÃO foi marcada
   -- a nova tabela será o retorno da função obterNovoTabuleiro
   novaRodada dados (obterNovoTabuleiro tabela '2' op 1) jogador1 jogador2 True


-- Função que identifica se algum jogador venceu
venceu :: Tabela -> Char -> Bool
venceu tabela c

 -- verifica primeiro nas linhas
 | (((tabela !! 0) == c) && ((tabela !! 1) == c) && ((tabela !! 2) == c) && (tabela !! 3) == c) = True
 | (((tabela !! 4) == c) && ((tabela !! 5) == c) && ((tabela !! 6) == c) && (tabela !! 7) == c) = True
 | (((tabela !! 8) == c) && ((tabela !! 9) == c) && ((tabela !! 10) == c) && (tabela !! 11) == c) = True
 | (((tabela !! 12) == c) && ((tabela !! 13) == c) && ((tabela !! 14) == c) && (tabela !! 15) == c) = True

 -- verifica nas colunas
 | (((tabela !! 0) == c) && ((tabela !! 4) == c) && ((tabela !! 8) == c) && (tabela !! 12) == c) = True
 | (((tabela !! 1) == c) && ((tabela !! 5) == c) && ((tabela !! 9) == c) && (tabela !! 13) == c) = True
 | (((tabela !! 2) == c) && ((tabela !! 6) == c) && ((tabela !! 10) == c) && (tabela !! 14) == c) = True
 | (((tabela !! 3) == c) && ((tabela !! 7) == c) && ((tabela !! 11) == c) && (tabela !! 15) == c) = True

 -- verifica nas diagonais
 | (((tabela !! 0) == c) && ((tabela !! 5) == c) && ((tabela !! 10) == c) && (tabela !! 15) == c) = True
 | (((tabela !! 12) == c) && ((tabela !! 9) == c) && ((tabela !! 6) == c) && (tabela !! 3) == c) = True
 | otherwise = False

-- Função que modifica o tabuleiro de acordo com a jogada feita
obterNovoTabuleiro :: Tabela -> Char -> Char -> Int -> Tabela
obterNovoTabuleiro [] _ _ _ = []
obterNovoTabuleiro (x:xs) c e i
 | ((e == '1') && x == '-' && (elem i [1, 5, 9, 13])) = ( [c] ++ xs)
 | ((e == '2') && x == '-' && (elem i [2, 6, 10, 14])) = ( [c] ++ xs)
 | ((e == '3') && x == '-' && (elem i [3, 7, 11, 15])) = ( [c] ++ xs)
 | ((e == '4') && x == '-' && (elem i [4, 8, 12, 16])) = ( [c] ++ xs)
 | otherwise = x:(obterNovoTabuleiro xs c e (i + 1))


-- Função que checa se a senha que um jogador escreveu corresponde à sua senha
checaSenha :: Jogadores -> String -> String -> Bool
checaSenha [] _ _ = False
checaSenha ((Jogador n s v e d):xs) nome senha
 | n == nome && s == senha = True
 | n == nome && (s /= senha) = False
 | otherwise = checaSenha xs nome senha


-- Função que atualiza os números de vitórias e derrotas para o vencedor e perdedor, respectivamente
atualizaVitoriaDerrota :: Jogadores -> String -> String -> Jogadores
atualizaVitoriaDerrota [] _ _ = []
atualizaVitoriaDerrota ((Jogador nome s v e d):xs) vencedor perdedor
 | (nome == vencedor) = [(Jogador nome s (v + 1) e d)] ++ (atualizaVitoriaDerrota xs vencedor perdedor)
 | (nome == perdedor) = [(Jogador nome s v e (d + 1))] ++ (atualizaVitoriaDerrota xs vencedor perdedor)
 | otherwise = (Jogador nome s v e d):(atualizaVitoriaDerrota xs vencedor perdedor)

-- Função que atualiza o número de empates para ambos
atualizaEmpate :: Jogadores -> String -> String -> Jogadores
atualizaEmpate [] _ _ = []
atualizaEmpate ((Jogador nome s v e d):xs) j1 j2
 | (nome == j1 || nome == j2) = [(Jogador nome s v (e + 1) d)] ++ xs
 | otherwise = (Jogador nome s v e d):(atualizaEmpate xs j1 j2)

-- Função que retorna o nome de um jogador
obterNome :: Jogador -> String
obterNome (Jogador n _ _ _ _ ) = n

-- Função que verifica se o nome de um jogador está cadastrado
existeJogador :: Jogadores -> Nome -> Bool
existeJogador [] _ = False
existeJogador ((Jogador n s v e d):xs) nome
 | n == nome = True
 | otherwise = existeJogador xs nome

-- Função que retorna o número de vitórias de um jogador
obterVitorias :: Jogador -> Vitorias
obterVitorias (Jogador _ _ v _ _ ) = v

-- Função que retorna o número de empates de um jogador
obterEmpates :: Jogador -> Empates
obterEmpates (Jogador _ _ _ e _ ) = e

-- Função que retorna o número de derrotas de um jogador
obterDerrotas :: Jogador -> Derrotas
obterDerrotas (Jogador _ _ _ _ d ) = d

-- Função que retorna a pontuação geral de um jogador
obterGeral :: Jogador -> Int
obterGeral (Jogador n s v e d) = 3*v + e - d

-- Função que ordena uma lista de jogadores
ordenar :: Jogadores -> Jogadores
ordenar dados = sortBy (compare `on` obterGeral) dados
