# LIG4
Jogo de LIG4 em Haskell. Neste jogo, as credenciais (nome e senha) de cada jogador são adicionadas em um arquivo texto chamado "Jogadores.txt".

- **Regras do jogo**
  - Uma partida contém dois jogadores, que jogam alternadamente, depositando uma peça no tabuleiro. O jogo acaba quando um deles formar uma linha de quatro peças na horizontal, vertical ou diagonal.
  - O tabuleiro é uma matriz 4x4, onde cada jogador escolhe a coluna em que vai depositar a sua peça, sendo que esta ocupará a linha mais alta dessa coluna não ocupada.
  - Após o final do jogo, o vencedor recebe uma vitória a mais e o perdedor, uma derrota a mais, sendo que o arquivo texto é atualizado com estes contadores. 
