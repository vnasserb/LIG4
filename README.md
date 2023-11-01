# README - Jogo de LIG4 em Haskell

## Descrição do Jogo
Este é um programa que implementa o jogo de LIG4 (também conhecido como "Connect Four") em Haskell. O jogo é projetado para ser jogado por dois jogadores, onde cada jogador insere suas peças em um tabuleiro vertical com 4 colunas e 4 linhas. O objetivo do jogo é ser o primeiro a conectar quatro de suas peças consecutivas (horizontal, vertical ou diagonalmente) no tabuleiro. Os jogadores alternam suas jogadas, escolhendo uma coluna na qual inserir sua peça, e o jogo continua até que um jogador vença ou o tabuleiro esteja completamente preenchido, resultando em um empate.

## Como Jogar
1. Execute o programa Haskell.
2. O jogo solicitará que cada jogador faça um cadastro com seus nomes e escolham uma senha. Estas credenciais serão registrados em um arquivo `Jogadores.txt` para manter um registro.
3. Os jogadores alternam suas jogadas, escolhendo a coluna na qual desejam inserir sua peça (representada por X ou O).
4. O jogo verifica se houve um vencedor após cada jogada.
5. O jogo termina quando um jogador ganha ou quando o tabuleiro está completamente preenchido (empate).
6. O resultado do jogo será exibido, e os nomes dos jogadores e o resultado serão registrados no arquivo `Jogadores.txt`.

## Arquivo de Cadastro de Jogadores
Os nomes dos jogadores e o resultado de cada jogo serão registrados em um arquivo `.txt` chamado "registro_jogadores.txt". O arquivo conterá informações sobre os jogadores e o resultado de cada jogo para que você possa manter um registro de quem ganhou e perdeu.

## Executando o Jogo
Para executar o jogo, siga estas etapas:

1. Abra um terminal.
2. Navegue até o diretório onde o programa Haskell está localizado.
3. Execute o programa Haskell.

```bash
ghc -o LIG4 LIG4.hs
./LIG4
```

## Requisitos
- GHC (Glasgow Haskell Compiler) instalado para compilar e executar o código Haskell.

## Autor
Este jogo de LIG4 em Haskell foi criado por Vinicius Nasser Bernaldo e é um projeto de código aberto.

## Contribuições
Contribuições são bem-vindas! Sinta-se à vontade para fazer melhorias no código ou adicionar novos recursos ao jogo. Se você quiser contribuir, crie um fork do repositório, faça suas alterações e envie uma solicitação pull.

Espero que você aproveite o jogo! Se tiver alguma dúvida ou precisar de ajuda, não hesite em entrar em contato.
