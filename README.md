# Análise de Grafos em R

Projeto em R para análise de grafos a partir de arquivos .txt, com suporte a grafos dirigidos e não dirigidos, exibição de dados, cálculo de grau de vértices, vizinhos, matriz de adjacência e plotagem gráfica.

# Funcionalidades
- Ler arquivos .txt contendo grafos (primeira linha: D para dirigido ou ND para não dirigido; demais linhas: pares de vértices separados por vírgula).
- Exibir informações do grafo: número de vértices, número de arestas, lista de vértices. - Verificar adjacência entre dois vértices.
- Calcular grau de vértices (entrada, saída e total para grafos dirigidos).
- Buscar vizinhos de um vértice.
- Listar todas as arestas do grafo.
- Exibir graficamente o grafo e salvar como imagem .png.
- Exibir matriz de adjacência.

# Requisitos

- ```R >= 4.0```
- Pacote igraph (instalado automaticamente na pasta library do projeto).
- Sistema operacional Windows 

# Como usar

- Coloque os arquivos .txt de grafos na mesma pasta do projeto.
- Execute o script principal no R:
```bash
Rscript main.r
```
- Siga o menu interativo no terminal.

```text
Estrutura dos arquivos .txt
D
1,2
2,3
3,1
```

- Primeira linha: tipo do grafo (D ou ND)

- Demais linhas: pares de vértices (origem,destino)

## Observações

- Suporta grafos com vértices numéricos ou alfanuméricos.

- O script cria uma pasta library para dependências locais.

| Método      | Tipo de Detecção                           | Ideal para                        |
| ----------- | ------------------------------------------ | --------------------------------- |
| **DFS**     | Componentes desconectados                  | Testes simples e grafos separados |
| **Louvain** | Comunidades densas (maximiza modularidade) | Redes grandes e conectadas        |
