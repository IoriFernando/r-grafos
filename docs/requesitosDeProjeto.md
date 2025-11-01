# PROJETO: IMPLEMENTAÇÃO E ANÁLISE DE GRAFOS

## VISÃO GERAL DO PROJETO

## PARTE 01 - IMPLEMENTAÇÃO DE REPRESENTAÇÃO DE GRAFOS

Desenvolvimento de um sistema completo para representação, análise e visualização de grafos, dividido em duas partes principais: implementação de estruturas de dados e algoritmos de busca, seguida de visualização gráfica.

### 1. ESTRUTURAS DE DADOS OBRIGATÓRIAS
Nesse projeto será utilizada a estrutura de dados: ```Matriz de Adjacência```

### 2. ALGORITMO DE BUSCA E PROBLEMA
Para solucionar Analisador de Comunidades em Redes Sociais será usado o algoritmos de  ```Busca em Profundidade (DFS)``` pois:

- Permite identificar componentes conectados em uma rede;
- Facilita a exploração completa de cada grupo antes de passar para o próximo;
- É a base de algoritmos de clustering como Louvain, Girvan-Newman e Label Propagation (que usam DFS internamente para varrer subgrafos).


### 3. LEITURA DE ARQUIVO DE ENTRADA

Formato do arquivo .txt:
```bash
D/ND
v1,v2
v3,v4
v1,v3
...
```
#### Especificações:
- 1ª Linha: "D" (Dirigido) ou "ND" (Não Dirigido)
- Linhas seguintes: Arestas no formato "vértice_origem,vértice_destino"

#### 4. FUNCIONALIDADES OBRIGATÓRIAS

Após leitura e armazenamento, o sistema deve:
- Verificar adjacência entre dois vértices (vX e vY)
- Calcular grau de um vértice qualquer
- Buscar todos os vizinhos de um vértice
- Visitar todas as arestas do grafo

## PARTE 02 - VISUALIZAÇÃO DE GRAFOS

REQUISITOS DE VISUALIZAÇÃO
- Gerar novo arquivo de texto a partir do grafo armazenado
- Realizar apresentação gráfica do grafo
- Permitido usar APIs ou códigos prontos para visualização