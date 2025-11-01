# Análise de Grafos em R
Este projeto oferece uma ferramenta completa para análise de grafos, permitindo carregar, visualizar e analisar estruturas de grafos a partir de arquivos de texto.

## 📋 Funcionalidades
Carregamento de Grafos: Suporte a grafos dirigidos (D) e não-dirigidos (ND) a partir de arquivos .txt

### Operações Básicas:

- Exibição de dados do grafo (tipo, vértices, arestas)
- Matriz de adjacência
- Verificação de adjacência entre vértices
- Cálculo de grau de vértices
- Busca de vizinhos
- Listagem de todas as arestas
- Visualização gráfica do grafo

## Análise de Comunidades:

- Algoritmo DFS para componentes conexos
- Algoritmo de Louvain para detecção de comunidades

## 🗂️ Estrutura de Arquivos
```text
projeto/
├── dados/          # Pasta com arquivos .txt dos grafos
├── plots/          # Pasta para salvar imagens geradas
├── library/        # Pasta para pacotes R instalados
└── script.R        # Script principal
```

## 📊 Formato do Arquivo de Entrada
O arquivo deve seguir este formato:

```text
D  ou  ND
1,2
1,3
2,4
3,4
```
- Primeira linha: "D" para dirigido, "ND" para não-dirigido
- Linhas seguintes: Pares de vértices separados por vírgula

## 🔍 Algoritmos de Busca de Comunidades

#### DFS (Depth-First Search)
- Objetivo: Identificar componentes conexos em grafos
- Funcionamento: Percorre o grafo em profundidade, agrupando vértices conectados
- Complexidade: O(V + E)
- Aplicação: Ideal para identificar componentes desconexos em grafos simples

#### Algoritmo de Louvain
- Objetivo: Detectar comunidades baseadas em modularidade
- Funcionamento: Algoritmo hierárquico que maximiza a modularidade através de agregação
- Complexidade: O(V log V)
- Aplicação: Melhor para encontrar estrutura de comunidades em grafos complexos

## 📈 Diferenças entre os Algoritmos
| Característica | DFS | Louvain |
|----------------|------|----------|
| **Objetivo** | Componentes conexos | Maximizar modularidade |
| **Complexidade** | Linear O(V+E) | Quase-linear O(V log V) |
| **Resultados** | Grupos por conectividade | Comunidades por densidade |
| **Aplicação** | Grafos simples | Grafos complexos |
| **Hierarquia** | Não | Sim |

# 🚀 Como Usar
- Coloque seus arquivos de grafo na pasta dados/
- Execute o script R
- Escolha o arquivo desejado
- Navegue pelo menu para realizar análises

## 📦 Dependências
- igraph - Para manipulação e visualização de grafos

O script gerencia automaticamente a instalação dos pacotes necessários.

## 💡 Exemplo de Uso
- Análise Básica: Verifique adjacência e graus dos vértices

- Visualização: Gere imagens do grafo e suas comunidades

- Comunidades: Compare resultados do DFS e Louvain

- Troca de Arquivos: Alterne entre diferentes grafos sem sair do programa

## 🎯 Resultados
- Relatórios textuais com métricas do grafo

- Imagens salvas automaticamente na pasta plots/

- Análise comparativa entre métodos de detecção de comunidades

### Exemplos usando o arquivo ```grafoNaoDirigito.txt```

![grafo não dirigido](/img/grafo_não_dirigido_20251101_152100.png)
![grafo busca dfs](/img/comunidades_não_dirigido_20251101_152102.png)
![grafo busca louvain](/img/comunidades_louvain_não_dirigido_20251101_152104.png)

Este projeto é ideal para aprendizado de teoria dos grafos e análise de redes sociais, biológicas ou quaisquer dados que possam ser representados como grafos.