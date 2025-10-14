# Função para limpar tela em diferentes sistemas operacionais
clear_console <- function() {
  os <- Sys.info()["sysname"]
  
  if (os == "Windows") {
    shell("cls")  # usa o shell do Windows em vez de system()
  } else {
    cat("\033[2J\033[H")  # limpa terminal no Linux/macOS
  }
  
  invisible()
}

# Função para pausa
aguardarRetorno <- function() {
  cat("\nPressione Enter para continuar...")
  readLines("stdin", n = 1)
}


# Função para configurar e carregar ambiente
inicializarAmbiente <- function() {
  diretorio <- getwd()
  caminho_lib <- file.path(diretorio, "library")
  
  # Cria pasta "library" se não existir
  if (!dir.exists(caminho_lib)) {
    dir.create(caminho_lib, recursive = TRUE)
    cat("📦 Pasta 'library' criada em:", caminho_lib, "\n")
  }
  
  # Define caminho local e repositório CRAN
  .libPaths(caminho_lib)
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  
  # Lista de pacotes necessários
  pacotes_necessarios <- c("igraph")
  
  # Instala apenas os que faltam
  pacotes_faltando <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages(lib.loc = caminho_lib)[, "Package"])]
  
  if (length(pacotes_faltando) > 0) {
    cat("🔧 Instalando pacotes faltantes:", paste(pacotes_faltando, collapse = ", "), "\n")
    install.packages(pacotes_faltando, lib = caminho_lib, quiet = TRUE)
  }
  
  # Carrega os pacotes
  lapply(pacotes_necessarios, library, character.only = TRUE)
  
  cat("Ambiente configurado e pacotes carregados!\n\n")

  clear_console()
}


exibirMatrizAdjacencia <- function(dados) {
  clear_console()
  cat("=== MATRIZ DE ADJACÊNCIA ===\n\n")
  
  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  vertices <- sort(unique(c(grafo$V1, grafo$V2)))
  
  # Cria matriz quadrada (vértices x vértices)
  matriz_adj <- matrix(0, nrow = length(vertices), ncol = length(vertices),
                       dimnames = list(vertices, vertices))
  
  # Preenche as conexões
  for (i in 1:nrow(grafo)) {
    origem <- as.character(grafo$V1[i])
    destino <- as.character(grafo$V2[i])
    matriz_adj[origem, destino] <- 1
    if (tipo == "Não Dirigido") {
      matriz_adj[destino, origem] <- 1
    }
  }
  
  print(matriz_adj)
  
  aguardarRetorno()
}


# Função que receberá os arquivo .txt conforme foi proposto nos requesitos
lerArquivo <- function() {
    cat("Insira o arquivo para ser analizado: \n")
    # Diretório do projeto
    diretorio <- getwd()
    cat("Diretório atual:", diretorio, "\n")

    cat("\n")
    # Lista arquivos .txt na pasta
    arquivos_txt <- list.files(path = diretorio, pattern = "\\.txt$", full.names = TRUE)

    if (length(arquivos_txt) == 0) {
    stop("Nenhum arquivo .txt encontrado na pasta.")
    }

    # Mostra arquivos disponíveis
    cat("Arquivos .txt disponíveis:\n")
    for (i in seq_along(arquivos_txt)) {
    cat(i, ":", basename(arquivos_txt[i]), "\n")
    }

    cat("\n")
    # Interação no terminal
    cat("Digite o número do arquivo que deseja ler: ")
    indice <- as.integer(readLines("stdin", n = 1))

    # Valida índice
    if (is.na(indice) || indice < 1 || indice > length(arquivos_txt)) {
    stop("Índice inválido!")
    }

    arquivo <- arquivos_txt[indice]

    return(arquivo)
}

obterDadosGrafo <- function(arquivo) {

  if (!file.exists(arquivo)) {
    stop("Arquivo não encontrado!") 
  }

  # Lê primeira linha (tipo do grafo)
  primeira_linha <- readLines(arquivo, n = 1)
  tipo_grafo <- trimws(primeira_linha)

  if (!(tipo_grafo %in% c("D", "ND"))) {
    stop("A primeira linha deve ser 'D' (dirigido) ou 'ND' (não dirigido).")
  }

  # Lê as arestas
  grafo <- read.table(arquivo, skip = 1, sep = ",", header = FALSE)

  if (ncol(grafo) != 2) {
    stop("Erro: O arquivo deve conter pares de vértices (origem, destino) em cada linha.")
  }

  if (any(grafo$V1 == grafo$V2)) {
    stop("Erro: Existem arestas com o mesmo vértice de origem e destino.")
  }

  num_arestas <- nrow(grafo)
  vertices_unicos <- unique(c(grafo$V1, grafo$V2))
  num_vertices <- length(vertices_unicos)

  resultado <- list(
    tipo = ifelse(tipo_grafo == "D", "Dirigido", "Não Dirigido"),
    num_vertices = num_vertices,
    num_arestas = num_arestas,
    vertices = vertices_unicos,
    matriz_arestas = grafo
  )

  return(resultado)
}

plotarGrafo <- function(dados) {
  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  nome_arquivo <- paste0("grafo_", tolower(tipo), ".png")
  
  g <- if (tipo == "Dirigido") {
    igraph::graph_from_edgelist(as.matrix(grafo), directed = TRUE)
  } else {
    igraph::graph_from_edgelist(as.matrix(grafo), directed = FALSE)
  }
  
  png(nome_arquivo, width = 800, height = 600)
  plot(g,
       main = paste("Grafo", tipo),
       vertex.color = "lightblue",
       vertex.size = 20,
       vertex.label.cex = 1.2,
       edge.arrow.size = 0.5,
       layout = igraph::layout_with_kk)
  dev.off()
  
  cat("\nGrafo salvo como imagem em:", normalizePath(nome_arquivo), "\n")
}

exibirDadosGrafo <- function(dados) {
  clear_console()
  cat("=== DADOS DO GRAFO ===\n\n")
  cat("Tipo:", dados$tipo, "\n")
  cat("Número de vértices:", dados$num_vertices, "\n")
  cat("Número de arestas:", dados$num_arestas, "\n")
  cat("Vértices:", paste(sort(dados$vertices), collapse = ", "), "\n")
  aguardarRetorno()
}

verificarAdjacencia <- function(grafo, tipo) {
  clear_console()
  cat("=== VERIFICAR ADJACÊNCIA ===\n\n")
  
  # Mostra os vértices disponíveis
  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")
  
  cat("Digite vértice X: ")
  vX <- as.integer(readLines("stdin", n = 1))
  cat("Digite vértice Y: ")
  vY <- as.integer(readLines("stdin", n = 1))
  
  if (any(grafo$V1 == vX & grafo$V2 == vY)) {
    cat("\nVértices", vX, "e", vY, "são adjacentes.\n")
  } else {
    cat("\nVértices", vX, "e", vY, "não são adjacentes.\n")
  }
  
  aguardarRetorno()
}


calcularGrau <- function(grafo, tipo) {
  clear_console()
  cat("=== CALCULAR GRAU DO VÉRTICE ===\n\n")
  
  # Exibe os vértices disponíveis
  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")
  
  cat("Digite o vértice: ")
  v <- trimws(readLines("stdin", n = 1))  # mantém como texto
  
  if (!(v %in% vertices_disponiveis)) {
    cat("\nVértice não encontrado no grafo!\n")
    aguardarRetorno()
    return()
  }
  
  if (tipo == "Dirigido") {
    grau_saida <- sum(grafo$V1 == v)
    grau_entrada <- sum(grafo$V2 == v)
    cat("\nGrau de saída:", grau_saida, "\n")
    cat("Grau de entrada:", grau_entrada, "\n")
    cat("Grau total:", grau_saida + grau_entrada, "\n")
  } else {
    grau <- sum(grafo$V1 == v | grafo$V2 == v)
    cat("\nGrau do vértice", v, ":", grau, "\n")
  }
  
  aguardarRetorno()
}



buscarVizinhos <- function(grafo, tipo) {
  clear_console()
  cat("=== BUSCAR VIZINHOS ===\n\n")
  
  # Exibe os vértices disponíveis
  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")
  
  cat("Digite o vértice (pode ser letra ou número): ")
  v <- trimws(readLines("stdin", n = 1))  # mantém como texto
  
  if (!(v %in% vertices_disponiveis)) {
    cat("\nVértice não encontrado no grafo!\n")
    aguardarRetorno()
    return()
  }
  
  vizinhos <- unique(grafo$V2[grafo$V1 == v])
  if (tipo == "Não Dirigido") {
    vizinhos <- unique(c(vizinhos, grafo$V1[grafo$V2 == v]))
  }
  
  if (length(vizinhos) == 0) {
    cat("\nO vértice", v, "não possui vizinhos.\n")
  } else {
    cat("\nVizinhos do vértice", v, ":", paste(vizinhos, collapse = ", "), "\n")
  }
  
  aguardarRetorno()
}


visitarArestas <- function(grafo) {
  clear_console()
  cat("=== TODAS AS ARESTAS DO GRAFO ===\n\n")
  for (i in 1:nrow(grafo)) {
    cat(grafo$V1[i], "->", grafo$V2[i], "\n")
  }
  aguardarRetorno()
}

exibirGraficamente <- function(dados) {
  clear_console()
  cat("=== EXIBINDO GRAFO GRAFICAMENTE ===\n\n")
  tryCatch({
    plotarGrafo(dados)
    cat("Grafo exibido com sucesso!\n")
  }, error = function(e) {
    cat("Erro ao plotar grafo:", e$message, "\n")
  })
  aguardarRetorno()
}

# Função para pré-processamento dos dados do arquivo
main <- function() {

  inicializarAmbiente()
  arquivo <- lerArquivo()
  dados <- obterDadosGrafo(arquivo)

  return(dados)
}

# MENU PRINCIPAL
menu <- function(dados) {
  grafo <- dados$matriz_arestas
  vertices <- dados$vertices
  tipo <- dados$tipo
  
  opcao <- -1
  while (opcao != 0) {
    clear_console()
    cat("=== ANÁLISE DE GRAFOS ===\n\n")
    cat("1 - Ver dados do grafo\n")
    cat("2 - Verificar adjacência entre dois vértices\n")
    cat("3 - Calcular grau de um vértice\n")
    cat("4 - Buscar vizinhos de um vértice\n")
    cat("5 - Visitar todas as arestas do grafo\n")
    cat("6 - Exibir grafo graficamente\n")
    cat("7 - Exibir matriz de adjacência\n")
    cat("0 - Sair\n")
    
    cat("\nEscolha uma opção: ")
    entrada <- readLines("stdin", n = 1)
    opcao <- suppressWarnings(as.integer(entrada))
    
    if (is.na(opcao)) {
      cat("Opção inválida. Tente novamente.\n")
      aguardarRetorno()
      next
    }
    
    switch(as.character(opcao),
      "1" = exibirDadosGrafo(dados),
      "2" = verificarAdjacencia(grafo, tipo),
      "3" = calcularGrau(grafo, tipo),
      "4" = buscarVizinhos(grafo, tipo),
      "5" = visitarArestas(grafo),
      "6" = exibirGraficamente(dados),
      "7" = exibirMatrizAdjacencia(dados),
      "0" = { cat("Encerrando programa...\n"); break },
      {
        cat("Opção inválida. Tente novamente.\n")
        aguardarRetorno()
      }
    )
  }
}

# Execução principal
dados <- main()
menu(dados)