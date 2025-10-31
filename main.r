# ==============================================================
# === FUNÇÕES DE AMBIENTE E CONFIGURAÇÃO ========================
# ==============================================================

# Limpar tela no terminal (Windows, Linux, macOS)
clear_console <- function() {
  os <- Sys.info()["sysname"]
  if (os == "Windows") {
    shell("cls")
  } else {
    cat("\033[2J\033[H")
  }
  invisible()
}

# Pausar execução aguardando Enter
aguardarRetorno <- function() {
  cat("\nPressione Enter para continuar...")
  readLines("stdin", n = 1)
}

# Configurar ambiente local e pacotes
inicializarAmbiente <- function() {
  diretorio <- getwd()
  caminho_lib <- file.path(diretorio, "library")

  if (!dir.exists(caminho_lib)) {
    dir.create(caminho_lib, recursive = TRUE)
    cat("📦 Pasta 'library' criada em:", caminho_lib, "\n")
  }

  .libPaths(caminho_lib)
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  pacotes_necessarios <- c("igraph")
  pacotes_faltando <- pacotes_necessarios[!(pacotes_necessarios %in%
    installed.packages(lib.loc = caminho_lib)[, "Package"])]

  if (length(pacotes_faltando) > 0) {
    cat("🔧 Instalando pacotes faltantes:", paste(pacotes_faltando, collapse = ", "), "\n")
    install.packages(pacotes_faltando, lib = caminho_lib, quiet = TRUE)
  }

  lapply(pacotes_necessarios, library, character.only = TRUE)

  cat("✅ Ambiente configurado e pacotes carregados!\n\n")
  aguardarRetorno()
  clear_console()
}


# ==============================================================
# === FUNÇÕES DE LEITURA E PRÉ-PROCESSAMENTO ===================
# ==============================================================

lerArquivo <- function() {
  cat("Insira o arquivo para ser analisado:\n")
  diretorio <- getwd()
  cat("Diretório atual:", diretorio, "\n\n")

  arquivos_txt <- list.files(path = diretorio, pattern = "\\.txt$", full.names = TRUE)
  if (length(arquivos_txt) == 0) stop("Nenhum arquivo .txt encontrado na pasta.")

  cat("Arquivos .txt disponíveis:\n")
  for (i in seq_along(arquivos_txt)) {
    cat(i, ":", basename(arquivos_txt[i]), "\n")
  }

  cat("\nDigite o número do arquivo que deseja ler: ")
  indice <- as.integer(readLines("stdin", n = 1))
  if (is.na(indice) || indice < 1 || indice > length(arquivos_txt)) stop("Índice inválido!")

  return(arquivos_txt[indice])
}

# Obter dados do grafo (tipo, vértices, arestas)
obterDadosGrafo <- function(arquivo) {
  if (!file.exists(arquivo)) stop("Arquivo não encontrado!")

  primeira_linha <- readLines(arquivo, n = 1)
  tipo_grafo <- trimws(primeira_linha)
  if (!(tipo_grafo %in% c("D", "ND"))) stop("A primeira linha deve ser 'D' ou 'ND'.")

  grafo <- read.table(arquivo, skip = 1, sep = ",", header = FALSE)
  if (ncol(grafo) != 2) stop("Erro: O arquivo deve conter pares de vértices (origem, destino).")
  if (any(grafo$V1 == grafo$V2)) stop("Erro: Arestas com mesmo vértice de origem e destino.")

  list(
    tipo = ifelse(tipo_grafo == "D", "Dirigido", "Não Dirigido"),
    num_vertices = length(unique(c(grafo$V1, grafo$V2))),
    num_arestas = nrow(grafo),
    vertices = unique(c(grafo$V1, grafo$V2)),
    matriz_arestas = grafo
  )
}


# ==============================================================
# === FUNÇÕES DO MENU PRINCIPAL ================================
# ==============================================================

# 1️⃣ Exibir dados do grafo
exibirDadosGrafo <- function(dados) {
  clear_console()
  cat("=== DADOS DO GRAFO ===\n\n")
  cat("Tipo:", dados$tipo, "\n")
  cat("Número de vértices:", dados$num_vertices, "\n")
  cat("Número de arestas:", dados$num_arestas, "\n")
  cat("Vértices:", paste(sort(dados$vertices), collapse = ", "), "\n")
  aguardarRetorno()
}

# 2️⃣ Exibir matriz de adjacência
exibirMatrizAdjacencia <- function(dados) {
  clear_console()
  cat("=== MATRIZ DE ADJACÊNCIA ===\n\n")

  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  vertices <- sort(unique(c(grafo$V1, grafo$V2)))

  matriz_adj <- matrix(0, nrow = length(vertices), ncol = length(vertices),
                       dimnames = list(vertices, vertices))

  for (i in 1:nrow(grafo)) {
    origem <- as.character(grafo$V1[i])
    destino <- as.character(grafo$V2[i])
    matriz_adj[origem, destino] <- 1
    if (tipo == "Não Dirigido") matriz_adj[destino, origem] <- 1
  }

  print(matriz_adj)
  aguardarRetorno()
}

# 3️⃣ Verificar adjacência entre dois vértices
verificarAdjacencia <- function(grafo, tipo) {
  clear_console()
  cat("=== VERIFICAR ADJACÊNCIA ===\n\n")

  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")

  cat("Digite vértice X: ")
  vX <- as.integer(readLines("stdin", n = 1))
  cat("Digite vértice Y: ")
  vY <- as.integer(readLines("stdin", n = 1))

  if (any(grafo$V1 == vX & grafo$V2 == vY))
    cat("\nVértices", vX, "e", vY, "são adjacentes.\n")
  else
    cat("\nVértices", vX, "e", vY, "não são adjacentes.\n")

  aguardarRetorno()
}

# 4️⃣ Calcular grau de um vértice
calcularGrau <- function(grafo, tipo) {
  clear_console()
  cat("=== CALCULAR GRAU DO VÉRTICE ===\n\n")

  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")

  cat("Digite o vértice: ")
  v <- trimws(readLines("stdin", n = 1))

  if (!(v %in% vertices_disponiveis)) {
    cat("\nVértice não encontrado!\n")
    aguardarRetorno()
    return()
  }

  if (tipo == "Dirigido") {
    grau_saida <- sum(grafo$V1 == v)
    grau_entrada <- sum(grafo$V2 == v)
    cat("\nGrau de saída:", grau_saida, "\nGrau de entrada:", grau_entrada,
        "\nGrau total:", grau_saida + grau_entrada, "\n")
  } else {
    grau <- sum(grafo$V1 == v | grafo$V2 == v)
    cat("\nGrau do vértice", v, ":", grau, "\n")
  }

  aguardarRetorno()
}

# 5️⃣ Buscar vizinhos de um vértice
buscarVizinhos <- function(grafo, tipo) {
  clear_console()
  cat("=== BUSCAR VIZINHOS ===\n\n")

  vertices_disponiveis <- sort(unique(c(grafo$V1, grafo$V2)))
  cat("Vértices disponíveis:", paste(vertices_disponiveis, collapse = ", "), "\n\n")

  cat("Digite o vértice: ")
  v <- trimws(readLines("stdin", n = 1))

  if (!(v %in% vertices_disponiveis)) {
    cat("\nVértice não encontrado!\n")
    aguardarRetorno()
    return()
  }

  vizinhos <- unique(grafo$V2[grafo$V1 == v])
  if (tipo == "Não Dirigido") vizinhos <- unique(c(vizinhos, grafo$V1[grafo$V2 == v]))

  if (length(vizinhos) == 0)
    cat("\nO vértice", v, "não possui vizinhos.\n")
  else
    cat("\nVizinhos de", v, ":", paste(vizinhos, collapse = ", "), "\n")

  aguardarRetorno()
}

# 6️⃣ Visitar todas as arestas
visitarArestas <- function(grafo) {
  clear_console()
  cat("=== TODAS AS ARESTAS DO GRAFO ===\n\n")
  for (i in 1:nrow(grafo)) cat(grafo$V1[i], "->", grafo$V2[i], "\n")
  aguardarRetorno()
}

# 7️⃣ Exibir grafo graficamente
plotarGrafo <- function(dados) {
  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  nome_arquivo <- paste0("grafo_", tolower(tipo), ".png")

  g <- if (tipo == "Dirigido")
    igraph::graph_from_edgelist(as.matrix(grafo), directed = TRUE)
  else
    igraph::graph_from_edgelist(as.matrix(grafo), directed = FALSE)

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

exibirGraficamente <- function(dados) {
  clear_console()
  cat("=== EXIBINDO GRAFO GRAFICAMENTE ===\n\n")
  tryCatch({
    plotarGrafo(dados)
    cat("✅ Grafo exibido com sucesso!\n")
  }, error = function(e) {
    cat("Erro ao plotar:", e$message, "\n")
  })
  aguardarRetorno()
}


# ==============================================================
# === ANÁLISE DE COMUNIDADES (DFS) =============================
# ==============================================================

dfsComunidades <- function(dados) {
  clear_console()
  cat("=== ANÁLISE DE COMUNIDADES (DFS) ===\n\n")

  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  vertices <- sort(unique(c(grafo$V1, grafo$V2)))

  adj <- setNames(vector("list", length(vertices)), vertices)
  for (i in 1:nrow(grafo)) {
    v1 <- as.character(grafo$V1[i])
    v2 <- as.character(grafo$V2[i])
    adj[[v1]] <- unique(c(adj[[v1]], v2))
    if (tipo == "Não Dirigido") adj[[v2]] <- unique(c(adj[[v2]], v1))
  }

  visitado <- setNames(rep(FALSE, length(vertices)), vertices)
  comunidades <- list()

  dfs <- function(v, grupo) {
    visitado[[v]] <<- TRUE
    grupo <- c(grupo, v)
    for (viz in adj[[v]]) {
      if (!visitado[[viz]]) grupo <- dfs(viz, grupo)
    }
    grupo
  }

  for (v in vertices) {
    if (!visitado[[v]]) {
      comunidade <- dfs(v, c())
      comunidades <- append(comunidades, list(comunidade))
    }
  }

  cat("Número de comunidades encontradas:", length(comunidades), "\n\n")
  for (i in seq_along(comunidades))
    cat("Comunidade", i, ":", paste(comunidades[[i]], collapse = ", "), "\n")

  aguardarRetorno()
}


# ==============================================================
# === EXECUÇÃO PRINCIPAL E MENU ================================
# ==============================================================

main <- function() {
  inicializarAmbiente()
  arquivo <- lerArquivo()
  obterDadosGrafo(arquivo)
}

menu <- function(dados) {
  grafo <- dados$matriz_arestas
  tipo <- dados$tipo
  opcao <- -1

  while (opcao != 0) {
    clear_console()
    cat("=== ANÁLISE DE GRAFOS ===\n\n")
    cat("1 - Ver dados do grafo\n")
    cat("2 - Exibir matriz de adjacência\n")
    cat("3 - Verificar adjacência entre vértices\n")
    cat("4 - Calcular grau de um vértice\n")
    cat("5 - Buscar vizinhos de um vértice\n")
    cat("6 - Visitar todas as arestas\n")
    cat("7 - Exibir grafo graficamente\n")
    cat("8 - Analisar comunidades (DFS)\n")
    cat("0 - Sair\n")

    cat("\nEscolha uma opção: ")
    opcao <- suppressWarnings(as.integer(readLines("stdin", n = 1)))
    if (is.na(opcao)) next

    switch(as.character(opcao),
      "1" = exibirDadosGrafo(dados),
      "2" = exibirMatrizAdjacencia(dados),
      "3" = verificarAdjacencia(grafo, tipo),
      "4" = calcularGrau(grafo, tipo),
      "5" = buscarVizinhos(grafo, tipo),
      "6" = visitarArestas(grafo),
      "7" = exibirGraficamente(dados),
      "8" = dfsComunidades(dados),
      "0" = { cat("Encerrando programa...\n"); break },
      { cat("Opção inválida!\n"); aguardarRetorno() }
    )
  }
}

# Execução
dados <- main()
menu(dados)
