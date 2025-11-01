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
  clear_console()
}

# Criar pasta para salvar imagens geradas
criarPastaPlots <- function(pasta = "plots") {
  caminho <- file.path(getwd(), pasta)
  if (!dir.exists(caminho)) {
    dir.create(caminho, recursive = TRUE)
    cat("📁 Pasta criada para salvar imagens em:", caminho, "\n")
  } else {
    cat("📂 Pasta existente para salvar imagens:", caminho, "\n")
  }
  return(caminho)
}


# ==============================================================
# === FUNÇÕES DE LEITURA E PRÉ-PROCESSAMENTO ===================
# ==============================================================

lerArquivo <- function(pasta = "dados") {
  # Caminho completo da pasta
  diretorio <- file.path(getwd(), pasta)

  # Verifica se a pasta existe
  if (!dir.exists(diretorio)) {
    stop(paste("Pasta", pasta, "não encontrada!"))
  }

  # Lista arquivos .txt
  arquivos_txt <- list.files(path = diretorio, pattern = "\\.txt$", full.names = TRUE)

  if (length(arquivos_txt) == 0) stop("Nenhum arquivo .txt encontrado na pasta!")

  cat("📄 Arquivos disponíveis:\n \n")
  for (i in seq_along(arquivos_txt)) {
    cat(i, ":", basename(arquivos_txt[i]), "\n")
  }

  cat("\nDigite o número do arquivo que deseja ler: ")
  indice <- as.integer(readLines("stdin", n = 1))
  if (is.na(indice) || indice < 1 || indice > length(arquivos_txt)) stop("⚠️ Índice inválido!")

  caminho_escolhido <- arquivos_txt[indice]
  cat("\n✅ Arquivo selecionado:", basename(caminho_escolhido), "\n")
  return(caminho_escolhido)
  aguardarRetorno()
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

  # Garante que a pasta "plots" exista
  pasta_plots <- criarPastaPlots("plots")

  # Nome do arquivo com data/hora para evitar sobrescrita
  nome_arquivo <- paste0(
    "grafo_", tolower(gsub(" ", "_", tipo)), "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"
  )
  caminho_saida <- file.path(pasta_plots, nome_arquivo)

  g <- if (tipo == "Dirigido")
    igraph::graph_from_edgelist(as.matrix(grafo), directed = TRUE)
  else
    igraph::graph_from_edgelist(as.matrix(grafo), directed = FALSE)

  png(caminho_saida, width = 800, height = 600)
  plot(
    g,
    main = paste("Grafo", tipo),
    vertex.color = "lightblue",
    vertex.size = 20,
    vertex.label.cex = 1.2,
    edge.arrow.size = 0.5,
    layout = igraph::layout_with_kk
  )
  dev.off()

  cat("\nGrafo salvo em:", normalizePath(caminho_saida), "\n")
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

  # --- Captura de todos os vértices ---
  vertices <- unique(c(as.character(grafo$V1), as.character(grafo$V2)))

  # --- Construção da lista de adjacência completa ---
  adj <- setNames(vector("list", length(vertices)), vertices)
  for (i in seq_len(nrow(grafo))) {
    v1 <- as.character(grafo$V1[i])
    v2 <- as.character(grafo$V2[i])

    adj[[v1]] <- unique(c(adj[[v1]], v2))
    if (tipo == "Não Dirigido") {
      adj[[v2]] <- unique(c(adj[[v2]], v1))
    }
  }

  # Garante que vértices isolados existam na lista
  for (v in vertices) {
    if (length(adj[[v]]) == 0) adj[[v]] <- character(0)
  }

  visitado <- setNames(rep(FALSE, length(vertices)), vertices)
  comunidades <- list()

  dfs <- function(v, grupo) {
    visitado[[v]] <<- TRUE
    grupo <- c(grupo, v)
    for (viz in adj[[v]]) {
      if (!visitado[[viz]]) grupo <- dfs(viz, grupo)
    }
    return(grupo)
  }

  # --- Busca por comunidades (componentes conexos) ---
  for (v in vertices) {
    if (!visitado[[v]]) {
      comunidade <- dfs(v, c())
      comunidades <- append(comunidades, list(sort(comunidade)))
    }
  }

  cat("Número de comunidades encontradas:", length(comunidades), "\n\n")
  for (i in seq_along(comunidades)) {
    cat("Comunidade", i, ":", paste(comunidades[[i]], collapse = ", "), "\n")
  }

  # --- Plotar e salvar resultado ---
  pasta_plots <- criarPastaPlots("plots")
  nome_arquivo <- paste0(
    "comunidades_", tolower(gsub(" ", "_", tipo)), "_",
    format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"
  )
  caminho_saida <- file.path(pasta_plots, nome_arquivo)

  g <- if (tipo == "Dirigido")
    igraph::graph_from_edgelist(as.matrix(grafo), directed = TRUE)
  else
    igraph::graph_from_edgelist(as.matrix(grafo), directed = FALSE)

  paleta <- rainbow(length(comunidades))
  cores_vertices <- rep("gray80", length(V(g)))
  nomes_vertices <- V(g)$name

  for (i in seq_along(comunidades)) {
    membros <- comunidades[[i]]
    cores_vertices[nomes_vertices %in% membros] <- paleta[i]
  }

  png(caminho_saida, width = 900, height = 700)
  plot(
    g,
    vertex.color = cores_vertices,
    vertex.size = 25,
    vertex.label.cex = 1.2,
    edge.arrow.size = 0.5,
    layout = igraph::layout_with_fr,
    main = paste("Comunidades Detectadas -", tipo)
  )
  legend(
    "bottomleft",
    legend = paste("Comunidade", seq_along(comunidades)),
    col = paleta, pch = 19, bty = "n", cex = 0.9
  )
  dev.off()

  cat("\nImagem com comunidades salva em:", normalizePath(caminho_saida), "\n")

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
