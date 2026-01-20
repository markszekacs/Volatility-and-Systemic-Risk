suppressPackageStartupMessages({
  library(igraph)
})

# Build an adjacency matrix from normalized FEVD or NET matrix.
# - If mode = "to": use directional spillovers TO others (column-based in norm_fevd).
# - If mode = "from": use directional spillovers FROM others (row-based in norm_fevd).
# - If mode = "net": use NET_MATRIX = t(norm_fevd) - norm_fevd.
build_adjacency <- function(norm_fevd = NULL, net_matrix = NULL, mode = c("net", "norm"), diag_zero = TRUE) {
  mode <- match.arg(mode)
  
  if (mode == "net") {
    if (is.null(net_matrix)) stop("Provide net_matrix for mode='net'.")
    A <- net_matrix
  } else {
    if (is.null(norm_fevd)) stop("Provide norm_fevd for mode='norm'.")
    A <- norm_fevd
  }
  
  A <- as.matrix(A)
  if (diag_zero) diag(A) <- 0
  A
}

# Apply an absolute threshold to prune weak edges.
# If threshold is NULL, no pruning is applied.
threshold_adjacency <- function(A, threshold = NULL) {
  A <- as.matrix(A)
  if (!is.null(threshold)) {
    A[abs(A) < threshold] <- 0
  }
  A
}

# Convert adjacency matrix to an igraph object.
# If directed = TRUE, keep directionality; otherwise symmetrize first.
adjacency_to_graph <- function(A, directed = TRUE, symmetrize = FALSE) {
  A <- as.matrix(A)
  
  if (!directed && symmetrize) {
    A <- (A + t(A)) / 2
  }
  
  igraph::graph_from_adjacency_matrix(
    A,
    mode = if (directed) "directed" else "undirected",
    weighted = TRUE,
    diag = FALSE
  )
}

# Compute standard network metrics from a weighted directed graph.
compute_network_metrics <- function(g) {
  if (!inherits(g, "igraph")) stop("g must be an igraph object.")
  
  w <- igraph::E(g)$weight
  
  # Strength = weighted degree.
  in_strength  <- igraph::strength(g, mode = "in", weights = w)
  out_strength <- igraph::strength(g, mode = "out", weights = w)
  total_strength <- igraph::strength(g, mode = "all", weights = w)
  
  # Centralities (weighted where supported).
  # Note: eigenvector_centrality supports weights for directed graphs.
  eigen <- igraph::eigen_centrality(g, weights = w, directed = igraph::is_directed(g))$vector
  
  # Betweenness typically treats weights as distances, so we invert weights for a "cost" graph.
  # Add a small epsilon to avoid division by zero.
  eps <- 1e-12
  inv_w <- 1 / (abs(w) + eps)
  igraph::E(g)$inv_weight <- inv_w
  betw <- igraph::betweenness(g, directed = igraph::is_directed(g), weights = igraph::E(g)$inv_weight, normalized = TRUE)
  
  data.frame(
    node = igraph::V(g)$name,
    in_strength = as.numeric(in_strength),
    out_strength = as.numeric(out_strength),
    total_strength = as.numeric(total_strength),
    eigen_centrality = as.numeric(eigen),
    betweenness = as.numeric(betw),
    stringsAsFactors = FALSE
  )
}

# Full pipeline for a single (static) connectedness matrix.
compute_network_from_connectedness <- function(norm_fevd = NULL, net_matrix = NULL,
                                               mode = c("net", "norm"),
                                               threshold = NULL,
                                               directed = TRUE,
                                               symmetrize = FALSE) {
  mode <- match.arg(mode)
  
  A <- build_adjacency(norm_fevd = norm_fevd, net_matrix = net_matrix, mode = mode, diag_zero = TRUE)
  A <- threshold_adjacency(A, threshold = threshold)
  
  g <- adjacency_to_graph(A, directed = directed, symmetrize = symmetrize)
  metrics <- compute_network_metrics(g)
  
  list(
    adjacency = A,
    graph = g,
    metrics = metrics
  )
}

# Export adjacency and metrics to disk.
export_network <- function(net_res, base_path_no_ext) {
  if (!is.list(net_res) || is.null(net_res$adjacency) || is.null(net_res$metrics)) {
    stop("net_res must contain adjacency and metrics.")
  }
  
  saveRDS(net_res, paste0(base_path_no_ext, "_full.rds"))
  
  utils::write.csv(net_res$adjacency, paste0(base_path_no_ext, "_adjacency.csv"), row.names = TRUE)
  utils::write.csv(net_res$metrics, paste0(base_path_no_ext, "_metrics.csv"), row.names = FALSE)
  
  invisible(TRUE)
}