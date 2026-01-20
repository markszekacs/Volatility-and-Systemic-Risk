suppressPackageStartupMessages({
  library(Matrix)
})

# Create a symmetric weight matrix from a possibly directed matrix.
# Common choice: W_sym = (|A| + |A|') / 2.
symmetrize_weights <- function(A, method = c("abs_avg", "avg")) {
  method <- match.arg(method)
  A <- as.matrix(A)
  
  if (method == "abs_avg") {
    W <- (abs(A) + t(abs(A))) / 2
  } else {
    W <- (A + t(A)) / 2
  }
  
  diag(W) <- 0
  W
}

# Build the (combinatorial) graph Laplacian L = D - W.
laplacian_from_weights <- function(W) {
  W <- as.matrix(W)
  diag(W) <- 0
  d <- rowSums(W)
  D <- diag(d)
  L <- D - W
  L
}

# Compute Moore-Penrose pseudoinverse of Laplacian via eigen-decomposition.
# Works for connected graphs; for disconnected graphs, results are still defined
# but interpretability changes (effective resistance blows up between components).
laplacian_pinv <- function(L, tol = 1e-10) {
  L <- as.matrix(L)
  eig <- eigen(L, symmetric = TRUE)
  
  vals <- eig$values
  vecs <- eig$vectors
  
  inv_vals <- ifelse(vals > tol, 1 / vals, 0)
  Lplus <- vecs %*% diag(inv_vals) %*% t(vecs)
  Lplus
}

# Effective resistance between nodes i and j:
# r_ij = L+_ii + L+_jj - 2 L+_ij
effective_resistance_matrix <- function(Lplus) {
  Lplus <- as.matrix(Lplus)
  n <- nrow(Lplus)
  diag_Lp <- diag(Lplus)
  
  R <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in i:n) {
      rij <- diag_Lp[i] + diag_Lp[j] - 2 * Lplus[i, j]
      R[i, j] <- rij
      R[j, i] <- rij
    }
  }
  diag(R) <- 0
  R
}

# Kirchhoff index (a.k.a. total effective resistance):
# Kf = sum_{i<j} r_ij
kirchhoff_index <- function(R_eff) {
  R_eff <- as.matrix(R_eff)
  sum(R_eff[upper.tri(R_eff)], na.rm = TRUE)
}

# One-shot pipeline from adjacency to Kirchhoff index.
compute_kirchhoff_from_adjacency <- function(A,
                                             sym_method = c("abs_avg", "avg"),
                                             threshold = NULL,
                                             tol = 1e-10) {
  sym_method <- match.arg(sym_method)
  A <- as.matrix(A)
  
  # Optional thresholding on absolute weights.
  if (!is.null(threshold)) {
    A[abs(A) < threshold] <- 0
  }
  diag(A) <- 0
  
  W <- symmetrize_weights(A, method = sym_method)
  L <- laplacian_from_weights(W)
  Lplus <- laplacian_pinv(L, tol = tol)
  R_eff <- effective_resistance_matrix(Lplus)
  Kf <- kirchhoff_index(R_eff)
  
  list(
    W = W,
    L = L,
    Lplus = Lplus,
    R_eff = R_eff,
    kirchhoff = Kf
  )
}

# Rolling Kirchhoff index from a list of adjacency matrices (time-ordered).
# adj_list: named list where names are dates (character) or a Date vector provided separately.
compute_rolling_kirchhoff <- function(adj_list, dates = NULL,
                                      sym_method = c("abs_avg", "avg"),
                                      threshold = NULL,
                                      tol = 1e-10) {
  sym_method <- match.arg(sym_method)
  
  if (!is.list(adj_list) || length(adj_list) == 0) stop("adj_list must be a non-empty list.")
  if (is.null(dates)) {
    dates <- names(adj_list)
    if (is.null(dates)) stop("Provide dates or name the list elements with dates.")
  }
  
  kf <- numeric(length(adj_list))
  for (i in seq_along(adj_list)) {
    res <- compute_kirchhoff_from_adjacency(adj_list[[i]], sym_method = sym_method, threshold = threshold, tol = tol)
    kf[i] <- res$kirchhoff
  }
  
  xts::xts(kf, order.by = as.Date(dates))
}