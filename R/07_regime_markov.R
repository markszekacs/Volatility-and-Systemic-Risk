suppressPackageStartupMessages({
  library(xts)
  library(zoo)
  library(MSwM)
})

# Fit a 2-state Markov-switching model to a single time series.
# The default is an intercept-only model with switching mean and variance,
# which works well for regime detection on a scalar series (e.g., rolling TCI).
fit_ms_regimes <- function(series_xts, k = 2, switch_mean = TRUE, switch_var = TRUE) {
  if (!xts::is.xts(series_xts)) stop("series_xts must be an xts object.")
  if (NCOL(series_xts) != 1) stop("series_xts must have exactly one column.")
  if (nrow(series_xts) < 50) stop("series_xts too short for regime estimation.")
  
  y <- as.numeric(series_xts)
  df <- data.frame(y = y)
  
  # Intercept-only linear model.
  base_lm <- stats::lm(y ~ 1, data = df)
  
  # sw: which coefficients switch across regimes.
  # For intercept-only model, sw = c(TRUE) toggles switching in the mean.
  sw <- c(isTRUE(switch_mean))
  
  ms_model <- MSwM::msmFit(
    object = base_lm,
    k = k,
    sw = sw,
    control = list(parallel = FALSE),
    p = 0,                 # no autoregressive terms; keep it simple for regime segmentation
    variance = switch_var  # allow regime-specific variance if TRUE
  )
  
  ms_model
}

# Extract smoothed regime probabilities as an xts object.
# Returns columns Regime1, Regime2, ..., RegimeK.
extract_ms_probs <- function(ms_model, dates) {
  if (is.null(ms_model@Fit@smoProb)) stop("No smoothed probabilities found in the ms_model.")
  probs <- ms_model@Fit@smoProb
  probs_xts <- xts::xts(probs, order.by = dates)
  colnames(probs_xts) <- paste0("Regime", seq_len(ncol(probs)))
  probs_xts
}

# Convert probabilities into a hard regime label (1..K) by argmax.
label_regimes_argmax <- function(probs_xts) {
  if (!xts::is.xts(probs_xts)) stop("probs_xts must be xts.")
  mat <- coredata(probs_xts)
  lab <- max.col(mat, ties.method = "first")
  xts::xts(lab, order.by = zoo::index(probs_xts))
}

# Identify "high" vs "low" regime using the series mean within each regime.
# Returns a list with labels and a mapping of which regime is "high".
map_regimes_by_level <- function(series_xts, regime_labels_xts) {
  if (!xts::is.xts(series_xts) || !xts::is.xts(regime_labels_xts)) stop("Inputs must be xts.")
  if (nrow(series_xts) != nrow(regime_labels_xts)) stop("Inputs must have same number of rows.")
  
  y <- as.numeric(series_xts)
  r <- as.integer(regime_labels_xts)
  
  means <- tapply(y, r, mean, na.rm = TRUE)
  high_regime <- as.integer(names(means)[which.max(means)])
  low_regime  <- as.integer(names(means)[which.min(means)])
  
  list(
    regime_means = means,
    high_regime = high_regime,
    low_regime = low_regime
  )
}

# Full pipeline: fit model -> probs -> labels -> mapping.
compute_regimes <- function(series_xts, k = 2, switch_mean = TRUE, switch_var = TRUE) {
  if (!xts::is.xts(series_xts)) stop("series_xts must be xts.")
  series_xts <- na.omit(series_xts)
  
  ms_model <- fit_ms_regimes(series_xts, k = k, switch_mean = switch_mean, switch_var = switch_var)
  dates <- zoo::index(series_xts)
  
  probs <- extract_ms_probs(ms_model, dates)
  labels <- label_regimes_argmax(probs)
  mapping <- map_regimes_by_level(series_xts, labels)
  
  list(
    model = ms_model,
    probs = probs,
    labels = labels,
    mapping = mapping
  )
}

# Export regime outputs (probs and labels) to disk.
export_regimes <- function(reg_res, base_path_no_ext) {
  if (!is.list(reg_res) || is.null(reg_res$probs) || is.null(reg_res$labels)) {
    stop("reg_res must be a list containing probs and labels.")
  }
  
  saveRDS(reg_res, paste0(base_path_no_ext, "_full.rds"))
  
  probs_df <- data.frame(date = zoo::index(reg_res$probs), coredata(reg_res$probs), check.names = FALSE)
  utils::write.csv(probs_df, paste0(base_path_no_ext, "_probs.csv"), row.names = FALSE)
  
  labels_df <- data.frame(date = zoo::index(reg_res$labels), regime = as.integer(reg_res$labels))
  utils::write.csv(labels_df, paste0(base_path_no_ext, "_labels.csv"), row.names = FALSE)
  
  invisible(TRUE)
}
