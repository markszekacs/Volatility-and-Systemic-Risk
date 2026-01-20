suppressPackageStartupMessages({
  library(xts)
  library(zoo)
})

# Save an xts object as RDS and CSV.
save_xts <- function(x, base_path_no_ext) {
  if (!xts::is.xts(x)) stop("x must be xts.")
  
  rds_path <- paste0(base_path_no_ext, ".rds")
  csv_path <- paste0(base_path_no_ext, ".csv")
  
  saveRDS(x, rds_path)
  
  df <- data.frame(date = zoo::index(x), coredata(x), check.names = FALSE)
  utils::write.csv(df, csv_path, row.names = FALSE)
  
  invisible(list(rds = rds_path, csv = csv_path))
}

# Plot an xts series (single-column) to file.
plot_xts_series <- function(x, file_path, main = "", xlab = "Date", ylab = "", width = 1200, height = 650) {
  if (!xts::is.xts(x)) stop("x must be xts.")
  if (NCOL(x) != 1) stop("x must have exactly 1 column for plot_xts_series().")
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  grDevices::png(filename = file_path, width = width, height = height)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  plot(
    zoo::index(x), as.numeric(x),
    type = "l",
    main = main,
    xlab = xlab,
    ylab = ylab
  )
}

# Plot rolling TCI.
plot_tci <- function(tci_xts, file_path, title = "Rolling Total Connectedness Index (TCI)") {
  plot_xts_series(
    x = tci_xts,
    file_path = file_path,
    main = title,
    ylab = "TCI"
  )
}

# Plot rolling NET for multiple tickers (overlaid lines).
# Note: base R plot, no custom colors specified.
plot_net_overlay <- function(net_xts, file_path, title = "Rolling Net Connectedness (NET)") {
  if (!xts::is.xts(net_xts)) stop("net_xts must be xts.")
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  grDevices::png(filename = file_path, width = 1400, height = 800)
  on.exit(grDevices::dev.off(), add = TRUE)
  
  mat <- coredata(net_xts)
  dates <- zoo::index(net_xts)
  
  matplot(
    x = dates, y = mat,
    type = "l", lty = 1,
    main = title,
    xlab = "Date", ylab = "NET"
  )
  legend(
    "topright",
    legend = colnames(net_xts),
    cex = 0.7,
    bty = "n"
  )
}

# Save a generic list object as RDS (for DY outputs, matrices, etc.).
save_object_rds <- function(obj, file_path) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, file_path)
  invisible(file_path)
}