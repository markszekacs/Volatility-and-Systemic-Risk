cfg <- list(
  # ---- paths ----
  paths = list(
    data_raw = "data/raw",
    data_processed = "data/processed",
    outputs = "outputs",
    figures = "outputs/figures",
    tables  = "outputs/tables"
  ),
  
  # ---- date range ----
  dates = list(
    from = as.Date("2005-01-01"),
    to   = as.Date("2025-01-01")
  ),
  
  # ---- data source ----
  source = list(
    prices_src = "yahoo"
  ),
  
  # ---- volatility construction ----
  vol = list(
    annual = 252,          # 252 trading days
    scale  = 0.361,        # nálad ez szerepel
    transform = "asinh"    # "asinh" vagy "none"
  ),
  
  # ---- VAR / FEVD / rolling defaults ----
  var = list(
    p = 1,
    type = "none"
  ),
  fevd = list(
    horizon = 10
  ),
  rolling = list(
    window = 250
  ),
  
  # ---- markets ----
  markets = list(
    EU = sort(c("AEG", "ALV.DE", "ZURN.SW", "CS.PA", "LGEN.L", "MUV2.DE", "PRU.L", "AV.L", "SREN.SW", "G.MI")),
    USA = sort(c("UNH", "BRK.A", "BRK.B", "MET", "PRU", "AIG", "ALL", "TRV", "CI", "HUM", "AFL"))
  )
)

# small helper directories
ensure_dirs <- function(paths) {
  for (p in paths) if (!dir.exists(p)) dir.create(p, recursive = TRUE)
  invisible(TRUE)
}

