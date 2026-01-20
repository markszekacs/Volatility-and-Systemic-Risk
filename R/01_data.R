suppressPackageStartupMessages({
  library(quantmod)
  library(xts)
})

cache_path_prices <- function(market, cfg) {
  file.path(cfg$paths$data_raw, paste0("prices_", market, "_", cfg$dates$from, "_", cfg$dates$to, ".rds"))
}

download_prices_env <- function(symbols, from, to, src = "yahoo") {
  env <- new.env(parent = emptyenv())
  quantmod::getSymbols(
    Symbols = symbols,
    src = src,
    from = from,
    to = to,
    env = env,
    auto.assign = TRUE,
    warnings = FALSE
  )
  env
}

save_prices_cache <- function(env, market, cfg) {
  fp <- cache_path_prices(market, cfg)
  saveRDS(as.list(env), fp)
  fp
}

load_prices_cache <- function(market, cfg) {
  fp <- cache_path_prices(market, cfg)
  if (!file.exists(fp)) return(NULL)
  readRDS(fp)
}

as_env <- function(x) {
  # x: list of xts objects -> env like getSymbols output
  env <- new.env(parent = emptyenv())
  for (nm in names(x)) assign(nm, x[[nm]], envir = env)
  env
}

get_close_panel <- function(env) {
  lst <- as.list(env)
  closes <- lapply(lst, quantmod::Cl)
  prices <- do.call(merge, closes)
  colnames(prices) <- names(lst)
  na.omit(prices)
}

get_prices_panel <- function(market, cfg, force_download = FALSE) {
  ensure_dirs(unlist(cfg$paths))
  
  symbols <- cfg$markets[[market]]
  if (is.null(symbols)) stop("Unknown market: ", market)
  
  cached <- if (!force_download) load_prices_cache(market, cfg) else NULL
  
  if (!is.null(cached)) {
    env <- as_env(cached)
  } else {
    env <- download_prices_env(
      symbols = symbols,
      from = cfg$dates$from,
      to = cfg$dates$to,
      src = cfg$source$prices_src
    )
    save_prices_cache(env, market, cfg)
  }
  
  get_close_panel(env)
}
