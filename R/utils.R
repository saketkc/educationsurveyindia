`%||%` <- function(x, y) if (is.null(x)) y else x

#' @noRd
.check_con <- function(con) {
  if (!DBI::dbIsValid(con)) {
    cli::cli_abort("Connection is not valid. Reconnect with {.fn esi_connect}.")
  }
  invisible(TRUE)
}

#' Default data directory: `esi.data_dir` option, else the CRAN-sanctioned
#' per-user data dir (CRAN forbids writing to the home directory by default)
#' @noRd
.esi_default_dir <- function() {
  getOption("esi.data_dir", tools::R_user_dir("educationsurveyindia", "data"))
}

#' Check mospiR is installed and resolve the API key (argument, else MOSPI_KEY)
#' @noRd
.esi_api_key <- function(api_key = NULL) {
  if (!requireNamespace("mospiR", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg mospiR} is required for downloading data.",
      "i" = "Install with {.code install.packages('mospiR', repos = c('https://saketlab.r-universe.dev', getOption('repos')))}",
      "i" = "Or download data manually and pass {.arg raw_dir} to {.fn esi_build}."
    ))
  }
  api_key <- api_key %||% Sys.getenv("MOSPI_KEY", unset = NA_character_)
  if (is.na(api_key) || !nzchar(api_key)) {
    cli::cli_abort(c(
      "MOSPI API key not found.",
      "i" = "Set the {.envvar MOSPI_KEY} environment variable.",
      "i" = "Get a key from {.url https://microdata.gov.in}"
    ))
  }
  api_key
}
