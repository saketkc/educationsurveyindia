#' Command-line interface
#'
#' Entry point behind the `esi` script shipped in the package's `exec/`
#' directory. Put it on your `PATH` with:
#'
#' ```sh
#' ln -s "$(Rscript -e 'cat(system.file("exec", "esi", package = "educationsurveyindia"))')" ~/.local/bin/esi
#' ```
#'
#' or call it without installing the script:
#' `Rscript -e 'educationsurveyindia::esi_cli()' build`.
#'
#' Commands:
#' * `esi download [--raw-dir DIR] [--overwrite]`: [esi_download()].
#' * `esi build [--output-dir DIR] [--raw-dir DIR] [--overwrite]`:
#'   [esi_build()]; downloads into `<output-dir>/raw` first unless
#'   `--raw-dir` is given.
#' * `esi info [--data-dir DIR]`: tables and row counts of a built
#'   directory, via [esi_available_data()].
#'
#' The API key is read from the `MOSPI_KEY` environment variable only, so it
#' never appears in the process list or shell history.
#'
#' @param args Character vector of command-line arguments.
#' @return Exit status (`0L` on success, `1L` on error), invisibly.
#' @export
#' @examples
#' esi_cli("help")
esi_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  status <- tryCatch(
    {
      .esi_cli_run(args)
      0L
    },
    error = function(e) {
      message("Error: ", conditionMessage(e))
      1L
    }
  )
  invisible(status)
}

.ESI_CLI_USAGE <- "Usage: esi <command> [options]

Commands:
  download   Download CMS-E raw files from microdata.gov.in (needs MOSPI_KEY)
  build      Download (if needed) and build Parquet + labels.duckdb
  info       Show tables and row counts of a built data directory
  help       Show this message

Options:
  --raw-dir DIR     Raw download directory
                    (download: default <data dir>/raw;
                     build: use this pre-downloaded directory instead of
                     downloading into <output-dir>/raw)
  --output-dir DIR  Build output directory (default <data dir>)
  --data-dir DIR    Built data directory for info (default <data dir>)
  --overwrite       Re-download files that already exist

<data dir> is the esi.data_dir option, else tools::R_user_dir(\"educationsurveyindia\", \"data\").
"

#' Commands and the options each one accepts (flags take no value)
#' @noRd
.ESI_CLI_COMMANDS <- list(
  download = list(values = "raw-dir", flags = "overwrite"),
  build = list(values = c("raw-dir", "output-dir"), flags = "overwrite"),
  info = list(values = "data-dir", flags = character(0))
)

#' @noRd
.esi_cli_run <- function(args) {
  if (length(args) == 0L || args[1] %in% c("help", "-h", "--help")) {
    cat(.ESI_CLI_USAGE)
    return(invisible(NULL))
  }
  cmd <- args[1]
  spec <- .ESI_CLI_COMMANDS[[cmd]]
  if (is.null(spec)) {
    cli::cli_abort("Unknown command {.val {cmd}}. Run {.code esi help}.")
  }
  opts <- .esi_cli_parse(args[-1], spec)
  # option names map onto the R arguments, so omitted ones keep the R defaults
  names(opts) <- gsub("-", "_", names(opts), fixed = TRUE)

  switch(cmd,
    download = do.call(esi_download, opts),
    build = do.call(esi_build, opts),
    info = {
      data_dir <- opts$data_dir %||% .esi_default_dir()
      con <- esi_connect(data_dir)
      on.exit(esi_disconnect(con), add = TRUE)
      print(esi_available_data(con))
    }
  )
  invisible(NULL)
}

#' Parse `--name value`, `--name=value` and `--flag` options
#' @noRd
.esi_cli_parse <- function(args, spec) {
  opts <- list()
  i <- 1L
  while (i <= length(args)) {
    arg <- args[i]
    if (!startsWith(arg, "--")) {
      cli::cli_abort("Unexpected argument {.val {arg}}. Run {.code esi help}.")
    }
    name <- sub("^--", "", arg)
    value <- NULL
    if (grepl("=", name, fixed = TRUE)) {
      value <- sub("^[^=]*=", "", name)
      name <- sub("=.*$", "", name)
    }
    if (name %in% spec$flags) {
      if (!is.null(value)) cli::cli_abort("Option {.code --{name}} takes no value.")
      opts[[name]] <- TRUE
    } else if (name %in% spec$values) {
      if (is.null(value)) {
        i <- i + 1L
        if (i > length(args) || startsWith(args[i], "--")) {
          cli::cli_abort("Option {.code --{name}} needs a value.")
        }
        value <- args[i]
      }
      if (!nzchar(value)) cli::cli_abort("Option {.code --{name}} needs a value.")
      opts[[name]] <- value
    } else {
      cli::cli_abort("Unknown option {.code --{name}}. Run {.code esi help}.")
    }
    i <- i + 1L
  }
  opts
}
