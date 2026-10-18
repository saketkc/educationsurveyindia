# Database connection management

#' Connect to CMS-E education survey data
#'
#' Open an in-memory DuckDB connection, register all Parquet files in
#' `data_dir` as views, and attach `labels.duckdb`. Queries are
#' lazy-evaluated via dplyr; call [dplyr::collect()] to materialise.
#'
#' @param data_dir Path to directory containing Parquet files and
#'   `labels.duckdb`. If `NULL`, uses `getOption("esi.data_dir")`, else
#'   [tools::R_user_dir()] (the [esi_build()] default).
#' @param read_only Logical; if `TRUE` (default), attach the labels
#'   database read-only.
#' @return A DBI connection object (DuckDB).
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_tables(con)
#' esi_disconnect(con)
esi_connect <- function(data_dir = NULL, read_only = TRUE) {
  data_dir <- data_dir %||% .esi_default_dir()
  if (!dir.exists(data_dir)) {
    cli::cli_abort(c(
      "Data directory {.path {data_dir}} does not exist.",
      "i" = "Pass {.arg data_dir} or set {.code options(esi.data_dir = '...')}.",
      "i" = "Run {.fn esi_build} first to generate the data files."
    ))
  }
  data_dir <- normalizePath(data_dir)

  pq_files <- list.files(data_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(pq_files) == 0L) {
    cli::cli_abort(c(
      "No Parquet files found in {.path {data_dir}}.",
      "i" = "Run {.fn esi_build} to generate data files."
    ))
  }

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

  tbl_names <- character(0)
  for (pq in pq_files) {
    tbl_name <- tools::file_path_sans_ext(basename(pq))
    sql <- sprintf("CREATE VIEW \"%s\" AS SELECT * FROM read_parquet('%s')", tbl_name, pq)
    ok <- tryCatch(
      {
        DBI::dbExecute(con, sql)
        TRUE
      },
      error = function(e) {
        cli::cli_warn("Failed to register {.file {basename(pq)}}: {conditionMessage(e)}")
        FALSE
      }
    )
    if (ok) tbl_names <- c(tbl_names, tbl_name)
  }

  labels_path <- file.path(data_dir, "labels.duckdb")
  if (file.exists(labels_path)) {
    ro <- if (read_only) "READ_ONLY" else ""
    sql <- sprintf("ATTACH '%s' AS labels (%s)", labels_path, ro)
    tryCatch(
      DBI::dbExecute(con, sql),
      error = function(e) cli::cli_warn("Failed to attach labels database: {conditionMessage(e)}")
    )
  }

  attr(con, "esi_data_dir") <- data_dir
  attr(con, "esi_tables") <- tbl_names
  cli::cli_inform(c(
    "v" = "Connected to CMS-E data in {.path {data_dir}}",
    "i" = "{length(pq_files)} table{?s} registered"
  ))
  con
}

#' Disconnect from CMS-E education survey data
#'
#' @param con A DBI connection from [esi_connect()].
#' @return Invisibly returns `NULL`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_disconnect(con)
esi_disconnect <- function(con) {
  .check_con(con)
  DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

#' List available tables
#'
#' @param con A DBI connection from [esi_connect()].
#' @return Character vector of table names (the Parquet-backed views
#'   registered by [esi_connect()]; excludes the attached `labels` schema).
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_tables(con)
#' esi_disconnect(con)
esi_tables <- function(con) {
  .check_con(con)
  attr(con, "esi_tables") %||% character(0)
}
