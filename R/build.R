# Stage 1 ETL: download CMS-E microdata and write Parquet + labels.duckdb

#' NSS 80th round CMS-E dataset id on microdata.gov.in
#' @noRd
.ESI_IDNO <- "DDI-IND-MOSPI-NSS-CMSE80-2025"

#' Official block-code codebook filename (authoritative value labels)
#' @noRd
.ESI_CODES_FILE <- "CODEs for Blocks of Sch - CMS-Education.xlsx"

#' STATA-format microdata archive on microdata.gov.in
#' @noRd
.ESI_STATA_ZIP <- "Data in STATA.zip"

#' CMS-E table specs: STATA file name -> (table name, join-key columns)
#' @noRd
.ESI_TABLES <- list(
  household = list(
    file = "CMSE80HH25.dta",
    keys = c("fsu_serial_no", "second_stage_stratum_no", "sample_hhld_no")
  ),
  person = list(
    file = "CMSE80PER25.dta",
    keys = c("fsu_serial_no", "second_stage_stratum_no", "sample_hhld_no", "person_serial_no")
  ),
  person_erstwhile = list(
    file = "CMSE80PERST25.dta",
    keys = c("fsu_serial_no", "second_stage_stratum_no", "sample_hhld_no", "erstwhile_person_serial_no")
  )
)

#' Build CMS-E data from a MOSPI download
#'
#' Downloads the NSS 80th round Comprehensive Modular Survey on Education
#' (CMS-E) STATA microdata via [esi_download()] (unless `raw_dir` is given),
#' then writes one Parquet file per table (household, person,
#' person_erstwhile) plus a `labels.duckdb` with variable and value labels
#' extracted from the STATA files. Adds a derived `state` column (first 2
#' digits of `nss_region`) and `weight = mult / 100` per the NSS CMS-E
#' methodology note. Re-running replaces earlier output.
#'
#' @param raw_dir Directory holding an already-downloaded `Data in
#'   STATA.zip` (or its extracted `.dta` files). If `NULL`, downloads into
#'   `file.path(output_dir, "raw")` via [esi_download()], reusing files a
#'   previous run already downloaded there.
#' @param output_dir Path where Parquet files and `labels.duckdb` are
#'   written. Created if missing. Defaults to `getOption("esi.data_dir")`,
#'   or [tools::R_user_dir()] when that option is unset.
#' @param api_key MOSPI API key. If `NULL`, reads `MOSPI_KEY` from the
#'   environment. Only used when `raw_dir` is `NULL`.
#' @param overwrite If `TRUE`, re-download the raw files even if they are
#'   already in the download directory. Only used when `raw_dir` is `NULL`.
#' @param compression Parquet compression codec. Default `"zstd"`.
#' @return Invisibly returns `output_dir`.
#' @export
#' @examplesIf nzchar(Sys.getenv("MOSPI_KEY")) && interactive()
#' esi_build(output_dir = tempfile())
esi_build <- function(raw_dir = NULL,
                      output_dir = .esi_default_dir(),
                      api_key = NULL,
                      overwrite = FALSE,
                      compression = "zstd") {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg arrow} is required. Install with {.code install.packages('arrow')}.")
  }
  if (!requireNamespace("haven", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg haven} is required. Install with {.code install.packages('haven')}.")
  }

  # arrow and duckdb do not expand "~" themselves
  output_dir <- path.expand(output_dir)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(raw_dir)) {
    raw_dir <- esi_download(file.path(output_dir, "raw"), api_key = api_key, overwrite = overwrite)
  }
  raw_dir <- path.expand(raw_dir)
  if (!dir.exists(raw_dir)) {
    cli::cli_abort("Raw data directory not found: {.path {raw_dir}}")
  }

  dta_dir <- .esi_locate_dta(raw_dir)
  codes <- .esi_read_codes(file.path(raw_dir, .ESI_CODES_FILE))

  labels_db <- file.path(output_dir, "labels.duckdb")
  labels_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = labels_db)
  on.exit(DBI::dbDisconnect(labels_con, shutdown = TRUE), add = TRUE)
  # recreate so a rebuild does not append duplicate label rows
  DBI::dbExecute(labels_con, "DROP TABLE IF EXISTS meta_variable_labels")
  DBI::dbExecute(labels_con, "DROP TABLE IF EXISTS meta_value_labels")
  DBI::dbExecute(labels_con, "
    CREATE TABLE meta_variable_labels (
      table_name VARCHAR, variable VARCHAR, variable_label VARCHAR
    )
  ")
  DBI::dbExecute(labels_con, "
    CREATE TABLE meta_value_labels (
      table_name VARCHAR, variable VARCHAR, value VARCHAR, label VARCHAR
    )
  ")

  for (tbl_name in names(.ESI_TABLES)) {
    spec <- .ESI_TABLES[[tbl_name]]
    dta_path <- file.path(dta_dir, spec$file)
    pq_path <- file.path(output_dir, paste0(tbl_name, ".parquet"))
    if (!file.exists(dta_path)) {
      cli::cli_warn("Missing {.file {spec$file}}, skipping {.val {tbl_name}}.")
      unlink(pq_path) # its labels were just dropped, so a stale table would be unlabelled
      next
    }

    cli::cli_inform(c("i" = "Reading {.file {spec$file}}"))
    df <- haven::read_dta(dta_path)

    .esi_extract_labels(labels_con, tbl_name, df, codes)

    df <- tibble::as_tibble(lapply(df, haven::zap_labels))
    if ("nss_region" %in% names(df)) {
      df$state <- substr(df$nss_region, 1, 2)
    }
    if ("mult" %in% names(df)) {
      df$weight <- df$mult / 100
    }

    arrow::write_parquet(df, pq_path, compression = compression)
    cli::cli_inform(c("v" = "  {.val {tbl_name}}: {nrow(df)} rows, {ncol(df)} cols"))
  }

  cli::cli_inform(c("v" = "Build complete. Output: {.path {output_dir}}"))
  invisible(output_dir)
}

#' Extract STATA variable/value labels into the labels database
#'
#' Variable labels come from the STATA `label` attribute. Value labels
#' prefer the official codes-for-blocks codebook (`codes`, matched by
#' variable label) over STATA's `labels` attribute: several CMS-E
#' variables (e.g. `sector`) are string-typed in the DTA export, which
#' Stata's value-label mechanism does not support, and haven ends up
#' reading a corrupted 0/0 code pair for them (see CLAUDE.md gotchas).
#' @noRd
.esi_extract_labels <- function(labels_con, tbl_name, df, codes = NULL) {
  var_labels <- vapply(df, function(col) attr(col, "label") %||% NA_character_, character(1))
  var_tbl <- tibble::tibble(
    table_name = tbl_name,
    variable = names(df),
    variable_label = unname(var_labels)
  )
  DBI::dbAppendTable(labels_con, "meta_variable_labels", var_tbl)

  for (col_name in names(df)) {
    var_label <- var_labels[[col_name]]
    from_codes <- if (!is.null(codes) && !is.na(var_label)) {
      codes[tolower(trimws(codes$variable_label)) == tolower(trimws(var_label)), c("code", "code_description")]
    } else {
      NULL
    }

    if (!is.null(from_codes) && nrow(from_codes) > 0L) {
      val_tbl <- tibble::tibble(
        table_name = tbl_name, variable = col_name,
        value = from_codes$code, label = from_codes$code_description
      )
    } else {
      labs <- attr(df[[col_name]], "labels")
      if (is.null(labs) || length(labs) == 0L) next
      val_tbl <- tibble::tibble(
        table_name = tbl_name, variable = col_name,
        value = as.character(unname(labs)), label = names(labs)
      )
    }
    # the codebook can list one variable label in more than one block
    DBI::dbAppendTable(labels_con, "meta_value_labels", unique(val_tbl))
  }
  invisible(NULL)
}

#' Read the official block-code codebook (variable label -> code -> description)
#' @noRd
.esi_read_codes <- function(codes_path) {
  if (!file.exists(codes_path)) {
    cli::cli_warn("Codebook {.file {basename(codes_path)}} not found; falling back to STATA value labels.")
    return(NULL)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_warn("Package {.pkg readxl} not installed; falling back to STATA value labels.")
    return(NULL)
  }
  sheets <- readxl::excel_sheets(codes_path)
  rows <- lapply(sheets, function(s) {
    d <- suppressMessages(readxl::read_excel(codes_path, sheet = s, col_names = FALSE, col_types = "text"))
    d <- d[-1, 1:5]
    names(d) <- c("block", "col_item", "variable_label", "code", "code_description")
    d
  })
  codes <- do.call(rbind, rows)
  codes <- codes[!is.na(codes$variable_label) & codes$variable_label != "VARIABLE_NAME" & !is.na(codes$code), ]
  tibble::as_tibble(codes)
}

#' Download CMS-E raw files from microdata.gov.in
#'
#' Downloads `Data in STATA.zip` and the official codes-for-blocks codebook
#' for the CMS-E dataset via [mospiR::download_file()], then extracts the
#' `.dta` files next to the zip. Files already in `raw_dir` are not
#' downloaded again unless `overwrite = TRUE`, so repeated calls are cheap.
#'
#' @param raw_dir Destination directory. Created if missing. Defaults to
#'   `raw` under `getOption("esi.data_dir")` (or [tools::R_user_dir()]).
#' @param api_key MOSPI API key. If `NULL`, reads `MOSPI_KEY` from the
#'   environment.
#' @param overwrite If `TRUE`, download and extract again even if the files
#'   are already in `raw_dir`.
#' @return Invisibly returns `raw_dir`.
#' @seealso [esi_download_all()] for every education round, not just CMS-E.
#' @export
#' @examplesIf nzchar(Sys.getenv("MOSPI_KEY")) && interactive()
#' esi_download(tempfile())
esi_download <- function(raw_dir = file.path(.esi_default_dir(), "raw"),
                         api_key = NULL,
                         overwrite = FALSE) {
  api_key <- .esi_api_key(api_key)

  raw_dir <- path.expand(raw_dir)
  dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

  idno <- .ESI_IDNO # cli cannot interpolate names starting with a dot
  wanted <- c(.ESI_STATA_ZIP, .ESI_CODES_FILE)
  todo <- if (overwrite) wanted else wanted[!file.exists(file.path(raw_dir, wanted))]
  if (length(todo) == 0L) {
    cli::cli_inform(c("v" = "Raw files already in {.path {raw_dir}}, skipping download."))
  } else {
    cli::cli_inform(c("i" = "Downloading {.val {idno}} from microdata.gov.in..."))
    files <- mospiR::list_files(.ESI_IDNO, api_key)
    if (is.null(files)) {
      cli::cli_abort("Cannot list files for {.val {idno}}. Check your API key and network connection.")
    }
    for (f in todo) {
      path <- mospiR::download_file(.ESI_IDNO, f, raw_dir, api_key, .files_cache = files)
      if (!is.null(path)) next
      if (f == .ESI_STATA_ZIP) {
        cli::cli_abort("Download of {.file {f}} failed. Check your API key and network connection.")
      }
      cli::cli_warn("Download of {.file {f}} failed; value labels will fall back to STATA labels.")
    }
  }

  .esi_locate_dta(raw_dir, force_unzip = .ESI_STATA_ZIP %in% todo)
  invisible(raw_dir)
}

#' Find the directory holding the .dta files (raw_dir itself, or unzip it)
#' @noRd
.esi_locate_dta <- function(raw_dir, force_unzip = FALSE) {
  has_dta <- length(list.files(raw_dir, pattern = "\\.dta$")) > 0
  if (has_dta && !force_unzip) {
    return(raw_dir)
  }
  zips <- list.files(raw_dir, pattern = "STATA.*\\.zip$", full.names = TRUE, ignore.case = TRUE)
  if (length(zips) == 0L) {
    cli::cli_abort("No .dta files or STATA zip found in {.path {raw_dir}}.")
  }
  extracted <- utils::unzip(zips[1], exdir = raw_dir, junkpaths = TRUE)
  # unzip() only warns on a corrupt archive, so check the result
  missing <- setdiff(vapply(.ESI_TABLES, `[[`, "", "file"), basename(extracted))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.file {missing}} not extracted from {.file {basename(zips[1])}}.",
      "i" = "The download may be incomplete; retry with {.code overwrite = TRUE}."
    ))
  }
  raw_dir
}
