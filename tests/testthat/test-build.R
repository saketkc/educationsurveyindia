.make_raw_dir <- function() {
  dir <- tempfile("esi_raw_")
  dir.create(dir)
  hh <- data.frame(
    fsu_serial_no = c(1, 2), nss_region = c("011", "272"), mult = c(1000, 2500),
    sector = haven::labelled(c(1, 2), c(rural = 1, urban = 2), label = "Sector")
  )
  haven::write_dta(hh, file.path(dir, "CMSE80HH25.dta"))
  dir
}

test_that("esi_build writes Parquet and labels, and a rebuild does not duplicate labels", {
  raw <- .make_raw_dir()
  out <- tempfile("esi_out_")
  for (i in 1:2) {
    suppressMessages(suppressWarnings(esi_build(raw_dir = raw, output_dir = out)))
  }

  hh <- arrow::read_parquet(file.path(out, "household.parquet"))
  expect_equal(hh$state, c("01", "27"), ignore_attr = TRUE)
  expect_equal(hh$weight, c(10, 25), ignore_attr = TRUE)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(out, "labels.duckdb"), read_only = TRUE)
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  vals <- DBI::dbGetQuery(con, "SELECT value, label FROM meta_value_labels WHERE variable = 'sector' ORDER BY value")
  expect_equal(vals$label, c("rural", "urban"))
  n_vals <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_value_labels")$n
  expect_equal(n_vals, 2) # no duplicate rows
  n_vars <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_variable_labels")$n
  expect_equal(n_vars, ncol(hh) - 2) # state/weight are derived, not labelled
})

test_that("esi_download skips files already present", {
  raw <- .make_raw_dir()
  file.create(file.path(raw, c(.ESI_STATA_ZIP, .ESI_CODES_FILE)))
  local_mocked_bindings(
    list_files = function(...) stop("should not hit the network"),
    .package = "mospiR"
  )
  expect_message(esi_download(raw, api_key = "k"), "skipping download")
})

test_that("esi_download aborts when the STATA zip download fails", {
  local_mocked_bindings(
    list_files = function(...) data.frame(name = character(0)),
    download_file = function(...) NULL,
    .package = "mospiR"
  )
  expect_error(suppressMessages(esi_download(tempfile(), api_key = "k")), "Download of .*failed")
})

test_that("esi_download requires an API key", {
  withr::local_envvar(MOSPI_KEY = "")
  expect_error(esi_download(tempfile()), "API key not found")
})

test_that(".esi_extract_labels writes a codebook code once when the codebook repeats it", {
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE meta_variable_labels (table_name VARCHAR, variable VARCHAR, variable_label VARCHAR)")
  DBI::dbExecute(con, "CREATE TABLE meta_value_labels (table_name VARCHAR, variable VARCHAR, value VARCHAR, label VARCHAR)")
  df <- data.frame(sector = "1")
  attr(df$sector, "label") <- "Sector"
  codes <- data.frame(variable_label = "Sector", code = c("1", "1"), code_description = c("rural", "rural"))
  .esi_extract_labels(con, "household", df, codes)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meta_value_labels")$n, 1)
})
