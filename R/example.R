#' Write a tiny synthetic CMS-E data directory
#'
#' Writes `household.parquet`, `person.parquet` and `labels.duckdb` with the
#' same columns the query functions use, a handful of made-up records each.
#' For examples and tests only: the values are not survey data.
#'
#' @param dir Directory to write into. Created if missing.
#' @return `dir`, invisibly.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_tables(con)
#' esi_disconnect(con)
esi_example_data <- function(dir = tempfile("esi_example_")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  tables <- list(
    household = data.frame(
      state = c("01", "01", "27", "27"),
      weight = c(100, 200, 150, 150),
      any_member_attending_school = c("1", "2", "1", "1"),
      usual_monthly_consumption_expenditure = c(1000, 2000, 1500, 2500),
      sector = c("1", "2", "1", "2")
    ),
    person = data.frame(
      state = c("01", "01", "27"),
      weight = c(100, 200, 150),
      currently_enrolled_school = c("1", "2", "1"),
      school_exp_total = c(500, NA, 800),
      school_exp_course_fee = c(400, NA, 800),
      received_private_coaching = c("2", "", "2"),
      private_coaching_exp_total = c(0, NA, NA),
      hostel_fee_expenditure = c("100", "", "")
    )
  )
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = file.path(dir, "labels.duckdb"))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  for (nm in names(tables)) {
    tmp <- paste0("tmp_", nm)
    duckdb::duckdb_register(con, tmp, tables[[nm]])
    DBI::dbExecute(con, sprintf(
      "COPY %s TO '%s' (FORMAT PARQUET)", tmp, file.path(dir, paste0(nm, ".parquet"))
    ))
    duckdb::duckdb_unregister(con, tmp)
  }
  DBI::dbWriteTable(con, "meta_variable_labels", data.frame(
    table_name = c("household", "household"),
    variable = c("sector", "usual_monthly_consumption_expenditure"),
    variable_label = c("Sector", "Usual monthly consumption expenditure")
  ))
  DBI::dbWriteTable(con, "meta_value_labels", data.frame(
    table_name = "household",
    variable = "sector",
    value = c("1", "2"),
    label = c("rural", "urban")
  ))
  invisible(dir)
}
