# Variable/value label lookups against labels.duckdb

#' Substring-search variable names and labels
#'
#' @param con Connection from [esi_connect()].
#' @param pattern Substring to search for (case-insensitive).
#' @param table Optional table filter (`"household"`, `"person"`,
#'   `"person_erstwhile"`).
#' @return Tibble with `table_name`, `variable`, `variable_label`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_search_vars(con, "expenditure")
#' esi_disconnect(con)
esi_search_vars <- function(con, pattern, table = NULL) {
  .check_con(con)
  pattern_esc <- gsub("([%_\\\\])", "\\\\\\1", pattern)
  pattern_like <- paste0("%", pattern_esc, "%")
  if (!is.null(table)) {
    sql <- paste(
      "SELECT table_name, variable, variable_label",
      "FROM labels.meta_variable_labels",
      "WHERE (variable ILIKE ? OR variable_label ILIKE ?) AND table_name = ?",
      "ORDER BY table_name, variable"
    )
    tibble::as_tibble(DBI::dbGetQuery(con, sql, list(pattern_like, pattern_like, table)))
  } else {
    sql <- paste(
      "SELECT table_name, variable, variable_label",
      "FROM labels.meta_variable_labels",
      "WHERE variable ILIKE ? OR variable_label ILIKE ?",
      "ORDER BY table_name, variable"
    )
    tibble::as_tibble(DBI::dbGetQuery(con, sql, list(pattern_like, pattern_like)))
  }
}

#' Look up value labels for one variable
#'
#' @param con Connection from [esi_connect()].
#' @param table Table name.
#' @param variable Variable name.
#' @return Tibble with `value`, `label`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_value_labels(con, "household", "sector")
#' esi_disconnect(con)
esi_value_labels <- function(con, table, variable) {
  .check_con(con)
  sql <- paste(
    "SELECT value, label FROM labels.meta_value_labels",
    "WHERE table_name = ? AND variable = ? ORDER BY TRY_CAST(value AS INTEGER)"
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql, list(table, variable)))
}

#' Decode a coded column using its stored value labels
#'
#' @param con Connection from [esi_connect()].
#' @param table Table name the column belongs to.
#' @param variable Variable (column) name.
#' @param x Vector of raw (coded) values to decode.
#' @return Character vector of labels; unmatched codes pass through as
#'   character.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_decode(con, "household", "sector", c("1", "2", "9"))
#' esi_disconnect(con)
esi_decode <- function(con, table, variable, x) {
  labs <- esi_value_labels(con, table, variable)
  if (nrow(labs) == 0L) {
    return(as.character(x))
  }
  lookup <- stats::setNames(labs$label, labs$value)
  hit <- lookup[as.character(x)]
  ifelse(is.na(hit), as.character(x), unname(hit))
}

#' CMS-E `enrolment_level` codes in school order
#'
#' The raw codes sort "below class I" (`"15"`) last. This function gives the
#' codes as a factor in school order: pre-primary, class I to XII, then the
#' two diploma or certificate codes.
#'
#' @param x Character vector of `enrolment_level` codes (`"01"` to `"15"`).
#'   Numbers and one-digit codes are also accepted.
#' @return A factor with short English labels, in school order. Codes that
#'   are not known become `NA`.
#' @export
#' @examples
#' esi_class_label(c("15", "01", "12", "13"))
#' levels(esi_class_label(character(0)))
esi_class_label <- function(x) {
  codes <- c("15", sprintf("%02d", 1:14))
  labels <- c(
    "Pre-primary", paste("Class", utils::as.roman(1:12)),
    "Diploma (up to secondary)", "Diploma (higher secondary)"
  )
  x <- as.character(x)
  x <- ifelse(grepl("^[0-9]$", x), paste0("0", x), x)
  factor(x, levels = codes, labels = labels)
}
