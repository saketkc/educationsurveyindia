#' Search variables with optional fuzzy matching
#'
#' Searches variable names and labels across all tables.
#' With `fuzzy = TRUE`, uses Jaro-Winkler similarity.
#'
#' @param con Connection from [esi_connect()].
#' @param keyword Search string.
#' @param table Optional table filter.
#' @param fuzzy If TRUE, use Jaro-Winkler fuzzy matching.
#' @param fuzzy_threshold Minimum similarity score (0-1, default 0.7).
#'
#' @return Tibble with variable, table, variable_label (and similarity if fuzzy).
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_find(con, "sector")
#' esi_find(con, "sectr", fuzzy = TRUE, fuzzy_threshold = 0.6)
#' esi_disconnect(con)
esi_find <- function(con, keyword, table = NULL, fuzzy = FALSE,
                     fuzzy_threshold = 0.7) {
  if (fuzzy && (!is.numeric(fuzzy_threshold) || fuzzy_threshold < 0 || fuzzy_threshold > 1)) {
    cli::cli_abort("{.arg fuzzy_threshold} must be between 0 and 1.")
  }
  .check_con(con)
  if (!fuzzy) {
    return(tibble::as_tibble(esi_search_vars(con, keyword, table = table)))
  }

  where <- if (!is.null(table)) "WHERE table_name = ?" else ""
  # ponytail: filter in an outer query rather than HAVING-without-GROUP-BY,
  # which recent DuckDB rejects for a non-aggregated column.
  sql <- paste(
    "SELECT variable, \"table\", variable_label, similarity FROM (",
    "  SELECT variable, table_name AS table, variable_label,",
    "    GREATEST(",
    "      jaro_winkler_similarity(LOWER(variable_label), LOWER(?)),",
    "      jaro_winkler_similarity(LOWER(variable), LOWER(?))",
    "    ) AS similarity",
    "  FROM labels.meta_variable_labels",
    where,
    ") WHERE similarity >= ?",
    "ORDER BY similarity DESC",
    "LIMIT 20"
  )
  params <- if (!is.null(table)) {
    list(keyword, keyword, table, fuzzy_threshold)
  } else {
    list(keyword, keyword, fuzzy_threshold)
  }

  tibble::as_tibble(DBI::dbGetQuery(con, sql, params))
}
