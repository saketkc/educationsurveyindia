# DuckDB-native SQL queries

#' Row counts and state coverage per table
#'
#' @param con Connection from [esi_connect()].
#' @return Tibble with `table_name`, `n_records`, `n_states`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_available_data(con)
#' esi_disconnect(con)
esi_available_data <- function(con) {
  .check_con(con)
  tbls <- esi_tables(con)
  rows <- lapply(tbls, function(tbl) {
    DBI::dbGetQuery(con, sprintf(
      "SELECT '%s' AS table_name, COUNT(*) AS n_records, COUNT(DISTINCT state) AS n_states FROM \"%s\"",
      tbl, tbl
    ))
  })
  tibble::as_tibble(do.call(rbind, rows))
}

#' School enrolment rate by state
#'
#' Weighted share of persons currently enrolled in school
#' (`currently_enrolled_school`), from the `person` table.
#'
#' @param con Connection from [esi_connect()].
#' @param by Grouping column(s); default `"state"`. Pass `NULL` for a
#'   national estimate.
#' @return Tibble with grouping columns, `n_persons`, `enrolment_rate`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_school_participation(con)
#' esi_school_participation(con, by = NULL)
#' esi_disconnect(con)
esi_school_participation <- function(con, by = "state") {
  .check_con(con)

  sql <- sprintf(
    "SELECT %s
       COUNT(*) AS n_persons,
       SUM(CASE WHEN currently_enrolled_school = '1' THEN weight ELSE 0 END)
         / NULLIF(SUM(weight), 0) AS enrolment_rate
     FROM person
     %s",
    .select_by(by), .group_by(by)
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' Education expenditure by state
#'
#' Weighted mean per-student school and private-coaching expenditure
#' (annual, from the `person` table). A student with no expenditure of a
#' type counts as 0 for that type. Thus `mean_total_expenditure` is the sum
#' of the other two means.
#'
#' @param con Connection from [esi_connect()].
#' @param by Grouping column(s); default `"state"`. Pass `NULL` for a
#'   national estimate.
#' @return Tibble with grouping columns, `n_students`,
#'   `mean_school_expenditure`, `mean_coaching_expenditure`,
#'   `mean_total_expenditure`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_expenditure_by_state(con)
#' esi_disconnect(con)
esi_expenditure_by_state <- function(con, by = "state") {
  .check_con(con)

  sql <- sprintf(
    "SELECT %s
       COUNT(*) AS n_students,
       SUM(COALESCE(school_exp_total, 0) * weight) / NULLIF(SUM(weight), 0) AS mean_school_expenditure,
       SUM(COALESCE(private_coaching_exp_total, 0) * weight) / NULLIF(SUM(weight), 0) AS mean_coaching_expenditure,
       -- COALESCE: a bare a + b is NULL when either is NULL, which drops the row
       SUM((COALESCE(school_exp_total, 0) + COALESCE(private_coaching_exp_total, 0)) * weight)
         / NULLIF(SUM(weight), 0) AS mean_total_expenditure
     FROM person
     WHERE currently_enrolled_school = '1'
     %s",
    .select_by(by), .group_by(by)
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' Household-level education spending summary by state
#'
#' Weighted mean usual monthly consumption expenditure allocated to
#' education-attending households, from the `household` table.
#'
#' @param con Connection from [esi_connect()].
#' @return Tibble with `state`, `n_households`, `share_with_student`,
#'   `mean_monthly_expenditure`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_state_household_summary(con)
#' esi_disconnect(con)
esi_state_household_summary <- function(con) {
  .check_con(con)
  sql <- "
    SELECT state,
      COUNT(*) AS n_households,
      SUM(CASE WHEN any_member_attending_school = '1' THEN weight ELSE 0 END)
        / NULLIF(SUM(weight), 0) AS share_with_student,
      SUM(usual_monthly_consumption_expenditure * weight) / NULLIF(SUM(weight), 0)
        AS mean_monthly_expenditure
    FROM household
    GROUP BY state
    ORDER BY state
  "
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' Private coaching participation and expenditure
#'
#' Weighted share of currently enrolled students who receive private
#' coaching, and the weighted mean annual coaching expenditure of those
#' students, from the `person` table.
#'
#' @inheritParams esi_expenditure_by_state
#' @return Tibble with grouping columns, `n_students`, `coaching_rate`,
#'   `mean_coaching_expenditure_coached`.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_coaching(con)
#' esi_disconnect(con)
esi_coaching <- function(con, by = "state") {
  .check_con(con)
  sql <- sprintf(
    "SELECT %s
       COUNT(*) AS n_students,
       SUM(CASE WHEN received_private_coaching = '1' THEN weight ELSE 0 END)
         / NULLIF(SUM(weight), 0) AS coaching_rate,
       SUM(CASE WHEN received_private_coaching = '1'
                THEN COALESCE(private_coaching_exp_total, 0) * weight ELSE 0 END)
         / NULLIF(SUM(CASE WHEN received_private_coaching = '1' THEN weight ELSE 0 END), 0)
         AS mean_coaching_expenditure_coached
     FROM person
     WHERE currently_enrolled_school = '1'
     %s",
    .select_by(by), .group_by(by)
  )
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

#' Expenditure line items of CMS-E, school expenditure first
#' @noRd
.ESI_EXPENSE_CATEGORIES <- c(
  "school_exp_course_fee", "school_exp_transport", "school_exp_uniform",
  "school_exp_textbooks_stationery", "school_exp_other",
  "private_coaching_exp_course_fee", "private_coaching_exp_transport",
  "private_coaching_exp_uniform", "private_coaching_exp_textbooks_material",
  "private_coaching_exp_other", "hostel_fee_expenditure"
)

#' Education expenditure by category
#'
#' Weighted mean annual expenditure of currently enrolled students on each
#' expenditure line item (course fee, transport, uniform, books, other,
#' for school and for private coaching, and hostel fee). A missing value
#' counts as 0.
#'
#' @inheritParams esi_expenditure_by_state
#' @param by Grouping column(s); default `NULL` (national).
#' @return Tibble with grouping columns, `n_students`, `category`,
#'   `mean_expenditure`, one row for each category in each group.
#' @export
#' @examples
#' con <- esi_connect(esi_example_data())
#' esi_expenditure_by_category(con)
#' esi_disconnect(con)
esi_expenditure_by_category <- function(con, by = NULL) {
  .check_con(con)
  cols <- intersect(.ESI_EXPENSE_CATEGORIES, .esi_columns(con, "person"))
  # hostel_fee_expenditure is string-typed in the Parquet, so cast every column
  means <- sprintf(
    "SUM(COALESCE(TRY_CAST(%s AS DOUBLE), 0) * weight) / NULLIF(SUM(weight), 0) AS %s",
    cols, cols
  )
  sql <- sprintf(
    "SELECT %s COUNT(*) AS n_students, %s
     FROM person WHERE currently_enrolled_school = '1' %s",
    .select_by(by), paste(means, collapse = ", "), .group_by(by)
  )
  wide <- DBI::dbGetQuery(con, sql)
  keep <- setdiff(names(wide), cols)
  long <- do.call(rbind, lapply(cols, function(col) {
    cbind(wide[keep], category = col, mean_expenditure = wide[[col]])
  }))
  tibble::as_tibble(long)
}

#' SQL fragments for an optional `by` grouping
#' @noRd
.select_by <- function(by) if (is.null(by)) "" else paste0(paste(by, collapse = ", "), ",")

#' @noRd
.group_by <- function(by) {
  if (is.null(by)) "" else sprintf("GROUP BY %1$s ORDER BY %1$s", paste(by, collapse = ", "))
}

#' Column names of a registered table
#' @noRd
.esi_columns <- function(con, table) {
  names(DBI::dbGetQuery(con, sprintf("SELECT * FROM \"%s\" LIMIT 0", table)))
}
