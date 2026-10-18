# State codes and geographic grouping

#' NSS state code mapping (CMS-E, 80th round)
#'
#' Named character vector mapping 2-digit NSS state codes (first 2 digits
#' of `nss_region`) to state names, per the "State code" sheet of
#' `Data_Layout_CMSE_2025.xlsx`.
#'
#' @format A named character vector of length 36.
#' @export
#' @examples
#' ESI_STATE_CODES[["27"]]
ESI_STATE_CODES <- c(
  "01" = "Jammu & Kashmir",
  "02" = "Himachal Pradesh",
  "03" = "Punjab",
  "04" = "Chandigarh",
  "05" = "Uttarakhand",
  "06" = "Haryana",
  "07" = "Delhi",
  "08" = "Rajasthan",
  "09" = "Uttar Pradesh",
  "10" = "Bihar",
  "11" = "Sikkim",
  "12" = "Arunachal Pradesh",
  "13" = "Nagaland",
  "14" = "Manipur",
  "15" = "Mizoram",
  "16" = "Tripura",
  "17" = "Meghalaya",
  "18" = "Assam",
  "19" = "West Bengal",
  "20" = "Jharkhand",
  "21" = "Odisha",
  "22" = "Chhattisgarh",
  "23" = "Madhya Pradesh",
  "24" = "Gujarat",
  "25" = "Dadra & Nagar Haveli & Daman & Diu",
  "27" = "Maharashtra",
  "28" = "Andhra Pradesh",
  "29" = "Karnataka",
  "30" = "Goa",
  "31" = "Lakshadweep",
  "32" = "Kerala",
  "33" = "Tamil Nadu",
  "34" = "Puducherry",
  "35" = "Andaman & Nicobar Islands",
  "36" = "Telangana",
  "37" = "Ladakh"
)

#' Historical/variant state name aliases -> canonical name
#' @noRd
.STATE_ALIASES <- c(
  "Orissa" = "Odisha",
  "Pondicherry" = "Puducherry",
  "Uttaranchal" = "Uttarakhand",
  "Tamilnadu" = "Tamil Nadu",
  "Puduchery" = "Puducherry",
  "A & N Islands" = "Andaman & Nicobar Islands",
  "A&N Islands" = "Andaman & Nicobar Islands",
  "D & N. Haveli & Daman & Diu" = "Dadra & Nagar Haveli & Daman & Diu",
  "Dadra,Nagar Haveli,Daman & Diu" = "Dadra & Nagar Haveli & Daman & Diu",
  "Andaman & Nicobar" = "Andaman & Nicobar Islands"
)

#' Normalize a state name to the canonical spelling
#'
#' @param state Character vector of state names.
#' @return Character vector with historical/variant spellings mapped to
#'   the canonical name, ignoring case; unmatched values pass through
#'   unchanged.
#' @export
#' @examples
#' esi_normalize_state(c("Orissa", "Kerala"))
esi_normalize_state <- function(state) {
  # case-insensitive: LGD boundary files spell names in upper case
  canonical <- unique(c(unname(ESI_STATE_CODES), .STATE_ALIASES))
  lookup <- c(stats::setNames(canonical, toupper(canonical)), stats::setNames(.STATE_ALIASES, toupper(names(.STATE_ALIASES))))
  hit <- unname(lookup[toupper(state)])
  ifelse(is.na(hit), state, hit)
}

#' Group by state
#'
#' @param tbl A `tbl_dbi` or data frame with a `state` column.
#' @return The input with `state` added to grouping.
#' @export
#' @examples
#' esi_by_state(data.frame(state = c("01", "27"), x = 1:2))
esi_by_state <- function(tbl) {
  dplyr::group_by(tbl, .data$state, .add = TRUE)
}

#' Build a SQL CASE expression mapping state code to state name
#' @noRd
.state_name_case <- function(col = "state") {
  whens <- vapply(names(ESI_STATE_CODES), function(code) {
    sprintf("WHEN %s = '%s' THEN '%s'", col, code, gsub("'", "''", ESI_STATE_CODES[[code]]))
  }, character(1))
  paste("CASE", paste(whens, collapse = " "), sprintf("ELSE %s END", col))
}

#' Attach LGD state boundaries for choropleth maps
#'
#' Downloads (and caches for the session) LGD state boundaries from
#' bharatviz.org
#' and left-joins them onto `df` by normalized state name.
#'
#' @param df Data frame with a `state` column (state names, not codes).
#' @return An `sf` object.
#' @export
#' @examplesIf requireNamespace("sf", quietly = TRUE) && interactive()
#' esi_attach_geometry(data.frame(state = "Kerala", value = 1))
esi_attach_geometry <- function(df) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg sf} is required. Install with {.code install.packages('sf')}.")
  }
  geo <- .esi_get_state_boundaries()
  df$.state_norm <- esi_normalize_state(df$state)
  geo$.state_norm <- esi_normalize_state(geo$state_name)
  dplyr::left_join(geo, df, by = ".state_norm", suffix = c("", ".data"))
}

.geo_cache <- new.env(parent = emptyenv())

#' @noRd
.esi_get_state_boundaries <- function() {
  if (!is.null(.geo_cache$states)) {
    return(.geo_cache$states)
  }
  url <- "https://geo.bharatviz.org/geojsons/admin/India-geodata-lgd-states.geojson"
  geo <- sf::st_read(url, quiet = TRUE)
  name_col <- intersect(c("state_name", "STATE", "st_nm", "name"), names(geo))[1]
  if (is.na(name_col) || is.null(name_col)) {
    cli::cli_abort("Could not find a state name column in the downloaded GeoJSON.")
  }
  geo$state_name <- geo[[name_col]]
  .geo_cache$states <- geo
  geo
}
