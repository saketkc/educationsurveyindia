# Registry of every NSS education survey on microdata.gov.in

#' NSS education surveys on the MoSPI microdata portal
#'
#' Every education survey round in the MoSPI microdata catalog, checked
#' against the full catalog listing on 2026-10-18. Only the 80th round
#' (CMS-E 2025) can be built into Parquet with [esi_build()]; the other
#' rounds can be downloaded with [esi_download_all()].
#'
#' The 52nd round's portal id ends in a dot, which the portal's API cannot
#' route (every request 404s as of 2026-10-18), so [esi_download_all()]
#' reports it as missing; download it from its catalog page (id 82) instead.
#'
#' @return A tibble with columns `round`, `period`, `title`, `idno`
#'   (portal dataset id), `catalog_id` and `folder` (subdirectory used by
#'   [esi_download_all()]).
#' @export
#' @examples
#' esi_surveys()
esi_surveys <- function() {
  round <- c("42", "47", "52", "64", "71", "75", "80")
  period <- c("1986-87", "1991", "1995-96", "2007-08", "2014", "2017-18", "2025")
  tibble::tibble(
    round = round,
    period = period,
    title = c(
      "Survey on Participation in Education",
      "Survey on Literacy and Culture",
      "Participation in Education",
      "Participation and Expenditure in Education",
      "Social Consumption - Education",
      "Household Social Consumption: Education (Schedule 25.2)",
      "Comprehensive Modular Survey on Education (CMS-E)"
    ),
    idno = c(
      "IND-NSSO-SPE-1986-v1.0",
      "IND-NSSO-SLC-1991-v1.0",
      "IND-MOSPI-NSSO-NSS-52-25.2-1995-V2.",
      "DDI-IND-MOSPI-NSSO-64-25-2-2011",
      "IND-NSSO-SCES-2014-v1.0",
      "DDI-IND-MOSPI-NSSO-75Rnd-Sch25.2-July2017-June2018",
      .ESI_IDNO
    ),
    catalog_id = c(51L, 68L, 82L, 300L, 136L, 151L, 255L),
    folder = paste0("nss", round, "_", gsub("-", "_", period))
  )
}

#' Download every NSS education survey round
#'
#' Downloads all files of every dataset in [esi_surveys()]: microdata in
#' each format the portal offers (CSV, STATA, SPSS, SAS, JSON, ...), the
#' DDI codebook, questionnaires, reports and code lists. Each round goes
#' into its own subdirectory of `dest_dir`. Files already there are kept
#' unless `overwrite = TRUE`, so an interrupted run can be repeated to
#' pick up what is missing.
#'
#' @param dest_dir Destination directory. Defaults to a `rounds`
#'   subdirectory of `getOption("esi.data_dir")`, or of
#'   [tools::R_user_dir()] when that option is unset.
#' @param rounds Character vector of rounds to download (see the `round`
#'   column of [esi_surveys()]). `NULL` (default) downloads all of them.
#' @param api_key MoSPI API key. If `NULL`, reads `MOSPI_KEY` from the
#'   environment.
#' @param overwrite If `TRUE`, download files again even if they exist.
#' @return Invisibly, a tibble with one row per round: `round`, `idno`,
#'   `dir`, `n_files` (files listed on the portal) and `missing` (a list
#'   column of file names that could not be downloaded).
#' @export
#' @examplesIf nzchar(Sys.getenv("MOSPI_KEY")) && interactive()
#' esi_download_all(tempfile(), rounds = "80")
esi_download_all <- function(dest_dir = file.path(.esi_default_dir(), "rounds"),
                             rounds = NULL,
                             api_key = NULL,
                             overwrite = FALSE) {
  api_key <- .esi_api_key(api_key)
  surveys <- esi_surveys()
  if (!is.null(rounds)) {
    bad <- setdiff(rounds, surveys$round)
    if (length(bad) > 0L) {
      cli::cli_abort("Unknown round{?s} {.val {bad}}; see {.fn esi_surveys}.")
    }
    surveys <- surveys[surveys$round %in% rounds, ]
  }

  dest_dir <- path.expand(dest_dir)
  results <- lapply(seq_len(nrow(surveys)), function(i) {
    s <- surveys[i, ]
    .esi_download_round(s$idno, file.path(dest_dir, s$folder), api_key, overwrite)
  })
  out <- tibble::tibble(
    round = surveys$round,
    idno = surveys$idno,
    dir = file.path(dest_dir, surveys$folder),
    n_files = vapply(results, `[[`, integer(1), "n_files"),
    missing = lapply(results, `[[`, "missing")
  )

  n_missing <- sum(lengths(out$missing))
  if (n_missing > 0L) {
    cli::cli_warn(c(
      "{n_missing} file{?s} could not be downloaded.",
      "i" = "Re-run {.fn esi_download_all} to retry; see the {.field missing} column."
    ))
  } else {
    cli::cli_inform(c("v" = "All {sum(out$n_files)} files of {nrow(out)} round{?s} in {.path {dest_dir}}"))
  }
  invisible(out)
}

#' Download all files of one dataset, returning what is still missing
#' @noRd
.esi_download_round <- function(idno, dir, api_key, overwrite) {
  cli::cli_inform(c("i" = "Dataset {.val {idno}}"))
  files <- mospiR::list_files(idno, api_key)
  if (is.null(files) || nrow(files) == 0L) {
    cli::cli_warn("Cannot list files for {.val {idno}}; skipping.")
    return(list(n_files = NA_integer_, missing = NA_character_))
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (f in files$name) {
    if (!overwrite && file.exists(file.path(dir, f))) next
    mospiR::download_file(idno, f, dir, api_key, .files_cache = files)
  }
  list(
    n_files = nrow(files),
    missing = files$name[!file.exists(file.path(dir, files$name))]
  )
}
