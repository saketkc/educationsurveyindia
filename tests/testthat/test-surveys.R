test_that("esi_surveys lists one row per round with unique ids and folders", {
  s <- esi_surveys()
  expect_equal(nrow(s), 7)
  expect_false(anyDuplicated(s$idno) > 0 || anyDuplicated(s$folder) > 0)
  expect_true(.ESI_IDNO %in% s$idno)
})

test_that("esi_download_all skips existing files and reports missing ones", {
  dest <- tempfile()
  dir <- file.path(dest, esi_surveys()$folder[esi_surveys()$round == "80"])
  dir.create(dir, recursive = TRUE)
  file.create(file.path(dir, "have.xml"))
  fetched <- character(0)
  local_mocked_bindings(
    list_files = function(...) data.frame(name = c("have.xml", "ok.zip", "fail.pdf")),
    download_file = function(idno, f, dir, ...) {
      fetched <<- c(fetched, f)
      if (f == "ok.zip") file.create(file.path(dir, f))
      invisible(NULL)
    },
    .package = "mospiR"
  )
  expect_warning(
    res <- suppressMessages(esi_download_all(dest, rounds = "80", api_key = "k")),
    "could not be downloaded"
  )
  expect_equal(fetched, c("ok.zip", "fail.pdf"))
  expect_equal(res$n_files, 3L)
  expect_equal(res$missing[[1]], "fail.pdf")
  expect_error(esi_download_all(dest, rounds = "99", api_key = "k"), "Unknown round")
})
