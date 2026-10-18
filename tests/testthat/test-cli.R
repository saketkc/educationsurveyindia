test_that(".esi_cli_parse handles values, = form and flags", {
  spec <- .ESI_CLI_COMMANDS$build
  expect_equal(
    .esi_cli_parse(c("--output-dir", "out", "--raw-dir=raw", "--overwrite"), spec),
    list(`output-dir` = "out", `raw-dir` = "raw", overwrite = TRUE)
  )
  expect_equal(.esi_cli_parse(character(0), spec), list())
})

test_that(".esi_cli_parse rejects bad input", {
  spec <- .ESI_CLI_COMMANDS$download
  expect_error(.esi_cli_parse("--output-dir=x", spec), "Unknown option")
  expect_error(.esi_cli_parse("--raw-dir", spec), "needs a value")
  expect_error(.esi_cli_parse(c("--raw-dir", "--overwrite"), spec), "needs a value")
  expect_error(.esi_cli_parse("--raw-dir=", spec), "needs a value")
  expect_error(.esi_cli_parse("--overwrite=yes", spec), "takes no value")
  expect_error(.esi_cli_parse("stray", spec), "Unexpected argument")
})

test_that("esi_cli returns exit status", {
  expect_output(expect_equal(esi_cli(character(0)), 0L), "Usage: esi")
  expect_message(expect_equal(esi_cli("nope"), 1L), "Unknown command")
})

test_that("esi_cli info reports a built directory", {
  dir <- .make_fixture()
  expect_output(expect_equal(suppressMessages(esi_cli(c("info", "--data-dir", dir))), 0L), "household")
})

test_that("esi_cli forwards only the given options, so R defaults apply", {
  seen <- NULL
  local_mocked_bindings(
    esi_build = function(...) seen <<- list(...),
    esi_download = function(...) seen <<- list(...)
  )
  esi_cli("build")
  expect_equal(seen, list())
  esi_cli(c("build", "--output-dir", "out", "--overwrite"))
  expect_equal(seen, list(output_dir = "out", overwrite = TRUE))
  esi_cli(c("download", "--raw-dir=r"))
  expect_equal(seen, list(raw_dir = "r"))
})
