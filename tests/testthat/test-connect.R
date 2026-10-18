test_that("esi_connect registers tables and esi_tables excludes labels schema", {
  dir <- .make_fixture()
  con <- esi_connect(dir)
  withr::defer(esi_disconnect(con))

  expect_setequal(esi_tables(con), c("household", "person"))
})

test_that("esi_available_data reports row counts", {
  dir <- .make_fixture()
  con <- esi_connect(dir)
  withr::defer(esi_disconnect(con))

  avail <- esi_available_data(con)
  expect_equal(avail$n_records[avail$table_name == "household"], 4)
  expect_equal(avail$n_records[avail$table_name == "person"], 3)
})
