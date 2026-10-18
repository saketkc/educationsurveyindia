test_that("esi_find and esi_decode round-trip variable/value labels", {
  dir <- .make_fixture()
  con <- esi_connect(dir)
  withr::defer(esi_disconnect(con))

  hit <- esi_find(con, "sector")
  expect_true("sector" %in% hit$variable)

  fuzzy_hit <- esi_find(con, "sectr", fuzzy = TRUE, fuzzy_threshold = 0.6)
  expect_true(nrow(fuzzy_hit) >= 1)

  decoded <- esi_decode(con, "household", "sector", c("1", "2"))
  expect_equal(unname(decoded), c("rural", "urban"))

  # unmatched codes pass through unchanged rather than erroring
  expect_equal(unname(esi_decode(con, "household", "sector", "9")), "9")
})

test_that("esi_normalize_state maps historical/variant spellings", {
  expect_equal(esi_normalize_state("Orissa"), "Odisha")
  expect_equal(esi_normalize_state("Kerala"), "Kerala")
})

test_that("esi_class_label orders classes from pre-primary to diplomas", {
  f <- esi_class_label(c("12", "15", "1", "13", "99"))
  expect_equal(as.character(f), c("Class XII", "Pre-primary", "Class I", "Diploma (up to secondary)", NA))
  expect_equal(levels(f)[1:2], c("Pre-primary", "Class I"))
  expect_length(levels(f), 15)
})

test_that("esi_normalize_state ignores case and maps LGD spellings", {
  expect_equal(esi_normalize_state("ANDAMAN & NICOBAR"), "Andaman & Nicobar Islands")
  expect_equal(esi_normalize_state("DADRA,NAGAR HAVELI,DAMAN & DIU"), "Dadra & Nagar Haveli & Daman & Diu")
  expect_equal(esi_normalize_state("KERALA"), "Kerala")
  expect_equal(esi_normalize_state("Atlantis"), "Atlantis")
})
