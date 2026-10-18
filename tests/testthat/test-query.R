test_that("esi_school_participation computes a weighted enrolment rate per state", {
  dir <- .make_fixture()
  con <- esi_connect(dir)
  withr::defer(esi_disconnect(con))

  out <- esi_school_participation(con)
  # state 01: weights 100 (enrolled) + 200 (not) -> 100/300
  expect_equal(out$enrolment_rate[out$state == "01"], 100 / 300)
  # state 27: single enrolled record -> rate 1
  expect_equal(out$enrolment_rate[out$state == "27"], 1)
})

test_that("esi_state_household_summary computes weighted shares and means", {
  dir <- .make_fixture()
  con <- esi_connect(dir)
  withr::defer(esi_disconnect(con))

  out <- esi_state_household_summary(con)
  expect_equal(out$n_households[out$state == "01"], 2)
  # state 01: (100*1000 + 200*2000) / 300
  expect_equal(out$mean_monthly_expenditure[out$state == "01"], (100 * 1000 + 200 * 2000) / 300)
})

test_that("esi_expenditure_by_state counts a NULL expenditure as 0 in the total", {
  con <- esi_connect(.make_fixture())
  withr::defer(esi_disconnect(con))

  out <- esi_expenditure_by_state(con)
  # state 27: school 800, coaching NULL -> total 800, not NULL
  expect_equal(out$mean_total_expenditure[out$state == "27"], 800)
  expect_equal(out$mean_total_expenditure, out$mean_school_expenditure + out$mean_coaching_expenditure)
})

test_that("esi_coaching gives a weighted rate and a mean over coached students only", {
  con <- esi_connect(.make_fixture())
  withr::defer(esi_disconnect(con))

  out <- esi_coaching(con, by = NULL)
  expect_equal(out$n_students, 2)
  expect_equal(out$coaching_rate, 0)
  expect_true(is.na(out$mean_coaching_expenditure_coached)) # nobody coached
})

test_that("esi_expenditure_by_category casts string columns and counts missing as 0", {
  con <- esi_connect(.make_fixture())
  withr::defer(esi_disconnect(con))

  out <- esi_expenditure_by_category(con, by = "state")
  hostel <- out[out$category == "hostel_fee_expenditure", ]
  expect_equal(hostel$mean_expenditure[hostel$state == "01"], 100)
  expect_equal(hostel$mean_expenditure[hostel$state == "27"], 0)
  expect_setequal(unique(out$category), c("school_exp_course_fee", "hostel_fee_expenditure"))
})
