#----------------------------------------------------------------------------
#
# Tests for general utililty formatting functions
#
#----------------------------------------------------------------------------

context("general util - fmt_int_or_decimal tests")

test_that("fmt_int_or_decimal returns characters", {
  expect_equal(class(fmt_int_or_decimal(2, 1)), "character")
})

test_that("fmt_int_or_decimal formats integers", {
  expect_equal(fmt_int_or_decimal(1, 2), "1")
  expect_equal(fmt_int_or_decimal(99, 0), "99")
  expect_equal(fmt_int_or_decimal(-1, 2), "-1")
})

test_that("fmt_int_or_decimal formats floats", {
  expect_equal(fmt_int_or_decimal(1.0, 2), "1")
  expect_equal(fmt_int_or_decimal(1.000001, 2), "1.00")
  expect_equal(fmt_int_or_decimal(99.18, 0), "99")
  expect_equal(fmt_int_or_decimal(-1, 2), "-1")
  expect_equal(fmt_int_or_decimal(-1.00001, 2), "-1.00")
})

test_that("fmt_int_or_decimal formats and rounds floats", {
  expect_equal(fmt_int_or_decimal(1.055, 2), "1.05")
  expect_equal(fmt_int_or_decimal(1.056, 2), "1.06")
  expect_equal(fmt_int_or_decimal(99.78, 0), "100")
  expect_equal(fmt_int_or_decimal(-1.566, 2), "-1.57")
})
