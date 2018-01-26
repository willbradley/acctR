# Test that format_acct works!

x <- c(1000, -1000, NA)

test_that("format_acct returns expected output", {

  expect_error(format_acct('test'))
  expect_true(any(is.na(format_acct(x))))
  expect_true(identical(format_acct(x[1]), "$1,000.00"))
  expect_true(identical(format_acct(x[2]), "$(1,000.00)"))
  expect_true(identical(format_acct(x[1], shown.in = 'k'), "$1.00K"))

})
