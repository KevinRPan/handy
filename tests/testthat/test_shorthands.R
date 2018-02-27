context("Feel good testing")

test_that("Test Strings", {
  expect_true(all.equal(cs(A, B, C), c("A", "B", "C")))
})

test_that("Test Lists", {
  A <- "char"
  B <- 1:5
  expect_true(all.equal(named_list(A, B), list(A = A, B = B)))
})

