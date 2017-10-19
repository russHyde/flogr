# Note that this runs using devtools::test() but doesn't run using
#   devtools::check()
if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Package Style", {
    lintr::expect_lint_free()
  })
}
