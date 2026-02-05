# Tests for COVID-19 Case Study

library(R6)

source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/explanatory_classes.R")
source("R/explanatory_case_study.R")

tests_run <- 0
tests_passed <- 0

test_that <- function(description, expr) {
  tests_run <<- tests_run + 1
  cat(sprintf("Test %d: %s... ", tests_run, description))

  tryCatch({
    expr
    tests_passed <<- tests_passed + 1
    cat("✓ PASS\n")
    TRUE
  }, error = function(e) {
    cat(sprintf("✗ FAIL\n  Error: %s\n", e$message))
    FALSE
  })
}

expect_equal <- function(actual, expected, tolerance = 0.0001) {
  if (is.numeric(actual) && is.numeric(expected)) {
    if (abs(actual - expected) > tolerance) {
      stop(sprintf("Expected %.4f, got %.4f", expected, actual))
    }
  } else if (!identical(actual, expected)) {
    stop(sprintf("Expected %s, got %s", expected, actual))
  }
}

expect_true <- function(condition) {
  if (!condition) {
    stop("Expected TRUE, got FALSE")
  }
}

cat("=== COVID-19 Case Study Tests ===\n\n")

# Test case study loads
test_that("create_covid_case_study returns ExplanatoryModel", {
  model <- create_covid_case_study()
  expect_true(inherits(model, "ExplanatoryModel"))
})

# Test has correct structure
test_that("COVID case study has 2 hypotheses, 2 accounts, 6 results", {
  model <- create_covid_case_study()

  expect_equal(length(model$hypotheses), 2)
  expect_equal(length(model$accounts), 2)
  expect_equal(length(model$results), 6)
})

# Test hypothesis priors
test_that("Hypotheses have equal priors (0.5 each)", {
  model <- create_covid_case_study()

  expect_equal(model$hypotheses[["H1"]]$prior, 0.5)
  expect_equal(model$hypotheses[["H2"]]$prior, 0.5)
  expect_true(model$validate_hypothesis_priors())
})

# Test results exist
test_that("All 6 results are present (R1-R6)", {
  model <- create_covid_case_study()

  expect_true("R1" %in% names(model$results))
  expect_true("R2" %in% names(model$results))
  expect_true("R3" %in% names(model$results))
  expect_true("R4" %in% names(model$results))
  expect_true("R5" %in% names(model$results))
  expect_true("R6" %in% names(model$results))
})

# Test auxiliaries
test_that("Account 1 has 4 auxiliaries (A1-A4)", {
  model <- create_covid_case_study()
  acc1 <- model$accounts[["Acc1"]]

  expect_equal(length(acc1$auxiliary_ids), 4)
  expect_true("A1" %in% acc1$auxiliary_ids)
  expect_true("A2" %in% acc1$auxiliary_ids)
  expect_true("A3" %in% acc1$auxiliary_ids)
  expect_true("A4" %in% acc1$auxiliary_ids)
})

# Test phenomena
test_that("Account 1 has 3 phenomena (PC1-PC3)", {
  model <- create_covid_case_study()
  acc1 <- model$accounts[["Acc1"]]

  expect_equal(length(acc1$phenomenon_ids), 3)
  expect_true("PC1" %in% acc1$phenomenon_ids)
  expect_true("PC2" %in% acc1$phenomenon_ids)
  expect_true("PC3" %in% acc1$phenomenon_ids)
})

# Test explanatory links
test_that("Account 1 has 7 explanatory links", {
  model <- create_covid_case_study()
  acc1 <- model$accounts[["Acc1"]]

  expect_equal(length(acc1$explanatory_link_ids), 7)
})

cat(sprintf("\n=== Summary: %d/%d tests passed ===\n", tests_passed, tests_run))
