# Unit Tests for Bayesian Spec Classes
#
# Tests the R6 classes in R/bayesian_spec_classes.R following the specification

library(R6)

# Source required modules
source("R/classes.R")
source("R/bayesian_spec_classes.R")

# Test counter
tests_run <- 0
tests_passed <- 0

# Helper function for test assertions
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

# Helper for assertions
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

expect_error <- function(expr, pattern = NULL) {
  error_occurred <- FALSE
  tryCatch({
    expr
  }, error = function(e) {
    error_occurred <<- TRUE
    if (!is.null(pattern) && !grepl(pattern, e$message)) {
      stop(sprintf("Error message '%s' did not match pattern '%s'", e$message, pattern))
    }
  })

  if (!error_occurred) {
    stop("Expected an error but none occurred")
  }
}

expect_warning <- function(expr, pattern = NULL) {
  warning_occurred <- FALSE
  tryCatch({
    expr
  }, warning = function(w) {
    warning_occurred <<- TRUE
    if (!is.null(pattern) && !grepl(pattern, w$message)) {
      stop(sprintf("Warning message '%s' did not match pattern '%s'", w$message, pattern))
    }
  })

  if (!warning_occurred) {
    stop("Expected a warning but none occurred")
  }
}

cat("=== Bayesian Spec Classes Unit Tests ===\n\n")

# ===== VALIDATION FUNCTION TESTS =====

cat("--- Validation Functions ---\n")

test_that("validate_probability accepts valid values", {
  validate_probability(0.0, "test")
  validate_probability(0.5, "test")
  validate_probability(1.0, "test")
  expect_true(TRUE)
})

test_that("validate_probability rejects values < 0", {
  expect_error(validate_probability(-0.1, "test"), "must be between 0 and 1")
})

test_that("validate_probability rejects values > 1", {
  expect_error(validate_probability(1.5, "test"), "must be between 0 and 1")
})

test_that("validate_probability rejects non-numeric values", {
  expect_error(validate_probability("abc", "test"), "must be a single numeric value")
})

test_that("validate_probability rejects NULL", {
  expect_error(validate_probability(NULL, "test"), "must be a single numeric value")
})

# ===== BAYESIAN HYPOTHESIS TESTS =====

cat("\n--- BayesianHypothesis Class ---\n")

test_that("BayesianHypothesis can be created with valid parameters", {
  h <- BayesianHypothesis$new(
    id = "H1",
    text = "Test hypothesis",
    source = "Test",
    prior = 0.6
  )

  expect_equal(h$id, "H1")
  expect_equal(h$type, "Hypothesis")
  expect_equal(h$prior, 0.6)
  expect_equal(h$level, "primary")
})

test_that("BayesianHypothesis uses default prior of 0.5", {
  h <- BayesianHypothesis$new(
    id = "H2",
    text = "Test",
    source = "Test",
  )

  expect_equal(h$prior, 0.5)
})

test_that("BayesianHypothesis rejects invalid prior", {
  expect_error({
    BayesianHypothesis$new(
      id = "H3",
      text = "Test",
      source = "Test",
      prior = 1.5
    )
  }, "must be between 0 and 1")
})

test_that("BayesianHypothesis to_list includes prior", {
  h <- BayesianHypothesis$new(
    id = "H4",
    text = "Test",
    source = "Test",
    prior = 0.7
  )

  lst <- h$to_list()
  expect_equal(lst$prior, 0.7)
  expect_equal(lst$type, "Hypothesis")
})

# ===== BAYESIAN AUXILIARY TESTS =====

cat("\n--- BayesianAuxiliary Class ---\n")

test_that("BayesianAuxiliary can be created with valid parameters", {
  a <- BayesianAuxiliary$new(
    id = "A1",
    text = "Test auxiliary",
    source = "Test",
    prior = 0.8
  )

  expect_equal(a$id, "A1")
  expect_equal(a$type, "AuxiliaryClaim")
  expect_equal(a$prior, 0.8)
  expect_equal(a$subtype, "BackgroundTheory")
})

test_that("BayesianAuxiliary uses default prior of 0.5", {
  a <- BayesianAuxiliary$new(
    id = "A2",
    text = "Test",
    source = "Test",
  )

  expect_equal(a$prior, 0.5)
})

test_that("BayesianAuxiliary to_list includes prior", {
  a <- BayesianAuxiliary$new(
    id = "A3",
    text = "Test",
    source = "Test",
    prior = 0.75
  )

  lst <- a$to_list()
  expect_equal(lst$prior, 0.75)
  expect_equal(lst$type, "AuxiliaryClaim")
})

# ===== BAYESIAN EXPLANATORY LINK TESTS =====

cat("\n--- BayesianExplanatoryLink Class ---\n")

test_that("BayesianExplanatoryLink can be created with valid parameters", {
  link <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "Test link",
    source = "Test",
    reliability = 0.90,
    epsilon = 0.01
  )

  expect_equal(link$id, "EL1")
  expect_equal(link$type, "ExplanatoryLink")
  expect_equal(link$source_ids, "H1")
  expect_equal(link$target_id, "P1")
  expect_equal(link$reliability, 0.90)
  expect_equal(link$epsilon, 0.01)
})

test_that("BayesianExplanatoryLink uses default reliability 0.95 and epsilon 0.01", {
  link <- BayesianExplanatoryLink$new(
    id = "EL2",
    source_id = "H1",
    target_id = "P1",
    text = "Test",
    source = "Test",
  )

  expect_equal(link$reliability, 0.95)
  expect_equal(link$epsilon, 0.01)
})

test_that("BayesianExplanatoryLink rejects invalid reliability", {
  expect_error({
    BayesianExplanatoryLink$new(
      id = "EL3",
      source_id = "H1",
      target_id = "P1",
      text = "Test",
      source = "Test",
      reliability = 1.2
    )
  }, "must be between 0 and 1")
})

test_that("BayesianExplanatoryLink warns if epsilon > 0.1", {
  expect_warning({
    BayesianExplanatoryLink$new(
      id = "EL4",
      source_id = "H1",
      target_id = "P1",
      text = "Test",
      source = "Test",
      epsilon = 0.15
    )
  }, "epsilon.*should typically be")
})

test_that("BayesianExplanatoryLink to_list includes reliability and epsilon", {
  link <- BayesianExplanatoryLink$new(
    id = "EL5",
    source_id = "H1",
    target_id = "P1",
    text = "Test",
    source = "Test",
    reliability = 0.92,
    epsilon = 0.02
  )

  lst <- link$to_list()
  expect_equal(lst$reliability, 0.92)
  expect_equal(lst$epsilon, 0.02)
  expect_equal(lst$type, "ExplanatoryLink")
})

# ===== BAYESIAN PHENOMENON TESTS =====

cat("\n--- BayesianPhenomenon Class ---\n")

test_that("BayesianPhenomenon can be created with valid parameters", {
  p <- BayesianPhenomenon$new(
    id = "P1",
    text = "Test phenomenon",
    source = "Test",
    strength = 0.90,
    base_rate = 0.05
  )

  expect_equal(p$id, "P1")
  expect_equal(p$type, "PhenomenonClaim")
  expect_equal(p$strength, 0.90)
  expect_equal(p$base_rate, 0.05)
})

test_that("BayesianPhenomenon uses defaults strength=0.90, base_rate=0.05", {
  p <- BayesianPhenomenon$new(
    id = "P2",
    text = "Test",
    source = "Test",
  )

  expect_equal(p$strength, 0.90)
  expect_equal(p$base_rate, 0.05)
})

test_that("BayesianPhenomenon warns if strength < base_rate", {
  expect_warning({
    BayesianPhenomenon$new(
      id = "P3",
      text = "Test",
      source = "Test",
      strength = 0.30,
      base_rate = 0.50
    )
  }, "strength.*base_rate.*LESS likely")
})

test_that("BayesianPhenomenon to_list includes strength and base_rate", {
  p <- BayesianPhenomenon$new(
    id = "P4",
    text = "Test",
    source = "Test",
    strength = 0.88,
    base_rate = 0.10
  )

  lst <- p$to_list()
  expect_equal(lst$strength, 0.88)
  expect_equal(lst$base_rate, 0.10)
  expect_equal(lst$type, "PhenomenonClaim")
})

# ===== BAYESIAN RESULT TESTS =====

cat("\n--- BayesianResult Class ---\n")

test_that("BayesianResult can be created with valid parameters", {
  r <- BayesianResult$new(
    id = "R1",
    text = "Test result",
    source = "Test",
    strength = 0.95,
    base_rate = 0.10,
    result_type = "Quantitative",
    sample_size = 100
  )

  expect_equal(r$id, "R1")
  expect_equal(r$type, "EmpiricalResult")
  expect_equal(r$strength, 0.95)
  expect_equal(r$base_rate, 0.10)
  expect_equal(r$result_type, "Quantitative")
  expect_equal(r$sample_size, 100)
})

test_that("BayesianResult uses defaults strength=0.95, base_rate=0.10", {
  r <- BayesianResult$new(
    id = "R2",
    text = "Test",
    source = "Test",
  )

  expect_equal(r$strength, 0.95)
  expect_equal(r$base_rate, 0.10)
})

test_that("BayesianResult to_list includes strength and base_rate", {
  r <- BayesianResult$new(
    id = "R3",
    text = "Test",
    source = "Test",
    strength = 0.93,
    base_rate = 0.12
  )

  lst <- r$to_list()
  expect_equal(lst$strength, 0.93)
  expect_equal(lst$base_rate, 0.12)
  expect_equal(lst$type, "EmpiricalResult")
})

# ===== INHERITANCE TESTS =====

cat("\n--- Inheritance Tests ---\n")

test_that("BayesianHypothesis inherits from Hypothesis", {
  h <- BayesianHypothesis$new(
    id = "H_inherit",
    text = "Test",
    source = "Test",
  )

  expect_true(inherits(h, "BayesianHypothesis"))
  expect_true(inherits(h, "Hypothesis"))
  expect_true(inherits(h, "Node"))
})

test_that("BayesianAuxiliary inherits from AuxiliaryClaim", {
  a <- BayesianAuxiliary$new(
    id = "A_inherit",
    text = "Test",
    source = "Test",
  )

  expect_true(inherits(a, "BayesianAuxiliary"))
  expect_true(inherits(a, "AuxiliaryClaim"))
  expect_true(inherits(a, "Node"))
})

test_that("BayesianExplanatoryLink inherits from ExplanatoryLink", {
  link <- BayesianExplanatoryLink$new(
    id = "EL_inherit",
    source_id = "H1",
    target_id = "P1",
    text = "Test",
    source = "Test",
  )

  expect_true(inherits(link, "BayesianExplanatoryLink"))
  expect_true(inherits(link, "ExplanatoryLink"))
  expect_true(inherits(link, "Node"))
})

test_that("BayesianPhenomenon inherits from PhenomenonClaim", {
  p <- BayesianPhenomenon$new(
    id = "P_inherit",
    text = "Test",
    source = "Test",
  )

  expect_true(inherits(p, "BayesianPhenomenon"))
  expect_true(inherits(p, "PhenomenonClaim"))
  expect_true(inherits(p, "Node"))
})

test_that("BayesianResult inherits from EmpiricalResult", {
  r <- BayesianResult$new(
    id = "R_inherit",
    text = "Test",
    source = "Test",
  )

  expect_true(inherits(r, "BayesianResult"))
  expect_true(inherits(r, "EmpiricalResult"))
  expect_true(inherits(r, "Node"))
})

# ===== SUMMARY =====

cat("\n=== Test Summary ===\n")
cat(sprintf("Tests run: %d\n", tests_run))
cat(sprintf("Tests passed: %d\n", tests_passed))
cat(sprintf("Tests failed: %d\n", tests_run - tests_passed))

if (tests_passed == tests_run) {
  cat("\n✓ All tests passed!\n")
  quit(status = 0)
} else {
  cat("\n✗ Some tests failed\n")
  quit(status = 1)
}
