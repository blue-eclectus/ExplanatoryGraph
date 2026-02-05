# Unit Tests for Selector and Account CPT Generation

library(R6)

source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/explanatory_classes.R")
source("R/bayesian_spec_cpt.R")

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

cat("=== Selector CPT Generation Tests ===\n\n")

# Helper to create test model
create_test_model <- function() {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 0.6)
  h2 <- BayesianHypothesis$new("H2", "Hyp 2", "Test", 0.8, prior = 0.4)
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc2a <- Account$new("Acc2a", "H2", "Account 2a")
  acc2b <- Account$new("Acc2b", "H2", "Account 2b")
  model$add_account(acc1a)
  model$add_account(acc2a)
  model$add_account(acc2b)

  model
}

# Test Selector CPT
test_that("generate_selector_cpt creates correct prior distribution", {
  selector <- Selector$new(create_test_model())
  cpt <- generate_selector_cpt(selector)

  # Should have states matching account IDs
  expect_equal(length(cpt$states), 3)
  expect_equal(cpt$states, selector$account_ids)

  # No parents
  expect_equal(length(cpt$parents), 0)

  # Probabilities should match selector priors
  expect_equal(cpt$probabilities[["Acc1a"]], 0.6)
  expect_equal(cpt$probabilities[["Acc2a"]], 0.2)
  expect_equal(cpt$probabilities[["Acc2b"]], 0.2)

  # Should sum to 1
  expect_equal(sum(cpt$probabilities), 1.0)
})

# Test Account CPT (deterministic given selector)
test_that("generate_account_cpt creates deterministic CPT", {
  selector <- Selector$new(create_test_model())
  cpt <- generate_account_cpt("Acc1a", selector)

  # Binary states
  expect_equal(cpt$states, c("true", "false"))

  # Parent is selector
  expect_equal(cpt$parents, c("Selector"))

  # Deterministic: P(Acc1a=true | S=Acc1a) = 1
  expect_equal(cpt$probabilities["true", "Acc1a"], 1.0)
  expect_equal(cpt$probabilities["false", "Acc1a"], 0.0)

  # P(Acc1a=true | S=Acc2a) = 0
  expect_equal(cpt$probabilities["true", "Acc2a"], 0.0)
  expect_equal(cpt$probabilities["false", "Acc2a"], 1.0)
})

cat(sprintf("\n=== Summary: %d/%d tests passed ===\n", tests_passed, tests_run))
