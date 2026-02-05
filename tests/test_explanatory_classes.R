# Unit Tests for Explanatory Framework Classes

library(R6)

# Source required modules
source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/explanatory_classes.R")

# Test counter
tests_run <- 0
tests_passed <- 0

# Helper functions (same pattern as existing tests)
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

expect_error <- function(expr, pattern = NULL) {
  error_occurred <- FALSE
  error_msg <- NULL
  tryCatch({
    expr
  }, error = function(e) {
    error_occurred <<- TRUE
    error_msg <<- e$message
  })
  if (!error_occurred) {
    stop("Expected an error but none occurred")
  }
  if (!is.null(pattern) && !grepl(pattern, error_msg)) {
    stop(sprintf("Error message '%s' does not match pattern '%s'", error_msg, pattern))
  }
}

cat("=== Explanatory Framework Classes Unit Tests ===\n\n")

# Test ExplanatoryModel creation
test_that("ExplanatoryModel can be created", {
  model <- ExplanatoryModel$new(name = "Test Model")
  expect_equal(model$name, "Test Model")
  expect_equal(length(model$results), 0)
  expect_equal(length(model$hypotheses), 0)
  expect_equal(length(model$accounts), 0)
})

# Test adding results
test_that("ExplanatoryModel can add results", {
  model <- ExplanatoryModel$new(name = "Test")
  result <- BayesianResult$new(
    id = "R1",
    text = "Test result",
    source = "Test",
    strength = 0.95,
    base_rate = 0.10
  )
  model$add_result(result)
  expect_equal(length(model$results), 1)
  expect_equal(model$results[["R1"]]$id, "R1")
})

# Test adding hypotheses with prior validation
test_that("ExplanatoryModel validates hypothesis priors sum to 1", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 0.6)
  h2 <- BayesianHypothesis$new("H2", "Hyp 2", "Test", 0.8, prior = 0.4)

  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  expect_true(model$validate_hypothesis_priors())
})

# Test Account creation
test_that("Account can be created with hypothesis_id", {
  account <- Account$new(
    id = "Acc1a",
    hypothesis_id = "H1",
    description = "Aerosol with sampling damage explanation"
  )
  expect_equal(account$id, "Acc1a")
  expect_equal(account$hypothesis_id, "H1")
  expect_equal(account$relative_weight, 1)
})

# Test Account tracks components
test_that("Account tracks phenomena and auxiliaries", {
  account <- Account$new(
    id = "Acc1a",
    hypothesis_id = "H1",
    description = "Test"
  )

  account$add_phenomenon_id("PC1")
  account$add_auxiliary_id("A1")
  account$add_explanatory_link_id("EL1")

  expect_equal(length(account$phenomenon_ids), 1)
  expect_equal(length(account$auxiliary_ids), 1)
  expect_equal(length(account$explanatory_link_ids), 1)
})

# Test compute_prior: single account
test_that("Account compute_prior: single account gets full hypothesis prior", {
  account <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Test account"
  )

  prior <- account$compute_prior(hypothesis_prior = 0.6, total_weight = 1)
  expect_equal(prior, 0.6)
})

# Test compute_prior: equal weights
test_that("Account compute_prior: equal weights split prior correctly", {
  account1 <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Account 1",
    relative_weight = 1
  )
  account2 <- Account$new(
    id = "Acc2",
    hypothesis_id = "H1",
    description = "Account 2",
    relative_weight = 1
  )

  total_weight <- account1$relative_weight + account2$relative_weight
  prior1 <- account1$compute_prior(hypothesis_prior = 0.6, total_weight = total_weight)
  prior2 <- account2$compute_prior(hypothesis_prior = 0.6, total_weight = total_weight)

  expect_equal(prior1, 0.3)
  expect_equal(prior2, 0.3)
  expect_equal(prior1 + prior2, 0.6)
})

# Test compute_prior: unequal weights
test_that("Account compute_prior: unequal weights allocate correctly", {
  account1 <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Account 1",
    relative_weight = 2
  )
  account2 <- Account$new(
    id = "Acc2",
    hypothesis_id = "H1",
    description = "Account 2",
    relative_weight = 1
  )

  total_weight <- account1$relative_weight + account2$relative_weight
  prior1 <- account1$compute_prior(hypothesis_prior = 0.6, total_weight = total_weight)
  prior2 <- account2$compute_prior(hypothesis_prior = 0.6, total_weight = total_weight)

  expect_equal(prior1, 0.4)  # 0.6 * (2/3)
  expect_equal(prior2, 0.2)  # 0.6 * (1/3)
  expect_equal(prior1 + prior2, 0.6)
})

# Test duplicate account prevention
test_that("ExplanatoryModel prevents duplicate account IDs", {
  model <- ExplanatoryModel$new(name = "Test")
  account1 <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "First account"
  )
  account2 <- Account$new(
    id = "Acc1",  # Same ID
    hypothesis_id = "H1",
    description = "Duplicate account"
  )

  model$add_account(account1)
  # Second add should throw an error (prevents duplicate IDs)
  expect_error(model$add_account(account2), "already exists")
  expect_equal(length(model$accounts), 1)
  expect_equal(model$accounts[["Acc1"]]$description, "First account")
})

# Test Account validation: empty id
test_that("Account rejects empty id", {
  tryCatch({
    account <- Account$new(
      id = "",
      hypothesis_id = "H1",
      description = "Test"
    )
    stop("Should have thrown error for empty id")
  }, error = function(e) {
    if (!grepl("id must be a non-empty string", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Account validation: empty hypothesis_id
test_that("Account rejects empty hypothesis_id", {
  tryCatch({
    account <- Account$new(
      id = "Acc1",
      hypothesis_id = "",
      description = "Test"
    )
    stop("Should have thrown error for empty hypothesis_id")
  }, error = function(e) {
    if (!grepl("hypothesis_id must be a non-empty string", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Account validation: empty description
test_that("Account rejects empty description", {
  tryCatch({
    account <- Account$new(
      id = "Acc1",
      hypothesis_id = "H1",
      description = ""
    )
    stop("Should have thrown error for empty description")
  }, error = function(e) {
    if (!grepl("description must be a non-empty string", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Account validation: negative relative_weight
test_that("Account rejects negative relative_weight", {
  tryCatch({
    account <- Account$new(
      id = "Acc1",
      hypothesis_id = "H1",
      description = "Test",
      relative_weight = -1
    )
    stop("Should have thrown error for negative weight")
  }, error = function(e) {
    if (!grepl("relative_weight must be positive", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Account validation: zero relative_weight
test_that("Account rejects zero relative_weight", {
  tryCatch({
    account <- Account$new(
      id = "Acc1",
      hypothesis_id = "H1",
      description = "Test",
      relative_weight = 0
    )
    stop("Should have thrown error for zero weight")
  }, error = function(e) {
    if (!grepl("relative_weight must be positive", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Account validation: non-numeric relative_weight
test_that("Account rejects non-numeric relative_weight", {
  tryCatch({
    account <- Account$new(
      id = "Acc1",
      hypothesis_id = "H1",
      description = "Test",
      relative_weight = "not a number"
    )
    stop("Should have thrown error for non-numeric weight")
  }, error = function(e) {
    if (!grepl("relative_weight must be numeric", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test compute_prior division by zero guard
test_that("Account compute_prior guards against division by zero", {
  account <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Test"
  )

  tryCatch({
    prior <- account$compute_prior(hypothesis_prior = 0.6, total_weight = 0)
    stop("Should have thrown error for zero total_weight")
  }, error = function(e) {
    if (!grepl("total_weight must be positive", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Selector creation from accounts
test_that("Selector computes priors from accounts", {
  # Create model with hypotheses
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 0.6)
  h2 <- BayesianHypothesis$new("H2", "Hyp 2", "Test", 0.8, prior = 0.4)
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # Create accounts (1 for H1, 2 for H2)
  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc2a <- Account$new("Acc2a", "H2", "Account 2a")
  acc2b <- Account$new("Acc2b", "H2", "Account 2b")
  model$add_account(acc1a)
  model$add_account(acc2a)
  model$add_account(acc2b)

  # Create selector
  selector <- Selector$new(model)

  # Check priors
  # Acc1a gets all of H1's prior: 0.6
  # Acc2a, Acc2b each get half of H2's prior: 0.2 each
  expect_equal(selector$priors[["Acc1a"]], 0.6)
  expect_equal(selector$priors[["Acc2a"]], 0.2)
  expect_equal(selector$priors[["Acc2b"]], 0.2)

  # Priors should sum to 1
  expect_equal(sum(unlist(selector$priors)), 1.0)
})

# Test Selector validates hypothesis priors
test_that("Selector validates hypothesis priors sum to 1", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 0.6)
  h2 <- BayesianHypothesis$new("H2", "Hyp 2", "Test", 0.8, prior = 0.3)  # Sum = 0.9, not 1.0
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  model$add_account(acc1a)

  tryCatch({
    selector <- Selector$new(model)
    stop("Should have thrown error for invalid hypothesis priors")
  }, error = function(e) {
    if (!grepl("Hypothesis priors must sum to 1.0", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Selector get_states method
test_that("Selector get_states returns account IDs", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 1.0)
  model$add_hypothesis(h1)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc1b <- Account$new("Acc1b", "H1", "Account 1b")
  model$add_account(acc1a)
  model$add_account(acc1b)

  selector <- Selector$new(model)
  states <- selector$get_states()

  expect_equal(length(states), 2)
  expect_true("Acc1a" %in% states)
  expect_true("Acc1b" %in% states)
})

# Test Selector get_prior_vector method
test_that("Selector get_prior_vector returns named vector", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 0.5)
  h2 <- BayesianHypothesis$new("H2", "Hyp 2", "Test", 0.8, prior = 0.5)
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc2a <- Account$new("Acc2a", "H2", "Account 2a")
  model$add_account(acc1a)
  model$add_account(acc2a)

  selector <- Selector$new(model)
  prior_vector <- selector$get_prior_vector()

  expect_equal(length(prior_vector), 2)
  expect_equal(prior_vector[["Acc1a"]], 0.5)
  expect_equal(prior_vector[["Acc2a"]], 0.5)
})

# Test Selector with unequal weights
test_that("Selector handles unequal account weights", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 1.0)
  model$add_hypothesis(h1)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a", relative_weight = 2)
  acc1b <- Account$new("Acc1b", "H1", "Account 1b", relative_weight = 1)
  model$add_account(acc1a)
  model$add_account(acc1b)

  selector <- Selector$new(model)

  # Acc1a should get 2/3, Acc1b should get 1/3
  expect_equal(selector$priors[["Acc1a"]], 2/3, tolerance = 0.001)
  expect_equal(selector$priors[["Acc1b"]], 1/3, tolerance = 0.001)
})

# Test Selector rejects account with non-existent hypothesis
test_that("Selector rejects account with non-existent hypothesis", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 1.0)
  model$add_hypothesis(h1)

  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc2a <- Account$new("Acc2a", "H2_NONEXISTENT", "Account 2a")  # Non-existent hypothesis
  model$add_account(acc1a)
  model$add_account(acc2a)

  tryCatch({
    selector <- Selector$new(model)
    stop("Should have thrown error for non-existent hypothesis")
  }, error = function(e) {
    if (!grepl("references non-existent hypothesis", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

# Test Selector rejects model with no accounts
test_that("Selector rejects model with no accounts", {
  model <- ExplanatoryModel$new(name = "Test")
  h1 <- BayesianHypothesis$new("H1", "Hyp 1", "Test", 0.8, prior = 1.0)
  model$add_hypothesis(h1)

  tryCatch({
    selector <- Selector$new(model)
    stop("Should have thrown error for model with no accounts")
  }, error = function(e) {
    if (!grepl("Cannot create Selector: model has no accounts", e$message)) {
      stop(sprintf("Wrong error message: %s", e$message))
    }
  })
})

test_that("Account to_list returns correct structure", {
  acc <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Test account",
    relative_weight = 2
  )
  acc$add_phenomenon_id("PC1")
  acc$add_auxiliary_id("A1")
  acc$add_explanatory_link_id("EL1")

  result <- acc$to_list()

  expect_equal(result$id, "Acc1")
  expect_equal(result$hypothesis_id, "H1")
  expect_equal(result$description, "Test account")
  expect_equal(result$relative_weight, 2)
  expect_true("PC1" %in% result$phenomenon_ids)
  expect_true("A1" %in% result$auxiliary_ids)
  expect_true("EL1" %in% result$explanatory_link_ids)
})

test_that("ExplanatoryModel to_json produces valid JSON", {
  model <- ExplanatoryModel$new(name = "Test Model")

  # Add a result
  model$add_result(BayesianResult$new(
    id = "R1", text = "Test result", source = "Test"
  ))

  # Add hypotheses
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H1", text = "Hypothesis 1", source = "Test", prior = 0.6
  ))
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H2", text = "Hypothesis 2", source = "Test", prior = 0.4
  ))

  # Add account
  acc <- Account$new(id = "Acc1", hypothesis_id = "H1", description = "Test account")
  model$add_account(acc)

  json_str <- model$to_json()

  # Should be valid JSON
  parsed <- jsonlite::fromJSON(json_str)

  expect_equal(parsed$format, "explanatory-accounts-model")
  expect_equal(parsed$version, "1.0")
  expect_equal(parsed$model$name, "Test Model")
  expect_true(!is.null(parsed$exported_at))
})

test_that("Account from_list recreates account correctly", {
  original <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Test account",
    relative_weight = 2
  )
  original$add_phenomenon_id("PC1")
  original$add_auxiliary_id("A1")

  data <- original$to_list()
  restored <- Account$from_list(data)

  expect_equal(restored$id, original$id)
  expect_equal(restored$hypothesis_id, original$hypothesis_id)
  expect_equal(restored$description, original$description)
  expect_equal(restored$relative_weight, original$relative_weight)
  expect_true("PC1" %in% restored$phenomenon_ids)
  expect_true("A1" %in% restored$auxiliary_ids)
})

test_that("ExplanatoryModel round-trip through JSON preserves data", {
  # Create original model
  model <- ExplanatoryModel$new(name = "Round Trip Test")

  model$add_result(BayesianResult$new(
    id = "R1", text = "Test result", source = "Study", strength = 0.9, base_rate = 0.01
  ))

  model$add_hypothesis(BayesianHypothesis$new(
    id = "H1", text = "Hypothesis 1", source = "Theory", prior = 0.5
  ))
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H2", text = "Hypothesis 2", source = "Theory", prior = 0.5
  ))

  model$add_auxiliary(BayesianAuxiliary$new(
    id = "A1", text = "Auxiliary 1", source = "Assumption", prior = 0.9
  ))

  model$add_phenomenon(BayesianPhenomenon$new(
    id = "PC1", text = "Phenomenon 1", source = "Observation", strength = 0.8, base_rate = 0.05
  ))

  acc <- Account$new(id = "Acc1", hypothesis_id = "H1", description = "Account 1")
  acc$add_phenomenon_id("PC1")
  acc$add_auxiliary_id("A1")
  model$add_account(acc)

  # Round trip
  json_str <- model$to_json()
  restored <- ExplanatoryModel$from_json(json_str)

  # Verify
  expect_equal(restored$name, "Round Trip Test")
  expect_equal(length(restored$results), 1)
  expect_equal(length(restored$hypotheses), 2)
  expect_equal(length(restored$accounts), 1)
  expect_equal(restored$results[["R1"]]$text, "Test result")
  expect_equal(restored$hypotheses[["H1"]]$prior, 0.5)
  expect_equal(restored$accounts[["Acc1"]]$hypothesis_id, "H1")
})

cat(sprintf("\n=== Summary: %d/%d tests passed ===\n", tests_passed, tests_run))
