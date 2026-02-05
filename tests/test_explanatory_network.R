# Unit Tests for Explanatory Network Construction

library(R6)

source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/explanatory_classes.R")
source("R/explanatory_network.R")

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

expect_true <- function(condition) {
  if (!condition) {
    stop("Expected TRUE, got FALSE")
  }
}

expect_equal <- function(actual, expected) {
  if (!identical(actual, expected)) {
    stop(sprintf("Expected %s, got %s", expected, actual))
  }
}

cat("=== Explanatory Network Construction Tests ===\n\n")

# Helper to create simple test model
create_simple_model <- function() {
  model <- ExplanatoryModel$new(name = "Simple Test")

  # Hypotheses
  h1 <- BayesianHypothesis$new("H1", "Hypothesis 1", "Test", 0.8, prior = 0.5)
  h2 <- BayesianHypothesis$new("H2", "Hypothesis 2", "Test", 0.8, prior = 0.5)
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # Result
  r1 <- BayesianResult$new("R1", "Result 1", "Test", 0.9,
                          strength = 0.95, base_rate = 0.10)
  model$add_result(r1)

  # Phenomenon
  pc1 <- BayesianPhenomenon$new("PC1", "Phenomenon 1", "Test", 0.9,
                               strength = 0.90, base_rate = 0.05)
  model$add_phenomenon(pc1)

  # Accounts
  acc1a <- Account$new("Acc1a", "H1", "Account 1a")
  acc2a <- Account$new("Acc2a", "H2", "Account 2a")
  model$add_account(acc1a)
  model$add_account(acc2a)

  # Explanatory links for Acc1a: Acc1a -> PC1 -> R1
  el1 <- BayesianExplanatoryLink$new("EL1", "Acc1a", "PC1", "Link 1", "Test", 0.95,
                                     reliability = 0.95, epsilon = 0.01)
  el2 <- BayesianExplanatoryLink$new("EL2", "PC1", "R1", "Link 2", "Test", 0.95,
                                     reliability = 0.95, epsilon = 0.01)
  model$add_explanatory_link(el1)
  model$add_explanatory_link(el2)
  acc1a$add_explanatory_link_id("EL1")
  acc1a$add_explanatory_link_id("EL2")
  acc1a$add_phenomenon_id("PC1")

  # Explanatory links for Acc2a: similar structure
  el3 <- BayesianExplanatoryLink$new("EL3", "Acc2a", "PC1", "Link 3", "Test", 0.95,
                                     reliability = 0.90, epsilon = 0.01)
  el4 <- BayesianExplanatoryLink$new("EL4", "PC1", "R1", "Link 4", "Test", 0.95,
                                     reliability = 0.90, epsilon = 0.01)
  model$add_explanatory_link(el3)
  model$add_explanatory_link(el4)
  acc2a$add_explanatory_link_id("EL3")
  acc2a$add_explanatory_link_id("EL4")
  acc2a$add_phenomenon_id("PC1")

  model
}

# Test basic network construction
test_that("build_bayesian_network creates EvidenceGraph with selector", {
  model <- create_simple_model()
  graph <- build_bayesian_network(model)

  # Should have Selector node
  expect_true("Selector" %in% names(graph$nodes))

  # Should have account nodes
  expect_true("Acc1a" %in% names(graph$nodes))
  expect_true("Acc2a" %in% names(graph$nodes))

  # Should have result nodes
  expect_true("R1" %in% names(graph$nodes))
})

# Test account nodes are children of selector
test_that("Account nodes have Selector as parent", {
  model <- create_simple_model()
  graph <- build_bayesian_network(model)

  # Check Acc1a has edge from Selector
  edges_to_acc1a <- graph$edges[graph$edges$to == "Acc1a", ]
  expect_true(any(edges_to_acc1a$from == "Selector"))
})

cat(sprintf("\n=== Summary: %d/%d tests passed ===\n", tests_passed, tests_run))
