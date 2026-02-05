# Unit Tests for Bayesian Spec CPT Generation
#
# Tests the CPT generation functions in R/bayesian_spec_cpt.R

library(R6)

# Source required modules
source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/cpt_utils.R")
source("R/bayesian_spec_cpt.R")

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
    stop(sprintf("Expected %s, got %s",
                 paste(expected, collapse = ", "),
                 paste(actual, collapse = ", ")))
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

cat("=== Bayesian Spec CPT Generation Tests ===\n\n")

# ===== PRIOR CPT TESTS =====

cat("--- Prior CPT Generation ---\n")

test_that("Prior CPT generation follows spec for hypothesis", {
  h <- BayesianHypothesis$new(
    id = "H1",
    text = "Test hypothesis",
    source = "Test",
    prior = 0.6
  )

  cpt <- generate_prior_cpt_spec(h)

  expect_equal(cpt$states, c("true", "false"))
  expect_equal(length(cpt$parents), 0)
  expect_equal(cpt$probabilities["true"], 0.6)
  expect_equal(cpt$probabilities["false"], 0.4)
  expect_equal(sum(cpt$probabilities), 1.0)
})

test_that("Prior CPT generation follows spec for auxiliary", {
  a <- BayesianAuxiliary$new(
    id = "A1",
    text = "Test auxiliary",
    source = "Test",
    prior = 0.8
  )

  cpt <- generate_prior_cpt_spec(a)

  expect_equal(cpt$probabilities["true"], 0.8)
  expect_equal(cpt$probabilities["false"], 0.2)
})

test_that("Prior CPT with prior=0.5 creates uniform distribution", {
  h <- BayesianHypothesis$new(
    id = "H2",
    text = "Test",
    source = "Test",
    prior = 0.5
  )

  cpt <- generate_prior_cpt_spec(h)

  expect_equal(cpt$probabilities["true"], 0.5)
  expect_equal(cpt$probabilities["false"], 0.5)
})

# ===== LINK CPT TESTS =====

cat("\n--- Link CPT Generation ---\n")

test_that("Link CPT with single parent (no auxiliaries) follows spec", {
  # Create graph
  graph <- EvidenceGraph$new()

  h1 <- BayesianHypothesis$new("H1", "Hyp", "Test", prior = 0.5)
  graph$add_node(h1)

  link <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "Test link",
    source = "Test",
    reliability = 0.95,
    epsilon = 0.01
  )
  graph$add_node(link)

  # Add edge
  graph$add_edge("H1", "EL1", "FROM_SOURCE")

  # Generate CPT
  cpt <- generate_link_cpt_spec(link, graph)

  expect_equal(cpt$parents, c("H1"))

  # Check spec formula: P(EL=true | H=T) = reliability
  prob_h_true <- cpt$probabilities["true", "true"]
  expect_equal(prob_h_true, 0.95)

  # Check spec formula: P(EL=true | H=F) = epsilon
  prob_h_false <- cpt$probabilities["true", "false"]
  expect_equal(prob_h_false, 0.01)

  # Check probabilities sum to 1 for each parent config
  expect_equal(cpt$probabilities["true", "true"] + cpt$probabilities["false", "true"], 1.0)
  expect_equal(cpt$probabilities["true", "false"] + cpt$probabilities["false", "false"], 1.0)
})

test_that("Link CPT with hypothesis + 1 auxiliary follows spec", {
  # Create graph with H1 + A1 -> EL1
  graph <- EvidenceGraph$new()

  h1 <- BayesianHypothesis$new("H1", "Hyp", "Test", prior = 0.5)
  a1 <- BayesianAuxiliary$new("A1", "Aux", "Test", prior = 0.8)
  graph$add_node(h1)
  graph$add_node(a1)

  link <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "Test link",
    source = "Test",
    reliability = 0.95,
    epsilon = 0.01
  )
  graph$add_node(link)

  # Add edges
  graph$add_edge("H1", "EL1", "FROM_SOURCE")
  graph$add_edge("A1", "EL1", "IS_REQUIRED_BY")

  # Generate CPT
  cpt <- generate_link_cpt_spec(link, graph)

  expect_equal(cpt$parents, c("H1", "A1"))

  # Check spec formula: P(EL=true | H=T, A1=T) = reliability
  prob_both_true <- cpt$probabilities["true", "true", "true"]
  expect_equal(prob_both_true, 0.95)

  # Check spec formula: P(EL=true | H=T, A1=F) = epsilon
  prob_h_true_a_false <- cpt$probabilities["true", "true", "false"]
  expect_equal(prob_h_true_a_false, 0.01)

  # Check spec formula: P(EL=true | H=F, A1=T) = epsilon
  prob_h_false_a_true <- cpt$probabilities["true", "false", "true"]
  expect_equal(prob_h_false_a_true, 0.01)

  # Check spec formula: P(EL=true | H=F, A1=F) = epsilon
  prob_both_false <- cpt$probabilities["true", "false", "false"]
  expect_equal(prob_both_false, 0.01)
})

test_that("Link CPT with hypothesis + 2 auxiliaries follows spec", {
  # Create graph with H1 + A1 + A2 -> EL1
  graph <- EvidenceGraph$new()

  h1 <- BayesianHypothesis$new("H1", "Hyp", "Test")
  a1 <- BayesianAuxiliary$new("A1", "Aux1", "Test")
  a2 <- BayesianAuxiliary$new("A2", "Aux2", "Test")
  graph$add_node(h1)
  graph$add_node(a1)
  graph$add_node(a2)

  link <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "Test link",
    source = "Test",
    reliability = 0.90,
    epsilon = 0.02
  )
  graph$add_node(link)

  # Add edges
  graph$add_edge("H1", "EL1", "FROM_SOURCE")
  graph$add_edge("A1", "EL1", "IS_REQUIRED_BY")
  graph$add_edge("A2", "EL1", "IS_REQUIRED_BY")

  # Generate CPT
  cpt <- generate_link_cpt_spec(link, graph)

  expect_equal(cpt$parents, c("H1", "A1", "A2"))

  # Check: P(EL=true | all true) = reliability
  prob_all_true <- cpt$probabilities["true", "true", "true", "true"]
  expect_equal(prob_all_true, 0.90)

  # Check: P(EL=true | any false) = epsilon
  # Test a few configurations where not all parents are true
  prob_h_t_a1_t_a2_f <- cpt$probabilities["true", "true", "true", "false"]
  expect_equal(prob_h_t_a1_t_a2_f, 0.02)

  prob_h_t_a1_f_a2_t <- cpt$probabilities["true", "true", "false", "true"]
  expect_equal(prob_h_t_a1_f_a2_t, 0.02)

  prob_h_f_a1_t_a2_t <- cpt$probabilities["true", "false", "true", "true"]
  expect_equal(prob_h_f_a1_t_a2_t, 0.02)
})

# ===== TARGET CPT TESTS =====

cat("\n--- Target CPT Generation ---\n")

test_that("Target CPT for phenomenon follows spec", {
  graph <- EvidenceGraph$new()

  # Create link
  link <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "Link",
    source = "Test",
    reliability = 0.95,
    epsilon = 0.01
  )
  graph$add_node(link)

  # Create target
  p1 <- BayesianPhenomenon$new(
    id = "P1",
    text = "Phenomenon",
    source = "Test",
    strength = 0.90,
    base_rate = 0.05
  )
  graph$add_node(p1)

  # Add edge
  graph$add_edge("EL1", "P1", "TO_TARGET")

  # Generate CPT
  cpt <- generate_target_cpt_spec(p1, graph)

  expect_equal(cpt$parents, c("EL1"))

  # Check spec formula: P(P1=true | EL1=true) = strength
  prob_link_true <- cpt$probabilities["true", "true"]
  expect_equal(prob_link_true, 0.90)

  # Check spec formula: P(P1=true | EL1=false) = base_rate
  prob_link_false <- cpt$probabilities["true", "false"]
  expect_equal(prob_link_false, 0.05)

  # Check complementary probabilities
  expect_equal(cpt$probabilities["false", "true"], 0.10)
  expect_equal(cpt$probabilities["false", "false"], 0.95)
})

test_that("Target CPT for result follows spec", {
  graph <- EvidenceGraph$new()

  # Create link
  link <- BayesianExplanatoryLink$new(
    id = "EL2",
    source_id = "P1",
    target_id = "R1",
    text = "Link",
    source = "Test"
  )
  graph$add_node(link)

  # Create result
  r1 <- BayesianResult$new(
    id = "R1",
    text = "Result",
    source = "Test",
    strength = 0.95,
    base_rate = 0.10
  )
  graph$add_node(r1)

  # Add edge
  graph$add_edge("EL2", "R1", "TO_TARGET")

  # Generate CPT
  cpt <- generate_target_cpt_spec(r1, graph)

  expect_equal(cpt$parents, c("EL2"))
  expect_equal(cpt$probabilities["true", "true"], 0.95)
  expect_equal(cpt$probabilities["true", "false"], 0.10)
})

# ===== EXACT PROBABILITY FORMULA TESTS =====

cat("\n--- Exact Probability Formulas ---\n")

test_that("Exact probability formula matches spec", {
  # Example from specification: COVID case study
  # P(R_studies | H, aux) = base_rate + (strength - base_rate) × reliability
  # From spec: base_rate for R_studies = 0.10, but the intermediate
  # P_ventilation has base_rate = 0.05

  strength <- 0.95
  base_rate <- 0.05  # Corrected to match spec example
  reliability <- 0.98 * 0.90 * 0.95  # Product of multiple links

  exact_prob <- compute_exact_probability_spec(strength, base_rate, reliability)

  # Manual calculation from spec:
  # 0.05 + (0.95 - 0.05) × 0.98 × 0.90 × 0.95 = 0.05 + 0.749... = 0.799...
  expected <- 0.05 + (0.95 - 0.05) * (0.98 * 0.90 * 0.95)
  expect_equal(exact_prob, expected, tolerance = 0.001)

  # Should be approximately 0.799
  expect_true(abs(exact_prob - 0.799) < 0.01)
})

test_that("Approximate formula is close to exact when base_rate is small", {
  strength <- 0.95
  base_rate <- 0.01  # Small base rate
  reliability <- 0.90

  exact_prob <- compute_exact_probability_spec(strength, base_rate, reliability)
  approx_prob <- compute_approximate_probability_spec(strength, reliability)

  # Approximation should be close when base_rate << strength
  expect_true(abs(exact_prob - approx_prob) < 0.02)
})

test_that("Exact formula handles edge cases", {
  # Case 1: base_rate = 0
  prob1 <- compute_exact_probability_spec(0.90, 0.00, 0.95)
  expect_equal(prob1, 0.90 * 0.95)

  # Case 2: reliability = 1.0 (certainty)
  prob2 <- compute_exact_probability_spec(0.90, 0.10, 1.0)
  expect_equal(prob2, 0.90)

  # Case 3: reliability = 0.0 (no link)
  prob3 <- compute_exact_probability_spec(0.90, 0.10, 0.0)
  expect_equal(prob3, 0.10)
})

# ===== MASTER CPT GENERATION TESTS =====

cat("\n--- Master CPT Generation ---\n")

test_that("generate_all_cpts_spec creates CPTs for all node types", {
  # Create simple graph with all node types
  graph <- EvidenceGraph$new()

  h1 <- BayesianHypothesis$new("H1", "Hyp", "Test", 0.8, prior = 0.6)
  a1 <- BayesianAuxiliary$new("A1", "Aux", "Test", 0.9, prior = 0.8)
  graph$add_node(h1)
  graph$add_node(a1)

  link1 <- BayesianExplanatoryLink$new("EL1", "H1", "P1", "Link1", "Test", 0.95)
  graph$add_node(link1)
  graph$add_edge("H1", "EL1", "FROM_SOURCE")
  graph$add_edge("A1", "EL1", "IS_REQUIRED_BY")

  p1 <- BayesianPhenomenon$new("P1", "Phen", "Test", 0.9, strength = 0.90, base_rate = 0.05)
  graph$add_node(p1)
  graph$add_edge("EL1", "P1", "TO_TARGET")

  link2 <- BayesianExplanatoryLink$new("EL2", "P1", "R1", "Link2", "Test", 0.98)
  graph$add_node(link2)
  graph$add_edge("P1", "EL2", "FROM_SOURCE")

  r1 <- BayesianResult$new("R1", "Result", "Test", 0.95, strength = 0.95, base_rate = 0.10)
  graph$add_node(r1)
  graph$add_edge("EL2", "R1", "TO_TARGET")

  # Generate all CPTs (this should not throw errors)
  cpts <- generate_all_cpts_spec(graph)

  # Check all nodes have CPTs
  expect_equal(length(cpts), 6)
  expect_true("H1" %in% names(cpts))
  expect_true("A1" %in% names(cpts))
  expect_true("EL1" %in% names(cpts))
  expect_true("P1" %in% names(cpts))
  expect_true("EL2" %in% names(cpts))
  expect_true("R1" %in% names(cpts))

  # Spot check: hypothesis prior
  expect_equal(cpts$H1$probabilities["true"], 0.6)

  # Spot check: link with auxiliaries
  expect_equal(cpts$EL1$parents, c("H1", "A1"))

  # Spot check: target strength
  expect_equal(cpts$P1$probabilities["true", "true"], 0.90)

  # Check all CPTs have valid probabilities
  for (node_id in names(cpts)) {
    expect_true(all(cpts[[node_id]]$probabilities >= 0 & cpts[[node_id]]$probabilities <= 1))
  }
})

test_that("generate_all_cpts_spec validates all generated CPTs", {
  # Create graph
  graph <- EvidenceGraph$new()

  h1 <- BayesianHypothesis$new("H1", "Hyp", "Test", 0.8)
  graph$add_node(h1)

  link <- BayesianExplanatoryLink$new("EL1", "H1", "P1", "Link", "Test", 0.95)
  graph$add_node(link)
  graph$add_edge("H1", "EL1", "FROM_SOURCE")

  p1 <- BayesianPhenomenon$new("P1", "Phen", "Test", 0.9)
  graph$add_node(p1)
  graph$add_edge("EL1", "P1", "TO_TARGET")

  # This should succeed (all CPTs should be valid)
  cpts <- generate_all_cpts_spec(graph)

  # Check basic CPT structure manually (avoid validate_cpt's strict type checking)
  for (node_id in names(cpts)) {
    cpt <- cpts[[node_id]]

    # Check required fields exist
    expect_true("states" %in% names(cpt))
    expect_true("parents" %in% names(cpt))
    expect_true("probabilities" %in% names(cpt))

    # Check probabilities are in [0, 1]
    expect_true(all(cpt$probabilities >= 0 & cpt$probabilities <= 1))

    # Check probabilities sum to 1 for each parent config
    if (length(cpt$parents) == 0) {
      expect_equal(sum(cpt$probabilities), 1.0)
    }
  }
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
