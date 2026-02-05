# Tests for Value of Information Analysis
#
# Tests for R/voi_analysis.R functions:
#   - compute_entropy()
#   - compute_tvd()
#   - marginalize_to_hypotheses()
#   - compute_evpi()
#   - compute_all_evpi()

library(R6)

# Source required modules
source("R/classes.R")
source("R/explanatory_classes.R")
source("R/bayesian_spec_classes.R")
source("R/bayesian_spec_cpt.R")
source("R/explanatory_network.R")
source("R/conversion.R")
source("R/bayesian.R")
source("R/voi_analysis.R")

# Helper: Generate all CPTs for a model (mirrors app.R function)
# This handles Selector, Account, and all spec nodes
generate_all_cpts_for_model <- function(model, bayesian_network, selector) {
  cpts <- list()

  # Selector CPT
  cpts[["Selector"]] <- generate_selector_cpt(selector)

  # Account CPTs
  for (acc_id in names(model$accounts)) {
    cpts[[acc_id]] <- generate_account_cpt(acc_id, selector)
  }

  # Auxiliary CPTs
  for (aux_id in names(model$auxiliaries)) {
    cpts[[aux_id]] <- generate_prior_cpt_spec(model$auxiliaries[[aux_id]])
  }

  # Phenomenon CPTs
  for (pc_id in names(model$phenomena)) {
    cpts[[pc_id]] <- generate_target_cpt_spec(model$phenomena[[pc_id]], bayesian_network)
  }

  # Result CPTs
  for (r_id in names(model$results)) {
    # Check if result has incoming TO_TARGET edge
    has_incoming <- any(bayesian_network$edges$to == r_id &
                       bayesian_network$edges$type == "TO_TARGET")

    if (has_incoming) {
      cpts[[r_id]] <- generate_target_cpt_spec(model$results[[r_id]], bayesian_network)
    } else {
      cpts[[r_id]] <- list(
        states = c("true", "false"),
        parents = character(0),
        probabilities = c(model$results[[r_id]]$base_rate,
                        1 - model$results[[r_id]]$base_rate)
      )
    }
  }

  # Explanatory Link CPTs
  for (link_id in names(model$explanatory_links)) {
    cpts[[link_id]] <- generate_link_cpt_spec(model$explanatory_links[[link_id]], bayesian_network)
  }

  return(cpts)
}

# Test counters
tests_run <- 0
tests_passed <- 0

# Test helper function
test_that <- function(description, expr) {
  tests_run <<- tests_run + 1
  cat(sprintf("Test %d: %s... ", tests_run, description))

  tryCatch({
    result <- expr
    if (isTRUE(result) || is.null(result)) {
      tests_passed <<- tests_passed + 1
      cat("\342\234\223 PASS\n")
      TRUE
    } else {
      cat(sprintf("\342\234\227 FAIL (returned %s)\n", toString(result)))
      FALSE
    }
  }, error = function(e) {
    cat(sprintf("\342\234\227 FAIL\n  Error: %s\n", e$message))
    FALSE
  })
}

# Assertion helpers
expect_equal <- function(actual, expected, tolerance = 1e-6) {
  if (is.numeric(actual) && is.numeric(expected)) {
    if (all(abs(actual - expected) < tolerance)) {
      return(TRUE)
    } else {
      stop(sprintf("Expected %s but got %s", toString(expected), toString(actual)))
    }
  } else {
    if (identical(actual, expected)) {
      return(TRUE)
    } else {
      stop(sprintf("Expected %s but got %s", toString(expected), toString(actual)))
    }
  }
}

expect_true <- function(condition, msg = "condition was FALSE") {
  if (isTRUE(condition)) {
    return(TRUE)
  } else {
    stop(msg)
  }
}

expect_error <- function(expr, pattern = NULL) {
  tryCatch({
    expr
    stop("Expected an error but none occurred")
  }, error = function(e) {
    if (!is.null(pattern) && !grepl(pattern, e$message)) {
      stop(sprintf("Error message '%s' did not match pattern '%s'", e$message, pattern))
    }
    return(TRUE)
  })
}

cat("\n=== Testing VOI Analysis Functions ===\n\n")

# ============================================
# Tests for compute_entropy()
# ============================================
cat("--- compute_entropy() ---\n")

test_that("entropy of uniform binary distribution = 1.0 bit", {
  result <- compute_entropy(c(0.5, 0.5))
  expect_equal(result, 1.0)
})

test_that("entropy of skewed distribution (0.9, 0.1) ≈ 0.469 bits", {
  result <- compute_entropy(c(0.9, 0.1))
  expect_equal(result, 0.469, tolerance = 0.001)
})

test_that("entropy of deterministic distribution (1.0, 0.0) = 0 bits", {
  result <- compute_entropy(c(1.0, 0.0))
  expect_equal(result, 0.0)
})

test_that("entropy of deterministic distribution (0.0, 1.0) = 0 bits", {
  result <- compute_entropy(c(0.0, 1.0))
  expect_equal(result, 0.0)
})

test_that("entropy of uniform 4-state distribution = 2.0 bits", {
  result <- compute_entropy(c(0.25, 0.25, 0.25, 0.25))
  expect_equal(result, 2.0)
})

test_that("entropy of uniform 3-state distribution = log2(3) bits", {
  result <- compute_entropy(c(1/3, 1/3, 1/3))
  expect_equal(result, log2(3))
})

test_that("entropy handles near-zero probabilities", {
  result <- compute_entropy(c(0.999999, 0.000001))
  expect_true(result >= 0 && result < 0.01)
})

test_that("entropy rejects negative probabilities", {
  expect_error(compute_entropy(c(0.5, -0.5)), "negative")
})

test_that("entropy rejects probabilities not summing to 1", {
  expect_error(compute_entropy(c(0.3, 0.3)), "sum to 1")
})

# ============================================
# Tests for compute_tvd()
# ============================================
cat("\n--- compute_tvd() ---\n")

test_that("TVD of identical distributions = 0", {
  result <- compute_tvd(c(0.6, 0.4), c(0.6, 0.4))
  expect_equal(result, 0.0)
})

test_that("TVD of completely disjoint distributions = 1", {
  result <- compute_tvd(c(1.0, 0.0), c(0.0, 1.0))
  expect_equal(result, 1.0)
})

test_that("TVD of partially overlapping distributions", {
  result <- compute_tvd(c(0.6, 0.4), c(0.75, 0.25))
  expect_equal(result, 0.15)
})

test_that("TVD is symmetric", {
  tvd1 <- compute_tvd(c(0.7, 0.3), c(0.4, 0.6))
  tvd2 <- compute_tvd(c(0.4, 0.6), c(0.7, 0.3))
  expect_equal(tvd1, tvd2)
})

test_that("TVD is bounded [0, 1]", {
  result <- compute_tvd(c(0.8, 0.2), c(0.3, 0.7))
  expect_true(result >= 0 && result <= 1)
})

test_that("TVD works for 3-state distributions", {
  result <- compute_tvd(c(0.5, 0.3, 0.2), c(0.4, 0.4, 0.2))
  expect_equal(result, 0.1)  # (|0.1| + |0.1| + |0|) / 2 = 0.1
})

test_that("TVD rejects different length vectors", {
  expect_error(compute_tvd(c(0.5, 0.5), c(0.33, 0.33, 0.34)), "same length")
})

# ============================================
# Tests for marginalize_to_hypotheses()
# ============================================
cat("\n--- marginalize_to_hypotheses() ---\n")

# Create a simple model for testing marginalization
create_test_model_for_marginalization <- function() {
  model <- ExplanatoryModel$new(name = "Test Model")

  # Add hypotheses
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H1", text = "Hypothesis 1", source = "Test",
    prior = 0.6
  ))
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H2", text = "Hypothesis 2", source = "Test",
    prior = 0.4
  ))

  # Add accounts: two for H1, one for H2
  acc1 <- Account$new("Acc1_H1", "H1", "Account 1 for H1", relative_weight = 1.0)
  acc2 <- Account$new("Acc2_H1", "H1", "Account 2 for H1", relative_weight = 1.0)
  acc3 <- Account$new("Acc1_H2", "H2", "Account 1 for H2", relative_weight = 1.0)

  model$add_account(acc1)
  model$add_account(acc2)
  model$add_account(acc3)

  return(model)
}

test_that("marginalization with single account per hypothesis", {
  model <- ExplanatoryModel$new(name = "Simple Model")
  model$add_hypothesis(BayesianHypothesis$new("H1", "Hyp 1", "Test", prior = 0.6))
  model$add_hypothesis(BayesianHypothesis$new("H2", "Hyp 2", "Test", prior = 0.4))
  model$add_account(Account$new("Acc1", "H1", "Account 1"))
  model$add_account(Account$new("Acc2", "H2", "Account 2"))

  selector_marginal <- c(Acc1 = 0.7, Acc2 = 0.3)
  result <- marginalize_to_hypotheses(selector_marginal, model)

  expect_equal(result["H1"], 0.7)
  expect_equal(result["H2"], 0.3)
})

test_that("marginalization with multiple accounts per hypothesis", {
  model <- create_test_model_for_marginalization()

  selector_marginal <- c(Acc1_H1 = 0.3, Acc2_H1 = 0.3, Acc1_H2 = 0.4)
  result <- marginalize_to_hypotheses(selector_marginal, model)

  expect_equal(result["H1"], 0.6)  # 0.3 + 0.3
  expect_equal(result["H2"], 0.4)
})

test_that("marginalization preserves sum to 1", {
  model <- create_test_model_for_marginalization()

  selector_marginal <- c(Acc1_H1 = 0.25, Acc2_H1 = 0.25, Acc1_H2 = 0.5)
  result <- marginalize_to_hypotheses(selector_marginal, model)

  expect_equal(sum(result), 1.0)
})

test_that("marginalization requires named vector", {
  model <- create_test_model_for_marginalization()
  expect_error(marginalize_to_hypotheses(c(0.3, 0.3, 0.4), model), "named vector")
})

# ============================================
# Tests for compute_evpi() with simple model
# ============================================
cat("\n--- compute_evpi() ---\n")

# Helper to build network and CPTs for a model
build_model_for_inference <- function(model) {
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)
  cpts <- generate_all_cpts_for_model(model, graph, selector)
  return(list(graph = graph, selector = selector, cpts = cpts))
}

# Create a minimal model for EVPI testing
create_simple_evpi_model <- function() {
  model <- ExplanatoryModel$new(name = "EVPI Test Model")

  # Add result
  model$add_result(BayesianResult$new(
    id = "R1", text = "Observed result", source = "Data",
    strength = 0.95, base_rate = 0.1
  ))

  # Add two hypotheses with equal priors
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H1", text = "Hypothesis 1", source = "Theory",
    prior = 0.5
  ))
  model$add_hypothesis(BayesianHypothesis$new(
    id = "H2", text = "Hypothesis 2", source = "Theory",
    prior = 0.5
  ))

  # Add auxiliary for H1
  model$add_auxiliary(BayesianAuxiliary$new(
    id = "A1", text = "Auxiliary assumption", source = "Background",
    prior = 0.8, subtype = "BackgroundTheory"
  ))

  # Add phenomenon
  model$add_phenomenon(BayesianPhenomenon$new(
    id = "P1", text = "Observable phenomenon", source = "Data",
    strength = 0.9, base_rate = 0.05
  ))

  # Create accounts
  acc1 <- Account$new("Acc_H1", "H1", "H1 explains via P1")
  acc2 <- Account$new("Acc_H2", "H2", "H2 explains via P1")
  model$add_account(acc1)
  model$add_account(acc2)

  # Create explanatory links
  # H1 pathway: Acc_H1 + A1 -> EL1 -> P1 -> EL2 -> R1
  el1 <- BayesianExplanatoryLink$new(
    id = "EL1", source_id = "Acc_H1", target_id = "P1",
    text = "H1 explains P1", source = "Theory",
    reliability = 0.95, epsilon = 0.01
  )
  el1$required_auxiliary_ids <- c("A1")

  el2 <- BayesianExplanatoryLink$new(
    id = "EL2", source_id = "P1", target_id = "R1",
    text = "P1 explains R1", source = "Theory",
    reliability = 0.95, epsilon = 0.01
  )

  # H2 pathway: Acc_H2 -> EL3 -> P1 -> EL2 -> R1
  el3 <- BayesianExplanatoryLink$new(
    id = "EL3", source_id = "Acc_H2", target_id = "P1",
    text = "H2 explains P1", source = "Theory",
    reliability = 0.95, epsilon = 0.01
  )

  model$add_explanatory_link(el1)
  model$add_explanatory_link(el2)
  model$add_explanatory_link(el3)

  acc1$add_phenomenon_id("P1")
  acc1$add_auxiliary_id("A1")
  acc1$add_explanatory_link_id("EL1")
  acc1$add_explanatory_link_id("EL2")

  acc2$add_phenomenon_id("P1")
  acc2$add_explanatory_link_id("EL3")
  acc2$add_explanatory_link_id("EL2")

  return(model)
}

test_that("compute_evpi returns all required fields", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi(
    node_id = "A1",
    target = "hypotheses",
    model = model,
    bayesian_network = inf$graph,
    cpts = inf$cpts,
    base_evidence = list(R1 = "true")
  )

  expect_true("evpi" %in% names(result))
  expect_true("evpi_pct" %in% names(result))
  expect_true("shift" %in% names(result))
  expect_true("max_gain" %in% names(result))
  expect_true("asymmetry" %in% names(result))
  expect_true("p_true" %in% names(result))
  expect_true("entropy_current" %in% names(result))
  expect_true("entropy_if_true" %in% names(result))
  expect_true("entropy_if_false" %in% names(result))
  expect_true("posteriors_if_true" %in% names(result))
  expect_true("posteriors_if_false" %in% names(result))
})

test_that("EVPI is non-negative", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "hypotheses", model, inf$graph, inf$cpts, list(R1 = "true"))
  expect_true(result$evpi >= 0, "EVPI must be non-negative")
})

test_that("EVPI is bounded by current entropy", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "hypotheses", model, inf$graph, inf$cpts, list(R1 = "true"))
  expect_true(result$evpi <= result$entropy_current + 1e-6,
              "EVPI must be <= current entropy")
})

test_that("shift (TVD) is bounded [0, 1]", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "hypotheses", model, inf$graph, inf$cpts, list(R1 = "true"))
  expect_true(result$shift >= 0 && result$shift <= 1,
              "shift must be bounded [0, 1]")
})

test_that("evpi_pct equals evpi / entropy_current * 100", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "hypotheses", model, inf$graph, inf$cpts, list(R1 = "true"))

  if (result$entropy_current > 0) {
    expected_pct <- (result$evpi / result$entropy_current) * 100
    expect_equal(result$evpi_pct, expected_pct)
  }
})

test_that("target='accounts' uses Selector directly", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "accounts", model, inf$graph, inf$cpts, list(R1 = "true"))

  # For accounts, posteriors should have account names
  expect_true(all(c("Acc_H1", "Acc_H2") %in% names(result$posteriors_if_true)))
})

test_that("target='hypotheses' marginalizes to hypotheses", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_evpi("A1", "hypotheses", model, inf$graph, inf$cpts, list(R1 = "true"))

  # For hypotheses, posteriors should have hypothesis names
  expect_true(all(c("H1", "H2") %in% names(result$posteriors_if_true)))
})

# ============================================
# Tests for get_candidate_nodes()
# ============================================
cat("\n--- get_candidate_nodes() ---\n")

test_that("get_candidate_nodes returns auxiliaries", {
  model <- create_simple_evpi_model()
  graph <- build_bayesian_network(model)

  candidates <- get_candidate_nodes(model, graph, list(),
                                    include_auxiliaries = TRUE,
                                    include_phenomena = FALSE,
                                    include_links = FALSE)

  expect_true("A1" %in% candidates$node_id)
  expect_true(all(candidates$node_type == "Auxiliary"))
})

test_that("get_candidate_nodes returns phenomena", {
  model <- create_simple_evpi_model()
  graph <- build_bayesian_network(model)

  candidates <- get_candidate_nodes(model, graph, list(),
                                    include_auxiliaries = FALSE,
                                    include_phenomena = TRUE,
                                    include_links = FALSE)

  expect_true("P1" %in% candidates$node_id)
  expect_true(all(candidates$node_type == "Phenomenon"))
})

test_that("get_candidate_nodes returns explanatory links", {
  model <- create_simple_evpi_model()
  graph <- build_bayesian_network(model)

  candidates <- get_candidate_nodes(model, graph, list(),
                                    include_auxiliaries = FALSE,
                                    include_phenomena = FALSE,
                                    include_links = TRUE)

  expect_true("EL1" %in% candidates$node_id)
  expect_true(all(candidates$node_type == "ExplanatoryLink"))
})

test_that("get_candidate_nodes excludes evidence nodes", {
  model <- create_simple_evpi_model()
  graph <- build_bayesian_network(model)

  # A1 is evidence
  candidates <- get_candidate_nodes(model, graph, list(A1 = "true"),
                                    include_auxiliaries = TRUE,
                                    include_phenomena = TRUE,
                                    include_links = TRUE)

  expect_true(!"A1" %in% candidates$node_id)
})

# ============================================
# Tests for compute_all_evpi()
# ============================================
cat("\n--- compute_all_evpi() ---\n")

test_that("compute_all_evpi returns correct structure", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_all_evpi(model, inf$graph, inf$cpts, list(R1 = "true"))

  expect_true("metadata" %in% names(result))
  expect_true("results" %in% names(result))
  expect_true("details" %in% names(result))
})

test_that("compute_all_evpi metadata contains required fields", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_all_evpi(model, inf$graph, inf$cpts, list(R1 = "true"))

  expect_true("timestamp" %in% names(result$metadata))
  expect_true("target" %in% names(result$metadata))
  expect_true("entropy_current" %in% names(result$metadata))
  expect_true("n_candidates" %in% names(result$metadata))
  expect_true("computation_time_seconds" %in% names(result$metadata))
})

test_that("compute_all_evpi results are sorted by EVPI descending", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_all_evpi(model, inf$graph, inf$cpts, list(R1 = "true"))

  evpi_values <- result$results$evpi
  expect_true(all(diff(evpi_values) <= 0), "Results must be sorted by EVPI descending")
})

test_that("compute_all_evpi respects include_* filters", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_all_evpi(model, inf$graph, inf$cpts, list(R1 = "true"),
                             include_auxiliaries = TRUE,
                             include_phenomena = FALSE,
                             include_links = FALSE)

  expect_true(all(result$results$node_type == "Auxiliary"))
})

test_that("compute_all_evpi handles empty candidates gracefully", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  # Condition all candidate nodes as evidence
  all_evidence <- list(R1 = "true", A1 = "true", P1 = "true",
                       EL1 = "true", EL2 = "true", EL3 = "true")

  result <- compute_all_evpi(model, inf$graph, inf$cpts, all_evidence)

  expect_equal(result$metadata$n_candidates, 0)
  expect_equal(nrow(result$results), 0)
})

test_that("compute_all_evpi details contain posteriors for each node", {
  model <- create_simple_evpi_model()
  inf <- build_model_for_inference(model)

  result <- compute_all_evpi(model, inf$graph, inf$cpts, list(R1 = "true"))

  for (node_id in result$results$node_id) {
    expect_true(node_id %in% names(result$details),
                sprintf("Details missing for node %s", node_id))
    expect_true("posteriors_if_true" %in% names(result$details[[node_id]]))
    expect_true("posteriors_if_false" %in% names(result$details[[node_id]]))
    expect_true("interpretation" %in% names(result$details[[node_id]]))
  }
})

# ============================================
# Tests for generate_evpi_interpretation()
# ============================================
cat("\n--- generate_evpi_interpretation() ---\n")

test_that("interpretation includes EVPI percentage", {
  evpi_result <- list(
    evpi = 0.15,
    evpi_pct = 15.5,
    shift = 0.2,
    asymmetry = 0.3,
    entropy_if_true = 0.7,
    entropy_if_false = 0.4,
    p_true = 0.6
  )
  posteriors_current <- c(H1 = 0.6, H2 = 0.4)

  interpretation <- generate_evpi_interpretation(evpi_result, "Test node", posteriors_current)

  expect_true(grepl("15", interpretation), "Should mention percentage")
})

test_that("interpretation classifies priority correctly", {
  # Critical priority (>= 15%)
  evpi_result <- list(evpi = 0.2, evpi_pct = 20, shift = 0.2, asymmetry = 0.1,
                      entropy_if_true = 0.7, entropy_if_false = 0.6, p_true = 0.5)
  interpretation <- generate_evpi_interpretation(evpi_result, "Test", c(H1 = 0.5, H2 = 0.5))
  expect_true(grepl("critical", interpretation))

  # Low priority (< 3%)
  evpi_result$evpi_pct <- 2
  interpretation <- generate_evpi_interpretation(evpi_result, "Test", c(H1 = 0.5, H2 = 0.5))
  expect_true(grepl("low", interpretation))
})

# ============================================
# Summary
# ============================================
cat(sprintf("\n=== Summary: %d/%d tests passed ===\n", tests_passed, tests_run))

if (tests_passed == tests_run) {
  cat("All tests passed!\n")
} else {
  cat(sprintf("WARNING: %d test(s) failed\n", tests_run - tests_passed))
}
