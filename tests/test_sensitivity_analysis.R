# Tests for Sensitivity Analysis Module
#
# Tests for R/sensitivity_analysis.R functions:
#   - get_sweepable_parameters()
#   - adjust_hypothesis_priors()
#   - compute_sweep_summary()
#   - compute_parameter_sweep()
#   - create_sweep_plot()

library(R6)

# Source required modules
source("R/classes.R")
source("R/explanatory_classes.R")
source("R/bayesian_spec_classes.R")
source("R/bayesian_spec_cpt.R")
source("R/explanatory_network.R")
source("R/conversion.R")
source("R/bayesian.R")
source("R/sensitivity_analysis.R")
source("R/explanatory_case_study.R")

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

# ============================================
# Helper: Create test model
# ============================================

create_test_model <- function() {
  model <- ExplanatoryModel$new(name = "Test Model")

  # Add two hypotheses (priors must sum to 1)
  h1 <- BayesianHypothesis$new(
    id = "H1",
    text = "First hypothesis",
    source = "Test",
    prior = 0.6
  )
  h2 <- BayesianHypothesis$new(
    id = "H2",
    text = "Second hypothesis",
    source = "Test",
    prior = 0.4
  )
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # Add an auxiliary
  a1 <- BayesianAuxiliary$new(
    id = "A1",
    text = "Auxiliary assumption",
    source = "Test",
    prior = 0.8
  )
  model$add_auxiliary(a1)

  # Add a phenomenon
  p1 <- BayesianPhenomenon$new(
    id = "P1",
    text = "Observable phenomenon",
    source = "Test",
    strength = 0.9,
    base_rate = 0.1
  )
  model$add_phenomenon(p1)

  # Add a result
  r1 <- BayesianResult$new(
    id = "R1",
    text = "Empirical result",
    source = "Test",
    strength = 0.95,
    base_rate = 0.05
  )
  model$add_result(r1)

  # Add explanatory links
  link1 <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "H1",
    target_id = "P1",
    text = "H1 explains P1",
    source = "Test",
    reliability = 0.95,
    epsilon = 0.01
  )
  link1$required_auxiliary_ids <- c("A1")
  model$add_explanatory_link(link1)

  link2 <- BayesianExplanatoryLink$new(
    id = "EL2",
    source_id = "P1",
    target_id = "R1",
    text = "P1 explains R1",
    source = "Test",
    reliability = 0.98,
    epsilon = 0.01
  )
  model$add_explanatory_link(link2)

  model
}

create_three_hypothesis_model <- function() {
  model <- ExplanatoryModel$new(name = "Three Hypothesis Model")

  h1 <- BayesianHypothesis$new(id = "H1", text = "H1", source = "Test", prior = 0.5)
  h2 <- BayesianHypothesis$new(id = "H2", text = "H2", source = "Test", prior = 0.3)
  h3 <- BayesianHypothesis$new(id = "H3", text = "H3", source = "Test", prior = 0.2)

  model$add_hypothesis(h1)
  model$add_hypothesis(h2)
  model$add_hypothesis(h3)

  model
}

cat("\n=== Testing Sensitivity Analysis Functions ===\n\n")

# ============================================
# Tests for get_sweepable_parameters()
# ============================================
cat("--- get_sweepable_parameters() ---\n")

test_that("returns data frame with correct columns", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  expected_cols <- c("node_id", "node_type", "param_name",
                     "current_value", "display_name", "description")
  expect_true(all(expected_cols %in% names(params)))
})

test_that("includes all hypothesis priors", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  h_params <- params[params$node_type == "hypothesis", ]
  expect_equal(nrow(h_params), 2)
  expect_true(all(c("H1", "H2") %in% h_params$node_id))
})

test_that("includes auxiliary priors", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  a_params <- params[params$node_type == "auxiliary", ]
  expect_equal(nrow(a_params), 1)
  expect_equal(a_params$node_id[1], "A1")
  expect_equal(a_params$current_value[1], 0.8)
})

test_that("includes phenomenon strength and base_rate", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  p_params <- params[params$node_type == "phenomenon", ]
  expect_equal(nrow(p_params), 2)  # strength and base_rate
  expect_true("strength" %in% p_params$param_name)
  expect_true("base_rate" %in% p_params$param_name)
})

test_that("includes result strength and base_rate", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  r_params <- params[params$node_type == "result", ]
  expect_equal(nrow(r_params), 2)  # strength and base_rate
})

test_that("includes link reliability and epsilon", {
  model <- create_test_model()
  params <- get_sweepable_parameters(model)

  l_params <- params[params$node_type == "link", ]
  expect_equal(nrow(l_params), 4)  # 2 links x 2 params each
  expect_true("reliability" %in% l_params$param_name)
  expect_true("epsilon" %in% l_params$param_name)
})

test_that("returns empty data frame for empty model", {
  model <- ExplanatoryModel$new(name = "Empty")
  params <- get_sweepable_parameters(model)

  expect_equal(nrow(params), 0)
  expect_true(is.data.frame(params))
})

# ============================================
# Tests for adjust_hypothesis_priors()
# ============================================
cat("\n--- adjust_hypothesis_priors() ---\n")

test_that("two hypotheses: adjusts complement correctly", {
  model <- create_test_model()  # H1=0.6, H2=0.4

  adjusted <- adjust_hypothesis_priors(model, "H1", 0.8)

  expect_equal(adjusted["H1"], 0.8)
  expect_equal(adjusted["H2"], 0.2)
  expect_equal(sum(adjusted), 1.0)
})

test_that("three hypotheses: proportional scaling", {
  model <- create_three_hypothesis_model()  # H1=0.5, H2=0.3, H3=0.2

  # Sweep H1 to 0.8 (new_other_sum = 0.2)
  # Original other_sum = 0.3 + 0.2 = 0.5
  # Scale factor = 0.2 / 0.5 = 0.4
  # H2' = 0.3 * 0.4 = 0.12
  # H3' = 0.2 * 0.4 = 0.08
  adjusted <- adjust_hypothesis_priors(model, "H1", 0.8)

  expect_equal(adjusted["H1"], 0.8)
  expect_equal(adjusted["H2"], 0.12, tolerance = 1e-10)
  expect_equal(adjusted["H3"], 0.08, tolerance = 1e-10)
  expect_equal(sum(adjusted), 1.0)
})

test_that("maintains relative proportions of other hypotheses", {
  model <- create_three_hypothesis_model()  # H1=0.5, H2=0.3, H3=0.2

  adjusted <- adjust_hypothesis_priors(model, "H1", 0.7)

  # Original ratio H2:H3 = 0.3:0.2 = 3:2
  # This ratio should be preserved
  ratio_original <- 0.3 / 0.2
  ratio_adjusted <- adjusted["H2"] / adjusted["H3"]
  expect_equal(ratio_adjusted, ratio_original)
})

test_that("sweep to 0.99 leaves 0.01 for others", {
  model <- create_test_model()

  adjusted <- adjust_hypothesis_priors(model, "H1", 0.99)

  expect_equal(adjusted["H1"], 0.99)
  expect_equal(adjusted["H2"], 0.01)
  expect_equal(sum(adjusted), 1.0)
})

test_that("edge case: one hypothesis at 0 gets share", {
  model <- ExplanatoryModel$new(name = "Edge Case")
  h1 <- BayesianHypothesis$new(id = "H1", text = "H1", source = "Test", prior = 1.0)
  h2 <- BayesianHypothesis$new(id = "H2", text = "H2", source = "Test", prior = 0.0)
  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # When H1 was 1.0 and we sweep it to 0.5, H2 should get 0.5
  # But other_sum = 0, so we distribute equally
  adjusted <- adjust_hypothesis_priors(model, "H1", 0.5)

  expect_equal(adjusted["H1"], 0.5)
  expect_equal(adjusted["H2"], 0.5)
  expect_equal(sum(adjusted), 1.0)
})

test_that("errors on unknown hypothesis", {
  model <- create_test_model()
  expect_error(adjust_hypothesis_priors(model, "H99", 0.5), "not found")
})

# ============================================
# Tests for compute_sweep_summary()
# ============================================
cat("\n--- compute_sweep_summary() ---\n")

test_that("computes min/max correctly", {
  param_values <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  posteriors <- data.frame(
    parameter_value = param_values,
    H1 = c(0.3, 0.4, 0.5, 0.6, 0.7),
    H2 = c(0.7, 0.6, 0.5, 0.4, 0.3)
  )
  current_posteriors <- list(H1 = 0.5, H2 = 0.5)

  summary <- compute_sweep_summary(param_values, posteriors, 0.3, current_posteriors)

  h1_row <- summary[summary$target_id == "H1", ]
  expect_equal(h1_row$min, 0.3)
  expect_equal(h1_row$max, 0.7)
})

test_that("computes range correctly", {
  param_values <- c(0.1, 0.2, 0.3)
  posteriors <- data.frame(
    parameter_value = param_values,
    H1 = c(0.2, 0.5, 0.8)
  )
  current_posteriors <- list(H1 = 0.5)

  summary <- compute_sweep_summary(param_values, posteriors, 0.2, current_posteriors)

  expect_equal(summary$range[1], 0.6)  # 0.8 - 0.2
})

test_that("identifies max change correctly", {
  param_values <- c(0.1, 0.2, 0.3, 0.4, 0.5)
  posteriors <- data.frame(
    parameter_value = param_values,
    H1 = c(0.1, 0.3, 0.5, 0.7, 0.9)
  )
  # Current is 0.5 at param_value 0.3
  current_posteriors <- list(H1 = 0.5)

  summary <- compute_sweep_summary(param_values, posteriors, 0.3, current_posteriors)

  # Max change is +0.4 at param_value 0.5, or -0.4 at param_value 0.1
  expect_equal(abs(summary$max_change[1]), 0.4)
})

test_that("returns correct structure", {
  param_values <- c(0.1, 0.5, 0.9)
  posteriors <- data.frame(
    parameter_value = param_values,
    H1 = c(0.3, 0.5, 0.7),
    H2 = c(0.7, 0.5, 0.3)
  )
  current_posteriors <- list(H1 = 0.5, H2 = 0.5)

  summary <- compute_sweep_summary(param_values, posteriors, 0.5, current_posteriors)

  expect_true(is.data.frame(summary))
  expect_equal(nrow(summary), 2)  # One row per target
  expect_true(all(c("target_id", "current", "min", "max", "range",
                    "max_change", "max_change_at") %in% names(summary)))
})

# ============================================
# Tests for create_sweep_plot()
# ============================================
cat("\n--- create_sweep_plot() ---\n")

test_that("creates ggplot object", {
  # Create mock sweep result
  sweep_result <- list(
    metadata = list(
      parameter = list(display_name = "Test param"),
      target = "hypotheses",
      evidence = list()
    ),
    current_value = 0.5,
    posteriors = data.frame(
      parameter_value = c(0.1, 0.3, 0.5, 0.7, 0.9),
      H1 = c(0.3, 0.4, 0.5, 0.6, 0.7),
      H2 = c(0.7, 0.6, 0.5, 0.4, 0.3)
    )
  )

  plot <- create_sweep_plot(sweep_result)

  expect_true("ggplot" %in% class(plot))
})

test_that("plot includes current value line when show_current=TRUE", {
  sweep_result <- list(
    metadata = list(
      parameter = list(display_name = "Test param"),
      target = "hypotheses",
      evidence = list()
    ),
    current_value = 0.5,
    posteriors = data.frame(
      parameter_value = c(0.1, 0.5, 0.9),
      H1 = c(0.3, 0.5, 0.7)
    )
  )

  plot <- create_sweep_plot(sweep_result, show_current = TRUE)

  # Check that a geom_vline layer exists
  layer_classes <- sapply(plot$layers, function(l) class(l$geom)[1])
  expect_true("GeomVline" %in% layer_classes)
})

test_that("respects custom colors", {
  sweep_result <- list(
    metadata = list(
      parameter = list(display_name = "Test param"),
      target = "hypotheses",
      evidence = list()
    ),
    current_value = 0.5,
    posteriors = data.frame(
      parameter_value = c(0.1, 0.5, 0.9),
      H1 = c(0.3, 0.5, 0.7),
      H2 = c(0.7, 0.5, 0.3)
    )
  )

  custom_colors <- c(H1 = "#FF0000", H2 = "#00FF00")
  plot <- create_sweep_plot(sweep_result, colors = custom_colors)

  expect_true("ggplot" %in% class(plot))
})

# ============================================
# Integration Tests with COVID Case Study
# ============================================
cat("\n--- Integration: COVID Case Study ---\n")

# Helper: Generate all CPTs for a model (mirrors app.R pattern)
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

test_that("get_sweepable_parameters works with COVID case study", {
  model <- create_covid_case_study()
  params <- get_sweepable_parameters(model)

  # Should have parameters for:
  # - 2 hypotheses (prior each)
  # - auxiliaries (prior each)
  # - phenomena (strength, base_rate each)
  # - results (strength, base_rate each)
  # - links (reliability, epsilon each)
  expect_true(nrow(params) > 20)
  expect_true("H1" %in% params$node_id)
  expect_true("H2" %in% params$node_id)
})

test_that("compute_parameter_sweep works with simple model", {
  # Use simple test model for faster testing
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  # Create CPT generator function
  cpt_generator <- function(m) {
    g <- build_bayesian_network(m)
    s <- Selector$new(m)
    generate_all_cpts_for_model(m, g, s)
  }

  # Create inference function
  inference_func <- function(evidence_graph, cpts, evidence) {
    compute_posteriors(evidence_graph, cpts, evidence)
  }

  # Get parameters
  params <- get_sweepable_parameters(model)
  aux_param <- params[params$node_id == "A1" & params$param_name == "prior", ]

  param_spec <- list(
    node_id = aux_param$node_id[1],
    node_type = aux_param$node_type[1],
    param_name = aux_param$param_name[1],
    display_name = aux_param$display_name[1]
  )

  # Run sweep with evidence
  evidence <- list(R1 = "true")

  result <- compute_parameter_sweep(
    parameter_spec = param_spec,
    range_min = 0.5,
    range_max = 0.95,
    n_points = 5,  # Fewer points for faster test
    target = "hypotheses",
    model = model,
    evidence_graph = graph,
    cpt_generator = cpt_generator,
    inference_func = inference_func,
    evidence = evidence
  )

  # Verify structure
  expect_true(is.list(result))
  expect_true("posteriors" %in% names(result))
  expect_true("summary" %in% names(result))
  expect_equal(length(result$parameter_values), 5)

  # Posteriors should sum to 1 at each point
  for (i in 1:5) {
    h1_post <- result$posteriors$H1[i]
    h2_post <- result$posteriors$H2[i]
    expect_equal(h1_post + h2_post, 1.0, tolerance = 0.01)
  }
})

test_that("sweep results show sensitivity to auxiliary prior", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)

  cpt_generator <- function(m) {
    g <- build_bayesian_network(m)
    s <- Selector$new(m)
    generate_all_cpts_for_model(m, g, s)
  }

  inference_func <- function(evidence_graph, cpts, evidence) {
    compute_posteriors(evidence_graph, cpts, evidence)
  }

  param_spec <- list(
    node_id = "A1",
    node_type = "auxiliary",
    param_name = "prior",
    display_name = "A1 prior"
  )

  result <- compute_parameter_sweep(
    parameter_spec = param_spec,
    range_min = 0.1,
    range_max = 0.99,
    n_points = 5,
    target = "hypotheses",
    model = model,
    evidence_graph = graph,
    cpt_generator = cpt_generator,
    inference_func = inference_func,
    evidence = list(R1 = "true")
  )

  # Summary should show non-zero range (sensitivity)
  h1_summary <- result$summary[result$summary$target_id == "H1", ]
  expect_true(h1_summary$range > 0, "H1 should be sensitive to A1 prior")
})

# ============================================
# Summary
# ============================================
cat("\n===========================================\n")
cat(sprintf("Tests completed: %d passed / %d total\n", tests_passed, tests_run))

if (tests_passed == tests_run) {
  cat("All tests PASSED!\n")
} else {
  cat(sprintf("FAILED: %d tests\n", tests_run - tests_passed))
}
cat("===========================================\n")
