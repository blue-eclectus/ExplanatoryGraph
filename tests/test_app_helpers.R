# Tests for App Helper Functions
#
# Tests for R/app_helpers.R functions:
#   - generate_inference_name()
#   - build_results_evidence()
#   - validate_id()
#   - get_selection_label()
#   - add_saved_inference_choices()
#   - generate_all_cpts_for_model()

library(R6)

# Source required modules
source("R/classes.R")
source("R/explanatory_classes.R")
source("R/bayesian_spec_classes.R")
source("R/bayesian_spec_cpt.R")
source("R/explanatory_network.R")
source("R/bayesian.R")
source("R/explanatory_case_study.R")
source("R/app_helpers.R")

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
      stop(sprintf("Expected '%s' but got '%s'", toString(expected), toString(actual)))
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

cat("\n=== Testing App Helper Functions ===\n\n")

# ============================================
# Tests for generate_inference_name()
# ============================================
cat("--- generate_inference_name() ---\n")

test_that("empty conditions returns 'All results=TRUE'", {
  result <- generate_inference_name(list())
  expect_equal(result, "All results=TRUE")
})

test_that("single TRUE condition formats correctly", {
  result <- generate_inference_name(list(A1 = "true"))
  expect_equal(result, "Results + A1=T")
})

test_that("single FALSE condition formats correctly", {
  result <- generate_inference_name(list(A1 = "false"))
  expect_equal(result, "Results + A1=F")
})

test_that("multiple conditions format correctly", {
  result <- generate_inference_name(list(A1 = "true", PC1 = "false"))
  expect_equal(result, "Results + A1=T, PC1=F")
})

test_that("multiple TRUE conditions format correctly", {
  result <- generate_inference_name(list(A1 = "true", A2 = "true", PC1 = "true"))
  expect_equal(result, "Results + A1=T, A2=T, PC1=T")
})

# ============================================
# Tests for build_results_evidence()
# ============================================
cat("\n--- build_results_evidence() ---\n")

test_that("model with 2 results returns named list of 'true' values", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_result(BayesianResult$new("R1", "Result 1", "Test", strength = 0.9, base_rate = 0.1))
  model$add_result(BayesianResult$new("R2", "Result 2", "Test", strength = 0.9, base_rate = 0.1))

  result <- build_results_evidence(model)

  expect_equal(length(result), 2)
  expect_equal(result[["R1"]], "true")
  expect_equal(result[["R2"]], "true")
  expect_true("R1" %in% names(result), "Result should have name R1")
  expect_true("R2" %in% names(result), "Result should have name R2")
})

test_that("model with 1 result returns single-element named vector", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_result(BayesianResult$new("R1", "Result 1", "Test", strength = 0.9, base_rate = 0.1))

  result <- build_results_evidence(model)

  expect_equal(length(result), 1)
  expect_equal(result[["R1"]], "true")
})

test_that("model with no results returns empty named vector", {
  model <- ExplanatoryModel$new(name = "Test")

  result <- build_results_evidence(model)

  expect_equal(length(result), 0)
})

test_that("all values in evidence are 'true'", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_result(BayesianResult$new("R1", "Result 1", "Test", strength = 0.9, base_rate = 0.1))
  model$add_result(BayesianResult$new("R2", "Result 2", "Test", strength = 0.9, base_rate = 0.1))
  model$add_result(BayesianResult$new("R3", "Result 3", "Test", strength = 0.9, base_rate = 0.1))

  result <- build_results_evidence(model)

  expect_true(all(result == "true"), "All evidence values must be 'true'")
})

# ============================================
# Tests for validate_id()
# ============================================
cat("\n--- validate_id() ---\n")

test_that("empty string ID throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  expect_error(validate_id("", model), "cannot be empty")
})

test_that("NULL ID throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  # NULL triggers an error (trimws(NULL) produces character(0), which fails nchar check)
  expect_error(validate_id(NULL, model))
})

test_that("whitespace-only ID throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  expect_error(validate_id("   ", model), "cannot be empty")
})

test_that("ID with invalid characters throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  expect_error(validate_id("R1 invalid", model), "only contain letters")
})

test_that("ID with special characters throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  expect_error(validate_id("R1@#$", model), "only contain letters")
})

test_that("ID with spaces throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  expect_error(validate_id("my id", model), "only contain letters")
})

test_that("duplicate result ID throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_result(BayesianResult$new("R1", "Result 1", "Test", strength = 0.9, base_rate = 0.1))
  expect_error(validate_id("R1", model, "result"), "already exists")
})

test_that("duplicate hypothesis ID throws error", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_hypothesis(BayesianHypothesis$new("H1", "Hyp 1", "Test", prior = 1.0))
  expect_error(validate_id("H1", model, "hypothesis"), "already exists")
})

test_that("valid alphanumeric ID returns TRUE", {
  model <- ExplanatoryModel$new(name = "Test")
  result <- validate_id("R1", model, "result")
  expect_true(result)
})

test_that("valid ID with underscores returns TRUE", {
  model <- ExplanatoryModel$new(name = "Test")
  result <- validate_id("my_result_1", model, "result")
  expect_true(result)
})

test_that("valid ID with hyphens returns TRUE", {
  model <- ExplanatoryModel$new(name = "Test")
  result <- validate_id("my-result-1", model, "result")
  expect_true(result)
})

test_that("non-duplicate ID across entity types returns TRUE", {
  model <- ExplanatoryModel$new(name = "Test")
  model$add_result(BayesianResult$new("R1", "Result 1", "Test", strength = 0.9, base_rate = 0.1))
  # Different entity type, same ID check against results should pass for hypothesis
  result <- validate_id("H1", model, "hypothesis")
  expect_true(result)
})

# ============================================
# Tests for get_selection_label()
# ============================================
cat("\n--- get_selection_label() ---\n")

test_that("NULL selection returns 'Baseline'", {
  result <- get_selection_label(NULL)
  expect_equal(result, "Baseline")
})

test_that("empty string returns 'Baseline'", {
  result <- get_selection_label("")
  expect_equal(result, "Baseline")
})

test_that("'baseline' returns 'Baseline'", {
  result <- get_selection_label("baseline")
  expect_equal(result, "Baseline")
})

test_that("'unconditional' returns 'Unconditional'", {
  result <- get_selection_label("unconditional")
  expect_equal(result, "Unconditional")
})

test_that("'current' returns 'Current'", {
  result <- get_selection_label("current")
  expect_equal(result, "Current")
})

test_that("arbitrary string returns as-is", {
  result <- get_selection_label("My Saved Inference")
  expect_equal(result, "My Saved Inference")
})

test_that("another arbitrary string returns as-is", {
  result <- get_selection_label("Results + A1=T")
  expect_equal(result, "Results + A1=T")
})

# ============================================
# Tests for add_saved_inference_choices()
# ============================================
cat("\n--- add_saved_inference_choices() ---\n")

test_that("empty saved inferences returns choices unchanged", {
  choices <- c(Baseline = "baseline", Current = "current")
  result <- add_saved_inference_choices(choices, list())
  expect_equal(result, choices)
})

test_that("non-empty saved inferences appends to choices", {
  choices <- c(Baseline = "baseline", Current = "current")
  saved <- list("Run 1" = list(marginals = list()), "Run 2" = list(marginals = list()))

  result <- add_saved_inference_choices(choices, saved)

  expect_equal(length(result), 4)
  expect_true("Run 1" %in% result, "Should contain 'Run 1'")
  expect_true("Run 2" %in% result, "Should contain 'Run 2'")
})

test_that("original choices are preserved after appending", {
  choices <- c(Baseline = "baseline", Current = "current")
  saved <- list("My Inference" = list(marginals = list()))

  result <- add_saved_inference_choices(choices, saved)

  expect_equal(result[["Baseline"]], "baseline")
  expect_equal(result[["Current"]], "current")
})

test_that("single saved inference appends correctly", {
  choices <- c(Baseline = "baseline")
  saved <- list("Single Run" = list(marginals = list()))

  result <- add_saved_inference_choices(choices, saved)

  expect_equal(length(result), 2)
  expect_true("Single Run" %in% result, "Should contain 'Single Run'")
})

# ============================================
# Tests for generate_all_cpts_for_model()
# ============================================
cat("\n--- generate_all_cpts_for_model() ---\n")

test_that("simple case study produces named list of CPTs", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  expect_true(is.list(cpts), "CPTs should be a list")
  expect_true(length(cpts) > 0, "CPTs should not be empty")
})

test_that("CPTs include Selector node", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  expect_true("Selector" %in% names(cpts), "CPTs should include 'Selector'")
})

test_that("CPTs include all account nodes", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  for (acc_id in names(model$accounts)) {
    expect_true(acc_id %in% names(cpts),
                sprintf("CPTs should include account '%s'", acc_id))
  }
})

test_that("CPTs include all auxiliary nodes", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  for (aux_id in names(model$auxiliaries)) {
    expect_true(aux_id %in% names(cpts),
                sprintf("CPTs should include auxiliary '%s'", aux_id))
  }
})

test_that("CPTs include all phenomenon nodes", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  for (pc_id in names(model$phenomena)) {
    expect_true(pc_id %in% names(cpts),
                sprintf("CPTs should include phenomenon '%s'", pc_id))
  }
})

test_that("CPTs include all result nodes", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  for (r_id in names(model$results)) {
    expect_true(r_id %in% names(cpts),
                sprintf("CPTs should include result '%s'", r_id))
  }
})

test_that("CPTs include all explanatory link nodes", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  for (link_id in names(model$explanatory_links)) {
    expect_true(link_id %in% names(cpts),
                sprintf("CPTs should include explanatory link '%s'", link_id))
  }
})

test_that("Selector CPT has correct states (one per account)", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)
  selector_cpt <- cpts[["Selector"]]

  expect_true("states" %in% names(selector_cpt), "Selector CPT should have 'states'")
  expect_equal(length(selector_cpt$states), length(model$accounts))
})

test_that("total CPT count matches expected node count", {
  model <- create_simple_test_case_study()
  graph <- build_bayesian_network(model)
  selector <- Selector$new(model)

  cpts <- generate_all_cpts_for_model(model, graph, selector)

  # Expected: 1 Selector + accounts + auxiliaries + phenomena + results + links
  expected_count <- 1 +
    length(model$accounts) +
    length(model$auxiliaries) +
    length(model$phenomena) +
    length(model$results) +
    length(model$explanatory_links)

  expect_equal(length(cpts), expected_count)
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
