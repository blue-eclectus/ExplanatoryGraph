# Tests for Probability Visualization Feature
# Task 8 from 2025-11-24-probability-visualization.md

# Load required packages
library(R6)
library(igraph)

# Source modules
source("R/bayesian_spec_classes.R")
source("R/probability_viz_helpers.R")

cat("\n========================================\n")
cat("PROBABILITY VISUALIZATION HELPER TESTS\n")
cat("========================================\n\n")

# Test counter
test_count <- 0
pass_count <- 0
fail_count <- 0

# Test helper
test <- function(description, code) {
  test_count <<- test_count + 1
  cat(sprintf("Test %d: %s\n", test_count, description))

  tryCatch({
    code
    cat("  PASS\n\n")
    pass_count <<- pass_count + 1
    TRUE
  }, error = function(e) {
    cat(sprintf("  FAIL: %s\n\n", e$message))
    fail_count <<- fail_count + 1
    FALSE
  })
}

# Assertion helpers
assert_equal <- function(actual, expected, msg = "") {
  if (!identical(actual, expected)) {
    stop(sprintf("Expected %s but got %s %s", expected, actual, msg))
  }
}

assert_true <- function(condition, msg = "") {
  if (!isTRUE(condition)) {
    stop(sprintf("Expected TRUE but got FALSE %s", msg))
  }
}

assert_match <- function(string, pattern, msg = "") {
  if (!grepl(pattern, string)) {
    stop(sprintf("'%s' does not match pattern '%s' %s", string, pattern, msg))
  }
}

# ============================================================================
# TEST 1: probability_to_color function
# ============================================================================

cat("TEST GROUP 1: probability_to_color function\n")
cat("--------------------------------------------\n\n")

test("probability_to_color(0) returns red (#FF0000)", {
  result <- probability_to_color(0)
  assert_equal(result, "#FF0000")
  cat(sprintf("  Result: %s\n", result))
})

test("probability_to_color(1) returns green (#00FF00)", {
  result <- probability_to_color(1)
  assert_equal(result, "#00FF00")
  cat(sprintf("  Result: %s\n", result))
})

test("probability_to_color(0.5) returns valid hex color", {
  result <- probability_to_color(0.5)
  assert_match(result, "^#[0-9A-F]{6}$")
  cat(sprintf("  Result: %s (should be yellowish)\n", result))
})

test("probability_to_color(NA) returns gray (#CCCCCC)", {
  result <- probability_to_color(NA)
  assert_equal(result, "#CCCCCC")
  cat(sprintf("  Result: %s\n", result))
})

test("probability_to_color clamps negative values to 0", {
  result <- probability_to_color(-0.5)
  expected <- probability_to_color(0)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to red)\n", result))
})

test("probability_to_color clamps values > 1 to 1", {
  result <- probability_to_color(1.5)
  expected <- probability_to_color(1)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to green)\n", result))
})

# ============================================================================
# TEST 2: delta_to_color function
# ============================================================================

cat("TEST GROUP 2: delta_to_color function\n")
cat("--------------------------------------\n\n")

test("delta_to_color(-1) returns blue (#0000FF)", {
  result <- delta_to_color(-1)
  assert_equal(result, "#0000FF")
  cat(sprintf("  Result: %s\n", result))
})

test("delta_to_color(1) returns orange (#FFA500)", {
  result <- delta_to_color(1)
  assert_equal(result, "#FFA500")
  cat(sprintf("  Result: %s\n", result))
})

test("delta_to_color(0) returns white (#FFFFFF)", {
  result <- delta_to_color(0)
  assert_equal(result, "#FFFFFF")
  cat(sprintf("  Result: %s\n", result))
})

test("delta_to_color(NA) returns gray (#CCCCCC)", {
  result <- delta_to_color(NA)
  assert_equal(result, "#CCCCCC")
  cat(sprintf("  Result: %s\n", result))
})

test("delta_to_color clamps values < -1 to -1", {
  result <- delta_to_color(-2)
  expected <- delta_to_color(-1)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to blue)\n", result))
})

test("delta_to_color clamps values > 1 to 1", {
  result <- delta_to_color(2)
  expected <- delta_to_color(1)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to orange)\n", result))
})

# ============================================================================
# TEST 3: absolute_change_to_color function
# ============================================================================

cat("TEST GROUP 3: absolute_change_to_color function\n")
cat("------------------------------------------------\n\n")

test("absolute_change_to_color(0) returns white (#FFFFFF)", {
  result <- absolute_change_to_color(0)
  assert_equal(result, "#FFFFFF")
  cat(sprintf("  Result: %s\n", result))
})

test("absolute_change_to_color(1) returns purple (#800080)", {
  result <- absolute_change_to_color(1)
  assert_equal(result, "#800080")
  cat(sprintf("  Result: %s\n", result))
})

test("absolute_change_to_color(NA) returns gray (#CCCCCC)", {
  result <- absolute_change_to_color(NA)
  assert_equal(result, "#CCCCCC")
  cat(sprintf("  Result: %s\n", result))
})

test("absolute_change_to_color clamps negative values to 0", {
  result <- absolute_change_to_color(-0.5)
  expected <- absolute_change_to_color(0)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to white)\n", result))
})

test("absolute_change_to_color clamps values > 1 to 1", {
  result <- absolute_change_to_color(1.5)
  expected <- absolute_change_to_color(1)
  assert_equal(result, expected)
  cat(sprintf("  Result: %s (clamped to purple)\n", result))
})

# ============================================================================
# TEST 4: get_node_type_color function
# ============================================================================

cat("TEST GROUP 4: get_node_type_color function\n")
cat("-------------------------------------------\n\n")

test("BayesianHypothesis returns blue (#3589DD)", {
  h1 <- BayesianHypothesis$new("H1", "test", "", 0.8, 0.5, "primary")
  result <- get_node_type_color(h1)
  assert_equal(result, "#3589DD")
  cat(sprintf("  Result: %s\n", result))
})

test("BayesianAuxiliary returns orange (#E8AB79)", {
  a1 <- BayesianAuxiliary$new("A1", "test", "", 0.8, 0.8, "BackgroundTheory")
  result <- get_node_type_color(a1)
  assert_equal(result, "#E8AB79")
  cat(sprintf("  Result: %s\n", result))
})

test("BayesianPhenomenon returns light teal (#A2E8DD)", {
  p1 <- BayesianPhenomenon$new("P1", "test", "", 0.8, 0.9, 0.05)
  result <- get_node_type_color(p1)
  assert_equal(result, "#A2E8DD")
  cat(sprintf("  Result: %s\n", result))
})

test("BayesianResult returns purple (#8D89BA)", {
  r1 <- BayesianResult$new("R1", "test", "", 0.8, 0.95, 0.1, "Quantitative", 100)
  result <- get_node_type_color(r1)
  assert_equal(result, "#8D89BA")
  cat(sprintf("  Result: %s\n", result))
})

# ============================================================================
# TEST SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("TEST SUMMARY\n")
cat("========================================\n")
cat(sprintf("Total tests: %d\n", test_count))
cat(sprintf("Passed: %d\n", pass_count))
cat(sprintf("Failed: %d\n", fail_count))

if (fail_count == 0) {
  cat("\nAll tests PASSED!\n\n")
} else {
  cat(sprintf("\nWARNING: %d test(s) FAILED!\n\n", fail_count))
  quit(status = 1)
}
