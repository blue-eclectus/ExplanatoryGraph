# Basic Tests for ExplanatoryAccounts Package
# These tests verify that core functionality works

test_that("BayesianHypothesis creates correctly", {
  h1 <- BayesianHypothesis$new(
    id = "H1",
    text = "Test hypothesis",
    source = "Test",
    prior = 0.5
  )

  expect_equal(h1$id, "H1")
  expect_equal(h1$text, "Test hypothesis")
  expect_equal(h1$prior, 0.5)
})

test_that("BayesianResult creates correctly", {
  r1 <- BayesianResult$new(
    id = "R1",
    text = "Test result",
    source = "Test"
  )

  expect_equal(r1$id, "R1")
  expect_equal(r1$text, "Test result")
  expect_true(r1$strength > 0)
  expect_true(r1$base_rate > 0)
})

test_that("ExplanatoryModel creates and adds hypotheses", {
  model <- ExplanatoryModel$new(name = "Test Model")

  h1 <- BayesianHypothesis$new(id = "H1", text = "Hypothesis 1", source = "Test", prior = 0.5)
  h2 <- BayesianHypothesis$new(id = "H2", text = "Hypothesis 2", source = "Test", prior = 0.5)

  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  expect_equal(length(model$hypotheses), 2)
  expect_true("H1" %in% names(model$hypotheses))
  expect_true("H2" %in% names(model$hypotheses))
})

test_that("Simple case study creates valid model", {
  model <- create_simple_test_case_study()

  expect_true(length(model$hypotheses) >= 2)
  expect_true(length(model$accounts) >= 2)
  expect_true(length(model$results) >= 1)
  expect_true(model$validate_hypothesis_priors())
})

test_that("COVID case study creates valid model", {
  model <- create_covid_case_study()

  expect_true(length(model$hypotheses) >= 2)
  expect_true(length(model$accounts) >= 2)
  expect_true(length(model$results) >= 1)
  expect_true(model$validate_hypothesis_priors())
})

test_that("build_bayesian_network works with simple model", {
  model <- create_simple_test_case_study()
  result <- build_bayesian_network(model)

  expect_true(!is.null(result$graph))
  expect_true(!is.null(result$selector))
  expect_true(length(result$graph$nodes) > 0)
  expect_true(nrow(result$graph$edges) > 0)
})
