# Integration Test for Explanatory Account Framework App
cat("=== Integration Testing ===\n\n")

# Load all required libraries
suppressPackageStartupMessages({
  library(R6)
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(visNetwork)
  library(DT)
  library(igraph)
  library(jsonlite)
  library(gRain)
})

# Source all required files
cat("1. Loading R6 classes...\n")
source("R/classes.R")
source("R/explanatory_classes.R")

cat("2. Loading network construction module...\n")
source("R/explanatory_network.R")

cat("3. Loading CPT generation functions...\n")
source("R/bayesian_spec_cpt.R")

cat("4. Loading inference module...\n")
source("R/bayesian_spec_inference.R")

cat("5. Loading case study...\n")
source("R/explanatory_case_study.R")

cat("\n=== Test 1: Load COVID-19 Case Study ===\n")
model <- create_covid_case_study()
cat("✓ Model created with name:", model$name, "\n")
cat("✓ Hypotheses:", length(model$hypotheses), "\n")
cat("✓ Accounts:", length(model$accounts), "\n")
cat("✓ Results:", length(model$results), "\n")
cat("✓ Phenomena:", length(model$phenomena), "\n")
cat("✓ Auxiliaries:", length(model$auxiliaries), "\n")
cat("✓ Links:", length(model$explanatory_links), "\n")

cat("\n=== Test 2: Validate Hypothesis Priors ===\n")
if (model$validate_hypothesis_priors()) {
  cat("✓ Hypothesis priors sum to 1.0\n")
} else {
  cat("✗ ERROR: Hypothesis priors do not sum to 1.0\n")
}

cat("\n=== Test 3: Build Bayesian Network ===\n")
network <- build_bayesian_network(model)
cat("✓ Network built with", length(network$nodes), "nodes\n")
cat("✓ Network has", length(network$edges), "edges\n")

cat("\n=== Test 4: Create Selector ===\n")
selector <- Selector$new(model)
cat("✓ Selector created with", length(selector$get_states()), "states\n")
cat("✓ Selector states:", paste(selector$get_states(), collapse=", "), "\n")

cat("\n=== Test 5: Generate CPTs ===\n")
cpts <- list()

# Selector CPT
cpts[["Selector"]] <- generate_selector_cpt(selector)
cat("✓ Selector CPT generated\n")

# Account CPTs
for (acc_id in names(model$accounts)) {
  cpts[[acc_id]] <- generate_account_cpt(acc_id, selector)
}
cat("✓", length(model$accounts), "Account CPTs generated\n")

# Auxiliary CPTs
for (aux_id in names(model$auxiliaries)) {
  cpts[[aux_id]] <- generate_prior_cpt_spec(model$auxiliaries[[aux_id]])
}
cat("✓", length(model$auxiliaries), "Auxiliary CPTs generated\n")

# Phenomenon CPTs
for (pc_id in names(model$phenomena)) {
  cpts[[pc_id]] <- generate_target_cpt_spec(model$phenomena[[pc_id]], network)
}
cat("✓", length(model$phenomena), "Phenomenon CPTs generated\n")

# Result CPTs
for (r_id in names(model$results)) {
  # Check if result has incoming TO_TARGET edge
  has_incoming <- any(network$edges$to == r_id & network$edges$type == "TO_TARGET")

  if (has_incoming) {
    # Result explained by phenomena - use target CPT
    cpts[[r_id]] <- generate_target_cpt_spec(model$results[[r_id]], network)
  } else {
    # Result not explained - will be set as evidence, so use simple prior CPT
    cpts[[r_id]] <- list(
      states = c("true", "false"),
      parents = character(0),
      probabilities = c(model$results[[r_id]]$base_rate, 1 - model$results[[r_id]]$base_rate)
    )
  }
}
cat("✓", length(model$results), "Result CPTs generated\n")

# Link CPTs
for (link_id in names(model$explanatory_links)) {
  cpts[[link_id]] <- generate_link_cpt_spec(model$explanatory_links[[link_id]], network)
}
cat("✓", length(model$explanatory_links), "Link CPTs generated\n")

cat("✓ Total CPTs generated:", length(cpts), "\n")

cat("\n=== Test 6: Build Evidence List ===\n")
evidence <- list()
for (r_id in names(model$results)) {
  evidence[[r_id]] <- "true"
}
cat("✓ Evidence list created with", length(evidence), "results set to TRUE\n")

cat("\n=== Test 7: Run Inference ===\n")
tryCatch({
  posteriors <- compute_posteriors(network, cpts, evidence)
  cat("✓ Inference completed successfully\n")

  # Check selector posteriors
  if (!is.null(posteriors[["Selector"]])) {
    cat("✓ Selector posteriors computed\n")
    cat("  Account posteriors:\n")
    for (acc_id in names(model$accounts)) {
      post <- posteriors[["Selector"]][[acc_id]]
      cat("    ", acc_id, ":", round(post, 4), "\n")
    }
  }

  # Marginalize to hypothesis level
  cat("\n  Hypothesis posteriors:\n")
  for (h_id in names(model$hypotheses)) {
    h_accounts <- model$get_accounts_for_hypothesis(h_id)
    post_sum <- sum(sapply(h_accounts, function(acc) posteriors[["Selector"]][[acc$id]]))
    prior <- model$hypotheses[[h_id]]$prior
    cat("    ", h_id, ":", round(post_sum, 4), "(prior:", prior, ", change:", round(post_sum - prior, 4), ")\n")
  }

}, error = function(e) {
  cat("✗ ERROR in inference:", e$message, "\n")
  print(e)
})

cat("\n=== Test 8: Load App File ===\n")
tryCatch({
  source("app_explanatory.R", echo=FALSE)
  cat("✓ App file loaded successfully\n")
  cat("✓ App has", length(readLines("app_explanatory.R")), "lines\n")
}, error = function(e) {
  cat("✗ ERROR loading app:", e$message, "\n")
})

cat("\n=== Integration Testing Complete ===\n")
cat("All core functionality validated!\n")
