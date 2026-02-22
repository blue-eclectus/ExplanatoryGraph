# ==============================================================================
# App Helper Functions
# ==============================================================================
#
# Pure (non-reactive) helper functions used by the Shiny app.
# Extracted from app.R to support modularization.
#
# Functions:
#   - generate_all_cpts_for_model(): Build CPTs for all nodes
#   - generate_inference_name(): Human-readable name for inference conditions
#   - get_marginals_for_selection(): Resolve dropdown selection to marginals
#   - get_selection_label(): Display label for a selection
#   - build_results_evidence(): Evidence list with all results TRUE
#   - add_saved_inference_choices(): Append saved inference names to choices
#   - validate_id(): Validate entity ID for model use
# ==============================================================================

# Source required modules (for development mode)
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  if (!exists("generate_selector_cpt")) source("R/bayesian_spec_cpt.R")
}

#' Generate all CPTs for a model
#'
#' Creates Conditional Probability Tables for all nodes in the Bayesian network:
#' - Selector: Equal probability across accounts
#' - Accounts: Conditional on Selector
#' - Auxiliaries: Simple prior CPTs
#' - Phenomena: Target CPTs based on incoming links
#' - Results: Target CPTs if explained, prior CPTs otherwise
#' - ExplanatoryLinks: Link CPTs based on sources and auxiliaries
#'
#' @param model ExplanatoryModel object
#' @param bayesian_network EvidenceGraph built from the model
#' @param selector Selector object for the model
#' @return Named list of CPTs, keyed by node ID
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
      # Result explained by phenomena - use target CPT
      cpts[[r_id]] <- generate_target_cpt_spec(model$results[[r_id]], bayesian_network)
    } else {
      # Result not explained - use simple prior CPT
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

#' Generate descriptive name for an inference based on conditions
#'
#' Creates a human-readable name describing what conditions were applied
#' during inference. Used for auto-naming saved inferences.
#'
#' @param conditions Named list of additional conditions (node_id -> "true"/"false")
#' @return String like "All results=TRUE" or "Results + A1=F, PC1=T"
generate_inference_name <- function(conditions) {
  if (length(conditions) == 0) {
    return("All results=TRUE")
  }

  parts <- sapply(names(conditions), function(node_id) {
    state <- if (conditions[[node_id]] == "true") "T" else "F"
    sprintf("%s=%s", node_id, state)
  })

  paste("Results +", paste(parts, collapse = ", "))
}

#' Get marginals for a comparison selection
#'
#' Resolves a dropdown selection to the corresponding marginals.
#'
#' @param selection The selection value from the dropdown
#' @param rv The reactive values object
#' @return Named list of marginal distributions, or NULL
get_marginals_for_selection <- function(selection, rv) {
  if (is.null(selection) || selection == "") return(rv$baseline)

  switch(selection,
    unconditional = rv$unconditional,
    baseline = rv$baseline,
    current = rv$posteriors,
    {
      if (selection %in% names(rv$saved_inferences)) {
        rv$saved_inferences[[selection]]$marginals
      } else {
        rv$baseline
      }
    }
  )
}

#' Get display label for a comparison/posterior selection
#'
#' @param selection The selection value from the dropdown
#' @return Human-readable label string
get_selection_label <- function(selection) {
  if (is.null(selection) || selection == "" || selection == "baseline") return("Baseline")
  if (selection == "unconditional") return("Unconditional")
  if (selection == "current") return("Current")
  selection
}

#' Build evidence list with all results set to TRUE
#'
#' @param model ExplanatoryModel object
#' @return Named list mapping result IDs to "true"
build_results_evidence <- function(model) {
  setNames(rep("true", length(model$results)), names(model$results))
}

#' Add saved inference names to a choices vector
#'
#' @param choices Existing named character vector of choices
#' @param saved_inferences List of saved inferences
#' @return Updated choices vector with saved inference names appended
add_saved_inference_choices <- function(choices, saved_inferences) {
  if (length(saved_inferences) > 0) {
    saved_names <- names(saved_inferences)
    saved_choices <- setNames(saved_names, saved_names)
    choices <- c(choices, saved_choices)
  }
  choices
}

#' Validate an entity ID
#'
#' Checks that an ID is valid for use in the model:
#' - Not empty or whitespace-only
#' - Contains only alphanumeric characters, underscores, and hyphens
#' - Not already used in the model
#'
#' @param id The ID to validate
#' @param model The ExplanatoryModel to check for duplicates
#' @param entity_type Type of entity ("result", "hypothesis", "account", "phenomenon", "auxiliary")
#' @return TRUE if valid, otherwise throws an error with a descriptive message
validate_id <- function(id, model, entity_type = "entity") {
  # Trim whitespace
  id <- trimws(id)

  # Check empty

  if (is.null(id) || nchar(id) == 0) {
    stop(sprintf("%s ID cannot be empty", tools::toTitleCase(entity_type)))
  }

  # Check valid characters (alphanumeric, underscore, hyphen)
  if (!grepl("^[A-Za-z0-9_-]+$", id)) {
    stop(sprintf("%s ID can only contain letters, numbers, underscores, and hyphens", tools::toTitleCase(entity_type)))
  }

  # Check for duplicates based on entity type
  existing_ids <- switch(entity_type,
    result = names(model$results),
    hypothesis = names(model$hypotheses),
    account = names(model$accounts),
    phenomenon = names(model$phenomena),
    auxiliary = names(model$auxiliaries),
    c(names(model$results), names(model$hypotheses), names(model$accounts),
      names(model$phenomena), names(model$auxiliaries))
  )

  if (id %in% existing_ids) {
    stop(sprintf("%s with ID '%s' already exists", tools::toTitleCase(entity_type), id))
  }

  return(TRUE)
}