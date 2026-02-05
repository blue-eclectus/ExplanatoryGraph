# ==============================================================================
# Explanatory Network Construction
# ==============================================================================
#
# Converts an ExplanatoryModel into an EvidenceGraph suitable for Bayesian
# inference. This is the bridge between the high-level explanatory framework
# and the low-level graph representation.
#
# NETWORK STRUCTURE
# -----------------
# The generated network has this structure:
#
#   Selector ──────────────────────────────┐
#      │                                   │
#      ▼                                   ▼
#   Account₁                            Account₂
#      │                                   │
#      ▼                                   ▼
#   EL₁ ← Aux₁                          EL₅ ← Aux₃
#      │                                   │
#      ▼                                   ▼
#   PC₁                                  PC₆
#      │                                   │
#      ▼                                   ▼
#   EL₂                                  EL₆
#      │                                   │
#      └───────────────► R₁ ◄──────────────┘
#
# DEPENDENCIES
# ------------
# - R/classes.R (EvidenceGraph, Node)
# - R/explanatory_classes.R (ExplanatoryModel, Selector)

if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  if (!exists("EvidenceGraph")) {
    source("R/classes.R")
  }

  if (!exists("ExplanatoryModel")) {
    source("R/explanatory_classes.R")
  }
}

#' Build Bayesian Network from ExplanatoryModel
#'
#' Transforms an ExplanatoryModel into an EvidenceGraph with all nodes and
#' edges needed for Bayesian belief propagation.
#'
#' CONSTRUCTION STEPS:
#' 1. Create Selector node (multi-state, one state per account)
#' 2. Create Account nodes (binary, deterministic given selector)
#' 3. Add Auxiliary nodes (root nodes with independent priors)
#' 4. Add Phenomenon nodes (intermediate explanatory targets)
#' 5. Add Result nodes (evidence corpus)
#' 6. Add ExplanatoryLink nodes with FROM_SOURCE, TO_TARGET, IS_REQUIRED_BY edges
#'
#' @param model ExplanatoryModel object (must pass validate_hypothesis_priors())
#' @return EvidenceGraph object ready for CPT generation and inference
#'
#' @examples
#' model <- create_covid_case_study()
#' graph <- build_bayesian_network(model)
#' # Next: generate CPTs and run inference
build_bayesian_network <- function(model) {
  if (!inherits(model, "ExplanatoryModel")) {
    stop("model must be an ExplanatoryModel object")
  }

  # Initialize empty graph
  graph <- EvidenceGraph$new()

  # -------------------------------------------------------------------------
  # Step 1: Create Selector node
  # The selector is a multi-state node; each state corresponds to an account.
  # -------------------------------------------------------------------------
  selector <- Selector$new(model)
  selector_node <- Node$new(
    id = "Selector",
    type = "Selector",
    text = "Account Selector",
    source = "Generated"
  )
  graph$add_node(selector_node)

  # -------------------------------------------------------------------------
  # Step 2: Create Account nodes
  # Binary nodes with deterministic CPT: true iff selector selects this account.
  # -------------------------------------------------------------------------
  for (acc_id in names(model$accounts)) {
    account <- model$accounts[[acc_id]]
    acc_node <- Node$new(
      id = acc_id,
      type = "Account",
      text = account$description,
      source = "User"
    )
    graph$add_node(acc_node)

    # Add edge from Selector to Account
    graph$add_edge("Selector", acc_id, "FROM_SOURCE")
  }

  # -------------------------------------------------------------------------
  # Step 3: Add Auxiliary nodes (root nodes with independent priors)
  # -------------------------------------------------------------------------
  for (aux_id in names(model$auxiliaries)) {
    auxiliary <- model$auxiliaries[[aux_id]]
    graph$add_node(auxiliary)
  }

  # -------------------------------------------------------------------------
  # Step 4: Add Phenomenon nodes (shared across accounts)
  # -------------------------------------------------------------------------
  for (pc_id in names(model$phenomena)) {
    phenomenon <- model$phenomena[[pc_id]]
    graph$add_node(phenomenon)
  }

  # -------------------------------------------------------------------------
  # Step 5: Add Result nodes (evidence corpus)
  # -------------------------------------------------------------------------
  for (r_id in names(model$results)) {
    result <- model$results[[r_id]]
    graph$add_node(result)
  }

  # -------------------------------------------------------------------------
  # Step 6: Add ExplanatoryLink nodes and wire up edges
  # Each link creates three types of edges:
  # - FROM_SOURCE: source(s) → link (supports conjunctive via multiple sources)
  # - IS_REQUIRED_BY: auxiliary → link (prerequisite dependencies)
  # - TO_TARGET: link → target (what the link explains)
  # -------------------------------------------------------------------------
  for (link_id in names(model$explanatory_links)) {
    link <- model$explanatory_links[[link_id]]
    graph$add_node(link)

    # FROM_SOURCE edges (supports conjunctive: multiple sources → one link)
    for (src_id in link$source_ids) {
      graph$add_edge(src_id, link_id, "FROM_SOURCE")
    }

    # IS_REQUIRED_BY edges (auxiliary prerequisites)
    if (!is.null(link$required_auxiliary_ids) && length(link$required_auxiliary_ids) > 0) {
      for (aux_id in link$required_auxiliary_ids) {
        graph$add_edge(aux_id, link_id, "IS_REQUIRED_BY")
      }
    }

    # TO_TARGET edge (what this link explains)
    graph$add_edge(link_id, link$target_id, "TO_TARGET")
  }

  return(graph)
}
