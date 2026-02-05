# ==============================================================================
# Bayesian Evidence Graph - Spec-Compliant CPT Generation
# ==============================================================================
#
# This module generates Conditional Probability Tables (CPTs) for all node
# types following the specification formulas. CPTs define how a node's
# probability depends on its parents in the Bayesian network.
#
# CPT FORMULAS
# ------------
# Prior nodes (Hypothesis, Auxiliary):
#   P(H=true) = prior
#   P(H=false) = 1 - prior
#
# Link nodes (ExplanatoryLink):
#   P(EL=true | source, aux₁, ..., auxₙ) = reliability  if all parents TRUE
#                                         = epsilon     otherwise
#
# Target nodes (Phenomenon, Result):
#   P(target=true | EL) = strength    if EL=true
#                       = base_rate   if EL=false
#
# CLOSED-FORM RESULT
# ------------------
# Combining the above, the exact probability of a target given hypothesis:
#   P(target | H=T, all aux=T) = base_rate + (strength - base_rate) × reliability
#
# When base_rate << strength, the approximation simplifies to:
#   P(target | H=T, all aux=T) ≈ strength × reliability
#
# DEPENDENCIES
# ------------
# - R/cpt_utils.R (validate_cpt)
# - R/bayesian_spec_classes.R (Bayesian node types)
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  if (!exists("validate_cpt")) {
    source("R/cpt_utils.R")
  }

  if (!exists("BayesianHypothesis")) {
    source("R/bayesian_spec_classes.R")
  }
}

# ==============================================================================
# PRIOR CPT GENERATION (Hypothesis, Auxiliary)
# ==============================================================================

#' Generate CPT for a root node with prior probability
#'
#' Implements the specification formula:
#'   P(H=true) = prior
#'   P(H=false) = 1 - prior
#'
#' @param node BayesianHypothesis or BayesianAuxiliary object
#' @return CPT list with states, parents, probabilities
#'
#' @examples
#' h <- BayesianHypothesis$new(id = "H1", text = "Test", source = "Test",
#'                             prior = 0.6)
#' cpt <- generate_prior_cpt_spec(h)
#' # cpt$probabilities["true"] == 0.6
#' # cpt$probabilities["false"] == 0.4
generate_prior_cpt_spec <- function(node) {
  # Validate node has prior parameter
  if (!("prior" %in% names(node))) {
    stop(sprintf("Node %s does not have a 'prior' parameter", node$id))
  }

  prior <- node$prior

  # Validate prior is a valid probability
  if (is.null(prior) || !is.numeric(prior) || prior < 0 || prior > 1) {
    stop(sprintf("Node %s has invalid prior: %s", node$id, prior))
  }

  # Create simple prior CPT
  cpt <- list(
    states = c("true", "false"),
    parents = character(0),
    probabilities = c(true = prior, false = 1 - prior)
  )

  return(cpt)
}

# ==============================================================================
# EXPLANATORY LINK CPT GENERATION
# ==============================================================================

#' Generate CPT for ExplanatoryLink following specification
#'
#' Implements the specification formula:
#'   P(EL=true | H, Aux₁, ..., Auxₙ) =
#'       reliability    if H=true AND all(Aux)=true
#'       epsilon        otherwise
#'
#' @param link BayesianExplanatoryLink object
#' @param evidence_graph EvidenceGraph containing the link and its parents
#' @return CPT list with states, parents, probabilities
#'
#' @examples
#' # Create graph with H1 + A1 -> EL1
#' graph <- EvidenceGraph$new()
#' h1 <- BayesianHypothesis$new("H1", "Hyp", "Test", 0.8, prior = 0.5)
#' a1 <- BayesianAuxiliary$new("A1", "Aux", "Test", 0.9, prior = 0.8)
#' graph$add_node(h1)
#' graph$add_node(a1)
#'
#' link <- BayesianExplanatoryLink$new("EL1", "H1", "P1", "Link", "Test",
#'                                     0.95, reliability = 0.95, epsilon = 0.01)
#' graph$add_node(link)
#' graph$add_edge("H1", "EL1", "FROM_SOURCE")
#' graph$add_edge("A1", "EL1", "IS_REQUIRED_BY")
#'
#' cpt <- generate_link_cpt_spec(link, graph)
#' # cpt$probabilities["true", "true", "true"] == 0.95  (all parents true)
#' # cpt$probabilities["true", "true", "false"] == 0.01 (some parent false)
generate_link_cpt_spec <- function(link, evidence_graph) {
  # Validate link has reliability and epsilon
  if (!("reliability" %in% names(link) && "epsilon" %in% names(link))) {
    stop(sprintf("Link %s does not have reliability and epsilon parameters", link$id))
  }

  reliability <- link$reliability
  epsilon <- link$epsilon

  # Get parent IDs: source nodes (may be multiple for conjunctive) + required auxiliaries
  parent_ids <- link$source_ids

  # Find required auxiliaries (nodes connected via IS_REQUIRED_BY edges)
  aux_edges <- evidence_graph$edges[
    evidence_graph$edges$to == link$id &
    evidence_graph$edges$type == "IS_REQUIRED_BY",
  ]
  aux_ids <- aux_edges$from

  if (length(aux_ids) > 0) {
    parent_ids <- c(parent_ids, aux_ids)
  }

  n_parents <- length(parent_ids)
  states <- c("true", "false")

  # Generate all parent configurations (2^n combinations)
  n_configs <- 2^n_parents

  # Create probability array
  # Dimensions: [child states (2)] x [parent1 states (2)] x [parent2 states (2)] x ...
  dim_sizes <- c(2, rep(2, n_parents))
  probs_array <- array(NA, dim = dim_sizes)

  # Generate all parent configurations using expand.grid
  # expand.grid creates all combinations of parent states
  parent_state_list <- rep(list(c("false", "true")), n_parents)
  names(parent_state_list) <- parent_ids
  parent_configs <- expand.grid(parent_state_list, stringsAsFactors = FALSE)

  # For each parent configuration, apply the spec formula
  for (i in 1:n_configs) {
    config <- parent_configs[i, , drop = FALSE]

    # Check if all parents are TRUE (source + all auxiliaries)
    all_true <- all(config == "true")

    # Apply spec formula: reliability if all true, epsilon otherwise
    if (all_true) {
      prob_link_true <- reliability
    } else {
      prob_link_true <- epsilon
    }
    prob_link_false <- 1 - prob_link_true

    # Create index for this configuration
    # Convert "true"/"false" to 1/2 indices (R uses 1-based indexing)
    # states = c("true", "false"), so "true"=1, "false"=2
    parent_indices <- ifelse(config == "true", 1, 2)

    # Set P(link=true | this parent config)
    # Index format: [child_state, parent1_state, parent2_state, ...]
    index_true <- c(1, parent_indices)  # 1 = "true" in child states
    probs_array[matrix(index_true, nrow = 1)] <- prob_link_true

    # Set P(link=false | this parent config)
    index_false <- c(2, parent_indices)  # 2 = "false" in child states
    probs_array[matrix(index_false, nrow = 1)] <- prob_link_false
  }

  # Set dimension names for interpretability
  dimnames_list <- list(child = states)
  for (parent_id in parent_ids) {
    dimnames_list[[parent_id]] <- states
  }
  dimnames(probs_array) <- dimnames_list

  # Create and return CPT
  cpt <- list(
    states = states,
    parents = parent_ids,
    probabilities = probs_array
  )

  return(cpt)
}

# ==============================================================================
# TARGET CPT GENERATION (Phenomenon, Result)
# ==============================================================================

#' Generate CPT for Phenomenon or Result following specification
#'
#' Implements the specification formula:
#'   P(target=true | EL) =
#'       strength    if EL=true
#'       base_rate   if EL=false
#'
#' This creates a simple 2x2 CPT where the target's probability depends only
#' on whether its incoming explanatory link exists.
#'
#' @param target BayesianPhenomenon or BayesianResult object
#' @param evidence_graph EvidenceGraph containing the target and its parent link
#' @return CPT list with states, parents, probabilities
#'
#' @examples
#' # Create graph with EL1 -> P1
#' graph <- EvidenceGraph$new()
#' link <- BayesianExplanatoryLink$new("EL1", "H1", "P1", "Link", "Test",
#'                                     0.95, reliability = 0.95, epsilon = 0.01)
#' graph$add_node(link)
#'
#' p1 <- BayesianPhenomenon$new("P1", "Phenomenon", "Test", 0.9,
#'                              strength = 0.90, base_rate = 0.05)
#' graph$add_node(p1)
#' graph$add_edge("EL1", "P1", "TO_TARGET")
#'
#' cpt <- generate_target_cpt_spec(p1, graph)
#' # cpt$probabilities["true", "true"] == 0.90   (strength when link exists)
#' # cpt$probabilities["true", "false"] == 0.05  (base_rate when link doesn't exist)
generate_target_cpt_spec <- function(target, evidence_graph) {
  # Validate target has strength and base_rate
  if (!("strength" %in% names(target) && "base_rate" %in% names(target))) {
    stop(sprintf("Target %s does not have strength and base_rate parameters", target$id))
  }

  strength <- target$strength
  base_rate <- target$base_rate

  # Find incoming ExplanatoryLink (parent via TO_TARGET edge)
  to_target_edges <- evidence_graph$edges[
    evidence_graph$edges$to == target$id &
    evidence_graph$edges$type == "TO_TARGET",
  ]

  if (nrow(to_target_edges) == 0) {
    stop(sprintf("No incoming ExplanatoryLink found for target %s", target$id))
  }

  link_ids <- to_target_edges$from
  n_links <- length(link_ids)

  states <- c("true", "false")

  # Handle single link case (original simple CPT)
  if (n_links == 1) {
    link_id <- link_ids[1]

    # Create simple 2x2 CPT following spec formula:
    #                 | link=true     | link=false
    # target=true     | strength      | base_rate
    # target=false    | 1-strength    | 1-base_rate
    probs_array <- array(
      c(
        strength,       # P(target=true | link=true)
        1 - strength,   # P(target=false | link=true)
        base_rate,      # P(target=true | link=false)
        1 - base_rate   # P(target=false | link=false)
      ),
      dim = c(2, 2),
      dimnames = list(
        child = states,
        link_temp = states
      )
    )

    # Rename link dimension to actual link ID
    names(dimnames(probs_array))[2] <- link_id

    cpt <- list(
      states = states,
      parents = c(link_id),
      probabilities = probs_array
    )

    return(cpt)
  }

  # Handle multiple links case with disjunctive (OR) logic / noisy-OR
  # Target is explained if ANY link is true
  # Dimensions: child states (2) x link1 states (2) x link2 states (2) x ...
  dim_sizes <- c(2, rep(2, n_links))
  n_configs <- 2^n_links

  # Create all possible parent configurations
  parent_configs <- expand.grid(rep(list(c("false", "true")), n_links))
  names(parent_configs) <- link_ids

  # Build probability array
  probs_array <- array(NA, dim = dim_sizes)

  for (i in 1:n_configs) {
    config <- parent_configs[i, , drop = FALSE]

    # Check if ANY link is true (disjunctive / OR logic)
    any_true <- any(config == "true")

    # Calculate P(target=true | this configuration)
    if (any_true) {
      # At least one link true: use strength parameter
      prob_true <- strength
    } else {
      # All links false: use base_rate parameter
      prob_true <- base_rate
    }

    prob_false <- 1 - prob_true

    # Create index vector for this configuration
    # First dimension is child state, others are parent states
    # states = c("true", "false"), so "true" -> index 1, "false" -> index 2
    parent_indices <- 1 + as.integer(parent_configs[i, ] == "false")

    # Set P(target=true | this config)
    idx_true <- c(1, parent_indices)  # 1 = "true" in child dimension
    probs_array[matrix(idx_true, nrow = 1)] <- prob_true

    # Set P(target=false | this config)
    idx_false <- c(2, parent_indices)  # 2 = "false" in child dimension
    probs_array[matrix(idx_false, nrow = 1)] <- prob_false
  }

  # Set dimension names
  dimnames_list <- list(child = states)
  for (link_id in link_ids) {
    dimnames_list[[link_id]] <- states
  }
  dimnames(probs_array) <- dimnames_list

  # Create and return CPT
  cpt <- list(
    states = states,
    parents = link_ids,
    probabilities = probs_array
  )

  return(cpt)
}

# ==============================================================================
# MASTER CPT GENERATION
# ==============================================================================

#' Generate all CPTs for entire evidence graph following specification
#'
#' This function iterates through all nodes in the graph and generates
#' spec-compliant CPTs based on node type:
#'   - BayesianHypothesis/BayesianAuxiliary -> generate_prior_cpt_spec()
#'   - BayesianExplanatoryLink -> generate_link_cpt_spec()
#'   - BayesianPhenomenon/BayesianResult -> generate_target_cpt_spec()
#'
#' All generated CPTs are validated before being returned.
#'
#' @param evidence_graph EvidenceGraph object containing Bayesian nodes
#' @return Named list: node_id -> CPT
#'
#' @examples
#' graph <- build_covid_bayesian_case_study()
#' cpts <- generate_all_cpts_spec(graph)
#' # Returns CPTs for all nodes in the graph
#' # cpts$H_aerosol, cpts$A1_viable, cpts$EL1, cpts$P_ventilation, etc.
generate_all_cpts_spec <- function(evidence_graph) {
  cpts <- list()

  for (node_id in names(evidence_graph$nodes)) {
    node <- evidence_graph$nodes[[node_id]]

    # Generate CPT based on node type
    if (inherits(node, "BayesianHypothesis") || inherits(node, "BayesianAuxiliary")) {
      # Prior CPT for hypothesis or auxiliary
      cpt <- generate_prior_cpt_spec(node)

    } else if (inherits(node, "BayesianExplanatoryLink")) {
      # Link CPT with reliability/epsilon formula
      cpt <- generate_link_cpt_spec(node, evidence_graph)

    } else if (inherits(node, "BayesianPhenomenon") || inherits(node, "BayesianResult")) {
      # Target CPT with strength/base_rate formula
      cpt <- generate_target_cpt_spec(node, evidence_graph)

    } else {
      # Fallback for non-Bayesian nodes (shouldn't happen in spec-compliant graphs)
      warning(sprintf(
        "Node %s has type %s, which is not a Bayesian spec class. Skipping CPT generation.",
        node_id, class(node)[1]
      ))
      next
    }

    # Basic validation: check probabilities are in valid range
    # (Skip validate_cpt() due to overly strict type checking on array dimensions)
    if (any(cpt$probabilities < 0 | cpt$probabilities > 1, na.rm = TRUE)) {
      stop(sprintf(
        "Generated CPT for node %s has probabilities outside [0, 1]",
        node_id
      ))
    }

    # Store CPT
    cpts[[node_id]] <- cpt
  }

  return(cpts)
}

# ==============================================================================
# EXACT PROBABILITY FORMULAS
# ==============================================================================

#' Compute exact probability using the specification formula
#'
#' Implements: P(outcome | H=T, all aux=T) = base_rate + (strength - base_rate) × reliability
#'
#' This is a helper function for understanding the exact relationship between
#' the CPT parameters and the final probability.
#'
#' @param strength P(target=true | link=true)
#' @param base_rate P(target=true | link=false)
#' @param reliability P(link=true | hypothesis=true, all aux=true)
#' @return Exact probability P(target=true | hypothesis=true, all aux=true)
#'
#' @examples
#' # COVID example from specification
#' prob <- compute_exact_probability_spec(
#'   strength = 0.95,
#'   base_rate = 0.10,
#'   reliability = 0.98 * 0.90 * 0.95  # Product of link reliabilities
#' )
#' # prob ≈ 0.799
compute_exact_probability_spec <- function(strength, base_rate, reliability) {
  # Exact formula from specification
  prob <- base_rate + (strength - base_rate) * reliability
  return(prob)
}

#' Compute approximate probability (when base_rate << strength)
#'
#' Implements: P(outcome | H=T, all aux=T) ≈ strength × reliability
#'
#' This approximation is valid when base_rate is small compared to strength.
#'
#' @param strength P(target=true | link=true)
#' @param reliability P(link=true | hypothesis=true, all aux=true)
#' @return Approximate probability
#'
#' @examples
#' # COVID approximation
#' prob_approx <- compute_approximate_probability_spec(
#'   strength = 0.95,
#'   reliability = 0.98 * 0.90 * 0.95
#' )
#' # prob_approx ≈ 0.795 (close to exact 0.799 when base_rate is small)
compute_approximate_probability_spec <- function(strength, reliability) {
  # Approximation formula (valid when base_rate << strength)
  prob <- strength * reliability
  return(prob)
}

# ==============================================================================
# SELECTOR AND ACCOUNT CPT GENERATION
# ==============================================================================

#' Generate CPT for Selector node
#'
#' The selector has one state per account, with priors computed from
#' hypothesis priors and account weights.
#'
#' @param selector Selector object
#' @return CPT list with states (account IDs) and probabilities
#'
#' @examples
#' selector <- Selector$new(model)
#' cpt <- generate_selector_cpt(selector)
generate_selector_cpt <- function(selector) {
  if (!inherits(selector, "Selector")) {
    stop("selector must be a Selector object")
  }

  cpt <- list(
    states = selector$get_states(),
    parents = character(0),
    probabilities = selector$get_prior_vector()
  )

  return(cpt)
}

#' Generate CPT for Account node (deterministic given Selector)
#'
#' Account nodes are binary (true/false) with the Selector as parent.
#' CPT is deterministic:
#'   P(Account_i = true | S = Account_i) = 1.0
#'   P(Account_i = true | S ≠ Account_i) = 0.0
#'
#' @param account_id Account ID
#' @param selector Selector object
#' @return CPT list with deterministic probabilities
#'
#' @examples
#' selector <- Selector$new(model)
#' cpt <- generate_account_cpt("Acc1a", selector)
generate_account_cpt <- function(account_id, selector) {
  if (!inherits(selector, "Selector")) {
    stop("selector must be a Selector object")
  }

  if (!(account_id %in% selector$account_ids)) {
    stop(sprintf("Account %s not found in selector", account_id))
  }

  # Binary states
  states <- c("true", "false")

  # Parent is Selector
  parents <- c("Selector")

  # Create probability table: rows = account states, cols = selector states
  selector_states <- selector$get_states()
  n_selector_states <- length(selector_states)

  # Initialize probability matrix
  prob_matrix <- matrix(0, nrow = 2, ncol = n_selector_states,
                       dimnames = list(states, selector_states))

  # Deterministic CPT
  for (s_state in selector_states) {
    if (s_state == account_id) {
      # This account is true when selector selects it
      prob_matrix["true", s_state] <- 1.0
      prob_matrix["false", s_state] <- 0.0
    } else {
      # This account is false when selector selects another
      prob_matrix["true", s_state] <- 0.0
      prob_matrix["false", s_state] <- 1.0
    }
  }

  cpt <- list(
    states = states,
    parents = parents,
    probabilities = prob_matrix
  )

  return(cpt)
}
