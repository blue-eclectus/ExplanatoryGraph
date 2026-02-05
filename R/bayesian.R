# ==============================================================================
# Evidence Graph Framework - Bayesian Inference Module
# ==============================================================================
#
# This module provides Bayesian belief propagation using the gRain package.
# Given an evidence graph with CPTs, it computes posterior probabilities
# for all nodes conditioned on observed evidence.
#
# MIGRATION NOTE
# --------------
# Migrated from BayesNetBP to gRain (2025-11-24) to fix the orphan auxiliary
# node bug. gRain handles disconnected components correctly.
#
# KEY FUNCTIONS
# -------------
# compute_posteriors()
#   Main inference function. Takes an EvidenceGraph, CPTs, and evidence;
#   returns posterior distributions for all nodes.
#
# sensitivity_analysis()
#   Varies a CPT parameter across a range and observes the effect on posteriors.
#   Useful for understanding which assumptions most affect conclusions.
#
# compute_coherence_score()
#   Quantitative measure of evidential support: P(H|E) - P(H)
#
# WORKFLOW
# --------
# 1. Build EvidenceGraph from ExplanatoryModel
# 2. Generate CPTs with generate_all_cpts_spec()
# 3. Set evidence (e.g., list(R1 = "true"))
# 4. Call compute_posteriors() to get all marginals
#
# DEPENDENCIES
# ------------
# - gRain (Bioconductor): Belief propagation engine
# - R/conversion.R: EvidenceGraph → graphNEL conversion
# - R/cpt_utils.R: CPT validation and manipulation

# Check if gRain is available
if (!requireNamespace("gRain", quietly = TRUE)) {
  stop("Package 'gRain' is required for Bayesian inference. Install with:\n",
       "install.packages('gRain')")
}

library(gRain)

# Source required modules (for development mode)
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  source("R/conversion.R")
  source("R/cpt_utils.R")
}

# ==============================================================================
# CPT FORMAT CONVERSION
# ==============================================================================

#' Convert internal CPT format to gRain cptable format
#'
#' Our CPT format: list(states, parents, probabilities)
#' gRain format: cptable(~Node | Parent1 + Parent2, values, levels)
#'
#' @param cpt_assignments Named list: node_id → CPT
#' @param valid_node_ids Character vector of all node IDs in the network
#' @return List of gRain cptable objects ready for compileCPT()
convert_cpts_to_grain_format <- function(cpt_assignments, valid_node_ids) {
  cptables <- list()

  for (node_id in valid_node_ids) {
    cpt <- cpt_assignments[[node_id]]

    states <- cpt$states
    parents <- cpt$parents
    probs <- cpt$probabilities

    if (length(parents) == 0) {
      # No parents: simple prior
      # Create formula: ~NodeID
      formula_str <- paste0("~", node_id)

      # Extract probabilities as vector
      prob_vec <- as.vector(probs[states])

      # Create cptable
      cptables[[node_id]] <- cptable(
        vpar = as.formula(formula_str),
        values = prob_vec,
        levels = states
      )

    } else {
      # Has parents: conditional table
      # Create formula: ~NodeID | Parent1 + Parent2 + ...
      formula_str <- paste0("~", node_id, "|", paste(parents, collapse = "+"))

      # Convert array to vector (child varies fastest, which matches gRain)
      # Our arrays are already in this format, so as.vector() works perfectly
      prob_vec <- as.vector(probs)

      # Create cptable
      cptables[[node_id]] <- cptable(
        vpar = as.formula(formula_str),
        values = prob_vec,
        levels = states
      )
    }
  }

  return(cptables)
}

# ==============================================================================
# MAIN INFERENCE FUNCTION
# ==============================================================================

#' Compute posterior probabilities for all nodes given evidence
#'
#' Main inference entry point. Converts the evidence graph to gRain format,
#' sets evidence, and returns posterior distributions for all nodes.
#'
#' ALGORITHM:
#' 1. Convert EvidenceGraph to graphNEL (filtering eliminated links)
#' 2. Validate all nodes have CPTs
#' 3. Convert CPTs to gRain cptable format
#' 4. Compile network and run belief propagation
#' 5. Extract marginal distributions
#'
#' @param evidence_graph EvidenceGraph object
#' @param cpt_assignments Named list: node_id → CPT
#' @param evidence_nodes Named list: node_id → observed state (e.g., list(R1 = "true"))
#' @return Named list: node_id → posterior distribution vector
#'
#' @examples
#' graph <- build_bayesian_network(model)
#' cpts <- generate_all_cpts_spec(graph)
#' posteriors <- compute_posteriors(graph, cpts, list(R1 = "true", R2 = "true"))
#' posteriors$H1["true"]  # P(H1 | evidence)
compute_posteriors <- function(evidence_graph, cpt_assignments, evidence_nodes = list()) {
  # Step 1: Convert EvidenceGraph to graphNEL
  conversion <- evidence_graph_to_graphNEL(evidence_graph)
  g_nel <- conversion$graph_nel
  node_mapping <- conversion$node_mapping
  valid_node_ids <- node_mapping$all_ids

  # Step 2: Validate all nodes have CPTs
  missing_cpts <- character()
  for (node_id in valid_node_ids) {
    if (!(node_id %in% names(cpt_assignments))) {
      missing_cpts <- c(missing_cpts, node_id)
    }
  }

  if (length(missing_cpts) > 0) {
    stop("Missing CPTs for nodes: ", paste(missing_cpts, collapse = ", "),
         "\nUse evidence_graph$auto_generate_cpts() or specify CPTs manually")
  }

  # Step 3: Run belief propagation using gRain
  tryCatch({
    # gRain API workflow:
    # 1. Convert CPTs to gRain cptable format
    # 2. Compile CPTs and create grain network
    # 3. Set evidence (if any)
    # 4. Query marginals (propagation happens automatically)

    # Step 3a: Convert our CPTs to gRain cptable format
    cptables <- convert_cpts_to_grain_format(cpt_assignments, valid_node_ids)

    # Step 3b: Compile CPTs and create grain network
    compiled_cpts <- compileCPT(cptables)
    grain_net <- grain(compiled_cpts)

    # Step 3c: Set evidence if any
    if (length(evidence_nodes) > 0) {
      # Validate evidence nodes
      evidence_vars <- character()
      evidence_values <- character()

      for (evidence_id in names(evidence_nodes)) {
        if (!(evidence_id %in% valid_node_ids)) {
          warning("Evidence node '", evidence_id, "' not in valid node set")
          next
        }
        evidence_vars <- c(evidence_vars, evidence_id)
        evidence_values <- c(evidence_values, evidence_nodes[[evidence_id]])
      }

      if (length(evidence_vars) > 0) {
        # Set evidence in grain network
        grain_net <- setEvidence(
          object = grain_net,
          nodes = evidence_vars,
          states = evidence_values
        )
      }
    }

    # Step 3d: Query marginals for all nodes
    # For non-evidence nodes, query their posteriors
    # For evidence nodes, create deterministic distributions
    evidence_node_ids <- names(evidence_nodes)
    query_node_ids <- setdiff(valid_node_ids, evidence_node_ids)

    posteriors <- list()

    # Get marginals for non-evidence nodes
    if (length(query_node_ids) > 0) {
      # querygrain returns a list of marginals
      marginals_result <- querygrain(
        object = grain_net,
        nodes = query_node_ids,
        type = "marginal"
      )

      # gRain returns marginals as vectors (or arrays for single node)
      # Ensure consistent format
      for (node_id in query_node_ids) {
        marginal <- marginals_result[[node_id]]

        # Convert to named vector if needed
        if (is.array(marginal) && length(dim(marginal)) == 1) {
          marginal <- as.vector(marginal)
        }

        # Ensure proper state names
        cpt <- cpt_assignments[[node_id]]
        names(marginal) <- cpt$states

        posteriors[[node_id]] <- marginal
      }
    }

    # For evidence nodes, posterior is deterministic: P(observed_state) = 1.0
    for (evidence_id in evidence_node_ids) {
      if (evidence_id %in% valid_node_ids) {
        observed_state <- evidence_nodes[[evidence_id]]
        cpt <- cpt_assignments[[evidence_id]]
        states <- cpt$states

        # Create deterministic distribution
        marginal <- rep(0.0, length(states))
        names(marginal) <- states
        marginal[observed_state] <- 1.0

        posteriors[[evidence_id]] <- marginal
      }
    }

    return(posteriors)

  }, error = function(e) {
    # Capture full error details for debugging
    error_msg <- paste0(
      "Belief propagation failed: ", e$message, "\n",
      "Error class: ", paste(class(e), collapse = ", "), "\n",
      "This may indicate:\n",
      "  - Invalid CPT structure\n",
      "  - Cycles in the graph (not a DAG)\n",
      "  - Numerical instability\n",
      "  - Empty or malformed data structures\n",
      "Check CPTs with validate_cpt() and graph structure with validate_dag()\n\n",
      "Call stack:\n",
      paste(capture.output(traceback()), collapse = "\n")
    )
    stop(error_msg)
  })
}

# ==============================================================================
# SENSITIVITY ANALYSIS
# ==============================================================================

#' Vary a CPT parameter and observe effect on posteriors
#'
#' Tests how sensitive conclusions are to specific probability assumptions.
#' Useful for identifying which parameters most affect the analysis.
#'
#' @param evidence_graph EvidenceGraph object
#' @param cpt_assignments Named list of CPTs
#' @param evidence_nodes Named list: node_id → observed state
#' @param vary_node Node ID whose CPT to vary
#' @param vary_param Parameter name (currently modifies all conditionals uniformly)
#' @param vary_range Sequence of values (e.g., seq(0.1, 0.9, 0.1))
#' @return Data.frame with columns: param_value, node_id, state, posterior_prob
sensitivity_analysis <- function(evidence_graph, cpt_assignments,
                                 evidence_nodes, vary_node,
                                 vary_param, vary_range) {
  results <- data.frame(
    param_value = numeric(),
    node_id = character(),
    state = character(),
    posterior_prob = numeric(),
    stringsAsFactors = FALSE
  )

  # Save original CPT
  original_cpt <- cpt_assignments[[vary_node]]

  for (value in vary_range) {
    # Modify CPT
    # For simplicity, assume we're varying P(node=true|parents) uniformly
    # More sophisticated parameter selection could be added
    modified_cpt <- modify_cpt_parameter(original_cpt, vary_param, value)
    cpt_assignments[[vary_node]] <- modified_cpt

    # Compute posteriors with modified CPT
    posteriors <- compute_posteriors(evidence_graph, cpt_assignments, evidence_nodes)

    # Record results
    for (node_id in names(posteriors)) {
      p <- posteriors[[node_id]]
      for (state in names(p)) {
        results <- rbind(results, data.frame(
          param_value = value,
          node_id = node_id,
          state = state,
          posterior_prob = p[state],
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Restore original CPT
  cpt_assignments[[vary_node]] <- original_cpt

  return(results)
}

#' Modify a specific parameter in a CPT (helper for sensitivity analysis)
#'
#' Simplified implementation that uniformly adjusts P(child=true|parents).
#' For more targeted modifications, create CPTs manually.
#'
#' @param cpt Original CPT
#' @param param_name Parameter to modify (currently ignored; modifies all)
#' @param new_value New probability value
#' @return Modified CPT (normalized)
modify_cpt_parameter <- function(cpt, param_name, new_value) {
  # For simplicity, we'll modify all P(child=true|parents) uniformly
  # More sophisticated parameter selection could be added based on param_name

  probs <- cpt$probabilities
  n_parents <- length(cpt$parents)

  if (n_parents == 0) {
    # No parents: modify prior
    probs["true"] <- new_value
    probs["false"] <- 1 - new_value
  } else {
    # Has parents: modify conditional probabilities
    # For each parent configuration, set P(true) = new_value
    dims <- dim(probs)

    if (length(dims) > 1) {
      # Multi-dimensional array
      # Modify first dimension (child states)
      for (i in 1:dims[1]) {
        if (i == 1) {  # Assuming index 1 = "true"
          # Set all P(true|parent configs) to new_value
          idx <- rep(list(NULL), length(dims))
          idx[[1]] <- 1
          probs <- replace(probs, do.call(`[`, c(list(probs), idx)), new_value)

          # Set P(false|parent configs) to 1 - new_value
          idx[[1]] <- 2
          probs <- replace(probs, do.call(`[`, c(list(probs), idx)), 1 - new_value)
        }
      }
    }
  }

  cpt$probabilities <- probs
  return(normalize_cpt(cpt))  # Ensure normalization
}

# ==============================================================================
# QUANTITATIVE COHERENCE SCORE
# ==============================================================================

#' Compute how strongly evidence supports a hypothesis
#'
#' Coherence score quantifies the shift from prior to posterior:
#'   coherence_score = P(H | evidence) - P(H)
#'
#' INTERPRETATION:
#' - Positive: Evidence supports hypothesis
#' - Negative: Evidence contradicts hypothesis
#' - Near zero: Evidence is uninformative
#'
#' @param posteriors Output from compute_posteriors()
#' @param hypothesis_id ID of hypothesis node
#' @param prior_prob Prior P(hypothesis=true); uses 0.5 if not provided
#' @return List with coherence_score, posterior_prob, prior_prob, interpretation
#'
#' @examples
#' posteriors <- compute_posteriors(graph, cpts, list(R1 = "true"))
#' score <- compute_coherence_score(posteriors, "H1", prior_prob = 0.5)
#' # score$coherence_score > 0 means evidence supports H1
compute_coherence_score <- function(posteriors, hypothesis_id, prior_prob = NULL) {
  if (!(hypothesis_id %in% names(posteriors))) {
    stop("Hypothesis '", hypothesis_id, "' not in posteriors")
  }

  posterior <- posteriors[[hypothesis_id]]
  posterior_prob <- posterior["true"]

  # If prior not provided, use uniform (0.5)
  if (is.null(prior_prob)) {
    prior_prob <- 0.5
    warning("Prior probability not provided, using uniform prior (0.5)")
  }

  # Compute coherence score
  coherence_score <- posterior_prob - prior_prob

  # Interpretation
  if (coherence_score > 0.3) {
    interpretation <- "Strong evidential support"
  } else if (coherence_score > 0.1) {
    interpretation <- "Moderate evidential support"
  } else if (coherence_score > -0.1) {
    interpretation <- "Evidence largely uninformative"
  } else if (coherence_score > -0.3) {
    interpretation <- "Moderate evidence against hypothesis"
  } else {
    interpretation <- "Strong evidence against hypothesis"
  }

  return(list(
    coherence_score = coherence_score,
    posterior_prob = posterior_prob,
    prior_prob = prior_prob,
    interpretation = interpretation
  ))
}

#' Extract prior probability from a CPT
#'
#' Returns P(node=true) from a node's CPT. Only valid for root nodes
#' (no parents); warns if node has conditional dependencies.
#'
#' @param cpt CPT for the node
#' @return Numeric probability for "true" state
get_prior_probability <- function(cpt) {
  if (length(cpt$parents) > 0) {
    warning("Node has parents - prior is conditional on parent states")
  }

  probs <- cpt$probabilities

  if ("true" %in% names(probs)) {
    return(probs["true"])
  } else if (is.array(probs)) {
    # If array, take first element (assuming it corresponds to P(true|...))
    return(probs[1])
  } else {
    stop("Cannot extract prior from CPT structure")
  }
}
