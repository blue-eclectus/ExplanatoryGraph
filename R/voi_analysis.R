# Value of Information Analysis for Explanatory Accounts
#
# This module implements Expected Value of Perfect Information (EVPI) analysis
# to help researchers identify which uncertain nodes would be most valuable
# to investigate.
#
# Key functions:
#   - compute_entropy(): Shannon entropy of a probability distribution
#   - compute_tvd(): Total Variation Distance between distributions
#   - marginalize_to_hypotheses(): Sum account posteriors to hypothesis posteriors
#   - compute_evpi(): EVPI for a single candidate node
#   - compute_all_evpi(): EVPI for all candidate nodes


#' Compute Shannon entropy of a probability distribution
#'
#' @param probs Numeric vector of probabilities (must sum to 1)
#' @param base Logarithm base (default: 2 for bits)
#' @return Numeric entropy value in bits (if base=2)
#'
#' @details
#' Entropy measures uncertainty in a probability distribution:
#'   H(P) = -sum(p_i * log(p_i))
#'
#' Handles edge cases:
#' - Returns 0 for deterministic distributions (any p = 1)
#' - Treats p = 0 as contributing 0 to entropy (limit of p*log(p) as p->0)
#'
#' @examples
#' compute_entropy(c(0.5, 0.5))        # = 1.0 bit
#' compute_entropy(c(0.9, 0.1))        # ≈ 0.469 bits
#' compute_entropy(c(1.0, 0.0))        # = 0 bits
#' compute_entropy(c(0.25, 0.25, 0.25, 0.25))  # = 2.0 bits
compute_entropy <- function(probs, base = 2) {
  # Validate input
  if (!is.numeric(probs)) {
    stop("probs must be a numeric vector")
  }
  if (any(probs < 0)) {
    stop("probabilities cannot be negative")
  }
  if (abs(sum(probs) - 1) > 1e-6) {
    stop("probabilities must sum to 1")
  }


  # Handle edge cases: p=0 contributes 0 to entropy
  # (limit of p*log(p) as p->0 is 0)
  probs <- probs[probs > 0]

  # If only one non-zero probability, entropy is 0

  if (length(probs) <= 1) {
    return(0)
  }

  # Compute entropy: -sum(p * log(p))
  entropy <- -sum(probs * log(probs, base = base))

  return(entropy)
}


#' Compute Total Variation Distance between two probability distributions
#'
#' @param p1 Numeric vector of probabilities (must sum to 1)
#' @param p2 Numeric vector of probabilities (must sum to 1)
#' @return Numeric TVD value in [0, 1]
#'
#' @details
#' Total Variation Distance is the standard measure of distance between
#' probability distributions:
#'   TVD(p1, p2) = (1/2) * sum(|p1_i - p2_i|)
#'
#' Properties:
#' - Bounded by [0, 1]
#' - TVD = 0 means identical distributions
#' - TVD = 1 means completely non-overlapping support
#' - Symmetric: TVD(p, q) = TVD(q, p)
#'
#' @examples
#' compute_tvd(c(0.6, 0.4), c(0.6, 0.4))   # = 0 (identical)
#' compute_tvd(c(0.6, 0.4), c(0.75, 0.25)) # = 0.15
#' compute_tvd(c(1.0, 0.0), c(0.0, 1.0))   # = 1 (completely different)
compute_tvd <- function(p1, p2) {
  # Validate inputs
  if (!is.numeric(p1) || !is.numeric(p2)) {
    stop("p1 and p2 must be numeric vectors")
  }
  if (length(p1) != length(p2)) {
    stop("p1 and p2 must have the same length")
  }
  if (abs(sum(p1) - 1) > 1e-6 || abs(sum(p2) - 1) > 1e-6) {
    stop("probabilities must sum to 1")
  }

  # TVD = (1/2) * sum(|p1 - p2|)
  tvd <- 0.5 * sum(abs(p1 - p2))

  return(tvd)
}


#' Marginalize account posteriors to hypothesis posteriors
#'
#' Each account is associated with exactly one hypothesis. This function
#' sums the probabilities of all accounts associated with each hypothesis
#' to produce marginal hypothesis probabilities.
#'
#' @param selector_marginal Named vector of account probabilities (must sum to 1)
#' @param model ExplanatoryModel object (contains account-to-hypothesis mapping)
#'
#' @return Named vector of hypothesis probabilities
#'
#' @details
#' For hypothesis H_i with associated accounts {A_i1, A_i2, ...}:
#'   P(H_i) = sum_j P(A_ij)
#'
#' @examples
#' # If H1 has accounts H1_weak and H1_strong:
#' # P(H1) = P(H1_weak) + P(H1_strong)
marginalize_to_hypotheses <- function(selector_marginal, model) {
  # Validate inputs
  if (!is.numeric(selector_marginal)) {
    stop("selector_marginal must be a numeric vector")
  }
  if (is.null(names(selector_marginal))) {
    stop("selector_marginal must be a named vector with account IDs")
  }

  # Get all hypothesis IDs
  hypothesis_ids <- names(model$hypotheses)

  if (length(hypothesis_ids) == 0) {
    stop("model has no hypotheses")
  }

  # Initialize result vector
  hypothesis_probs <- setNames(rep(0, length(hypothesis_ids)), hypothesis_ids)

  # Sum account probabilities for each hypothesis
  for (account_id in names(selector_marginal)) {
    account <- model$accounts[[account_id]]
    if (is.null(account)) {
      warning(sprintf("Account '%s' not found in model", account_id))
      next
    }

    hyp_id <- account$hypothesis_id
    if (hyp_id %in% hypothesis_ids) {
      hypothesis_probs[hyp_id] <- hypothesis_probs[hyp_id] + selector_marginal[account_id]
    }
  }

  # Validate result sums to 1 (within tolerance)
  if (abs(sum(hypothesis_probs) - 1) > 1e-6) {
    warning(sprintf("Marginalized hypothesis probabilities sum to %.6f, not 1",
                    sum(hypothesis_probs)))
  }

  return(hypothesis_probs)
}


#' Compute Expected Value of Perfect Information for a single node
#'
#' EVPI quantifies the expected reduction in uncertainty about a target
#' (hypotheses or accounts) if we were to learn the true state of a candidate node.
#'
#' @param node_id ID of the candidate node
#' @param target Character: "hypotheses" or "accounts"
#' @param model ExplanatoryModel object
#' @param bayesian_network EvidenceGraph object (from build_bayesian_network)
#' @param cpts List of CPTs (from generate_all_cpts_spec)
#' @param base_evidence Named list of evidence for starting point (e.g., list(R1="true"))
#' @param base_posteriors Pre-computed posteriors at base_evidence (optional, for efficiency)
#'
#' @return List containing:
#'   - evpi: Expected value of perfect information (bits)
#'   - evpi_pct: EVPI as percentage of current entropy
#'   - shift: Expected posterior shift magnitude (Total Variation Distance)
#'   - max_gain: Maximum possible information gain
#'   - asymmetry: Information asymmetry between outcomes
#'   - p_true: Current probability node is TRUE
#'   - entropy_current: Current target entropy
#'   - entropy_if_true: Target entropy if node = TRUE
#'   - entropy_if_false: Target entropy if node = FALSE
#'   - posteriors_current: Current target distribution
#'   - posteriors_if_true: Target distribution if node = TRUE
#'   - posteriors_if_false: Target distribution if node = FALSE
compute_evpi <- function(node_id, target, model, bayesian_network, cpts,
                         base_evidence = list(), base_posteriors = NULL) {

  # Validate target
  if (!target %in% c("hypotheses", "accounts")) {
    stop("target must be 'hypotheses' or 'accounts'")
  }

  # Compute base posteriors if not provided
  if (is.null(base_posteriors)) {
    base_posteriors <- compute_posteriors(bayesian_network, cpts, base_evidence)
  }

  # Get current target distribution
  if (target == "accounts") {
    # Use Selector marginal directly
    posteriors_current <- base_posteriors$Selector
  } else {
    # Marginalize to hypotheses
    posteriors_current <- marginalize_to_hypotheses(base_posteriors$Selector, model)
  }

  # Current entropy
  entropy_current <- compute_entropy(posteriors_current)

  # Get current probability of node being TRUE
  node_marginal <- base_posteriors[[node_id]]
  if (is.null(node_marginal)) {
    stop(sprintf("Node '%s' not found in posteriors", node_id))
  }
  p_true <- node_marginal["true"]
  p_false <- node_marginal["false"]

  # Compute posteriors if node = TRUE
  evidence_true <- base_evidence
  evidence_true[[node_id]] <- "true"
  posteriors_true_full <- compute_posteriors(bayesian_network, cpts, evidence_true)

  if (target == "accounts") {
    posteriors_if_true <- posteriors_true_full$Selector
  } else {
    posteriors_if_true <- marginalize_to_hypotheses(posteriors_true_full$Selector, model)
  }
  entropy_if_true <- compute_entropy(posteriors_if_true)

  # Compute posteriors if node = FALSE
  evidence_false <- base_evidence
  evidence_false[[node_id]] <- "false"
  posteriors_false_full <- compute_posteriors(bayesian_network, cpts, evidence_false)

  if (target == "accounts") {
    posteriors_if_false <- posteriors_false_full$Selector
  } else {
    posteriors_if_false <- marginalize_to_hypotheses(posteriors_false_full$Selector, model)
  }
  entropy_if_false <- compute_entropy(posteriors_if_false)

  # Compute EVPI: H_current - E[H | X]
  expected_entropy <- p_true * entropy_if_true + p_false * entropy_if_false
  evpi <- entropy_current - expected_entropy

  # Compute EVPI as percentage of current entropy
  if (entropy_current > 0) {
    evpi_pct <- (evpi / entropy_current) * 100
  } else {
    evpi_pct <- 0
  }

  # Compute posterior shift (expected TVD)
  tvd_if_true <- compute_tvd(posteriors_if_true, posteriors_current)
  tvd_if_false <- compute_tvd(posteriors_if_false, posteriors_current)
  shift <- p_true * tvd_if_true + p_false * tvd_if_false

  # Compute max gain: best-case entropy reduction
  max_gain <- entropy_current - min(entropy_if_true, entropy_if_false)

  # Compute asymmetry: difference in entropy between outcomes
  asymmetry <- abs(entropy_if_true - entropy_if_false)

  return(list(
    evpi = evpi,
    evpi_pct = evpi_pct,
    shift = shift,
    max_gain = max_gain,
    asymmetry = asymmetry,
    p_true = as.numeric(p_true),
    entropy_current = entropy_current,
    entropy_if_true = entropy_if_true,
    entropy_if_false = entropy_if_false,
    posteriors_current = posteriors_current,
    posteriors_if_true = posteriors_if_true,
    posteriors_if_false = posteriors_if_false
  ))
}


#' Get candidate nodes for EVPI analysis
#'
#' Returns IDs of nodes that can be investigated (not already conditioned as evidence).
#'
#' @param model ExplanatoryModel object
#' @param bayesian_network EvidenceGraph object
#' @param base_evidence Named list of current evidence
#' @param include_auxiliaries Logical: include auxiliary nodes
#' @param include_phenomena Logical: include phenomenon nodes
#' @param include_links Logical: include explanatory link nodes
#'
#' @return Data frame with columns: node_id, node_type, node_description
get_candidate_nodes <- function(model, bayesian_network, base_evidence = list(),
                                include_auxiliaries = TRUE,
                                include_phenomena = TRUE,
                                include_links = TRUE) {

  # Get evidence node IDs

  evidence_ids <- names(base_evidence)

  candidates <- data.frame(
    node_id = character(),
    node_type = character(),
    node_description = character(),
    stringsAsFactors = FALSE
  )

  # Add auxiliaries
  if (include_auxiliaries && length(model$auxiliaries) > 0) {
    for (aux_id in names(model$auxiliaries)) {
      if (!aux_id %in% evidence_ids) {
        aux <- model$auxiliaries[[aux_id]]
        candidates <- rbind(candidates, data.frame(
          node_id = aux_id,
          node_type = "Auxiliary",
          node_description = aux$text,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Add phenomena
  if (include_phenomena && length(model$phenomena) > 0) {
    for (phen_id in names(model$phenomena)) {
      if (!phen_id %in% evidence_ids) {
        phen <- model$phenomena[[phen_id]]
        candidates <- rbind(candidates, data.frame(
          node_id = phen_id,
          node_type = "Phenomenon",
          node_description = phen$text,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Add explanatory links
  if (include_links && length(model$explanatory_links) > 0) {
    for (link_id in names(model$explanatory_links)) {
      if (!link_id %in% evidence_ids) {
        link <- model$explanatory_links[[link_id]]
        candidates <- rbind(candidates, data.frame(
          node_id = link_id,
          node_type = "ExplanatoryLink",
          node_description = link$text,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  return(candidates)
}


#' Compute EVPI for all candidate nodes
#'
#' @param model ExplanatoryModel object
#' @param bayesian_network EvidenceGraph object (from build_bayesian_network)
#' @param cpts List of CPTs (from generate_all_cpts_spec)
#' @param base_evidence Named list of evidence for starting point
#' @param target Character: "hypotheses" or "accounts"
#' @param include_auxiliaries Logical: include auxiliary nodes
#' @param include_phenomena Logical: include phenomenon nodes
#' @param include_links Logical: include explanatory link nodes
#' @param progress_callback Function called with (current, total, node_id) for progress updates
#'
#' @return List containing:
#'   - metadata: list with timestamp, target, base_evidence, entropy_current, n_candidates, computation_time
#'   - results: data frame sorted by EVPI descending
#'   - details: list keyed by node_id with posteriors and interpretation
compute_all_evpi <- function(model, bayesian_network, cpts, base_evidence = list(),
                             target = "hypotheses",
                             include_auxiliaries = TRUE,
                             include_phenomena = TRUE,
                             include_links = TRUE,
                             progress_callback = NULL) {

  start_time <- Sys.time()

  # Validate target
  if (!target %in% c("hypotheses", "accounts")) {
    stop("target must be 'hypotheses' or 'accounts'")
  }

  # Get candidate nodes
  candidates <- get_candidate_nodes(
    model, bayesian_network, base_evidence,
    include_auxiliaries, include_phenomena, include_links
  )

  n_candidates <- nrow(candidates)

  # Handle empty case
  if (n_candidates == 0) {
    return(list(
      metadata = list(
        timestamp = Sys.time(),
        target = target,
        base_evidence = base_evidence,
        entropy_current = NA,
        n_candidates = 0,
        computation_time_seconds = 0
      ),
      results = data.frame(
        node_id = character(),
        node_type = character(),
        node_description = character(),
        evpi = numeric(),
        evpi_pct = numeric(),
        shift = numeric(),
        max_gain = numeric(),
        asymmetry = numeric(),
        p_true = numeric(),
        entropy_if_true = numeric(),
        entropy_if_false = numeric(),
        stringsAsFactors = FALSE
      ),
      details = list()
    ))
  }

  # Compute base posteriors once (for efficiency)
  base_posteriors <- compute_posteriors(bayesian_network, cpts, base_evidence)

  # Get current entropy
  if (target == "accounts") {
    posteriors_current <- base_posteriors$Selector
  } else {
    posteriors_current <- marginalize_to_hypotheses(base_posteriors$Selector, model)
  }
  entropy_current <- compute_entropy(posteriors_current)

  # Initialize results
  results <- data.frame(
    node_id = character(),
    node_type = character(),
    node_description = character(),
    evpi = numeric(),
    evpi_pct = numeric(),
    shift = numeric(),
    max_gain = numeric(),
    asymmetry = numeric(),
    p_true = numeric(),
    entropy_if_true = numeric(),
    entropy_if_false = numeric(),
    stringsAsFactors = FALSE
  )

  details <- list()

  # Compute EVPI for each candidate
  for (i in seq_len(n_candidates)) {
    node_id <- candidates$node_id[i]
    node_type <- candidates$node_type[i]
    node_description <- candidates$node_description[i]

    # Progress callback
    if (!is.null(progress_callback)) {
      progress_callback(i, n_candidates, node_id)
    }

    # Compute EVPI for this node
    evpi_result <- compute_evpi(
      node_id = node_id,
      target = target,
      model = model,
      bayesian_network = bayesian_network,
      cpts = cpts,
      base_evidence = base_evidence,
      base_posteriors = base_posteriors
    )

    # Add to results
    results <- rbind(results, data.frame(
      node_id = node_id,
      node_type = node_type,
      node_description = node_description,
      evpi = evpi_result$evpi,
      evpi_pct = evpi_result$evpi_pct,
      shift = evpi_result$shift,
      max_gain = evpi_result$max_gain,
      asymmetry = evpi_result$asymmetry,
      p_true = evpi_result$p_true,
      entropy_if_true = evpi_result$entropy_if_true,
      entropy_if_false = evpi_result$entropy_if_false,
      stringsAsFactors = FALSE
    ))

    # Store detailed results
    details[[node_id]] <- list(
      posteriors_if_true = evpi_result$posteriors_if_true,
      posteriors_if_false = evpi_result$posteriors_if_false,
      interpretation = generate_evpi_interpretation(
        evpi_result, node_description, posteriors_current
      )
    )
  }

  # Sort by EVPI descending
  results <- results[order(results$evpi, decreasing = TRUE), ]
  rownames(results) <- NULL

  end_time <- Sys.time()
  computation_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  return(list(
    metadata = list(
      timestamp = end_time,
      target = target,
      base_evidence = base_evidence,
      posteriors_current = posteriors_current,
      entropy_current = entropy_current,
      n_candidates = n_candidates,
      computation_time_seconds = computation_time
    ),
    results = results,
    details = details
  ))
}


#' Generate natural language interpretation of EVPI results
#'
#' @param evpi_result List from compute_evpi()
#' @param node_description Text description of the node
#' @param posteriors_current Current target distribution
#'
#' @return Character string with interpretation
generate_evpi_interpretation <- function(evpi_result, node_description, posteriors_current) {

  # Determine priority level based on evpi_pct
  priority <- if (evpi_result$evpi_pct >= 15) {
    "critical priority"
  } else if (evpi_result$evpi_pct >= 8) {
    "high priority"
  } else if (evpi_result$evpi_pct >= 3) {
    "medium priority"
  } else {
    "low priority"
  }

  # Determine which outcome is more informative
  if (evpi_result$entropy_if_true < evpi_result$entropy_if_false) {
    more_informative <- "TRUE"
    less_informative <- "FALSE"
    more_entropy <- evpi_result$entropy_if_false
    less_entropy <- evpi_result$entropy_if_true
    more_likely <- evpi_result$p_true
  } else {
    more_informative <- "FALSE"
    less_informative <- "TRUE"
    more_entropy <- evpi_result$entropy_if_true
    less_entropy <- evpi_result$entropy_if_false
    more_likely <- 1 - evpi_result$p_true
  }

  # Build interpretation
  interpretation <- sprintf(
    "Learning the value of this node would reduce uncertainty by approximately %.1f%% (%.3f bits). ",
    evpi_result$evpi_pct, evpi_result$evpi
  )

  # Add asymmetry information if notable

  if (evpi_result$asymmetry > 0.1) {
    interpretation <- paste0(
      interpretation,
      sprintf(
        "The %s outcome would be more informative (entropy %.3f vs %.3f bits), ",
        more_informative, less_entropy, more_entropy
      )
    )

    if (more_likely < 0.3) {
      interpretation <- paste0(
        interpretation,
        sprintf("but is unlikely (%.0f%%). ", more_likely * 100)
      )
    } else if (more_likely > 0.7) {
      interpretation <- paste0(
        interpretation,
        sprintf("and is likely (%.0f%%). ", more_likely * 100)
      )
    } else {
      interpretation <- paste0(
        interpretation,
        sprintf("with probability %.0f%%. ", more_likely * 100)
      )
    }
  }

  # Add priority classification
  interpretation <- paste0(
    interpretation,
    sprintf("This is a %s investigation.", priority)
  )

  return(interpretation)
}
