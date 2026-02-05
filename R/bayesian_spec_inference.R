# ==============================================================================
# Bayesian Evidence Graph - Spec-Compliant Inference
# ==============================================================================
#
# This module performs Bayesian inference over evidence graphs, computing
# posterior probabilities for hypotheses given observed evidence.
#
# KEY FUNCTIONS
# -------------
# compute_conditional_likelihood_spec()
#   Computes P(target | H, auxiliaries) using the specification formula.
#   Traces the explanatory path and multiplies conditional probabilities.
#
# compute_posteriors_with_formula_spec()
#   Full Bayesian update: computes P(H | Evidence) with detailed formula
#   breakdown for transparency and debugging.
#
# compute_coherence_score_spec()
#   Quantitative measure of evidential support:
#   Coherence = P(H | E) - P(H)
#   Positive scores indicate evidence supports the hypothesis.
#
# INFERENCE APPROACH
# ------------------
# Uses gRain's belief propagation for exact inference on the compiled
# Bayesian network. Evidence is set as hard constraints, and marginals
# are computed for all query nodes.
#
# DEPENDENCIES
# ------------
# - R/bayesian.R (compute_posteriors, gRain interface)
# - R/bayesian_spec_cpt.R (CPT generation)

# Source required modules (for development mode)
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  if (!exists("compute_posteriors")) {
    source("R/bayesian.R")
  }

  if (!exists("generate_all_cpts_spec")) {
    source("R/bayesian_spec_cpt.R")
  }
}

# ==============================================================================
# CONDITIONAL LIKELIHOOD COMPUTATION
# ==============================================================================

#' Compute exact likelihood P(target | H, auxiliaries) using specification formula
#'
#' Implements the spec formula:
#'   P(target | H=T, all aux=T) = base_rate + (strength - base_rate) × reliability
#'
#' This computes the likelihood by finding the explanatory path from hypothesis to
#' target and multiplying the conditional probabilities along the path.
#'
#' @param evidence_graph EvidenceGraph containing Bayesian nodes
#' @param cpts Named list of CPTs (from generate_all_cpts_spec)
#' @param hypothesis_id ID of hypothesis node
#' @param target_id ID of target (phenomenon or result) node
#' @param auxiliary_states Named list of auxiliary states (aux_id -> "true"/"false")
#'
#' @return List with:
#'   - likelihood_true: P(target=true | hypothesis=true, auxiliaries)
#'   - likelihood_false: P(target=true | hypothesis=false, auxiliaries)
#'   - formula_components: Breakdown of calculation for display
#'
#' @examples
#' graph <- build_covid_bayesian_case_study()
#' cpts <- generate_all_cpts_spec(graph)
#' lik <- compute_conditional_likelihood_spec(
#'   graph, cpts, "H_aerosol", "R_studies",
#'   auxiliary_states = list(A1_viable = "true", A2_concentration = "true", A3_dilutes = "true")
#' )
#' # lik$likelihood_true ≈ 0.799 (from spec example)
compute_conditional_likelihood_spec <- function(evidence_graph, cpts,
                                                hypothesis_id, target_id,
                                                auxiliary_states = list()) {
  # Find path from hypothesis to target
  path <- evidence_graph$find_explanatory_path(hypothesis_id, target_id)

  if (is.null(path) || length(path) == 0) {
    warning(sprintf("No path from %s to %s", hypothesis_id, target_id))
    return(list(
      likelihood_true = NA,
      likelihood_false = NA,
      formula_components = NULL
    ))
  }

  # Extract links and intermediate nodes from path
  # Path format: H1 -> EL1 -> P1 -> EL2 -> R1
  links <- path[grepl("^EL_", path)]

  if (length(links) == 0) {
    warning(sprintf("No explanatory links found in path from %s to %s", hypothesis_id, target_id))
    return(list(
      likelihood_true = NA,
      likelihood_false = NA,
      formula_components = NULL
    ))
  }

  # Compute likelihood by marginalizing over intermediate nodes
  # For a chain H -> EL1 -> P -> EL2 -> R:
  # P(R | H, aux) = Σ_{EL1,P,EL2} P(R|EL2) × P(EL2|P,aux) × P(P|EL1) × P(EL1|H,aux)

  # We'll use the BayesNetBP infrastructure to compute this exactly
  # Set hypothesis and auxiliaries as evidence, then query target

  # Initialize likelihood values
  likelihood_true <- NA
  likelihood_false <- NA

  evidence_nodes <- list()
  evidence_nodes[[hypothesis_id]] <- "true"

  # Add auxiliary states to evidence
  for (aux_id in names(auxiliary_states)) {
    evidence_nodes[[aux_id]] <- auxiliary_states[[aux_id]]
  }

  # Compute posteriors with hypothesis=true and auxiliaries
  tryCatch({
    posteriors_h_true <- compute_posteriors(evidence_graph, cpts, evidence_nodes)
    likelihood_true <- posteriors_h_true[[target_id]]["true"]
  }, error = function(e) {
    warning(sprintf("Failed to compute likelihood with H=true: %s", e$message))
    likelihood_true <<- NA
  })

  # Compute posteriors with hypothesis=false and auxiliaries
  evidence_nodes[[hypothesis_id]] <- "false"
  tryCatch({
    posteriors_h_false <- compute_posteriors(evidence_graph, cpts, evidence_nodes)
    likelihood_false <- posteriors_h_false[[target_id]]["true"]
  }, error = function(e) {
    warning(sprintf("Failed to compute likelihood with H=false: %s", e$message))
    likelihood_false <<- NA
  })

  # Build formula components for display
  # Get the final target node and its incoming link
  target_node <- evidence_graph$nodes[[target_id]]

  # Find incoming link
  to_target_edges <- evidence_graph$edges[
    evidence_graph$edges$to == target_id &
    evidence_graph$edges$type == "TO_TARGET",
  ]

  if (nrow(to_target_edges) > 0) {
    link_id <- to_target_edges$from[1]
    link_node <- evidence_graph$nodes[[link_id]]

    # Extract parameters
    strength <- if ("strength" %in% names(target_node)) target_node$strength else NA
    base_rate <- if ("base_rate" %in% names(target_node)) target_node$base_rate else NA
    reliability <- if ("reliability" %in% names(link_node)) link_node$reliability else NA

    # Check if all auxiliaries for this link are true
    aux_edges <- evidence_graph$edges[
      evidence_graph$edges$to == link_id &
      evidence_graph$edges$type == "IS_REQUIRED_BY",
    ]

    all_aux_true <- TRUE
    if (nrow(aux_edges) > 0) {
      for (aux_id in aux_edges$from) {
        if (aux_id %in% names(auxiliary_states) && auxiliary_states[[aux_id]] != "true") {
          all_aux_true <- FALSE
          break
        }
      }
    }

    # Compute probability of link given hypothesis and auxiliaries
    prob_link_given_h_true <- if (all_aux_true) reliability else link_node$epsilon

    formula_components <- list(
      exact_formula = sprintf(
        "P(%s | %s, aux) = base_rate + (strength - base_rate) × reliability",
        target_id, hypothesis_id
      ),
      parameters = list(
        strength = strength,
        base_rate = base_rate,
        reliability = if (all_aux_true) reliability else link_node$epsilon,
        link_prob = prob_link_given_h_true
      ),
      calculation = if (!is.na(strength) && !is.na(base_rate) && !is.na(prob_link_given_h_true)) {
        sprintf(
          "%.4f + (%.4f - %.4f) × %.4f = %.4f",
          base_rate, strength, base_rate, prob_link_given_h_true,
          base_rate + (strength - base_rate) * prob_link_given_h_true
        )
      } else {
        "Parameters not available"
      },
      approximation = if (!is.na(strength) && !is.na(prob_link_given_h_true)) {
        sprintf(
          "≈ strength × reliability = %.4f × %.4f = %.4f",
          strength, prob_link_given_h_true,
          strength * prob_link_given_h_true
        )
      } else {
        "Parameters not available"
      }
    )
  } else {
    formula_components <- list(
      exact_formula = "No incoming link found",
      parameters = list(),
      calculation = "N/A",
      approximation = "N/A"
    )
  }

  return(list(
    likelihood_true = likelihood_true,
    likelihood_false = likelihood_false,
    formula_components = formula_components
  ))
}

# ==============================================================================
# POSTERIOR COMPUTATION WITH FORMULA DISPLAY
# ==============================================================================

#' Compute posterior P(H | Evidence) using Bayes' rule with formula display
#'
#' Implements Bayes' rule:
#'   P(H | E) = [P(E | H) × P(H)] / P(E)
#'   where P(E) = P(E | H) × P(H) + P(E | ¬H) × P(¬H)
#'
#' This function computes posteriors using the full belief propagation from
#' BayesNetBP, but also provides detailed formula breakdowns for transparency.
#'
#' @param evidence_graph EvidenceGraph containing Bayesian nodes
#' @param cpts Named list of CPTs
#' @param hypothesis_id ID of hypothesis to query
#' @param evidence_nodes Named list of observed evidence (node_id -> "true"/"false")
#'
#' @return List with:
#'   - posteriors: Named list of posterior distributions for all nodes
#'   - likelihood_details: List of likelihood calculations for each evidence node
#'   - bayes_formula: String showing Bayes' rule calculation
#'   - prior: Prior probability P(H)
#'   - posterior: Posterior probability P(H | E)
#'   - coherence_score: P(H | E) - P(H) (Bayesian coherence measure)
#'
#' @examples
#' graph <- build_covid_bayesian_case_study()
#' cpts <- generate_all_cpts_spec(graph)
#' result <- compute_posteriors_with_formula_spec(
#'   graph, cpts, "H_aerosol",
#'   evidence_nodes = list(R_studies = "true")
#' )
#' # result$posterior gives P(H_aerosol | R_studies=true)
#' # result$bayes_formula shows the calculation
compute_posteriors_with_formula_spec <- function(evidence_graph, cpts,
                                                 hypothesis_id, evidence_nodes) {
  # Get prior
  h_node <- evidence_graph$nodes[[hypothesis_id]]
  prior <- if ("prior" %in% names(h_node)) h_node$prior else 0.5

  # Compute likelihood for each piece of evidence
  likelihood_details <- list()

  # For the Bayes' rule calculation, we'll compute:
  # P(E | H=true) and P(E | H=false)
  #
  # Note: The specification discusses conditional vs marginalized likelihood.
  # Here we compute P(E | H) marginalizing over auxiliaries (their priors).
  #
  # For full conditional likelihood P(E | H, all aux=true), user should
  # include auxiliaries in evidence_nodes.

  # Compute posteriors with all evidence
  all_posteriors <- compute_posteriors(evidence_graph, cpts, evidence_nodes)

  # Get posterior for hypothesis
  posterior_h <- all_posteriors[[hypothesis_id]]["true"]

  # To compute the likelihoods P(E | H=true) and P(E | H=false),
  # we need to use Bayes' rule in reverse:
  # P(H | E) = P(E | H) × P(H) / P(E)
  # Therefore: P(E | H) = P(H | E) × P(E) / P(H)

  # Compute P(E) by marginalizing over H:
  # P(E) = P(E | H=true) × P(H=true) + P(E | H=false) × P(H=false)

  # Alternative: Compute posteriors with H=true and H=false as evidence
  evidence_h_true <- c(evidence_nodes, list())
  evidence_h_true[[hypothesis_id]] <- "true"
  posteriors_given_h_true <- compute_posteriors(evidence_graph, cpts, evidence_h_true)

  evidence_h_false <- c(evidence_nodes, list())
  evidence_h_false[[hypothesis_id]] <- "false"
  posteriors_given_h_false <- compute_posteriors(evidence_graph, cpts, evidence_h_false)

  # Compute detailed likelihood for each evidence node
  for (evidence_id in names(evidence_nodes)) {
    # Use conditional likelihood computation for this evidence
    lik <- compute_conditional_likelihood_spec(
      evidence_graph, cpts, hypothesis_id, evidence_id
    )
    likelihood_details[[evidence_id]] <- lik
  }

  # For Bayes' rule display, we'll use a simplified calculation
  # showing the relationship between prior and posterior

  # Compute evidence probability P(E)
  # Using the law of total probability:
  # P(E) = P(H | E) × P(E) / P(H)  (rearranging Bayes' rule)
  # But we need P(E | H) and P(E | ¬H) to show the formula properly

  # Simplified approach: Show the prior-to-posterior update
  coherence_score <- posterior_h - prior

  # Build Bayes formula string
  evidence_str <- paste(names(evidence_nodes), collapse = ", ")
  bayes_formula <- sprintf(
    "P(%s | %s) = %.4f\nPrior P(%s) = %.4f\nCoherence Score = %.4f - %.4f = %.4f",
    hypothesis_id, evidence_str, posterior_h,
    hypothesis_id, prior,
    posterior_h, prior, coherence_score
  )

  return(list(
    posteriors = all_posteriors,
    likelihood_details = likelihood_details,
    bayes_formula = bayes_formula,
    prior = prior,
    posterior = posterior_h,
    coherence_score = coherence_score
  ))
}

# ==============================================================================
# COHERENCE SCORE COMPUTATION
# ==============================================================================

#' Compute Bayesian coherence score
#'
#' The coherence score measures how much the evidence supports the hypothesis
#' beyond its prior probability:
#'   Coherence Score = P(H | E) - P(H)
#'
#' Positive scores indicate evidence supports the hypothesis.
#' Negative scores indicate evidence contradicts the hypothesis.
#'
#' @param posteriors Named list of posterior distributions
#' @param hypothesis_id ID of hypothesis
#' @param prior Prior probability P(H)
#'
#' @return List with posterior_prob, coherence_score, and interpretation
compute_coherence_score_spec <- function(posteriors, hypothesis_id, prior) {
  posterior_prob <- posteriors[[hypothesis_id]]["true"]
  coherence_score <- posterior_prob - prior

  # Interpret the coherence score
  if (coherence_score > 0.3) {
    interpretation <- "Strong support from evidence"
  } else if (coherence_score > 0.1) {
    interpretation <- "Moderate support from evidence"
  } else if (coherence_score > -0.1) {
    interpretation <- "Weak or no support from evidence"
  } else if (coherence_score > -0.3) {
    interpretation <- "Moderate contradiction with evidence"
  } else {
    interpretation <- "Strong contradiction with evidence"
  }

  return(list(
    posterior_prob = posterior_prob,
    coherence_score = coherence_score,
    interpretation = interpretation
  ))
}
