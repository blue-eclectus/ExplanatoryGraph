# ==============================================================================
# Sensitivity Analysis Module
# ==============================================================================
#
# This module provides functions for exploring how hypothesis and account
# posteriors change as model parameters are varied. It implements parameter
# sweep analysis as specified in docs/plans/sensitivity_analysis_specification.md
#
# MAIN FUNCTIONS
# --------------
# get_sweepable_parameters()  - List all parameters that can be swept
# adjust_hypothesis_priors()  - Proportionally adjust priors when sweeping one
# compute_parameter_sweep()   - Run sweep computation across parameter range
# compute_sweep_summary()     - Calculate sensitivity metrics from sweep results
# create_sweep_plot()         - Generate ggplot2 visualization of sweep
#
# DEPENDENCIES
# ------------
# Requires: ggplot2, gRain (for inference)
# Uses: ExplanatoryModel, BayesianHypothesis, BayesianAuxiliary, etc.

library(ggplot2)

# ==============================================================================
# GET SWEEPABLE PARAMETERS
# ==============================================================================

#' Get list of all sweepable parameters from a model
#'
#' Extracts all parameters that can be varied in a sensitivity analysis sweep.
#' Parameters are organized by type: hypothesis priors, auxiliary priors,
#' phenomenon parameters, result parameters, and link parameters.
#'
#' @param model ExplanatoryModel object
#'
#' @return Data frame with columns:
#'   - node_id: ID of containing node
#'   - node_type: Type of node ("hypothesis", "auxiliary", "phenomenon", "result", "link")
#'   - param_name: Parameter name ("prior", "strength", "base_rate", "reliability", "epsilon")
#'   - current_value: Current value of the parameter
#'   - display_name: Human-readable name for UI
#'   - description: Description of what the parameter represents
#'
#' @export
get_sweepable_parameters <- function(model) {
  params <- list()


  # Hypothesis priors
  for (h_id in names(model$hypotheses)) {
    h <- model$hypotheses[[h_id]]
    params[[length(params) + 1]] <- list(
      node_id = h_id,
      node_type = "hypothesis",
      param_name = "prior",
      current_value = h$prior,
      display_name = paste0(h_id, " prior"),
      description = paste0("Prior probability of hypothesis: ", h$text)
    )
  }

  # Auxiliary priors
  for (a_id in names(model$auxiliaries)) {
    a <- model$auxiliaries[[a_id]]
    params[[length(params) + 1]] <- list(
      node_id = a_id,
      node_type = "auxiliary",
      param_name = "prior",
      current_value = a$prior,
      display_name = paste0(a_id, " prior"),
      description = paste0("Prior probability of auxiliary: ", a$text)
    )
  }

  # Phenomenon parameters (strength and base_rate)
  for (p_id in names(model$phenomena)) {
    p <- model$phenomena[[p_id]]
    params[[length(params) + 1]] <- list(
      node_id = p_id,
      node_type = "phenomenon",
      param_name = "strength",
      current_value = p$strength,
      display_name = paste0(p_id, " strength"),
      description = paste0("P(", p_id, " | parent link active)")
    )
    params[[length(params) + 1]] <- list(
      node_id = p_id,
      node_type = "phenomenon",
      param_name = "base_rate",
      current_value = p$base_rate,
      display_name = paste0(p_id, " base_rate"),
      description = paste0("P(", p_id, " | parent link inactive)")
    )
  }

  # Result parameters (strength and base_rate)
  for (r_id in names(model$results)) {
    r <- model$results[[r_id]]
    params[[length(params) + 1]] <- list(
      node_id = r_id,
      node_type = "result",
      param_name = "strength",
      current_value = r$strength,
      display_name = paste0(r_id, " strength"),
      description = paste0("P(", r_id, " | parent link active)")
    )
    params[[length(params) + 1]] <- list(
      node_id = r_id,
      node_type = "result",
      param_name = "base_rate",
      current_value = r$base_rate,
      display_name = paste0(r_id, " base_rate"),
      description = paste0("P(", r_id, " | parent link inactive)")
    )
  }

  # Link parameters (reliability and epsilon)
  for (l_id in names(model$explanatory_links)) {
    l <- model$explanatory_links[[l_id]]
    params[[length(params) + 1]] <- list(
      node_id = l_id,
      node_type = "link",
      param_name = "reliability",
      current_value = l$reliability,
      display_name = paste0(l_id, " reliability"),
      description = paste0("P(", l_id, " active | all parent conditions met)")
    )
    params[[length(params) + 1]] <- list(
      node_id = l_id,
      node_type = "link",
      param_name = "epsilon",
      current_value = l$epsilon,
      display_name = paste0(l_id, " epsilon"),
      description = paste0("P(", l_id, " active | some parent conditions not met)")
    )
  }

  # Convert list of lists to data frame
  if (length(params) == 0) {
    return(data.frame(
      node_id = character(),
      node_type = character(),
      param_name = character(),
      current_value = numeric(),
      display_name = character(),
      description = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, lapply(params, as.data.frame, stringsAsFactors = FALSE))
}

# ==============================================================================
# ADJUST HYPOTHESIS PRIORS
# ==============================================================================

#' Adjust hypothesis priors proportionally when sweeping one hypothesis
#'
#' When sweeping a hypothesis prior, other hypothesis priors must be adjusted
#' to maintain the constraint that all priors sum to 1. This function uses
#' proportional adjustment: other hypotheses are scaled so that they maintain
#' their relative proportions while summing to 1 - new_prior.
#'
#' @param model ExplanatoryModel object
#' @param hypothesis_id ID of hypothesis being swept
#' @param new_prior New prior value for the swept hypothesis
#'
#' @return Named numeric vector of adjusted priors for all hypotheses
#'
#' @details
#' Algorithm:
#' 1. Compute sum of other priors: S = sum(priors) - current_prior
#' 2. Compute new sum needed: S' = 1 - new_prior
#' 3. If S > 0, scale each other prior by S'/S
#' 4. If S = 0 (edge case), distribute S' equally among other hypotheses
#'
#' @export
adjust_hypothesis_priors <- function(model, hypothesis_id, new_prior) {
  # Get current priors
  priors <- sapply(model$hypotheses, function(h) h$prior)

  if (!(hypothesis_id %in% names(priors))) {
    stop(sprintf("Hypothesis '%s' not found in model", hypothesis_id))
  }

  # Current prior of swept hypothesis
  current <- priors[hypothesis_id]

  # Sum of other priors
  other_sum <- sum(priors) - current  # Should be 1 - current

  # New sum needed for others
  new_other_sum <- 1 - new_prior

  # Compute adjusted priors
  adjusted <- priors
  adjusted[hypothesis_id] <- new_prior

  if (other_sum > 1e-10) {
    # Scale others proportionally
    scale_factor <- new_other_sum / other_sum
    for (h_id in names(priors)) {
      if (h_id != hypothesis_id) {
        adjusted[h_id] <- priors[h_id] * scale_factor
      }
    }
  } else {
    # Edge case: other priors were all zero
    # Distribute equally among other hypotheses
    n_others <- length(priors) - 1
    if (n_others > 0) {
      for (h_id in names(priors)) {
        if (h_id != hypothesis_id) {
          adjusted[h_id] <- new_other_sum / n_others
        }
      }
    }
    # If n_others == 0, there's only one hypothesis, nothing to adjust
  }

  return(adjusted)
}

# ==============================================================================
# COMPUTE PARAMETER SWEEP
# ==============================================================================

#' Compute a parameter sweep for sensitivity analysis
#'
#' Systematically varies a single parameter across a range of values while
#' holding all other parameters fixed, computing the posterior distribution
#' at each point using gRain inference.
#'
#' @param parameter_spec List specifying the parameter to sweep:
#'   - node_id: ID of the node containing the parameter
#'   - param_name: Name of the parameter ("prior", "strength", "base_rate", "reliability", "epsilon")
#'   - node_type: Type of node ("hypothesis", "auxiliary", "phenomenon", "result", "link")
#' @param range_min Minimum value for sweep (default: current - 0.3, clamped to 0.01)
#' @param range_max Maximum value for sweep (default: current + 0.3, clamped to 0.99)
#' @param n_points Number of points in sweep (default: 21)
#' @param target Character: "hypotheses" or "accounts"
#' @param model ExplanatoryModel object
#' @param evidence_graph EvidenceGraph object (the Bayesian network structure)
#' @param cpt_generator Function that generates CPTs from model (takes model, returns cpt_assignments)
#' @param inference_func Function that runs inference (takes evidence_graph, cpts, evidence; returns posteriors)
#' @param evidence Named list of current evidence (node_id -> "true"/"false")
#' @param progress_callback Optional function called with (current, total) for progress updates
#'
#' @return List containing:
#'   - parameter_values: Numeric vector of swept values
#'   - posteriors: Data frame with columns for each hypothesis/account
#'   - current_value: Original parameter value
#'   - current_posteriors: Posteriors at original value
#'   - summary: Data frame with sensitivity metrics per hypothesis/account
#'   - metadata: List with parameter info, evidence, timestamp
#'
#' @export
compute_parameter_sweep <- function(parameter_spec,
                                    range_min = NULL,
                                    range_max = NULL,
                                    n_points = 21,
                                    target = "hypotheses",
                                    model,
                                    evidence_graph,
                                    cpt_generator,
                                    inference_func,
                                    evidence = list(),
                                    progress_callback = NULL) {

  start_time <- Sys.time()

  # Extract parameter info

node_id <- parameter_spec$node_id
  node_type <- parameter_spec$node_type
  param_name <- parameter_spec$param_name

  # Get current value
  current_value <- get_parameter_value(model, node_id, node_type, param_name)

  # Set default range if not specified
  if (is.null(range_min)) {
    range_min <- max(0.01, current_value - 0.3)
  }
  if (is.null(range_max)) {
    range_max <- min(0.99, current_value + 0.3)
  }

  # Validate range
  if (range_min >= range_max) {
    stop("range_min must be less than range_max")
  }
  if (range_min <= 0 || range_max >= 1) {
    stop("Range must be within (0, 1)")
  }

  # Generate sweep values (21 evenly distributed points)
  parameter_values <- seq(range_min, range_max, length.out = n_points)

  # Get hypothesis/account IDs for tracking
  if (target == "hypotheses") {
    target_ids <- names(model$hypotheses)
  } else {
    target_ids <- names(model$accounts)
  }

  # Initialize results storage
  posteriors_list <- vector("list", n_points)

  # Run sweep
  for (i in seq_along(parameter_values)) {
    param_value <- parameter_values[i]

    # Progress callback
    if (!is.null(progress_callback)) {
      progress_callback(i, n_points)
    }

    # Temporarily modify the parameter
    modified_model <- clone_and_modify_parameter(
      model, node_id, node_type, param_name, param_value
    )

    # Regenerate CPTs with modified model
    cpts <- cpt_generator(modified_model)

    # Run inference
    all_posteriors <- inference_func(evidence_graph, cpts, evidence)

    # Extract target posteriors
    if (target == "hypotheses") {
      target_posteriors <- extract_hypothesis_posteriors(all_posteriors, target_ids, modified_model)
    } else {
      target_posteriors <- extract_account_posteriors(all_posteriors, target_ids)
    }

    posteriors_list[[i]] <- target_posteriors
  }

  # Combine into data frame
  posteriors_df <- data.frame(
    parameter_value = parameter_values,
    stringsAsFactors = FALSE
  )

  for (tid in target_ids) {
    posteriors_df[[tid]] <- sapply(posteriors_list, function(p) p[[tid]])
  }

  # Get current posteriors (at original parameter value)
  current_cpts <- cpt_generator(model)
  current_all_posteriors <- inference_func(evidence_graph, current_cpts, evidence)
  if (target == "hypotheses") {
    current_posteriors <- extract_hypothesis_posteriors(current_all_posteriors, target_ids, model)
  } else {
    current_posteriors <- extract_account_posteriors(current_all_posteriors, target_ids)
  }

  # Compute summary statistics
  summary_df <- compute_sweep_summary(
    parameter_values, posteriors_df, current_value, current_posteriors
  )

  end_time <- Sys.time()

  # Build result
  list(
    metadata = list(
      timestamp = Sys.time(),
      parameter = list(
        node_id = node_id,
        node_type = node_type,
        param_name = param_name,
        display_name = parameter_spec$display_name %||% paste0(node_id, " ", param_name)
      ),
      range_min = range_min,
      range_max = range_max,
      n_points = n_points,
      target = target,
      evidence = evidence,
      computation_time_seconds = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    current_value = current_value,
    parameter_values = parameter_values,
    posteriors = posteriors_df,
    current_posteriors = unlist(current_posteriors),
    summary = summary_df
  )
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Get current value of a parameter from the model
#' @keywords internal
get_parameter_value <- function(model, node_id, node_type, param_name) {
  node <- switch(node_type,
    "hypothesis" = model$hypotheses[[node_id]],
    "auxiliary" = model$auxiliaries[[node_id]],
    "phenomenon" = model$phenomena[[node_id]],
    "result" = model$results[[node_id]],
    "link" = model$explanatory_links[[node_id]],
    stop(sprintf("Unknown node type: %s", node_type))
  )

  if (is.null(node)) {
    stop(sprintf("Node '%s' not found in model", node_id))
  }

  value <- node[[param_name]]
  if (is.null(value)) {
    stop(sprintf("Parameter '%s' not found on node '%s'", param_name, node_id))
  }

  value
}

#' Clone model and modify a single parameter
#' @keywords internal
clone_and_modify_parameter <- function(model, node_id, node_type, param_name, new_value) {
  # Clone the model structure (shallow clone is sufficient since we'll deep clone the nodes we modify)
  modified <- model$clone(deep = FALSE)

  # Deep clone ALL R6 objects in lists to avoid modifying the original
  # This is necessary because R6's clone(deep=TRUE) doesn't deep clone R6 objects in lists
  modified$hypotheses <- lapply(model$hypotheses, function(h) h$clone(deep = TRUE))
  modified$auxiliaries <- lapply(model$auxiliaries, function(a) a$clone(deep = TRUE))
  modified$phenomena <- lapply(model$phenomena, function(p) p$clone(deep = TRUE))
  modified$results <- lapply(model$results, function(r) r$clone(deep = TRUE))
  modified$explanatory_links <- lapply(model$explanatory_links, function(l) l$clone(deep = TRUE))
  modified$accounts <- lapply(model$accounts, function(a) a$clone(deep = TRUE))

  # Special handling for hypothesis priors (must maintain sum = 1)
  if (node_type == "hypothesis" && param_name == "prior") {
    adjusted_priors <- adjust_hypothesis_priors(model, node_id, new_value)
    for (h_id in names(adjusted_priors)) {
      # Use unname() to ensure the prior is a simple numeric, not a named value
      # This prevents name propagation through compute_prior -> Selector -> CPT
      modified$hypotheses[[h_id]]$prior <- unname(adjusted_priors[h_id])
    }
  } else {
    # Get the node and modify the parameter
    node <- switch(node_type,
      "hypothesis" = modified$hypotheses[[node_id]],
      "auxiliary" = modified$auxiliaries[[node_id]],
      "phenomenon" = modified$phenomena[[node_id]],
      "result" = modified$results[[node_id]],
      "link" = modified$explanatory_links[[node_id]]
    )
    node[[param_name]] <- new_value
  }

  modified
}

#' Extract hypothesis posteriors from inference results
#'
#' Hypotheses are marginalized from the Selector node, which contains
#' account posteriors. Each account belongs to exactly one hypothesis,
#' so we sum account probabilities for each hypothesis.
#'
#' @param posteriors Named list of posterior distributions from compute_posteriors
#' @param hypothesis_ids Character vector of hypothesis IDs
#' @param model ExplanatoryModel object (needed for account-hypothesis mapping)
#' @keywords internal
extract_hypothesis_posteriors <- function(posteriors, hypothesis_ids, model = NULL) {
  # If Selector node exists, marginalize from accounts to hypotheses
  if ("Selector" %in% names(posteriors) && !is.null(model)) {
    selector <- posteriors[["Selector"]]

    # Initialize hypothesis probabilities as numeric (not list)
    result <- setNames(rep(0.0, length(hypothesis_ids)), hypothesis_ids)

    # Sum account probabilities for each hypothesis
    for (acc_id in names(selector)) {
      account <- model$accounts[[acc_id]]
      if (!is.null(account)) {
        h_id <- account$hypothesis_id
        if (h_id %in% hypothesis_ids) {
          # Extract the probability value - handle both named vector and list formats
          prob_val <- selector[[acc_id]]
          if (is.numeric(prob_val) && length(prob_val) == 1) {
            result[h_id] <- result[h_id] + prob_val
          }
        }
      }
    }

    # Convert to list for consistent return type
    return(as.list(result))
  }

  # Fallback: try to find hypothesis nodes directly
  result <- list()
  for (h_id in hypothesis_ids) {
    if (h_id %in% names(posteriors)) {
      # Get probability of "true" state
      p <- posteriors[[h_id]]
      result[[h_id]] <- if ("true" %in% names(p)) p[["true"]] else p[[1]]
    } else {
      result[[h_id]] <- NA_real_
    }
  }
  result
}

#' Extract account posteriors from Selector node
#' @keywords internal
extract_account_posteriors <- function(posteriors, account_ids) {
  # The Selector node contains the account distribution
  if ("Selector" %in% names(posteriors)) {
    selector <- posteriors[["Selector"]]
    result <- list()
    for (a_id in account_ids) {
      result[[a_id]] <- if (a_id %in% names(selector)) selector[[a_id]] else NA_real_
    }
    return(result)
  }

  # Fallback: try to find account nodes directly
  result <- list()
  for (a_id in account_ids) {
    if (a_id %in% names(posteriors)) {
      p <- posteriors[[a_id]]
      result[[a_id]] <- if ("true" %in% names(p)) p[["true"]] else p[[1]]
    } else {
      result[[a_id]] <- NA_real_
    }
  }
  result
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============================================================================
# COMPUTE SWEEP SUMMARY
# ==============================================================================

#' Compute summary statistics for a parameter sweep
#'
#' @param parameter_values Numeric vector of parameter values
#' @param posteriors Data frame with posterior columns for each hypothesis/account
#' @param current_value Current parameter value
#' @param current_posteriors Named list of current posteriors
#'
#' @return Data frame with columns:
#'   - target_id: Hypothesis or account ID
#'   - current: Current posterior
#'   - min: Minimum posterior in sweep
#'   - max: Maximum posterior in sweep
#'   - range: max - min
#'   - max_change: Largest absolute change from current
#'   - max_change_at: Parameter value at max change
#'
#' @export
compute_sweep_summary <- function(parameter_values, posteriors,
                                  current_value, current_posteriors) {
  # Get target IDs (all columns except parameter_value)
  target_ids <- setdiff(names(posteriors), "parameter_value")

  summary_rows <- lapply(target_ids, function(tid) {
    values <- posteriors[[tid]]
    current <- current_posteriors[[tid]]

    # Compute changes from current
    changes <- values - current
    abs_changes <- abs(changes)
    max_change_idx <- which.max(abs_changes)

    data.frame(
      target_id = tid,
      current = current,
      min = min(values, na.rm = TRUE),
      max = max(values, na.rm = TRUE),
      range = max(values, na.rm = TRUE) - min(values, na.rm = TRUE),
      max_change = changes[max_change_idx],
      max_change_at = parameter_values[max_change_idx],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, summary_rows)
}

# ==============================================================================
# CREATE SWEEP PLOT
# ==============================================================================

#' Create a ggplot2 visualization of a parameter sweep
#'
#' @param sweep_result Result from compute_parameter_sweep()
#' @param highlight_id Optional: ID of hypothesis/account to highlight
#' @param show_current Logical: whether to show vertical line at current value (default TRUE)
#' @param colors Optional named vector of colors for each hypothesis/account
#'
#' @return ggplot2 object
#'
#' @export
create_sweep_plot <- function(sweep_result,
                              highlight_id = NULL,
                              show_current = TRUE,
                              colors = NULL) {

  # Reshape posteriors to long format for ggplot
  posteriors <- sweep_result$posteriors
  target_ids <- setdiff(names(posteriors), "parameter_value")

  plot_data <- do.call(rbind, lapply(target_ids, function(tid) {
    data.frame(
      parameter_value = posteriors$parameter_value,
      target_id = tid,
      posterior = posteriors[[tid]],
      stringsAsFactors = FALSE
    )
  }))

  # Set up colors
  if (is.null(colors)) {
    # Default colors matching spec
    n_targets <- length(target_ids)
    if (n_targets <= 2) {
      colors <- c("#3589DD", "#E8AB79")[1:n_targets]
      names(colors) <- target_ids
    } else {
      colors <- viridis::viridis(n_targets)
      names(colors) <- target_ids
    }
  }

  # Create base plot
  p <- ggplot(plot_data, aes(x = parameter_value, y = posterior, color = target_id)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2) +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = paste("Sensitivity to", sweep_result$metadata$parameter$display_name),
      subtitle = if (length(sweep_result$metadata$evidence) > 0) {
        paste("Evidence:", paste(names(sweep_result$metadata$evidence), "=",
                                  sweep_result$metadata$evidence, collapse = ", "))
      } else {
        "No evidence set"
      },
      x = sweep_result$metadata$parameter$display_name,
      y = "Posterior Probability",
      color = if (sweep_result$metadata$target == "hypotheses") "Hypothesis" else "Account"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )

  # Add current value line
  if (show_current) {
    p <- p + geom_vline(
      xintercept = sweep_result$current_value,
      linetype = "dashed",
      color = "#666666",
      linewidth = 0.8
    ) +
    annotate(
      "text",
      x = sweep_result$current_value,
      y = 0.95,
      label = "current",
      hjust = -0.1,
      size = 3,
      color = "#666666"
    )
  }

  # Highlight specific target if requested
  if (!is.null(highlight_id) && highlight_id %in% target_ids) {
    highlight_data <- plot_data[plot_data$target_id == highlight_id, ]
    p <- p + geom_line(
      data = highlight_data,
      linewidth = 2.5,
      alpha = 0.3
    )
  }

  p
}
