# ==============================================================================
# Probability Visualization Helpers
# ==============================================================================
#
# Utility functions for mapping probabilities to colors for network
# visualization. These functions create intuitive color gradients that
# help users understand probability distributions at a glance.
#
# COLOR SCHEMES
# -------------
# probability_to_color():     Red (0) → Yellow (0.5) → Green (1)
#                             For absolute probability values
#
# delta_to_color():           Blue (-1) → White (0) → Orange (+1)
#                             For probability changes
#
# ratio_to_color():           Red (decrease) → White (0) → Green (increase)
#                             For log-ratio comparisons (prior vs posterior)
#
# absolute_change_to_color(): White (0) → Purple (1)
#                             For magnitude of change
#
# info_gain_to_color():       White (0) → Blue → Purple (1)
#                             For information gain in bits
#
# USAGE
# -----
# color <- probability_to_color(0.75)  # Returns green-ish hex
# color <- delta_to_color(-0.3)        # Returns blue-ish hex
# color <- ratio_to_color(0.5, 0.8)    # Returns green-ish hex (increase)

#' Convert probability [0, 1] to red-yellow-green color
#'
#' Maps probability to a traffic-light color scheme: low (red) to high (green).
#' Gray (#CCCCCC) is returned for NA/NULL values.
#'
#' @param p Numeric probability value between 0 and 1
#' @return Hex color string (e.g., "#FF0000" for red)
probability_to_color <- function(p) {
  if (is.na(p) || is.null(p)) {
    return("#CCCCCC")  # Gray for missing values
  }

  # Clamp to [0, 1]
  p <- max(0, min(1, p))

  # Create gradient: red (0) -> yellow (0.5) -> green (1)
  colors <- colorRampPalette(c("#FF0000", "#FFFF00", "#00FF00"))(101)
  index <- round(p * 100) + 1
  return(colors[index])
}

#' Convert delta [-1, 1] to blue-white-orange color
#'
#' @param delta Numeric change in probability between -1 and 1
#' @return Hex color string
delta_to_color <- function(delta) {
  if (is.na(delta) || is.null(delta)) {
    return("#CCCCCC")  # Gray for missing values
  }

  # Clamp to [-1, 1]
  delta <- max(-1, min(1, delta))

  # Map [-1, 1] to [1, 101] for color index
  # -1 -> 1 (blue), 0 -> 51 (white), 1 -> 101 (orange)
  colors <- colorRampPalette(c("#0000FF", "#FFFFFF", "#FFA500"))(101)
  index <- round((delta + 1) * 50) + 1
  return(colors[index])
}

#' Convert absolute change [0, 1] to white-purple color
#'
#' @param abs_delta Numeric absolute change between 0 and 1
#' @return Hex color string
absolute_change_to_color <- function(abs_delta) {
  if (is.na(abs_delta) || is.null(abs_delta)) {
    return("#CCCCCC")  # Gray for missing values
  }

  # Clamp to [0, 1]
  abs_delta <- max(0, min(1, abs_delta))

  # Create gradient: white (0) -> purple (1)
  colors <- colorRampPalette(c("#FFFFFF", "#800080"))(101)
  index <- round(abs_delta * 100) + 1
  return(colors[index])
}

#' Convert probability ratio to red-white-green color
#'
#' @param prior Numeric prior probability (before evidence)
#' @param posterior Numeric posterior probability (after evidence)
#' @return Hex color string
ratio_to_color <- function(prior, posterior) {
  # Handle edge cases
  if (is.na(prior) || is.na(posterior) || is.null(prior) || is.null(posterior)) {
    return("#CCCCCC")  # Gray for missing values
  }

  if (prior == 0 && posterior == 0) {
    return("#CCCCCC")  # Gray for undefined (both zero)
  }

  if (prior == 0) {
    return("#006400")  # Dark green (infinite increase from zero)
  }

  if (posterior == 0) {
    return("#8B0000")  # Dark red (decreased to zero)
  }

  # Compute log-ratio
  log_ratio <- log(posterior / prior)

  # Clamp to reasonable range [-2.3, 2.3]
  # This covers 0.1x to 10x (log(0.1) ≈ -2.3, log(10) ≈ 2.3)
  log_ratio_clamped <- max(-2.3, min(2.3, log_ratio))

  # Map to color: red (-2.3) → white (0) → green (2.3)
  colors <- colorRampPalette(c("#FF0000", "#FFFFFF", "#00FF00"))(101)
  index <- round((log_ratio_clamped + 2.3) / 4.6 * 100) + 1

  return(colors[index])
}

#' Convert information gain [0, 1] to white-blue-purple color
#'
#' @param info_gain Numeric information gain value in bits (0 to ~1)
#' @return Hex color string
info_gain_to_color <- function(info_gain) {
  if (is.na(info_gain) || is.null(info_gain)) {
    return("#CCCCCC")  # Gray for missing/non-evidence nodes
  }

  # Clamp to [0, 1] (max info gain for binary is 1 bit)
  info_gain <- max(0, min(1, info_gain))

  # Create gradient: white (0) -> light blue (0.5) -> purple (1)
  colors <- colorRampPalette(c("#FFFFFF", "#6495ED", "#800080"))(101)
  index <- round(info_gain * 100) + 1
  return(colors[index])
}

#' Convert EVPI percentage [0, 100] to purple gradient
#'
#' Uses percentage of current entropy reduced (evpi_pct) for consistent
#' interpretation across models with different baseline entropy.
#'
#' @param evpi_pct EVPI as percentage of current entropy (0 to 100+)
#' @return Hex color string
#' @details
#' Interpolates from white to purple (#6A46CE):
#' - 0%: White (low priority)
#' - 100%: Purple (high priority)
#' - NA/NULL: Gray (non-candidate nodes)
#'
#' Values above 100% are clamped to 100%.
evpi_to_color <- function(evpi_pct) {
  if (is.na(evpi_pct) || is.null(evpi_pct)) {
    return("#CCCCCC")  # Gray for non-candidate nodes
  }

  # Clamp to [0, 100]
  evpi_pct <- max(0, min(100, evpi_pct))

  # Interpolate from white to purple
  # White: RGB(255, 255, 255)
  # Purple: RGB(106, 70, 206) - #6A46CE
  t <- evpi_pct / 100

  r <- round(255 * (1 - t) + 106 * t)
  g <- round(255 * (1 - t) + 70 * t)
  b <- round(255 * (1 - t) + 206 * t)

  sprintf("#%02X%02X%02X", r, g, b)
}

# ==============================================================================
# NODE TYPE COLOR MAPPING
# ==============================================================================
# Centralized color definitions for node types. These are used by both
# get_node_type_color() (for R6 objects) and get_node_color_by_type_string()
# (for string-based type lookup in visualization code).

NODE_TYPE_COLORS <- list(
  Hypothesis = "#3589DD",       # Blue
  Account = "#3589DD",          # Blue (same as Hypothesis)
  AuxiliaryClaim = "#E8AB79",   # Orange
  ExplanatoryLink = "#5F5566",  # Dark gray
  PhenomenonClaim = "#A2E8DD",  # Light teal
  EmpiricalResult = "#8D89BA",  # Purple
  Selector = "#cccccc",         # Light gray
  Default = "#95a5a6"           # Gray fallback
)

#' Get color for a node based on its type string
#'
#' String-based lookup for node type colors. This is useful when working with
#' the node$type field directly (e.g., "Hypothesis", "PhenomenonClaim") rather
#' than checking R6 class inheritance.
#'
#' @param type_string String identifying the node type (e.g., "Hypothesis")
#' @return Hex color string
#' @examples
#' get_node_color_by_type_string("Hypothesis")  # Returns "#3589DD"
#' get_node_color_by_type_string("unknown")     # Returns "#95a5a6"
get_node_color_by_type_string <- function(type_string) {
  if (is.null(type_string) || !is.character(type_string)) {
    return(NODE_TYPE_COLORS$Default)
  }

  color <- NODE_TYPE_COLORS[[type_string]]
  if (is.null(color)) {
    return(NODE_TYPE_COLORS$Default)
  }
  return(color)
}

#' Get default color for a node based on its R6 class
#'
#' Returns a consistent color scheme for node types when probability-based
#' coloring is not available. Used as fallback in visualizations.
#'
#' Uses NODE_TYPE_COLORS for consistent color definitions.
#'
#' @param node Node object (inherits from Node class)
#' @return Hex color string
get_node_type_color <- function(node) {
  # Map R6 class to type string, then look up color
  if (inherits(node, "BayesianHypothesis")) {
    return(NODE_TYPE_COLORS$Hypothesis)
  } else if (inherits(node, "BayesianAuxiliary")) {
    return(NODE_TYPE_COLORS$AuxiliaryClaim)
  } else if (inherits(node, "BayesianExplanatoryLink")) {
    return(NODE_TYPE_COLORS$ExplanatoryLink)
  } else if (inherits(node, "BayesianPhenomenon")) {
    return(NODE_TYPE_COLORS$PhenomenonClaim)
  } else if (inherits(node, "BayesianResult")) {
    return(NODE_TYPE_COLORS$EmpiricalResult)
  } else {
    return(NODE_TYPE_COLORS$Default)
  }
}

# ==============================================================================
# COLOR MODE FUNCTIONS FOR NETWORK VISUALIZATION
# ==============================================================================
# These functions compute node colors based on different visualization modes.
# Used by the Shiny app to color nodes in the Bayesian network view.

#' Get structure-based color for a node
#'
#' Returns color based on node type (Hypothesis, Result, etc.)
#'
#' @param node Node object with $type field
#' @return Hex color string
get_structure_color <- function(node) {
  if (is.null(node)) return(NODE_TYPE_COLORS$Default)
  get_node_color_by_type_string(node$type)
}

#' Get baseline probability color for a node
#'
#' Baseline marginals are computed with all results conditioned as TRUE.
#'
#' @param node_id Node identifier
#' @param baseline Named list of baseline distributions (results=TRUE)
#' @return Hex color string
get_baseline_color <- function(node_id, baseline) {
  if (is.null(baseline[[node_id]])) return("#cccccc")
  prob <- baseline[[node_id]]["true"]
  if (is.na(prob)) return("#cccccc")
  probability_to_color(prob)
}

#' Get posterior probability color for a node
#'
#' @param node_id Node identifier
#' @param posteriors Named list of posterior distributions
#' @return Hex color string
get_posterior_color <- function(node_id, posteriors) {
  if (is.null(posteriors[[node_id]])) return("#cccccc")
  prob <- posteriors[[node_id]]["true"]
  if (is.na(prob)) return("#cccccc")
  probability_to_color(prob)
}

#' Get delta (to - from) color for a node
#'
#' Compares two sets of marginals and returns a color representing the change.
#'
#' @param node_id Node identifier
#' @param marginals_from Named list of "from" marginal distributions
#' @param marginals_to Named list of "to" marginal distributions
#' @return Hex color string
get_delta_color <- function(node_id, marginals_from, marginals_to) {
  if (is.null(marginals_from) || is.null(marginals_to)) return("#cccccc")
  if (is.null(marginals_from[[node_id]]) || is.null(marginals_to[[node_id]])) return("#cccccc")
  from_p <- marginals_from[[node_id]]["true"]
  to_p <- marginals_to[[node_id]]["true"]
  if (is.na(from_p) || is.na(to_p)) return("#cccccc")
  delta <- to_p - from_p
  delta_to_color(delta)
}

#' Get ratio (to / from) color for a node
#'
#' Compares two sets of marginals and returns a color representing the ratio.
#'
#' @param node_id Node identifier
#' @param marginals_from Named list of "from" marginal distributions (denominator)
#' @param marginals_to Named list of "to" marginal distributions (numerator)
#' @return Hex color string
get_ratio_color <- function(node_id, marginals_from, marginals_to) {
  if (is.null(marginals_from) || is.null(marginals_to)) return("#cccccc")
  if (is.null(marginals_from[[node_id]]) || is.null(marginals_to[[node_id]])) return("#cccccc")
  from_p <- marginals_from[[node_id]]["true"]
  to_p <- marginals_to[[node_id]]["true"]
  if (is.na(from_p) || is.na(to_p)) return("#cccccc")
  ratio_to_color(from_p, to_p)
}

#' Get EVPI-based color for a node
#'
#' Returns color based on Expected Value of Perfect Information.
#' Non-candidate nodes (evidence, selector, accounts, hypotheses) are shown in gray.
#'
#' @param node_id Node identifier
#' @param evpi_results Results from compute_all_evpi() containing results data frame
#' @return Hex color string
get_evpi_color <- function(node_id, evpi_results) {
  if (is.null(evpi_results) || is.null(evpi_results$results)) {
    return("#CCCCCC")  # Gray if no EVPI computed
  }

  results_df <- evpi_results$results
  row_idx <- which(results_df$node_id == node_id)

  if (length(row_idx) == 0) {
    return("#CCCCCC")  # Gray for non-candidate nodes
  }

  evpi_pct <- results_df$evpi_pct[row_idx]
  evpi_to_color(evpi_pct)
}

#' Compute node colors for all nodes based on visualization mode
#'
#' Main entry point for computing colors in the network visualization.
#' Dispatches to the appropriate color function based on mode.
#'
#' @param node_ids Vector of node IDs to color
#' @param nodes Named list of node objects
#' @param color_mode One of: "structure", "baseline", "posterior", "delta", "ratio", "evpi"
#' @param baseline Named list of baseline distributions (for "baseline" mode - results=TRUE)
#' @param posteriors Named list of posterior distributions (for "posterior" mode)
#' @param compare_from Named list of marginals for comparison baseline (for delta/ratio modes)
#' @param compare_to Named list of marginals for comparison target (for delta/ratio modes)
#' @param evpi_results Results from compute_all_evpi() (for "evpi" mode)
#' @return Character vector of hex color strings
compute_node_colors <- function(node_ids, nodes, color_mode,
                                baseline = NULL, posteriors = NULL,
                                compare_from = NULL, compare_to = NULL,
                                evpi_results = NULL) {
  sapply(node_ids, function(nid) {
    node <- nodes[[nid]]

    switch(color_mode,
      "structure" = get_structure_color(node),
      "baseline" = get_baseline_color(nid, baseline),
      "posterior" = get_posterior_color(nid, posteriors),
      "delta" = get_delta_color(nid, compare_from, compare_to),
      "ratio" = get_ratio_color(nid, compare_from, compare_to),
      "evpi" = get_evpi_color(nid, evpi_results),
      # Default fallback to structure
      get_structure_color(node)
    )
  })
}

# ==============================================================================
# TOOLTIP GENERATION FOR NETWORK VISUALIZATION
# ==============================================================================

#' Build parameter info string for a node
#'
#' Returns a string describing the Bayesian parameters of a node.
#'
#' @param node Node object
#' @return String describing node parameters, or empty string if none
get_node_param_info <- function(node) {
  if (inherits(node, "BayesianHypothesis") || inherits(node, "BayesianAuxiliary")) {
    if (!is.null(node$prior)) {
      return(sprintf("Prior: %.2f", node$prior))
    }
  } else if (inherits(node, "BayesianExplanatoryLink")) {
    if (!is.null(node$reliability) && !is.null(node$epsilon)) {
      return(sprintf("Reliability: %.2f, Epsilon: %.3f", node$reliability, node$epsilon))
    }
  } else if (inherits(node, "BayesianPhenomenon") || inherits(node, "BayesianResult")) {
    if (!is.null(node$strength) && !is.null(node$base_rate)) {
      return(sprintf("Strength: %.2f, Base Rate: %.2f", node$strength, node$base_rate))
    }
  }
  return("")
}

#' Build probability info string for a node
#'
#' Returns HTML string showing baseline and/or posterior probabilities.
#' When in delta/ratio mode with comparison marginals, shows the comparison values.
#' When in evpi mode, shows EVPI values.
#'
#' @param node_id Node identifier
#' @param baseline Named list of baseline distributions (results=TRUE)
#' @param posteriors Named list of posterior distributions
#' @param color_mode Current color mode (for log-ratio display)
#' @param compare_from Named list of "from" marginals for comparison
#' @param compare_to Named list of "to" marginals for comparison
#' @param from_label Display label for the "from" marginals
#' @param to_label Display label for the "to" marginals
#' @param evpi_results Results from compute_all_evpi() (for "evpi" mode)
#' @return HTML string with probability information
get_node_prob_info <- function(node_id, baseline, posteriors, color_mode,
                                compare_from = NULL, compare_to = NULL,
                                from_label = "Baseline", to_label = "Posterior",
                                evpi_results = NULL) {
  prob_info <- ""

  # In evpi mode, show EVPI values
  if (color_mode == "evpi") {
    if (!is.null(evpi_results) && !is.null(evpi_results$results)) {
      results_df <- evpi_results$results
      row_idx <- which(results_df$node_id == node_id)

      if (length(row_idx) > 0) {
        row <- results_df[row_idx, ]
        prob_info <- sprintf(
          "<br><b>EVPI: %.4f bits (%.1f%%)</b><br><b>Shift: %.3f</b><br><b>P(TRUE): %.2f</b>",
          row$evpi, row$evpi_pct, row$shift, row$p_true
        )

        # Add priority label
        priority <- if (row$evpi_pct >= 15) "Critical"
                    else if (row$evpi_pct >= 8) "High"
                    else if (row$evpi_pct >= 3) "Medium"
                    else "Low"
        prob_info <- paste0(prob_info, sprintf("<br><b>Priority: %s</b>", priority))
      } else {
        prob_info <- "<br><i>Not a candidate node</i>"
      }
    } else {
      prob_info <- "<br><i>No VOI analysis computed</i>"
    }
    return(prob_info)
  }

  # In delta/ratio mode with comparison marginals, show comparison values
  if (color_mode %in% c("delta", "ratio") && !is.null(compare_from) && !is.null(compare_to)) {
    from_prob <- NULL
    to_prob <- NULL

    if (!is.null(compare_from[[node_id]])) {
      from_prob <- compare_from[[node_id]]["true"]
      if (!is.na(from_prob)) {
        prob_info <- sprintf("<br><b>%s: %.4f</b>", from_label, from_prob)
      }
    }

    if (!is.null(compare_to[[node_id]])) {
      to_prob <- compare_to[[node_id]]["true"]
      if (!is.na(to_prob)) {
        prob_info <- paste0(prob_info, sprintf("<br><b>%s: %.4f</b>", to_label, to_prob))
      }
    }

    # Add delta
    if (!is.null(from_prob) && !is.null(to_prob) && !is.na(from_prob) && !is.na(to_prob)) {
      delta <- to_prob - from_prob
      prob_info <- paste0(prob_info, sprintf("<br><b>Delta: %+.4f</b>", delta))

      # Add log-ratio in ratio mode
      if (color_mode == "ratio") {
        if (from_prob > 0 && to_prob > 0) {
          log_ratio <- log(to_prob / from_prob)
          prob_info <- paste0(prob_info, sprintf("<br><b>Log-ratio: %.3f</b>", log_ratio))
        } else if (from_prob > 0 && to_prob == 0) {
          prob_info <- paste0(prob_info, "<br><b>Log-ratio: -Inf (posterior is 0)</b>")
        } else if (from_prob == 0 && to_prob > 0) {
          prob_info <- paste0(prob_info, "<br><b>Log-ratio: Inf (prior was 0)</b>")
        } else {
          prob_info <- paste0(prob_info, "<br><b>Log-ratio: N/A</b>")
        }
      }
    }
  } else {
    # Standard mode: show baseline and posterior
    baseline_prob <- NULL
    post_prob <- NULL

    if (!is.null(baseline) && !is.null(baseline[[node_id]])) {
      baseline_prob <- baseline[[node_id]]["true"]
      if (!is.na(baseline_prob)) {
        prob_info <- sprintf("<br><b>Baseline: %.4f</b>", baseline_prob)
      }
    }

    if (!is.null(posteriors) && !is.null(posteriors[[node_id]])) {
      post_prob <- posteriors[[node_id]]["true"]
      if (!is.na(post_prob)) {
        if (nchar(prob_info) > 0) {
          prob_info <- paste0(prob_info, sprintf("<br><b>Posterior: %.4f</b>", post_prob))
        } else {
          prob_info <- sprintf("<br><b>Posterior: %.4f</b>", post_prob)
        }
      }
    }
  }

  return(prob_info)
}

#' Build complete tooltip content for a node
#'
#' Creates HTML tooltip with node info, parameters, and probabilities.
#'
#' @param node Node object
#' @param node_id Node identifier
#' @param baseline Named list of baseline distributions (results=TRUE)
#' @param posteriors Named list of posterior distributions
#' @param color_mode Current visualization color mode
#' @param compare_from Named list of "from" marginals for comparison
#' @param compare_to Named list of "to" marginals for comparison
#' @param from_label Display label for the "from" marginals
#' @param to_label Display label for the "to" marginals
#' @param evpi_results Results from compute_all_evpi() (for "evpi" mode)
#' @param model ExplanatoryModel (optional, used to show hypothesis info for Account nodes)
#' @return HTML string for tooltip content
build_node_tooltip <- function(node, node_id, baseline, posteriors, color_mode,
                                compare_from = NULL, compare_to = NULL,
                                from_label = "Baseline", to_label = "Posterior",
                                evpi_results = NULL, model = NULL) {
  if (is.null(node)) return("")

  node_class <- class(node)[1]
  param_info <- get_node_param_info(node)
  prob_info <- get_node_prob_info(node_id, baseline, posteriors, color_mode,
                                   compare_from, compare_to, from_label, to_label,
                                   evpi_results)

  # For Account nodes, display hypothesis info instead
  if (!is.null(node$type) && node$type == "Account" && !is.null(model)) {
    account <- model$accounts[[node$id]]
    if (!is.null(account)) {
      hypothesis <- model$hypotheses[[account$hypothesis_id]]
      if (!is.null(hypothesis)) {
        display_id <- hypothesis$id
        type_display <- "Hypothesis"
        display_text <- hypothesis$text
        return(paste0("<b>", display_id, "</b><br>",
                      "<i>", type_display, "</i><br><br>",
                      display_text,
                      prob_info))
      }
    }
  }

  # Build tooltip for other node types
  type_display <- if (!is.null(node$type)) node$type else gsub("Bayesian", "", node_class)

  paste0("<b>", node$id, "</b><br>",
         "<i>", type_display, "</i><br><br>",
         node$text,
         if (nchar(param_info) > 0) paste0("<br><br>", param_info) else "",
         prob_info)
}

#' Compute tooltips for all nodes in network visualization
#'
#' @param node_ids Vector of node IDs
#' @param nodes Named list of node objects
#' @param baseline Named list of baseline distributions (results=TRUE)
#' @param posteriors Named list of posterior distributions
#' @param color_mode Current visualization color mode
#' @param compare_from Named list of "from" marginals for comparison
#' @param compare_to Named list of "to" marginals for comparison
#' @param from_label Display label for the "from" marginals
#' @param to_label Display label for the "to" arginals
#' @param evpi_results Results from compute_all_evpi() (for "evpi" mode)
#' @param model ExplanatoryModel (optional, used to show hypothesis info for Account nodes)
#' @return Character vector of HTML tooltip strings
compute_node_tooltips <- function(node_ids, nodes, baseline, posteriors, color_mode,
                                   compare_from = NULL, compare_to = NULL,
                                   from_label = "Baseline", to_label = "Posterior",
                                   evpi_results = NULL, model = NULL) {
  sapply(node_ids, function(nid) {
    build_node_tooltip(nodes[[nid]], nid, baseline, posteriors, color_mode,
                       compare_from, compare_to, from_label, to_label, evpi_results, model)
  })
}
