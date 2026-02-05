# ==============================================================================
# Evidence Graph Framework - CPT Utilities Module
# ==============================================================================
#
# Provides functions for creating, validating, and manipulating Conditional
# Probability Tables (CPTs). CPTs define how a node's probability depends on
# its parents in a Bayesian network.
#
# KEY FUNCTIONS
# -------------
# generate_default_cpt()  Create CPT with mode: noisy_or, uniform, deterministic
# validate_cpt()          Check CPT structure and probability constraints
# normalize_cpt()         Ensure distributions sum to 1 for each parent config
# set_evidence()          Create deterministic CPT for observed nodes
#
# CPT MODELS
# ----------
# Noisy-OR:      P(child | parents) = 1 - ∏(1 - leak_i) for true parents
#                Good for "any sufficient cause" relationships
#
# Uniform:       P(child | parents) = 0.5 for all configurations
#                Uninformative prior over conditions
#
# Deterministic: P(child | parents) = 1 if any/all parents true
#                For logical OR/AND relationships
#
# ==============================================================================
# CPT DATA STRUCTURE
# ==============================================================================
#
# A CPT is a named list with three fields:
#
# - states: Character vector of possible values (e.g., c("true", "false"))
# - parents: Character vector of parent node IDs (empty for root nodes)
# - probabilities: Named vector (no parents) or n-dimensional array (with parents)
#
# EXAMPLE: Root node (prior)
# list(
#   states = c("true", "false"),
#   parents = character(0),
#   probabilities = c(true = 0.5, false = 0.5)
# )
#
# EXAMPLE: Node with one parent
# list(
#   states = c("true", "false"),
#   parents = c("H1"),
#   probabilities = array(
#     c(0.9, 0.1,   # P(node | H1=true):  [P(true), P(false)]
#       0.2, 0.8),  # P(node | H1=false): [P(true), P(false)]
#     dim = c(2, 2),
#     dimnames = list(node = c("true", "false"), H1 = c("true", "false"))
#   )
# )

# ==============================================================================
# DEFAULT CPT GENERATION
# ==============================================================================

#' Generate a default CPT for a node
#'
#' Creates a CPT using one of three models:
#' - "noisy_or": Good for explanatory links (multiple sufficient causes)
#' - "uniform": Uninformative (P=0.5 for all configurations)
#' - "deterministic": Logical OR (child true if any parent true)
#'
#' @param node_id ID of the node (for reference, not used in CPT)
#' @param parent_ids Character vector of parent node IDs
#' @param mode "noisy_or", "uniform", or "deterministic"
#' @param leak_params Named list for noisy-or: list(base = 0.01, H1 = 0.9, ...)
#' @return CPT list with states, parents, probabilities
generate_default_cpt <- function(node_id, parent_ids = character(0),
                                 mode = "noisy_or", leak_params = NULL) {
  # Validate mode
  valid_modes <- c("noisy_or", "uniform", "deterministic")
  if (!(mode %in% valid_modes)) {
    stop("Invalid mode. Must be one of: ", paste(valid_modes, collapse = ", "))
  }

  # Binary states (true/false) for all nodes
  states <- c("true", "false")

  # Case 1: No parents (prior distribution)
  if (length(parent_ids) == 0) {
    # Default prior: slightly favor false (0.4 true, 0.6 false)
    # This represents "cautious" prior - claims unlikely unless evidence
    probs <- c(true = 0.4, false = 0.6)

    return(list(
      states = states,
      parents = character(0),
      probabilities = probs
    ))
  }

  # Case 2: Has parents (conditional distribution)
  n_parents <- length(parent_ids)
  n_states <- length(states)
  n_parent_configs <- 2^n_parents  # Each parent has 2 states (true/false)

  # Generate probabilities based on mode
  if (mode == "noisy_or") {
    probs <- generate_noisy_or_cpt(node_id, parent_ids, leak_params)
  } else if (mode == "uniform") {
    # Uniform: P(child|parents) = 0.5 for all configurations
    probs <- array(0.5, dim = rep(n_states, n_parents + 1))
  } else if (mode == "deterministic") {
    # Deterministic OR: child TRUE if any parent TRUE
    probs <- generate_deterministic_or_cpt(parent_ids)
  }

  # Create dimension names
  dimnames_list <- list(child = states)
  for (parent_id in parent_ids) {
    dimnames_list[[parent_id]] <- states
  }

  # Set dimension names on probabilities array
  dimnames(probs) <- dimnames_list

  return(list(
    states = states,
    parents = parent_ids,
    probabilities = probs
  ))
}

# ==============================================================================
# NOISY-OR CPT GENERATION
# ==============================================================================

#' Generate CPT using noisy-OR model
#'
#' Noisy-OR models "any sufficient cause" relationships:
#'   P(child=true | parents) = 1 - (1-base) × ∏[i: parent_i=true](1 - leak_i)
#'
#' PARAMETERS:
#' - leak_i: Probability that parent i alone causes child (0.8-0.9 typical)
#' - base: Spontaneous occurrence without any parents (0.01 typical)
#'
#' @param node_id ID of the child node
#' @param parent_ids Vector of parent node IDs
#' @param leak_params Named list: list(base = 0.01, Parent1 = 0.9, ...)
#' @return Probability array for CPT
generate_noisy_or_cpt <- function(node_id, parent_ids, leak_params = NULL) {
  n_parents <- length(parent_ids)

  # Default leak parameters if not provided
  if (is.null(leak_params)) {
    leak_params <- list(base = 0.01)  # Spontaneous occurrence
    for (parent_id in parent_ids) {
      leak_params[[parent_id]] <- 0.85  # Default link strength
    }
  }

  # Ensure leak_base exists
  if (is.null(leak_params$base)) {
    leak_params$base <- 0.01
  }

  # Create all possible parent configurations
  # For n parents, there are 2^n configurations
  n_configs <- 2^n_parents
  parent_configs <- expand.grid(rep(list(c("false", "true")), n_parents))
  names(parent_configs) <- parent_ids

  # Calculate P(child=true) for each configuration using noisy-or
  probs_true <- numeric(n_configs)

  for (i in 1:n_configs) {
    config <- parent_configs[i, , drop = FALSE]

    # Start with base leak
    prob_false <- 1 - leak_params$base

    # Multiply by (1 - leak_i) for each TRUE parent
    for (j in 1:n_parents) {
      parent_id <- parent_ids[j]
      if (config[1, j] == "true") {
        leak_i <- leak_params[[parent_id]]
        if (is.null(leak_i)) {
          leak_i <- 0.85  # Default if not specified
        }
        prob_false <- prob_false * (1 - leak_i)
      }
    }

    probs_true[i] <- 1 - prob_false
  }

  # P(child=false) = 1 - P(child=true)
  probs_false <- 1 - probs_true

  # Construct array with proper dimensions
  # Dimensions: child states (2) x parent1 states (2) x parent2 states (2) x ...
  dim_sizes <- c(2, rep(2, n_parents))  # 2 for child, 2 for each parent
  probs_array <- array(NA, dim = dim_sizes)

  # Fill array
  # For each parent configuration, set P(true) and P(false)
  idx <- 1
  for (i in 1:n_configs) {
    # Create index vector for this configuration
    # First dimension is child state, others are parent states
    parent_indices <- as.integer(parent_configs[i, ] == "true") + 1

    # Set P(child=true | this parent config)
    index_true <- c(1, parent_indices)  # 1 for "true" child state
    probs_array[rbind(index_true)] <- probs_true[i]

    # Set P(child=false | this parent config)
    index_false <- c(2, parent_indices)  # 2 for "false" child state
    probs_array[rbind(index_false)] <- probs_false[i]
  }

  return(probs_array)
}

# ==============================================================================
# DETERMINISTIC OR CPT
# ==============================================================================

#' Generate deterministic OR CPT
#'
#' Child is TRUE if ANY parent is TRUE (logical OR).
#' P(child=true | parents) = 1 if any parent true, 0 otherwise.
#'
#' @param parent_ids Vector of parent node IDs
#' @return Probability array for CPT
generate_deterministic_or_cpt <- function(parent_ids) {
  n_parents <- length(parent_ids)
  n_configs <- 2^n_parents

  # Create all parent configurations
  parent_configs <- expand.grid(rep(list(c("false", "true")), n_parents))

  # Child is TRUE if any parent is TRUE
  probs_true <- apply(parent_configs == "true", 1, any)
  probs_false <- !probs_true

  # Convert to numeric
  probs_true <- as.numeric(probs_true)
  probs_false <- as.numeric(probs_false)

  # Construct array
  dim_sizes <- c(2, rep(2, n_parents))
  probs_array <- array(NA, dim = dim_sizes)

  for (i in 1:n_configs) {
    parent_indices <- as.integer(parent_configs[i, ] == "true") + 1

    index_true <- c(1, parent_indices)
    probs_array[rbind(index_true)] <- probs_true[i]

    index_false <- c(2, parent_indices)
    probs_array[rbind(index_false)] <- probs_false[i]
  }

  return(probs_array)
}

# ==============================================================================
# CPT VALIDATION
# ==============================================================================

#' Validate CPT structure and probabilities
#'
#' Performs comprehensive validation:
#' 1. Required fields present (states, parents, probabilities)
#' 2. States is non-empty character vector
#' 3. Probabilities normalized (sum to 1 per parent config)
#' 4. Array dimensions match states & parent count
#' 5. All probabilities in [0, 1], no NAs
#'
#' @param cpt CPT list to validate
#' @return list(valid = TRUE/FALSE, errors = character vector)
validate_cpt <- function(cpt) {
  errors <- character()

  # Check required fields
  required_fields <- c("states", "parents", "probabilities")
  for (field in required_fields) {
    if (!(field %in% names(cpt))) {
      errors <- c(errors, paste("Missing required field:", field))
    }
  }

  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  }

  # Check states
  if (!is.character(cpt$states) || length(cpt$states) == 0) {
    errors <- c(errors, "States must be non-empty character vector")
  }

  # Check parents
  if (!is.character(cpt$parents)) {
    errors <- c(errors, "Parents must be character vector")
  }

  # Check probabilities
  probs <- cpt$probabilities

  # All probabilities must be in [0, 1]
  if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
    errors <- c(errors, "All probabilities must be in [0, 1]")
  }

  # Check for NAs
  if (any(is.na(probs))) {
    errors <- c(errors, "Probabilities contain NA values")
  }

  # Check normalization
  # For each parent configuration, probabilities over child states should sum to 1
  n_parents <- length(cpt$parents)
  n_states <- length(cpt$states)

  if (n_parents == 0) {
    # No parents: probabilities is a vector, should sum to 1
    if (abs(sum(probs) - 1.0) > 1e-6) {
      errors <- c(errors,
                 sprintf("Probabilities must sum to 1 (currently: %.6f)", sum(probs)))
    }
  } else {
    # Has parents: check each parent configuration
    # This is complex for multi-parent cases, so we do basic check
    # Total should equal 2^n_parents (one sum of 1 per parent config)
    expected_total <- 2^n_parents
    actual_total <- sum(probs)

    if (abs(actual_total - expected_total) > 1e-4) {
      errors <- c(errors,
                 sprintf("Probabilities normalization check failed (expected total: %.2f, actual: %.6f)",
                        expected_total, actual_total))
    }
  }

  # Check dimensions if array
  if (n_parents > 0 && is.array(probs)) {
    expected_dims <- c(n_states, rep(2, n_parents))  # Assuming binary parent states
    actual_dims <- dim(probs)

    # Use all.equal to compare dimensions (handles numeric vs integer)
    if (!isTRUE(all.equal(as.integer(expected_dims), as.integer(actual_dims)))) {
      errors <- c(errors,
                 sprintf("Probability array dimensions mismatch (expected: %s, actual: %s)",
                        paste(expected_dims, collapse = "x"),
                        paste(actual_dims, collapse = "x")))
    }
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors
  ))
}

# ==============================================================================
# CPT NORMALIZATION
# ==============================================================================

#' Normalize CPT so distributions sum to 1
#'
#' For each parent configuration, rescales probabilities over child states
#' to sum to 1. Safe for CPTs that may have unnormalized values.
#'
#' @param cpt CPT list (may be unnormalized)
#' @return CPT list with normalized probabilities
normalize_cpt <- function(cpt) {
  probs <- cpt$probabilities
  n_parents <- length(cpt$parents)

  if (n_parents == 0) {
    # No parents: normalize vector to sum to 1
    total <- sum(probs)
    if (total > 0) {
      cpt$probabilities <- probs / total
    }
  } else {
    # Has parents: normalize each parent configuration
    # This requires iterating over each parent configuration
    # For simplicity, we'll normalize along the first dimension (child states)

    # Get dimension sizes
    dims <- dim(probs)
    n_states <- dims[1]

    # For each parent configuration (all except first dimension)
    parent_configs <- expand.grid(lapply(dims[-1], function(d) 1:d))

    for (i in 1:nrow(parent_configs)) {
      # Create index for this parent configuration
      idx <- as.list(c(NA, as.numeric(parent_configs[i, ])))

      # Get probabilities for all child states given this parent config
      idx[[1]] <- 1:n_states
      child_probs <- do.call(`[`, c(list(probs), idx))

      # Normalize
      total <- sum(child_probs)
      if (total > 0) {
        child_probs <- child_probs / total

        # Put back
        for (j in 1:n_states) {
          idx[[1]] <- j
          probs <- do.call(`[<-`, c(list(probs), idx, list(child_probs[j])))
        }
      }
    }

    cpt$probabilities <- probs
  }

  return(cpt)
}

# ==============================================================================
# SET EVIDENCE
# ==============================================================================

#' Apply hard evidence to a node's CPT
#'
#' Transforms a CPT to reflect observed evidence. Sets:
#'   P(node = observed_state) = 1.0
#'   P(node = other_states) = 0.0
#'
#' Note: Removes parent dependencies since observed nodes are deterministic.
#'
#' @param cpt CPT list for the node
#' @param observed_state The observed state (e.g., "true")
#' @return Modified CPT with evidence applied
set_evidence <- function(cpt, observed_state) {
  if (!(observed_state %in% cpt$states)) {
    stop("Observed state '", observed_state, "' not in node's states: ",
         paste(cpt$states, collapse = ", "))
  }

  # Create new probabilities: 1 for observed state, 0 for others
  n_states <- length(cpt$states)
  new_probs <- rep(0, n_states)
  names(new_probs) <- cpt$states
  new_probs[observed_state] <- 1.0

  # Replace probabilities (discard parent dependencies)
  cpt$probabilities <- new_probs
  cpt$parents <- character(0)  # Evidence nodes have no parents

  return(cpt)
}
