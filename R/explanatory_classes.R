# ==============================================================================
# Explanatory Account Framework - Core Classes
# ==============================================================================
#
# This module implements the explanatory-eliminative approach for comparing
# competing scientific hypotheses. The key abstraction is an "Account"—a
# specific explanatory pathway connecting a hypothesis to evidence.
#
# CONCEPTUAL MODEL
# ----------------
# ExplanatoryModel contains:
#   - Hypotheses: Competing theories (e.g., aerosol vs. droplet transmission)
#   - Accounts: Specific explanatory pathways (one hypothesis can have many)
#   - Evidence Corpus: Empirical results all accounts must explain
#   - Shared pools: Phenomena and auxiliaries reusable across accounts
#
# The Selector node enforces mutual exclusivity: exactly one account is true.
#
# WORKFLOW
# --------
# 1. Create ExplanatoryModel
# 2. Add results (evidence corpus)
# 3. Add hypotheses with priors that sum to 1.0
# 4. Build accounts linking hypotheses to results via phenomena
# 5. Call build_bayesian_network() to generate the inference graph
#
# KEY CLASSES
# -----------
# - ExplanatoryModel: Central container for all model components
# - Account: Explanatory pathway (hypothesis → phenomena → results)
# - Selector: Multi-state node for account selection (generated automatically)

library(R6)

# Source dependencies (for development mode)
if (!isNamespaceLoaded("ExplanatoryAccounts") && !exists("BayesianHypothesis")) {
  source("R/bayesian_spec_classes.R")
}

# ==============================================================================
# EXPLANATORY MODEL
# ==============================================================================

#' ExplanatoryModel - Container for explanatory analysis
#'
#' Central data structure for the explanatory-eliminative framework. Contains
#' all components needed to compare competing hypotheses against evidence.
#'
#' @field name Model name/description
#' @field results Evidence corpus (BayesianResult objects)
#' @field hypotheses Competing theories (BayesianHypothesis objects)
#' @field accounts Explanatory pathways (Account objects)
#' @field phenomena Shared pool of intermediate claims (BayesianPhenomenon)
#' @field auxiliaries Shared pool of background assumptions (BayesianAuxiliary)
#' @field explanatory_links All "explains" relationships (BayesianExplanatoryLink)
#'
#' @examples
#' model <- ExplanatoryModel$new(name = "COVID Transmission")
#' model$add_result(BayesianResult$new("R1", "Ventilation effect", "Study"))
#' model$add_hypothesis(BayesianHypothesis$new("H1", "Aerosol mode", "Theory", prior = 0.5))
#' model$add_hypothesis(BayesianHypothesis$new("H2", "Droplet mode", "Theory", prior = 0.5))
ExplanatoryModel <- R6Class("ExplanatoryModel",
  public = list(
    name = NULL,
    results = list(),
    hypotheses = list(),
    accounts = list(),
    phenomena = list(),
    auxiliaries = list(),
    explanatory_links = list(),

    #' @description Create new ExplanatoryModel
    #' @param name Model name
    initialize = function(name = "Explanatory Model") {
      self$name <- name
      self$results <- list()
      self$hypotheses <- list()
      self$accounts <- list()
      self$phenomena <- list()
      self$auxiliaries <- list()
      self$explanatory_links <- list()
    },

    #' @description Add result to evidence corpus
    #' @param result BayesianResult object
    add_result = function(result) {
      if (!inherits(result, "BayesianResult")) {
        stop("result must be a BayesianResult object")
      }
      if (result$id %in% names(self$results)) {
        stop(sprintf("Result with ID '%s' already exists", result$id))
      }
      self$results[[result$id]] <- result
    },

    #' @description Add hypothesis
    #' @param hypothesis BayesianHypothesis object
    add_hypothesis = function(hypothesis) {
      if (!inherits(hypothesis, "BayesianHypothesis")) {
        stop("hypothesis must be a BayesianHypothesis object")
      }
      if (hypothesis$id %in% names(self$hypotheses)) {
        stop(sprintf("Hypothesis with ID '%s' already exists", hypothesis$id))
      }
      self$hypotheses[[hypothesis$id]] <- hypothesis
    },

    #' @description Add account
    #' @param account Account object
    add_account = function(account) {
      if (!inherits(account, "Account")) {
        stop("account must be an Account object")
      }
      if (account$id %in% names(self$accounts)) {
        stop(sprintf("Account with ID '%s' already exists", account$id))
      }
      self$accounts[[account$id]] <- account
    },

    #' @description Add phenomenon to shared pool
    #' @param phenomenon BayesianPhenomenon object
    add_phenomenon = function(phenomenon) {
      if (!inherits(phenomenon, "BayesianPhenomenon")) {
        stop("phenomenon must be a BayesianPhenomenon object")
      }
      if (phenomenon$id %in% names(self$phenomena)) {
        stop(sprintf("Phenomenon with ID '%s' already exists", phenomenon$id))
      }
      self$phenomena[[phenomenon$id]] <- phenomenon
    },

    #' @description Add auxiliary to shared pool
    #' @param auxiliary BayesianAuxiliary object
    add_auxiliary = function(auxiliary) {
      if (!inherits(auxiliary, "BayesianAuxiliary")) {
        stop("auxiliary must be a BayesianAuxiliary object")
      }
      if (auxiliary$id %in% names(self$auxiliaries)) {
        stop(sprintf("Auxiliary with ID '%s' already exists", auxiliary$id))
      }
      self$auxiliaries[[auxiliary$id]] <- auxiliary
    },

    #' @description Add explanatory link
    #' @param link BayesianExplanatoryLink object
    add_explanatory_link = function(link) {
      if (!inherits(link, "BayesianExplanatoryLink")) {
        stop("link must be a BayesianExplanatoryLink object")
      }
      if (link$id %in% names(self$explanatory_links)) {
        stop(sprintf("Explanatory link with ID '%s' already exists", link$id))
      }
      self$explanatory_links[[link$id]] <- link
    },

    #' @description Validate hypothesis priors sum to 1.0
    #' @param tolerance Numerical tolerance (default 0.001)
    #' @return TRUE if valid, FALSE otherwise
    validate_hypothesis_priors = function(tolerance = 0.001) {
      if (length(self$hypotheses) == 0) {
        return(TRUE)
      }

      prior_sum <- sum(sapply(self$hypotheses, function(h) h$prior))
      abs(prior_sum - 1.0) < tolerance
    },

    #' @description Get all accounts for a hypothesis
    #' @param hypothesis_id Hypothesis ID
    #' @return List of Account objects
    get_accounts_for_hypothesis = function(hypothesis_id) {
      Filter(function(acc) acc$hypothesis_id == hypothesis_id, self$accounts)
    },

    #' @description Convert to plain list for serialization
    #' @return Named list of all model components
    to_list = function() {
      list(
        name = self$name,
        results = lapply(self$results, function(r) r$to_list()),
        hypotheses = lapply(self$hypotheses, function(h) h$to_list()),
        accounts = lapply(self$accounts, function(a) a$to_list()),
        phenomena = lapply(self$phenomena, function(p) p$to_list()),
        auxiliaries = lapply(self$auxiliaries, function(a) a$to_list()),
        explanatory_links = lapply(self$explanatory_links, function(l) l$to_list())
      )
    },

    #' @description Export model to JSON string
    #' @return JSON string
    to_json = function() {
      export_data <- list(
        format = "explanatory-accounts-model",
        version = "1.0",
        exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        model = self$to_list()
      )
      jsonlite::toJSON(export_data, pretty = TRUE, auto_unbox = TRUE)
    }
  )
)

# ==============================================================================
# ACCOUNT
# ==============================================================================

#' Account - Explanatory pathway from hypothesis to evidence
#'
#' Represents one specific way a hypothesis explains the evidence. Multiple
#' accounts can support the same hypothesis (alternative mechanisms), and
#' the Selector enforces mutual exclusivity across all accounts.
#'
#' PRIOR ALLOCATION:
#' Account priors are computed from hypothesis priors using relative weights:
#'   P(Account) = P(Hypothesis) × (relative_weight / total_weight_for_hypothesis)
#'
#' @field id Unique identifier
#' @field hypothesis_id Parent hypothesis this account supports
#' @field description Text distinguishing this explanatory pathway
#' @field relative_weight Weight for prior allocation within hypothesis (default 1)
#' @field phenomenon_ids Phenomena used in this account's explanatory chains
#' @field auxiliary_ids Background assumptions required by this account
#' @field explanatory_link_ids Links forming this account's explanatory chains
#'
#' @examples
#' acc <- Account$new(
#'   id = "Acc1",
#'   hypothesis_id = "H1",
#'   description = "Aerosol transmission via close-range + ventilation",
#'   relative_weight = 1
#' )
#' acc$add_phenomenon_id("PC1")
#' acc$add_explanatory_link_id("EL1")
Account <- R6Class("Account",
  public = list(
    id = NULL,
    hypothesis_id = NULL,
    description = NULL,
    relative_weight = 1,
    phenomenon_ids = NULL,
    auxiliary_ids = NULL,
    explanatory_link_ids = NULL,

    #' @description Create new Account
    #' @param id Account ID
    #' @param hypothesis_id Parent hypothesis ID
    #' @param description Account description
    #' @param relative_weight Weight for prior allocation (default 1)
    initialize = function(id, hypothesis_id, description, relative_weight = 1) {
      # Validate id
      if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
        stop("id must be a non-empty string")
      }

      # Validate hypothesis_id
      if (!is.character(hypothesis_id) || length(hypothesis_id) != 1 || nchar(hypothesis_id) == 0) {
        stop("hypothesis_id must be a non-empty string")
      }

      # Validate description
      if (!is.character(description) || length(description) != 1 || nchar(description) == 0) {
        stop("description must be a non-empty string")
      }

      # Validate relative_weight is numeric
      if (!is.numeric(relative_weight)) {
        stop("relative_weight must be numeric")
      }

      # Validate relative_weight is positive
      if (relative_weight <= 0) {
        stop("relative_weight must be positive")
      }

      self$id <- id
      self$hypothesis_id <- hypothesis_id
      self$description <- description
      self$relative_weight <- relative_weight
      self$phenomenon_ids <- character(0)
      self$auxiliary_ids <- character(0)
      self$explanatory_link_ids <- character(0)
    },

    #' @description Add phenomenon ID to account
    #' @param phenomenon_id Phenomenon ID
    add_phenomenon_id = function(phenomenon_id) {
      if (!(phenomenon_id %in% self$phenomenon_ids)) {
        self$phenomenon_ids <- c(self$phenomenon_ids, phenomenon_id)
      }
    },

    #' @description Add auxiliary ID to account
    #' @param auxiliary_id Auxiliary ID
    add_auxiliary_id = function(auxiliary_id) {
      if (!(auxiliary_id %in% self$auxiliary_ids)) {
        self$auxiliary_ids <- c(self$auxiliary_ids, auxiliary_id)
      }
    },

    #' @description Add explanatory link ID to account
    #' @param link_id Explanatory link ID
    add_explanatory_link_id = function(link_id) {
      if (!(link_id %in% self$explanatory_link_ids)) {
        self$explanatory_link_ids <- c(self$explanatory_link_ids, link_id)
      }
    },

    #' @description Compute account prior from hypothesis prior
    #' @param hypothesis_prior Prior of parent hypothesis
    #' @param total_weight Sum of relative weights for all accounts under this hypothesis
    #' @return Account prior probability
    compute_prior = function(hypothesis_prior, total_weight) {
      # Guard against division by zero
      if (!is.numeric(total_weight) || total_weight <= 0) {
        stop("total_weight must be positive")
      }

      hypothesis_prior * (self$relative_weight / total_weight)
    },

    #' @description Convert to plain list for serialization
    #' @return Named list of all fields
    to_list = function() {
      list(
        id = self$id,
        hypothesis_id = self$hypothesis_id,
        description = self$description,
        relative_weight = self$relative_weight,
        phenomenon_ids = self$phenomenon_ids,
        auxiliary_ids = self$auxiliary_ids,
        explanatory_link_ids = self$explanatory_link_ids
      )
    }
  )
)

# ==============================================================================
# SELECTOR
# ==============================================================================

#' Selector - Multi-state node enforcing account mutual exclusivity
#'
#' The Selector is a key architectural component that ensures exactly one
#' explanatory account is active. It's a multi-state node where each state
#' corresponds to an account. Priors are computed automatically from
#' hypothesis priors and account relative weights.
#'
#' STRUCTURE:
#'   Selector → Account₁, Account₂, ..., Accountₙ
#'
#' CPT: Deterministic mapping from selector state to account states
#'   P(Accountᵢ = true | Selector = Accountᵢ) = 1.0
#'   P(Accountᵢ = true | Selector ≠ Accountᵢ) = 0.0
#'
#' @field account_ids Vector of account IDs (one per selector state)
#' @field priors Named list: account_id → prior probability
#'
#' @examples
#' # Created automatically from ExplanatoryModel:
#' selector <- Selector$new(model)
#' selector$get_states()       # Returns account IDs
#' selector$get_prior_vector() # Returns named vector of priors
Selector <- R6Class("Selector",
  public = list(
    account_ids = NULL,
    priors = NULL,

    #' @description Create Selector from ExplanatoryModel
    #' @param model ExplanatoryModel object
    initialize = function(model) {
      if (!inherits(model, "ExplanatoryModel")) {
        stop("model must be an ExplanatoryModel object")
      }

      # Validate hypothesis priors sum to 1
      if (!model$validate_hypothesis_priors()) {
        stop("Hypothesis priors must sum to 1.0")
      }

      # Validate model has accounts
      if (length(model$accounts) == 0) {
        stop("Cannot create Selector: model has no accounts")
      }

      self$account_ids <- names(model$accounts)
      self$priors <- list()

      # Compute prior for each account
      for (acc_id in self$account_ids) {
        account <- model$accounts[[acc_id]]
        hypothesis <- model$hypotheses[[account$hypothesis_id]]

        if (is.null(hypothesis)) {
          stop(sprintf("Account %s references non-existent hypothesis %s",
                      acc_id, account$hypothesis_id))
        }

        # Get all accounts for this hypothesis
        hypothesis_accounts <- model$get_accounts_for_hypothesis(account$hypothesis_id)

        # Sum relative weights
        total_weight <- sum(sapply(hypothesis_accounts, function(a) a$relative_weight))

        # Compute account prior
        self$priors[[acc_id]] <- account$compute_prior(hypothesis$prior, total_weight)
      }
    },

    #' @description Get states for selector node
    #' @return Vector of state names (account IDs)
    get_states = function() {
      self$account_ids
    },

    #' @description Get prior probability vector
    #' @return Named vector of priors
    get_prior_vector = function() {
      unlist(self$priors)
    }
  )
)

# ==============================================================================
# STATIC DESERIALIZATION METHODS
# ==============================================================================

# Null-coalescing operator helper
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

#' Create Account from list representation
#' @param data List from to_list() or JSON parse
#' @return Account object
Account$from_list <- function(data) {
  acc <- Account$new(
    id = data$id,
    hypothesis_id = data$hypothesis_id,
    description = data$description,
    relative_weight = data$relative_weight %||% 1
  )

  # Restore phenomenon IDs
  if (!is.null(data$phenomenon_ids)) {
    for (pid in data$phenomenon_ids) {
      acc$add_phenomenon_id(pid)
    }
  }

  # Restore auxiliary IDs
  if (!is.null(data$auxiliary_ids)) {
    for (aid in data$auxiliary_ids) {
      acc$add_auxiliary_id(aid)
    }
  }

  # Restore explanatory link IDs
  if (!is.null(data$explanatory_link_ids)) {
    for (lid in data$explanatory_link_ids) {
      acc$add_explanatory_link_id(lid)
    }
  }

  acc
}

#' Create ExplanatoryModel from JSON string
#' @param json_str JSON string from to_json()
#' @return ExplanatoryModel object
ExplanatoryModel$from_json <- function(json_str) {
  data <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)

  # Validate format
  if (is.null(data$format) || data$format != "explanatory-accounts-model") {
    stop("Invalid format: not an Explanatory Accounts model file")
  }

  model_data <- data$model
  model <- ExplanatoryModel$new(name = model_data$name)

  # Restore results
  for (r_data in model_data$results) {
    result <- BayesianResult$new(
      id = r_data$id,
      text = r_data$text,
      source = r_data$source,
      strength = r_data$strength %||% 0.9,
      base_rate = r_data$base_rate %||% 0.001
    )
    model$add_result(result)
  }

  # Restore hypotheses
  for (h_data in model_data$hypotheses) {
    hypothesis <- BayesianHypothesis$new(
      id = h_data$id,
      text = h_data$text,
      source = h_data$source,
      prior = h_data$prior
    )
    model$add_hypothesis(hypothesis)
  }

  # Restore auxiliaries
  for (a_data in model_data$auxiliaries) {
    auxiliary <- BayesianAuxiliary$new(
      id = a_data$id,
      text = a_data$text,
      source = a_data$source,
      prior = a_data$prior
    )
    model$add_auxiliary(auxiliary)
  }

  # Restore phenomena
  for (p_data in model_data$phenomena) {
    phenomenon <- BayesianPhenomenon$new(
      id = p_data$id,
      text = p_data$text,
      source = p_data$source,
      strength = p_data$strength %||% 0.9,
      base_rate = p_data$base_rate %||% 0.001
    )
    model$add_phenomenon(phenomenon)
  }

  # Restore explanatory links
  for (l_data in model_data$explanatory_links) {
    link <- BayesianExplanatoryLink$new(
      id = l_data$id,
      text = l_data$text,
      source = l_data$source,
      source_ids = l_data$source_ids,
      target_id = l_data$target_id,
      reliability = l_data$reliability %||% 0.9,
      epsilon = l_data$epsilon %||% 0.01
    )
    model$add_explanatory_link(link)
  }

  # Restore accounts
  for (acc_data in model_data$accounts) {
    model$add_account(Account$from_list(acc_data))
  }

  model
}
