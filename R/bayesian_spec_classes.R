# ==============================================================================
# Bayesian Evidence Graph - Spec-Compliant Classes
# ==============================================================================
#
# This module extends base Evidence Graph classes with Bayesian parameters,
# enabling probabilistic inference over explanatory accounts.
#
# PARAMETER OVERVIEW
# ------------------
# BayesianHypothesis:       prior         - P(H=true) before evidence
# BayesianAuxiliary:        prior         - P(Aux=true) background probability
# BayesianExplanatoryLink:  reliability   - P(link | all conditions true)
#                           epsilon       - P(link | conditions not met), ~0.01
# BayesianPhenomenon:       strength      - P(phenomenon | link exists)
#                           base_rate     - P(phenomenon | link absent)
# BayesianResult:           strength      - P(result | link exists)
#                           base_rate     - P(result | link absent)
#
# CPT FORMULAS (from specification)
# ---------------------------------
# Link:   P(EL=true | H, Aux) = reliability if all true, else epsilon
# Target: P(target=true | EL) = strength if EL=true, else base_rate
# Exact:  P(outcome | H, aux) = base_rate + (strength - base_rate) × reliability
#
# DEPENDENCIES
# ------------
# Inherits from: classes.R (Node, Hypothesis, AuxiliaryClaim, etc.)

library(R6)

# ==============================================================================
# DEFAULT PROBABILITY CONSTANTS
# ==============================================================================
# These constants define the default values for Bayesian parameters across
# the codebase. Centralizing them here ensures consistency and makes it
# easy to adjust defaults in one place.

# Link parameters: P(link=true | conditions)
DEFAULT_LINK_RELIABILITY <- 0.95  # P(link | all conditions true)
DEFAULT_LINK_EPSILON <- 0.01      # P(link | conditions not met) - for numerical stability

# Target parameters (Phenomena): P(target=true | link)
DEFAULT_PHENOMENON_STRENGTH <- 0.90   # P(phenomenon | link=true)
DEFAULT_PHENOMENON_BASE_RATE <- 0.05  # P(phenomenon | link=false)

# Target parameters (Results): P(result=true | link)
DEFAULT_RESULT_STRENGTH <- 0.95       # P(result | link=true) - higher for direct evidence
DEFAULT_RESULT_BASE_RATE <- 0.10      # P(result | link=false) - moderate false positive

# Prior defaults
DEFAULT_HYPOTHESIS_PRIOR <- 0.5   # Uninformative prior for hypotheses
DEFAULT_AUXILIARY_PRIOR <- 0.8    # Background assumptions usually likely

# Source base classes if not already loaded (for development mode)
if (!isNamespaceLoaded("ExplanatoryAccounts") && !exists("Node")) {
  source("R/classes.R")
}

# ==============================================================================
# PARAMETER VALIDATION
# ==============================================================================

#' Validate that a value is a probability in [0, 1]
#'
#' @param value Numeric value to validate
#' @param param_name Parameter name for error messages
#' @return Invisible NULL on success; stops with error on failure
validate_probability <- function(value, param_name) {
  if (is.null(value) || !is.numeric(value) || length(value) != 1) {
    stop(sprintf("%s must be a single numeric value, got: %s",
                 param_name, paste(value, collapse = ", ")))
  }

  if (value < 0 || value > 1) {
    stop(sprintf("%s must be between 0 and 1, got: %.4f",
                 param_name, value))
  }

  invisible(NULL)
}

#' Validate Bayesian Hypothesis parameters
#'
#' @param h BayesianHypothesis object
#' @return NULL on success, throws error on failure
validate_bayesian_hypothesis <- function(h) {
  validate_probability(h$prior, "prior")
  invisible(NULL)
}

#' Validate Bayesian Auxiliary parameters
#'
#' @param a BayesianAuxiliary object
#' @return NULL on success, throws error on failure
validate_bayesian_auxiliary <- function(a) {
  validate_probability(a$prior, "prior")
  invisible(NULL)
}

#' Validate Bayesian ExplanatoryLink parameters
#'
#' @param link BayesianExplanatoryLink object
#' @return NULL on success, throws error/warning on issues
validate_bayesian_link <- function(link) {
  validate_probability(link$reliability, "reliability")
  validate_probability(link$epsilon, "epsilon")

  # Epsilon should typically be small (~0.01)
  if (link$epsilon > 0.1) {
    warning(sprintf("Link %s has epsilon = %.3f > 0.1. Epsilon should typically be ~0.01 for numerical stability.",
                   link$id, link$epsilon))
  }

  invisible(NULL)
}

#' Validate Bayesian target node (Phenomenon or Result) parameters
#'
#' @param target BayesianPhenomenon or BayesianResult object
#' @return NULL on success, throws error/warning on issues
validate_bayesian_target <- function(target) {
  validate_probability(target$strength, "strength")
  validate_probability(target$base_rate, "base_rate")

  # Strength should typically be > base_rate
  # (otherwise the explanatory link makes the outcome LESS likely!)
  if (target$strength < target$base_rate) {
    warning(sprintf(
      "Target %s has strength (%.3f) < base_rate (%.3f). This means the explanatory link makes the outcome LESS likely, which is unusual.",
      target$id, target$strength, target$base_rate
    ))
  }

  invisible(NULL)
}

# ==============================================================================
# BAYESIAN HYPOTHESIS
# ==============================================================================

#' BayesianHypothesis - Hypothesis with prior probability
#'
#' Extends the base Hypothesis class with a prior probability parameter.
#'
#' @field prior P(Hypothesis=true) - baseline probability before evidence
#'
#' @examples
#' h <- BayesianHypothesis$new(
#'   id = "H1",
#'   text = "Aerosol transmission is primary mode",
#'   source = "Hypothesis",
#'   prior = 0.5  # Uninformed prior
#' )
BayesianHypothesis <- R6Class("BayesianHypothesis",
  inherit = Hypothesis,
  public = list(
    prior = NULL,  # P(Hypothesis=true), default 0.5

    #' @description Create a new BayesianHypothesis
    #' @param id Unique identifier
    #' @param text Natural language description
    #' @param source Citation or provenance
    #' @param prior Prior probability P(H=true), default 0.5
    #' @param level "primary" or "secondary"
    #' @param notes Optional annotations
    initialize = function(id, text, source,
                         prior = DEFAULT_HYPOTHESIS_PRIOR, level = "primary", notes = NULL) {
      # Call parent constructor
      super$initialize(id, text, source, level, notes)

      # Set and validate Bayesian parameter
      self$prior <- prior
      validate_bayesian_hypothesis(self)
    },

    #' @description Convert to plain list for serialization
    to_list = function() {
      c(super$to_list(), list(prior = self$prior))
    }
  )
)

# ==============================================================================
# BAYESIAN AUXILIARY
# ==============================================================================

#' BayesianAuxiliary - Auxiliary assumption with prior probability
#'
#' Extends the base AuxiliaryClaim class with a prior probability parameter.
#'
#' @field prior P(Auxiliary=true) - baseline probability this condition holds
#'
#' @examples
#' a <- BayesianAuxiliary$new(
#'   id = "A1",
#'   text = "Virus remains viable in aerosols >2 hours",
#'   source = "Literature",
#'   prior = 0.8  # High probability based on studies
#' )
BayesianAuxiliary <- R6Class("BayesianAuxiliary",
  inherit = AuxiliaryClaim,
  public = list(
    prior = NULL,  # P(Auxiliary=true), default 0.5

    #' @description Create a new BayesianAuxiliary
    #' @param id Unique identifier
    #' @param text Natural language description
    #' @param source Citation or provenance
    #' @param prior Prior probability P(A=true), default 0.5
    #' @param subtype One of AUXILIARY_SUBTYPES
    #' @param notes Optional annotations
    initialize = function(id, text, source,
                         prior = 0.5, subtype = "BackgroundTheory", notes = NULL) {
      # Call parent constructor
      super$initialize(id, text, source, subtype, notes)

      # Set and validate Bayesian parameter
      self$prior <- prior
      validate_bayesian_auxiliary(self)
    },

    #' @description Convert to plain list for serialization
    to_list = function() {
      c(super$to_list(), list(prior = self$prior))
    }
  )
)

# ==============================================================================
# BAYESIAN EXPLANATORY LINK
# ==============================================================================

#' BayesianExplanatoryLink - Link with reliability and epsilon parameters
#'
#' Extends the base ExplanatoryLink class with reliability and epsilon parameters
#' following the specification's CPT formula:
#'   P(EL=true | H, Aux1, ..., AuxN) = reliability if all conditions true, else epsilon
#'
#' @field reliability P(link exists | hypothesis true, all auxiliaries true)
#'   Represents confidence in the explanatory mechanism.
#' @field epsilon P(link exists | conditions not met), typically 0.01
#'   Small probability for numerical stability.
#'
#' Note: source_id can be a single ID or a vector of IDs for conjunctive explanations
#' where multiple phenomena together explain a result through one link.
#'
#' @examples
#' # Single source
#' link <- BayesianExplanatoryLink$new(
#'   id = "EL1",
#'   source_id = "H1",
#'   target_id = "P1",
#'   text = "Aerosol mechanism explains ventilation effect",
#'   source = "Theory",
#'   reliability = 0.95,  # High confidence in mechanism
#'   epsilon = 0.01       # Standard stability value
#' )
#'
#' # Multiple sources (conjunctive)
#' link <- BayesianExplanatoryLink$new(
#'   id = "EL2",
#'   source_id = c("PC1", "PC2"),  # Both phenomena together explain R1
#'   target_id = "R1",
#'   text = "Close-range + ventilation together explain outbreak patterns",
#'   source = "Theory",
#'   reliability = 0.95,
#'   epsilon = 0.01
#' )
BayesianExplanatoryLink <- R6Class("BayesianExplanatoryLink",
  inherit = ExplanatoryLink,
  public = list(
    reliability = NULL,  # P(link exists | H=true, all aux=true)
    epsilon = NULL,      # P(link exists | conditions not met), default 0.01

    #' @description Create a new BayesianExplanatoryLink
    #' @param id Unique identifier
    #' @param source_id ID of explaining node
    #' @param target_id ID of explained node
    #' @param text Natural language description
    #' @param source Citation or provenance
    #' @param reliability P(link | H=true, all aux=true), default 0.95
    #' @param epsilon P(link | conditions not met), default 0.01
    #' @param evidential_strength Optional strength parameter
    #' @param notes Optional annotations
    initialize = function(id, source_id, target_id, text, source,
                         reliability = DEFAULT_LINK_RELIABILITY, epsilon = DEFAULT_LINK_EPSILON,
                         evidential_strength = NULL, notes = NULL) {
      # Call parent constructor
      super$initialize(id, source_id, target_id, text, source,
                      evidential_strength, notes)

      # Set and validate Bayesian parameters
      self$reliability <- reliability
      self$epsilon <- epsilon
      validate_bayesian_link(self)
    },

    #' @description Convert to plain list for serialization
    to_list = function() {
      c(super$to_list(),
        list(reliability = self$reliability, epsilon = self$epsilon))
    }
  )
)

# ==============================================================================
# BAYESIAN PHENOMENON
# ==============================================================================

#' BayesianPhenomenon - Phenomenon with strength and base_rate parameters
#'
#' Extends the base PhenomenonClaim class with strength and base_rate parameters
#' following the specification's CPT formula:
#'   P(phenomenon=true | EL) = strength if EL=true, else base_rate
#'
#' @field strength P(phenomenon=true | link exists)
#'   If explanatory mechanism holds, how likely is the phenomenon?
#' @field base_rate P(phenomenon=true | link doesn't exist)
#'   Probability through alternative unmodeled pathways.
#'
#' @examples
#' p <- BayesianPhenomenon$new(
#'   id = "P1",
#'   text = "Ventilation strongly affects transmission",
#'   source = "Observation",
#'   strength = 0.90,    # High probability if mechanism holds
#'   base_rate = 0.05    # Low probability through other pathways
#' )
BayesianPhenomenon <- R6Class("BayesianPhenomenon",
  inherit = PhenomenonClaim,
  public = list(
    strength = NULL,    # P(phenomenon=true | link exists)
    base_rate = NULL,   # P(phenomenon=true | link doesn't exist)

    #' @description Create a new BayesianPhenomenon
    #' @param id Unique identifier
    #' @param text Natural language description
    #' @param source Citation or provenance
    #' @param strength P(phenomenon | link exists), default 0.90
    #' @param base_rate P(phenomenon | link doesn't exist), default 0.05
    #' @param notes Optional annotations
    initialize = function(id, text, source,
                         strength = DEFAULT_PHENOMENON_STRENGTH, base_rate = DEFAULT_PHENOMENON_BASE_RATE, notes = NULL) {
      # Call parent constructor
      super$initialize(id, text, source, notes)

      # Set and validate Bayesian parameters
      self$strength <- strength
      self$base_rate <- base_rate
      validate_bayesian_target(self)
    },

    #' @description Convert to plain list for serialization
    to_list = function() {
      c(super$to_list(),
        list(strength = self$strength, base_rate = self$base_rate))
    }
  )
)

# ==============================================================================
# BAYESIAN RESULT
# ==============================================================================

#' BayesianResult - Empirical result with strength and base_rate parameters
#'
#' Extends the base EmpiricalResult class with strength and base_rate parameters.
#' Follows the same CPT formula as BayesianPhenomenon.
#'
#' @field strength P(result=true | link exists)
#' @field base_rate P(result=true | link doesn't exist)
#'
#' @examples
#' r <- BayesianResult$new(
#'   id = "R1",
#'   text = "Studies show 5-10x risk reduction with ventilation",
#'   source = "Meta-analysis",
#'   strength = 0.95,      # High detection probability if phenomenon true
#'   base_rate = 0.10,     # Moderate false positive rate
#'   result_type = "Quantitative",
#'   sample_size = 150
#' )
BayesianResult <- R6Class("BayesianResult",
  inherit = EmpiricalResult,
  public = list(
    strength = NULL,    # P(result=true | link exists)
    base_rate = NULL,   # P(result=true | link doesn't exist)

    #' @description Create a new BayesianResult
    #' @param id Unique identifier
    #' @param text Natural language description
    #' @param source Citation or provenance
    #' @param strength P(result | link exists), default 0.95
    #' @param base_rate P(result | link doesn't exist), default 0.10
    #' @param result_type "Quantitative" or "Qualitative"
    #' @param method_description How data was collected
    #' @param sample_size For quantitative results
    #' @param notes Optional annotations
    initialize = function(id, text, source,
                         strength = DEFAULT_RESULT_STRENGTH, base_rate = DEFAULT_RESULT_BASE_RATE,
                         result_type = "Quantitative",
                         method_description = NULL,
                         sample_size = NULL, notes = NULL) {
      # Call parent constructor
      super$initialize(id, text, source,
                      result_type, method_description, sample_size, notes)

      # Set and validate Bayesian parameters
      self$strength <- strength
      self$base_rate <- base_rate
      validate_bayesian_target(self)
    },

    #' @description Convert to plain list for serialization
    to_list = function() {
      c(super$to_list(),
        list(strength = self$strength, base_rate = self$base_rate))
    }
  )
)
