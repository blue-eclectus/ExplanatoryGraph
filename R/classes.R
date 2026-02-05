# ==============================================================================
# Evidence Graph Framework - Core R6 Classes
# ==============================================================================
#
# This module defines the foundational data model for representing scientific
# arguments as directed acyclic graphs (DAGs). Unlike knowledge graphs that
# represent factual relationships, evidence graphs capture the *explanatory*
# structure of how hypotheses account for empirical evidence.
#
# CONCEPTUAL OVERVIEW
# -------------------
# An evidence graph models the logical structure of scientific arguments:
#
#   Hypothesis --explains--> Phenomenon --explains--> EmpiricalResult
#              \                       /
#               `--requires--> Auxiliary
#
# Key insight: "Explains" relationships are REIFIED as ExplanatoryLink nodes,
# allowing us to attach auxiliary assumptions to specific explanatory steps
# and track which links are eliminated during analysis.
#
# ARCHITECTURE
# ------------
# - R6 classes provide object-oriented design with inheritance
# - EvidenceGraph stores nodes (R6 objects) + edges (data.frame)
# - igraph is used internally for path-finding algorithms
# - All eliminable entities (auxiliaries, links) track elimination state
#
# NODE TYPES
# ----------
# - Hypothesis: Claims under evaluation (primary or secondary)
# - PhenomenonClaim: General patterns that need explanation
# - AuxiliaryClaim: Background assumptions required for explanations
# - EmpiricalResult: Observed data to be explained
# - ExplanatoryLink: Reified "explains" relationship (the key innovation)
#
# EDGE TYPES
# ----------
# - FROM_SOURCE: Source → ExplanatoryLink (first half of explanation)
# - TO_TARGET: ExplanatoryLink → Target (second half of explanation)
# - IS_REQUIRED_BY: Auxiliary → ExplanatoryLink (dependency)
#
# USAGE
# -----
# graph <- EvidenceGraph$new("My Analysis")
# graph$add_node(Hypothesis$new("H1", "Theory A explains X", "Paper"))
# graph$add_node(PhenomenonClaim$new("P1", "Pattern X observed", "Data"))
# link <- graph$create_explanatory_link("H1", "P1")

library(R6)
library(igraph)
library(jsonlite)

# ==============================================================================
# TYPE CONSTANTS (Enum-like validation sets)
# ==============================================================================

# Valid node types in the evidence graph
NODE_TYPES <- c(
  "Hypothesis",          # Claim under evaluation (what we're testing)
  "PhenomenonClaim",     # General pattern requiring explanation
  "AuxiliaryClaim",      # Background assumption (Duhem-Quine thesis)
  "EmpiricalResult",     # Observed data (evidence corpus)
  "ExplanatoryLink",     # Reified "explains" relationship
  "Selector",            # Multi-state node enforcing mutual exclusivity
  "Account"              # Specific explanatory pathway (hypothesis → evidence)
)

# Subtypes for auxiliary claims (helps categorize assumptions)
AUXILIARY_SUBTYPES <- c(
  "BackgroundTheory",           # Established principles (e.g., physics laws)
  "MethodologicalAssumption",   # Assumptions about measurement/collection
  "DataProductionClaim"         # Assumptions about data generation/interpretation
)

# Edge types define structural relationships.
# Explanatory relationships are split into TWO edges via reification:
#   Source --FROM_SOURCE--> ExplanatoryLink --TO_TARGET--> Target
# This enables ExplanatoryLinks to appear in paths for auxiliary tracking.
EDGE_TYPES <- c(
  "FROM_SOURCE",      # Source → ExplanatoryLink (explanation origin)
  "TO_TARGET",        # ExplanatoryLink → Target (explanation destination)
  "IS_REQUIRED_BY"    # Auxiliary → ExplanatoryLink (prerequisite dependency)
)

# ==============================================================================
# BASE NODE CLASS
# ==============================================================================

#' Node - Abstract base class for all graph nodes
#'
#' All specialized node types inherit from this class. Provides common fields
#' and serialization. Do not instantiate directly; use subclasses.
#'
#' @field id Unique identifier (string, used as key in graph$nodes)
#' @field type One of NODE_TYPES (set by subclass constructor)
#' @field text Human-readable description of the claim/node
#' @field source Citation, provenance, or data origin
#' @field notes Optional annotations for analysis
Node <- R6Class("Node",
  public = list(
    id = NULL,
    type = NULL,
    text = NULL,
    source = NULL,
    notes = NULL,

    #' @description Create a node (called by subclass constructors)
    #' @param id Unique identifier
    #' @param type Node type from NODE_TYPES
    #' @param text Description text
    #' @param source Citation/provenance
    #' @param notes Optional annotations
    initialize = function(id, type, text, source, notes = NULL) {
      self$id <- id
      self$type <- type
      self$text <- text
      self$source <- source
      self$notes <- notes
    },

    #' @description Convert to plain list for JSON export or storage
    #' @return Named list of all fields
    to_list = function() {
      list(
        id = self$id,
        type = self$type,
        text = self$text,
        source = self$source,
        notes = self$notes
      )
    }
  )
)

# ==============================================================================
# HYPOTHESIS NODE
# ==============================================================================

#' Hypothesis - Claim under evaluation
#'
#' Represents a theory or explanation being tested against evidence. In an
#' evidence graph, hypotheses sit at the top of explanatory chains.
#'
#' @field level "primary" (main hypothesis) or "secondary" (alternative)
#'
#' @examples
#' h <- Hypothesis$new(
#'   id = "H1",
#'   text = "Aerosol transmission is the primary mode",
#'   source = "Theory",
#'   level = "primary"
#' )
Hypothesis <- R6Class("Hypothesis",
  inherit = Node,
  public = list(
    level = NULL,

    initialize = function(id, text, source, level = "primary", notes = NULL) {
      super$initialize(id, "Hypothesis", text, source, notes)
      self$level <- level
    },

    to_list = function() {
      c(super$to_list(), list(level = self$level))
    }
  )
)

# ==============================================================================
# PHENOMENON CLAIM NODE
# ==============================================================================

#' PhenomenonClaim - General pattern or regularity to be explained
#'
#' Represents intermediate explanatory targets between hypotheses and raw data.
#' Hypotheses explain phenomena, which in turn explain empirical results.
#'
#' @examples
#' p <- PhenomenonClaim$new(
#'   id = "P1",
#'   text = "Ventilation strongly affects transmission rates",
#'   source = "Epidemiological observations"
#' )
PhenomenonClaim <- R6Class("PhenomenonClaim",
  inherit = Node,
  public = list(
    initialize = function(id, text, source, notes = NULL) {
      super$initialize(id, "PhenomenonClaim", text, source, notes)
    }
  )
)

# ==============================================================================
# AUXILIARY CLAIM NODE
# ==============================================================================

#' AuxiliaryClaim - Background assumption required for explanations
#'
#' Auxiliaries represent the "hidden" assumptions that connect hypotheses to
#' evidence (Duhem-Quine thesis). They can be eliminated to test the robustness
#' of an explanatory account—if an auxiliary fails, dependent links are broken.
#'
#' @field subtype Category from AUXILIARY_SUBTYPES (BackgroundTheory, etc.)
#' @field eliminated Whether this auxiliary has been ruled out
#' @field elimination_reason Explanation for why it was eliminated
#'
#' @examples
#' aux <- AuxiliaryClaim$new(
#'   id = "A1",
#'   text = "Virus remains viable in aerosols for >2 hours",
#'   source = "Lab studies",
#'   subtype = "BackgroundTheory"
#' )
#' # Later, during analysis:
#' aux$eliminate("New study shows rapid inactivation")
AuxiliaryClaim <- R6Class("AuxiliaryClaim",
  inherit = Node,
  public = list(
    subtype = NULL,
    eliminated = FALSE,
    elimination_reason = NULL,

    initialize = function(id, text, source,
                         subtype = "BackgroundTheory", notes = NULL) {
      super$initialize(id, "AuxiliaryClaim", text, source, notes)
      self$subtype <- subtype
    },

    #' @description Mark this auxiliary as eliminated (cascades to dependent links)
    #' @param reason Human-readable explanation for elimination
    eliminate = function(reason) {
      self$eliminated <- TRUE
      self$elimination_reason <- reason
      self$notes <- paste("ELIMINATED:", reason)
    },

    to_list = function() {
      c(super$to_list(),
        list(
          subtype = self$subtype,
          eliminated = self$eliminated,
          elimination_reason = self$elimination_reason
        ))
    }
  )
)

# ==============================================================================
# EMPIRICAL RESULT NODE
# ==============================================================================

#' EmpiricalResult - Observed data or measurement to be explained
#'
#' Represents the evidence corpus—specific observations that hypotheses must
#' account for. In Bayesian mode, results are typically conditioned as TRUE
#' (observed) and we compute which hypotheses best explain them.
#'
#' @field result_type "Quantitative" (measurements) or "Qualitative" (case studies)
#' @field method_description How the data was collected/measured
#' @field sample_size Sample size for quantitative results
#'
#' @examples
#' r <- EmpiricalResult$new(
#'   id = "R1",
#'   text = "5-10x risk reduction with improved ventilation",
#'   source = "Meta-analysis of 15 studies",
#'   result_type = "Quantitative",
#'   sample_size = 1500
#' )
EmpiricalResult <- R6Class("EmpiricalResult",
  inherit = Node,
  public = list(
    result_type = NULL,
    method_description = NULL,
    sample_size = NULL,

    initialize = function(id, text, source,
                         result_type = "Quantitative",
                         method_description = NULL,
                         sample_size = NULL,
                         notes = NULL) {
      super$initialize(id, "EmpiricalResult", text, source, notes)
      self$result_type <- result_type
      self$method_description <- method_description
      self$sample_size <- sample_size
    },

    to_list = function() {
      c(super$to_list(),
        list(
          result_type = self$result_type,
          method_description = self$method_description,
          sample_size = self$sample_size
        ))
    }
  )
)

# ==============================================================================
# EXPLANATORY LINK NODE (Reified Edge)
# ==============================================================================

#' ExplanatoryLink - Reified "explains" relationship
#'
#' The key architectural innovation: explanatory relationships are represented
#' as first-class nodes rather than simple edges. This enables:
#'
#' 1. Attaching auxiliary assumptions to specific explanatory steps
#' 2. Eliminating individual links when auxiliaries fail
#' 3. Including links in paths for coherence checking and CPT computation
#'
#' STRUCTURE:
#'   Source --FROM_SOURCE--> ExplanatoryLink --TO_TARGET--> Target
#'
#' Conjunctive explanations are supported: source_ids can be a vector when
#' multiple phenomena together explain a result through one link.
#'
#' @field source_ids Vector of explaining node IDs (single or conjunctive)
#' @field target_id ID of the explained node
#' @field evidential_strength Optional strength measure
#' @field required_auxiliary_ids Auxiliaries that must hold for this link
#' @field eliminated Whether this link has been ruled out
#' @field elimination_reason Why it was eliminated
#' @field cpt Conditional probability table (for Bayesian mode)
#'
#' @examples
#' # Simple link: H1 explains P1
#' link <- ExplanatoryLink$new(
#'   id = "EL1",
#'   source_id = "H1",
#'   target_id = "P1",
#'   text = "Aerosol theory explains ventilation effect",
#'   source = "Theory"
#' )
#' # Conjunctive link: PC1 AND PC2 together explain R1
#' link <- ExplanatoryLink$new(
#'   id = "EL2",
#'   source_id = c("PC1", "PC2"),
#'   target_id = "R1",
#'   text = "Both factors together explain outcome",
#'   source = "Theory"
#' )
ExplanatoryLink <- R6Class("ExplanatoryLink",
  inherit = Node,
  public = list(
    source_ids = NULL,
    target_id = NULL,
    evidential_strength = NULL,
    required_auxiliary_ids = NULL,
    eliminated = FALSE,
    elimination_reason = NULL,
    cpt = NULL,

    initialize = function(id, source_id, target_id, text, source,
                         evidential_strength = NULL, notes = NULL) {
      super$initialize(id, "ExplanatoryLink", text, source, notes)
      self$source_ids <- as.character(source_id)  # Supports vector for conjunctive
      self$target_id <- target_id
      self$evidential_strength <- evidential_strength
      self$required_auxiliary_ids <- character(0)
    },

    #' @description Mark this link as eliminated
    #' @param reason Human-readable explanation
    eliminate = function(reason) {
      self$eliminated <- TRUE
      self$elimination_reason <- reason
      self$notes <- paste("ELIMINATED:", reason)
    },

    #' @description Assign a validated CPT to this link (Bayesian mode)
    #' @param cpt CPT list with states, parents, probabilities
    set_cpt = function(cpt) {
      if (!isNamespaceLoaded("ExplanatoryAccounts") && !exists("validate_cpt")) source("R/cpt_utils.R")
      validation <- validate_cpt(cpt)
      if (!validation$valid) {
        stop(paste("Invalid CPT for", self$id, ":",
                  paste(validation$errors, collapse = ", ")))
      }
      self$cpt <- cpt
      invisible(self)
    },

    #' @description Check if a CPT has been assigned
    #' @return TRUE if CPT exists, FALSE otherwise
    has_cpt = function() {
      !is.null(self$cpt) && length(self$cpt) > 0
    },

    to_list = function() {
      c(super$to_list(),
        list(
          source_ids = self$source_ids,
          target_id = self$target_id,
          evidential_strength = self$evidential_strength,
          eliminated = self$eliminated,
          elimination_reason = self$elimination_reason,
          has_cpt = self$has_cpt()
        ))
    }
  )
)

# ==============================================================================
# EVIDENCE GRAPH CLASS
# ==============================================================================

#' EvidenceGraph - Main container for evidence graph analysis
#'
#' The central data structure that manages nodes, edges, and graph operations.
#' Nodes are stored as R6 objects in a named list; edges are stored in a
#' data.frame for efficiency. Path-finding uses igraph internally.
#'
#' @field name User-settable graph name/description
#' @field nodes Named list: node_id → Node object
#' @field edges Data.frame with columns: from, to, type (from EDGE_TYPES)
#' @field cpts Named list of CPTs for Bayesian inference
#' @field link_counter Auto-incrementing counter for unique link IDs
#'
#' @examples
#' # Create graph and add nodes
#' graph <- EvidenceGraph$new("COVID Analysis")
#' graph$add_node(Hypothesis$new("H1", "Aerosol transmission", "Theory"))
#' graph$add_node(PhenomenonClaim$new("P1", "Ventilation effect", "Data"))
#'
#' # Create explanatory relationship (reified as ExplanatoryLink)
#' link <- graph$create_explanatory_link("H1", "P1")
#'
#' # Find path from hypothesis to evidence
#' path <- graph$find_explanatory_path("H1", "P1")
#' # Returns: c("H1", "EL_H1_P1_001", "P1")
EvidenceGraph <- R6Class("EvidenceGraph",
  public = list(
    name = NULL,
    nodes = NULL,
    edges = NULL,
    cpts = NULL,
    link_counter = 0,

    initialize = function(name = "Untitled Graph") {
      self$name <- name
      self$nodes <- list()
      self$edges <- data.frame(
        from = character(),
        to = character(),
        type = character(),
        stringsAsFactors = FALSE
      )
      self$link_counter <- 0
      self$cpts <- list()
    },

    add_node = function(node) {
      if (node$id %in% names(self$nodes)) {
        stop(paste("Node", node$id, "already exists"))
      }
      self$nodes[[node$id]] <- node
      invisible(self)
    },

    add_edge = function(from_id, to_id, edge_type) {
      # V4: Edge type validation
      if (!(edge_type %in% EDGE_TYPES)) {
        stop(paste("Invalid edge type:", edge_type))
      }

      if (!(from_id %in% names(self$nodes))) {
        stop(paste("Source node", from_id, "not found"))
      }
      if (!(to_id %in% names(self$nodes))) {
        stop(paste("Target node", to_id, "not found"))
      }

      new_edge <- data.frame(
        from = from_id,
        to = to_id,
        type = edge_type,
        stringsAsFactors = FALSE
      )
      self$edges <- rbind(self$edges, new_edge)

      invisible(self)
    },

    create_explanatory_link = function(source_id, target_id,
                                      auxiliary_ids = c(),
                                      evidential_strength = NULL) {
      # Validate source and target
      source_node <- self$nodes[[source_id]]
      target_node <- self$nodes[[target_id]]

      if (is.null(source_node)) {
        stop(paste("Source node", source_id, "not found"))
      }
      if (is.null(target_node)) {
        stop(paste("Target node", target_id, "not found"))
      }

      # Validate types
      if (!(source_node$type %in% c("Hypothesis", "PhenomenonClaim"))) {
        stop("Source must be Hypothesis or PhenomenonClaim")
      }
      if (!(target_node$type %in% c("PhenomenonClaim", "EmpiricalResult"))) {
        stop("Target must be PhenomenonClaim or EmpiricalResult")
      }

      # Generate unique ID with counter to prevent collisions
      self$link_counter <- self$link_counter + 1
      link_id <- sprintf("EL_%s_%s_%03d",
                        source_id, target_id, self$link_counter)

      link_text <- paste0(
        substr(source_node$text, 1, 30), "... explains ",
        substr(target_node$text, 1, 30), "..."
      )

      link <- ExplanatoryLink$new(
        id = link_id,
        source_id = source_id,
        target_id = target_id,
        text = link_text,
        source = "Generated",
        evidential_strength = evidential_strength
      )

      self$add_node(link)

      # Create structural edges
      self$add_edge(source_id, link_id, "FROM_SOURCE")
      self$add_edge(link_id, target_id, "TO_TARGET")

      # Add auxiliary dependencies
      for (aux_id in auxiliary_ids) {
        if (!(aux_id %in% names(self$nodes))) {
          stop(paste("Auxiliary", aux_id, "not found"))
        }
        if (self$nodes[[aux_id]]$type != "AuxiliaryClaim") {
          stop(paste("Node", aux_id, "is not an auxiliary"))
        }
        self$add_edge(aux_id, link_id, "IS_REQUIRED_BY")
      }

      return(link)
    },

    get_required_auxiliaries = function(link_id) {
      aux_edges <- self$edges[
        self$edges$to == link_id & self$edges$type == "IS_REQUIRED_BY",
      ]
      aux_ids <- aux_edges$from
      return(self$nodes[aux_ids])
    },

    get_links_requiring_auxiliary = function(aux_id) {
      link_edges <- self$edges[
        self$edges$from == aux_id & self$edges$type == "IS_REQUIRED_BY",
      ]
      link_ids <- link_edges$to
      return(self$nodes[link_ids])
    },

    eliminate_auxiliary = function(aux_id, reason) {
      auxiliary <- self$nodes[[aux_id]]
      if (is.null(auxiliary) || auxiliary$type != "AuxiliaryClaim") {
        stop(paste(aux_id, "is not a valid auxiliary"))
      }

      # Mark auxiliary as eliminated
      auxiliary$eliminate(reason)

      # Eliminate dependent links
      dependent_links <- self$get_links_requiring_auxiliary(aux_id)
      eliminated_ids <- character()

      for (link in dependent_links) {
        link$eliminate(paste("Required auxiliary", aux_id,
                           "eliminated:", reason))
        eliminated_ids <- c(eliminated_ids, link$id)
      }

      return(eliminated_ids)
    },

    # ==========================================================================
    # PATH FINDING
    # ==========================================================================

    #' Find explanatory path between two nodes
    #'
    #' Returns the shortest path from start to end, INCLUDING ExplanatoryLink
    #' nodes. This is critical for:
    #' - Coherence checking (detecting eliminated links)
    #' - Auxiliary tracking (finding which auxiliaries a path requires)
    #' - CPT computation (identifying all conditional dependencies)
    #'
    #' RETURN FORMAT:
    #' Full path including links: c("H1", "EL_H1_P1_001", "P1", "EL_P1_R1_002", "R1")
    #' Use get_semantic_path() for display: c("H1", "P1", "R1")
    #'
    #' @param start_id ID of starting node (typically hypothesis)
    #' @param end_id ID of ending node (typically result)
    #' @param include_eliminated If TRUE, include eliminated links in paths
    #' @return Character vector of node IDs forming the path, or NULL if no path
    find_explanatory_path = function(start_id, end_id,
                                    include_eliminated = FALSE) {
      # Create empty graph with named vertices (includes ALL nodes, even links)
      node_names <- names(self$nodes)
      g_simple <- make_empty_graph(n = 0)
      g_simple <- add_vertices(g_simple, length(node_names),
                               name = node_names)

      # Separate FROM_SOURCE and TO_TARGET edges for processing
      from_edges <- self$edges[self$edges$type == "FROM_SOURCE", ]
      to_edges <- self$edges[self$edges$type == "TO_TARGET", ]

      vnames <- V(g_simple)$name

      # Add FROM_SOURCE edges (Source -> ExplanatoryLink)
      # These form the first half of each explanatory relationship
      for (i in seq_len(nrow(from_edges))) {
        link_id <- from_edges$to[i]  # The 'to' is the ExplanatoryLink
        link <- self$nodes[[link_id]]

        # Skip eliminated links unless explicitly requested
        if (!include_eliminated &&
            inherits(link, "ExplanatoryLink") &&
            link$eliminated) {
          next
        }

        source_node <- from_edges$from[i]
        from_idx <- match(source_node, vnames)
        to_idx <- match(link_id, vnames)

        if (!is.na(from_idx) && !is.na(to_idx)) {
          g_simple <- add_edges(g_simple, c(from_idx, to_idx))
        }
      }

      # Add TO_TARGET edges (ExplanatoryLink -> Target)
      # These form the second half of each explanatory relationship
      for (i in seq_len(nrow(to_edges))) {
        link_id <- to_edges$from[i]  # The 'from' is the ExplanatoryLink
        link <- self$nodes[[link_id]]

        # Skip eliminated links unless explicitly requested
        if (!include_eliminated &&
            inherits(link, "ExplanatoryLink") &&
            link$eliminated) {
          next
        }

        target_node <- to_edges$to[i]
        from_idx <- match(link_id, vnames)
        to_idx <- match(target_node, vnames)

        if (!is.na(from_idx) && !is.na(to_idx)) {
          g_simple <- add_edges(g_simple, c(from_idx, to_idx))
        }
      }

      # Find shortest path using igraph algorithm
      tryCatch({
        path <- shortest_paths(g_simple, from = start_id, to = end_id,
                              output = "vpath")$vpath[[1]]

        if (length(path) == 0) {
          return(NULL)
        }

        # V4: Return vertex names INCLUDING ExplanatoryLink nodes
        # Example return: c("H1", "EL_H1_P2_001", "P2", "EL_P2_R4_002", "R4")
        return(V(g_simple)$name[path])
      }, error = function(e) {
        return(NULL)
      })
    },

    #' Convert full path to semantic path (display helper)
    #'
    #' Strips out ExplanatoryLink nodes for cleaner display:
    #'   Full path:     c("H1", "EL_H1_P1_001", "P1", "EL_P1_R1_002", "R1")
    #'   Semantic path: c("H1", "P1", "R1")
    #'
    #' Use for UI display and narrative explanations. Do NOT use for coherence
    #' checking or auxiliary tracking—those need the full path.
    #'
    #' @param path Character vector from find_explanatory_path()
    #' @return Character vector with only hypothesis/phenomenon/result nodes
    get_semantic_path = function(path) {
      if (is.null(path) || length(path) == 0) {
        return(NULL)
      }

      # Filter out ExplanatoryLink nodes, keeping only H/P/R nodes
      semantic_path <- character()
      for (node_id in path) {
        node <- self$nodes[[node_id]]
        if (!inherits(node, "ExplanatoryLink")) {
          semantic_path <- c(semantic_path, node_id)
        }
      }
      return(semantic_path)
    },

    to_json = function() {
      nodes_list <- lapply(self$nodes, function(n) n$to_list())
      jsonlite::toJSON(
        list(
          name = self$name,
          nodes = nodes_list,
          edges = self$edges,
          link_counter = self$link_counter
        ),
        pretty = TRUE,
        auto_unbox = TRUE
      )
    },

    save = function(filepath) {
      # Ensure directory exists
      dir_path <- dirname(filepath)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
      saveRDS(self, filepath)
    },

    # ==========================================================================
    # BAYESIAN MODE METHODS
    # ==========================================================================
    # These methods support CPT generation and belief propagation.

    #' Get parent node IDs for Bayesian network construction
    #'
    #' Returns parents based on edge types:
    #' - Regular nodes: incoming TO_TARGET edges (from ExplanatoryLinks)
    #' - Link nodes: FROM_SOURCE (sources) + IS_REQUIRED_BY (auxiliaries)
    #'
    #' @param node_id ID of node to get parents for
    #' @return Character vector of parent node IDs
    get_parents = function(node_id) {
      if (!(node_id %in% names(self$nodes))) {
        stop(paste("Node", node_id, "not found"))
      }

      # Find edges where this node is the target
      # For regular nodes (P1, R1): TO_TARGET edges give us the links
      # For link nodes: FROM_SOURCE and IS_REQUIRED_BY edges give us sources and auxiliaries
      parent_edges <- self$edges[
        self$edges$to == node_id &
        self$edges$type %in% c("FROM_SOURCE", "TO_TARGET", "IS_REQUIRED_BY"),
      ]

      return(unique(parent_edges$from))
    },

    #' Get node IDs that are missing CPTs
    #' @return Character vector of node IDs without CPTs assigned
    get_nodes_missing_cpts = function() {
      missing <- character()
      for (node_id in names(self$nodes)) {
        node <- self$nodes[[node_id]]
        # Only ExplanatoryLinks need CPTs for now
        # (Other nodes can have CPTs added later if needed)
        if (inherits(node, "ExplanatoryLink") &&
            (is.null(node$cpt) || length(node$cpt) == 0)) {
          missing <- c(missing, node_id)
        }
      }
      return(missing)
    },

    #' Auto-generate default CPTs for all nodes
    #'
    #' Convenience method for quick Bayesian setup. Generated CPTs can be
    #' overridden with custom values afterward.
    #'
    #' @param mode CPT generation mode: "noisy_or", "uniform", or "deterministic"
    #' @return self (invisible, for chaining)
    auto_generate_cpts = function(mode = "noisy_or") {
      # Source CPT utilities (for development mode)
      if (!isNamespaceLoaded("ExplanatoryAccounts") && !exists("generate_default_cpt")) {
        source("R/cpt_utils.R")
      }

      # Clear existing CPTs
      self$cpts <- list()

      for (node_id in names(self$nodes)) {
        node <- self$nodes[[node_id]]

        # Get parents for CPT conditioning
        parents <- self$get_parents(node_id)

        # Generate default CPT
        cpt <- generate_default_cpt(node_id, parents, mode)

        # Store CPT in graph's CPT collection
        self$cpts[[node_id]] <- cpt

        # Also attach CPT to link node for backward compatibility
        if (inherits(node, "ExplanatoryLink")) {
          node$cpt <- cpt
        }
      }

      invisible(self)
    },

    #' Set analysis mode (placeholder for UI integration)
    #' @param new_mode "qualitative" or "quantitative"
    set_mode = function(new_mode) {
      if (!(new_mode %in% c("qualitative", "quantitative"))) {
        stop("Mode must be 'qualitative' or 'quantitative'")
      }

      # Validate can switch to quantitative
      if (new_mode == "quantitative") {
        missing_cpts <- self$get_nodes_missing_cpts()
        if (length(missing_cpts) > 0) {
          warning(paste("Nodes missing CPTs:",
                       paste(missing_cpts, collapse = ", "),
                       "\nUse auto_generate_cpts() to create default CPTs"))
        }
      }

      # For now, mode is not stored as a field
      # This method is a placeholder for UI integration
      # In full implementation, would add: self$mode <- new_mode

      invisible(self)
    }
  ),

  active = list(
    node_count = function() length(self$nodes),
    edge_count = function() nrow(self$edges)
  )
)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Load a saved EvidenceGraph from file
#'
#' @param filepath Path to RDS file saved with graph$save()
#' @return EvidenceGraph object
#'
#' @examples
#' graph$save("my_analysis.rds")
#' graph <- load_evidence_graph("my_analysis.rds")
load_evidence_graph <- function(filepath) {
  readRDS(filepath)
}
