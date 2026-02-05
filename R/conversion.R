# ==============================================================================
# Evidence Graph Framework - Graph Conversion Module
# ==============================================================================
#
# Converts EvidenceGraph objects to graphNEL format for Bayesian inference.
# graphNEL is the standard graph representation used by Bioconductor's gRain
# package for belief propagation.
#
# KEY FUNCTIONS
# -------------
# evidence_graph_to_graphNEL()  Main conversion (filters eliminated links)
# validate_dag()                Check for cycles (must be acyclic)
# create_node_mapping()         Bidirectional ID ↔ index mapping
# get_node_parents()            Find semantic parents for CPT conditioning
#
# IMPORTANT: Eliminated links are EXCLUDED from conversion
# This ensures Bayesian inference only uses valid explanatory paths.
#
# DEPENDENCIES
# ------------
# Bioconductor packages: graph, RBGL
# Install with:
#   BiocManager::install("graph")
#   BiocManager::install("RBGL")

# Check if required packages are available
if (!requireNamespace("graph", quietly = TRUE)) {
  stop("Package 'graph' from Bioconductor is required for Phase 3. Install with:\n",
       "BiocManager::install('graph')")
}

if (!requireNamespace("RBGL", quietly = TRUE)) {
  stop("Package 'RBGL' from Bioconductor is required for Phase 3. Install with:\n",
       "BiocManager::install('RBGL')")
}

library(graph)
library(RBGL)

# ==============================================================================
# MAIN CONVERSION FUNCTION
# ==============================================================================

#' Convert EvidenceGraph to graphNEL format
#'
#' Creates a graphNEL suitable for gRain belief propagation:
#' 1. Includes all nodes (hypotheses, phenomena, links, results)
#' 2. Adds FROM_SOURCE and TO_TARGET edges for explanatory structure
#' 3. EXCLUDES eliminated ExplanatoryLinks (broken chains)
#' 4. Validates DAG structure (no cycles allowed)
#'
#' @param evidence_graph EvidenceGraph object
#' @return List with:
#'   - graph_nel: graphNEL object for gRain
#'   - node_mapping: Bidirectional ID mapping
#'   - excluded_nodes: IDs of eliminated links (for diagnostics)
evidence_graph_to_graphNEL <- function(evidence_graph) {
  if (!inherits(evidence_graph, "EvidenceGraph")) {
    stop("Input must be an EvidenceGraph object")
  }

  # Get all node IDs (we'll filter eliminated links below)
  all_node_ids <- names(evidence_graph$nodes)

  if (length(all_node_ids) == 0) {
    stop("Cannot convert empty graph")
  }

  # Filter out eliminated ExplanatoryLinks
  # We include eliminated auxiliaries (they're still in the structure)
  # but exclude eliminated links (they break explanatory chains)
  valid_node_ids <- character()
  for (node_id in all_node_ids) {
    node <- evidence_graph$nodes[[node_id]]

    # Exclude eliminated ExplanatoryLinks
    if (inherits(node, "ExplanatoryLink") && node$eliminated) {
      next
    }

    valid_node_ids <- c(valid_node_ids, node_id)
  }

  if (length(valid_node_ids) == 0) {
    stop("No valid nodes after filtering eliminated links")
  }

  # Create empty graphNEL
  # graphNEL requires an edge list format: list(node1 = list(edges = c("node2", "node3")))
  edge_list <- list()

  # Initialize each node with empty edge list
  for (node_id in valid_node_ids) {
    edge_list[[node_id]] <- list(edges = character(0))
  }

  # Add edges from the EvidenceGraph
  # We add FROM_SOURCE and TO_TARGET edges to preserve explanatory structure
  from_edges <- evidence_graph$edges[evidence_graph$edges$type == "FROM_SOURCE", ]
  to_edges <- evidence_graph$edges[evidence_graph$edges$type == "TO_TARGET", ]

  # Add FROM_SOURCE edges (Source → ExplanatoryLink)
  for (i in seq_len(nrow(from_edges))) {
    source_id <- from_edges$from[i]
    link_id <- from_edges$to[i]

    # Skip if either node is not in valid set (e.g., link eliminated)
    if (!(source_id %in% valid_node_ids) || !(link_id %in% valid_node_ids)) {
      next
    }

    # Add edge to edge_list
    edge_list[[source_id]]$edges <- c(edge_list[[source_id]]$edges, link_id)
  }

  # Add TO_TARGET edges (ExplanatoryLink → Target)
  for (i in seq_len(nrow(to_edges))) {
    link_id <- to_edges$from[i]
    target_id <- to_edges$to[i]

    # Skip if either node is not in valid set
    if (!(link_id %in% valid_node_ids) || !(target_id %in% valid_node_ids)) {
      next
    }

    # Add edge to edge_list
    edge_list[[link_id]]$edges <- c(edge_list[[link_id]]$edges, target_id)
  }

  # Create graphNEL
  g_nel <- graphNEL(nodes = valid_node_ids, edgeL = edge_list, edgemode = "directed")

  # Validate DAG structure (no cycles)
  if (!validate_dag(g_nel)) {
    stop("Graph contains cycles - Bayesian networks must be acyclic (DAG)")
  }

  # Create node mapping for CPT attachment and result extraction
  node_mapping <- create_node_mapping(valid_node_ids)

  return(list(
    graph_nel = g_nel,
    node_mapping = node_mapping,
    excluded_nodes = setdiff(all_node_ids, valid_node_ids)  # For diagnostics
  ))
}

# ==============================================================================
# DAG VALIDATION
# ==============================================================================

#' Validate that a graph is a DAG (directed acyclic graph)
#'
#' Bayesian networks require acyclicity—cycles create undefined circular
#' dependencies. Uses topological sort which fails on cyclic graphs.
#'
#' @param g_nel graphNEL object
#' @return TRUE if acyclic (valid DAG), FALSE if contains cycles
validate_dag <- function(g_nel) {
  tryCatch({
    # Topological sort only works on DAGs
    # If the graph has cycles, tsort will error
    tsort(g_nel)
    return(TRUE)  # No error = acyclic
  }, error = function(e) {
    # Error in tsort = cycles detected
    return(FALSE)
  })
}

# ==============================================================================
# NODE MAPPING
# ==============================================================================

#' Create bidirectional mapping between node IDs and indices
#'
#' Utility for converting between EvidenceGraph IDs and array indices.
#'
#' @param node_ids Character vector of node IDs
#' @return List with id_to_idx, idx_to_id, and all_ids
create_node_mapping <- function(node_ids) {
  n <- length(node_ids)

  # Create mappings
  id_to_idx <- setNames(1:n, node_ids)
  idx_to_id <- setNames(node_ids, 1:n)

  return(list(
    id_to_idx = id_to_idx,
    idx_to_id = idx_to_id,
    all_ids = node_ids
  ))
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Extract node types for CPT generation
#'
#' Different node types may need different default CPTs (e.g., hypotheses
#' get priors, links get conditional tables).
#'
#' @param evidence_graph EvidenceGraph object
#' @param node_ids Character vector of node IDs
#' @return Named character vector: node_id → node_type
get_node_types <- function(evidence_graph, node_ids) {
  types <- character(length(node_ids))
  names(types) <- node_ids

  for (node_id in node_ids) {
    node <- evidence_graph$nodes[[node_id]]
    types[node_id] <- node$type
  }

  return(types)
}

#' Get semantic parents for CPT conditioning
#'
#' Returns parent nodes in the Bayesian network sense. For evidence graphs:
#' - Regular nodes: incoming links (TO_TARGET edges)
#' - ExplanatoryLinks: source nodes (FROM_SOURCE) + auxiliaries (IS_REQUIRED_BY)
#'
#' Used for CPT construction where we need P(node | parents).
#'
#' @param evidence_graph EvidenceGraph object
#' @param node_id ID of node to get parents for
#' @return Character vector of parent node IDs
get_node_parents <- function(evidence_graph, node_id) {
  # For Bayesian networks, we need to find the semantic parents (source nodes)
  # not the link nodes. The structure is:
  #   Source --FROM_SOURCE--> Link --TO_TARGET--> Target
  #
  # So to find semantic parents of a target:
  # 1. Find TO_TARGET edges into this node (gives us the links)
  # 2. Find FROM_SOURCE edges into those links (gives us the sources)

  parents <- character()

  # Find TO_TARGET edges coming into this node
  to_target_edges <- evidence_graph$edges[
    evidence_graph$edges$to == node_id &
    evidence_graph$edges$type == "TO_TARGET",
  ]

  # For each link, find its source
  for (link_id in to_target_edges$from) {
    from_source_edges <- evidence_graph$edges[
      evidence_graph$edges$to == link_id &
      evidence_graph$edges$type == "FROM_SOURCE",
    ]
    parents <- c(parents, from_source_edges$from)
  }

  # Return unique parent IDs
  unique(parents)
}
