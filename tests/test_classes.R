# Unit Tests for Evidence Graph Framework - R6 Classes
# V4 Implementation

library(R6)
library(igraph)
library(jsonlite)

source("R/classes.R")

cat("Testing Evidence Graph Framework - R6 Classes\n")
cat("==============================================\n\n")

# Test counters
tests_run <- 0
tests_passed <- 0
tests_failed <- 0

# Helper function for assertions
assert <- function(condition, test_name) {
  tests_run <<- tests_run + 1
  if (condition) {
    cat("✓", test_name, "\n")
    tests_passed <<- tests_passed + 1
    return(TRUE)
  } else {
    cat("✗ FAILED:", test_name, "\n")
    tests_failed <<- tests_failed + 1
    return(FALSE)
  }
}

# ===== Test Node Classes =====
cat("\n--- Testing Node Classes ---\n")

# Test Hypothesis creation
h1 <- Hypothesis$new(
  id = "H1",
  text = "Test hypothesis",
  source = "Test",
  level = "primary"
)

assert(h1$id == "H1", "Hypothesis: ID set correctly")
assert(h1$type == "Hypothesis", "Hypothesis: Type set correctly")
assert(h1$text == "Test hypothesis", "Hypothesis: Text set correctly")
assert(h1$level == "primary", "Hypothesis: Level set correctly")

# Test PhenomenonClaim creation
p1 <- PhenomenonClaim$new(
  id = "P1",
  text = "Test phenomenon",
  source = "Test"
)

assert(p1$id == "P1", "PhenomenonClaim: ID set correctly")
assert(p1$type == "PhenomenonClaim", "PhenomenonClaim: Type set correctly")

# Test AuxiliaryClaim creation and elimination
a1 <- AuxiliaryClaim$new(
  id = "A1",
  text = "Test auxiliary",
  source = "Test",
  subtype = "BackgroundTheory"
)

assert(a1$type == "AuxiliaryClaim", "AuxiliaryClaim: Type set correctly")
assert(a1$subtype == "BackgroundTheory", "AuxiliaryClaim: Subtype set correctly")
assert(!a1$eliminated, "AuxiliaryClaim: Not eliminated initially")

# Test elimination
a1$eliminate("Testing elimination")
assert(a1$eliminated == TRUE, "AuxiliaryClaim: Elimination flag set")
assert(!is.null(a1$elimination_reason), "AuxiliaryClaim: Elimination reason recorded")

# Test EmpiricalResult creation
r1 <- EmpiricalResult$new(
  id = "R1",
  text = "Test result",
  source = "Test",
  result_type = "Quantitative",
  sample_size = 100
)

assert(r1$type == "EmpiricalResult", "EmpiricalResult: Type set correctly")
assert(r1$result_type == "Quantitative", "EmpiricalResult: Result type set correctly")
assert(r1$sample_size == 100, "EmpiricalResult: Sample size set correctly")

# Test ExplanatoryLink creation
link1 <- ExplanatoryLink$new(
  id = "EL1",
  source_id = "H1",
  target_id = "P1",
  text = "Test explains test",
  source = "Generated"
)

assert(link1$type == "ExplanatoryLink", "ExplanatoryLink: Type set correctly")
assert(all(link1$source_ids == "H1"), "ExplanatoryLink: Source IDs set correctly")
assert(link1$target_id == "P1", "ExplanatoryLink: Target ID set correctly")
assert(!link1$eliminated, "ExplanatoryLink: Not eliminated initially")

# Test link elimination
link1$eliminate("Testing link elimination")
assert(link1$eliminated == TRUE, "ExplanatoryLink: Elimination flag set")

# ===== Test EvidenceGraph Class =====
cat("\n--- Testing EvidenceGraph Class ---\n")

# Test graph creation
graph <- EvidenceGraph$new(name = "Test Graph")
assert(graph$name == "Test Graph", "EvidenceGraph: Name set correctly")
assert(graph$node_count == 0, "EvidenceGraph: Starts with zero nodes")
assert(graph$edge_count == 0, "EvidenceGraph: Starts with zero edges")
assert(graph$link_counter == 0, "EvidenceGraph: Link counter initialized to 0")

# Test node addition
h2 <- Hypothesis$new("H2", "Hypothesis 2", "Test")
graph$add_node(h2)
assert(graph$node_count == 1, "EvidenceGraph: Node count incremented")
assert("H2" %in% names(graph$nodes), "EvidenceGraph: Node added to nodes list")

# Test duplicate node rejection
duplicate_error <- FALSE
tryCatch({
  graph$add_node(h2)
}, error = function(e) {
  duplicate_error <<- TRUE
})
assert(duplicate_error, "EvidenceGraph: Rejects duplicate node IDs")

# Test edge addition
p2 <- PhenomenonClaim$new("P2", "Phenomenon 2", "Test")
graph$add_node(p2)

graph$add_edge("H2", "P2", "FROM_SOURCE")
assert(graph$edge_count == 1, "EvidenceGraph: Edge count incremented")

# Test edge type validation (V4 feature)
invalid_edge_error <- FALSE
tryCatch({
  graph$add_edge("H2", "P2", "INVALID_TYPE")
}, error = function(e) {
  invalid_edge_error <<- TRUE
})
assert(invalid_edge_error, "EvidenceGraph: V4 edge type validation rejects invalid types")

# Test explanatory link creation
r2 <- EmpiricalResult$new("R2", "Result 2", "Test", "Quantitative", sample_size = 50)
graph$add_node(r2)

link <- graph$create_explanatory_link(
  source_id = "H2",
  target_id = "R2",
  auxiliary_ids = c()
)

assert(!is.null(link), "EvidenceGraph: Creates explanatory link")
assert(inherits(link, "ExplanatoryLink"), "EvidenceGraph: Returns ExplanatoryLink object")
assert(all(link$source_ids == "H2"), "EvidenceGraph: Link has correct source")
assert(link$target_id == "R2", "EvidenceGraph: Link has correct target")

# Check that edges were created
from_source_exists <- any(graph$edges$from == "H2" &
                         graph$edges$to == link$id &
                         graph$edges$type == "FROM_SOURCE")
to_target_exists <- any(graph$edges$from == link$id &
                       graph$edges$to == "R2" &
                       graph$edges$type == "TO_TARGET")

assert(from_source_exists, "EvidenceGraph: Creates FROM_SOURCE edge")
assert(to_target_exists, "EvidenceGraph: Creates TO_TARGET edge")

# Test link ID counter (prevents collisions)
link2 <- graph$create_explanatory_link("H2", "R2", c())
assert(link$id != link2$id, "EvidenceGraph: V4 link counter prevents ID collisions")

# Test explanatory link with auxiliaries
a2 <- AuxiliaryClaim$new("A2", "Auxiliary 2", "Test", "MethodologicalAssumption")
graph$add_node(a2)

link3 <- graph$create_explanatory_link(
  source_id = "H2",
  target_id = "P2",
  auxiliary_ids = c("A2")
)

# Check IS_REQUIRED_BY edge was created
aux_edge_exists <- any(graph$edges$from == "A2" &
                      graph$edges$to == link3$id &
                      graph$edges$type == "IS_REQUIRED_BY")
assert(aux_edge_exists, "EvidenceGraph: Creates IS_REQUIRED_BY edge for auxiliaries")

# Test get_required_auxiliaries
aux_list <- graph$get_required_auxiliaries(link3$id)
assert(length(aux_list) == 1, "EvidenceGraph: Returns correct auxiliary count")
assert("A2" %in% names(aux_list), "EvidenceGraph: Returns correct auxiliary ID")

# Test get_links_requiring_auxiliary
links_list <- graph$get_links_requiring_auxiliary("A2")
assert(length(links_list) == 1, "EvidenceGraph: Returns correct link count requiring auxiliary")
assert(link3$id %in% names(links_list), "EvidenceGraph: Returns correct link ID")

# Test auxiliary elimination cascades
eliminated_links <- graph$eliminate_auxiliary("A2", "Test elimination")
assert(length(eliminated_links) == 1, "EvidenceGraph: Eliminates dependent links")
assert(link3$id %in% eliminated_links, "EvidenceGraph: Identifies correct eliminated link")
assert(link3$eliminated == TRUE, "EvidenceGraph: Marks dependent link as eliminated")
assert(a2$eliminated == TRUE, "EvidenceGraph: Marks auxiliary as eliminated")

# ===== Test Path Finding (V4 Critical Fix) =====
cat("\n--- Testing Path Finding (V4 Feature) ---\n")

# Create a simple path: H3 -> link -> P3 -> link -> R3
graph2 <- EvidenceGraph$new(name = "Path Test")

h3 <- Hypothesis$new("H3", "Hypothesis 3", "Test")
p3 <- PhenomenonClaim$new("P3", "Phenomenon 3", "Test")
r3 <- EmpiricalResult$new("R3", "Result 3", "Test", "Quantitative")

graph2$add_node(h3)
graph2$add_node(p3)
graph2$add_node(r3)

link_hp <- graph2$create_explanatory_link("H3", "P3", c(), 0.8)
link_pr <- graph2$create_explanatory_link("P3", "R3", c(), 0.85)

# Find path
path <- graph2$find_explanatory_path("H3", "R3")

assert(!is.null(path), "PathFinding: Finds valid path")
assert(length(path) > 0, "PathFinding: Path is not empty")

# V4 CRITICAL FIX: Check that path includes ExplanatoryLink nodes
link_count_in_path <- sum(sapply(path, function(id) {
  node <- graph2$nodes[[id]]
  inherits(node, "ExplanatoryLink")
}))

assert(link_count_in_path == 2, "PathFinding: V4 FIX - Path includes ExplanatoryLink nodes")
assert("H3" %in% path, "PathFinding: Path includes hypothesis")
assert("P3" %in% path, "PathFinding: Path includes phenomenon")
assert("R3" %in% path, "PathFinding: Path includes result")
assert(link_hp$id %in% path, "PathFinding: Path includes first link")
assert(link_pr$id %in% path, "PathFinding: Path includes second link")

# Test semantic path (for display)
semantic_path <- graph2$get_semantic_path(path)
assert(link_count_in_path == 2 && length(semantic_path) == 3,
       "PathFinding: V4 semantic path filters out links")
assert("H3" %in% semantic_path, "PathFinding: Semantic path includes hypothesis")
assert("P3" %in% semantic_path, "PathFinding: Semantic path includes phenomenon")
assert("R3" %in% semantic_path, "PathFinding: Semantic path includes result")
assert(!(link_hp$id %in% semantic_path), "PathFinding: Semantic path excludes links")

# Test path finding excludes eliminated links
link_pr$eliminate("Test")
path_after_elimination <- graph2$find_explanatory_path("H3", "R3")
assert(is.null(path_after_elimination), "PathFinding: Excludes eliminated links by default")

# Test path finding includes eliminated links when requested
path_with_eliminated <- graph2$find_explanatory_path("H3", "R3", include_eliminated = TRUE)
assert(!is.null(path_with_eliminated), "PathFinding: Includes eliminated links when requested")

# ===== Test Serialization =====
cat("\n--- Testing Serialization ---\n")

# Test to_list for nodes
h_list <- h3$to_list()
assert(is.list(h_list), "Serialization: Node to_list returns list")
assert(h_list$id == "H3", "Serialization: Node list includes ID")
assert(h_list$type == "Hypothesis", "Serialization: Node list includes type")

# Test to_json for graph
json_output <- graph2$to_json()
assert(is.character(json_output), "Serialization: Graph to_json returns string")

# Test that JSON can be parsed
parsed <- fromJSON(json_output)
assert(!is.null(parsed$name), "Serialization: JSON includes graph name")
assert(!is.null(parsed$nodes), "Serialization: JSON includes nodes")
assert(!is.null(parsed$edges), "Serialization: JSON includes edges")
assert(!is.null(parsed$link_counter), "Serialization: JSON includes link counter")

# Test save/load
temp_file <- tempfile(fileext = ".rds")
graph2$save(temp_file)
assert(file.exists(temp_file), "Serialization: Save creates file")

loaded_graph <- load_evidence_graph(temp_file)
assert(!is.null(loaded_graph), "Serialization: Load returns object")
assert(loaded_graph$name == graph2$name, "Serialization: Loaded graph has same name")
assert(loaded_graph$node_count == graph2$node_count, "Serialization: Loaded graph has same node count")
assert(loaded_graph$edge_count == graph2$edge_count, "Serialization: Loaded graph has same edge count")

# Cleanup
unlink(temp_file)

# ===== Test New Node Types (Selector and Account) =====
cat("\n--- Testing New Node Types (Selector and Account) ---\n")

# Test Selector node creation
selector_node <- Node$new(
  id = "Selector",
  type = "Selector",
  text = "Account Selector",
  source = "Generated"
)

assert(selector_node$id == "Selector", "Selector: ID set correctly")
assert(selector_node$type == "Selector", "Selector: Type set correctly")
assert("Selector" %in% NODE_TYPES, "Selector: Type is in NODE_TYPES constant")

# Test Account node creation
account_node <- Node$new(
  id = "Acc1",
  type = "Account",
  text = "Account 1 description",
  source = "User"
)

assert(account_node$id == "Acc1", "Account: ID set correctly")
assert(account_node$type == "Account", "Account: Type set correctly")
assert("Account" %in% NODE_TYPES, "Account: Type is in NODE_TYPES constant")

# Test adding Selector and Account nodes to graph
graph3 <- EvidenceGraph$new(name = "Selector Test")
graph3$add_node(selector_node)
graph3$add_node(account_node)

assert(graph3$node_count == 2, "Graph: Added Selector and Account nodes")
assert("Selector" %in% names(graph3$nodes), "Graph: Contains Selector node")
assert("Acc1" %in% names(graph3$nodes), "Graph: Contains Account node")

# Test edge between Selector and Account
graph3$add_edge("Selector", "Acc1", "FROM_SOURCE")
selector_edge_exists <- any(graph3$edges$from == "Selector" &
                           graph3$edges$to == "Acc1" &
                           graph3$edges$type == "FROM_SOURCE")
assert(selector_edge_exists, "Graph: Can create edge from Selector to Account")

# ===== Test Type Validation =====
cat("\n--- Testing Type Validation ---\n")

# Test invalid source type for explanatory link
r4 <- EmpiricalResult$new("R4", "Result 4", "Test", "Quantitative")
r5 <- EmpiricalResult$new("R5", "Result 5", "Test", "Quantitative")
graph2$add_node(r4)
graph2$add_node(r5)

invalid_source_error <- FALSE
tryCatch({
  graph2$create_explanatory_link("R4", "R5", c(), 0.8)
}, error = function(e) {
  invalid_source_error <<- TRUE
})
assert(invalid_source_error, "Validation: Rejects invalid source type for explanatory link")

# Test invalid target type
invalid_target_error <- FALSE
tryCatch({
  graph2$create_explanatory_link("H3", "H3", c(), 0.8)
}, error = function(e) {
  invalid_target_error <<- TRUE
})
assert(invalid_target_error, "Validation: Rejects invalid target type for explanatory link")

# Test non-existent node
nonexistent_error <- FALSE
tryCatch({
  graph2$create_explanatory_link("NONEXISTENT", "P3", c(), 0.8)
}, error = function(e) {
  nonexistent_error <<- TRUE
})
assert(nonexistent_error, "Validation: Rejects non-existent nodes")

# Test non-auxiliary in auxiliary list
invalid_aux_error <- FALSE
tryCatch({
  graph2$create_explanatory_link("H3", "P3", c("R4"), 0.8)
}, error = function(e) {
  invalid_aux_error <<- TRUE
})
assert(invalid_aux_error, "Validation: Rejects non-auxiliary nodes in auxiliary list")

# ===== Summary =====
cat("\n==============================================\n")
cat("Test Summary:\n")
cat("  Total tests run:", tests_run, "\n")
cat("  Tests passed:", tests_passed, "\n")
cat("  Tests failed:", tests_failed, "\n")

if (tests_failed == 0) {
  cat("\n✓ All tests passed!\n")
  quit(status = 0)
} else {
  cat("\n✗ Some tests failed!\n")
  quit(status = 1)
}
