# ==============================================================================
# Case Studies for Explanatory Account Framework
# ==============================================================================
#
# Pre-built case studies demonstrating the explanatory-eliminative framework.
# Use these to test the system or as templates for new analyses.
#
# AVAILABLE CASE STUDIES
# ----------------------
# 1. create_simple_test_case_study()
#    Minimal 2-hypothesis example for testing and debugging.
#
# 2. create_covid_case_study()
#    Full COVID-19 transmission mode analysis comparing aerosol vs.
#    droplet-and-contact hypotheses against 4 evidence categories.
#
# USAGE
# -----
# model <- create_simple_test_case_study()
# graph <- build_bayesian_network(model)
# cpts <- generate_all_cpts_spec(graph)
# posteriors <- compute_posteriors(graph, cpts, list(R1 = "true"))

if (!isNamespaceLoaded("ExplanatoryAccounts") && !exists("ExplanatoryModel")) {
  source("R/explanatory_classes.R")
}

# ===== SIMPLE TEST CASE STUDY =====

#' Create Simple Test Case Study
#'
#' A minimal example with:
#' - 2 hypotheses (H1: Theory 1, H2: Theory 2)
#' - 2 accounts (one per hypothesis)
#' - 1 phenomenon per account (PC1, PC2)
#' - 1 result (R1)
#'
#' Structure:
#'   H1 -> Acc1 -> EL1 -> PC1 -> EL3 -> R1
#'   H2 -> Acc2 -> EL2 -> PC2 -> EL4 -> R1
#'
#' @return ExplanatoryModel object
#'
#' @examples
#' model <- create_simple_test_case_study()
#' graph <- build_bayesian_network(model)
create_simple_test_case_study <- function() {
  model <- ExplanatoryModel$new(name = "Simple Test Model")

  # ===== RESULT =====
  r1 <- BayesianResult$new(
    id = "R1",
    text = "Observed experimental outcome",
    source = "Lab study",
    strength = 0.90,
    base_rate = 0.001
  )
  model$add_result(r1)

  # ===== HYPOTHESES =====
  h1 <- BayesianHypothesis$new(
    id = "H1",
    text = "H1",
    source = "Hypothesis",
    prior = 0.5
  )

  h2 <- BayesianHypothesis$new(
    id = "H2",
    text = "H2",
    source = "Hypothesis",
    prior = 0.5
  )

  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # ===== ACCOUNT 1 (for H1) =====
  acc1 <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Account based on Theory 1",
    relative_weight = 1
  )
  model$add_account(acc1)

  # Phenomenon for Account 1
  pc1 <- BayesianPhenomenon$new(
    id = "PC1",
    text = "Mechanism A produces the effect",
    source = "Theory 1 prediction",
    strength = 0.95,
    base_rate = 0.001
  )
  model$add_phenomenon(pc1)
  acc1$add_phenomenon_id("PC1")

  # Explanatory links for Account 1
  el1 <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "Acc1",
    target_id = "PC1",
    text = "Theory 1 predicts Mechanism A",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  el3 <- BayesianExplanatoryLink$new(
    id = "EL3",
    source_id = "PC1",
    target_id = "R1",
    text = "Mechanism A explains the observed outcome",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  # Auxiliaries for Account 1
  a1 <- BayesianAuxiliary$new(
    id = "A1",
    text = "auxiliary 1",
    source = "Aerosol dispersion modeling",
    prior = 0.9
  )

  a2 <- BayesianAuxiliary$new(
    id = "A2",
    text = "auxiliary 2",
    source = "Methodological literature",
    prior = 0.9
  )

  # Add auxiliaries to the model (this was missing!)
  model$add_auxiliary(a1)
  model$add_auxiliary(a2)

  model$add_explanatory_link(el1)
  model$add_explanatory_link(el3)
  acc1$add_explanatory_link_id("EL1")
  acc1$add_explanatory_link_id("EL3")

  el1$required_auxiliary_ids <- c("A1")
  el3$required_auxiliary_ids <- c("A2")
  acc1$add_auxiliary_id("A1")
  acc1$add_auxiliary_id("A2")

  # ===== ACCOUNT 2 (for H2) =====
  acc2 <- Account$new(
    id = "Acc2",
    hypothesis_id = "H2",
    description = "Account based on Theory 2",
    relative_weight = 1
  )
  model$add_account(acc2)

  # Phenomenon for Account 2
  pc2 <- BayesianPhenomenon$new(
    id = "PC2",
    text = "Mechanism B produces the effect",
    source = "Theory 2 prediction",
    strength = 0.85,
    base_rate = 0.001
  )
  model$add_phenomenon(pc2)
  acc2$add_phenomenon_id("PC2")

  # Explanatory links for Account 2
  el2 <- BayesianExplanatoryLink$new(
    id = "EL2",
    source_id = "Acc2",
    target_id = "PC2",
    text = "Theory 2 predicts Mechanism B",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  el4 <- BayesianExplanatoryLink$new(
    id = "EL4",
    source_id = "PC2",
    target_id = "R1",
    text = "Mechanism B explains the observed outcome",
    source = "Theory",
    reliability = 0.80,
    epsilon = 0.01
  )

  a3 <- BayesianAuxiliary$new(
    id = "A3",
    text = "auxiliary 3",
    source = "Methodological literature",
    prior = 0.9
  )

  # Add auxiliary to the model (this was missing!)
  model$add_auxiliary(a3)

  model$add_explanatory_link(el2)
  model$add_explanatory_link(el4)
  acc2$add_explanatory_link_id("EL2")
  acc2$add_explanatory_link_id("EL4")
  acc2$add_auxiliary_id("A3")

  el2$required_auxiliary_ids <- c("A3")

  return(model)
}

# ===== COVID-19 CASE STUDY =====

#' Create COVID-19 Transmission Case Study
#'
#' Builds complete ExplanatoryModel matching the chapter diagrams with:
#' - 2 hypotheses (aerosol vs droplet-and-contact)
#' - 2 accounts (one per hypothesis)
#' - 4 result categories (evidence corpus)
#' - 10 phenomena claims (PC1-PC5 aerosol, PC6-PC10 droplet)
#' - Explanatory chains connecting accounts to results
#'
#' @return ExplanatoryModel object
#'
#' @examples
#' model <- create_covid_case_study()
#' graph <- build_bayesian_network(model)
create_covid_case_study <- function() {
  model <- ExplanatoryModel$new(name = "COVID-19 Transmission Mode")

  # ===== RESULTS (Evidence Corpus) =====
  # Based on the 4 result categories from the chapter diagrams

  r1 <- BayesianResult$new(
    id = "R1",
    text = "Outbreak Patterns (indoor/outdoor difference, ventilation effects, long-range)",
    source = "Epidemiological studies",
    strength = 0.90,
    base_rate = 0.001
  )

  r2 <- BayesianResult$new(
    id = "R2",
    text = "Low to Moderate R₀",
    source = "Epidemiological estimates",
    strength = 0.85,
    base_rate = 0.001
  )

  r3 <- BayesianResult$new(
    id = "R3",
    text = "Mask Efficacy (surgical and cotton masks moderately effective)",
    source = "Masking studies",
    strength = 0.85,
    base_rate = 0.001
  )

  r4 <- BayesianResult$new(
    id = "R4",
    text = "Failure to Detect Viable SARS-CoV-2 in Aerosols",
    source = "Air sampling studies",
    strength = 0.85,
    base_rate = 0.001
  )

  model$add_result(r1)
  model$add_result(r2)
  model$add_result(r3)
  model$add_result(r4)

  # ===== HYPOTHESES =====

  h1 <- BayesianHypothesis$new(
    id = "H1",
    text = "Aerosol transmission is the predominant mode",
    source = "Hypothesis",
    prior = 0.5
  )

  h2 <- BayesianHypothesis$new(
    id = "H2",
    text = "Droplet-and-contact transmission is the predominant mode",
    source = "Hypothesis",
    prior = 0.5
  )

  model$add_hypothesis(h1)
  model$add_hypothesis(h2)

  # ===== ACCOUNT 1: AEROSOL =====

  acc1 <- Account$new(
    id = "Acc1",
    hypothesis_id = "H1",
    description = "Aerosol transmission account",
    relative_weight = 1
  )
  model$add_account(acc1)

  # Auxiliaries for Account 1
  a1 <- BayesianAuxiliary$new(
    id = "A1",
    text = "Remaining Aerosols Typically Below Infectious Dose",
    source = "Aerosol dispersion modeling",
    prior = 0.9
  )

  a2 <- BayesianAuxiliary$new(
    id = "A2",
    text = "Aerosol Sampling Techniques Unreliable",
    source = "Methodological literature",
    prior = 0.9
  )

  model$add_auxiliary(a1)
  model$add_auxiliary(a2)
  acc1$add_auxiliary_id("A1")
  acc1$add_auxiliary_id("A2")

  # ===== AEROSOL ACCOUNT PHENOMENA (PC1-PC5) =====

  pc1 <- BayesianPhenomenon$new(
    id = "PC1",
    text = "Efficient aerosol transmission ≲2m",
    source = "Aerosol theory",
    strength = 0.90,
    base_rate = 0.001
  )

  pc2 <- BayesianPhenomenon$new(
    id = "PC2",
    text = "Ventilation has strong effect on transmission",
    source = "Aerosol physics",
    strength = 0.95,
    base_rate = 0.001
  )

  pc3 <- BayesianPhenomenon$new(
    id = "PC3",
    text = "Inefficient aerosol transmission ≳2m in most settings",
    source = "Aerosol dispersion and dose",
    strength = 0.85,
    base_rate = 0.001
  )

  pc4 <- BayesianPhenomenon$new(
    id = "PC4",
    text = "Masks block some aerosols",
    source = "Filtration studies",
    strength = 0.90,
    base_rate = 0.001
  )

  pc5 <- BayesianPhenomenon$new(
    id = "PC5",
    text = "Significant viral levels in aerosols",
    source = "Aerosol generation studies",
    strength = 0.85,
    base_rate = 0.001
  )

  model$add_phenomenon(pc1)
  model$add_phenomenon(pc2)
  model$add_phenomenon(pc3)
  model$add_phenomenon(pc4)
  model$add_phenomenon(pc5)
  acc1$add_phenomenon_id("PC1")
  acc1$add_phenomenon_id("PC2")
  acc1$add_phenomenon_id("PC3")
  acc1$add_phenomenon_id("PC4")
  acc1$add_phenomenon_id("PC5")

  # ===== AEROSOL ACCOUNT EXPLANATORY LINKS =====

  # Acc1 -> PC1, PC2, PC3, PC4, PC5
  el1 <- BayesianExplanatoryLink$new(
    id = "EL1",
    source_id = "Acc1",
    target_id = "PC1",
    text = "Aerosol mechanism predicts efficient close-range transmission",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  el2 <- BayesianExplanatoryLink$new(
    id = "EL2",
    source_id = "Acc1",
    target_id = "PC2",
    text = "Aerosol mechanism predicts strong ventilation effect",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  el3 <- BayesianExplanatoryLink$new(
    id = "EL3",
    source_id = "Acc1",
    target_id = "PC3",
    text = "Aerosol mechanism predicts inefficient long-range transmission",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )
  el3$required_auxiliary_ids <- c("A1")

  el4 <- BayesianExplanatoryLink$new(
    id = "EL4",
    source_id = "Acc1",
    target_id = "PC4",
    text = "Aerosol mechanism predicts mask filtration",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  el5 <- BayesianExplanatoryLink$new(
    id = "EL5",
    source_id = "Acc1",
    target_id = "PC5",
    text = "Aerosol mechanism predicts viable virus in aerosols",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  # PC1 + PC2 -> R1 (close-range + ventilation together explain outbreak patterns)
  # This is a conjunctive explanation: both phenomena together explain R1
  el6 <- BayesianExplanatoryLink$new(
    id = "EL6",
    source_id = c("PC1", "PC2"),
    target_id = "R1",
    text = "Close-range transmission + ventilation effects together explain outbreak patterns",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  # PC3 -> R2 (inefficient long-range explains low R₀)
  el8 <- BayesianExplanatoryLink$new(
    id = "EL8",
    source_id = "PC3",
    target_id = "R2",
    text = "Inefficient long-range transmission explains low to moderate R₀",
    source = "Theory",
    reliability = 0.85,
    epsilon = 0.01
  )

  # PC4 -> R3 (mask blocking explains efficacy)
  el9 <- BayesianExplanatoryLink$new(
    id = "EL9",
    source_id = "PC4",
    target_id = "R3",
    text = "Mask filtration explains moderate efficacy",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  # PC5 -> R4 (significant virus contradicts detection failures - needs auxiliary explanation)
  el10 <- BayesianExplanatoryLink$new(
    id = "EL10",
    source_id = "PC5",
    target_id = "R4",
    text = "Viable virus in aerosols requires sampling methodology explanation for failures",
    source = "Theory",
    reliability = 0.70,
    epsilon = 0.01
  )
  el10$required_auxiliary_ids <- c("A2")

  model$add_explanatory_link(el1)
  model$add_explanatory_link(el2)
  model$add_explanatory_link(el3)
  model$add_explanatory_link(el4)
  model$add_explanatory_link(el5)
  model$add_explanatory_link(el6)
  # EL7 removed - merged into EL6 as conjunctive link
  model$add_explanatory_link(el8)
  model$add_explanatory_link(el9)
  model$add_explanatory_link(el10)

  acc1$add_explanatory_link_id("EL1")
  acc1$add_explanatory_link_id("EL2")
  acc1$add_explanatory_link_id("EL3")
  acc1$add_explanatory_link_id("EL4")
  acc1$add_explanatory_link_id("EL5")
  acc1$add_explanatory_link_id("EL6")
  # EL7 removed - merged into EL6 as conjunctive link
  acc1$add_explanatory_link_id("EL8")
  acc1$add_explanatory_link_id("EL9")
  acc1$add_explanatory_link_id("EL10")

  # ===== ACCOUNT 2: DROPLET-AND-CONTACT =====

  acc2 <- Account$new(
    id = "Acc2",
    hypothesis_id = "H2",
    description = "Droplet-and-contact transmission account",
    relative_weight = 1
  )
  model$add_account(acc2)

  # Auxiliaries for Account 2
  a3 <- BayesianAuxiliary$new(
    id = "A3",
    text = "Unobserved Contact/Proximity",
    source = "Contact tracing limitations",
    prior = 0.5
  )

  model$add_auxiliary(a3)
  acc2$add_auxiliary_id("A3")

  # ===== DROPLET ACCOUNT PHENOMENA (PC6-PC10) =====

  pc6 <- BayesianPhenomenon$new(
    id = "PC6",
    text = "Transmission limited to ≲2m proximity",
    source = "Droplet physics",
    strength = 0.90,
    base_rate = 0.001
  )

  pc7 <- BayesianPhenomenon$new(
    id = "PC7",
    text = "Ventilation has minimal effect on transmission",
    source = "Droplet theory",
    strength = 0.90,
    base_rate = 0.001
  )

  pc8 <- BayesianPhenomenon$new(
    id = "PC8",
    text = "Droplet-and-contact transmission not highly efficient",
    source = "Droplet theory",
    strength = 0.85,
    base_rate = 0.001
  )

  pc9 <- BayesianPhenomenon$new(
    id = "PC9",
    text = "Masks block droplets",
    source = "Physical barrier studies",
    strength = 0.90,
    base_rate = 0.001
  )

  pc10 <- BayesianPhenomenon$new(
    id = "PC10",
    text = "Insignificant viral levels in aerosols",
    source = "Droplet-only hypothesis",
    strength = 0.90,
    base_rate = 0.001
  )

  model$add_phenomenon(pc6)
  model$add_phenomenon(pc7)
  model$add_phenomenon(pc8)
  model$add_phenomenon(pc9)
  model$add_phenomenon(pc10)
  acc2$add_phenomenon_id("PC6")
  acc2$add_phenomenon_id("PC7")
  acc2$add_phenomenon_id("PC8")
  acc2$add_phenomenon_id("PC9")
  acc2$add_phenomenon_id("PC10")

  # ===== DROPLET ACCOUNT EXPLANATORY LINKS =====

  # Acc2 -> PC6, PC7, PC8, PC9, PC10
  el11 <- BayesianExplanatoryLink$new(
    id = "EL11",
    source_id = "Acc2",
    target_id = "PC6",
    text = "Droplet mechanism predicts close proximity requirement",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  el12 <- BayesianExplanatoryLink$new(
    id = "EL12",
    source_id = "Acc2",
    target_id = "PC7",
    text = "Droplet mechanism predicts minimal ventilation effect",
    source = "Theory",
    reliability = 0.95,
    epsilon = 0.01
  )

  el13 <- BayesianExplanatoryLink$new(
    id = "EL13",
    source_id = "Acc2",
    target_id = "PC8",
    text = "Droplet mechanism predicts moderate transmission efficiency",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  el14 <- BayesianExplanatoryLink$new(
    id = "EL14",
    source_id = "Acc2",
    target_id = "PC9",
    text = "Droplet mechanism predicts mask barrier effect",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  el15 <- BayesianExplanatoryLink$new(
    id = "EL15",
    source_id = "Acc2",
    target_id = "PC10",
    text = "Droplet mechanism predicts no viable virus in aerosols",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  # PC6 + PC7 -> R1 (requires A3 auxiliary to explain apparent contradictions)
  # This is a conjunctive explanation: both phenomena together explain R1
  # Requires A3 auxiliary (unobserved contact/proximity) - if A3 is true, the explanation works well
  # The weakness of this explanation comes from A3's prior (0.5), not from low reliability
  el16 <- BayesianExplanatoryLink$new(
    id = "EL16",
    source_id = c("PC6", "PC7"),
    target_id = "R1",
    text = "Proximity limit + minimal ventilation explain outbreak patterns (given unobserved contact)",
    source = "Theory",
    reliability = 0.85,
    epsilon = 0.01
  )
  el16$required_auxiliary_ids <- c("A3")

  # PC8 -> R2 (moderate efficiency explains low R₀)
  el18 <- BayesianExplanatoryLink$new(
    id = "EL18",
    source_id = "PC8",
    target_id = "R2",
    text = "Moderate efficiency explains low to moderate R₀",
    source = "Theory",
    reliability = 0.85,
    epsilon = 0.01
  )

  # PC9 -> R3 (mask blocking explains efficacy)
  el19 <- BayesianExplanatoryLink$new(
    id = "EL19",
    source_id = "PC9",
    target_id = "R3",
    text = "Droplet blocking explains mask efficacy",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  # PC10 -> R4 (no virus explains detection failures)
  el20 <- BayesianExplanatoryLink$new(
    id = "EL20",
    source_id = "PC10",
    target_id = "R4",
    text = "No airborne virus explains detection failures",
    source = "Theory",
    reliability = 0.90,
    epsilon = 0.01
  )

  model$add_explanatory_link(el11)
  model$add_explanatory_link(el12)
  model$add_explanatory_link(el13)
  model$add_explanatory_link(el14)
  model$add_explanatory_link(el15)
  model$add_explanatory_link(el16)
  # EL17 removed - merged into EL16 as conjunctive link
  model$add_explanatory_link(el18)
  model$add_explanatory_link(el19)
  model$add_explanatory_link(el20)

  acc2$add_explanatory_link_id("EL11")
  acc2$add_explanatory_link_id("EL12")
  acc2$add_explanatory_link_id("EL13")
  acc2$add_explanatory_link_id("EL14")
  acc2$add_explanatory_link_id("EL15")
  acc2$add_explanatory_link_id("EL16")
  # EL17 removed - merged into EL16 as conjunctive link
  acc2$add_explanatory_link_id("EL18")
  acc2$add_explanatory_link_id("EL19")
  acc2$add_explanatory_link_id("EL20")

  return(model)
}
