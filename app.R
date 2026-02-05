# ==============================================================================
# Explanatory Account Framework - Shiny Application
# ==============================================================================
#
# Interactive web application for building and analyzing explanatory accounts
# using Bayesian inference. Implements the explanatory-eliminative approach
# for comparing competing scientific hypotheses.
#
# APPLICATION STRUCTURE
# ---------------------
# The app has four main tabs:
#
# 1. BUILD MODEL - Step-by-step wizard:
#    Step 1: Define Evidence Corpus (empirical results to explain)
#    Step 2: Define Hypotheses (competing theories with priors)
#    Step 3: Build Accounts (explanatory pathways: H → Phenomena → Results)
#    Step 4: Review & Validate
#
# 2. ANALYSIS - Run Bayesian inference:
#    - Condition on evidence (results TRUE by default + optional conditions)
#    - Compute posterior probabilities for hypotheses and accounts
#
# 3. VISUALIZATION - Network view:
#    - Full network or individual account views
#    - Color modes: structure, prior, posterior, delta, log-ratio
#    - Hover tooltips show parameters and probabilities
#
# 4. HELP - Documentation
#
# QUICK START
# -----------
# Click "Load Simple Test" or "Load COVID-19 Example" to load a pre-built
# model and see the system in action.
#
# DEPENDENCIES
# ------------
# - shiny, shinydashboard, shinyjs: UI framework
# - visNetwork: Interactive network visualization
# - DT: Data tables
# - All R/ modules (classes, inference, CPT generation)

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(visNetwork)
library(DT)
library(R6)

# Note: When running as a package, these modules are loaded automatically.
# The source() calls below are only used when running app.R directly
# during development (via run_app() which ensures correct working directory).
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  source("R/classes.R")
  source("R/bayesian_spec_classes.R")
  source("R/explanatory_classes.R")
  source("R/explanatory_network.R")
  source("R/explanatory_case_study.R")
  source("R/bayesian_spec_cpt.R")
  source("R/bayesian_spec_inference.R")
  source("R/probability_viz_helpers.R")
  source("R/voi_analysis.R")
  source("R/sensitivity_analysis.R")
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Generate all CPTs for a model
#'
#' Creates Conditional Probability Tables for all nodes in the Bayesian network:
#' - Selector: Equal probability across accounts
#' - Accounts: Conditional on Selector
#' - Auxiliaries: Simple prior CPTs
#' - Phenomena: Target CPTs based on incoming links
#' - Results: Target CPTs if explained, prior CPTs otherwise
#' - ExplanatoryLinks: Link CPTs based on sources and auxiliaries
#'
#' @param model ExplanatoryModel object
#' @param bayesian_network EvidenceGraph built from the model
#' @param selector Selector object for the model
#' @return Named list of CPTs, keyed by node ID
generate_all_cpts_for_model <- function(model, bayesian_network, selector) {
  cpts <- list()

  # Selector CPT
  cpts[["Selector"]] <- generate_selector_cpt(selector)

  # Account CPTs
  for (acc_id in names(model$accounts)) {
    cpts[[acc_id]] <- generate_account_cpt(acc_id, selector)
  }

  # Auxiliary CPTs
  for (aux_id in names(model$auxiliaries)) {
    cpts[[aux_id]] <- generate_prior_cpt_spec(model$auxiliaries[[aux_id]])
  }

  # Phenomenon CPTs
  for (pc_id in names(model$phenomena)) {
    cpts[[pc_id]] <- generate_target_cpt_spec(model$phenomena[[pc_id]], bayesian_network)
  }

  # Result CPTs
  for (r_id in names(model$results)) {
    # Check if result has incoming TO_TARGET edge
    has_incoming <- any(bayesian_network$edges$to == r_id &
                       bayesian_network$edges$type == "TO_TARGET")

    if (has_incoming) {
      # Result explained by phenomena - use target CPT
      cpts[[r_id]] <- generate_target_cpt_spec(model$results[[r_id]], bayesian_network)
    } else {
      # Result not explained - use simple prior CPT
      cpts[[r_id]] <- list(
        states = c("true", "false"),
        parents = character(0),
        probabilities = c(model$results[[r_id]]$base_rate,
                        1 - model$results[[r_id]]$base_rate)
      )
    }
  }

  # Explanatory Link CPTs
  for (link_id in names(model$explanatory_links)) {
    cpts[[link_id]] <- generate_link_cpt_spec(model$explanatory_links[[link_id]], bayesian_network)
  }

  return(cpts)
}

#' Generate descriptive name for an inference based on conditions
#'
#' Creates a human-readable name describing what conditions were applied
#' during inference. Used for auto-naming saved inferences.
#'
#' @param conditions Named list of additional conditions (node_id -> "true"/"false")
#' @return String like "All results=TRUE" or "Results + A1=F, PC1=T"
generate_inference_name <- function(conditions) {
  if (length(conditions) == 0) {
    return("All results=TRUE")
  }

  parts <- sapply(names(conditions), function(node_id) {
    state <- if (conditions[[node_id]] == "true") "T" else "F"
    sprintf("%s=%s", node_id, state)
  })

  paste("Results +", paste(parts, collapse = ", "))
}

#' Get marginals for a comparison selection
#'
#' Resolves a dropdown selection to the corresponding marginals.
#'
#' @param selection The selection value from the dropdown
#' @param rv The reactive values object
#' @return Named list of marginal distributions, or NULL
get_marginals_for_selection <- function(selection, rv) {
  if (is.null(selection) || selection == "") return(rv$baseline)

  switch(selection,
    unconditional = rv$unconditional,
    baseline = rv$baseline,
    current = rv$posteriors,
    {
      if (selection %in% names(rv$saved_inferences)) {
        rv$saved_inferences[[selection]]$marginals
      } else {
        rv$baseline
      }
    }
  )
}

#' Get display label for a comparison/posterior selection
#'
#' @param selection The selection value from the dropdown
#' @return Human-readable label string
get_selection_label <- function(selection) {
  if (is.null(selection) || selection == "" || selection == "baseline") return("Baseline")
  if (selection == "unconditional") return("Unconditional")
  if (selection == "current") return("Current")
  selection
}

#' Build evidence list with all results set to TRUE
#'
#' @param model ExplanatoryModel object
#' @return Named list mapping result IDs to "true"
build_results_evidence <- function(model) {
  setNames(rep("true", length(model$results)), names(model$results))
}

#' Add saved inference names to a choices vector
#'
#' @param choices Existing named character vector of choices
#' @param saved_inferences List of saved inferences
#' @return Updated choices vector with saved inference names appended
add_saved_inference_choices <- function(choices, saved_inferences) {
  if (length(saved_inferences) > 0) {
    saved_names <- names(saved_inferences)
    saved_choices <- setNames(saved_names, saved_names)
    choices <- c(choices, saved_choices)
  }
  choices
}

#' Validate an entity ID
#'
#' Checks that an ID is valid for use in the model:
#' - Not empty or whitespace-only
#' - Contains only alphanumeric characters, underscores, and hyphens
#' - Not already used in the model
#'
#' @param id The ID to validate
#' @param model The ExplanatoryModel to check for duplicates
#' @param entity_type Type of entity ("result", "hypothesis", "account", "phenomenon", "auxiliary")
#' @return TRUE if valid, otherwise throws an error with a descriptive message
validate_id <- function(id, model, entity_type = "entity") {
  # Trim whitespace
  id <- trimws(id)

  # Check empty

  if (is.null(id) || nchar(id) == 0) {
    stop(sprintf("%s ID cannot be empty", tools::toTitleCase(entity_type)))
  }

  # Check valid characters (alphanumeric, underscore, hyphen)
  if (!grepl("^[A-Za-z0-9_-]+$", id)) {
    stop(sprintf("%s ID can only contain letters, numbers, underscores, and hyphens", tools::toTitleCase(entity_type)))
  }

  # Check for duplicates based on entity type
  existing_ids <- switch(entity_type,
    result = names(model$results),
    hypothesis = names(model$hypotheses),
    account = names(model$accounts),
    phenomenon = names(model$phenomena),
    auxiliary = names(model$auxiliaries),
    c(names(model$results), names(model$hypotheses), names(model$accounts),
      names(model$phenomena), names(model$auxiliaries))
  )

  if (id %in% existing_ids) {
    stop(sprintf("%s with ID '%s' already exists", tools::toTitleCase(entity_type), id))
  }

  return(TRUE)
}

# ==============================================================================
# UI DEFINITION
# ==============================================================================
# Uses shinydashboard for layout with sidebar navigation and tabbed content.

ui <- dashboardPage(
  dashboardHeader(title = "ExplanatoryGraph"),

  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Build Model", tabName = "build"),
      menuItem("Analysis", tabName = "analysis"),
      menuItem("Visualization", tabName = "viz"),
      menuItem("Tutorial", tabName = "help"),
      # Hidden menu items for tab navigation (required by updateTabItems)
      div(style = "display: none;",
        menuItem("VOI", tabName = "voi"),
        menuItem("Sensitivity", tabName = "sensitivity")
      )
    ),

    # Advanced Features section
    tags$div(
      style = "padding: 0 15px; margin-top: 5px;",
      tags$p(
        style = "color: #b8c7ce; font-size: 14px; margin-bottom: 5px; padding: 10px 5px;",
        "Advanced Features"
      ),
      tags$div(
        style = "padding-left: 15px;",
        actionLink("nav_voi",
          tags$span(style = "font-size: 12px;", "Research Priorities"),
          style = "color: #b8c7ce; display: block; padding: 6px 5px;"
        ),
        actionLink("nav_sensitivity",
          tags$span(style = "font-size: 12px;", "Sensitivity Analysis"),
          style = "color: #b8c7ce; display: block; padding: 6px 5px;"
        )
      )
    ),

    hr(),

    # Load case study buttons
    actionButton("load_simple_case", "Load Simple Test", class = "btn-success"),
    actionButton("load_covid_case", "Load COVID-19 Example", class = "btn-info"),

    hr(),

    # Import/Export buttons
    fileInput("import_model", NULL,
              buttonLabel = tags$span(icon("upload"), " Import Model"),
              accept = ".json",
              width = "100%"),
    downloadButton("export_model", "Export Model",
                   icon = icon("download"), class = "btn-success btn-block")
  ),

  dashboardBody(
    useShinyjs(),

    # Include custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Inline CSS for sidebar import/export buttons (ensures no caching issues)
      tags$style(HTML("
        /* Hide file input extras */
        .main-sidebar .input-group .form-control[readonly],
        .main-sidebar .form-group.shiny-input-container > label.control-label,
        .main-sidebar .shiny-file-input-progress {
          display: none !important;
        }
        /* Remove all default spacing from file input container */
        .main-sidebar .form-group.shiny-input-container,
        .main-sidebar .input-group,
        .main-sidebar .input-group-btn {
          margin: 0 !important;
          padding: 0 !important;
          width: 100% !important;
          display: block !important;
        }
        /* All sidebar buttons: same margin, same spacing */
        .main-sidebar .btn {
          margin: 0 15px 6px 15px !important;
          width: calc(100% - 30px) !important;
        }
        /* Import/Export buttons: white background style */
        .main-sidebar .btn-file,
        .main-sidebar a.btn.shiny-download-link {
          display: block !important;
          background: #FFFFFF !important;
          color: #1E293B !important;
          border: 1px solid #E2E8F0 !important;
          border-radius: 6px !important;
          font-weight: 500 !important;
          padding: 8px 16px !important;
          text-align: center !important;
          text-decoration: none !important;
        }
        .main-sidebar .btn-file:hover,
        .main-sidebar a.btn.shiny-download-link:hover {
          background: #F1F5F9 !important;
          color: #1E293B !important;
        }
      "))
    ),

    # Global busy indicator
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      div(
        class = "shiny-busy-panel",
        div(class = "loading-spinner"),
        span(class = "loading-text", "Processing...")
      )
    ),

    tabItems(
      # ===== Build Model Tab =====
      tabItem(
        tabName = "build",
        fluidRow(
          box(
            title = "Build Explanatory Model",
            width = 12,
            status = "primary",
            solidHeader = TRUE,

            # Step indicator
            uiOutput("step_indicator"),

            hr(),

            # Step content (dynamic)
            uiOutput("step_content"),

            hr(),

            # Navigation buttons
            fluidRow(
              column(6,
                actionButton("prev_step", "← Previous", class = "btn-default")
              ),
              column(6, align = "right",
                actionButton("next_step", "Next →", class = "btn-primary")
              )
            )
          )
        )
      ),

      # ===== Analysis Tab =====
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(
            title = "Evidence Corpus",
            width = 12,
            status = "primary",
            solidHeader = TRUE,

            p("By default, these results are conditioned as TRUE during inference. Use Additional Conditions below to override for counterfactual analysis."),

            uiOutput("evidence_corpus_display")
          )
        ),

        fluidRow(
          box(
            title = "Additional Conditions",
            width = 12,
            status = "info",

            fluidRow(
              column(4,
                selectInput("cond_node", "Node:", choices = NULL)
              ),
              column(3,
                selectInput("cond_state", "State:",
                           choices = c("TRUE" = "true", "FALSE" = "false"))
              ),
              column(5,
                actionButton("add_condition", "Add Condition", class = "btn-info"),
                actionButton("clear_conditions", "Clear All")
              )
            ),

            hr(),

            h4("Current Conditions:"),
            verbatimTextOutput("current_conditions")
          )
        ),

        fluidRow(
          box(
            title = "Inference",
            width = 12,
            status = "success",
            solidHeader = TRUE,

            fluidRow(
              column(6,
                actionButton("run_inference", "Run Inference",
                            class = "btn-success btn-lg", icon = icon("calculator"))
              ),
              column(6,
                actionButton("save_inference", "Save Results...",
                            class = "btn-info", icon = icon("save")),
                span(style = "margin-left: 10px; color: gray;",
                     textOutput("saved_count_text", inline = TRUE))
              )
            )
          )
        ),

        fluidRow(
          box(
            title = "Results",
            width = 6,

            h4("Hypothesis Posteriors"),
            tableOutput("hypothesis_posteriors")
          ),

          box(
            title = "Account Posteriors",
            width = 6,

            tableOutput("account_posteriors")
          )
        )
      ),

      # ===== Research Priorities Tab =====
      tabItem(
        tabName = "voi",
        fluidRow(
          box(
            title = "Value of Information Analysis",
            width = 12,
            status = "primary",
            solidHeader = TRUE,

            p("Identify which uncertain nodes would be most valuable to investigate.",
              "EVPI (Expected Value of Perfect Information) quantifies the expected",
              "reduction in uncertainty from learning a node's true state."),

            tags$details(
              tags$summary(style = "cursor: pointer; color: #337ab7;", icon("info-circle"), " How to interpret results"),
              div(style = "margin: 10px 0; padding: 10px; background: #f8f8f8; border-radius: 4px;",
                tags$ul(
                  tags$li(
                    tags$strong("EVPI%"), " - Expected percentage reduction in entropy (uncertainty) from learning this node's true state. ",
                    "Computed as: P(node=true) × info_gain_if_true + P(node=false) × info_gain_if_false. ",
                    "Higher values indicate more valuable investigations. Note: nodes with high priors (e.g., auxiliaries at 0.9) ",
                    "may have low EVPI because the informative outcome (false) is unlikely."
                  ),
                  tags$li(
                    tags$strong("Gain if False"), " - Entropy reduction (in bits) if the node is found to be FALSE. ",
                    "Unlike EVPI, this ignores the probability of the outcome and shows the potential information gain. ",
                    "Particularly useful for auxiliaries with high priors, where falsification is unlikely but would be highly informative."
                  ),
                  tags$li(
                    tags$strong("Shift if False"), " - How much hypothesis probabilities would change if the node is FALSE. ",
                    "Measured as Total Variation Distance: the absolute change in P(H1) (equivalently, the average absolute change across all hypotheses). ",
                    "For example, a value of 0.48 means P(H1) would shift by 48 percentage points. ",
                    "Higher values indicate more decisive findings that would substantially change your conclusions."
                  ),
                  tags$li(
                    tags$strong("Shift"), " - Expected change in hypothesis probabilities, weighted by the probability of each outcome. ",
                    "Computed as: P(node=true) × shift_if_true + P(node=false) × shift_if_false. ",
                    "Similar to EVPI but measures probability shift rather than entropy reduction."
                  ),
                  tags$li(
                    tags$strong("Max Gain"), " - Best-case entropy reduction: how much uncertainty would decrease if you observed the most informative outcome. ",
                    "Computed as: current_entropy - min(entropy_if_true, entropy_if_false). ",
                    "Useful for identifying nodes where at least one outcome would be highly decisive."
                  ),
                  tags$li(
                    tags$strong("Asymmetry"), " - Difference in informativeness between the two outcomes: |entropy_if_true - entropy_if_false|. ",
                    "High asymmetry indicates one outcome is much more informative than the other (a potential 'smoking gun'). ",
                    "Low asymmetry means both outcomes are similarly informative."
                  )
                )
              )
            ),

            hr(),

            # Configuration panel
            fluidRow(
              column(4,
                selectInput("voi_starting_evidence", "Starting Evidence:",
                           choices = c("Baseline Marginals (Results=TRUE)" = "baseline",
                                      "Unconditional" = "unconditional")),
                helpText("Baseline: after observing results. Unconditional: before any evidence.")
              ),
              column(4,
                radioButtons("voi_target", "Target:",
                            choices = c("Hypotheses" = "hypotheses",
                                       "Accounts" = "accounts"),
                            selected = "hypotheses",
                            inline = TRUE),
                helpText("Hypotheses: aggregate uncertainty. Accounts: full distribution.")
              ),
              column(4,
                checkboxGroupInput("voi_node_types", "Include Node Types:",
                                  choices = c("Auxiliaries" = "auxiliaries",
                                             "Phenomena" = "phenomena",
                                             "Explanatory Links" = "links"),
                                  selected = c("auxiliaries", "phenomena", "links"),
                                  inline = TRUE),
                helpText("Select which node types to analyze.")
              )
            ),

            fluidRow(
              column(6,
                actionButton("voi_compute", "Compute Value of Information",
                            class = "btn-success btn-lg", icon = icon("calculator"))
              ),
              column(6,
                downloadButton("voi_export_csv", "Export CSV", class = "btn-info")
              )
            )
          )
        ),

        # Current Uncertainty Summary
        fluidRow(
          box(
            title = "Current Uncertainty",
            width = 12,
            status = "info",

            uiOutput("voi_current_uncertainty")
          )
        ),

        # Results Table
        fluidRow(
          box(
            title = "Research Priority Ranking",
            width = 12,
            status = "success",
            solidHeader = TRUE,

            DTOutput("voi_results_table"),

            p(class = "text-muted", style = "margin-top: 10px;",
              "Click a row to see detailed analysis below.")
          )
        ),

        # Detail Panel
        fluidRow(
          box(
            title = "Detailed Analysis",
            width = 12,
            status = "warning",

            uiOutput("voi_detail_panel")
          )
        )
      ),

      # ===== Sensitivity Analysis Tab =====
      tabItem(
        tabName = "sensitivity",
        fluidRow(
          box(
            title = "Sensitivity Analysis - Parameter Sweep",
            width = 12,
            status = "primary",
            solidHeader = TRUE,

            p("Explore how hypothesis posteriors change as model parameters are varied.",
              "Select a parameter, configure the sweep range, and visualize the sensitivity."),

            tags$details(
              tags$summary(style = "cursor: pointer; color: #337ab7;", icon("info-circle"), " How to use"),
              div(style = "margin: 10px 0; padding: 10px; background: #f8f8f8; border-radius: 4px;",
                tags$ol(
                  tags$li("Select a parameter from the tree on the left"),
                  tags$li("Configure the sweep range (default: current value ± 0.3)"),
                  tags$li("Click 'Run Sweep' to compute posteriors across the range"),
                  tags$li("Examine the plot to see how conclusions depend on this parameter")
                ),
                p(style = "margin-top: 10px;",
                  tags$strong("Interpretation:"),
                  " Steep lines indicate high sensitivity - small parameter changes",
                  " significantly affect conclusions. Flat lines indicate robustness.")
              )
            )
          )
        ),

        # Current Evidence State
        fluidRow(
          box(
            title = "Current Evidence State",
            width = 12,
            status = "info",
            collapsible = TRUE,
            collapsed = TRUE,

            uiOutput("sensitivity_evidence_display")
          )
        ),

        # Main content: parameter selection and configuration
        fluidRow(
          # Parameter Selection Panel
          box(
            title = "Parameter Selection",
            width = 4,
            status = "primary",

            uiOutput("sensitivity_param_tree"),

            hr(),

            # Selected parameter info
            uiOutput("sensitivity_param_info")
          ),

          # Sweep Configuration Panel
          box(
            title = "Sweep Configuration",
            width = 4,
            status = "primary",

            uiOutput("sensitivity_selected_display"),

            hr(),

            fluidRow(
              column(6,
                numericInput("sensitivity_range_min", "Min:", value = 0.01, min = 0.01, max = 0.98, step = 0.05)
              ),
              column(6,
                numericInput("sensitivity_range_max", "Max:", value = 0.99, min = 0.02, max = 0.99, step = 0.05)
              )
            ),

            actionButton("sensitivity_full_range", "Full Range (0.01-0.99)", class = "btn-default btn-sm"),

            hr(),

            radioButtons("sensitivity_target", "Target:",
                        choices = c("Hypotheses" = "hypotheses",
                                   "Accounts" = "accounts"),
                        selected = "hypotheses",
                        inline = TRUE),

            hr(),

            fluidRow(
              column(6,
                actionButton("sensitivity_run", "Run Sweep",
                            class = "btn-success btn-lg", icon = icon("play"))
              ),
              column(6,
                downloadButton("sensitivity_export_csv", "Export CSV", class = "btn-info"),
                downloadButton("sensitivity_export_png", "Export PNG", class = "btn-info")
              )
            )
          ),

          # Status/Progress Panel
          box(
            title = "Status",
            width = 4,
            status = "info",

            uiOutput("sensitivity_status")
          )
        ),

        # Results: Plot
        fluidRow(
          box(
            title = "Sweep Results",
            width = 12,
            status = "success",
            solidHeader = TRUE,

            plotOutput("sensitivity_plot", height = "450px")
          )
        ),

        # Results: Summary Table
        fluidRow(
          box(
            title = "Summary Statistics",
            width = 12,
            status = "warning",

            DTOutput("sensitivity_summary_table")
          )
        )
      ),

      # ===== Visualization Tab =====
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            title = "Network Visualization",
            width = 12,
            status = "success",
            solidHeader = TRUE,

            fluidRow(
              column(3,
                selectInput("viz_view", "View:",
                           choices = c("Full Bayesian Network" = "full")),
                div(style = "margin-left: 10px;",
                  checkboxInput("viz_hide_aux", "Hide Auxiliaries", value = FALSE)
                )
              ),
              column(5,
                radioButtons("viz_color_mode", "Color Mode:",
                            choices = c("Network Structure" = "structure",
                                       "Baseline Marginals (Results=TRUE)" = "baseline",
                                       "Analysis Posterior (Custom Conditions from Analysis Tab)" = "posterior",
                                       "Change (Δ)" = "delta",
                                       "Log-Ratio" = "ratio",
                                       "Research Priority" = "evpi"),
                            inline = FALSE)
              ),
              column(4,
                conditionalPanel(
                  condition = "input.viz_color_mode == 'posterior'",
                  h5("Display:"),
                  selectInput("posterior_select", "Marginals:",
                             choices = c("Current Posterior" = "current"))
                ),
                conditionalPanel(
                  condition = "input.viz_color_mode == 'delta' || input.viz_color_mode == 'ratio'",
                  h5("Compare:"),
                  selectInput("compare_from", "From:",
                             choices = c("Unconditional" = "unconditional",
                                        "Baseline Marginals (Results=TRUE)" = "baseline")),
                  selectInput("compare_to", "To:",
                             choices = c("Unconditional" = "unconditional",
                                        "Baseline Marginals (Results=TRUE)" = "baseline"))
                )
              )
            ),

            hr(),

            visNetworkOutput("network_viz", height = "600px")
          )
        ),

        fluidRow(
          box(
            title = "Legend",
            width = 12,
            status = "info",

            uiOutput("viz_legend")
          )
        )
      ),

      # ===== Help Tab =====
      tabItem(
        tabName = "help",
        fluidRow(
          column(
            width = 10, offset = 1,
            div(
              style = "background: white; padding: 30px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); zoom: 1.1;",
              includeMarkdown("docs/tutorial.md")
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
# Handles user interactions, model building, inference, and visualization.

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # REACTIVE STATE
  # ---------------------------------------------------------------------------
  # Core application state stored in reactiveValues for Shiny reactivity.

  rv <- reactiveValues(
    current_step = 1,
    model = ExplanatoryModel$new(name = "Untitled Model"),
    selected_account_id = NULL,
    # Unconditional marginals (no evidence)
    unconditional = NULL,
    # Baseline marginals (conditioned on all results = TRUE)
    baseline = NULL,
    # Posterior marginals (conditioned on results = TRUE + additional conditions)
    posteriors = NULL,
    # Saved inference results for comparison (max 10, session-scoped)
    saved_inferences = list(),
    # User preference for network change warnings
    suppress_invalidation_warning = FALSE,
    # Pending action when waiting for invalidation confirmation
    pending_invalidation_action = NULL,
    # VOI Analysis state
    evpi_results = NULL,           # Results from compute_all_evpi()
    evpi_selected_node = NULL,     # Currently selected node in detail view
    evpi_computing = FALSE,        # Flag for computation in progress
    evpi_cache = list(),           # Cache for EVPI results keyed by configuration
    evpi_cache_key = NULL,         # Current cache key for cache hit detection
    # Sensitivity Analysis state
    sensitivity_params = NULL,     # Data frame of sweepable parameters
    sensitivity_selected = NULL,   # Currently selected parameter spec
    sensitivity_result = NULL,     # Result from compute_parameter_sweep()
    sensitivity_computing = FALSE  # Flag for computation in progress
  )

  # ---------------------------------------------------------------------------
  # ADVANCED FEATURES NAVIGATION
  # ---------------------------------------------------------------------------
  # Handle clicks on the Advanced Features links in the sidebar

  observeEvent(input$nav_voi, {
    updateTabItems(session, "sidebar_menu", "voi")
  })

  observeEvent(input$nav_sensitivity, {
    updateTabItems(session, "sidebar_menu", "sensitivity")
  })

  # ---------------------------------------------------------------------------
  # STEP INDICATOR UI
  # ---------------------------------------------------------------------------
  # Visual progress indicator for the model-building wizard.

  output$step_indicator <- renderUI({
    steps <- c("Evidence Corpus", "Hypotheses", "Accounts", "Review")
    current <- rv$current_step

    step_items <- lapply(1:4, function(i) {
      status <- if (i < current) "completed"
                else if (i == current) "current"
                else "pending"

      icon_html <- if (i < current) {
        icon("check")
      } else {
        tags$span(i)
      }

      tags$div(
        class = paste("step-item", status),
        tags$div(class = "step-circle", icon_html),
        tags$div(class = "step-label", steps[i])
      )
    })

    div(class = "step-wizard", step_items)
  })

  # ---------------------------------------------------------------------------
  # STEP CONTENT (Dynamic UI)
  # ---------------------------------------------------------------------------
  # Renders the appropriate UI for each step of the model-building wizard.

  output$step_content <- renderUI({
    switch(rv$current_step,
      "1" = step1_evidence_ui(),
      "2" = step2_hypotheses_ui(),
      "3" = step3_accounts_ui(),
      "4" = step4_review_ui()
    )
  })

  # ===== Step 1: Evidence Corpus UI =====

  step1_evidence_ui <- function() {
    tagList(
      h3("Step 1: Define Evidence Corpus"),
      p("Specify the empirical results that all explanatory accounts must explain."),

      DTOutput("results_table"),

      hr(),

      h4("Add Result"),
      textInput("result_id", "ID:"),
      textAreaInput("result_text", "Description:", rows = 3),
      textInput("result_source", "Source/Citation (optional):"),
      actionButton("add_result", "Add Result", class = "btn-success")
    )
  }

  # ===== Step 2: Hypotheses UI =====

  step2_hypotheses_ui <- function() {
    tagList(
      h3("Step 2: Define Competing Hypotheses"),
      p("Create the hypotheses you want to compare. Priors must sum to 1.0."),

      DTOutput("hypotheses_table"),

      hr(),

      h4("Add Hypothesis"),
      textInput("hyp_id", "ID:"),
      textAreaInput("hyp_text", "Description:", rows = 3),
      numericInput("hyp_prior", "Prior:", value = 0.5, min = 0, max = 1, step = 0.01),
      actionButton("add_hypothesis", "Add Hypothesis", class = "btn-success"),

      hr(),

      uiOutput("prior_validation")
    )
  }

  # ===== Step 3: Accounts UI =====

  step3_accounts_ui <- function() {
    tagList(
      h3("Step 3: Build Explanatory Accounts"),
      p("Construct explanatory pathways linking hypotheses to results via phenomena."),

      fluidRow(
        # Left panel: Account list
        column(4,
          h4("Accounts"),
          uiOutput("accounts_list"),

          hr(),

          h4("Add Account"),
          selectInput("new_account_hyp", "Hypothesis:",
                     choices = NULL),
          textInput("new_account_id", "Account ID:"),
          textAreaInput("new_account_desc", "Description:", rows = 2),
          actionButton("add_account", "Add Account", class = "btn-success")
        ),

        # Right panel: Account editor
        column(8,
          conditionalPanel(
            condition = "output.account_selected",

            h4("Edit Account"),
            verbatimTextOutput("selected_account_info"),

            hr(),

            h4("Auxiliaries"),
            p("Background assumptions used by this account."),
            uiOutput("account_auxiliaries"),

            actionButton("add_auxiliary_to_account", "Add Auxiliary", icon = icon("plus")),

            hr(),

            h4("Explanatory Chains"),
            p("Each chain connects the account to a result via a phenomenon."),

            uiOutput("chains_list"),

            hr(),

            actionButton("add_chain", "Add Chain", class = "btn-primary", icon = icon("plus"))
          ),

          conditionalPanel(
            condition = "!output.account_selected",
            div(
              style = "text-align: center; padding: 50px; color: gray;",
              icon("arrow-left", style = "font-size: 48px;"),
              h4("Select an account to edit")
            )
          )
        )
      )
    )
  }

  # ===== Step 4: Review UI =====

  step4_review_ui <- function() {
    tagList(
      h3("Step 4: Review Model"),

      fluidRow(
        valueBox(
          length(rv$model$hypotheses),
          "Hypotheses",
          icon = icon("lightbulb"),
          color = "blue"
        ),
        valueBox(
          length(rv$model$accounts),
          "Accounts",
          icon = icon("project-diagram"),
          color = "green"
        ),
        valueBox(
          length(rv$model$results),
          "Results",
          icon = icon("file-alt"),
          color = "orange"
        )
      ),

      hr(),

      h4("Validation"),
      uiOutput("model_validation"),

      hr(),

      actionButton("build_network", "Build Network for Analysis",
                  class = "btn-success btn-lg", icon = icon("cogs"))
    )
  }

  # ---------------------------------------------------------------------------
  # NAVIGATION BUTTONS
  # ---------------------------------------------------------------------------

  observeEvent(input$prev_step, {
    if (rv$current_step > 1) {
      rv$current_step <- rv$current_step - 1
    }
  })

  observeEvent(input$next_step, {
    if (rv$current_step < 4) {
      rv$current_step <- rv$current_step + 1
    }
  })

  # Update button states
  observe({
    shinyjs::toggleState("prev_step", rv$current_step > 1)
    shinyjs::toggleState("next_step", rv$current_step < 4)
  })

  # ===== Step 1: Results Table =====

  output$results_table <- renderDT({
    if (length(rv$model$results) == 0) {
      return(data.frame(ID = character(), Description = character(), Source = character()))
    }

    df <- data.frame(
      ID = names(rv$model$results),
      Description = sapply(rv$model$results, function(r) substr(r$text, 1, 80)),
      Source = sapply(rv$model$results, function(r) r$source),
      stringsAsFactors = FALSE
    )

    datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })

  # Add result
  observeEvent(input$add_result, {
    req(input$result_id, input$result_text)

    tryCatch({
      result_id <- trimws(input$result_id)
      validate_id(result_id, rv$model, "result")

      result <- BayesianResult$new(
        id = result_id,
        text = trimws(input$result_text),
        source = trimws(input$result_source)
        # Uses DEFAULT_RESULT_STRENGTH and DEFAULT_RESULT_BASE_RATE from bayesian_spec_classes.R
      )

      rv$model$add_result(result)

      # Clear inputs
      updateTextInput(session, "result_id", value = "")
      updateTextAreaInput(session, "result_text", value = "")
      updateTextInput(session, "result_source", value = "")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })

  # ===== Step 2: Hypotheses Table =====

  output$hypotheses_table <- renderDT({
    if (length(rv$model$hypotheses) == 0) {
      return(data.frame(ID = character(), Description = character(), Prior = numeric()))
    }

    df <- data.frame(
      ID = names(rv$model$hypotheses),
      Description = sapply(rv$model$hypotheses, function(h) substr(h$text, 1, 80)),
      Prior = sapply(rv$model$hypotheses, function(h) h$prior),
      stringsAsFactors = FALSE
    )

    datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })

  # Add hypothesis
  observeEvent(input$add_hypothesis, {
    req(input$hyp_id, input$hyp_text)

    tryCatch({
      hyp_id <- trimws(input$hyp_id)
      validate_id(hyp_id, rv$model, "hypothesis")

      hypothesis <- BayesianHypothesis$new(
        id = hyp_id,
        text = trimws(input$hyp_text),
        source = "User",
        prior = input$hyp_prior
      )

      rv$model$add_hypothesis(hypothesis)

      # Clear inputs
      updateTextInput(session, "hyp_id", value = "")
      updateTextAreaInput(session, "hyp_text", value = "")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })

  # Prior validation
  output$prior_validation <- renderUI({
    if (length(rv$model$hypotheses) == 0) {
      return(NULL)
    }

    prior_sum <- sum(sapply(rv$model$hypotheses, function(h) h$prior))

    if (rv$model$validate_hypothesis_priors()) {
      div(
        style = "color: green;",
        icon("check-circle"),
        sprintf(" Priors sum to %.3f (valid)", prior_sum)
      )
    } else {
      div(
        style = "color: red;",
        icon("exclamation-triangle"),
        sprintf(" Priors sum to %.3f (must equal 1.0)", prior_sum)
      )
    }
  })

  # ===== Step 3: Accounts =====

  # Update hypothesis choices for new account
  observe({
    req(rv$model)
    req(length(rv$model$hypotheses) > 0)

    choices <- setNames(names(rv$model$hypotheses),
                       sapply(rv$model$hypotheses, function(h) h$text))
    updateSelectInput(session, "new_account_hyp", choices = choices)
  })

  # Accounts list
  output$accounts_list <- renderUI({
    if (length(rv$model$accounts) == 0) {
      return(p("No accounts yet. Add one below."))
    }

    # Group by hypothesis
    account_buttons <- lapply(names(rv$model$hypotheses), function(h_id) {
      hypothesis <- rv$model$hypotheses[[h_id]]
      h_accounts <- rv$model$get_accounts_for_hypothesis(h_id)

      if (length(h_accounts) == 0) {
        return(NULL)
      }

      tagList(
        h5(hypothesis$text),
        lapply(h_accounts, function(acc) {
          actionButton(
            paste0("select_acc_", acc$id),
            acc$id,
            class = if (identical(rv$selected_account_id, acc$id)) "btn-primary" else "btn-default",
            style = "margin: 2px;"
          )
        }),
        br()
      )
    })

    tagList(account_buttons)
  })

  # Add account
  observeEvent(input$add_account, {
    req(input$new_account_id, input$new_account_hyp)

    tryCatch({
      account_id <- trimws(input$new_account_id)
      validate_id(account_id, rv$model, "account")

      account <- Account$new(
        id = account_id,
        hypothesis_id = input$new_account_hyp,
        description = trimws(input$new_account_desc)
      )

      rv$model$add_account(account)

      # Clear inputs
      updateTextInput(session, "new_account_id", value = "")
      updateTextAreaInput(session, "new_account_desc", value = "")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })

  # Account selection (dynamic observers)
  observe({
    lapply(names(rv$model$accounts), function(acc_id) {
      observeEvent(input[[paste0("select_acc_", acc_id)]], {
        rv$selected_account_id <- acc_id
      })
    })
  })

  # Account selected flag
  output$account_selected <- reactive({
    !is.null(rv$selected_account_id) && rv$selected_account_id %in% names(rv$model$accounts)
  })
  outputOptions(output, "account_selected", suspendWhenHidden = FALSE)

  # Selected account info
  output$selected_account_info <- renderText({
    req(rv$selected_account_id)
    account <- rv$model$accounts[[rv$selected_account_id]]
    sprintf("Account: %s\nHypothesis: %s\nDescription: %s",
            account$id, account$hypothesis_id, account$description)
  })

  # Chains list
  output$chains_list <- renderUI({
    req(rv$selected_account_id)
    account <- rv$model$accounts[[rv$selected_account_id]]

    if (length(account$explanatory_link_ids) == 0) {
      return(p("No chains yet. Click 'Add Chain' to create one."))
    }

    # Get chains (pairs of links: Account->PC and PC->R)
    # For now, just list the links
    chain_items <- lapply(account$explanatory_link_ids, function(link_id) {
      link <- rv$model$explanatory_links[[link_id]]
      if (is.null(link)) return(NULL)

      div(
        style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0;",
        sprintf("%s → %s", link$source_id, link$target_id),
        tags$br(),
        sprintf("Reliability: %.2f, Epsilon: %.2f", link$reliability, link$epsilon),
        if (length(link$required_auxiliary_ids) > 0) {
          tagList(tags$br(), sprintf("Requires: %s", paste(link$required_auxiliary_ids, collapse = ", ")))
        }
      )
    })

    tagList(chain_items)
  })

  # Add chain modal
  observeEvent(input$add_chain, {
    req(rv$selected_account_id)

    showModal(modalDialog(
      title = "Add Explanatory Chain",
      size = "l",

      p("Build a chain: Account → Phenomenon → Result"),

      h4("Link 1: Account → Phenomenon"),
      selectInput("chain_phenomenon", "Phenomenon:",
                 choices = c("(Create new...)" = "_new_",
                            setNames(names(rv$model$phenomena),
                                    sapply(rv$model$phenomena, function(p) p$text)))),

      conditionalPanel(
        condition = "input.chain_phenomenon == '_new_'",
        textInput("chain_new_pc_id", "New Phenomenon ID:"),
        textAreaInput("chain_new_pc_text", "Description:", rows = 2)
      ),

      selectInput("chain_link1_aux", "Required Auxiliaries:",
                 choices = setNames(names(rv$model$auxiliaries),
                                  sapply(rv$model$auxiliaries, function(a) a$text)),
                 multiple = TRUE),

      hr(),

      h4("Link 2: Phenomenon → Result"),
      selectInput("chain_result", "Result:",
                 choices = setNames(names(rv$model$results),
                                  sapply(rv$model$results, function(r) r$text))),

      selectInput("chain_link2_aux", "Required Auxiliaries:",
                 choices = setNames(names(rv$model$auxiliaries),
                                  sapply(rv$model$auxiliaries, function(a) a$text)),
                 multiple = TRUE),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("create_chain", "Create Chain", class = "btn-primary")
      )
    ))
  })

  # Create chain
  observeEvent(input$create_chain, {
    req(rv$selected_account_id, input$chain_result)

    tryCatch({
      account <- rv$model$accounts[[rv$selected_account_id]]

      # Handle phenomenon (new or existing)
      pc_id <- input$chain_phenomenon
      if (pc_id == "_new_") {
        req(input$chain_new_pc_id, input$chain_new_pc_text)
        pc_id <- trimws(input$chain_new_pc_id)
        validate_id(pc_id, rv$model, "phenomenon")

        # Create new phenomenon (uses DEFAULT_PHENOMENON_STRENGTH and DEFAULT_PHENOMENON_BASE_RATE)
        pc <- BayesianPhenomenon$new(
          id = pc_id,
          text = trimws(input$chain_new_pc_text),
          source = "User"
        )
        rv$model$add_phenomenon(pc)
      }

      # Add phenomenon to account
      account$add_phenomenon_id(pc_id)

      # Create Link 1: Account -> Phenomenon (uses DEFAULT_LINK_RELIABILITY and DEFAULT_LINK_EPSILON)
      link1_id <- paste0("EL_", account$id, "_", pc_id)
      link1 <- BayesianExplanatoryLink$new(
        id = link1_id,
        source_id = account$id,
        target_id = pc_id,
        text = sprintf("%s explains %s", account$id, pc_id),
        source = "User"
      )
      link1$required_auxiliary_ids <- input$chain_link1_aux
      rv$model$add_explanatory_link(link1)
      account$add_explanatory_link_id(link1_id)

      # Add auxiliaries to account
      for (aux_id in input$chain_link1_aux) {
        account$add_auxiliary_id(aux_id)
      }

      # Create Link 2: Phenomenon -> Result (uses DEFAULT_LINK_RELIABILITY and DEFAULT_LINK_EPSILON)
      link2_id <- paste0("EL_", pc_id, "_", input$chain_result)
      link2 <- BayesianExplanatoryLink$new(
        id = link2_id,
        source_id = pc_id,
        target_id = input$chain_result,
        text = sprintf("%s explains %s", pc_id, input$chain_result),
        source = "User"
      )
      link2$required_auxiliary_ids <- input$chain_link2_aux
      rv$model$add_explanatory_link(link2)
      account$add_explanatory_link_id(link2_id)

      # Add auxiliaries to account
      for (aux_id in input$chain_link2_aux) {
        account$add_auxiliary_id(aux_id)
      }

      removeModal()
      showNotification("Chain created successfully!", type = "message")
    }, error = function(e) {
      showNotification(e$message, type = "error")
      removeModal()
    })
  })

  # Account auxiliaries
  output$account_auxiliaries <- renderUI({
    req(rv$selected_account_id)
    account <- rv$model$accounts[[rv$selected_account_id]]

    if (length(account$auxiliary_ids) == 0) {
      return(p("No auxiliaries in this account."))
    }

    aux_items <- lapply(account$auxiliary_ids, function(aux_id) {
      auxiliary <- rv$model$auxiliaries[[aux_id]]
      if (is.null(auxiliary)) return(NULL)

      div(
        style = "border: 1px solid #ddd; padding: 5px; margin: 3px 0;",
        sprintf("%s: %s (prior: %.2f)", aux_id, substr(auxiliary$text, 1, 60), auxiliary$prior)
      )
    })

    tagList(aux_items)
  })

  # Add auxiliary modal
  observeEvent(input$add_auxiliary_to_account, {
    req(rv$selected_account_id)

    showModal(modalDialog(
      title = "Add Auxiliary",

      selectInput("aux_select", "Auxiliary:",
                 choices = c("(Create new...)" = "_new_",
                            setNames(names(rv$model$auxiliaries),
                                    sapply(rv$model$auxiliaries, function(a) a$text)))),

      conditionalPanel(
        condition = "input.aux_select == '_new_'",
        textInput("new_aux_id", "ID:"),
        textAreaInput("new_aux_text", "Description:", rows = 2),
        numericInput("new_aux_prior", "Prior:", value = 0.8, min = 0, max = 1, step = 0.01)
      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("add_aux_confirm", "Add", class = "btn-primary")
      )
    ))
  })

  # Confirm add auxiliary
  observeEvent(input$add_aux_confirm, {
    req(rv$selected_account_id)

    tryCatch({
      account <- rv$model$accounts[[rv$selected_account_id]]
      aux_id <- input$aux_select

      # Create new if needed
      if (aux_id == "_new_") {
        req(input$new_aux_id, input$new_aux_text)
        aux_id <- trimws(input$new_aux_id)
        validate_id(aux_id, rv$model, "auxiliary")

        auxiliary <- BayesianAuxiliary$new(
          id = aux_id,
          text = trimws(input$new_aux_text),
          source = "User",
          prior = input$new_aux_prior
        )
        rv$model$add_auxiliary(auxiliary)
      }

      account$add_auxiliary_id(aux_id)

      removeModal()
    }, error = function(e) {
      showNotification(e$message, type = "error")
      removeModal()
    })
  })

  # ===== Step 4: Review and Validation =====

  # Model validation
  output$model_validation <- renderUI({
    issues <- list()

    # Check hypothesis priors
    if (!rv$model$validate_hypothesis_priors()) {
      issues <- c(issues, "Hypothesis priors must sum to 1.0")
    }

    # Check at least 2 hypotheses
    if (length(rv$model$hypotheses) < 2) {
      issues <- c(issues, "At least 2 hypotheses required")
    }

    # Check at least 1 result
    if (length(rv$model$results) == 0) {
      issues <- c(issues, "At least 1 result required")
    }

    # Check each account has chains
    for (acc_id in names(rv$model$accounts)) {
      account <- rv$model$accounts[[acc_id]]
      if (length(account$explanatory_link_ids) == 0) {
        issues <- c(issues, sprintf("Account %s has no explanatory chains", acc_id))
      }
    }

    if (length(issues) == 0) {
      div(
        style = "color: green; font-size: 16px;",
        icon("check-circle"),
        " Model is ready to build!"
      )
    } else {
      div(
        style = "color: red;",
        icon("exclamation-triangle"),
        " Issues to fix:",
        tags$ul(
          lapply(issues, function(issue) tags$li(issue))
        )
      )
    }
  })

  # ---------------------------------------------------------------------------
  # NETWORK BUILDING
  # ---------------------------------------------------------------------------
  # Converts the ExplanatoryModel to a Bayesian network with CPTs.

  build_network_for_analysis <- function(switch_to_analysis = TRUE) {
    # Validate first
    if (!rv$model$validate_hypothesis_priors()) {
      showNotification("Fix validation issues first", type = "error")
      return(FALSE)
    }

    tryCatch({
      rv$bayesian_network <- build_bayesian_network(rv$model)
      rv$selector <- Selector$new(rv$model)

      # Clear VOI cache when model/network changes
      rv$evpi_cache <- list()
      rv$evpi_results <- NULL
      rv$evpi_cache_key <- NULL

      # Generate all CPTs using helper function
      cpts <- generate_all_cpts_for_model(rv$model, rv$bayesian_network, rv$selector)

      # Compute unconditional marginals (no evidence - true priors)
      unconditional <- compute_posteriors(rv$bayesian_network, cpts, list())

      # Compute baseline marginals (conditioned on all results being true)
      evidence_results <- build_results_evidence(rv$model)
      baseline <- compute_posteriors(rv$bayesian_network, cpts, evidence_results)

      rv$unconditional <- unconditional
      rv$baseline <- baseline
      rv$cpts <- cpts

      if (switch_to_analysis) {
        showNotification("Network built successfully! Go to Analysis tab.", type = "message")
        # Switch to Analysis tab
        updateTabItems(session, "sidebar_menu", "analysis")
      }

      return(TRUE)

    }, error = function(e) {
      showNotification(paste("Error building network:", e$message), type = "error")
      return(FALSE)
    })
  }

  # Build network button
  observeEvent(input$build_network, {
    # Check if warning needed for saved inferences
    if (length(rv$saved_inferences) > 0 && !rv$suppress_invalidation_warning) {
      show_invalidation_warning("rebuild the Bayesian network", "build_network")
      return()  # Wait for user confirmation
    }

    # Clear saved inferences if any (silently if warnings suppressed)
    if (length(rv$saved_inferences) > 0) {
      rv$saved_inferences <- list()
      showNotification("Saved inferences cleared due to network rebuild.", type = "warning")
    }

    build_network_for_analysis(switch_to_analysis = TRUE)
  })

  # ---------------------------------------------------------------------------
  # ANALYSIS TAB
  # ---------------------------------------------------------------------------
  # Handles evidence corpus display, additional conditions, and inference.

  # Evidence corpus display
  output$evidence_corpus_display <- renderUI({
    if (length(rv$model$results) == 0) {
      return(p("No results defined. Go to Build Model tab."))
    }

    result_items <- lapply(rv$model$results, function(r) {
      div(
        style = "border: 1px solid green; padding: 10px; margin: 5px 0; background: #f0fff0;",
        icon("check-circle", style = "color: green;"),
        sprintf(" %s: %s", r$id, r$text)
      )
    })

    tagList(result_items)
  })

  # Update condition node choices
  observe({
    req(rv$model)

    # Combine results, auxiliaries, phenomena, and links
    choices <- c()

    # Results can be overridden (default is TRUE, but can be set to FALSE for counterfactual analysis)
    if (length(rv$model$results) > 0) {
      choices <- c(choices,
        setNames(names(rv$model$results),
                paste("RESULT:", sapply(rv$model$results, function(r) r$text)))
      )
    }

    if (length(rv$model$auxiliaries) > 0) {
      choices <- c(choices,
        setNames(names(rv$model$auxiliaries),
                paste("AUX:", sapply(rv$model$auxiliaries, function(a) a$text)))
      )
    }

    if (length(rv$model$phenomena) > 0) {
      choices <- c(choices,
        setNames(names(rv$model$phenomena),
                paste("PC:", sapply(rv$model$phenomena, function(p) p$text)))
      )
    }

    if (length(rv$model$explanatory_links) > 0) {
      choices <- c(choices,
        setNames(names(rv$model$explanatory_links),
                paste("LINK:", sapply(rv$model$explanatory_links, function(l) l$text)))
      )
    }

    updateSelectInput(session, "cond_node", choices = choices)
  })

  # Additional conditions
  rv$additional_conditions <- list()

  observeEvent(input$add_condition, {
    req(input$cond_node)

    rv$additional_conditions[[input$cond_node]] <- input$cond_state
  })

  observeEvent(input$clear_conditions, {
    rv$additional_conditions <- list()
  })

  output$current_conditions <- renderText({
    if (length(rv$additional_conditions) == 0) {
      return("(none)")
    }

    paste(
      sapply(names(rv$additional_conditions), function(node_id) {
        sprintf("%s = %s", node_id, rv$additional_conditions[[node_id]])
      }),
      collapse = "\n"
    )
  })

  # Run inference
  observeEvent(input$run_inference, {
    req(rv$bayesian_network, rv$selector)

    tryCatch({
      # Generate all CPTs using helper function
      cpts <- generate_all_cpts_for_model(rv$model, rv$bayesian_network, rv$selector)

      # Build evidence list: all results TRUE plus additional conditions
      evidence <- build_results_evidence(rv$model)
      for (node_id in names(rv$additional_conditions)) {
        evidence[[node_id]] <- rv$additional_conditions[[node_id]]
      }

      # Run inference
      posteriors <- compute_posteriors(rv$bayesian_network, cpts, evidence)

      # Also recompute baseline for visualization (conditioned on results being true only)
      baseline <- compute_posteriors(rv$bayesian_network, cpts, build_results_evidence(rv$model))

      rv$posteriors <- posteriors
      rv$baseline <- baseline
      rv$cpts <- cpts

      showNotification("Inference completed successfully!", type = "message")

    }, error = function(e) {
      showNotification(paste("Inference error:", e$message), type = "error")
    })
  })

  # ---------------------------------------------------------------------------
  # SAVE INFERENCE FUNCTIONALITY
  # ---------------------------------------------------------------------------

  # Saved inference count display
  output$saved_count_text <- renderText({
    count <- length(rv$saved_inferences)
    if (count > 0) sprintf("(%d/10 saved)", count) else ""
  })

  # Save inference button - show modal
  observeEvent(input$save_inference, {
    req(rv$posteriors)

    # Check limit
    if (length(rv$saved_inferences) >= 10) {
      showNotification("Maximum 10 saved inferences reached. Delete some first.", type = "warning")
      return()
    }

    # Generate auto-name
    auto_name <- generate_inference_name(rv$additional_conditions)

    showModal(modalDialog(
      title = "Save Inference Results",

      textInput("save_inference_name", "Name:", value = auto_name),

      p(style = "color: gray; font-size: 12px;",
        sprintf("Conditions: %s",
                if (length(rv$additional_conditions) == 0) "(none)"
                else paste(names(rv$additional_conditions), "=", rv$additional_conditions, collapse = ", "))),

      if (length(rv$saved_inferences) > 0) {
        tagList(
          hr(),
          h5("Currently Saved:"),
          uiOutput("saved_list_in_modal")
        )
      },

      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_save", "Save", class = "btn-primary")
      )
    ))
  })

  # List of saved inferences in modal
  output$saved_list_in_modal <- renderUI({
    if (length(rv$saved_inferences) == 0) return(NULL)

    lapply(names(rv$saved_inferences), function(name) {
      saved <- rv$saved_inferences[[name]]
      div(
        style = "display: flex; justify-content: space-between; align-items: center; padding: 5px 0; border-bottom: 1px solid #eee;",
        span(name, style = "flex: 1;"),
        span(style = "color: gray; font-size: 11px; margin-right: 10px;",
             format(saved$timestamp, "%H:%M")),
        actionButton(
          inputId = paste0("delete_saved_", gsub("[^a-zA-Z0-9]", "_", name)),
          label = NULL,
          icon = icon("trash"),
          class = "btn-xs btn-danger"
        )
      )
    })
  })

  # Confirm save
  observeEvent(input$confirm_save, {
    req(input$save_inference_name, rv$posteriors)

    name <- trimws(input$save_inference_name)

    # Validate name
    if (nchar(name) == 0) {
      showNotification("Name cannot be empty", type = "error")
      return()
    }

    if (name %in% names(rv$saved_inferences)) {
      showNotification("Name already exists. Choose a different name.", type = "error")
      return()
    }

    # Save the inference
    rv$saved_inferences[[name]] <- list(
      name = name,
      auto_name = generate_inference_name(rv$additional_conditions),
      marginals = rv$posteriors,
      conditions = rv$additional_conditions,
      timestamp = Sys.time()
    )

    removeModal()
    showNotification(sprintf("Saved: %s", name), type = "message")
  })

  # Dynamic delete observers for saved inferences
  observe({
    lapply(names(rv$saved_inferences), function(name) {
      btn_id <- paste0("delete_saved_", gsub("[^a-zA-Z0-9]", "_", name))
      observeEvent(input[[btn_id]], {
        rv$saved_inferences[[name]] <- NULL
        showNotification(sprintf("Deleted: %s", name), type = "message")
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # Hypothesis posteriors
  output$hypothesis_posteriors <- renderTable({
    req(rv$posteriors, rv$selector)

    # Marginalize account posteriors to hypotheses
    hyp_posteriors <- list()

    for (h_id in names(rv$model$hypotheses)) {
      h_accounts <- rv$model$get_accounts_for_hypothesis(h_id)

      # Sum posteriors of accounts for this hypothesis
      post_sum <- sum(sapply(h_accounts, function(acc) {
        rv$posteriors[["Selector"]][[acc$id]]
      }))

      hyp_posteriors[[h_id]] <- post_sum
    }

    df <- data.frame(
      Hypothesis = names(hyp_posteriors),
      Prior = sapply(rv$model$hypotheses, function(h) h$prior),
      Posterior = unlist(hyp_posteriors),
      Change = unlist(hyp_posteriors) - sapply(rv$model$hypotheses, function(h) h$prior),
      stringsAsFactors = FALSE
    )

    df
  }, digits = 3)

  # Account posteriors
  output$account_posteriors <- renderTable({
    req(rv$posteriors, rv$selector)

    account_ids <- names(rv$model$accounts)
    prior_vector <- rv$selector$get_prior_vector()

    df <- data.frame(
      Account = account_ids,
      Hypothesis = sapply(rv$model$accounts, function(a) a$hypothesis_id),
      # Index priors by account name to ensure correct alignment
      Prior = sapply(account_ids, function(acc_id) prior_vector[[acc_id]]),
      Posterior = sapply(account_ids, function(acc_id) {
        rv$posteriors[["Selector"]][[acc_id]]
      }),
      stringsAsFactors = FALSE
    )

    df$Change <- df$Posterior - df$Prior

    df
  }, digits = 3)

  # ---------------------------------------------------------------------------
  # VISUALIZATION TAB
  # ---------------------------------------------------------------------------
  # Interactive network visualization with multiple color modes.

  # Update visualization view choices
  observe({
    req(rv$model)

    choices <- c("Full Bayesian Network" = "full")

    # Add individual account views
    for (acc_id in names(rv$model$accounts)) {
      account <- rv$model$accounts[[acc_id]]
      choices[[sprintf("Explanatory Account: %s", acc_id)]] <- acc_id
    }

    updateSelectInput(session, "viz_view", choices = choices)
  })

  # Update comparison dropdown choices when saved inferences change
  observe({
    choices <- c(
      "Unconditional (no evidence)" = "unconditional",
      "Baseline Marginals (Results=TRUE)" = "baseline"
    )
    choices <- add_saved_inference_choices(choices, rv$saved_inferences)
    if (!is.null(rv$posteriors)) {
      choices <- c(choices, "Current Posterior" = "current")
    }

    # Preserve selections if still valid
    current_from <- isolate(input$compare_from)
    current_to <- isolate(input$compare_to)
    new_from <- if (!is.null(current_from) && current_from %in% choices) current_from else "baseline"
    new_to <- if (!is.null(current_to) && current_to %in% choices) {
      current_to
    } else if (!is.null(rv$posteriors)) {
      "current"
    } else {
      "baseline"
    }

    updateSelectInput(session, "compare_from", choices = choices, selected = new_from)
    updateSelectInput(session, "compare_to", choices = choices, selected = new_to)
  })

  # Update posterior dropdown choices when saved inferences change
  observe({
    choices <- if (!is.null(rv$posteriors)) {
      c("Current Posterior" = "current", "Baseline Marginals (Results=TRUE)" = "baseline")
    } else {
      c("Baseline Marginals (Results=TRUE)" = "baseline")
    }
    choices <- add_saved_inference_choices(choices, rv$saved_inferences)

    # Preserve selection if still valid
    current_sel <- isolate(input$posterior_select)
    new_sel <- if (!is.null(current_sel) && current_sel %in% choices) {
      current_sel
    } else if (!is.null(rv$posteriors)) {
      "current"
    } else {
      "baseline"
    }

    updateSelectInput(session, "posterior_select", choices = choices, selected = new_sel)
  })

  # Network visualization
  output$network_viz <- renderVisNetwork({
    req(rv$bayesian_network)

    # Build nodes and edges based on view
    view <- input$viz_view %||% "full"
    hide_aux <- input$viz_hide_aux %||% FALSE

    # Get all nodes
    node_ids <- names(rv$bayesian_network$nodes)

    # Filter by view
    if (view != "full") {
      # Individual account view
      account <- rv$model$accounts[[view]]
      req(account)

      # Include: account, its phenomena, auxiliaries, results, links
      node_ids <- c(
        view,  # Account node
        account$phenomenon_ids,
        account$auxiliary_ids,
        names(rv$model$results),  # All results
        account$explanatory_link_ids
      )
    }

    # Filter out auxiliaries if requested
    if (hide_aux) {
      node_ids <- setdiff(node_ids, names(rv$model$auxiliaries))
    }

    # Build visNetwork nodes
    vis_nodes <- data.frame(
      id = node_ids,
      stringsAsFactors = FALSE
    )

    # Add node labels
    vis_nodes$label <- sapply(rv$bayesian_network$nodes[node_ids], function(n) {
      if (is.null(n)) return("")
      # Hide label for ExplanatoryLink to minimize clutter
      if (n$type == "ExplanatoryLink") {
        ""
      } else if (n$type == "Account") {
        # Display hypothesis ID instead of account ID
        account <- rv$model$accounts[[n$id]]
        if (!is.null(account)) account$hypothesis_id else n$id
      } else {
        n$id
      }
    })

    # Add shapes based on node type
    vis_nodes$shape <- sapply(rv$bayesian_network$nodes[node_ids], function(n) {
      if (is.null(n)) return("dot")
      if (n$type == "Hypothesis") {
        "ellipse"
      } else if (n$type == "AuxiliaryClaim") {
        "triangle"
      } else if (n$type == "ExplanatoryLink") {
        "diamond"
      } else if (n$type == "PhenomenonClaim") {
        "box"
      } else if (n$type == "EmpiricalResult") {
        "box"
      } else if (n$type == "Selector") {
        "ellipse"
      } else if (n$type == "Account") {
        "ellipse"
      } else {
        "dot"
      }
    })

    # Add sizes based on node type
    vis_nodes$size <- sapply(rv$bayesian_network$nodes[node_ids], function(n) {
      if (is.null(n)) return(25)
      if (n$type == "Hypothesis" || n$type == "Selector" || n$type == "Account") {
        40
      } else if (n$type == "AuxiliaryClaim") {
        10
      } else if (n$type == "ExplanatoryLink") {
        5
      } else if (n$type == "PhenomenonClaim" || n$type == "EmpiricalResult") {
        30
      } else {
        25
      }
    })

    # Add colors based on mode using compute_node_colors() from probability_viz_helpers.R
    color_mode <- input$viz_color_mode %||% "structure"

    # Resolve comparison marginals from dropdown selections
    compare_from <- get_marginals_for_selection(input$compare_from, rv)
    compare_to <- get_marginals_for_selection(input$compare_to, rv)

    # Resolve posterior marginals from dropdown selection (for posterior mode)
    selected_posteriors <- get_marginals_for_selection(input$posterior_select, rv)

    vis_nodes$color <- compute_node_colors(
      node_ids = node_ids,
      nodes = rv$bayesian_network$nodes,
      color_mode = color_mode,
      baseline = rv$baseline,
      posteriors = selected_posteriors,
      compare_from = compare_from,
      compare_to = compare_to,
      evpi_results = rv$evpi_results
    )

    # Set font properties
    vis_nodes$font.size <- 14
    vis_nodes$font.color <- "#000000"
    vis_nodes$borderWidth <- 1

    from_label <- get_selection_label(input$compare_from)
    to_label <- get_selection_label(input$compare_to)

    # Add tooltips using compute_node_tooltips() from probability_viz_helpers.R
    vis_nodes$title <- compute_node_tooltips(
      node_ids = node_ids,
      nodes = rv$bayesian_network$nodes,
      baseline = rv$baseline,
      posteriors = selected_posteriors,
      color_mode = color_mode,
      compare_from = compare_from,
      compare_to = compare_to,
      from_label = from_label,
      to_label = to_label,
      evpi_results = rv$evpi_results,
      model = rv$model
    )

    # Build edges - filter to only include those between visible nodes
    edges_df <- rv$bayesian_network$edges
    filtered <- edges_df[edges_df$from %in% node_ids & edges_df$to %in% node_ids, ]
    vis_edges <- data.frame(
      from = filtered$from,
      to = filtered$to,
      arrows = "to",
      stringsAsFactors = FALSE
    )

    visNetwork(vis_nodes, vis_edges) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLayout(randomSeed = 42)
  })

  # Legend
  output$viz_legend <- renderUI({
    color_mode <- input$viz_color_mode %||% "structure"

    from_label <- get_selection_label(input$compare_from)
    to_label <- get_selection_label(input$compare_to)

    if (color_mode == "structure") {
      div(
        tags$h5(style = "margin-bottom: 15px; font-weight: 600;", "Node colors by type:"),
        div(
          class = "legend-container",
          tags$div(class = "legend-item",
            tags$div(class = "legend-color", style = "background: #3589DD;"),
            tags$span("Hypotheses & Accounts")
          ),
          tags$div(class = "legend-item",
            tags$div(class = "legend-color", style = "background: #E8AB79;"),
            tags$span("Auxiliaries")
          ),
          tags$div(class = "legend-item",
            tags$div(class = "legend-color", style = "background: #A2E8DD;"),
            tags$span("Phenomena")
          ),
          tags$div(class = "legend-item",
            tags$div(class = "legend-color", style = "background: #8D89BA;"),
            tags$span("Results")
          ),
          tags$div(class = "legend-item",
            tags$div(class = "legend-color", style = "background: #5F5566;"),
            tags$span("Links")
          )
        )
      )
    } else if (color_mode == "baseline") {
      div(
        tags$h5(style = "margin-bottom: 5px; font-weight: 600;", "Baseline probability scale"),
        tags$p(class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 15px;",
          "Marginals conditioned on all Results = TRUE"),
        div(
          class = "legend-gradient",
          div(class = "gradient-bar",
              style = "background: linear-gradient(to right, #EF4444, #FBBF24, #22C55E);"),
          div(class = "legend-labels",
            tags$span("0%"),
            tags$span("50%"),
            tags$span("100%")
          )
        )
      )
    } else if (color_mode == "posterior") {
      posterior_label <- get_selection_label(input$posterior_select)
      div(
        tags$h5(style = "margin-bottom: 5px; font-weight: 600;",
                sprintf("Posterior probability: %s", posterior_label)),
        tags$p(class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 15px;",
          "Marginals after conditioning on evidence"),
        div(
          class = "legend-gradient",
          div(class = "gradient-bar",
              style = "background: linear-gradient(to right, #EF4444, #FBBF24, #22C55E);"),
          div(class = "legend-labels",
            tags$span("0%"),
            tags$span("50%"),
            tags$span("100%")
          )
        )
      )
    } else if (color_mode == "delta") {
      div(
        tags$h5(style = "margin-bottom: 5px; font-weight: 600;",
                sprintf("Probability change: %s \u2192 %s", from_label, to_label)),
        div(
          class = "legend-gradient",
          style = "margin-bottom: 10px;",
          div(class = "gradient-bar",
              style = "background: linear-gradient(to right, #3B82F6, #FFFFFF, #F97316);"),
          div(class = "legend-labels",
            tags$span("-100%"),
            tags$span("0%"),
            tags$span("+100%")
          )
        ),
        tags$p(class = "text-muted", style = "font-size: 0.85rem;",
          tags$span(style = "color: #3B82F6;", "\u25CF"), " Decrease  ",
          tags$span(style = "color: #9CA3AF;", "\u25CF"), " No change  ",
          tags$span(style = "color: #F97316;", "\u25CF"), " Increase"
        )
      )
    } else if (color_mode == "ratio") {
      div(
        tags$h5(style = "margin-bottom: 5px; font-weight: 600;",
                sprintf("Log-ratio: log(%s / %s)", to_label, from_label)),
        div(
          class = "legend-gradient",
          style = "margin-bottom: 10px;",
          div(class = "gradient-bar",
              style = "background: linear-gradient(to right, #EF4444, #FFFFFF, #22C55E);"),
          div(class = "legend-labels",
            tags$span("0.1x"),
            tags$span("1x"),
            tags$span("10x")
          )
        ),
        tags$p(class = "text-muted", style = "font-size: 0.85rem;",
          tags$span(style = "color: #EF4444;", "\u25CF"), " 10% (log=-2.3)  ",
          tags$span(style = "color: #9CA3AF;", "\u25CF"), " 100% (log=0)  ",
          tags$span(style = "color: #22C55E;", "\u25CF"), " 1000% (log=+2.3)"
        )
      )
    } else if (color_mode == "evpi") {
      if (is.null(rv$evpi_results)) {
        div(
          tags$h5(style = "margin-bottom: 5px; font-weight: 600;", "Research Priority (EVPI)"),
          tags$p(class = "text-muted",
            "No VOI analysis computed. Go to Research Priorities tab and click 'Compute Value of Information'."),
          tags$p(class = "text-muted", style = "font-size: 0.85rem;",
            "Gray nodes indicate no EVPI data available.")
        )
      } else {
        div(
          tags$h5(style = "margin-bottom: 5px; font-weight: 600;",
                  "Research Priority (EVPI % of current entropy)"),
          tags$p(class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 15px;",
            sprintf("Based on VOI analysis with %d candidate nodes",
                    rv$evpi_results$metadata$n_candidates)),
          div(
            class = "legend-gradient",
            style = "margin-bottom: 10px;",
            div(class = "gradient-bar",
                style = "background: linear-gradient(to right, #FFFFFF, #6A46CE); border: 1px solid #E2E8F0;"),
            div(class = "legend-labels",
              tags$span("0%"),
              tags$span("50%"),
              tags$span("100%")
            )
          ),
          tags$p(class = "text-muted", style = "font-size: 0.85rem;",
            tags$span(style = "color: #9CA3AF;", "\u25CF"), " Low priority  ",
            tags$span(style = "color: #6A46CE;", "\u25CF"), " High priority  ",
            tags$span(style = "color: #CBD5E1;", "\u25CF"), " Non-candidate nodes"
          )
        )
      }
    } else {
      p("Unknown color mode")
    }
  })

  # ---------------------------------------------------------------------------
  # INVALIDATION WARNING SYSTEM
  # ---------------------------------------------------------------------------
  # Warns users before clearing saved inferences due to model changes.

  # Helper to show invalidation warning
  # Returns TRUE if warning was shown (action pending), FALSE if can proceed
  show_invalidation_warning <- function(action_desc, pending_action) {
    if (length(rv$saved_inferences) == 0 || rv$suppress_invalidation_warning) {
      # No saved inferences or warnings suppressed - clear and proceed
      if (length(rv$saved_inferences) > 0) {
        rv$saved_inferences <- list()
        showNotification("Saved inferences cleared due to model change.", type = "warning")
      }
      return(FALSE)
    }

    # Show warning modal
    showModal(modalDialog(
      title = "Warning: Saved Inferences Will Be Cleared",
      p(sprintf("You are about to %s.", action_desc)),
      p(sprintf("This will invalidate and delete your %d saved inference(s).",
                length(rv$saved_inferences))),
      tags$ul(
        lapply(names(rv$saved_inferences), function(n) tags$li(n))
      ),
      checkboxInput("dont_warn_invalidation", "Don't warn me again this session", FALSE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_invalidation", "Continue", class = "btn-warning")
      )
    ))

    rv$pending_invalidation_action <- pending_action
    return(TRUE)
  }

  # Confirmation handler for invalidation
  observeEvent(input$confirm_invalidation, {
    req(rv$pending_invalidation_action)

    # Update suppression preference and clear saved inferences
    rv$suppress_invalidation_warning <- input$dont_warn_invalidation
    rv$saved_inferences <- list()

    # Get pending action and clear it
    pending <- rv$pending_invalidation_action
    rv$pending_invalidation_action <- NULL

    removeModal()
    showNotification("Saved inferences cleared.", type = "warning")

    # Execute the pending action using a dispatch table
    case_studies <- list(
      load_simple_case = list(
        fn = create_simple_test_case_study,
        msg = "Simple test case study loaded and network built!"
      ),
      load_covid_case = list(
        fn = create_covid_case_study,
        msg = "COVID-19 case study loaded and network built!"
      )
    )

    if (pending %in% names(case_studies)) {
      cs <- case_studies[[pending]]
      rv$model <- cs$fn()
      if (build_network_for_analysis(switch_to_analysis = FALSE)) {
        showNotification(cs$msg, type = "message")
        updateTabItems(session, "sidebar_menu", "viz")
      }
    } else if (pending == "build_network") {
      build_network_for_analysis(switch_to_analysis = TRUE)
    }
  })

  # ---------------------------------------------------------------------------
  # CASE STUDY LOADERS
  # ---------------------------------------------------------------------------
  # Pre-built examples for quick demonstration.

  # Helper to load a case study with invalidation check
  load_case_study <- function(create_fn, warning_desc, pending_action, success_msg) {
    if (show_invalidation_warning(warning_desc, pending_action)) {
      return()
    }
    rv$model <- create_fn()
    if (build_network_for_analysis(switch_to_analysis = FALSE)) {
      showNotification(success_msg, type = "message")
      updateTabItems(session, "sidebar_menu", "viz")
    }
  }

  observeEvent(input$load_simple_case, {
    load_case_study(
      create_simple_test_case_study,
      "load the Simple Test case study",
      "load_simple_case",
      "Simple test case study loaded and network built!"
    )
  })

  observeEvent(input$load_covid_case, {
    load_case_study(
      create_covid_case_study,
      "load the COVID-19 case study",
      "load_covid_case",
      "COVID-19 case study loaded and network built!"
    )
  })

  # ---------------------------------------------------------------------------
  # VALUE OF INFORMATION ANALYSIS
  # ---------------------------------------------------------------------------

  # Helper: Generate cache key for EVPI results
  # Key is based on: model structure, starting evidence, target, node type filters
  generate_evpi_cache_key <- function() {
    # Create a fingerprint of the model structure
    model_fingerprint <- paste(
      length(rv$model$hypotheses),
      length(rv$model$auxiliaries),
      length(rv$model$phenomena),
      length(rv$model$results),
      length(rv$model$explanatory_links),
      length(rv$model$accounts),
      sep = "|"
    )

    # Get current settings
    starting_evidence <- input$voi_starting_evidence
    target <- input$voi_target
    node_types <- paste(sort(input$voi_node_types), collapse = ",")

    # Combine into cache key
    paste(model_fingerprint, starting_evidence, target, node_types, sep = ":")
  }

  # Compute VOI button handler
  observeEvent(input$voi_compute, {
    req(rv$bayesian_network, rv$selector, rv$model)

    # Check if model has accounts
    if (length(rv$model$accounts) == 0) {
      showNotification("No accounts in model. Build a complete model first.", type = "error")
      return()
    }

    # Check cache first
    cache_key <- generate_evpi_cache_key()
    if (!is.null(rv$evpi_cache[[cache_key]])) {
      # Cache hit - use cached results
      rv$evpi_results <- rv$evpi_cache[[cache_key]]
      rv$evpi_cache_key <- cache_key
      rv$evpi_selected_node <- NULL

      n_candidates <- nrow(rv$evpi_results$results)
      showNotification(
        sprintf("VOI results loaded from cache (%d nodes)", n_candidates),
        type = "message"
      )
      return()
    }

    # Cache miss - compute
    rv$evpi_computing <- TRUE
    rv$evpi_results <- NULL
    rv$evpi_selected_node <- NULL

    tryCatch({
      # Generate CPTs
      cpts <- generate_all_cpts_for_model(rv$model, rv$bayesian_network, rv$selector)

      # Build base evidence based on starting evidence selection
      base_evidence <- if (input$voi_starting_evidence == "baseline") {
        build_results_evidence(rv$model)
      } else {
        list()
      }

      # Get node type filters
      include_aux <- "auxiliaries" %in% input$voi_node_types
      include_phen <- "phenomena" %in% input$voi_node_types
      include_links <- "links" %in% input$voi_node_types

      # Show progress notification
      progress_id <- showNotification("Computing Value of Information...",
                                       duration = NULL, type = "message")

      # Compute EVPI for all candidates
      evpi_results <- compute_all_evpi(
        model = rv$model,
        bayesian_network = rv$bayesian_network,
        cpts = cpts,
        base_evidence = base_evidence,
        target = input$voi_target,
        include_auxiliaries = include_aux,
        include_phenomena = include_phen,
        include_links = include_links
      )

      # Store in cache
      rv$evpi_cache[[cache_key]] <- evpi_results
      rv$evpi_cache_key <- cache_key

      rv$evpi_results <- evpi_results
      rv$evpi_computing <- FALSE

      # Remove progress notification
      removeNotification(progress_id)

      # Show completion message
      n_candidates <- nrow(evpi_results$results)
      time_taken <- round(evpi_results$metadata$computation_time_seconds, 2)
      showNotification(
        sprintf("VOI analysis complete: %d nodes evaluated in %.2fs", n_candidates, time_taken),
        type = "message"
      )

    }, error = function(e) {
      rv$evpi_computing <- FALSE
      showNotification(paste("VOI computation error:", e$message), type = "error")
    })
  })

  # Manage VOI button states

  observe({
    # Disable compute button during computation or when no network
    can_compute <- !is.null(rv$bayesian_network) &&
                   !is.null(rv$model) &&
                   length(rv$model$accounts) > 0 &&
                   !isTRUE(rv$evpi_computing)
    shinyjs::toggleState("voi_compute", can_compute)

    # Disable export when no results
    has_results <- !is.null(rv$evpi_results)
    shinyjs::toggleState("voi_export_csv", has_results)
  })

  # Current Uncertainty display
  output$voi_current_uncertainty <- renderUI({
    # Check if network is ready
    if (is.null(rv$bayesian_network)) {
      return(div(class = "alert alert-warning",
        icon("exclamation-triangle"), " ",
        "Build a complete model first. Go to the Model tab and add hypotheses, phenomena, results, and accounts."
      ))
    }

    if (is.null(rv$evpi_results)) {
      if (isTRUE(rv$evpi_computing)) {
        return(p(class = "text-info",
          icon("spinner", class = "fa-spin"), " Computing Value of Information..."
        ))
      }
      return(p(class = "text-muted", "Click 'Compute Value of Information' to analyze research priorities."))
    }

    metadata <- rv$evpi_results$metadata
    posteriors <- metadata$posteriors_current

    # Format posteriors as string
    posterior_str <- paste(
      sapply(names(posteriors), function(h) sprintf("%s: %.1f%%", h, posteriors[h] * 100)),
      collapse = "  |  "
    )

    tagList(
      fluidRow(
        column(4,
          strong("Target: "), metadata$target
        ),
        column(4,
          strong("Entropy: "), sprintf("%.4f bits", metadata$entropy_current)
        ),
        column(4,
          strong("Candidates: "), metadata$n_candidates, " nodes"
        )
      ),
      hr(),
      fluidRow(
        column(12,
          strong("Current Distribution: "), posterior_str
        )
      )
    )
  })

  # Results table
  output$voi_results_table <- renderDT({
    req(rv$evpi_results)

    df <- rv$evpi_results$results

    if (nrow(df) == 0) {
      return(NULL)
    }

    # Compute gain if false (entropy reduction if node=false)
    entropy_current <- rv$evpi_results$metadata$entropy_current
    gain_if_false <- entropy_current - df$entropy_if_false

    # Compute posterior shift if false (TVD between current and if-false)
    posteriors_current <- rv$evpi_results$metadata$posteriors_current
    shift_if_false <- sapply(df$node_id, function(node_id) {
      posteriors_if_false <- rv$evpi_results$details[[node_id]]$posteriors_if_false
      0.5 * sum(abs(posteriors_if_false - posteriors_current))
    })

    # Create display dataframe with formatted columns
    display_df <- data.frame(
      Node = df$node_id,
      Type = df$node_type,
      EVPI = sprintf("%.4f", df$evpi),
      `% Red` = sprintf("%.1f%%", df$evpi_pct),
      `Gain if False` = sprintf("%.3f", gain_if_false),
      `Shift if False` = sprintf("%.3f", shift_if_false),
      Shift = sprintf("%.3f", df$shift),
      `Max Gain` = sprintf("%.3f", df$max_gain),
      Asymmetry = sprintf("%.3f", df$asymmetry),
      `P(TRUE)` = sprintf("%.2f", df$p_true),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # Add priority badge based on evpi_pct
    display_df$Priority <- sapply(df$evpi_pct, function(pct) {
      if (pct >= 15) "Critical"
      else if (pct >= 8) "High"
      else if (pct >= 3) "Medium"
      else "Low"
    })

    # Reorder columns
    display_df <- display_df[, c("Priority", "Node", "Type", "EVPI", "% Red",
                                  "Gain if False", "Shift if False", "Shift", "Max Gain", "Asymmetry", "P(TRUE)")]

    datatable(
      display_df,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 15,
        order = list(list(3, "desc")),  # Sort by EVPI descending
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        )
      )
    ) %>%
      formatStyle(
        "Priority",
        backgroundColor = styleEqual(
          c("Critical", "High", "Medium", "Low"),
          c("#f8d7da", "#fff3cd", "#d1ecf1", "#e2e3e5")
        ),
        fontWeight = "bold"
      )
  })

  # Handle row selection for detail panel
  observeEvent(input$voi_results_table_rows_selected, {
    req(rv$evpi_results)
    selected_row <- input$voi_results_table_rows_selected

    if (length(selected_row) > 0) {
      rv$evpi_selected_node <- rv$evpi_results$results$node_id[selected_row]
    } else {
      rv$evpi_selected_node <- NULL
    }
  })

  # Detail panel
  output$voi_detail_panel <- renderUI({
    if (is.null(rv$evpi_selected_node) || is.null(rv$evpi_results)) {
      return(p(class = "text-muted", "Select a row in the table above to see detailed analysis."))
    }

    node_id <- rv$evpi_selected_node
    results_df <- rv$evpi_results$results

    # Check if details exist for this node
    if (!node_id %in% names(rv$evpi_results$details)) {
      return(p(class = "text-muted", "Details not available for this node."))
    }

    details <- rv$evpi_results$details[[node_id]]

    # Get row data
    row_idx <- which(results_df$node_id == node_id)
    if (length(row_idx) == 0) return(NULL)

    row <- results_df[row_idx, ]

    # Get posteriors
    posteriors_true <- details$posteriors_if_true
    posteriors_false <- details$posteriors_if_false
    posteriors_current <- rv$evpi_results$metadata$posteriors_current

    # Build comparison table
    comparison_rows <- lapply(names(posteriors_current), function(h) {
      curr <- posteriors_current[h]
      if_true <- posteriors_true[h]
      if_false <- posteriors_false[h]

      delta_true <- if_true - curr
      delta_false <- if_false - curr

      tags$tr(
        tags$td(h),
        tags$td(sprintf("%.3f (%s%.3f)", if_true,
                        ifelse(delta_true >= 0, "+", ""), delta_true)),
        tags$td(sprintf("%.3f (%s%.3f)", if_false,
                        ifelse(delta_false >= 0, "+", ""), delta_false))
      )
    })

    tagList(
      h4(node_id, " - ", row$node_type),
      p(em(row$node_description)),

      hr(),

      fluidRow(
        column(6,
          strong("Current probability: "),
          sprintf("P(TRUE) = %.2f, P(FALSE) = %.2f", row$p_true, 1 - row$p_true)
        ),
        column(6,
          strong("EVPI: "),
          sprintf("%.4f bits (%.1f%% of current uncertainty)", row$evpi, row$evpi_pct)
        )
      ),

      hr(),

      h5("Conditional Posteriors"),
      tags$table(
        class = "table table-bordered table-striped",
        tags$thead(
          tags$tr(
            tags$th("Hypothesis"),
            tags$th(sprintf("If TRUE (%.0f%% likely)", row$p_true * 100)),
            tags$th(sprintf("If FALSE (%.0f%% likely)", (1 - row$p_true) * 100))
          )
        ),
        tags$tbody(comparison_rows)
      ),

      fluidRow(
        column(6,
          strong("Entropy if TRUE: "), sprintf("%.4f bits", row$entropy_if_true)
        ),
        column(6,
          strong("Entropy if FALSE: "), sprintf("%.4f bits", row$entropy_if_false)
        )
      ),

      hr(),

      h5("Interpretation"),
      p(details$interpretation)
    )
  })

  # CSV Export
  output$voi_export_csv <- downloadHandler(
    filename = function() {
      paste0("voi_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      tryCatch({
        req(rv$evpi_results)

        df <- rv$evpi_results$results

        # Add metadata as comment rows or separate section
        metadata <- rv$evpi_results$metadata

        # Add priority column
        df$priority <- sapply(df$evpi_pct, function(pct) {
          if (pct >= 15) "Critical"
          else if (pct >= 8) "High"
          else if (pct >= 3) "Medium"
          else "Low"
        })

        # Reorder and rename for export
        export_df <- df[, c("node_id", "node_type", "node_description", "priority",
                            "evpi", "evpi_pct", "shift", "max_gain", "asymmetry",
                            "p_true", "entropy_if_true", "entropy_if_false")]

        # Write with metadata header
        writeLines(c(
          sprintf("# VOI Analysis Export"),
          sprintf("# Timestamp: %s", metadata$timestamp),
          sprintf("# Target: %s", metadata$target),
          sprintf("# Current Entropy: %.4f bits", metadata$entropy_current),
          sprintf("# Candidates Evaluated: %d", metadata$n_candidates),
          sprintf("# Computation Time: %.2f seconds", metadata$computation_time_seconds),
          ""
        ), file)

        # Append data
        write.table(export_df, file, append = TRUE, sep = ",",
                    row.names = FALSE, quote = TRUE)
      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
      })
    }
  )

  # ===========================================================================
  # MODEL EXPORT/IMPORT
  # ===========================================================================

  output$export_model <- downloadHandler(
    filename = function() {
      model_name <- gsub("[^a-zA-Z0-9]", "_", rv$model$name)
      paste0(model_name, "-", format(Sys.Date(), "%Y%m%d"), ".json")
    },
    content = function(file) {
      writeLines(rv$model$to_json(), file)
    },
    contentType = "application/json"
  )

  observeEvent(input$import_model, {
    req(input$import_model)

    tryCatch({
      json_str <- paste(readLines(input$import_model$datapath, warn = FALSE), collapse = "\n")
      imported_model <- ExplanatoryModel$from_json(json_str)
      rv$model <- imported_model

      if (build_network_for_analysis(switch_to_analysis = FALSE)) {
        updateTabItems(session, "sidebar_menu", "viz")
        showNotification(paste("Model loaded:", imported_model$name), type = "message")
      } else {
        showNotification("Model loaded but network build failed.", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Failed to import model:", e$message), type = "error")
    })
  })

  # ---------------------------------------------------------------------------
  # SENSITIVITY ANALYSIS
  # ---------------------------------------------------------------------------

  # Update sweepable parameters when model changes
  observe({
    req(rv$model)
    rv$sensitivity_params <- get_sweepable_parameters(rv$model)
  })

  # Parameter tree UI
  output$sensitivity_param_tree <- renderUI({
    params <- rv$sensitivity_params
    if (is.null(params) || nrow(params) == 0) {
      return(p(class = "text-muted", "No parameters available. Build a model first."))
    }

    # Group by node type
    param_types <- unique(params$node_type)
    type_labels <- c(
      hypothesis = "Hypothesis Priors",
      auxiliary = "Auxiliary Priors",
      phenomenon = "Phenomenon Parameters",
      result = "Result Parameters",
      link = "Link Parameters"
    )

    # Build grouped choices for selectInput
    choices_grouped <- lapply(param_types, function(ptype) {
      type_params <- params[params$node_type == ptype, ]
      as.list(setNames(
        paste(type_params$node_id, type_params$param_name, sep = "::"),
        type_params$display_name
      ))
    })
    names(choices_grouped) <- type_labels[param_types]

    # Get first choice for default selection
    first_choice <- if (nrow(params) > 0) {
      paste(params$node_id[1], params$param_name[1], sep = "::")
    } else {
      NULL
    }

    selectInput("sensitivity_param_select", "Select Parameter:",
                choices = choices_grouped,
                selected = first_choice)
  })

  # Update selected parameter when selection changes
  # This reactive computes the selected parameter info from the input
  sensitivity_selection <- reactive({
    selection <- input$sensitivity_param_select
    params <- rv$sensitivity_params

    if (is.null(selection) || selection == "" || is.null(params) || nrow(params) == 0) {
      return(NULL)
    }

    # Parse selection (format: "node_id::param_name")
    parts <- strsplit(selection, "::")[[1]]
    if (length(parts) != 2) return(NULL)

    node_id <- parts[1]
    param_name <- parts[2]

    # Find matching parameter
    match_idx <- which(params$node_id == node_id & params$param_name == param_name)
    if (length(match_idx) == 0) return(NULL)

    # Extract values as scalars (not data frame columns)
    idx <- match_idx[1]
    list(
      node_id = as.character(params$node_id[idx]),
      node_type = as.character(params$node_type[idx]),
      param_name = as.character(params$param_name[idx]),
      current_value = as.numeric(params$current_value[idx]),
      display_name = as.character(params$display_name[idx]),
      description = as.character(params$description[idx])
    )
  })

  # Update rv$sensitivity_selected whenever the reactive changes
  observe({
    sel <- sensitivity_selection()
    rv$sensitivity_selected <- sel

    # Update range inputs based on current value
    if (!is.null(sel)) {
      new_min <- max(0.01, sel$current_value - 0.3)
      new_max <- min(0.99, sel$current_value + 0.3)
      updateNumericInput(session, "sensitivity_range_min", value = round(new_min, 2))
      updateNumericInput(session, "sensitivity_range_max", value = round(new_max, 2))
    }
  })

  # Full range button
  observeEvent(input$sensitivity_full_range, {
    updateNumericInput(session, "sensitivity_range_min", value = 0.01)
    updateNumericInput(session, "sensitivity_range_max", value = 0.99)
  })

  # Selected parameter info display
  output$sensitivity_param_info <- renderUI({
    sel <- rv$sensitivity_selected
    if (is.null(sel)) {
      return(p(class = "text-muted", "Select a parameter above."))
    }

    tagList(
      p(tags$strong("Selected: "), sel$display_name),
      p(tags$strong("Current value: "), sprintf("%.3f", sel$current_value)),
      p(class = "text-muted", sel$description)
    )
  })

  # Selected parameter display in configuration panel
  output$sensitivity_selected_display <- renderUI({
    sel <- rv$sensitivity_selected
    if (is.null(sel)) {
      return(p(class = "text-muted", "No parameter selected."))
    }

    p(tags$strong("Sweeping: "), sel$display_name,
      tags$br(),
      tags$small(class = "text-muted", sprintf("Current: %.3f", sel$current_value)))
  })

  # Current evidence display
  output$sensitivity_evidence_display <- renderUI({
    if (is.null(rv$model) || length(rv$model$results) == 0) {
      return(p("No evidence corpus defined."))
    }

    result_ids <- names(rv$model$results)

    tagList(
      p("Results are conditioned as TRUE by default:"),
      tags$ul(
        lapply(result_ids, function(r_id) {
          tags$li(paste0(r_id, " = TRUE"))
        })
      )
    )
  })

  # Status display
  output$sensitivity_status <- renderUI({
    if (rv$sensitivity_computing) {
      return(tagList(
        div(class = "text-center",
          icon("spinner", class = "fa-spin fa-2x"),
          p("Computing sweep...")
        )
      ))
    }

    if (is.null(rv$sensitivity_result)) {
      return(tagList(
        p(class = "text-muted", "No sweep computed yet."),
        p("Select a parameter and click 'Run Sweep' to begin.")
      ))
    }

    result <- rv$sensitivity_result
    meta <- result$metadata

    tagList(
      p(icon("check-circle", class = "text-success"), " Sweep completed"),
      tags$ul(
        tags$li(sprintf("Parameter: %s", meta$parameter$display_name)),
        tags$li(sprintf("Range: %.2f to %.2f", meta$range_min, meta$range_max)),
        tags$li(sprintf("Points: %d", meta$n_points)),
        tags$li(sprintf("Time: %.2f seconds", meta$computation_time_seconds))
      )
    )
  })

  # Run sweep button handler
  observeEvent(input$sensitivity_run, {
    req(rv$sensitivity_selected, rv$model, rv$bayesian_network, rv$selector)

    # Validate range
    range_min <- input$sensitivity_range_min
    range_max <- input$sensitivity_range_max

    if (is.na(range_min) || is.na(range_max)) {
      showNotification("Invalid range values", type = "error")
      return()
    }

    if (range_min >= range_max) {
      showNotification("Min must be less than max", type = "error")
      return()
    }

    if (range_min <= 0 || range_max >= 1) {
      showNotification("Range must be within (0, 1)", type = "error")
      return()
    }

    rv$sensitivity_computing <- TRUE
    rv$sensitivity_result <- NULL

    # Create CPT generator function
    cpt_generator <- function(m) {
      g <- build_bayesian_network(m)
      s <- Selector$new(m)
      generate_all_cpts_for_model(m, g, s)
    }

    # Create inference function
    inference_func <- function(evidence_graph, cpts, evidence) {
      compute_posteriors(evidence_graph, cpts, evidence)
    }

    # Build evidence list (all results = TRUE)
    evidence <- build_results_evidence(rv$model)

    tryCatch({
      result <- compute_parameter_sweep(
        parameter_spec = rv$sensitivity_selected,
        range_min = range_min,
        range_max = range_max,
        n_points = 21,
        target = input$sensitivity_target,
        model = rv$model,
        evidence_graph = rv$bayesian_network,
        cpt_generator = cpt_generator,
        inference_func = inference_func,
        evidence = evidence
      )

      rv$sensitivity_result <- result
      showNotification("Sweep completed successfully", type = "message")

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })

    rv$sensitivity_computing <- FALSE
  })

  # Sweep plot
  output$sensitivity_plot <- renderPlot({
    result <- rv$sensitivity_result
    if (is.null(result)) {
      return(NULL)
    }

    create_sweep_plot(result, show_current = TRUE)
  })

  # Summary table
  output$sensitivity_summary_table <- renderDT({
    result <- rv$sensitivity_result
    if (is.null(result)) {
      return(NULL)
    }

    summary_df <- result$summary
    summary_df$current <- round(summary_df$current, 4)
    summary_df$min <- round(summary_df$min, 4)
    summary_df$max <- round(summary_df$max, 4)
    summary_df$range <- round(summary_df$range, 4)
    summary_df$max_change <- round(summary_df$max_change, 4)
    summary_df$max_change_at <- round(summary_df$max_change_at, 3)

    names(summary_df) <- c("Target", "Current", "Min", "Max", "Range", "Max Change", "Max Change At")

    datatable(summary_df,
              options = list(
                dom = 't',
                pageLength = 50,
                ordering = FALSE
              ),
              rownames = FALSE)
  })

  # Enable/disable UI elements based on state
  observe({
    can_run <- !is.null(rv$sensitivity_selected) &&
               !is.null(rv$bayesian_network) &&
               !isTRUE(rv$sensitivity_computing)
    shinyjs::toggleState("sensitivity_run", can_run)

    has_results <- !is.null(rv$sensitivity_result)
    shinyjs::toggleState("sensitivity_export_csv", has_results)
    shinyjs::toggleState("sensitivity_export_png", has_results)
  })

  # CSV Export
  output$sensitivity_export_csv <- downloadHandler(
    filename = function() {
      paste0("sensitivity_", rv$sensitivity_selected$node_id, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      tryCatch({
        req(rv$sensitivity_result)

        result <- rv$sensitivity_result
        meta <- result$metadata

        # Write metadata header
        writeLines(c(
          "# Sensitivity Analysis Export",
          sprintf("# Timestamp: %s", meta$timestamp),
          sprintf("# Parameter: %s", meta$parameter$display_name),
          sprintf("# Range: %.3f to %.3f", meta$range_min, meta$range_max),
          sprintf("# Target: %s", meta$target),
          sprintf("# Computation Time: %.2f seconds", meta$computation_time_seconds),
          ""
        ), file)

        # Append posteriors data
        write.table(result$posteriors, file, append = TRUE, sep = ",",
                    row.names = FALSE, quote = TRUE)
      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
      })
    }
  )

  # PNG Export
  output$sensitivity_export_png <- downloadHandler(
    filename = function() {
      paste0("sensitivity_", rv$sensitivity_selected$node_id, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      tryCatch({
        req(rv$sensitivity_result)

        p <- create_sweep_plot(rv$sensitivity_result, show_current = TRUE)
        ggsave(file, plot = p, width = 10, height = 6, dpi = 150)
      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
      })
    }
  )
}

# Run the app (when sourced directly or via runApp)
# When sourced from root app.R, this returns the shiny app object
shinyApp(ui = ui, server = server)
