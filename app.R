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
  source("R/app_helpers.R")
  source("R/mod_voi.R")
  source("R/mod_sensitivity.R")
  source("R/mod_visualization.R")
  source("R/mod_analysis.R")
  source("R/mod_build.R")
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
      mod_build_ui("build_mod"),

      # ===== Analysis Tab =====
      mod_analysis_ui("analysis_mod"),

      # ===== Research Priorities Tab =====
      mod_voi_ui("voi_mod"),

      # ===== Sensitivity Analysis Tab =====
      mod_sensitivity_ui("sensitivity_mod"),

      # ===== Visualization Tab =====
      mod_visualization_ui("viz_mod"),

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
    sensitivity_computing = FALSE, # Flag for computation in progress
    # Additional conditions for analysis (moved from inline assignment)
    additional_conditions = list()
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
  # BUILD MODEL (module)
  # ---------------------------------------------------------------------------
  mod_build_server("build_mod", rv,
    build_network_fn = build_network_for_analysis,
    show_invalidation_warning_fn = show_invalidation_warning
  )

  # ---------------------------------------------------------------------------
  # NETWORK BUILDING
  # ---------------------------------------------------------------------------
  # Converts the ExplanatoryModel to a Bayesian network with CPTs.
  # Cross-cutting: called by case study loaders, import, and build module.

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

  # ---------------------------------------------------------------------------
  # ANALYSIS TAB (module)
  # ---------------------------------------------------------------------------
  mod_analysis_server("analysis_mod", rv)

  # ---------------------------------------------------------------------------
  # VISUALIZATION (module)
  # ---------------------------------------------------------------------------
  mod_visualization_server("viz_mod", rv)

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
  # VALUE OF INFORMATION ANALYSIS (module)
  # ---------------------------------------------------------------------------
  mod_voi_server("voi_mod", rv)

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
  # SENSITIVITY ANALYSIS (module)
  # ---------------------------------------------------------------------------
  mod_sensitivity_server("sensitivity_mod", rv)
}

# Run the app (when sourced directly or via runApp)
# When sourced from root app.R, this returns the shiny app object
shinyApp(ui = ui, server = server)
