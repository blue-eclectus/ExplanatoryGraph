# ==============================================================================
# Module: Value of Information (VOI) / Research Priorities
# ==============================================================================
#
# Shiny module for the Research Priorities tab.
# Computes Expected Value of Perfect Information (EVPI) to identify which
# uncertain nodes would be most valuable to investigate.
#
# Usage in app.R:
#   UI:     mod_voi_ui("voi_mod")
#   Server: mod_voi_server("voi_mod", rv)
#
# rv fields read:  rv$bayesian_network, rv$selector, rv$model,
#                  rv$baseline, rv$unconditional, rv$cpts
# rv fields written: rv$evpi_results (read by Visualization EVPI color mode),
#                    rv$evpi_selected_node, rv$evpi_computing,
#                    rv$evpi_cache, rv$evpi_cache_key

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

mod_voi_ui <- function(id) {
  ns <- NS(id)

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
                "Computed as: P(node=true) \u00d7 info_gain_if_true + P(node=false) \u00d7 info_gain_if_false. ",
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
                "Computed as: P(node=true) \u00d7 shift_if_true + P(node=false) \u00d7 shift_if_false. ",
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
            selectInput(ns("voi_starting_evidence"), "Starting Evidence:",
                       choices = c("Baseline Marginals (Results=TRUE)" = "baseline",
                                  "Unconditional" = "unconditional")),
            helpText("Baseline: after observing results. Unconditional: before any evidence.")
          ),
          column(4,
            radioButtons(ns("voi_target"), "Target:",
                        choices = c("Hypotheses" = "hypotheses",
                                   "Accounts" = "accounts"),
                        selected = "hypotheses",
                        inline = TRUE),
            helpText("Hypotheses: aggregate uncertainty. Accounts: full distribution.")
          ),
          column(4,
            checkboxGroupInput(ns("voi_node_types"), "Include Node Types:",
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
            actionButton(ns("voi_compute"), "Compute Value of Information",
                        class = "btn-success btn-lg", icon = icon("calculator"))
          ),
          column(6,
            downloadButton(ns("voi_export_csv"), "Export CSV", class = "btn-info")
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

        uiOutput(ns("voi_current_uncertainty"))
      )
    ),

    # Results Table
    fluidRow(
      box(
        title = "Research Priority Ranking",
        width = 12,
        status = "success",
        solidHeader = TRUE,

        DTOutput(ns("voi_results_table")),

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

        uiOutput(ns("voi_detail_panel"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

mod_voi_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
      shinyjs::toggleState(ns("voi_compute"), can_compute)

      # Disable export when no results
      has_results <- !is.null(rv$evpi_results)
      shinyjs::toggleState(ns("voi_export_csv"), has_results)
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
  })
}
