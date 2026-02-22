# ==============================================================================
# Module: Sensitivity Analysis
# ==============================================================================
#
# Shiny module for the Sensitivity Analysis tab.
# Performs parameter sweeps to show how hypothesis posteriors change
# as individual model parameters are varied.
#
# Usage in app.R:
#   UI:     mod_sensitivity_ui("sensitivity_mod")
#   Server: mod_sensitivity_server("sensitivity_mod", rv)
#
# rv fields read:  rv$model, rv$bayesian_network, rv$selector
# rv fields written: rv$sensitivity_params, rv$sensitivity_selected,
#                    rv$sensitivity_result, rv$sensitivity_computing

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

mod_sensitivity_ui <- function(id) {
  ns <- NS(id)

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
              tags$li("Configure the sweep range (default: current value +/- 0.3)"),
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

        uiOutput(ns("sensitivity_evidence_display"))
      )
    ),

    # Main content: parameter selection and configuration
    fluidRow(
      # Parameter Selection Panel
      box(
        title = "Parameter Selection",
        width = 4,
        status = "primary",

        uiOutput(ns("sensitivity_param_tree")),

        hr(),

        # Selected parameter info
        uiOutput(ns("sensitivity_param_info"))
      ),

      # Sweep Configuration Panel
      box(
        title = "Sweep Configuration",
        width = 4,
        status = "primary",

        uiOutput(ns("sensitivity_selected_display")),

        hr(),

        fluidRow(
          column(6,
            numericInput(ns("sensitivity_range_min"), "Min:", value = 0.01, min = 0.01, max = 0.98, step = 0.05)
          ),
          column(6,
            numericInput(ns("sensitivity_range_max"), "Max:", value = 0.99, min = 0.02, max = 0.99, step = 0.05)
          )
        ),

        actionButton(ns("sensitivity_full_range"), "Full Range (0.01-0.99)", class = "btn-default btn-sm"),

        hr(),

        radioButtons(ns("sensitivity_target"), "Target:",
                    choices = c("Hypotheses" = "hypotheses",
                               "Accounts" = "accounts"),
                    selected = "hypotheses",
                    inline = TRUE),

        hr(),

        fluidRow(
          column(6,
            actionButton(ns("sensitivity_run"), "Run Sweep",
                        class = "btn-success btn-lg", icon = icon("play"))
          ),
          column(6,
            downloadButton(ns("sensitivity_export_csv"), "Export CSV", class = "btn-info"),
            downloadButton(ns("sensitivity_export_png"), "Export PNG", class = "btn-info")
          )
        )
      ),

      # Status/Progress Panel
      box(
        title = "Status",
        width = 4,
        status = "info",

        uiOutput(ns("sensitivity_status"))
      )
    ),

    # Results: Plot
    fluidRow(
      box(
        title = "Sweep Results",
        width = 12,
        status = "success",
        solidHeader = TRUE,

        plotOutput(ns("sensitivity_plot"), height = "450px")
      )
    ),

    # Results: Summary Table
    fluidRow(
      box(
        title = "Summary Statistics",
        width = 12,
        status = "warning",

        DTOutput(ns("sensitivity_summary_table"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

mod_sensitivity_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      selectInput(ns("sensitivity_param_select"), "Select Parameter:",
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
      shinyjs::toggleState(ns("sensitivity_run"), can_run)

      has_results <- !is.null(rv$sensitivity_result)
      shinyjs::toggleState(ns("sensitivity_export_csv"), has_results)
      shinyjs::toggleState(ns("sensitivity_export_png"), has_results)
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
  })
}
