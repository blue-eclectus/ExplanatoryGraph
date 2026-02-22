# ==============================================================================
# Module: Network Visualization
# ==============================================================================
#
# Shiny module for the Visualization tab.
# Renders an interactive Bayesian network graph with multiple color modes
# (structure, baseline, posterior, delta, log-ratio, EVPI) and hover tooltips.
#
# Usage in app.R:
#   UI:     mod_visualization_ui("viz_mod")
#   Server: mod_visualization_server("viz_mod", rv)
#
# rv fields read:  rv$bayesian_network, rv$model, rv$baseline,
#                  rv$posteriors, rv$saved_inferences, rv$unconditional,
#                  rv$evpi_results
# rv fields written: (none -- pure consumer)
#
# Helper functions used (from R/app_helpers.R):
#   get_marginals_for_selection(), get_selection_label(),
#   add_saved_inference_choices()
#
# Helper functions used (from R/probability_viz_helpers.R):
#   compute_node_colors(), compute_node_tooltips()

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

mod_visualization_ui <- function(id) {
  ns <- NS(id)

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
            selectInput(ns("viz_view"), "View:",
                       choices = c("Full Bayesian Network" = "full")),
            div(style = "margin-left: 10px;",
              checkboxInput(ns("viz_hide_aux"), "Hide Auxiliaries", value = FALSE)
            )
          ),
          column(5,
            radioButtons(ns("viz_color_mode"), "Color Mode:",
                        choices = c("Network Structure" = "structure",
                                   "Baseline Marginals (Results=TRUE)" = "baseline",
                                   "Analysis Posterior (Custom Conditions from Analysis Tab)" = "posterior",
                                   "Change (\u0394)" = "delta",
                                   "Log-Ratio" = "ratio",
                                   "Research Priority" = "evpi"),
                        inline = FALSE)
          ),
          column(4,
            conditionalPanel(
              condition = sprintf("input['%s'] == 'posterior'", ns("viz_color_mode")),
              h5("Display:"),
              selectInput(ns("posterior_select"), "Marginals:",
                         choices = c("Current Posterior" = "current"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'delta' || input['%s'] == 'ratio'",
                                  ns("viz_color_mode"), ns("viz_color_mode")),
              h5("Compare:"),
              selectInput(ns("compare_from"), "From:",
                         choices = c("Unconditional" = "unconditional",
                                    "Baseline Marginals (Results=TRUE)" = "baseline")),
              selectInput(ns("compare_to"), "To:",
                         choices = c("Unconditional" = "unconditional",
                                    "Baseline Marginals (Results=TRUE)" = "baseline"))
            )
          )
        ),

        hr(),

        visNetworkOutput(ns("network_viz"), height = "600px")
      )
    ),

    fluidRow(
      box(
        title = "Legend",
        width = 12,
        status = "info",

        uiOutput(ns("viz_legend"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

mod_visualization_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
  })
}
