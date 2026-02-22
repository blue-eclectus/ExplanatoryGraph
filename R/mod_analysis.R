# ==============================================================================
# Module: Analysis
# ==============================================================================
#
# Shiny module for the Analysis tab.
# Handles evidence corpus display, additional conditions, running inference,
# saving inference results, and displaying posterior tables.
#
# Usage in app.R:
#   UI:     mod_analysis_ui("analysis_mod")
#   Server: mod_analysis_server("analysis_mod", rv)
#
# rv fields read:  rv$bayesian_network, rv$selector, rv$model, rv$unconditional
# rv fields written: rv$posteriors, rv$baseline, rv$saved_inferences,
#                    rv$additional_conditions, rv$cpts
#
# Helper functions used (from R/app_helpers.R):
#   generate_all_cpts_for_model(), build_results_evidence(),
#   generate_inference_name()
#
# Helper functions used (from R/bayesian.R):
#   compute_posteriors()

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

mod_analysis_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "analysis",
    fluidRow(
      box(
        title = "Evidence Corpus",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        p("By default, these results are conditioned as TRUE during inference. Use Additional Conditions below to override for counterfactual analysis."),

        uiOutput(ns("evidence_corpus_display"))
      )
    ),

    fluidRow(
      box(
        title = "Additional Conditions",
        width = 12,
        status = "info",

        fluidRow(
          column(4,
            selectInput(ns("cond_node"), "Node:", choices = NULL)
          ),
          column(3,
            selectInput(ns("cond_state"), "State:",
                       choices = c("TRUE" = "true", "FALSE" = "false"))
          ),
          column(5,
            actionButton(ns("add_condition"), "Add Condition", class = "btn-info"),
            actionButton(ns("clear_conditions"), "Clear All")
          )
        ),

        hr(),

        h4("Current Conditions:"),
        verbatimTextOutput(ns("current_conditions"))
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
            actionButton(ns("run_inference"), "Run Inference",
                        class = "btn-success btn-lg", icon = icon("calculator"))
          ),
          column(6,
            actionButton(ns("save_inference"), "Save Results...",
                        class = "btn-info", icon = icon("save")),
            span(style = "margin-left: 10px; color: gray;",
                 textOutput(ns("saved_count_text"), inline = TRUE))
          )
        )
      )
    ),

    fluidRow(
      box(
        title = "Results",
        width = 6,

        h4("Hypothesis Posteriors"),
        tableOutput(ns("hypothesis_posteriors"))
      ),

      box(
        title = "Account Posteriors",
        width = 6,

        tableOutput(ns("account_posteriors"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

mod_analysis_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # -------------------------------------------------------------------------
    # Evidence corpus display
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # Condition node choices
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # Additional conditions handlers
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # Run inference
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # SAVE INFERENCE FUNCTIONALITY
    # -------------------------------------------------------------------------

    # Saved inference count display
    output$saved_count_text <- renderText({
      count <- length(rv$saved_inferences)
      if (count > 0) sprintf("(%d/10 saved)", count) else ""
    })

    # Save inference button - show modal
    # CRITICAL: All modal inputs must use ns() explicitly since showModal()
    # inside moduleServer does NOT auto-namespace inputs.
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

        textInput(ns("save_inference_name"), "Name:", value = auto_name),

        p(style = "color: gray; font-size: 12px;",
          sprintf("Conditions: %s",
                  if (length(rv$additional_conditions) == 0) "(none)"
                  else paste(names(rv$additional_conditions), "=", rv$additional_conditions, collapse = ", "))),

        if (length(rv$saved_inferences) > 0) {
          tagList(
            hr(),
            h5("Currently Saved:"),
            uiOutput(ns("saved_list_in_modal"))
          )
        },

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_save"), "Save", class = "btn-primary")
        )
      ))
    })

    # List of saved inferences in modal
    # CRITICAL: actionButton inputId must use ns() for dynamic delete buttons
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
            inputId = ns(paste0("delete_saved_", gsub("[^a-zA-Z0-9]", "_", name))),
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
    # CRITICAL: input[[btn_id]] reads do NOT need ns() -- Shiny auto-strips
    # the module prefix when reading inputs inside moduleServer.
    observe({
      lapply(names(rv$saved_inferences), function(name) {
        btn_id <- paste0("delete_saved_", gsub("[^a-zA-Z0-9]", "_", name))
        observeEvent(input[[btn_id]], {
          rv$saved_inferences[[name]] <- NULL
          showNotification(sprintf("Deleted: %s", name), type = "message")
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # -------------------------------------------------------------------------
    # Hypothesis posteriors table
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # Account posteriors table
    # -------------------------------------------------------------------------

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
  })
}
