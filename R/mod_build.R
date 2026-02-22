# ==============================================================================
# Module: Build Model
# ==============================================================================
#
# Shiny module for the Build Model tab.
# Implements a 4-step wizard for constructing explanatory models:
#   Step 1: Define Evidence Corpus (empirical results)
#   Step 2: Define Hypotheses (competing theories with priors)
#   Step 3: Build Accounts (explanatory pathways: H -> Phenomena -> Results)
#   Step 4: Review & Validate
#
# Usage in app.R:
#   UI:     mod_build_ui("build_mod")
#   Server: mod_build_server("build_mod", rv, build_network_fn,
#                            show_invalidation_warning_fn)
#
# rv fields read:    rv$saved_inferences, rv$suppress_invalidation_warning
# rv fields written: rv$model, rv$current_step, rv$selected_account_id
#
# Callbacks:
#   build_network_fn(switch_to_analysis)  - builds the Bayesian network
#   show_invalidation_warning_fn(desc, action) - shows invalidation warning
#
# Helper functions used (from R/app_helpers.R):
#   validate_id()

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

mod_build_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "build",
    fluidRow(
      box(
        title = "Build Explanatory Model",
        width = 12,
        status = "primary",
        solidHeader = TRUE,

        # Step indicator
        uiOutput(ns("step_indicator")),

        hr(),

        # Step content (dynamic)
        uiOutput(ns("step_content")),

        hr(),

        # Navigation buttons
        fluidRow(
          column(6,
            actionButton(ns("prev_step"), paste0("\u2190", " Previous"), class = "btn-default")
          ),
          column(6, align = "right",
            actionButton(ns("next_step"), paste0("Next ", "\u2192"), class = "btn-primary")
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

mod_build_server <- function(id, rv, build_network_fn, show_invalidation_warning_fn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # =========================================================================
    # STEP INDICATOR UI
    # =========================================================================
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

    # =========================================================================
    # STEP CONTENT (Dynamic UI)
    # =========================================================================
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

        DTOutput(ns("results_table")),

        hr(),

        h4("Add Result"),
        textInput(ns("result_id"), "ID:"),
        textAreaInput(ns("result_text"), "Description:", rows = 3),
        textInput(ns("result_source"), "Source/Citation (optional):"),
        actionButton(ns("add_result"), "Add Result", class = "btn-success")
      )
    }

    # ===== Step 2: Hypotheses UI =====

    step2_hypotheses_ui <- function() {
      tagList(
        h3("Step 2: Define Competing Hypotheses"),
        p("Create the hypotheses you want to compare. Priors must sum to 1.0."),

        DTOutput(ns("hypotheses_table")),

        hr(),

        h4("Add Hypothesis"),
        textInput(ns("hyp_id"), "ID:"),
        textAreaInput(ns("hyp_text"), "Description:", rows = 3),
        numericInput(ns("hyp_prior"), "Prior:", value = 0.5, min = 0, max = 1, step = 0.01),
        actionButton(ns("add_hypothesis"), "Add Hypothesis", class = "btn-success"),

        hr(),

        uiOutput(ns("prior_validation"))
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
            uiOutput(ns("accounts_list")),

            hr(),

            h4("Add Account"),
            selectInput(ns("new_account_hyp"), "Hypothesis:",
                       choices = NULL),
            textInput(ns("new_account_id"), "Account ID:"),
            textAreaInput(ns("new_account_desc"), "Description:", rows = 2),
            actionButton(ns("add_account"), "Add Account", class = "btn-success")
          ),

          # Right panel: Account editor
          column(8,
            conditionalPanel(
              condition = sprintf("output['%s']", ns("account_selected")),

              h4("Edit Account"),
              verbatimTextOutput(ns("selected_account_info")),

              hr(),

              h4("Auxiliaries"),
              p("Background assumptions used by this account."),
              uiOutput(ns("account_auxiliaries")),

              actionButton(ns("add_auxiliary_to_account"), "Add Auxiliary", icon = icon("plus")),

              hr(),

              h4("Explanatory Chains"),
              p("Each chain connects the account to a result via a phenomenon."),

              uiOutput(ns("chains_list")),

              hr(),

              actionButton(ns("add_chain"), "Add Chain", class = "btn-primary", icon = icon("plus"))
            ),

            conditionalPanel(
              condition = sprintf("!output['%s']", ns("account_selected")),
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
        uiOutput(ns("model_validation")),

        hr(),

        actionButton(ns("build_network"), "Build Network for Analysis",
                    class = "btn-success btn-lg", icon = icon("cogs"))
      )
    }

    # =========================================================================
    # NAVIGATION BUTTONS
    # =========================================================================

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

    # =========================================================================
    # Step 1: Results Table
    # =========================================================================

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

    # =========================================================================
    # Step 2: Hypotheses Table
    # =========================================================================

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

    # =========================================================================
    # Step 3: Accounts
    # =========================================================================

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
              ns(paste0("select_acc_", acc$id)),
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
    # Track which account IDs already have observers to prevent accumulation
    wired_account_ids <- reactiveVal(character(0))

    observe({
      current_ids <- names(rv$model$accounts)
      already_wired <- wired_account_ids()
      new_ids <- setdiff(current_ids, already_wired)

      for (acc_id in new_ids) {
        local({
          local_id <- acc_id
          observeEvent(input[[paste0("select_acc_", local_id)]], {
            rv$selected_account_id <- local_id
          })
        })
      }

      wired_account_ids(union(already_wired, new_ids))
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

      chain_items <- lapply(account$explanatory_link_ids, function(link_id) {
        link <- rv$model$explanatory_links[[link_id]]
        if (is.null(link)) return(NULL)

        div(
          style = "border: 1px solid #ddd; padding: 10px; margin: 5px 0;",
          sprintf("%s \u2192 %s", link$source_id, link$target_id),
          tags$br(),
          sprintf("Reliability: %.2f, Epsilon: %.2f", link$reliability, link$epsilon),
          if (length(link$required_auxiliary_ids) > 0) {
            tagList(tags$br(), sprintf("Requires: %s", paste(link$required_auxiliary_ids, collapse = ", ")))
          }
        )
      })

      tagList(chain_items)
    })

    # -------------------------------------------------------------------------
    # Add chain modal
    # -------------------------------------------------------------------------
    # CRITICAL: All modal inputs must use ns() since showModal() inside
    # moduleServer does NOT auto-namespace inputs.

    observeEvent(input$add_chain, {
      req(rv$selected_account_id)

      showModal(modalDialog(
        title = "Add Explanatory Chain",
        size = "l",

        p("Build a chain: Account \u2192 Phenomenon \u2192 Result"),

        h4("Link 1: Account \u2192 Phenomenon"),
        selectInput(ns("chain_phenomenon"), "Phenomenon:",
                   choices = c("(Create new...)" = "_new_",
                              setNames(names(rv$model$phenomena),
                                      sapply(rv$model$phenomena, function(p) p$text)))),

        conditionalPanel(
          condition = sprintf("input['%s'] == '_new_'", ns("chain_phenomenon")),
          textInput(ns("chain_new_pc_id"), "New Phenomenon ID:"),
          textAreaInput(ns("chain_new_pc_text"), "Description:", rows = 2)
        ),

        selectInput(ns("chain_link1_aux"), "Required Auxiliaries:",
                   choices = setNames(names(rv$model$auxiliaries),
                                    sapply(rv$model$auxiliaries, function(a) a$text)),
                   multiple = TRUE),

        hr(),

        h4("Link 2: Phenomenon \u2192 Result"),
        selectInput(ns("chain_result"), "Result:",
                   choices = setNames(names(rv$model$results),
                                    sapply(rv$model$results, function(r) r$text))),

        selectInput(ns("chain_link2_aux"), "Required Auxiliaries:",
                   choices = setNames(names(rv$model$auxiliaries),
                                    sapply(rv$model$auxiliaries, function(a) a$text)),
                   multiple = TRUE),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("create_chain"), "Create Chain", class = "btn-primary")
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

          # Create new phenomenon
          pc <- BayesianPhenomenon$new(
            id = pc_id,
            text = trimws(input$chain_new_pc_text),
            source = "User"
          )
          rv$model$add_phenomenon(pc)
        }

        # Add phenomenon to account
        account$add_phenomenon_id(pc_id)

        # Create Link 1: Account -> Phenomenon
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

        # Create Link 2: Phenomenon -> Result
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

    # -------------------------------------------------------------------------
    # Account auxiliaries
    # -------------------------------------------------------------------------

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

    # -------------------------------------------------------------------------
    # Add auxiliary modal
    # -------------------------------------------------------------------------
    # CRITICAL: All modal inputs must use ns() since showModal() inside
    # moduleServer does NOT auto-namespace inputs.

    observeEvent(input$add_auxiliary_to_account, {
      req(rv$selected_account_id)

      showModal(modalDialog(
        title = "Add Auxiliary",

        selectInput(ns("aux_select"), "Auxiliary:",
                   choices = c("(Create new...)" = "_new_",
                              setNames(names(rv$model$auxiliaries),
                                      sapply(rv$model$auxiliaries, function(a) a$text)))),

        conditionalPanel(
          condition = sprintf("input['%s'] == '_new_'", ns("aux_select")),
          textInput(ns("new_aux_id"), "ID:"),
          textAreaInput(ns("new_aux_text"), "Description:", rows = 2),
          numericInput(ns("new_aux_prior"), "Prior:", value = 0.8, min = 0, max = 1, step = 0.01)
        ),

        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("add_aux_confirm"), "Add", class = "btn-primary")
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

    # =========================================================================
    # Step 4: Review and Validation
    # =========================================================================

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

    # =========================================================================
    # NETWORK BUILDING (button handler)
    # =========================================================================
    # Uses callbacks to build_network_for_analysis and show_invalidation_warning
    # which remain in app.R as cross-cutting functions.

    observeEvent(input$build_network, {
      # Check if warning needed for saved inferences
      if (length(rv$saved_inferences) > 0 && !rv$suppress_invalidation_warning) {
        show_invalidation_warning_fn("rebuild the Bayesian network", "build_network")
        return()  # Wait for user confirmation
      }

      # Clear saved inferences if any (silently if warnings suppressed)
      if (length(rv$saved_inferences) > 0) {
        rv$saved_inferences <- list()
        showNotification("Saved inferences cleared due to network rebuild.", type = "warning")
      }

      build_network_fn(switch_to_analysis = TRUE)
    })
  })
}
