# Refactor app.R into Shiny Modules

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Decompose the monolithic 3,182-line `app.R` into 5 colocated Shiny modules (UI + server per file), one helpers file, and a thin ~250-line entrypoint.

**Architecture:** Each tab becomes a Shiny module using `moduleServer`/`NS`. Shared state flows through `rv` (reactiveValues) passed to each module. Cross-cutting concerns (invalidation warnings, case study loaders, import/export, `build_network_for_analysis`) stay in the top-level `app.R` server.

**Tech Stack:** R, Shiny (`moduleServer`/`NS`), shinydashboard, shinyjs, visNetwork, DT, R6

---

## Migration Order

Helpers → VOI → Sensitivity → Visualization → Analysis → Build Model

(Most self-contained first, most interconnected last.)

---

## Phase 0: Extract Pure Helpers

### Task 0.1: Create `R/app_helpers.R`

**Files:**
- Create: `R/app_helpers.R`
- Modify: `app.R` (lines 53-64 source block, delete lines 66-252)

**Step 1:** Create `R/app_helpers.R` containing these 7 functions copied verbatim from `app.R`:

| Function | Current lines | Used by |
|---|---|---|
| `generate_all_cpts_for_model()` | 84-131 | build_network, run_inference, VOI, sensitivity |
| `generate_inference_name()` | 140-151 | Analysis |
| `get_marginals_for_selection()` | 160-175 | Visualization |
| `get_selection_label()` | 181-186 | Visualization |
| `build_results_evidence()` | 192-194 | build_network, run_inference, VOI, sensitivity |
| `add_saved_inference_choices()` | 201-208 | Visualization |
| `validate_id()` | 221-252 | Build |

Add source guard at top matching project pattern:
```r
if (!isNamespaceLoaded("ExplanatoryAccounts")) {
  if (!exists("generate_selector_cpt")) source("R/bayesian_spec_cpt.R")
}
```

**Step 2:** In `app.R`, delete lines 66-252 (the HELPER FUNCTIONS section). Add `source("R/app_helpers.R")` *inside* the existing `if (!isNamespaceLoaded("ExplanatoryAccounts"))` guard block (lines 53-64), alongside the other `source()` calls. Do NOT place it outside the guard — that would break package-mode startup where the namespace is already loaded.

**Step 3:** Run `for f in tests/test_*.R; do Rscript "$f"; done && Rscript tests/integration_test.R` to verify nothing broke.

**Step 4:** Commit: `refactor: extract pure helpers from app.R to R/app_helpers.R`

### Task 0.2: Write tests for app_helpers.R

**Files:**
- Create: `tests/test_app_helpers.R`

Use the project's hand-rolled test framework (matching `tests/test_voi_analysis.R` pattern). Test:

- `generate_inference_name()`: empty → `"All results=TRUE"`; single condition; multiple conditions
- `build_results_evidence()`: model with 2 results → named list of `"true"` values
- `validate_id()`: empty → error; invalid chars → error; duplicate → error; valid → TRUE
- `get_selection_label()`: NULL → "Baseline"; "unconditional" → "Unconditional"; "current" → "Current"; arbitrary → as-is
- `add_saved_inference_choices()`: empty saved → unchanged; non-empty → appended
- `generate_all_cpts_for_model()`: load simple case study, build network, verify output is named list with Selector CPT

Run: `Rscript tests/test_app_helpers.R`

**Step 5:** Commit: `test: add unit tests for app_helpers.R`

### Task 0.3: Deduplicate `generate_all_cpts_for_model` in test files

**Files:**
- Modify: `tests/test_voi_analysis.R` (replace lines 24-73 with `source("R/app_helpers.R")`)
- Modify: `tests/test_sensitivity_analysis.R` (replace lines 463-509 with `source("R/app_helpers.R")`)

Run both test files to verify. Commit: `refactor: deduplicate generate_all_cpts_for_model in tests`

---

## Phase 1: VOI Module

### Task 1.1: Create `R/mod_voi.R`

**Files:**
- Create: `R/mod_voi.R`
- Modify: `app.R`

**Extract from `app.R`:**
- UI: lines 489-619 (`tabItem(tabName = "voi", ...)`)
- Server: lines 2402-2801 (VOI section including `generate_evpi_cache_key()`)

**Module pattern:**
```r
mod_voi_ui <- function(id) {
  ns <- NS(id)
  tabItem(tabName = "voi", ...)  # ns() all input/output IDs
}

mod_voi_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # needed for renderUI-generated inputs
    ...
  })
}
```

**IDs to namespace with `ns()`:** `voi_starting_evidence`, `voi_target`, `voi_node_types`, `voi_compute`, `voi_export_csv`, `voi_current_uncertainty`, `voi_results_table`, `voi_detail_panel`

**rv fields:** Reads `rv$bayesian_network`, `rv$selector`, `rv$model`, `rv$baseline`, `rv$unconditional`. Writes `rv$evpi_results` (read by Visualization EVPI color mode), plus VOI-internal fields (`rv$evpi_selected_node`, `rv$evpi_computing`, `rv$evpi_cache`, `rv$evpi_cache_key`).

**Wire into `app.R`:**
- UI: Replace tabItem block with `mod_voi_ui("voi_mod")`
- Server: Remove VOI section, add `mod_voi_server("voi_mod", rv)`
- Source: Add `source("R/mod_voi.R")` to source block

### Task 1.2: Validate VOI module

Manual test: Load COVID example → Research Priorities → Compute VOI → verify table, row selection, detail panel, CSV export. Then Visualization → EVPI color mode → verify colors appear.

Run: `Rscript tests/test_voi_analysis.R && Rscript tests/test_app_helpers.R`

Commit: `refactor: extract VOI tab into R/mod_voi.R Shiny module`

---

## Phase 2: Sensitivity Module

### Task 2.1: Create `R/mod_sensitivity.R`

**Files:**
- Create: `R/mod_sensitivity.R`
- Modify: `app.R`

**Extract from `app.R`:**
- UI: lines 622-757 (`tabItem(tabName = "sensitivity", ...)`)
- Server: lines 2837-3177 (Sensitivity section)

**Critical: `renderUI`-generated input.** `output$sensitivity_param_tree` (line 2848) creates a `selectInput("sensitivity_param_select", ...)` dynamically. Inside the module, this must use `session$ns`:
```r
selectInput(ns("sensitivity_param_select"), "Select Parameter:", ...)
```

**IDs to namespace:** `sensitivity_evidence_display`, `sensitivity_param_tree`, `sensitivity_param_info`, `sensitivity_selected_display`, `sensitivity_range_min`, `sensitivity_range_max`, `sensitivity_full_range`, `sensitivity_target`, `sensitivity_run`, `sensitivity_export_csv`, `sensitivity_export_png`, `sensitivity_status`, `sensitivity_plot`, `sensitivity_summary_table`, plus the dynamically-generated `sensitivity_param_select`

**rv fields:** All sensitivity fields are tab-internal. Reads `rv$model`, `rv$bayesian_network`, `rv$selector`.

**Wire into `app.R`:** Same pattern as VOI.

### Task 2.2: Validate Sensitivity module

Manual test: Load case study → Sensitivity Analysis → select parameter → verify range auto-updates → Full Range button → Run Sweep → verify plot + summary table → CSV/PNG export.

Commit: `refactor: extract Sensitivity tab into R/mod_sensitivity.R Shiny module`

---

## Phase 3: Visualization Module

### Task 3.1: Create `R/mod_visualization.R`

**Files:**
- Create: `R/mod_visualization.R`
- Modify: `app.R`

**Extract from `app.R`:**
- UI: lines 759-821 (`tabItem(tabName = "viz", ...)`)
- Server: lines 1923-2289 (Visualization section)

**Critical: `conditionalPanel` JS namespacing.** There are 3 conditionalPanels in the viz UI (lines 788, 794). JavaScript conditions must reference namespaced IDs:

```r
# Line 788 becomes:
conditionalPanel(
  condition = sprintf("input['%s'] == 'posterior'", ns("viz_color_mode")),
  ...
)

# Line 794 becomes:
conditionalPanel(
  condition = sprintf("input['%s'] == 'delta' || input['%s'] == 'ratio'",
                      ns("viz_color_mode"), ns("viz_color_mode")),
  ...
)
```

**IDs to namespace:** `viz_view`, `viz_hide_aux`, `viz_color_mode`, `posterior_select`, `compare_from`, `compare_to`, `network_viz`, `viz_legend`

**rv fields:** Pure consumer — reads `rv$bayesian_network`, `rv$model`, `rv$baseline`, `rv$posteriors`, `rv$saved_inferences`, `rv$unconditional`, `rv$evpi_results`. No writes to shared state.

### Task 3.2: Validate Visualization module

Manual test: Load case study → run inference → test all 6 color modes → test view dropdown → test "Hide Auxiliaries" → verify conditionalPanel show/hide works → save inference in Analysis → return to Viz → verify it appears in comparison dropdowns.

Commit: `refactor: extract Visualization tab into R/mod_visualization.R Shiny module`

---

## Phase 4: Analysis Module

### Task 4.1: Create `R/mod_analysis.R`

**Files:**
- Create: `R/mod_analysis.R`
- Modify: `app.R`

**Extract from `app.R`:**
- UI: lines 406-487 (`tabItem(tabName = "analysis", ...)`)
- Server: lines 1643-1921 (Analysis + Save Inference sections)

**Critical: Modal input namespacing.** `showModal()` called inside `moduleServer` does NOT auto-namespace inputs. All modal inputs must use `ns()` explicitly:

```r
# Save inference modal (line 1784):
ns <- session$ns
showModal(modalDialog(
  textInput(ns("save_inference_name"), "Name:", value = auto_name),
  ...
  uiOutput(ns("saved_list_in_modal")),
  ...
  actionButton(ns("confirm_save"), "Save", class = "btn-primary")
))
```

**Critical: Dynamic delete button observers (line 1860).** The `renderUI` creating delete buttons must namespace them with `session$ns`. The `observeEvent` listeners reference them as `input[[btn_id]]` (no `ns()` needed on read — module handles it):

```r
output$saved_list_in_modal <- renderUI({
  ...
  actionButton(inputId = ns(paste0("delete_saved_", ...)), ...)
  ...
})

observe({
  lapply(names(rv$saved_inferences), function(name) {
    btn_id <- paste0("delete_saved_", gsub("[^a-zA-Z0-9]", "_", name))
    observeEvent(input[[btn_id]], { ... }, ignoreInit = TRUE, once = TRUE)
  })
})
```

**Housekeeping:** Move `rv$additional_conditions = list()` initialization from inline (line 1705) into the `reactiveValues()` call in `app.R`.

**rv fields:** Writes `rv$posteriors`, `rv$baseline`, `rv$saved_inferences` (all read by Visualization). Reads `rv$bayesian_network`, `rv$selector`, `rv$model`.

### Task 4.2: Validate Analysis module

Manual test: Load case study → Analysis → verify evidence corpus → add conditions → run inference → verify posteriors tables → save inference → open save modal → verify saved list → delete a saved inference → go to Viz → verify saved inference in dropdowns.

Commit: `refactor: extract Analysis tab into R/mod_analysis.R Shiny module`

---

## Phase 5: Build Model Module (Most Complex)

### Task 5.1: Create `R/mod_build.R`

**Files:**
- Create: `R/mod_build.R`
- Modify: `app.R`

**Extract from `app.R`:**
- UI: lines 373-404 (`tabItem(tabName = "build", ...)`)
- Server: lines 896-1575 (step indicator, step content, all 4 step UIs and handlers, navigation buttons)
- Also: `build_network` button handler (lines 1627-1641)

**Module signature — pass cross-cutting functions as callbacks:**
```r
mod_build_server <- function(id, rv, build_network_fn, show_invalidation_warning_fn) {
  moduleServer(id, function(input, output, session) {
    ...
    observeEvent(input$build_network, {
      if (length(rv$saved_inferences) > 0 && !rv$suppress_invalidation_warning) {
        show_invalidation_warning_fn("rebuild the Bayesian network", "build_network")
        return()
      }
      ...
      build_network_fn(switch_to_analysis = TRUE)
    })
  })
}
```

**Critical: Step UI functions generate ALL inputs via `renderUI`.** The functions `step1_evidence_ui()` through `step4_review_ui()` are called from `output$step_content <- renderUI(...)`. Every input/output they create must use `session$ns`:

Step 1 IDs: `results_table`, `result_id`, `result_text`, `result_source`, `add_result`
Step 2 IDs: `hypotheses_table`, `hyp_id`, `hyp_text`, `hyp_prior`, `add_hypothesis`, `prior_validation`
Step 3 IDs: `new_account_hyp`, `new_account_id`, `new_account_desc`, `add_account`, `accounts_list`, `account_selected`, `selected_account_info`, `account_auxiliaries`, `add_auxiliary_to_account`, `chains_list`, `add_chain`
Step 4 IDs: `model_validation`, `build_network`
Main UI IDs: `step_indicator`, `step_content`, `prev_step`, `next_step`

**Critical: Modal inputs (2 modals).** Chain modal (line 1338) and auxiliary modal (line 1476) create inputs that must be namespaced:

Chain modal: `chain_phenomenon`, `chain_new_pc_id`, `chain_new_pc_text`, `chain_link1_aux`, `chain_result`, `chain_link2_aux`, `create_chain`
Auxiliary modal: `aux_select`, `new_aux_id`, `new_aux_text`, `new_aux_prior`, `add_aux_confirm`

**Critical: `conditionalPanel` inside modals.** Lines 1350 and 1484 use JS conditions referencing bare input IDs. Must use namespaced form:
```r
conditionalPanel(
  condition = sprintf("input['%s'] == '_new_'", ns("chain_phenomenon")),
  ...
)
```

**Critical: `conditionalPanel` for account editor.** Lines 1006 and 1032 use `output.account_selected`:
```r
conditionalPanel(
  condition = sprintf("output['%s']", ns("account_selected")),
  ...
)
```

**Critical: Fix dynamic account selection observers (line 1283).** The current pattern accumulates handlers without `once=TRUE` — every time `rv$model$accounts` changes, new overlapping `observeEvent` handlers are created for every account. Fix during this refactor by adding `once = TRUE` (matching the saved-inference delete pattern) so each observer self-destructs after firing. The outer `observe` will recreate them on the next model change. The renderUI buttons must use `ns()`, and the observers read via `input[[...]]` (auto-namespaced by module).

```r
observe({
  lapply(names(rv$model$accounts), function(acc_id) {
    observeEvent(input[[paste0("select_acc_", acc_id)]], {
      rv$selected_account_id <- acc_id
    }, once = TRUE)  # <-- prevents handler accumulation
  })
})
```

**rv fields:** Reads/writes `rv$model`, `rv$current_step`, `rv$selected_account_id`. Reads `rv$saved_inferences`, `rv$suppress_invalidation_warning`. Calls callbacks for `build_network_for_analysis` and `show_invalidation_warning`.

### Task 5.2: Validate Build Model module

Most thorough manual test:
1. Fresh start → step wizard renders at Step 1
2. Add 2 results → navigate Step 2 → add 2 hypotheses (priors sum to 1.0) → verify validation
3. Step 3 → add account → select it → add auxiliary via modal → add chain via modal (test new phenomenon + existing)
4. Step 4 → verify review counts → Build Network → verify navigation to Analysis
5. Test invalid IDs (empty, duplicate, special chars)
6. Test prev/next navigation

Commit: `refactor: extract Build Model tab into R/mod_build.R Shiny module`

---

## Phase 6: Final Cleanup

### Task 6.1: Clean up `app.R`

Verify `app.R` is now a thin entrypoint (~250 lines) containing:
- Package loading + source() calls (including all `R/mod_*.R`)
- Sidebar UI (menu, case study buttons, import/export)
- `dashboardBody` wrapper calling `mod_*_ui()` for each tab
- Help tab (inline, too small to modularize)
- Server: `reactiveValues` init, navigation handlers, module server calls, `build_network_for_analysis()`, invalidation warning system, case study loaders, import/export

### Task 6.2: Full end-to-end validation

1. Load Simple Test → verify Viz tab
2. All 6 color modes work
3. Analysis → run inference → save 2 inferences
4. Viz → verify saved inferences in dropdowns
5. Research Priorities → compute VOI → Viz → EVPI color mode
6. Sensitivity → select param → run sweep → verify plot
7. Build Model → create model from scratch (all 4 steps) → Build Network
8. Export model → re-import
9. Load COVID with saved inferences → verify invalidation warning modal

Run the full test suite (all test files, not just a subset):
```bash
for f in tests/test_*.R; do echo "=== $f ===" && Rscript "$f"; done
Rscript tests/integration_test.R
```

Commit: `refactor: complete app.R modularization into 5 Shiny modules`

---

## Final File Structure

| File | Purpose | ~Lines |
|---|---|---|
| `app.R` | Thin entrypoint, cross-cutting logic | 250 |
| `R/app_helpers.R` | 7 pure helper functions | 200 |
| `R/mod_build.R` | Build Model tab (UI + server) | 700 |
| `R/mod_analysis.R` | Analysis tab (UI + server) | 300 |
| `R/mod_visualization.R` | Visualization tab (UI + server) | 400 |
| `R/mod_voi.R` | VOI tab (UI + server) | 400 |
| `R/mod_sensitivity.R` | Sensitivity tab (UI + server) | 350 |
| `tests/test_app_helpers.R` | Unit tests for helpers | 120 |

## Key Risks

| Risk | Mitigation |
|---|---|
| Modal inputs not namespaced → silent button failures | Use `ns <- session$ns` in every `showModal()` call; test every modal |
| `conditionalPanel` JS conditions break → panels stuck visible/hidden | Use `sprintf("input['%s']", ns("id"))` pattern; test all 7 conditionalPanels |
| Dynamic `renderUI` inputs not namespaced → observers don't fire | Use `ns()` for all IDs in `renderUI` blocks; especially step UIs and account buttons |
| Dynamic observer accumulation on account selection | Fix during refactor: add `once = TRUE` to match saved-inference delete pattern |
| `build_network_for_analysis` needs top-level `session` | Keep in `app.R`; pass as callback to mod_build |
