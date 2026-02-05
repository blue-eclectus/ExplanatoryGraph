# Tutorial: Building Your First Explanatory Model

This step-by-step guide walks you through building an explanatory model, running Bayesian inference, and interpreting the results.

## Prerequisites

Clone the repository and install dependencies:

```bash
git clone https://github.com/blue-eclectus/ExplanatoryGraph.git
cd ExplanatoryGraph
```

```r
# Install dependencies (including Bioconductor packages)
source("R/package_utils.R")
install_dependencies()
```

## Starting the App

Launch the interactive Shiny application:

```r
source("R/package_utils.R")
run_app()

# Or directly:
shiny::runApp(".")
```

The app opens in your browser. The sidebar on the left contains:

- **Build Model**: Step-by-step wizard for creating your model
- **Analysis**: Run Bayesian inference
- **Visualization**: Interactive network view
- **Tutorial**: Documentation
- **Advanced Features**:
  - **Research Priorities**: Value of Information analysis
  - **Sensitivity Analysis**: Parameter sweep analysis

## Explore an Example

Before building your own model, it may be useful to explore a pre-built example.

1. In the sidebar, click **Load Simple Test**
2. The app loads a model with two competing hypotheses that both attempt to explain a shared result

**Explore the model structure:**
- The visualization shows the full Bayesian network with hypotheses (H1, H2) at the top
- Each hypothesis has an account node that connects through explanatory chains to the shared result (R1)
- Use the **View** selector to restrict the view to a single explanatory account rather than the complete network
- Hover over nodes to see their descriptions and probability information
- Try different **Color Modes** to see baseline probabilities, posteriors, and changes

**Run analysis under custom scenarios:**
- Go to **Analysis** to compute Bayesian posteriors under custom truth value assignments
- Add conditions by setting nodes to TRUE or FALSE
- Click **Run Inference** to compute the updated posteriors

Now that you've seen an explanatory graph model, build one from scratch.

## Building Your Own Explanatory Graph

### Step 1: Define Your Evidence Corpus

The evidence corpus is the set of empirical results that competing hypotheses must explain. ExplanatoryGraph begins here because the goal of an explanatory account is to explain why the evidence appears as it does.

1. Navigate to **Build Model** → **Step 1: Evidence Corpus**
2. For each result you want to include:
   - **ID**: A short identifier (e.g., "R1", "ventilation_effect")
   - **Description**: What was observed
   - **Source**: Citation or reference (optional)
3. Click **Add Result**

**Example:**
```
ID: R1
Description: Improved ventilation reduces COVID transmission by 5-10x
Source: Morawska et al. 2020
```

### Step 2: Define Competing Hypotheses

Hypotheses are the base claims of the explanatory accounts you are comparing. The explanatory accounts framework assumes that the hypotheses you use are mutually exclusive (at most one can be true) and exhaustive (exactly one is true), so their priors must sum to 1.0. 

1. In the **Build Model** screen, go to **Step 2: Hypotheses**
2. For each hypothesis, provide:
   - **ID**: Short identifier (e.g., "H1", "H_aerosol")
   - **Description**: The theory's core claim
   - **Prior**: Your initial probability estimate (0-1)
3. Click **Add Hypothesis**

**Example:**
```
H1: COVID-19 transmits primarily via aerosols (prior: 0.5)
H2: COVID-19 transmits primarily via large droplets (prior: 0.5)
```

### Step 3: Build Explanatory Accounts

This is where you specify *how* each hypothesis explains the evidence. Each account provides explanatory chains from hypothesis to results.

#### Creating an Account

1. Select which **Hypothesis** this account supports
2. Enter an **Account ID** (e.g., "A1_aerosol")
3. Add a **Description** of the explanatory story
4. Click **Add Account**

#### Building Explanatory Chains

For each account, create chains linking it to results:

1. Select an account from the list (click on its button)
2. Click **Add Chain**
3. Configure the chain:
   - **Link 1 (Account → Phenomenon)**: Select or create a phenomenon, and choose required auxiliaries
   - **Link 2 (Phenomenon → Result)**: Select the result this chain explains, and choose required auxiliaries
4. Click **Create Chain**

**Example Chain:**
```
Account: A1_aerosol (supports H1: Aerosol transmission)
    │
    ▼ Link 1 (requires: Aux_viable = "Virus stays viable in aerosols")
    │
Phenomenon: PC1 = "Airborne particles accumulate in poorly ventilated spaces"
    │
    ▼ Link 2 (requires: Aux_ventilation = "Ventilation removes airborne particles")
    │
Result: R1 = "Ventilation reduces transmission by 5-10x"
```

#### Adding Auxiliaries

Auxiliaries are background assumptions needed for an explanatory link to hold. You can create them when building chains or add them separately:

1. With an account selected, click **Add Auxiliary**
2. Select an existing auxiliary or create new:
   - **ID**: Short identifier
   - **Description**: The assumption
   - **Prior**: How plausible is this assumption (0-1)

**Tips:**
- Auxiliaries with high priors (~0.9) are uncontroversial assumptions
- Auxiliaries with lower priors (~0.5-0.7) are more speculative
- Different accounts can share auxiliaries

### Step 4: Review and Build Network

Before running inference, validate your model:

1. Go to **Step 4: Review**
2. Check the validation panel for issues:
   - Hypothesis priors must sum to 1.0
   - At least 2 hypotheses required
   - Each account needs at least one chain
3. Review the model summary
4. Click **Build Network for Analysis**

If successful, you'll see "Network built successfully!" and can proceed to analysis.

## Running Inference

With the network built, run Bayesian inference:

**Run analysis under custom scenarios:**
- Go to **Analysis** to compute Bayesian posteriors under custom truth value assignments
- Add conditions by setting nodes to TRUE or FALSE
- Click **Run Inference** to compute the updated posteriors

### Interpreting Results

The Results panel shows:

**Hypothesis Posteriors** display how beliefs about each hypothesis update:
- **Prior**: Your initial probability estimate
- **Posterior**: Updated probability given the conditions
- **Change**: Difference between posterior and prior

If a hypothesis's posterior is greater than its prior, the conditions support that hypothesis. The magnitude indicates strength of support.

**Account Posteriors** show the probability of each explanatory account. When there are multiple explanatory accounts for a hypothesis, account posteriors can differ from one another—the hypothesis posterior equals the sum of its account posteriors.

### Saving Inferences

You can save inference results for comparison:

1. Click **Save Results...**
2. Enter a name (or use auto-generated)
3. Saved inferences appear in the comparison dropdown

## Visualization

Explore your model visually:

1. Go to the **Visualization** tab
2. **View**: Full Bayesian Network or individual accounts
3. **Color Mode**:
   - Network Structure: Node types by color
   - Baseline Marginals (Results=TRUE): Colored by probability with results observed
   - Analysis Posterior: Colored by posterior probability after custom conditions
   - Change (Δ): Colored by change (posterior - prior)
   - Log-Ratio: Colored by log(posterior/prior)
   - Research Priority: Colored by value of information
4. **Hide Auxiliaries**: Simplify the view
5. Hover over nodes for details

## Additional Analysis Tools

### Value of Information Analysis

Value of Information (VOI) analysis identifies which assumptions or claims would be most valuable to investigate. It computes how much learning the true state of each node would reduce uncertainty about the hypotheses.

1. Go to **Advanced Features** → **Research Priorities**
2. Configure the analysis:
   - **Starting Evidence**: Baseline (results observed) or unconditional
   - **Target**: Hypotheses (aggregate uncertainty) or Accounts (full distribution)
   - **Include Node Types**: Select which node types to analyze (auxiliaries, phenomena, explanatory links)
3. Click **Compute Value of Information**

The results table ranks nodes by research priority with key columns:
- **Priority**: Critical, High, Medium, or Low based on EVPI
- **% Red**: Expected percentage reduction in uncertainty
- **Gain if False**: How informative if this node turns out to be FALSE
- **Shift if False**: How much posteriors would shift if FALSE

Focus research on nodes with "Critical" priority or high "Gain if False" values.

### Sensitivity Analysis

Sensitivity analysis explores how much hypothesis posteriors depend on specific parameter values in the explanatory accounts graph. It sweeps a parameter across a range and shows how hypothesis posteriors respond.

1. Go to **Advanced Features** → **Sensitivity Analysis**
2. Select a parameter to sweep (e.g., a hypothesis or auxiliary prior)
3. Set the sweep range (min and max values)
4. Choose target: Hypotheses or Accounts
5. Click **Run Sweep**

The plot shows how hypothesis posteriors change as the parameter varies. Steep curves indicate high sensitivity to that parameter.

## Saving and Loading Models

**Export your model:**
1. Click **Export Model** in the sidebar
2. A JSON file downloads with your complete model
3. The filename includes the model name and date

**Import a model:**
1. Click **Import Model** in the sidebar
2. Select a previously exported `.json` file
3. The model loads and the network is built automatically

**Load a case study:**
- Click "Load Simple Test" or "Load COVID-19 Example" in the sidebar
- These load pre-built models for exploration

## Programmatic Usage

You can also use the framework programmatically:

```r
# Load the framework (from project root)
source("R/classes.R")
source("R/bayesian_spec_classes.R")
source("R/explanatory_classes.R")
source("R/explanatory_network.R")
source("R/bayesian_spec_cpt.R")
source("R/bayesian_spec_inference.R")

# Create a model
model <- ExplanatoryModel$new(name = "My Analysis")

# Add results
model$add_result(BayesianResult$new(
  id = "R1",
  text = "Key observation",
  source = "Study 2024"
))

# Add hypotheses
model$add_hypothesis(BayesianHypothesis$new(
  id = "H1", text = "Theory 1", source = "User", prior = 0.5
))
model$add_hypothesis(BayesianHypothesis$new(
  id = "H2", text = "Theory 2", source = "User", prior = 0.5
))

# Build accounts and chains...
# (See case study code in R/explanatory_case_study.R for full examples)

# Build network and run inference
graph <- build_bayesian_network(model)
cpts <- generate_all_cpts_spec(graph)
posteriors <- compute_posteriors(graph, cpts, list(R1 = "true"))

# Access results
posteriors$H1  # Posterior distribution for H1
```

## Next Steps

- Explore the **[COVID-19 Case Study](covid19_case_study_overview.md)** for a detailed real-world example (load it in the app with "Load COVID-19 Example")
- Read the **[Primer](primer.md)** for conceptual background
- Examine **R/explanatory_case_study.R** for programmatic model construction
