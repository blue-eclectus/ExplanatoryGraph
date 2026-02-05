# Explanatory Accounts Framework: A Quick Primer

This document provides a conceptual overview of the Explanatory Accounts Framework for newcomers. If you're ready to build a model, see the [Tutorial](tutorial.md).

## What Problem Does This Solve?

Scientists often need to combine diverse bodies of evidence to evaluate competing hypotheses. This is particularly challenging when evidence is complex and multimodal—produced by different methods, addressing different sub-questions, and sometimes yielding conflicting results.

Standard evidence amalgamation techniques struggle with this challenge:

- **Meta-analysis** requires quantitative results from similar experimental designs, excluding qualitative evidence and indirect lines of reasoning
- **Robustness analysis** tells us little when evidence doesn't neatly converge—as it rarely does in practice
- **Bayesian approaches**, while flexible, are often highly sensitive to prior assumptions and provide little guidance on how to actually structure the inference

Part of the problem is that much relevant evidence is highly indirect. An empirical result may not directly address a hypothesis, but connect to it through chains of reasoning involving auxiliary assumptions and other findings. When methods treat the relationship between hypothesis and evidence as a black box, this structure is obscured.

The Explanatory Accounts Framework addresses these issues by making the explanatory structure explicit—modeling not just *that* a hypothesis explains evidence, but *how* it does so through intermediate phenomena claims and auxiliary assumptions. This allows researchers to systematically develop explanatory accounts and attempt to eliminate them by showing that specific claims or inferential steps are implausible.

## When to Use This Framework

**Good use cases:**
- Comparing competing scientific theories against shared evidence
- Making sense of apparently conflicting results
- Making auxiliary assumptions explicit and evaluating their impact
- Identifying which assumptions most affect your conclusions
- Prioritizing which uncertainties would be most valuable to resolve

**Not designed for:**
- Simple parameter estimation
- Situations with only one plausible hypothesis
- Purely predictive tasks where explanatory mechanism doesn't matter

## Core Concepts

### The Building Blocks

**Hypotheses** are the competing theories you want to evaluate. For example:
- H1: "COVID-19 transmits primarily via aerosols"
- H2: "COVID-19 transmits primarily via large droplets"

**Results** are the empirical observations (evidence corpus) that any adequate theory must explain:
- "Study X found 18.7x higher transmission indoors vs outdoors"
- "Outbreak investigation at Restaurant Y found infections among diners with no direct contact"

**Phenomena claims** are intermediate patterns or regularities that connect hypotheses to results:
- "Virus particles remain airborne for extended periods"
- "Enclosed spaces concentrate infectious particles"

**Explanatory links** connect nodes in the explanatory pathway (e.g., hypothesis → phenomenon, or phenomenon → result). Each link has a reliability parameter representing how likely the connection is to hold. In the network implementation, links are represented as nodes (labeled EL) so their reliability can be modeled probabilistically.

**Auxiliaries** are background assumptions required for specific explanatory links to work:
- "Virus remains viable in aerosols for >2 hours"
- "Aerosol sampling techniques damage viral viability"

**Accounts** are complete explanatory pathways connecting a hypothesis to results via phenomena claims and auxiliaries. One hypothesis may have multiple accounts, since there may be multiple sets of phenomena claims and auxiliaries through which a hypothesis can explain the results.

*Note:* All nodes in the framework are propositional—they represent claims that are either true or false, each with a probability of being true or false—claims like "COVID-19 transmits primarily via aerosols" or "global temperature increase will exceed 2°C." This differs from Bayesian networks that model continuous random variables, where a node might represent temperature with a probability distribution over all possible temperature values. This framework uses propositional nodes because the goal is to evaluate the truth of explanatory accounts rather than estimate physical parameter values.

### The Explanatory Structure

```
Hypothesis ──→ [EL] ──→ Phenomenon ──→ [EL] ──→ Result
                              ↑
                          Auxiliary
```

Each account specifies *how* its hypothesis explains a result. The strength of an account depends on the reliability of its explanatory links and the plausibility of its auxiliaries. An account with implausible auxiliaries or weak links will receive less support from the evidence.

### Why Explanatory Links Are Nodes

In a simple graph, you might represent "Phenomenon explains Result" as a directed edge. But real explanatory reasoning is more complex:

1. **Auxiliaries attach to links**: An auxiliary assumption may be required for a specific inferential step—not for the nodes themselves. For example, "aerosol sampling techniques damage viral viability" is needed for the link connecting negative detection results to the aerosol hypothesis (see the [COVID-19 Case Study](covid_case_study_overview.md) for details). Making links nodes allows auxiliaries to target them directly.

2. **Links have reliability**: Each explanatory connection has its own strength. Representing this reliability as a random variable—one that auxiliaries can influence—requires a node.

3. **Links can be true or false**: An explanatory connection may or may not hold. Representing links as nodes with binary states allows us to reason about whether each inferential step actually works—and to eliminate accounts by showing that specific links are implausible.

## A Simple Example

The framework includes a "Simple Test" case you can load in the app. It illustrates the basic structure:

**Two competing hypotheses** both try to explain the same observed result (R1):

```
Theory A Account:                Theory B Account:

H1 (Theory A)                    H2 (Theory B)
    │                                │
  [EL1]                            [EL2]
    │                                │
    ▼                                ▼
   PC1                              PC2
(Mechanism A)                   (Mechanism B)
    │                                │
  [EL3]                            [EL4]
    │                                │
    └───────────┬────────────────────┘
                ▼
               R1 (Observed outcome)
```

Both accounts can explain R1, but they do so through different mechanisms (PC1 vs PC2). The framework computes how much support each hypothesis receives from the evidence, based on the reliability of each explanatory pathway and the plausibility of any required auxiliaries.

To explore this example, load "Simple Test" in the app and observe how the posterior probabilities shift when you condition on R1 being true.

## How Bayesian Inference Fits In

The framework translates explanatory structure into a Bayesian network:

1. **Hypothesis priors** reflect an initial credence in each hypothesis
2. **Auxiliary priors** reflect an initial credence in each background assumption
3. **Link parameters** capture the strength of explanatory connections:
   - *Reliability*: How likely is the connection to hold when the source is true? A reliability of 0.9 means that when the source node is true, there's a 90% chance the explanatory connection succeeds. Lower reliability represents weaker or more uncertain explanatory relationships.
   - *Epsilon*: A small baseline probability that allows results to occur even when unexplained. Without epsilon, a result would be impossible if no account explains it—but real-world results can occur for reasons outside the model. Epsilon is typically set very low (e.g., 0.01).
4. **Evidence conditioning** treats observed results as TRUE by default—because results represent what you've observed, inference asks: given these observations, which hypothesis best explains them? You can override individual results to FALSE for counterfactual analysis ("what if we hadn't observed this?")

When you run inference, the framework computes posterior probabilities for each hypothesis. Hypotheses with accounts that better explain the evidence—through more reliable links and more plausible auxiliaries—receive higher posterior probability.

## Key Analyses the Framework Provides

### Value of Information Analysis
Which uncertain nodes would be most valuable to investigate? The framework computes Expected Value of Perfect Information (EVPI) for each node—measuring how much the posterior probability of hypotheses would change, on average, if you learned that node's true state. Nodes with high EVPI are high-priority research targets: resolving their uncertainty would most affect your conclusions. You can analyze auxiliaries, phenomena, or links.

### Sensitivity Analysis
How much do conclusions depend on specific parameter choices? The framework sweeps parameters (like auxiliary priors or link reliabilities) across a range and plots how posterior probabilities change. This reveals which assumptions are "load-bearing"—if small changes to a parameter dramatically shift conclusions, that assumption deserves scrutiny.

### Transparent Reasoning
Every step from hypothesis to prediction is explicit. Unlike black-box statistical approaches, you can trace exactly why one hypothesis is favored: which links are strong, which auxiliaries are plausible, and where competing accounts diverge. This makes it easier to identify points of disagreement and communicate reasoning to others.

## The Network Visualization

The app provides an interactive visualization of the explanatory network. Nodes are colored by different aspects depending on the selected mode:

- **Structure**: Node types (hypotheses, phenomena, results, auxiliaries, links)
- **Prior**: Probabilities before conditioning on evidence
- **Posterior**: Probabilities after conditioning on results
- **Delta**: Change from prior to posterior (highlights nodes most affected by evidence)
- **Log-ratio**: Log odds ratio showing strength and direction of evidential impact

Click on nodes to see details, and use the layout controls to arrange the graph.

## Next Steps

- **Tutorial**: Step-by-step guide to building a model — [tutorial.md](tutorial.md)
- **Case Study**: A worked example with COVID-19 transmission — [covid_case_study_overview.md](covid_case_study_overview.md)
- **Theoretical Background**: The dissertation chapter behind this framework — [Dissertation Chapter](https://blue-eclectus.github.io/dissertation-chapter-explanatory-accounts.pdf)
- **Try the Shiny App**: Load "Simple Test" or "COVID-19 Example" from the sidebar
