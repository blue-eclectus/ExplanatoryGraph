# The ExplanatoryGraph Framework: A Primer

ExplanatoryGraph implements a framework based on the idea of **explanatory accounts**. This document explains the idea and how ExplanatoryGraph implements it. If you're ready to build a model, see the [Tutorial](tutorial.md). To explore an example, see the [COVID-19 Case Study](covid19_case_study_overview.md).

## What Problem Does This Solve?

Scientists often need to integrate complex bodies of evidence to evaluate competing hypotheses. This is particularly challenging when evidence is indirect, produced by diverse methods, or includes seemingly conflicting results.

Standard evidence synthesis techniques struggle with this challenge:

- **Meta-analysis** requires quantitative results from similar experimental designs, excluding qualitative evidence and indirect lines of reasoning
- **Robustness analysis and methodological triangulation** tell us little when evidence doesn't neatly converge, as it rarely does in practice
- **Bayesian approaches**, while flexible, often provide little guidance on how to actually structure the inference

The ExplanatoryGraph framework addresses these issues by making the explanatory structure explicit—modeling exactly *how* a hypothesis explains the body of evidence through intermediate phenomena claims and auxiliary assumptions. This allows researchers to systematically compare hypotheses based on their connections to the evidence base, even when evidence is indirect, diverse, and discordant.

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

### Explanatory Accounts

Explaining a body of evidence usually requires more than a hypothesis alone. Consider the aerosol transmission hypothesis for COVID-19. To become widely accepted, it had to explain, among other things, (1) why several studies failed to detect viable SARS-CoV-2 from aerosols, (2) why SARS-CoV-2 has a lower basic reproduction number than most known airborne viruses such as measles, and (3) why surgical masks are somewhat effective in reducing transmission even though they are not designed to stop aerosols. To explain these facts, proponents of aerosol transmission had to appeal to a variety of methodological and empirical claims. For instance, to explain (1), they pointed out that common aerosol sampling techniques tend to damage viruses—so an inability to detect viable virus in aerosols would not actually rule out the hypothesis. Fully assessing empirical support for the aerosol hypothesis thus required assessing a **set** of explanations. An explanatory account is simply a set of explanations that a hypothesis uses to explain the evidence base.

A single hypothesis can sometimes explain the evidence base in multiple ways. Different background assumptions or intermediate claims may be logically compatible with a hypothesis and help to explain the empirical results. In these cases, there are multiple explanatory accounts for a hypothesis. The ExplanatoryGraph framework allows for this and incorporates it into its Bayesian capabilities.

### Node Types

**Hypotheses** are the base claims of competing accounts you want to evaluate. For example:
- H1: "COVID-19 transmits primarily via aerosols"
- H2: "COVID-19 transmits primarily via large droplets"

**Results** are the empirical observations (evidence corpus) that any adequate account must explain:
- "Study X found 18.7x higher transmission risk indoors vs outdoors"
- "Outbreak investigation at Restaurant Y found infections among diners with no direct contact"

**Phenomena claims** are intermediate patterns or regularities that connect hypotheses to results:
- "Virus particles remain airborne for extended periods"
- "Enclosed spaces concentrate infectious particles"

**Explanatory links** connect nodes in the explanatory pathway (e.g., hypothesis → phenomenon, or phenomenon → result). Each link has a reliability parameter representing how likely the connection is to hold. In the network implementation, links are represented as nodes (labeled EL) so their reliability can be modeled probabilistically.

**Auxiliaries** are background assumptions required for specific explanatory links to work:
- "Virus remains viable in aerosols for >2 hours"
- "Aerosol sampling techniques damage viral viability"

*Note:* All nodes in the framework are propositional—they represent claims that are either true or false—claims like "COVID-19 transmits primarily via aerosols" or "global temperature increase will exceed 2°C." This differs from Bayesian networks that model continuous random variables, where a node might represent temperature with a probability distribution over possible temperature values. ExplanatoryGraph currently uses propositional nodes only. Future versions may incorporate nodes with continuous variables.

### The Explanatory Structure

```
Hypothesis ──→ [EL] ──→ Phenomenon ──→ [EL] ──→ Result
                ↑
            Auxiliary
```

Each account specifies *how* its hypothesis explains a result. The strength of an account depends on the reliability of its explanatory links and the plausibility of its auxiliaries. An account with implausible auxiliaries or weak links will receive less support from the evidence.

### Why Explanatory Links Are Nodes

In a simple graph, you might represent "Phenomenon explains Result" as a directed edge. But real explanatory reasoning is more complex:

1. **Auxiliaries attach to links**: An auxiliary assumption may be required for a specific inferential step—not for the nodes themselves. For example, "aerosol sampling techniques damage viral viability" is needed for the link connecting negative detection results to the aerosol hypothesis (see the [COVID-19 Case Study](covid19_case_study_overview.md) for details). Making links nodes allows auxiliaries to target them directly.

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

To explore this example, load "Simple Test" in the app and observe how the posterior probabilities shift when you condition on various nodes being set to true or false.

## How Bayesian Inference Fits In

The framework translates explanatory structure into a Bayesian network:

1. **Hypothesis priors** reflect an initial credence in each hypothesis
2. **Auxiliary priors** reflect an initial credence in each background assumption
3. **Link parameters** capture the strength of explanatory connections:
   - *Reliability*: How likely is the connection to hold when the source and all required auxiliaries are true? A reliability of 0.9 means that when the source node and its auxiliaries are all true, there's a 90% chance the explanatory connection succeeds. Lower reliability represents weaker or more uncertain explanatory relationships.
   - *Epsilon*: A small baseline probability that an explanatory link succeeds even when its conditions (source or auxiliaries) are not all met. Without epsilon, a link would be impossible whenever any condition fails. Epsilon is typically set very low (e.g., 0.01).
4. **Target parameters** capture how strongly results and phenomena respond to explanatory links:
   - *Strength*: How likely is the target to be true when its incoming explanatory link succeeds? A strength of 0.9 means a 90% chance the target is true when the link holds.
   - *Base rate*: A small baseline probability that a result or phenomenon is true even when no explanatory link succeeds. This allows results to occur for reasons outside the model. Base rate is typically set very low (e.g., 0.01).
5. **Evidence conditioning** treats observed results as TRUE by default. Because results represent what you've observed, inference asks: given these observations, what is the posterior probability for each hypothesis and other nodes? If you'd like, you can override the default behavior by setting result nodes to FALSE.

When you run inference, the framework computes posterior probabilities for each hypothesis and explanatory account. (If there is only one account per hypothesis, the hypothesis posterior will be identical to its account posterior.) Accounts that better explain the evidence corpus through more reliable links and more plausible auxiliaries will tend to receive higher posterior probability.

## Further Analyses ExplanatoryGraph Provides

### Value of Information Analysis
Which uncertain nodes would be most valuable to investigate? ExplanatoryGraph computes Expected Value of Perfect Information (EVPI) for each node—measuring how much the posterior probability of hypotheses would change, on average, if you learned that node's true state. Nodes with high EVPI are high-priority research targets: resolving their uncertainty would most affect your conclusions. You can analyze auxiliaries, phenomena, or links.

### Sensitivity Analysis
How much do conclusions depend on specific parameter choices? The framework sweeps parameters (like auxiliary priors or link reliabilities) across a range and plots how posterior probabilities change. This helps reveal which modeling assumptions are "load-bearing"—if small changes to a parameter dramatically shift conclusions, that assumption deserves scrutiny.


## Network Visualization

The app provides an interactive visualization of the explanatory graph. Nodes are colored by different features depending on the selected mode:

- **Structure**: Node types (hypotheses, phenomena, results, auxiliaries, links)
- **Baseline**: Probabilities after conditioning on all results as TRUE (the default evidence state)
- **Posterior**: Probabilities given the current evidence configuration in the Analysis tab
- **Delta**: Difference between two selected sets of marginals, useful for seeing how conditioning shifts probabilities
- **Log-ratio**: Log odds ratio showing strength and direction of evidential impact
- **Research Priority**: EVPI (Expected Value of Perfect Information) showing which nodes would most shift hypothesis posteriors if you learned their true state

Click on nodes to see details, and use the layout controls to arrange the graph.

## Next Steps

- **Tutorial**: Step-by-step guide to building a model — [tutorial.md](tutorial.md)
- **Case Study**: A worked example with COVID-19 transmission — [covid_case_study_overview.md](covid_case_study_overview.md)
- **Theoretical Background**: The dissertation chapter behind this framework — [Dissertation Chapter](https://blue-eclectus.github.io/dissertation-chapter-explanatory-accounts.pdf)
- **Try the Shiny App**: Load "Simple Test" or "COVID-19 Example" from the sidebar
