# COVID-19 Case Study Overview

This case study applies the Explanatory Accounts Framework to the scientific debate over COVID-19 transmission mechanisms during the early stages of the pandemic (2020-2021). During this period, health agencies like the WHO and CDC had to assess a methodologically diverse body of evidence in which studies sometimes appeared to support contradictory conclusions. Standard amalgamation techniques struggled with this evidence base, partly because the evidence was highly indirect—individual results did not directly test transmission hypotheses but had to be related to them through long inferential chains, which at times relied on highly uncertain auxiliary assumptions.

The case study models two competing explanatory accounts—aerosol transmission versus droplet-and-contact transmission—each attempting to explain the evidence base. Exploring the accounts reveals the different explanatory strategies each employs and the assumptions each requires. In the Shiny app, you can interact with the underlying Bayesian network by setting claims to true or false—for example, learning that a key auxiliary assumption is implausible—and observing how this propagates through the network to affect the marginal probabilities of each hypothesis.

For a detailed discussion of this case, see [section 3.1 of my dissertation chapter](https://blue-eclectus.github.io/dissertation-chapter-explanatory-accounts.pdf).

## The Scientific Question

Two broad hypotheses competed to explain how SARS-CoV-2 spreads:

| | H1: Aerosol | H2: Droplet-and-contact |
|---|-------------|-------------------------|
| **Mechanism** | Small particles (<5μm) remain suspended in air | Large droplets (>5μm) fall quickly to surfaces |
| **Major Implications** | Transmission beyond 2m common; ventilation has major impact on spread | Proximity (<2m) and surface contact are main risks; ventilation has minimal effect |

## The Evidence

In adjudicating between these two hypotheses, four categories of empirical results were particularly relevant:

**R1: Outbreak Patterns**
Indoor/outdoor differences, ventilation effects, and occasional long-range transmission events (e.g., restaurant outbreaks, choir superspreading events).

**R2: Low to Moderate R₀**
The basic reproduction number was estimated at 2-3, lower than highly contagious airborne diseases like measles.

**R3: Mask Efficacy**
Surgical and cotton masks showed moderate effectiveness, despite not being designed for aerosol filtration.

**R4: Failure to Detect Viable Virus in Aerosols**
Air sampling studies frequently failed to culture live virus from air samples, despite detecting viral RNA.

## The Aerosol Account

The aerosol account (Acc1) sought to explain the evidence through five major phenomena claims:

| Phenomenon | Claim |
|------------|-------|
| PC1 | SARS-CoV-2 transmits efficiently via aerosols at close range (≲2m) |
| PC2 | Ventilation strongly affects transmission risk |
| PC3 | SARS-CoV-2 generally transmits less efficiently at longer ranges (≳2m) |
| PC4 | Masks block some aerosols |
| PC5 | Respiratory aerosols contain significant levels of viable virus |

### Explanatory Paths

```
Acc1 ──→ PC1 ──┐
               ├──→ R1 (Outbreak patterns)
Acc1 ──→ PC2 ──┘

Acc1 ──→ PC3 ──→ R2 (Low R₀)
      ↑
      A1

Acc1 ──→ PC4 ──→ R3 (Mask efficacy)

Acc1 ──→ PC5 ──→ R4 (Detection failures)
              ↑
              A2

A1: Aerosols typically remain below infectious dose at longer ranges
A2: Aerosol sampling techniques damage viral viability
```

The aerosol account explains R1 through a conjunction of close-range efficiency (PC1) and ventilation effects (PC2), and explains R3 straightforwardly via PC4. The other two results require auxiliary assumptions. To explain R2 (the relatively low R₀), the account invokes A1—that aerosol concentrations typically remain below infectious dose at longer ranges, limiting transmission efficiency. To explain R4 (why studies failed to culture viable virus from air samples), the account invokes A2—that aerosol sampling techniques damage viral viability, causing false negatives.

## The Droplet-and-Contact Account

The droplet-and-contact account (Acc2) sought to explain the evidence through these major phenomena claims:

| Phenomenon | Claim |
|------------|-------|
| PC6 | SARS-CoV-2 transmission is limited to close proximity (≲2m) |
| PC7 | Ventilation has minimal effect on transmission |
| PC8 | Droplet-and-contact transmission is not highly efficient |
| PC9 | Masks effectively block droplet particles |
| PC10 | Respiratory aerosols contain insignificant levels of viable virus |

### Explanatory Paths

```
Acc2 ──→ PC6 ──┐
               ├──→ R1 (Outbreak patterns)
Acc2 ──→ PC7 ──┘
               ↑
               A3

Acc2 ──→ PC8 ──→ R2 (Low R₀)

Acc2 ──→ PC9 ──→ R3 (Mask efficacy)

Acc2 ──→ PC10 ──→ R4 (Detection failures)

A3: Apparent long-range transmission involved unobserved close contact
```

The droplet-and-contact account explains R2, R3, and R4 fairly straightforwardly. However, the account struggles with R1: observed outbreak patterns (ventilation effects, long-range transmission) contradict predictions from PC6 and PC7. To maintain this explanatory link, the account must invoke A3—that apparent long-range cases actually involved unobserved close contact. Supporters of the droplet-and-contact account, including the WHO, asserted this as the likely explanation.

## Using This Case Study

### In the Shiny App

**Loading the Case Study**
- The COVID-19 case study can be loaded from the Case Studies menu

**Exploring the Network**
- Use the Visualization tab to see all nodes and explanatory links
- Hover over nodes to see details; click to select
- Switch between color modes to see different aspects:
  - **Prior**: Probabilities before observing evidence
  - **Posterior**: Probabilities after conditioning on results
  - **Delta**: Change from prior to posterior
  - **Log-ratio**: Strength of evidential impact

**Setting Evidence**
- Set results (R1–R4) to TRUE to condition on observed evidence
- Set auxiliary assumptions to TRUE or FALSE to explore counterfactuals
- For example: What happens to H1 and H2 if we learn A3 is false (i.e., there was no unobserved close contact)?

**Value of Information Analysis**
- Use the VOI tab to identify which uncertain nodes would be most valuable to investigate
- This ranks auxiliaries and phenomena by how much learning their true value would affect the posterior probabilities of H1 and H2

**Sensitivity Analysis**
- Use the Sensitivity tab to sweep parameters across ranges
- See which assumptions most affect the conclusions

## References

### Cited

- Bhagat, R.K., Davies Wykes, M.S., Dalziel, S.B., & Linden, P.F. (2020). Effects of ventilation on the indoor spread of COVID-19. *Journal of Fluid Mechanics*, 903, F1.
- Coleman, K.K., & Frieman, M.B. (2021). Coronaviruses: Important emerging human pathogens. *Journal of Virology*.
- Lednicky, J.A., et al. (2020). Viable SARS-CoV-2 in the air of a hospital room with COVID-19 patients. *International Journal of Infectious Diseases*, 100, 476–482.
- Lu, J., et al. (2020). COVID-19 outbreak associated with air conditioning in restaurant, Guangzhou, China. *Emerging Infectious Diseases*, 26(7), 1628–1631.
- Miller, S.L., et al. (2021). Transmission of SARS-CoV-2 by inhalation of respiratory aerosol in the Skagit Valley Chorale superspreading event. *Indoor Air*, 31, 314–323.
- Pan, M., Lednicky, J.A., & Wu, C.-Y. (2019). Collection, particle sizing and detection of airborne viruses. *Journal of Applied Microbiology*, 127(6), 1596–1611.
- Park, S., et al. (2021). Natural ventilation strategy and related issues to prevent coronavirus disease 2019 (COVID-19) airborne transmission in a school building. *Science of the Total Environment*, 789, 147764.
- Qian, H., et al. (2021). Indoor transmission of SARS-CoV-2. *Indoor Air*, 31(3), 639–645.
- Santarpia, J.L., et al. (2020). Aerosol and surface contamination of SARS-CoV-2 observed in quarantine and isolation care. *Scientific Reports*, 10(1), 12732.
- Wang, C.C., Prather, K.A., Sznitman, J., Jimenez, J.L., Lakdawala, S.S., Tufekci, Z., & Marr, L.C. (2021). Airborne transmission of respiratory viruses. *Science*, 373(6558), eabd9149.

### Further Reading

- Conly, J., Seto, W.H., Pittet, D., Holmes, A., Chu, M., Hunter, P.R., et al. (2020). Use of medical face masks versus particulate respirators as a component of personal protective equipment for health care workers in the context of the COVID-19 pandemic. *Antimicrobial Resistance & Infection Control*, 9(1), 126.
- Greenhalgh, T., Jimenez, J.L., Prather, K.A., Tufekci, Z., Fisman, D., & Schooley, R. (2021). Ten scientific reasons in support of airborne transmission of SARS-CoV-2. *The Lancet*, 397(10285), 1603–1605.
- Jiang, G., Wang, C., Song, L., Wang, X., Zhou, Y., Fei, C., & Liu, H. (2021). Aerosol transmission, an indispensable route of COVID-19 spread: Case study of a department-store cluster. *Frontiers of Environmental Science & Engineering*, 15(3), 46.
- Jimenez, J.L., Marr, L.C., Randall, K., Ewing, E.T., Tufekci, Z., Greenhalgh, T., Tellier, R., et al. (2022). What were the historical reasons for the resistance to recognizing airborne transmission during the COVID-19 pandemic? *Indoor Air*, 32(8), e13070.
- Morawska, L., & Milton, D.K. (2020). It is time to address airborne transmission of coronavirus disease 2019 (COVID-19). *Clinical Infectious Diseases*, 71(9), 2311–2313.
- Shen, Y., Li, C., Dong, H., Wang, Z., Martinez, L., Sun, Z., Handel, A., et al. (2020). Community outbreak investigation of SARS-CoV-2 transmission among bus riders in eastern China. *JAMA Internal Medicine*, 180(12), 1665–1671.
- Tellier, R. (2022). COVID-19: The case for aerosol transmission. *Interface Focus*, 12(2), 20210072.
