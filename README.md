# Volatility and Systemic Risk in the European Insurance Sector

This repository contains the research code for a modular pipeline studying **volatility spillovers, connectedness, and systemic risk** using network-based methods in financial markets.

The project focuses on quantifying **dynamic interdependencies across assets** and extracting **systemic risk signals** using econometric and graph-theoretic tools.

---

## Research Motivation

Financial markets exhibit strong cross-sectional dependence, especially during periods of stress.  
Traditional univariate or pairwise approaches often fail to capture the **network structure of risk transmission**.

This project aims to:
- model **time-varying volatility spillovers**
- represent spillovers as **directed weighted networks**
- extract **systemic risk indicators** from network topology
- study how these properties evolve across **market regimes**

---

## Methodological Overview

The pipeline combines econometric modeling with network analysis:

- Volatility modeling and preprocessing
- VAR-based spillover measures (Diebold–Yilmaz framework)
- Generalized Forecast Error Variance Decomposition (GFEVD)
- Rolling-window estimation for time variation
- Regime detection using Markov switching models
- Network construction and graph-based systemic risk metrics
- Tail risk measures (MES, CoVaR-style extensions)

The design emphasizes **modularity, reproducibility, and interpretability** rather than black-box prediction.

---

## Project Structure

The repository is organized as a modular research pipeline:

```text
.
├── README.md
├── run_pipeline.R
├── .gitignore
│
├── R/
│   ├── 01_data.R
│   ├── 02_features.R
│   ├── 03_connectedness.R
│   ├── 04_gfevd.R
│   ├── 05_rolling.R
│   ├── 07_regime_markov.R
│   ├── 08_network_metrics.R
│   ├── 09_kirchhoff_systematic.R
│   └── 10_tail_risk_mes_covar.R
│
├── data/
│   └── README.md
│
├── output/
    ├── figures/
    └── results/
```
Each module is self-contained and designed to be reusable across related projects.

---

## Key Outputs

- Time-varying connectedness indices
- Directed spillover networks
- Regime-dependent systemic risk measures
- Network statistics capturing concentration and fragility
- Tail risk indicators linked to network centrality

---

## Reproducibility

- Implemented in **R**
- Modular design enables partial or full execution
- No proprietary data included in this repository  
  (data access instructions or synthetic examples can be added if needed)

---

## Empirical Insights

The analysis highlights several recurring empirical patterns commonly observed in financial networks:

- Volatility spillovers intensify during periods of market stress, leading to higher system-wide connectedness.
- Network structures become more concentrated in turbulent regimes, with a small number of assets acting as dominant transmitters of risk.
- Regime-switching dynamics capture shifts between stable and stressed market states, reflected in both connectedness measures and network topology.
- Tail risk measures are closely linked to network centrality, suggesting that highly connected nodes play a key role in systemic risk transmission.

These findings are consistent with the broader literature on financial connectedness and systemic risk.

---

## Relevance to Quantitative Finance

While the project is methodological in nature, its primary motivation and applications lie in quantitative finance.

The framework is particularly relevant for:
- systemic risk analysis and financial stability monitoring
- volatility spillover modeling across asset classes
- stress-period dependency and contagion analysis
- regime-aware risk management and portfolio diagnostics

The methods implemented in this repository are designed to complement traditional quantitative finance tools by providing a network-based perspective on risk transmission.

---

## Intended Audience

This repository is intended for:
- Quantitative researchers
- Risk modelers
- Econometrics and financial networks researchers
- Hiring managers evaluating applied research capability

---

## Notes

This repository reflects an **active research project**.  
Code is shared for transparency and evaluation of methodology rather than as a polished software package.

---

## Contact

If you have questions about the methodology or structure, feel free to reach out.
