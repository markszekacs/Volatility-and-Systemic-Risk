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

## Why the Insurance Sector and Long-Horizon Perspective?

The insurance sector offers a uniquely informative setting for studying volatility spillovers and systemic risk, yet remains relatively underexplored compared to banking or asset management.

Unlike short-horizon trading institutions, insurance firms are long-term risk holders. Their balance sheets are exposed not only to contemporaneous market fluctuations, but also to persistent volatility dynamics, regime shifts, and tail events unfolding over extended horizons. As a result, short-term dependence measures often fail to capture the structural risk transmission mechanisms relevant for insurers.

From an empirical perspective, the insurance sector exhibits several characteristics that motivate a network-based, long-horizon approach:
- strong cross-sectional dependence driven by common exposures and reinsurance links
- sensitivity to prolonged volatility regimes rather than isolated shocks
- asymmetric risk transmission during stress periods
- heightened relevance of tail risk and systemic concentration

Despite these features, much of the existing literature focuses on banks or high-frequency market interactions. Applications of connectedness and network methods to the insurance sector, particularly in a dynamic and regime-aware framework, remain comparatively limited.

This project addresses this gap by combining established econometric spillover measures with network-theoretic tools, explicitly designed for long-horizon analysis. The methodology emphasizes interpretability and structural insights rather than short-term prediction, aligning closely with the decision-making needs of insurance risk management and financial stability analysis.

The empirical patterns uncovered by the framework are consistent with stylized facts documented in the broader systemic risk literature, providing validation that the approach captures meaningful economic mechanisms rather than artifact-driven relationships.

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
