# Coral Metacommunity IPM Model

> An Integral Projection Model (IPM) for coral metacommunities simulating coral community dynamics under varying bleaching frequencies.

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

## About
This repository contains the source code for the scientific article entitled:  
> *[Title of the upcoming article]*  
> *[Nicolas Den Dryver]*

The model implements an **Integral Projection Model (IPM)** to simulate the spatiotemporal dynamics of a coral metacommunity (*Acropora*, *Porites*, *Pocillopora* genera) under varying disturbance frequencies (constant or progressive coral bleaching), integrating spatial connectivity and competition.

---

## Repository Structure

The project is organized as follows:

```text
├── src/
│   ├── 1_Fonctions_modele.R        # Fundamental functions (survival, growth, recruitment, projections)
│   ├── 2_Param_Scenarios.R         # Scenario parameterization and population initialization
│   ├── 2_Param_Scenarios_bis.R     # Scenario parameterization with a different connectivity structure
│   ├── 3_Simulations_x100.R        # Execution of serial stochastic simulations (x100 simulations)
│   └── 4_graphiques.R              # Generation of plots (colorblind-friendly) and mixed models (lmer)
├── Resultats_Globaux/              # Storage folder for global data tables
├── Resultats_Par_Frequence/        # Graphical outputs and statistics by disturbance regime
├── Licence                         # GNU Affero General Public License v3.0
└── README.md

```

## Prerequisites and Installation 

The project relies on R (v4.0 or higher). Required R packages can be installed using the following command:

```R
install.packages(c("dplyr", "tidyr", "stringr", "ggplot2", "patchwork", "cowplot", "viridis", "truncnorm",
  "lmerTest", "emmeans", "performance"))
```

> **Note on Parallelization:** `3_Simulations_x100.R` leverages `parallel::mclapply` for multi-core computation (defaulted to 16 cores). If running on Windows, adapt this script to use socket-based parallel processing (e.g., `parallel::makeCluster`).

---

## Usage Guide

To reproduce the study's simulations and figures, execute the scripts as follow:

```R
# 1. Load mathematical core functions
source("1_Fonctions_modele.R")

# 2. Initialize scenarios and environmental parameters
source("2_Param_Scenarios.R")

# 3. Run parallel stochastic simulations (100 iterations per scenario)
source("3_Simulations_x100.R")

# 4. Perform statistical analyses and generate paper figures
source("4_graphiques.R")
```

---

## Outputs and Data Generated

Execution of the pipeline will genrate two main directories:
1. **`Resultats_Globaux/`**
   * `Tableau_Global_Tous_Scenarios.csv`: Dataset containing time series across all stochastic runs and scenario grids.
2. **`Resultats_Par_Frequence/`** (Organized into subfolders per disturbance frequency):
   * **Figures 1A & 1B:** Temporal dynamics of metacommunity spatial coverage and total abundance.
   * **Figure 2:** Genus-specific relative temporal dynamics of spatial coverage (*Acropora*, *Porites*, *Pocillopora*).
   * **Figures 3–5:** Inter-run variance, spatial dispersion, and connectivity impacts.
   * `Graphique_Tendances_Seul.png`: Estimated marginal trend slopes derived from linear mixed-effects modeling.

---

## Citation

If you use this model or code in your research, please cite the associated article:

```bibtex
@article{coral_demographic_model_2026,
  title={Modelling spatial metapopulation dynamics of coral communities under thermal stress regimes},
  author={[Nicolas Den Dryver]},
  journal={[Journal Name]},
  year={2026},
  url={[DOI/URL]}
}
```
