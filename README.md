# Coral Metacommunity IPM Model

> An Integral Projection Model (IPM) for coral metacommunities simulating coral community dynamics under varying bleaching frequencies.

[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

## 📖 About
This repository contains the source code for the scientific article entitled:  
> *[Title of the upcoming article]*  
> *[Nicolas Den Dryver]*

The model implements an **Integral Projection Model (IPM)** to simulate the spatiotemporal dynamics of a coral metacommunity (*Acropora*, *Porites*, *Pocillopora* genera) under varying disturbance frequencies (constant or progressive coral bleaching), integrating spatial connectivity and competition.

---

## 📂 Repository Structure

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
