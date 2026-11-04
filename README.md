# Coral Metacommunity IPM Model

> An Integral Projection Model (IPM) for coral metacommunities simulating population dynamics under varying bleaching frequencies.

[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)

## 📖 À propos / About
Ce dépôt contient le code source de l'article scientifique intitulé :  
> *[Titre de l'article à venir]*  
> *[Nciolas Den Dryver]*

Le modèle implémente un **Modèle de Projection Intégral (IPM)** pour simuler la dynamique spatio-temporelle d'une métacommunauté de coraux (genres *Acropora*, *Porites*, *Pocillopora*) soumise à différentes fréquences de perturbations (blanchiments coralliens constants ou progressifs), en intégrant la connectivité spatiale et la compétition.

---

## 📂 Structure du dépôt (Repository Structure)

Le projet est organisé de la manière suivante :

```text
├── src/
│   ├── 1_Fonctions_modele.R        # Fonctions fondamentales (survie, croissance, recrutement, projections)
│   ├── 2_Param_Scenarios.R         # Paramétrage des scénarios et initialisation des populations
│   ├── 2_Param_Scenarios_bis.R     # Variante de paramétrage des scénarios
│   ├── 3_Simulations_x100.R        # Exécution des simulations stochastiques en série (x100)
│   └── 4_graphiques.R              # Génération des graphiques (adaptés aux daltoniens) et modèles mixtes (lmer)
├── Resultats_Globaux/              # Dossier de stockage des tableaux de données globaux
├── Resultats_Par_Frequence/        # Sorties graphiques et statistiques par régime de perturbation
├── Licence ...
└── README.md
