  # ============================================================================
  # Chargement des bibliothèques
  # ============================================================================
  library(dplyr)
  # library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  
  
  # ============================================================================
  # 1. Paramètres fixes
  # ============================================================================
  L <- 0
  U <- 5.7 
  mesh <- 100
  dz <- (U - L) / mesh
  z <- seq(L + dz/2, U - dz/2, length = mesh)
  Taille_reelle <- (10^z) - 1  
  nb_pop <- 6
  surface_max <- rep(1000000, nb_pop) # 100 m² par site
  genres <- c("acropora","porites","pocillopora")
  
  #temps de simulation
  annees = 50
  burn_in = 50
  
  auto_cor <- c(0.05,0.15,0.30)   
  flux_ext <- c(0.0001, 0.001, 0.01, 0.1)     
  
  # Parametres de recrutement 
  recru_params <- list(acropora = c(fec_base=200, prop_fixation=0.015,
                                    recr_zmean=0.6,recr_zsd=0.2,
				    taille_maturite = 7),
                       porites = c(fec_base=20, prop_fixation=0.03,
                                   recr_zmean=0.6, recr_zsd=0.2, 
				   taille_maturite = 2 * sqrt(4 / pi)), 
                       pocillopora = c(fec_base=100, prop_fixation=0.03,
                                       recr_zmean=0.6,recr_zsd=0.2, 
				       taille_maturite = 14))
  
  # Parametres de blanchissement
  freq_blanchissement <- c(0, 1/10, 1/5, 1/4, 1/3, 1/2, 1)
  param_surv_blanchissement <- list(
    acropora    = c(mean = 0.10, sd = 0.05),
    porites     = c(mean = 0.90, sd = 0.05),
    pocillopora = c(mean = 0.15, sd = 0.05)
  )
    
  #Intensitée de blanchissement progressive
  pas_int <- 50
  maxmin_moy_intensite <- list(acropora=c(0.10,0.03),
                               porites=c(0.90,0.3),
                               pocillopora=c(0.15,0.05))
  # Intensité progrssive en fonction du pas
  moy_intensite_prog <- list(acropora=seq(maxmin_moy_intensite[[1]][1],
                                          maxmin_moy_intensite[[1]][2],
                                          length.out = (annees / pas_int)),
                             porites=seq(maxmin_moy_intensite[[2]][1],
                                         maxmin_moy_intensite[[2]][2],
                                         length.out = (annees / pas_int)),
                             pocillopora=seq(maxmin_moy_intensite[[3]][1],
                                             maxmin_moy_intensite[[3]][2],
                                             length.out = (annees / pas_int)))
  # Vecteur de 50 ans avec répétition par palier dépendant du pas
  moyenne_int <- lapply(moy_intensite_prog, function(x) rep(x, each = pas_int))
  moyenne_int <- lapply(moyenne_int, function(x) x[1:annees])
  
  ecart_type_int <- 0.1
  
  
  #Fréquence de blanchissement progressive
  pas_freq <- 5
  maxmin_freq <- c(0.2, 0.5)
  # Fréquence progrssive en fonction du pas
  freq_prog <- seq(maxmin_freq[1], maxmin_freq[2], length.out = (annees / pas_freq))
  # Vecteur de 50 ans avec répétition par palier dépendant du pas
  freq_blanchissement_temporelle <- rep(freq_prog, each = pas_freq)[1:annees]
  
  
  prop_fec_params <- c(0.25, 0.25, 0.50, 0.75)
  prop_growth_params <- c(0.1, 0.25, 0.50, 0.75)
  
  
  
  
  
  
  # ============================================================================
  # 2. création des différents scénarios
  # ============================================================================
 scenarios <- list()
  
 # Scénario 1a.c1 :
 scenario <- "Scenario 1a.c1"
 ## Populations :
 nb_type1 <- 6
 nb_type2 <- 0
 resultats <- param_scenarios(nb_pop = nb_pop, 
                              nb_type1 = nb_type1,
                              nb_type2 = nb_type2)
 
# Matrice de connectivité 
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1a.c1 = list(populations = resultats$populations,
                                               surv_params = resultats$surv_params,
                                               grow_params = resultats$grow_params,
                                               matrice = c_matrix
                                               )))
  
  






# Scénario 2a.c1 :
scenario <- "Scenario 2a.c1"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2a.c1 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))







# Scénario 3a.c1 :
## Populations :
scenario <- "Scenario 3a.c1"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3a.c1 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))








# Scénario 4a.c1 :
scenario <- "Scenario 4a.c1"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4a.c1 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))









# Scénario 1b.c1 :
scenario <- "Scenario 1b.c1"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1b.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2b.c1 :
scenario <- "Scenario 2b.c1"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2b.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3b.c1 :
## Populations :
scenario <- "Scenario 3b.c1"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3b.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4b.c1 :
scenario <- "Scenario 4b.c1"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4b.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1c.c1 :
scenario <- "Scenario 1c.c1"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1c.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2c.c1 :
scenario <- "Scenario 2c.c1"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2c.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3c.c1 :
## Populations :
scenario <- "Scenario 3c.c1"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3c.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4c.c1 :
scenario <- "Scenario 4c.c1"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4c.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1d.c1 :
scenario <- "Scenario 1d.c1"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1d.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2d.c1 :
scenario <- "Scenario 2d.c1"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2d.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3d.c1 :
## Populations :
scenario <- "Scenario 3d.c1"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3d.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4d.c1 :
scenario <- "Scenario 4d.c1"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4d.c1 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))











































# Scénario 1a.c2 :
 scenario <- "Scenario 1a.c2"
 ## Populations :
 nb_type1 <- 6
 nb_type2 <- 0
 resultats <- param_scenarios(nb_pop = nb_pop, 
                              nb_type1 = nb_type1,
                              nb_type2 = nb_type2)
 
# Matrice de connectivité 
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1a.c2 = list(populations = resultats$populations,
                                               surv_params = resultats$surv_params,
                                               grow_params = resultats$grow_params,
                                               matrice = c_matrix
                                               )))
  
  






# Scénario 2a.c2 :
scenario <- "Scenario 2a.c2"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2a.c2 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))







# Scénario 3a.c2 :
## Populations :
scenario <- "Scenario 3a.c2"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3a.c2 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))








# Scénario 4a.c2 :
scenario <- "Scenario 4a.c2"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4a.c2 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))









# Scénario 1b.c2 :
scenario <- "Scenario 1b.c2"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1b.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2b.c2 :
scenario <- "Scenario 2b.c2"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2b.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3b.c2 :
## Populations :
scenario <- "Scenario 3b.c2"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3b.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4b.c2 :
scenario <- "Scenario 4b.c2"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4b.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1c.c2 :
scenario <- "Scenario 1c.c2"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1c.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2c.c2 :
scenario <- "Scenario 2c.c2"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2c.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3c.c2 :
## Populations :
scenario <- "Scenario 3c.c2"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3c.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4c.c2 :
scenario <- "Scenario 4c.c2"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4c.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1d.c2 :
scenario <- "Scenario 1d.c2"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1d.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2d.c2 :
scenario <- "Scenario 2d.c2"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2d.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3d.c2 :
## Populations :
scenario <- "Scenario 3d.c2"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3d.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4d.c2 :
scenario <- "Scenario 4d.c2"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[2],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[2],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[2],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[2],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4d.c2 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))











































# Scénario 1a.c3 :
 scenario <- "Scenario 1a.c3"
 ## Populations :
 nb_type1 <- 6
 nb_type2 <- 0
 resultats <- param_scenarios(nb_pop = nb_pop, 
                              nb_type1 = nb_type1,
                              nb_type2 = nb_type2)
 
# Matrice de connectivité 
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1a.c3 = list(populations = resultats$populations,
                                               surv_params = resultats$surv_params,
                                               grow_params = resultats$grow_params,
                                               matrice = c_matrix
                                               )))
  
  






# Scénario 2a.c3 :
scenario <- "Scenario 2a.c3"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2a.c3 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))







# Scénario 3a.c3 :
## Populations :
scenario <- "Scenario 3a.c3"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3a.c3 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))








# Scénario 4a.c3 :
scenario <- "Scenario 4a.c3"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4a.c3 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))









# Scénario 1b.c3 :
scenario <- "Scenario 1b.c3"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1b.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2b.c3 :
scenario <- "Scenario 2b.c3"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2b.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3b.c3 :
## Populations :
scenario <- "Scenario 3b.c3"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3b.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4b.c3 :
scenario <- "Scenario 4b.c3"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4b.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1c.c3 :
scenario <- "Scenario 1c.c3"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1c.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2c.c3 :
scenario <- "Scenario 2c.c3"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2c.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3c.c1 :
## Populations :
scenario <- "Scenario 3c.c1"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3c.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4c.c3 :
scenario <- "Scenario 4c.c3"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4c.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1d.c1 :
scenario <- "Scenario 1d.c1"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1d.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2d.c3 :
scenario <- "Scenario 2d.c3"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[3],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[3],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[3],         flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2d.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3d.c3 :
## Populations :
scenario <- "Scenario 3d.c3"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3d.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4d.c3 :
scenario <- "Scenario 4d.c3"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[3],       flux_ext[3],       flux_ext[1],       flux_ext[3],       flux_ext[1],      # Source: Pop1 
  flux_ext[3],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[3],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[3],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4d.c3 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))











































# Scénario 1a.c4 :
 scenario <- "Scenario 1a.c4"
 ## Populations :
 nb_type1 <- 6
 nb_type2 <- 0
 resultats <- param_scenarios(nb_pop = nb_pop, 
                              nb_type1 = nb_type1,
                              nb_type2 = nb_type2)
 
# Matrice de connectivité 
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1a.c4 = list(populations = resultats$populations,
                                               surv_params = resultats$surv_params,
                                               grow_params = resultats$grow_params,
                                               matrice = c_matrix
                                               )))
  
  






# Scénario 2a.c4 :
scenario <- "Scenario 2a.c4"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)


diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2a.c4 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))







# Scénario 3a.c4 :
## Populations :
scenario <- "Scenario 3a.c4"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3a.c4 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))








# Scénario 4a.c4 :
scenario <- "Scenario 4a.c4"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4a.c4 = list(populations = resultats$populations,
                                             surv_params = resultats$surv_params,
                                             grow_params = resultats$grow_params,
                                             matrice = c_matrix
)))









# Scénario 1b.c4 :
scenario <- "Scenario 1b.c4"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1b.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2b.c4 :
scenario <- "Scenario 2b.c4"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[3]
diag(c_matrix)[6]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2b.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3b.c4 :
## Populations :
scenario <- "Scenario 3b.c4"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3b.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4b.c4 :
scenario <- "Scenario 4b.c4"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[3]
diag(c_matrix)[1]   <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4b.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1c.c4 :
scenario <- "Scenario 1c.c4"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario1c.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2c.c4 :
scenario <- "Scenario 2c.c4"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[1:5] <- auto_cor[1]
diag(c_matrix)[6]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario2c.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3c.c4 :
## Populations :
scenario <- "Scenario 3c.c4"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario3c.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4c.c4 :
scenario <- "Scenario 4c.c4"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix)[2:6] <- auto_cor[1]
diag(c_matrix)[1]   <- auto_cor[3]

scenarios <- c(scenarios,list(scenario4c.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))









# Scénario 1d.c4 :
scenario <- "Scenario 1d.c4"
## Populations :
nb_type1 <- 6
nb_type2 <- 0

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario1d.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 2d.c4 :
scenario <- "Scenario 2d.c4"
## Populations :
nb_type1 <- 5
nb_type2 <- 1

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
 # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop1 
  flux_ext[1],         auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],       flux_ext[4],      # Source: Pop2 
  flux_ext[1],         flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[4],      # Source: Pop4 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],      # Source: Pop5 
  flux_ext[4],         flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario2d.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))







# Scénario 3d.c4 :
## Populations :
scenario <- "Scenario 3d.c4"
nb_type1 <- 1
nb_type2 <- 5

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario3d.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))








# Scénario 4d.c4 :
scenario <- "Scenario 4d.c4"
## Populations :
nb_type1 <- 0
nb_type2 <- 6

resultats <- param_scenarios(nb_pop = nb_pop, 
                             nb_type1 = nb_type1,
                             nb_type2 = nb_type2)

# Matrice de connectivité
c_matrix <- matrix(c(
  # Cible: Pop1          Pop2               Pop3               Pop4               Pop5               Pop6
  auto_cor[1],         flux_ext[4],       flux_ext[4],       flux_ext[1],       flux_ext[4],       flux_ext[1],      # Source: Pop1 
  flux_ext[4],         auto_cor[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[1],      # Source: Pop2 
  flux_ext[4],         flux_ext[1],       auto_cor[1],       flux_ext[3],       flux_ext[1],       flux_ext[1],      # Source: Pop3 
  flux_ext[1],         flux_ext[1],       flux_ext[3],       auto_cor[1],       flux_ext[1],       flux_ext[1],      # Source: Pop4 
  flux_ext[4],         flux_ext[1],       flux_ext[1],       flux_ext[1],       auto_cor[1],       flux_ext[3],      # Source: Pop5 
  flux_ext[1],         flux_ext[1],       flux_ext[1],       flux_ext[1],       flux_ext[3],       auto_cor[1]       # Source: Pop6 
), nrow = 6, ncol = 6, byrow = TRUE)

diag(c_matrix) <- auto_cor[1]

scenarios <- c(scenarios,list(scenario4d.c4 = list(populations = resultats$populations,
                                                surv_params = resultats$surv_params,
                                                grow_params = resultats$grow_params,
                                                matrice = c_matrix
)))
