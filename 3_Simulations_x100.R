# ============================================================================
# EXÉCUTION, VISUALISATION ET SAUVEGARDE DES SCÉNARIOS 
# ============================================================================

setwd("/mnt/fast2To/pourNicolasDD")

library(parallel)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)

# Désactivation du multithreading implicite pour respecter le quota de 16 cœurs
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OPENBLAS_NUM_THREADS = 1)
Sys.setenv(MKL_NUM_THREADS = 1)
Sys.setenv(BLAS_NUM_THREADS = 1)

print("Lancement des codes sources 1 et 2")
source("Code/1_Fonctions_modele.R")
source("Code/2_Param_Scenarios.R")

# Paramètres globaux pour l'extraction
noms_frequences <- c("No disturbance", 
                     "Once every 10 years", 
                     "Once every 5 years",
                     "Once every 4 years",
                     "Once every 3 years",
                     "Once every 2 years", 
                     "Once a year")

nom_progressive <- paste("Progressive from", maxmin_freq[1], "to", maxmin_freq[2])
toutes_frequences <- c(noms_frequences, nom_progressive)

surface_totale_max <- sum(surface_max) 

# ============================================================================
# 1. LANCEMENT DE TOUTES LES SIMULATIONS POUR TOUS LES SCÉNARIOS
# ============================================================================
nb_simu <- 100
nb_cores <- 16 
tous_les_resultats <- list()

for (scenar in 1:length(scenarios)) {
  
  nom_scenario_actuel <- names(scenarios)[scenar]
  print(paste("=================================================="))
  print(paste("SIMULATION DU SCÉNARIO :", nom_scenario_actuel))
  print(paste("=================================================="))

  res_parallele <- mclapply(1:nb_simu, function(simu) {
    print(paste("Lancement de le simu", simu))

    # 1. Simulation classique
    res_classique <- executer_projection_temporelle(
      annees = annees, burn_in = burn_in,
      freq_blanchissement = freq_blanchissement,
      populations = scenarios[[scenar]][["populations"]],
      surv_params = scenarios[[scenar]][["surv_params"]],
      grow_params = scenarios[[scenar]][["grow_params"]],
      recru_params = recru_params, surface_max = surface_max,
      Taille_reelle = Taille_reelle, z = z, mesh = mesh, L = L, U = U, dz = dz,
      c_matrix = scenarios[[scenar]][["matrice"]],
      param_surv_blanchissement = param_surv_blanchissement,
      prop_fec_params = prop_fec_params, prop_growth_params = prop_growth_params,
      genres = genres, seed = simu
    )
    
    # 2. Simulation progressive
    res_prog <- executer_projection_temporelle_progressif(
      annees = annees, burn_in = burn_in,
      freq_blanchissement_vec = freq_blanchissement_temporelle, 
      moyenne_int = moyenne_int,
      populations = scenarios[[scenar]][["populations"]],
      surv_params = scenarios[[scenar]][["surv_params"]],
      grow_params = scenarios[[scenar]][["grow_params"]],
      recru_params = recru_params, surface_max = surface_max,
      Taille_reelle = Taille_reelle, z = z, mesh = mesh, L = L, U = U, dz = dz,
      c_matrix = scenarios[[scenar]][["matrice"]],
      param_surv_blanchissement = param_surv_blanchissement,
      prop_fec_params = prop_fec_params, prop_growth_params = prop_growth_params,
      genres = genres, seed = simu
    )
    
    return(list(classique = res_classique, progressive = res_prog))
    
  }, mc.cores = nb_cores, mc.preschedule = FALSE)
  
  resultats_simulation <- lapply(res_parallele, function(x) x$classique)
  resultats_simulation_progressive <- lapply(res_parallele, function(x) x$progressive)
  
  print("Extraction des résultats...")

  # --- EXTRACTION CLASSIQUE ---
  total_rows <- nb_simu * length(freq_blanchissement) * annees
  res_list <- vector("list", total_rows)
  counter <- 1
  
  for (simu in 1:nb_simu) {
    traj_simu <- resultats_simulation[[simu]]$traj_blanchissment
    episodes_simu <- resultats_simulation[[simu]]$episode_blanchissement
    
    for (freq_b in seq_along(freq_blanchissement)) {
      freq_name <- noms_frequences[freq_b]
      
      for (t in 1:annees) {
        abondance_totale <- 0
        surface_totale <- 0
        surface_genre <- c(acropora = 0, porites = 0, pocillopora = 0)
        
        for (i in 1:nb_pop) {
          for (g in seq_along(genres)) {
            genre_name <- genres[g]
            pop_vector <- traj_simu[[freq_b]][[i]][[g]][t, ]
            surf_pop <- sum(pop_vector * Taille_reelle)
            
            abondance_totale <- abondance_totale + sum(pop_vector)
            surface_totale <- surface_totale + surf_pop
            surface_genre[genre_name] <- surface_genre[genre_name] + surf_pop
          }
        }
        
        res_list[[counter]] <- data.frame(
          Scenario = nom_scenario_actuel,
          Simulation = simu,
          Annee = t,
          Frequence = freq_name,
          Abondance_Meta = unname(abondance_totale),
          Surface_Meta_pct = unname((surface_totale / surface_totale_max) * 100),
          acropora = unname((surface_genre["acropora"] / surface_totale_max) * 100),
          porites = unname((surface_genre["porites"] / surface_totale_max) * 100),
          pocillopora = unname((surface_genre["pocillopora"] / surface_totale_max) * 100)
        )
        counter <- counter + 1
      }
    }
  }
  df_classique <- bind_rows(res_list)
  
  # --- EXTRACTION PROGRESSIVE ---
  total_rows_prog <- nb_simu * annees
  res_list_prog <- vector("list", total_rows_prog)
  counter_prog <- 1
  
  for (simu in 1:nb_simu) {
    traj_simu_prog <- resultats_simulation_progressive[[simu]]$traj_blanchissment
    
    for (t in 1:annees) {
      abondance_totale <- 0
      surface_totale <- 0
      surface_genre <- c(acropora = 0, porites = 0, pocillopora = 0)
      
      for (i in 1:nb_pop) {
        for (g in seq_along(genres)) {
          genre_name <- genres[g]
          pop_vector <- traj_simu_prog[[i]][[g]][t, ]
          surf_pop <- sum(pop_vector * Taille_reelle)
          
          abondance_totale <- abondance_totale + sum(pop_vector)
          surface_totale <- surface_totale + surf_pop
          surface_genre[genre_name] <- surface_genre[genre_name] + surf_pop
        }
      }
      
      res_list_prog[[counter_prog]] <- data.frame(
        Scenario = nom_scenario_actuel,
        Simulation = simu,
        Annee = t,
        Frequence = nom_progressive,
        Abondance_Meta = unname(abondance_totale),
        Surface_Meta_pct = unname((surface_totale / surface_totale_max) * 100),
        acropora = unname((surface_genre["acropora"] / surface_totale_max) * 100),
        porites = unname((surface_genre["porites"] / surface_totale_max) * 100),
        pocillopora = unname((surface_genre["pocillopora"] / surface_totale_max) * 100)
      )
      counter_prog <- counter_prog + 1
    }
  }
  df_prog <- bind_rows(res_list_prog)
  
  tous_les_resultats[[scenar]] <- bind_rows(df_classique, df_prog)
}

# Grand tableau fusionnant tous les scénarios (AVEC CORRECTION DES EXTRACTIONS)
df_global <- bind_rows(tous_les_resultats) %>%
  mutate(
    Frequence = factor(Frequence, levels = toutes_frequences),
    Groupe_Num = factor(str_extract(Scenario, "[0-9]+"), levels = as.character(1:5)), # Lignes 1 à 5
    Sous_Type = factor(str_extract(Scenario, "(?<=[0-9])[a-d]"), levels = c("a", "b", "c", "d")), # Colonnes a à d (corrigé)
    Connectivite = str_extract(Scenario, "c[0-9]+") # Connectivité c1 à c4
  )

# ----------------------------------------------------------------------------
# SAUVEGARDE DU GRAND TABLEAU GÉNÉRAL
# ----------------------------------------------------------------------------
dir.create("Resultats_Globaux", showWarnings = FALSE, recursive = TRUE)
chemin_tableau_global <- "Resultats_Globaux/Tableau_Global_Tous_Scenarios.csv"
write.csv(df_global, file = chemin_tableau_global, row.names = FALSE)
print(paste("Grand tableau général sauvegardé dans :", chemin_tableau_global))

print("Traitement et sauvegarde organisée par fréquence terminés avec succès !")