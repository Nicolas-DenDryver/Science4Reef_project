# ============================================================================
# Chargement des bibliothèques
# ============================================================================
library(dplyr)
library(truncnorm)
# library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)

# ============================================================================
# 1. DÉFINITION DES SOUS-FONCTIONS
# ============================================================================

# --- A. Création des cohortes ---
creer_cohortes <- function(n_cohortes, taille_min, taille_max, 
                           sd_taille, n_individus, z, mesh) {
  cohortes <- matrix(0, nrow = n_cohortes, ncol = mesh)
  for(age in 1:n_cohortes) {
    taille_moyenne <- taille_min + (age-1) * (taille_max - taille_min) / n_cohortes
    dist_cohorte <- dnorm(z, mean = taille_moyenne, sd = sd_taille)
    dist_cohorte <- dist_cohorte / sum(dist_cohorte)
    survie <- exp(-0.05 * (age-1))
    cohortes[age, ] <- dist_cohorte * survie
  }
  dist_totale <- colSums(cohortes)
  dist_totale <- dist_totale / sum(dist_totale) * n_individus
  return(dist_totale)
}

# --- B. Calcul de la survie ---
calc_survie <- function(z, LE, s_z) {
  z_bar <- mean(z)
  s_base <- 0.01^(1/LE)
  logit_base <- log(s_base / (1 - s_base))
  logit_s <- logit_base + s_z * (z - z_bar)
  return(plogis(logit_s))
}

# --- C. Croissance ---
croissance_potentielle <- function(z, perte, dr) {
  x <- 10^z - 1
  x_apres_perte <- x * (1 - perte)
  r <- sqrt(x_apres_perte / pi)
  
  r_new <- r + dr
  x_potentiel <- pi * r_new^2
  z_prime <- log10(x_potentiel + 1)
  return(z_prime)
}

construire_matrice_croissance <- function(z, mesh, L, U, dz,
                                          perte, dr, g_sd,
                                          surface_totale_occupee, surface_max) {
  G <- matrix(0, nrow = mesh, ncol = mesh)
  competition <- min(1, surface_totale_occupee / surface_max)
  
  for (i in 1:mesh) {
    mu_theorique <- croissance_potentielle(z[i], perte, dr)
    col_theorique <- pnorm(z + dz/2, mean = mu_theorique, sd = g_sd) - 
      pnorm(z - dz/2, mean = mu_theorique, sd = g_sd)
    col_theorique[mesh] <- col_theorique[mesh] + (1 - pnorm(U, mean = mu_theorique, sd = g_sd))
    col_theorique[1]    <- col_theorique[1] + pnorm(L, mean = mu_theorique, sd = g_sd)
    col_theorique       <- col_theorique / sum(col_theorique)
    
    col_stase <- dnorm(z, mean = z[i], sd = g_sd * 0.1)
    col_stase <- col_stase / sum(col_stase)
    
    G[, i] <- (1 - competition) * col_theorique + competition * col_stase
  }
  return(G)
}


# --- E. Recrutement ---
recrutement_surface_max <- function(pop_source, pop_cible, z, Taille_reelle, fecondite,
                                    prop_fixation, surface_max_cible, coef_dispersion,
                                    recr_zmean,dist_recrues, taille_maturite) {
  mature_ind <- which(sqrt((10^z-1)/pi)*2 > taille_maturite) # selection des tailles d'individus mature à la production de larves
  surface_source <- pop_source[mature_ind] * Taille_reelle[mature_ind]
  surface_cible  <- sum(pop_cible * Taille_reelle)
  
  fec_z=0.5 #effet de la taille sur fécondité
  if(length(mature_ind) == 0) {
    larves_produites <- 0
  } else {
    larves_produites <- sum((fecondite+fec_z*(z[mature_ind]-mean(z[mature_ind]))) * surface_source)
  }
  larves_qui_arrivent <- larves_produites * coef_dispersion * prop_fixation
  
  surface_dispo <- max(0, surface_max_cible - surface_cible)
  alpha <- (3 * (10^recr_zmean - 1))^2
  n_max_recrue <- (surface_dispo / alpha)*rnorm(1,mean=1/3,sd=0.05)
  
  n_recrues <- min(larves_qui_arrivent, n_max_recrue)
  return(n_recrues * dist_recrues)
}


# ============================================================================
# 2. Fonctions de parametrisation
# ============================================================================
param_scenarios <- function(nb_pop, nb_type1, nb_type2) {
  # Populations
  populations <- list()
  for (i in 1:nb_pop) {
    dist <- list()
    for (j in 1:3) {
      dist <- c(dist, list(creer_cohortes(10, 0.5, 1.5, 0.3, 150, z, mesh)))
      dist[[j]][70:100] <- 0
      dist[[j]] <- dist[[j]] / sum(dist[[j]]) * 150
    }
    names(dist) <- c("acropora", "porites", "pocillopora")
    populations <- c(populations, list(dist))
    names(populations)[i] <- paste0("Pop", i)
  }
  
  # Survie (Correction ici avec nb_type1 et nb_type2)
  surv_params <- list()
  for (i in seq_len(nb_type1)) {
    surv_params <- c(surv_params, list(list(acropora = c(s_z=1, LE=20),  
                                            porites = c(s_z=1, LE=100),  
                                            pocillopora = c(s_z=1, LE=30))))
    names(surv_params)[i] <- paste0("Pop", i, "_type1")
  }
  for (i in seq_len(nb_type2)) {
    surv_params <- c(surv_params, list(list(acropora = c(s_z=1, LE=15),  
                                            porites = c(s_z=1, LE=75),  
                                            pocillopora = c(s_z=1, LE=23.5))))
    names(surv_params)[nb_type1 + i] <- paste0("Pop", nb_type1 + i, "_type2")
  }
  
  # Croissance (Correction ici aussi avec nb_type1 et nb_type2)
  grow_params <- list()
  for (i in seq_len(nb_type1)) {
    grow_params <- c(grow_params, list(list(acropora = c(perte=0.1, dr=5, g_sd=0.1),
                                            porites = c(perte=0.1, dr=1, g_sd=0.1),
                                            pocillopora = c(perte=0.1, dr=3, g_sd=0.1))))
    names(grow_params)[i] <- paste0("Pop", i, "_type1")
  }
  for (i in seq_len(nb_type2)) {
    grow_params <- c(grow_params, list(list(acropora = c(perte=0.1, dr=5*(2/3), g_sd=0.1),
                                            porites = c(perte=0.1, dr=1*(2/3), g_sd=0.1),
                                            pocillopora = c(perte=0.15, dr=3*(2/3), g_sd=0.1))))
    names(grow_params)[nb_type1 + i] <- paste0("Pop", nb_type1 + i, "_type2")
  }
  
  return(list(
    populations = populations,
    surv_params = surv_params,
    grow_params = grow_params
  ))
}

# ============================================================================
# 3. FONCTION PRINCIPALE FREQUENCE CONSTANTE (AVEC BURN-IN)
# ============================================================================
executer_projection_temporelle <- function(annees = 50, 
                                           burn_in = 30, 
                                           freq_blanchissement, 
                                           populations, 
                                           surv_params, 
                                           grow_params,
                                           recru_params,
                                           surface_max, 
                                           Taille_reelle, 
                                           z, mesh, L, U, dz, 
                                           c_matrix, fec_base, prop_fixation,
                                           param_surv_blanchissement, 
                                           prop_fec_params,
                                           prop_growth_params,
                                           genres, seed = 42) {
  
  annees_totales <- burn_in + annees
  traj_blanchissment <- list()
  episode_blanchissement <- list()
  
  for (freq_b in 1:length(freq_blanchissement)) {
    freq <- freq_blanchissement[freq_b]  
    set.seed(seed)
    
    df_blanchissement <- data.frame(année = 1:annees_totales)
    df_surv_blanchissement <- data.frame(année = 1:annees_totales)
    df_prop_fec <- data.frame(année = 1:annees_totales)
    df_prop_growth <- data.frame(année = 1:annees_totales)
    
    for(p in 1:length(populations)) {
      df_blanchissement[[paste0("Pop", p)]] <- 0
      df_surv_blanchissement[[paste0("Pop", p)]] <- 1
      df_prop_fec[[paste0("Pop", p)]] <- 1
      df_prop_growth[[paste0("Pop", p)]] <- 1
    }
    
    traj <- list()
    for (i in 1:length(populations)) {
      traj[[i]] <- list()
      for (g in 1:length(genres)) {
        traj[[i]][[g]] <- matrix(NA, nrow = annees_totales, ncol = mesh)
        traj[[i]][[g]][1, ] <- populations[[i]][[g]] 
      }
    }
    
    for (t in 2:annees_totales) {
      # --- Définition de la fréquence effective (0 pendant le burn-in) ---
      freq_effective <- if (t <= burn_in) 0 else freq
      
      if (runif(1) < freq_effective) {
        blanchissement <- 1
      }else{
        blanchissement <- 0
      }
      
      for (i in 1:length(populations)) {
        # --- Tirage dynamique des facteurs de blanchissement ---
        if (blanchissement == 1) {
          facteur_blanchiment <- sapply(1:length(genres), function(g) {
            rtruncnorm(n = 1, a = 0.0001, b = 1,
                       mean = param_surv_blanchissement[[g]][1],
                       sd = param_surv_blanchissement[[g]][2])
          })
          prop_fec <- rnorm(1, mean = prop_fec_params[1], sd = 0.01)
          prop_growth <- rnorm(1, mean = prop_growth_params[1], sd = 0.01)
        } else {
          facteur_blanchiment <- rep(1, length(genres)) 
          prop_fec <- 1
          prop_growth <- 1
        } 
        
        # Effets retardés post-blanchissement
        if (t > 1) {
          if (df_surv_blanchissement[t-1, i + 1] < 1) {
            prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[2], sd = 0.01)
            prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[2], sd = 0.01)
          }
        }
        if (t > 2) {
          if (df_surv_blanchissement[t-2, i + 1] < 1) {
            prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[3], sd = 0.01)
            prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[3], sd = 0.01)
          }
        }
        if (t > 3) {
          if (df_surv_blanchissement[t-3, i + 1] < 1) {
            prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[4], sd = 0.01)
            prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[4], sd = 0.01)
          }
        }
        
        df_blanchissement[t, i + 1] <- blanchissement
        df_surv_blanchissement[t, i + 1] <- mean(facteur_blanchiment)
        df_prop_fec[t, i + 1] <- prop_fec
        df_prop_growth[t, i + 1] <- prop_growth
        
        for (g in 1:length(genres)) {
          ## Survie
          survies <- calc_survie(z = z,
                                 LE = rnorm(1, mean = surv_params[[i]][[g]][["LE"]], sd = 1),
                                 s_z = surv_params[[i]][[g]][["s_z"]])
          
          survie_ajustee <- survies * facteur_blanchiment[g] 
          traj[[i]][[g]][t, ] <- traj[[i]][[g]][t-1, ] * survie_ajustee
        }
        
        ## Croissance
        surface_totale_occupee <- sum(sapply(1:length(populations[[i]]), function(genre) {
          sum(traj[[i]][[genre]][t, ] * Taille_reelle)
        }))
        
        for (g in 1:length(genres)) {
          params <- grow_params[[i]][[g]]
          
          G_dyn <- construire_matrice_croissance(
            z = z, mesh = mesh, L = L, U = U, dz = dz,
            perte = rnorm(1, mean = params["perte"], sd = 0.01), 
            dr = rnorm(1, mean = params["dr"], sd = 0.01)*df_prop_growth[t, i + 1], 
            g_sd = params["g_sd"],
            surface_totale_occupee = surface_totale_occupee,
            surface_max = surface_max[i]
          )
          
          traj[[i]][[g]][t, ] <- G_dyn %*% traj[[i]][[g]][t, ]
        }
      }
      
      traj_temp <- traj
      
      ## Recrutement
      for (i in 1:length(populations)) {
        for (g in 1:length(genres)) {
          params <- recru_params[[g]]
          dist_recrues <- dnorm(z, mean = params["recr_zmean"], sd = params["recr_zsd"])
          dist_recrues <- dist_recrues / sum(dist_recrues)
          prop_fec_local <- df_prop_fec[t, i + 1]
          
          recrut_local <- recrutement_surface_max(
            pop_source = traj_temp[[i]][[g]][t, ], 
            pop_cible = traj[[i]][[g]][t, ], 
            z=z,                 
            Taille_reelle = Taille_reelle, 
            fecondite = rnorm(1, mean = params["fec_base"], sd = 5) * prop_fec_local,
            prop_fixation = rnorm(1, mean = params["prop_fixation"], sd = 0.005),              
            surface_max_cible = surface_max[i], 
            coef_dispersion = c_matrix[i, i], 
            recr_zmean = params["recr_zmean"],
            dist_recrues = dist_recrues,
            taille_maturite = params["taille_maturite"]
          )
          traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] + recrut_local
          
          for (j in 1:length(populations)) {
            if (i != j && c_matrix[j, i] > 0) {
              prop_fec_source <- df_prop_fec[t, j + 1]
              
              recrut_externe <- recrutement_surface_max(
                pop_source = traj_temp[[j]][[g]][t, ], 
                pop_cible = traj[[i]][[g]][t, ],
                z=z,
                Taille_reelle = Taille_reelle, 
                fecondite = rnorm(1, mean = params["fec_base"], sd = 5) * prop_fec_source, 
                prop_fixation = rnorm(1, mean = params["prop_fixation"], sd = 0.005),          
                surface_max_cible = surface_max[i], 
                coef_dispersion = c_matrix[j, i], 
                recr_zmean = params["recr_zmean"],
                dist_recrues = dist_recrues,
                taille_maturite = params["taille_maturite"]
              )
              traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] + recrut_externe
            }
          }
          traj[[i]][[g]][t, ][traj[[i]][[g]][t, ] < 0] <- 0
        }
      }
      
      # Plafond strict : Mortalité par compétition spatiale
      for (i in 1:length(populations)) {
        surface_finale <- sum(sapply(1:length(genres), function(g) {
          sum(traj[[i]][[g]][t, ] * Taille_reelle)
        }))
        
        if (surface_finale > surface_max[i]) {
          ratio_ajustement <- surface_max[i] / surface_finale
          for (g in 1:length(genres)) {
            traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] * ratio_ajustement
          }
        }
      }
    }
    
    # --- Extraction uniquement des années post-burn-in ---
    idx_post_burnin <- (burn_in + 1):annees_totales
    
    traj_filtre <- list()
    for (i in 1:length(populations)) {
      traj_filtre[[i]] <- list()
      for (g in 1:length(genres)) {
        traj_filtre[[i]][[g]] <- traj[[i]][[g]][idx_post_burnin, ]
      }
    }
    
    df_blanchissement_f <- df_blanchissement[idx_post_burnin, ]
    df_surv_blanchissement_f <- df_surv_blanchissement[idx_post_burnin, ]
    df_prop_fec_f <- df_prop_fec[idx_post_burnin, ]
    df_prop_growth_f <- df_prop_growth[idx_post_burnin, ]
    
    df_blanchissement_f$année <- 1:annees
    df_surv_blanchissement_f$année <- 1:annees
    df_prop_fec_f$année <- 1:annees
    df_prop_growth_f$année <- 1:annees
    
    traj_blanchissment[[freq_b]] <- traj_filtre
    episode_blanchissement[[freq_b]] <- list(
      "evenement_blanchissement" = df_blanchissement_f,
      "taux_survie_blanchissement" = df_surv_blanchissement_f,
      "effet_blanchissement_fecondation" = df_prop_fec_f,
      "effet_blanchissement_growth" = df_prop_growth_f
    )
  }
  
  return(list(
    traj_blanchissment = traj_blanchissment,
    episode_blanchissement = episode_blanchissement
  ))
}


# ============================================================================
# 4. FONCTION PRINCIPALE FREQUENCE PROGRESSIVE (AVEC BURN-IN)
# ============================================================================
executer_projection_temporelle_progressif <- function(annees = 50, 
                                                      burn_in = 30, 
                                                      freq_blanchissement_vec, 
                                                      moyenne_int, 
                                                      populations, 
                                                      surv_params, 
                                                      grow_params,
                                                      recru_params,
                                                      surface_max, 
                                                      Taille_reelle, 
                                                      z, mesh, L, U, dz, 
                                                      c_matrix, fec_base,
                                                      prop_fixation,
                                                      param_surv_blanchissement,
                                                      prop_fec_params,
                                                      prop_growth_params,
                                                      genres, seed = 42) {
  
  annees_totales <- burn_in + annees
  traj_blanchissment <- list()
  episode_blanchissement <- list()
  
  set.seed(seed)
  
  param_surv_local <- param_surv_blanchissement
  df_blanchissement <- data.frame(année = 1:annees_totales)
  df_surv_blanchissement <- data.frame(année = 1:annees_totales)
  df_prop_fec <- data.frame(année = 1:annees_totales)
  df_prop_growth <- data.frame(année = 1:annees_totales)
  
  for(p in 1:length(populations)) {
    df_blanchissement[[paste0("Pop", p)]] <- 0
    df_surv_blanchissement[[paste0("Pop", p)]] <- 1
    df_prop_fec[[paste0("Pop", p)]] <- 1
    df_prop_growth[[paste0("Pop", p)]] <- 1
  }
  
  traj <- list()
  for (i in 1:length(populations)) {
    traj[[i]] <- list()
    for (g in 1:length(genres)) {
      traj[[i]][[g]] <- matrix(NA, nrow = annees_totales, ncol = mesh)
      traj[[i]][[g]][1, ] <- populations[[i]][[g]] 
    }
  }
  
  for (t in 2:annees_totales) {
    # --- Indexation temporelle post-burn-in ---
    if (t <= burn_in) {
      freq_effective <- 0
      param_surv_local[[1]][1] <- moyenne_int[[1]][1]
      param_surv_local[[2]][1] <- moyenne_int[[2]][1]
      param_surv_local[[3]][1] <- moyenne_int[[3]][1]
    } else {
      t_post <- t - burn_in
      freq_effective <- freq_blanchissement_vec[t_post]
      param_surv_local[[1]][1] <- moyenne_int[[1]][t_post]
      param_surv_local[[2]][1] <- moyenne_int[[2]][t_post]
      param_surv_local[[3]][1] <- moyenne_int[[3]][t_post]
    }
    
    if (runif(1) < freq_effective) {
      blanchissement <- 1
    }else{
      blanchissement <- 0
    }
    
    for (i in 1:length(populations)) {
      # --- Tirage dynamique des facteurs de blanchissement ---
      if (blanchissement == 1) {
        facteur_blanchiment <- sapply(1:length(genres), function(g) {
          rtruncnorm(n = 1, a = 0.0001, b = 1,
                     mean = param_surv_blanchissement[[g]][1],
                     sd = param_surv_blanchissement[[g]][2])
        })
        prop_fec <- rnorm(1, mean = prop_fec_params[1], sd = 0.01)
        prop_growth <- rnorm(1, mean = prop_growth_params[1], sd = 0.01)
      } else {
        facteur_blanchiment <- rep(1, length(genres)) 
        prop_fec <- 1
        prop_growth <- 1
      } 
      
      # Effets retardés post-blanchissement
      if (t > 1) {
        if (df_surv_blanchissement[t-1, i + 1] < 1) {
          prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[2], sd = 0.01)
          prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[2], sd = 0.01)
        }
      }
      if (t > 2) {
        if (df_surv_blanchissement[t-2, i + 1] < 1) {
          prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[3], sd = 0.01)
          prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[3], sd = 0.01)
        }
      }
      if (t > 3) {
        if (df_surv_blanchissement[t-3, i + 1] < 1) {
          prop_fec <- prop_fec * rnorm(1, mean = prop_fec_params[4], sd = 0.01)
          prop_growth <- prop_growth * rnorm(1, mean = prop_growth_params[4], sd = 0.01)
        }
      }
      
      df_blanchissement[t, i + 1] <- blanchissement
      df_surv_blanchissement[t, i + 1] <- mean(facteur_blanchiment)
      df_prop_fec[t, i + 1] <- prop_fec
      df_prop_growth[t, i + 1] <- prop_growth
      
      for (g in 1:length(genres)) {
        ## Survie
        survies <- calc_survie(z = z,
                               LE = rnorm(1, mean = surv_params[[i]][[g]][["LE"]], sd = 1),
                               s_z = surv_params[[i]][[g]][["s_z"]])
        
        survie_ajustee <- survies * facteur_blanchiment[g] 
        traj[[i]][[g]][t, ] <- traj[[i]][[g]][t-1, ] * survie_ajustee
      }
      
      ## Croissance
      surface_totale_occupee <- sum(sapply(1:length(populations[[i]]), function(genre) {
        sum(traj[[i]][[genre]][t, ] * Taille_reelle)
      }))
      
      for (g in 1:length(genres)) {
        params <- grow_params[[i]][[g]]
        
        G_dyn <- construire_matrice_croissance(
          z = z, mesh = mesh, L = L, U = U, dz = dz,
          perte = rnorm(1, mean = params["perte"], sd = 0.01), 
          dr = rnorm(1, mean = params["dr"], sd = 0.01)*df_prop_growth[t, i + 1], 
          g_sd = params["g_sd"],
          surface_totale_occupee = surface_totale_occupee,
          surface_max = surface_max[i]
        )
        
        traj[[i]][[g]][t, ] <- G_dyn %*% traj[[i]][[g]][t, ]
      }
    }
    
    traj_temp <- traj
    
    ## Recrutement
    for (i in 1:length(populations)) {
      for (g in 1:length(genres)) {
        params <- recru_params[[g]]
        dist_recrues <- dnorm(z, mean = params["recr_zmean"], sd = params["recr_zsd"])
        dist_recrues <- dist_recrues / sum(dist_recrues)
        prop_fec_local <- df_prop_fec[t, i + 1]
        
        recrut_local <- recrutement_surface_max(
          pop_source = traj_temp[[i]][[g]][t, ], 
          pop_cible = traj[[i]][[g]][t, ], 
          z=z,                 
          Taille_reelle = Taille_reelle, 
          fecondite = rnorm(1, mean = params["fec_base"], sd = 5) * prop_fec_local,
          prop_fixation = rnorm(1, mean = params["prop_fixation"], sd = 0.005),              
          surface_max_cible = surface_max[i], 
          coef_dispersion = c_matrix[i, i], 
          recr_zmean = params["recr_zmean"],
          dist_recrues = dist_recrues,
          taille_maturite = params["taille_maturite"]
        )
        traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] + recrut_local
        
        for (j in 1:length(populations)) {
          if (i != j && c_matrix[j, i] > 0) {
            prop_fec_source <- df_prop_fec[t, j + 1]
            
            recrut_externe <- recrutement_surface_max(
              pop_source = traj_temp[[j]][[g]][t, ], 
              pop_cible = traj[[i]][[g]][t, ],
              z=z,
              Taille_reelle = Taille_reelle, 
              fecondite = rnorm(1, mean = params["fec_base"], sd = 5) * prop_fec_source, 
              prop_fixation = rnorm(1, mean = params["prop_fixation"], sd = 0.005),          
              surface_max_cible = surface_max[i], 
              coef_dispersion = c_matrix[j, i], 
              recr_zmean = params["recr_zmean"],
              dist_recrues = dist_recrues,
              taille_maturite = params["taille_maturite"]
            )
            traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] + recrut_externe
          }
        }
        traj[[i]][[g]][t, ][traj[[i]][[g]][t, ] < 0] <- 0
      }
    }
    
    # Plafond strict : Mortalité par compétition spatiale
    for (i in 1:length(populations)) {
      surface_finale <- sum(sapply(1:length(genres), function(g) {
        sum(traj[[i]][[g]][t, ] * Taille_reelle)
      }))
      
      if (surface_finale > surface_max[i]) {
        ratio_ajustement <- surface_max[i] / surface_finale
        for (g in 1:length(genres)) {
          traj[[i]][[g]][t, ] <- traj[[i]][[g]][t, ] * ratio_ajustement
        }
      }
    }
  }
  
  # --- Extraction uniquement des années post-burn-in ---
  idx_post_burnin <- (burn_in + 1):annees_totales
  
  traj_filtre <- list()
  for (i in 1:length(populations)) {
    traj_filtre[[i]] <- list()
    for (g in 1:length(genres)) {
      traj_filtre[[i]][[g]] <- traj[[i]][[g]][idx_post_burnin, ]
    }
  }
  
  df_blanchissement_f <- df_blanchissement[idx_post_burnin, ]
  df_surv_blanchissement_f <- df_surv_blanchissement[idx_post_burnin, ]
  df_prop_fec_f <- df_prop_fec[idx_post_burnin, ]
  df_prop_growth_f <- df_prop_growth[idx_post_burnin, ]
  
  df_blanchissement_f$année <- 1:annees
  df_surv_blanchissement_f$année <- 1:annees
  df_prop_fec_f$année <- 1:annees
  df_prop_growth_f$année <- 1:annees
  
  traj_blanchissment <- traj_filtre
  episode_blanchissement <- list(
    "evenement_blanchissement" = df_blanchissement_f,
    "taux_survie_blanchissement" = df_surv_blanchissement_f,
    "effet_blanchissement_fecondation" = df_prop_fec_f,
    "effet_blanchissement_growth" = df_prop_growth_f
  )
  
  return(list(
    traj_blanchissment = traj_blanchissment,
    episode_blanchissement = episode_blanchissement
  ))
}