## ============================================================================
## GRAPHIQUES DESCRIPTIFS (1-5) + MODÈLES MIXTES & TENDANCES
## (Version accessible aux daltoniens)
## ============================================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)
library(lmerTest)   # Pour obtenir les p-values des modèles mixtes
library(emmeans)    # Pour les tendances (emtrends) et l'état final (emmeans)
library(performance)# Pour le calcul du R2
library(viridis)    # Palette de couleurs universelle et adaptée aux daltoniens

# 1. Charger directement le grand tableau global
print("Chargement du grand tableau global...")
df_global <- read.csv("Resultats_Globaux/Tableau_Global_Tous_Scenarios.csv")

noms_frequences <- c("No disturbance", 
                     "Once every 10 years", 
                     "Once every 5 years",
                     "Once every 4 years",
                     "Once every 3 years",
                     "Once every 2 years", 
                     "Once a year")

nom_progressive <- paste("Progressive from", maxmin_freq[1], "to", maxmin_freq[2])
toutes_frequences <- c(noms_frequences, nom_progressive)

# 3. Préparation et nettoyage des facteurs du grand tableau
df_global <- df_global %>%
  mutate(
    Frequence = factor(Frequence, levels = toutes_frequences),
    Groupe_Num = factor(str_extract(Scenario, "[0-9]+"), levels = as.character(1:5)),
    Sous_Type = factor(str_extract(Scenario, "(?<=[0-9])[a-d]"), levels = c("a", "b", "c", "d")),
    Connectivite = str_extract(Scenario, "c[0-9]+"),
    Annee_scaled = Annee / max(Annee),
    Sim_Unique = paste0(Scenario, "_", Simulation) 
  )

# Correction propre pour Sim_Unique
df_global$Sim_Unique <- paste0(df_global$Scenario, "_", df_global$Simulation)

my_theme <- theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey50"),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    legend.position = "bottom"
  )

print("Données prêtes. Lancement de la boucle globale par fréquence...")

# 4. Boucle principale sur l'ensemble des fréquences
for (freq in toutes_frequences) {
  print(paste("=================================================="))
  print(paste("Traitement de la fréquence :", freq))
  print(paste("=================================================="))
  
  dossier_freq <- file.path("Resultats_Par_Frequence_bis", gsub(" ", "_", freq))
  dir.create(dossier_freq, showWarnings = FALSE, recursive = TRUE)
  
  df_freq <- df_global %>% filter(Frequence == freq)
  connectivites_uniques <- unique(df_freq$Connectivite)
  
  # --------------------------------------------------------------------------
  # PARTIE A : FIGURES 1 à 5 (par niveau de connectivité)
  # --------------------------------------------------------------------------
  for (conn in connectivites_uniques) {
    if (is.na(conn)) next
    
    df_conn_sub <- df_freq %>% filter(Connectivite == conn)
    
    # Résumé pour Figures 1A et 1B
    df_summary_grid <- df_conn_sub %>%
      group_by(Scenario, Groupe_Num, Sous_Type, Annee) %>%
      summarise(
        Abondance_mean = mean(Abondance_Meta, na.rm = TRUE),
        Abondance_q05  = quantile(Abondance_Meta, 0.05, na.rm = TRUE),
        Abondance_q95  = quantile(Abondance_Meta, 0.95, na.rm = TRUE),
        Surface_mean   = mean(Surface_Meta_pct, na.rm = TRUE),
        Surface_q05    = quantile(Surface_Meta_pct, 0.05, na.rm = TRUE),
        Surface_q95    = quantile(Surface_Meta_pct, 0.95, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Figure 1A (Abondance)
    fig1A <- ggplot(df_summary_grid, aes(x = Annee)) +
      geom_ribbon(aes(ymin = Abondance_q05, ymax = Abondance_q95), fill = "steelblue", alpha = 0.3) +
      geom_line(aes(y = Abondance_mean), color = "dodgerblue4", linewidth = 1) +
      facet_grid(Groupe_Num ~ Sous_Type) +
      labs(title = paste("Figure 1A: Abundance Dynamics - Connectivity", toupper(conn)),
           subtitle = paste("Disturbance regime:", freq, "| Rows: Scenarios 1-5 | Cols: Variants a-d"),
           x = "Simulation years", y = "Number of individuals") +
      my_theme
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_1A_Abondance_Connectivity_", conn, ".png")),
           plot = fig1A, width = 14, height = 12, dpi = 300, bg = "white")
    
    # Figure 1B (Surface)
    fig1B <- ggplot(df_summary_grid, aes(x = Annee)) +
      geom_ribbon(aes(ymin = Surface_q05, ymax = Surface_q95), fill = "coral", alpha = 0.3) +
      geom_line(aes(y = Surface_mean), color = "firebrick", linewidth = 1) +
      facet_grid(Groupe_Num ~ Sous_Type) +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(title = paste("Figure 1B: Covered Surface Dynamics - Connectivity", toupper(conn)),
           subtitle = paste("Disturbance regime:", freq, "| Rows: Scenarios 1-5 | Cols: Variants a-d"),
           x = "Simulation years", y = "Covered surface (%)") +
      my_theme
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_1B_Surface_Connectivity_", conn, ".png")),
           plot = fig1B, width = 14, height = 12, dpi = 300, bg = "white")
    
    # Figure 2 (Genres par Grille 5x4) - Utilisation de Viridis (daltonien-friendly)
    df_summary_genre_grid <- df_conn_sub %>%
      pivot_longer(cols = c(acropora, porites, pocillopora), names_to = "Genre", values_to = "Surface_pct") %>%
      group_by(Scenario, Groupe_Num, Sous_Type, Annee, Genre) %>%
      summarise(
        Surface_mean = mean(Surface_pct, na.rm = TRUE),
        Surface_q05  = quantile(Surface_pct, 0.05, na.rm = TRUE),
        Surface_q95  = quantile(Surface_pct, 0.95, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Genre = str_to_title(Genre))
    
    fig2 <- ggplot(df_summary_genre_grid, aes(x = Annee)) +
      geom_ribbon(aes(ymin = Surface_q05, ymax = Surface_q95, fill = Genre), alpha = 0.3) +
      geom_line(aes(y = Surface_mean, color = Genre), linewidth = 1) +
      facet_grid(Genre ~ Groupe_Num + Sous_Type) +
      scale_fill_viridis_d(option = "viridis") +
      scale_color_viridis_d(option = "viridis") +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(title = paste("Figure 2: Coral Genus Dynamics - Connectivity", toupper(conn)),
           subtitle = paste("Disturbance regime:", freq, "| Genus x Scenarios (1-5 x a-d)"),
           x = "Simulation years", y = "Surface (%)") +
      my_theme +
      theme(strip.text = element_text(size = 8))
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_2_Genres_Connectivity_", conn, ".png")),
           plot = fig2, width = 18, height = 10, dpi = 300, bg = "white")
    
    # Figures 3, 4 et 5 (Ajout de linetype + Dark2 pour accessibilité daltoniens)
    df_summary_conn <- df_conn_sub %>%
      group_by(Scenario, Annee) %>%
      summarise(
        Abondance_mean   = mean(Abondance_Meta, na.rm = TRUE),
        Abondance_median = median(Abondance_Meta, na.rm = TRUE),
        Abondance_sd     = sd(Abondance_Meta, na.rm = TRUE),
        Surface_mean     = mean(Surface_Meta_pct, na.rm = TRUE),
        Surface_median   = median(Surface_Meta_pct, na.rm = TRUE),
        Surface_sd       = sd(Surface_Meta_pct, na.rm = TRUE),
        SD_Surface       = ifelse(!is.na(Surface_sd), Surface_sd, 0),
        .groups = "drop"
      )
    
    fig3_conn <- ggplot(df_summary_conn, aes(x = Annee, y = SD_Surface, color = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = paste("Figure 3: Surface Dispersion - Connectivity", toupper(conn)),
        subtitle = freq, x = "Years", y = "Standard Deviation of Surface (%)", 
        color = "Scenario:", linetype = "Scenario:") +
      my_theme
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_3_SD_Surface_Connectivity_", conn, ".png")),
           plot = fig3_conn, width = 10, height = 6, dpi = 300, bg = "white")
    
    fig4A_conn <- ggplot(df_summary_conn, aes(x = Annee, y = Abondance_mean, color = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = paste("Figure 4A: Mean Abundance - Connectivity", toupper(conn)), 
           subtitle = freq, x = "Years", y = "Individuals", color = "Scenario:", linetype = "Scenario:") +
      my_theme
    
    fig4B_conn <- ggplot(df_summary_conn, aes(x = Annee, y = Surface_mean, color = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(title = paste("Figure 4B: Mean Surface Area - Connectivity", toupper(conn)), 
           subtitle = freq, x = "Years", y = "Surface (%)", color = "Scenario:", linetype = "Scenario:") +
      my_theme
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_4_Mean_Connectivity_", conn, ".png")),
           plot = fig4A_conn / fig4B_conn, width = 10, height = 12, dpi = 300, bg = "white")
    
    fig5A_conn <- ggplot(df_summary_conn, aes(x = Annee, y = Abondance_median, color = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_brewer(palette = "Dark2") +
      labs(title = paste("Figure 5A: Median Abundance - Connectivity", toupper(conn)), 
           subtitle = freq, x = "Years", y = "Individuals", color = "Scenario:", linetype = "Scenario:") +
      my_theme
    
    fig5B_conn <- ggplot(df_summary_conn, aes(x = Annee, y = Surface_median, color = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.2) +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
      labs(title = paste("Figure 5B: Median Surface Area - Connectivity", toupper(conn)), 
           subtitle = freq, x = "Years", y = "Surface (%)", color = "Scenario:", linetype = "Scenario:") +
      my_theme
    
    ggsave(filename = file.path(dossier_freq, paste0("Figure_5_Median_Connectivity_", conn, ".png")),
           plot = fig5A_conn / fig5B_conn, width = 10, height = 12, dpi = 300, bg = "white")
  }
  
  # --------------------------------------------------------------------------
  # PARTIE B : MODÈLES MIXTES & GRAPHIQUE DES TENDANCES
  # --------------------------------------------------------------------------
  df_final_freq <- df_freq %>% filter(Annee > 10)
  
  if (nrow(df_final_freq) > 0) {
    print(paste("Ajustement des modèles mixtes pour :", freq))
    
    modele_mixte2 <- lmer(
      Surface_Meta_pct ~ Scenario * Annee_scaled + (Annee_scaled | Sim_Unique),
      data = df_final_freq)
    
    print("--- ANOVA GLOBALE ---")
    print(anova(modele_mixte2))
    
    print("Calcul des tendances temporelles (emtrends)...")
    tendances_scenarios <- emtrends(modele_mixte2, ~ Scenario, var = "Annee_scaled")
    df_pentes_clean <- as.data.frame(test(tendances_scenarios, null = 0))
    
    em_final_annee <- emmeans(modele_mixte2, ~ Scenario, at = list(Annee_scaled = 1))
    df_final_state <- as.data.frame(summary(em_final_annee)) %>%
      rename(Surface_Finale_Estimee = emmean, SE_Final = SE)
    
    df_synthese_globale <- df_pentes_clean %>%
      left_join(df_final_state %>% select(Scenario, Surface_Finale_Estimee, SE_Final), by = "Scenario") %>%
      mutate(
        Groupe_Num = as.factor(str_extract(Scenario, "(?<=scenario)[0-9]+")),
        Sous_Type  = as.factor(str_extract(Scenario, "[a-d](?=\\.)")),
        Connectivite = as.factor(str_extract(Scenario, "c[1-4]"))
      )
    
    min_y <- floor(min(df_synthese_globale$Annee_scaled.trend - 1))
    max_y <- ceiling(max(df_synthese_globale$Annee_scaled.trend + 1))
    
    # Graphique de tendances avec palette Dark2 (daltonien-friendly)
    p_trend <- ggplot(df_synthese_globale, aes(x = Connectivite, y = Annee_scaled.trend, color = Groupe_Num, shape = Sous_Type)) +
      geom_point(size = 3.5, position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = Annee_scaled.trend - 1.96 * SE, 
                        ymax = Annee_scaled.trend + 1.96 * SE),
                    width = 0.2, position = position_dodge(width = 0.5)) +
      scale_shape_manual(values = c(16, 17, 18, 8)) + 
      scale_color_brewer(palette = "Dark2") + # Palette sécurisée daltoniens
      scale_y_continuous(
        breaks = seq(min_y, max_y, by = 1),
        minor_breaks = seq(min_y, max_y, by = 0.5)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
      theme_minimal(base_size = 11) +
      labs(
        title = "Dynamique : Pentes de tendance",
        subtitle = paste("Fréquence de perturbation :", freq),
        x = "Niveau de Connectivité",
        y = "Pente de la tendance (Annee_scaled.trend)",
        color = "Groupe",
        shape = "Sous-Type"
      ) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_line(color = "grey92", linetype = "dotted")
      )
    
    ggsave(filename = file.path(dossier_freq, "Graphique_Tendances_Seul.png"),
           plot = p_trend, width = 11, height = 8, dpi = 300, bg = "white")
    
    r2_resultats <- r2(modele_mixte2)
    print(r2_resultats)
    print(paste("Le design des scénarios explique",
                round(as.numeric(r2_resultats$R2_conditional) * 100, 2),
                "% de la variance totale pour la fréquence :", freq))
  }
}

print("Traitement et sauvegarde organisée par fréquence terminés avec succès !")
