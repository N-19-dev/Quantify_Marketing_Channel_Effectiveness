# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(lubridate)
library(forecast)
library(dplyr)

import_and_clean_data <- function(file_path) {
  # Lire le CSV avec des virgules comme séparateur
  data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  # Supprimer les trois premières colonnes contenant des métadonnées
  data <- data[, -c(1:3)]

  # print(ncol(data))
  # print(names(data))
  
  # Renommer les colonnes
  names(data) <- c("CPI", "CCI", "PPI", "UNIT_PRICE", "POS_SUPPLY",
                   "SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET",
                   "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV")
  
  
  # Convertir les colonnes numériques (remplacer les virgules dans les nombres par des points si nécessaire)
  numeric_cols <- names(data)[2:length(names(data))]
  for(col in numeric_cols) {
    data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
  }
  
  # Retirer les lignes avec toutes les valeurs NA
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  
  return(data)
}

# hemin_fichier <- "MMM_Data.csv"
# data <- import_and_clean_data(chemin_fichier)

# 2. Feature Engineering and Data Preparation
prepare_data <- function(data) {
  data %>%
    mutate(
      
      # Calculate ROI for each channel
      ROI_SMS = SALES / AD_SMS,
      ROI_Newspaper = SALES / AD_NEWSPAPER,
      ROI_Radio = SALES / AD_RADIO,
      ROI_TV = SALES / AD_TV,
      ROI_Internet = SALES / AD_INTERNET,
      
      # Calculate efficiency metrics (GRP per dollar spent)
      Efficiency_SMS = GRP_SMS / AD_SMS,
      Efficiency_Newspaper = GRP_NEWSPAPER / AD_NEWSPAPER,
      Efficiency_Radio = GRP_RADIO / AD_RADIO,
      Efficiency_TV = GRP_TV / AD_TV,
      Efficiency_Internet = GRP_INTERNET / AD_INTERNET
    ) %>%
    na.omit()
}

chemin_fichier <- "MMM_Data.csv"
data <- import_and_clean_data(chemin_fichier)
# print(head(data))  # Afficher les données nettoyées

# Tester la préparation des données
result <- prepare_data(data)
# print(head(result))

# 3. Exploratory Data Analysis

analyze_channels <- function(data) {
  # Vérifie que les colonnes nécessaires existent dans le data frame
  required_columns <- c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET", "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV")
  if (!all(required_columns %in% colnames(data))) {
    stop("Le data frame ne contient pas toutes les colonnes nécessaires.")
  }

  # Analyse des dépenses publicitaires
  expenses_long <- data %>%
    select(AD_SMS, AD_NEWSPAPER, AD_RADIO, AD_TV, AD_INTERNET) %>%
    gather(key = "Channel", value = "Spend") %>%
    mutate(Channel = gsub("AD_", "", Channel))
  
  # Créer le graphique de distribution des dépenses
  spend_plot <- ggplot(expenses_long, aes(x = Channel, y = Spend, fill = Channel)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of Advertising Spend by Channel",
         y = "Spend ($)")
  
  # Analyse des GRP
  grp_long <- data %>%
    select(starts_with("GRP_")) %>%
    gather(key = "Channel", value = "GRP") %>%
    mutate(Channel = gsub("GRP_", "", Channel))
  
  # Créer le graphique de distribution des GRP
  grp_plot <- ggplot(grp_long, aes(x = Channel, y = GRP, fill = Channel)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of GRP by Channel")
  
  # Analyse de corrélation
  ad_cols <- c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET")
  correlation_matrix <- cor(data[, ad_cols], use = "complete.obs")  # Utiliser "complete.obs" pour gérer les NA
  
  # Retourner les graphiques et la matrice de corrélation
  list(
    spend_distribution = spend_plot,
    grp_distribution = grp_plot,
    correlations = correlation_matrix
  )
}


result <- analyze_channels(data)
# print(result$spend_distribution)
# print(result$grp_distribution)
# print(result$correlations)

# 4. Marketing Mix Modeling
build_mmm <- function(data) {
  # Vérifie que les colonnes nécessaires existent
  required_columns <- c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET", "CPI", "CCI", "PPI", "UNIT_PRICE")
  if (!all(required_columns %in% colnames(data))) {
    stop("Le data frame ne contient pas toutes les colonnes nécessaires.")
  }
  
  # Extraire le mois de la colonne DATE si ce n'est pas déjà fait
  if (!"Month" %in% colnames(data)) {
    data$Month <- format(data$DATE, "%m")  # Créer une colonne de mois au format "MM"
  }
  
  # Modèle basique avec les dépenses publicitaires
  basic_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET,
                    data = data, na.action = na.omit)
  
  # Modèle avancé incluant les conditions de marché et la saisonnalité
  advanced_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET +
                       CPI + CCI + PPI + UNIT_PRICE + factor(Month),
                       data = data, na.action = na.omit)
  
  # Calculer les élasticités pour chaque canal
  means <- colMeans(data[, c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET")], na.rm = TRUE)
  
  coef_advanced <- coef(advanced_model)
  
  elasticities <- data.frame(
    Channel = c("SMS", "Newspaper", "Radio", "TV", "Internet"),
    Elasticity = c(
      coef_advanced["AD_SMS"] * means["AD_SMS"] / means["SALES"],
      coef_advanced["AD_NEWSPAPER"] * means["AD_NEWSPAPER"] / means["SALES"],
      coef_advanced["AD_RADIO"] * means["AD_RADIO"] / means["SALES"],
      coef_advanced["AD_TV"] * means["AD_TV"] / means["SALES"],
      coef_advanced["AD_INTERNET"] * means["AD_INTERNET"] / means["SALES"]
    )
  )
  
  list(
    basic_model = basic_model,
    advanced_model = advanced_model,
    elasticities = elasticities
  )
}

result <- build_mmm(data)
print(result$basic_model)
print(result$advanced_model)
print(result$elasticities)