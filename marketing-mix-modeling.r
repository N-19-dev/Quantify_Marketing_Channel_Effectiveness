# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(lubridate)
library(forecast)
library(dplyr)
library(gridExtra)

# 1. Import and Clean Data
import_and_clean_data <- function(file_path) {
  # Lire le CSV avec des virgules comme séparateur
  data <- read.csv(file_path, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  
  # Supprimer les trois premières colonnes contenant des métadonnées
  data <- data[, -c(1, 3)]

  # Renommer les colonnes
  names(data) <- c("DATE", "CPI", "CCI", "PPI", "UNIT_PRICE", "POS_SUPPLY",
                   "SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET",
                   "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV")
  
  # Convertir la colonne DATE en format Date (si elle n'est pas déjà convertie)
  if (!inherits(data$DATE, "Date")) {
    data$DATE <- as.Date(data$DATE, format = "%m/%d/%Y")
  }
  
  # Convertir les colonnes numériques (remplacer les virgules par des points si nécessaire)
  numeric_cols <- names(data)[3:length(names(data))]
  for(col in numeric_cols) {
    data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
  }
  
  # Retirer les lignes avec toutes les valeurs NA
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  
  return(data)
}

# chemin_fichier <- "MMM_Data.csv"
# data <- import_and_clean_data(chemin_fichier)
# # print(head(data))

# 2. Feature Engineering and Data Preparation
prepare_data <- function(data) {
  data %>%
    mutate(
      # Extraire le mois de la colonne DATE
      Month = format(DATE, "%m"),
      
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


# print(head(data))  # Afficher les données nettoyées

# Tester la préparation des données
# result <- prepare_data(data)
# # print(head(result))

# 3. Exploratory Data Analysis
analyze_channels <- function(data) {
  # Vérifie que les colonnes nécessaires existent dans le data frame
  required_columns <- c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET", "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV", "DATE")
  if (!all(required_columns %in% colnames(data))) {
    stop("Le data frame ne contient pas toutes les colonnes nécessaires.")
  }

  # Extraire le mois si ce n'est pas déjà fait
  if (!"Month" %in% colnames(data)) {
    data$Month <- format(data$DATE, "%m")
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


# result <- analyze_channels(data)
# grid.arrange(result$spend_distribution, result$grp_distribution, ncol = 1)
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
  
  # Vérifiez que `Month` contient plusieurs niveaux
  if (length(unique(data$Month)) < 2) {
    stop("La variable 'Month' doit contenir au moins deux niveaux distincts pour inclure un effet de saisonnalité.")
  }
  
  # Convertir `Month` en facteur pour le modèle
  data$Month <- as.factor(data$Month)
  
  # Modèle basique avec les dépenses publicitaires
  basic_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET,
                    data = data, na.action = na.omit)
  
  # Modèle avancé incluant les conditions de marché et la saisonnalité
  advanced_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET +
                       CPI + CCI + PPI + UNIT_PRICE + Month,
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

# result <- build_mmm(data)
# print(result$basic_model)
# print(result$advanced_model)
# print(result$elasticities)

# 5. Analyse de l'efficacité
analyze_effectiveness <- function(data, model_results) {
  # Vérifier que les résultats du modèle contiennent le modèle avancé
  if (is.null(model_results$advanced_model)) {
    stop("Le modèle avancé n'est pas disponible dans les résultats fournis.")
  }
  
  # Coefficients du modèle avancé
  coef_advanced <- coef(model_results$advanced_model)

  # Calcul des ventes attribuées pour chaque canal
  data$Sales_Attributable_SMS <- data$AD_SMS * coef_advanced["AD_SMS"]
  data$Sales_Attributable_Newspaper <- data$AD_NEWSPAPER * coef_advanced["AD_NEWSPAPER"]
  data$Sales_Attributable_Radio <- data$AD_RADIO * coef_advanced["AD_RADIO"]
  data$Sales_Attributable_TV <- data$AD_TV * coef_advanced["AD_TV"]
  data$Sales_Attributable_Internet <- data$AD_INTERNET * coef_advanced["AD_INTERNET"]

  # Somme des ventes attribuées
  total_sales <- sum(data$SALES, na.rm = TRUE)

  # Coûts des dépenses publicitaires pour chaque canal
  costs <- colSums(data[, c("AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET")], na.rm = TRUE)

  # Calcul du ROI pour chaque canal
  roi <- sapply(c("AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET"), function(channel) {
    sales_attributable <- sum(data[[paste0("Sales_Attributable_", gsub("AD_", "", channel))]], na.rm = TRUE)
    investment <- costs[[channel]]
    if (investment == 0) {
      return(NA)  # Éviter la division par zéro
    }
    return((sales_attributable - investment) / investment * 100)
  })

  # Création d'un data frame pour les résultats du ROI
  roi_results <- data.frame(
    Channel = c("SMS", "Newspaper", "Radio", "TV", "Internet"),
    ROI = roi
  )

  return(roi_results)
}

# roi_results <- analyze_effectiveness(data, result)

# # Afficher les résultats du ROI
# # print(roi_results)

generate_report <- function(data, model_results, effectiveness_results) {
  
  # 1. Résumé des données
  cat("### Résumé des données\n")
  cat("Nombre d'observations:", nrow(data), "\n")
  cat("Nombre de variables:", ncol(data), "\n")
  cat("Variables:\n")
  print(colnames(data))
  
  cat("\n---\n")

  # 2. Résumé des résultats des modèles
  cat("### Résultats des modèles de Marketing Mix\n")
  # grid.arrange(result$spend_distribution, result$grp_distribution, ncol = 1)
  
  # Modèle de base
  cat("\n**Modèle de base**:\n")
  print(summary(model_results$basic_model))
  
  # Modèle avancé
  cat("\n**Modèle avancé**:\n")
  print(summary(model_results$advanced_model))
  
  cat("\n---\n")

  # 3. Analyse de l'efficacité
  cat("### Analyse de l'efficacité des canaux publicitaires\n")
  print(effectiveness_results)

  cat("\n---\n")

  # 4. Visualisations
  # Graphique des ventes attribuées par canal
  sales_attributed <- data.frame(
    Channel = c("SMS", "Newspaper", "Radio", "TV", "Internet"),
    Sales = c(
      sum(data$Sales_Attributable_SMS, na.rm = TRUE),
      sum(data$Sales_Attributable_Newspaper, na.rm = TRUE),
      sum(data$Sales_Attributable_Radio, na.rm = TRUE),
      sum(data$Sales_Attributable_TV, na.rm = TRUE),
      sum(data$Sales_Attributable_Internet, na.rm = TRUE)
    )
  )

  ggplot(sales_attributed, aes(x = Channel, y = Sales, fill = Channel)) +
    geom_bar(stat = "identity") +
    labs(title = "Ventes attribuées par canal publicitaire",
         x = "Canal",
         y = "Ventes attribuées") +
    theme_minimal()

  # Graphique des ROI
  roi_plot <- ggplot(effectiveness_results, aes(x = Channel, y = ROI, fill = Channel)) +
    geom_bar(stat = "identity") +
    labs(title = "Retour sur investissement (ROI) par canal publicitaire",
         x = "Canal",
         y = "ROI (%)") +
    theme_minimal()

  # print(roi_plot)

  # Sauvegarde du rapport
  ggsave("report.pdf")
  cat("Rapport généré et sauvegardé sous le nom 'report.pdf'.\n")
}

# generate_report(data, result, roi_results)


run_analysis <- function(file_path) {
  
  # 1. Importer et nettoyer les données
  data <- import_and_clean_data(file_path)
  
  # 2. Analyser les données
  data <- prepare_data(data)
  
  # 3. Construire le modèle de marketing mix
  result <- analyze_channels(data)
  grid.arrange(result$spend_distribution, result$grp_distribution, ncol = 1)
  print(result$correlations)

  # 4. Marketing Mix Modeling
  model_results <- build_mmm(data)
  
  # 5. Analyser l'efficacité des canaux publicitaires
  effectiveness_results <- analyze_effectiveness(data, model_results)
  
  # 6. Générer le rapport
  generate_report(data, model_results, effectiveness_results)
  
  cat("Analyse complète terminée. Rapport généré avec succès.\n")
}

run_analysis('MMM_data.csv')