# Load required libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(lubridate)
library(forecast)

# 1. Data Import and Initial Processing
import_and_clean_data <- function(file_path) {
  # Read CSV with specific handling for your file format
  data <- read.csv(file_path, sep = ";", header = TRUE, stringsAsFactors = FALSE)
  
  # Remove first two columns that contain metadata
  data <- data[, -c(1:3)]
  
  # Clean column names
  names(data) <- c("DATE", "DEMAND", "CPI", "CCI", "PPI", "UNIT_PRICE", "POS_SUPPLY",
                   "SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET",
                   "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV")
  
  # Convert date format (assuming your dates are in DD/MM/YY format)
  data$DATE <- as.Date(data$DATE, format="%d/%m/%y")
  
  # Convert numeric columns and handle any commas in numbers
  numeric_cols <- names(data)[2:length(names(data))]
  for(col in numeric_cols) {
    data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
  }
  
  # Remove any rows with all NA values
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  
  return(data)
}

# 2. Feature Engineering and Data Preparation
prepare_data <- function(data) {
  data %>%
    mutate(
      # Add temporal features
      Month = month(DATE),
      Year = year(DATE),
      Quarter = quarter(DATE),
      
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

# 3. Exploratory Data Analysis
analyze_channels <- function(data) {
  # Advertising expenses analysis
  expenses_long <- data %>%
    select(DATE, AD_SMS, AD_NEWSPAPER, AD_RADIO, AD_TV, AD_INTERNET) %>%
    gather(key = "Channel", value = "Spend", -DATE) %>%
    mutate(Channel = gsub("AD_", "", Channel))
  
  # Create spend distribution plot
  spend_plot <- ggplot(expenses_long, aes(x = Channel, y = Spend, fill = Channel)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of Advertising Spend by Channel",
         y = "Spend ($)")
  
  # GRP analysis
  grp_long <- data %>%
    select(DATE, starts_with("GRP_")) %>%
    gather(key = "Channel", value = "GRP", -DATE) %>%
    mutate(Channel = gsub("GRP_", "", Channel))
  
  # Create GRP distribution plot
  grp_plot <- ggplot(grp_long, aes(x = Channel, y = GRP, fill = Channel)) +
    geom_boxplot() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Distribution of GRP by Channel")
  
  # Correlation analysis
  ad_cols <- c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET")
  correlation_matrix <- cor(data[, ad_cols])
  
  list(
    spend_distribution = spend_plot,
    grp_distribution = grp_plot,
    correlations = correlation_matrix
  )
}

# 4. Marketing Mix Modeling
build_mmm <- function(data) {
  # Basic model with advertising expenses
  basic_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET,
                    data = data)
  
  # Advanced model including market conditions and seasonality
  advanced_model <- lm(SALES ~ AD_SMS + AD_NEWSPAPER + AD_RADIO + AD_TV + AD_INTERNET +
                      CPI + CCI + PPI + UNIT_PRICE + factor(Month),
                      data = data)
  
  # Calculate elasticities for each channel
  means <- colMeans(data[, c("SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET")])
  
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

# 5. Channel Effectiveness Analysis
analyze_effectiveness <- function(data, model_results) {
  # Calculate average ROI by channel
  roi_summary <- data %>%
    summarise(
      ROI_SMS = mean(ROI_SMS, na.rm = TRUE),
      ROI_Newspaper = mean(ROI_Newspaper, na.rm = TRUE),
      ROI_Radio = mean(ROI_Radio, na.rm = TRUE),
      ROI_TV = mean(ROI_TV, na.rm = TRUE),
      ROI_Internet = mean(ROI_Internet, na.rm = TRUE)
    ) %>%
    gather(key = "Channel", value = "ROI") %>%
    mutate(Channel = gsub("ROI_", "", Channel))
  
  # Combine ROI with elasticities
  effectiveness <- merge(
    roi_summary,
    model_results$elasticities,
    by = "Channel"
  ) %>%
    mutate(
      Effectiveness_Score = scale(ROI) * scale(Elasticity),
      Recommendation = case_when(
        Effectiveness_Score > 1 ~ "Increase budget",
        Effectiveness_Score > 0 ~ "Maintain budget",
        TRUE ~ "Review strategy"
      )
    )
  
  list(
    roi_by_channel = roi_summary,
    effectiveness = effectiveness,
    model_summary = summary(model_results$advanced_model)
  )
}

# 6. Generate Report
generate_report <- function(data, model_results, effectiveness_results) {
  cat("Marketing Mix Modeling Analysis Report\n")
  cat("=====================================\n\n")
  
  cat("1. Overview\n")
  cat("Total Sales: $", format(sum(data$SALES, na.rm = TRUE), big.mark=","), "\n")
  cat("Total Advertising Spend: $", 
      format(sum(data$AD_SMS + data$AD_NEWSPAPER + data$AD_RADIO + 
                data$AD_TV + data$AD_INTERNET, na.rm = TRUE), big.mark=","), "\n\n")
  
  cat("2. Channel Effectiveness Summary\n")
  print(effectiveness_results$effectiveness)
  cat("\n")
  
  cat("3. Model Performance\n")
  cat("R-squared:", round(summary(model_results$advanced_model)$r.squared, 4), "\n")
  cat("Adjusted R-squared:", round(summary(model_results$advanced_model)$adj.r.squared, 4), "\n\n")
  
  cat("4. Investment Recommendations:\n")
  recommendations <- effectiveness_results$effectiveness %>%
    arrange(desc(Effectiveness_Score))
  print(recommendations)
}

# Fonction principale pour exécuter toute l'analyse
run_analysis <- function(file_path) {
  # Import et nettoyage des données
  data <- import_and_clean_data(file_path)
  print("Données importées avec succès")
  
  # Préparation des données
  clean_data <- prepare_data(data)
  print("Données préparées avec succès")
  
  # Analyse exploratoire
  exploration <- analyze_channels(clean_data)
  print("Analyse exploratoire terminée")
  
  # Construction du modèle
  model_results <- build_mmm(clean_data)
  print("Modèle construit avec succès")
  
  # Analyse d'efficacité
  effectiveness_results <- analyze_effectiveness(clean_data, model_results)
  print("Analyse d'efficacité terminée")
  
  # Génération du rapport
  report <- generate_report(clean_data, model_results, effectiveness_results)
  
  # Retourner tous les résultats
  return(list(
    data = clean_data,
    exploration = exploration,
    model_results = model_results,
    effectiveness = effectiveness_results
  ))
}