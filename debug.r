# Fonction de débogage pour l'import
debug_import <- function(file_path) {
  # Essayer de lire les premières lignes du fichier
  cat("Tentative de lecture du fichier...\n")
  raw_lines <- readLines(file_path, n = 5)
  cat("Premières lignes du fichier:\n")
  print(raw_lines)
  
  # Tenter différentes méthodes d'import
  cat("\nTentative d'import avec read.csv standard...\n")
  data1 <- try(read.csv(file_path, sep = ",", header = TRUE))
  if(!inherits(data1, "try-error")) {
    cat("Nombre de colonnes:", ncol(data1), "\n")
    cat("Noms des colonnes:\n")
    print(names(data1))
  }
  
  # Tenter avec read.table
  cat("\nTentative d'import avec read.table...\n")
  data2 <- try(read.table(file_path, sep = ",", header = TRUE))
  if(!inherits(data2, "try-error")) {
    cat("Nombre de colonnes:", ncol(data2), "\n")
    cat("Noms des colonnes:\n")
    print(names(data2))
  }
  
  return(list(raw_lines = raw_lines,
             data1 = if(!inherits(data1, "try-error")) data1 else NULL,
             data2 = if(!inherits(data2, "try-error")) data2 else NULL))
}

# Version modifiée de la fonction d'import
import_and_clean_data <- function(file_path) {
  # Lire le fichier avec read.table pour plus de flexibilité
  data <- read.table(file_path, 
                    sep = ",", 
                    header = TRUE, 
                    stringsAsFactors = FALSE,
                    na.strings = c("", "NA", "NULL"),
                    fill = TRUE,
                    quote = "")
  
  # Afficher la structure pour vérification
  cat("Structure des données importées:\n")
  str(data)
  
  # Si les colonnes ne sont pas correctement nommées, les renommer
  if(ncol(data) == 18) {  # Vérifie si nous avons le bon nombre de colonnes
    names(data) <- c("DATE", "DEMAND", "CPI", "CCI", "PPI", "UNIT_PRICE", "POS_SUPPLY",
                     "SALES", "AD_SMS", "AD_NEWSPAPER", "AD_RADIO", "AD_TV", "AD_INTERNET",
                     "GRP_NEWSPAPER", "GRP_SMS", "GRP_RADIO", "GRP_INTERNET", "GRP_TV")
  } else {
    stop("Le nombre de colonnes ne correspond pas à ce qui est attendu")
  }
  
  # Convertir la colonne DATE
  data$DATE <- as.Date(data$DATE, format="%d/%m/%y")
  
  # Convertir les colonnes numériques
  numeric_cols <- names(data)[2:ncol(data)]
  for(col in numeric_cols) {
    data[[col]] <- as.numeric(gsub(",", ".", data[[col]]))
  }
  
  return(data)
}

# Fonction principale de test
test_import <- function(file_path) {
  # D'abord, exécuter le débogage
  debug_results <- debug_import(file_path)
  
  # Ensuite, tenter l'import avec la nouvelle fonction
  cat("\nTentative d'import avec la nouvelle fonction...\n")
  data <- try(import_and_clean_data(file_path))
  
  if(!inherits(data, "try-error")) {
    cat("\nImport réussi!\n")
    cat("Dimensions du dataset:", dim(data), "\n")
    cat("Premières lignes:\n")
    print(head(data))
    return(data)
  } else {
    cat("\nL'import a échoué. Veuillez vérifier les messages d'erreur ci-dessus.\n")
    return(NULL)
  }
}

