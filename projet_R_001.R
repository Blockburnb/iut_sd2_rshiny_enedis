install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
# Importer les bibliothèques nécessaires
library(httr)
library(jsonlite)
library(dplyr)

# Charger les codes postaux depuis le fichier CSV
adresses_69 <- read.csv("C:/Users/nalem/Downloads/adresses-69.csv/adresses-69.csv", sep = ';', stringsAsFactors = FALSE)

# Extraire les codes postaux uniques
codes_postaux <- unique(adresses_69$code_postal)

# URL de base pour les logements existants et neufs
base_url_existants <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
base_url_neufs <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialiser deux dataframes vides pour stocker les résultats
df_existants <- data.frame()
df_neufs <- data.frame()

### Partie 1 : Récupérer les logements existants

for (code_postal in codes_postaux) {
  # Paramètres pour logements existants
  params_existants <- list(
    page = 1,
    size = 10000,
    select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE",
    q = as.character(code_postal),
    q_fields = "Code_postal_(BAN)",
    qs = "Date_réception_DPE:[2021-01-01 TO 2024-12-31]"
  )
  
  # Encodage de l'URL pour logements existants
  url_encoded_existants <- modify_url(base_url_existants, query = params_existants)
  
  # Effectuer la requête GET pour les logements existants
  response_existants <- GET(url_encoded_existants)
  
  if (status_code(response_existants) == 200) {
    content_existants <- fromJSON(rawToChar(response_existants$content), flatten = TRUE)
    
    if (!is.null(content_existants$result)) {
      df_temp <- as.data.frame(content_existants$result)
      df_existants <- bind_rows(df_existants, df_temp)
    }
  }
}
summary(df_existants)
# Exporter les logements existants vers un fichier CSV
write.csv(df_existants, "existants_69.csv", row.names = FALSE)