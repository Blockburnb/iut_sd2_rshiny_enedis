#install.packages("httr")
#install.packages("jsonlite")
#install.packages("dplyr")
# Importer les bibliothèques nécessaires
library(httr)
library(jsonlite)
library(dplyr)

# Charger les codes postaux depuis le fichier CSV
adresses_69 <- read.csv("adresses-69.csv", sep = ';', stringsAsFactors = FALSE)

# Extraire les codes postaux uniques
codes_postaux <- unique(adresses_69$code_postal)

# URL de base pour les logements existants et neufs
base_url_existants <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
base_url_neufs <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"

# Initialiser deux dataframes vides pour stocker les résultats
df_existants <- data.frame()
df_neufs <- data.frame()

### Partie 1 : Récupérer les logements existants
numeric_range <- 2021:as.numeric(format(Sys.Date(), format="%Y"))
for(i in numeric_range) { #boucle par année
  for (code_postal in codes_postaux) {#boucle par code postaux
    # Paramètres pour logements existants
    params <- list(
      page = 1,
      size = 10000,
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE",
      q = as.character(code_postal),
      q_fields = "Code_postal_(BAN)",
      #qs = paste("Date_réception_DPE:[",i,"-01-01 TO ",as.numeric(format(Sys.Date(), format="%Y")),"-12-31]")
      qs = paste0("Date_réception_DPE:[",i,"-01-01 TO ",i,"-12-31]")
    )
    
    # Encodage de l'URL pour logements existants
    url_encoded_existants <- modify_url(base_url_existants, query = params)
    url_encoded_neufs <- modify_url(base_url_neufs, query = params)
    
    # Effectuer la requête GET pour les logements existants
    response_existants <- GET(url_encoded_existants)
    response_neufs <- GET(url_encoded_neufs)
    
    if (status_code(response_existants) == 200) {
      content_existants <- fromJSON(rawToChar(response_existants$content), flatten = TRUE)
      
      if (!is.null(content_existants$result)) {
        df_temp <- as.data.frame(content_existants$result)
        df_existants <- bind_rows(df_existants, df_temp)
      }
    }
    if (status_code(response_neufs) == 200) {
      content_neufs <- fromJSON(rawToChar(response_neufs$content), flatten = TRUE)
      
      if (!is.null(content_neufs$result)) {
        df_temp2 <- as.data.frame(content_neufs$result)
        df_neufs <- bind_rows(df_neufs, df_temp2)
      }
    }
  }
}

#Ajout des colonnes de qualif et création d'un df unique
df_existants$Type_de_logement <- "existant"
df_neufs$Type_de_logement <- "neuf"
df <- bind_rows(df_existants,df_neufs)

# Exporter les logements vers un fichier CSV
write.csv(df, "logements_69.csv", row.names = FALSE)