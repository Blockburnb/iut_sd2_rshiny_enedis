# Installer les packages requis (à exécuter une seule fois)
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("leaflet")
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("shinythemes")
#install.packages("jsonlite")
#install.packages("shinyjs")

# Charger les bibliothèques nécessaires
options(shiny.maxRequestSize = 1000 * 1024^2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyjs)

# CSS personnalisé avec style visuel plus détaillé
css_custom <- "
  body {
    font-family: Arial, sans-serif;
    background-color: #f5f5f5;
  }
  .navbar, .sidebar {
    background-color: #333333;
    color: #FFFFFF;
  }
  .value-box {
    font-size: 20px;
    font-weight: bold;
  }
"

# Interface utilisateur
ui <- navbarPage(
  title = "Mon Application Shiny",
  
  # Sélecteur de thème
  tabPanel("Paramètres",
           sidebarPanel(
             selectInput("theme", "Choisissez un thème", choices = c("cosmo", "flatly", "cerulean")),
             actionButton("apply_theme", "Appliquer le thème")
           ),
           mainPanel(
             h4("Sélection de thème dynamique pour l'application")
           )
  ),
  
  # Onglet Contexte
  tabPanel("Contexte",
           sidebarPanel(
             fileInput("file_input", "Télécharger le fichier CSV", accept = ".csv"),
             checkboxInput("show_data", "Afficher les données", value = FALSE),
             selectInput("var_select", "Variable pour les KPI", choices = NULL),
             h5("Dernière mise à jour :"), textOutput("last_update")
           ),
           mainPanel(
             h3("Présentation des Données"),
             conditionalPanel(
               condition = "input.show_data == true",
               dataTableOutput("data_table")
             )
           )
  ),
  
  # Onglet Visualisations
  tabPanel("Visualisations",
           sidebarPanel(
             sliderInput("obs", "Nombre d'observations:", min = 10, max = 100, value = 50),
             selectInput("plot_type", "Type de graphique", choices = c("Histogramme", "Boîte à moustache", "Diagramme", "Nuage de points")),
             radioButtons("color_scheme", "Choisir un schéma de couleur :", choices = c("Bleu", "Rouge", "Vert")),
             downloadButton("downloadPlot", "Télécharger le graphique")
           ),
           mainPanel(
             h3("Indicateurs Clés"),
             fluidRow(
               valueBoxOutput("kpi1", width = 6),
               valueBoxOutput("kpi2", width = 6)
             ),
             plotOutput("plot")
           )
  ),
  
  # Onglet Carte
  tabPanel("Carte",
           sidebarPanel(
             checkboxInput("show_markers", "Afficher les markers", value = TRUE)
           ),
           mainPanel(
             leafletOutput("map")
           )
  ),
  
  # Onglet Analyse Avancée
  tabPanel("Analyse Avancée",
           sidebarPanel(
             selectInput("x_var", "Variable X", choices = NULL),
             selectInput("y_var", "Variable Y", choices = NULL),
             actionButton("calculate", "Calculer la corrélation"),
             downloadButton("downloadData", "Télécharger les données")
           ),
           mainPanel(
             textOutput("correlation_output"),
             plotOutput("scatter_plot")
           )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Variable réactive pour stocker les données
  data_reactive <- reactiveVal()
  last_update <- reactiveVal()
  
  # Charger les données depuis un fichier CSV
  observeEvent(input$file_input, {
    req(input$file_input)
    data <- read.csv(input$file_input$datapath, header = TRUE, sep = ";")
    data_reactive(data)
    last_update(Sys.time())
  })
  
  # Afficher la date de la dernière mise à jour
  output$last_update <- renderText({
    req(last_update())
    format(last_update(), "%d %b %Y %H:%M:%S")
  })
  
  # Mettre à jour les options des sélections de variables
  observe({
    req(data_reactive())
    updateSelectInput(session, "var_select", choices = names(data_reactive()))
    updateSelectInput(session, "x_var", choices = names(data_reactive()))
    updateSelectInput(session, "y_var", choices = names(data_reactive()))
  })
  
  # Afficher le tableau de données
  output$data_table <- renderDataTable({
    req(data_reactive())
    datatable(data_reactive())
  })
  
  # Calculs de KPI avec des icônes
  output$kpi1 <- renderValueBox({
    req(input$var_select)
    valueBox(round(mean(data_reactive()[[input$var_select]], na.rm = TRUE), 2), 
             "Moyenne", icon = icon("chart-line"))
  })
  output$kpi2 <- renderValueBox({
    req(input$var_select)
    valueBox(round(median(data_reactive()[[input$var_select]], na.rm = TRUE), 2), 
             "Médiane", icon = icon("balance-scale"))
  })
  
  # Graphiques dynamiques avec schémas de couleurs
  output$plot <- renderPlot({
    req(input$plot_type, input$var_select, data_reactive())
    data <- data_reactive()[1:input$obs, ]
    color <- ifelse(input$color_scheme == "Bleu", "blue", ifelse(input$color_scheme == "Rouge", "red", "green"))
    
    if (input$plot_type == "Histogramme") {
      ggplot(data, aes_string(input$var_select)) + geom_histogram(fill = color)
    } else if (input$plot_type == "Boîte à moustache") {
      ggplot(data, aes_string(x = "1", y = input$var_select)) + geom_boxplot(fill = color)
    } else if (input$plot_type == "Diagramme") {
      ggplot(data, aes_string(x = seq_along(data[[input$var_select]]), y = input$var_select)) + geom_line(color = color)
    } else if (input$plot_type == "Nuage de points") {
      ggplot(data, aes_string(x = input$var_select, y = input$var_select)) + geom_point(color = color)
    }
  })
  
  # Carte interactive avec Leaflet
  output$map <- renderLeaflet({
    req(data_reactive())
    leaflet(data_reactive()) %>%
      addTiles() %>%
      { if (input$show_markers) addMarkers(lng = ~lon,lat= ~lat, popup = ~as.character(name)) else . }
  })
  
  # Calcul de corrélation et régression linéaire
  observeEvent(input$calculate, {
    req(input$x_var, input$y_var)
    output$correlation_output <- renderText({
      cor_val <- cor(data_reactive()[[input$x_var]], data_reactive()[[input$y_var]], use = "complete.obs")
      paste("Corrélation:", round(cor_val, 2))
    })
    
    output$scatter_plot <- renderPlot({
      ggplot(data_reactive(), aes_string(x = input$x_var, y = input$y_var)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        labs(title = "Régression Linéaire", x = input$x_var, y = input$y_var)
    })
  })
  
  # Téléchargement des graphiques et données
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("graphique-", Sys.Date(), ".png", sep="") },
    content = function(file) {
      ggsave(file, plot = last_plot())
    }
  )
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
