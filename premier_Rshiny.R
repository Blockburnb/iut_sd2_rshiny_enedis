# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)
library(jsonlite)  # Pour lire les données depuis l'API JSON

# CSS personnalisé
css_custom <- "
  body {
    font-family: Arial, sans-serif;
    background-color: #f5f5f5;
  }
  .navbar, .sidebar {
    background-color: #333333;
    color: #FFFFFF;
  }
"

# Fonction pour charger les données depuis une API
load_data_from_api <- function() {
  # Remplacez par l'URL de votre API
  data <- jsonlite::fromJSON("http://example.com/api/data")
  return(data)
}

# Interface utilisateur
ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "Mon Application Shiny",
  
  # Onglet Contexte
  tabPanel("Contexte",
    sidebarPanel(
      actionButton("refresh_data", "Rafraîchir les données"),
      checkboxInput("show_data", "Afficher les données", value = FALSE),
      selectInput("var_select", "Variable pour les KPI", choices = NULL)
    ),
    mainPanel(
      h3("Présentation des Données"),
      img(src = "https://example.com/image.jpg", height = "200px"),
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
      downloadButton("downloadPlot", "Télécharger le graphique")
    ),
    mainPanel(
      h3("Indicateurs Clés"),
      fluidRow(
        valueBoxOutput("kpi1"),
        valueBoxOutput("kpi2")
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
  
  # Variable réactive pour stocker les données chargées de l'API
  data_reactive <- reactiveVal()
  
  # Charger les données initiales depuis l'API au lancement de l'application
  data_reactive(load_data_from_api())
  
  # Mettre à jour les choix des variables pour les sélections après chargement des données
  observe({
    updateSelectInput(session, "var_select", choices = names(data_reactive()))
    updateSelectInput(session, "x_var", choices = names(data_reactive()))
    updateSelectInput(session, "y_var", choices = names(data_reactive()))
  })
  
  # Rafraîchir les données lorsque le bouton est cliqué
  observeEvent(input$refresh_data, {
    new_data <- load_data_from_api()
    data_reactive(new_data)  # Met à jour les données avec les nouvelles données de l'API
  })

  # Afficher le tableau de données
  output$data_table <- renderDataTable({
    datatable(data_reactive())
  })

  # Calculs de KPI
  output$kpi1 <- renderValueBox({
    valueBox(value = round(mean(data_reactive()[[input$var_select]], na.rm = TRUE), 2), "Moyenne")
  })
  output$kpi2 <- renderValueBox({
    valueBox(value = round(median(data_reactive()[[input$var_select]], na.rm = TRUE), 2), "Médiane")
  })

  # Graphiques dynamiques
  output$plot <- renderPlot({
    req(input$plot_type)
    data <- data_reactive()[1:input$obs, ]
    
    if (input$plot_type == "Histogramme") {
      ggplot(data, aes_string(input$var_select)) + geom_histogram()
    } else if (input$plot_type == "Boîte à moustache") {
      ggplot(data, aes_string(x = "1", y = input$var_select)) + geom_boxplot()
    } else if (input$plot_type == "Diagramme") {
      ggplot(data, aes_string(x = seq_along(data[[input$var_select]]), y = input$var_select)) + geom_line()
    } else if (input$plot_type == "Nuage de points") {
      ggplot(data, aes_string(x = input$var_select, y = input$var_select)) + geom_point()
    }
  })

  # Carte interactive avec Leaflet
  output$map <- renderLeaflet({
    leaflet(data_reactive()) %>%
      addTiles() %>%
      { if (input$show_markers) addMarkers(~lon, ~lat, popup = ~as.character(name)) else . }
  })

  # Calcul de corrélation et régression linéaire
  observeEvent(input$calculate, {
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
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("data_selection-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(data_reactive(), file)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)
