# Charger les bibliothèques nécessaires
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(ggplot2)
library(plotly)
library(shinythemes)

# CSS pour la charte visuelle
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

# UI - Interface utilisateur
ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "Mon Application Shiny",
  
  # Page Contexte
  tabPanel("Contexte",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 h3("Filtrer les Données"),
                 selectInput("var_select", "Variable", choices = names(df)),
                 checkboxInput("show_table", "Afficher les données", TRUE)
               ),
               mainPanel(
                 h2("Contexte des Données"),
                 img(src = "data_image.jpg", height = "200px"),
                 p("Voici une présentation générale des données utilisées dans cette application."),
                 conditionalPanel(
                   condition = "input.show_table == true",
                   dataTableOutput("data_table")
                 )
               )
             )
           )
  ),
  
  # Page Visualisations
  tabPanel("Visualisations",
           sidebarLayout(
             sidebarPanel(
               sliderInput("obs", "Nombre d'observations:", 1, nrow(df), 50),
               radioButtons("plot_type", "Type de graphique:", 
                            choices = c("Histogramme", "Boîte à moustache", "Diagramme", "Nuage de points")),
               downloadButton("downloadPlot", "Télécharger le graphique")
             ),
             mainPanel(
               plotOutput("plot"),
               h3("KPI"),
               valueBoxOutput("kpi1"),
               valueBoxOutput("kpi2")
             )
           )
  ),
  
  # Page Carte
  tabPanel("Carte",
           leafletOutput("map"),
           sidebarLayout(
             sidebarPanel(
               h3("Paramètres de Carte"),
               checkboxInput("show_markers", "Afficher les markers", TRUE)
             ),
             mainPanel(
               h2("Carte Interactive"),
               p("Utilisez cette carte pour explorer les emplacements des données.")
             )
           )
  ),
  
  # Page Analyse Avancée
  tabPanel("Analyse Avancée",
           sidebarLayout(
             sidebarPanel(
               selectInput("x_var", "Variable X", choices = names(df)),
               selectInput("y_var", "Variable Y", choices = names(df)),
               actionButton("calculate", "Calculer la Corrélation et Régression"),
               downloadButton("downloadData", "Exporter les Données")
             ),
             mainPanel(
               plotOutput("scatter_plot"),
               verbatimTextOutput("correlation_output")
             )
           )
  )
)

# Serveur - Logiciel d'application
server <- function(input, output, session) {
  
  # Tableau de données
  output$data_table <- renderDataTable({
    datatable(df)
  })
  
  # KPI
  output$kpi1 <- renderValueBox({
    valueBox(value = round(mean(df[[input$var_select]], na.rm = TRUE), 2), "Moyenne")
  })
  output$kpi2 <- renderValueBox({
    valueBox(value = round(median(df[[input$var_select]], na.rm = TRUE), 2), "Médiane")
  })
  
  # Graphique dynamique
  output$plot <- renderPlot({
    req(input$plot_type)
    data <- df[1:input$obs, ]
    if (input$plot_type == "Histogramme") {
      ggplot(data, aes(x = !!sym(input$var_select))) + geom_histogram()
    } else if (input$plot_type == "Boîte à moustache") {
      ggplot(data, aes(y = !!sym(input$var_select))) + geom_boxplot()
    } else if (input$plot_type == "Diagramme") {
      ggplot(data, aes(x = seq_along(!!sym(input$var_select)), y = !!sym(input$var_select))) + geom_line()
    } else if (input$plot_type == "Nuage de points") {
      ggplot(data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) + geom_point()
    }
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    leaflet(df) %>%
      addTiles() %>%
      { if (input$show_markers) addMarkers(~lon, ~lat, popup = ~as.character(name)) else . }
  })
  
  # Corrélation et régression
  observeEvent(input$calculate, {
    output$correlation_output <- renderText({
      cor_val <- cor(df[[input$x_var]], df[[input$y_var]], use = "complete.obs")
      paste("Corrélation:", round(cor_val, 2))
    })
    output$scatter_plot <- renderPlot({
      ggplot(df, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
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
      write.csv(df, file)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)
