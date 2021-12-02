library(shiny)
library(tidyverse)
library(shinythemes)
bcl <- read_csv("bcl-data.csv")
options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)


ui <- fluidPage(
    titlePanel("BCL app"),
    "(Explanation goes here) 
  This is an app that helps you explore the alcohol
  content of beverages at BC Liquor stores between
  two price points.",
  tags$br(),
  tags$br(),
  
  titlePanel("Tabsets"), 
  
  sidebarLayout(
      sidebarPanel(
          sliderInput("my_slider", "Select a price range", 
                      min = 0, max = 200, value = c(10, 30)),
          radioButtons(
              "my_radio", "Select beverage type.", 
              choices = unique(bcl$Type)
          )
      ),
      mainPanel(
        img(src='myImage.png', align = "right"),
        tabsetPanel(
          tabPanel("Plot", uiOutput("my_random")),
          tabPanel("Table", tableOutput("my_table")),
          tabPanel("Download Data",  downloadButton("downloadData"))
         )
      )
  )
)


server <- function(input, output, session) {
    
    output$my_random <- renderUI({
        if (input$my_radio == "WINE") {
            plotOutput("my_plot")
        }
    })
    
    filtered <- reactive({
        #print(input$my_slider)
        #print(input$my_radio)
        bcl %>% 
            filter(Price < input$my_slider[2],
                   Price > input$my_slider[1],
                   Type == input$my_radio)
    })
    
    output$my_plot <- renderPlot(
        filtered() %>% 
            ggplot(aes(Alcohol_Content)) +
            geom_histogram()
    )
    
    output$my_table <- renderTable(
        filtered()
    )
    
#Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data.csv")
        },
        content = function(file) {
          write.csv(filtered(), file)
        }
      )
  
}


shinyApp(ui = ui, server = server)