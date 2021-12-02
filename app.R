library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
bcl <- read_csv("bcl-data.csv")
options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)


ui <- fluidPage( theme = shinytheme("cosmo"),
    titlePanel("BCL app"),
    "This app allows you to explore the bc liqour database and the types of alcoholic and non-alcholic drinks they sell,
      the alcholo content, price and countries or origin.",
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
          tabPanel("Table", DTOutput("my_table")),
          tabPanel("Download Data",  downloadButton("downloadData"))
         )
      )
  )
)


server <- function(input, output, session) {
    
  observeEvent(input$column, {
    updateSelectInput(session, "value", paste("Select", input$column, "value(s)"),
                      choices = sort(unique(mtcars[[input$column]])))
  })
  
  output$selected <- renderText({
    paste0(input$column, ": ", paste(input$value, collapse = ", "))
  })
  
  
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
    
    output$my_table <- renderDT(
      filtered(), options = list(lengthChange = FALSE)
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
  
    output$image <- renderImage({
      filename <- normalizePath(file.path(paste0('www/', myImage, '.png')))
      list(
        src = filename, 
        height = 200
      )
    }, deleteFile = FALSE)    
    
    
}


shinyApp(ui = ui, server = server)