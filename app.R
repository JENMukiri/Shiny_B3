library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
bcl <- read_csv("bcl-data.csv")
options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)


#in the UI I added a logo from the internet https://en.wikipedia.org/wiki/File:BC_Liquor_Store_logo.svg changing the format from svg to png
# I also changed the theme of the shiny app to united from https://rstudio.github.io/shinythemes/
# I adjusted the layout for the app to have panels for viewing the plot, table and downloading data in csv format
# I also included the table to be interactive
# In summary all these additions help the user when interacting with the app, the can view the table and plots different, the can download data from which they selected specific beverage
ui <- fluidPage( theme = shinytheme("united"),
                 img(src='myImage.png', height="25%", width="25%",align = "right"),
    titlePanel("BCL app"),
    "This app allows you to explore the bc liqour database and the types of alcoholic and non-alcholic drinks they sell,
      the alchol content, price and countries or origin.",
  tags$br(),
  tags$br(),
  
  titlePanel("App selections"), 
  
  
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
        
        tabsetPanel(
          tabPanel("Plot", uiOutput("my_random")),
          tabPanel("Table", DTOutput("my_table")),
          tabPanel("Download Data",  downloadButton("downloadData"))
         )
      )
  )
)

#in the server i changed the table to be interactive this was possible with the DT package making the app interactive
# I also included the download function which allows the user to create a csv based on the beverage type they have selected

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