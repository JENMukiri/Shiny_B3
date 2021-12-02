library(shiny)
library(tidyverse)
library(shinythemes)
library(DT)
library(shinyBS)

bcl <- read_csv("bcl-data.csv")
global_size = 20
options(shiny.autoreload = TRUE)
#max_price <- max(bcl$Price, na.rm = TRUE)


#In the UI:
# 1. using img and renderImage: I added a logo from the internet https://en.wikipedia.org/wiki/File:BC_Liquor_Store_logo.svg changing the format from svg to png
# 2. shinytheme on the fluid page: I  changed the theme of the shiny app to united from https://rstudio.github.io/shinythemes/
# 3. TtabsetPanel and tabPanel:I adjusted the layout for the app to have panels for viewing the plot, table and downloading data in csv format
# 4. DTOutput:  I also included the table to be interactive
# 5. downloadButton: to download data
# 6. Shiny reset button added to reset data using the shinyBS package
# In summary all these additions help the user when interacting with the app, the can view the table and plots different, the can download data from which they selected specific beverage and the subtype of the beverage
#graphs
ui <- fluidPage( theme = shinytheme("united"),
    titlePanel(img(src='myImage.png', height="100", width="250",align = "right")),
    titlePanel("BCL app"),
    "This app allows you to explore the bc liqour database and the types of alcoholic and non-alcholic drinks they sell,
      the alchol content, price and countries or origin.",
  tags$br(),
  tags$br(),
  
  titlePanel("App selections"), 
  
 
  sidebarLayout(
      sidebarPanel( width = 4,
          sliderInput("my_slider", "Select a price range", 
                      min = 0, max = 200, value = c(10, 30)),
          radioButtons(
              "my_radio", "Select beverage type.", 
              choices = unique(bcl$Type)
          
          ),
          tags$a(href="javascript:history.go(0)", 
                 popify(tags$i(class="fa fa-refresh fa-5x"),
                        title = "Reload", 
                        content = "Click here to restart the Shiny session",
                        placement = "right")),
          tags$br(),
          tags$br(), 
          
      ),
      
      mainPanel(
        
        tabsetPanel(
          tabPanel("Plot", uiOutput("my_random")),
          tabPanel("Table", downloadButton("downloadData"),DTOutput("my_table"))
         )
      )
  )
)

#in the server:
# 1 renderDT: Ichanged the table to be interactive this was possible with the DT package making the app interactive
# 2. downloadHandler:I also included the download function which allows the user to create a csv based on the beverage type they have selected
# 3. Renderplot: I changed the visualization of the graph
server <- function(input, output, session) {
    
  
    output$my_random <- renderUI({
        if (input$my_radio == "WINE") {
            plotOutput("my_plot")
        }
          else if (input$my_radio == "BEER") {
            plotOutput("my_plot")
          } else if 
      (input$my_radio == "SPIRITS") {
        plotOutput("my_plot")
      }
      else if 
      (input$my_radio == "REFRESHMENT") {
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
            ggplot(aes(Alcohol_Content, fill = Subtype)) +
            geom_histogram(bins = 30)+
          ylab("frequency") +
          xlab ("Alcohol contnent %")+
          theme_classic(base_size = global_size)+
          ggtitle(paste("Histogram of alcohol conent based on",input$my_radio))
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