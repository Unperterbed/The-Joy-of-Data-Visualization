
library(shiny)
library(tidyverse)

    dates <- data.frame(
      name = c("jahr10", "jahr9", "jahr8", "jahr7", "jahr6", "jahr5", "jahr4", "jahr3", "jahr2", "jahr1", "jahr"),
      year = as.character((as.integer(format(Sys.Date(), "%Y"))-10):as.integer(format(Sys.Date(), "%Y")))
    )
  
    
    df %>% 
      pivot_longer(cols = 2:12) %>% 
      left_join(dates, by = "name")

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarPanel(
    checkboxGroupInput(
      inputId = 'checkboxes',
      label = "Years",
      choices = years
    )
  ),
  mainPanel(
    textOutput("text"),
    plotOutput("barplot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$text <- renderText(
    input$checkboxes
  )
  
  output$barplot <- renderPlot(
    ggplot2::diamonds %>% 
      count(cut,clarity) %>% 
      as.data.frame() %>% 
      filter(clarity %in% input$checkboxes) %>% 
      ggplot(aes(x=cut, y = n, fill = clarity)) + geom_col(position = "dodge")
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
