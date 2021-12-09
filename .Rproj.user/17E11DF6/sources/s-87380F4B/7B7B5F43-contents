library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel(
                title = "ggplot",
                uiOutput("gg_plot")
                ),
            tabPanel(
                title = "plotly",
                uiOutput("pl_otly")
                ),
            tabPanel(
                title = "data",
                htmlOutput("data_code")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- ggplot2::diamonds %>% 
        group_by(cut) %>% 
        summarize(price = mean(price), 
                  table = mean(table), 
                  depth = mean(depth))
    
    output$data_code <- renderUI({
        column(
            width = 12,
            code("df <- ggplot2::diamonds %>%", br(),
                 "group_by(cut) %>% ", br(),
                 "summarize(price = mean(price),", br(),
                 "table = mean(table),", br(),
                 "depth = mean(depth))"),
            br(),
            br(),
            renderTable(df)
        )
        })
    
    
    
    output$gg_plot <- renderUI(
        
        column(
            width = 12,
            code(
                "df %>% ggplot(", br(),
                "aes(x = cut, y = price)) +", br(),
                "geom_col()"
            ),
            renderPlot(
                ggplot(
                    data = df,
                    aes(x = cut, y = price)
                ) +
                    geom_col()
            )
        )
        
    )
        
    
    output$pl_otly <- renderUI(
        
        column(
            width = 12,
            code(
                "df %>% plot_ly(", br(),
                    "x = ~cut, y = ~price,",br(),
                    "type = 'bar')"
            ),
            renderPlotly(
                plot_ly(
                    data = df,
                    x = ~cut,
                    y = ~price,
                    type = "bar"
                )
            )
        )
        
        
    )

}



# Run the application 
shinyApp(ui = ui, server = server)
