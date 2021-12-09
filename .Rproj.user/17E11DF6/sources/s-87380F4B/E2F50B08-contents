library(shiny)
library(tidyverse)
library(colourpicker)

ggplot2::theme_set(theme_minimal(base_size = 14))
ggplot2::theme_update(panel.grid = element_blank())


hex_hue_shade <- function(hue, shade){
    if(shade >= 100){
        return(hue)
    }
    
    if(str_length(shade)==1){
        shade <- paste0(0,shade)
    }
    
    return(paste0(hue, shade))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Visual Encodings of Data"),
    
    tabsetPanel(
        tabPanel(
            title = "Position",
            splitLayout(
                numericInput(
                    inputId = "x_pos",
                    label = "X Value",
                    min = 0, max = 100, value = 50, step = 5
                ),
                numericInput(
                    inputId = "y_pos",
                    label = "Y Value",
                    min = 0, max = 100, value = 50, step = 5
                )
            ),
            plotOutput("position_plot")
        ),
        tabPanel(
            title = "Length",
            splitLayout(
                numericInput(
                    inputId = "A_bar",
                    label = "A Value",
                    min = 0, max = 100, value = 25, step = 5
                ),
                numericInput(
                    inputId = "B_bar",
                    label = "B Value",
                    min = 0, max = 100, value = 50, step = 5
                ),
                numericInput(
                    inputId = "C_bar",
                    label = "C Value",
                    min = 0, max = 100, value = 75, step = 5
                )
            ),
            plotOutput("length_plot")
        ),
        tabPanel(
            title = "Color",
            column(
                width = 4,
                fluidRow(
                    column(
                        width = 6,
                        colourInput(
                            inputId = "col_A_hue",
                            label = "Group A Hue",
                            value = "#984EA3",
                            showColour = "both",
                            palette = "limited",
                            allowedCols = RColorBrewer::brewer.pal(9,"Set1")
                        )),
                    column(
                        width = 6,
                        numericInput(
                            inputId = "col_A_val",
                            label = "Shade",
                            min = 0, max = 100, value = 100, step = 5
                        ))
                ),
                fluidRow(
                    column(
                        width = 6,
                        colourInput(
                            inputId = "col_B_hue",
                            label = "Group B Hue",
                            value = "#FF7F00",
                            showColour = "both",
                            palette = "limited",
                            allowedCols = RColorBrewer::brewer.pal(9,"Set1")
                        )),
                    column(
                        width = 6,
                        numericInput(
                            inputId = "col_B_val",
                            label = "Shade",
                            min = 0, max = 100, value = 75, step = 5
                        ))
                ),
                fluidRow(
                    column(
                        width = 6,
                        colourInput(
                            inputId = "col_C_hue",
                            label = "Group C Hue",
                            value = "#377EB8",
                            showColour = "both",
                            palette = "limited",
                            allowedCols = RColorBrewer::brewer.pal(9,"Set1")
                        )),
                    column(
                        width = 6,
                        numericInput(
                            inputId = "col_C_val",
                            label = "Shade",
                            min = 0, max = 100, value = 50, step = 5
                        ))
                ),
                fluidRow(
                    column(
                        width = 6,
                        colourInput(
                            inputId = "col_D_hue",
                            label = "Group D Hue",
                            value = "#999999",
                            showColour = "both",
                            palette = "limited",
                            allowedCols = RColorBrewer::brewer.pal(9,"Set1")
                        )),
                    column(
                        width = 6,
                        numericInput(
                            inputId = "col_D_val",
                            label = "Shade",
                            min = 0, max = 100, value = 25, step = 5
                        ))
                )),
            column(
                width = 8,
                plotOutput("color_plot")
            )

            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$position_plot <- renderPlot({
        
        xval <-  input$x_pos
        yval <-  input$y_pos
        
        p <- ggplot(
            data = data.frame(xval = xval, yval = yval),
            aes(x = xval, y = yval)) + 
            geom_point(size = 6) + 
            labs(x = "X Value", y = "Y Value") +
            xlim(0,100) + ylim(0,100)
        
        p
    })
    output$length_plot <- renderPlot({
        
        aval <- input$A_bar
        bval <- input$B_bar
        cval <- input$C_bar
        
        p <- ggplot(
            data = data.frame(A = aval, B = bval, C = cval) %>% pivot_longer(cols = 1:3),
            aes(x = name, y = value)) + geom_col() + 
            labs(x = "Categorical Group", y = "Numeric Value") +
            ylim(0,100)
        
        p
    })
    output$color_plot <- renderPlot({
        
        # aval <- input$col_A_val
        # bval <- input$col_B_val
        # cval <- input$col_C_val
        # dval <- input$col_D_val
        
        ahue <- hex_hue_shade(input$col_A_hue, input$col_A_val)
        bhue <- hex_hue_shade(input$col_B_hue, input$col_B_val)
        chue <- hex_hue_shade(input$col_C_hue, input$col_C_val)
        dhue <- hex_hue_shade(input$col_D_hue, input$col_D_val)
        
        # cat(paste(ahue, bhue, chue, dhue))
        
        p <- ggplot(
            data = data.frame(X=LETTERS[1:4]),
            aes(x = X, y = 100)) + geom_col(fill = c(ahue, bhue, chue, dhue)) +
            labs(x = "Group", y = "") +
            ylim(0,100)

        p
        
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
