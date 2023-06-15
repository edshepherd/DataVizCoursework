#Set up and imports
setwd("/Users/burrisfaculty/Desktop/DSCode/DSCI605")
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)

veg <- read.csv('kalimati_tarkari_dataset.csv')
veg$Date <- as.Date(veg$Date)
#Basic Plot
ui <- fluidPage(
  selectizeInput(
    inputId = "produce", 
    label = "Select Produce", 
    choices = unique(sort(veg$Commodity)), 
    selected = "",
    multiple = FALSE,
    
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, ...) {
  output$p <- renderPlotly({
    plot_ly(veg, x = ~Date, y = ~Average) %>%
      filter(Commodity %in% input$produce) %>%
      group_by(Commodity) %>%
      add_lines()
  })
}

shinyApp(ui, server)

# Add Panes
ui <- fluidPage(
  titlePanel("Cost of Produce Over Time"),
  selectInput(
    inputId = "produce", 
    label = "Select Produce", 
    choices = unique(sort(veg$Commodity)), 
    selected = "",
    multiple = FALSE,
    
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, ...) {
  output$p <- renderPlotly({
    plot_ly(veg, x = ~Date, y = ~Average) %>%
      filter(Commodity %in% input$produce) %>%
      group_by(Commodity) %>%
      add_lines()
  })
}

shinyApp(ui, server)

#TRY WITH GGPLOT

ui <- fluidPage(
  selectInput(inputId = 'Produce', 
              label = 'Select Produce', 
              choices = unique(sort(veg$Commodity))),
  plotOutput(outputId = 'p')
 
)

server <- function(input, output, session) {
  output$p <- renderPlot({
    veg %>%
      plot_ly(veg, x = ~Date, y = ~Average)%>%
      filter(Commodity %in% input$Produce) %>%
      group_by(Commodity) %>%
    ggplot(aes(x = Date, y = Average))+
      ggtitle(paste("Average Price of", input$Produce, 'Over Time in India') )+
      geom_line(color = 'navy') + 
      xlab('Date') +
      ylab('Average Price per Kilogram') +
      theme(plot.title = (element_text(color = 'cadetblue', size = 24, face = 'bold')),
          axis.title.x = element_text(color = 'cadetblue', size = 20, face = 'bold'),
          axis.title.y = element_text(color = 'cadetblue', size = 20, face = 'bold'),
          text = element_text(size=16, face = 'bold'),panel.background = element_rect(fill = 'white', color = 'darkgrey'),
          panel.grid.major = element_line(color = 'darkgray', linetype = 'dotted'),
          plot.background = element_rect(fill = "lightgrey")
          
          
          )
    
  })
  
}

shinyApp(ui, server)

