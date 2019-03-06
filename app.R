#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Ultimatum Game"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("x",
                     "Player 1 offers x=",
                     min = 0,
                     max = 1,
                     value = 0,
                     step = 0.05,
                     animate = T
                     ),
         sliderInput("b2",
                     "b2 = ",
                     min = 0,
                     max = 5,
                     value = 1,
                     step = 0.1,
                     animate = T),
         sliderInput("b1",
                     "b1 = ",
                     min = 0.0,
                     max = 5.0,
                     value = 1.0,
                     step = 0.1,
                     animate = T)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("Player2"),
         plotOutput("Player1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  p = ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
      xlim(0, 5) + ylim(0, 1) + ylab('x')
  
  ub_2_1 = function(b2) ifelse(b2>1, NA, 1)
  ub_2_2 = function(b2) ifelse(b2<1, NA, b2/(2*b2-1))
  lb_2   = function(b2) b2/(1+2*b2)
  
  ub_1   = function(b1) (1+b1)/(1+2*b1)
  lb_1_1 = function(b1) ifelse(b1>1, NA, 0)
  lb_1_2 = function(b1) ifelse(b1<1, NA, (b1-1)/(2*b1-1))
  
  
  
  output$Player2 = renderPlot({
    
    u2 = input$x - input$b2*abs(2*input$x-1) %>% round(5)
    if(u2 > 0){
      col = 'blue'
      size = u2 * 25
    }else if(u2 < 0){
      col = 'red'
      size = abs(u2) * 25
    }else{
      col = 'green'
      size = 10
    }
    
    p +
      stat_function(fun = ub_2_1) +
      stat_function(fun = ub_2_2) +
      stat_function(fun = lb_2) +
      geom_point(aes(input$b2, input$x), col = col, size = size, show.legend = F, tag = u2) +
      xlab("b2")

    
  })
 
  output$Player1 = renderPlot({
    
    u1 = 1 - input$x - input$b1*abs(2*input$x-1)%>% round(5)
    if(u1 > 0){
      col = 'blue'
      size = u1 * 25
    }else if(u1 < 0){
      col = 'red'
      size = abs(u1) * 25
    }else{
      col = 'green'
      size = 10
    }
    
    p +
      stat_function(fun = ub_1) +
      stat_function(fun = lb_1_1) +
      stat_function(fun = lb_1_2) +
      geom_point(aes(input$b1, input$x), col = col, size = size, show.legend = F, tag = u1) +
      xlab("b1")
    

    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

