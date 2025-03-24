#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

car_sales <- read.csv("./car_sales.csv")
# Define server logic required to draw a histogram
function(input, output, session) {
  observeEvent(input$Company, {
    filtered_models <- unique(car_sales$Model[car_sales$Company == input$Company])
    updateSelectInput(session, "Model", choices = filtered_models)
  })
  
  # Show selected values
  output$selected_model <- renderText({
    paste("You selected:", input$Company, "-", input$Model)
  })
  
}


  
