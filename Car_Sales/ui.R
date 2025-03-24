#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
car_sales <- read.csv("./car_sales.csv")

fluidPage(
  # Application title
  titlePanel("Car Sales"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId ="Company", 
                  label = "Select a Company:", 
                  choices = unique(car_sales$Company)),
      selectInput(inputId = "Model", 
                  label = "Select a Model:", 
                  choices = NULL)
    ),
    mainPanel(
      textOutput("selected_model")
    )
  )
)
