#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(plotly)

car_sales <- read.csv("./car_sales.csv")
original_car_sales <- read.csv("./car_sales.csv")

fluidPage(
  theme = bs_theme(
    bg = "#000544",
    fg = "#18DDEC",
    primary = "#E902D3",
    base_font = font_google("Fuzzy Bubbles", local = TRUE),
    code_font = font_google("Cantarell", local = TRUE),
    heading_font = font_google("Della Respira", local = TRUE)
  ),
  page_fillable(
    # Application title
    tags$head(
      tags$style(HTML("#sidebar {font-size: 14px;  
    /* Adjust the font size */
    }

    /* Reduce font size inside selectInput and checkboxGroupInput */
      .shiny-input-container {
        font-size: 12px !important;  /* Adjust text size */
    }
                    
    /* Reduce font size inside dropdown menus */
      .selectize-input {
        font-size: 14px !important;  /* Adjust font size */
        padding: 1px 2px !important; /* Adjust padding */
    }
                    
    /* Reduce font size for checkbox labels */
      .shiny-input-checkboxgroup label {
        font-size: 12px !important;
    }
    
    .plotly {
       overflow: visible !important;
    }
    "))
    ),
    titlePanel("Who's Buying The Car?"),
    # Create the navigation menu
    navset_tab(
      nav_menu("Car Sales Report",
               nav_panel("Welcome",
                         h2("Welcome to the Car Sales Report!"),
                         h4("This Shiny App allows you to dive into the world of marketing 
      to explore various companies and models of cars, in order to determine which kind of
                            car is selling the best."),
                         img(src = "32070896_Unknown.png", height = "500px"),
                         style = "text-align: center"
               ),
               nav_panel("About the Data", 
                         div(
                           h1("This collection of data, Car Sales Report, came from Kaggle.
                It is a detailed analysis of car sales trends."),
                           style = "text-align: center"),
                         HTML("<p> Kaggle: <a href = 'https://www.kaggle.com/datasets/missionjee/car-sales-report'>Car Sales Report</a></p>"),
                         fluidRow(
                           column(3, align = "left",
                                  h5("The original dataset had 16 columns and 23,906 rows of observations. The data was narrowed down to 8 columns.
                                     The data analyzes the company, model, gender, color, transmission, price, annual income, and dealer region."),
                                  p(HTML("<small>Company: The make of the car.</small>")),
                                  p(HTML("<small>Model: The model of the car.</small>")),
                                  p(HTML("<small>Gender: The gender of the buyer.</small>")),
                                  p(HTML("<small>Color: The color the buyer chose - black, pale white, or red.</small>")),
                                  p(HTML("<small>Transmission: If automatic or manual was purchased.</small>")),
                                  p(HTML("<small>Price: The price ($) the car was sold at.</small>")),
                                  p(HTML("<small>Annual Income: The Annual Income ($) the buyer makes.</small>")),
                                  p(HTML("<small>Dealer Region: Where the car was purchased.</small>"))
                           ),
                           column(9, align = "center",
                                  plotOutput("df_model", width = "800px", height = "700px")  # Adjust height as needed
                           )
                         )
               )   
      ),
      
      
      nav_menu("The Data", 
               nav_panel("Car Company Features", 
                         h6("Choose the car company you would like to explore."),
                         selectInput(inputId = "Company",
                                     label = "Select a Company:",
                                     choices = unique(car_sales$Company),
                                     selected = unique(car_sales$Company)[1]),
                         layout_columns(
                           mainPanel(
                             plotlyOutput("company_box", width = "150%")
                           ),
                           
                           mainPanel(
                             plotlyOutput("company_hist", width = "150%")
                           ),
                           
                           col_widths = c(6, 6)
                         ),
                         layout_columns(
                           mainPanel(
                             plotlyOutput("company_dot", width = "150%")
                           ),
                           
                           mainPanel(
                             plotOutput("company_bar", width = "150%")
                           ),
                           
                           col_widths = c(6, 6)
                         )
               ),
               
               nav_panel("Car Price vs Income of Consumer",
                         p("Car Price vs Income of Consumer"),
                         accordion(
                           accordion_panel("Average Car Price vs Average Income of Consumer",
                                           mainPanel(
                                             width = 12,
                                             textOutput("correlation"),
                                             plotlyOutput("total_scatter")
                                           )
                           ), 
                           # accordion_panel("Total Car Price vs Income of Consumer",
                           #                 mainPanel(
                           #                   width = 12,
                           #                   plotlyOutput("total_box")
                           #                 )
                           #                ),
                           accordion_panel("Car Price vs Income of Consumer by Model",
                                           sidebarLayout(
                                             sidebarPanel(
                                               id = "sidebar",
                                               width = 3,
                                               selectInput(inputId = "Company",
                                                           label = "Select a Company:",
                                                           choices = sort(unique(car_sales$Company)),
                                                           selected = sort(unique(car_sales$Company))[1]),
                                               selectInput(inputId = "Model",
                                                           label = "Select a Model:",
                                                           choices = NULL),
                                               checkboxGroupInput(inputId = "Dealer_Region",
                                                                  label = "Select Dealer Regions:",
                                                                  choices = unique(car_sales$Dealer_Region),
                                                                  selected = unique(car_sales$Dealer_Region))
                                             ),
                                             mainPanel(
                                               width = 9,
                                               textOutput("selected_model"),
                                               textOutput("body_style_text"),
                                               fluidRow(
                                                 column(6, plotlyOutput("price_bar")),  # Car Price Bar Graph
                                                 column(6, plotlyOutput("income_bar"))  # Annual Income Bar Graph
                                               )
                                               
                                               
                                             )
                                           )
                           ),
                         )
               )   
      ),
      
      
      nav_panel(
        "Buyer Preferences",
        p("Sankey Plot of Buyer Preferences"),
        p("The diagram shows the type of transmission and color chosen 
          based on gender and dealer region for each make and model."),
        sidebarLayout(
          sidebarPanel(
            id = "sidebar1",
            width = 3,
            selectInput(inputId = "Company1",
                        label = "Select a Company:",
                        choices = unique(car_sales$Company),
                        selected = unique(car_sales$Company)[1]),
            selectInput(inputId = "Model1",
                        label = "Select a Model:",
                        choices = NULL),
            checkboxGroupInput(inputId = "Dealer_Region1",
                               label = "Select Dealer Regions:",
                               choices = unique(car_sales$Dealer_Region),
                               selected = unique(car_sales$Dealer_Region))
          ),
          
          mainPanel(
            width = 9,
            textOutput("selected_model1"),
            textOutput("body_style_text1"),
            plotlyOutput("model_sankey")
          )
        )
      ),
      
      nav_menu(
        "Totals",
        nav_panel(
          "Total Revenue",
          p("What's Bringing in the Most Revenue?"),
          p("The horizontal bar plot shows how much revenue each category is bringing in."),
          sidebarLayout(
            sidebarPanel(
              id = "sidebar2",
              width = 3,
              selectInput(inputId = "revenue_category", 
                          label = "Select Category:", 
                          choices = c("Dealer Region" = "Dealer_Region",
                                      "Annual Income" = "Income_Bins",
                                      "Company" = "Company", 
                                      "Color" = "Color", 
                                      "Transmission" = "Transmission", 
                                      "Gender" = "Gender", 
                                      "Body Style" = "Body.Style"),
                          selected = "Dealer_Region")
            ),
            
            mainPanel(
              width = 9,
              textOutput("selected_revenue"),
              plotOutput("revenue_barplot")
            )
          )
        ),
        nav_panel(
          "Total Sales",
          p("What's Bringing in the Most Sales?"),
          p("The horizontal bar plot shows how many sales each category has had."),
          sidebarLayout(
            sidebarPanel(
              id = "sidebar2",
              width = 3,
              selectInput(inputId = "sales_category", 
                          label = "Select Category:", 
                          choices = c("Dealer Region" = "Dealer_Region",
                                      "Annual Income" = "Income_Bins",
                                      "Company" = "Company", 
                                      "Color" = "Color", 
                                      "Transmission" = "Transmission", 
                                      "Gender" = "Gender", 
                                      "Body Style" = "Body.Style"),
                          selected = "Dealer_Region")
            ),
            
            mainPanel(
              width = 9,
              textOutput("selected_sales"),
              plotOutput("sales_barplot")
            )
          )
        ),
      ),
      
      id = "tab"  # Add id for navigation
    ),
    
    mainPanel(
    )
  )
)





