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
library(bslib)
library(ggplot2)
library(tidyr)
library(plotly)
library(forcats)


car_sales <- read.csv("./car_sales.csv")
original_car_sales <- read.csv("./car_sales.csv")

# Define server logic required to draw a histogram
function(input, output, session) {
  
  company_df <- car_sales %>%
    group_by(Company) %>%
    summarise(
      #Total_Sales = n(),
      Male_Count = sum(Gender == "Male", na.rm = TRUE),
      Female_Count = sum(Gender == "Female", na.rm = TRUE),
      Automatic_Count = sum(Transmission == "Auto", na.rm = TRUE),
      Manual_Count = sum(Transmission == "Manual", na.rm = TRUE),
      Black_Count = sum(Color == "Black", na.rm = TRUE),
      White_Count = sum(Color == "Pale White", na.rm = TRUE),
      Red_Count = sum(Color == "Red", na.rm = TRUE),
      Avg_Price = mean(Price...., na.rm = TRUE),
      #Avg_Income = mean(Annual.Income, na.rm = TRUE)
    )
  
  output$df_model <- renderPlot({
    plot(select(company_df, -Company))
  })
  
  company_data <- reactive({
    req(input$Company)
    car_sales %>% 
      filter(Company == input$Company)  # Filter for selected company
  })
  
  # Histogram
  output$company_hist <- renderPlotly({
    p <- ggplot(company_data(), aes(x = Price...., fill = Body.Style)) +  # Corrected dataset
      geom_histogram(binwidth = 10000, alpha = 0.6) +
      facet_wrap(~ Body.Style) +  # Facet by Body Style
      labs(title = paste("Car Price by Body\nStyle for", input$Company),
           x = "Price ($)",
           y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(p) %>%
      layout(
        title = list(
          text = paste("Car Price by Body Style for", input$Company), 
          font = list(size = 16)
        )
      )
  })
  
  summary_stats <- reactive({
    company_data() %>%
      group_by(Model) %>%
      summarize(
        median_price = median(Price...., na.rm = TRUE),
        upper_whisker = quantile(Price...., 0.75, na.rm = TRUE) + 1.5 * IQR(Price....),
        lower_whisker = quantile(Price...., 0.25, na.rm = TRUE) - 1.5 * IQR(Price....),
        max_price = max(Price...., na.rm = TRUE),
        min_price = min(Price...., na.rm = TRUE),
        price_range = max_price - min_price,
        .groups = 'drop'
      )
  })
  
  # Boxplot 
  output$company_box <- renderPlotly({
    stats <- summary_stats()
    
    ordered_data <- company_data() %>%
      left_join(stats, by = "Model") %>%
      mutate(Model = fct_reorder(Model, price_range))
    
    p <- ggplot(ordered_data, aes(x = Model, y = Price...., fill = Model)) +  # Corrected dataset
      geom_boxplot() +
      labs(title = paste("Car Price Distribution of\nModels for", input$Company),
           x = "Model",
           y = "Price ($)") +
      theme_minimal() +
      coord_flip() +  # Flip for better readability
      theme(legend.position = "none")
    ggplotly(p, tooltip = "y") %>%
      layout(
        hoverlabel = list(
          font = list(size = 8),  # Smaller text
          padding = list(t = 2, b = 2, l = 2, r = 2),  # Less padding
          margin = list(l = 150, r = 150, t = 50, b = 50)
        ),
        title = list(
          text = paste("Car Price Distribution of\nModels for", input$Company), 
          font = list(size = 16)
        )
      )
  })
  
  gender_counts <- reactive({
    req(input$Company)
    car_sales %>% 
      filter(Company == input$Company) %>%  # Filter for selected company
      count(Model, Gender)
  })
  
  # Cleveland Dotplot
  output$company_dot <- renderPlotly({
    p <- ggplot(gender_counts(), aes(x = n, y = Model, color = Gender)) +
      geom_point(size = 4) +  # Plot dots for Men/Women
      geom_line(aes(group = Model), color = "gray", linetype = "dashed") +  # Connect same Company
      labs(title = paste("Gender Breakdown of Buyers\nby Model for", input$Company),
           x = "Number of Buyers",
           y = "Model",
           color = "Gender") +
      theme_minimal()
    ggplotly(p) %>%
      layout(
        title = list(
          text = paste("Gender Breakdown of Buyers\nby Model for", input$Company), 
          font = list(size = 16)
        )
      )
  })
  
  color_counts <- reactive({
    req(input$Company)
    car_sales %>% 
      filter(Company == input$Company) %>%  # Filter for selected company
      count(Model, Color)
  })
  
  # Stacked Barplot
  output$company_bar <- renderPlot({
    req(color_counts())
    
    # Get color counts and total per model
    data <- color_counts()
    
    # Compute total cars per model
    model_totals <- data %>%
      group_by(Model) %>%
      summarise(total_n = sum(n), .groups = 'drop')
    
    # Join back with the original data to get total counts
    sorted_data <- data %>%
      left_join(model_totals, by = "Model") %>%
      mutate(Model = forcats::fct_reorder(Model, -total_n))  # Reorder factor by total
    
    ggplot(sorted_data, aes(x = Model, y = n, fill = Color)) +
      geom_bar(stat = "identity") +  # Stacked bars
      geom_text(aes(label = n), size = 3,
                position = position_stack(vjust = 0.5)) +
      labs(title = paste("Number of Colors for Each Model\nPurchased for", input$Company),
           x = "Model",
           y = "Number of Cars",
           fill = "Color") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  
  average_stats <- car_sales %>%
      group_by(Company) %>%
      summarize(
        mean_price = mean(Price...., na.rm = TRUE),
        mean_income = mean(Annual.Income, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
    mutate(Company = factor(Company, levels = sort(unique(Company)))) 
  
  
  # Total Scatter
  output$total_scatter <- renderPlotly({
    p <- ggplot(average_stats, aes(x = mean_income, y = mean_price, color = Company)) +
      geom_point(size = 4, alpha = 0.7) +
      labs(title = "Average Car Price vs Average Annual Income",
           x = "Average Annual Income",
           y = "Average Price ($)",
           fill = "Company") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # total_stats <- car_sales %>%
  #   select(Price = `Price....`, Income = `Annual.Income`) %>%
  #   pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")
  # 
  # #Total Box
  # output$total_box <- renderPlotly({
  #   p <- ggplot(total_stats, aes(x = Variable, y = Value, fill = Variable)) +
  #     geom_boxplot() +
  #     labs(
  #       title = "Distribution of Car Price and Annual Income",
  #       x = "Variable",
  #       y = "Value ($)"
  #     ) +
  #     theme_minimal() +
  #     theme(legend.position = "none")
  #   
  #   ggplotly(p)
  # })
  
  output$correlation <- renderText({
    correlation <- cor(average_stats$mean_income, average_stats$mean_price, use = "complete.obs")
    paste("r =", round(correlation, 2))
  })
  
  
  
  observe({
    updateSelectInput(session, "Company", choices = sort(unique(car_sales$Company)))
    updateCheckboxGroupInput(session, "Dealer_Region", 
                             choices = sort(unique(car_sales$Dealer_Region)),
                             selected = sort(unique(car_sales$Dealer_Region)))
  })
  
  observe({
    updateSelectInput(session, "Company1", choices = sort(unique(car_sales$Company)))
    updateCheckboxGroupInput(session, "Dealer_Region1", 
                             choices = sort(unique(car_sales$Dealer_Region)),
                             selected = sort(unique(car_sales$Dealer_Region)))
  })
  
  observeEvent(input$Company, {
    filtered_models <- sort(unique(car_sales$Model[car_sales$Company == input$Company]))
    updateSelectInput(session, "Model", choices = filtered_models)
  })
  
  
  observeEvent(input$Company1, {
    filtered_models <- sort(unique(car_sales$Model[car_sales$Company == input$Company1]))
    updateSelectInput(session, "Model1", choices = filtered_models)
  })
  
  observeEvent(input$Model, {
    req(input$Company)
    
    available_styles <- car_sales %>%
      filter(Company == input$Company, Model == input$Model) %>%
      pull(Body.Style) %>%
      unique() %>%
      sort()
    
    updateCheckboxGroupInput(session, "Body.Style", 
                             choices = available_styles,
                             selected = available_styles)
  })
  
  observeEvent(input$Model1, {
    req(input$Company1)
    
    available_styles <- car_sales %>%
      filter(Company == input$Company1, Model == input$Model1) %>%
      pull(Body.Style) %>%
      unique() %>%
      sort()
    
    updateCheckboxGroupInput(session, "Body.Style1", 
                             choices = available_styles,
                             selected = available_styles)
  })
  
  
  # Show selected values
  output$selected_model <- renderText({
    paste("You selected:", input$Company, "-", input$Model)
  })
  
  output$selected_model1 <- renderText({
    paste("You selected:", input$Company1, "-", input$Model1)
  })
  
  output$body_style_text <- renderText({
    req(input$Company, input$Model)  # Ensure selections exist
    
    # Extract the corresponding Body Style
    body_style <- unique(car_sales$Body.Style[car_sales$Model == input$Model])
    
    # Ensure only one body style is shown, or handle multiple
    if (length(body_style) == 1) {
      paste("Body Style:", body_style)
    } else {
      "Body Style: Multiple or Unknown"
    }
  })
  
  output$body_style_text1 <- renderText({
    req(input$Company1, input$Model1)  # Ensure selections exist
    
    # Extract the corresponding Body Style
    body_style <- unique(car_sales$Body.Style[car_sales$Model == input$Model])
    
    # Ensure only one body style is shown, or handle multiple
    if (length(body_style) == 1) {
      paste("Body Style:", body_style)
    } else {
      "Body Style: Multiple or Unknown"
    }
  })
  
  
  
  
  # Car Price Bar Graph
  output$price_bar <- renderPlotly({
    req(input$Company, input$Model, input$Dealer_Region)
    
    filtered_data <- car_sales %>%
      filter(Company == input$Company & 
               Model == input$Model &
               Body.Style %in% input$Body.Style &
               Dealer_Region %in% input$Dealer_Region)
    
    p <- ggplot(filtered_data, aes(x = Price....)) + 
      geom_density(aes(fill = Dealer_Region), position = "dodge", alpha = 0.7) + 
      labs(title = "Car Price Distribution",
           x = "Price ($)",
           y = "Density of Cars",
           fill = "Dealer Region") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(
      showlegend = TRUE,
      legend = list(
        x = 1, y = 1,  # Place the legend outside the plot
        orientation = "v",
        font = list(size = 8),
        title = list(font = 10))
    )
  })
  
  # Annual Income Bar Graph
  output$income_bar <- renderPlotly({
    req(input$Company, input$Model, input$Dealer_Region)
    
    filtered_data <- car_sales %>%
      filter(Company == input$Company & 
               Model == input$Model &
               Body.Style %in% input$Body.Style &
               Dealer_Region %in% input$Dealer_Region)
    
    p <- ggplot(filtered_data, aes(x = Annual.Income, fill = Dealer_Region)) +
      geom_density(aes(fill = Dealer_Region), position = "dodge", alpha = 0.7) + 
      scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = " mil")) +
      labs(title = "Annual Income Distribution",
           x = "Annual Income ($)",
           y = "Density of People",
           fill = "Dealer Region") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(
      showlegend = TRUE,
      legend = list(
        x = 1, y = 1,  # Place the legend outside the plot
        orientation = "v",
        font = list(size = 8),
        title = list(font = 10))
    )
  })
  
  
  # Sankey Plot
  output$model_sankey <- renderPlotly({
    req(input$Company1, input$Model1, input$Body.Style1, input$Dealer_Region1)
    
    # Filter the data based on inputs
    filtered_data <- car_sales %>%
      filter(Company == input$Company1 & 
               Model == input$Model1 &
               Body.Style %in% input$Body.Style1 &
               Dealer_Region %in% input$Dealer_Region1)
    
    # Create sankey data where flows are grouped by Dealer Region, Gender, Transmission, and Color
    sankey_data <- filtered_data %>%
      count(Dealer_Region, Gender, Transmission, Color, name = "count")
    
    # Create unique node labels (Dealer Region, Gender, Transmission, Color)
    nodes <- data.frame(name = unique(c(sankey_data$Dealer_Region, sankey_data$Gender, sankey_data$Transmission, sankey_data$Color)))
    
    # Function to get node index
    get_index <- function(value) which(nodes$name == value) - 1
    
    # Create links: source & target indices for Sankey plot
    links <- data.frame(
      source = c(sapply(sankey_data$Dealer_Region, get_index), 
                 sapply(sankey_data$Gender, get_index), 
                 sapply(sankey_data$Transmission, get_index)),
      target = c(sapply(sankey_data$Gender, get_index), 
                 sapply(sankey_data$Transmission, get_index), 
                 sapply(sankey_data$Color, get_index)),
      value = rep(sankey_data$count, 3),  # Assigning counts to all flows
      dealer_region = rep(sankey_data$Dealer_Region, 3)  # Assign Dealer Region for coloring
    )
    
    
    # Assign unique colors to each Dealer Region
    unique_regions <- unique(sankey_data$Dealer_Region)
    region_colors <- setNames(RColorBrewer::brewer.pal(min(length(unique_regions), 8), "Set2"), unique_regions)
    
    # Map region colors to links based on Dealer Region
    link_colors <- unname(region_colors[links$dealer_region])
    
    # Create the Sankey plot with colored strands
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes$name,
        color = "lightgray"
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value,
        color = link_colors  # Use the colors mapped to Gender, Transmission, and Color
      )
    )
    
    ggplotly(p) %>%
      layout(title = list(text = "Car Features Preference by Dealer Region and Gender", y = 0.97))
  })
  
  output$selected_revenue <- renderText({
    paste("You selected:", input$revenue_category)
  })
  
  #Horizontal Bar Plot
  output$revenue_barplot <- renderPlot({
    req(input$revenue_category)
    
    # Aggregate total revenue based on the selected category
    revenue_data <- car_sales %>%
      mutate(Income_Bins = cut(`Annual.Income`, 
                               breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, Inf), 
                               labels = c("<500k", "500k-1mil", "1mil-1.5mil", "1.5mil-2mil", "2mil-2.5mil", "2.5mil+"),
                               include.lowest = TRUE)) %>%  # Bin income
      group_by(.data[[input$revenue_category]]) %>%
      summarise(Total_Revenue = sum(Price...., na.rm = TRUE)) %>%
      arrange(desc(Total_Revenue))
    
    # Create a horizontal bar plot
    ggplot(revenue_data, aes(x = reorder(.data[[input$revenue_category]], Total_Revenue), 
                                  y = Total_Revenue, fill = .data[[input$revenue_category]])) +
      geom_col() +
      geom_text(aes(label = Total_Revenue), 
                hjust = 1.2, size = 4) +  # Positioning & size of text
      coord_flip() +  # Horizontal bars
      scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = " mil")) +
      labs(title = "Total Revenue by Selected Category",
           x = input$revenue_category,
           y = "Total Revenue ($)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$selected_sales <- renderText({
    paste("You selected:", input$sales_category)
 })
  
  #Horizontal Bar Plot
  output$sales_barplot <- renderPlot({
    req(input$sales_category)
    
    # Aggregate total revenue based on the selected category
    sales_data <- car_sales %>%
      mutate(Income_Bins = cut(`Annual.Income`, 
                               breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000, Inf), 
                               labels = c("<500k", "500k-1mil", "1mil-1.5mil", "1.5mil-2mil", "2mil-2.5mil", "2.5mil+"),
                               include.lowest = TRUE)) %>%  # Bin income
      group_by(.data[[input$sales_category]]) %>%
      summarise(Total_Sales = n()) %>%
      arrange(desc(Total_Sales))
    
    # Create a horizontal bar plot
    ggplot(sales_data, aes(x = reorder(.data[[input$sales_category]], Total_Sales), 
                                  y = Total_Sales, fill = .data[[input$sales_category]])) +
      geom_col() +
      geom_text(aes(label = Total_Sales), 
                hjust = 1.2, size = 4) +  # Positioning & size of text
      coord_flip() +  # Horizontal bars
      labs(title = "Total Sales by Selected Category",
           x = input$sales_category,
           y = "Total Sales") +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
}

