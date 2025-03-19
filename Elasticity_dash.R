library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Market Equilibrium & Price Elasticity"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("demand_elasticity", "Demand Elasticity (Ed)", min = -2, max = 0, value = -1, step = 0.1),
      sliderInput("supply_elasticity", "Supply Elasticity (Es)", min = 0.1, max = 2, value = 0.5, step = 0.1),
      sliderInput("shock", "Demand/Supply Shock (Shift in Quantity)", min = -50, max = 50, value = 0, step = 5)
    ),
    mainPanel(
      plotOutput("equilibriumPlot"),
      verbatimTextOutput("equilibriumText")
    )
  )
)

server <- function(input, output) {
  
  output$equilibriumPlot <- renderPlot({
    # Define price range
    P_seq <- seq(10, 200, by = 1)  # Increased range to accommodate extreme cases
    
    # Compute demand and supply functions
    Qd <- 100 + input$demand_elasticity * P_seq  # Demand function
    Qs <- input$supply_elasticity * P_seq - 20   # Supply function
    
    # Apply shock to demand curve
    Qd_new <- 100 + input$demand_elasticity * P_seq + input$shock
    
    # Find equilibrium points (before and after shock)
    eq_index <- which.min(abs(Qd - Qs))
    eq_price <- P_seq[eq_index]
    eq_quantity <- Qd[eq_index]
    
    eq_index_new <- which.min(abs(Qd_new - Qs))
    eq_price_new <- P_seq[eq_index_new]
    eq_quantity_new <- Qd_new[eq_index_new]
    
    # Create dataframe for plotting
    data <- data.frame(
      Price = rep(P_seq, 3),
      Quantity = c(Qd, Qd_new, Qs),
      Curve = rep(c("Original Demand", "New Demand", "Supply"), each = length(P_seq))
    )
    
    # Adjust plot limits dynamically
    x_min <- min(Qd, Qd_new, Qs, eq_quantity, eq_quantity_new) * 0.9
    x_max <- max(Qd, Qd_new, Qs, eq_quantity, eq_quantity_new) * 1.1
    y_min <- min(P_seq, eq_price, eq_price_new) * 0.9
    y_max <- max(P_seq, eq_price, eq_price_new) * 1.1
    
    # Plot demand, supply, and new demand curves
    ggplot(data, aes(x = Quantity, y = Price, color = Curve)) +
      geom_line(size = 1.2) +
      geom_point(aes(x = eq_quantity, y = eq_price), color = "red", size = 4) +  # Original equilibrium
      geom_point(aes(x = eq_quantity_new, y = eq_price_new), color = "darkgreen", size = 4) +  # New equilibrium
      expand_limits(x = c(x_min, x_max), y = c(y_min, y_max)) +  # Ensures equilibrium fits in plot
      labs(title = "Market Equilibrium",
           x = "Quantity", y = "Price",
           subtitle = paste("Original Equilibrium: P =", round(eq_price, 2), "Q =", round(eq_quantity, 2), 
                            "| New Equilibrium: P =", round(eq_price_new, 2), "Q =", round(eq_quantity_new, 2))) +
      theme_minimal()
  })
  
  output$equilibriumText <- renderText({
    # Redefine P_seq inside this function
    P_seq <- seq(10, 200, by = 1)  # Keep in sync with plot
    
    # Compute demand and supply functions again
    Qd <- 100 - input$demand_elasticity * P_seq  
    Qs <- input$supply_elasticity * P_seq - 20  
    Qd_new <- 100 - input$demand_elasticity * P_seq + input$shock  
    
    # Find equilibrium points
    eq_index <- which.min(abs(Qd - Qs))
    eq_price <- P_seq[eq_index]
    eq_quantity <- Qd[eq_index]
    
    eq_index_new <- which.min(abs(Qd_new - Qs))
    eq_price_new <- P_seq[eq_index_new]
    eq_quantity_new <- Qd_new[eq_index_new]
    
    paste("Original Equilibrium Price:", round(eq_price, 2), 
          "\nOriginal Equilibrium Quantity:", round(eq_quantity, 2),
          "\nNew Equilibrium Price:", round(eq_price_new, 2), 
          "\nNew Equilibrium Quantity:", round(eq_quantity_new, 2))
  })
}

shinyApp(ui = ui, server = server)
