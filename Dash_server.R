library(shiny)
library(ggplot2)

# Define server logic
server <- function(input, output) {
  
  # Reactive function to calculate demand and supply curves and equilibrium
  market_data <- reactive({
    # Demand: P = a + b*Q (where b is negative) => Q = (P - a) / b
    demand_func <- function(q) {
      input$demand_intercept + input$demand_slope * q
    }
    inverse_demand_func <- function(p) {
      (p - input$demand_intercept) / input$demand_slope
    }
    
    # Supply: P = c + d*Q (where d is positive) => Q = (P - c) / d
    supply_func <- function(q) {
      input$supply_intercept + input$supply_slope * q
    }
    inverse_supply_func <- function(p) {
      (p - input$supply_intercept) / input$supply_slope
    }
    
    # Calculate initial equilibrium
    eq_q_initial <- (input$demand_intercept - input$supply_intercept) / (input$supply_slope - input$demand_slope)
    eq_p_initial <- demand_func(eq_q_initial)
    
    # Apply shift
    shifted_demand_intercept <- input$demand_intercept
    shifted_supply_intercept <- input$supply_intercept
    
    if (input$shift_type == "Demand") {
      shifted_demand_intercept <- input$demand_intercept + input$shift_amount
    } else if (input$shift_type == "Supply") {
      shifted_supply_intercept <- input$supply_intercept + input$shift_amount
    }
    
    shifted_demand_func <- function(q) {
      shifted_demand_intercept + input$demand_slope * q
    }
    shifted_inverse_demand_func <- function(p) {
      (p - shifted_demand_intercept) / input$demand_slope
    }
    
    shifted_supply_func <- function(q) {
      shifted_supply_intercept + input$supply_slope * q
    }
    shifted_inverse_supply_func <- function(p) {
      (p - shifted_supply_intercept) / input$supply_slope
    }
    
    # Calculate new equilibrium
    eq_q_new <- (shifted_demand_intercept - shifted_supply_intercept) / (input$supply_slope - input$demand_slope)
    eq_p_new <- shifted_demand_func(eq_q_new)
    
    # Generate data for plotting
    quantity_range <- seq(0, max(eq_q_initial * 2, eq_q_new * 2, 50), length.out = 100)
    demand_data <- data.frame(Quantity = quantity_range, Price = demand_func(quantity_range), Curve = "Demand")
    supply_data <- data.frame(Quantity = quantity_range, Price = supply_func(quantity_range), Curve = "Supply")
    shifted_demand_data <- data.frame(Quantity = quantity_range, Price = shifted_demand_func(quantity_range), Curve = "Shifted Demand")
    shifted_supply_data <- data.frame(Quantity = quantity_range, Price = shifted_supply_func(quantity_range), Curve = "Shifted Supply")
    
    list(demand = demand_data, supply = supply_data,
         shifted_demand = shifted_demand_data, shifted_supply = shifted_supply_data,
         eq_p_initial = eq_p_initial, eq_q_initial = eq_q_initial,
         eq_p_new = eq_p_new, eq_q_new = eq_q_new,
         shift_type = input$shift_type, shift_amount = input$shift_amount,
         demand_slope = input$demand_slope, supply_slope = input$supply_slope,
         eq_p_initial = eq_p_initial, eq_q_initial = eq_q_initial)
  })
  
  # Output initial equilibrium
  output$initial_equilibrium <- renderText({
    market <- market_data()
    paste("Price: $", round(market$eq_p_initial, 2), "\nQuantity:", round(market$eq_q_initial, 2))
  })
  
  # Output new equilibrium
  output$new_equilibrium <- renderText({
    market <- market_data()
    if (market$shift_type == "None") {
      "No shift applied."
    } else {
      paste("Price: $", round(market$eq_p_new, 2), "\nQuantity:", round(market$eq_q_new, 2))
    }
  })
  
  # Output plot
  output$equilibriumPlot <- renderPlot({
    market <- market_data()
    
    ggplot() +
      geom_line(data = market$demand, aes(x = Quantity, y = Price, color = Curve), linewidth = 1) +
      geom_line(data = market$supply, aes(x = Quantity, y = Price, color = Curve), linewidth = 1) +
      {if (market$shift_type == "Demand") geom_line(data = market$shifted_demand, aes(x = Quantity, y = Price, color = Curve), linewidth = 1, linetype = "dashed")} +
      {if (market$shift_type == "Supply") geom_line(data = market$shifted_supply, aes(x = Quantity, y = Price, color = Curve), linewidth = 1, linetype = "dashed")} +
      geom_point(aes(x = market$eq_q_initial, y = market$eq_p_initial), color = "blue", size = 4) +
      annotate("text", x = market$eq_q_initial, y = market$eq_p_initial, label = "Initial Equilibrium", vjust = -1, hjust = 0.5, color = "blue") +
      {if (market$shift_type != "None") geom_point(aes(x = market$eq_q_new, y = market$eq_p_new), color = "red", size = 4)} +
      {if (market$shift_type != "None") annotate("text", x = market$eq_q_new, y = market$eq_p_new, label = "New Equilibrium", vjust = -1, hjust = 0.5, color = "red")} +
      labs(title = "Linear Supply and Demand Equilibrium",
           x = "Quantity",
           y = "Price",
           color = "Curve") +
      theme_minimal() +
      scale_color_manual(values = c("Demand" = "steelblue", "Supply" = "forestgreen",
                                    "Shifted Demand" = "steelblue", "Shifted Supply" = "forestgreen"))
  })
  
  # Output elasticity information at the initial equilibrium
  output$elasticity_info <- renderText({
    market <- market_data()
    p0 <- market$eq_p_initial
    q0 <- market$eq_q_initial
    slope_demand <- market$demand_slope
    slope_supply <- 1 / market$supply_slope # Inverse for Q on x-axis
    
    # Point elasticity of demand: Ed = (dQ/dP) * (P/Q) = (1/slope_demand) * (P/Q)
    price_elasticity_demand <- (1 / slope_demand) * (p0 / q0)
    # Point elasticity of supply: Es = (dQ/dP) * (P/Q) = (1/slope_supply) * (P/Q)
    price_elasticity_supply <- (1 / slope_supply) * (p0 / q0)
    
    paste("At Initial Equilibrium:\n",
          "Price Elasticity of Demand:", round(price_elasticity_demand, 2), "\n",
          "Price Elasticity of Supply:", round(price_elasticity_supply, 2))
  })
}
