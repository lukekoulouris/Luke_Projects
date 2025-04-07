library(shiny)

ui <- fluidPage(
  titlePanel("Linear Supply and Demand Elasticity Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Curve Parameters"),
      numericInput("demand_intercept", "Demand Intercept (Price axis):", value = 20, min = 1),
      numericInput("demand_slope", "Demand Slope (negative):", value = -0.2, max = 0, step = 0.01),
      numericInput("supply_intercept", "Supply Intercept (Price axis):", value = 5, min = 0),
      numericInput("supply_slope", "Supply Slope (positive):", value = 0.1, min = 0.01, step = 0.01),
      hr(),
      h4("Initial Equilibrium"),
      verbatimTextOutput("initial_equilibrium"),
      hr(),
      h4("Demand/Supply Shift"),
      radioButtons("shift_type", "Shift Curve:",
                   choices = c("None", "Demand", "Supply"),
                   selected = "None"),
      numericInput("shift_amount", "Shift Amount (Intercept Change):", value = 2, step = 0.1),
      helpText("Positive for rightward shift, negative for leftward shift (applied to the intercept).")
    ),
    
    mainPanel(
      plotOutput("equilibriumPlot"),
      h4("New Equilibrium:"),
      verbatimTextOutput("new_equilibrium"),
      h4("Elasticity at Initial Equilibrium:"),
      verbatimTextOutput("elasticity_info")
    )
  )
)
