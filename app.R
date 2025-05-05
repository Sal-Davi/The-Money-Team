
library(shiny)
# app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Retirement & Investment Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("start",    "Starting Amount ($)",      value = 5000, min = 0, step = 100),
      numericInput("contrib",  "Annual Contribution ($)",  value = 100,  min = 0, step = 50),
      sliderInput("rate",      "Expected Annual Return (%)",
                  min = 0, max = 15, value = 4, step = 0.1),
      sliderInput("years",     "Years to Grow",
                  min = 1, max = 50, value = 10, step = 1)
    ),
    mainPanel(
      uiOutput("fv_text"),
      tabsetPanel(
        tabPanel("Growth Over Time", plotOutput("plot_growth", height = "400px")),
        tabPanel("Year-End Breakdown", plotOutput("plot_pie",    height = "400px"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive future value calculation
  fv <- reactive({
    P <- input$start
    C <- input$contrib
    r <- input$rate / 100
    n <- input$years
    P * (1 + r)^n + C * (((1 + r)^n - 1) / r)
  })
  
  output$fv_text <- renderUI({
    h3(sprintf("This investment will be worth: $%s",
               format(round(fv(), 2), big.mark = ",")))
  })
  
  output$plot_growth <- renderPlot({
    P <- input$start
    C <- input$contrib
    r <- input$rate / 100
    n <- input$years
    start_year <- as.integer(format(Sys.Date(), "%Y"))
    
    yrs <- 0:n
    principal   <- P * (1 + r)^yrs
    contrib_cum <- C * (((1 + r)^yrs - 1) / r)
    total       <- principal + contrib_cum
    interest    <- total - principal - contrib_cum
    
    df <- data.frame(
      Year         = start_year + yrs,
      Principal    = principal,
      Contribution = contrib_cum,
      Interest     = interest
    )
    # drop the year-0 bar if you like:
    df <- df[-1, ]
    
    ggplot(df, aes(x = Year)) +
      geom_col(aes(y = Principal,    fill = "Principal")) +
      geom_col(aes(y = Contribution, fill = "Contributions"), position = "stack") +
      geom_col(aes(y = Interest,     fill = "Interest"),      position = "stack") +
      scale_fill_manual(values = c(
        "Principal"     = "#1f77b4",
        "Contributions" = "#2ca02c",
        "Interest"      = "#ff7f0e"
      )) +
      labs(title = "Investment Growth Over Time",
           y     = "Balance ($)", fill = "") +
      theme_minimal()
  })
  
  output$plot_pie <- renderPlot({
    P <- input$start
    C <- input$contrib
    r <- input$rate / 100
    n <- input$years
    start_year <- as.integer(format(Sys.Date(), "%Y"))
    
    final_princ    <- P * (1 + r)^n
    final_contribs <- C * n
    total_contrib  <- C * (((1 + r)^n - 1) / r)
    total_interest <- final_princ + total_contrib - P - final_contribs
    
    pie_df <- data.frame(
      category = c("Starting Amount", "Total Contributions", "Total Interest"),
      value    = c(P, final_contribs, total_interest)
    )
    
    ggplot(pie_df, aes(x = "", y = value, fill = category)) +
      geom_col(width = 1, color = "white") +
      coord_polar("y") +
      scale_fill_manual(values = c(
        "Starting Amount"    = "#1f77b4",
        "Total Contributions" = "#2ca02c",
        "Total Interest"      = "#ff7f0e"
      )) +
      theme_void() +
      labs(title = paste("Balance Breakdown at Year", start_year + n))
  })
}

shinyApp(ui, server)
