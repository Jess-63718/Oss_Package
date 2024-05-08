library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Car Price Classification"),

  # Sidebar layout with a file input for CSV file
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv")
    ),

    # Show the pie chart
    mainPanel(
      plotOutput("price_distribution")
    )
  )
)

server <- function(input, output) {

  cars <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })


  observeEvent(input$file, {

    cars_data <- cars()
    cars_data$Price_Range <- cut(cars_data$Selling_Price, breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                                 labels = c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30+"))

    # Create pie chart
    output$price_distribution <- renderPlot({
      ggplot(cars_data, aes(x = "", fill = Price_Range)) +
        geom_bar(width = 1) +
        coord_polar(theta = "y") +
        labs(title = "Car Price Distribution",
             fill = "Price Range",
             x = NULL, y = NULL) +
        theme_void() +
        theme(legend.position = "bottom")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
