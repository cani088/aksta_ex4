
library(shiny)
library(DT)
library(jsonlite)
library(ggplot2)
library(plotly)
library(viridis)

# Define UI
ui <- fluidPage(
  titlePanel("CIA World Factbook 2020"),
  p("Welcome to my shiny app, which allows you to visualize variables from the CIA 2020 factbook on the world map, generate descriptive statistics and statistical graphics."),
  sidebarLayout(
    sidebarPanel(
     tabsetPanel(
        tabPanel("Univariate Analysis",
                 selectInput("variable", "Select a variable:",
                             choices = list("Median Age" = "median_age",
                                            "Youth Unemployment Rate" = "youth_unempl_rate",
                                            "Net Migration Rate" = "net_migr_rate",
                                            "Population Growth Rate" = "pop_growth_rate",
                                            "Electricity Fossil Fuel" = "electricity_fossil_fuel",
                                            "Life Expectancy" = "life_expectancy")),
                 actionButton("view_data", "View Raw Data"),
                 dataTableOutput("data_table")
        ),
        tabPanel("Multivariate Analysis",
                 h4("Content for multivariate analysis will go here.")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 h6("The map contains values of the selected variables. 
                    The countries with grey areas have a missing value for the visualized variable."),
                 plotlyOutput("map")
        ),
        tabPanel("Boxplot (overall)",
                 plotlyOutput("boxplot_overall")
        ),
        tabPanel("Boxplot per continent",
                 plotlyOutput("boxplot_continent")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data <- fromJSON("data_cia.json")
  
  # Reactive expression to get the selected variable data
  selected_data <- reactive({
    req(input$variable)
    data[, c("country", "continent", input$variable), drop = FALSE]
  })
  
  # Render the data table
  output$data_table <- renderDataTable({
    req(input$view_data)
    datatable(selected_data(), options = list(pageLength = 15))
  })
  
  # Render the map
  output$map <- renderPlotly({
    req(input$variable)
    
    # Create a world map data frame
    world <- map_data("world")
    
    # Merge with selected data
    map_data <- merge(world, data, by.x = "region", by.y = "country", all.x = TRUE)
    
    # Create the ggplot
    p <- ggplot(map_data, aes_string(x = "long", y = "lat", group = "group", fill = input$variable)) +
      geom_polygon(color = "blue") +
      scale_fill_viridis_c(option = "viridis", na.value = "grey50", name = input$variable) +
      theme_minimal() +
      labs(title = paste("World Map of", input$variable))
    
    # Convert to plotly
    ggplotly(p, tooltip = c("text", input$variable)) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  })
  
  
  # Render the overall boxplot
  output$boxplot_overall <- renderPlotly({
    req(input$variable)
    
    # Create the ggplot
    p <- ggplot(data, aes_string(y = input$variable)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Overall Boxplot of", input$variable), y = input$variable)
    
    # Convert to plotly
    ggplotly(p)
  })
  
  
  # Render the boxplot by continent
  output$boxplot_continent <- renderPlotly({
    req(input$variable)
    
    # Create the ggplot
    p <- ggplot(data, aes_string(x = "continent", y = input$variable)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boxplot of", input$variable, "by Continent"), x = "Continent", y = input$variable)
    
    # Convert to plotly
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
