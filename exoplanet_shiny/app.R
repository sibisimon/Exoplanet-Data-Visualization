#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exoplanet Data"),
   
   
   # Sidebar with a slider input for year 
   sidebarLayout(
      sidebarPanel(
         helpText("Filter data by selecting year and type of exoplanet"),
         sliderInput("year",
                     "Select Year:",
                     min = 2009,
                     max = 2018,
                     value = 2018,
                     sep=""),
         selectInput("type", "Select Type:", 
                     choices = list("All" = "all", "Hot Jupiters" = "hot_jupiters",
                                    "Rocky" = "rocky", "Cold Gas Giants" = "cold_gas_giants", 
                                    "Unknown" = "unknown"), selected = "all")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      filter_data <- exoplanet_cmplt_data %>% filter(year == input$year)
      if (input$type == "all") {
        ggplot(filter_data, aes(x=log(mass), y=log(dist), color=type)) + 
          geom_point() +
          labs(title = "Mass Vs Distance Scatter Plot", x="Mass", y="Distance") +
          theme_bw()
      }
      else {
        plt_title <- paste("Mass Vs Distance scatter plot for", input$type)
        filter_data <- exoplanet_cmplt_data %>% filter(year == input$year, type == input$type)
        ggplot(filter_data, aes(x=log(mass), y=log(dist), size=1.5)) + 
          geom_point() +
          labs(title = plt_title, x="Mass", y="Distance") +
          theme_bw()
      }
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      # hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 800))

