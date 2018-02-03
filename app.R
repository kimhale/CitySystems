#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(sp)
library(tidyverse)

#Load world data ##need to change to COCOM, but the shapefile is messed up
wrld <- readOGR(dsn = "continents", layer = "continent")
wrld@data$id <-  rownames(wrld@data)
wrld.points <-  fortify(wrld, region="id")
wrld.df <-  plyr::join(wrld.points, wrld@data, by="id")

worldcities <- read.csv("simplemaps-worldcities-basic.csv", stringsAsFactors = FALSE)
coordinates(worldcities) <- ~lng+lat
proj4string(wrld) 
proj4string(worldcities) <- proj4string(wrld) 
worldcities@data <- cbind(worldcities@data, over(worldcities, wrld[,"CONTINENT"]))
worldcities.df <- cbind(worldcities@data, worldcities@coords) %>%
                        arrange(desc(pop))

#Build basemap
basemap <- ggplot() +
  geom_polygon(dat = wrld.df, aes(x = long, y = lat, group = group, fill = CONTINENT), color = "grey10") +
  #geom_point(dat = filter(worldcities, pop >= 200000), aes(x = lng, y = lat)) +
  theme_void() +
  coord_equal()

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("City System of Systems"),
   
   # Horizontal rule
   hr(),
   
   # Sidebar with input controls
   sidebarLayout(
     
     sidebarPanel(
        # COCOM group of checkboxes
        checkboxGroupInput("AOR", label = h3("Continents to Include"), 
                      choices = list("Africa" = "Africa", "Anatarctica" = "Anatarctica", "Asia" = "Asia", 
                                     "Australia" = "Australia", "Europe" = "Europe", 
                                     "North America" = "North America",
                                     "Oceania" = "Oceania", "South America" = "South America"),
                      #"AFRICOM" = 1, "CENTCOM" = 2, "PACOM" = 3, 
                      #             "SOUTHCOM" = 4, "EUCOM" = 5),
                      selected = 1),
   
        uiOutput("cityControls")
     ),
        
     mainPanel(tabsetPanel(type = "tabs",
                tabPanel("Cities Compared", plotOutput("map")),
                tabPanel("City Systems Compared", uiOutput("subsysplots")),
                tabPanel("System Indicators Compared", uiOutput("indicatorplots")),
                tabPanel("Controls Tab"),
                tabPanel("Data Input"),
                tabPanel("Documentation")
                )
        )
   
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderPlot({
    #cities <- head(worldcities.df[worldcities.df$CONTINENT %in% input$AOR, 1])
    basemap + geom_point(dat = filter(worldcities.df, 
                                      (city %in% input$chosencities & CONTINENT %in% input$AOR)), 
                         aes(x = lng, y = lat))   
  })
   
  output$cityControls <- renderUI({
     cities <- head(worldcities.df[worldcities.df$CONTINENT %in% input$AOR, 1])
     checkboxGroupInput("chosencities", "Choose up to 3 Cities", cities)
   })
  
  max_plots <- 3
  output$subsysplots <- renderUI({
    plot_output_list <- lapply(1:length(input$chosencities), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 280, width = 250)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for (i in 1:max_plots) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- i
      plotname <- paste("plot", my_i, sep="")
      
      output[[plotname]] <- renderPlot({
        plot(1:my_i, 1:my_i,
             xlim = c(1, max_plots),
             ylim = c(1, max_plots),
             main = paste("1:", my_i, ".  n is ", input$n, sep = "")
        )
      })
    })
  }
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

