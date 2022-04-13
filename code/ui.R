library(tidyverse)
library(here)

regions <- unique(df$region)

regions <- sort(regions[regions!=toupper(regions)])


# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Population Projections at 2010 vital rates"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    sidebarPanel(
      selectInput("region",label = "Region", choices = regions),
      sliderInput("number_proj",
                  "Project to year :",
                  value = 2020,
                  min = 2020,
                  max = 2200, 
                  step = 5,
                  sep = "", animate =
                    animationOptions(interval = 600, loop = FALSE)),
      helpText("Choose a region and the year to project population. Press play to see the projection. Data on 2010 populations, mortality, and fertility rates are from the UN Population Division World Population Prospects.")
      
    ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("popPlot")  
    )
    
  )
)