library(tidyverse)
library(here)
library(patchwork)

regions <- unique(bigWPP$region)

regions <- sort(regions[regions!=toupper(regions)])

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Population Projections from 2015, at 2015 vital rates"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    sidebarPanel(
      selectInput("region",label = "Region", choices = regions),
      sliderInput("last_proj_year",
                  "Project to year :",
                  value = 2020,
                  min = 2020,
                  max = 2200, 
                  step = 5,
                  sep = "", animate =
                    animationOptions(interval = 400, loop = FALSE)),
      
      checkboxInput(inputId = 'pyramid_style', 
                    label   = 'Right-hand Plot as Population Bars?',
                    value   = FALSE),
      
      checkboxInput(inputId = 'flip_bars', 
                    label   = 'Rotate Bar Plot, Pyramid style?',
                    value   = FALSE),
      
      helpText("Choose a region and a projection year. Press play to see the projection. Data on 2015 populations, mortality, and fertility rates are from the UN Population Division World Population Prospects 2019.")
      
    ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("popPlot")  
    )
    
  )
)