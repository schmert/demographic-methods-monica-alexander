library(tidyverse)
library(here)
library(patchwork)

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
                  value = 2015,
                  min = 2015,
                  max = 2200, 
                  step = 5,
                  sep = "", animate =
                    animationOptions(interval = 400, loop = FALSE)),
      
      fluidRow(
        column(6,checkboxInput(inputId = 'rhs_barplot', 
                    label   = 'Right-hand Plot as Bars?',
                    value   = FALSE)),

        column(6,checkboxInput(inputId = 'rhs_rotate', 
                    label   = 'Rotate Bars (pyramid-style)?',
                    value   = FALSE))
      ),
      
      helpText("Choose a region and a projection year. Press play to see the projection. Data on 2015 populations, mortality, and fertility rates are from the UN Population Division World Population Prospects 2019.")
      
    ),
    
    # Create a spot for the plot
    mainPanel(
      plotOutput("popPlot")  
    )
    
  )
)