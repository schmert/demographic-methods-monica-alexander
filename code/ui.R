library(tidyverse)
library(here)

df <- read_csv(here("data", "WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.csv")
               , skip = 16) %>% 
  filter(Type != 'Label/Separator') %>% 
  rename(region = `Region, subregion, country or area *`, period = Period) %>% 
  select(-Index, -Variant, -Notes, -`Country code`,-`Parent code`, -Type) %>% 
  rename('15'='15-19', '20'='20-24', '25'='25-29', '30'='30-34',
         '35'='35-39', '40'='40-44', '45'='45-49') %>% 
  mutate(year = as.numeric(substr(period, 1, 4))) %>% 
  gather(age, Fx, -region, -period, -year) %>% 
  mutate(age = as.numeric(age), Fx = as.numeric(Fx)/1000)

regions <- unique(df$region)

regions <- sort(regions[regions!=toupper(regions)])


# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("Population projections"),
  
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