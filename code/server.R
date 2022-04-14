#...................................................................
# Shiny App for population projections/stable age structure
# Carl Schmertmann
# 14 Apr 2022
# 
# modified version of Monica Alexander's initial code
# (which she amazingly created in about 30 minutes, two days ago!)
#...................................................................
library(tidyverse)
library(here)
library(patchwork)

precomputed = here("data", "precompute-projections-WPP-2019.Rdata")
load( precomputed)

years = as.numeric( dimnames(bigProj)[[3]] )
ages  = as.numeric( dimnames(bigProj)[[2]] )

Kdf_shell = expand.grid( age= ages, year = years) %>% 
             as_tibble()

# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  
  output$popPlot <- renderPlot({

    # convert the selected projection result to a long dataframe
    
    maxpop = max(colSums(bigProj[input$region,,]) )
    
    this_TFR = bigTFR[input$region]
    this_r   = bigR[input$region]
    
    this_subtitle = paste0('TFR in 2015 = ', sprintf('%3.2f',this_TFR),
                           '\nLR growth rate = ', sprintf('%5.4f', this_r))
      
    Kdf = Kdf_shell %>% 
            add_column(Kx = as.vector( bigProj[input$region, , ]),
                       Cx = as.vector(   bigCx[input$region,,]) ) %>% 
          filter( year <= input$last_proj_year)
    
    Kdf_total = Kdf %>% 
                  group_by(year) %>% 
                  summarize(pop = sum(Kx))

    p1 <-  Kdf_total %>% 
            ggplot(aes(year, pop)) + geom_line(lwd = 1.5) + 
            labs(x="Year",y='Population (000s)',
                 title= paste(input$region,'Female Population (1000s)'),
                 subtitle = this_subtitle) +
            theme_bw(base_size = 14)+
            xlim(c(2020, 2205)) +
            ylim(range(0,maxpop))
    
  # consolidate into 10-year age groups for plotting  
  
    Kdf10 <- Kdf %>% 
      mutate(age10 = factor(10* floor(age/10)) ) %>% 
      group_by(year, age=age10) %>% 
      summarize(Cx = sum(Cx)) %>% 
      ungroup() 
      
    
    age_text = Kdf10 %>% 
                filter(year == max(year)) %>% 
                mutate(age = as.character(age)) %>% 
                select(year, age, Cx)
    
    p2 <- ggplot(data=Kdf10) +
            aes(year, Cx, color = age) + 
            geom_line(lwd = 1.5) + 
            theme_bw(base_size = 14)+
            labs(title="Proportion by 10-year Age Group",
                x='Year',
                y='Fraction of Population',
                color='10-yr\nage group') +
            guides(color='none',label='none') +
#            scale_color_viridis_d(option='A') +
scale_color_manual(values=rainbow(11)) +
            xlim(c(2020, 2205)) +
            geom_text(data=age_text, 
                      aes(x=year+3, y=Cx, label=age),
                      size=3)

    p1+p2
    
  })
}