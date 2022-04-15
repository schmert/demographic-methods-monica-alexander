#...................................................................
# Shiny App for population projections/stable age structure
# Carl Schmertmann
# 14 Apr 2022
# 
# modified version of Monica Alexander's initial code
# (which she amazingly created in about 30 minutes, two days ago!)
#...................................................................
library(tidyverse)
library(patchwork)

options(warn = -1)  # turn off warnings
options(dplyr.summarise.inform = FALSE) # turn off tidyverse messages

precomputed = "precompute-projections-WPP-2019.Rdata"
load( precomputed )

years = as.numeric( dimnames(bigProj)[[3]] )
ages  = as.numeric( dimnames(bigProj)[[2]] )

Kdf_shell = expand.grid( age= ages, year = years) %>% 
             as_tibble()

# Define a server for the Shiny app ----

server = function(input, output) {
  
  # Fill in the spot we created for a plot
  
  output$popPlot <- renderPlot({

    # convert the selected projection result to a long dataframe

    Kdf = Kdf_shell %>% 
      add_column(Kx = as.vector( bigProj[input$region, , ]),
                 Cx = as.vector(   bigCx[input$region,,]) ) 
    
    # consolidate into 10-year age groups for plotting  
    
    Kdf10 <- Kdf %>% 
      mutate(age10 = factor(10* floor(age/10)) ) %>% 
      group_by(year, age=age10) %>% 
      summarize(Cx = sum(Cx), Kx=sum(Kx)) %>% 
      ungroup() 

    max_Kx = max(Kdf10$Kx)
    max_Cx = max(Kdf10$Cx)
    maxpop = max(colSums(bigProj[input$region,,]) )
    
    Kdf10 = filter(Kdf10, year <= input$last_proj_year)
    
    this_TFR = bigTFR[input$region]
    this_r   = bigR[input$region]
    
    this_subtitle = paste0('TFR in 2015 = ', sprintf('%3.2f',this_TFR),
                           '\nLR growth rate = ', sprintf('%5.4f', this_r))
      

    Kdf_total = Kdf10 %>% 
                  group_by(year) %>% 
                  summarize(pop = sum(Kx))

    p1 <-  Kdf_total %>% 
            ggplot(aes(year, pop)) + geom_line(lwd = 1.5) + 
            labs(x="Year",y='Population (000s)',
                 title= paste(input$region,'Female Population (1000s)'),
                 subtitle = this_subtitle) +
            theme_bw(base_size = 14)+
            xlim(c(2020, 2180)) +
            ylim(range(0,maxpop))
    
    
    age_text = Kdf10 %>% 
                filter(year == max(year)) %>% 
                mutate(age = as.character(age)) %>% 
                select(year, age, Cx)
    
    if (input$rhs_barplot) {
      
    group_lab = paste0(seq(0,100,10),'-', seq(9,109,10), sep='')
    group_lab[11] = '100+'
                       
    p2 <- ggplot(data=filter(Kdf10, year==input$last_proj_year)) +
            aes(year, x= age, y=Kx) + 
            geom_bar(stat='identity', fill='royalblue', width=.80) + 
            theme_bw(base_size = 14)+
            theme(axis.text.x = element_text(size=10)) +
            labs(title="Population Size By 10-Yr Age Group",
                 subtitle= input$last_proj_year,
                x='Age Group',
                y='# Women (000s)') +
            scale_y_continuous(limits=range(0,max_Kx)) +
            scale_x_discrete(breaks=seq(0,100,10), labels = group_lab)
    
    if (input$rhs_rotate) p2 = p2 + coord_flip()
    
    } else {
    
      p2 <- ggplot(data=Kdf10) +
      aes(year, Cx, color = age) + 
      geom_line(lwd = 1.5) + 
      theme_bw(base_size = 14)+
      labs(title="Proportion by 10-year Age Group",
           subtitle= input$last_proj_year,
           x='Year',
           y='Fraction of Population',
           color='10-yr\nage group') +
      guides(color='none',label='none') +
      scale_color_manual(values=rainbow(ngroups)) +
      scale_y_continuous(limits=range(0,max_Cx)) +
      xlim(c(2020, 2180)) +
      geom_text(data=age_text, 
                aes(x=year+3, y=Cx, label=age),
                size=3, face='bold')
    }
    
    p1+p2
    
  })
}

# Define the user interface for the Shiny app ----

ui = fluidPage(    
    
   theme = bslib::bs_theme(
     bootswatch = "pulse",
     base_font = bslib::font_google('Sarabun')
   ),
    
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
                    max = 2175, 
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


# Run the application ----
shinyApp(ui = ui, server = server)