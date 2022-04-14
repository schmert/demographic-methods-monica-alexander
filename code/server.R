library(tidyverse)
library(here)
library(patchwork)

# read ASFR file ----
# objective output is a tibble with columns
# region, period, year, age, Fx (in per women, not per 1000)

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

# convert to an array of latest( 2015-2020) fertility rates

bigFx = df %>% 
         filter( year==2015 ) %>% 
         pull(Fx) %>% 
         matrix(., nrow=length(unique(df$region)))
         
dimnames(bigFx) = list( unique(df$region), unique(df$age))

# read abridge life table file ----

#dl <- read_csv(here("data", "WPP2017_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.csv"), skip = 16)

# objective output is a tibble with columns
# region, period, year, age, Lx (w/ radix=1)

# modified Monica's code so that the [0,1) and [1,5) groups
# are combined immediately in the dl dataset, rather than being
# calculated later on the fly

age_crosswalk = tibble(
  age  = c(0,1,seq(5,100,5)),
  age5 = 5*floor(age/5) 
)

dl <- read_csv(here("data", "WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.csv"), 
               skip = 12) %>% 
  rename(region = `Region, subregion, country or area *`,
         Lx = `Number of person-years lived L(x,n)`,
         age = `Age (x)`,
         period = Period) %>% 
  select(region, period, age, Lx) %>% 
  mutate(year = as.numeric(substr(period, 1, 4)), Lx = Lx/1e5) %>% 
      left_join(age_crosswalk) %>% 
      group_by(region, period, age = age5, year) %>% 
      summarize( Lx = sum(Lx)) %>% 
      filter(is.finite(Lx))


# convert to an array of latest( 2015-2020) Lx values

bigLx = dl %>% 
  filter( year==2015 ) %>% 
  pull(Lx) %>% 
  matrix(., nrow=length(unique(dl$age))) %>% 
  t()

dimnames(bigLx) = list(  unique(dl$region), unique(dl$age) )

@@WORKING HERE@@

# read female population file ----
d_female <- read_csv(here("data", "WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.csv"), 
                     skip = 12) %>% 
              rename(region = `Region, subregion, country or area *`,
                     year = `Reference date (as of 1 July)`) %>% 
              select(-Index, -Variant, -Notes, -`Country code`)


Leslie <- function(Lx, Fx, ffab = 0.4886) {
  n  = length(Lx)
  L  = diag(0, n)
  
  # (n-1) surv elements for subdiagonal  
  Sx = Lx[2:n] / Lx[1:(n-1)] 
  diag(L[-1,]) = Sx
  
  # (slightly arbitrary) survival prob for 100+ -> 100+
  # assume 100+ mortal rate approx = 0.7, so 5-yr survival is exp(-3.5)
  # or approx 3%
  L[n,n] = .03
  
  # top row (just fill out for groups 0,5,10,...,45)
  L[1,1:10] = ffab * Lx[1]/2*(Fx[1:10]+ Sx[1:10]*Fx[2:11]) 
 
  return(L)
}




# Define a server for the Shiny app
function(input, output) {
  
  # Fill in the spot we created for a plot
  
  output$popPlot <- renderPlot({

  # for the selected country, get Lx, Fx, and Kx (pop) 
  # for five-yr age groups starting at 0,5,...,95 and 100+
    
    nLx <- dl %>% 
      left_join(df) %>% 
      filter(year==2010, region == input$region) %>% 
      pull(Lx) 
    
    nFx <- dl %>% 
      left_join(df) %>% 
      filter(year==2010, region == input$region) %>% 
      mutate(Fx = ifelse(is.na(Fx), 0, Fx)) %>% 
      pull(Fx) 

    Kt <- d_female  %>% 
      filter(region==input$region, year==2010) %>% 
      gather(age, pop, -region, -year, -Type, -`Parent code`) %>% 
      mutate(age = as.numeric(age)) %>% 
      mutate(pop = as.numeric(pop)) %>% 
      pull(pop)
    
    A <- Leslie(nLx, nFx)

    n = length(Lx)  # number of age groups
  
    n_projections <- (input$number_proj - 2010)/5
    
    K = matrix(0, nrow=n, ncol=n_projections+1)
    
    K[,1] = Kt

    # do the projection!
    for(i in 2:(n_projections+1)){
      K[,i] <- A %*% K[,i-1] 
    }
    
    # convert the projection result to a long dataframe
    Kdf <- as_tibble(K)
    colnames(Kdf) <- seq(from = 2010, to = (2010+n_projections*5), by = 5)
    Kdf <- cbind(age = 5*(seq(Kt)-1), Kdf)
    
    # get in long format and then add proportion of population in each age group
    dk <- Kdf %>% 
      gather(year, population, -age) %>%
      mutate(year = as.numeric(year)) %>% 
      group_by(year) %>%
      mutate(proportion = population/sum(population))
    
    
    p1 <- dk %>% 
      group_by(year) %>% 
      summarise(pop = sum(population)) %>% 
      ggplot(aes(year, pop)) + geom_line(lwd = 1.5) + 
      ggtitle("Total population")+
      labs(title= input$region, 
           subtitle='Female Population (1000s)') +
      theme_bw(base_size = 14)+
      xlim(c(2020, 2200)) +
      ylim(range(0,colSums(K)))
    
  # consolidate into 10-year age groups for plotting  
  
    dk10 <- dk %>% 
      mutate(age10 = factor(10* floor(age/10)) ) %>% 
      group_by(year, age=age10) %>% 
      summarize(population = sum(population),
                proportion = sum(proportion)) %>% 
      ungroup() 
      
    
    age_text = dk10 %>% 
                filter(year == max(year)) %>% 
                mutate(age = as.character(age)) %>% 
                select(year, age, proportion)
    
    p2 <- ggplot(data=dk10, aes(year, proportion, color = age)) + 
            geom_line(lwd = 1.5) + 
            ggtitle("Proportion by age group")+
            theme_bw(base_size = 14)+
            labs(color='10-yr\nage group') +
            guides(label='none') +
            scale_color_viridis_d(option='A') +
            xlim(c(2020, 2200)) +
            geom_text(data=age_text, 
                      aes(x=year+3, y= proportion, label=age),
                      size=3)

    p1+p2
    
  })
}