#.................................................................
# Carl Schmertmann
# 14 Apr 2022
# 
# variant of Monica Alexander's shiny app code for projecting WPP
# populations forward at constant rates in order to 
# illustrate stable age structure
# 
# in this version we pre-compute all projections for all
# countries in order to speed up display
#.................................................................

library(tidyverse)
library(patchwork)

start_year = 2015
final_year = 2175
nsteps     = 1 +(final_year - start_year)/5

# read WPP 2019 abridged life table file ----

# modified Monica's code so that the [0,1) and [1,5) groups
# are combined immediately in the dl dataset, rather than being
# calculated later on the fly

age_crosswalk = tibble(
  age  = c(0,1,seq(5,100,5)),
  age5 = 5*floor(age/5) 
)

dl <- read_csv("WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.csv", 
               skip = 12) %>% 
  rename(region = `Region, subregion, country or area *`,
         Lx = `Number of person-years lived L(x,n)`,
         age = `Age (x)`,
         period = Period) %>% 
  select(Type,region, period, age, Lx) %>% 
  mutate(year = as.numeric(substr(period, 1, 4)), Lx = Lx/1e5) %>% 
  filter(year == start_year) %>% 
  left_join(age_crosswalk) %>% 
  group_by(Type,region, period, age = age5, year) %>% 
  summarize( Lx = sum(Lx)) %>% 
  filter(is.finite(Lx)) %>% 
  pivot_wider(id_cols=c('Type','region','period','year'), 
              values_from = 'Lx', 
              names_prefix = 'Lx',
              names_from='age')


# read WPP 2019 ASFR file ----

df <- read_csv("WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.csv", skip = 16) %>% 
  filter(Type != 'Label/Separator') %>% 
  rename(region = `Region, subregion, country or area *`, period = Period) %>% 
  select(-Index, -Variant, -Notes, -`Country code`,-`Parent code`) %>% 
  rename('15'='15-19', '20'='20-24', '25'='25-29', '30'='30-34',
         '35'='35-39', '40'='40-44', '45'='45-49') %>% 
  mutate(year = as.numeric(substr(period, 1, 4))) %>% 
  filter(year==start_year) %>% 
  gather(age, Fx, -region, -period, -year, -Type) %>% 
  mutate(age = as.numeric(age), Fx = as.numeric(Fx)/1000) %>% 
  pivot_wider(id_cols=c('Type','region','period','year'), 
              values_from = 'Fx', 
              names_prefix = 'Fx',
              names_from='age')


# read WPP 2019 female population file ----

d_female <- read_csv("WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.csv", 
                     skip = 12) %>% 
              rename(region = `Region, subregion, country or area *`,
                     year = `Reference date (as of 1 July)`) %>% 
              filter(year == start_year, Type != 'Label/Separator') %>% 
              select(Type,region, year, `0`:`100`) %>% 
              mutate(across(`0`:`100`, as.numeric)) %>% 
              rename_with( ~ paste0('Kx',.), -c('region','year','Type'))

# join the files to make sure that all the region names are 
# properly matched

bigWPP = df %>% 
          left_join(dl) %>% 
          left_join(d_female)

# arrange WPP 2015 data into arrays for computation ----

# Lx 
bigLx = bigWPP %>% 
         select(starts_with('Lx')) %>% 
         as.matrix()


dimnames(bigLx) = list( bigWPP$region, paste(seq(0,100,5)) )

#Fx
bigFx = 0*bigLx

Fxdata = bigWPP %>% 
          select(starts_with('Fx')) %>% 
          as.matrix()

bigFx[, paste(seq(15,45,5))] = Fxdata

#Kx

bigKx = bigWPP %>% 
        select(starts_with('Kx')) %>% 
        as.matrix()

dimnames(bigKx) = list( bigWPP$region, paste(seq(0,100,5)) )

# pre-compute all projections ----
#   from 2015 female populations
#   at 2015-2020 rates

# age group survival probs
# with (slightly arbitrary) survival prob for 100+ -> 100+
# assume 100+ mortal rate approx = 0.7, so 5-yr survival is exp(-3.5)
# or approx 3%

L       = paste( seq(from=0, to=95 , by=5)) 
H       = paste( seq(from=5, to=100, by=5))
ngroups = length(L)+1

bigSx           = cbind( bigLx[, H] / bigLx[, L], 0.03)
dimnames(bigSx) = dimnames(bigLx)

# fertility multipliers 

DPB = .4886 # daughters per birth

bigFmult = cbind( DPB * bigLx[,'0']/2 * (bigFx[,L] + bigSx[,L] * bigFx[,H]),
                  0)

# bigProj will be country x age x time
bigProj = array(0, dim=c(nrow(bigKx), ncol(bigKx), nsteps))
dimnames(bigProj) = list( rownames(bigKx),
                          colnames(bigKx),
                          seq(start_year, final_year, by=5))
bigProj[,,1] = bigKx

for (y in 2:nsteps) {
  bigProj[, H , y] = bigProj[, L, y-1] * bigSx[,L]
  bigProj[, ngroups, y] = bigProj[, ngroups, y] +
                          bigSx[,ngroups] * bigProj[, ngroups, y-1] 
  bigProj[, '0', y] = rowSums( bigProj[,,y-1] * bigFmult)
}

# projected proportions by age
bigCx = prop.table(bigProj, margin=c(1,3))

# calculate summaries for all populations ----
# Total fertility rates
# Net reproduction rates
# mean ages of childbearing,
# (approximate) intrinsic growth rates
# (approximate) stable age structure

bigTFR = rowSums( 5     * bigFx)
bigNRR = rowSums( bigLx * bigFx * DPB)

mid_ages = seq(from=2.5, to=102.5, by=5)

bigMACB = apply(bigFx * bigLx, 1, 
                function(fx) {weighted.mean(mid_ages, w=fx)} )

bigR = log(bigNRR) / bigMACB

# approx stable age structure (by 5-year groups)
bigCx_stable = (bigLx * outer( bigR, mid_ages, 
                        function(rr,xx) exp(-rr*xx))) %>% 
                prop.table(margin=1)


# create a list of selection options in a custom order:
# sorted alphabetically within (World, Region, Subregion, Country/Area) types
keepers = c('World','Region','Subregion','Country/Area')

regions = bigWPP %>% 
            filter(Type %in% keepers) %>% 
            mutate(categ = factor(Type, levels=keepers)) %>% 
            group_by(categ, region) %>% 
            slice(1) %>% 
            pull(region)


vars_to_save = c("bigCx", "bigCx_stable", "bigFmult", "bigFx", "bigKx", "bigLx", 
                 "bigMACB", "bigNRR", "bigProj", "bigR", "bigSx", "bigTFR", "bigWPP",
                 "start_year", "final_year", "nsteps", "ngroups", "regions")

save(list = vars_to_save, file='precompute-projections-WPP-2019.Rdata')
