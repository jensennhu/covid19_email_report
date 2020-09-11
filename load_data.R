# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(data.table) # formatting 
library(knitr) # kniting 
library(ggplot2) # visualizations
library(matrixStats) # stats
library(plotly) # visualizations
library(zoo) # working w/time series data
library(blastula) #compose and send emails 


# load github data from ny times
covid_us <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
covid_state <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
covid_county <- fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# load github data from johns hopkins
jhu_cases <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## format dates ####
covid_us$date <- ymd(covid_us$date)
covid_state$date <- ymd(covid_state$date)
covid_county$date <- ymd(covid_county$date)

## explore overall US #### 
#--dates captured in dataset--#
days_monitored <- n_distinct(covid_us$date)
first_day <- min(covid_us$date)
most_recent <- max(covid_us$date)


# get functions
source("function.R")

# calculate metrics
daily_new <- state_fun(covid_us)

# select states
ca <- state_fun(covid_state, "California")
ny <- state_fun(covid_state, "New York")
il <- state_fun(covid_state, "Illinois")
wa <- state_fun(covid_state, "Washington")
ma <- state_fun(covid_state, "Massachusetts")
tx <- state_fun(covid_state, "Texas")
nc <- state_fun(covid_state, "North Carolina")
mo <- state_fun(covid_state, "Missouri")

# select cities
nyc <- state_fun(covid_county %>% filter(county == "New York City"), "New York")
la  <- state_fun(covid_county %>% filter(county == "Los Angeles"), "California")
ck  <- state_fun(covid_county %>% filter(county == "Cook"), "Illinois")
kg  <- state_fun(covid_county %>% filter(county == "King"), "Washington")
sf  <- state_fun(covid_county %>% filter(county == "Suffolk"), "Massachusetts")
mk  <- state_fun(covid_county %>% filter(county == "Mecklenburg"), "North Carolina")
sb  <- state_fun(covid_county %>% filter(county == "San Bernardino"), "California")
hr  <- state_fun(covid_county %>% filter(county == "Harris"), "Texas")
sl  <- state_fun(covid_county %>% filter(county == "St. Louis"), "Missouri")

# international locations
hk <- state_fun(jhu_func(jhu_cases, "Province", "Hong Kong"))
sg <- state_fun(jhu_func(jhu_cases, "Province", "Shanghai"))


# compose body of email for select locations
usa_body <- email_body_func(daily_new, "the USA.") 
nyc_body <- email_body_func(nyc, "New York City, NY.")
la_body <- email_body_func(la, "Los Angeles County, CA.")
seattle_body <- email_body_func(kg, "Kings County, WA.")
boston_body <- email_body_func(sf, "Suffolk County, MA.")
chicago_body <- email_body_func(ck, "Cook County, IL.")
mecklenburg_body <- email_body_func(mk, "Mecklenburg, NC.")
hk_body <- email_body_func(hk, "Hong Kong.")
shanghai_body <- email_body_func(sg, "Shanghai.")
sanbernardino_body <- email_body_func(sb, "San Bernardino County, CA.")
harris_body <- email_body_func(hr, "Harris County, TX.")
texas_body <- email_body_func(tx, "Texas, USA.")
ncarolina_body <- email_body_func(nc, "North Carolina, USA.")
missouri_body <- email_body_func(mo, "Missouri, USA.")
saint_louis_body <- email_body_func(sl, "Saint Louis County, MO.")
illinois_body <- email_body_func(il, "Illinois, USA.")
