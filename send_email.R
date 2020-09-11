# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(blastula) #compose and send emails 


Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
# render Rmd into email-able html format
master_body <- render_email("C:/Users/Jensen/Documents/projects/covid19_email_script/master.Rmd")

# Get a nicely formatted date/time string
date_time <- add_readable_time()

#create_smtp_creds_key(
#  id = "gmail",
#  user = "hu.jensenhu@gmail.com",
#  provider = "gmail",
#  overwrite = T
#)

master_body %>% 
  smtp_send(
    from = "hu.jensenhu@gmail.com",
    to = "jensennhu@gmail.com", 
    subject = paste0("COVID-19 Report: ", date_time),
    credentials = creds_key(id = "gmail")
  )
