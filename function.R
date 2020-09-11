# load libraries
library(tidyverse) # data manipulation and visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(knitr) # kniting 
library(ggplot2) # visualizations
library(matrixStats) # stats
library(plotly) # visualizations
library(zoo) # working w/time series data
library(blastula) #compose and send emails 

# create view for international datasets 
jhu_func <- function(.data, switch, places){
  .data %>% 
    {if (switch == "Province")
      filter(.data, .data$`Province/State` == places)
      else filter(.data, .data$`Country/Region` == places)} %>% 
    melt(id.vars = colnames(.data)[1:4]) %>% 
    mutate(date = mdy(.data$variable),
           cases = .data$value)
}

# create views for states/other regions
state_fun <- function(.data ,area){
  area = enquo(area)
  .data %>% 
    {if ("state" %in% colnames(.data))
       filter(.data, .data$state == !!area)
      else .data} %>% 
    # row difference w/NA padding for x record(s) depending on look back
    transform(cases_new = c(NA, diff(.$cases)),
              cases_new_week = c(rep(NA, 7), diff(.$cases, lag = 7)),
              cases_new_two_week = c(rep(NA, 14), diff(.$cases, lag = 14)),
              deaths_new = c(NA, diff(.$deaths)),
              deaths_new_week = c(rep(NA, 7), diff(.$deaths, lag = 7)),
              deaths_new_two_week = c(rep(NA, 14), diff(.$deaths, lag =  14)))%>% 
    # percent change in cases from yesterday
    mutate(cases_pct_growth = (cases/lag(cases) - 1) * 100,
           # 7 day moving average for new cases 
           cases_week_mavg = rollmean(cases_new, k = 7, fill = NA),
           # percent growth in 7 days
           cases_week_pct_growth = (cases/lag(cases, n = 7) - 1) * 100,
           # percent change in 7 day moving average in new cases (from the previous week)
           cases_week_pct_mavg = (cases_week_mavg/lag(cases_week_mavg, n = 7) - 1) * 100,
           # percent change in 7 day moving average in new cases (from the previous day)
           cases_day_pct_mavg = (cases_week_mavg/lag(cases_week_mavg, n = 1) - 1) * 100,
           # Show sign
           sign = sign(cases_week_pct_mavg)
)
}


# function creating body of email
email_body_func <- function(.data, name){
  # most recent date
  most_recent <- max(.data$date)
  # determine inflection points for 7 day moving average
  updn <- c(0, diff(sign(.data$cases_week_pct_mavg)))
  ix <- which(updn != 0)
  # date of inflection points
  infl_dates <- .data[ix]$date
  # inflection recent (3 days ago)?
  recent_infl <- infl_dates[length(infl_dates)] == most_recent - 3
  # time since last inflection
  time_since_infl <- most_recent - infl_dates[length(infl_dates)] 
  
  # report on the current status
  total_reported <- .data %>% filter(date == max(date))
  paste0("As of yesterday",
         ", there has been a total of ", 
         formatC(total_reported$cases, big.mark=","), 
         " cases of COVID-19 in " ,
         name,
         " This is a change of ",
         formatC(total_reported$cases_new, big.mark=","),
         " (", round(total_reported$cases_pct_growth, 2),"%)",
         " new cases since the day prior.",
         " The 7 day moving average is currently ",
         formatC(round(.data %>% filter(date == most_recent - 3) %>% select(cases_week_mavg) %>% pull(), 2), big.mark=","),
         " new cases which is a ",
         formatC(round(.data %>% filter(date == most_recent - 3) %>% select(cases_day_pct_mavg) %>% pull(), 2), big.mark=","),
         "% change since the day prior and ",
         formatC(round(.data %>% filter(date == most_recent - 3) %>% select(cases_week_pct_mavg) %>% pull(), 2), big.mark=","),
         "% change since the week prior. ",
         "The immediate trend is heading ",
         ifelse((.data %>% 
                   filter(date == most_recent - 3) %>% 
                   select(sign) %>% pull()) < 0, "downward.", "upward."), 
         " Based on the addition of yesterday's data, there ",
         ifelse(recent_infl  == FALSE, "was not a recent (within 3 days) inflection point", paste0("was a recent (", infl_dates[length(infl_dates)], ") inflection point")),
         ifelse(recent_infl  == FALSE, paste0(" (days since last inflection point: ", time_since_infl, " --> ", infl_dates[length(infl_dates)], ")."),
                paste0(" (days since last inflection point: ", most_recent - infl_dates[length(infl_dates) - 1] , " --> ", infl_dates[length(infl_dates) - 1], ").")
          )
  )
}

# function to create 7-day moving average plots
mavg_plot_func <- function(.data){
  .data %>% 
    ggplot() +
    # daily new cases
    geom_bar(
      aes(x = date, y = cases_new),
      stat = "identity",
      width = 1.0,
      color = "grey"
    ) +
    # 7-day average of new cases 
    geom_line(aes(x = date, y = cases_week_mavg),
              color = "blue",
              size = 2) +
    xlab("Date") +
    ylab("Cases Count") +
    scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
    theme_classic()
}