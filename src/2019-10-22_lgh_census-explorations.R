
#'--- 
#' title: "LGH Daily census - time series analysis"
#' author: "Nayef Ahmad"
#' date: "2019-10-22"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(tsibble)
library(feasts)
library(lubridate)

setup_denodo()

#' # Overview
#'
#' Unlike daily ED visits data, daily census count data is likely to have
#' correlated errors. So it's not a good idea to use classical normal-based
#' regression for inference or prediction. Instead, let's use time series
#' methods.
#' 

#+ rest 
#' # Parameters  
#' 
site <- "LGH"
n_unit <- "LGH 4 East"

start_param <- "20180101"
end_param <- "20191020"

#' # Data 
#' 


df1.census <- vw_census %>% 
  filter(facility_short_name == glue::glue(site), 
         nursing_unit_desc_at_census == glue::glue(n_unit), 
         census_date_id >= start_param,  
         census_date_id < end_param) %>% 
  select(patient_id, 
         census_date_id) %>% 
  collect


df2.census_group <- 
  df1.census %>% 
  count(census_date_id) %>% 
  mutate(census_date = ymd(census_date_id), 
         diff = n - lag(n)) %>% 
  fill_dates(census_date, 
             ymd(start_param), 
             ymd(end_param)) %>% 
  replace_na(list(n = 0, 
                  diff = 0)) %>% 
  # remove last row (it will have NAs)
  slice(1:n()-1)


df2.census_group %>% 
  ggplot(aes(x = census_date, 
             y = n)) + 
  geom_line()

df2.census_group %>% 
  ggplot(aes(x = census_date, 
             y = diff)) + 
  geom_line()

#' ## Time series structure 
#' 

acf(df2.census_group$diff)
pacf(df2.census_group$diff)

#' ## Interpreting the ACF & PACF 
#' 
#' From [Shumway and Stoffer](http://db.ucsd.edu/static/TimeSeries.pdf), p108:
#'
#' ![](`r here::here("images", "acf-and-pacf.jpg") `)
#'
#' Also see examples
#' [here](http://people.stat.sfu.ca/~lockhart/richard/804/06_1/lectures/IdentExamples/web.pdf).
#'

#' **Note:** the series must be stationary for the ACF/PACF to depend only on
#' the lag. The differenced series does look pretty stationary, but I haven't
#' actually tested for that.
#' 

#' \  
#' \  
#' 
#' *Following notes are for LGH 4 East. They will not apply if you change those parameters.* 
#' 
#' Looks like this should be a MA model. The ACF pretty much cuts off after lag
#' 1, while the PACF tails off.
#'
#' There is some correlation at lag 7, 14, 21, etc. representing weekly effects.
#' They're smaller than the effect of the MA lag 1, but would probably be best
#' to do the following:
#'
#' 1. Regress *first difference* of daily census on day of week (model 1)
#'
#' 2. Take residuals from model 1
#'

#' 3. **Inference**: look at ACF/PACF of residuals. If they look like an MA
#' process, this implies that there are no other explanatory variables. Census
#' changes are "random" in the sense of an MA process.
#' 

#' 4. **Prediction**: fit a MA 1 model to the residuals. The forecast for the census *change* for any
#' future day will be the sum of the fcast from the MA process and the
#' day-of-week effect.
#' 

#' Okay, in practice, the process of fitting an AR or MA model on the
#' differenced series and then integrating back to the series we care about will
#' be handled by fitting an ARIMA model.
#' 



#' # Models 
#' 





#' # Appendix
#' ## Checks 
#' 
#' 
num_days <- difftime(ymd(end_param),
                     ymd(start_param)) %>% as.numeric()

#' `r abs(num_days - nrow(df2.census_group)) <= 1`