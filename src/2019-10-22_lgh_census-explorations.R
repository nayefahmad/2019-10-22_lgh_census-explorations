
#'--- 
#' title: "LGH Census explorations"
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
#' Unlike ED visits data, census is likely to have correlated errors. So it's
#' not a good idea to use classical normal-based regression. Instead, let's use
#' time series methods.
#' 

#' # Data 
#' 

#+ rest 
df1.census <- vw_census %>% 
  filter(facility_short_name == "LGH", 
         nursing_unit_desc_at_census == "LGH 4 East", 
         census_date_id >= "20180101", 
         census_date_id < "20191021") %>% 
  select(patient_id, 
         census_date_id) %>% 
  collect


df2.census_group <- 
  df1.census %>% 
  count(census_date_id) %>% 
  mutate(census_date = ymd(census_date_id), 
         diff = n - lag(n)) %>% 
  drop_na()


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

#' **Interpreting the ACF & PACF:**
#' 
#' #' From Shumway and Stoffer, p108:
#'
#' ![](`r here::here("images", "acf-and-pacf.jpg") `)
#'
#' Also see examples
#' [here](http://people.stat.sfu.ca/~lockhart/richard/804/06_1/lectures/IdentExamples/web.pdf).
#'
#' 
#'
#' Looks like this should be a MA model. The ACF pretty much cuts off after lag
#' 1, while the PACF tails off.
#'
#' There is some correlation at lag 7, 14, 21, etc. representing weekly effects.
#' They're smaller than the effect of the MA lag 1, but would probably be best
#' to do the following:
#'
#' 1. Regress census on day of week (model 1)
#'
#' 2. Take residuals from model 1
#'
#' 3. **Inference**: look at ACF/PACF of residuals. If they look like an MA
#' process, this implies that there are no other explanatory variables.
#'
#' 4. **Prediction**: fit a MA 1 model to the residuals. The forecast for any
#' future day will be the sum of the fcast from the MA process and the
#' day-of-week effect.
#'



#' # Models 
#' 