##############################
##                          ##
##  Retrospective Analysis  ##
##                          ##
##############################

# Explanation for CDC Vaccine Data's dip on 9/23/2021
# https://www.cdc.gov/coronavirus/2019-ncov/vaccines/distributing/vaccination-data-archived-updates.html
# On September 23, 2021, data review and reporting adjustments resulted in a decrease in the number of 
# vaccine doses administered for Washington State of 473,191 doses. The adjustments are the results of 
# updates to how pharmacies report data to CDC and/or the jurisdictions.

# Packages -----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(httr) # api 
library(jsonlite) # api

# Data --------------------------------------------------------------------
cases_raw <- read_csv(here("data", "skcph-covid-cases.csv"))
hospital_raw <- read_csv(here("data", "skcph-covid-hospitalizations.csv"), skip = 1)
utilization_raw <- read_csv(here("data", "skcph-covid-hospital-utilization.csv"))

# Note: using the 2020 Census data to get King County's population of 2,269,675  
# https://www.census.gov/quickfacts/kingcountywashington
kc_population <- 2269675



# King County Vaccine Data from CDC
# https://data.cdc.gov/resource/8xkx-amqh.json
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
vaccine_api_pull <- GET("https://data.cdc.gov/resource/8xkx-amqh.json?fips=53033")
vaccine_raw <- jsonlite::fromJSON(rawToChar(vaccine_api_pull$content))



# Quick Cleanup -----------------------------------------------------------

cases <- cases_raw %>% 
  clean_names() %>% 
  rename(date = collection_date,
         cases_average_7_day = x7_day_average) %>% 
  mutate(date = mdy(date),
         cases_100k_7_day = ((cases_average_7_day*7) / kc_population)*100000) # Getting the "Total number of cases over 7 days per 100k" rate
head(cases)
tail(cases)

hospital <- hospital_raw %>% 
  clean_names() %>% 
  rename(hospitalizations_average_7_day = x7_day_average) %>% 
  mutate(hospitalizations_100k_7_day = ((hospitalizations_average_7_day*7) / kc_population)*100000)
head(hospital)
tail(hospital)

utilization <- utilization_raw %>% 
  clean_names() %>% 
  rename(date = collection_date) %>% 
  mutate(date = mdy(date))
head(utilization)


vaccine <- vaccine_raw %>% 
  select(date, recip_county, series_complete_pop_pct, booster_doses, booster_doses_vax_pct, census2019, census2019_5pluspop) %>% 
  mutate(date = ymd(str_sub(date, 1, 10))) %>% 
  filter(date > ymd("2021-07-01")) %>% 
  rename(vaccinated_percent = series_complete_pop_pct,
         booster_percent = booster_doses_vax_pct)
head(vaccine)



# Analysis ----------------------------------------------------------------
# creating a merged file
guidelines_everything <- cases %>%
  left_join(hospital, by = "date") %>% 
  left_join(utilization, by = "date")%>% 
  left_join(vaccine, by = "date") %>% 
  mutate(cases_under_200 = case_when(cases_100k_7_day < 200 ~ 1,
                                     TRUE ~ 0)) %>% 
  mutate(low_covid_hospital = case_when(hospitalizations_100k_7_day < 10 ~ "low",
                                        hospitalizations_100k_7_day >= 10 & hospitalizations_100k_7_day < 20 ~ "medium",
                                        hospitalizations_100k_7_day > 20 ~ "high"),
         high_covid_hospital = case_when(hospitalizations_100k_7_day < 10 ~ "medium",
                                         hospitalizations_100k_7_day >= 10 ~ "high")) %>% 
  mutate(low_covid_bed_pct = case_when(bed_occupancy < 10 ~ "low",
                                       bed_occupancy >= 10 & bed_occupancy < 14.9 ~ "medium",
                                       bed_occupancy >= 15 ~ "high"),
         high_covid_bed_pct = case_when(bed_occupancy < 10 ~ "medium",
                                        bed_occupancy >= 10 ~ "high")) %>% 
  mutate(low_both = paste(low_covid_hospital, low_covid_bed_pct),
         high_both = paste(high_covid_hospital, high_covid_bed_pct)) %>% 
  mutate(final_low = case_when(str_detect(low_both, "high") ~ "high",
                               str_detect(low_both, "medium") ~ "medium",
                               str_detect(low_both, "low") ~ "low")) %>% 
  mutate(final_high = case_when(str_detect(high_both, "high") ~ "high",
                                str_detect(high_both, "medium") ~ "medium",
                                str_detect(high_both, "low") ~ "low")) %>% 
  mutate(new_cdc_guideline = case_when(cases_under_200 == 1 ~ final_low,
                                       cases_under_200 == 0 ~ final_high))


# only keeping the data needed for the visuals
guidelines_clean <- guidelines_everything %>% 
  select(date,
         cases_100k_7_day,
         hospitalizations_100k_7_day,
         bed_occupancy,
         vaccinated_percent,
         booster_percent,
         new_cdc_guideline)

# reshaping for vaccine visual
guidelines_vaccine <- guidelines_everything %>% 
  select(date,
         two_doses = vaccinated_percent,
         booster = booster_percent,
         new_cdc_guideline) %>% 
  pivot_longer(two_doses:booster, names_to = "vaccine_type", values_to = "vaccination_percent") %>% 
  mutate(vaccination_percent = as.numeric(vaccination_percent))

# ggplot ------------------------------------------------------------------
# Cases by CDC Guideline
ggplot(guidelines_clean, aes(x = date, y = cases_100k_7_day)) +
  geom_line() + 
  geom_point(aes(color = new_cdc_guideline)) +
  geom_hline(yintercept=10) + # Low
  geom_hline(yintercept=50) + # Moderate
  geom_hline(yintercept=100) # Substantial; higher is High



# Vaccine by CDC Guideline
ggplot(guidelines_vaccine, aes(x = date, y = vaccination_percent, color = new_cdc_guideline, group = vaccine_type)) +
  geom_line()
