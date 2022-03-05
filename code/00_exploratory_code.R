library(tidyverse)
library(readxl)
library(httr) # api 
library(jsonlite) # api
library(lubridate)
library(janitor)
# install.packages("RSocrata")
# library("RSocrata")
# https://www.r-bloggers.com/2018/10/converting-nested-json-to-a-tidy-data-frame-with-r/
kc_zips <- read_csv("C:\\Users\\Yuri\\Downloads\\king_county_zips.csv")


dat <- readxl::read_excel("C:\\Users\\Yuri\\Downloads\\biweekly_geo-feb-22.xlsx")
dat2 <- read_csv("C:\\Users\\Yuri\\Downloads\\covid-hospitalizations.csv")
head(dat2)

# Manually logged Hospitalization data from King County Public Health (https://kingcounty.gov/depts/health/covid-19/data/summary-dashboard.aspx)
# because I wasn't able to make my CDC data pull match the CDC's data. Mine was a bit higher -- enough to make it so it looks like we 
# should be rated as "medium" and not low.
# Though I'm going to keep using the CDC data for hospital utilization because that data is closer to what the CDC presents.
skcph_hospitalization <- read_csv("C:\\Users\\Yuri\\Downloads\\King County Hospitalization - Seattle King County Public Health - Sheet1.csv", skip = 1)

# Vaccine -----------------------------------------------------------------
# King County Vaccine Data from CDC
# https://data.cdc.gov/resource/8xkx-amqh.json
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh
api_king_county_vaccination <- GET("https://data.cdc.gov/resource/8xkx-amqh.json?fips=53033")
kc_vac_df <- jsonlite::fromJSON(rawToChar(king_county_vaccination$content))


# Filtering down
names(kc_vac_df)
kc_vac_df_filtered<- kc_vac_df %>% 
  select(date, recip_county, series_complete_pop_pct, booster_doses, booster_doses_vax_pct, census2019, census2019_5pluspop) %>% 
  mutate(date = ymd(str_sub(date, 1, 10))) %>% 
  filter(date > ymd("2021-07-01"))
head(kc_vac_df_filtered)

ggplot(data = kc_vac_df_filtered, aes(x = date, y = as.numeric(series_complete_pop_pct))) +
  geom_line()

# Cases -------------------------------------------------------------------
# https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-County-Level-of-Community-T/nra9-vzzn
api_king_county_cases <- GET("https://data.cdc.gov/resource/nra9-vzzn?fips_code=53033")
kc_case_df <- jsonlite::fromJSON(rawToChar(api_king_county_cases$content))
kc_case_df

# Filtering down
names(kc_case_df)
kc_case_df_filtered<- kc_case_df %>% 
  select(county_name, date, cases_per_100k_7_day_count , community_transmission_level) %>% 
  mutate(date = ymd(str_sub(date, 1, 10)),
         cases_per_100k_7_day_count = as.numeric(str_replace_all(cases_per_100k_7_day_count, ",", ""))) %>% 
  filter(date > ymd("2021-07-01")) %>% 
  select(date, county_name, cases_per_100k_7_day_count, community_transmission_level) 
kc_case_df_filtered



# Cases SKCPH -------------------------------------------------------------
# CDC data is very weird.
# Their API and site data for 2/24/22 is 211.96 for 7-day/100k;
# but their new guideline data that was updated 2/24/22 is at 112.35 -- big difference!
# seeeing what the Seattle King County Public Health has to say about all this
skcph_cases <- read_csv("C:\\Users\\Yuri\\Downloads\\SKCPH Cases - Sheet1.csv")



skcph_cases_clean <- skcph_cases %>% 
  mutate(date = mdy(Collection_Date)) %>% 
  clean_names() %>% 
  select(date, cases = positives, x7_day_average) %>% 
  mutate(cases_per_100k_7_day_count = ((x7_day_average/2253000)*100000)*7)


# Hospitalization ----------------------------------------------------------
# this data is a bit more complicated.
# https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u

api_king_county_hospitalizations <- GET("https://healthdata.gov/resource/anag-cw7u.json?fips_code=53033")
kc_hospital_df <- jsonlite::fromJSON(rawToChar(api_king_county_hospitalizations$content))
names(kc_hospital_df)
kc_hospital_df_filtered <- kc_hospital_df %>% 
  select(fips_code, hospital_pk:hospital_name, total_beds_7_day_avg, inpatient_beds_used_covid_7_day_avg) %>% 
  mutate(total_beds_7_day_avg = as.numeric(str_replace_all(total_beds_7_day_avg, ",", "")),
         inpatient_beds_used_covid_7_day_avg = as.numeric(str_replace_all(inpatient_beds_used_covid_7_day_avg, ",", ""))) %>% 
  filter(inpatient_beds_used_covid_7_day_avg >= 0) %>% 
  group_by(fips_code, collection_week) %>% 
  summarise(average_available_beds_7day = sum(total_beds_7_day_avg, na.rm = T),
            average_covid_hospitalized_7day = sum(inpatient_beds_used_covid_7_day_avg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(percent_beds_covid = average_covid_hospitalized_7day / average_available_beds_7day,
         date = str_sub(collection_week, 1, 10),
         county = "King County") %>% 
  # Adding average per 100k, using 2020 Census population of 2,253,000
  mutate(covid_hospitalized_per_100k = (average_covid_hospitalized_7day/2253000)*100000) %>% 
  mutate(date_fancy = ymd(date)) %>% 
  select(date = date_fancy, county, hospitalized_per_100k = covid_hospitalized_per_100k, average_covid_hospitalized_7day, average_available_beds_7day, percent_beds_covid)


# Adding every date since this is only weekly average
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/seq.Date


kc_hospital_usage_daily <- data.frame(date = seq(as.Date("2021/2/19"), as.Date("2022/2/22"), "days")) %>% 
  left_join(kc_hospital_df_filtered, by = c("date" = "date_fancy")) %>% 
  # https://tidyr.tidyverse.org/reference/fill.html
  fill(county, .direction = "down") %>% 
  fill(covid_hospitalized_per_100k, .direction = "down") %>% 
  fill(average_covid_hospitalized_7day, .direction = "down") %>% 
  fill(average_available_beds_7day, .direction = "down") %>% 
  fill(percent_beds_covid, .direction = "down") %>% 
  filter(date > ymd("2021-07-01")) %>% 
  select(date, county, average_available_beds_7day, percent_beds_covid)

skcph_hospitalization_clean <- skcph_hospitalization %>% 
  clean_names() %>% 
  mutate(hospitalized_per_100k = ((x7_day_average*7) / 2253000)*100000)



# Merging Data ------------------------------------------------------------
# Merging datasets to get the new CDC guidelines
# https://www.cdc.gov/coronavirus/2019-ncov/your-health/covid-by-county.html
# If cases per 100k is fewer than 200:
# New COVID-19 admissions per 100,000 population (7-day total): Low (<10), Medium (10.0 - 19.9), High (<=20)
# Percent of staffed inpatient beds occupied by COVID-19 patients (7-day average): Low (<10), Medium (10.0 - 14.9), High (<=15)
# If cases per 100k is 200+:
# New COVID-19 admissions per 100,000 population (7-day total): Low (NA), Medium (<10), High (>10)
# Percent of staffed inpatient beds occupied by COVID-19 patients (7-day average): Low (NA), Medium (<10), High (>10)



cdc_new_guidelines <- skcph_cases_clean %>% 
  left_join(skcph_hospitalization_clean, by = "date") %>% 
  left_join(kc_hospital_usage_daily, by = "date") %>% 
  # left_join(kc_case_df_filtered %>% select(-county_name), by = "date") %>% 
  mutate(cases_under_200 = case_when(cases_per_100k_7_day_count < 200 ~ 1,
                                     TRUE ~ 0)) %>% 
  mutate(low_covid_hospital = case_when(hospitalized_per_100k < 10 ~ "low",
                                        hospitalized_per_100k >= 10 & hospitalized_per_100k < 20 ~ "medium",
                                        hospitalized_per_100k > 20 ~ "high"),
         high_covid_hospital = case_when(hospitalized_per_100k < 10 ~ "medium",
                                         hospitalized_per_100k >= 10 ~ "high")) %>% 
  mutate(low_covid_bed_pct = case_when(percent_beds_covid < .10 ~ "low",
                                       percent_beds_covid >= .10 & percent_beds_covid < .149 ~ "medium",
                                       percent_beds_covid >= .15 ~ "high"),
         high_covid_bed_pct = case_when(percent_beds_covid < .10 ~ "medium",
                                        percent_beds_covid >= .10 ~ "high")) %>% 
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



cdc_new_guidelines_cdc_data <- kc_case_df_filtered %>% 
  left_join(kc_hospital_df_filtered %>% select(-percent_beds_covid), by = "date") %>% 
  left_join(kc_hospital_usage_daily, by = "date") %>% 
  # left_join(kc_case_df_filtered %>% select(-county_name), by = "date") %>% 
  mutate(cases_under_200 = case_when(cases_per_100k_7_day_count < 200 ~ 1,
                                     TRUE ~ 0)) %>% 
  mutate(low_covid_hospital = case_when(hospitalized_per_100k < 10 ~ "low",
                                        hospitalized_per_100k >= 10 & hospitalized_per_100k < 20 ~ "medium",
                                        hospitalized_per_100k > 20 ~ "high"),
         high_covid_hospital = case_when(hospitalized_per_100k < 10 ~ "medium",
                                         hospitalized_per_100k >= 10 ~ "high")) %>% 
  mutate(low_covid_bed_pct = case_when(percent_beds_covid < .10 ~ "low",
                                       percent_beds_covid >= .10 & percent_beds_covid < .149 ~ "medium",
                                       percent_beds_covid >= .15 ~ "high"),
         high_covid_bed_pct = case_when(percent_beds_covid < .10 ~ "medium",
                                        percent_beds_covid >= .10 ~ "high")) %>% 
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


ggplot(cdc_new_guidelines, aes(x = date, y = cases_per_100k_7_day_count)) +
  geom_line() + 
  geom_point(aes(color = new_cdc_guideline)) +
  geom_hline(yintercept=10) + # Low
  geom_hline(yintercept=50) + # Moderate
  geom_hline(yintercept=100) # Substantial; higher is High


ggplot(cdc_new_guidelines_cdc_data, aes(x = date, y = cases_per_100k_7_day_count)) +
  geom_line() + 
  geom_point(aes(color = new_cdc_guideline)) +
  geom_hline(yintercept=10) + # Low
  geom_hline(yintercept=50) + # Moderate
  geom_hline(yintercept=100) # Substantial; higher is High


# CDC New Guideline Data --------------------------------------------------

cdc_source_of_truth <- read_csv("C:\\Users\\Yuri\\Downloads\\COVID-19 Community Levels - County.csv") %>% filter(FIPS == "53033")


