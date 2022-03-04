# New CDC Guidelines Retrospective

## What this is
When the CDC announced new ["Community-Level"](https://www.cdc.gov/coronavirus/2019-ncov/your-health/covid-by-county.html) guidelines to determine masking recommendations, I was struck by how much more lax they seemed compared to the existing ["Community Transmission"](https://covid.cdc.gov/covid-data-tracker/#county-view?list_select_state=all_states&list_select_county=all_counties&data-type=Risk) levels. 

To that end, I wanted see how the new guidelines would look if they had been applied at earlier stages of the pandemic -- specifically right before, during, and after the Delta variant spike. I focused on King County, Washington, where I live, to do this analysis.

## A note on the data
The CDC data is messier and less transparent than I would have hoped. I originally tried to pull all the data directly from the CDC in order to best replicate their analysis. This ended up not working.

If you go to the ["U.S. COVID-19 Community Levels by County Map"](https://www.cdc.gov/coronavirus/2019-ncov/science/community-levels-county-map.html) page, you can see the rating of your county with the new guidelines. You can even download the data, but the CSV file does not have any historical data. Along with this, the data here does not seem to match the data from their data API (that does allow for historical data pulls).

As of March 1st, 2022, the ["U.S. COVID-19 Community Levels by County Map"](https://www.cdc.gov/coronavirus/2019-ncov/science/community-levels-county-map.html) data for King County is updated as of February 24th, 2022. And for February 24th, 2022, it says there are 112.35 Cases per 100k in the past 7 days. 

Going to the [data.cdc.gov site for COVID-19 cases by county](https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-County-Level-of-Community-T/nra9-vzzn), the King County, for Feburary 24th, 2022, it says there are 211.96 cases per 100k in the past 7 days.

Given these wide disparities, I decided to grab data from the [Seattle King County Public Health site](https://kingcounty.gov/depts/health/covid-19/data/summary-dashboard.aspx). They have data for cases, hospitalizations, and bed occupancy -- the variables needed. However, I had to manually grab the hospitalization and bed occupancy data because it's not avaiale in any downloadable form. Very annoying.
