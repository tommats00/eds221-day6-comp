rm(list = ls()) # clear global environment

library(tidyverse)
library(here)
library(janitor)

# Reading in data sets

wb_indicators <- read_csv(here("data", "wb_indicators.csv"), na = c("..", ""))
wb_metadata <- read_csv(here("data", "wb_indicators_metadata.csv"))


## Cleaning data from wide to long

wb_indicators_long <- wb_indicators %>%
  pivot_longer(cols = '2001 [YR2001]':'2020 [YR2020]',
               names_to = "year",
               values_to = "indicator_values")


## Using separate function to tidy it up, getting rid of unwanted columns

wb_data_clean <- wb_indicators_long %>%
  separate(col = year, into = c("year", "year_chr"), sep = " ") %>%
  dplyr::select(-year_chr, -"Country Code", -"Series Code")


# Dropping lines that have NA's in them from Series Name column
# Also using pivot_wider() to make more columns from Series Name

wb_data_tidy <- wb_data_clean %>%
  tidyr::drop_na("Series Name") %>%
  tidyr::pivot_wider(names_from = "Series Name",
                     values_from = indicator_values)


## Renaming columns using names() function

names(wb_data_tidy) <- c("country",
                         "year",
                         "access_clean_fuels_pp",
                         "access_electricity_pp",
                         "co2_emissions_kt",
                         "fossil_fuel_cons_pt",
                         "water_stress")



# Exclude rows using filter() function, filtering rows only for US

us_wb <- wb_data_tidy %>%
  filter(country == "United States")


## Interested in nicargua co2 emissions, use filter for only Nicaragua, then use select for just
## year and co2 emissions columns


nicaragua_co2 <- wb_data_tidy %>%
  filter(country == "Nicaragua") %>%
  select(year, co2_emissions_kt)


## Taking out columns of variables we don't want

wb_subset <- wb_data_tidy %>%
  select(-c(water_stress, access_electricity_pp))

# Renaming columns

wb_newnames <- wb_data_tidy %>%
  rename(elec = access_electricity_pp, co2 = co2_emissions_kt)



# Using mutate to change class of variables, "year" is a character string
class(wb_data_tidy$year)

wb_data_tidy$year <- as.numeric(wb_data_tidy$year) # this is how to do it in [base] R, but....

wb_data_tidy <- wb_data_tidy %>%
  mutate(year = as.numeric(year)) # it is better to use tidyverse syntax especially since all the clean
# is coming from the tidyverse package.

class(wb_data_tidy$year) # is now numeric



# creating a new column using mutate()
wb_co2_tons <- wb_data_tidy %>%
  mutate(co2_tons = co2_emissions_kt * 1000)

# Group() and summarize function to group it by country and add up all co2_emissions_kt for all data.
co2_total <- wb_data_tidy %>%
  group_by(country) %>%
  summarize(total_co2_kt = sum(co2_emissions_kt, na.rm = TRUE))


## filter tidy data with just US and Mexico

#using %in% will look for countries "in" this vector

us_ca_data <- wb_data_tidy %>%
  filter(country %in% c("United States", "Canada"))

# filtering is really good at looking at specific values for variables (columns)
data_2020 <- wb_data_tidy %>%
  filter(year == 2020)

### Mutate() is adding a new column using existing data,

### Group_by() and summarize() collapses data

## Group_by puts all data into that one row. So

co2_annual <- wb_data_tidy %>%
  group_by(year) %>%
  summarize(annual_total_co2_kt = sum(co2_emissions_kt, na.rm = TRUE))

ggplot(data = co2_annual, aes(x = year, y = annual_total_co2_kt)) +
  geom_line()

