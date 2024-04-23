
library("tidyverse")
library("readr")

# ParlGov Development Version (4. Nov. 2023) ------------------------------
# if countries are not included (e.g. North Macedonia, Montenegro) --> Wikipedia

election_dates <- read_csv("view_election.csv")

# Get the Year of the Elections (and month & day for later)

election_dates$year <- year(election_dates$election_date)
election_dates$month <- month(election_dates$election_date)
election_dates$day <- day(election_dates$election_date)

# Combine the two in case there are multiple election within a year

election_dates <- election_dates %>% mutate(unite(election_dates, 
                                                  year_month, c(year, month)))

# Filter for parliamentary elections in countries and time span of ESS Round 10

election_dates <- election_dates %>% select(country_name_short, country_name,
                                            election_date, election_type, 
                                            year, month, year_month, day) %>% 
                filter(year >= 2015 & election_type == "parliament") %>% 
  filter(country_name == "Austria" | country_name == "Belgium" | 
           country_name == "Bulgaria" |country_name == "Croatia" | 
           country_name == "Cyprus" | country_name == "Czech Republic" | 
           country_name == "Estonia" | country_name == "Finland" | 
           country_name == "France" | country_name == "Germany" | 
           country_name == "Greece" | country_name == "Hungary" |
           country_name == "Iceland" | country_name == "Ireland" | 
           country_name == "Israel" | country_name == "Italy" | 
           country_name == "Latvia" | country_name == "Lithuania" | 
           country_name == "Montenegro" | country_name == "Netherlands" | 
           country_name == "North Macedonia" | country_name == "Norway" | 
           country_name == "Poland" | country_name == "Portugal" |
           country_name == "Serbia" | country_name == "Slovakia" | 
           country_name == "Slovenia" | country_name == "Spain" | 
           country_name == "Sweden" | country_name == "Switzerland" |
           country_name == "United Kingdom") %>% distinct()

# Create data frame to match to Cohens' work  -----------------------------

# Manually imputing the ESS10 field periods per country from ESS website
# https://ess.sikt.no/en/study/172ac431-2a06-41df-9dab-c1fd8f3877e7/433
# (accessed: 23.04.2024)

nd_add <- data.frame(
  cntry = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "EE", "ES", "FI", "FR",
            "GB", "GR", "HR", "HU", "IE", "IL", "IS", "IT", "LT", "LV", "ME",
            "MK", "NL", "NO", "PL", "PT", "RS", "SE", "SI", "SK"),
  essround = rep(10, 31),
  # manual imputation
  field_start = as.Date(c("2021-08-30", "2021-10-27", "2021-06-28", "2021-05-04",
                          "2022-03-09", "2021-07-07", "2021-10-05", "2021-06-07",
                          "2022-01-21", "2021-08-31", "2021-08-23", "2021-08-15",
                          "2021-11-09", "2021-05-05", "2021-06-10", "2021-11-23",
                          "2022-02-01", "2021-07-28", "2021-10-25", "2021-07-01", 
                          "2021-11-01", "2021-11-03", "2021-10-23", "2021-10-01", 
                          "2021-06-10", "2022-01-25", "2021-08-16", "2022-01-11", 
                          "2021-12-10", "2020-09-18", "2021-05-25")),
  field_end = as.Date(c("2021-12-06","2022-09-03","2021-09-30", "2022-05-02", 
                        "2022-08-19", "2021-09-21", "2022-01-04", "2021-12-31",
                        "2022-05-31", "2022-01-31", "2021-12-31", "2022-09-02",
                        "2022-05-23", "2021-11-26", "2021-10-16", "2022-12-16",
                        "2022-07-17", "2022-02-11", "2022-04-26", "2021-12-15", 
                        "2022-01-31", "2022-03-30", "2022-03-07", "2022-04-03", 
                        "2022-05-04", "2022-05-25", "2022-03-06", "2022-05-25", 
                        "2022-01-17", "2021-08-26", "2021-10-21")),
  recent_election = as.Date(c("2019-09-29", "2019-05-26", "2021-07-11", 
                              "2019-10-20", "2021-05-30", "2017-10-21", 
                              "2021-09-26", "2019-03-03", "2019-11-10", 
                              "2019-04-14", "2017-06-18", "2019-12-12", 
                              "2019-07-07", "2020-07-05", "2018-04-08", 
                              "2020-02-08", "2021-03-23", "2021-09-25", 
                              "2018-03-04", "2020-10-11", "2018-10-06", 
                              "2020-08-30", "2020-07-15", "2021-03-17", 
                              "2021-09-13", "2019-10-13", "2022-01-30", 
                              "2022-04-03", "2018-09-09", "2018-06-03", 
                              "2020-02-29")),
  max_days_since_election = rep(NA, 31),
  split_wave = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
                 FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, 
                 TRUE, FALSE, FALSE, FALSE),
  recent_election_split1 = as.Date(rep(NA, 31)),
  max_days_since_election_split1 = rep(NA, 31))

# Add missing data from Split Elections

nd_add$recent_election_split1[nd_add$cntry == "BG"] <- as.Date("2021-04-04")
nd_add$recent_election_split1[nd_add$cntry == "IS"] <- as.Date("2021-04-04")
nd_add$recent_election_split1[nd_add$cntry == "NO"] <- as.Date("2017-09-11")
nd_add$recent_election_split1[nd_add$cntry == "PT"] <- as.Date("2019-10-06")
nd_add$recent_election_split1[nd_add$cntry == "RS"] <- as.Date("2017-04-02")

# Merge with Cohens' work -------------------------------------------------

load("ess_election_dates_added.RData")
