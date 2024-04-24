
library("tidyverse")
library("readr")

# ParlGov Development Version (4. Nov. 2023) ------------------------------
# if countries are not included (e.g. North Macedonia, Montenegro) --> Wikipedia
# https://www.parlgov.org/data-info/ 
# (accessed: 23.04.2024)

election_dates <- read_csv("view_election.csv")

# Split date of the Elections into components

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

# Add Number of days between most recent election and last interview 

nd_add <- nd_add %>% mutate(max_days_since_election = round(difftime(field_end, 
                                          recent_election, units = "days")))

# Augmentation of Cohens' original work -----------------------------------

load("ess_election_dates_added.RData")

# Add missing information & correct errors from Cohens' work (same sources as above)
# I chose not to add Information on Russia, because those elections are not comparable
# Albania

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "AL" & 
                         ess_election_dates_added$essround == 6] <- "2009-06-28"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "AL" & 
                        ess_election_dates_added$essround == 6] <- FALSE
# Austria

ess_election_dates_added$field_start[ess_election_dates_added$cntry == "AT" & 
                         ess_election_dates_added$essround == 4] <- "2010-11-01"

ess_election_dates_added$field_end[ess_election_dates_added$cntry == "AT" & 
                         ess_election_dates_added$essround == 4] <- "2011-02-28"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "AT" & 
                          ess_election_dates_added$essround == 4] <- FALSE

ess_election_dates_added$field_start[ess_election_dates_added$cntry == "AT" & 
                         ess_election_dates_added$essround == 5] <- "2013-05-24"

ess_election_dates_added$field_end[ess_election_dates_added$cntry == "AT" & 
                         ess_election_dates_added$essround == 5] <- "2013-10-10"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "AT" & 
                          ess_election_dates_added$essround == 5] <- TRUE

ess_election_dates_added$recent_election_split1[ess_election_dates_added$cntry == "AT" & 
                         ess_election_dates_added$essround == 5] <- "2008-09-28"

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "AT" & 
                       ess_election_dates_added$essround == 5] <- "2013-09-29"

# Bulgaria 

ess_election_dates_added$field_start[ess_election_dates_added$cntry == "BG" & 
                       ess_election_dates_added$essround == 9] <- "2018-11-16"

ess_election_dates_added$field_end[ess_election_dates_added$cntry == "BG" & 
                       ess_election_dates_added$essround == 9] <- "2018-12-15"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "BG" & 
                        ess_election_dates_added$essround == 9] <- FALSE

# Denmark 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "DK" & 
                              ess_election_dates_added$essround == 9] <- FALSE

# Estonia 

ess_election_dates_added$field_start[ess_election_dates_added$cntry == "EE" & 
                         ess_election_dates_added$essround == 5] <- "2010-10-10"

ess_election_dates_added$field_end[ess_election_dates_added$cntry == "EE" & 
                         ess_election_dates_added$essround == 5] <- "2011-05-28"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "EE" & 
                          ess_election_dates_added$essround == 5] <- TRUE

ess_election_dates_added$recent_election_split1[ess_election_dates_added$cntry == "EE" & 
                            ess_election_dates_added$essround == 5] <- "2007-03-04"

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "EE" & 
                           ess_election_dates_added$essround == 5] <- "2011-03-06"

# Spain 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "ES" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Croatia

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "HR" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Iceland 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "IS" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Lithuania 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "LT" & 
                              ess_election_dates_added$essround == 9] <- FALSE

# Latvia 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "LV" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Portugal 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "PT" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Serbia 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "RS" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Sweden 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "SE" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Slovakia 

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "SK" & 
                            ess_election_dates_added$essround == 9] <- FALSE

# Ukraine 

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "UA" & 
                          ess_election_dates_added$essround == 2] <- "2002-03-31"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 2] <- FALSE

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 3] <- "2006-03-26"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 3] <- FALSE

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 4] <- "2007-09-30"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 4] <- FALSE

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "UA" & 
                        ess_election_dates_added$essround == 5] <- "2007-09-30"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "UA" & 
                              ess_election_dates_added$essround == 5] <- FALSE

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "UA" & 
                          ess_election_dates_added$essround == 6] <- "2012-10-28"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "UA" & 
                            ess_election_dates_added$essround == 6] <- FALSE

# Kosovo 

ess_election_dates_added$recent_election[ess_election_dates_added$cntry == "XK" & 
                         ess_election_dates_added$essround == 6] <- "2010-12-12"

ess_election_dates_added$split_wave[ess_election_dates_added$cntry == "XK" & 
                        ess_election_dates_added$essround == 6] <- FALSE

# Add missing Number of days between most recent election and last interview 

ess_election_dates_added <- ess_election_dates_added %>%  
  mutate(max_days_since_election = round(difftime(field_end, 
                                        recent_election, units = "days")))

# Merge with Cohens' work -------------------------------------------------

ess_election_dates_R1_10 <- ess_election_dates_added %>% 
  bind_rows(nd_add) %>% 
  arrange(cntry, essround)

save(ess_election_dates_R1_10, file = "ess_election_dates_R1_10.RData")
write.csv(ess_election_dates_R1_10, "ess_election_dates_R1_10.csv")

