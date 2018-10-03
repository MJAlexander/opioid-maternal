## 00c_create_working_dataset.R
## 
## Code to create a working dataset that will be used to plot and
## share aggregated values within NCHS restrictions (i.e., N >= 10):
##      Gemmill A, Kiang MV, and Alexander MJ. Trends in pregnancy-associated
##      mortality involving opioids in the United States, 2007-2016. American 
##      Journal of Obstetrics and Gynecology. Published ahead-of-print 
##      (September 2018). doi: 10.1016/j.ajog.2018.09.028
##  
## NOTE: This is for documentation but requires restricted-access data to 
##       run without error. See README for more details.

## Libraries ----
library(tidyverse)
library(here)
library(narcan)

## Load restricted access data ----
restrict_df  <- readRDS(here("data", "restricted_data.RDS"))

## Load public data ----
births_total <- read_csv(here("data", "live_births_state.csv"))
births_nonhs <- read_csv(here("data", "live_births_state_race_nh.csv"))

## Constants ----
## Make named vectors to replace state names/fips
abbrev_fips        <- narcan::st_fips_map$abbrev
names(abbrev_fips) <- narcan::st_fips_map$fips

abbrev_name        <- narcan::st_fips_map$abbrev
names(abbrev_name) <- narcan::st_fips_map$name

## Make a vector of states that have introduced the checkbox by 2008
## Source: 
##  Davis NL, Hoyert DL, Goodman DA, et al. Contribution of maternal 
##  age and pregnancy checkbox on maternal mortality ratios in 
##  the United States, 1978e2012. Am J Obstet Gynecol 2017;217:352.e1-7.
sts_cb <- c("CT","DC","DE","FL","ID","KS","MI","MT","NE","NH", "NJ", "NM", 
            "NY", "OH", "OK", "OR", "RI", "SC", "SD", "TX", "UT", "WA", "WY")

## Munging birth data ----
## Total population
births_total$state <- stringr::str_replace_all(births_total$State, 
                                               pattern = abbrev_name)

births_total <- births_total %>% 
    rename(year = Year, births = Births) %>% 
    filter(state %in% sts_cb) %>% 
    group_by(year) %>% 
    summarise(births = sum(births)) %>% 
    ungroup()


# Race-specific non-Hispanic population
births_nonhs$state <- stringr::str_replace_all(births_nonhs$State, 
                                               pattern = abbrev_name)
b_race <- births_nonhs %>% 
    rename(year = Year, 
           births = Births, 
           race_name = `Bridged Race`) %>% 
    filter(state %in% sts_cb) %>% 
    group_by(year, race_name) %>% 
    summarise(births = sum(births)) %>% 
    mutate(
        race = ifelse(race_name == "Black or African American", 2, 1), 
        race3 = ifelse(race_name == "Black or African American", 
                       "nhb", "nhw")
        ) %>% 
    ungroup()

## Munging restricted data ----
## Create a column for state abbreviations
restrict_df$state <- stringr::str_replace_all(restrict_df$st_fip, 
                                              pattern = abbrev_fips)

## Filter to states that introduced the checkbox by 2007
restrict_df <- restrict_df %>% 
    filter(state %in% sts_cb, 
           year > 2006)

## Create a race column that matches other data
restrict_df <- restrict_df %>% 
    mutate(race3 = case_when(
        hspanicr == 6 ~ "nhw", 
        hspanicr == 7 ~ "nhb", 
        TRUE ~ "other")) 

## Calculate MMR ----
race_mmr <- restrict_df %>%
    filter(maternal_death_late == 1) %>%
    group_by(year, race3) %>%
    summarise(deaths = sum(maternal_death_late)) %>%
    left_join(b_race) %>%
    mutate(mmr = deaths / births * 10^5, 
           se_mmr = sqrt(mmr^2 / deaths),
           log_mmr = log(mmr),
           log_se = sqrt(se_mmr^2 / mmr^2),
           type = "all deaths") %>% 
    filter(race3 != "other") %>% 
    rowwise() %>% 
    mutate(lower = qchisq(0.025, 2 * deaths) / 2 / births*10^5,
           upper = qchisq(0.975, 2 * (deaths + 1)) / 2 / births*10^5) %>% 
    ungroup()

total_mmr <- restrict_df %>%
    filter(maternal_death_late == 1) %>%
    group_by(year) %>%
    summarise(deaths = sum(maternal_death_late)) %>%
    left_join(births_total)  %>%
    mutate(mmr = deaths / births * 10^5, 
           se_mmr = sqrt(mmr^2 / deaths), 
           log_mmr = log(mmr),
           log_se = sqrt(se_mmr^2 / mmr^2),
           race3="total", 
           type = "all deaths") %>% 
    rowwise() %>% 
    mutate(lower = qchisq(0.025, 2 * deaths) / 2 / births*10^5,
           upper = qchisq(0.975, 2 * (deaths + 1)) / 2 / births*10^5) %>% 
    ungroup()

## Calculate the opioid-related MMR
race_opioid <- restrict_df %>%
    filter(maternal_death_late == 1, 
           opioid_death == 1) %>%
    group_by(year, race3) %>%
    summarise(deaths = sum(maternal_death_late)) %>%
    left_join(b_race) %>%
    mutate(mmr = deaths / births * 10^5, 
           se_mmr = sqrt(mmr^2 / deaths), 
           log_mmr = log(mmr),
           log_se = sqrt(se_mmr^2 / mmr^2),
           type = "opioid-related deaths") %>% 
    filter(race3!="other") %>% 
    rowwise() %>% 
    mutate(lower = qchisq(0.025, 2 * deaths) / 2 / births*10^5,
           upper = qchisq(0.975, 2 * (deaths + 1)) / 2 / births*10^5) %>% 
    ungroup()

total_opioid <- restrict_df %>%
    filter(maternal_death_late == 1, 
           opioid_death == 1) %>%
    group_by(year) %>%
    summarise(deaths = sum(maternal_death_late)) %>%
    left_join(births_total) %>%
    mutate(mmr = deaths / births * 10^5, 
           se_mmr = sqrt(mmr^2 / deaths), 
           log_mmr = log(mmr),
           log_se = sqrt(se_mmr^2 / mmr^2),
           race3="total", type = "opioid-related deaths") %>% 
    rowwise() %>% 
    mutate(lower = qchisq(0.025, 2 * deaths) / 2 / births*10^5,
           upper = qchisq(0.975, 2 * (deaths+1)) / 2 / births*10^5) %>% 
    ungroup()

## Save working data ----
## Remove all deaths < 10 (NCHS data use agreement
mmrs <- bind_rows(race_mmr, total_mmr, race_opioid, total_opioid)
mmrs <- mmrs %>% filter(deaths >= 10)

mmrs <- mmrs %>% 
    mutate(type_cat = factor(type, 
                             levels = c("all deaths", "opioid-related deaths"), 
                             labels = c("All causes", "Opioid-related"), 
                             ordered = TRUE), 
           race_cat = factor(race3, 
                             levels = c("total", "nhw", "nhb"), 
                             labels = c("Total", "Non-Hispanic White", 
                                        "Non-Hispanic Black"), 
                             ordered = TRUE)
           ) %>% 
    select(-race, -race_name)

write_csv(mmrs, "./data/working_data_2007_2016.csv")
