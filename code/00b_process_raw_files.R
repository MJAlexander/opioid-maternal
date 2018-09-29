## 00b_process_raw_files.R
## 
## Code to process restricted access into the values of interest. This code
## is for our research letter:
##  [CITATION]
##  
## NOTE: This is for documentation but requires restricted-access data to 
##       run without error. See README for more details.

## Libraries ----
library(tidyverse)
library(narcan)
library(foreach)

## Load checkbox data ----
## See 99_generate_checkbox_years_table.R for how this was made
cb_df <- read_csv("./data/checkbox_years.csv")

## Alias the hidden function ----
import_rmcod <- narcan:::.import_restricted_data

## Make a dictionary of year:filename ----
file_dict <- list(
    y2007 = "./restricted_data/MULT2007.USPART2.dat", 
    y2008 = "./restricted_data/MULT2008.USPART2.dat", 
    y2009 = "./restricted_data/MULT2009.USPART2.dat", 
    y2010 = "./restricted_data/MULT2010.USPART2.dat", 
    y2011 = "./restricted_data/MULT2011.USPART2.dat", 
    y2012 = "./restricted_data/MULT2012.USPART2.dat", 
    y2013 = "./restricted_data/MULT2013.USPART2.dat", 
    y2014 = "./restricted_data/MULT2014.USPART2.dat", 
    y2015 = "./restricted_data/MULT2015.USPART2.dat", 
    y2016 = "./restricted_data/MULT2016.USAllCnty.txt"
    )

## Process all the raw files one by one ----
## Subset and munge as we go, resulting in a single raw file with 
## just the deaths that we will use for analysis.
## 
## WARNING! MEMORY INTENSIVE. 
## 3 cores needs ~16 GB of RAM -- set to cores = 1 if memory-constrained
doParallel::registerDoParallel(cores = 3)

restricted_working <- 
    foreach(y = 2007:2016, .combine = bind_rows, 
        .packages = c('tidyverse', 'narcan')) %dopar% {
            
            ## Load the file
            temp_df <- import_rmcod(file_dict[[paste0("y", y)]], y)
            
            ## Make a vector of states that had the checkbox this year 
            states_with_checkbox <- cb_df %>% 
                dplyr::filter(cb_year <= y)
            
            ## Subset to columns we want
            temp_df <- temp_df %>% 
                dplyr::select(dplyr::one_of(c("year", "race", "hspanicr", 
                                              "sex", "ager27", "restatus", 
                                              "ucod", "countyoc")),
                              dplyr::starts_with("record_"), 
                              dplyr::starts_with("rnifla"))
            
            ## Drop nonresidents
            temp_df <- narcan::subset_residents(temp_df)
            
            ## Unite all 20 contributory cause columns
            temp_df <- narcan::unite_records(temp_df)
            
            ## Convert age, add hspanicr, remap race, and add categories
            temp_df <- temp_df %>% 
                narcan::convert_ager27(.) %>% 
                narcan::remap_race(.) %>% 
                dplyr::mutate(
                    race_cat = narcan::categorize_race(race), 
                    hsp_cat  = narcan::categorize_hspanicr(hspanicr), 
                    age_cat  = narcan::categorize_age_5(age), 
                    female = ifelse(sex == "F", 1, 0)
                )
            
            ## Subset to females of reproductive age
            temp_df <- temp_df %>% 
                dplyr::filter(female == 1, 
                              age >= 15, 
                              age < 50)
            
            ## Flag opioid, heroin, drugs, nonopioid drugs, maternal deaths, 
            ## maternal late deaths, etc.
            temp_df <- temp_df %>% 
                narcan::flag_maternal_deaths(.) %>% 
                narcan::flag_maternal_deaths_late(.) %>% 
                narcan::flag_drug_deaths(.) %>% 
                narcan::flag_opioid_deaths(.) %>% 
                narcan::flag_opioid_contributed(.) %>% 
                narcan::flag_opioid_types(.) %>% 
                narcan::flag_nonopioid_drug_deaths(.)
            
            ## Create a checkbox flag
            temp_df <- temp_df %>%
                dplyr::mutate(
                    st_fip = substr(countyoc, 1, 2),
                    checkbox =
                        dplyr::case_when(
                            st_fip %in% states_with_checkbox$st_fip ~ 1,
                            TRUE ~ 0
                        )
                )
            
            ## Reorder columns
            temp_df <- temp_df %>% 
                dplyr::select(-sex, -female, -countyoc) %>% 
                dplyr::select(checkbox, st_fip, year, race, race_cat, 
                              hspanicr, hsp_cat, age, age_cat, ucod,
                              f_records_all, dplyr::everything())
            
            ## Return
            temp_df
        }

saveRDS(restricted_working, "./data/restricted_data.RDS")
