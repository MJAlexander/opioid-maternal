## This code manually generates a dataframe of when states implemented a 
## pregnancy checkbox in the death certificate data. 
## 
## Most of this data come from parts of Table 1 in:
##  MacDorman MF, Declercq E, Cabral H, Morton C. Is the United States 
##  Maternal Mortality Rate Increasing? Disentangling trends from measurement 
##  issues Short title: U.S. Maternal Mortality Trends. Obstetrics and 
##  gynecology. 2016;128(3):447-455. doi:10.1097/AOG.0000000000001556.
##  
##  See: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5001799/
##  See: https://www.ncbi.nlm.nih.gov/pubmed/28483570
library(tidyverse)

## Note that the order of this dataframe is not the same as the table: I moved
## Washington DC to the last row to make it easier for naming.
cb_df <- data_frame(state   = c(state.name, "Washington DC"), 
                    abbrev  = c(state.abb, "DC"), 
                    st_fip  = narcan::state_abbrev_to_fips(abbrev), 
                    cb_year = as.integer(c(
                      # Alabama
                      NA, 2014, 2010, 2008, 2003, NA, 2005, 2007, 
                      # Florida
                      2005, 2008, 2014, 2003, 2008, 2008, 2011, 2005, 
                      # Kentucky
                      2011, 2013, 2011, NA, 2015, 2004, 2012, 2012, 
                      # Missouri
                      2010, 2003, 2005, 2008, 2005, 2004, 2006, 2003, 
                      # North Carolina
                      2014, 2008, 2007, 2004, 2006, 2012, 2006, 2005, 
                      # South Dakota
                      2004, 2012, 2006, 2005, 2009, 2015, 2004, NA, 
                      # Wisconsin
                      2014, 2004, 
                      # Washington DC
                      2006)))

## Manually update more recent states (i.e. the NAs up above).
cb_df <- cb_df %>% 
    mutate(cb_year = 
               case_when(
                   abbrev == "AL" ~ 2016L, 
                   abbrev == "CO" ~ 2015L, 
                   abbrev == "MD" ~ 2001L, 
                   abbrev == "WV" ~ 2018L, 
                   TRUE ~ cb_year
               )
    )

write.csv(cb_df, "./data/checkbox_years.csv", row.names = FALSE)
