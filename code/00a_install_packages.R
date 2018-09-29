## 00a_install_packages.R
##
## This script will check to see if the user has the required packages below
## and if they do not, will install them. It will also install `narcan` from
## our Github account in order to manipulate and download the MCOD data.

req_packages <- c("devtools", "tidyverse", "doParallel", 
                  "yaml",  "foreach", "here")

for (p in req_packages) {
    if (!require(p, character.only = TRUE)) {
        install.packages(p, dependencies = TRUE)
    } 
    library(p, character.only = TRUE)
}

## Install narcan for calculating rates and manipulating MCOD files
devtools::install_github("mkiang/narcan")
