# Set up ----

library(tidyverse)
library(lubridate)
library(here)
library(gtsummary)
library(ggalluvial)

# Include files ----

source(here("R/handle_data.R"))
source(here("R/plotting_helpers.R"))

# Set dates ----

# Wave one vs wave two
w1_start <- as.Date("2020-03-11") # WHO pandemic definition
w2_start <- as.Date("2020-09-02") # Should probably use 2/9 (Closest Weds to 1/9, ICNARC)
w1_end   <- as.Date(w2_start - days(1)) 
w2_end   <- as.Date(w1_start + days(363)) # 363 days as leap year

# Lock downs
ld1_start <- as.Date("2020-03-23")
ld1_end   <- as.Date("2020-07-04")
ld1_label <- "1st national lockdown"

ld2_start <- as.Date("2020-10-14")
ld2_end   <- as.Date("2020-11-04")
ld2_label <- "3-tier restrictions"

ld3_start <- as.Date("2020-11-05")
ld3_end   <- as.Date("2020-12-01")
ld3_label <- "2nd national lockdown"

ld4_start <- as.Date("2020-12-02")
ld4_end   <- as.Date("2021-01-05")
ld4_label <- "4-tier restrictions"

ld5_start <- as.Date("2021-01-06")
ld5_end   <- as.Date("2021-03-28")
ld5_label <- "3rd national lockdown"

# Policy changes
age_1_date  <- as.Date("2020-03-23")
age_1_label <- "DBD < 60, DCD < 50"

age_2_date  <- as.Date("2020-04-07")
age_2_label <- "DBD < 75, DCD < 50"

age_3_date  <- as.Date("2020-06-01")
age_3_label <- "DBD < 75, DCD < 60"

age_4_date  <- as.Date("2020-07-29")
age_4_label <- "All age restrictions lifted"