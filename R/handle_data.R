# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Functions for importing and munging data                                 #
# Nick Plummer (nickplummer@cantab.net)                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Weekly data set for plotting ----

get_weekly_data <- function(w1_start, w2_start) {
  
  # Start and end dates
  w1_end   <- as.Date(w2_start - days(1)) 
  w2_end   <- as.Date(w1_start + days(363))
  
  year_start <- as.Date(w1_start - days(14))
  year_end   <- as.Date(w2_end + days(14)) 
  
  # Weekly donation data ----
  
  # Get daily data from Dale's dataset
  donation_weekly <- readxl::read_excel("data/COVID Donation and Tx Data - all data from daily.xlsx", 
                                       sheet="Weekly",range="A2:M418",
                                       col_names = c('date',
                                                     'referrals',
                                                     'registrations',
                                                     'donors',
                                                     'organs_retrieved',
                                                     'organs_transplanted',
                                                     'dbd_donors',
                                                     'dcd_donors',
                                                     'kidneys',
                                                     'heart',
                                                     'lungs',
                                                     'liver',
                                                     'pancreas')) %>% 
    # Select target year
    filter(date %within% interval(year_start, year_end)) %>% 
    ## Convert to ISO weeks
    # mutate(week = stringr::str_pad(isoweek(ymd(date)), width = 2, pad = "0"),
    #        year = isoyear(ymd(date))) %>% 
    # unite(ym, c(year, week), sep = "-") %>% 
    # group_by(ym) %>% 
    # mutate(wc = first(date)) %>% 
    mutate(wc = floor_date(date, "weeks", week_start = 3)) %>% 
    # Sum approaches, donations, etc by week
    group_by(wc) %>% 
    summarise_if(is.numeric, sum) %>% 
    # Add other metrics
    mutate(transplant_gap = organs_transplanted/organs_retrieved,
           dbd_dcd_ratio = round(dbd_donors/dcd_donors, 2),
           organs_per_registered = round(organs_transplanted/registrations, 2),
           kidneys_per_registered = round(kidneys/registrations, 2),
           wc = as.Date(wc)) %>% 
    na_if(Inf)
  
  # Weekly COVID data ----
  
  # Get Gov.uk raw data
  temp <- tempfile()
  source <- "https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=covidOccupiedMVBeds&metric=hospitalCases&metric=newCasesBySpecimenDate&metric=newOnsDeathsByRegistrationDate&format=csv"
  temp <- curl::curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
  covid_daily <- read_csv(temp)[4:8] %>% 
    arrange(-desc(date))

  # Convert to same weeks
  covid_weekly <- covid_daily %>% 
    filter(date %within% interval(year_start, year_end)) %>% 
    mutate(wc = floor_date(date, "weeks", week_start = 3)) %>% 
    group_by(wc) %>% 
    summarise(cases      = sum(newCasesBySpecimenDate, na.rm = TRUE),
              hospital   = mean(hospitalCases, na.rm = TRUE),
              ventilated = mean(covidOccupiedMVBeds, na.rm = TRUE),
              deaths     = sum(newOnsDeathsByRegistrationDate, na.rm = TRUE))
    
  # Combine and add wave data ----
  all_data_weekly <- full_join(donation_weekly, covid_weekly) %>% 
    mutate(wave = as_factor(if_else(wc %within% interval(w1_start, w1_end), 1,
                                    if_else(wc %within% interval(w2_start, w2_end), 2,
                                            0))))
  
  return(all_data_weekly)
}

# Wave mortality data ----

get_mortality_data <- function() {

  mortality_long <- readxl::read_excel("data/COVID data_COD and non referrals_v3.xls.xlsx",
                                       sheet="Audited deaths COD",
                                       range="A2:G44",
                                       col_names = c('cause_of_death',
                                                     'wave1_19.noncovid',
                                                     'wave2_19.noncovid',
                                                     'wave1_20',
                                                     'wave1_20.covid',
                                                     'wave2_20',
                                                     'wave2_20.covid')) %>% 
    # Calculate non-COVID deaths by subtracting COVID from total
    mutate(wave1_20.noncovid = (wave1_20 - wave1_20.covid),
           wave2_20.noncovid = (wave2_20 - wave2_20.covid)) %>% 
    # Remove totals
    select(-c(wave1_20, wave2_20))
  
  return(mortality_long)
    
}
  
plottable_mortality <- function(dt) {  
  
  mortality_long <- dt %>%
    # Convert to coarser groups for plotting
    mutate(cause_of_death = fct_collapse(factor(cause_of_death),
                                         "Neurological" = c("Intracranial haemorrhage",
                                                            "Intracranial thrombosis",
                                                            "Hypoxic brain damage - all causes",
                                                            "Intracranial - type unclassified (CVA)"),
                                         "Malignant"    = c("Brain tumour",
                                                            "Cancer, other than brain tumour"),
                                         "RTC"          = c("Trauma - RTA - car",
                                                            "Trauma - RTA - motorbike",
                                                            "Trauma - RTA - pushbike",
                                                            "Trauma - RTA - pedestrian",
                                                            "Trauma - RTA - other",
                                                            "Trauma - RTA - unknown type"),
                                         "DSH/suicide"  = c("Other trauma - suicide",
                                                            "Carbon monoxide poisoning",
                                                            "Alcohol poisoning",
                                                            "Paracetamol overdose",
                                                            "Other drug overdose",
                                                            "Self poisoning (type unclassified)"),
                                         "Other trauma" = c("Other trauma - accident",
                                                            "Other trauma - unknown cause",
                                                            "Burns"),
                                         "Cardiovascular"= c("Cardiac arrest",
                                                             "Myocardial infarction",
                                                             "Ischaemic heart disease",
                                                             "Congestive cardiac failure",
                                                             "Cardiovascular - type unclassified",
                                                             "Aortic Aneurysm"),
                                         "Respiratory"  = c("Pulmonary embolism",
                                                            "Chronic pulmonary disease",
                                                            "Pneumonia",
                                                            "Asthma",
                                                            "Respiratory failure",
                                                            "Respiratory - type unclassified (inc smoke inhalation)"),
                                         "Other infective" = c("Meningitis",
                                                               "Septicaemia",
                                                               "Infections - type unclassified"),
                                         "AKI/ALF"       = c("Liver failure (not self poisoning)",
                                                             "Renal failure"),
                                         "MOF"            = c("Multi-organ failure"),
                                         other_level = "Other")) %>%
    # Collapse and summarize by new groups
    group_by(cause_of_death) %>%
    summarise_if(is.numeric, sum) %>%
    # Pivot longer for plotting
    pivot_longer(cols = -cause_of_death,
                 names_to = c("period", "covid_status"),
                 names_pattern = "(.*)\\.(.*)",
                 values_to = "total") %>%
    # Reorder levels for plotting
    mutate(period = factor(period, levels = c("wave1_19",
                                              "wave1_20",
                                              "wave2_19",
                                              "wave2_20")),
           cause_of_death = fct_reorder(cause_of_death, total, .desc = FALSE) %>% 
             fct_relevel("Other") %>%
             fct_rev())

  return(mortality_long)
}
 
# Wave referral data ----

get_ref_data <- function(ref_type, range) {
  # Helper function for get_referrals()
  
  # Spreadsheet setting
  col_names_all <- c('not_refd', 'wave1_19','wave2_19',
                     'wave1_20.allnonrefs', 'wave1_20.noncovnonrefs', 'wave1_20',
                     'wave2_20.allnonrefs', 'wave2_20.noncovnonrefs', 'wave2_20')
  col_names_to_keep <- c('not_refd', 'wave1_19', 'wave1_20', 'wave2_19', 'wave2_20')
  data_source <- "data/COVID data_COD and non referrals_v3.xls.xlsx"
  sheet = "Non referrals reasons"
  
  # Get the data
  return(
    readxl::read_excel(path = data_source, sheet = sheet, range = range,
                                       col_names = col_names_all) %>%
      select(all_of(col_names_to_keep)) %>%
      mutate(ref_type = ref_type)
  )
}


get_referrals <- function() {
  # Gets all different types of referrals and merges
  referrals_all_wide <- get_ref_data(ref_type = "all", range = "A16:I17")
  referrals_dbd_wide <- get_ref_data(ref_type = "dbd", range = "A28:I29")
  referrals_dcd_wide <- get_ref_data(ref_type = "dcd", range = "A46:I47")

  referrals_long <- 
    # Combine wide tables
    bind_rows(referrals_all_wide, referrals_dbd_wide, referrals_dcd_wide) %>%
    # Rename - total meeting ref criteria, total not ref'd (meeting ref criteroa)
    mutate(not_refd = if_else(str_detect(not_refd, "meeting"), "total_meeting", "total_not_refd")) %>%
    # Pivot
    pivot_longer(cols = -c(not_refd,ref_type),
               names_to = "period",
               values_to = "total") %>%
    # Reorder levels for plotting/calcs
    mutate(period = factor(period, levels = c("wave1_19",
                                              "wave1_20",
                                              "wave2_19",
                                              "wave2_20"))) %>%
    pivot_wider(names_from = not_refd,
                values_from = total) %>%
    # Calculate number who were ref'd
    mutate(total_refd = total_meeting - total_not_refd)
  
  return(referrals_long)
}

# TODO

# Wave PDA data ----

get_pda_data <- function() {
  pda_data_all <- readxl::read_excel("data/COVID PDA data by period periodfmt_v3.xlsx",
                                   sheet = "DBD and DCD combined",
                                   range = "B10:T13",
                                   col_names = c("period",
                                                 "all_audited_deaths",
                                                 "ever_ventilated",
                                                 "all_meeting_criteria",
                                                 "all_referred",
                                                 "potential_donor",
                                                 "eligible_donor",
                                                 "eligible_donor_family_approached",
                                                 "eligible_donor_family_approached_opt_out",
                                                 "eligible_donor_family_approached_consented",
                                                 "donated",
                                                 "eligible_donor_family_approached_ODR",
                                                 "eligible_donor_family_approached_ODR_consented",
                                                 "eligible_donor_family_approached_unknown",
                                                 "eligible_donor_family_approached_unknown_consented",
                                                 "eligible_donor_family_approached_with_snod",
                                                 "eligible_donor_family_approached_with_snod_consented",
                                                 "eligible_donor_family_approached_without_snod",
                                                 "eligible_donor_family_approached_without_snod_consented"
                                   )
) %>%
  mutate(period = fct_recode(factor(period),
                             "wave1_19" = "11Mar19-01Sep19",
                             "wave1_20" = "11Mar20-01Sep20",
                             "wave2_19" = "02Sep19-10Mar20",
                             "wave2_20" = "02Sep20-10Mar21"))
  
  return(pda_data_all)
}

# ## Load Sankey data ----
get_sankey_data <- function() {
  
  yes_no_function <- function(x){
    ifelse(x==1, "Yes", "No")
  }
  
  Sankey_PDA_data <- readxl::read_excel(here("data/Sankey PDA data.xlsx")) %>% 
    mutate(
      # Recode eligibility
      path_eli = if_else(path_eli !=0, 1, 0),
      # Proceeded to donation
      path_proc_2 = if_else(path_dbd_proc ==1, "DBD", 
                            if_else(path_dcd_proc == 1, "DCD", "No donation")),
      # Change BSD confirmed to N/A if BSTs not done
      path_bsd_confirm_2 = if_else(path_tested == 0, "NA",
                                   if_else(path_bsd_confirm == 1, "Yes", "No"))
      ) %>% 
    # Recode to yes/no
    mutate(across(4:15, yes_no_function))
    

  return(Sankey_PDA_data)
}

get_alluvial_data <- function() {
  
  alluvial_data <- readxl::read_excel(here("data/Sankey PDA data.xlsx")) %>% 
    select(-periodfmt) %>% 
    mutate(
      path_eli         = if_else(path_eli != 0,        "Eligible", "Not eligible"),
      path_vent        = if_else(path_vent ==1,        "Ventilated", "Never ventilated"),
      path_bsd_sus     = if_else(path_bsd_sus ==1,     "BSD suspected", "BSD not suspected"),
      # Change BSD confirmed to N/A if BSTs not done
      path_bsd_confirm = if_else(path_tested == 0,     "Not tested",
                                 if_else(path_bsd_confirm ==1, "BSD confirmed", "BSD not confirmed")),
      # Then rename path_tested
      path_tested      = if_else(path_tested ==1,      "BSTs performed", "BSTs not performed"),
      path_imm_death   = if_else(path_imm_death ==1,   "Death imminently expected", "Death not imminently expected"),
      path_trt_wthdrwn = if_else(path_trt_wthdrwn ==1, "Treatment withdrawn", "Treatment not withdrawn"),
      path_appro       = if_else(path_appro ==1,       "Family approached", "Family not approached"),
      path_consent     = if_else(path_appro == "Family not approached", "Not approached",
                                 if_else(path_consent ==1,      "Consent given", "Consent refused")),                           
      # Proceeded to donation
      proceeded = if_else(path_dbd_proc ==1, "DBD", 
                            if_else(path_dcd_proc == 1, "DCD", "No donation"))
    )
  
  return(alluvial_data)
}
