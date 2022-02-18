# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Analysis of referral rates and approaches                                #
# Nick Plummer (nickplummer@cantab.net)                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

source(here::here("R/set_up.R"))

referrals_long <- get_referrals()
pda_data_all <- get_pda_data()

# Table 6: Referrals by period ----

### All ----
referrals_long %>% do_ref_rate_comparison(rtype = "all", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "all", period_string = "^wave2")

### DBD ----
referrals_long %>% do_ref_rate_comparison(rtype = "dbd", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "dbd", period_string = "^wave2")

### DCD ----
referrals_long %>% do_ref_rate_comparison(rtype = "dcd", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "dcd", period_string = "^wave2")


# Table 7: Eligible donors etc by period ----

## Total eligible donors ----
pda_data_all %>% 
  select(period, eligible_donor) %>% 
  do_wave_comparison(values_from = "eligible_donor")
  
## Approaches ----

# Numbers for table
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor) %>% 
  mutate(eligible_donor_family_not_approached = eligible_donor - eligible_donor_family_approached) %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_not_approached)

# Comparison to pre-pandemic
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor) %>% 
  mutate(eligible_donor_family_not_approached = eligible_donor - eligible_donor_family_approached) %>% 
  do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached", negatives = "eligible_donor_family_not_approached")
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor) %>% 
  mutate(eligible_donor_family_not_approached = eligible_donor - eligible_donor_family_approached) %>% 
  do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached", negatives = "eligible_donor_family_not_approached")

# Comparison between waves
pda_data_all %>% 
  mutate(eligible_not_approached = eligible_donor - eligible_donor_family_approached) %>% 
  do_rate_comparison(period_string = "20$", positives = "donated", negatives = "eligible_not_approached")

## Family consent rate  ----

# Numbers for table
pda_data_all %>% 
  filter(str_detect(period, "^wave2")) %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_donor_family_approached_not_consented = eligible_donor_family_approached - eligible_donor_family_approached_consented) %>% 
  select(eligible_donor_family_approached_consented, eligible_donor_family_approached_not_consented)

# Comparison to pre-pandemic
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_donor_family_approached_not_consented = eligible_donor_family_approached - eligible_donor_family_approached_consented) %>% 
  do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_donor_family_approached_not_consented = eligible_donor_family_approached - eligible_donor_family_approached_consented) %>% 
  do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")

# Comparison between waves
pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_donor_family_approached_not_consented = eligible_donor_family_approached - eligible_donor_family_approached_consented) %>% 
  do_rate_comparison(period_string = "20$", positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")

## SNOD rates ----

# Numbers for table
pda_data_all %>% 
  filter(str_detect(period, "^wave2")) %>% 
  select(eligible_donor_family_approached_with_snod, eligible_donor_family_approached_without_snod)

# Approaches with SNODS verses pre-pandemic
pda_data_all %>% 
  do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")
pda_data_all %>% 
  do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")

# SNOD presence between waves
pda_data_all %>% 
  do_rate_comparison(period_string = "20$", positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")

# Consent rate with SNOD present
pda_data_all %>% 
  select(period, 
         eligible_donor_family_approached_with_snod, eligible_donor_family_approached_with_snod_consented,
         eligible_donor_family_approached_without_snod, eligible_donor_family_approached_without_snod_consented) %>% 
  mutate(snod_no_consent = eligible_donor_family_approached_with_snod - eligible_donor_family_approached_with_snod_consented,
         no_snod_no_consent = eligible_donor_family_approached_without_snod - eligible_donor_family_approached_without_snod_consented) %>% 
  do_rate_comparison(period_string = "20$", positives = "eligible_donor_family_approached_with_snod_consented", negatives = "snod_no_consent")


# Consent rate without SNOD present
pda_data_all %>% 
  select(period, 
         eligible_donor_family_approached_with_snod, eligible_donor_family_approached_with_snod_consented,
         eligible_donor_family_approached_without_snod, eligible_donor_family_approached_without_snod_consented) %>% 
  mutate(snod_no_consent = eligible_donor_family_approached_with_snod - eligible_donor_family_approached_with_snod_consented,
         no_snod_no_consent = eligible_donor_family_approached_without_snod - eligible_donor_family_approached_without_snod_consented) %>% 
  do_rate_comparison(period_string = "20$", positives = "eligible_donor_family_approached_without_snod_consented", negatives = "no_snod_no_consent")

## Donations ----

# Numbers for table
pda_data_all %>% 
  select(period, donated, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_but_no_donation = eligible_donor_family_approached_consented - donated) %>% 
  select(period, donated, eligible_but_no_donation)

# Of those approached, vs pre-pandemic
pda_data_all %>% 
  mutate(eligible_but_no_donation = eligible_donor_family_approached_consented - donated) %>% 
  do_rate_comparison(period_string = "^wave1", positives = "donated", negatives = "eligible_but_no_donation")
pda_data_all %>% 
  mutate(eligible_but_no_donation = eligible_donor_family_approached_consented - donated) %>% 
  do_rate_comparison(period_string = "^wave2", positives = "donated", negatives = "eligible_but_no_donation")

# As a proportion of all comers, by wave
pda_data_all %>% 
  mutate(eligible_no_donation = eligible_donor - donated) %>% 
  do_rate_comparison(period_string = "20$", positives = "donated", negatives = "eligible_no_donation")

# Figure 5: Alluvial charts ----

# Get pathway data
alluvial_data <- get_alluvial_data()

# Relabel periods for facet_wrap
period_labels <- c("1st wave (March 2020 - September 2020)",
                   "2nd wave (September 2020 - March 2021)")
names(period_labels) <- c(3,4)

# Plot...
alluvial_data %>% 
  # Select appropriate data and make alluvial ready
  filter(path_eli == "Eligible" & period %in% c(3,4)) %>% 
  group_by(period, 
           path_bsd_sus, 
           path_tested, 
           path_bsd_confirm, 
           path_appro,
           path_consent,
           proceeded)  %>%
  count() %>% 
  # Refactor for nicer plots
  mutate(path_bsd_sus = factor(path_bsd_sus, levels = c("BSD suspected", "BSD not suspected")),
         path_tested  = factor(path_tested,  levels = c("BSTs performed", "BSTs not performed"))) %>% 
  # Do plot
  ggplot() +
  # Axis for each strata point
  aes(axis1 = path_bsd_sus,
      axis2 = path_tested,
      axis3 = path_bsd_confirm,
      axis4 = path_appro,
      axis5 = path_consent,
      axis6 = proceeded,
      y = n) +
  # Colour by eventual outcome
  geom_alluvium(aes(fill = proceeded)) +
  geom_stratum(aes(fill = proceeded)) +
  scale_fill_manual(values = c("Darkgreen", "Green", "Darkred")) +
  # Label with name and percentage
  geom_label(stat = "stratum",
             aes(label = paste(after_stat(stratum), scales::percent(after_stat(prop), accuracy = .1))),
             size = 2.5) +
  # Facet by wave and relabel
  facet_wrap(~period,
             nrow = 2,
             scales = "fixed",
             labeller = labeller(period = period_labels)) +
  scale_x_discrete() + # Get rid of x-labels and fit onto image
  labs(y = "Number of eligible donors") +
  theme_minimal() +
  theme(legend.position = "none")




