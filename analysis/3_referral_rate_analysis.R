# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Analysis of referral rates and approaches                                #
# Nick Plummer (nickplummer@cantab.net)                                       #
# Revision 3 (17/6/22)                                                        #
# Released under the Open Government License v3.0                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

source(here::here("R/set_up.R"))

referrals_long <- get_referrals() %>% mutate(total_pc = 100*total_refd/total_meeting)
pda_data_all <- get_pda_data()

# Table 6: Referrals by period ----

### Totals and percentages ----
referrals_long %>% filter(ref_type == "all")
referrals_long %>% filter(ref_type == "dbd")  
referrals_long %>% filter(ref_type == "dcd")

### All ----
referrals_long %>% do_ref_rate_year_comparison(rtype = "all")
referrals_long %>% do_ref_rate_comparison(rtype = "all", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "all", period_string = "^wave2")

### DBD ----
referrals_long %>% do_ref_rate_year_comparison(rtype = "dbd")
referrals_long %>% do_ref_rate_comparison(rtype = "dbd", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "dbd", period_string = "^wave2")

### DCD ----
referrals_long %>% do_ref_rate_year_comparison(rtype = "dcd")
referrals_long %>% do_ref_rate_comparison(rtype = "dcd", period_string = "^wave1")
referrals_long %>% do_ref_rate_comparison(rtype = "dcd", period_string = "^wave2")


# Table 7: Eligible donors etc by period ----

## Total eligible donors ----
pda_data_all %>% 
  select(period, eligible_donor) %>% 
  do_wave_comparison(values_from = "eligible_donor")

pda_data_all %>% 
  select(period, eligible_donor) %>%
  rename(total = eligible_donor) %>% 
  do_year_comparison(values_from = "total")
  
## Approaches ----

# Numbers for table
(approaches <- pda_data_all %>% 
  select(period, eligible_donor, eligible_donor_family_approached, ) %>% 
  mutate(eligible_donor_family_not_approached = eligible_donor - eligible_donor_family_approached,
         eligible_donor_approached_pc = eligible_donor_family_approached/eligible_donor))

# Comparison to pre-pandemic
approaches %>% do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached", negatives = "eligible_donor_family_not_approached")
approaches %>% do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached", negatives = "eligible_donor_family_not_approached")

# Comparison between waves
approaches %>% do_year_rate_comparison(positives = "eligible_donor_family_approached", negatives = "eligible_donor_family_not_approached")

## Family consent rate  ----

# Numbers for table
(consents <- pda_data_all %>% 
  select(period, eligible_donor_family_approached, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_donor_family_approached_not_consented = eligible_donor_family_approached - eligible_donor_family_approached_consented,
         eligible_donor_family_consented_pc = eligible_donor_family_approached_consented/eligible_donor_family_approached))

# Comparison to pre-pandemic
consents %>% do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")
consents %>% do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")

# Comparison between waves
consents %>% do_year_rate_comparison(positives = "eligible_donor_family_approached_consented", negatives = "eligible_donor_family_approached_not_consented")

## SNOD rates ----

# Numbers for table
(snods <- pda_data_all %>% 
  select(period, eligible_donor_family_approached_with_snod, eligible_donor_family_approached_without_snod) %>% 
  mutate(eligible_donor_family_approached_with_snod_pc = eligible_donor_family_approached_with_snod / (eligible_donor_family_approached_with_snod + eligible_donor_family_approached_without_snod)) )

# Approaches with SNODS verses pre-pandemic
snods %>% do_rate_comparison(period_string = "^wave1", positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")
snods %>% do_rate_comparison(period_string = "^wave2", positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")

# SNOD presence between waves
snods %>% do_year_rate_comparison(positives = "eligible_donor_family_approached_with_snod", negatives = "eligible_donor_family_approached_without_snod")

# Consent rates vs SNOD present
(snod_consent <- pda_data_all %>% 
  select(period, 
         eligible_donor_family_approached_with_snod, eligible_donor_family_approached_with_snod_consented,
         eligible_donor_family_approached_without_snod, eligible_donor_family_approached_without_snod_consented) %>% 
  mutate(snod_no_consent = eligible_donor_family_approached_with_snod - eligible_donor_family_approached_with_snod_consented,
         no_snod_no_consent = eligible_donor_family_approached_without_snod - eligible_donor_family_approached_without_snod_consented,
         snod_consent_pc = 100*eligible_donor_family_approached_with_snod_consented/eligible_donor_family_approached_with_snod,
         no_snod_consent_pc = 100*eligible_donor_family_approached_without_snod_consented/eligible_donor_family_approached_without_snod))

# Consent rate with SNOD
snod_consent %>% 
  do_year_rate_comparison(positives = "eligible_donor_family_approached_with_snod_consented", negatives = "snod_no_consent")
# Consent rate without SNOD present
snod_consent %>% 
  do_year_rate_comparison(positives = "eligible_donor_family_approached_without_snod_consented", negatives = "no_snod_no_consent")

## Donations ----

pda_data_all %>% 
  select(period, donated) %>%
  rename(total = donated) %>% 
  do_year_comparison(values_from = "total")

# Numbers for table
(donated <- pda_data_all %>% 
  select(period, donated, eligible_donor_family_approached_consented) %>% 
  mutate(eligible_but_no_donation = eligible_donor_family_approached_consented - donated,
         donated_pc = 100*donated/eligible_donor_family_approached_consented))

# Of those approached, vs pre-pandemic
donated %>% do_rate_comparison(period_string = "^wave1", positives = "donated", negatives = "eligible_but_no_donation")
donated %>% do_rate_comparison(period_string = "^wave2", positives = "donated", negatives = "eligible_but_no_donation")

# As a proportion of all comers, by wave
(donated_all <- pda_data_all %>% 
  select(period, donated, eligible_donor) %>% 
  mutate(eligible_no_donation = eligible_donor - donated,
         donated_pc = 100*donated/eligible_donor))

# Comparision between waves
donated_all %>% 
  do_year_rate_comparison(positives = "donated", negatives = "eligible_no_donation")

# Whole year comparison
(whole_year_pda <- pda_data_all %>% separate(period, sep = "_", into = c("wave", "yr")) %>% # Split into wave/yr
  group_by(yr) %>%
  mutate(eligible_no_donation = eligible_donor - donated) %>% 
  summarise(across(is.numeric, sum)) )
  
whole_year_pda %>% 
  arrange(desc(yr)) %>% 
  select(donated, eligible_no_donation) %>% 
  rstatix::fisher_test(detailed = TRUE)

whole_year_pda %>% 
  arrange(desc(yr)) %>% 
  select(donated, eligible_no_donation) %>% 
  mutate

# Figure 5: Alluvial charts ----

# Get pathway data
alluvial_data <- get_alluvial_data()

# Relabel periods for facet_wrap
period_labels <- c("1st wave (March 2020 - September 2020)",
                   "2nd wave (September 2020 - March 2021)")
names(period_labels) <- c(3,4)

# Plot...
fig5 <- alluvial_data %>% 
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
             size = 3.5) +
  # Facet by wave and relabel
  facet_wrap(~period,
             nrow = 2,
             scales = "fixed",
             labeller = labeller(period = period_labels)) +
  scale_x_discrete() + # Get rid of x-labels and fit onto image
  labs(y = "Number of eligible donors",
       x = "Organ donation pathway") +
  cowplot::theme_cowplot() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        plot.background = element_rect(fill = "white"))

ggsave(plot = fig5, filename = "output/figure5.png", width = 297, height = 210, units = "mm")



