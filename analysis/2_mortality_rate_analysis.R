# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Analysis of mortality rates between weeks                                #
# Nick Plummer (nickplummer@cantab.net)                                       #
# Revision 2 (18/3/22)                                                        #
# Released under the Open Government License v3.0                             #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

source(here::here("R/set_up.R"))

# Granular mortality data
mortality_wide <- get_mortality_data()
# Grouped mortality for plotting
mortality_long <- plottable_mortality(mortality_wide)

# Table 4: Mortality by year and wave ----

## All-cause mortality ----
mortality_long %>% 
  group_by(period) %>% 
  # Combine ALL deaths by period
  summarise_if(is.numeric, sum) %>% 
  # Compare by waves
  do_wave_comparison(values_from = "total")

## Non-COVID mortality ----
non_cov <- mortality_long %>% 
  filter(covid_status == "noncovid") %>% 
  group_by(period) %>% 
  # Combine ALL deaths by period
  summarise_if(is.numeric, sum)

# Compare by waves
non_cov %>% do_wave_comparison(values_from = "total")

# Compare overall 12months
non_cov %>% do_year_comparison(values_from = "total")


# Table 5: Specific non-COVID mortality ----

## Cardiac arrests ----
# Select cardiac arrests from granular dataset (combined into cardiac causes in long)
cardiac_arrests <- mortality_wide %>% 
  filter(cause_of_death == "Cardiac arrest") %>% 
  pivot_longer(cols = -cause_of_death,
               names_to = c("period", "covid_status"),
               names_pattern = "(.*)\\.(.*)",
               values_to = "total") %>% 
  filter(covid_status == "noncovid") %>% 
  select(-c(cause_of_death, covid_status)) 

cardiac_arrests %>% do_year_comparison(values_from = "total")
cardiac_arrests %>% do_wave_comparison(values_from = "total")

## Neuro catastrophies ----
neuro <- mortality_wide %>% 
  filter(cause_of_death %in% c(
    "Intracranial haemorrhage",
    "Intracranial thrombosis",
    "Hypoxic brain damage - all causes",
    "Intracranial - type unclassified (CVA)"
  )) %>% 
  pivot_longer(cols = -cause_of_death,
               names_to = c("period", "covid_status"),
               names_pattern = "(.*)\\.(.*)",
               values_to = "total") %>% 
  filter(covid_status == "noncovid") %>% 
  group_by(period) %>% 
  summarise_if(is.numeric, sum) 

neuro %>% do_year_comparison(values_from = "total")
neuro %>% do_wave_comparison(values_from = "total")

## Suicide/DSH  ----
suidice_dsh <- mortality_long %>% 
  filter(covid_status == "noncovid",
         cause_of_death == "DSH/suicide") %>% 
  group_by(period) %>% 
  summarise_if(is.numeric, sum) 

suidice_dsh %>% do_year_comparison(values_from = "total")
suidice_dsh %>% do_wave_comparison(values_from = "total")


## RTCs ----
rtcs <- mortality_long %>% 
  filter(covid_status == "noncovid",
         cause_of_death == "RTC") %>% 
  group_by(period) %>% 
  summarise_if(is.numeric, sum)

rtcs %>% do_year_comparison(values_from = "total")
rtcs %>% do_wave_comparison(values_from = "total")

## Respiratory ----
respiratory <- mortality_long %>% 
  filter(covid_status == "noncovid",
         cause_of_death == "Respiratory") %>% 
  group_by(period) %>% 
  summarise_if(is.numeric, sum) 

respiratory %>% do_year_comparison(values_from = "total")
respiratory %>% do_wave_comparison(values_from = "total")

## MOF ----
mof <- mortality_long %>% 
  filter(covid_status == "noncovid",
         cause_of_death == "MOF") %>% 
  group_by(period) %>% 
  summarise_if(is.numeric, sum)

mof %>% do_year_comparison(values_from = "total")
mof %>% do_wave_comparison(values_from = "total")

# Figure 4: Mortality plots ----

period_labs_rows = c("wave1" = "1st wave (March - September)",
                     "wave2" = "2nd wave (September - March)")
period_labs_cols = c("19" = "Pre-pandemic (2019-2020)",
                     "20" = "Pandemic (2020-2021)")

mortality_long %>% 
  separate(period, into = c("wave", "yy")) %>% 
  ggplot() +
  geom_bar(aes(y = total,
               x = cause_of_death,
               fill = cause_of_death,
               alpha = covid_status),
           stat = "identity", position = "stack", colour = "black", size = 0.5) +
  # ggpattern::geom_bar_pattern(data = mortality_long,
  #                             aes(y = total, 
  #                                 x = cause_of_death, 
  #                                 fill = cause_of_death,
  #                                 pattern = covid_status),
  #                             stat = "identity",
  #                             position = "stack") +
  facet_grid(cols = vars(yy), rows = vars(wave),
             labeller = labeller(yy = period_labs_cols, wave = period_labs_rows)) +
  scale_alpha_manual(name = "COVID-19 status",
                     labels = c("Positive", "Negative"),
                     values = c(1.0, 0.6)) +
  scale_fill_brewer(name = "Cause of death",
                    palette = "Paired") +
  #ggpattern::scale_pattern_manual(values = c(covid = "stripe", noncovid = "none")) +
  #ggthemes::theme_few() +
  cowplot::theme_cowplot() +
  xlab("") +
  ylab("Total audited deaths") +
  scale_y_continuous(expand = c(0,0), # Force x-axis to start at zero
                     limits = c(0,7200)) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        strip.background = element_blank())
