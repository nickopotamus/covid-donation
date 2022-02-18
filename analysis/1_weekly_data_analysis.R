# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Analysis of weekly donation/transplant and COVID data                    #
# Nick Plummer (nickplummer@cantab.net)                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

source(here::here("R/set_up.R"))

all_data_weekly <- get_weekly_data(w1_start, w2_start)

# Models - generates figures and supplement tables ----

## Relationship between transplanted organs and COVID ICU use ----

# Build weekly dataset across waves 1 and 2
wave_data_weekly <- all_data_weekly %>%
  filter(wave != 0) %>% mutate(wave = factor(wave, levels = c(1,2))) %>% 
  # Add age intervention periods
  mutate(int_period = case_when(wc < age_1_date                    ~ "restrict_0",
                                wc >= age_1_date & wc < age_2_date ~ "restrict_1",
                                wc >= age_2_date & wc < age_3_date ~ "restrict_2",
                                wc >= age_3_date & wc < age_4_date ~ "restrict_3",
                                wc >= age_4_date                   ~ "restrict_0"))

### a) Referrals ----
ref <- glm(referrals ~ ventilated * wave + int_period,
           data = wave_data_weekly,
           family = "poisson")

### b) Donors ----
don <- glm(donors ~ ventilated * wave + referrals,
           data = wave_data_weekly,
           family = "poisson")

### c) Organs retrieved ----
ret <- glm(organs_retrieved ~ ventilated * wave + donors,
           data = wave_data_weekly,
           family = "poisson")

### d) Total organs Transplanted ----
trans <- glm(organs_transplanted ~ ventilated * wave + organs_retrieved,
             data = wave_data_weekly,
             family = "poisson")

### e) Livers only ----

livers <- glm(liver ~ ventilated * wave + organs_retrieved,
              data = wave_data_weekly,
              family = "poisson")

### f) Kidneys only ----

# Against ventilated patients...
kidn_v <- glm(kidneys ~ ventilated * wave + organs_retrieved,
            data = wave_data_weekly,
            family = "poisson")


# And against hospitalized patients...
kidn_h <- glm(kidneys ~ hospital * wave + organs_retrieved,
              data = wave_data_weekly,
              family = "poisson")


## DBD/DCD ratio ----

# Deal with the divide by zero issue
ratio_data <- wave_data_weekly %>% 
  # Highest ratio is 7, so use this
  mutate(dbd_dcd_ratio = if_else(is.na(dbd_dcd_ratio), 7, dbd_dcd_ratio))

### Table 3: Summary by intervention period ----
ratio_data %>% 
  select(c(int_period, dbd_donors, dcd_donors)) %>% 
  group_by(int_period) %>% 
  summarise_if(is.numeric, sum) %>% 
  mutate(total_donors = dbd_donors + dcd_donors,
         ratio = dbd_donors/dcd_donors) %>% 
  rowwise() %>% 
  mutate(
         CI_lower = DescTools::BinomRatioCI(x1 = dbd_donors, n1 = total_donors, x2 = dcd_donors, n2 = total_donors)[2],
         CI_higher = DescTools::BinomRatioCI(x1 = dbd_donors, n1 = total_donors, x2 = dcd_donors, n2 = total_donors)[3])

# Compare between waves (see also table 1)
t.test(dbd_dcd_ratio ~ wave, data = ratio_data, var.equal = TRUE)
  
# Adjust for number of ventilated patients (can't adjust for referrals as correlated)
ratio_lm <- lm(dbd_dcd_ratio ~ int_period + ventilated,
               data = ratio_data)


## Retrieval utilization ratio ("Transplant gap") ----
t.test(transplant_gap ~ wave, data = ratio_data, var.equal = TRUE)

tg <- lm(transplant_gap ~ ventilated * wave + organs_retrieved + dbd_dcd_ratio,
         data = ratio_data)

#plot_regression(tg, ylab = "Retrieval utilisation ratio")

# Plots ----

## Fig 1: Weekly COVID resource use vs transplant rates ----

plot_donations_all <- all_data_weekly %>% 
  # Select cols interested in
  select(c(wc, referrals, donors, organs_transplanted, kidneys)) %>% 
  # Pivot longer for plotting
  pivot_longer(!wc, names_to = "series", values_to = "value") %>%
  # Reorder series for plotting
  mutate(series = factor(series, levels = c("referrals", "donors", "organs_transplanted", "kidneys"))) %>% 
  ggplot() +
    geom_line(aes(x = wc, y = value, color = series)) +
    #geom_smooth(method = "loess", span = 0.2) +
    # Color scheme and legend
    scale_color_manual(name = "Total weekly:",
                       labels = c("Referrals", "Donors", "Total transplanted organs", "Transplanted kidneys"),
                       values = c("Darkblue", "Pink", "Darkgreen", "Purple")) +
    # Annotate waves
    geom_vline(xintercept = w1_start, linetype = "dashed") +
    graph_label(label = "1st \"wave\"", start = w1_start, end = w1_end, y_height = 125) +
    geom_vline(xintercept = w2_start, linetype = "dashed") +
    graph_label(label = "2nd \"wave\"", start = w2_start, end = w2_end, y_height = 125) +
    geom_vline(xintercept = w2_end, linetype = "dashed") +
    # Set time-series scale
    date_scale(start = w1_start, end = w2_end, extra_days = 7) +
    # Theme options
    ggpubr::theme_pubr() +
    theme(legend.box = "horizontal", legend.position = "top") +
    xlab("Week commencing") +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1, size = 9)) +
    ylab("") 
  

plot_covid_all <- all_data_weekly %>% 
  # Select cols interested in and pivot longer for plotting
  select(c(wc, hospital, ventilated)) %>% 
  pivot_longer(!wc, names_to = "series", values_to = "value") %>%
  ggplot() + 
    geom_line(aes(x = wc, y = value, color = series)) +
    # Add lockdowns
    geom_vline(xintercept = ld1_start, linetype = "dashed") +
    graph_label(label = ld1_label, start = ld1_start, end = ld1_end, y_height = 20000, angle = 90) +
    geom_vline(xintercept = ld1_end, linetype = "dashed") +
    geom_vline(xintercept = ld2_start, linetype = "dashed") +
    graph_label(label = ld2_label, start = ld2_start, end = ld2_end, y_height = 20000, angle = 90) +
    geom_vline(xintercept = ld3_start, linetype = "dashed") +
    graph_label(label = ld3_label, start = ld3_start, end = ld3_end, y_height = 20000, angle = 90) +  
    geom_vline(xintercept = ld4_start, linetype = "dashed") +
    graph_label(label = ld4_label, start = ld4_start, end = ld4_end, y_height = 20000, angle = 90) +
    geom_vline(xintercept = ld5_start, linetype = "dashed") +
    graph_label(label = ld5_label, start = ld5_start, end = ld5_end, y_height = 20000, angle = 90) +
    # Color scheme and legend
    scale_color_manual(name = "Mean weekly:",
                     labels = c("Hospitalised COVID-19 patients", "COVID-19 patients being ventilated"),
                     values = c("Blue", "Red")) +
    # Set time-series scale
    date_scale(start = w1_start, end = w2_end, extra_days = 7) +
    # Theme options
    ggpubr::theme_pubr() +
    theme(legend.box = "horizontal", legend.position = "top") +
    xlab("Week commencing") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ylab("")
    

# Plot with combined x axis (800w for poster)
egg::ggarrange(plot_covid_all + # Remove x-axis
                 theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank()),
               plot_donations_all,
               ncol = 1)

## Fig 2: Referrals/donation/transplant by waves ----

# Individual plots
fig2a <- plot_regression(ref, ylab = "Weekly referrals")
fig2b <- plot_regression(don, ylab = "Weekly organ donors")
fig2c <- plot_regression(ret, ylab = "Organs retrieved")
fig2d <- plot_regression(trans, ylab = "Organs transplanted")
fig2e <- plot_regression(livers, ylab = "Livers transplanted")
fig2f <- plot_regression(kidn_v, ylab = "Kidneys transplanted")
#fig2x <- plot_regression(kidn_h, ylab = "Total weekly kidneys transplanted", xlab = "Mean weekly hospitalised COVID-19 patients")

# Build uberplot
ggpubr::ggarrange(fig2a, fig2b, fig2c,
                  fig2d, fig2e, fig2f,
                  labels = "auto", common.legend = TRUE)

## Fig 3: DBD/DCD ratio ----

plot_ratio <- 
  ratio_data %>% 
  # Select cols interested in and pivot longer for plotting
  select(c(wc, dbd_donors, dcd_donors, dbd_dcd_ratio)) %>% 
  pivot_longer(!wc, names_to = "series", values_to = "value") %>%
  mutate(series = factor(series, levels = c("dbd_donors", "dcd_donors", "dbd_dcd_ratio"))) %>% 
  ggplot() +
  geom_line(aes(x = wc, y = value, color = series),
              method = "loess", span = 0.15) +
  # Color scheme and legend
  scale_color_manual(name = "",
                     labels = c("Total weekly DBD", "Total weekly DCD", "Weekly DBD/DCD ratio"),
                     values = c("palevioletred2", "violetred4", "royalblue2")) +
  # Annotate waves
  #geom_vline(xintercept = w1_start, linetype = "dashed") +
  #graph_label(label = "1st \"wave\"", start = w1_start, end = w1_end, y_height = 30) +
  geom_vline(xintercept = w2_start, linetype = "dashed") +
  #graph_label(label = "2nd \"wave\"", start = w2_start, end = w2_end, y_height = 30) +
  #geom_vline(xintercept = w2_end, linetype = "dashed") +
  # Annotate policy changes
  geom_vline(xintercept = age_1_date, linetype = "dotted") +
  intervention_label(label = age_1_label, date = age_1_date, y_height = 12) +
  geom_vline(xintercept = age_2_date, linetype = "dotted") +
  intervention_label(label = age_2_label, date = age_2_date, y_height = 12) +
  geom_vline(xintercept = age_3_date, linetype = "dotted") +
  intervention_label(label = age_3_label, date = age_3_date, y_height = 12) +
  geom_vline(xintercept = age_4_date, linetype = "dotted") +
  intervention_label(label = age_4_label, date = age_4_date, y_height = 12) +
  # Set time-series scale
  date_scale(start = w1_start, end = w2_end, extra_days = 7) +
  # Theme options
  ggpubr::theme_pubr() +
  theme(legend.box = "horizontal", legend.position = "top") +
  #theme(legend.box = "horizontal", legend.position = c(0.8, 0.5)) +
  xlab("Week commencing") +
  ylab("") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust=1, size = 9))


plot_vent <- all_data_weekly %>% 
  # Select cols interested in and pivot longer for plotting
  select(c(wc, ventilated)) %>% 
  pivot_longer(!wc, names_to = "series", values_to = "value") %>%
  ggplot() + 
  geom_line(aes(x = wc, y = value, color = series),
              method = "loess", span = 0.15) +
  # Color scheme and legend
  scale_color_manual(name = "",
                     labels = c("Mean weekly COVID-19 patients being ventilated"),
                     values = c("Red")) +
  # Set time-series scale
  date_scale(start = w1_start, end = w2_end, extra_days = 7) +
  # Theme options
  ggpubr::theme_pubr() +
  theme(legend.box = "horizontal", legend.position = c(0.5, 0.9)) +
  xlab("Week commencing") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("")

egg::ggarrange(plot_vent + # Remove x-axis
                 theme(axis.text.x = element_blank(),
                       axis.title.x = element_blank()),
               plot_ratio,
               ncol = 1)

# Tables----

## Table 2: Unadjusted weekly activity by wave ----
ratio_data %>%
  filter(wave != 0) %>% mutate(wave = factor(wave, levels = c(1,2))) %>% 
  select(-wc) %>% 
  tbl_summary(by = "wave",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} / {N} ({p}%)"),
              type = list(lungs ~ "continuous")) %>% 
  add_difference() %>% 
  add_q(pvalue_fun = function(x) style_pvalue(x, digits = 3)) %>% 
  as_flex_table()

## Tables S1-S8 ----

regression_table(ref) # S1
regression_table(don) # S2
regression_table(ret) # S3
regression_table(trans) # S4
regression_table(livers) # S5
regression_table(kidn_v) # S6a
regression_table(kidn_h)  # S6b

sjPlot::tab_model(ratio_lm) # S7 (add p-values to table 3)
sjPlot::tab_model(tg) # S8

