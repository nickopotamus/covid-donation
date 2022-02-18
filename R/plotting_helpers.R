# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exploring the impact of COVID-19 on organ donation and transplant rates     #
#  - Helper functions for generating plots/tables                             #
# Nick Plummer (nickplummer@cantab.net)                                       #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Regression functions ----

regression_table <- function(regression) {
  # Outputs poisson regression table ready for supplement
  tbl_regression(regression, exponentiate = TRUE,
                 pvalue_fun = ~style_sigfig(., digits = 3)) %>%
    add_global_p() %>% 
    gtsummary::as_flex_table()
}

plot_regression <- function(regression, ylab, xlab = "Mean ventilated COVID-19") {
  # Plots regression of $OUTCOME on [ventilated] patients, interaction by wave
  sjPlot::plot_model(regression, type = "int", show.data = TRUE) +
    theme_classic() +
    theme(legend.position = c(.85, .85)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle("")
}

date_scale <- function(start, end, extra_days) {
  # Plots x-axis with overlap
  scale_x_date(date_labels = "%d-%b-%y",
               date_breaks = "4 weeks",
               date_minor_breaks = "1 weeks",
               limits = c(as.Date(start - days(extra_days)),
                          as.Date(end + days(extra_days))))
}

graph_label <- function(label, start, end, y_height, angle = 0) {
  # Adds a wave label between start and end
  annotate("text", x = as.Date((as.numeric(end)-as.numeric(start))/2, origin = start),
           y = y_height, label = label, size = 3, angle = angle)
}

intervention_label <- function(label, date, y_height, angle = 90, delay = 4) {
  # Adds an intervention label next to a vline()
  annotate("text", x = as.Date(date)+days(delay),
           y = y_height, label = label, size = 4, angle = angle)
}

# Between wave comparisons ----

do_wave_comparison <- function(dt, values_from) {
  # Compares a given group by wave
  dt %>% 
    # Split into wave/yr
    separate(period, sep = "_", into = c("wave", "yr")) %>% 
    # Pivot by year
    group_by(wave) %>%
    pivot_wider(names_from = "yr", values_from = values_from) %>% 
    # Do Poisson tests between years
    rowwise %>% 
    mutate(rate_ratio = poisson.test(c(`20`,`19`))$estimate,
           conf_int_l = poisson.test(c(`20`,`19`))$conf.int[1],
           conf_int_h = poisson.test(c(`20`,`19`))$conf.int[2],
           p_value = poisson.test(c(`20`,`19`))$p.value)
}

do_ref_rate_comparison <- function(dt, rtype, period_string) {
  # Compares referral types ("all", "DBD", "DCD") by period ("^wave1" or "^wave2")
  dt %>% filter(ref_type == rtype,
         str_detect(period, period_string)) %>% 
    select(total_refd, total_not_refd) %>% 
    rstatix::prop_test(detailed = TRUE) %>% 
    mutate(delta = estimate1-estimate2)
}

do_rate_comparison <- function(dt, period_string, positives, negatives) {
  # Compares referral types ("all", "DBD", "DCD") by period ("^wave1" or "^wave2")
  dt %>% filter(str_detect(period, period_string)) %>% 
    select(positives, negatives) %>% 
    rstatix::prop_test(detailed = TRUE) %>% 
    mutate(delta = estimate1-estimate2)
}

# Alluvial plot

plot_alluvial_wave_data <- function(period_num, text_size = 5, data, title_text){
  
  wave_plot <- 
    # Get data for specific wave
    data %>%
    filter(path_eli == "Eligible" & period == period_num) %>% 
    group_by(path_bsd_sus, 
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
    aes(axis1 = path_bsd_sus,
        axis2 = path_tested,
        axis3 = path_bsd_confirm,
        axis4 = path_appro,
        axis5 = path_consent,
        axis6 = proceeded,
        y = n) +
    geom_alluvium(aes(fill = proceeded)) +
    geom_stratum(aes(fill = proceeded)) +
    geom_label(stat = "stratum",
               aes(label = after_stat(stratum)),
               size = text_size) +
    scale_x_discrete() +
    scale_fill_manual(values = c("Darkgreen", "Green", "Darkred")) +
    labs(
      title = title_text,
      y = "Number of eligible donors") +
    theme_minimal() +
    theme(legend.position = "none")
  
  return(wave_plot)
}
  
  