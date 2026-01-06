#####################################################
setwd('~/casino_income/')
library(tidysynth); library(ggplot2); library(htmlTable); library(dplyr); library(tidyr); library(patchwork)
### county
load('data_county.RDa'); data = data_county
unique(data$county); summary(data)
(counties_to_remove = data %>% filter(year == 2018, is.na(pop)) %>% pull(county)); length(unique(data$county))
# create job growth
data = data %>% group_by(county) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(emp_lag = lag(emp, n = 1),
         job = (emp - emp_lag) / emp_lag) %>% ungroup() %>% filter(year>2011 & !is.na(job))
data = data %>% select(year, county, n1, job, per_wage, unemployed, 
                       edu, old, young, lfp, black, hispanic, pop, m_share); unique(data$county)
county_sc = function(treated, exclude, time, period) {
  data %>% filter(!(county %in% exclude)) %>% 
    synthetic_control(outcome = job, 
                      unit = county, 
                      time = year, 
                      i_unit = treated, 
                      i_time = time,
                      generate_placebos=F) %>%
    generate_predictor(time_window = 2017:2018,
                       unemployed = mean(unemployed, na.rm = T),
                       edu = mean(edu, na.rm = T),
                       old = mean(old, na.rm = T),
                       young = mean(young, na.rm = T),
                       lfp = mean(lfp, na.rm= T),
                       black = mean(black, na.rm = T),
                       hispanic = mean(hispanic, na.rm = T),
                       m_share = mean(m_share, na.rm = T)) %>%
    generate_predictor(time_window = 2012, value11 = job) %>% 
    generate_predictor(time_window = 2015, value15 = job) %>% 
    generate_predictor(time_window = 2018, value18 = job) %>% 
    generate_weights(optimization_window = period, sigf_ipop = 5) %>% 
    generate_control()
}
gc2 = county_sc('Garland', c('Jefferson', 'Crittenden'), 2019, 2011:2018)
cc2 = county_sc('Crittenden', c('Jefferson', 'Garland'), 2019, 2011:2018)
jc2 = county_sc('Jefferson', c('Garland', 'Crittenden'), 2020, 2011:2019)
(gc_control = control(gc2) %>% mutate(x = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(x))
(cc_control = control(cc2) %>% mutate(y = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(y))
(jc_control = control(jc2) %>% mutate(z = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(z))
(gc_control = paste('Synthetic Garland:', paste(gc_control, collapse = ', ')))
(cc_control = paste('Synthetic Crittenden:', paste(cc_control, collapse = ', ')))
(jc_control = paste('Synthetic Jefferson:', paste(jc_control, collapse = ', ')))
(county_control = t(data.frame(a = gc_control, b =cc_control, c = jc_control)))
writeLines(htmlTable(county_control, rnames = F), "control_county_job.html")

# customzing plot_trends
plot_custom = function (data, time_window = NULL, title) {
  if (!(".meta" %in% colnames(data))) {
    stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")
  }
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  if (is.null(time_window)) {
    time_window <- unique(data$.original_data[[1]][[time_index]])
  }
  data %>% grab_synthetic_control(placebo = FALSE) %>% filter(time_unit %in% time_window) %>%
    rename(Synthetic = synth_y,  treated = real_y, year = time_unit) %>%  
    ggplot() + ggtitle(title) +
    geom_vline(xintercept = trt_time, color = "black") +
    scale_y_continuous(name = "", limits = c(-0.3, 0.3)) + 
    scale_x_continuous(name = '', breaks = seq(2012, 2023, 1)) +
    theme_classic() + 
    theme(legend.position = c(0.05, 0.99), 
          legend.justification = c("left", "top"), legend.text = element_text(size = 16),
          legend.background = element_rect(fill = "white", color = "white"), 
          plot.title = element_text(hjust = 0.5, size = 18)) + 
    guides(color=guide_legend(title=""))
}
load('AR_emp.RDa'); AR_emp
(AR_emp = AR_emp %>% mutate(emp_lag = lag(emp, n = 1),
                            job = (emp - emp_lag) / emp_lag) %>% filter(year>2011))
AR = geom_line(data = AR_emp, aes(x=year, y = job, color = 'Arkansas'), linewidth = 1, linetype='dotted') 
p4 = gc2 %>% plot_custom(title = 'Garland County') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic Garland'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'Garland'), linewidth = 1) +
  scale_color_manual(values = c('Garland' = 'black', 'Synthetic Garland' = 'darkgrey', 'Arkansas' = 'black'),
                     breaks = c('Garland', 'Synthetic Garland', 'Arkansas'));p4
p5 = jc2 %>% plot_custom(title = 'Jefferson County') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic Jefferson'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'Jefferson'), linewidth = 1) +
  scale_color_manual(values = c('Jefferson' = 'black', 'Synthetic Jefferson' = 'darkgrey', 'Arkansas' = 'black'),
                     breaks = c('Jefferson', 'Synthetic Jefferson', 'Arkansas'));p5
p6 = cc2 %>% plot_custom(title = 'Crittenden County') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic Crittenden'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'Crittenden'), linewidth = 1) +
  scale_color_manual(values = c('Crittenden' = 'black', 'Synthetic Crittenden' = 'darkgrey', 'Arkansas' = 'black'),
                     breaks = c('Crittenden', 'Synthetic Crittenden', 'Arkansas' ));p6
p = p4/p5/p6 ; ggsave(plot = p, filename = 'figure4.png', width = 10, height=13)

## predictors summary stats
library(forcats)
gc_sum = gc2 %>% grab_balance_table() %>% select(-donor_sample)
jc_sum = jc2 %>% grab_balance_table() %>% select(-donor_sample)
cc_sum = cc2 %>% grab_balance_table() 
(sum_stats = gc_sum %>% left_join(jc_sum, by = 'variable') %>% left_join(cc_sum, by = 'variable'))
colnames(sum_stats) = c('Predictors', 'Garland', 'Synthetic<br>Garland', 
                        'Jefferson', 'Synthetic<br>Jefferson', 
                        'Crittenden', 'Synthetic<br>Crittenden', 'Sample mean')
sum_stats
desired_order = c('unemployed', 'lfp', 'edu', 'm_share',
                  'young', 'old', 'black', 'hispanic',
                  'value11', 'value15', 'value18')
sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors),
                                 Predictors = fct_relevel(Predictors, desired_order)) %>% 
  arrange(Predictors) %>% select(-Predictors)
var_name = c('Unemployment rate', 'Labor force participation rate', 'Share college graduate',
             'Share manufacturing jobs',
             'Share younger (<15)', 'Share older (>65)',  'Share Black people', 'Share Hispanic people', 
             'Average job growth in 2012', 'Average job growth in 2015',  'Average job growth in 2018')
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
colnames(summary_table)[1] = 'Predictors'
summary_table[1:8,-1] = lapply(summary_table[1:8,-1], function(x) round(x, 1))
summary_table[9:11,-1] = lapply(summary_table[9:11,-1], function(x) round(x, 3))
(county_table = summary_table)
writeLines(htmlTable(county_table, rnames = F), "predictors_county_job.html")

