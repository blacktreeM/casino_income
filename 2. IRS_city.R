setwd('~/casino_income/')
library(tidysynth); library(ggplot2); library(htmlTable); library(dplyr); library(tidyr); library(patchwork)
load('data_city.RDa'); data = data_city
data = subset(data, year<2023)
unique(data$city); summary(data); table(data$year)
# must have covariates
(cities_to_remove = data %>% filter(year == 2018, is.na(pop)) %>% pull(city)); length(unique(data$city))
data = data %>% filter(!city %in% cities_to_remove); length(unique(data$city))
# balanced panel
table(data$city)
data = data %>% group_by(city) %>% mutate(n = n()) %>% filter(n==12) %>% select(-n); table(data$city)
(top100 = data %>% filter(year==2018 & !(city %in% c('Hot Springs', 'Pine Bluff', 'West Memphis'))) %>% 
    arrange(desc(wages_n)) %>% select(city) %>% head(100))
data = data %>% filter(city %in% top100$city | city %in% c('Hot Springs', 'Pine Bluff', 'West Memphis')) %>% 
  select(year, city, n1, per_income, pay, per_wage, wages_n, unemployed, edu, old, young, lfp, black, hispanic, pop); unique(data$city)
data %>% filter(year==2018) %>% arrange(wages_n) %>% select(city, wages_n) 
data %>% filter(city=='Hot Springs')
city_sc = function(treated, exclude, time, period) {
  data %>% filter(!(city %in% exclude)) %>% 
    synthetic_control(outcome = per_wage, 
                      unit = city, 
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
                       hispanic = mean(hispanic, na.rm = T)) %>%
    generate_predictor(time_window = 2012, value11 = per_wage) %>% 
    generate_predictor(time_window = 2015, value15 = per_wage) %>% 
    generate_predictor(time_window = 2018, value18 = per_wage) %>% 
    generate_weights(optimization_window = period, sigf_ipop = 5) %>% # 6 won't work for west memphis
    generate_control()
}
hs = city_sc('Hot Springs', c('Pine Bluff', 'West Memphis'), 2019, 2011:2018)
pb = city_sc('Pine Bluff', c('West Memphis', 'Hot Springs'), 2020, 2011:2019)
wm = city_sc('West Memphis', c('Pine Bluff', 'Hot Springs'), 2019, 2011:2018)
control = function(data){
  bind_rows(grab_unit_weights(data, placebo = FALSE) %>% 
              mutate(type = "Control Unit Weights (W)"), grab_predictor_weights(data,placebo = FALSE) %>% 
              rename(unit = variable)) %>% arrange(desc(weight)) %>% 
    mutate(unit = forcats::fct_reorder(unit, weight)) %>% filter(type!= "Variable Weights (V)") %>%
    select(-type) %>% filter(weight>0.01)
}
(hs_control = control(hs) %>% mutate(x = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(x))
(pb_control = control(pb) %>% mutate(y = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(y))
(wm_control = control(wm) %>% mutate(z = paste0(unit, ' (', round(weight, 3), ')')) %>% pull(z))
(hs_control = paste('Synthetic Hot Springs:', paste(hs_control, collapse = ', ')))
(pb_control = paste('Synthetic Pine Bluff:', paste(pb_control, collapse = ', ')))
(wm_control = paste('Synthetic West Memphis:', paste(wm_control, collapse = ', ')))
city_control = t(data.frame(a = hs_control, b = pb_control, c = wm_control))
writeLines(htmlTable(city_control, rnames = F), "control_city.html")

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
    scale_y_continuous(name = "", limits = c(25000, 60000)) + 
    scale_x_continuous(name = '', breaks = seq(2011, 2022, 1)) +
    theme_classic() + 
    theme(legend.position = c(0.05, 0.99), 
          legend.justification = c("left", "top"), 
          legend.text = element_text(size = 16),
          legend.background = element_rect(fill = "white", color = "white"),
          plot.title = element_text(hjust = 0.5, size = 18))+
    guides(color=guide_legend(title=""))
}
load('AR_income.RDa'); AR_income
AR = geom_line(data = AR_income, aes(x=year, y = per_wage, color = 'Arkansas'), linewidth = 1, linetype='dotted') 
p1 = hs %>% plot_custom(title = 'Hot Springs') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic Hot Springs'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'Hot Springs'), linewidth = 1) +
  scale_color_manual(values = c('Hot Springs' = 'black', 'Synthetic Hot Springs' = 'darkgrey',
                                'Arkansas'='black'),
                     breaks = c('Hot Springs', 'Synthetic Hot Springs', 'Arkansas'));p1
p2 = pb %>% plot_custom(title = 'Pine Bluff') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic Pine Bluff'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'Pine Bluff'), linewidth = 1) +
  scale_color_manual(values = c('Pine Bluff' = 'black', 'Synthetic Pine Bluff' = 'darkgrey', 
                                'Arkansas'='black'),
                     breaks = c('Pine Bluff', 'Synthetic Pine Bluff', 'Arkansas'));p2
p3 = wm %>% plot_custom(title = 'West Memphis') + AR +
  geom_line(aes(x=year, y = Synthetic, color = 'Synthetic West Memphis'), linewidth = 1) + 
  geom_line(aes(x=year, y = treated, color = 'West Memphis'), linewidth = 1) +
  scale_color_manual(values = c('West Memphis' = 'black', 'Synthetic West Memphis' = 'darkgrey', 
                                'Arkansas'='black'),
                     breaks = c('West Memphis', 'Synthetic West Memphis', 'Arkansas'));p3
p = p1/p2/p3; ggsave(plot = p, filename = 'figure1.png', width = 10, height=13)

## predictors summary stats
library(forcats)
hs_sum = hs %>% grab_balance_table() %>% select(-donor_sample)
pb_sum = pb %>% grab_balance_table() %>% select(-donor_sample)
wm_sum = wm %>% grab_balance_table() 
(sum_stats = hs_sum %>% left_join(pb_sum, by = 'variable') %>% left_join(wm_sum, by = 'variable'))
colnames(sum_stats) = c('Predictors', 'Hot Springs', 'Synthetic<br>Hot Springs', 
                        'Pine Bluff', 'Synthetic<br>Pine Bluff', 
                        'West Memphis', 'Synthetic<br>West Memphis', 'Sample mean')
sum_stats
desired_order = c('unemployed', 'lfp', 'edu',
                  'young', 'old', 'black', 'hispanic',
                  'value11', 'value15', 'value18')
sum_stats = sum_stats %>% mutate(Predictors = as.factor(Predictors),
                                 Predictors = fct_relevel(Predictors, desired_order)) %>% 
  arrange(Predictors) %>% select(-Predictors)
var_name = c('Unemployment rate', 'Labor force participation rate', 'Share college graduate',
             'Share younger (<15)', 'Share older (>65)',  'Share Black people', 'Share Hispanic people', 
             'Average income in 2011', 'Average income in 2015',  'Average income in 2018')
(summary_table = as.data.frame(cbind(var_name, sum_stats)))
colnames(summary_table)[1] = 'Predictors'
summary_table[,-1] = lapply(summary_table[,-1], function(x) round(x, 1))
(city_table = summary_table)
writeLines(htmlTable(city_table, rnames = F), "predictors_city.html")
