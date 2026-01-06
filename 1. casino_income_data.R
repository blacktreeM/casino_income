library(tidyr); library(dplyr); library(zipcodeR); library(tools)
setwd('~/casino_income/')
# create aggregate for in Arkansas
if(F){
  (all_files = list.files(path = '~/casino_income/CBP_data'))
  (CBP = all_files[grepl("st", all_files, ignore.case = TRUE)])
  data = data.frame(fipstate = as.numeric(), emp = as.numeric(), year = as.numeric())
  for (i in CBP){
    df = read.csv(paste0('~/casino_income/CBP_data/', i)); print(i)
    colnames(df) = tolower(colnames(df))
    Year = substring(i, 4, 5)
    df = df %>% filter(naics=='------' & lfo == '-') %>% select(fipstate, emp) %>% mutate(year = Year)
    data = rbind(data, df)
  }
  data$year = 2000 + as.numeric(data$year)
  table(data$year)
  AR_emp = subset(data, fipstate==5)
  save(AR_emp, file = 'AR_emp.RDa')
  ########
  (files = paste0('IRS_data/', 11:22, 'zpallagi.csv'))
  all_data = lapply(files, function(path) {
    year = as.numeric(paste0(20, substring(path, 1, 2)))
    df = read.csv(path); print(path)
    colnames(df) = tolower(colnames(df))
    df = df %>% filter(state == 'AR' & zipcode ==0)
    colnames(df) = tolower(colnames(df))
    two_digit_year = gsub(".*?(\\d{2}).*", "\\1", basename(path))
    four_digit_year = as.integer(paste0("20", two_digit_year))
    df$year = four_digit_year
    return(df)
  })
  data = bind_rows(all_data); table(data$year)
  # a00200 salaries and wages amount
  data = data %>% rename('income' = 'a00100', 'wages_n' = 'n00200', 'wages' = 'a00200')
  data = data %>% group_by(year) %>% 
    summarise(n1 = sum(n1), income = sum(income), wages = sum(wages), wages_n = sum(wages_n)) %>% ungroup() %>% 
    mutate(per_income = 1000*income/n1, per_wage = 1000*wages/wages_n); data
  AR_income = data %>% select(year, per_wage, wages_n);data
  save(AR_income, file = 'AR_income.RDa')
}
# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-zip-code-data-soi
(files = paste0('IRS_data/', 11:22, 'zpallagi.csv'))
all_data = lapply(files, function(path) {
  year = as.numeric(paste0(20, substring(path, 1, 2)))
  df = read.csv(path)
  df = df %>% filter(STATE == 'AR')
  colnames(df) = tolower(colnames(df))
  two_digit_year = gsub(".*?(\\d{2}).*", "\\1", basename(path))
  four_digit_year = as.integer(paste0("20", two_digit_year))
  df$year = four_digit_year
  return(df)
})
data = bind_rows(all_data); table(data$year)
zip_lookup = reverse_zipcode(data$zipcode) %>% select(zipcode, major_city, county) %>% distinct() %>% 
  mutate(zipcode = as.integer(zipcode)) %>% rename('city' = 'major_city'); head(zip_lookup)
zip_lookup %>% arrange(city) %>% unique() %>% print(n=500)
data = data %>% select(zipcode, agi_stub, n1, a00100, a00200, n00200, year); head(data); summary(data)
data %>% filter(zipcode %in% c(72801, 72802) & year==2022)
# n1 number of returns
# a00100 adjust gross income
# a00200 salaries and wages amount
data = data %>% rename('income' = 'a00100', 'wages_n' = 'n00200', 'wages' = 'a00200'); head(data)
data = data %>% left_join(zip_lookup, by = "zipcode") 
#1 = $1 under $25,000
#2 = $25,000 under $50,000
#3 = $50,000 under $75,000
#4 = $75,000 under $100,000
#5 = $100,000 under $200,000
#6 = $200,000 or more
irs_city = data %>% group_by(year, city) %>% 
  summarise(n1 = sum(n1), income = sum(income), wages = sum(wages), wages_n = sum(wages_n)) %>% ungroup() %>% 
  mutate(per_income = 1000*income/n1, per_wage = 1000*wages/wages_n,
         city = ifelse(city=='Hot Springs National Park', 'Hot Springs', city))
subset(irs_city, city=='Russellville'); subset(irs_city, city=='Hot Springs')
irs_county = data %>% group_by(year, county) %>% 
  summarise(n1 = sum(n1), income = sum(income), wages = sum(wages), wages_n = sum(wages_n)) %>% ungroup() %>% 
  mutate(per_income = 1000*income/n1, per_wage = 1000*wages/wages_n, county = gsub(' County', '', county))
subset(irs_county, county=='Pope')
save(irs_city, file = 'irs_city.RDa')
save(irs_county, file = 'irs_county.RDa')
irs_city %>% filter(city == 'Pine Bluff')
irs_city %>% filter(city == 'West Memphis')
irs_city %>% filter(city == 'Hot Springs')
############################################
### CBP zipcode data
# EMP             N       Total Mid-March Employees with Noise
# QP1             N       Total First Quarter Payroll ($1,000) with Noise
(files = paste0('CBP_data/zbp', 11:23, 'totals.txt'))
data = read.csv(paste0('CBP_data/zbp', 23, 'totals.txt')); head(data)
cities = lapply(files, function(i){
  year = as.numeric(paste0(20, substring(i, 13, 14))); print(year)
  df = read.csv(i)
  city = df %>% filter(stabbr == 'AR') %>% group_by(city) %>% 
    summarise(qp1 = sum(qp1), emp = sum(emp)) %>% ungroup() %>% 
    mutate(city = toTitleCase(tolower(city)), pay = 1000*4*qp1/emp,
           year = year) %>% filter(!is.na(pay))
  return(city)
})
cities = bind_rows(cities); table(cities$year); head(cities)
cities = cities %>% mutate(city = ifelse(city=='Hot Springs National Park', 'Hot Springs', city))
cities %>% filter(city %in% c('Hot Springs',  'West Memphis') & year==2023)
cities %>% filter(city %in% c('Pine Bluff') )
# pine bluff too high?
pb = data %>% filter(city=='PINE BLUFF');pb
1000*4*sum(pb$qp1)/sum(pb$emp)
1000*sum(pb$ap)/sum(pb$emp)
#
counties = lapply(files, function(i){
  year = as.numeric(paste0(20, substring(i, 13, 14))); print(year)
  df = read.csv(i)
  county = df %>% filter(stabbr == 'AR') %>% 
    mutate(cty_name = ifelse(cty_name=="SAINT FRANCIS", "ST. FRANCIS" , cty_name)) %>% 
    group_by(cty_name) %>% 
    summarise(qp1 = sum(qp1), emp = sum(emp)) %>% ungroup() %>% 
    mutate(county = toTitleCase(tolower(cty_name)), pay = 1000*4*qp1/emp,
           year = year) %>% 
    select(county, pay, year, emp) %>% filter(!is.na(pay))
  return(county)
})
counties = bind_rows(counties); table(counties$year); table(counties$county); head(counties)
subset(counties, county=='Pope'); subset(cities, city=='Russellville')
summary(cities)
# delete infinity and zero pay
cities = cities %>% filter(pay>0 & !is.infinite(pay));summary(cities)
save(cities, file = 'CBP_cities.RDa')
save(counties, file = 'CBP_county.RDa')
### city ACS
key = '60afb8b8550abb1dc40fef1e46bf9c492b1dc98d'
library(tidycensus); library(dplyr); library(tidyr)
census_api_key(key, install=T, overwrite = T)
acs =  function(year){
  get_acs(geography = "place", state = "AR",
          variables = c(income = 'B19013_001', employed = 'B23025_004', unemployed = 'B23025_005',
                        pop = 'B03002_001', black = 'B03002_004', hispanic = 'B03002_012', 
                        male = 'B15002_002', ns1 = 'B15002_003',
                        ns2 = 'B15002_004', ns3 = 'B15002_005',
                        ns4 = 'B15002_006', ns5 = 'B15002_007', 
                        ns6 = 'B15002_008', ns7 = 'B15002_009',
                        ns8 = 'B15002_010', collegeM = 'B15002_015',
                        masterM = 'B15002_016', profM = 'B15002_017',
                        doctorM = 'B15002_018', 
                        female = 'B15002_019', nsf1 = 'B15002_020',
                        nsf2 = 'B15002_021', nsf3 = 'B15002_022',
                        nsf4 = 'B15002_023', nsf5 = 'B15002_024', 
                        nsf6 = 'B15002_025', nsf7 = 'B15002_026',
                        nsf8 = 'B15002_027', collegeF = 'B15002_032',
                        masterF = 'B15002_033', profF = 'B15002_034',
                        doctorF = 'B15002_035',
                        pop16 = 'DP03_0001',
                        lf = 'DP03_0003',
                        pop_foreign = 'DP02_0088',
                        foreign = 'DP02_0094',
                        men = 'DP02_0025',
                        men_marry = 'DP02_0027',
                        women = 'DP02_0031',
                        women_marry = 'DP02_0033',
                        age_pop = 'DP05_0001',
                        old = 'DP05_0024',
                        young = 'DP05_0019',
                        median_age = 'DP05_0018'),
          year = year)  %>% select(-moe) %>% 
    rename('fips' = 'GEOID', 'city' = 'NAME') %>% 
    pivot_wider(names_from = variable, values_from = estimate)%>% 
    mutate(fips = as.numeric(fips), 
           edu = 100*(collegeM + masterM + profM + doctorM+collegeF + masterF + profF +  doctorF)/(male+female),
           drop=100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8+nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/(male+female),
           edu_male = 100*(collegeM + masterM + profM + doctorM)/male,
           edu_female = 100*(collegeF + masterF + profF +  doctorF)/female,
           drop_male = 100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8)/male,
           drop_female = 100*(nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/female) %>% 
    mutate(men_married = 100*men_marry/men,
           women_married = 100*women_marry/women,
           young = 100*young/age_pop, old = 100*old/age_pop,
           lfp = 100*lf/pop16, 
           foreign = 100*foreign/pop_foreign,
           black = 100*black/pop, hispanic = 100*hispanic/pop,
           unemployed = 100*unemployed / (employed + unemployed),
           year = year) %>% select(fips, year, city, unemployed, lfp, old, black, hispanic, young, edu, pop)
}
acs16 = acs(2016)
acs17 = acs(2017)
acs18 = acs(2018)
acs = rbind(acs17, acs18) # 2016 acs different variables
city = acs %>% mutate(city = gsub(', Arkansas', '', city), city = gsub(' city| town', '', city)); unique(city$city)
save(city, file = 'acs_city.RDa')
# ACS county
acs =  function(year){
  get_acs(geography = "county", state = "AR",
          variables = c(income = 'B19013_001', employed = 'B23025_004', unemployed = 'B23025_005',
                        pop = 'B03002_001', black = 'B03002_004', hispanic = 'B03002_012', 
                        male = 'B15002_002', ns1 = 'B15002_003',
                        ns2 = 'B15002_004', ns3 = 'B15002_005',
                        ns4 = 'B15002_006', ns5 = 'B15002_007', 
                        ns6 = 'B15002_008', ns7 = 'B15002_009',
                        ns8 = 'B15002_010', collegeM = 'B15002_015',
                        masterM = 'B15002_016', profM = 'B15002_017',
                        doctorM = 'B15002_018', 
                        female = 'B15002_019', nsf1 = 'B15002_020',
                        nsf2 = 'B15002_021', nsf3 = 'B15002_022',
                        nsf4 = 'B15002_023', nsf5 = 'B15002_024', 
                        nsf6 = 'B15002_025', nsf7 = 'B15002_026',
                        nsf8 = 'B15002_027', collegeF = 'B15002_032',
                        masterF = 'B15002_033', profF = 'B15002_034',
                        doctorF = 'B15002_035',
                        pop16 = 'DP03_0001',
                        lf = 'DP03_0003',
                        pop_foreign = 'DP02_0088',
                        foreign = 'DP02_0094',
                        men = 'DP02_0025',
                        men_marry = 'DP02_0027',
                        women = 'DP02_0031',
                        women_marry = 'DP02_0033',
                        age_pop = 'DP05_0001',
                        old = 'DP05_0024',
                        young = 'DP05_0019',
                        median_age = 'DP05_0018'),
          year = year)  %>% select(-moe) %>% 
    rename('fips' = 'GEOID', 'county' = 'NAME') %>% 
    pivot_wider(names_from = variable, values_from = estimate)%>% 
    mutate(fips = as.numeric(fips), 
           edu = 100*(collegeM + masterM + profM + doctorM+collegeF + masterF + profF +  doctorF)/(male+female),
           drop=100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8+nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/(male+female),
           edu_male = 100*(collegeM + masterM + profM + doctorM)/male,
           edu_female = 100*(collegeF + masterF + profF +  doctorF)/female,
           drop_male = 100*(ns1+ns2+ns3+ns4+ns5+ns6+ns7+ns8)/male,
           drop_female = 100*(nsf1+nsf2+nsf3+nsf4+nsf5+nsf6+nsf7+nsf8)/female) %>% 
    mutate(men_married = 100*men_marry/men,
           women_married = 100*women_marry/women,
           young = 100*young/age_pop, old = 100*old/age_pop,
           lfp = 100*lf/pop16, 
           foreign = 100*foreign/pop_foreign,
           black = 100*black/pop, hispanic = 100*hispanic/pop,
           unemployed = 100*unemployed / (employed + unemployed),
           year = year)%>% select(fips, year, county, unemployed, lfp, old, black, hispanic, young, edu,pop)
}
acs16 = acs(2016)
acs17 = acs(2017)
acs18 = acs(2018)
acs = rbind(acs17, acs18)
county = acs %>% mutate(county = gsub(' County, Arkansas', '', county)); unique(county$county)
save(county, file = '~/casino_income/acs_county.RDa')
#################### manufacturing
(all_files = list.files(path = '~/casino_income/CBP_data'))
(CBP = all_files[grepl("\\co.txt$", all_files, ignore.case = TRUE) &
                   grepl("cbp", all_files, ignore.case = TRUE)])
data = data.frame(fipscty = as.numeric(), emp = as.numeric(), mfg = as.numeric(), 
                  m_share = as.numeric(), year = as.numeric())
for (i in CBP){
  df = read.csv(paste0('~/casino_income/CBP_data/', i)); print(i)
  Year = substring(i, 4, 5)
  colnames(df) = tolower(colnames(df))
  mfg = df %>% filter(grepl("^3", naics) & fipstate == 5 & fipscty!=999) %>% select(fipscty, naics, emp); head(mfg, 10)
  mfg = mfg %>% filter(grepl("///", naics) | grepl("----", naics)); head(mfg, 10)# 32 and 33 don't have "----"
  mfg1 = mfg %>% filter(naics == '31----'); head(mfg1)
  mfg2 = mfg %>% filter(!grepl("^31", naics)); head(mfg2) # need exclude 311/// for example
  mfg = mfg1 %>% bind_rows(mfg2) %>% arrange(fipscty); rm(mfg1, mfg2); head(mfg, 20)
  mfg %>% filter(fipscty==115)
  mfg = mfg %>% group_by(fipscty) %>% summarise(mfg = sum(emp, na.rm = TRUE), .groups = 'drop'); head(mfg)
  total = df %>% filter(naics == '------' & fipstate == 5 & fipscty!=999) %>% select(fipscty, emp); head(total)
  emp = total %>% left_join(mfg, by = 'fipscty') %>% mutate(m_share = 100*mfg/emp); emp
  emp = emp %>% mutate(m_share = ifelse(is.na(m_share), mean(m_share, na.rm = T), m_share),
                       year = Year); emp
  data = rbind(data, emp)
}
data$year = 2000 + as.numeric(data$year)
table(data$year); summary(data)
aggregate(mfg~year, data = data, mean)
mfg = data
save(mfg, file = 'mfg.RDa')
########## 
load('irs_city.RDa'); head(irs_city)
load('CBP_cities.RDa'); head(cities)
# merge city level data
cities = cities %>% left_join(irs_city, by = c('year', 'city')); head(cities)
load('acs_city.RDa'); head(city)
data_city = cities %>% left_join(city, by = c('city', 'year')) %>% filter(!is.na(city)); unique(data_city$city)
summary(data_city)
save(data_city, file = 'data_city.RDa')
########## merge county
load('irs_county.RDa')
load('acs_county.RDa')
load('CBP_county.RDa')
load('mfg.RDa'); head(mfg)
aggregate(emp~year, data = mfg, mean)
mfg = mfg %>% mutate(fips = 5000 + as.numeric(fipscty)) %>% select(-fipscty, -emp); head(mfg)
unique(mfg$fips)
unique(county$fips)
counties = counties %>% left_join(irs_county, by = c('year', 'county')); head(counties)
data_county = counties %>% left_join(county, by = c('county', 'year')) %>% 
  group_by(county) %>% fill(fips, .direction = "downup") %>%  ungroup() %>%  # fips only availabe for ACS years
  filter(!is.na(county)); unique(data_county$county)
data_county = data_county %>% left_join(mfg, by = c('fips', 'year')) 
summary(data_county)
table(data_city$year); table(data_county$year)
save(data_county, file = 'data_county.RDa')
