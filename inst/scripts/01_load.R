library(data.table)
library(magrittr)

#### load data ####
d <- CelloLoad::load_cello('//chidns-mediauk/insight media (charterhouse)$/Research/2018/180233/Data/Data/Data/Final data inc recontacts/p1867904164_sandeep.shetty_193740364.sav')
d1 <- d$data %>% 
  .[status == 'complete']

# identify respondent id and country variable
id_mask <- 'respid'
country_mask <- 'h0'

# transfrom country to factor
d1[, 'country' := haven::as_factor(get(country_mask))]


#### identify proportion of progression ####
# Of the [INSERT CODE 03 @ Q9] patients with Non-Metastatic Hormone Sensitive Prostate 
# Cancer (M0 HS PC) that you personally managed in the last 3 months, what proportion would 
# you expect to progress directly onto the stages of prostate cancer listed below?
q14_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q14')

# fix labels for q14
new_labs <- c('Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC)', 
              'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC)',
              'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC)')
d$labels[variable %in% q14_mask, 'label' := new_labs]


#### identify patient case load ####
# Of the [INSERT CODE 01”Confirmed” @ S7] patients with prostate cancer that 
# you [IF CODE 01 or 02 “SPECIALIST”@ S2 personally managed IF CODE 03 “PCP” @ S2 saw] 
# in the last 3 months,  how many currently fall into each of the following patient groups?
# 
# keeping raw patient numbers for weighting purposes
q9_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q9_a_([1-8])')

# fix labels for q9
new_labs <- c('Localised Prostate Cancer (LPC)  and Locally Advanced Prostate Cancer (LAPC)', 
              'Indolent disease',
              'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC)',
              'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC)',
              'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC)',
              'De Novo Metastatic Prostate Cancer (De Novo M1 PC)',
              'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC)',
              'Other')
d$labels[variable %in% q9_mask, 'label' := new_labs]


#### identify % of direct progression to prostate cancer ####
# Of the [INSERT CODE 01 @ Q9] patients with Localised Prostate Cancer (LPC) and 
# Locally Advanced Prostate Cancer (LAPC) that you [IF CODE 01 or 02 “SPECIALIST”@ S2 
# personally managed IF CODE 03 “PCP” @ S2 saw] in the last 3 months, what proportion 
# would you expect to progress directly onto the stages of prostate cancer listed below?
q11_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q11')
new_labs <- c('Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC)', 
              'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC)')
d$labels[variable %in% q11_mask, 'label' := new_labs]

#### identify progression from LPC and LAPC ####
# Thinking of your last 5 patients who progressed from Localised Prostate Cancer (LPC) 
# and Locally Advanced Prostate Cancer (LAPC) to each of the below stages, how long did 
# it take these patients to progress? 
# 
# Please provide your best estimate in years and months (0-11).
# format q12_x_y_z where x = y (years) or m (months), y = 1:5 patient id, z = 1 (M0 HS PC) or 2 (M1 HS PC)
q12_mask0 <- d1 %>% 
  names %>% 
  stringr::str_subset('q12')

# calculate single duration variables in months
aux_d <- d1[,c(id_mask, q12_mask0), with = F] %>% 
  data.table::melt.data.table(id.vars = id_mask) %>% 
  .[, 'time_metric' := ifelse(stringr::str_detect(variable, 'y'), 'years', 'months')] %>% 
  .[, 'patient' := paste0('q12new_',stringr::str_replace_all(variable, '^(q12_.{1}_)|(_.{1})$', ''))] %>% 
  .[, 'cancer' := stringr::str_replace_all(variable, 'q12_.{1}_.{1}_', '')] %>% 
  data.table::dcast.data.table(respid + patient + cancer ~ time_metric,
                               value.var = 'value') %>% 
  .[, 'new_months' := years * 12 + months]
aux_d <- data.table::rbindlist(list(aux_d,
                                    aux_d %>% 
                                      .[, mean(new_months, na.rm = TRUE), by = c('respid', 'cancer')] %>% 
                                      .[, 'patient' := 'q12new_tot'] %>% 
                                      .[, 'new_months' := V1] %>% 
                                      .[, 'V1' := NULL]),
                               fill = TRUE) %>% 
  data.table::dcast.data.table(respid ~ patient + cancer,
                               value.var = 'new_months')
d1 <- d1[aux_d]
q12_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q12new')

d$labels <- rbind(d$labels,data.table::data.table(variable = q12_mask,
                                   label = c('Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Patient 1',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 1',
                                             'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Patient 2',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 2',
                                             'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Patient 3',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 3',
                                             'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Patient 4',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 4',
                                             'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Patient 5',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 5',
                                             'Non-Metastatic Hormone Sensitive Prostate Cancer (M0 HS PC) - Total',
                                             'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Total')))

d$labels[variable %in% q12_mask]

#### identify progression from M0 HS PC ####
# Thinking of your last 5 patients who progressed from Non-Metastatic Hormone Sensitive 
# Prostate Cancer (M0 HS PC) to each of the below stages, how long did it take these patients 
# to progress? 
#
# Please provide your best estimate in years and months (0-11)
# format q15_x_y_z where x = y (years) or m (months), y = 1:5 patient id, z = 1 (M0 CR PC) or 2 (M1 HS PC) or 3 (Ms CR PC)
q15_mask0 <- d1 %>% 
  names %>% 
  stringr::str_subset('q15')

# calculate single duration variables in months
aux_d <- d1[,c(id_mask, q15_mask0), with = F] %>% 
  data.table::melt.data.table(id.vars = id_mask) %>% 
  .[, 'time_metric' := ifelse(stringr::str_detect(variable, 'y'), 'years', 'months')] %>% 
  .[, 'patient' := paste0('q15new_',stringr::str_replace_all(variable, '^(q15_.{1}_)|(_.{1})$', ''))] %>% 
  .[, 'cancer' := stringr::str_replace_all(variable, 'q15_.{1}_.{1}_', '')] %>% 
  data.table::dcast.data.table(respid + patient + cancer ~ time_metric,
                               value.var = 'value') %>% 
  .[, 'new_months' := years * 12 + months]
aux_d <- data.table::rbindlist(list(aux_d,
                                    aux_d %>% 
                                      .[, mean(new_months, na.rm = TRUE), by = c('respid', 'cancer')] %>% 
                                      .[, 'patient' := 'q15new_tot'] %>% 
                                      .[, 'new_months' := V1] %>% 
                                      .[, 'V1' := NULL]),
                               fill = TRUE) %>% 
  data.table::dcast.data.table(respid ~ patient + cancer,
                               value.var = 'new_months')
d1 <- d1[aux_d]
q15_mask <- d1 %>%
  names %>%
  stringr::str_subset('q15new')

d$labels <- rbind(d$labels,data.table::data.table(variable = q15_mask,
                                                  label = c('Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Patient 1',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 1',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Patient 1',
                                                            'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Patient 2',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 2',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Patient 2',
                                                            'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Patient 3',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 3',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Patient 3',
                                                            'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Patient 4',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 4',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Patient 4',
                                                            'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Patient 5',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Patient 5',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Patient 5',
                                                            'Non-Metastatic Castrate Resistant Prostate Cancer (M0 CR PC) - Total',
                                                            'Metastatic Hormone Sensitive Prostate Cancer (M1 HS PC) - Total',
                                                            'Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) - Total')))

d$labels[variable %in% q15_mask]



#### identify M1 CR PC Line of Treatment proportions ####
# Of the [INSERT CODE 07 @ Q9] patients with Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) 
# that you personally managed in the last 3 months, what proportion are currently on each of the following 
# lines of treatment? 
q25_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q25')


#### identify M1 CR PC 1st Line Treatment progression ####
# Of the [INSERT CODE 07 @ Q9] patients with Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) currently 
# on 1st line treatment that you personally managed in the last 3 months:
#
# a)	What proportion are currently receiving, or did receive, each of the following treatments? (Count each patient only once)
# b)	How long do patients typically remain on these treatments before discontinuing? 
#   (Please provide your answer in months) 
# c)	What proportion would you expect to discontinue these treatments before progression of their disease?
#
# format q26_x_y where x = a, b, or c se above, y = 1:8 per treatment.
q26_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q26_.{1}_([1-9]){1}$')

q26b_mask <- q26_mask %>% stringr::str_subset('b')
q26a_mask <- q26_mask %>% stringr::str_subset('a')

aux_d <- d1[, c(id_mask, 'country', q26a_mask, q26b_mask), with = F] %>% 
  data.table::melt.data.table(id.vars = c(id_mask, 'country')) %>% 
  .[, 'metric' := ifelse(stringr::str_detect(variable, 'a'), 'proportion', 'duration')] %>% 
  .[, 'treatment' := paste0('q26treat_', stringr::str_extract(variable, '\\d{1}$'))] %>% 
  data.table::dcast.data.table(respid + country + treatment ~ metric,
                               value.var = 'value') %>% 
  .[, 'duration' := duration * proportion/100] %>% 
  .[, sum(duration, na.rm = TRUE), by = 'respid'] %>% 
  data.table::setnames('V1', 'q26duration')

d1 <- d1[aux_d]


#### identify M1 CR PC 2nd Line Treatment progression ####
# Of the [INSERT CODE 07 @ Q9] patients with Metastatic Castrate Resistant Prostate Cancer (M1 CR PC) currently on 
# 2nd line treatment that you personally managed in the last 3 months:
#
# a)	What proportion are currently receiving, or did receive, each of the following treatments? (Count each patient only once)
# b)	How long do patients typically remain on these treatments before discontinuing? 
#   (Please provide your answer in months) 
# c)	What proportion would you expect to discontinue these treatments before progression of their disease?
#
# format q27_x_y where x = a, b, or c se above, y = 1:8 per treatment.
q27_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q27_.{1}_([1-9]){1}$')

q27_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q27_.{1}_([1-9]){1}$')

q27b_mask <- q27_mask %>% stringr::str_subset('b')
q27a_mask <- q27_mask %>% stringr::str_subset('a')

aux_d <- d1[, c(id_mask, 'country', q27a_mask, q27b_mask), with = F] %>% 
  data.table::melt.data.table(id.vars = c(id_mask, 'country')) %>% 
  .[, 'metric' := ifelse(stringr::str_detect(variable, 'a'), 'proportion', 'duration')] %>% 
  .[, 'treatment' := paste0('q27treat_', stringr::str_extract(variable, '\\d{1}$'))] %>% 
  data.table::dcast.data.table(respid + country + treatment ~ metric,
                               value.var = 'value') %>% 
  .[, 'duration' := duration * proportion/100] %>% 
  .[, sum(duration, na.rm = TRUE), by = 'respid'] %>% 
  data.table::setnames('V1', 'q27duration')

d1 <- d1[aux_d]
