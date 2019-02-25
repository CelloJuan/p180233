library(data.table)
library(magrittr)

#### load data ####
d <- CelloLoad::load_cello('//chidns-mediauk/insight media (charterhouse)$/Research/2018/180233/Data/Data/Data/Final data inc recontacts/p1867904164_sandeep.shetty_193740364.sav')
d1 <- d$data %>% 
  .[status == 'complete']

# identify respondent id variable
id_mask <- 'respid'


#### identify proportion of progression ####
# Of the [INSERT CODE 03 @ Q9] patients with Non-Metastatic Hormone Sensitive Prostate 
# Cancer (M0 HS PC) that you personally managed in the last 3 months, what proportion would 
# you expect to progress directly onto the stages of prostate cancer listed below?
q14_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q14')
d1[, ..q14_mask]


#### identify patient case load ####
# Of the [INSERT CODE 01”Confirmed” @ S7] patients with prostate cancer that 
# you [IF CODE 01 or 02 “SPECIALIST”@ S2 personally managed IF CODE 03 “PCP” @ S2 saw] 
# in the last 3 months,  how many currently fall into each of the following patient groups?
# 
# keeping raw patient numbers for weighting purposes
q9_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q9_a_([1-8])')
d1[,..q9_mask]


#### identify % of direct progression to prostate cancer ####
# Of the [INSERT CODE 01 @ Q9] patients with Localised Prostate Cancer (LPC) and 
# Locally Advanced Prostate Cancer (LAPC) that you [IF CODE 01 or 02 “SPECIALIST”@ S2 
# personally managed IF CODE 03 “PCP” @ S2 saw] in the last 3 months, what proportion 
# would you expect to progress directly onto the stages of prostate cancer listed below?
q11_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q11')


#### identify progression from LPC and LAPC ####
# Thinking of your last 5 patients who progressed from Localised Prostate Cancer (LPC) 
# and Locally Advanced Prostate Cancer (LAPC) to each of the below stages, how long did 
# it take these patients to progress? 
# 
# Please provide your best estimate in years and months (0-11).
# format q12_x_y_z where x = y (years) or m (months), y = 1:5 patient id, z = 1 (M0 HS PC) or 2 (M1 HS PC)
q12_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q12')


#### identify progression from M0 HS PC ####
# Thinking of your last 5 patients who progressed from Non-Metastatic Hormone Sensitive 
# Prostate Cancer (M0 HS PC) to each of the below stages, how long did it take these patients 
# to progress? 
#
# Please provide your best estimate in years and months (0-11)
# format q15_x_y_z where x = y (years) or m (months), y = 1:5 patient id, z = 1 (M0 CR PC) or 2 (M1 HS PC) or 3 (Ms CR PC)
q15_mask <- d1 %>% 
  names %>% 
  stringr::str_subset('q15')


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
