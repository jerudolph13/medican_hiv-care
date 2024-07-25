
################################################################################
#
# Project: HIV Care Cascade
#
# Purpose: Describe sample
#
# Author: Jacqueline Rudolph
#
# Last Update: 10 Oct 2023
#
################################################################################


packages <- c("dplyr", "tidyr", "magrittr", "readr", "lubridate", "tableone")
for (package in packages){
  library(package, character.only=T)
}

mpr_limit <- 80

# Read in data ------------------------------------------------------------

dat <- read_csv("./care-cascade/data/care-cascade_data.csv", 
                col_types=c("cDDfffDDfDDfdcddDdDdddDDddfdddddddddddddddd")) %>% 
  filter(sex!="") %>% 
  mutate(n_cond = myocard_infarct + cong_heart_fail + periph_vas_dis + cereb_vas_dis + 
           dementia + chron_pulm_dis + rheumatic_dis + peptic_ulcer + 
           mild_liv_dis + mod_sevr_liver_dis + diabt_wo_chron_complict + 
           diabt_chron_complict + hemiplegia + renal_dis + 
           metastatic + any_malignancy,
         n_cond = factor(ifelse(n_cond==0, 0,
                                ifelse(n_cond==1, 1, 2))),
         yr_base = year(baseline),
         enroll_period = case_when(yr_base %in% 2001:2005 ~ 1,
                                   yr_base %in% 2006:2010 ~ 2,
                                   T ~ 3),
         enroll_period = factor(enroll_period, levels=1:3, labels=c("2001-2005", "2006-2010", "2011-2015")),
         region = case_when(
           first_elig_state %in% c("MA", "NY", "PA") ~ "northeast",
           first_elig_state %in% c("IL", "OH") ~ "midwest",
           first_elig_state %in% c("AL", "FL", "GA", "MD", "NC", "TX") ~ "south",
           first_elig_state %in% c("CA", "CO", "WA") ~ "west"),
         race = factor(race, levels=1:4, labels=c("NH white", "NH Black", "Hisp", "Other")),
         state = case_when(
           (dod > int_start) & (dod<=int_end) ~ 5, # Dead
           int_end==end_date ~ 0,                  # Censored
           mpr>=mpr_limit & retained==1 ~ 1,       # Retained; ART adherent
           mpr<mpr_limit & retained==1 ~ 2,        # Retained; Not ART adherent
           mpr>=mpr_limit & retained==0 ~ 3,       # Not retained; ART adherent
           mpr<mpr_limit & retained==0 ~ 4))


# Describe sample ---------------------------------------------------------

# Median follow-up
pre.drop <- filter(dat, drop!=1) %>% 
  filter(!duplicated(bene_id, fromLast=T))
summary(pre.drop$int)

# Characteristics
base <- filter(dat, !duplicated(bene_id))  

table.base <- as_tibble(print(CreateTableOne(data=base, vars=c("age_base", "sex", "race", "first_elig_state", "enroll_period", "n_cond")),
                              contDigits=1, noSpaces=T, nonnormal="age_base"),
                        rownames="Variable")

dat.state <- filter(dat, state %in% 1:4) %>% 
  mutate(state = factor(state, levels=1:4, labels=c("Retained; ART adherent", "Retained; Not ART adherent",
                                                    "Not retained; ART adherent", "Not retained; Not ART adherent")))
table.state <- as_tibble(print(CreateTableOne(data=dat.state, 
                                              vars=c("age_base", "sex", "race", "first_elig_state", "enroll_period", "n_cond"),
                                              strata="state", test=F),
                               contDigits=1, noSpaces=T, nonnormal="age_base"),
                         rownames="Variable")

table <- left_join(table.base, table.state, by="Variable")
write_csv(table, "./care-cascade/results/table1.csv")
