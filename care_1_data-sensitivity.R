
################################################################################
#
# Project: HIV Care Cascade
#
# Purpose: Manage raw data (first eligible period)
#
# Last Update: 15 Apr 2024
#
################################################################################


packages <- c("dplyr", "tidyr", "magrittr", "readr", "lubridate", "zoo", "ggplot2")
for (package in packages){
  library(package, character.only=T)
}


# Read in data ------------------------------------------------------------

# Diagnosis data
dat <- read_csv(file="../../data/MedicanDataRequest/jackie/20230628_jackie_incidences_v5/incid_first_prd_1_v5.csv",
                col_types=paste0("cfffffccDDDDf", paste(rep("D", 67), collapse=""))) %>%
  rename(start_date = first_elig_period_start_date_65,
         end_date = first_elig_period_end_date_65,
         benefits = first_elig_period_rstrct_bnft_65) %>%
  select(bene_id, dob, dod, race, sex, first_elig_state,
         start_date, end_date, benefits,
         hiv_cms_typ2_dt, myocard_infarct_typ1_dt, cong_heart_fail_typ1_dt,
         periph_vas_dis_typ1_dt, cereb_vas_dis_typ1_dt, dementia_typ1_dt,
         chron_pulm_dis_typ1_dt, rheumatic_dis_typ1_dt, peptic_ulcer_typ1_dt,
         mild_liv_dis_typ1_dt, mod_sevr_liver_dis_typ1_dt, diabt_wo_chron_complict_typ1_dt,
         diabt_chron_complict_da_typ1_dt, hemiplegia_paraplegia_d_typ1_dt,
         renal_dis_typ1_dt, metastatic_typ1_dt, any_malignancy_typ1_dt)

# ART data
art <- read_csv(file="../../data/MedicanDataRequest/jackie/20231025_jackie_hiv_engagement_v2/hiv_rx.csv",
                col_types="ccDfd") %>%
  filter(art_type==0) %>%
  rename(int_start = yr_mo) %>%
  select(-c(first_elig_state, art_type))

# Doctor visits
proc <- read_csv(file="../../data/MedicanDataRequest/jackie/20231025_jackie_hiv_engagement_v2/hiv_proc.csv",
                 col_types="cDf")


# Manage data -------------------------------------------------------------

# Identify beneficiaries with HIV
hiv.dat <- dat %>%
  # Define baseline age and HIV status
  mutate(baseline = start_date + months(6),
         hiv = case_when(is.na(hiv_cms_typ2_dt) ~ 0,
                         hiv_cms_typ2_dt > end_date ~ 0,
                         # baseline HIV
                         hiv_cms_typ2_dt <= baseline ~ 1,
                         # incident HIV
                         hiv_cms_typ2_dt > baseline ~ 2)) %>%
  filter(hiv>0) %>%
  filter(benefits==1) # Include beneficiaries with full benefits

# Set up for longitudinal data, with time since HIV diagnosis
hiv.dat2 <- hiv.dat %>%
  mutate(# Baseline: First date of month following diagnosis or first month after run-in period
    baseline = ifelse(hiv==1, baseline, floor_date(hiv_cms_typ2_dt + months(1), "month")),
    baseline = as.Date(baseline, origin=ymd("1970-01-01")),
    age_base = (dob %--% baseline)/years(1),
    
    # In this analysis, end follow-up on Sep 30, 2015 for all states
    admin_censor = "2015-09-30",
    end_date = ifelse(end_date>ymd(admin_censor), ymd(admin_censor), end_date),
    end_date = as.Date(end_date, origin=ymd("1970-01-01")),
    
    months_follow = ceiling((baseline %--% end_date)/months(1))) %>%
  filter((dod > baseline) | is.na(dod)) %>%  # Remove individuals who died prior to index date
  filter(months_follow>0) # To be included, must have at least 1 month of follow-up

# Create longitudinal data
hiv.long <- bind_rows(lapply(hiv.dat2, rep, hiv.dat2$months_follow))

hiv.long <- hiv.long %>%
  group_by(bene_id) %>%
  mutate(# Set up time
    n = 1,
    int = cumsum(n),
    
    # Start date of month interval
    int_start = baseline + months(int-1),
    int_start = ifelse(is.na(int_start),
                       ymd(substr(baseline + dmonths(int-1), 1, 10)),
                       int_start),
    int_start = as.Date(int_start, origin=ymd("1970-01-01")),
    start = (ymd("2001-07-01") %--% int_start)/days(1),
    
    # End date of month interval
    int_end = lead(int_start), # For everything except last record
    int_end = ifelse(is.na(int_end), end_date, int_end),
    int_end = as.Date(int_end, origin=ymd("1970-01-01")),
    end = (ymd("2001-07-01") %--% int_end)/days(1)) %>%
  select(-n) %>%
  ungroup()


# Merge ART data ----------------------------------------------------------

# Merge ART data
hiv.long2 <- left_join(hiv.long, art, by=c("bene_id", "int_start"))

summ.art <- hiv.long2 %>%
  group_by(int_start) %>%
  summarize(avg_mpr = mean(mpr, na.rm=T),
            med_mpr = median(mpr, na.rm=T),
            p25_mpr = quantile(mpr, p=0.25, na.rm=T),
            p75_mpr = quantile(mpr, p=0.75, na.rm=T),
            n = n())


# Merge care data ---------------------------------------------------------

# Retained in care = 1 visit or 1 VL every 6 months
proc2 <- proc %>%
  mutate(int_start = floor_date(proc_date, "month"),
         n = 1) %>%
  group_by(bene_id, int_start) %>%
  summarize(n_proc = sum(n),
            visit_dt = last(proc_date)) %>%
  select(bene_id, int_start, n_proc, visit_dt)

hiv.long3 <- left_join(hiv.long2, proc2, by=c("bene_id", "int_start")) %>%
  mutate(n_proc = ifelse(is.na(n_proc), 0, n_proc))

# Determine time since last care visit
date.fill <- hiv.long3 %>%
  select(bene_id, int_start, visit_dt) %>%
  group_by(bene_id) %>%
  mutate_at(vars(-group_cols()), list( ~ na.locf(., na.rm = FALSE))) %>%
  rename(last_visit_dt = visit_dt)

hiv.long4 <- left_join(hiv.long3, date.fill, by=c("bene_id", "int_start")) %>%
  group_by(bene_id) %>%
  mutate(last_visit_dt = ifelse(n_proc>0, lag(last_visit_dt), last_visit_dt),
         last_visit_dt = as.Date(last_visit_dt, origin=ymd("1970-01-01")),
         
         # Months since last care visit
         month_btw_visit = case_when(
           is.na(last_visit_dt) & n_proc==0 ~ (baseline %--% int_end)/months(1),
           is.na(last_visit_dt) & n_proc>0 ~ (baseline %--% visit_dt)/months(1),
           n_proc==0 ~ (last_visit_dt %--% int_end)/months(1),
           n_proc>0 ~ (last_visit_dt %--% visit_dt)/months(1)),
         
         # Retained in care
         retained = as.numeric(month_btw_visit<=6))

summ.visit <- hiv.long4 %>%
  group_by(int_start) %>%
  summarize(prev_retained = mean(retained, na.rm=T))


# Set up for analysis -----------------------------------------------------

# Define loss to follow-up (censoring prior to admin censoring)
hiv.long5 <- hiv.long4 %>%
  mutate(last = as.numeric(!duplicated(bene_id, fromLast=T)),
         date65 = dob + years(65) - months(1),
         date65 = ifelse(is.na(date65),
                         ymd(substr(dob + years(65) - dmonths(1), 1, 10)),
                         date65),
         date65 = ifelse(is.na(date65),
                         ymd(substr(dob - dmonths(1) + years(65), 1, 10)),
                         date65),
         date65 = as.Date(date65, origin=ymd("1970-01-01")),
         drop = as.numeric(last==1 & int_end<date65 & int_end<admin_censor))

# Define comorbidity covariates
hiv.long6 <- hiv.long5 %>%
  mutate(myocard_infarct = as.numeric(myocard_infarct_typ1_dt<=int_start &
                                        !is.na(myocard_infarct_typ1_dt)),
         cong_heart_fail = as.numeric(cong_heart_fail_typ1_dt<=int_start &
                                        !is.na(cong_heart_fail_typ1_dt)),
         periph_vas_dis = as.numeric(periph_vas_dis_typ1_dt<=int_start &
                                       !is.na(periph_vas_dis_typ1_dt)),
         cereb_vas_dis = as.numeric(cereb_vas_dis_typ1_dt<=int_start &
                                      !is.na(cereb_vas_dis_typ1_dt)),
         dementia = as.numeric(dementia_typ1_dt<=int_start & !is.na(dementia_typ1_dt)),
         chron_pulm_dis = as.numeric(chron_pulm_dis_typ1_dt<=int_start &
                                       !is.na(chron_pulm_dis_typ1_dt)),
         rheumatic_dis = as.numeric(rheumatic_dis_typ1_dt<=int_start &
                                      !is.na(rheumatic_dis_typ1_dt)),
         peptic_ulcer = as.numeric(peptic_ulcer_typ1_dt<=int_start &
                                     !is.na(peptic_ulcer_typ1_dt)),
         mild_liv_dis = as.numeric(mild_liv_dis_typ1_dt<=int_start &
                                     !is.na(mild_liv_dis_typ1_dt)),
         mod_sevr_liver_dis = as.numeric(mod_sevr_liver_dis_typ1_dt<=int_start &
                                           !is.na(mod_sevr_liver_dis_typ1_dt)),
         diabt_wo_chron_complict = as.numeric(diabt_wo_chron_complict_typ1_dt<=int_start &
                                                !is.na(diabt_wo_chron_complict_typ1_dt)),
         diabt_chron_complict = as.numeric(diabt_chron_complict_da_typ1_dt<=int_start &
                                             !is.na(diabt_chron_complict_da_typ1_dt)),
         hemiplegia = as.numeric(hemiplegia_paraplegia_d_typ1_dt<=int_start &
                                   !is.na(hemiplegia_paraplegia_d_typ1_dt)),
         renal_dis = as.numeric(renal_dis_typ1_dt<=int_start &
                                  !is.na(renal_dis_typ1_dt)),
         metastatic = as.numeric(metastatic_typ1_dt<=int_start &
                                   !is.na(metastatic_typ1_dt)),
         any_malignancy = as.numeric(any_malignancy_typ1_dt<=int_start &
                                       !is.na(any_malignancy_typ1_dt))) %>%
  select(-c(last, date65, myocard_infarct_typ1_dt, cong_heart_fail_typ1_dt, periph_vas_dis_typ1_dt,
            cereb_vas_dis_typ1_dt, dementia_typ1_dt, chron_pulm_dis_typ1_dt,
            rheumatic_dis_typ1_dt, peptic_ulcer_typ1_dt, mild_liv_dis_typ1_dt,
            mod_sevr_liver_dis_typ1_dt, diabt_wo_chron_complict_typ1_dt,
            diabt_chron_complict_da_typ1_dt, hemiplegia_paraplegia_d_typ1_dt,
            renal_dis_typ1_dt, metastatic_typ1_dt, any_malignancy_typ1_dt))


# Output data -------------------------------------------------------------

write_csv(hiv.long6, "./care-cascade/data/care-cascade_data_sensitivity.csv")
