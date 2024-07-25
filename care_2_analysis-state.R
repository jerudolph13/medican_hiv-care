
################################################################################
#
# Project: HIV Care Cascade
#
# Purpose: Manage raw data (first eligible period)
#
# Author: Jacqueline Rudolph
#
# Last Update: 05 Sep 2023
#
################################################################################


packages <- c("dplyr", "tidyr", "magrittr", "readr", "lubridate", "splines", 
              "broom", "survival", "ggplot2", "parallel")
for (package in packages){
  library(package, character.only=T)
}

nboot <- 500

args <- commandArgs(trailingOnly=T)
mpr_limit <- as.numeric(args[1])


# Read in data ------------------------------------------------------------

if (mpr_limit>0) {
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
           enroll_period = factor(case_when(yr_base %in% 2001:2005 ~ 1,
                                            yr_base %in% 2006:2010 ~ 2,
                                            T ~ 3)),
           region = case_when(
             first_elig_state %in% c("MA", "NY", "PA") ~ "northeast",
             first_elig_state %in% c("IL", "OH") ~ "midwest",
             first_elig_state %in% c("AL", "GA", "MD", "NC", "FL", "TX") ~ "south",
             first_elig_state %in% c("CA", "CO", "WA") ~ "west"),
           state = case_when(
             (dod > int_start) & (dod<=int_end) ~ 5, # Dead
             int_end==end_date ~ 0,                  # Censored
             mpr>=mpr_limit & retained==1 ~ 1,       # Retained; ART adherent
             mpr<mpr_limit & retained==1 ~ 2,        # Retained; Not ART adherent
             mpr>=mpr_limit & retained==0 ~ 3,       # Not retained; ART adherent
             mpr<mpr_limit & retained==0 ~ 4))
  
} else {
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
           enroll_period = factor(case_when(yr_base %in% 2001:2005 ~ 1,
                                            yr_base %in% 2006:2010 ~ 2,
                                            T ~ 3)),
           region = case_when(
             first_elig_state %in% c("MA", "NY", "PA") ~ "northeast",
             first_elig_state %in% c("IL", "OH") ~ "midwest",
             first_elig_state %in% c("AL", "GA", "MD", "NC", "FL", "TX") ~ "south",
             first_elig_state %in% c("CA", "CO", "WA") ~ "west"),
           state = case_when(
             (dod > int_start) & (dod<=int_end) ~ 5, # Dead
             int_end==end_date ~ 0,                  # Censored
             mpr>mpr_limit & retained==1 ~ 1,        # Retained; ART adherent
             mpr<=mpr_limit & retained==1 ~ 2,       # Retained; Not ART adherent
             mpr>mpr_limit & retained==0 ~ 3,        # Not retained; ART adherent
             mpr<=mpr_limit & retained==0 ~ 4))
  
}

# Don't code deaths as drop outs
dat$drop[dat$state==5] <- 0


# Run analysis by region --------------------------------------------------

for (s in c("northeast", "midwest", "south", "west")) {
  
  new.dat <- filter(dat, region==s)
 
  
# Crude multistate model --------------------------------------------------
  
  surv <- survfit(Surv(start, end, as.factor(state)) ~ 1, 
                  data=new.dat, id=bene_id, conf.type="log-log")
  
  risk <- tidy(surv) %>% 
    filter(state!="(s0)") %>% 
    select(time, estimate, std.error, state)
  
  risk1 <- filter(risk, state==1) %>% 
    rename(est1 = estimate,
           se1 = std.error) %>% 
    select(-state)
  risk2 <- filter(risk, state==2) %>% 
    rename(est2 = estimate,
           se2 = std.error) %>% 
    select(-state)
  risk3 <- filter(risk, state==3) %>% 
    rename(est3 = estimate,
           se3 = std.error) %>% 
    select(-state)
  risk4 <- filter(risk, state==4) %>% 
    rename(est4 = estimate,
           se4 = std.error) %>% 
    select(-state)
  risk5 <- filter(risk, state==5) %>% 
    rename(est5 = estimate,
           se5 = std.error) %>% 
    select(-state)
  
  res <- left_join(risk1, risk2, by="time") %>% 
    left_join(risk3, by="time") %>% 
    left_join(risk4, by="time") %>% 
    left_join(risk5, by="time")
  
  write_csv(res, paste0("./care-cascade/results/mpr", mpr_limit, "/risk_crude_", s, ".csv"))
  
  
# Weighted multistate model -----------------------------------------------
  
  base <- filter(new.dat, !duplicated(bene_id))
  
  boot_rep <- function(r) {
    
    # Bootstrap resample  
    set.seed(r+1)
    samp <- table(base[sample(1:nrow(base), nrow(base), replace=T), (names(base)=="bene_id")])
    
    # The step below pulls in the real data for boot=0; 
    # otherwise grabs all records for the resampled observations
    boot <- NULL
    if(r==0){
      boot <- new.dat %>% 
        rename(bid = bene_id)
    } else{
      for(zzz in 1:max(samp)){ 
        cc <- new.dat[new.dat$bene_id %in% names(samp[samp %in% c(zzz:max(samp))]),]
        cc$bid <- paste0(cc$bene_id, zzz)
        boot <- rbind(boot, cc)
      }
      boot <- select(boot, -bene_id)
    }
    
    # Inverse probability of censoring weights
    mod_drop <- glm(I(drop==0) ~ bs(int, df=5) + bs(age_base, df=3) + race + sex + 
                      n_cond + enroll_period,
                    family=binomial(link="logit"), data=boot)$fitted.values
    den_drop <- ifelse(boot$drop==0, mod_drop, 1 - mod_drop)
    
    mod_drop <- glm(I(drop==0) ~ bs(int, df=5),
                    family=binomial(link="logit"), data=boot)$fitted.values
    num_drop <- ifelse(boot$drop==0, mod_drop, 0)
    
    boot$wt_drop <- num_drop/den_drop
    
    boot2 <- boot %>% 
      group_by(bid) %>% 
      mutate(cum_wt = cumprod(wt_drop)) %>% 
      filter(cum_wt!=0) %>% 
      ungroup()
    
    surv.wt <- survfit(Surv(start, end, as.factor(state)) ~ 1, 
                       data=boot2, id=bid, conf.type="log-log", weights=cum_wt)
    
    risk.wt <- tidy(surv.wt) %>% 
      filter(state!="(s0)") %>% 
      select(time, estimate, state)
    
    risk1.wt <- filter(risk.wt, state==1) %>% 
      rename(est1 = estimate) %>% 
      select(-state)
    risk2.wt <- filter(risk.wt, state==2) %>% 
      rename(est2 = estimate) %>% 
      select(-state)
    risk3.wt <- filter(risk.wt, state==3) %>% 
      rename(est3 = estimate) %>% 
      select(-state)
    risk4.wt <- filter(risk.wt, state==4) %>% 
      rename(est4 = estimate) %>% 
      select(-state)
    risk5.wt <- filter(risk.wt, state==5) %>% 
      rename(est5 = estimate) %>% 
      select(-state)
    
    res.wt <- left_join(risk1.wt, risk2.wt, by="time") %>% 
      left_join(risk3.wt, by="time") %>% 
      left_join(risk4.wt, by="time") %>% 
      left_join(risk5.wt, by="time") %>% 
      mutate(boot_rep = r)
    
    return(res.wt)
  }
  
  all.boot <- mclapply(0:nboot, function(tt) {boot_rep(tt)}, mc.cores=10, mc.set.seed=F)
  all.boot <- do.call(rbind, all.boot)
  
  # For point estimate, pull out results where boot=0
  est <- filter(all.boot, boot_rep==0)
  
  # Summarize over bootstraps
  boot.summ <- all.boot %>%
    group_by(time) %>%
    summarize(est1_ll = quantile(est1, p=0.025, na.rm=T),
              est1_ul = quantile(est1, p=0.975, na.rm=T),
              est2_ll = quantile(est2, p=0.025, na.rm=T),
              est2_ul = quantile(est2, p=0.975, na.rm=T),
              est3_ll = quantile(est3, p=0.025, na.rm=T),
              est3_ul = quantile(est3, p=0.975, na.rm=T),
              est4_ll = quantile(est4, p=0.025, na.rm=T),
              est4_ul = quantile(est4, p=0.975, na.rm=T),
              est5_ll = quantile(est5, p=0.025, na.rm=T),
              est5_ul = quantile(est5, p=0.975, na.rm=T))
  
  # Merge back to point estimates
  est <- left_join(est, boot.summ, by="time") %>%
    select(-boot_rep)
  
  write_csv(est, paste0("./care-cascade/results/mpr", mpr_limit, "/risk_wt_", s, ".csv"))

}