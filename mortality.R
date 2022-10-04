# edit to your filepath containing csv-files from "refine_FI_analyses.Rmd"
wd <- "" 
setwd(wd)

library(dplyr)
library(survival)
library(ggplot2)
library(ggsignif)
library(survminer)
library(coxphw)
library(lubridate)
library(ADNIMERGE)

##read the ADNI dataset with the FI-variables, the recent adverse events log and the 
# registry file.

#Edit to name of csv-file with the computed FI variables (with .csv as suffix)
csv_file = ""
dev_val_combine <- readr::read_csv(csv_file)
recadv_tmp <- ADNIMERGE::recadv
registry <- ADNIMERGE::registry

stopif <- function(x) stopifnot(!x)

# https://adni.loni.usc.edu/wp-content/uploads/2012/08/instruction-about-data.pdf
# From DATADIC.CSV:
# AEHONSDT - Onset Date
# AEHDTHDT - Date of death
# AEHADMDT - If hospitalized, provide Admission Date
# EXAMDATE - Examination Date

df_baseline_dates <- registry %>%
  filter(VISCODE == "bl") %>%
  dplyr::rename(baseline_date = EXAMDATE) %>%
  select(RID, baseline_date)
stopif(any(duplicated(df_baseline_dates$RID)))

df_death_dates <- recadv_tmp %>%
  filter(!is.na(AEHDTHDT)) %>%
  group_by(RID) %>%
  dplyr::summarize(death_date = first(AEHDTHDT))

df_days_to_death <- recadv_tmp %>%
  semi_join(df_baseline_dates, by = "RID") %>%
  left_join(df_baseline_dates, by = "RID") %>%
  mutate(days_to_death = difftime(AEHDTHDT, baseline_date, units = "days")) %>%
  select(RID, days_to_death) %>%
  group_by(RID) %>%
  dplyr::summarize(days_to_death = first(na.omit(as.double(days_to_death))))

df_hosp_dates <- recadv_tmp %>%
  filter(hospitalized == 1) %>%
  filter(!is.na(AEHADMDT)) %>%
  group_by(RID) %>%
  arrange(AEHADMDT) %>%
  dplyr::summarize(hospitalization_date = first(AEHADMDT))

df_days_to_hospitalization <- recadv_tmp %>%
  semi_join(df_baseline_dates, by = "RID") %>%
  left_join(df_baseline_dates, by = "RID") %>%
  filter(hospitalized == 1) %>%
  mutate(days_to_hospitalization = AEHADMDT - baseline_date) %>%
  select(RID, days_to_hospitalization) %>%
  group_by(RID) %>%
  dplyr::summarize(days_to_hospitalization = first(na.omit(as.double(days_to_hospitalization))))

df_last_event_dates <- registry %>%
  filter(!is.na(EXAMDATE)) %>%
  group_by(RID) %>%
  arrange(EXAMDATE) %>%
  dplyr::summarize(last_event_date = max(EXAMDATE))

df_days_to_last_event <- df_last_event_dates %>%
  left_join(df_baseline_dates, by = "RID") %>%
  mutate(days_to_last_event = as.integer(difftime(last_event_date, baseline_date, units="days"))) %>%
  select(RID, days_to_last_event)
stopif(any(duplicated(df_days_to_last_event$RID)))

min_na <- function(a, b) {
  if(is.na(a)) { b }
  else if(is.na(b)) { a }
  else { min(a,b) }
}

df_hosp_death_and_last_event <- registry %>%
  select(RID) %>%
  distinct(RID) %>%
  left_join(df_days_to_hospitalization, by = "RID") %>%
  left_join(df_days_to_death, by = "RID") %>%
  left_join(df_days_to_last_event, by = "RID") %>%
  left_join(df_baseline_dates, by = "RID") %>%
  left_join(df_hosp_dates, by = "RID") %>%
  left_join(df_death_dates, by = "RID") %>%
  left_join(df_last_event_dates, by = "RID") %>%
  dplyr::rowwise() %>%
  dplyr::mutate(days_to_hospitalization_or_death = min_na(days_to_hospitalization, days_to_death)) %>%
  select(days_to_hospitalization_or_death, days_to_death, days_to_hospitalization_or_death, everything()) %>%
  dplyr::mutate(days_to_hospitalization_or_death = ifelse(days_to_hospitalization_or_death < 0, 0, days_to_hospitalization_or_death)) %>%
  dplyr::mutate(days_to_hospitalization = ifelse(days_to_hospitalization < 0, 0, days_to_hospitalization)) %>%
  dplyr::mutate(death = as.numeric(!is.na(days_to_death))) %>%
  dplyr::mutate(days_to_death_censored = ifelse(death == 1, days_to_death, days_to_last_event)) %>%
  dplyr::mutate(hospitalization_or_death = as.numeric(!is.na(days_to_hospitalization_or_death))) %>%
  dplyr::mutate(days_to_hospitalization_or_death_censored = ifelse(!is.na(days_to_hospitalization_or_death), days_to_hospitalization_or_death, days_to_last_event)) %>%
  dplyr::mutate(hospitalization = as.numeric(!is.na(days_to_hospitalization))) %>%
  dplyr::mutate(days_to_hospitalization_censored = ifelse(!is.na(days_to_hospitalization), days_to_hospitalization, days_to_last_event))

stopifnot(all(df_hosp_death_and_last_event$days_to_hospitalization >= 0, na.rm = T))
stopifnot(all(df_hosp_death_and_last_event$days_to_hospitalization_or_death >= 0, na.rm = T))

recadv_hosp_and_death <- recadv_tmp %>%
  left_join(df_hosp_death_and_last_event, by = "RID")

dev_val_combine_full <- dev_val_combine %>%
  left_join(df_hosp_death_and_last_event, by = "RID")

View(dev_val_combine_full %>%
       select(hospitalization,
              days_to_last_event,
              days_to_hospitalization,
              days_to_hospitalization_censored))

sum(dev_val_combine_full$hospitalization)

saveRDS(dev_val_combine_full, "dev_val_combine_full.rds")
saveRDS(recadv_hosp_and_death, "recadv_hosp_and_death.rds")

#import development and validation sample mortality and hospitalization data. 
##Note: hospitalization data was not used in ms due to low number of registered events in validation. 

death <- readRDS("~/dev_val_combine_full.rds")

death <- death %>%
  mutate(FIc_100 = FIc * 100) %>%
  mutate(FIr_100 = FIr * 100) %>%
  mutate(FIs_100 = FIs * 100)

names(death)[names(death) == 'days_to_death_censored'] <- 'Time_death'
names(death)[names(death) == 'death'] <- 'Status_death'

#prepare data
death$Sex <- factor(death$Sex)
death$Sex <- factor(death$Sex)

##create time variables in weeks and years

death$Time_death_weeks <- death$Time_death / 7
death$Time_death_years <- death$Time_death_weeks / 52

#remove subjects without follow-up data for death 
death <- death %>% filter(Time_death > "0")

#split into development and validation samples
dev_death <- death %>% filter(Sample == "Development")

val_death <- death %>% filter(Sample == "Validation")

#Report number of deaths and median follow-up time

table(dev_death$Status_death, dev_death$Dx)
table(val_death$Status_death, val_death$Dx)

median(dev_death$Time_death)
median(val_death$Time_death)

## Kaplan Meier survival curves shown in Supplementary fig. 4

#prepare

dev_death$FIs_tertile_dev <- dplyr::ntile(dev_death$FIs, 3)
dev_death$FIr_tertile_dev <- dplyr::ntile(dev_death$FIr, 3)
dev_death$FIc_tertile_dev <- dplyr::ntile(dev_death$FIc, 3)

val_death$FIs_tertile_val <- dplyr::ntile(val_death$FIs, 3)
val_death$FIr_tertile_val <- dplyr::ntile(val_death$FIr, 3)
val_death$FIc_tertile_val <- dplyr::ntile(val_death$FIc, 3)

#fit

dev_death_FIs_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIs_tertile_dev, data=dev_death)
dev_death_FIr_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIr_tertile_dev, data=dev_death)
dev_death_FIc_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIc_tertile_dev, data=dev_death)

val_death_FIs_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIs_tertile_val, data=val_death)
val_death_FIr_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIr_tertile_val, data=val_death)
val_death_FIc_tert.km.fit<-survfit(Surv(Time_death, Status_death) ~ FIc_tertile_val, data=val_death)

survplotlist <- list()
survplotlist[[1]] <- ggsurvplot(dev_death_FIs_tert.km.fit, data=dev_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIs tertile (ADNI1)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))
survplotlist[[3]] <- ggsurvplot(dev_death_FIr_tert.km.fit, data=dev_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIr tertile (ADNI1)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))
survplotlist[[5]] <- ggsurvplot(dev_death_FIc_tert.km.fit, data=dev_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIc tertile (ADNI1)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))
survplotlist[[2]] <- ggsurvplot(val_death_FIs_tert.km.fit, data=val_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIs tertile (ADNI2+GO)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))
survplotlist[[4]] <- ggsurvplot(val_death_FIr_tert.km.fit, data=val_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIr tertile (ADNI2+GO)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))
survplotlist[[6]] <- ggsurvplot(val_death_FIc_tert.km.fit, data=val_death, pval= T, xlim = c(0, 4000), legend = c(0.75, 0.22), legend.title = "FIc tertile (ADNI2+GO)", legend.labs=c("1st tertile", "2nd tertile", "3rd tertile"))

arrange_ggsurvplots(
  survplotlist,
  print = TRUE,
  title = NA,
  ncol = 3,
  nrow = 2,
  surv.plot.height = NULL,
  risk.table.height = NULL,
  ncensor.plot.height = NULL
)           


## Cox regressions 

#Death

## Cox model 0 Crude mortality FIr hazard ratio across all participants

cox_death_all <- coxph(Surv(Time_death, Status_death) ~ FIr_100, data=death)
cox_death_all_sum=summary(cox_death_all)
cox_death_all_sum
cox.zph(cox_death_all)


## Weighted cox model 1: Age, Sex and Education as covariates reported in Supplementary table 5, with average hazard ratio estimates due to non-proportional hazards

#FIs

cox_dev_death_FIs_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIs_100 + Age + Sex + PTEDUCAT, data=dev_death)
cox_dev_death_sum_FIs_ahr=summary(cox_dev_death_FIs_ahr)
cox_dev_death_sum_FIs_ahr

cox_val_death_FIs_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIs_100 + Age + Sex + PTEDUCAT, data=val_death)
cox_val_death_sum_FIs_ahr=summary(cox_val_death_FIs_ahr)
cox_val_death_sum_FIs_ahr


#FIr

cox_dev_death_FIr_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIr_100 + Age + Sex + PTEDUCAT, data=dev_death)
cox_dev_death_sum_FIr_ahr=summary(cox_dev_death_FIr_ahr)
cox_dev_death_sum_FIr_ahr

cox_val_death_FIr_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIr_100 + Age + Sex + PTEDUCAT, data=val_death)
cox_val_death_sum_FIr_ahr=summary(cox_val_death_FIr_ahr)
cox_val_death_sum_FIr_ahr


#FIc

cox_dev_death_FIc_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIc_100 + Age + Sex + PTEDUCAT, data=dev_death)
cox_dev_death_sum_FIc_ahr=summary(cox_dev_death_FIc_ahr)
cox_dev_death_sum_FIc_ahr

cox_val_death_FIc_ahr <- coxphw(Surv(Time_death, Status_death) ~ FIc_100 + Age + Sex + PTEDUCAT, data=val_death)
cox_val_death_sum_FIc_ahr=summary(cox_val_death_FIc_ahr)
cox_val_death_sum_FIc_ahr


## Weighted cox model 2: Age, Sex, Education, and MMSE as covariates reported in Supplementary table 5, with average hazard ratio estimates due to non-proportional hazards

#FIs

cox_dev_death_FIs_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIs_100 + Age + Sex + PTEDUCAT + MMSE, data=dev_death)
cox_dev_death_sum_FIs_ahr_mmse=summary(cox_dev_death_FIs_ahr_mmse)
cox_dev_death_sum_FIs_ahr_mmse

cox_val_death_FIs_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIs_100 + Age + Sex + PTEDUCAT + MMSE, data=val_death)
cox_val_death_sum_FIs_ahr_mmse=summary(cox_val_death_FIs_ahr_mmse)
cox_val_death_sum_FIs_ahr_mmse


#FIr

cox_dev_death_FIr_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIr_100 + Age + Sex + PTEDUCAT + MMSE, data=dev_death)
cox_dev_death_sum_FIr_ahr_mmse=summary(cox_dev_death_FIr_ahr_mmse)
cox_dev_death_sum_FIr_ahr_mmse

cox_val_death_FIr_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIr_100 + Age + Sex + PTEDUCAT + MMSE, data=val_death)
cox_val_death_sum_FIr_ahr_mmse=summary(cox_val_death_FIr_ahr_mmse)
cox_val_death_sum_FIr_ahr_mmse


#FIc

cox_dev_death_FIc_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIc_100 + Age + Sex + PTEDUCAT + PTEDUCAT + MMSE, data=dev_death)
cox_dev_death_sum_FIc_ahr_mmse=summary(cox_dev_death_FIc_ahr_mmse)
cox_dev_death_sum_FIc_ahr_mmse

cox_val_death_FIc_ahr_mmse <- coxphw(Surv(Time_death, Status_death) ~ FIc_100 + Age + Sex + PTEDUCAT + MMSE, data=val_death)
cox_val_death_sum_FIc_ahr_mmse=summary(cox_val_death_FIc_ahr_mmse)
cox_val_death_sum_FIc_ahr_mmse
