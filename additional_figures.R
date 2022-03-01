# edit to your filepath containing csv-files from "refine_FI_analyses.Rmd"
wd <- "" 
setwd(wd)


#Edit to name of csv-files (with .csv as suffix)
ADNI1_file = ""
ADNI2_and_ADNIGO_file = ""

library("gmodels")
library("car")
library("DescTools")
library("ggplot2")
library("qqplotr")
library("dplyr")
library(moments)
library(lsmeans)
library(msir)
library(corpcor)
library(ppcor)
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(e1071)
library(caret)
library(rpart)
library(MASS)
library(class)
library(ggplot2)
library(reshape2)
library(randomForest)
library(ROCR)
library(fmsb)
library(RColorBrewer)
library(foreach)
library(Hmisc)
library(ADNIMERGE)
library(lubridate)
library(msSurv)
library(zoo)
library(sjPlot)
library(stargazer)
library(compareGroups)
library(moonBook)
library(rrtable)
library(tables)
library(plyr)
library(ggsignif)
library(survminer)

#import development and validation sample data



dev_all <- read.csv(paste0(wd, ADNI1_file), header = TRUE)
val_all <- read.csv(paste0(wd, ADNI2_and_ADNIGO_file), header = TRUE)


#prepare data
dev_all$Sex <- factor(dev_all$Sex)
val_all$Sex <- factor(val_all$Sex)

dev_all <- dplyr::rename(dev_all, FIs = FI_expert)
dev_all <- dplyr::rename(dev_all, FIr = FI_refined_no_cov)
dev_all <- dplyr::rename(dev_all, FIc = FI_Canevelli)

val_all <- dplyr::rename(val_all, FIs = FI_expert)
val_all <- dplyr::rename(val_all, FIr = FI_refined_no_cov)
val_all <- dplyr::rename(val_all, FIc = FI_Canevelli)

## For survival analysis

#select MCI_subjects
dev_all_mci <- dev_all %>% filter(Dx == "MCI")

##rename Time and Conversion status variables for survival analysis 
names(dev_all_mci)[names(dev_all_mci) == 'Coxreg_update_time_conversion'] <- 'Time'
names(dev_all_mci)[names(dev_all_mci) == 'Coxreg_update_status'] <- 'Status'

val_all_mci <- val_all %>% filter(Dx == "MCI")
names(val_all_mci)[names(val_all_mci) == 'Coxreg_update_time_conversion'] <- 'Time'
names(val_all_mci)[names(val_all_mci) == 'Coxreg_update_status'] <- 'Status'

##create time variable in years

val_all_mci$Time_weeks <- val_all_mci$Time / 7 
dev_all_mci$Time_weeks <- dev_all_mci$Time / 7 

val_all_mci$Time_years <- val_all_mci$Time_weeks / 52 
dev_all_mci$Time_years <- dev_all_mci$Time_weeks / 52 

##create variable with MCI subjects with follow-up examinations only

attach(dev_all_mci)
dev_all_mci_time_1plus <- dev_all_mci[ which(Time >= 1),]
detach(dev_all_mci)

attach(val_all_mci)
val_all_mci_time_1plus <- val_all_mci[ which(Time >= 1),]
detach(val_all_mci)


#FIG. 2A: Density plots showing three frailty index (FI) distributions for ADNI1 (development) and ADNI2/GO (validation) cohorts. FIs = A 93-item FI created according to standard procedure by the authors. FIr = A 26-item FI created by adding a data-driven supplement to the standard procedure. FIc = A 40-item FI created according to standard procedure by Canevelli, et al., (22)

#density plot for development

dev_all %>% dplyr::select(one_of("FIs","FIr", "FIc")) %>% 
  melt() %>% 
  ggplot(aes(value, color = variable, group = variable, fill = variable)) +
  coord_fixed(0.04) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(name = "FI",
                     breaks = seq(0, 0.7, 0.2),
                     limits=c(0, 0.6)) +
  theme_classic(base_size = 30, base_family = "Arial") +
  theme(legend.position="none") 


#density plot for validation

val_all %>% dplyr::select(one_of("FIs","FIr", "FIc")) %>% 
  melt() %>% 
  ggplot(aes(value, color = variable, group = variable, fill = variable)) +
  coord_fixed(0.04) +
  geom_density(alpha = 0.4) +
  scale_x_continuous(name = "FI",
                     breaks = seq(0, 0.7, 0.2),
                     limits=c(0, 0.6)) +
  theme_classic(base_size = 30, base_family = "Arial") +
  theme(legend.position="none") 

#FIG. 2B: Boxplots illustrating central tendency and variability of the three different FI-variables for cognitively normal (healthy) controls (HC), and subjects living with mild cognitive impairment (MCI) or Alzheimer’s disease dementia (AD). P-values are from Wilcoxon rank sum tests comparing diagnostic group differences in FI-scores.


##box-plots of FIs, FIr and FIc in dev

ggplot(dev_all, aes(x=reorder(Dx,FIr,na.rm=TRUE), y=FIr, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIr",
       x="Diagnostic group (Dx)")

ggplot(dev_all, aes(x=reorder(Dx,FIs,na.rm=TRUE), y=FIs, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIs",
       x="Diagnostic group (Dx)")

ggplot(dev_all, aes(x=reorder(Dx,FIc,na.rm=TRUE), y=FIc, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIc",
       x="Diagnostic group (Dx)")

##box-plots of FIs, FIr, and FIc in val

ggplot(val_all, aes(x=reorder(Dx,FIr,na.rm=TRUE), y=FIr, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIr",
       x="Diagnostic group (Dx)")

ggplot(val_all, aes(x=reorder(Dx,FIs,na.rm=TRUE), y=FIs, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIs",
       x="Diagnostic group (Dx)")

ggplot(val_all, aes(x=reorder(Dx,FIc,na.rm=TRUE), y=FIc, fill=Dx))+
  coord_fixed(6)+
  geom_boxplot(width=0.7,notch = TRUE, outlier.shape=NA, size=0.75)+
  geom_signif(comparisons = list(c("HC","MCI")), map_signif_level = FALSE,
              y_position=0.5, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("MCI","AD")), map_signif_level = FALSE,
              y_position=0.63, size=0.75, textsize=4.5)+
  geom_signif(comparisons = list(c("HC","AD")), map_signif_level = FALSE,
              y_position = 0.7, size=0.75, textsize=4.5)+
  #ylim(c(0,0.70))+ ##this prevents stars from getting cut off in the multiplot
  scale_y_continuous(limits=c(0,0.78), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme_classic(base_size = 30, base_family = "Arial")+
  #theme(legend.position="bottom")+
  scale_fill_brewer(palette="Accent")+
  labs(y="FIc",
       x="Diagnostic group (Dx)")

# Fig 2C: show Kaplan-Meier survival curves for sample quartiles calculated for each FI. The survival probabilities indicate the probability of remaining stable MCI at time of follow-up, and vertical lines through each line indicate censoring. 

##quartiles

#prepare

dev_all_mci_time_1plus$FIs_quartile <- dplyr::ntile(dev_all_mci_time_1plus$FIs, 4)
dev_all_mci_time_1plus$FIr_quartile <- dplyr::ntile(dev_all_mci_time_1plus$FIr, 4)
dev_all_mci_time_1plus$FIc_quartile <- dplyr::ntile(dev_all_mci_time_1plus$FIc, 4)

val_all_mci_time_1plus$FIs_quartile <- dplyr::ntile(val_all_mci_time_1plus$FIs, 4)
val_all_mci_time_1plus$FIr_quartile <- dplyr::ntile(val_all_mci_time_1plus$FIr, 4)
val_all_mci_time_1plus$FIc_quartile <- dplyr::ntile(val_all_mci_time_1plus$FIc, 4)

#fit

dev_all_mci_FIs_quart.km.fit<-survfit(Surv(Time, Status) ~ FIs_quartile, data=dev_all_mci_time_1plus)
dev_all_mci_FIr_quart.km.fit<-survfit(Surv(Time, Status) ~ FIr_quartile, data=dev_all_mci_time_1plus)
dev_all_mci_FIc_quart.km.fit<-survfit(Surv(Time, Status) ~ FIc_quartile, data=dev_all_mci_time_1plus)


val_all_mci_FIs_quart.km.fit<-survfit(Surv(Time, Status) ~ FIs_quartile, data=val_all_mci_time_1plus)
val_all_mci_FIr_quart.km.fit<-survfit(Surv(Time, Status) ~ FIr_quartile, data=val_all_mci_time_1plus)
val_all_mci_FIc_quart.km.fit<-survfit(Surv(Time, Status) ~ FIc_quartile, data=val_all_mci_time_1plus)


#dev

#FIs

dev_all_mci_FIs_quart.km.surv <- ggsurvplot(dev_all_mci_FIs_quart.km.fit, data=dev_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
)

dev_all_mci_FIs_quart.km.surv$plot <- dev_all_mci_FIs_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

dev_all_mci_FIs_quart.km.surv

#FIr

dev_all_mci_FIr_quart.km.surv <- ggsurvplot(dev_all_mci_FIr_quart.km.fit, data=dev_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            #legend.labs=c("Q1", "Q2", "Q3", "Q4"), 
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
) 

dev_all_mci_FIr_quart.km.surv$plot <- dev_all_mci_FIr_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

dev_all_mci_FIr_quart.km.surv

#FIc

dev_all_mci_FIc_quart.km.surv <- ggsurvplot(dev_all_mci_FIc_quart.km.fit, data=dev_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
)


dev_all_mci_FIc_quart.km.surv$plot <- dev_all_mci_FIc_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

dev_all_mci_FIc_quart.km.surv

##validation


#FIs

val_all_mci_FIs_quart.km.surv <- ggsurvplot(val_all_mci_FIs_quart.km.fit, data=val_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
)

val_all_mci_FIs_quart.km.surv$plot <- val_all_mci_FIs_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

val_all_mci_FIs_quart.km.surv

#FIr

val_all_mci_FIr_quart.km.surv <- ggsurvplot(val_all_mci_FIr_quart.km.fit, data=val_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
) 

val_all_mci_FIr_quart.km.surv$plot <- val_all_mci_FIr_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

val_all_mci_FIr_quart.km.surv

#FIc

val_all_mci_FIc_quart.km.surv <- ggsurvplot(val_all_mci_FIc_quart.km.fit, data=val_all_mci_time_1plus,
                                            conf.int = F,
                                            cumevents=FALSE,
                                            censor = TRUE, 
                                            xlim = c(-25, 1825),
                                            break.time.by = 365,
                                            size = 1.2,
                                            legend="none",
                                            xlab="", 
                                            ylab="",
                                            #ylim= c(-0.05,1),
                                            surv.scale="percent",
                                            pval= T,
                                            font.y= c(12, "plain"),
                                            font.x = c(12, "plain"),
                                            pval.size = 6,
                                            pval.method = F,
                                            pval.coord = c(-25, 0.38),
                                            ggtheme = theme_classic(base_size = 30, base_family = "Arial") + theme(aspect.ratio = 1),
                                            tables.theme = theme(aspect.ratio = 0.20),
                                            risk.table = T,
                                            risk.table.pos = "in",
                                            axes.offset = T
)

val_all_mci_FIc_quart.km.surv$plot <- val_all_mci_FIc_quart.km.surv$plot  + scale_x_continuous(breaks = c(0,365, 730, 1095, 1460,1825), labels = c(0,1, 2, 3, 4, 5)) 

val_all_mci_FIc_quart.km.surv


# Fig. 2D: Estimated AUC(t) for prediction of AD conversion in subjects with MCI at baseline plotted over five years of follow-up for three rival continuous FI variables in development and validation cohorts. 

## Development

ROC_dev_FIs_years<-timeROC(T=dev_all_mci_time_1plus$Time_years,
                           delta=dev_all_mci_time_1plus$Status, marker=dev_all_mci_time_1plus$FIs,
                           cause=1, weighting="marginal",
                           #times=c(12,24,48), #months
                           #times=c(365, 730, 1460), #days
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)


ROC_dev_FIr_years<-timeROC(T=dev_all_mci_time_1plus$Time_years,
                           delta=dev_all_mci_time_1plus$Status, marker=dev_all_mci_time_1plus$FIr,
                           cause=1, weighting="marginal",
                           #times=c(12,24,48), #months
                           #times=c(365, 730, 1460), #days
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)


ROC_dev_FIc_years<-timeROC(T=dev_all_mci_time_1plus$Time_years,
                           delta=dev_all_mci_time_1plus$Status, marker=dev_all_mci_time_1plus$FIc,
                           cause=1, weighting="marginal",
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)

plotAUCcurve(ROC_dev_FIs_years, conf.int = FALSE, conf.band = FALSE, col="red4") 
plotAUCcurve(ROC_dev_FIr_years, conf.int = FALSE, conf.band = FALSE, col="orange", add=TRUE)
plotAUCcurve(ROC_dev_FIc_years, conf.int = FALSE, conf.band = FALSE, col="purple", add=TRUE)
legend("topright",c("FIs","FIr", "FIc"),col=c("red4","orange","purple"),lty=1,lwd=3)


##Validation

ROC_val_FIs_years<-timeROC(T=val_all_mci_time_1plus$Time_years,
                           delta=val_all_mci_time_1plus$Status, marker=val_all_mci_time_1plus$FIs,
                           cause=1, weighting="marginal",
                           #times=c(12,24,48), #months
                           #times=c(365, 730, 1460), #days
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)


ROC_val_FIr_years<-timeROC(T=val_all_mci_time_1plus$Time_years,
                           delta=val_all_mci_time_1plus$Status, marker=val_all_mci_time_1plus$FIr,
                           cause=1, weighting="marginal",
                           #times=c(12,24,48), #months
                           #times=c(365, 730, 1460), #days
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)


ROC_val_FIc_years<-timeROC(T=val_all_mci_time_1plus$Time_years,
                           delta=val_all_mci_time_1plus$Status, marker=val_all_mci_time_1plus$FIc,
                           cause=1, weighting="marginal",
                           times=c(1, 2, 3, 4, 5), #days
                           iid=TRUE)

plotAUCcurve(ROC_val_FIs_years, conf.int = FALSE, conf.band = FALSE, col="red4") 
plotAUCcurve(ROC_val_FIr_years, conf.int = FALSE, conf.band = FALSE, col="orange", add=TRUE)
plotAUCcurve(ROC_val_FIc_years, conf.int = FALSE, conf.band = FALSE, col="purple", add=TRUE)
legend("topright",c("FIs","FIr", "FIc"),col=c("red4","orange","purple"),lty=1,lwd=3)


##eFigure2

##scatterplots

##development

FIs_agecor<-cor(dev_all$FIs,dev_all$Age, method = c("pearson"))
FIs_agescatter <- ggplot(dev_all, aes(x=Age, y=FIs, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIs_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

FIr_agecor<-cor(dev_all$FIr,dev_all$Age)
FIr_agescatter <- ggplot(dev_all, aes(x=Age, y=FIr, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIr_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

FIr_agescatter

FIc_agecor <-cor(dev_all$FIc,dev_all$Age)
FIc_agescatter <- ggplot(dev_all, aes(x=Age, y=FIc, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIc_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

#validation age vs fi scatterplot

FIs_agecor<-cor(val_all$FIs,val_all$Age)
FIs_agescatter <- ggplot(val_all, aes(x=Age, y=FIs, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIs_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

FIr_agecor<-cor(val_all$FIr,val_all$Age)
FIr_agescatter <- ggplot(val_all, aes(x=Age, y=FIr, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIr_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

FIr_agescatter

FIc_agecor <-cor(val_all$FIc,val_all$Age)
FIc_agescatter <- ggplot(val_all, aes(x=Age, y=FIc, color=Dx))+
  geom_point(size = 1)+
  coord_fixed(90)+
  #geom_smooth(color = "black", method="loess")+ #one line per group
  geom_smooth(color = "black", size = 1.5, method="loess")+ #one STRAIGHT line for ALL
  theme(legend.position="none")+
  theme_classic(base_size = 30, base_family = "Arial")+
  scale_y_continuous(limits=c(0,0.6), breaks=seq(0,0.6,0.2), expand = c(0, 0)) +
  annotate("text", size = 6, x=62, y = 0.50, label = paste0("r = ", round(FIc_agecor,3)))+
  scale_color_brewer(palette="Accent")+
  labs(y="FI-value")

#eFigure 6: AUC(t)-curves of FI performance in prediction of future dementia conversion in subjects with mild cognitive impairment at baseline 

##Development

plotAUCcurve(ROC_dev_FIs_years, conf.int = FALSE, conf.band = TRUE, col="red4")
legend("topright",c("FIs"),col=c("red4"),lty=1,lwd=3)

plotAUCcurve(ROC_dev_FIr_years, conf.int = FALSE, conf.band = TRUE, col="orange")
legend("topright",c("FIr"),col=c("orange"),lty=1,lwd=3)

plotAUCcurve(ROC_dev_FIc_years, conf.int = FALSE, conf.band = TRUE, col="purple")
legend("topright",c("FIc"),col=c("purple"),lty=1,lwd=3)


##validation

plotAUCcurve(ROC_val_FIs_years, conf.int = FALSE, conf.band = TRUE, col="red4")
legend("topright",c("FIs"),col=c("red4"),lty=1,lwd=3)

plotAUCcurve(ROC_val_FIr_years, conf.int = TRUE, conf.band = TRUE, col="orange")
legend("topright",c("FIr"),col=c("orange"),lty=1,lwd=3)

plotAUCcurve(ROC_val_FIc_years, conf.int = FALSE, conf.band = TRUE, col="purple")
legend("topright",c("FIc"),col=c("purple"),lty=1,lwd=3)


#eFigure 7 Differences in FI AUC(t) curves (TimeROC Fig 2D needs to be computed first)

##Development 

plotAUCcurveDiff(ROC_dev_FIr_years, ROC_dev_FIs_years, add = FALSE, conf.int = TRUE,
                 conf.band = TRUE, col = "red", ylim = c(-0.1, 0.2))

plotAUCcurveDiff(ROC_dev_FIr_years, ROC_dev_FIc_years, add = FALSE, conf.int = TRUE,
                 conf.band = TRUE, col = "blue", ylim = c(-0.1, 0.2))

plotAUCcurveDiff(ROC_dev_FIs_years, ROC_dev_FIc_years, add = FALSE, conf.int = TRUE,
                 conf.band = FALSE, col = "blue", ylim = c(-0.1, 0.2))

##validation

plotAUCcurveDiff(ROC_val_FIr_years, ROC_val_FIs_years, add = FALSE, conf.int = TRUE,
                 conf.band = TRUE, col = "red", ylim = c(-0.1, 0.2))

plotAUCcurveDiff(ROC_val_FIr_years, ROC_val_FIc_years, add = FALSE, conf.int = TRUE,
                 conf.band = TRUE, col = "blue", ylim = c(-0.1, 0.2))

plotAUCcurveDiff(ROC_val_FIs_years, ROC_val_FIc_years, add = FALSE, conf.int = TRUE,
                 conf.band = TRUE, col = "blue", ylim = c(-0.1, 0.2))

