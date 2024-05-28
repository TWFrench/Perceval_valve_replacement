################################################################################
#Outcomes of patients undergoing Aortic Valve Replacement
#Thomas French 
################################################################################
library(dplyr)
library(ggplot2)
library(arsenal)
library(MatchIt)
library(CIplot)
library(survival)
library(ggplot2)
library(ggsurvfit)
library(readxl)
library(dplyr)

set.seed(135)
#################################################################################
#filtering 
Tissue_AVR_data_2017_2022 <- read_excel("Tissue_AVR_data_2017_2022.xlsx")

TAP <- Tissue_AVR_data_2017_2022

TAP <- subset(TAP, TAP$`Cardiac Procedure` != 'CABG + valve' & 
                TAP$`Cardiac Procedure` != 'CABG + valve + other')
TAP <- subset(TAP, TAP$`Valve Pos 1` != 'Mitral' & 
                TAP$`Valve Pos 2` != 'Mitral')
TAP <- subset(TAP, TAP$Age >= 50 & TAP$Age <= 85)
TAP$ValveType <- ifelse(TAP$`Valve Manufacturer 1`== 'Sorin Perceval S', 1, 0)

#################################################################################
#computing time to event 
TAP$censor_date <- as.Date("2023-03-30")
TAP$Event <- ifelse(is.na(TAP$DOD), 0, 1)
TAP$TTE_dead <- TAP$DOD - TAP$`Surgery Start Date`
TAP$TTE_dead <- TAP$TTE_dead/86400
TAP$TTE_dead <- as.numeric(TAP$TTE_dead)
TAP$TTE_dead[is.na(TAP$TTE_dead)] <- 0 
TAP$censor_date <- as.POSIXct(TAP$censor_date)
TAP$TTE_alive <- TAP$censor_date - TAP$`Surgery Start Date`
TAP$TTE_alive <- ifelse(is.na(TAP$DOD), TAP$TTE_alive, 0)
TAP$TTE <- TAP$TTE_alive + TAP$TTE_dead
hist(TAP$TTE)
################################################################################
#Section 1 - Dummy coding 

TAP$AnginaStatus <- TAP$`Angina Status`
TAP$AnginaStatus[TAP$AnginaStatus == 'Not recorded'] <- NA
TAP$CCS_1 <- ifelse(TAP$AnginaStatus == "CCS1 - Only on strenuous exertion", 1, 0)
TAP$CCS_2 <- ifelse(TAP$AnginaStatus == "CCS2 - On moderate exertion (e.g. climbing stairs rapidly)", 1, 0)
TAP$CCS_3 <- ifelse(TAP$AnginaStatus== "CCS3 - On mild exertion (e.g. walking1-2 blocks at normal pace)", 1, 0)
TAP$CCS_4 <- ifelse(TAP$AnginaStatus == "CCS4 - On any activity or at rest", 1, 0)
TAP$CCS_1 <- as.factor(TAP$CCS_1)
TAP$CCS_2 <- as.factor(TAP$CCS_2)
TAP$CCS_3 <- as.factor(TAP$CCS_3)
TAP$CCS_4 <- as.factor(TAP$CCS_4)
TAP$AnginaStatus <- as.factor(TAP$AnginaStatus)

TAP$DyspnoeaStatus <- TAP$`Dyspnoea Status`
TAP$DyspnoeaStatus[TAP$DyspnoeaStatus == 'Not Recorded'] <- 0
TAP$NYHA_II <- ifelse(TAP$DyspnoeaStatus =='NYHA I - None or only on strenuous exertion', 1, 0)
TAP$NYHA_III <- ifelse(TAP$DyspnoeaStatus =='NYHA III - On mild exertion (e.g. walking 1-2 blocks at normal pace)', 1, 0)
TAP$NYHA_IV <- ifelse(TAP$DyspnoeaStatus =='NYHA IV - On any activity or at rest', 1, 0)
TAP$NYHA_II <- as.factor(TAP$NYHA_II)
TAP$NYHA_III <- as.factor(TAP$NYHA_III)
TAP$NYHA_IV <- as.factor(TAP$NYHA_IV)

TAP$LVEF[TAP$LVEF=='Not Recorded' | TAP$LVEF=='NULL']<- NA
TAP$LVEF[TAP$LVEF=='Fair (LVEF 31-50%)'] <- 'Moderate'
TAP$LVEF[TAP$LVEF=='Moderate (LVEF 31 - 50%)'] <- 'Moderate'
TAP$LVEF[TAP$LVEF=='Poor (LVEF 21-30%)'] <- 'Poor'
TAP$LVEF[TAP$LVEF=='Poor (LVEF 21 - 30%)'] <- 'Poor'
TAP$LVEF[TAP$LVEF=='Very Poor (LVEF < 21%)'] <- 'Very poor'
TAP$LVEF[TAP$LVEF=='Very Poor LVEF <21%'] <- 'Very poor'
table(TAP$LVEF)
TAP$moderate_LVEF <- ifelse(TAP$LVEF == 'Moderate', 1, 0)
TAP$poor_LVEF <- ifelse(TAP$LVEF =='Poor', 1, 0)
TAP$very_poor_LVEF <- ifelse(TAP$LVEF=='Very poor', 1, 0)
TAP$LVEF <- as.factor(TAP$LVEF)

TAP$Smoking<- as.factor(TAP$Smoking)
TAP$ex_smoker <- ifelse(TAP$Smoking == 'Ex smoker', 1, 0)
TAP$Smoker <- ifelse(TAP$Smoking == 'Current smoker', 1, 0)

TAP$Diabetes <- as.factor(TAP$Diabetes)
TAP$Diet <- ifelse(TAP$Diabetes =='Diet', 1, 0)
TAP$oral_therapy <- ifelse(TAP$Diabetes == 'Oral therapy', 1, 0)
TAP$insulin <- ifelse(TAP$Diabetes == 'Insulin', 1, 0)

TAP$PreOp_Heart_Rhy<-TAP$`Pre-Op_Heart_Rhy`
TAP$PreOp_Heart_Rhy[TAP$PreOp_Heart_Rhy=='NULL' | TAP$PreOp_Heart_Rhy == 'Not Recorded'] <- NA
TAP$PreOp_Heart_Rhy <- as.factor(TAP$PreOp_Heart_Rhy)


TAP$Hypertension[TAP$Hypertension=='Not Recorded'] <- 0
TAP$Hypertension <- ifelse(TAP$Hypertension=='No hypertension', 0, 1)
TAP$Hypertension <- as.factor(TAP$Hypertension)

TAP$LastMI <- TAP$`Last MI`
TAP$LastMI[TAP$LastMI=='NULL'] <- NA
TAP$previous_MI <- ifelse(TAP$LastMI =='No previous myocardial infarction', 0, 1)
TAP$previous_MI <- as.factor(TAP$previous_MI)
table(TAP$previous_MI)

TAP$Atrial_Fibrillation <- ifelse(TAP$PreOp_Heart_Rhy=='Artrial fibrillation/flutter', 1, 0)
TAP$Atrial_Fibrillation[is.na(TAP$Atrial_Fibrillation)] <- 0

TAP$ActiveEndocarditis <- TAP$`Active Endocarditis`
TAP$ActiveEndocarditis[TAP$ActiveEndocarditis== 'NULL'] <- NA
TAP$ActiveEndocarditis <- ifelse(TAP$ActiveEndocarditis=='Yes', 1, 0)
TAP$ActiveEndocarditis <- as.factor(TAP$ActiveEndocarditis)

TAP$REOPERATION <- as.factor(TAP$REOPERATION)


TAP$OperativePriority <- TAP$`Operative Priority`
TAP$Urgent <- ifelse(TAP$OperativePriority == 'Urgent', 1, 0)
TAP$Emergency <- ifelse(TAP$OperativePriority=='Emergency', 1, 0)
TAP$Salvage <- ifelse(TAP$OperativePriority=='Salvage', 1, 0)

TAP$Female <- ifelse(TAP$SEX=='Female', 1, 0)
################################################################################
#section 2 - plotting distributions and checking normality assumptions
#age
ggplot(TAP, aes(x=Age)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$Age, 'pnorm')

#BMI
TAP$Height_squared <- TAP$HEIGHT*TAP$HEIGHT
TAP$BMI <- TAP$WEIGHT/TAP$Height_squared
TAP$BMI[TAP$BMI>100] <- median(TAP$BMI, na.rm=TRUE)
ggplot(TAP, aes(x=BMI)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$BMI, 'pnorm')

#cross-clamp time 
TAP$CrossClampTime<-as.numeric(TAP$`Cross Clamp Time`)
TAP$CrossClampTime[TAP$CrossClampTime>1000] <- NA
ggplot(TAP, aes(x=CrossClampTime)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$CrossClampTime, 'pnorm')

#bypass time 
TAP$BypassTime<-as.numeric(TAP$`Bypass Time`)
ggplot(TAP, aes(x=BypassTime)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$BypassTime, 'pnorm')

#length of stay 
TAP$TotalLengthofStayDays <- as.numeric(TAP$`Total LOS`)
ggplot(TAP, aes(x=TotalLengthofStayDays)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$TotalLengthofStayDays, 'pnorm')

#additive euroscore 
TAP$AdditiveEuroscore <- as.numeric(TAP$`Additive Euroscore`)
ggplot(TAP, aes(x=AdditiveEuroscore)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$AdditiveEuroscore, 'pnorm')

#time ventilated
TAP$TimeVentilatedDays <- as.numeric(TAP$Ventilation)
TAP$TimeVentilatedHours <- TAP$TimeVentilatedDays*24
ggplot(TAP, aes(x=TimeVentilatedHours)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$TimeVentilatedHours, 'pnorm')

#time in theatre 
TAP$TimeinTheatreMinutes<-as.numeric(TAP$`Surgery Finish`-TAP$`Surgery Start`)
TAP$TimeinTheatreMinutes[TAP$TimeinTheatreMinutes<0]<- NA
ggplot(TAP, aes(x=TimeinTheatreMinutes)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$TimeinTheatreMinutes, 'pnorm')

#length of CICU stay 
TAP$LengthofCICUstayHours <- as.numeric(TAP$CICU)
ggplot(TAP, aes(x=LengthofCICUstayHours)) + 
  geom_density(fill="#36648B", color="red", alpha=0.8)
ks.test(TAP$LengthofCICUstayHours, 'pnorm')

TAP$LogisticEuroscore <- as.numeric(TAP$`Logistic Euroscore`)
################################################################################
#Section 3 - descriptive statistics 

#continuous variables, pre-operative 
mycontrols  <- tableby.control(test=TRUE, total=FALSE,
                               numeric.test="kwt", cat.test="chisq",
                               numeric.stats=c("N", "median", "q1q3"),
                               cat.stats=c("N","countpct"),
                               stats.labels=list(N='Count', median='Median', q1q3='Q1,Q3'))




categorical_demographic <- tableby(ValveType ~ SEX +Smoking +Diabetes +Hypertension +LVEF +
                                     DyspnoeaStatus + AnginaStatus + PreOp_Heart_Rhy+ previous_MI + 
                                     OperativePriority + REOPERATION, data=TAP, control=mycontrols)
summary(categorical_demographic)

continuous_demographic <- tableby(ValveType ~ Age + BMI + AdditiveEuroscore +
                                    LogisticEuroscore, data=TAP, control=mycontrols)

summary(continuous_demographic)

intraoperative <- tableby(ValveType ~ BypassTime + CrossClampTime + 
                            TimeinTheatreMinutes + TimeVentilatedHours, data=TAP, 
                          control=mycontrols)
summary(intraoperative)


TAP$Complication_AF <-  ifelse(TAP$`Complication 1` == 'Atrial Fibrillation' | 
                                  TAP$`Complication 2` == 'Atrial Fibrillation' |
                                  TAP$`Complication 3` == 'Atrial Fibrillation' |
                                 TAP$`Complication 4` == 'Atrial Fibrillation' |
                                 TAP$`Complication 5` == 'Atrial Fibrillation' |
                                 TAP$`Complication 6` == 'Atrial Fibrillation' |
                                 TAP$`Complication 7` == 'Atrial Fibrillation' |
                                 TAP$`Complication 8` == 'Atrial Fibrillation' |
                                 TAP$`Complication 9` == 'Atrial Fibrillation' |
                                 TAP$`Complication 10` == 'Atrial Fibrillation', 1, 0)
TAP$Complication_AF <- as.factor(TAP$Complication_AF) 
table(TAP$Complication_AF)

stroke = c("CVA with mild residual deficit", "CVA with severe residual deficit", "CVA with no residual deficit")
TAP$Complication_CVA <-  ifelse(TAP$`Complication 1` %in% stroke |
                                  TAP$`Complication 2` %in% stroke | 
                                  TAP$`Complication 3` %in% stroke | 
                                  TAP$`Complication 4` %in% stroke | 
                                  TAP$`Complication 5` %in% stroke | 
                                  TAP$`Complication 6` %in% stroke | 
                                  TAP$`Complication 7` %in% stroke | 
                                  TAP$`Complication 8` %in% stroke | 
                                  TAP$`Complication 9` %in% stroke | 
                                  TAP$`Complication 10` %in% stroke, 1, 0)
TAP$Complication_CVA <- as.factor(TAP$Complication_CVA) 

TAP$Complication_Delirium <-  ifelse(TAP$`Complication 1` == 'Delirium' | 
                                 TAP$`Complication 2` == 'Delirium' |
                                 TAP$`Complication 3` == 'Delirium' |
                                 TAP$`Complication 4` == 'Delirium' |
                                 TAP$`Complication 5` == 'Delirium' |
                                 TAP$`Complication 6` == 'Delirium' |
                                 TAP$`Complication 7` == 'Delirium' |
                                 TAP$`Complication 8` == 'Delirium' |
                                 TAP$`Complication 9` == 'Delirium' |
                                 TAP$`Complication 10` == 'Delirium', 1, 0)
TAP$Complication_Delirium <- as.factor(TAP$Complication_Delirium) 


TAP$Complication_MI <-  ifelse(TAP$`Complication 1` == 'Myocardial infarction' | 
                                       TAP$`Complication 2` == 'Myocardial infarction' |
                                       TAP$`Complication 3` == 'Myocardial infarction' |
                                       TAP$`Complication 4` == 'Myocardial infarction' |
                                       TAP$`Complication 5` == 'Myocardial infarction' |
                                       TAP$`Complication 6` == 'Myocardial infarction' |
                                       TAP$`Complication 7` == 'Myocardial infarction' |
                                       TAP$`Complication 8` == 'Myocardial infarction' |
                                       TAP$`Complication 9` == 'Myocardial infarction' |
                                       TAP$`Complication 10` == 'Myocardial infarction', 1, 0)
TAP$Complication_MI <- as.factor(TAP$Complication_MI) 

TAP$Complication_Pneumonia <-  ifelse(TAP$`Complication 1` == 'Chest infection requiring antibiotics' | 
                                 TAP$`Complication 2` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 3` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 4` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 5` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 6` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 7` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 8` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 9` == 'Chest infection requiring antibiotics' |
                                 TAP$`Complication 10` == 'Chest infection requiring antibiotics', 1, 0)
TAP$Complication_Pneumonia <- as.factor(TAP$Complication_Pneumonia) 

TAP$Complication_pneumothorax <-  ifelse(TAP$`Complication 1` == 'Pneumothorax requiring drain' | 
                                        TAP$`Complication 2` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 3` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 4` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 5` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 6` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 7` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 8` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 9` == 'Pneumothorax requiring drain' |
                                        TAP$`Complication 10` == 'Pneumothorax requiring drain', 1, 0)
TAP$Complication_pneumothorax <- as.factor(TAP$Complication_pneumothorax) 

TAP$Complication_PPM <-  ifelse(TAP$`Complication 1` == 'Permanent Pace Maker' | 
                                           TAP$`Complication 2` == 'Permanent Pace Maker' |
                                           TAP$`Complication 3` == 'Permanent Pace Maker' |
                                           TAP$`Complication 4` == 'Permanent Pace Maker' |
                                           TAP$`Complication 5` == 'Permanent Pace Maker' |
                                           TAP$`Complication 6` == 'Permanent Pace Maker' |
                                           TAP$`Complication 7` == 'Permanent Pace Maker' |
                                           TAP$`Complication 8` == 'Permanent Pace Maker' |
                                           TAP$`Complication 9` == 'Permanent Pace Maker' |
                                           TAP$`Complication 10` == 'Permanent Pace Maker', 1, 0)
TAP$Complication_PPM <- as.factor(TAP$Complication_PPM) 

TAP$Complication_VTVF <-  ifelse(TAP$`Complication 1` == 'VT/VF' | 
                                  TAP$`Complication 2` == 'VT/VF' |
                                  TAP$`Complication 3` == 'VT/VF' |
                                  TAP$`Complication 4` == 'VT/VF' |
                                  TAP$`Complication 5` == 'VT/VF' |
                                  TAP$`Complication 6` == 'VT/VF' |
                                  TAP$`Complication 7` == 'VT/VF' |
                                  TAP$`Complication 8` == 'VT/VF' |
                                  TAP$`Complication 9` == 'VT/VF' |
                                  TAP$`Complication 10` == 'VT/VF', 1, 0)
TAP$Complication_VTVF <- as.factor(TAP$Complication_VTVF) 



TAP$Inhospitalmortality <- ifelse(TAP$`Hospital Outcome` == 'Died in cardiac unit'|
                                    TAP$`Hospital Outcome`=='Died in ward', 1, 0)
TAP$Inhospitalmortality <- as.factor(TAP$Inhospitalmortality) 

TAP$Thirtydaymortality <- ifelse(TAP$TTE <= 30 & TAP$Event==1, 1, 0)
TAP$Thirtydaymortality <- as.factor(TAP$Thirtydaymortality) 

TAP$IntraoperativeMortality <- ifelse(TAP$`Theatre Outcome` != 'NULL', 1, 0)
TAP$IntraoperativeMortality <- as.factor(TAP$IntraoperativeMortality) 

categorical_outcomes <- tableby(ValveType ~ IntraoperativeMortality + Inhospitalmortality +
                                  Thirtydaymortality + Complication_AF + Complication_PPM + 
                                  Complication_MI + Complication_VTVF +Complication_CVA + 
                                  Complication_pneumothorax + Complication_Pneumonia + 
                                  Complication_Delirium, data=TAP, control=mycontrols)

summary(categorical_outcomes)
continuous_outcomes <- tableby(ValveType ~ TotalLengthofStayDays + LengthofCICUstayHours, 
                               data=TAP2, control=mycontrols)
summary(continuous_outcomes)

################################################################################
# section 4 - propensity score matching

#Section 4.1 - Selecting variables for inclusion in propensity score generation based on p<=0.25
options(scipen=999)
df <- data.frame("Characteristic", "Odds_ratio", "2.5%", "97.5%", "p_value")

list  <- c('AdditiveEuroscore', 'BMI',  'Age', 'Female', 
           'NYHA_II','NYHA_III','NYHA_IV','CCS_1','CCS_2','CCS_3','CCS_4',
           'oral_therapy','insulin','ex_smoker', 'Smoker',
           'Atrial_Fibrillation','Hypertension','REOPERATION','previous_MI','moderate_LVEF', 
           'poor_LVEF', 'very_poor_LVEF', 'Urgent', 'Emergency', 'Salvage')

for (var in list) {
  
  formula <- as.formula(paste('ValveType ~', var))
  model1 <- glm(formula, data=TAP, family = binomial)
  output = c(var, exp(model1$coefficients[2]), exp(confint.default(model1))[2], exp(confint.default(model1))[4], coef(summary(model1))[,4][2])
  df = rbind(df, output)
}
View(df)

#for ease of viewing in propensity score matching analysis love plot
TAP$NYHA_II <- as.numeric(TAP$NYHA_II)
TAP$NYHA_III <- as.numeric(TAP$NYHA_III)
TAP$NYHA_IV <- as.numeric(TAP$NYHA_IV)
TAP$Hypertension <- as.numeric(TAP$Hypertension)

TAP$AdditiveEuroscore[is.na(TAP$AdditiveEuroscore)] <- median(TAP$AdditiveEuroscore, na.rm=TRUE)

m.out <- matchit(ValveType ~ AdditiveEuroscore +  Age  + Female +
                   NYHA_II + NYHA_III + NYHA_IV + 
                   oral_therapy + insulin + ex_smoker + Smoker  + 
                    Hypertension,
                 data=TAP, method="nearest", distance="glm", link= "linear.logit",
                 caliper=0.2) #case complete model. Gives significant pneumonia/AF without significant difference in euroscore 

summary(m.out)
plot(summary(m.out))

plot(m.out, type = "jitter", interactive = FALSE)
plot(m.out, type = "density", interactive = FALSE,
     which.xs = ~ Age + AdditiveEuroscore)

#rewriting original data with propensity-matched dataset 
TAP2<-match.data(m.out)

################################################################################
#Section 5 - repeating descriptive statistics 
#at this point, sections 2-3 were repeated to give descriptive statistics for matched dataset 
#Original 'TAP' dataframe was switched for matched 'TAP2' dataframe 

################################################################################
#Section 6 - Unadjusted Logistic regression 
df <- data.frame("Characteristic", "Odds_ratio", "2.5%", "97.5%", "p_value")


list  <- c("Complication_AF","Complication_Pneumonia", 
           "Complication_PPM","Complication_Delirium",    
           "Complication_MI","Complication_VTVF","Complication_pneumothorax",
           "Complication_CVA", "Inhospitalmortality", "Thirtydaymortality")

for (var in list) {
  
  formula <- as.formula(paste(var, "~ ValveType"))
  model1 <- glm(formula, data=TAP2, family = binomial)
  output = c(var, exp(model1$coefficients[2]), exp(confint.default(model1))[2], exp(confint.default(model1))[4], coef(summary(model1))[,4][2])
  df = rbind(df, output)
}
df
################################################################################
#Section 7 - Logistic regression adjusted for AdditiveEuroscore
df <- data.frame("Characteristic", "Odds_ratio", "2.5%", "97.5%", "p_value")


for (var in list) {
  
  formula <- as.formula(paste(var, "~ ValveType + AdditiveEuroscore"))
  model1 <- glm(formula, data=TAP2, family = binomial)
  output = c(var, exp(model1$coefficients[2]), exp(confint.default(model1))[2], exp(confint.default(model1))[5], coef(summary(model1))[,4][2])
  df = rbind(df, output)
}
df
################################################################################
#Section 8 - Survival analysis 

# The colors
BLUE <- "#076fa2"
RED <- "#E3120B"
BLACK <- "#202020"
GREY <- "grey50"

#KM plot before matching 
AVR<-TAP
AVR$years <- AVR$TTE/365
Y = Surv(AVR$years, AVR$Event == 1)
kmfit = survfit(Y ~ AVR$ValveType)
p1 <- survdiff(formula = Surv(years, Event) ~ ValveType, data=AVR)
p1 #log rank test result 
km <- ggsurvfit(kmfit, lwd=0.5) + add_confidence_interval(type='lines', alpha=0.2, lwd=0.5) +
  scale_ggsurvfit() +
  coord_cartesian(xlim = c(0, 5), ylim =c(0.5, 1)) +
  labs(x="Years since valve replacement",
       y="Cumulative survival",
       caption="Log rank p value: 0.03", 
       title="Before matching") + 
  scale_color_manual(values=c(BLUE, RED), labels=c("Sutured", "Perceval")) + 
  scale_fill_manual(values=c(BLUE, RED), labels=c("Sutured", "Perceval")) 

km <- km + theme(legend.position=c(0.21, 0.3)) +
  theme(legend.direction="vertical") +
  theme(legend.key.size = unit(0.2, 'cm')) + 
  theme(legend.text = element_text(family="Arial",size=5)) + 
  theme(legend.box.background = element_rect(colour = "white")) + 
  theme(legend.box.margin = unit(c(1, 0, 0, 0), "cm")) +
  theme(axis.line = element_line(color='black')) + 
  theme(panel.border = element_blank()) + 
  theme(plot.caption = element_text(hjust = 0.85, size=5, vjust=250, face="italic")) + 
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.text.y = element_text(family = "Arial", size = 5)) + 
  theme(axis.text.x = element_text(family = "Arial", size = 5)) + 
  theme(axis.title.x = element_text(family = "Arial",size=7)) + 
  theme(axis.title.y = element_text(family = "Arial",size=7)) + 
  theme(plot.title = element_text(family="Arial", size=10, hjust=0.5)) + 
  theme(panel.grid.major = element_line(color = "grey",
                                        linewidth=0.1,
                                        linetype = 'dashed')) + 
  theme(panel.grid.minor = element_line(color="grey", linewidth = 0.1, linetype = 'dotted')) 

km
View(TAP)
#log rank test after matching
TAP2$years <- TAP2$TTE/365
Y2 = Surv(TAP2$years, TAP2$Event == 1)
kmfit2 = survfit(Y2 ~ TAP2$ValveType)
p2 <- survdiff(formula = Surv(years, Event) ~ ValveType, data=TAP2)
p2
#KM plot after matching 
km2 <- ggsurvfit(kmfit2, lwd=0.5) + add_confidence_interval(type='lines', alpha=0.2, lwd=0.5) + 
  scale_ggsurvfit() +
  coord_cartesian(xlim = c(0, 5), ylim =c(0.5, 1)) +
  labs(x="Years since valve replacement",
       y="Cumulative survival",
       caption="Log rank p value: 0.40",
       title="After matching") + 
  scale_color_manual(values=c(BLUE, RED), labels=c("Sutured", "Perceval")) + 
  scale_fill_manual(values=c(BLUE, RED), labels=c("Sutured", "Perceval")) 

km2 <- km2 + theme(legend.position=c(0.21, 0.3)) +
  theme(legend.direction="vertical") +
  theme(legend.key.size = unit(0.2, 'cm')) + 
  theme(legend.text = element_text(family="Arial",size=5)) + 
  theme(legend.box.background = element_rect(colour = "white")) + 
  theme(legend.box.margin = unit(c(0, 0, 0, 0), "cm")) +
  theme(axis.line = element_line(color='black')) + 
  theme(panel.border = element_blank()) + 
  theme(plot.caption = element_text(hjust = 0.85, size=5, vjust=250, face="italic")) + 
  theme(panel.background = element_rect(fill = "white")) + 
  theme(axis.text.y = element_text(family = "Arial", size = 5)) + 
  theme(axis.text.x = element_text(family = "Arial", size = 5)) + 
  theme(axis.title.x = element_text(family = "Arial",size=7)) + 
  theme(axis.title.y = element_blank()) + 
  theme(plot.title = element_text(family="Arial", size=10, hjust=0.5)) + 
  theme(panel.grid.major = element_line(color = "grey",
                                        linewidth=0.1,
                                        linetype = 'dashed')) + 
  theme(panel.grid.minor = element_line(color="grey", linewidth = 0.1, linetype = 'dotted'))


km2


#combine plots 
library("ggpubr")
figure <- ggarrange(km, km2,
                    ncol = 2, nrow = 1)
figure

#1 year, 2 year and 4 year survival
summary(survfit(Surv(years, Event==1) ~ValveType, data=TAP2), times = 1)
summary(survfit(Surv(years, Event==1) ~ValveType, data=TAP2), times = 2)
summary(survfit(Surv(years, Event==1) ~ValveType, data=TAP2), times = 4)

#median follow-up time 
median(AVR$years)

################################################################################
#Section 9 - Further graphics 

#9a) Grouped bar chart for outcomes in propensity matched cohort 

library(shadowtext)
comp <- c(rep("In-hospital mortality" , 2) , rep("Thirty day mortality" , 2) ,
          rep("Atrial fibrillation" , 2) , rep("Pneumonia" , 2),
          rep("Delirium", 2),rep("Stroke", 2),rep("PPM", 2))
comp <- factor(comp, levels=c("In-hospital mortality", "Thirty day mortality",
                              "PPM", "Stroke", "Delirium", "Pneumonia", "Atrial fibrillation"))
label <- c("In-hospital mortality","In-hospital mortality",
           "Thirty day mortality","Thirty day mortality",
           "Atrial fibrillation","Atrial fibrillation","Pneumonia","Pneumonia","Delirium",
           "Delirium","Stroke","Stroke","PPM","PPM")
label <- factor(label, levels=c('In-hospital mortality','Thirty day mortality',
                                'Atrial fibrillation','Pneumonia','Delirium',
                                'Stroke','PPM'))
x_pos <- NA
label_colour <- c("black","black","black","black","white","white",
                  "white","white","white","white","black","black","black","black")
valve <- rep(c("Sutured", "Perceval") , 7)
valve <- factor(valve, levels=c("Perceval", "Sutured"))
value <- c(2,1,4,3,58,39,36,21,27,28,4,6,4,6)
data <- data.frame(comp,valve,value,label,x_pos,label_colour)
data$x_pos <- ifelse(data$value>10, data$value-10, 0)
data$x_pos <- ifelse(data$value<10, 14, data$x_pos)
data$x_pos <- as.numeric(data$x_pos)
View(data)

# Grouped
p2 <- ggplot(data, aes(fill=comp, y=valve, x=value, label=label)) + 
  geom_bar(position=position_dodge(0.99), stat="identity", width=1) + 
  scale_fill_manual(values=c("#E64E53","#FF8785","#98DAFF","#7BBFFC",
                             "#5DA4DF","#3D89C3","#00588D")) + xlab('Frequency of complication')


p2 <- p2 + scale_x_continuous(
  limits=c(0,60),
  breaks=seq(0,60, by=10),
  expand=c(0,0),
  position="top"
) + geom_text(position=position_dodge(width=0.99), aes(label=label, x=x_pos,colour=label_colour)) +
  scale_colour_manual(values=c(GREY, "white")) +
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.major.x = element_line(color ="#A8BAC4", size=0.3, linetype = 'dotted'), 
    axis.title.x = element_text(family = "Arial",size=10),
    axis.title.y = element_blank(),
    axis.text.y.left = element_text(family = "Arial",size=10),
    axis.line.x.top = element_line(color="black", size = 0.2),
    legend.position = "none",
    axis.text.x.top =element_text(family = "Arial",size=10),
    plot.margin=unit(c(0.4, 0.5,0.1,0.1), 'cm')
  )


p2

#9b) Density plots - time spent under cross clamping and bypass
TAP$ValveType <- as.factor(TAP$ValveType)

#crossclamping 
Cross_clamp <- ggplot(TAP, aes(x=CrossClampTime, color=ValveType, fill=ValveType)) +
  geom_density(alpha=0.7, lwd=0.4) + 
  scale_fill_manual("Valve Type:  ", values=c("#076fa2",  "#E3120B"), labels=c("Sutured", "Perceval")) + 
  scale_color_manual(values=c("#00588D", "#E64E53"), guide="none") + 
  labs(x="Cross clamp time (minutes)", y = "Density") + 
  scale_x_continuous(breaks = c(0, 25, 50, 100, 150, 200, 250)) 

Cross_clamp <- Cross_clamp + theme(
  axis.text = element_text(size=5),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y=element_blank(),
  axis.title =element_text(size=10),
  legend.position=c(0.6,0.8),
  legend.key.size = unit(0.5, "cm"),
  legend.text = element_text(family="arial", size=5),
  legend.title = element_text(family="arial", size=7),
  plot.margin=unit(c(0.2,0.2,0.2,0.2), 'cm'), 
  axis.line = element_line(color='black'),
  panel.border = element_blank(),
  panel.background = element_rect(fill = 'white', color = 'white'),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  axis.line.x = element_line(arrow=arrow(length=unit(0.20,"cm"), ends="last", type = "closed"), size=0.2))
Cross_clamp

#bypass 
Bypass <- ggplot(TAP, aes(x=BypassTime, color=ValveType, fill=ValveType)) +
  geom_density(alpha=0.7, lwd=0.4) + 
  scale_fill_manual("Valve Type:  ", values=c("#076fa2",  "#E3120B"), labels=c("Sutured", "Perceval")) + 
  scale_color_manual(values=c("#00588D", "#E64E53"), guide="none") + 
  labs(x="Cardiopulmonary bypass time (minutes)", y = "Density") + 
  scale_x_continuous(breaks = c(0, 28, 50, 100, 150, 200, 250, 300)) 

Bypass <- Bypass + theme(
  axis.text = element_text(size=5),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y=element_blank(),
  axis.title =element_text(size=10),
  legend.position=c(0.6,0.8),
  legend.key.size = unit(0.5, "cm"),
  legend.text = element_text(family="arial", size=5),
  legend.title = element_text(family="arial", size=7),
  plot.margin=unit(c(0.2,0.2,0.2,0.2), 'cm'), 
  axis.line = element_line(color='black'),
  panel.border = element_blank(),
  panel.background = element_rect(fill = 'white', color = 'white'),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), 
  axis.line.x = element_line(arrow=arrow(length=unit(0.20,"cm"), ends="last", type = "closed"), size=0.2))
Bypass