rm(list=ls())
cat("\014")
dev.off()
options(scipen = 999)

library(data.table)
library(tidyr)
library(dplyr)
library(stringr)
library(psych)
library(ggplot2)
library(broom)
library(MASS)
library(tableone)

setwd("~/OneDrive - King's College London/Holmusk")

######################################
###########Data processing############
######################################

#####demographics
demo<-fread("~/OneDrive - King's College London/Holmusk/demographics.csv") 
demo<-setDT(demo)
names(demo) #5 variables
length(unique(demo$patient_id)) #3000 patients 
#check column types
democolumn_types <- sapply(demo, class)
print(democolumn_types)
#format dob 
demo$date_of_birth<-as.Date(demo$date_of_birth,"%d/%m/%Y")

#summary of gender 
demo$gender<-factor(demo$gender)
summary(demo$gender) #f = 101, female = 1396, m = 170, male = 1333

#re-coding the gender variable
demo$gender_recoded <- ifelse(demo$gender %in% c("f", "Female"), "female",
                              ifelse(demo$gender %in% c("m", "Male"), "male", NA))

demo$gender_recoded<-factor(demo$gender_recoded)
summary(demo$gender_recoded) #females = 1497, males = 1503 (matches above)

#summary of ethnicity 
demo$race<-factor(demo$race)
summary(demo$race) #chinese = 307 Chinese = 1608  India = 100  Indian = 195  Malay = 629  Others = 161

#re-coding the race variable
demo$race_recoded <- ifelse(tolower(demo$race) %in% c("chinese", "chinese"), "Chinese",
                            ifelse(tolower(demo$race) %in% c("indian", "india"), "Indian", demo$race))
table(demo$race_recoded)

#re-code Malay and other (ifelse removed label)
demo$race_recoded <- ifelse(demo$race_recoded == 5, "Malay",
                            ifelse(demo$race_recoded == 6, "Other", demo$race_recoded))

demo$race_recoded<-factor(demo$race_recoded)
summary(demo$race_recoded) #Chinese = 1915  Indian = 295  Malay = 629  Other = 161 (matches above)

#####clinical variables
clin<-fread("~/OneDrive - King's College London/Holmusk/clinical_data.csv") 
clin<-setDT(clin)
names(clin) #27 variables
length(clin$id) #3400 events
length(unique(clin$id)) #3000 patients, 400 duplicated (patients with multiple admissions)
clincolumn_types <- sapply(clin, class)
print(clincolumn_types)

#rename clin ID variable to match demo ID variable
names(clin)[names(clin)=="id"] <- "patient_id" 

head(clin) #date format need sorting and ID variable needs renamed to match name in demo data set
#height is in cm, needs to be m to match kg for weight for any BMI/obesity calculations
#format dates 
clin$date_of_admission<-as.Date(clin$date_of_admission,"%d/%m/%Y")
clin$date_of_discharge<-as.Date(clin$date_of_discharge,"%d/%m/%Y")
#convert cm to m
clin$height_in_metres <- clin$height / 100
#calculate BMI which could be used later 
clin$BMI <- clin$weight / (clin$height_in_metres ^ 2)
#create BMI groups 
clin$BMI_category <- cut(clin$BMI, 
                         breaks = c(0, 18.5, 24.9, 29.9, Inf),
                         labels = c("Underweight", "Normal weight", "Overweight", "Obese"),
                         include.lowest = TRUE)
clin$BMI_category<-factor(clin$BMI_category)
table(clin$BMI_category)
head(clin)

#create a binary variable for current obesity diagnosis which could be used later 
clin$current_obe <- ifelse(clin$BMI >= 30, "Obese", "Not Obese")
clin$current_obe<-factor(clin$current_obe)
summary(clin$current_obe) #Not Obese = 2039 Obese = 1361

#####billing ID
billid<-fread("~/OneDrive - King's College London/Holmusk/bill_id.csv") 
billid<-setDT(billid)
names(billid) #3 variables
head(billid) #need to format date
length(billid$patient_id) #13600 events
length(unique(billid$patient_id)) #3000 patients
billidcolumn_types <- sapply(billid, class)
print(billidcolumn_types)

#format date of admission 
billid$date_of_admission<-as.Date(billid$date_of_admission,"%d/%m/%Y")
head(billid)

#billing ID and patient ID
billam<-fread("~/OneDrive - King's College London/Holmusk/bill_amount.csv") 
head(billam)
billamcolumn_types <- sapply(billam, class)
print(billamcolumn_types)

#join billing ID to billing amount by bill_id variable 
billsdf<-setDT(full_join(billid,billam,by="bill_id"))
head(billsdf)

#join the clinical variables to billing variables by ID and date of admission 

#order by patient ID and date of admission 
clin2<-clin[order(patient_id,date_of_admission)] 
billsdf<-billsdf[order(patient_id,date_of_admission)]

clin_bill<-setDT(inner_join(clin2,billsdf,by=c('patient_id','date_of_admission')))
head(clin_bill)
length(unique(clin_bill$patient_id)) #patients have multiple amounts for the same admission 

#sum the column amounts by ID and date of admission 
result <- clin_bill %>%
  group_by(patient_id, date_of_admission) %>%
  summarise(total_amount = sum(amount, na.rm = TRUE), .groups = "drop")

#rejoin to the clinical file by ID and date of admission 
clin_bill2<-setDT(inner_join(clin2,result,by=c('patient_id','date_of_admission')))

#remove the duplicated patients (keeping only the earliest admission)
clin_bill2<-clin_bill2[order(patient_id,date_of_admission)]
head(clin_bill2)
clin_billFE<-clin_bill2[,head(.SD,1),by=patient_id] #keeps the first admission only 

#checked this worked
clin_bill2%>%filter(patient_id=="0284ec8aa1e6ced9ddaa7702b841248d") #1st admission was 2014-02-26
clin_billFE%>%filter(patient_id=="0284ec8aa1e6ced9ddaa7702b841248d") #date of admission is 2014-02-26

clin_bill2%>%filter(patient_id=="012c5eb5397a72f8aeb64f942b60846d") #1st admission was 2011-06-08
clin_billFE%>%filter(patient_id=="012c5eb5397a72f8aeb64f942b60846d") #date of admission is 2011-06-08 

clin_bill2%>%filter(patient_id=="03d6bdbc614a5bdbeadf5b981a59c423") #1st admission was 2011-02-01
clin_billFE%>%filter(patient_id=="03d6bdbc614a5bdbeadf5b981a59c423") #date of admission is 2011-02-01 

#######create final data set
df<-setDT(inner_join(demo,clin_billFE,by="patient_id"))
length(unique(df$patient_id)) #3000
names(df) #37 variables 

#turn medical history variables into factors for analysis 
df$medical_history_dia<-factor(df$medical_history_dia)
df$medical_history_sud<-factor(df$medical_history_sud)
df$medical_history_hbp<-factor(df$medical_history_hbp)
df$medical_history_ren<-factor(df$medical_history_ren)
df$medical_history_tum<-factor(df$medical_history_tum)
df$medical_history_anx<-factor(df$medical_history_anx)
df$medical_history_mood<-factor(df$medical_history_mood)

summary(df$medical_history_dia) #0 = 2501 1 = 499
summary(df$medical_history_sud) #0 = 1936 1 = 862 NAs = 202 
summary(df$medical_history_hbp) #miss coding for 0s and 1s,  0 = 1911  1 = 307  No = 679  Yes = 103
#re-code 0 and 1 and No and Yes
df$medical_history_hbp2 <- ifelse(df$medical_history_hbp %in% c("0", "No"), "0",
                            ifelse(df$medical_history_hbp %in% c("1", "Yes"), "1", NA))
df$medical_history_hbp2<-factor(df$medical_history_hbp2)
summary(df$medical_history_hbp2) #0 = 2590  1 = 410 (matches above)

summary(df$medical_history_ren) #0 = 2843 1 = 157
summary(df$medical_history_tum) #0 = 2556 1 = 175  NAs = 269
summary(df$medical_history_anx) #0 = 2251 1 = 749
summary(df$medical_history_mood)#0 = 2232 0 = 768 

#costs variable 
describe(df$total_amount) #mean costs = $21,798.34 (10,161)
hist(df$total_amount) #some outliers 

#create age at admission variable 
df$age_at_admission <- round(as.numeric(difftime(df$date_of_admission, df$date_of_birth, units = "days") / 365.25))
describe(df$age_at_admission) #mean age at admission = 52.38 (14.66)

#create duration of care variable 
df$duration_of_care <- as.numeric(difftime(df$date_of_discharge, df$date_of_admission, units = "days"))
describe(df$duration_of_care) #mean days in care = 11.08 (2.86)

#create a multiple treatment variable 
df[, poly := rowSums(.SD, na.rm = TRUE), .SDcols = c("trt_anx", "trt_con","trt_adt","trt_ssr","trt_the", "trt_oth")]           
describe(df$poly) #patients on a median of 4 treatments 

#outcome variable for models - CGIS difference 
describe(df$cgis_adm) #mean at admission = 4.29 (1.27)
hist(df$cgis_adm)
describe(df$cgis_dis) #mean at discharge = 3.93 (1.04) 
hist(df$cgis_dis)

#create a variable which is the difference admission scores and discharge scores (a negative variable = improvement during episode of care)
df$cgis_dif<-df$cgis_dis-df$cgis_adm

#checking this worked 
df_cgis_check <- dplyr::select(df, "cgis_adm", "cgis_dis", "cgis_dif") #works,-3 = improvement 

#see what the scores look like
hist(df$cgis_dif)
describe(df$cgis_dif) #mean difference = -0.36 (1.59)

dfcolumn_types <- sapply(df, class)
print(dfcolumn_types)

###############################
##Demographics of sample#######
###############################
myVars <- c("age_at_admission","gender_recoded","race_recoded","duration_of_care","medical_history_dia","medical_history_sud","medical_history_hbp2",
            "medical_history_ren","medical_history_tum","medical_history_anx","medical_history_mood","poly","cgis_adm","cgis_dis","cgis_dif","total_amount", 
            "trt_anx", "trt_con","trt_adt","trt_ssr","trt_the", "trt_oth")
## Vector of categorical variables that need transformation
catVars <- c("gender_recoded","race_recoded","medical_history_dia","medical_history_sud",
             "medical_history_hbp2","medical_history_ren","medical_history_tum","medical_history_anx","medical_history_mood", 
             "trt_anx", "trt_con","trt_adt","trt_ssr","trt_the", "trt_oth")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = df, factorVars = catVars)

data<-print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(data,'MDDDemographicDataJan24.csv', na = "")

###############################
############Analysis###########
###############################
#CGIS change overall 
#model did not include costs - didn't add anything and is hard to interpret without information of where the data is from/in relation to what treatment
change_mod<-lm(cgis_dif ~ age_at_admission + gender_recoded + race_recoded + duration_of_care + medical_history_dia + medical_history_sud + medical_history_hbp2 + medical_history_ren +
                medical_history_tum + medical_history_anx + medical_history_mood + poly, data=df)
summary(change_mod)

#checking CGIS change model
hist(change_mod$residuals) #residuals look ok 
vif(change_mod)#no multicollinearity among the predictors, none greater than 5
durbinWatsonTest(change_mod) #haven't violated the independence assumption (residuals aren't correlated with each other)
#homoscedasticity
residuals_vs_fitted <- ggplot(augment(change_mod), aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")
residuals_vs_fitted #doesn't look great
ncvTest(change_mod) #data is homoscedastic 
cutoff <- 4/((nrow(change_mod)-length(change_mod$coefficients)-2)) 
plot(change_mod, which=4, cook.levels=cutoff) #a few outliers

#######Sub-analysis 
########################
#CGIS change in MDD patients with a history of diabetes
#demographics
myVars <- c("age_at_admission","gender_recoded","race_recoded","duration_of_care","medical_history_dia","medical_history_sud","medical_history_hbp2",
            "medical_history_ren","medical_history_tum","medical_history_anx","medical_history_mood","poly","cgis_adm","cgis_dis","cgis_dif","total_amount", 
            "trt_anx", "trt_con","trt_adt","trt_ssr","trt_the", "trt_oth", "current_obe")
## Vector of categorical variables that need transformation
catVars <- c("gender_recoded","race_recoded","medical_history_dia","medical_history_sud",
             "medical_history_hbp2","medical_history_ren","medical_history_tum","medical_history_anx","medical_history_mood", 
             "trt_anx", "trt_con","trt_adt","trt_ssr","trt_the", "trt_oth")
## Create a TableOne object
tab2 <- CreateTableOne(vars = myVars, data = df_dia, factorVars = catVars)

data_2<-print(tab2, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

write.csv(data_2,'MDD_diaDemographicDataJan24.csv', na = "")

df_dia<-filter(df,medical_history_dia=="1")

#included current obesity due to relationship with diabetes 
change_mod_dia<-lm(cgis_dif ~ age_at_admission + gender_recoded + race_recoded + duration_of_care + current_obe + medical_history_sud + medical_history_ren +
                 medical_history_tum + medical_history_anx + medical_history_mood + poly, data=df_dia)
summary(change_mod_dia)

#checking Diabetes CGIS change model
hist(change_mod_dia$residuals)#look ok 
vif(change_mod_dia)#no multicollinearity among the predictors, none greater than 5
durbinWatsonTest(change_mod_dia)#haven't violated the independence assumption
residuals_vs_fitted <- ggplot(augment(change_mod_dia), aes(.fitted, .resid)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")
residuals_vs_fitted #doesn't look great (worse than above)
ncvTest(change_mod_dia) #data is homoscedastic (but doesn't look great)
cutoff <- 4/((nrow(change_mod_dia)-length(change_mod_dia$coefficients)-2)) 
plot(change_mod_dia, which=4, cook.levels=cutoff) #a few outliers

