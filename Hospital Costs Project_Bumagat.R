#Loading Dataset
HospitalCosts

#Loading library
library(dplyr)

#1 To record the patient statistics, the agency wants to find the age category of people who frequent the hospital and has the maximum expenditure. 
hist(HospitalCosts$AGE)
table <- table(HospitalCosts$AGE)
HospitalCosts %>% group_by(AGE) %>%  summarize(count = n(), totalexp = sum(TOTCHG)) %>% arrange(desc(count,totalexp)) #This line of code was provided by Rajib Layek

#2 In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis related group that has maximum hospitalization and expenditure. 
APRDRG_table <- table(HospitalCosts$APRDRG)
max(APRDRG_table)
which.max(APRDRG_table) #640 has the max entries of hospitalization

TOTCHG_table <- table(HospitalCosts$TOTCHG)
max(TOTCHG_table) #max frequency is 8 of the unit
which.max(TOTCHG_table) #1096 is the most common discharge cost out of other 54 variables

max(HospitalCosts$TOTCHG) #48388 is the max discharge cost

library(dplyr) #640 has the highest count with the highest total expenditure of 437978
HospitalCosts %>% group_by(APRDRG) %>%  summarize(count = n(), totalexp = sum(TOTCHG)) %>% arrange(desc(count,totalexp))

#3 To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs. 
race <- as.factor(HospitalCosts$RACE)
summary(race)

HospitalCostsna <- na.omit(HospitalCosts)
modelannova <- aov(TOTCHG~race, data=HospitalCosts)
summary(modelannova) #done

#4 To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for proper allocation of resources. 
lm_4 <- lm(TOTCHG~AGE+FEMALE, data=HospitalCosts) 
summary(lm_4)
plot(lm_4)

#5 Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race. 
lm_5 <- lm(LOS~AGE+FEMALE+RACE, data=HospitalCosts) 
summary(lm_5)

#6 To perform a complete analysis, the agency wants to find the variable that mainly affects the hospital costs. 
lm_6 <- lm(TOTCHG ~ .,data = HospitalCosts) 
summary(lm2)




#This course requirement was guided by a Simplilearn instructor named Rajib Layek.

