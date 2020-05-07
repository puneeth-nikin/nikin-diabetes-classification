# installing required libraries libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)

#Importing the dataset
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",dl)
unzip(dl)
rm(dl)
diabetic_data<- read_csv('dataset_diabetes/diabetic_data.csv')

#Summary of the dataset

names(diabetic_data)
summary(diabetic_data)

# Cleaning the dataset


## check for NA
na_check<-apply(diabetic_data, 2, function(x) any(is.na(x)))
table(na_check)

## check for missing values
missing_values<-diabetic_data%>%gather(x,value,encounter_id:readmitted)%>%group_by(x)%>%count(missing=(value=='?'))

## dropping missing rows for diag_1,diag_2,diag_3
diabetic_data<-diabetic_data%>%filter(diag_1!='?',diag_2!='?',diag_3!='?',race!='?')
## removing gender marked as invalid
diabetic_data<-diabetic_data %>% filter(gender!='Unknown/Invalid')
## converting categorical features to factors
diabetic_data<-diabetic_data%>%
  mutate(race=as.factor(race),
         gender=as.factor(gender),
         age=as.factor(age),
         weight=as.factor(weight),
         admission_type_id=as.factor(admission_type_id),
         discharge_disposition_id=as.factor(discharge_disposition_id),
         admission_source_id=as.factor(admission_source_id),
         payer_code=as.factor(payer_code),
         medical_specialty=as.factor(medical_specialty),
         diag_1=as.factor(diag_1),
         diag_2=as.factor(diag_2),
         diag_3=as.factor(diag_3),
         max_glu_serum=as.factor(max_glu_serum),
         A1Cresult=as.factor(A1Cresult),
         metformin=as.factor(metformin),
         repaglinide=as.factor(repaglinide),
         nateglinide=as.factor(nateglinide),
         chlorpropamide=as.factor(chlorpropamide),
         glimepiride=as.factor(glimepiride),
         acetohexamide=as.factor(acetohexamide),
         glipizide=as.factor(glipizide),
         glyburide=as.factor(glyburide),
         tolbutamide=as.factor(tolbutamide),
         pioglitazone=as.factor(pioglitazone),
         rosiglitazone=as.factor(rosiglitazone),
         acarbose=as.factor(acarbose),
         miglitol=as.factor(miglitol),
         troglitazone=as.factor(troglitazone),
         tolazamide=as.factor(tolazamide),
         examide=as.factor(examide),
         citoglipton=as.factor(citoglipton),
         insulin=as.factor(insulin),
         `glyburide-metformin`=as.factor(`glyburide-metformin`),
         `glipizide-metformin`=as.factor(`glipizide-metformin`),
         `glimepiride-pioglitazone`=as.factor(`glimepiride-pioglitazone`),
         `metformin-rosiglitazone`=as.factor(`metformin-rosiglitazone`),
         `metformin-pioglitazone`=as.factor(`metformin-pioglitazone`),
         change=as.factor(change),
         diabetesMed=as.factor(diabetesMed),
         readmitted=as.factor(readmitted))


# Split data for validation, 10% of diabetic_data
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
validate_index <- createDataPartition(y = diabetic_data$readmitted,times=1,p=0.1,list=FALSE)
validation <- diabetic_data[validate_index,]
diabetes <- diabetic_data[-validate_index,]


# analysis of features

proportion_analysis<- function(feature){
  diabetes %>%group_by(feature=diabetes[[feature]])%>%
    summarise(n_patients=n(),
              single_visit=mean(readmitted=='NO'),
              greater_30_visits=mean(readmitted=='>30'),
              lesser_30_visits=mean(readmitted=='<30'))
}
plot_proportion_analysis<- function(analysis){
  analysis%>%
    select(-n_patients)%>%
    gather(n_readmitted,proportions,single_visit:lesser_30_visits)%>%
    ggplot(aes(x=n_readmitted,y=proportions,color=feature))+
    geom_jitter()+
    scale_x_discrete(label=abbreviate)
}

## analysis of readmitted with race
a_race<- proportion_analysis("race")
plot_proportion_analysis(a_race)

## analysis of readmitted with gender
a_gender<- proportion_analysis("gender")
plot_proportion_analysis(a_gender)

## analysis of readmitted with age
a_age <- proportion_analysis("age")
plot_proportion_analysis(a_age)

#analysis of readmitted with weight
a_weight<- proportion_analysis('weight')
plot_proportion_analysis(a_weight)

## analysis of readmitted with admission type 
a_adm_type<-proportion_analysis('admission_type_id')
plot_proportion_analysis(a_adm_type)

##analysis of readmitted with discharge_disposition
a_discharge_disp<-proportion_analysis('discharge_disposition_id')
plot_proportion_analysis(a_discharge_disp)

## analysis of readmitted with admission source
a_adm_source<- proportion_analysis('admission_source_id')
plot_proportion_analysis(a_adm_source)

##analysis of readmitted and time spent in hospital
diabetes%>%
  ggplot(aes(x=time_in_hospital))+
  geom_histogram()+
  facet_wrap(~readmitted,ncol = 1)

## analysis of readmitted payer_code 
a_pay<- proportion_analysis('payer_code')
plot_proportion_analysis(a_pay)

## analysis of readmitted with medical_speciality
a_med_spec<- proportion_analysis('medical_specialty')
a_med_spec%>%
  select(-n_patients)%>%
  gather(n_readmitted,proportions,single_visit:lesser_30_visits)%>%
  ggplot(aes(x=feature,y=proportions,color=n_readmitted))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))


## analysis of readmitted with number of lab procedures,number of procedures,
##number of medications, number of outpatient, number of emergency and number of inpatient
diabetes%>%
  select(readmitted,
         num_lab_procedures,
         num_procedures,
         num_medications,
         number_outpatient,
         number_emergency,
         number_inpatient)%>%
  gather(key,value,num_lab_procedures:number_inpatient)%>%
  ggplot(aes(value))+
  geom_histogram(stat = 'proportion')+
  scale_y_continuous(trans = 'log2')+
  facet_wrap(key~readmitted,ncol=3)

##analysis of readmitted with diag_1,diag_2, diag_3
a_diag<-diabetes%>%
  select(readmitted,diag_1,diag_2,diag_3)%>%
  gather(key,value,diag_1:diag_3)
levels(factor(a_diag$value))

## analysis of readmitted with number of diagnosis
diabetes%>% 
  ggplot(aes(x=number_diagnoses))+
  geom_histogram()+
  facet_wrap(~readmitted,ncol = 1)

## analysis of readmitted with tests

## analysis of readmitted with max_glu_serum
a_m_glu_serum <-proportion_analysis('max_glu_serum')
plot_proportion_analysis(a_m_glu_serum)

## analysis of readmitted with A1Cresult
a_a1c_result<-proportion_analysis('A1Cresult')
plot_proportion_analysis(a_a1c_result)

## tests with 4 levels namely Down,Up,No, Steady

diabetes%>%
  select(c(25:47,50))%>%
  gather(test,result,-readmitted)%>%
  group_by(test,result)%>%
  summarise(single_visit=mean(readmitted=='NO'),
            greater_30_visits=mean(readmitted=='>30'),
            lesser_30_visits=mean(readmitted=='<30'))%>%
  gather(visit,value,single_visit:lesser_30_visits)%>%
  ggplot(aes(x=visit,y=value,color=result))+
  geom_jitter()+
  scale_x_discrete(label=abbreviate)+
  facet_wrap(~test,ncol =5)


## analysis of readmitted with change
a_change<-proportion_analysis('change')
plot_proportion_analysis(a_change)

##analysis of readmitted with diabetes medicine
a_diabetes_med<- proportion_analysis('diabetesMed')
plot_proportion_analysis(a_diabetes_med)

# Modeling

## split data to train and test

test_index<- createDataPartition(diabetes$readmitted,times=1,p=0.3,list=FALSE)
test_set <- diabetes[test_index,]
train_set<- diabetes[-test_index,]

fit_lda<-train_set%>%train(readmitted ~ race+gender+age,method='lda',data=.)

