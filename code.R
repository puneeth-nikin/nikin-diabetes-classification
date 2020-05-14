# installing required libraries libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
library(gridExtra)
library(rvest)
library(tidyverse)
library(caret)

#Importing the dataset
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip",dl)
unzip(dl)
rm(dl)
##Diabetic Data
diabetic_data<- read_csv('dataset_diabetes/diabetic_data.csv')

##description of sone features
dataset_description<-read_csv('dataset_diabetes/IDs_mapping.csv')
admission_type_description<-dataset_description[1:8,]
discharge_disposition_description<-dataset_description[11:40,]
names(discharge_disposition_description)<-c('discharge_disposition_id','decription')
admission_source_description<-dataset_description[43:67,]
names(admission_source_description)<-c('admission_source_id','description')

#Summary of the dataset

names(diabetic_data)
summary(diabetic_data)
diabetic_data%>%group_by(readmitted)%>%summarise(n=n())

# Cleaning the dataset

## check for missing values
number_of_obs<-dim(diabetic_data)[1]
diabetic_data%>%
  gather(x,value,encounter_id:readmitted)%>%
  filter(value=='?')%>%
  group_by(x)%>%
  summarise(proportion_missing=n()/number_of_obs)%>%
  arrange(desc(proportion_missing))
diabetic_data<-diabetic_data%>%select(-weight,-medical_specialty,-payer_code)



# repetition of patients removing multiple encounters
diabetic_data<-
  diabetic_data[!duplicated(diabetic_data$patient_nbr),]

## checking for predictors with nzv and zero variance
nearZeroVar(diabetic_data,saveMetrics = TRUE)%>%
  as_tibble(rownames = "feature")%>%
  filter(zeroVar==TRUE | nzv == TRUE)
near_zero_variance <- nearZeroVar(diabetic_data)
diabetic_data<-diabetic_data[,-near_zero_variance]




##categorising ICD-9 codes
url<- "https://icd.codes/icd9cm"
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab<-tab[[1]]
tab <- tab %>% html_table
icd_9table<-tab
tab<-tab%>%
  filter(Chapter<18)
regex_codes<-"^(\\d{3})-(\\d{3})$"

### classifying diag_1 to ICD9 codes
temp_diabetic_data<-
  diabetic_data%>%
  filter(str_detect(diag_1,"^[VE\\?]")==FALSE)%>%
  mutate(diag_1=as.numeric(diag_1))

cat_list<-
  sapply(as.vector(tab$`Code Range`),function(x){
  between(temp_diabetic_data$diag_1,
          as.numeric(str_match(x,regex_codes)[,2]),
          as.numeric(str_match(x,regex_codes)[,3]))
})
cat_df<-as_tibble(cat_list)
temp_diabetic_data<-cbind(temp_diabetic_data,cat_df)
temp_diabetic_data<-
  temp_diabetic_data%>%
  gather(category_diag_1,value,`001-139`:`800-999`)%>%
  filter(value==TRUE)%>%
  select(patient_nbr,category_diag_1)
diabetic_data<-diabetic_data%>%
  left_join(temp_diabetic_data,by='patient_nbr')
diabetic_data$category_diag_1[str_detect(diabetic_data$diag_1,"^V")]<-"V01-V91"
diabetic_data$category_diag_1[str_detect(diabetic_data$diag_1,"^E")]<-"E000-E999"
diabetic_data$category_diag_1[str_detect(diabetic_data$diag_1,"^\\?")]<-"?"
diabetic_data$category_diag_1[str_detect(diabetic_data$diag_1,"^250")]<-"250"

###classifying diag_2 to ICD9 codes
temp_diabetic_data<-diabetic_data%>%
  filter(str_detect(diag_2,"^[VE\\?]")==FALSE)%>%
  mutate(diag_2=as.numeric(diag_2))
cat_list<-sapply(as.vector(tab$`Code Range`),function(x){
  between(temp_diabetic_data$diag_2,
          as.numeric(str_match(x,regex_codes)[,2]),
          as.numeric(str_match(x,regex_codes)[,3]))
})
cat_df<-as_tibble(cat_list)
temp_diabetic_data<-cbind(temp_diabetic_data,cat_df)
temp_diabetic_data<-temp_diabetic_data%>%
  gather(category_diag_2,value,`001-139`:`800-999`)%>%
  filter(value==TRUE)%>%
  select(patient_nbr,category_diag_2)
diabetic_data<-diabetic_data%>%
  left_join(temp_diabetic_data,by='patient_nbr')
diabetic_data$category_diag_2[str_detect(diabetic_data$diag_2,"^V")]<-"V01-V91"
diabetic_data$category_diag_2[str_detect(diabetic_data$diag_2,"^E")]<-"E000-E999"
diabetic_data$category_diag_2[str_detect(diabetic_data$diag_2,"^\\?")]<-"?"
diabetic_data$category_diag_2[str_detect(diabetic_data$diag_2,"^250")]<-"250"

###classifying diag_3 to ICD9 codes
temp_diabetic_data<-diabetic_data%>%
  filter(str_detect(diag_3,"^[VE\\?]")==FALSE)%>%
  mutate(diag_3=as.numeric(diag_3))

cat_list<-sapply(as.vector(tab$`Code Range`),function(x){
  between(temp_diabetic_data$diag_3,
          as.numeric(str_match(x,regex_codes)[,2]),
          as.numeric(str_match(x,regex_codes)[,3]))
})
cat_df<-as_tibble(cat_list)
temp_diabetic_data<-cbind(temp_diabetic_data,cat_df)
temp_diabetic_data<-temp_diabetic_data%>%
  gather(category_diag_3,value,`001-139`:`800-999`)%>%
  filter(value==TRUE)%>%select(patient_nbr,category_diag_3)
diabetic_data<-diabetic_data%>%left_join(temp_diabetic_data,by='patient_nbr')
diabetic_data$category_diag_3[str_detect(diabetic_data$diag_3,"^V")]<-"V01-V91"
diabetic_data$category_diag_3[str_detect(diabetic_data$diag_3,"^E")]<-"E000-E999"
diabetic_data$category_diag_3[str_detect(diabetic_data$diag_3,"^\\?")]<-"?"
diabetic_data$category_diag_3[str_detect(diabetic_data$diag_3,"^250")]<-"250"

### remove diag_1, diag_2, diag_3
diabetic_data<-diabetic_data%>%select(-c(diag_1,diag_2,diag_3))




##removing /grouping/merging categories with sparse data

### race
diabetic_data%>%group_by(race)%>%summarise(n=n())%>%arrange(n)

### gender
diabetic_data%>%group_by(gender)%>%summarise(n=n())%>%arrange(n)
diabetic_data<-diabetic_data %>% filter(gender %in% c("Female","Male"))

### age
diabetic_data%>%group_by(age)%>%summarise(n=n())%>%arrange(n)
diabetic_data$age[diabetic_data$age %in% c("[0-10)","[10-20)")]<-"[0-20)"
###admission_type_id
diabetic_data%>%group_by(admission_type_id)%>%summarise(n=n())%>%arrange(n)
diabetic_data$admission_type_id[diabetic_data$admission_type_id %in% c(4,7,8)]<-8

###discharge_disposition_id and removing expired(11)
diabetic_data%>%group_by(discharge_disposition_id)%>%summarise(n=n())%>%arrange(n)
diabetic_data$discharge_disposition_id[diabetic_data$discharge_disposition_id %in% c(20,12,16,27,10,19,17,9,24,15,8,28,25)]<-25
diabetic_data<-diabetic_data%>%filter(discharge_disposition_id != 11) 

###admission_source_id
diabetic_data%>%group_by(admission_source_id)%>%summarise(n=n())%>%arrange(n)
diabetic_data<-diabetic_data%>%filter(!admission_source_id %in% c(11,13,14,25,10,22,8))

###category_diag_1
diabetic_data%>%group_by(category_diag_1)%>%summarise(n=n())%>%arrange(n)
diabetic_data<-diabetic_data%>%filter(!category_diag_1 %in% c("E000-E999","?","740-759"))

###category_diag_2
diabetic_data%>%group_by(category_diag_2)%>%summarise(n=n())%>%arrange(n)
diabetic_data$category_diag_2[diabetic_data$category_diag_2 == "740-759"]<-'?'

###category_diag_3
diabetic_data%>%group_by(category_diag_3)%>%summarise(n=n())%>%arrange(n)
diabetic_data$category_diag_3[diabetic_data$category_diag_3 == "740-759"]<-'?'

###A1C result
diabetic_data%>%group_by(A1Cresult)%>%summarise(n=n())%>%arrange(n)

###metformin
diabetic_data%>%group_by(metformin)%>%summarise(n=n())%>%arrange(n) 

### glipizide
diabetic_data%>%group_by(glipizide)%>%summarise(n=n())%>%arrange(n)

### glyburide
diabetic_data%>%group_by(glyburide)%>%summarise(n=n())%>%arrange(n)

###pioglitazone
diabetic_data%>%group_by(pioglitazone)%>% summarise(n=n())%>%arrange(n)

###rosiglitazone
diabetic_data%>%group_by(rosiglitazone)%>%summarise(n=n())%>%arrange(n)

###insulin
diabetic_data%>%group_by(insulin)%>%summarise(n=n())%>%arrange(n)

###change
diabetic_data%>%group_by(change)%>%summarise(n=n())%>%arrange(n)

###diabetesMed
diabetic_data%>%group_by(diabetesMed)%>%summarise(n=n())%>%arrange(n)

##Centering and Scaling the numeric predictors

diabetic_data<- diabetic_data%>%
  mutate(
    time_in_hospital=(time_in_hospital-mean(time_in_hospital))/sd(time_in_hospital),
    num_lab_procedures=(num_lab_procedures-mean(num_lab_procedures))/sd(num_lab_procedures),
    num_procedures=(num_procedures-mean(num_procedures))/sd(num_procedures),
    num_medications=(num_medications-mean(num_medications))/sd(num_medications),
    number_outpatient=(number_outpatient-mean(number_outpatient))/sd(number_outpatient),
    number_emergency=(number_emergency-mean(number_emergency))/sd(number_emergency),
    number_inpatient=(number_inpatient-mean(number_inpatient))/sd(number_inpatient),
    number_diagnoses=(number_diagnoses-mean(number_diagnoses))/sd(number_diagnoses)
    )
## converting categorical features to factors
diabetic_data<-diabetic_data%>%
  mutate(race=as.factor(race),
         gender=as.factor(gender),
         age=as.factor(age),
         admission_type_id=as.factor(admission_type_id),
         discharge_disposition_id=as.factor(discharge_disposition_id),
         admission_source_id=as.factor(admission_source_id),
         category_diag_1=as.factor(category_diag_1),
         category_diag_2=as.factor(category_diag_2),
         category_diag_3=as.factor(category_diag_3),
         A1Cresult=as.factor(A1Cresult),
         metformin=as.factor(metformin),
         glipizide=as.factor(glipizide),
         glyburide=as.factor(glyburide),
         pioglitazone=as.factor(pioglitazone),
         rosiglitazone=as.factor(rosiglitazone),
         insulin=as.factor(insulin),
         change=as.factor(change),
         diabetesMed=as.factor(diabetesMed),
         readmitted=as.factor(readmitted))


# Split data for validation, 10% of diabetic_data
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
validate_index <- createDataPartition(y = diabetic_data$readmitted,times=1,p=0.1,list=FALSE)
validation <- diabetic_data[validate_index,]
diabetes <- diabetic_data[-validate_index,]



# Analysis of features

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
plot_proportion_analysis(a_race)+ggtitle("Race")

## analysis of readmitted with gender
a_gender<- proportion_analysis("gender")
plot_proportion_analysis(a_gender)+ggtitle("Gender")

## analysis of readmitted with age
a_age <- proportion_analysis("age")
plot_proportion_analysis(a_age)+ggtitle("Age")

## analysis of readmitted with admission type 
a_adm_type<-proportion_analysis('admission_type_id')
plot_proportion_analysis(a_adm_type)+ggtitle("Admission Type")

##analysis of readmitted with discharge disposition
a_discharge_disp<-proportion_analysis('discharge_disposition_id')
plot_proportion_analysis(a_discharge_disp)+ggtitle("Discharge Disposition")

## analysis of readmitted with admission source
a_adm_source<- proportion_analysis('admission_source_id')
plot_proportion_analysis(a_adm_source)+ggtitle("Admission Source")

##analysis of readmitted and time spent in hospital
diabetes%>%
  ggplot(aes(x=readmitted,y=time_in_hospital))+
  geom_boxplot()+
  ggtitle("Time Spent in Hospital")

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
  ggplot(aes(readmitted,value))+
  geom_boxplot()+
  scale_y_continuous(trans = 'log2')+
  facet_wrap(~key,ncol=3)+
  ggtitle("Procedures, Medications,Outpatient, Emergency, Inpatient")

##analysis of readmitted with category_diag_1, category_diag_2 and category_diag_3
diabetes%>%
  select(metformin,glipizide,glyburide,pioglitazone,rosiglitazone,insulin,readmitted)%>%
  gather(test,result,-readmitted)%>%
  group_by(test,result)%>%
  summarise(single_visit=mean(readmitted=='NO'),
            greater_30_visits=mean(readmitted=='>30'),
            lesser_30_visits=mean(readmitted=='<30'))%>%
  gather(visit,value,single_visit:lesser_30_visits)%>%
  ggplot(aes(x=visit,y=value,color=result))+
  geom_jitter()+
  scale_x_discrete(label=abbreviate)+
  facet_wrap(~test,ncol =3)+
  ggtitle("metformin,glipizide,glyburide,pioglitazone,\n rosiglitazone & insulin")


## analysis of readmitted with number of diagnosis
diabetes%>% 
  ggplot(aes(x=readmitted,y=number_diagnoses))+
  geom_boxplot()+
ggtitle("Number of Diagnosis")




## analysis of readmitted with A1Cresult
a_a1c_result<-proportion_analysis('A1Cresult')
plot_proportion_analysis(a_a1c_result)+
  ggtitle("A1C Result")

## tests with metformin,glipizide,glyburide,pioglitazone,rosiglitazone & insulin

diabetes%>%
  select(metformin,glipizide,glyburide,pioglitazone,rosiglitazone,insulin,readmitted)%>%
  gather(test,result,-readmitted)%>%
  group_by(test,result)%>%
  summarise(single_visit=mean(readmitted=='NO'),
            greater_30_visits=mean(readmitted=='>30'),
            lesser_30_visits=mean(readmitted=='<30'))%>%
  gather(visit,value,single_visit:lesser_30_visits)%>%
  ggplot(aes(x=visit,y=value,color=result))+
  geom_jitter()+
  scale_x_discrete(label=abbreviate)+
  facet_wrap(~test,ncol =3)+
  ggtitle("metformin,glipizide,glyburide,pioglitazone,\n rosiglitazone & insulin")


## analysis of readmitted with change
a_change<-proportion_analysis('change')
plot_proportion_analysis(a_change)+
  ggtitle("Change")

##analysis of readmitted with diabetes medicine
a_diabetes_med<- proportion_analysis('diabetesMed')
plot_proportion_analysis(a_diabetes_med)+
  ggtitle("Diabetes Medicine")

# Modeling

## split data to train and test
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
test_index<- createDataPartition(diabetes$readmitted,times=1,p=0.3,list=FALSE)
test_set <- diabetes[test_index,]
train_set<- diabetes[-test_index,]

##Linear Discriminant Analysis
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
fit_lda<-train_set%>%
  train(readmitted ~ . - 
          encounter_id -
          patient_nbr - 
          num_procedures -
          num_medications -
          number_outpatient -
          number_diagnoses,
        method='lda',
        data=.)
predictions_lda<-predict(fit_lda,newdata = test_set,type = 'raw')
confusionMatrix(predictions_lda,reference = test_set$readmitted)

##Quadratic Discriminant Analysis
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
fit_qda<-train_set%>%train(readmitted ~ . - 
                             encounter_id -
                             patient_nbr - 
                             num_procedures -
                             num_medications -
                             number_outpatient -
                             number_diagnoses,
                           method='qda',
                           data=.)
predictions_qda<-predict(fit_qda,newdata = test_set,type = 'raw')
confusionMatrix(predictions_qda,reference = test_set$readmitted)

##KNN
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
control_knn<-trainControl(method = 'cv',number = 5,p=0.9)
fit_knn<-train_set%>%train(readmitted ~ . - 
                             encounter_id -
                             patient_nbr - 
                             num_procedures -
                             num_medications -
                             number_outpatient -
                             number_diagnoses,
                           method='knn',
                           trControl=control_knn,
                           tuneGrid=data.frame(k=seq(100,120,4)),
                           data=.)
predictions_knn<-predict(fit_knn,newdata = test_set,type = 'raw')
confusionMatrix(predictions_knn,reference = test_set$readmitted)

# Result 
set.seed(1, sample.kind="Rounding")
#### if using R 3.5 or earlier, use `set.seed(1)` instead
fit_qda<-diabetes%>%train(readmitted ~ . - 
                            encounter_id -
                            patient_nbr - 
                            num_procedures -
                            num_medications -
                            number_outpatient -
                            number_diagnoses,
                          method='qda',
                          data=.)
predictions_qda<-predict(fit_qda,newdata = validation,type = 'raw')
confusionMatrix(predictions_qda,reference = validation$readmitted)
