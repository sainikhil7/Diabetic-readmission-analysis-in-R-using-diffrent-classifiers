#set working directory
setwd("C:\\Users\\prtk1\\OneDrive\\Desktop\\predictive modelling\\presenatation\\dataset_diabetes\\dataset_diabetes\\")

#install required packages
#install.packages(c("anchors"))
#install.packages(c("mltools"))
#install.packages(c("tidyext"))
#install.packages(c("CRAN"))
#install.packages(c("ICSNP"))
#install.packages(c("RANN"))
#install.packages("klaR")

#import libraries
library(ICSNP)
library(purrr)
library(tibble)
library(tidyverse)
library(tidyr)
library(corrplot)
library(ggcorrplot)
library(timeDate)
library(magrittr)
library(ggfortify)
library(gridExtra)
library(forecast)
library(fpp2)
library(knitr)
library(GGally)
library(fma)
library(kableExtra)
library(e1071)
library(DataExplorer)
library(psych)
library(mlbench)
library(ggplot2)
library(questionr)
library(dplyr)
library(caret)
library(anchors)
library(RANN)
library(superml)
library(mltools)
library(CRAN)
library(reshape2)
library(pROC)
library(kernlab)
library(klaR)

#read dataset from the local file
diabetic <- read.csv(file = 'diabetic_data.csv')

#print dataset first 5 rows
head(diabetic)

#remove unwanted 'ID' columns
dim(diabetic)
diabetic = subset(diabetic, select = -c(encounter_id, patient_nbr))
dim(diabetic)

#remove extra IDs
dim(diabetic)
diabetic <- subset(diabetic, diabetic$discharge_disposition_id != c(11,13,14,19,20,21))
dim(diabetic)

#replacing target variable values to yes/no
head(diabetic$readmitted)
diabetic <- replace.value(diabetic,c("readmitted"),c("<30", ">30"),"Yes")
diabetic <- replace.value(diabetic,c("readmitted"),c("NO"),"No")
head(diabetic$readmitted)

## convert ? to NA:
sum(is.na(diabetic))
diabetic <- replace.value(diabetic,c("race","weight","payer_code","medical_specialty", "diag_1", "diag_2", "diag_3"),'?',as.double(NA))
sum(is.na(diabetic))

#removing zero variance columns
dim(diabetic)
nzv <- nearZeroVar(diabetic)
colnames(diabetic[nzv])
diabetic <- diabetic[-nzv]
dim(diabetic)

#individual predictors NA count
sum(is.na(diabetic$race))
sum(is.na(diabetic$weight))
sum(is.na(diabetic$payer_code))
sum(is.na(diabetic$medical_specialty))
sum(is.na(diabetic$diag_1))
sum(is.na(diabetic$diag_2))
sum(is.na(diabetic$diag_3))

plot_missing(diabetic)

#remove unwanted columns with null values
dim(diabetic)
diabetic = subset(diabetic, select = -c(diag_1,	diag_2, diag_3))
dim(diabetic)

#handle weight column
diabetic$weight <- ifelse(is.na(diabetic$weight), "No", "Yes")

#replace null with unknown category
diabetic <- replace.value(diabetic,c("race","payer_code","medical_specialty"),as.double(NA),'Unknown')

plot_missing(diabetic)

#concatenate 3 ID columns
diabetic$cat_3_cols <- paste(as.character(diabetic$admission_type_id), as.character(diabetic$discharge_disposition_id), as.character(diabetic$admission_source_id), sep="_")

#remove unwanted columns
dim(diabetic)
diabetic = subset(diabetic, select = -c(admission_type_id, discharge_disposition_id, admission_source_id))
dim(diabetic)


#create new variables based on datatype
num_cols = c('time_in_hospital', 'num_lab_procedures', 'num_procedures', 'num_medications', 'number_outpatient', 'number_emergency', 'number_inpatient','number_diagnoses')
#cat_cols = c('weight', 'race', 'gender',  'A1Cresult', 'metformin', 'glipizide', 'glyburide', 'pioglitazone', 'rosiglitazone', 'insulin', 'change', 'diabetesMed', 'payer_code')

#group by before adding others
diabetic %>%
  group_by(medical_specialty) %>%
  summarize(n())

#top10 = c('Unknown','Internal Medicine', 'Emergency/Trauma', 'Family/General Practice', 'Cardiology', 'Surgery-General', 'Nephrology', 'orthopedics', 'Orthopedics-Reconstructive', 'Radiologist')
#remaining <- unique(diabetic$medical_specialty)
#diabetic$medical_specialty[diabetic$medical_specialty != top10] <- "Others"

diabetic$medical_specialty[diabetic$medical_specialty != c("Unknown") & diabetic$medical_specialty != c("Internal Medicine") & diabetic$medical_specialty != c("Emergency/Trauma") & diabetic$medical_specialty != c("Family/General Practice") & diabetic$medical_specialty != c("Cardiology") & diabetic$medical_specialty != c("Surgery-General") & diabetic$medical_specialty != c("Nephrology") & diabetic$medical_specialty != c("orthopedics") & diabetic$medical_specialty != c("Orthopedics-Reconstructive") & diabetic$medical_specialty != c("Radiologist")] <- "Others"

#group by after adding others
diabetic %>%
  group_by(medical_specialty) %>%
  summarize(n())



#Histogram
diabetic %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


diabetic_copy <- diabetic

#dim(diabetic_cat)
#data split for one hot
diabetic_num <- subset(diabetic, select = num_cols)
diabetic_cat <- subset(diabetic, select = -c(readmitted, time_in_hospital, num_lab_procedures, num_procedures, num_medications, number_outpatient, number_emergency, number_inpatient,number_diagnoses))
#target <- subset(diabetic, select = c(readmitted))
target <- diabetic$readmitted


#create ID column for merging
diabetic_num$ID <- seq.int(nrow(diabetic_num))
diabetic_cat$ID <- seq.int(nrow(diabetic_cat))

#one hot encoding
diabetic_cat_ohe <- dcast(data = melt(diabetic_cat, id.vars = "ID"), ID ~ variable + value, length)

dim(diabetic_cat_ohe)

#save data
write.csv(diabetic_cat_ohe, "diabetic_cat_ohe_new.csv")

#read file from local
diabetic_cat_local <- read.csv(file = 'diabetic_cat_ohe_new.csv')
dim(diabetic_cat_local)

#merge cat and num data -- skip
#diabetic_merge <- merge(diabetic_num, diabetic_cat_local, by="ID")
#dim(diabetic_merge)

#remove one-hot extra columns
dim(diabetic_cat_local)
diabetic_cat_local = subset(diabetic_cat_local, select = -c(ID, age_.0.10.,A1Cresult_.7, race_AfricanAmerican, gender_Female,  payer_code_BC, medical_specialty_Cardiology,metformin_Down,glipizide_Down, glyburide_Down,pioglitazone_Down, rosiglitazone_Down,  insulin_Down,  change_Ch, diabetesMed_No, cat_3_cols_1_1_1))
dim(diabetic_cat_local)

#nzv
dim(diabetic_cat_local)
nzv <- nearZeroVar(diabetic_cat_local)
colnames(diabetic_cat_local[nzv])
diabetic_cat_local <- diabetic_cat_local[-nzv]
dim(diabetic_cat_local)




#correlation plots on num
dim(diabetic_num)

x <- cor(diabetic_num[1:8])
corrplot(x, type="upper", order="hclust")

#correlation plots on cat
y1 <- cor(diabetic_cat_local)
corrplot(y1, type="upper", order="hclust")

#correlation plots numeric
mcor<-round(cor(diabetic_num[1:8]),2)
mcor

mcor<-round(cor(diabetic_cat_local),2)
mcor


#remove highly correlated predictors
corThresh <- .9
hc <- findCorrelation(cor(diabetic_cat_local), corThresh)
length(hc)
dim(diabetic_cat_local)
diabetic_cat_local <- diabetic_cat_local[, -hc]
dim(diabetic_cat_local)
corrplot(cor(diabetic_cat_local), type="upper", order="hclust")


#Histogram extra
diabetic_num[1:8] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#BoxCox transformation
trans <- preProcess(diabetic_num[1:8], method = c("center", "scale", "BoxCox"))
transformed <- predict(trans, diabetic_num[1:8])
transformed

transformed[1:8] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

#skew
skew(diabetic_num[1:8], na.rm = TRUE)
skew(transformed, na.rm = TRUE)
names(transformed)

#boxplot before
diabetic_num[1:8] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot()

#boxplot after
transformed %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot()


#PCA transformation
#trans <- preProcess(diabetic_num[1:8], method = c("pca"))
#transformed_pca <- predict(trans, diabetic_num[1:8])
#transformed_pca

#skew(transformed_pca, na.rm = TRUE)

#x <- cor(transformed_pca)
#corrplot(x, type="upper", order="hclust")

#pca another method with summary
pca <- prcomp(diabetic_num, scale = TRUE)
summary(pca)

#add ID column in transformed data
dim(transformed)
transformed$X <- seq.int(nrow(transformed))
dim(transformed)

#merge cat and num (final) data
diabetic_cat_local$X <- seq.int(nrow(diabetic_cat_local))
diabetic_merge_final <- merge(transformed, diabetic_cat_local, by="X")
dim(diabetic_merge_final)

diabetic_merge_final <- subset(diabetic_merge_final, select = -c(X))


pca <- prcomp(diabetic_merge_final, scale = TRUE)
summary(pca)
diabetic_merge_final_pca <- pca$x[,1:28]

write.csv(diabetic_merge_final_pca, "diabetic_merge_final_pca.csv")

dim(diabetic_merge_final_pca)
length(target)

#diabetic_merge_final$readmitted = target
#write.csv(diabetic_merge_final, "diabetic_cat_ohe_11.csv")

########### Resampling #########

## The caret package has various functions for data splitting. For example, to
## use repeated training/test splits, the function createDataPartition could be
## used again with an additional argument named times to generate multiple
## splits.


## The caret package has functions createResamples (for bootstrapping),
## createFolds (for k-fold cross-validation) and createMultiFolds (for repeated
## cross-validation). To create indicators for 10-fold cross-validation.

set.seed(1000)

shuffle <- sample(nrow(diabetic_merge_final_pca))
length(shuffle)
shuffle <- head(shuffle, length(shuffle)*0.2)
length(shuffle)
shuffle


diabetic_merge_final_pca <- diabetic_merge_final_pca[shuffle,]
target <- target[shuffle]
dim(diabetic_merge_final_pca)
length(target)

fold1 <- createDataPartition(target, 
                             p = .8, 
                             list = FALSE)

## train
train_X <- diabetic_merge_final_pca[fold1,]
train_y <- target[fold1]

## test
test_X <- diabetic_merge_final_pca[-fold1,]
test_y <- target[-fold1]

train_y <- as.factor(train_y)
test_y <- as.factor(test_y)


#model1 - Logistic Regression
set.seed(1234)

ctrl <- trainControl(method = "cv", number=5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

logistic <- train(x = train_X,
                  y = train_y,
                  method = 'glm',
                  preProc=c("center","scale"),
                  metric = 'ROC',
                  trControl = ctrl)

logistic

pred_logistic_train <- predict(logistic, train_X)
confusionMatrix(data = pred_logistic_train, 
                reference = train_y)



pred_logistic_test <- predict(logistic, test_X)
confusionMatrix(data = pred_logistic_test, 
                reference = test_y)



pred_logistic_train_prob <- predict(logistic, train_X, type="prob") 
pred_logistic_test_prob <- predict(logistic, test_X, type="prob" )

logistic_roc_train <- roc(response=train_y, predictor=pred_logistic_train_prob[,1] )
plot(logistic_roc_train)
logistic_roc_train$auc[1]

logistic_roc_test <- roc(response=test_y, predictor=pred_logistic_test_prob[,1] )
plot(logistic_roc_test)
logistic_roc_test$auc[1]

plot(logistic_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='Logistic Regresison')
lines(logistic_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)



#model2 - LDA
set.seed(1234)

ctrl <- trainControl(method = "cv", number=5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

lda <- train( x = train_X,
              y = train_y,
              method = 'lda',
              preProc=c("center","scale"),
              metric = 'ROC',
              trControl = ctrl)

lda
plot(lda)

pred_lda_train <- predict(lda, train_X)
confusionMatrix(data = pred_lda_train, 
                reference = train_y)

pred_lda_test <- predict(lda, test_X)
confusionMatrix(data = pred_lda_test, 
                reference = test_y)

pred_lda_train_prob <- predict(lda, train_X, type="prob") 
pred_lda_test_prob <- predict(lda, test_X, type="prob" )

lda_roc_train <- roc(response=train_y, predictor=pred_lda_train_prob[,1] )
plot(lda_roc_train)
lda_roc_train$auc[1]

lda_roc_test <- roc(response=test_y, predictor=pred_lda_test_prob[,1] )
plot(lda_roc_test)
lda_roc_test$auc[1]

plot(lda_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='LDA Model')
lines(lda_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)





#model3 - PLSDA
set.seed(1234)
pls_grid <- expand.grid(.ncomp=1:10)

ctrl <- trainControl(method = "cv", number=5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

pslda <- train(x = train_X,
               y = train_y,
               method = 'pls',
               metric = 'ROC',
               trControl = ctrl,
               preProc = c("center", "scale"),
               tuneGrid = pls_grid)

pslda
plot(pslda)


pred_pslda_train <- predict(pslda, train_X)
confusionMatrix(data = pred_pslda_train, 
                reference = train_y)

pred_pslda_test <- predict(pslda, test_X)
confusionMatrix(data = pred_pslda_test, 
                reference = test_y)

pred_pslda_train_prob <- predict(pslda, train_X, type="prob") 
pred_pslda_test_prob <- predict(pslda, test_X, type="prob" )

pslda_roc_train <- roc(response=train_y, predictor=pred_pslda_train_prob[,1] )
plot(pslda_roc_train)
pslda_roc_train$auc[1]

pslda_roc_test <- roc(response=test_y, predictor=pred_pslda_test_prob[,1] )
plot(pslda_roc_test)
pslda_roc_test$auc[1]

plot(pslda_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='PSLDA Model')
lines(pslda_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)




#model4 Penalized

set.seed(1234)

ctrl <- trainControl(method = "cv", number=5,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

penalized_grid <- expand.grid(.alpha = c(0, .1, .2, .4, .5, 0.75, 1),
                              .lambda = seq(.01, 0.2, length = 20))
penalized <- train(x = train_X,
                   y = train_y,
                   method = 'glmnet',
                   metric = 'ROC',
                   trControl = ctrl,
                   preProc = c("center", "scale"),
                   tuneGrid = penalized_grid)

penalized
plot(penalized)


pred_penalized_train <- predict(penalized, train_X)
confusionMatrix(data = pred_penalized_train, 
                reference = train_y)

pred_penalized_test <- predict(penalized, test_X)
confusionMatrix(data = pred_penalized_test, 
                reference = test_y)

pred_penalized_train_prob <- predict(penalized, train_X, type="prob") 
pred_penalized_test_prob <- predict(penalized, test_X, type="prob" )

penalized_roc_train <- roc(response=train_y, predictor=pred_penalized_train_prob[,1] )
plot(penalized_roc_train)
penalized_roc_train$auc[1]

penalized_roc_test <- roc(response=test_y, predictor=pred_penalized_test_prob[,1] )
plot(penalized_roc_test)
penalized_roc_test$auc[1]

plot(penalized_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='Penalized Model')
lines(penalized_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)


#model5 SVM

set.seed(1234)

ctrl <- trainControl(method = "cv", number=5, summaryFunction = twoClassSummary,
                     classProbs = TRUE)

sigmaRangeReduced <- sigest(train_X)

svm_grid <- expand.grid(.sigma = sigmaRangeReduced[1],
                        .C = 2^(seq(-4, 6)))
svm <- train(x = train_X,
             y = train_y,
             method = 'svmRadial',
             metric = 'ROC',
             trControl = ctrl,
             preProc = c("center", "scale"),
             fit = FALSE,
             tuneGrid = svm_grid)


svm
plot(svm)


pred_svm_train <- predict(svm, train_X)
confusionMatrix(data = pred_svm_train, 
                reference = train_y)

pred_svm_test <- predict(svm, test_X)
confusionMatrix(data = pred_svm_test, 
                reference = test_y)

pred_svm_train_prob <- predict(svm, train_X, type="prob") 
pred_svm_test_prob <- predict(svm, test_X, type="prob" )

svm_roc_train <- roc(response=train_y, predictor=pred_svm_train_prob[,1] )
plot(svm_roc_train)
svm_roc_train$auc[1]

svm_roc_test <- roc(response=test_y, predictor=pred_svm_test_prob[,1] )
plot(svm_roc_test)
svm_roc_test$auc[1]

plot(svm_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='svm Model')
lines(svm_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)



#model6 knn

set.seed(1234)

ctrl <- trainControl(method='cv', 
                     number=5, 
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

knn <- train(x = train_X,
             y = train_y,
             method = 'knn',
             metric = 'ROC',
             trControl = ctrl,
             preProc = c("center", "scale"),
             tuneGrid = data.frame(.k = 1:50))

knn
plot(knn)

pred_knn_train <- predict(knn, train_X)
confusionMatrix(data = pred_knn_train, 
                reference = train_y)

pred_knn_test <- predict(knn, test_X)
confusionMatrix(data = pred_knn_test, 
                reference = test_y)

pred_knn_train_prob <- predict(knn, train_X, type="prob") 
pred_knn_test_prob <- predict(knn, test_X, type="prob" )

knn_roc_train <- roc(response=train_y, predictor=pred_knn_train_prob[,1] )
plot(knn_roc_train)
knn_roc_train$auc[1]

knn_roc_test <- roc(response=test_y, predictor=pred_knn_test_prob[,1] )
plot(knn_roc_test)
knn_roc_test$auc[1]

plot(knn_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='knn Model')
lines(knn_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)






#model7 nb

set.seed(1234)

ctrl <- trainControl(method='cv', 
                     number=5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

nb <- train(x = train_X,
            y = train_y,
            method = 'nb',
            metric = 'ROC',
            preProc = c("center", "scale"),
            trControl = ctrl,
            tuneGrid = data.frame(.fL = 2,.usekernel = TRUE,.adjust = TRUE))


nb
plot(nb)


pred_nb_train <- predict(nb, train_X)
confusionMatrix(data = pred_nb_train, 
                reference = train_y)

pred_nb_test <- predict(nb, test_X)
confusionMatrix(data = pred_nb_test, 
                reference = test_y)

pred_nb_train_prob <- predict(nb, train_X, type="prob") 
pred_nb_test_prob <- predict(nb, test_X, type="prob" )

nb_roc_train <- roc(response=train_y, predictor=pred_nb_train_prob[,1] )
plot(nb_roc_train)
nb_roc_train$auc[1]

nb_roc_test <- roc(response=test_y, predictor=pred_nb_test_prob[,1] )
plot(nb_roc_test)
nb_roc_test$auc[1]

plot(nb_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='Naive Bayes Model')
lines(nb_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)





#model8 mda

set.seed(1234)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary) 

mda <- train(x = train_X,
             y = train_y,
             method = 'mda',
             metric = 'ROC',
             preProc = c("center", "scale"),
             trControl = ctrl,
             tuneGrid = expand.grid(.subclasses = 1:10))


mda

plot(mda)


pred_mda_train <- predict(mda, train_X)
confusionMatrix(data = pred_mda_train, 
                reference = train_y)

pred_mda_test <- predict(mda, test_X)
confusionMatrix(data = pred_mda_test, 
                reference = test_y)

pred_mda_train_prob <- predict(mda, train_X, type="prob") 
pred_mda_test_prob <- predict(mda, test_X, type="prob" )

mda_roc_train <- roc(response=train_y, predictor=pred_mda_train_prob[,1] )
plot(mda_roc_train)
mda_roc_train$auc[1]

mda_roc_test <- roc(response=test_y, predictor=pred_mda_test_prob[,1] )
plot(mda_roc_test)
mda_roc_test$auc[1]

plot(mda_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='mda Model')
lines(mda_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)





#model9 fda

set.seed(1234)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary) 

marsGrid <- expand.grid(.degree = 1:3, .nprune = 2:35)

fda <- train(x = train_X,
             y = train_y,
             method = 'fda',
             metric = 'ROC',
             preProc = c("center", "scale"),
             trControl = ctrl,
             tuneGrid = marsGrid)

fda
plot(fda)


pred_fda_train <- predict(fda, train_X)
confusionMatrix(data = pred_fda_train, 
                reference = train_y)

pred_fda_test <- predict(fda, test_X)
confusionMatrix(data = pred_fda_test, 
                reference = test_y)

pred_fda_train_prob <- predict(fda, train_X, type="prob") 
pred_fda_test_prob <- predict(fda, test_X, type="prob" )

fda_roc_train <- roc(response=train_y, predictor=pred_fda_train_prob[,1] )
plot(fda_roc_train)
fda_roc_train$auc[1]

fda_roc_test <- roc(response=test_y, predictor=pred_fda_test_prob[,1] )
plot(fda_roc_test)
fda_roc_test$auc[1]

plot(fda_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='fda Model')
lines(fda_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)




#model10 nnet

set.seed(1234)

ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE)
nnetGrid <- expand.grid(.size = 1:10, .decay = c(0, .1, 1, 2))			 
maxSize <- max(nnetGrid$.size)
numWts <- (maxSize * (dim(train_X)[2] + 1) + (maxSize+1)*2) ## 4 is the number of predictors


nnet <- train(x = train_X,
              y = train_y,
              method = 'nnet',
              metric = 'ROC',
              preProc = c("center", "scale", "spatialSign"),
              tuneGrid = nnetGrid,
              trace = FALSE,
              maxit = 500,
              MaxNWts = numWts,
              trControl = ctrl)

nnet
plot(nnet)


pred_nnet_train <- predict(nnet, train_X)
confusionMatrix(data = pred_nnet_train, 
                reference = train_y)

pred_nnet_test <- predict(nnet, test_X)
confusionMatrix(data = pred_nnet_test, 
                reference = test_y)

pred_nnet_train_prob <- predict(nnet, train_X, type="prob") 
pred_nnet_test_prob <- predict(nnet, test_X, type="prob" )

nnet_roc_train <- roc(response=train_y, predictor=pred_nnet_train_prob[,1] )
plot(nnet_roc_train)
nnet_roc_train$auc[1]

nnet_roc_test <- roc(response=test_y, predictor=pred_nnet_test_prob[,1] )
plot(nnet_roc_test)
nnet_roc_test$auc[1]

plot(nnet_roc_train,col="red",lty=1,ylab="Sensitivity",lwd=2,xlab="Specificity", main='nnet Model')
lines(nnet_roc_test,col="black",lty=1,ylab="Value",lwd=2,xlab="Year",xaxt="n")
grid()
legend("topleft",legend=c('train', 'test'),lty=c(1,1),col=c("red","black"),bg="white",lwd=2)

