library(haven) #import data
library(tidyverse)
library(dplyr) #data mining/ data cleaning
library(naniar) #replace with NA
library(leaps) #best subset
library(glmnet) # lasso
library(caret) # KNN 
library(ggplot2) # plotting results
library(randomForest) # Random forest classified
library(pROC) # AU-ROC curves

# set your personal working directory where this data lives setwd("~/Downloads/ML project/Data")
data <- read_dta("TEDS-D-2006-2011-DS0001-data-stata.dta")

#this step is to eliminate redundant/irrelevant columns 
new_data <- subset(data, select = -c(CASEID, DETNLF,PMSA,DETNLF, STFIPS,CBSA,REGION,
                                     DIVISION, SERVSETD,DETCRIM,ROUTE1,FRSTUSE1,
                                     ROUTE2,FRSTUSE2,ROUTE3,FRSTUSE3,
                                     ALCFLG,COKEFLG,MARFLG,HERFLG,METHFLG,OPSYNFLG,
                                     PCPFLG,HALLFLG,MTHAMFLG,AMPHFLG,STIMFLG,BENZFLG,TRNQFLG,
                                     BARBFLG,SEDHPFLG,INHFLG,OTCFLG,OTHERFLG,DSMCRIT,
                                     PRIMPAY, DAYWAIT,LOS,FREQ2,FREQ3,PRIMINC,HLTHINS,PREG, SUB2, SUB3))
#recode for NA based on codebook
new_data <- replace_with_na(new_data, replace= list(DISYR=-9, AGE=-9, GENDER=-9, PSYPROB=-9,
                                                    RACE=-9, ETHNIC=-9, MARSTAT=-9, EDUC=-9, NUMSUBS=-9,
                                                    EMPLOY=-9, VET=-9, LIVARAG=-9, ARRESTS=-9, ARRESTS=-8,
                                                    METHUSE=-9, PSOURCE=-9, FREQ1=-9, IDU=-9, IDU=-8,
                                                    REASON=-9, NOPRIOR=-9, SUB1=-9, ALCDRUG=-9))

# Creating factor levels for categorical variables
index <- 1:ncol(new_data)
new_data[ , index] <- lapply(new_data[ , index], as.factor)
str(new_data)
summary(new_data)

###Mode imputation for NA values since all variables are categorical
# Checking proportions to determine mode for NAs in the variables
prop.table(table(new_data$GENDER))# 1 i.e Male is the mode
new_data$GENDER[which(is.na(new_data$GENDER))] <- 1 # Populating all NAs with Male
prop.table(table(new_data$GENDER, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$RACE)) # 5 i.e White is the mode
new_data$RACE[which(is.na(new_data$RACE))] <- 5
prop.table(table(new_data$RACE, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$ETHNIC)) # 5 i.e Not of hispanic origin is the mode
new_data$ETHNIC[which(is.na(new_data$ETHNIC))] <- 5
prop.table(table(new_data$ETHNIC, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$MARSTAT))# 1 i.e Never Married is the mode
new_data$MARSTAT[which(is.na(new_data$MARSTAT))] <- 1
prop.table(table(new_data$MARSTAT, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$EDUC))# 3 i.e 12 years is the mode
new_data$EDUC[which(is.na(new_data$EDUC))] <- 3
prop.table(table(new_data$EDUC, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_employ <- table(new_data$EMPLOY)
prop.table(table_employ) #3 and 4 are close and so we populate the NAs randomly between these two categories
ind_na <- which(is.na(new_data$EMPLOY))
ind_na_half <- sample(ind_na, size = round(length(ind_na)/2,0), replace = F)
new_data$EMPLOY[ind_na_half] <- 3
new_data$EMPLOY[is.na(new_data$EMPLOY)] <- 4
prop.table(table(new_data$EMPLOY, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$LIVARAG)) # 3 i.e Independent Living
new_data$LIVARAG[which(is.na(new_data$LIVARAG))] <- 3
prop.table(table(new_data$LIVARAG, exclude=NULL)) # to ensure no NAs in post-imputation variable set

prop.table(table(new_data$ARRESTS)) #  0 i.e None
new_data$ARRESTS[which(new_data$ARRESTS==-8)] <- NA
new_data$ARRESTS[which(is.na(new_data$ARRESTS))] <- 0
prop.table(table(new_data$ARRESTS, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_methuse <- table(new_data$METHUSE)
prop.table(table_methuse)*100 
prop.table(table(new_data$METHUSE))# 2 i.e No
new_data$METHUSE[which(is.na(new_data$METHUSE))] <- 2
prop.table(table(new_data$METHUSE, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_reason <- table(new_data$REASON)
prop.table(table_reason)*100 # 1 i.e Treatment Completed
new_data$REASON[which(is.na(new_data$REASON))] <- 1
prop.table(table(new_data$REASON, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_psource <- table(new_data$PSOURCE)
prop.table(table_psource)*100 # 
ind_na2 <- which(is.na(new_data$PSOURCE)) # 1 and 7 are close so we populate the NAs randomly between these two categories
ind_na_half2 <- sample(ind_na2, size = round(length(ind_na2)/2,0), replace = F)
new_data$PSOURCE[ind_na_half2] <- 1
new_data$PSOURCE[is.na(new_data$PSOURCE)] <- 7
prop.table(table(new_data$PSOURCE, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_sub1 <- table(new_data$SUB1)
prop.table(table_sub1)*100 # 2 i.e Alcohol
new_data$SUB1[which(is.na(new_data$SUB1))] <- 2
prop.table(table(new_data$SUB1, exclude=NULL)) # to ensure no NAs in post-imputation variable set

table_freq1 <- table(new_data$FREQ1)
prop.table(table_freq1)*100 # 5 i.e daily
new_data$FREQ1[which(is.na(new_data$FREQ1))] <- 5
prop.table(table(new_data$FREQ1,exclude=NULL)) # to ensure no NAs in post-imputation variable set

# No NAs in NUMSUBS, IDU and ALCDRUG

table_psyprob <- table(new_data$PSYPROB)
prop.table(table_psyprob)*100 # 2 i.e No
new_data$PSYPROB[which(is.na(new_data$PSYPROB))] <- 2
prop.table(table(new_data$PSYPROB, exclude=NULL)) # to ensure no NAs in post-imputation variable set

new_data2 <- new_data%>%filter(., VET==1)
new_data2<- new_data2 %>% drop_na(., NOPRIOR)
new_data2 <- new_data2 %>% filter(., AGE!=2)
new_data3 <- new_data2
new_data3 <- new_data3 %>% mutate(., READMIT = ifelse(NOPRIOR==0, 0, 1)) %>% select(., -NOPRIOR)
#new variable READMIT is computed as 0 for NOPRIOR==0 (meaning no readmissions) and 1 for NOPRIOR>0 (meaning readmissions) 

new_data3$READMIT <- as.factor(new_data3$READMIT)
new_data3[ ,index] <- lapply(new_data3[ ,index], as.factor) #to ensure all variables will be treated as categorical variables
lapply(new_data3[ ,index], is.factor)

###Lasso feature selection
n <- nrow(new_data3)
set.seed(1)
sample <- sample(n/2, replace=F) #Training dataset sampled randomly to be 50% of the working dataset

tr_dat <- new_data3[sample, ]
te_dat <- new_data3[-sample, ]

lasso_x_tr <- as.matrix(tr_dat[, -21])
lasso_y_tr <- tr_dat[, 21, drop = T]
lasso_x_te <- as.matrix(te_dat[, -21])
lasso_y_te <- te_dat[, 21, drop = T]

cv_fit_lasso <- cv.glmnet(data.matrix(tr_dat[, -21]), factor(lasso_y_tr), family="binomial", alpha=1)
lasso_lambda <- cv_fit_lasso$lambda.min
lasso_coef <- coef(cv_fit_lasso,lasso_lambda)
lasso.model <- glmnet(data.matrix(tr_dat[, -21]), factor(lasso_y_tr),  alpha=1, family="binomial",lambda = cv_fit_lasso$lambda.min)

lasso_tr_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(tr_dat[, -21]), type="class")
lasso_te_pred <- predict(cv_fit_lasso, s=lasso_lambda, newx =data.matrix(te_dat[, -21]), type="class")

lasso_tr_error <- mean(lasso_tr_pred !=lasso_y_tr)
lasso_te_error <- mean(lasso_te_pred !=lasso_y_te)
lasso_tr_error
lasso_te_error

cbind(lasso_coef)

###KNN prediction model
tr_dat2 <- tr_dat %>% select(-c(VET, ETHNIC, ARRESTS,ALCDRUG))
te_dat2 <- te_dat %>% select(-c(VET, ETHNIC, ARRESTS,ALCDRUG))

k_seq <- c(5, seq(10,100, by=10))

te_error<- c()

for(i in seq_along(k_seq)){
  knn_fit <-caret::knn3(READMIT~., data=tr_dat2 ,k=k_seq[i])
  knn_pred <- predict(knn_fit, newdata= te_dat2, type="class")
  te_error[i] <- mean(knn_pred != te_dat2$READMIT)
}

df <- data.frame(k_seq, te_error)

ggplot(data = df, mapping = aes(x=k_seq, y=te_error))+geom_line(col="red")+geom_point()+xlab("k value")+ylab("Testing error")

### 10-fold Cross-Validation
new_data3 <- new_data3 %>% select(-c(VET, ETHNIC, ARRESTS,ALCDRUG))

Kfold <- 10

set.seed(1)
n_sample <- sample(1:Kfold,nrow(new_data3), replace = T)

mean(sapply(1:Kfold, function(v){
  knn_fit <-knn3(READMIT~., data=new_data3[n_sample !=v,] ,k=50)
  knn_pred <- predict(knn_fit, newdata= new_data3[n_sample ==v,], type="class")
  te_error <- mean(knn_pred != new_data3[n_sample ==v,]$READMIT)
}))

#these steps are to construct confusionMatrix of cross-validation
CV_knn <- sapply(1:Kfold, function(v){
  knn_fit <-knn3(READMIT~., data=new_data3[n_sample !=v,] ,k=50)
  knn_pred <- predict(knn_fit, newdata= new_data3[n_sample ==v,], type="class")
  return(data.frame(knn_pred, real = new_data3[n_sample ==v,]))
})
CV_matrix<-CV_knn
CV_pred<-cbind(knn_pred = CV_knn[[1]])%>%recode(., '1'='0', '2'='1') %>%as.numeric() %>%as.data.frame()
CV_real <- cbind(knn_real = CV_knn[[nrow(CV_knn)]])%>%recode(., '1'='0', '2'='1') %>%as.numeric() %>%as.data.frame()
CV_matrix <- data.frame(knn_pred=CV_pred,knn_real=CV_real) %>% rename(., "knn_pred"=., "knn_real"='..1')
table(CV_matrix$knn_pred,CV_matrix$knn_real) %>%confusionMatrix()

### RandomForest classifier 
set.seed(1)
rf.model1 <- randomForest(READMIT~ ., data = tr_dat2, importance = TRUE) #randomForest model with default hyperparameter values

#randomForest model extending no.of trees value to 1000 to check for variance in OOB error
rf.model2 <- randomForest(READMIT~ ., data = tr_dat2, importance = TRUE, ntree=1000) 
oob.error.data <- data.frame(Trees=c(1:nrow(rf.model2$err.rate)),"OOB error" =rf.model2$err.rate[, "OOB"], "Not Readmission" =rf.model2$err.rate[, "0"], "Readmission" =rf.model2$err.rate[, "1"])
oob.error.data$Trees[which.min(oob.error.data$OOB.error)]
ggplot(data = oob.error.data, mapping = aes(x=Trees, y=`OOB.error`))+geom_line()

#these steps extend the feature subset hyperparameter past default value of sqrt(p) for categorical variables to check for variance in OOB error
mtry <- 1:10
oob.values <- vector(length = 10)
for(i in 1:10){
  temp.rf <- randomForest(READMIT~., data = tr_dat2, mtry=i, ntree=700)
  oob.values[i] <- temp.rf$err.rate[nrow(temp.rf$err.rate),1]
}
which.min(oob.values)
data.frame(mtry,oob.values)

#final randomForest model with tuned hyperparameters
set.seed(1)
rf.model <- randomForest(READMIT~ ., data = tr_dat2, importance = TRUE, mtry= 4, ntree=700)
yhat_te <- predict(rf.model, newdata = te_dat2, type = "class")
te_error <- mean(yhat_te != te_dat2$READMIT)
te_error

confusionMatrix(yhat_te,te_dat2$READMIT)
rf.model

importance(rf.model) #importance of features for classification
varImpPlot(rf.model)

###recoding to consider READMIT==1 as positive class
te_dat2$READMIT <- ordered(te_dat2$READMIT, levels = c(1,0))
yhat_te.or <- ordered(yhat_te, levels= c(1,0))

CV_matrix2 <- CV_matrix
CV_matrix2$knn_pred <- ordered(CV_matrix2$knn_pred, levels= c(1,0))
CV_matrix2$knn_real <- ordered(CV_matrix2$knn_real, levels= c(1,0))

confusionMatrix(CV_matrix2$knn_pred, CV_matrix2$knn_real)
confusionMatrix(yhat_te.or, te_dat2$READMIT)

### AUC-ROC plot for model evaluation
roc_knn <- roc(CV_matrix$knn_pred, CV_matrix$knn_real)

te_dat2$READMIT <- ordered(te_dat2$READMIT, levels = c(0,1))
roc_rf <- roc(yhat_te, te_dat2$READMIT)

roc_label <- c(paste0("KNN",", AUC =",paste(round(roc_knn$auc,3))),paste0("randomForest",", AUC =",paste(round(roc_rf$auc,3))))
ggroc(list("KNN"=roc_knn, "randomForest"=roc_rf))+ scale_color_discrete(labels=roc_label)