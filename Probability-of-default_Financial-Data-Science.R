#------------------------------------------------------------------------------------ #
#     Financial Data Science - Group Project
#     NOVA IMS
#     Group Elements (FDS3): Carlos Cardoso | 20211220   Carlota Reis | 20211208   
#                            Guilherme Miranda | 20210420    Mariana Garcia | 20210838 
#------------------------------------------------------------------------------------ #

rm(list=ls(all=TRUE))
options(scipen = 999)

#-------------------------------------------------
# Loading Libraries
#-------------------------------------------------

library("pacman")
library("Hmisc")
library("dplyr")
library("ggplot2")
library("gmodels")
library("aod")
library("pROC")
library("ROCR")
library("psych")
library("summarytools")
library("generalhoslem")
library("DataExplorer")
library("tree")



#Import the raw data

train_data <- read.csv("train_Kaggle_BIO_fall21.csv", sep = ",", stringsAsFactors = T)
test_data <- read.csv("test_Kaggle_BIO_fall21.csv", sep = ",", stringsAsFactors = T)


#-------------------------------------------------
#Exploratory Data Analysis
#-------------------------------------------------

#1.Data description

View(train_data)
str(train_data)
summary(train_data)
describe(train_data)

plot_histogram(train_data)


#Loan Term plot
ggplot(train_data, aes(term) ) + geom_bar(aes(fill =as.factor(term))) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(width=1),vjust=0.5) +
  scale_fill_discrete(name="Term") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold",size="12", margin=margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face="bold",size="12", margin=margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(hjust=0.5, face="bold", size = "15")) +
  labs(x= "Term",y= "Count" , title = "Loan Term")

#Home Ownership plot
ggplot(train_data, aes(home_ownership) ) + geom_bar(aes(fill =as.factor(home_ownership))) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(width=1),vjust=0.5) +
  scale_fill_discrete(name="Home Ownership") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold",size="12", margin=margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face="bold",size="12", margin=margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(hjust=0.5, face="bold", size = "15")) +
  labs(x= "Home Ownership",y= "Count" , title = "Home Ownership")

#Loan Grade plot
ggplot(train_data, aes(grade) ) + geom_bar(aes(fill =as.factor(grade))) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(width=1),vjust=0.5) +
  scale_fill_discrete(name="Grade") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold",size="12", margin=margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face="bold",size="12", margin=margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(hjust=0.5, face="bold", size = "15")) +
  labs(x= "Grade",y= "Count" , title = "Loan Grade")

#Employment title
train_data %>% 
  group_by(emp_title) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count), emp_title) %>%
  mutate(Percentage = Count/sum(Count)*100)

#Employment length
barplot(table(train_data$emp_length), main="Employment length in years",
        xlab = "emp_length", ylab = "Count")

#Annual Income
annual_inc_test <- ifelse(train_data$annual_inc<50000 | train_data$annual_inc==50000, "0-50000",
                          ifelse(train_data$annual_inc>50000 & train_data$annual_inc<100000 | train_data$annual_inc==100000, "50000-100000",
                                 ifelse(train_data$annual_inc>100000,"100000+",NA)))
barplot(table(annual_inc_test), main= "Annual Income",
        xlab = "Anual Income", ylab = "Count")
rm(annual_inc_test)

#Address State
train_data %>% 
  group_by(addr_state) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count), addr_state) %>%
  mutate(Percentage = Count/sum(Count)*100)

#Purpose
ggplot(train_data, aes(purpose) ) + geom_bar(aes(fill =as.factor(purpose))) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5)) +
  scale_fill_discrete(name="Purpose") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold",size="12", margin=margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face="bold",size="12", margin=margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(hjust=0.5, face="bold", size = "15")) +
  labs(x= "Purpose",y= "Count" , title = "Loan Purpose")

#Risk
ggplot(train_data, aes(risk) ) + geom_bar(aes(fill =as.factor(risk))) +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(width=1),vjust=0.5) +
  scale_fill_discrete(name="Risk") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold",size="12", margin=margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(face="bold",size="12", margin=margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(hjust=0.5, face="bold", size = "15")) +
  labs(x= "Risk",y= "Count" , title = "Risk")



#2. Missing values and Outliers
sum(is.na(train_data$loan_amnt))
sum(is.na(train_data$funded_amnt))
sum(is.na(train_data$funded_amnt_inv))
sum(is.na(train_data$term))
sum(is.na(train_data$int_rate))
sum(is.na(train_data$installment))
sum(is.na(train_data$grade))
sum(is.na(train_data$emp_title))
sum(is.na(train_data$emp_length))
sum(is.na(train_data$home_ownership))
sum(is.na(train_data$annual_inc))
sum(is.na(train_data$verification_status))
sum(is.na(train_data$issued_d))
sum(is.na(train_data$loan_status))
sum(is.na(train_data$purpose))
sum(is.na(train_data$addr_state))
sum(is.na(train_data$dti))
sum(is.na(train_data$delinq_2yrs))
sum(is.na(train_data$earliest_cr_line))
sum(is.na(train_data$inq_last_6mths))
sum(is.na(train_data$open_acc))
sum(is.na(train_data$pub_rec))
sum(is.na(train_data$revol_bal))
sum(is.na(train_data$revol_util))
sum(is.na(train_data$total_acc))
sum(is.na(train_data$out_prncp))
sum(is.na(train_data$total_pymnt))
sum(is.na(train_data$risk))


# replacing missing values with Average for dti
mean(train_data$dti, na.rm = T)
train_data$dti[is.na(train_data$dti)] <- mean(train_data$dti, na.rm = T)

# replacing missing values with Average for inq_last_6mths
mean(train_data$inq_last_6mths, na.rm = T)
train_data$inq_last_6mths[is.na(train_data$inq_last_6mths)] <- mean(train_data$inq_last_6mths, na.rm = T)

# replacing missing values with Average for revol_util
mean(train_data$revol_util, na.rm = T)
train_data$revol_util[is.na(train_data$revol_util)] <- mean(train_data$revol_util, na.rm = T)

boxplot(train_data, main="Original Boxplot")

boxplot(train_data$annual_inc) 
boxplot.stats(train_data$annual_inc)


# Remove all outliers suggested by boxplot for annual income
which(train_data$annual_inc==300000)
train_data_without_outliers <- subset(train_data, train_data$annual_inc < 300000)


boxplot(train_data_without_outliers)

n_distinct(train_data)
n_distinct(train_data_without_outliers)

# Remove all outliers suggested by boxplot for revolving balance
boxplot(train_data_without_outliers$revol_bal)
boxplot.stats(train_data_without_outliers$revol_bal)

which(train_data_without_outliers$revol_bal==133414)
train_data_without_outliers <- subset(train_data_without_outliers, train_data_without_outliers$revol_bal< 133414)
n_distinct(train_data)
n_distinct(train_data_without_outliers)

boxplot(train_data_without_outliers - train_data_without_outliers$id, main = "Boxplot of the data without outliers")


#4. Data transformation
train_data_2 <- train_data_without_outliers

empln <- train_data_2$emp_length
empln[empln == "n/a"] <- NA
empln <- droplevels(empln)
levels(empln) <- c("0", "1", "10", "2", "3", "4", "5", "6", "7", "8", "9")
empln <- as.numeric(levels(empln)[empln])
train_data_2$emp_length <- empln;
rm(empln)

trm <- train_data_2$term
trm[trm == "n/a"] <- NA
trm <- droplevels(trm)
levels(trm) <- c("3", "5")
trm <- as.numeric(levels(trm)[trm])
train_data_2$term <- trm;
rm(trm)

prp <- train_data_2$purpose
prp[prp == "n/a"] <- NA
prp <- droplevels(prp)
levels(prp) <- c("1", "2", "3","4", "5", "6", "7", "8", "9", "10", "11", "12","13")
prp <- as.numeric(levels(prp)[prp])
train_data_2$purpose <- prp;
rm(prp)

#Remove the NAs from the dataset (emp_length)
train_data_2 <- subset(train_data_2, train_data_2$emp_length > 0 |
                             train_data_2$emp_length == 0)

plot_correlation(na.omit(train_data_2))



#5. Chi-square P-Values for each contingency table
chisq.test(train_data_2$risk, train_data_2$loan_amnt)
chisq.test(train_data_2$risk, train_data_2$dti)
chisq.test(train_data_2$risk, train_data_2$term)
chisq.test(train_data_2$risk, train_data_2$annual_inc)
chisq.test(train_data_2$risk, train_data_2$emp_length)
chisq.test(train_data_2$risk, train_data_2$int_rate)
chisq.test(train_data_2$risk, train_data_2$open_acc)
chisq.test(train_data_2$risk, train_data_2$revol_util)
chisq.test(train_data_2$risk, train_data_2$purpose)
chisq.test(train_data_2$risk, train_data_2$pub_rec)
chisq.test(train_data_2$risk, train_data_2$installment)


loan_data = subset(train_data_2,select=c(id,loan_amnt,dti,term,annual_inc,emp_length,int_rate,open_acc,revol_util,purpose,pub_rec,installment,risk))

#6 Break Data into Training and Validation Sample

set.seed(123456)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(loan_data), nrow(loan_data)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Clear the R program
rm(d)
rm(tds)
rm(train_data)
rm(train_data_without_outliers)


#7 Linear Regression
mlr<- lm(risk ~ . -id, data=loan_data)
summary(mlr)


#8. Logistic Regression Model
#8.1 Logistic regression with no predictor variables
nopred <- glm(risk ~ 1 -id, family=binomial, data=loan_data)
summary(nopred)
coef(nopred) #beta0
exp(coef(nopred))/(1+exp(coef(nopred))) #probability of default


#Clear the R program
rm(nopred)

#8.2 regression for the full model
full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = train)
summary(full_model)


#8.3 Model with only interception
full_modelRed = glm(risk ~ 1 -id, family=binomial(link=logit), data = full_model$model)

#Deviance Table
anova(full_modelRed, full_model, test="Chisq")


# Odds-Ratio
exp(cbind(OddsRatio = coef(full_model), confint(full_model)))
exp(cbind(OddsRatio = coef(full_modelRed), confint(full_modelRed)))

# Probability of Default
CrossTable(full_model$model$risk)

#9. Tests of Logistic Regression Models' Validity
#9.1 Likelihood Ratio Test
logLik(full_model)
logLik(full_modelRed)
LR = 2*(logLik(full_model)-logLik(full_modelRed))
LR
rm(LR)

#9.2 PSEUDO R-2 (McFadden 1974)
1-(logLik(full_model))/(logLik(full_modelRed))

#9.3 Wald test
full_model$terms
wald.test(b = coef(full_model), Sigma = vcov(full_model), Terms = 2:5)
wald.test(b = coef(full_model), Sigma = vcov(full_model), Terms = 6:8)
wald.test(b = coef(full_model), Sigma = vcov(full_model), Terms = 9:12)



#9.4 AIC
AIC(full_model)
AIC(full_modelRed)


#9.5 Stepwise Procedures
#Backward Selection
backward <- step(full_model, direction="backward")
summary(full_model)
summary(backward)
AIC(backward)

#Forward Selection
forward <- step(full_model, direction="forward")
summary(forward)
AIC(forward)

#Stepwise Selection
reg <- step(full_model, scope=formula(full_model), direction="both", k=2)  # k=2 -> AIC
summary(reg)
AIC(reg)



#9.6 Haty and Confusion Matrix
haty <- predict(full_model, validation, type ='response')

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)

binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#Positive Predictive Value
u[2,2]/(u[2,2]+u[2,1])

#False Predictive Value
u[1,1]/(u[1,1]+u[1,2])

#False Positive Rate
1-cutoff[2,1]
#specificity
cutoff[2,1]

#False Negative Rate
1-cutoff[1,1]
#sensitivity
cutoff[1,1]

# Accuracy vs cutoff
acc.perf = performance(pred, "acc")
plot(acc.perf, col='blue', ylab="Accuracy", xlab="cutoff")

#F1-Score
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

# Precision/recall graphs:
perf.prec <- performance(pred, "prec", "rec")
plot(perf.prec, col='blue')

# The strategy table
pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate


#9.7 ROC Curve
validation$score <- predict(full_model, type='response', validation)
roccurve <- roc(validation$risk ~ validation$score)
plot(roccurve, col='blue', main='ROC curve')
auc(roccurve)


#10. Machine Learning model


#Sample 1
set.seed(123456)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(loan_data), nrow(loan_data)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#10.1 Decision Tree
first.tree <- tree(risk ~. -id,data=train)
summary(first.tree)
plot(first.tree)
text(first.tree,pretty=0)
first.tree
haty <- predict(first.tree, validation)
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)

binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#Positive Predictive Value
u[2,2]/(u[2,2]+u[2,1])

#Negative Predictive Value
u[1,1]/(u[1,1]+u[1,2])

#False Positive Rate
1-cutoff[2,1]

#False Negative Rate
1-cutoff[1,1]

# Accuracy vs cutoff
acc.perf = performance(pred, "acc")
plot(acc.perf, col='blue', ylab="Accuracy", xlab="cutoff")

#F1-Score
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

validation$score <- predict(first.tree, validation)
roccurve <- roc(validation$risk ~ validation$score)
plot(roccurve, col='blue', main='ROC curve')
auc(roccurve)

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

# Precision/recall graphs:
perf.prec <- performance(pred, "prec", "rec")
plot(perf.prec, col='blue')

#11. Discussion

#11.1 Cross validation

#Sample 2
set.seed(90)
tds = 2/3
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))
train <- loan_data[d,]
validation <- loan_data[-d,]

#Logistic Model
full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = train)
predfull <- predict(full_model, newdata = validation, type="response")
haty <- predict(full_model, validation, type ='response')
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)
binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)


# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate


#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(full_model, type='response', validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 2
set.seed(90)
tds = 2/3
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))
train <- loan_data[d,]
validation <- loan_data[-d,]

#Decision Tree
first.tree <- tree(risk ~. -id,data=train)
predfull <- predict(first.tree, newdata = validation)
haty <- predict(first.tree, validation)
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)
binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)


# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate


#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(first.tree, validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)


#Sample 3

set.seed(32)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Logistic Model
full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = train)
predfull <- predict(full_model, newdata = validation, type="response")
haty <- predict(full_model, validation, type ='response')
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)

binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(full_model, type='response', validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 3
set.seed(32)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Decision Tree
first.tree <- tree(risk ~. -id,data=train)
predfull <- predict(first.tree, newdata = validation)
haty <- predict(first.tree, validation)
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)
binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1])/dim(validation)[1]
acc.rate
#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(first.tree, validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 4
set.seed(15)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Logistic Model
full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = train)
predfull <- predict(full_model, newdata = validation, type="response")
haty <- predict(full_model, validation, type ='response')
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)

binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(full_model, type='response', validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 4
set.seed(15)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Decision Tree
first.tree <- tree(risk ~. -id,data=train)
predfull <- predict(first.tree, newdata = validation)
haty <- predict(first.tree, validation)
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)
binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1])/dim(validation)[1]
acc.rate

#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*0/(0+0))/(cutoff[1,1]+0/(0+0))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(first.tree, validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 5
set.seed(23897)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Logistic Model
full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = train)
predfull <- predict(full_model, newdata = validation, type="response")
haty <- predict(full_model, validation, type ='response')
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)

binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(full_model, type='response', validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)

#Sample 5
set.seed(47263)
tds = 2/3  # proportion in training data
d = sort(sample(nrow(train_data_2), nrow(train_data_2)*tds))

train <- loan_data[d,]
validation <- loan_data[-d,]

#Decision Tree
first.tree <- tree(risk ~. -id,data=train)
predfull <- predict(first.tree, newdata = validation)
haty <- predict(first.tree, validation)
pred <- prediction(haty, validation$risk)
perf <- performance(pred, "tpr", "fpr") 
cutoff <- opt.cut(perf, pred)
binhaty <- (haty > cutoff[3,1])
u <- table(binhaty, validation$risk)
u

# Model Accuracy rate
acc.rate = (u[1,1]+u[2,2])/dim(validation)[1]
acc.rate

#F1-Score (2*sensitivity*Positive Predictive Value)/(sensitivity+Positive Predictive Value)
2*(cutoff[1,1]*u[2,2]/(u[2,2]+u[2,1]))/(cutoff[1,1]+u[2,2]/(u[2,2]+u[2,1]))

pred_full_20 <- ifelse(haty > cutoff[3,1], 1, 0)
true_and_predval <- cbind(validation$risk, pred_full_20)
accepted_loans <- true_and_predval[pred_full_20 == 0, 1]
accepted_rate <- length(accepted_loans)/dim(validation)[1]
bad_rate <- sum(accepted_loans)/length(accepted_loans)
bad_rate

validation$score <- predict(first.tree, validation)
roccurve <- roc(validation$risk ~ validation$score)
auc(roccurve)



#Testing model on test data

#Final Results - test_data
test_data <- read.csv("test_Kaggle_BIO_fall21.csv", sep = ",", stringsAsFactors = T)

eln <- test_data$emp_length
eln[eln == "n/a"] <- NA
eln <- droplevels(eln)
levels(eln) <- c("0", "1", "10", "2", "3", "4", "5", "6", "7", "8", "9")
eln <- as.numeric(levels(eln)[eln])
test_data$emp_length <- eln;
rm(eln)

eln <- test_data$term
eln[eln == "n/a"] <- NA
eln <- droplevels(eln)
levels(eln) <- c("3", "5")
eln <- as.numeric(levels(eln)[eln])
test_data$term <- eln;
rm(eln)

eln <- test_data$purpose
eln[eln == "n/a"] <- NA
eln <- droplevels(eln)
levels(eln) <- c("1", "2", "3","4", "5", "6", "7", "8", "9", "10", "11", "12","13")
eln <- as.numeric(levels(eln)[eln])
test_data$purpose <- eln;
rm(eln)

full_model <- glm(risk ~ .-id, family = binomial(link=logit), data = loan_data)
test_data$score <- predict(full_model, type='response', test_data)

#cut-off: 0.3345
test_data$risk <- ifelse(test_data$score > 0.3345, 1, 0)
submission_file <- subset(test_data,select=c(id,risk))
write.csv(submission_file, file = "Submission file.csv")









