#Load required packages
library(tidyverse)
library(psych)
library(Amelia)
library(mice)
library(ggplot2)
library(GGally)
library(caret)
library(caretEnsemble)
library(rpart)
library(randomForest)
library(dplyr)
library(Amelia)
library(MASS)
library(car)
library(caret)
library(ROCR)

#reading the dataset
loanData <- read.csv("C:/Users/noyon/Desktop/archive/train.csv", fileEncoding = 'UTF-8-BOM')

#setting vars as categorical
loanData$Property_Area <- factor(loanData$Property_Area, levels = c("Rural", "Semiurban","Urban"), labels = c(0,1,2))
loanData$Gender <- factor(loanData$Gender, levels = c("Male", "Female"), labels = c(1,2))
loanData$Married <- factor(loanData$Married, levels = c("No", "Yes"), labels = c(0,1))
loanData$Education <- factor(loanData$Education, levels = c("Not Graduate", "Graduate"), labels = c(0,1))
loanData$Self_Employed <- factor(loanData$Self_Employed, levels = c("No", "Yes"), labels = c(0,1))
loanData$Loan_Status <- factor(loanData$Loan_Status, levels = c("N","Y"), labels = c(0,1))
loanData$Dependents <- factor(loanData$Dependents, levels = c("0","1","2","3+"), labels = c(0,1,2,3))

str(loanData)
head(loanData)
describe(loanData)

#Convert categorical values and empty cells into null values
loanData[,][loanData[,] == ""] <- NA

#Visualize missing data
missmap(loanData,col=c("gray90", "blue"))

mice_plot <- aggr(loanData, col=c('green','black'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(loanData), cex.axis=.5,
                  gap=7, ylab=c("Missing value","Pattern"))


#Use mice package to predict missing values
mice_mod <- mice(loanData[, c("Gender","Married","Dependents","Education","Self_Employed","ApplicantIncome","CoapplicantIncome","LoanAmount","Loan_Amount_Term","Credit_History","Property_Area")], method='rf')
mice_complete <- complete(mice_mod)

#Transfer the predicted missing values into the data set
loanData$Credit_History <- mice_complete$Credit_History
loanData$Loan_Amount_Term <- mice_complete$Loan_Amount_Term
loanData$LoanAmount <- mice_complete$LoanAmount
loanData$Self_Employed <- mice_complete$Self_Employed
loanData$Property_Area <- mice_complete$Property_Area
loanData$Gender <- mice_complete$Gender
loanData$Married <- mice_complete$Married
loanData$Dependents <- mice_complete$Dependents

#Visualize it again to check missing data
missmap(loanData,col=c("gray90", "black"))

#look at the distribution of the data
barplot(table(loanData$Gender),col= "black", main="Gender factor")
barplot(table(loanData$Married),col= "black", main="Married factor")
barplot(table(loanData$Dependents),col= "black", main="Dependents factor")
barplot(table(loanData$Education),col= "black", main="Education factor")
barplot(table(loanData$Self_Employed),col= "black", main="Self_Employed factor")
barplot(table(loanData$Property_Area),col= "black", main="Property_Area factor")
boxplot(loanData$ApplicantIncome,loanData$CoapplicantIncome,names=c("App Income","Coapp Income"),main="Applicant Income - Coapplicant Income")
boxplot(loanData$LoanAmount,col= "black",main="Loan Amount")
boxplot(loanData$LoanAmount,loanData$Loan_Amount_Term,names=c("App Income","Coapp Income"),main="LoanAmount - Loan_Amount_Term")


#relationship between two variables. Specifically the dependent vs independent Variables
par(mfrow=c(2,3))

cnt <- table(loanData$Loan_Status, loanData$Gender)
barplot(cnt, main="Loan Status by Gender",
        xlab="Gender", col=c("blue","brown"),
        legend = rownames(cnt))

cnt2 <- table(loanData$Loan_Status, loanData$Education)
barplot(cnt2, main="Loan Status by Education",
        xlab="Education", col=c("blue","brown"),
        legend = rownames(cnt2))

cnt3 <- table(loanData$Loan_Status, loanData$Married)
barplot(cnt3, main="Loan Status by Married",
        xlab="Married", col=c("blue","brown"),
        legend = rownames(cnt3))

cnt4 <- table(loanData$Loan_Status, loanData$Self_Employed)
barplot(cnt4, main="Loan Status by Self Employed",
        xlab="Self_Employed", col=c("blue","brown"),
        legend = rownames(cnt4))

cnt5 <- table(loanData$Loan_Status, loanData$Property_Area)
barplot(cnt5, main="Loan Status by Property_Area",
        xlab="Property_Area", col=c("blue","brown"),
        legend = rownames(cnt5))

cnt6 <- table(loanData$Loan_Status, loanData$Credit_History)
barplot(cnt6, main="Loan Status by Credit_History",
        xlab="Credit_History", col=c("blue","brown"),
        legend = rownames(cnt5))


dev.off()






#Building a Logistic Model


data <- subset(loanData,select=c(2,3,5,6,7,8,10,12,13))

is.factor(data$Gender)
is.factor(data$Education)
contrasts(data$Gender)

data <- data[!is.na(data$Loan_Status),]
rownames(data) <- NULL

loanData$Loan_Status <- as.factor(loanData$Loan_Status)
loanData$Loan_Status <- as.factor(loanData$Loan_Status)
lr_model <- glm (Loan_Status ~ Dependents + Gender + Self_Employed + Married + Education + Property_Area + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History ,data = loanData, family = binomial)

logistic.reg <- lr_model %>% stepAIC(directiom = "both")

coef(logistic.reg)


prob.train <- predict(logistic.reg, newdata = loanData, type = "response")
prediction.train <- ifelse(prob.train > 0.5, '1', '0')
tab.train <-table(as.factor(loanData$Loan_Status), prediction.train)
confusionMatrix(tab.train, positive = "1")

prob <- predict(logistic.reg, newdata = loanData, type = "response")
prediction <- ifelse(prob >= 0.5, '1', '0')
tab <- table(as.factor(loanData$Loan_Status), prediction)
performance <- confusionMatrix(tab, positive = "1"); performance

# ROC and AUC
roc(loanData$Loan_Status ~ prob, plot = TRUE, print.auc = TRUE)


dev.off()



#Building a Random Forest Model
library(party)
library(randomForest)

# Create the forest.
rf <- randomForest(Loan_Status ~ Dependents + Gender + Self_Employed + Married + 
                     Education + Property_Area + ApplicantIncome + CoapplicantIncome 
                   + LoanAmount + Loan_Amount_Term + Credit_History, data=loanData , 
                   importance = TRUE)

rf.prob.train <- predict(rf, newdata = loanData, type = "prob")
pred.train <- ifelse(rf.prob.train > 0.5, '1', '0')
rf.tab.train <- table(as.factor(pred.train[,2]), loanData$Loan_Status)
confusionMatrix(rf.tab.train, positive = "1")


rf.prob <- predict(rf, newdata = loanData1, type = "prob")
pred <- ifelse(rf.prob > 0.5, '1', '0')
rf.tab <- table(as.factor(pred[,2]), loanData1$Loan_Status)
rf.per <- confusionMatrix(rf.tab, positive = "1"); rf.per

# ROC and AUC
roc(loanData$Loan_Status ~ rf.prob[,1], plot = TRUE, print.auc = TRUE)













