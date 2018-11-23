### Daniel Kaminsky - Auto Insurance Problem ###
### Kaggle competition name: DanielK ###

# In case they are not installed, install the following packages
install.packages(c("aod", "car", "caTools", "flux", "ggplot2", "grid", "gridExtra", "gvlma",
                   "leaps", "MASS", "Metrics", "moments", "plyr", "rattle", "ROCR", "rpart", "scales"))
# Load the packages 
library(aod)
library(car)
library(caTools)
library(flux)
library(ggplot2)
library(grid)
library(gridExtra)
library(gvlma)
library(leaps)
library(MASS)
library(Metrics)
library(moments)
library(plyr)
library(rattle)
library(ROCR)
library(rpart)
library(rpart.plot)
library(scales)


## Creating the TRAINING Dataset ###
# Reading the file into R
mydata <- read.csv("D:/logit_insurance.csv", sep = ",")

# Check mydata using str()
str(mydata) # 'data.frame': 8161 obs. of 26 variables
head(mydata)
tail(mydata)

# Transform to Numeric
mydata$INCOME <-as.numeric(sub(',','', sub('\\$','',mydata$INCOME)))
mydata$HOME_VAL <-as.numeric(sub(',','', sub('\\$','',mydata$HOME_VAL)))
mydata$BLUEBOOK <-as.numeric(sub(',','', sub('\\$','',mydata$BLUEBOOK)))
mydata$OLDCLAIM <-as.numeric(sub(',','', sub('\\$','',mydata$OLDCLAIM)))
# Checking that the Transformation to Numeric Worked
str(mydata)
head(mydata)


# Use summary() to obtain and present descriptive statistics from mydata.
summary(mydata)


# 3-Way Frequency Table
FreqTbl_3 <- xtabs(~CAR_USE + EDUCATION + TARGET_FLAG, data = mydata)
ftable(FreqTbl_3) # print table 
summary(FreqTbl_3) # chi-square test of indepedence

# 2-Way Frequency Table
FreqTbl_2 <- xtabs(~CAR_USE + TARGET_FLAG, data = mydata)
ftable(FreqTbl_2) # print table 
summary(FreqTbl_2) # chi-square test of indepedence

# Missing Values - Count per Variable
sapply(mydata, function(mydata) sum(is.na(mydata)))

# Fixing Missing Values with MEDIANs. Select rows where the Variable Observation is NA and replace it with MEDIAN
mydata$AGE[is.na(mydata$AGE)==T] <- median(mydata$AGE, na.rm = TRUE)
median(mydata$AGE, na.rm = F) # Testing for NA values
mydata$YOJ[is.na(mydata$YOJ)==T] <- median(mydata$YOJ, na.rm = TRUE)
median(mydata$YOJ, na.rm = F) # Testing for NA values
mydata$INCOME[is.na(mydata$INCOME)==T] <- median(mydata$INCOME, na.rm = TRUE)
median(mydata$INCOME, na.rm = F) # Testing for NA values
mydata$HOME_VAL[is.na(mydata$HOME_VAL)==T] <- median(mydata$HOME_VAL, na.rm = TRUE)
median(mydata$HOME_VAL, na.rm = F) # Testing for NA values
mydata$CAR_AGE[is.na(mydata$CAR_AGE)==T] <- median(mydata$CAR_AGE, na.rm = TRUE)
median(mydata$CAR_AGE, na.rm = F) # Testing for NA values

# Missing Values - Count per Variable
sapply(mydata, function(mydata) sum(is.na(mydata)))

# If a value in column "CAR_AGE" is Less than 0 set it to 0
mydata[mydata$CAR_AGE < 0, "CAR_AGE"] = 0
str(mydata)

# Histogram and Q-Q Plots CAR_AGE
par(mfrow = c(2, 2), mar = c(5.1, 6.1, 4.1, 2.1))
hist(mydata$CAR_AGE, col = "deepskyblue3", main = "Histogram of CAR_AGE", xlab = "CAR_AGE",
     cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqnorm(mydata$CAR_AGE, col = "deepskyblue3", pch = 'o', main = "Normal Q-Q Plot",
       cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqline(mydata$CAR_AGE, col = "darkred", lty = 2, lwd = 3)
boxplot(mydata$CAR_AGE[mydata$CAR_AGE], col = "red", ylim = c(0.00, 2000), pch = 16,
        main = "CAR_AGE", cex = 2.0, cex.axis = 1.65, cex.lab = 1.75, cex.main = 2.0)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# Checking skewness and kurtosis helps to reveal more about distribution shape.  
# A normal distribution has a skewness of zero and kurtosis of 3.0.
moments::skewness(mydata$CAR_AGE)
moments::kurtosis(mydata$CAR_AGE)

# ANOVA of TARGET_FLAG with Predictor Variable CAR_AGE
ANOVA1 <- aov(TARGET_FLAG ~ CAR_AGE, data = mydata)
summary(ANOVA1)

# Histogram and Q-Q Plots TRAVTIME
par(mfrow = c(2, 2), mar = c(5.1, 6.1, 4.1, 2.1))
hist(mydata$TRAVTIME, col = "deepskyblue3", main = "Histogram of TRAVTIME", xlab = "TRAVTIME",
     cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqnorm(mydata$TRAVTIME, col = "deepskyblue3", pch = 'o', main = "Normal Q-Q Plot",
       cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqline(mydata$TRAVTIME, col = "darkred", lty = 2, lwd = 3)
boxplot(mydata$TRAVTIME[mydata$TRAVTIME], col = "red", ylim = c(0.00, 2000), pch = 16,
        main = "TRAVTIME", cex = 2.0, cex.axis = 1.65, cex.lab = 1.75, cex.main = 2.0)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# Checking skewness and kurtosis helps to reveal more about distribution shape.  
# A normal distribution has a skewness of zero and kurtosis of 3.0.
moments::skewness(mydata$TRAVTIME)
moments::kurtosis(mydata$TRAVTIME)

# ANOVA of TARGET_FLAG with Predictor Variable CAR_AGE
ANOVA1 <- aov(TARGET_FLAG ~ TRAVTIME, data = mydata)
summary(ANOVA1)

# Histogram and Q-Q Plots TARGET_FLAG
par(mfrow = c(2, 2), mar = c(5.1, 6.1, 4.1, 2.1))
hist(mydata$TARGET_FLAG, col = "deepskyblue3", main = "Histogram of TARGET_FLAG", xlab = "TARGET_FLAG",
     cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqnorm(mydata$TARGET_FLAG, col = "deepskyblue3", pch = 'o', main = "Normal Q-Q Plot",
       cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqline(mydata$TARGET_FLAG, col = "darkred", lty = 2, lwd = 3)
boxplot(mydata$TARGET_FLAG[mydata$TRAVTIME], col = "red", ylim = c(0.00, 2000), pch = 16,
        main = "TARGET_FLAG", cex = 2.0, cex.axis = 1.65, cex.lab = 1.75, cex.main = 2.0)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# Checking skewness and kurtosis helps to reveal more about distribution shape.  
# A normal distribution has a skewness of zero and kurtosis of 3.0.
moments::skewness(mydata$TARGET_FLAG)
moments::kurtosis(mydata$TARGET_FLAG)

# Histogram and Q-Q Plots TARGET_AMT
par(mfrow = c(2, 2), mar = c(5.1, 6.1, 4.1, 2.1))
hist(mydata$TARGET_AMT, col = "deepskyblue3", main = "Histogram of TARGET_AMT", xlab = "TARGET_AMT",
     cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqnorm(mydata$TARGET_AMT, col = "deepskyblue3", pch = 'o', main = "Normal Q-Q Plot",
       cex = 2, cex.axis = 1.5, cex.lab = 2.0, cex.main = 2, cex.sub = 1.5)
qqline(mydata$TARGET_AMT, col = "darkred", lty = 2, lwd = 3)
boxplot(mydata$TARGET_AMT[mydata$TRAVTIME], col = "red", ylim = c(0.00, 2000), pch = 16,
        main = "TARGET_AMT", cex = 2.0, cex.axis = 1.65, cex.lab = 1.75, cex.main = 2.0)
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# Checking skewness and kurtosis helps to reveal more about distribution shape.  
# A normal distribution has a skewness of zero and kurtosis of 3.0.
moments::skewness(mydata$TARGET_AMT)
moments::kurtosis(mydata$TARGET_AMT)

# Use summary() to obtain and present descriptive statistics from mydata.
summary(mydata)
str(mydata)

# Logistic Regression TARGET_FLAG as the dependent variable on All Predictor Variables as a BASELINE Model
LogisticMLR <- glm(TARGET_FLAG ~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL +
                   MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF +
                   CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE +
                   URBANICITY, data = mydata, family = "binomial"(link="logit"))
  
summary(LogisticMLR)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(LogisticMLR)

# Evaluate Collinearity
vif(LogisticMLR) # variance inflation factors 
sqrt(vif(LogisticMLR)) > 2 # problem?

# Evaluate Nonlinearity
# component + residual plot 
crPlots(LogisticMLR)

# Test for Autocorrelated Errors
durbinWatsonTest(LogisticMLR)

# Logistic Regression Stepwise Selection TARGET_FLAG as the dependent variable on All Predictor Variables
LogisticMLR_STEP <- glm(TARGET_FLAG ~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL +
                     MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF +
                     CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE +
                     URBANICITY, data = mydata, family = "binomial"(link="logit"))

Step <- stepAIC(LogisticMLR_STEP, direction="both")
Step$anova # display results
coefficients(Step) # Model coefficients
summary(Step)
  
# Logistic Regression TARGET_FLAG as the dependent variable Model 1 , control = list(maxit = 2, epsilon=1)
LogisticMLR1 <- glm(TARGET_FLAG ~ INCOME + TRAVTIME + CAR_USE, data = mydata, 
                    family = "binomial"(link="logit"), y=FALSE, model=FALSE,
                    control = list(trace=TRUE))

summary(LogisticMLR1) 

# Logistic Regression TARGET_FLAG as the dependent variable Model 2
LogisticMLR2 <- glm(TARGET_FLAG ~ TRAVTIME, data = mydata, family = "binomial"(link="logit"))

summary(LogisticMLR2) 

# Logistic Regression TARGET_FLAG as the dependent variable Model 3 , control = list(maxit = 2, epsilon=1)
LogisticMLR3 <- glm(TARGET_FLAG ~ BLUEBOOK + CAR_TYPE + CAR_AGE, data = mydata, 
                    family = "binomial"(link="logit"), y=FALSE, model=FALSE,
                    control = list(trace=TRUE))

summary(LogisticMLR3)

# Using the Probit Model - TARGET_FLAG as the dependent variable Model 4
ProbitMLR4 <- glm(TARGET_FLAG ~ BLUEBOOK + CAR_TYPE + CAR_AGE, data = mydata, 
                  family = binomial(link = "probit"))

## model summary
summary(ProbitMLR4)

# Preparing for RMSE metric
Actual <- mydata$TARGET_FLAG
Pred_LogisticMLR <- fitted(LogisticMLR)
Pred_LogisticMLRstep <- fitted(LogisticMLR_STEP)
Pred_LogisticMLR1 <- fitted(LogisticMLR1)
Pred_LogisticMLR2 <- fitted(LogisticMLR2)
Pred_LogisticMLR3 <- fitted(LogisticMLR3)
Pred_ProbitMLR4 <- fitted(ProbitMLR4)

# RMSE library(Metrics)
rmse(Actual, Pred_LogisticMLR)
rmse(Actual, Pred_LogisticMLRstep)
rmse(Actual, Pred_LogisticMLR1)
rmse(Actual, Pred_LogisticMLR2)
rmse(Actual, Pred_LogisticMLR3)
rmse(Actual, Pred_ProbitMLR4)

# ROC AUC - Model: LogisticMLR3
# library(ROCR)
p <- predict(LogisticMLR3, newdata=subset(mydata,select=c(4,5,6,7,8,9,10,11,12,13
                                                          ,14,15,16,17,18,19,20,21
                                                          ,22,23,24,25,26)), type="response")
pr <- prediction(p, mydata$TARGET_FLAG)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "darkred", main = "ROC Curve - AUC = 0.6222815",
     cex = 2, cex.main = 2, lwd = 4, lty = 1)
abline(a=0, b=1, col = "darkblue", lwd = 4, lty = 2)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.6222815

# ROC AUC - Model: LogisticMLR_STEP
# library(ROCR)
p <- predict(LogisticMLR_STEP, newdata=subset(mydata,select=c(4,5,6,7,8,9,10,11,12,13
                                                          ,14,15,16,17,18,19,20,21
                                                          ,22,23,24,25,26)), type="response")
pr <- prediction(p, mydata$TARGET_FLAG)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, col = "darkred", main = "ROC Curve - AUC = 0.8135616",
     cex = 2, cex.main = 2, lwd = 4, lty = 1)
abline(a=0, b=1, col = "darkblue", lwd = 4, lty = 2)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc # 0.8135616

# Decision Tree for the full model LogisticMLR
DTreeFull <- rpart(TARGET_FLAG ~ KIDSDRIV + AGE + HOMEKIDS + YOJ + INCOME + PARENT1 + HOME_VAL +
                     MSTATUS + SEX + EDUCATION + JOB + TRAVTIME + CAR_USE + BLUEBOOK + TIF +
                     CAR_TYPE + RED_CAR + OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + CAR_AGE +
                     URBANICITY, data = mydata)
summary(DTreeFull)
# plot(DTreeFull)
# text(DTreeFull)
fancyRpartPlot(DTreeFull)

# Decision Tree for the BLUEBOOK variable used as Response variable
DTree_BLUEBOOK = rpart(BLUEBOOK ~ CAR_AGE + CAR_TYPE, data=mydata)
summary(DTree_BLUEBOOK)
# plot(DTree_BLUEBOOK)
# text(DTree_BLUEBOOK)
fancyRpartPlot(DTree_BLUEBOOK)

# Using rattle() to generate a Decision Tree
rattle()

### Looking ONLY to people that CRASHED their CARS using subset function 
Crash_Only_Data <- subset(mydata, TARGET_AMT > 0)
str(Crash_Only_Data)
head(Crash_Only_Data)
# Missing Values - Count per Variable
sapply(mydata, function(mydata) sum(is.na(mydata)))
sapply(Crash_Only_Data, function(Crash_Only_Data) sum(is.na(Crash_Only_Data)))
# Looking at the stats for Crash_Only_Data
summary(Crash_Only_Data)

# MLR Model to Predict Loss Cost per Crash data=Crash_Only_Data
CrashrRegAMT <- lm(TARGET_AMT ~ BLUEBOOK + CAR_TYPE + CAR_AGE, data=Crash_Only_Data)
summary(CrashrRegAMT)


#### CREATING A TEST DATASET ###
# Reading the file into R
TESTdata <- read.csv("D:/logit_insurance_test.csv", sep = ",")

# Check mydata using str()
str(TESTdata) # 'data.frame': 2141 obs. of 26 variables
head(TESTdata)
tail(TESTdata)

# Transform to Numeric
TESTdata$INCOME <-as.numeric(sub(',','', sub('\\$','',TESTdata$INCOME)))
TESTdata$HOME_VAL <-as.numeric(sub(',','', sub('\\$','',TESTdata$HOME_VAL)))
TESTdata$BLUEBOOK <-as.numeric(sub(',','', sub('\\$','',TESTdata$BLUEBOOK)))
TESTdata$OLDCLAIM <-as.numeric(sub(',','', sub('\\$','',TESTdata$OLDCLAIM)))
# Checking that the Transformation to Numeric Worked
str(TESTdata)
head(TESTdata)

# Use summary() to obtain and present descriptive statistics from mydata.
summary(TESTdata)

# Missing Values - Count per Variable
sapply(TESTdata, function(TESTdata) sum(is.na(TESTdata)))

# Fixing Missing Values with MEDIANs. Select rows where the Variable Observation is NA and replace it with MEDIAN
TESTdata$AGE[is.na(TESTdata$AGE)==T] <- median(mydata$AGE, na.rm = TRUE)
median(TESTdata$AGE, na.rm = F) # Testing for NA values
TESTdata$YOJ[is.na(TESTdata$YOJ)==T] <- median(mydata$YOJ, na.rm = TRUE)
median(TESTdata$YOJ, na.rm = F) # Testing for NA values
TESTdata$INCOME[is.na(TESTdata$INCOME)==T] <- median(mydata$INCOME, na.rm = TRUE)
median(TESTdata$INCOME, na.rm = F) # Testing for NA values
TESTdata$HOME_VAL[is.na(TESTdata$HOME_VAL)==T] <- median(mydata$HOME_VAL, na.rm = TRUE)
median(TESTdata$HOME_VAL, na.rm = F) # Testing for NA values
TESTdata$CAR_AGE[is.na(TESTdata$CAR_AGE)==T] <- median(mydata$CAR_AGE, na.rm = TRUE)
median(TESTdata$CAR_AGE, na.rm = F) # Testing for NA values

# Missing Values - Count per Variable
sapply(TESTdata, function(TESTdata) sum(is.na(TESTdata)))

# If a value in column "CAR_AGE" is Less than 0 set it to 0
TESTdata[TESTdata$CAR_AGE < 0, "CAR_AGE"] = 0
str(TESTdata)

# Regression Model Test Dataset
TESTdata$YHAT <-	( -1.053e+00
                   +TESTdata$KIDSDRIV * (4.176e-01)
                   +TESTdata$INCOME * (-3.486e-06)
                   +outer(TESTdata$PARENT1 == 'Yes', 1*(4.602e-0))
                   +TESTdata$HOME_VAL * (-1.342e-06)
                   +outer(TESTdata$MSTATUS == 'z_No', 1*(4.719e-01))
                   +outer(TESTdata$EDUCATION == 'Bachelors', 1*(-3.868e-01))
                   +outer(TESTdata$EDUCATION == 'Masters', 1*(-3.032e-01))
                   +outer(TESTdata$EDUCATION == 'PhD', 1*(-1.818e-01))
                   +outer(TESTdata$EDUCATION == 'z_High School', 1*(1.487e-02))
                   +outer(TESTdata$JOB == 'Clerical', 1*(4.141e-01))
                   +outer(TESTdata$JOB == 'Doctor', 1*(-4.475e-01))
                   +outer(TESTdata$JOB == 'Home Maker', 1*(2.748e-01))
                   +outer(TESTdata$JOB == 'Lawyer', 1*(9.715e-02))
                   +outer(TESTdata$JOB == 'Manager', 1*(-5.649e-01))
                   +outer(TESTdata$JOB == 'Professional', 1*(1.548e-01))
                   +outer(TESTdata$JOB == 'Student', 1*(2.751e-01))
                   +outer(TESTdata$JOB == 'z_Blue Collar', 1*(3.098e-01))
                   +TESTdata$TRAVTIME * (1.448e-02)
                   +outer(TESTdata$CAR_USE == 'Private', 1*(-7.574e-01))
                   +TESTdata$BLUEBOOK * (-2.308e-05)
                   +TESTdata$TIF * (-5.538e-02)
                   +outer(TESTdata$CAR_TYPE == 'Panel Truck', 1*(6.090e-01))
                   +outer(TESTdata$CAR_TYPE == 'Pickup', 1*(5.503e-01))
                   +outer(TESTdata$CAR_TYPE == 'Sports Car', 1*(9.726e-01))
                   +outer(TESTdata$CAR_TYPE == 'Van', 1*(6.466e-01))
                   +outer(TESTdata$CAR_TYPE == 'z_SUV', 1*(7.156e-01))
                   +TESTdata$OLDCLAIM * (-1.405e-05)
                   +TESTdata$CLM_FREQ * (1.963e-01)
                   +outer(TESTdata$REVOKED == 'Yes', 1*(8.927e-01))
                   +TESTdata$MVR_PTS * (1.143e-01)
                   +outer(TESTdata$URBANICITY == 'z_Highly Rural/ Rural', 1*(-2.389e+00)))

# Use summary() to obtain and present descriptive statistics from mydata.
summary(TESTdata)
head(TESTdata)

# Converting from LOG ODDS to Probability (The exp(YHAT) is the ODDS)
TESTdata$P_TARGET_FLAG <- (exp(TESTdata$YHAT) / (1 + exp(TESTdata$YHAT)))
head(TESTdata)

# CrashrRegAMT Regression Model for TESTdata dataset
TESTdata$P_TARGET_AMT <- (4510.6856
                          +TESTdata$BLUEBOOK * 0.1035
                          +outer(TESTdata$CAR_TYPE == 'Panel Truck', 1*(379.9023))
                          +outer(TESTdata$CAR_TYPE == 'Pickup', 1*(64.3368))
                          +outer(TESTdata$CAR_TYPE == 'Sports Car', 1*(104.4274))
                          +outer(TESTdata$CAR_TYPE == 'Van', 1*(704.0469))
                          +outer(TESTdata$CAR_TYPE == 'z_SUV', 1*(-75.9061))
                          +TESTdata$CAR_AGE	* (-51.9321))

#TESTdata$P_TARGET_AMT
summary(TESTdata$P_TARGET_AMT)
head(TESTdata)

# Calculating the Pure Premium in the TESTdata dataset
TESTdata$PURE_PREMIUM = (TESTdata$P_TARGET_FLAG * TESTdata$P_TARGET_AMT)
head(TESTdata$PURE_PREMIUM)
str(TESTdata$PURE_PREMIUM)
head(TESTdata)


#### ### ### ### ### ### ### ### ### ### ### ### ###
### Creating the Output file with the INDEX, P_TARGET_FLAG and P_TARGET_AMT
Daniel_Kaminsky_AutoInsProb_Sec60_R_Output <- data.frame(TESTdata$INDEX, TESTdata$P_TARGET_FLAG, TESTdata$P_TARGET_AMT)
names(Daniel_Kaminsky_AutoInsProb_Sec60_R_Output) <-c("INDEX", "P_TARGET_FLAG", "P_TARGET_AMT")

# Checking the Output Dataset
str(Daniel_Kaminsky_AutoInsProb_Sec60_R_Output) # 'data.frame': 2141 obs. of 3 variables
head(Daniel_Kaminsky_AutoInsProb_Sec60_R_Output)

#### ### ### ### ### ### ### ### ### ### ### ### ###
### Write TESTdata to CSV ###
write.csv(Daniel_Kaminsky_AutoInsProb_Sec60_R_Output, 
          file = "D:/MyDocuments/Northwestern_MSPA/PREDICT411/Unit_02/Auto_Insurance_Problem/Daniel_Kaminsky_AutoInsProb_Sec60_R_Output.csv", 
          row.names = FALSE)

### Creating the Output file with the INDEX and P_TARGET_AMT (PURE_PREMIUM) from TESTdata dataset
Daniel_Kaminsky_AutoInsLOSSES_Sec60_R_Output <- data.frame(TESTdata$INDEX, TESTdata$PURE_PREMIUM)
names(Daniel_Kaminsky_AutoInsLOSSES_Sec60_R_Output) <-c("INDEX", "P_TARGET_AMT")

# Checking the Output Dataset
str(Daniel_Kaminsky_AutoInsLOSSES_Sec60_R_Output) # 'data.frame': 2141 obs. of 2 variables
head(Daniel_Kaminsky_AutoInsLOSSES_Sec60_R_Output)

### Write TESTdata to CSV ###
write.csv(Daniel_Kaminsky_AutoInsLOSSES_Sec60_R_Output, 
          file = "D:/Daniel_Kaminsky_AutoInsLOSSES_R_Output.csv", 
          row.names = FALSE)





