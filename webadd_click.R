#--------------------------------Logistic Regression Modelling--------------------------------#


#------------------->Basic Functional Form:
#P(Y=1)=e^Z/(1+e^Z), e refers to exponential
#where Z=B0+B1X1+B2X2+..........+BNXN


#Problem Statement: 


#To predict who is likely going to click on the Advertisement so it can contribute to the more revenue generation to the organization.

#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

#library(remotes)
#install_version("gbRd", "0.4-11")

library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
library(BaylorEdPsych)#R-SQUARE
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)
library(DescTools)#TRAIN AND TEST SPLIT
library(dplyr)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"D:/IVY/R/R final project/logistic reg"

setwd(Path)
getwd()


data<-read.csv("Web_data.csv",header = TRUE)
data1=data#To create a backup of original data
head(data1)

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)
#converting target variable to factor
data1$Clicked<-as.factor(data1$Clicked) 

names<-c("Ad_Topic","Country_Name","City_code","Male","Time_Period","Weekday","Month")

data1[,names]<-lapply(data1[,names],factor)
str(data1)
levels(data1$City_code)

table(data1$Country_Name)
table(data1$Ad_Topic)
table(data1$City_code)
#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
library(Amelia)
missmap(data1)

data.frame(colSums(is.na(data1)))
#no missing value in the data

#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#

#-----------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
num <- data1[,-c(6:12)]#Numerical Data Frame
cat <- data1[,c(6:12,14)]#Categorical Data Frame
head(cat)
head(num)
str(num)
str(cat)
colnames(cat)

#---------------------------------------IV for numeric data-------------------------------------------------------#


IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}


a1<- IVCal("VistID","Clicked",num,groups=10)
a2<- IVCal("Time_Spent","Clicked",num,groups=10)
a3<- IVCal("Age","Clicked",num,groups=10)
a4<- IVCal("Avg_Income","Clicked",num,groups=10)
a5<- IVCal("Internet_Usage","Clicked",num,groups=10)
a6<- IVCal("Year","Clicked",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6))
IV_num


#-------------------------------------Information Value for categorical data----------------------------------------------------------#

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A<- CA("Clicked","Ad_Topic",cat)
B<- CA("Clicked","Country_Name",cat)
C<- CA("Clicked","City_code",cat)
D<- CA("Clicked","Male",cat)
E<- CA("Clicked","Time_Period",cat)
F<- CA("Clicked","Weekday",cat)

G<- CA("Clicked","Month",cat)


IV_cat<- data.frame(rbind(A,B,C,D,E,F,G))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")

#removing the unnecessary columns
data2=subset(data1,select = -c(Country_Name,VistID,Year))

#------------------------------Univariate and bivariate analysis-----------------------------------------------------------------------------------------
colnames(data1)
library(corrplot)
correlations <- cor(data[,c(2:5,14)])
corrplot(correlations, method="circle")
#pairs(data1, col=data1$Clicked)

library(ggplot2)
library(ggcorrplot)
data(data1, package="mosaicData")

# select numeric variables
df <- dplyr::select_if(data1, is.numeric)

# calulate the correlations
r <- cor(df, use="complete.obs")
round(r,2)
ggcorrplot(r)

#Distribution of Age
par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = F),
       widths=c(1,1), heights=c(1,1))
boxplot(data1$Age,
        main = "Age ")         # box plot 
plot(density(data1$Age),
     main = "Distribution of Age",
     xlab = "Age(years)")   # kernel density plot
# to normalize the density plot
plot(density(data1$Age^0.5),
     main = "Distribution of Age",
     xlab = "Age(years)")   # normalized kernel density plot

x1 <- data1$Age
m <- mean(x)
s <- sd(x)
c(average = m, sd = s)
p9 <- ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
  stat_function(fun = dnorm)
p9

#Distribution of Time_Spent
par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = F),
       widths=c(1,1), heights=c(1,1))
boxplot(data1$Time_Spent,
        main = "Time Spent ")         # box plot 
plot(density(data1$Time_Spent),
     main = "Distribution of Time Spent",
     xlab = "Time_Spent(minutes)")   # kernel density plot
# to normalize the density plot
plot(density(data1$Time_Spent^0.5),
     main = "Distribution of Time Spent",
     xlab = "Time_Spent(minutes)")   


#Distribution of Avg_Income
options(scipen = 999)
outliers<-boxplot(data1$Avg_Income, plot=TRUE)$out

x<-data1
x<- x[-which(x$Avg_Income %in% outliers),]
summary(x)
data1<-x
par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = F),
       widths=c(1,1), heights=c(1,1))
boxplot(data1$Avg_Income,
        main = "Average Income ")         # box plot 
plot(density(data1$Avg_Income),
     main = "Distribution of Average Income",
     xlab = "Average Income")   # kernel density plot
# to normalize the density plot
plot(density(data1$Avg_Income^0.5),
     main = "Distribution of Average Income",
     xlab = "Average Income")  

#Distribution of daily internet usage
par(mfrow=c(2,2))
layout(matrix(c(1,1,2,3), 2, 2, byrow = F),
       widths=c(1,1), heights=c(1,1))
boxplot(data1$Internet_Usage,
        main = "Internet Usage")         # box plot 
plot(density(data1$Internet_Usage),
     main = "Distribution of Internet Usage",
     xlab = "Internet Usage")   # kernel density plot
# to normalize the density plot
plot(density(data1$Internet_Usage^0.5),
     main = "Distribution of Internet Usage",
     xlab = "Internet Usage")  



# Clicked vs. Age
library(ggplot2)

ggplot(data1, 
       aes(x = Age, 
           fill = Clicked)) +
  geom_density(alpha = 0.4) +
  labs(title = "Age distribution by Clicked")


# Clicked vs. Avg_Income
ggplot(data1, 
       aes(x = Clicked, 
           y = Avg_Income)) +
  geom_violin(fill = "cornflowerblue") +
  geom_boxplot(width = .2, 
               fill = "orange",
               outlier.color = "orange",
               outlier.size = 2) + 
  labs(title = "Avg_Income distribution by Clicked")

AV<-data1[data1$Avg_Income>50000,]
ggplot(data1, 
       aes(x = Avg_Income, 
           fill = Clicked)) +
  geom_density(alpha = 0.4) +
  labs(title = "Average Income by Clicked")

# Clicked vs. Internet_Usage
IU<-data1[data1$Internet_Usage>200,c(5,14)]
ggplot(data1, 
       aes(x = Internet_Usage, 
           fill = Clicked)) +
  geom_density(alpha = 0.4) +
  labs(title = "Internet_Usage by Clicked")

#Time Spent by Clicked
#The more time people spent on the website, the less likely are they to click on Add
A<-data1[data1$Time_Spent>80,]
ggplot(data1, 
       aes(x = Time_Spent, 
           fill = Clicked)) +
  geom_density(alpha = 0.4) +
  labs(title = "Time_Spent by Clicked")

#no of add clicks by time period
ggplot(data1, 
       aes(x = Time_Period, 
           fill = Clicked)) + 
  geom_bar(position = position_dodge(preserve = "single"))

ggplot(data1, 
       aes(x = Weekday, 
           fill = Clicked)) + 
  geom_bar(position = position_dodge(preserve = "single"))
#Ad click by City_code
ggplot(data1, 
       aes(x = City_code, 
           fill = Clicked)) + 
  geom_bar(position = position_dodge(preserve = "single"))

#Internet_Usage vs Age  
ggplot(data1,
       aes(x = Age, 
           y = Internet_Usage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")

#Avg_Income vs Internet_Usage 
ggplot(data1,
       aes(x = Avg_Income, 
           y = Internet_Usage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")

ggplot(data1,
       aes(x = Avg_Income, 
           y = Internet_Usage)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

#anova test
summary(aov(Clicked ~., data = data1))

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data2$Clicked, 0.7)
data.train = subset(data2, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data2, spl == FALSE)
str(data.test)
dim(data.test)


#-------------------------------------Logistic Regression Model Building------------------------------------------#


model <- glm(Clicked~., data=data.train, family=binomial())
summary(model)



## Remove the insignificant variable
model1 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_10")+I(Ad_Topic=="product_12")+ I(Ad_Topic=="product_13")
             +I(Ad_Topic=="product_14")+ I(Ad_Topic=="product_15")+I(Ad_Topic=="product_16")+ I(Ad_Topic=="product_17")+I(Ad_Topic=="product_18")+
               I(Ad_Topic=="product_2")+ I(Ad_Topic=="product_20")+I(Ad_Topic=="product_21")+I(Ad_Topic=="product_22")+I(Ad_Topic=="product_23")+I(Ad_Topic=="product_25")
             +I(Ad_Topic=="product_26")+I(Ad_Topic=="product_27")+I(Ad_Topic=="product_28")+I(Ad_Topic=="product_29")+I(Ad_Topic=="product_3")+I(Ad_Topic=="product_4")
             +I(Ad_Topic=="product_5")+I(Ad_Topic=="product_6")+I(Ad_Topic=="product_7")+I(Ad_Topic=="product_8")+I(Ad_Topic=="product_9")+	I(City_code=="City_2") + I(City_code=="City_3") 
             + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")+I(City_code=="City_8")+I(Male=="Yes")
             + I(Time_Period=="Evening") + I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning") + I(Time_Period=="Night") + I(Time_Period=="Noon") + I(Weekday=="Monday") + I(Weekday=="Saturday") 
             + I(Weekday=="Sunday") + I(Weekday=="Thursday")+ I(Weekday=="Tuesday") + I(Weekday=="Wednesday") +	I(Month=="February")+ I(Month=="January")+I(Month=="July")
             + I(Month=="March")+I(Month=="May"), data=data.train, family=binomial())
summary(model1)


#Iteration2
model2 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_10")+I(Ad_Topic=="product_12")+ I(Ad_Topic=="product_13")
              + I(Ad_Topic=="product_15")+I(Ad_Topic=="product_16")+ I(Ad_Topic=="product_18")+
                I(Ad_Topic=="product_2")+ I(Ad_Topic=="product_20")+I(Ad_Topic=="product_21")+I(Ad_Topic=="product_22")+I(Ad_Topic=="product_25")
              +I(Ad_Topic=="product_26")+I(Ad_Topic=="product_28")+I(Ad_Topic=="product_29")+I(Ad_Topic=="product_3")+I(Ad_Topic=="product_4")
              +I(Ad_Topic=="product_5")+I(Ad_Topic=="product_6")+I(Ad_Topic=="product_7")+I(Ad_Topic=="product_8")+I(Ad_Topic=="product_9")+	I(City_code=="City_2") + I(City_code=="City_3") 
              + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")+I(City_code=="City_8")+I(Male=="Yes")
              + I(Time_Period=="Evening") + I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning") + I(Time_Period=="Night") + I(Time_Period=="Noon") + I(Weekday=="Monday") + I(Weekday=="Saturday") 
              + I(Weekday=="Sunday") +  I(Weekday=="Tuesday") +	I(Month=="February")+ I(Month=="January")+I(Month=="July")
              + I(Month=="March"), data=data.train, family=binomial())
summary(model2)


#Iteration3
model3 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_12")+ I(Ad_Topic=="product_13")
              + I(Ad_Topic=="product_15")+I(Ad_Topic=="product_16")+
                I(Ad_Topic=="product_2")+ I(Ad_Topic=="product_21")+I(Ad_Topic=="product_22")+I(Ad_Topic=="product_25")
              +I(Ad_Topic=="product_28")+I(Ad_Topic=="product_29")+I(Ad_Topic=="product_3")
              +I(Ad_Topic=="product_8")+	I(City_code=="City_2") + I(City_code=="City_3") 
              + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")+I(City_code=="City_8")+I(Male=="Yes")
              + I(Time_Period=="Evening") + I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning") + I(Time_Period=="Night") + I(Time_Period=="Noon") + I(Weekday=="Monday") + I(Weekday=="Saturday") 
              + I(Weekday=="Sunday") +  I(Weekday=="Tuesday") + I(Month=="January")+I(Month=="July")
              + I(Month=="March"), data=data.train, family=binomial())
summary(model3)

#Iteration4
model4 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_12")
              +I(Ad_Topic=="product_16")+ I(Ad_Topic=="product_2")+ I(Ad_Topic=="product_21")+I(Ad_Topic=="product_28")+I(Ad_Topic=="product_3")
              +I(Ad_Topic=="product_8")+I(City_code=="City_2") + I(City_code=="City_3") 
              + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")
               + I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning"), data=data.train, family=binomial())
summary(model4)

vif(model4)

#no multicollinearity in the model, all values are below 1.7

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)

#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(model4), Sigma= vcov(model4), Terms=1:20)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess whether the current variable 
#significantly improves the model fit or not)


# Difference betweene null deviance and deviance
modelChi <- model$null.deviance - model$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- model$df.null - model$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 4)


#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(model4,c( "McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "AldrichNelson", "VeallZimmermann", "Efron", "McKelveyZavoina", "Tjur", "all")) #Expected Good Range of R-Square is between 0.2 - 0.4
#Expected Good Range of R-Square is between 0.2 - 0.4
# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelChi/model$null.deviance
R2.hl


# Cox and Snell R Square (the last number; 

R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data.train))
R.cs

# Max rescaled R square (Nagelkarke) 

R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data.train))))))
R.n



#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(model) # deviance residuals
residuals(model, "pearson") # pearson residuals

sum(residuals(model, type = "pearson")^2)
deviance(model)

#########Larger p value indicate good model fit
1-pchisq(deviance(model), df.residual(model))
#Thus, we accept the Null Hypothesis Ho that Observed Frequencies = Expected Frequencies




################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value incidates the model fits well


# Hosmer and Lemeshow test 


hl <- hoslem.test(as.integer(data.train$Clicked), fitted(model4), g=10)
hl


#####################################################################################################################
# Coefficients (Odds)
model4$coefficients
# Coefficients (Odds Ratio)
exp(model4$coefficients)#Interpret 


# Variable Importance of the model
varImp(model4)

# Predicted Probabilities
prediction <- predict(model4,newdata = data.train,type="response")
prediction

write.csv(prediction,"pred_training.csv")


rocCurve   <- roc(response = data.train$Clicked, predictor = prediction, 
                  levels = rev(levels(data.train$Clicked)))
data.train$Clicked <- as.factor(data.train$Clicked)

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best",transpose = TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Clicked)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

#Gini 0.4-0.8 then its good

AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose = TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
data.train$m1.yhat <- predict(model, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Clicked)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

############################################################################################################
###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################

# Logistic Regression on full data
#options(scipen = 999)
#options(digits = 2)

#Iteration1
modelt <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_12")
              +I(Ad_Topic=="product_16")+ I(Ad_Topic=="product_2")+ I(Ad_Topic=="product_21")+I(Ad_Topic=="product_28")+I(Ad_Topic=="product_3")
              +I(Ad_Topic=="product_8")+I(City_code=="City_2") + I(City_code=="City_3") 
              + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_6")+ I(City_code=="City_7")
              + I(Time_Period=="Mid-Night")+ I(Time_Period=="Morning"), data=data.test, family=binomial())
summary(modelt)

#Iteration2 removing City_6, Time_PeriodMid-Night,Time_PeriodMorning, I(Ad_Topic=="product_8"),I(Ad_Topic=="product_21")
modelt1 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage + I(Ad_Topic=="product_12")
               +I(Ad_Topic=="product_16")+ I(Ad_Topic=="product_2")+I(Ad_Topic=="product_28")+I(Ad_Topic=="product_3")
               +I(City_code=="City_2") + I(City_code=="City_3") 
               + I(City_code=="City_4")+ I(City_code=="City_5")+ I(City_code=="City_7")
               , data=data.test, family=binomial())
summary(modelt1)

#Iteration3 removing I(Ad_Topic=="product_28"), I(Ad_Topic=="product_16"), I(Ad_Topic=="product_12"),I(City_code=="City_4"), I(Ad_Topic=="product_2"),I(City_code=="City_5")
modelt2 <- glm(Clicked~ Time_Spent +	Age + Avg_Income + Internet_Usage 
               +I(Ad_Topic=="product_3")+I(City_code=="City_2") + I(City_code=="City_3") 
               + I(City_code=="City_7"), data=data.test, family=binomial())
summary(modelt2)

vif(modelt2)
#no multicollinearity exists as are values are <1.7

#--------------->using Wald Test is like F test
wald.test(b=coef(modelt2), Sigma= vcov(modelt2), Terms=1:9)#Here Terms, no. of independent variables in your final test model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0

library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

PseudoR2(modelt2,c( "McFadden", "McFaddenAdj", "CoxSnell", "Nagelkerke", "AldrichNelson", "VeallZimmermann", "Efron", "McKelveyZavoina", "Tjur", "all")) #Expected Good Range of R-Square is between 0.2 - 0.4


# Hosmer and Lemeshow R square
R2.hl<-modelChi/modelt2$null.deviance
R2.hl


# Cox and Snell R Square (the last number; here is 2000 should be total no. of ovservation)

R.cs <- 1 - exp ((modelt$deviance - modelt$null.deviance) /2000)
R.cs

# Max rescaled R square (Nagelkarke) (the last number; here is 2000 should be total no. of ovservation)

R.n <- R.cs /(1-(exp(-(modelt$null.deviance/2000))))
R.n



######### Lackfit Deviance ######################################################
residuals(modelt2) # deviance residuals
residuals(modelt2, "pearson") # pearson residuals

sum(residuals(modelt2, type = "pearson")^2)
deviance(modelt2)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt2), df.residual(modelt2))

#######################################################################################
#Function - HS Test

hosmerlem <- function (y, yhat, g = 10) {
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)),
                 include.lowest = TRUE)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}
################################################################################################################
# How to use the function. Data.test is the name of the dataset. model is name of the glm output
## High p value indicates the model fits well

hosmerlem(y = data.test$Clicked, yhat = fitted(modelt2))
################################################################################################################
# Hosmer and Lemeshow test in a different way
## High p value indicates the model fits well
options (scipen = 999)
library(ResourceSelection)
hl <- hoslem.test(data.test$Clicked, fitted(modelt), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modelt2$coefficients
# Coefficients (Odds Ratio)
exp(modelt2$coefficients)

# Variable Importance of the model
varImp(modelt2)

# Predicted Probabilities
prediction <- predict(modelt2,newdata = data.test,type="response")
prediction

write.csv(prediction, file = "pred_test.csv")


rocCurve   <- roc(response = data.test$Clicked, predictor = prediction, 
                  levels = rev(levels(data.test$Clicked)))
data.test$Clicked <- as.factor(data.test$Clicked)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best",transpose = TRUE)[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Clicked)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose = TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

#########################################################################################################################
### KS statistics calculation
data.test$m1.yhat <- predict(modelt2, data.test, type = "response")

library(ROCR)
m1.scores <- prediction(data.test$m1.yhat, data.test$Clicked)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

###########################################################################################################################
# plot results
library(ggplot2)
library(visreg)
GA<-data1[data1$Time_Spent>90,c(2,9,14)]
visreg(model1, "Time_Spent",
       by = "Male",
       gg = TRUE, 
       scale="response") +
  labs(y = "Prob(Clicked)", 
       x = "Time_Spent",
       title = "Relationship of Time_Spent and Gender",
       subtitle = "----",
       caption = "-----")

# calculate means and standard errors by Gender and Time spent
plotdata <- data1 %>%
  group_by(Male) %>%
  summarize(n = n(),
            mean = mean(Time_Spent),
            sd = sd(Time_Spent),
            se = sd/sqrt(n))

ggplot(plotdata, 
      aes(x = Male, 
          y = mean,
          group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se), 
                width = .1)


