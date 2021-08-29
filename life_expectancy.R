#----------------------------------Multiple Linear Regression Modelling--------------------------------#


#------------------->Basic Functional Form:
#Y=a0+a1X1+ a2X2+a3X3+..................aNXN+e, 
#Y=Dependent Var
#X1,X2,.......XN=Independent Var
#e= Error Term 


#Problem Statement: to predict next year value using linear regression.

#We are provided with average life expectancy of people of 193 Countries. 
#Business context: what are the attributes of people of higher life expectency


library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)
library(Metrics)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<-"D:/IVY/R/R final project/linear reg"
setwd(Path)
getwd()

data=read.csv("Life Expectancy Data.csv")
data1=data#To create a backup of original data


#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)
names<-c("Country","Status")

data1[,names]<-lapply(data1[,names],factor)
str(data1)



#-------------->Outlier Treatment through quantile method

quantile(final_ds$Life_Expectancy,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
#no outliers in life expectancy

#outlier removal in Measles and HIV.AIDS
par(mfrow=c(1,2))
boxplot(data1$Measles,
        ylab = "Measles",
        main = "Boxplot of Measles")
boxplot(data1$HIV.AIDS,
        ylab = "HIV.AIDS",
        main = "Boxplot of HIV.AIDS")

outliers<-boxplot(data1$Measles, plot=TRUE)$out
x<-data1
x<- x[-which(x$Measles %in% outliers),]
summary(x)

outliers<-boxplot(x$HIV.AIDS, plot=TRUE)$out
x<- x[-which(x$HIV.AIDS %in% outliers),]
summary(x)

#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
missing_values<-data.frame(colSums(is.na(x)))
write.csv(missing_values,"Missing_values.csv")
#library(Amelia)
#missmap(data2)
library(mice)
library(VIM)
aggr_plot <- aggr(x, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(x[c(4,5)])
tempData <- mice(x,m=5,maxit=20,meth='cart',seed=500)
summary(tempData)

summary(tempData$imp$Hepatitis_B)
summary(tempData$imp$Total_Expenditure)
data1
final_ds=complete(tempData,3)
summary(final_ds)
#exporting the treated data into csv file
write.csv(final_ds,"final_data1.csv",row.names = FALSE)
#final_ds=read.csv("final_data1.csv")
final_ds[,names]<-lapply(final_ds[,names],factor)



#------------------------------generating correlation matrix-----------------------------------------------------------------------------------------
colnames(final_ds)

library(formattable)
data.frame(cor(final_ds[,c(2,4:23)])) %>%
  round(4) %>%
  formattable(list(
    area(col = 1:21, row = 1:21) ~
      color_tile("white", "red")
  ))

library(formattable)
data.frame(cor(final_ds[,c(4:6,12,18:23)])) %>%
  round(4) %>%
  formattable(list(
    area(col = 1:10, row = 1:10) ~
      color_tile("white", "red")
  ))

#removing the unnecessary column and one of the variables with high correlation
#Infant_Deaths and Under.five_Deaths, Thinness_1.19_Years and Thinness_5.9_Years, Income_Composition_of_Resources and Schooling are highly correlated 
data2=subset(final_ds,select = -c(Country,Infant_Deaths,Thinness_5.9_Years))
#-----------------------------------------Univariate and bivariate analysis-----------------------------------------------------------------------------
par(mfrow=c(1,2))
# target variable
# histogram
hist(final_ds$Life_Expectancy,
     main = "Life_Expectancy Distribution",
     xlab = "Life Expectancy(yrs)")
# kernel density plot with a vertical indication of location of the mean
plot(density(final_ds$Life_Expectancy),
     main = "Life_Expectancy Distribution",
     xlab = "Life Expectancy (yrs)")
abline(v=mean(final_ds$Life_Expectancy))


# check correlations of the target variable with the first 5 predictors using Pearson correlation
library(psych)
pairs.panels(final_ds[,4:9], 
             method = "pearson", # correlation method
             hist.col = "green",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

library(corrplot)
correlations <- cor(final_ds[,c(2,4:8)])
corrplot(correlations, method="circle")

correlations <- cor(final_ds[,c(4,9:13)])
corrplot(correlations, method="circle")

correlations <- cor(final_ds[,c(4,14:18)])
corrplot(correlations, method="circle")

correlations <- cor(final_ds[,c(4,19:23)])
corrplot(correlations, method="circle")

#life expectancy vs Per_Capita_GDP
ggplot(final_ds,
       aes(x = Life_Expectancy,
           y = Per_Capita_GDP)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm")

#Schooling vs Income_Composition_of_Resources
ggplot(final_ds,
       aes(x = Income_Composition_of_Resources,
           y = Schooling)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

ggplot(final_ds,
       aes(x = Schooling,
           y = Life_Expectancy)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

# life expectancy vs. income composition - positively correlated
plot(y = final_ds$Life_Expectancy,
     x = final_ds$Income_Composition_of_Resources,
     main = "Life Expectancy vs. Income compositions",
     xlab = "Income composition of resources",
     ylab = "Life Expectancy",
     pch = 19,
     col = "yellowgreen")
abline(60,1,
       col = "red")       # 45 degree line (line with slope 1)


plot(y = final_ds$Life_Expectancy,
     x = final_ds$Schooling,
     main = "Life Expectancy vs. Schooling",
     xlab = "Schooling",
     ylab = "Life Expectancy",
     pch = 19,
     col = "rosybrown1")
abline(50,1,
       col = "red")      # 45 degree line (line with slope 1)


plot(y = final_ds$Life_Expectancy,
     x = final_ds$Adult_Mortality,
     main = "Life Expectancy vs. Adult Mortality",
     xlab = "Adult Mortality",
     ylab = "Life Expectancy",
     pch = 19,
     col = "mediumpurple1")
abline(80, - 1,
       col = "red")       # 135 degree line (line with slope -1)

ggplot(final_ds,
       aes(x = Adult_Mortality,
           y = Life_Expectancy)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

#Life_Expectancy vs Population
plot(y = final_ds$Life_Expectancy,
     x = final_ds$Population,
     main = "Life Expectancy vs. Population",
     xlab = "Population",
     ylab = "Life Expectancy",
     pch = 19,
     col = "lightsteelblue3")
abline(50,1,
       col = "red")      # 45 degree line (line with slope 1)

ggplot(final_ds,
       aes(x = Population,
           y = Life_Expectancy)) +
  geom_point(color= "steelblue") +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 2), 
              color = "indianred3")

#Life Expectancy vs. Year
plot(y = final_ds$Life_Expectancy,
     x = final_ds$Year,
     main = "Life Expectancy vs. Year",
     xlab = "Year",
     ylab = "Life Expectancy",
     pch = 19,
     col = "lightsteelblue3")
abline(50,1,
       col = "red") 


#------------------------Filtering the data and assigning them rank according to life expectancy-------------------------------------------------
plotdata <- data1 %>%
  filter(Year == 2015) 

long_life <- plotdata %>%
  arrange(desc(Life_Expectancy)) %>%
  mutate(rank = c(1:183)) %>%
  filter(rank <= 20| grepl("United States|Russia|India", Country))


table(data1$Country)
#------------------------ChoroplethrMaps of Life_Expectancy----------------------------------------
data(country.map, package = "choroplethrMaps")
tail(unique(country.map$region), 500)

library(choroplethr)
library(choroplethrMaps)
plotdata <- data1 %>%
  filter(Year == 2015) %>%
  rename(region = Country,
         value = Life_Expectancy) %>%
  mutate(region = tolower(region))%>%
  mutate(region = recode(region,
                         "united states"    = "united states of america",
                         "congo, dem. rep." = "democratic republic of the congo",
                         "congo, rep."      = "republic of congo",
                         "korea, dem. rep." = "south korea",
                         "korea. rep."      = "north korea",
                         "tanzania"         = "united republic of tanzania",
                         "serbia"           = "republic of serbia",
                         "slovak republic"  = "slovakia",
                         "yemen, rep."      = "yemen",
                         "russian federation" = "russia"))
unique(plotdata$region)
table(plotdata$region)
plotdata<-plotdata[-156,]#removing duplicate row of switzerland
country_choropleth(plotdata)
country_choropleth(plotdata,
                   num_colors=9) +
  scale_fill_brewer(palette="YlOrRd") +
  labs(title = "Life expectancy by Country",
       subtitle = "",
       caption = "Created by:Sreya Basak",
       fill = "Years")


#-------------------------------------------------------------------
# Fancy Cleveland plot to list top 20 countries with highest life expectancy
ggplot(long_life, 
       aes(x=Life_Expectancy, 
           y=reorder(Country, Life_Expectancy))) +
  geom_point(color="blue", 
             size = 2) +
  geom_segment(aes(x = 40, 
                   xend = Life_Expectancy, 
                   y = reorder(Country, Life_Expectancy), 
                   yend = reorder(Country, Life_Expectancy)),
               color = "lightgrey") +
  labs (x = "Life Expectancy (years)",
        y = "",
        title = "Life Expectancy by Country")+
        
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# plot life expectancy by year, for each country

library(ggalt)
library(tidyr)
library(dplyr)


# convert data to wide format
# subset data

plotdata_long <- filter(final_ds,
                        Status=="Developed" &
                          Year %in% c(2000, 2015))  %>%
  select(Country, Year, Life_Expectancy) 
plotdata_wide <- spread(plotdata_long, Year, Life_Expectancy)
names(plotdata_wide) <- c("country", "y2000", "y2015")


# create dumbbell plot
ggplot(plotdata_wide, 
       aes(y = reorder(country, y2000),
           x = y2000,
           xend = y2015)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red") +
  theme_minimal() + 
  labs(title = "Change in Life Expectancy",
       subtitle = "2000 to 2015",
       x = "Life Expectancy (years)",
       y = "")



library(CGPfunctions)

# Select countries data for 2000, 2005, 2010, 2015
index <- seq(2000, 2015, by = 5)
df <- data1 %>%
  filter(Year %in% index &
           Country %in% c("India", "United States of America", 
                          "Japan", "United Kingdom of Great Britain and Northern Ireland", 
                          "Afghanistan", "Russian Federation",
                          "China")) %>%
  mutate(Year = factor(Year),
         Life_Expectancy = round(Life_Expectancy)) 

# create slope graph

newggslopegraph(df, Year, Life_Expectancy, Country) +
  labs(title="Life Expectancy by Country", 
       subtitle="", 
       caption="")
#anova test
summary(aov(Life_Expectancy ~ ., data = final_ds))
#-----------------------Null Model---------------------------------------------
# baseline model with no predictors
nullModel <- lm(Life_Expectancy ~ 1,
                data = data2)

# check the model
summary(nullModel)

nullModelPreds <- predict(nullModel,  # my model
                          newdata = data2, # dataset
                          type = "response") # to get predicted values

#-------------------------EDA based Model---------------------------------------
# baseline model with predictors that strongly correlated to target variable
EDAModel <- lm(Life_Expectancy ~ Schooling +  Adult_Mortality + Income_Composition_of_Resources,
               data = data2)

# check the model
summary(EDAModel)

#prediction
EDAModelPreds <- predict(EDAModel,  # my model
                         newdata = data2, # dataset
                         type = "response") # to get predicted values

# actual vs predicted
plot(y = EDAModel$fitted.values,
     x = data2$Life_Expectancy,
     main = "Actual vs Predicted using EDA model",
     xlab = "Actual",
     ylab = "Predicted(EDAModel)",
     pch = 19)
abline(0,1, col = "green", lwd = 2)  # this is a perfect prediction - 45 degree line

# add the regression line 
abline(lm(EDAModel$fitted.values ~ EDAModel$model$Life_Expectancy),
       col = "red", lwd = 2)

#Here is the Actual vs Predicted Plot of EDAModel. The green line represents a perfect prediction, while the red line represents the regression line.
# predict Life expectancy
predictedLE2 <- predict(EDAModel, interval = "prediction")

# combine the actual data and predicted data
comb2 <- cbind.data.frame(data2, predictedLE2)

# Plotting the combined data
ggplot(comb2, aes(Life_Expectancy, fit)) +
  geom_point() + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  stat_smooth(method = lm) +
  geom_smooth(method=lm, se=TRUE)+
  ggtitle("Actual vs. Predicted for EDAModel with CI") +
  xlab("Actual Life Expectancy") + 
  ylab("Predicted Life Expectancy")

#finding rmse
rmse(EDAModel$fitted.values,EDAModel$model$Life_Expectancy)
#--------------------------Splitting the data into training and test data set------------------------#
set.seed(123)#This is used to produce reproducible results, everytime we run the model


spl = sample.split(data2$Life_Expectancy, 0.7)#Splits the overall data into train and test data in 70:30 ratio

original.data = subset(data2, spl == TRUE)
str(original.data)
dim(original.data)

test.data = subset(data2, spl == FALSE)
str(test.data)
dim(test.data)


#------------------------------------------Fitting the model: Backward Step Model---------------------------------------#
#Iteration.1 We start with testing all variables
options(scipen = 999)

LinearModel1=lm(Life_Expectancy~.,data=original.data)#'.', refers to all variables
summary(LinearModel1)

#Iteration. 2 removing Diphtheria , Measles, BMI  
LinearModel2=lm(Life_Expectancy~ Year+I(Status=="Developing")+ Adult_Mortality + Hepatitis_B +  Under.five_Deaths + Polio +
                  HIV.AIDS + GDP +Per_Capita_GDP + Population  +  Thinness_1.19_Years + 
                  Schooling+Total_Expenditure+Income_Composition_of_Resources+Percentage_Expenditure + Alcohol  ,data=original.data)
summary(LinearModel2)

#Iteration. 3 removing Polio ,Hepatitis_B, Alcohol, Percentage_Expenditure ,Year
LinearModel3=lm(Life_Expectancy~ Status+ Adult_Mortality +  Under.five_Deaths + 
                  HIV.AIDS + GDP +Per_Capita_GDP + Population  +  Thinness_1.19_Years + 
                  Schooling+Total_Expenditure+Income_Composition_of_Resources ,data=original.data)
summary(LinearModel3)

# actual vs predicted
plot(y = LinearModel3$fitted.values,
     x = original.data$Life_Expectancy,
     main = "Actual vs Predicted using Backward Step model",
     xlab = "Actual",
     ylab = "Predicted(LinearModel3)",
     pch = 19)
abline(0,1, col = "green", lwd = 2)  # this is a perfect prediction - 45 degree line

# add the regression line 
abline(lm(LinearModel3$fitted.values ~ LinearModel3$model$Life_Expectancy),
       col = "red", lwd = 2)



# predict Life expectancy
predictedLE3 <- predict(LinearModel3, interval = "prediction")

# combine the actual data and predicted data
comb3 <- cbind.data.frame(original.data, predictedLE3)

# Plotting the combined data
ggplot(comb3, aes(Life_Expectancy, fit)) +
  geom_point() + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") +
  geom_line(aes(y = upr), color = "red", linetype = "dashed") +
  stat_smooth(method = lm) +
  geom_smooth(method=lm, se=TRUE)+
  ggtitle("Actual vs. Predicted for BackwardStepModel with CI") +
  xlab("Actual Life Expectancy") + 
  ylab("Predicted Life Expectancy")

#finding rmse
rmse(LinearModel3$fitted.values,LinearModel3$model$Life_Expectancy)

vif(LinearModel3)
#Vif reduced model---remove Population , Schooling, GDP 
LinearModel4=lm(Life_Expectancy~ I(Status=="Developing")+ Adult_Mortality +  Under.five_Deaths + 
                  HIV.AIDS + Per_Capita_GDP +   Thinness_1.19_Years + 
                  Total_Expenditure+Income_Composition_of_Resources ,data=original.data)
summary(LinearModel4)


vif(LinearModel4)

#finding rmse
rmse(LinearModel4$fitted.values,LinearModel4$model$Life_Expectancy)

# actual vs predicted
plot(y = LinearModel4$fitted.values,
     x = original.data$Life_Expectancy,
     main = "Actual vs Predicted using Fit model",
     xlab = "Actual",
     ylab = "Predicted(fitModel)",
     pch = 19)
abline(0,1, col = "green", lwd = 2)  # this is a perfect prediction - 45 degree line

# add the regression line 
abline(lm(LinearModel4$fitted.values ~ LinearModel4$model$Life_Expectancy),
       col = "red", lwd = 2)

FinalModel=LinearModel4

## Get the predicted or fitted values
fitted(FinalModel)

par(mfrow=c(2,2))
plot(FinalModel)



## MAPE
original.data$pred <- fitted(FinalModel)
Actual_Pred<-select(original.data,c(Life_Expectancy,pred))
Actual_Pred$error<-Actual_Pred$Life_Expectancy-Actual_Pred$pred
summary(Actual_Pred$error)
write.csv(original.data,"mape.csv")


#Calculating MAPE
attach(original.data)
MAPE<-print((sum((abs(Life_Expectancy-pred))/Life_Expectancy))/nrow(original.data))

############ Residual Analysis ############################################################################

res <- original.data

res$stu_res <- studres(FinalModel) ##Studentized residuals
res$stud.del.resids <- rstudent(FinalModel) ##studentized deleted residuals
res$leverage <- hatvalues(FinalModel) ## leverage values (hi)
res$cooks_dis <- cooks.distance(FinalModel) ## Cook's distance
res$dffits <- dffits(FinalModel) ## Dffit
res$dfbetas <- dfbetas(FinalModel) ## Dfbetas
res$cov_ratio <- covratio(FinalModel) ## Covariance Ratio

write.csv(res,"res.csv")

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value

durbinWatsonTest(FinalModel)
dwt(FinalModel)

# Checking multicollinearity
vif(FinalModel) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(FinalModel)  # Null hypothesis -> error is non-homogenious (p value should be more than 0.05)

###########################################################################################################################
############## Testing the model on test data ############################################################################
###########################################################################################################################


##Final model 
fit1<- lm(Life_Expectancy~ Status+ Adult_Mortality +  Under.five_Deaths + 
            HIV.AIDS + Per_Capita_GDP +   Thinness_1.19_Years + 
            Total_Expenditure+Income_Composition_of_Resources ,data=test.data)
summary(fit1)



# actual vs predicted
plot(y = fit1$fitted.values,
     x = test.data$Life_Expectancy,
     main = "Actual vs Predicted using FinalFit model",
     xlab = "Actual",
     ylab = "Predicted(fit1)",
     pch = 19)
abline(0,1, col = "green", lwd = 2)  # this is a perfect prediction - 45 degree line

# add the regression line 
abline(lm(fit1$fitted.values ~ fit1$model$Life_Expectancy),
       col = "red", lwd = 2)

#Check Vif, vif>2 means presence of multicollinearity
vif(fit1)


## Get the predicted or fitted values
fitted(fit1)


#finding rmse
rmse(fit1$fitted.values,fit1$model$Life_Expectancy)

## MAPE
test.data$pred <- fitted(fit1)
#write.csv(ori_data,"mape.csv")

#Calculating MAPE
attach(test.data)
(sum((abs(Life_Expectancy-pred))/Life_Expectancy))/nrow(test.data)

##################################### Checking of Assumption ############################################

# residuals should be uncorrelated ##Autocorrelation
# Null H0: residuals from a linear regression are uncorrelated. Value should be close to 2. 
#Less than 1 and greater than 3 -> concern
## Should get a high p value
durbinWatsonTest(fit1)
dwt(fit1)

# Checking multicollinearity
vif(fit1) # should be within 2. If it is greater than 10 then serious problem

################ Constant error variance ##########Heteroscedasticity


# Breusch-Pagan test
bptest(fit1)  # Null hypothesis -> error is homogenious (p value should be more than 0.05)


------------------------------------------------------------------------------------------------
#LinearRehression plots using visreg
  library(ggplot2)
library(visreg)
colnames(data1)
visreg(fit1, "Total_Expenditure", gg = TRUE) 
visreg(fit1, "HIV.AIDS", gg = TRUE) 
visreg(fit1, "Thinness_1.19_Years", gg = TRUE) 
visreg(fit1, "Under.five_Deaths", gg = TRUE) 

