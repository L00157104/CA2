########################### Start of CA 2 ##########################

#### Installing Libraries ####
install.packages("igraph")
install.packages("treemap")
install.packages("viridis")
install.packages("hrbrthemes")
install.packages("GGally")
install.packages("plyr")
install.packages("tidyverse")
install.packages("naniar")
install.packages("skimr")
install.packages("caret")
install.packages("MLmetrics")
install.packages("imbalance")
install.packages("gridExtra")
install.packages("patchwork")
install.packages("ggplot2")
library(igraph)
library(treemap)
library(viridis)
library(hrbrthemes)
library(GGally)
library(plyr)
library(tidyverse)   # metapackage of all tidyverse packages
library(naniar)      # handling missing data
library(skimr)       # quick overview over the dataset 
library(caret)       # ML toolkit
library(MLmetrics)   # F1 Score
library(imbalance)   # algorithms to deal with imbalanced dataset
library(gridExtra)   # display plots in grids
library(patchwork)   # arrange plots side by side
library(ggplot2)
install.packages("psych")
library(psych)

#### End of Library ####

# Stroke dataset 
# Load the dataset into a data frame first

Stroke_data <- read.csv("Stroke.csv", na = "")
str(Stroke_data)
# convert to a data frame
Stroke_data_df <- as.data.frame(Stroke_data)
head(Stroke_data_df)

class(Stroke_data_df)
str(Stroke_data_df)

# check for NAs using Vim or Mice
# view the no of records with NA, Could use the complete cases command
na_records <- Stroke_data_df[!complete.cases(Stroke_data_df),]
summary(na_records)

# count the no of NA records
nrow(na_records)
nrow(Stroke_data_df)
any(is.na(Stroke_data_df))

# Visualise the patterns of NA data using mice or VIM
library(mice)
md.pattern(Stroke_data_df)

# VIM
library(VIM)
missing_values <- aggr(Stroke_data_df, prop = FALSE, numbers = TRUE)
summary(missing_values)

# work with BMI variable # NAs and Convert to numeric
str(Stroke_data_df$bmi)
Stroke_data_df <- Stroke_data_df[-which(Stroke_data_df$bmi=='N/A'),]
Stroke_data_df$bmi <- as.numeric(as.character(Stroke_data_df$bmi))
names(Stroke_data_df)
str(Stroke_data_df$bmi)

# replace NA with 0 
Stroke_data_df[is.na(Stroke_data_df)] <- 0
# any missing values
sum(is.na(Stroke_data_df))
nrow(Stroke_data_df)

# several variables need to be converted

# gender - Male = 0, Female = 1
# ever_married - Yes = 1, No = 0
# Residence_type - Urban = 1, Rural = 0

# work_type contains 5 categories
# work_type - children, Private, Self-employed, Govt_job, Never_worked
# N = 5, so we need n-1 indicator variables # = 4 indicator variables

# smoking_status contains 4 categories
# smoking_status - never smoked, Unknown, formerly smoked, smokes, 
# N = 4, so we need n-1 indicator variables # = 3 indicator variables
# Code variables in alphabetical order

# Convert variables as described above
names(Stroke_data_df)
attach(Stroke_data_df)

Stroke_data_df$gender <- factor(gender,
                             levels = c("Male", "Female"), 
                             ordered = FALSE)

Stroke_data_df$ever_married <- factor(ever_married,
                             levels = c("Yes", "No"), 
                             ordered = FALSE)

Stroke_data_df$Residence_type <- factor(Residence_type, 
                                     levels = c("Urban", "Rural"), 
                                     ordered = FALSE)

Stroke_data_df$work_type <- factor(work_type,  
                                levels = c("children", "Private", "Self-employed", "Govt_job", "Never_worked"), 
                                ordered = FALSE)

Stroke_data_df$smoking_status <- factor(smoking_status,  
                                levels = c("never smoked", "Unknown", "formerly smoked", "smokes"), 
                                ordered = FALSE)
detach(Stroke_data_df)
str(Stroke_data_df)

# Research Question 
# does age, gender, residence type, work type and smoking cause more Stroke ?

###############################
# check model assumptions

# Linearity - There is a linear Relationships between variables?
# Normality - residuals are normally distributed 
# Homoscedasticity - residuals have a constant variance
# No collineaity - variables are not linear combinations of each other
# Independence - resuduals are independent and are not correlated

# check for Linearity
# variables choose that will be used to build model

names(Stroke_data_df)

# Remove "Other" record from variable gender of Stroke_data_df df
Stroke_data_df <- subset(Stroke_data_df, Stroke_data_df$gender != "Other")
str(Stroke_data_df)
nrow(Stroke_data_df)

# selecting variables of interest for model build
colnames(Stroke_data_df)
glimpse(Stroke_data_df)

# could remove a subset of the data first
# choose the vars and show them in the pairs function
# Variabls of interest only needed when selecting particular values for evaluation

variables_of_interest <- c("stroke",
                           "age",
                           "gender",
                           "heart_disease",
                           "hypertension",
                           "ever_married",
                           "Residence_type",
                           "work_type",
                           "avg_glucose_level",
                           "bmi")

# Seems there could be a positive correlation between 
# smoker and stroke 
# age and stroke
# BMI and stroke

pairs(Stroke_data_df[variables_of_interest])

#This chart provides a general level of detail on linearity of the independent variables with the depend
#variable.
#chart shows that stroke rate may be bimodal and that each of the predictor variables is skewed
#to some extent.
#stroke rates rise with age and heart disease.

# Initial investigation of data variables

set.seed(1)
s_model <- lm(formula = stroke ~ age + gender + heart_disease + hypertension + ever_married + Residence_type + work_type + avg_glucose_level + bmi, data = Stroke_data_df )
summary(s_model)


str(Stroke_data_df)
Stroke_data_df$gender <- ifelse(Stroke_data_df$gender == "Male", 1, 0)
Stroke_data_df$ever_married <- ifelse(Stroke_data_df$ever_married == "Yes", 1, 0)
Stroke_data_df$Residence_type <- ifelse(Stroke_data_df$Residence_type == "Urban", 1, 0)

Stroke_data_df$gender
head(Stroke_data_df, 5)

#Stroke_data_df$hypertension <- as.numeric(Stroke_data_df$hypertension)
str(Stroke_data_df)

# scatter plot for Age 
scatter.smooth(x = Stroke_data_df$stroke,
               y = Stroke_data_df$age,
               main = "Stroke ~ Age",
               xlab = "Stroke",
               ylab = "Age")

# scatter plot for Hypertension  
#scatter.smooth(x = Stroke_data_df$stroke,
#               y = Stroke_data_df$hypertension ,
#               main = "Stroke ~ Hypertension ",
#               xlab = "Stroke",
#               ylab = "Hypertension ")

# scatter plot for heart_disease  
#scatter.smooth(x = Stroke_data_df$stroke,
#               y = Stroke_data_df$heart_disease ,
#               main = "Stroke ~ heart_disease ",
#               xlab = "Stroke",
#              ylab = "heart_disease ")

# scatter plot for avg_glucose_level 
scatter.smooth(x = Stroke_data_df$stroke,
               y = Stroke_data_df$avg_glucose_level,
               main = "Stroke ~ Avg Glucose Level",
               xlab = "Stroke",
               ylab = "Avg Glucose Level")

# scatter plot for BMI 
scatter.smooth(x = Stroke_data_df$stroke,
               y = Stroke_data_df$bmi,
               main = "Stroke ~ BMI",
               xlab = "Stroke",
               ylab = "BMI")


# check numerically correlation of these variables 
# examine all other correlations using the cor() function.
# values of -0.2 < x < 0.2 <- low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$age) 
# The correlation test shows that the correlation between age and stroke 
# variables = 0.232313 indicating a medium correlation

cor(Stroke_data_df$stroke, Stroke_data_df$hypertension)
# The correlation test shows that the correlation between hypertension and stroke 
# variables = 0.1425031 indicating a low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$heart_disease)
# The correlation test shows that the correlation between heart_disease and stroke 
# variables = 0.1379293 indicating a low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$ever_married)
# The correlation test shows that the correlation between ever_married and stroke 
# variables = 0.1050508 indicating a low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$Residence_type)
# The correlation test shows that the correlation between Residence_type and stroke 
# variables = 0.00598849 indicating a low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$avg_glucose_level)
# The correlation test shows that the correlation between avg_glucose_level and stroke 
# variables = 0.1389836 indicating a low correlation

cor(Stroke_data_df$stroke, Stroke_data_df$bmi)
# The correlation test shows that the correlation between bmi and stroke 
# variables = 0.04234128 indicating a low correlation


paste("correlation for Stroke and age: ",               cor(Stroke_data_df$stroke, Stroke_data_df$age))         
paste("correlation for Stroke and hypertension: ",      cor(Stroke_data_df$stroke, Stroke_data_df$hypertension))
paste("correlation for Stroke and heart_disease: ",     cor(Stroke_data_df$stroke, Stroke_data_df$heart_disease))
paste("correlation for Stroke and ever_married: ",      cor(Stroke_data_df$stroke, Stroke_data_df$ever_married))
paste("correlation for Stroke and Residence_type: ",    cor(Stroke_data_df$stroke, Stroke_data_df$Residence_type))
paste("correlation for Stroke and avg_glucose_level: ", cor(Stroke_data_df$stroke, Stroke_data_df$avg_glucose_level))
paste("correlation for Stroke and bmi: ",               cor(Stroke_data_df$stroke, Stroke_data_df$bmi))


#It appears that the variable Residence_type and ever_married has a vary low correlation with Stroke, Therefore will
#remove it from the dataset or exclude from final model when constructing the linear model.

Stroke_data_df = subset(Stroke_data_df, select = -c(id))
Stroke_data_df = subset(Stroke_data_df, select = -c(Date))
Stroke_data_df = subset(Stroke_data_df, select = -c(Residence_type))
Stroke_data_df = subset(Stroke_data_df, select = -c(ever_married))
Stroke_data_df = subset(Stroke_data_df, select = -c(gender))
Stroke_data_df = subset(Stroke_data_df, select = -c(work_type))
Stroke_data_df = subset(Stroke_data_df, select = -c(smoking_status))
head(Stroke_data_df)

colnames(Stroke_data_df)

revised_model <- lm(formula = stroke ~ age + heart_disease + hypertension + avg_glucose_level + bmi, data = Stroke_data_df )
summary(revised_model)

# check for outliers 
# Outlier = 1.5 * IQR
# Need to check for all variables that if we have outliers or not using boxplot

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,5)) # Divide graph area in 1 row by 5 cols

# Outlier checking for Age 
boxplot(Stroke_data_df$age, 
        main="Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Stroke_data_df$age)$out))

# Outlier checking for heart_disease 
boxplot(Stroke_data_df$heart_disease, 
        main="Heart Disease", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Stroke_data_df$heart_disease)$out))

# Outlier checking for hypertension 
boxplot(Stroke_data_df$hypertension, 
        main="Hypertension", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Stroke_data_df$hypertension)$out))

# Outlier checking for avg_glucose_level 
boxplot(Stroke_data_df$avg_glucose_level, 
        main="Average Glucose Level", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Stroke_data_df$avg_glucose_level)$out))

# Outlier checking for bmi 
boxplot(Stroke_data_df$bmi, 
        main="BMI", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(Stroke_data_df$bmi)$out))

par(opar)

# use boxplot.stats function to extract the outliers

outlier_values <- boxplot.stats(Stroke_data_df$heart_disease)$out
paste("Heart Disease Outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Stroke_data_df$hypertension)$out
paste("Heart Disease Outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Stroke_data_df$avg_glucose_level)$out
paste("Heart Disease Outliers: ", paste(outlier_values, collapse = ", "))

outlier_values <- boxplot.stats(Stroke_data_df$bmi)$out
paste("Heart Disease Outliers: ", paste(outlier_values, collapse = ", "))

# Check for collinearity of the model
revised_model <- lm(formula = stroke ~ age + heart_disease + hypertension + avg_glucose_level + bmi, data = Stroke_data_df )
summary(revised_model)


# check for normality
#install.packages("e1071")
library(e1071)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("varhandle")
library(varhandle)

# densly plot 
opar <- par(no.readonly = TRUE)
par(mfrow = c(1,5)) # divide graph area into 1 row x 5 cols
# minimally skewed to the left
# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

# Age
plot(density(Stroke_data_df$age), 
     main = "Density plot: Age ",
     ylab="Freequency",
     sub=paste("skewness:", round(e1071::skewness(Stroke_data_df$age),2)))

# fill in the area under the plot with red
polygon(density(Stroke_data_df$age), col = "red")

# hypertension 
plot(density(Stroke_data_df$hypertension ), 
     main = "Density plot: Hypertension",
     ylab="Freequency",
     sub=paste("skewness:", round(e1071::skewness(Stroke_data_df$hypertension ),2)))

# fill in the area under the plot with red
polygon(density(Stroke_data_df$hypertension ), col = "red")

# heart_disease  
plot(density(Stroke_data_df$heart_disease  ), 
     main = "Density plot: Heart Disease",
     ylab="Freequency",
     sub=paste("skewness:", round(e1071::skewness(Stroke_data_df$heart_disease  ),2)))

# fill in the area under the plot with red
polygon(density(Stroke_data_df$heart_disease), col = "red")

# avg_glucose_level  
plot(density(Stroke_data_df$avg_glucose_level  ), 
     main = "Density plot: Avg Glucose Level",
     ylab="Freequency",
     sub=paste("skewness:", round(e1071::skewness(Stroke_data_df$avg_glucose_level),2)))

# fill in the area under the plot with red
polygon(density(Stroke_data_df$avg_glucose_level), col = "red")

# bmi   
plot(density(Stroke_data_df$bmi   ), 
     main = "Density plot: BMI ",
     ylab="Freequency",
     sub=paste("skewness:", round(e1071::skewness(Stroke_data_df$bmi ),2)))

# fill in the area under the plot with red
polygon(density(Stroke_data_df$bmi), col = "red")
par <- opar

########## Skewness
paste("Skewness for age : ",               round(e1071::skewness(Stroke_data_df$age),2))
paste("Skewness for hypertension : ",      round(e1071::skewness(Stroke_data_df$hypertension),2))
paste("Skewness for heart_disease : ",     round(e1071::skewness(Stroke_data_df$heart_disease), 2))
paste("Skewness for bmi : ",               round(e1071::skewness(Stroke_data_df$bmi), 2))
paste("Skewness for avg_glucose_level : ", round(e1071::skewness(Stroke_data_df$avg_glucose_level), 2))

#########################
opar <- par(no.readonly = TRUE)
par(mfrow = c(3, 2)) # divide graph area in 3 row by 2 columns
hist(Stroke_data_df$stroke, main = "Normality proportion of Stroke", xlab = "Stroke")
qqnorm(Stroke_data_df$stroke)
qqline(Stroke_data_df$stroke)

hist(Stroke_data_df$age, main = "Normality proportion of Age", xlab = "Age")
qqnorm(Stroke_data_df$age)
qqline(Stroke_data_df$age)

hist(Stroke_data_df$bmi, main = "Normality proportion of BMI", xlab = "BMI")
qqnorm(Stroke_data_df$bmi)
qqline(Stroke_data_df$bmi)

par(mfrow = c(3, 2))
hist(Stroke_data_df$hypertension, main = "Normality proportion of Hypertension", xlab = "Hypertension")
qqnorm(Stroke_data_df$hypertension)
qqline(Stroke_data_df$hypertension)

hist(Stroke_data_df$heart_disease, main = "Normality proportion of Heart Disease", xlab = "Heart Disease")
qqnorm(Stroke_data_df$heart_disease)
qqline(Stroke_data_df$heart_disease)

hist(Stroke_data_df$avg_glucose_level, main = "Normality proportion of Avg Glucose Level", xlab = "Avg Glucose Level")
qqnorm(Stroke_data_df$avg_glucose_level)
qqline(Stroke_data_df$avg_glucose_level)
########################

####################################
par <- opar
opar <- par(no.readonly = TRUE)

set.seed(1)
str(Stroke_data_df)

# converting all variables to number as same type.

############# Build the model 
attach(Stroke_data_df)
# check the length of each variables first 
length(age)
length(hypertension)
length(heart_disease)
length(avg_glucose_level)
length(bmi)
length(stroke)
fit <- lm(stroke ~ age + hypertension + heart_disease + avg_glucose_level + bmi, data = Stroke_data_df )
detach(Stroke_data_df)
summary(fit)

#############################

library(car)

################ Build Training and Testing data 
set.seed(1)
no_rows_data <- nrow(Stroke_data_df)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
training_data <- Stroke_data_df[sample, ]
testing_data <- Stroke_data_df[-sample, ]


fit <- lm(stroke ~ age + hypertension + heart_disease + bmi + avg_glucose_level, data = training_data)
summary(fit)
confint(fit)

library(car)

###### Linearity 
###### partial residual plots

crPlots(fit)
##########

#### Homoscedasticity 
ncvTest(fit)

####

##### Global validation of linear model assumption:
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)

###### Multicollinearity
library(car)
vif(fit)

# We can check whether any of the variables indicate a multicollinearity problem
# if the value > 2
sqrt(vif(fit)) > 2

##################

##### STEPWISE backward REGRESSION 

library(MASS)

fit_test <- lm(stroke ~ age + hypertension + heart_disease + bmi + avg_glucose_level, data = training_data)
stepAIC(fit_test, direction="backward")
#######

####
#install.packages("leaps")
library(leaps)
leaps <- regsubsets(stroke ~ age + hypertension + heart_disease + bmi + avg_glucose_level, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

####### Prediction

predicted_stroke <- predict(fit, testing_data)

#### # make actuals_predicted dataframe
actuals_predictions <- data.frame(cbind(actuals = testing_data$stroke,
                                        predicted = predicted_stroke))
head(actuals_predictions)
######

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
######

# RSE
sigma(fit)/ mean(testing_data$stroke) # 4.207298

############# Run some output with the final model
## Inputs to the MLR model

summary(Stroke_data_df)

# Test 1: Age less, with less BMI and less avg_glucose_level
df <- data.frame(age = c(25), hypertension = c(0.00000), heart_disease = c(0.00000), bmi = c(10.30), avg_glucose_level = c(55))
predicted_strk <- predict(fit, df)
predicted_strk

# Test 2: Age high = 82, with high hypertension and heart_disease along with less BMI and avg_glucose_level
df <- data.frame(age = c(82), hypertension = c(1.00000), heart_disease = c(1.00000), bmi = c(10.30), avg_glucose_level = c(55))
predicted_strk <- predict(fit, df)
predicted_strk

# Test 3: Age high = 82, with high hypertension and heart_disease along with max BMI and less avg_glucose_level
df <- data.frame(age = c(82), hypertension = c(1.00000), heart_disease = c(0.00000), bmi = c(97.60), avg_glucose_level = c(55))
predicted_strk <- predict(fit, df)
predicted_strk

# Test 4: Age high = 82, with no hypertension and heart_disease, mean BMI and max avg_glucose_level
df <- data.frame(age = c(82), hypertension = c(0.00000), heart_disease = c(0.00000), bmi = c(28.89), avg_glucose_level = c(271.74))
predicted_strk <- predict(fit, df)
predicted_strk


# Test 5: Age high = 82, with no hypertension and heart_disease, less BMI and max avg_glucose_level = 271.74
df <- data.frame(age = c(82), hypertension = c(0.00000), heart_disease = c(0.00000), bmi = c(10), avg_glucose_level = c(271.74))
predicted_strk <- predict (fit, df)
predicted_strk


########################### End of CA 2 ##########################