#install.packages("Hmisc")
#install.packages("zoo")
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("mlbench")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("h2o")
#install.packages("class")
#install.packages("e1071")
#install.packages("MLmetrics")

# Loading all the required libraries to the project
library(MLmetrics)
library(Hmisc)
library(zoo)
library(ggplot2)
library(corrplot)
library(mlbench)
library(caret)
library(randomForest)
library(h2o)
library(class)
library(e1071)

#Read the dataset
dengue_labels_train <- read.csv("C:/Users/manohar/Documents/R/DengAI/dengue_features_train.csv")
dengue_features_train <- read.csv("C:/Users/manohar/Documents/R/DengAI/dengue_labels_train.csv")

# merging train labels and feautures
train <- merge(dengue_labels_train,dengue_features_train,by=c('city','year','weekofyear'))

#list of feature names
colnames(train)

#converting all Kelvin temperatures to Celsius
for (i in c("reanalysis_air_temp_k", "reanalysis_avg_temp_k","reanalysis_dew_point_temp_k",
            "reanalysis_max_air_temp_k","reanalysis_min_air_temp_k")){
  train[i] <- train[i] - 273.15
}

#splitting the data into San Juan and Iquitos observations
sj.train <- subset(train, city == 'sj')
iq.train <- subset(train, city == 'iq')

#generally dengue has an incubation period of 4-10 days 
#so we shift cases back by a week using "Hmisc" package
library(Hmisc)
sj.train$total_cases <- Lag(sj.train$total_cases, 1)
iq.train$total_cases <- Lag(iq.train$total_cases, 1)

#summary of data with number of NA's
summary(train)

#attributes that are known to follow seasonal trends, we can replace them by 
#taking the most recent values
#Replacing NA's with the latest value using the zoo package's na.locf function
#
attributes <- c("precipitation_amt_mm","reanalysis_air_temp_k","reanalysis_avg_temp_k",
                "reanalysis_dew_point_temp_k","reanalysis_max_air_temp_k","reanalysis_min_air_temp_k",
                "reanalysis_precip_amt_kg_per_m2","reanalysis_relative_humidity_percent",
                "reanalysis_sat_precip_amt_mm","reanalysis_specific_humidity_g_per_kg",
                "reanalysis_tdtr_k","station_avg_temp_c","station_diur_temp_rng_c",
                "station_max_temp_c","station_min_temp_c","station_precip_mm","ndvi_ne","ndvi_nw","ndvi_se","ndvi_sw")

library(zoo)

for (i in attributes){
  sj.train[i] <- na.locf(sj.train[i], fromLast = TRUE)
  sj.train[i] <- as.numeric(unlist(sj.train[i]))
  iq.train[i] <- na.locf(iq.train[i], fromLast = TRUE)
  iq.train[i] <- as.numeric(unlist(iq.train[i]))
}

library(ggplot2)
#transforming the dates to a usable format
sj.train$week_start_date <- as.Date(sj.train$week_start_date, format = "%d-%m-%Y")
iq.train$week_start_date <- as.Date(iq.train$week_start_date, format = "%d-%m-%Y")

#plotting the total number of cases
ggplot() + 
  geom_line(data = sj.train, aes(x = week_start_date, y = total_cases, color = "red")) +
  geom_line(data = iq.train, aes(x = week_start_date, y = total_cases, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") +
  scale_colour_discrete(name = "Legend", labels = c("Iquitos", "San Juan"),guide = guide_legend(reverse = TRUE))

#Ranking of the attributes
sj.train.subset <- sj.train[c('year','weekofyear',attributes,'total_cases')]
rf <- randomForest(total_cases ~ ., data = sj.train.subset, na.action = na.omit)
varImpPlot(rf, type = 2)

#Correlation Plot
num.sj.train <- train[c(attributes,'total_cases')]
library(corrplot)
cor <- cor(num.sj.train, use = "na.or.complete")
corrplot(cor, method = "shade", type = "lower", tl.col = "black")

#Removal of feautures
for.removal <- c('city','reanalysis_tdtr_k','reanalysis_relative_humidity_percent',
                 'reanalysis_specific_humidity_g_per_kg','station_diur_temp_rng_c')


sj.train <- sj.train[, !(names(sj.train) %in% for.removal)]
iq.train <- iq.train[, !(names(iq.train) %in% for.removal)]

#autocorrelation plot
acf <- acf(ts(sj.train$total_cases), plot = FALSE, na.action = na.pass)
acf.plot <- with(acf, data.frame(lag, acf))

ggplot(data = acf.plot, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))

# adding 5 lag features
sj.train[c('lag 1','lag 2','lag 3','lag 4','lag 5')] <- NA
for (i in 1:5){
  colname = paste('lag',i)
  sj.train[colname] = Lag(sj.train$total_cases, i)
}

iq.train[c('lag 1','lag 2','lag 3','lag 4','lag 5')] <- NA
for (i in 1:5){
  colname = paste('lag',i)
  iq.train[colname] = Lag(iq.train$total_cases, i)
}

# Training and Validation
set.seed(412)
h2o <- h2o.init() # starting h2o
predictors <- colnames(sj.train[, !(names(sj.train) %in% c('week_start_date','total_cases'))])
target <- 'total_cases'

sj.train.train <- sj.train[1:round(nrow(sj.train)*0.8),]
sj.train.test <- sj.train[round(nrow(sj.train)*0.8):nrow(sj.train),]

iq.train.train <- iq.train[1:round(nrow(iq.train)*0.8),]
iq.train.test <- iq.train[round(nrow(iq.train)*0.8):nrow(iq.train),]

sj.train.train.frame <- as.h2o(sj.train.train, destination_frame = "sj.train.train")
iq.train.train.frame <- as.h2o(iq.train.train, destination_frame = "iq.train.train")

sj.train.test.frame <- as.h2o(sj.train.test, destination_frame = "sj.train.test")
iq.train.test.frame <- as.h2o(iq.train.test, destination_frame = "iq.train.test")

#Random Forest Classifier
sj.rf.train = h2o.randomForest(
  x = predictors,
  y = target,
  training_frame = sj.train.train.frame,
  validation_frame = NULL,
  model_id = "rf_sj_dengai_train",
  ntrees = 1000, mtries = 10)

iq.rf.train = h2o.randomForest(
  x = predictors,
  y = target,
  training_frame = iq.train.train.frame,
  validation_frame = NULL,
  model_id = "rf_iq_dengai_train",
  ntrees = 1000, mtries = 10)

h2o.mae(sj.rf.train)
h2o.rmse(sj.rf.train)
h2o.r2(sj.rf.train)
h2o.mae(iq.rf.train)
h2o.rmse(iq.rf.train)
h2o.r2(iq.rf.train)

sj.test.pred <- as.data.frame(h2o.predict(sj.rf.train, sj.train.test.frame))
iq.test.pred <- as.data.frame(h2o.predict(iq.rf.train, iq.train.test.frame))

sj.train.test$pred <- Lag(sj.test.pred$predict,-2)

ggplot() + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = total_cases, color = "red")) + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("San Juan") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))

iq.train.test$pred <- Lag(iq.test.pred$predict,-2)

ggplot() + 
  geom_line(data = iq.train.test, aes(x = week_start_date, y = total_cases, color = "red")) +
  geom_line(data = iq.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("Iquitos") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))



#Deep Learning Classifier
sj.dl.train = h2o.deeplearning(x = predictors,  # column numbers for predictors
                               y = target,   # column number for label
                               training_frame = sj.train.train.frame, # data in H2O format
                               activation = "RectifierWithDropout", # or 'Tanh' or Rectifier
                               hidden = c(50,50,50,50,50)) # hidden layers

iq.dl.train = h2o.deeplearning(x = predictors,  # column numbers for predictors
                               y = target,   # column number for label
                               training_frame = iq.train.train.frame, # data in H2O format
                               activation = "RectifierWithDropout", # or 'Tanh' 
                               hidden = c(50,50,50,50,50)) # hidden layers

sj.test.pred <- as.data.frame(h2o.predict(sj.dl.train, sj.train.test.frame))
iq.test.pred <- as.data.frame(h2o.predict(iq.dl.train, iq.train.test.frame))

h2o.mae(sj.dl.train)
h2o.rmse(sj.dl.train)
h2o.r2(sj.dl.train)
h2o.mae(iq.dl.train)
h2o.rmse(iq.dl.train)
h2o.r2(iq.dl.train)

sj.train.test$pred <- Lag(sj.test.pred$predict,-2)

ggplot() + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = total_cases, color = "red")) + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("San Juan") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))

iq.train.test$pred <- Lag(iq.test.pred$predict,-2)

ggplot() + 
  geom_line(data = iq.train.test, aes(x = week_start_date, y = total_cases, color = "red")) +
  geom_line(data = iq.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("Iquitos") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))


# Gradient Boosting Classifier

sj.gb.train = h2o.gbm(
  x = predictors,
  y = target,
  training_frame = sj.train.train.frame,
  validation_frame = NULL,
  model_id = "rf_sj_dengai_train",
  ntrees = 1000, stopping_rounds = 10, max_depth = 4, learn_rate = 0.01)

iq.gb.train = h2o.gbm(
  x = predictors,
  y = target,
  training_frame = iq.train.train.frame,
  validation_frame = NULL,
  model_id = "rf_iq_dengai_train",
  ntrees = 1000, stopping_rounds = 10, max_depth = 4, learn_rate = 0.01)

h2o.mae(sj.gb.train)
h2o.mae(iq.gb.train)

sj.test.pred <- as.data.frame(h2o.predict(sj.gb.train, sj.train.test.frame))
iq.test.pred <- as.data.frame(h2o.predict(iq.gb.train, iq.train.test.frame))

sj.train.test$pred <- Lag(sj.test.pred$predict,-2)

ggplot() + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = total_cases, color = "red")) + 
  geom_line(data = sj.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("San Juan") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))

iq.train.test$pred <- Lag(iq.test.pred$predict,-2)

ggplot() + 
  geom_line(data = iq.train.test, aes(x = week_start_date, y = total_cases, color = "red")) +
  geom_line(data = iq.train.test, aes(x = week_start_date, y = pred, color = "blue")) +
  xlab("Weeks") + ylab("Total number of cases") + ggtitle("Iquitos") +
  scale_colour_discrete(name = "Legend", labels = c("Actual", "Predicted"))





