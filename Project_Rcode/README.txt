Instructions for executing the Project
------------------------------------------------------------
We have done the coding in R programming Language and IDE is R Studio.
1) Open RSTUDIO and make sure that the below packages are installed
   Install them if they are not installed using > install.packages("Packagename", DEPENDENCIES = TRUE)
   or simply uncomment the install.packages() lines in "projectCode.R" R code.
   
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

2) Give dataset dengue_features.csv and dengue_labels.csv path at dat <- read.csv("/Provide Path of Dataset here")
3) Run the R code file to output preprocessing strategies and mean absolute error values and 
plot of predicted vs actual cases of two two cities with time scale.
4) Since our problem is regression we used model metrics as MAE and RMSE.
5) Out of all our classifiers Gradient Boosting gave us good performance. 