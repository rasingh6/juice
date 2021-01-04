# This file was created from the Rmd file using the knitr::purl command as shown below
# knitr::purl("FINAL_PROJECT.Rmd")

# It is essentially the same file as as the Rmd file with the text removed
# Rmd code sections are still visible as comments and have the code chunk name and options shown



## ----setup, include=TRUE, message=FALSE, warning=FALSE, echo=TRUE--------------------------------------------------------------
#set global options for code chunks to no messages or warnings
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = TRUE
)

#install packages if needed
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(rpart)) install.packages("rpart")
if(!require(rpart.plot)) install.packages("rpart.plot")
if(!require(AppliedPredictiveModeling)) install.packages("AppliedPredictiveModeling")
if(!require(broom)) install.packages("broom")
if(!require(corrplot)) install.packages("corrplot")
if(!require(kernlab)) install.packages("kernlab")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(knitr)) install.packages("knitr")

if(!require(mboost)) install.packages("mboost")
if(!require(import)) install.packages("import")
if(!require(gam)) install.packages("gam")
if(!require(kknn)) install.packages("kknn")
if(!require(Rborist)) install.packages("Rborist")
if(!require(mgcv)) install.packages("mgcv")
if(!require(nlme)) install.packages("nlme")
if(!require(RSNNS)) install.packages("RSNNS") 
if(!require(corrplot)) install.packages("corrplot")
if(!require(randomForest)) install.packages("randomForest")
if(!require(FactoMineR)) install.packages("FactoMineR")
if(!require(factoextra)) install.packages("factoextra")
detach(package:RSNNS) #this package masks key functions from the caret package


#Load the libraries
library(tidyverse)
library(caret)
library(knitr)
library(AppliedPredictiveModeling)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(kableExtra)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rpart)
library(broom)
library(corrplot)

#turn off scientific notation and set number of digits to print
options(scipen = 999, digits = 5)



## ----read_data-----------------------------------------------------------------------------------------------------------------
# read the dataset for white wine
white <- read_delim("juice.csv", delim = ";")
# get rid of the spaces in the column names
colnames(white)<- colnames(white) %>% str_replace_all(" ", "_")



## ----data_str------------------------------------------------------------------------------------------------------------------
#display the data structure
str(white)


## ----check_NA------------------------------------------------------------------------------------------------------------------
#get index of any NA values
which(is.na(white))


## ----data_summary--------------------------------------------------------------------------------------------------------------
# display summary statistics
summary(white)


## ----create_check_and_train-----------------------------------------------------------------------------------------------------
# set the seed for repeatability
set.seed(831, sample.kind = "Rounding")

#create train and check sets for white wine
check_index <- createDataPartition(white$quality, times = 1, p = 0.2, list = FALSE)
train_rating <- white %>% slice(-check_index)
check_rating <- white %>% slice(check_index)



## ----create_check_and_train2, echo=TRUE-----------------------------------------------------------------------------------------
# Create a second train and check set with categories instead of ratings
# The categories are acceptable, good, premium
# These are the same data sets as the rating sets, but with the quality factor 
# Changed to a category
check_category <- check_rating %>%
  mutate(quality = as.factor(
    ifelse(quality >=8, "premium",
           ifelse(quality <= 4, "acceptable", "good") )))

train_category <- train_rating %>%
  mutate(quality = as.factor(
    ifelse(quality >=8, "premium",
           ifelse(quality <= 4, "acceptable", "good") )))

check_category <- check_rating %>%
  mutate(quality = as.factor(quality))

train_category <- train_rating %>%
  mutate(quality = as.factor(quality))


## ----correlation_matrix, echo=FALSE, fig.cap="Correlation Plot"----------------------------------------------------------------
# display a table containing the correlation matrix
kable(cor(train_rating), booktabs = T, caption = "Correlation Matrix") %>%
  kable_styling(latex_options = c("scale_down", "hold_position"))
# create the correlation plot
corrplot(cor(train_rating))


## ----rating_hist, echo=FALSE, fig.cap="Quality Rating Histogram"---------------------------------------------------------------
# display a histogram of quality ratings
train_rating %>% ggplot(aes(quality)) +
  geom_histogram(bins = 7, col="red") + 
  labs(title = "Quality Ratings") 



## ----rating_trend, echo=FALSE, fig.cap="Features Check Results"-----------------------------------------------------------------
# reshape the training data to long for use with facet plotting
wlong_rating <- train_rating  %>% mutate(obs = 1:n()) %>%
  select(obs, everything())%>% 
  pivot_longer(cols = fixed_acidity:alcohol,
               names_to = "check",
               values_to = "result")
# create the plots
wlong_rating %>% ggplot(aes(quality, result)) +
  geom_point() +
  geom_smooth(method = "lm", ) +
  facet_wrap(check ~ ., scales = "free")



## ----significance, echo=FALSE--------------------------------------------------------------------------------------------------
# get the column names excluding "quality"
cn <- colnames(train_rating)[-12]

# this function returns the p value for a data column
# it is implementing the function: lm(quality ~ col_name)
# it uses the train_rating dataframe
linear_fit <- function(col_name){
  # create a formula using the input check_name
  fmla <- as.formula(paste("quality ~", col_name))
  #calculate the fit using the linear model
  fit <- lm(fmla, data = train_rating)
  # returns the p value for a model
  p_val <- summary(fit)$coefficients[,"Pr(>|t|)"][2]
  #round the returned p value to 4 digits
  return(round(p_val,4))
}

#calculate the p value for the columns 
sa <- sapply(cn, linear_fit)
#name the columns
names(sa) <- cn

# display the significant p values
sig <- sa < 0.05
sa[sig]




## ----significance2, echo=FALSE-------------------------------------------------------------------------------------------------
# display the p values that are not significant
sa[!sig]




## ----box_plot_facet, fig.width=7, fig.height=7, fig.cap="Check Result vs Quality", echo=FALSE-----------------------------------
#create a narrow data frame
#add observation the the data frame and make it the first column
# reshape the data to facilitate plotting and data visualization
wlong <- train_rating  %>% 
  mutate(obs = 1:n()) %>%
  mutate(quality = as.factor(ifelse(quality >=8, "premium",
                                    ifelse(quality <= 4, 
                                           "acceptable", "good") ))) %>%
  select(obs, everything())%>% 
  pivot_longer(cols = fixed_acidity:alcohol,
               names_to = "check",
               values_to = "result")

# produce the box plots using facet_wrap
wlong %>% ggplot(aes(result, quality)) +
  geom_boxplot() +
  facet_wrap(check ~ ., scales = "free")



## ----scatterplot1, fig.cap="Features Pairwise Comparison 1", fig.height=7, fig.width=7, echo=FALSE-----------------------------
#set transparency for the feature plots
transparentTheme(trans = .4)

# reduce categories to three, acceptable, good, premium
tw <- train_rating %>% 
  mutate(quality = as.factor(
    ifelse(quality >=8, "premium",
           ifelse(quality <= 4, "acceptable", "good") )))
#plot the first 5 features
featurePlot(x = tw[, 1:5], 
            y = tw$quality, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))


## ----scatterplot2, fig.cap="Features Pairwise Comparison 2", fig.height=7, fig.width=7, echo=FALSE-----------------------------
#plot the remaining 6 features
featurePlot(x = tw[, 6:11], 
            y = tw$quality, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))


## ----standardize---------------------------------------------------------------------------------------------------------------
# using caret
# Create the subset of features
# features <- train_rating[ , -12]
y_num <- train_rating[ , 12]
y_cat <- train_category[ , 12]
y_check_num <- check_rating[ , 12]
y_check_cat <- check_category[ , 12]

#obtain the preProcessValues
# there is only one set of features, so only 1 preProcValues object is needed.
preProcValues <- preProcess(train_rating[,-12], method = c("center", "scale"))

# calculate the features for the training set
train_feat <- predict(preProcValues, train_rating[ , -12])
# the check set is transformed using the mean and sd from the train set
# that are stored in the preProcValues List
check_feat <- predict(preProcValues, check_rating[ , -12])



## ----check_standardized, echo=FALSE--------------------------------------------------------------------------------------------
# check the transformations
# function calculates the mean and sd for a feature column
mean_and_sd <- function(feature){
  c_mu <- mean(feature) %>% round(3)
  c_sd <- sd(feature) %>% round(3)
  return (c(c_mu, c_sd))
} 

#display the transformed data means and standard deviations
cat("Mean and Standard Deviations for the training set:\n")
sapply(train_feat, mean_and_sd)

cat("\n\nMean and Standard Deviations for the check set:\n")
# we are only checking the transformation of the features here
# we are not making any other changes
sapply(check_feat, mean_and_sd)
#sapply(check_category_xformed, mean_and_sd)


# remove the non-transformed data sets to free memory and prevent accidental use
rm(train_rating, check_rating, train_category, check_category)


## ----decision_tree, fig.cap="Decision Tree for Numeric Ratings"----------------------------------------------------------------
#create the training and check sets from the standardized features
train <- cbind(train_feat, y_num)
check <- cbind(check_feat, y_check_num )
# train the rpart model 
train_rpart2 <- train(quality ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0.0,0.1, len=25)),
                      data = train)
# check the rmse of the model 
y_hat <- predict(train_rpart2, check)
rmse_rpart_tuned <- RMSE(check$quality, y_hat)
#display the rmse results
cat("The RMSE for the model produced by the decision tree is", rmse_rpart_tuned )

# predictors from the rpart model 
rpart.plot(train_rpart2$finalModel,
           main = "Decision Tree\nStandardized Features",
           type = 5,
           yesno = 2,
           cex = .7)



## ----dt_features, echo=FALSE---------------------------------------------------------------------------------------------------
#this chunk of code returns the tree terms that are used in the model
ind <- !(train_rpart2$finalModel$frame$var == "<leaf>")
tree_terms <-
  train_rpart2$finalModel$frame$var[ind] %>%
  unique() %>%
  as.character()
#display the tree terms
tree_terms



## ----varimp--------------------------------------------------------------------------------------------------------------------
#variable importance
vi <- varImp(train_rpart2)$importance %>% arrange(desc(Overall))
#save the variable importance for future comparison to other unsupervised models
vi_table <- as_tibble_col(rownames(vi)[1:4], column_name = "rating")
#display the variable importance
vi


## ----premium_decision_tree, fig.cap="Decision Tree for Premium Category"-------------------------------------------------------
#set up the categories as premium or other
y_prem <- y_num %>% mutate(quality = ifelse(quality < 6, "other", "premium"))
#create the training data set
train <- cbind(train_feat, y_prem)
#train the model for the category analysis
train_rpart2 <- train(quality ~ ., 
                      method = "rpart", 
                      tuneGrid = data.frame(cp = seq(0.0,0.1, len=25)),
                      data = train)

# prune the tree to reduce the number of branches
pruned <- prune(train_rpart2$finalModel, cp = 0.01)

#plot the tree
rpart.plot(pruned,
           main = "Decision Tree - Premium Category\nStandardized Features",
           type = 5,
           yesno = 2,
           cex = .7)


## ----premium_dt_varimp---------------------------------------------------------------------------------------------------------
#this chunk of code returns the tree terms that are used in the model
ind <- !(train_rpart2$finalModel$frame$var == "<leaf>")
tree_terms <-
  train_rpart2$finalModel$frame$var[ind] %>%
  unique() %>%
  as.character()
#display the tree terms
tree_terms

#variable importance
vi <- varImp(train_rpart2)$importance %>% arrange(desc(Overall))
#save the variable importance for future comparison to other unsupervised models
vi_table <- add_column(vi_table, premium = rownames(vi)[1:4])
#display the variable importance
vi



## ----PCA, fig.cap="PCA Variables Plot", echo=FALSE-----------------------------------------------------------------------------
#perform the pca analysis using the FactoMineR library
res.pca <- PCA(train_feat, graph = FALSE)
#plot the variable contributions for dimensions 1 and 2
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))



## ----corrplot, fig.cap="Correlation Plot", echo=FALSE--------------------------------------------------------------------------
# get the pca results for the variables
var <- get_pca_var(res.pca)
# plot the correlation
corrplot(var$cos2, is.corr=FALSE)

#calculate feature contribution for the first dimension
vc <- as_tibble(var$contrib)%>% 
  select(Dim.1)
#put the rownames from the var$contrib matrix on the tibble as the feature col
vc$feature <- rownames(var$contrib)
#move the feature column to the first column and sort by descending value
vc <- relocate(vc, feature, .before = Dim.1) %>% 
  arrange(desc(Dim.1))
#display the variable contribution of dimension 1
vc


## ----PCA_factors, fig.cap="Important Features from PCA", echo=FALSE------------------------------------------------------------
# select the most important features and store them in the tw object
# at the same time, convert the quality ratings into acceptable, good, premium
tw <- train_feat %>% select(fixed_acidity, density, pH, 
                            alcohol, residual_sugar, 
                            total_sulfur_dioxide) %>% 
  cbind(y_num) %>%
  mutate(quality = as.factor(
    ifelse(quality >=8, "premium",
           ifelse(quality <= 4, "acceptable", "good") )))

# create the feature plots
featurePlot(x = tw[, 1:6 ], 
            y = tw$quality, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))





## ----pca_variable_importance---------------------------------------------------------------------------------------------------
#variable importance
#save the variable importance for future comparison to other unsupervised models
vi_table <- add_column(vi_table, PCA = vc$feature[1:4])
#display the variable importance table
kable(vi_table, caption = "Variable Importance", booktabs=T)%>% 
  kable_styling(latex_options = "hold_position")


## ----regression_models, cache=TRUE---------------------------------------------------------------------------------------------
#create the check and training data sets from the standardized features
train <- train_feat %>% cbind(y_num)
check <- check_feat %>% cbind(y_check_num)


# create a list of machine learning models to evaluate
models <- c("glm", "svmLinear", "gamboost","gamLoess", "knn", 
            "kknn", "gam","ranger", "rf", "Rborist", "mlp", 
            "svmRadial", "svmRadialCost", "svmRadialSigma")

#this function will evaluate each function in the supplied list
#it returns a list of training models
group_train <- function(model_list, seed = 831){
  # set the seed for the model run or use the user override
  set.seed(seed, sample.kind = "Rounding")
  # get the length of the model list
  l <- length(model_list)
  # train the first model in the input list and initialize the train_list
  train_result <- train(quality ~ ., method = model_list[1], data = train)
  train_list <- list(train_result)
  # train the remaining models
  for (i in 2:l){
    train_result <- train(quality ~ ., method = model_list[i], data = train)
    train_list[[i]] <- train_result
  }
  # name the models in the list
  names(train_list) <- model_list
  # return the list of training models
  return(train_list)
}
# train the models
model_list <- group_train(models)

# Create a prediction matrix with a column for each ML Model
p <- sapply(model_list,predict, check)

# Calculate the rmse for each model
a <- apply(p, 2, RMSE, check$quality) %>% enframe(name="Model", value = "RMSE")
# display the rmse table
kable(a, caption = "Regression Models", booktabs=T)%>% 
  kable_styling(latex_options = "hold_position")




## ----random_forest, echo=TRUE, cache=TRUE--------------------------------------------------------------------------------------
#reset the train and check sets for evaluation as a numeric
train <- train_feat %>% cbind(y_num)
check <- check_feat %>% cbind(y_check_num)

#train the model using a tuning grid
train_rborist <- train(quality ~ .,
                       method = "Rborist",
                       tuneGrid = data.frame(predFixed = 2,
                                             minNode = c(3, 50)),
                       data = train)

#calculate the predicted values for the check set
y_hat <- predict(train_rborist, check)
#calculate the model RMSE
rmse_rborist <- RMSE(check$quality, y_hat)
#display the results
cat("The RMSE for the tuned Rborist model is", rmse_rborist)



## ----classification_models, cache=TRUE-----------------------------------------------------------------------------------------

#reset the train and check sets for modeling as categories
train <- train_feat %>% cbind(y_cat)
check <- check_feat %>% cbind(y_check_cat)

# create a list of machine learning models to evaluate
models <- c("knn", "kknn", "Rborist", "ranger", "mlp","svmRadial")

#this function will evaluate each function in the supplied list
#it returns a list of training models
group_train <- function(model_list, seed = 831){
  # set the seed for the model run or use the user override
  set.seed(seed, sample.kind = "Rounding")
  # get the length of the model list
  l <- length(model_list)
  # train the first model in the input list and initialize the train_list
  train_result <- train(quality ~ ., method = model_list[1], data = train)
  train_list <- list(train_result)
  # train the remaining models
  for (i in 2:l){
    train_result <- train(quality ~ ., method = model_list[i], data = train)
    train_list[[i]] <- train_result
  }
  # name the models in the list
  names(train_list) <- model_list
  # return the list of training models
  return(train_list)
}
# train the models
model_list <- group_train(models)

# calculate the rmse
RMSE2 <- function(predicted_ratings, true_ratings){
  #the min factor is 3, so add 2 to the conversion to get the rating correct
  tr <- as.numeric(true_ratings) +2
  pr <- as.numeric(predicted_ratings)
  sqrt(mean((tr - pr)^2))
}

# Create a prediction matrix with a column for each ML Model
p <- sapply(model_list, predict, check)

# Calculate the rmse for each model
a <- apply(p, 2, RMSE2, check$quality) %>% enframe(name="Model", value = "RMSE")
kable(a, caption = "Classification Models", booktabs=T) %>% 
  kable_styling(latex_options = "hold_position")

#initialize an empty tibble to hold the accuracy results
accuracy_df <- tibble(Model = character(),
                      Accuracy = numeric())
# calculate and display the accuracy for all of the models run
for(i in 1:ncol(p)){
  y_hat <- factor(p[,i], levels(check$quality))
  cm <- confusionMatrix(y_hat, check$quality)
  accuracy_df[i,1] <- models[i]
  accuracy_df[i,2] <- cm$overall["Accuracy"]
}
kable(accuracy_df, caption = "Classification Model Accuracy", booktabs=T )%>% 
  kable_styling(latex_options = "hold_position")

# Get the predictors for the best model, which is "ranger"
y_hat <- factor(p[,"ranger"], levels(check$quality))

# print out the specificity and sensitivity
cm <- confusionMatrix(y_hat, check$quality)
cm$overall["Accuracy"]
#cm$byClass[,1:2]

#use the broom function to get the confusion matrix output in tidy format
tcm <- tidy(cm)
#drop the first two rows of the data and then create a wide version
tcm <- tcm[3:nrow(tcm),] %>% 
  select(term, class, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  select(class, sensitivity, specificity, prevalence, detection_rate)
#display the results
tcm


#save the predictions from the ranger model to use in an ensemble
y_hat_ranger <- y_hat 


## ----knn_cm, cache=TRUE--------------------------------------------------------------------------------------------------------
# Get the predictors for the knn model as an example
y_hat <- factor(p[,1], levels(check$quality))
# print out the specificity and sensitivity
cm <- confusionMatrix(y_hat, check$quality)
cm$overall["Accuracy"]
#cm$byClass[,1:2]

#use the broom function to get the confusion matrix output in tidy format
tcm2 <- tidy(cm)
#drop the first two rows of the data and then create a wide version
tcm2 <- tcm2[3:nrow(tcm2),] %>% 
  select(term, class, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  select(class, sensitivity, specificity, prevalence, detection_rate)
#display the results
tcm2


## ----prevalence_dataset, fig.cap="Leveled Quality Ratings"---------------------------------------------------------------------
# create clean train and check sets
train <- train_feat %>% cbind(y_cat)
check <- check_feat %>% cbind(y_check_cat)

#determine the number of observations by rating
obs_by_category <- train %>% group_by(quality) %>% summarize(obs = n())

#create a leveled dataset with 2000 observations for each rating
train_level_prev <- train
for(i in 1:7){
  obs <- obs_by_category[i,2] %>% .$obs
  qlty <- obs_by_category[i, 1] %>% .$quality
  sample_obs <- train %>% filter(quality == qlty)
  ind <- sample(1:nrow(sample_obs), 2000 - obs, replace = TRUE)
  new_obs <- sample_obs[ind,]
  train_level_prev <- rbind(train_level_prev, new_obs)
}



## ----leveled_models, cache=TRUE------------------------------------------------------------------------------------------------
#set the training set for the leveled model run 
train <- train_level_prev
#remove the redundant object
rm(train_level_prev)

#reset the check set for modeling as categories
# train <- train_feat %>% cbind(y_cat)
check <- check_feat %>% cbind(y_check_cat)

# create a list of machine learning models to evaluate
models <- c("knn", "kknn", "ranger", "rf", "Rborist", "mlp")

#this function will evaluate each function in the supplied list
#it returns a list of training models
group_train <- function(model_list, seed = 831){
  # set the seed for the model run or use the user override
  set.seed(seed, sample.kind = "Rounding")
  # get the length of the model list
  l <- length(model_list)
  # train the first model in the input list and initialize the train_list
  train_result <- train(quality ~ ., method = model_list[1], data = train)
  train_list <- list(train_result)
  # train the remaining models
  for (i in 2:l){
    # print(model_list[i])
    train_result <- train(quality ~ ., method = model_list[i], data = train)
    train_list[[i]] <- train_result
  }
  # name the models in the list
  names(train_list) <- model_list
  # return the list of training models
  return(train_list)
}
# train the models
model_list <- group_train(models)

# Create a prediction matrix with a column for each ML Model
# using the caret predict function
p <- sapply(model_list, predict, check)

# Get the predictors for the ranger model results
y_hat <- factor(p[,"ranger"], levels(check$quality))

#calculate and display the confusion matrix
cm <- confusionMatrix(y_hat, check$quality)
cm$overall["Accuracy"]

#use the broom function to get the confusion matrix output in tidy format
tcm_r <- tidy(cm)
#drop the first two rows of the data and then create a wide version
tcm_r <- tcm_r[3:nrow(tcm_r),] %>% 
  select(term, class, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  select(class, sensitivity, specificity, prevalence, detection_rate)
#display the results
cat("\n\n\nThe confusion matrix results for the ranger model after leveling are:\n")
tcm_r

# create a data frame to store the accuracy results
accuracy_df <- tibble(Model = character(),
                      Accuracy = numeric())
# print the accuracy for all of the models run
for(i in 1:ncol(p)){
  y_hat <- factor(p[,i], levels(check$quality))
  cm <- confusionMatrix(y_hat, check$quality)
  accuracy_df[i,1] <- models[i]
  accuracy_df[i,2] <- cm$overall["Accuracy"]
}
kable(accuracy_df, caption = "Leveled Model Accuracy", booktabs=T )%>% 
  kable_styling(latex_options = "hold_position")





## ---- leveled_results, cache=TRUE----------------------------------------------------------------------------------------------
#create a function to calculate the RMSE values 
RMSE2 <- function(predicted_ratings, true_ratings){
  #the min factor is 3, so add 2 to the conversion to get the rating correct
  tr <- as.numeric(true_ratings) +2
  pr <- as.numeric(predicted_ratings)
  sqrt(mean((tr - pr)^2))
}

# Calculate the rmse for each model
a <- apply(p, 2, RMSE2, check$quality) %>% enframe(name="Model", value = "RMSE")
# display the rmse results
kable(a, caption = "Leveled Model RMSE", booktabs=T)%>% 
  kable_styling(latex_options = "hold_position")



## ----knn changes---------------------------------------------------------------------------------------------------------------
# show the pre-leveled results
cat("The confusion matrix results for the knn model prior to leveling were:\n")
tcm2
# Get the predictors for the knn model results
y_hat <- factor(p[,1], levels(check$quality))

#calculate and display the confusion matrix
cm <- confusionMatrix(y_hat, check$quality)
cm$overall["Accuracy"]

#use the broom function to get the confusion matrix output in tidy format
tcm <- tidy(cm)
#drop the first two rows of the data and then create a wide version
tcm <- tcm[3:nrow(tcm),] %>% 
  select(term, class, estimate) %>%
  pivot_wider(names_from = term, values_from = estimate) %>%
  select(class, sensitivity, specificity, prevalence, detection_rate)
#display the results
cat("\n\n\nThe confusion matrix results for the knn model after leveling are:\n")
tcm
