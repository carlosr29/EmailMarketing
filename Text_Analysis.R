library(rpart)
library(pROC)
library(data.table)
library(ggplot2)
library(rpart.utils)
library(rpart.plot)
library(ROSE)
library(lubridate)
library(stringr)

try(source(file = "Cleaning_Functions.R"))
try(source(file = "Geo_Eco_Data.R"))
try(source(file = "get_text_features.R"))
campaign <- as.data.table(read.csv("campaign_course.csv")) #we use data.table for faster and more
campaign[,age:=round((Sys.Date() - as.Date(birthday))/lubridate::dyears(1))]

campaign <- campaign[age >= 18 & age <= 100,]
print(nrow(campaign)) # number of rows of the data set 
print(ncol(campaign)) # number of columns for the data set
summary(campaign)

campaign$Open <- as.factor(campaign$Open)
levels(campaign$Open) <- c("NoOpen","Open")

campaign$subject <- as.character(campaign$subject)

campaign <- na.omit(campaign)

#Adding age groups
campaign[,birthday:=NULL]
campaign[,age_groups:=age_groups(age)]

#Adding length of the subject
campaign[,length:=add_length(subject)]


#For the rest of the analysis we need to transform the timesend variable to have days of the week and hours
time_treatment_df <- convert_timestamp_dummy(campaign$sendtime)
campaign <- cbind(campaign, time_treatment_df)
campaign[,sendtime:=NULL] #deleting the timestamp variable

#Add variables for user targeting
campaign <- cbind(campaign, user_targeting_type_dummy(campaign$subject))

#Add quoted prices and promotions variables
campaign <- cbind(campaign, detect_prices(campaign$subject))

#Add all caps detection and hostnames
campaign[,caps:=detect_caps(campaign$subject)]
campaign[,hostname := get_hostnames(campaign$email, threshold = 1000)]
campaign[,email:=NULL]
#Making sure we have the correct format
campaign$age <- as.factor(campaign$age)
campaign$age_groups <- as.factor(campaign$age_groups)
campaign$caps <- as.factor(campaign$caps)
campaign$inscription_email <- as.factor(campaign$inscription_email)
campaign$inscription_firstname <- as.factor(campaign$inscription_firstname)
campaign$inscription_lastname <- as.factor(campaign$inscription_lastname)
campaign$inscription_zipcode <- as.factor(campaign$inscription_zipcode)

campaign$promotion <- as.factor(campaign$promotion)
campaign$brand <- as.factor(detect_brand_product(campaign$subject))
campaign$timeref <- detect_time_reference(campaign$subject)
campaign <- cbind(campaign, detect_punctuation(campaign$subject))
campaign$wordscount <- add_words_count(campaign$subject)
campaign$shortwordscount <- add_short_words_count(campaign$subject)

#Adding geo features
#adding eco features

campaign <- add_eco_features(campaign)
campaign <- add_pov_features(campaign)
campaign <- add_geo_features(campaign)
campaign <- na.omit(campaign)

campaign$code_post_alt_mean <- scale(campaign$code_post_alt_mean)
campaign$code_post_pop_mean <- scale(campaign$code_post_pop_mean)
campaign$code_post_densite_mean <- scale(campaign$code_post_densite_mean)
campaign$code_post_superficie_mean <- scale(campaign$code_post_superficie_mean)

#Adding text features
subject <- campaign$subject
#if high tech
#campaign[,c("dot","promotion","currency_mention","inscription_gender","inscription_zipcode","inscription_lastname","inscription_email"):=NULL]
old_campaign <- campaign
add_dtm = F; add_bigram = T;
if (add_dtm == T){
  campaign <- cbind(campaign, add_dtm_features(subject))
} else if (add_bigram == T){
  campaign <- cbind(campaign, add_bigram_features(subject))
}
#Number of words
#Number of short keywords (less than four characters)
#If the subject contains numbers
#Wether the subject line contains punctuation

#Simple modelisation using CART
#Number of words
#Number of short keywords (less than four characters)
#If the subject contains numbers
#Wether the subject line contains punctuation

#CART Model
#To properly assess the performance of our model we need to split our data set into a training and a test set
campaign[,subject:=NULL] #We don't need a string feature
campaign$hostname <- as.factor(campaign$hostname)
campaign$zipcode <- as.factor(campaign$zipcode)
campaign$proploc <- as.factor(campaign$proploc)
campaign$professional <- as.factor(campaign$professional)
campaign$budget <- as.factor(campaign$budget)

# We use a fixed random seed for the experiments to have reproductible results 
set.seed(123)

ind <- sample(1:nrow(campaign), size = floor(0.8*nrow(campaign))) #index to split the dataset
selected_predictors <- setdiff(colnames(campaign), c("age", "zipcode","X", "code_post_superficie_mean","code_post_pop_mean"))
train <- campaign[ind,selected_predictors, with=F]
test <- campaign[-ind,selected_predictors, with=F]

tree_model <- rpart(Open ~., data = train, method = "class")
summary(tree_model)

predictions <- predict(tree_model, test[,-c("Open")], type = "class")
caret::confusionMatrix(predictions, test$Open)

#Problem: since the data is imbalanced, the model only predicts the majority class (Accuracy of 94%, sensitivity = 1, specificity = 0)
#Solutions to solve this problem include: 
#Undersampling the majority class in the training dataset 
#Oversampling the minority class in the training dataset
#A combination of under and oversamlping
#Training the model by using an other metric, instead of using accuracy, use AUC, F1 measure ... 
#Apply weighting on the majority and minority class to account for the unabalance
#Class imbalance redux method

#We will use simply undersampling for now 
tree_control <- rpart::rpart.control(maxdepth = 15)
train_under <- ovun.sample(Open~., data = train, method = "under")
tree_model <- rpart(Open ~., data = train_under$data, method = "class", control = tree_control)
summary(tree_model)

rpart.utils::rpart.lists(tree_model)
rpart.plot(tree_model, fallen.leaves = F)
predictions_tree <- predict(tree_model, test[,-c("Open")], type = "class")

caret::confusionMatrix(predictions, test$Open, positive = "Open")
caret::F_meas(caret::confusionMatrix(predictions_tree, test$Open, positive = "Open")$table)

#We use the ROC Curve to asses the performance of our models
predictions_tree_prob <- predict(tree_model, test[,-c("Open")], type = "prob")

cat("Current model: simple tree model has AUC of:",pROC::auc(test$Open, predictions_tree_prob[,2]), "\n")
plot.roc(test$Open, predictions_tree_prob[,2], ci = T, print.auc = T)
pROC::roc(test$Open, predictions_tree_prob[,2], ci = T)

rf_model <- randomForest::randomForest(as.factor(Open)~., data=train_under$data, ntree=501)
predictions <- predict(rf_model, test[,-c("Open")], type = "class")
caret::confusionMatrix(predictions, test$Open, positive = "Open")
caret::F_meas(caret::confusionMatrix(predictions, test$Open, positive = "Open")$table)

#We use the ROC Curve to asses the performance of our models
predictions_proba <- predict(rf_model, test[,-c("Open")], type = "prob")

cat("Current model: random Forest model has AUC of:",pROC::auc(test$Open, predictions_proba[,2]), "\n")
pROC::roc(test$Open, predictions_proba[,2], ci = T)
plot.roc(test$Open, predictions_proba[,2], ci = T, print.auc = T)
randomForest::varImpPlot(rf_model)
