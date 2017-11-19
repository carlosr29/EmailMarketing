#Simple modelisation using CART

library(rpart)
library(data.table)
library(ggplot2)
library(ROSE)

try(source(file = "Cleaning_Functions.R"))
campaign <- as.data.table(read.csv("PerfumeCampaigns.csv")) #we use data.table for faster and more 
campaign[,X:=NULL]
campaign[,email:=NULL]
campaign[,MessageId:=NULL]
campaign[,routerId:=NULL]
campaign[,programId:=NULL]
#efficient data manipulation
print(nrow(campaign)) # number of rows of the data set 
print(ncol(campaign)) # number of columns for the data set
summary(campaign)

campaign$Open <- as.factor(campaign$Open)
levels(campaign$Open) <- c("NoOpen","Open")

#Cleaning subject
campaign$subject <- as.character(campaign$subject)
campaign$subject <- clean_subject(subject = campaign$subject)

campaign <- na.omit(campaign)

#Adding age groups
campaign[,age_groups:=age_groups(age)]

#For the rest of the analysis we need to transform the timesend variable to have days of the week and hours
time_treatment_df <- convert_timestamp_dummy(campaign$timesend)
campaign <- cbind(campaign, time_treatment_df)
campaign[,timesend:=NULL] #deleting the timestamp variable

#Add variables for user targeting
campaign <- cbind(campaign, user_targeting_type_dummy(campaign$subject))

#Add quoted prices and promotions variables
campaign <- cbind(campaign, detect_prices(campaign$subject))

#Add all caps detection
campaign[,caps:=detect_caps(campaign$subject)]

#Making sure we have the correct format
campaign$age <- as.factor(campaign$age)
campaign$age_groups <- as.factor(campaign$age_groups)
campaign$caps <- as.factor(campaign$caps)
campaign$inscription_email <- as.factor(campaign$inscription_email)
campaign$inscription_firstname <- as.factor(campaign$inscription_firstname)
campaign$inscription_lastname <- as.factor(campaign$inscription_lastname)
campaign$inscription_zipcode <- as.factor(campaign$inscription_zipcode)

campaign$promotion <- as.factor(campaign$promotion)
campaign$prices <- as.factor(campaign$prices)

#CART Model
#To properly assess the performance of our model we need to split our data set into a training and a test set
campaign[,subject:=NULL] #We don't need a string feature

# We use a fixed random seed for the experiments to have reproductible results 
set.seed(123)

ind <- sample(1:nrow(campaign), size = floor(0.8*nrow(campaign))) #index to split the dataset
train <- campaign[ind,]
test <- campaign[-ind,]

tree_model <- rpart(Open ~., data = train, method = "class")
summary(tree_model)

predictions <- predict(tree_model, test[,-c("Open")], type = "class")
caret::confusionMatrix(predictions, test$Open)

#Problem: since the data is imbalanced, the model only predicts the majority class
#Solutions to solve this problem include: 
#Undersampling the majority class in the training dataset 
#Oversampling the minority class in the training dataset
#A combination of under and oversamlping
#Training the model by using an other metric, instead of using accuracy, use AUC, F1 measure ... 
#Apply weighting on the majority and minority class to account for the unabalance
#Class imbalance redux method

#We will use simply undersampling for now 

train_under <- ovun.sample(Open~., data = train, method = "under")
tree_model <- rpart(Open ~., data = train_under$data, method = "class")
summary(tree_model)

predictions <- predict(tree_model, test[,-c("Open")], type = "class")
caret::confusionMatrix(predictions, test$Open)

plot(tree_model)
text(tree_model, use.n = T)

