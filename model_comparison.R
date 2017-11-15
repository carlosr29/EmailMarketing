library(rpart)
library(pROC)
library(data.table)
library(ggplot2)
library(rpart.utils)
library(rpart.plot)
library(ROSE)
library(stringr)

try(source(file = "Cleaning_Functions.R"))
try(source(file = "Geo_Eco_Data.R"))
try(source(file = "get_text_features.R"))
campaign <- as.data.table(read.csv("campaign_vetement.csv")) #we use data.table for faster and more
campaign[,age:=round((Sys.Date() - as.Date(birthday))/dyears(1))]

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
campaign <- cbind(campaign, user_targeting_type_dummy(campaign$subject, only_targetings = T))

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
old_campaign <- campaign
add_dtm = F; add_bigram = F;
if (add_dtm == T){
  campaign <- cbind(campaign, add_dtm_features(subject))
} else if (add_bigram == T){
  campaign <- cbind(campaign, add_bigram_features(subject))
}
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
selected_predictors <- setdiff(colnames(campaign), c("age", "zipcode","X","notre","êtes","des","pour","votre","nous","vous", "code_post_superficie_mean","code_post_pop_mean"))
train <- campaign[ind,selected_predictors, with=F]
test <- campaign[-ind,selected_predictors, with=F]

train_under <- ovun.sample(Open~., data = train, method = "under")


library(caret)
library(doParallel)
cl <- makeCluster(4, outfile = "")
doParallel::registerDoParallel(cl)
#registerDoMC(8)
#registerDoMC(cores=12)
set.seed(123) #make sur to have reproductible results

#GBM
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9, 11), 
                        n.trees = (1:30)*50+1, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

fitControl <- trainControl(method = "cv",
                           number = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           search = "grid",
                           summaryFunction = twoClassSummary,
                           verboseIter=TRUE,
                           allowParallel=TRUE)

gbmFit <- train(Open ~ ., data = train_under$data, 
                method = "gbm", 
                trControl = fitControl, 
                tuneGrid = gbmGrid,
                ## Specify which metric to optimize
                metric = "ROC")
#KNN 
knnGrid <- expand.grid(k= seq(1,15,by = 2))

knnFit <- train(Open ~ ., data = train_under$data, 
                method = "knn",
                tuneGrid = knnGrid,
                metric = "ROC",
                trControl = trainControl(method = "cv",
                                         number = 10,
                                         ## Estimate class probabilities
                                         classProbs = TRUE,
                                         ## Evaluate performance using 
                                         ## the following function
                                         search = "grid",
                                         summaryFunction = twoClassSummary,
                                         verboseIter=TRUE,
                                         allowParallel=TRUE))
#RF
rfcontrol <- trainControl(method="cv", number=10, search="grid", classProbs = TRUE,
                          ## Evaluate performance using 
                          ## the following function
                          summaryFunction = twoClassSummary,
                          verboseIter=TRUE,
                          allowParallel=TRUE)

rfGrid <- expand.grid(mtry=1:ceiling(sqrt(ncol(train_under$data))))



rf_gridsearch <- train(Open~., data=train_under$data, 
                       method="rf", metric="ROC", 
                       tuneGrid=rfGrid, 
                       trControl=rfcontrol,
                       ntree= 1501)
print(rf_gridsearch)
plot(rf_gridsearch)


nn.grid <- expand.grid(.decay = c(0.5, 0.3, 0.1), .size = c(3, 5, 6, 7, 9))
nn.fit <- train(Open ~ ., data = train_under$data, method = "nnet", maxit = 1000, tuneGrid = nn.grid, 
                trControl = fitControl, metric = "ROC")



plot(nn.fit)
stopCluster(cl)

# Comparing models
model_list <- list(GBM = gbmFit,
                   RF = rf_gridsearch,
                   NN = nn.fit)
resamps <- resamples(model_list)

summary(resamps)
xyplot(resamples(model_list))

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(resamps, metric = "ROC")

splom(resamps)

difValues <- diff(resamps)
summary(difValues)

trellis.par.set(theme1)
bwplot(difValues, layout = c(3, 1))

trellis.par.set(caretTheme())
dotplot(difValues)


# Ensembling different models

modelscor <- modelCor(resamps)

modelscor

#Apply maj vote of each model on test set

caret::confusionMatrix(predict(gbmFit, test[,-c("Open")]), test$Open, positive= "Open")
caret::confusionMatrix(predict(rf_gridsearch, test[,-c("Open")]), test$Open, positive= "Open")
caret::confusionMatrix(predict(nn.fit, test[,-c("Open")]), test$Open, positive= "Open")

caret::F_meas(caret::confusionMatrix(predict(gbmFit, test[,-c("Open")]), test$Open, positive= "Open")$table)
caret::F_meas(caret::confusionMatrix(predict(rf_gridsearch, test[,-c("Open")]), test$Open, positive= "Open")$table)
caret::F_meas(caret::confusionMatrix(predict(nn.fit, test[,-c("Open")]), test$Open, positive= "Open")$table)


gbm_preds <- predict(gbmFit, test[,-c("Open")])
rf_preds <- predict(rf_gridsearch, test[,-c("Open")])
nn_preds <- predict(nn.fit, test[,-c("Open")])

gbm_preds_prob <- predict(gbmFit, test[,-c("Open")], "prob")[,2]
rf_preds_prob <- predict(rf_gridsearch, test[,-c("Open")], "prob")[,2]
nn_preds_prob <- predict(nn.fit, test[,-c("Open")], "prob")[,2]

roc(test$Open, gbm_preds_prob, ci = T)
roc(test$Open, rf_preds_prob, ci = T)
roc(test$Open, nn_preds_prob, ci = T)


global_preds <- data.table(predictions_1=gbm_preds, predictions_2=rf_preds, predictions_3 = nn_preds)

global_preds_prob <- data.table(predictions_1=gbm_preds_prob, predictions_2=rf_preds_prob, predictions_3= nn_preds_prob)


C1 <- apply(global_preds, 1, function(x){return(sum(x=="Open"))})
C2 <- apply(global_preds, 1, function(x){return(sum(x=="NoOpen"))})
result_global <- ifelse(C1 > C2, "Open", "NoOpen")
tab_conf <- caret::confusionMatrix(result_global, test$Open, positive = "Open")
caret::F_meas(tab_conf$table, positive = "Open")

library(pROC)
proba_aggre <- apply(global_preds_prob, 1,mean)
roc(test$Open, proba_aggre, ci = T)
plot.roc(test$Open, proba_aggre, ci = T, print.auc = T)

## Finding best weights, using a glm model
ind <- sample(1:nrow(test),0.5*nrow(test))
validation <- test[ind,]
test_final <- test[-ind,]
validation <- ovun.sample(Open~., data = validation)
pos_open <- which(colnames(validation$data)=="Open")

stack <- cbind(validation$data, 
               gbm = predict(gbmFit, validation$data[,-pos_open]), 
               rf = predict(rf_gridsearch, validation$data[,-pos_open]),
               nn = predict(nn.fit, validation$data[,-pos_open]))

test_class_rand <- train(Open~., data = stack, 
                         method = "glmnet",
                         trControl = trainControl(method = "cv", number = 3, 
                                                  classProbs = TRUE, summaryFunction = twoClassSummary),
                         tuneLength = 4,  metric = "ROC")

final_result <- predict(test_class_rand, cbind(test_final[,-"Open"], 
                                               gbm = predict(gbmFit, test_final[,-"Open"]), 
                                               rf = predict(rf_gridsearch, test_final[,-"Open"]),
                                               nn = predict(nn.fit, test_final[,-"Open"])))
final_result_prob <- predict(test_class_rand, cbind(test_final[,-"Open"], 
                                                    gbm = predict(gbmFit, test_final[,-"Open"]), 
                                                    rf = predict(rf_gridsearch, test_final[,-"Open"]),
                                                    nn = predict(nn.fit, test_final[,-"Open"])), "prob")[,2]
tab_conf <- caret::confusionMatrix(final_result, test_final$Open, positive = "Open")
tab_conf
library(pROC)
roc(test_final$Open, final_result_prob)
plot.roc(test_final$Open, final_result_prob, ci = T, print.auc = T)
