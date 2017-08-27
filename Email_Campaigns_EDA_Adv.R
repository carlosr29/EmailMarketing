library(RMySQL)
library(data.table)
library(lubridate)
library(ggplot2)

try(source(file = "Cleaning_Functions.R"))
campaign <- as.data.table(read.csv("TravelCampaigns.csv")) #we use data.table for faster and more 
campaign[,X:=NULL]
campaign[,email:=NULL]
campaign[,MessageId:=NULL]
campaign[,routerId:=NULL]
campaign[,programId:=NULL]
#efficient data manipulation
print(nrow(campaign)) # number of rows of the data set 
print(ncol(campaign)) # number of columns for the data set

campaign$Open <- as.factor(campaign$Open)
levels(campaign$Open) <- c("NoOpen","Open")

#Cleaning subject
campaign$subject <- as.character(campaign$subject)
campaign$subject <- clean_subject(subject = campaign$subject)

summary(campaign)
table(campaign$Open)
print(paste0("Proportion of Opens ",table(campaign$Open)[2]/table(campaign$Open)[1]))

#Advanced Data vizualisation
campaign <- na.omit(campaign)

#Plots related to subjects
#Adding variables related to subject: if brand mentionned, if price quoted, if some words in all caps

campaign[,target_types:=user_targeting_type(campaign$subject)]
pl <- ggplot(campaign, aes(target_types, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#Detecting quoted prices and promotions
campaign <- cbind(campaign, detect_prices(campaign$subject))
pl <- ggplot(campaign, aes(prices, fill = Open)) + geom_bar(position = "fill")
plot(pl)

pl <- ggplot(campaign, aes(promotion, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#Detecting all caps
campaign[,caps:=detect_caps(campaign$subject)]
pl <- ggplot(campaign, aes(caps, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#For the rest of the analysis we need to transform the timesend variable to have days of the week, months and hours
time_treatment_df <- convert_timestamp(campaign$timesend)
campaign <- cbind(campaign, time_treatment_df)
campaign[,timesend:=NULL] #deleting the timestamp variable

#Continuous variables
#Density plot code_post_densite_mean
pl <- ggplot(campaign, aes(code_post_densite_mean)) + geom_area(stat = "bin")
plot(pl)

#Density plot code_post_alt_mean
pl <- ggplot(campaign, aes(code_post_alt_mean)) + geom_area(stat = "bin")
plot(pl)


#boxplots against Open/NoOpen
pl <- ggplot(campaign, aes(Open, code_post_alt_mean)) + geom_boxplot()
plot(pl)

pl <- ggplot(campaign[code_post_densite_mean <=15,], aes(Open, code_post_densite_mean)) + geom_boxplot()
plot(pl)

#Correlation densité de population et altitude moyenne: négative, densité plus faible quand l'altitude augmente
cor.test(campaign$code_post_densite_mean, campaign$code_post_alt_mean)

#scatterplots 

#alt_mean
pl <- ggplot(campaign, aes(y=code_post_alt_mean, x=Open)) + geom_point(position = "jitter", alpha = 0.1)
plot(pl)

#densite_pop
pl <- ggplot(campaign, aes(Open, code_post_densite_mean)) + geom_point(alpha = 0.01)
plot(pl)

#Other scatter plots
pl <- ggplot(campaign, aes(code_post_alt_mean, code_post_densite_mean)) +
  geom_point(aes(colour=as.factor(Open)))
plot(pl)

#See the impact of unemployment, poverty and salary