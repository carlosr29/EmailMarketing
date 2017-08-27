library(RMySQL)
library(data.table)
library(lubridate)
library(ggplot2)

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

campaign$Open <- as.factor(campaign$Open)
levels(campaign$Open) <- c("NoOpen","Open")

#Cleaning subject
campaign$subject <- as.character(campaign$subject)
campaign$subject <- clean_subject(subject = campaign$subject)

summary(campaign)
table(campaign$Open)
print(paste0("Proportion of Opens ",table(campaign$Open)[2]/table(campaign$Open)[1]))

#Basic Data vizualisation to determine what affects Open/Nopen of emails

#visualizing hostnames
pl <- ggplot(campaign, aes(hostname, fill = Open)) + geom_bar()
plot(pl)

pl <- ggplot(campaign, aes(hostname, fill = Open)) + geom_bar(position = "fill") #Explain why position = fill
plot(pl)

#visualizing title (M, Mrs, ...)
pl <- ggplot(campaign, aes(title, fill = Open)) + geom_bar()
plot(pl)

pl <- ggplot(campaign, aes(title, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#visualizing by age (we only analyze age <= 90 and non NA values)
pl <- ggplot(na.omit(campaign[age <= 90,]), aes(age, fill = Open)) + geom_bar()
plot(pl)

pl <- ggplot(na.omit(campaign[age <= 90,]), aes(age, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#Visualizing by 5 age groups
age_groups_vect <- age_groups(campaign$age)
campaign[,age_groups:=age_groups_vect]

pl <- ggplot(na.omit(campaign), aes(age_groups, fill = Open)) + geom_bar()
plot(pl)

pl <- ggplot(na.omit(campaign), aes(age_groups, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#visualizing age groups + gender
pl <- ggplot(na.omit(campaign), aes(age_groups, fill = Open)) + geom_bar() + facet_grid(age_groups ~ title)
plot(pl)

pl <- ggplot(na.omit(campaign), aes(age_groups, fill = Open)) + geom_bar(position = "fill") + facet_grid(age_groups ~ title)
plot(pl)


#For the rest of the analysis we need to transform the timesend variable to have days of the week and hours
time_treatment_df <- convert_timestamp(campaign$timesend)
campaign <- cbind(campaign, time_treatment_df)
campaign[,timesend:=NULL] #deleting the timestamp variable

#visualizing by day of the week
pl <- ggplot(campaign, aes(weekday_vector, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#visualizing by week end
pl <- ggplot(campaign, aes(weekends, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#visualizing by hours 
pl <- ggplot(campaign, aes(hour_vector, fill = Open)) + geom_bar(position = "fill")
plot(pl)

#visualizing by hours and week ends
pl <- ggplot(campaign, aes(hour_vector, fill = Open)) + geom_bar() + facet_grid(weekends ~.)
plot(pl)

pl <- ggplot(campaign, aes(hour_vector, fill = Open)) + geom_bar(position = "fill") + facet_grid(weekends ~.)
plot(pl)

#visualizing by hours and week days
pl <- ggplot(campaign, aes(hour_vector, fill = Open)) + geom_bar() + facet_grid(weekday_vector ~.)
plot(pl)

pl <- ggplot(campaign, aes(hour_vector, fill = Open)) + geom_bar(position = "fill") + facet_grid(weekday_vector ~.)
plot(pl)

