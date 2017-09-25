library(data.table)
library(lubridate)
library(ggplot2)
library(tm)
library(stringr)

clean_subject <- function(subject){# Function to clean the subjects: remove punctuation, 
  #remove unnecessary characters
  if (!is.character(subject)){
    tryCatch(as.character(subject), error = function(x) stop("Error: is the subject in a character format?"))
  }
  Encoding(subject) <- "utf-8" #We make sure the subject has the right encoding
  subject <- tolower(subject)
  subject <- str_replace_all(subject, "[[:punct:]]", " ")
  subject <- str_replace_all(subject, "[\\|]", "")  
  subject <- str_replace_all(subject, "subscriber", "")
  subject <- str_replace_all(subject, "attribute", "")  
  subject <- str_replace_all(subject, "firstname", "")  
  subject <- str_replace_all(subject, "lastname", "")  
  subject <- str_replace_all(subject, "zipcode", "")
  subject <- str_replace_all(subject, "\\s+", " ")#two spaces or more will be replaced by one space
  subject <- trimws(subject, which = "both")#trim white space (left and right) 
  return(subject)
}
clean_subject_stopwords <- function(subject){
  library(tm)
  subject <- tolower(subject)
  corpusObject <- tm::Corpus(tm::VectorSource(subject), list(language = "fr"))
  corpusObject <- tm::tm_map(corpusObject, tm::removeWords, tm::stopwords("fr"))
  return(content(corpusObject))
}

add_length <- function(subject){
  lengths <- nchar(subject)
  return(lengths)
}
user_targeting_type_dummy <- function(subject, custom_tag, only_targetings = F){
  if(missing(custom_tag)) {
    inscription_email <- ifelse(str_detect(subject, "subscriber_email"), 1, 0)
    inscription_firstname <- ifelse(str_detect(subject, "subscriber_firstname"), 1, 0)
    inscription_lastname <- ifelse(str_detect(subject, "subscriber_lastname"), 1, 0)
    inscription_zipcode <- ifelse(str_detect(subject, "subscriber_attribute_zipcode"), 1, 0)
    target_types <- cbind(inscription_email, inscription_firstname, inscription_lastname, inscription_zipcode)
    
  } else {
    inscription_email <- ifelse(str_detect(subject, "subscriber_email"), 1, 0)
    inscription_firstname <- ifelse(str_detect(subject, "subscriber_firstname"), 1, 0)
    inscription_lastname <- ifelse(str_detect(subject, "subscriber_lastname"), 1, 0)
    inscription_zipcode <- ifelse(str_detect(subject, "subscriber_attribute_zipcode"), 1, 0)
    inscription_custom <- ifelse(str_detect(subject, custom_tag), 1, 0)
    target_types <- cbind(inscription_email, inscription_firstname, inscription_lastname, inscription_zipcode,
                      inscription_custom)
  }
  if(only_targetings==T){
    target_types <- target_types[,-which(colSums(target_types) == 0)]
  } 
  return(target_types)
}

user_targeting_type <- function(subject, custom_tag){
  if(missing(custom_tag)) {
    target_types <- ifelse(str_detect(subject, "subscriber_email"), "email", 
                                ifelse(str_detect(subject, "subscriber_firstname"), "firstname", 
                                ifelse(str_detect(subject, "subscriber_lastname"), "lastname",
                                ifelse(str_detect(subject, "subscriber_attribute_zipcode"),"zipcode","None"))))
    
  } else {
    target_types <- ifelse(str_detect(subject, "subscriber_email"), "email", 
                           ifelse(str_detect(subject, "subscriber_firstname"), "firstname", 
                            ifelse(str_detect(subject, "subscriber_lastname"), "lastname",
                            ifelse(str_detect(subject, "zipcode"),"zipcode", 
                            ifelse(str_detect(subject,custom_tag), custom_tag, "None")))))
  }
  return(target_types)
}
convert_timestamp_dummy <- function(timestmp, get_months = F){#Function for creating dummy variables (hour ,day of the week, month)
  #from a timstamp varaible
  library(nnet)
  library(lubridate)
  library(data.table)
  library(chron)#to determine if a date is a week end or not
  date_vector <- ymd_hms(timestmp)
  month_vector <- lubridate::month(date_vector)
  
  weekday_vector <- lubridate::wday(date_vector)
  hour_vector <- lubridate::hour(date_vector)
  
  months <- class.ind(month_vector)
  colnames(months) <- paste0("M_",colnames(months))
  
  weekdays <- class.ind(weekday_vector)
  colnames(weekdays) <- paste0("W_",colnames(weekdays))

  weekends <- is.weekend(lubridate::as_date(timestmp))
  
  hour <- class.ind(hour_vector)
  colnames(hour) <- paste0("H_",colnames(hour))
  
  if (get_months){
  dataset <- as.data.frame(cbind(weekdays, weekends, months, hour))
  } else {
    dataset <- as.data.frame(cbind(weekdays, weekends, hour))
  }
  #transforming format of dummy variables to factor
  for (name in colnames(dataset)){
    dataset[,name] <- as.factor(dataset[,name])
  }
  return(as.data.table(dataset))
}


convert_timestamp <- function(timestmp){#Function for creating variables (hour ,day of the week, month)
  #from a timstamp varaible
  library(nnet)
  library(lubridate)
  library(data.table)
  library(chron)#to determine if a date is a week end or not
  date_vector <- ymd_hms(timestmp)
  
  month_vector <- lubridate::month(date_vector)
  weekday_vector <- lubridate::wday(date_vector)
  hour_vector <- lubridate::hour(date_vector)
  weekends <- is.weekend(lubridate::as_date(timestmp))
  
  month_vector <- as.factor(month_vector)
  weekday_vector <- as.factor(weekday_vector)
  hour_vector <- as.factor(hour_vector)
  weekends <- as.factor(weekends)
  
  #Getting the correct factor levels for each variable
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  dow <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
  levels(hour_vector) <- paste0("H_", levels(hour_vector))
  levels(weekday_vector) <- dow[as.numeric(levels(weekday_vector))]
  levels(month_vector) <- months[as.numeric(levels(month_vector))]
  
  dataset <- data.frame(weekday_vector, weekends, month_vector, hour_vector)
  return(as.data.table(dataset))
}

#tranches d'ages 
age_groups_dummy <- function(ages){
  library(lubridate)
  library(nnet)
  ages[ages < 0] <- NA
  #tranches d'ages
  tranche_1 <- ifelse(ages <= 25, 1, 0)
  tranche_2 <- ifelse(ages > 25 & ages <=35, 1, 0)
  tranche_3 <- ifelse(ages > 35 & ages <=45, 1, 0)
  tranche_4 <- ifelse(ages > 45 & ages <=55, 1, 0)
  tranche_5 <- ifelse(ages > 55 & ages <=65, 1, 0)
  tranche_6 <- ifelse(ages > 65, 1, 0)  
  
  new_ages <- cbind(tranche_1,tranche_2,tranche_3,tranche_4,tranche_5,tranche_6)
  new_ages <- as.data.table(new_ages)
  colnames(new_ages) <-c("age_group1","age_group2","age_group3","age_group4","age_group5","age_group6")
  return(new_ages)
}

age_groups <- function(ages){
  library(lubridate)
  library(nnet)
  ages[ages < 0] <- NA
  #tranches d'ages
  tranches <- ifelse(ages <= 25, "age_group1", ifelse(ages > 25 & ages <=35, "age_group2", 
                                               ifelse(ages > 35 & ages <=45, "age_group3", 
                                               ifelse(ages > 45 & ages <=55, "age_group4",
                                               ifelse(ages > 55 & ages <=65, "age_group5", "age_group6")))))
  
  return(tranches)
}

get_hostnames <- function(emails, threshold=100){
  library(stringr)
  sub_email <- str_sub(emails ,str_locate(emails, "@")[,1] + 1)
  hostnames <- str_sub(sub_email, 0, str_locate(sub_email, "\\.")[,1] - 1)
  hostnames <- common_hostnames(hostnames, threshold)
  return(hostnames)
}

common_hostnames <- function(hostnames, threshold = 100){
  hostnames[which(!hostnames %in% c(names(which(table(hostnames) > threshold))))] <- c("Other")
  return(hostnames)
}

