library(data.table)
library(lubridate)
library(ggplot2)
library(tm)
library(stringr)

clean_subject <- function(subject){# Function to clean the subjects: remove punctuation, 
  #remove unnecessary characters
  if (!is.character(subject)){
    stop("Error: is the subject in a character formet ? ")
  }
  Encoding(subject) <- "UTF-8" #We make sure the subject has the right encoding
  subject <- tolower(subject)
  subject <- str_replace_all(subject, "[[:punct:]]", " ")
  subject <- str_replace_all(subject, "[\\|]", "")  
  subject <- str_replace_all(subject, "\\s+", " ")#two spaces or more will be replaced by one space
  subject <- trimws(subject, which = "both")#trim white space (left and right) 
  return(subject)
}

convert_timestamp_dummy <- function(timestmp, origin){#Function for creating dummy variables (hour ,day of the week, month)
  #from a timstamp varaible
  library(nnet)
  library(lubridate)
  library(data.table)
  library(chron)#to determine if a date is a week end or not
  if(missing(origin)){
    origin <- "1970-01-01 00:00:00"
  }
  date_vector <- ymd_hms(origin) + dseconds(as.numeric(timestmp))
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
  
  dataset <- as.data.frame(cbind(weekdays, weekends, months, hour))
  
  #transforming format of dummy variables to factor
  for (name in colnames(dataset)){
    dataset[,name] <- as.factor(dataset[,name])
  }
  return(as.data.table(dataset))
}


convert_timestamp <- function(timestmp, origin){#Function for creating variables (hour ,day of the week, month)
  #from a timstamp varaible
  library(nnet)
  library(lubridate)
  library(data.table)
  library(chron)#to determine if a date is a week end or not
  if(missing(origin)){
    origin <- "1970-01-01 00:00:00"
  }
  date_vector <- ymd_hms(origin) + dseconds(as.numeric(timestmp))
  
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

