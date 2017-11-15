library(RMySQL)
library(data.table)
library(lubridate)
library(ggplot2)

try(source(file = "Cleaning_Functions.R"))
try(source(file = "get_text_features.R"))
try(source(file = "Geo_Eco_Data.R"))
for (f in c("campaign_course.csv", "campaign_high_tech.csv", "campaign_vetement.csv")){
  campaign <- as.data.table(read.csv(f)) #we use data.table for faster and more 
  campaign[,X:=NULL]
  campaign[,age:=round((Sys.Date() - as.Date(birthday))/dyears(1))]
  campaign <- campaign[age >= 18 & age <= 100,]
  
  #efficient data manipulation
  print(nrow(campaign)) # number of rows of the data set 
  print(ncol(campaign)) # number of columns for the data set
  
  curr_camp <- str_sub(f, 1, str_locate(f, ".csv")[1]-1)
  
  campaign$Open <- as.factor(campaign$Open)
  levels(campaign$Open) <- c("NoOpen","Open")
  
  #Cleaning subject
  campaign$subject <- as.character(campaign$subject)
  
  summary(campaign)
  table(campaign$Open)
  print(paste0("Proportion of Opens ",table(campaign$Open)[2]/(table(campaign$Open)[1]+table(campaign$Open)[2])))
  
  #Advanced Data vizualisation
  campaign <- na.omit(campaign)
  
  #Plots related to subjects
  
  #Adding variables related to subject: if brand mentionned, if price quoted, if some words in all caps
  campaign[,target_types:=user_targeting_type(campaign$subject)]
  
  campaign[,hostname:=get_hostnames(email)]
  campaign[,email:=NULL]
  
  #Visualizing by 5 age groups
  age_groups_vect <- age_groups(campaign$age)
  campaign[,age_groups:=age_groups_vect]
  campaign[,birthday:=NULL]
  
  #For the rest of the analysis we need to transform the sendtime variable to have days of the week and hours
  time_treatment_df <- convert_timestamp(campaign$sendtime)
  campaign <- cbind(campaign, time_treatment_df)
  campaign[,sendtime:=NULL] #deleting the timestamp variable
  campaign <- na.omit(campaign)
  
  #Detecting quoted prices and promotions
  campaign <- cbind(campaign, detect_prices(campaign$subject))
  
  #Detecting all caps
  campaign[,caps:=detect_caps(campaign$subject)]
  
  #Detecting brands/products
  campaign[,brand:=detect_brand_product(campaign$subject)]
  
  
  #Detecting wordscount
  campaign[,wordscount:=add_words_count(campaign$subject)]
  
  #Detecting shortwordscount
  campaign[,shortwordscount:=add_short_words_count(campaign$subject)]
  #Detecting punctuation
  campaign <- cbind(campaign, detect_punctuation(campaign$subject))
  #Detecting time_reference
  campaign[,time_ref:=detect_time_reference(campaign$subject)]
  
  #For the rest of the analysis we need to add eco/social and geographic features
  campaign <- add_eco_features(campaign)
  campaign <- add_pov_features(campaign)
  campaign <- add_geo_features(campaign)
  campaign <- na.omit(campaign)
  
  
  #drop columns with levels of length < 2 (non informative variables)
  campaign <- campaign[, sapply(campaign, function(col) length(unique(col))) > 1, with=F]
  
  #Given the large sample size, p-values will all be < 0.05, we need to account for effect size and use a more sophisticated method
  library(lsr)
  #Continuous variables
  cont_var_table <- data.table(Variable = character(), t_test_p_value = numeric(), Cohen_d = numeric())
  for (col in c("code_post_densite_mean", "code_post_alt_mean", "code_post_pop_mean", "code_post_superficie_mean", 
                "med_niv_vie","rappD9D1","taux_pauvr","part_minima_soc","unemp_rate_2017")){
    
    cat(paste0("current variable : ", col, "\n"))
    current_cohen_d <- lsr::cohensD(as.formula(paste0(col, "~Open")), data = campaign)
    current_p_val <- t.test(as.formula(paste0(col, "~Open")), data = campaign, alternative = "two.sided")$p.value
    print(paste0("p.value = ", current_p_val))
    print(paste0("Cohen's d = ", current_cohen_d))
    cont_var_table <- rbind(cont_var_table, cbind(Variable=col, t_test_p_value=current_p_val, Cohen_d=current_cohen_d))
  }
  setorder(cont_var_table, -Cohen_d)
  
  # Test for discrete variables
  #Chi square test of independance, p-value < 0.05, there is a dependance between variables but since we have a large sample size we need to take into consideration the size effects
  #Cohen, Jacob, 1988, Statistical power and analysis for the behavioral sciences (2nd ed.), Hillsdale, N.J., Lawrence Erlbaum Associates, Inc.
  #Murphy, K.R., and Myors, Brett, 1998, Statistical power analysis—A simple and general model for traditional and modern hypothesis tests: Mahwah, N.J., Lawrence Erlbaum Associates, Inc.
  #https://www.fort.usgs.gov/sites/landsat-imagery-unique-resource/statistical-interpretation
  
  # Magnitude of Effect Size	Cramer’s V/phi	Cohen’s d
  # 
  #         Small                 0.1           0.2
  # 
  #         Medium                0.3           0.2 
  # 
  #         Large                 0.5           0.8
  # 
  disc_var_table <- data.table(Variable = character(), Chisq_test_p_value = numeric(), Cramer_v = numeric())
  
  for (col in setdiff(colnames(campaign), c("code_post_densite_mean", "code_post_alt_mean", "code_post_pop_mean", "month_vector", 
                                            "code_post_superficie_mean", "zipcode","sendtime", "age", "birthday","subject",
                                            "med_niv_vie","rappD9D1","taux_pauvr","part_minima_soc","unemp_rate_2017", "Open"))){
    if(length(summary(campaign[,col,with=F])) > 2){
      cat(paste0("current variable : ", col, "\n"))
      current_cramer_v <- lsr::cramersV(table(unlist(campaign[,col, with=F]), campaign$Open))
      current_p_val <- chisq.test(table(unlist(campaign[,col, with=F]), campaign$Open))$p.value
      print(paste0("p.value = ", current_p_val))
      print(paste0("Cramer's V = ", current_cramer_v))
      disc_var_table <- rbind(disc_var_table, cbind(Variable=col, Chisq_test_p_value=current_p_val, Cramer_v=current_cramer_v))
      
    }
  }
  
  setorder(disc_var_table, -Cramer_v)
  write.csv(cont_var_table, paste0(curr_camp, "_cont_statistical_tests.csv"))
  write.csv(disc_var_table, paste0(curr_camp, "_disc_statistical_tests.csv"))
}