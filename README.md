# EmailMarketing
Madriss Seksaoui - Email marketing professional thesis - Msc in Data Sciences and Business Analytics

This git repository contains all the necessary script files and anonymized sample data sets to reproduce all the results from:

### E-mail marketing: a personal experience with Big Data

Three sample data sets are provided:

* Fahsion : **sample_campaign_fashion.csv**
* High tech : **sample_campaign_high_tech.csv**
* Retail : **sample_campaign_retail.csv**

Note that each sample data set contains 10 000 instances, the open proportion and other statistics are given in the table below: 

| Dataset   | Open proportion |
| --------- |:-------------:|
| Fashion   | 8.26 %|
| High tech | 9.69 %|
| Retail    | 9.64 %|

File description:

* **train_test_vizu.R** : Provides the user with the plots from Chapter 3 to illustrate overfitting/underfitting
* **Preliminary_Modeling.R** : Trains benchmark models including CART, RF and Logistic Regression
* **model_comparison.R** : Trains and compares the models listed in Chapter 5, the ensemble model is also trained in this file
* **Cleaning_Functions.R**:	Contains several functions to obtains explanatory variables from the sample data sets
* **get_text_features.R**: The file contains a set of functions to extract features from subject lines
* **Email_Campaigns_EDA.R**: This file contains the code for the plots appearing in Chapter 4
* **Email_Campaigns_EDA_Adv.R**: This file is used to obtain more advanced plots
* **Text_Analysis.R**: Is used to to train models using the BOW and 2-Gram features from the subject lines
