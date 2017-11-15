set.seed(1412)
library(caret)
library(data.table)
library(ggplot2)
class_dat <- twoClassSim(1000)
lda_data <- learing_curve_dat(dat = class_dat, 
                              outcome = "Class",
                              test_prop = 1/4, 
                              ## `train` arguments:
                              method = "lda", 
                              metric = "ROC",
                              trControl = trainControl(classProbs = TRUE, 
                                                       summaryFunction = twoClassSummary))

lda_data <- as.data.table(lda_data)

ggplot(lda_data, aes(x = Training_Size, y = ROC, color = Data)) + 
  geom_smooth(method = loess, span = .8) + 
  theme_bw()
