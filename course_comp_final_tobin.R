rm(list=ls())
setwd("~/Desktop/API_plane")

options(scipen=999)

library(tidyverse)
library(glmnet)
library (gbm)
library(missForest)
library(randomForest)
library(tree)
library(tidyr)
library(dplyr)
library(ISLR)
library(stringr)
library(MLmetrics)
library(tidyverse)
library(caret)
library(leaps)
library(dplyr)
library(e1071)
library(MASS)
library(randomForest)

# Get data and summarize
db<- read.csv("training.csv")

ls.str(db)
colSums(is.na(db))

#erase columns with too many variables and with high NAs
db$ward <- NULL
db$lga <- NULL
db$permit <- NULL
db$public_meeting <- NULL
#db$construction_year <- NULL
db$recorded_by <- NULL


db <- na.omit(db)

db <- db %>%
  mutate(year_construct_ranges = 
           case_when(construction_year == 0 ~ "value0",
                     construction_year > 0 & construction_year <= 1980 ~ "years_1960_1980",
                     construction_year > 1980 & construction_year <= 2000 ~ "years_1981_2000",
                     construction_year > 2000 ~ "years_2001_present"))
db$construction_year <- NULL
db$year_construct_ranges <- as.factor(db$year_construct_ranges)

##Load F1 Score
##########################
f1_score <- function(predicted_y, true_y) {
  library(dplyr)
  num_unique_y      <- length(unique(true_y))
  scores            <- vector(length = num_unique_y, mode = "double")
  
  for (i in 1:num_unique_y) {
    trans_pred      <- as.numeric(predicted_y == i)
    trans_true      <- as.numeric(true_y == i)
    df              <- cbind.data.frame(trans_pred, trans_true)
    colnames(df)    <- c("pred", "true")
    df              <- df %>%
      mutate(true_pos = ((pred == 1) & (true == 1)),
             true_neg = ((pred == 0) & (true == 0)),
             false_pos = ((pred == 1) & (true == 0)),
             false_neg = ((pred == 0) & (true == 1))) %>%
      summarise(true_pos = sum(true_pos),
                false_pos = sum(false_pos),
                false_neg = sum(false_neg))
    scores[i]       <- 2 * df$true_pos / (2 * df$true_pos + 
                                            df$false_neg + 
                                            df$false_pos)
    
  }
  F1                <- mean(scores)
  return(F1)
}


# SPLIT DATA
## Let's split the data into training and test sets

set.seed(2070231)  
train               <- sample(seq(nrow(db)),
                              round(nrow(db) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(db)) %in% train))

######### RANDOM FOREST CLASSIFICATION MODEL
status.rf <- randomForest(status_group~., data=data.frame(db[-test,]), importance =TRUE)
yhat.rf <- as.numeric(predict(status.rf, newdata=data.frame(db[test,])))

## Find F1 score
## F1_Score(y_true, y_pred, positive = NULL)
status.test <- as.numeric(db[test ,"status_group"])   
f1_rf_amy <- f1_score(yhat.rf, status.test)
print(f1_rf_amy)

#####################################

testing <- read.csv("test.csv")

testing$ward <- NULL
testing$lga <- NULL
testing$permit <- NULL
testing$public_meeting <- NULL

testing$num_private <- NULL
testing$recorded_by <- NULL

testing <- testing %>%
  mutate(year_construct_ranges = 
           case_when(construction_year == 0 ~ "value0",
                     construction_year > 0 & construction_year <= 1980 ~ "years_1960_1980",
                     construction_year > 1980 & construction_year <= 2000 ~ "years_1981_2000",
                     construction_year > 2000 ~ "years_2001_present"))
testing$construction_year <- NULL
testing$year_construct_ranges <- as.factor(testing$year_construct_ranges)


testing <- na.omit(testing)
testing$amount_tsh <- as.double(testing$amount_tsh)

testing_ids <- testing[,1]

sapply(testing, typeof)
sapply(db, is.integer)
sapply(db, class)
sapply(testing, class)

db_new <- db %>% dplyr::select(-status_group)
all <- rbind(db_new, testing)
testing_new <- all[54620:56619, ]

yhat.rf.final <- as.numeric(predict(status.rf, newdata=data.frame(testing_new)))
#sapply(testing, typeof) == sapply(db, typeof)

output = matrix(, ncol = 2)
output <- cbind(testing_ids, yhat.rf.final)
colnames(output) = c("Id", "Prediction")
output <- data.frame(output)
#output <- output %>% mutate("Id" = testing_ids)
#rename(output, c("testing_ids"="Id", "yhat.rf.final"="Prediction"))
write.csv(output, file = "tobin_prediction.csv",  row.names = FALSE)
# To note functional is 1, functional needs repair is 2, and non functional is 3
output2 <- output %>%
  mutate(Prediction = 
           case_when(Prediction == 1 ~ "functional",
                     Prediction == 2  ~ "functional needs repair",
                     Prediction == 3 ~ "non functional"))
write.csv(output2, file = "tobin_prediction_withnames.csv",  row.names = FALSE)



