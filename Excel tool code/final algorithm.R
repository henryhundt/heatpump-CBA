setwd("~/Desktop/ENV 810/Data/RECS data")

library(dplyr)
library(stringr)
library(caret)

full <- read.csv("recs2015_public_v4.csv")

full$CLIMATE_REGION_PUB <- str_remove_all(full$CLIMATE_REGION_PUB, "[-/ ]")
full$IECC_CLIMATE_PUB <- str_remove_all(full$IECC_CLIMATE_PUB, "-")
full <- filter(full, (IECC_CLIMATE_PUB == "5A" | IECC_CLIMATE_PUB == "5B5C" | IECC_CLIMATE_PUB == "6A6B") & DIVISION == 3)

#full$METROMICRO <- as.factor(full$METROMICRO)
#full$UATYP10 <- as.factor(full$UATYP10)
full$STORIES <- as.factor(full$STORIES) 
full$DIVISION <- as.factor(full$DIVISION)
full$REGIONC <- as.factor(full$REGIONC)
full$CELLAR <- as.factor(full$CELLAR)
full$ATTIC <- as.factor(full$ATTIC)
full$PRKGPLC1 <- as.factor(full$PRKGPLC1)
full$YEARMADERANGE <- as.factor(full$YEARMADERANGE)
#full$OCCUPYYRANGE <- as.factor(full$OCCUPYYRANGE)
full$TYPEHUQ <- as.factor(full$TYPEHUQ)
full$STUDIO <- as.factor(full$STUDIO)
full$WALLTYPE <- as.factor(full$WALLTYPE)
#full$CLIMATE_REGION_PUB <- as.factor(full$CLIMATE_REGION_PUB)
#full$IECC_CLIMATE_PUB <- as.factor(full$IECC_CLIMATE_PUB)

full_temp <- select(full, TOTALBTUSPH, TOTSQFT_EN,DIVISION, 
                YEARMADERANGE, NWEIGHT)
gc()
final <- NA
for(i in 1:nrow(full_temp)){
  temp <- full_temp[i,]
  temp2 <- temp[rep(1,round(temp$NWEIGHT/500)),]
  final <- rbind(final, temp2)
  print(i)
}

final <- select(final, -NWEIGHT)
#mean(final$TOTALBTUSPH, na.rm = T)/1000

#library(ggplot2)
#ggplot(final, aes(y = TOTALBTUSPH, x = HDD30YR)) + geom_point(alpha = 0.05) + geom_smooth()
#model <- lm(TOTALBTUSPH ~ ., data = final)
#plot(model)


set.seed(1)
final <- select(final, -DIVISION)
random <- sample(1:nrow(final), .75 * nrow(final)) # 75%: training data, 25%: test data
train <- final[random, ]
test <- final[-random, ]
# random <- sample(1:nrow(full_temp), 0.75 * nrow(full_temp)) # 75%: training data, 25%: test data
# train <- full_temp[random,]
# test <- full_temp[random,]

model <- lm(TOTALBTUSPH ~ ., data = train)
#test_WI <- filter(test, DIVISION == 3 & (IECC_CLIMATE_PUB == "5A" | IECC_CLIMATE_PUB == "5B5C" | IECC_CLIMATE_PUB == "6A6B"))
test_WI <- test
pred <- predict(model,test_WI)
RMSE(pred, test_WI$TOTALBTU)
#test_WI$pred_lm <- pred
#test_WI$pred_RMSE <- (test_WI$pred_lm - test_WI$TOTALBTUSPH)^2
#sqrt(sum(test_WI$pred_RMSE)/nrow(test_WI))

# blah <- na.omit(data.frame("TOTALBTUSPH" = NA, "pred_lm" = NA))
# for(i in 1:(nrow(test_WI)/26)){
#   hold <- test_WI[c(((26*(i-1))+1):(26*i)),] 
#   blah[i,]$TOTALBTUSPH <- mean(hold$TOTALBTUSPH)
#   blah[i,]$pred_lm <- mean(hold$pred_lm)
# }
# blah$pred_RMSE <- (blah$pred_lm - blah$TOTALBTUSPH)^2
# sqrt(sum(blah$pred_RMSE)/nrow(blah))

#max(test$pred_distance_lm)
#hist(test$pred_distance_lm)
summary(model)


library(randomForest)
library(ranger)
tuneGrid <- expand.grid(
  .mtry = seq(1, length(model$coefficients), 3),
  .splitrule = "variance",
  .min.node.size = seq(1, 51, 5)
)

# Fit random forest: model
rf_model <- train(
  formula(model$terms),
  tuneLength = 1,
  data = train,
  method = "ranger",
  tuneGrid = tuneGrid,
  trControl = trainControl(
    method = "cv",
    number = 5,
    verboseIter = T
  )
)

bestmtry <- rf_model$bestTune$mtry
bestnode <- rf_model$bestTune$min.node.size

weekly_rf <- randomForest(formula = formula(model$terms),
                          mtry = bestmtry,
                          nodesize = bestnode,
                          data = train)
pred <- predict(weekly_rf,test)
RMSE(pred, test$TOTALBTUSPH)
plot(rf_model)
test$pred_RF <- pred
test$pred_distance_RF <- abs(test$TOTALBTUSPH - test$pred_RF)
max(test$pred_distance_RF)
hist(test$pred_distance_RF)
