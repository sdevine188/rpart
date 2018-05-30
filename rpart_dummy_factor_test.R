library(dplyr)
library(rpart)
library(rsample)
library(caret)
library(rpart.plot)
library(purrr)
library(reprex)

data(attrition)
glimpse(attrition)


# rpart uses categorical variables as factors, allowing for splits containing combinations of several factor levels
attrition %>% select(Attrition, OverTime, JobRole) %>% glimpse()

attrition_rpart2 <- rpart(Attrition ~ OverTime + JobRole, data = attrition, method = "class")
attrition_rpart2
rpart.plot(attrition_rpart2, tweak = 1.3, extra = 104, nn = TRUE)

attrition2 <- attrition
attrition2$pred <- predict(object = attrition_rpart2, newdata = attrition2, type = "class")
confusionMatrix(data = attrition2$pred, reference = attrition2$Attrition)


######################################


# using dummy variables instead of factors results in less splits since no single dummy has enough explanatory power to be its own split
attrition_subset <- attrition %>% select(OverTime, JobRole, Attrition)
attrition_dummy_model <- dummyVars(~ ., data = attrition_subset)
attrition_dummy <- data.frame(predict(object = attrition_dummy_model, newdata = attrition_subset))
attrition_dummy <- attrition_dummy %>% select(-Attrition.No)
attrition_dummy <- map_dfr(.x = attrition_dummy, .f = ~ as.factor(.x))
glimpse(attrition_dummy)

attrition_rpart3 <- rpart(Attrition.Yes ~ ., data = attrition_dummy, method = "class")
attrition_rpart3
rpart.plot(attrition_rpart3, tweak = 1.3, extra = 104, nn = TRUE)

attrition_dummy$pred <- predict(object = attrition_rpart3, newdata = attrition_dummy, type = "class")
confusionMatrix(data = attrition_dummy$pred, reference = attrition_dummy$Attrition.Yes)


#########################################


# when using the variables as dummies, the test set accuracy is much less than using the variables as factors
attrition_subset <- attrition %>% select(OverTime, JobRole, Attrition)

set.seed(123)
in_train <- createDataPartition(attrition_subset$Attrition, p = .7, list = FALSE)
attrition_train <- attrition_subset[in_train, ]
attrition_test <- attrition_subset[-in_train, ]
glimpse(attrition_train)
glimpse(attrition_test)

control <- trainControl(method = "none")
attrition_caret1 <- train(Attrition ~ OverTime + JobRole, data = attrition_train, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret1$finalModel

attrition_test$pred <- predict(object = attrition_caret1, newdata = attrition_test, type = "raw")
glimpse(attrition_test)
confusionMatrix(data = attrition_test$pred, reference = attrition_test$Attrition)


###########################################


# odd error?? with rpart and caret - the model uses OverTime as first split when JobRole is included, but does not split at all when just OverTime included
attrition_subset <- attrition %>% select(Attrition, OverTime, JobRole)
glimpse(attrition_subset)

set.seed(123)
in_train <- createDataPartition(attrition_subset$Attrition, p = .7, list = FALSE)
attrition_train <- attrition_subset[in_train, ]
attrition_test <- attrition_subset[-in_train, ]
glimpse(attrition_train)
glimpse(attrition_test)

# replicate with rpart
attrition_rpart_error <- rpart(Attrition ~ OverTime, data = attrition_train, method = "class", cp = .01)
attrition_rpart_error 

attrition_rpart_error <- rpart(Attrition ~ OverTime + JobRole, data = attrition_train, method = "class", cp = .01)
attrition_rpart_error 

# replicate with caret
control <- trainControl(method = "none")
attrition_caret2 <- train(Attrition ~ OverTime, data = attrition_train, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret2$finalModel

control <- trainControl(method = "none")
attrition_caret2 <- train(Attrition ~ OverTime + JobRole, data = attrition_train, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret2$finalModel


###########################################


# using the variables as factors gets better test set performance than using them as dummies
attrition_subset <- attrition %>% select(Attrition, OverTime, JobRole)
glimpse(attrition_subset)

set.seed(123)
in_train <- createDataPartition(attrition_subset$Attrition, p = .7, list = FALSE)
attrition_train <- attrition_subset[in_train, ]
attrition_test <- attrition_subset[-in_train, ]
glimpse(attrition_train)
glimpse(attrition_test)

# try converting variable to character instead of factor - it doesn't find a split, which is odd, since OverTime is the first split and remains a factor
# https://stackoverflow.com/questions/37313786/r-a-full-decision-tree-grows-but-not-its-simpler-partial-counterpart
# attrition_train$OverTime <- as.character(attrition_train$JobRole)
# glimpse(attrition_train)

# control <- trainControl(method = "none")
# attrition_caret2 <- train(x = attrition_train$OverTime, y = attrition_train$Attrition, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
# attrition_caret2$finalModel

control <- trainControl(method = "none")
attrition_caret2 <- train(x = attrition_train[ , -1], y = attrition_train$Attrition, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret2$finalModel

attrition_test$pred <- predict(object = attrition_caret2, newdata = attrition_test, type = "raw")
glimpse(attrition_test)
confusionMatrix(data = attrition_test$pred, reference = attrition_test$Attrition)


###########################################


# if we add row_number as a dummy, the model doesn't use it since no specific row_number is a useful enough split
# notice the test set accuracy is much less than using the variables as factors though
attrition_subset <- attrition %>% select(Attrition, OverTime, JobRole) %>% mutate(row_number = factor(row_number()))
glimpse(attrition_subset)

set.seed(123)
in_train <- createDataPartition(attrition_subset$Attrition, p = .7, list = FALSE)
attrition_train <- attrition_subset[in_train, ]
attrition_test <- attrition_subset[-in_train, ]

attrition_caret4 <- train(Attrition ~ OverTime + JobRole + row_number, data = attrition_train, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret4$finalModel

attrition_test$pred <- predict(object = attrition_caret4, newdata = attrition_test, type = "raw")
glimpse(attrition_test)
confusionMatrix(data = attrition_test$pred, reference = attrition_test$Attrition)


#########################################


# with caret
# but if we add row_number as a factor (ie our factor has too many levels) the tree finds the splits for perfect classification
# note it's no use trying to predict this model on test data, since the row_number factors would all be unknown levels
attrition_subset <- attrition %>% select(Attrition, OverTime, JobRole) %>% mutate(row_number = factor(row_number()))
glimpse(attrition_subset)
control <- trainControl(method = "none")
attrition_caret3 <- train(x = attrition_subset[ , -1], y = attrition_subset$Attrition, method = "rpart", trControl = control, tuneGrid = expand.grid(cp = .01))
attrition_caret3$finalModel


###############################################


# with rpart
# if we add row_number as a factor (ie our factor has too many levels) the tree finds the splits for perfect classification
# note it's no use trying to predict this model on test data, since the row_number factors would all be unknown levels

attrition_subset <- attrition %>% select(Attrition, OverTime, JobRole) %>% mutate(row_number = factor(row_number()))
glimpse(attrition_subset)
attrition_rpart4 <- rpart(Attrition ~ OverTime + JobRole + row_number, data = attrition_subset, method = "class")
attrition_rpart4

attrition_subset$pred <- predict(object = attrition_rpart4, newdata = attrition_subset, type = "class")
glimpse(attrition_subset)
confusionMatrix(data = attrition_subset$pred, reference = attrition_subset$Attrition)

