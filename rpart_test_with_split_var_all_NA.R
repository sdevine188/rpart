library(dplyr)
library(rpart)
library(rpart.plot)

mtcars_rpart <- rpart(mpg ~ hp + cyl, data = mtcars, cp = .01)
mtcars_rpart
rpart.plot(mtcars_rpart)

mtcars2 <- mtcars %>% mutate(hp = NA_integer_)
mtcars2$pred <- predict(object = mtcars_rpart, newdata = mtcars2)
mtcars2 %>% select(mpg, hp, cyl, pred)
mtcars2 %>% filter(cyl >= 5) %>% select(mpg, hp, cyl, pred)


#####################################################################


# using caret
mtcars_caret <- train(mpg ~ hp + cyl, data = mtcars, method = "rpart", tuneGrid = expand.grid(cp = .01))
mtcars_caret$finalModel
rpart.plot(mtcars_caret$finalModel)

mtcars2 <- mtcars %>% mutate(hp = NA_integer_)
mtcars2$pred <- predict(object = mtcars_caret$finalModel, newdata = mtcars2)
mtcars2 %>% select(mpg, hp, cyl, pred)
mtcars2 %>% filter(cyl >= 5) %>% select(mpg, hp, cyl, pred)
