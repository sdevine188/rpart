library(dplyr)
library(rpart)
library(rpart.plot)


# test replacing a split variable with all NA in newdata and see if rpart model can still predict using surrogates/majority rule
# result: it can predict without issues

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



############################################################################
##########################################################################
##########################################################################


# test to see if rpart outputs node_paths with single or double quotes - useful when using parse_expr(node_path) for tidy checks

# results:
# note that quotes are not used in rpart node_path output
# also note that for splits with multiple levels, it lists the levels un-quoted, with commas seperating

library(rsample)
data(attrition)
glimpse(attrition)

attrition_rpart <- rpart(Attrition ~ WorkLifeBalance + PerformanceRating + OverTime, data = attrition, cp = 0)
attrition_rpart


# get clean model output of stats for each node
attrition_rpart_output <- attrition_rpart$frame
attrition_rpart_output_part2 <- data.frame(attrition_rpart$frame$yval2)
glimpse(attrition_rpart_output)
glimpse(attrition_rpart_output_part2)
attrition_rpart_output <- cbind(attrition_rpart_output, attrition_rpart_output_part2)
glimpse(attrition_rpart_output)
attrition_rpart_output

# clean model output
# note that output dataframe has same stats for each node number as the rpart output object, 
# but the variable listed for each node number in the df is actually the variable listed for the subsequent node number on output object
# basically, if you just ignore the variable listed on the rpart output object, and refer only by node number, it's the same
# the output object is listing the variable that HAS been split on to get to current node, but
# the df is listing the variable that will be split on FROM that node

attrition_rpart_output <- attrition_rpart_output %>% select(-yval2, V1) %>%
        rename(obs_in_node = n, var_to_be_split_next = var, misclassified_count = dev, predicted_class = yval,
               class_1_obs_in_node = V2, class_2_obs_in_node = V3, prob_class_1 = V4, 
               prob_class_2 = V5, pct_obs_in_node = nodeprob) %>%
        mutate(terminal_node = rownames(attrition_rpart_output))

attrition_rpart_output
glimpse(attrition_rpart_output)

# filter output to just terminal nodes
attrition_rpart_output_terminal_nodes <- attrition_rpart_output %>% 
        filter(var_to_be_split_next == "<leaf>") %>% 
        arrange(desc(prob_class_2))

attrition_rpart_output_terminal_nodes

# get leaf node paths 
# ended up building custom function
# https://stackoverflow.com/questions/36086990/how-to-climb-the-tree-structure-of-rpart-object-using-path-in-order-to-purge-man
# https://stackoverflow.com/questions/13548266/define-all-functions-in-one-r-file-call-them-from-another-r-file-how-if-pos

# call get_node_path function
current_wd <- getwd()
setwd("H:/R/rpart")
source("get_node_paths.R")
setwd(current_wd)

attrition_rpart_node_paths <- get_node_paths(model_fit = attrition_rpart, 
                                                                       output_terminal_nodes = attrition_rpart_output_terminal_nodes)
attrition_rpart_node_paths

# add node paths to output_terminal nodes
attrition_rpart_output_terminal_nodes <- left_join(attrition_rpart_output_terminal_nodes,
                                                                             attrition_rpart_node_paths,
                                                                             by = c("terminal_node" = "terminal_node"))
attrition_rpart_output_terminal_nodes


# note that quotes are not used in rpart node_path output
# also note that for splits with multiple levels, it lists the levels un-quoted, with commas seperating
