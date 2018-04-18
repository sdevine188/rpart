library(mlbench)
library(dplyr)
library(rpart)
library(rpart.plot)
library(rsample)
library(stringr)

# https://www.salford-systems.com/resources/webinars-tutorials/tips-and-tricks/using-surrogates-to-improve-datasets-with-missing-values
# https://stackoverflow.com/questions/48116796/clarification-of-decision-tree-surrogate-splits?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

# create some NA values in mpg
mtcars2 <- mtcars
mtcars2$mpg[1:5] <- NA
head(mtcars2)

# build model using mpg
# mtcars_rpart <- rpart(am ~ mpg + cyl, data = mtcars2, method = "class",
#                       minsplit = 2, minbucket = 1, cp = -1)
mtcars_rpart <- rpart(am ~ mpg + cyl, data = mtcars2, method = "class")
mtcars_rpart
rpart.plot(mtcars_rpart, extra = 104, nn = TRUE)

# inspect 
summary(mtcars_rpart)
attributes(summary(mtcars_rpart))
attributes(mtcars_rpart)
mtcars_rpart$frame
mtcars2 %>% filter(is.na(mpg)) %>% select(mpg, cyl, am)

# get surrogate output
source("get_surrogate_output.R")
get_surrogate_output(mtcars_rpart)
surrogate_output <- get_surrogate_output(mtcars_rpart)
surrogate_output


# inspect how mpg = NA obs are handled
# cyl < 5 is a surrogate split for mpg >= 19.5; both split to the right, and so are likely to be classified as am = 1
mtcars2 %>% mutate(mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(mpg_greater_19.5)
mtcars2 %>% mutate(mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(mpg_greater_19.5, am) %>% adorn_percentages() %>% 
         adorn_pct_formatting() %>% adorn_ns() %>% adorn_title() 

mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0)) %>% tabyl(cyl_less_5, am) %>% adorn_percentages() %>% 
        adorn_pct_formatting() %>% adorn_ns() %>% adorn_title() 

mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5, am) %>% 
      adorn_title() 

# so cyl < 5 and mpg >= 19.5 agree on which way to route observations 96.3% of time (note denominator is non-NA cases), which is what we see in rpart summary
# majority rule only agrees with split 59.3%
mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5) %>% 
        adorn_title() 
mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5) %>% 
        adorn_percentages(denominator = "all") %>% adorn_totals(c("row", "col")) %>% adorn_pct_formatting() %>% adorn_title()

majority_rule <- mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5) %>% 
        adorn_percentages(denominator = "all") %>% adorn_totals(c("row", "col")) %>% adorn_pct_formatting() %>% filter(cyl_less_5 == "Total") %>% pull(`0`)
majority_rule

count_surrogate_splits_same_as_primary <- 16 + 10
count_surrogate_splits_same_as_primary
count_total_obs_at_node <- 16 + 10 + 1
count_total_obs_at_node
surrogate_agreement_w_primary <- count_surrogate_splits_same_as_primary / count_total_obs_at_node
surrogate_agreement_w_primary 
summary(mtcars_rpart)

# the adjusted agreement is the improvement of the surrogate over the default surrogate of classifying obs based on majority class of primary split (majority rule)
# see page 12 of https://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf 
mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5) %>% 
        adorn_title()
mtcars2 %>% mutate(cyl_less_5 = ifelse(cyl < 5, 1, 0), mpg_greater_19.5 = ifelse(mpg >= 19.45, 1, 0)) %>% tabyl(cyl_less_5, mpg_greater_19.5) %>% 
        adorn_percentages(denominator = "all") %>% adorn_title()
majority_rule_agreement_with_primary <- 16
(count_surrogate_splits_same_as_primary - majority_rule_agreement_with_primary) / (count_total_obs_at_node - majority_rule_agreement_with_primary)


# confirm that predict_nodes function can apply surrogate splits
source("predict_nodes.R")
mtcars2$predicted_node <- predict_nodes(object = mtcars_rpart, newdata = mtcars2)
head(mtcars2)
mtcars2 %>% filter(is.na(mpg)) %>% select(mpg, cyl, predicted_node)


##########################################################
###########################################################
#############################################################

data(attrition)
glimpse(attrition)

# create some NA values in mpg
attrition2 <- attrition
attrition2$OverTime[1:5] <- NA
attrition2$WorkLifeBalance[7] <- NA
attrition2$TrainingTimesLastYear[14] <- NA
attrition2$TotalWorkingYears[20] <- NA
attrition2$PerformanceRating[22] <- NA

# build model
attrition_rpart <- rpart(Attrition ~ OverTime + TotalWorkingYears + TrainingTimesLastYear + JobInvolvement, data = attrition2, method = "class")
attrition_rpart
rpart.plot(attrition_rpart, extra = 104, nn = TRUE)

# inspect 
summary(attrition_rpart)
attributes(attrition_rpart)
attrition_rpart$splits
attrition_rpart$frame
attrition_rpart %>% filter(is.na(mpg)) %>% select(mpg, cyl, am)

# inspect splits

# node1
# says "splits as LR" because OverTime only has two levels; if it had more, there'd be more e.g. LRL; this sequence signifies the classification for each level
# majority rule is 71%, and no surrogate is listed because no surrogate meets the qualificaiton of having agreement beating the default majority rule
attrition2 %>% tabyl(OverTime)

# node3
# there is 1 NA for TotalWorkingYears, and the surrogate TrainingTimesLastYear is used to split, since it beats the majority rule (agreement = 58.3% vs 56.5%)
# the majority of TotalWorkingYears (56.5%) are  TotalWorkingYears >= 8.5, so the majority rule is to route NAs TotalWorkingYears >= 8.5
attrition2 %>% filter(OverTime != "No") %>% mutate(total_working_years_greater_8.5 = ifelse(TotalWorkingYears >= 8.5, 1, 0)) %>% tabyl(total_working_years_greater_8.5)

# node 14
# note how it lists qualified surrogates that beat the majority rule, but doesn't use them
# conversely, on nodes without any qualified surrogates listed (e.g. node 7) it's because the majority rule beat the potential surrogates, so they didn't qualify


# for those split nodes without any surrogates listed, it's because 

# predict nodes
source("predict_nodes.R")
attrition3 <- attrition
attrition3$JobInvolvement[1:5] <- NA
head(attrition3)
summary(attrition_rpart)
attrition3$terminal_node <- predict_nodes(object = attrition_rpart, newdata = attrition3)

# get node paths
source("get_node_paths.R")
attrition3$node_paths <- get_node_paths(model_fit = attrition_rpart, output_terminal_nodes = attrition3)
attrition3 %>% select(Attrition, OverTime, TotalWorkingYears, JobInvolvement, TrainingTimesLastYear, terminal_node, node_paths) %>% head(10)


################################



############################################





