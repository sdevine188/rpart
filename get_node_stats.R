library(rlang)
library(stringr)
library(dplyr)
library(purrr)

# tree_output_terminal_nodes <- treatment_pre_interview_train_rpart_caret_output_terminal_nodes
# data_updated_w_terminal_nodes_and_weights <- treatment_pre_interview_train_updated
# outcome_variable <- "Case_OutcomeDenied"
# train_model_node_order <- treatment_post_interview_train_node_stats %>% pull(terminal_node)

# tree_output_terminal_nodes <- m1_denial_treatment_pre_interview_test_rpart_output
# observations_updated <- treatment_pre_interview_test_updated
# outcome_variable <- "Case_OutcomeDenied"

get_node_stats <- function(data_updated_w_terminal_nodes_and_weights, tree_output_terminal_nodes, outcome_variable,
                           train_model_node_order = NULL) {
        
        # convert outcome_variable string to quosure for dplyr processing
        outcome_variable_sym <- sym(outcome_variable)
        
        outcome_variable_dummy <- str_c(outcome_variable, "_dummy", sep = "")
        outcome_variable_dummy_sym <- sym(outcome_variable_dummy)
        
        # need to create outcome dummy with integers, since original outcome is a factor
        data_updated_w_terminal_nodes_and_weights <- data_updated_w_terminal_nodes_and_weights %>%
                mutate(!!outcome_variable_dummy_sym := case_when(!!outcome_variable_sym == outcome_label_positive ~ 1,
                                                                 !!outcome_variable_sym == outcome_label_negative ~ 0))

        # get weighted proportion for each terminal node
        node_p_weighted <- data_updated_w_terminal_nodes_and_weights %>% 
                mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight) %>% 
                group_by(terminal_node) %>%
                summarize(p_weighted = sum(weighted_outcome) / sum(weight), obs_in_node = n(),
                          weight_sum = sum(weight), weighted_outcome_sum = sum(weighted_outcome), 
                          outcome_positive_sum = sum(!!outcome_variable_dummy_sym)) %>% 
                mutate(lower_limit_p_weighted = 0, upper_limit_p_weighted = 0)
        
        # create placeholder for node_stats, which will be created in loop
        node_stats <- data.frame()
        
        # loop through high-prob pred_nodes calculate p_tilda_weighted and conf. int. using wilson score equation
        terminal_node_list <- data_updated_w_terminal_nodes_and_weights %>% distinct(terminal_node) %>% pull(terminal_node)
        
        for(i in 1:length(terminal_node_list)) {
                
                # get wilson score bounds for p_weighted
                current_terminal_node <- terminal_node_list[i]

                n_weighted <- node_p_weighted %>% filter(terminal_node == current_terminal_node) %>% pull(weight_sum)
                p_weighted <- node_p_weighted %>% filter(terminal_node == current_terminal_node) %>% pull(p_weighted)
                z <- 1.96

                upper_limit_p_weighted_value <- (1 / (2 * (n_weighted   + z^2)) ) * ( (2 * n_weighted * p_weighted + z^2) + 
                                                                (z * sqrt(4 * n_weighted * p_weighted * (1 - p_weighted) + z^2) ) )
                
                lower_limit_p_weighted_value <- (1 / (2 * (n_weighted + z^2)) ) * ( (2 * n_weighted * p_weighted + z^2) - 
                                                                (z * sqrt(4 * n_weighted * p_weighted * (1 - p_weighted) + z^2) ) )

                # save upper/lower_limit_p_weighted
                node_p_weighted <- node_p_weighted %>% 
                        mutate(upper_limit_p_weighted = case_when(terminal_node == current_terminal_node ~ upper_limit_p_weighted_value,
                                                                  TRUE ~ upper_limit_p_weighted),
                               lower_limit_p_weighted = case_when(terminal_node == current_terminal_node ~ lower_limit_p_weighted_value,
                                                                  TRUE ~ lower_limit_p_weighted))
                
                
                ###################################################
                
                
                # create dummy flags for obs that are part of complement to each terminal node
                complement_of_current_terminal_node_var_name <- str_c("complement_of_", current_terminal_node, sep = "")
                complement_of_current_terminal_node_sym <- sym(complement_of_current_terminal_node_var_name)
        
                data_updated_w_terminal_nodes_and_weights <- data_updated_w_terminal_nodes_and_weights %>% 
                        mutate(!!complement_of_current_terminal_node_sym := case_when(terminal_node != current_terminal_node ~ 1, 
                                                                                      terminal_node == current_terminal_node ~ 0, TRUE ~ 99))
                
                # get node_stats
                current_node_stats <- data_updated_w_terminal_nodes_and_weights %>% mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight) %>%
                        filter((!!complement_of_current_terminal_node_sym) == 1) %>%
                        summarize(complement_of_terminal_node = current_terminal_node, p_weighted_complement = sum(weighted_outcome) / sum(weight), 
                                  obs_in_node = n(), weight_sum_complement = sum(weight), weighted_outcome_sum_complement = sum(weighted_outcome))
                
                
                #####################################################
                
                
                # get wilson score bounds for p_weighted_complement
                n_weighted_complement <- current_node_stats %>% pull(weight_sum_complement)
                p_weighted_complement <- current_node_stats %>% filter(complement_of_terminal_node == current_terminal_node) %>% 
                        pull(p_weighted_complement)
                z <- 1.96
                
                upper_limit_p_weighted_complement_value <- (1 / (2 * (n_weighted_complement + z^2)) ) * 
                        ( (2 * n_weighted_complement * p_weighted_complement + z^2) + 
                        (z * sqrt(4 * n_weighted_complement * p_weighted_complement * (1 - p_weighted_complement) + z^2) ) )
                
                lower_limit_p_weighted_complement_value <- (1 / (2 * (n_weighted_complement + z^2)) ) * 
                                        ( (2 * n_weighted_complement * p_weighted_complement + z^2) - 
                                        (z * sqrt(4 * n_weighted_complement * p_weighted_complement * (1 - p_weighted_complement) + z^2) ) )
                
                # save upper/lower_limit_p_weighted_complement
                current_node_stats <-  current_node_stats %>% 
                        mutate(upper_limit_p_weighted_complement = upper_limit_p_weighted_complement_value,
                               lower_limit_p_weighted_complement = lower_limit_p_weighted_complement_value)

              
                ##############################################################
                
                
                # calculate conf. int. for diff in proportion btw domain and complement using newcombe formula
                p_weighted <- node_p_weighted %>% filter(terminal_node == current_terminal_node) %>% pull(p_weighted)
                upper_limit_p_weighted <- node_p_weighted %>% filter(terminal_node == current_terminal_node) %>% pull(upper_limit_p_weighted)
                lower_limit_p_weighted <- node_p_weighted %>% filter(terminal_node == current_terminal_node) %>% pull(lower_limit_p_weighted)
                
                p_weighted_complement <- current_node_stats$p_weighted_complement
                upper_limit_p_weighted_complement <- current_node_stats$upper_limit_p_weighted_complement
                lower_limit_p_weighted_complement <- current_node_stats$lower_limit_p_weighted_complement
                
                p_weighted_diff_value <- p_weighted - p_weighted_complement
                
                lower_limit_diff_value <- (p_weighted - p_weighted_complement) - 
                        sqrt((p_weighted - lower_limit_p_weighted)^2 + (upper_limit_p_weighted_complement - p_weighted_complement)^2)
                
                upper_limit_diff_value <- (p_weighted - p_weighted_complement) + 
                        sqrt((upper_limit_p_weighted - p_weighted)^2 + (p_weighted_complement - lower_limit_p_weighted_complement)^2)
                
                # save diff_lower/upper_limit to current_node_stats, so it can be rbind to node_stats
                # and then cbind to node_p_weighted for final output as node_p_weighted_combined
                current_node_stats <-  current_node_stats %>%
                        mutate(p_weighted_diff = p_weighted_diff_value, lower_limit_diff = lower_limit_diff_value, 
                               upper_limit_diff = upper_limit_diff_value)
                
                
                #####################################################################
                
                
                # now use reid's derivation to convert difference in domain/complement proportion/conf.int. into difference btw domain/overall
                current_node_stats <- current_node_stats %>%
                        mutate(p_diff_overall = (1 - (n_weighted / (n_weighted + n_weighted_complement))) * p_weighted_diff, 
                               lower_limit_p_diff_overall = (1 - (n_weighted / (n_weighted + n_weighted_complement))) * lower_limit_diff,
                               upper_limit_p_diff_overall = (1 - (n_weighted / (n_weighted + n_weighted_complement))) * upper_limit_diff)
                
                
                #######################################################################
                
                # rbind current_node_stats to node_stats
                node_stats <- rbind(node_stats, current_node_stats)
                

        }
        
        # combine node_p_weighted and node_stats
        node_p_weighted <- node_p_weighted %>% arrange(terminal_node)
        
        node_stats <- node_stats %>% arrange(as.numeric(complement_of_terminal_node)) %>% 
                rename(obs_in_node_complement = obs_in_node)
        
        node_p_weighted_combined <- (cbind(node_p_weighted, node_stats))
        node_p_weighted_combined <- node_p_weighted_combined %>% rowwise() %>% 
                mutate(total_obs = sum(obs_in_node, obs_in_node_complement)) %>% ungroup()
        
        
        #########################################################################
        
        
        # create p_weighted_overall, upper/lower_limit_p_weighted_overall 
        n_weighted_overall <- node_p_weighted_combined %>% summarize(n_weighted_overall = sum(weight_sum)) %>% pull(n_weighted_overall)
        p_weighted_overall_value <- node_p_weighted_combined %>% summarize(p_weighted_overall = sum(weighted_outcome_sum) / sum(weight_sum)) %>%
                pull(p_weighted_overall)
        z <- 1.96

        upper_limit_p_weighted_overall_value <- (1 / (2 * (n_weighted_overall + z^2)) ) *
                ( (2 * n_weighted_overall * p_weighted_overall_value + z^2) +
                          (z * sqrt(4 * n_weighted_overall * p_weighted_overall_value * (1 - p_weighted_overall_value) + z^2) ) )

        lower_limit_p_weighted_overall_value <- (1 / (2 * (n_weighted_overall + z^2)) ) *
                ( (2 * n_weighted_overall * p_weighted_overall_value + z^2) -
                          (z * sqrt(4 * n_weighted_overall * p_weighted_overall_value * (1 - p_weighted_overall_value) + z^2) ) )
        
        # add p_weighted_overall and lower/upper_limit_p_weighted_overall to node_p_weighted_combined
        node_p_weighted_combined <- node_p_weighted_combined %>% mutate(p_weighted_overall = p_weighted_overall_value,
                                                lower_limit_p_weighted_overall = lower_limit_p_weighted_overall_value,
                                                upper_limit_p_weighted_overall = upper_limit_p_weighted_overall_value)
        
        
        ###############################################################
        
        
        # create between_range function to get significance of domain/overall diff 
        # p_diff_overall is significant if it's conf. int. does not include zero
        between_range <- function(df, reference = 20, left_range, right_range) {
                between_logical <- between(x = reference, left = left_range, right = right_range)
                ifelse(between_logical == FALSE, 1, 0)
        }
        
        left_range_var_sym <- sym("lower_limit_p_diff_overall")
        right_range_var_sym <- sym("upper_limit_p_diff_overall")
        
        p_diff_overall_significance_value <- node_p_weighted_combined %>% 
                mutate(left_range_var = !!left_range_var_sym, right_range_var = !!right_range_var_sym) %>% 
                rowwise() %>% 
                do(output = between_range(df = ., reference = 0, left_range = .$left_range_var, right_range = .$right_range_var)) %>%
                unnest() %>% pull(output)
        
        # add p_weighted_overall_significance to node_p_weighted_combined
        node_p_weighted_combined <- node_p_weighted_combined %>%
                mutate(p_diff_overall_significance = p_diff_overall_significance_value)
        
        
        ###########################################################
        
        
        # add roc and lift stats
        
        # need to order nodes by node_order_train, since that node order is what we would have used to assign obs to having positive outcomes
        # so when evaluating what tpr we wouldve gotten if we classified node according to train model, we need to incrementally assign
        # nodes as positive according to the training model ordering of high prob nodes
        # it would be unfair to evalute the test set by seeing what tpr results from incrementally assigning the nodes as positive according to the
        # test set node probabilities/order
        
        if(!is.null(train_model_node_order)) {
                print("not training data")
        
                # get stats based on high-low probabilities ordering from training model
                node_p_weighted_combined_reordered <- node_p_weighted_combined[
                        order(match(node_p_weighted_combined$terminal_node, train_model_node_order)), ]

                node_p_weighted_combined <- node_p_weighted_combined_reordered %>%
                        mutate(cum_true_positive_rate_weighted = cumsum(weighted_outcome_sum) / sum(weighted_outcome_sum),
                               cum_true_positive_rate = cumsum(outcome_positive_sum) / sum(outcome_positive_sum),
                               pct_weighted_obs_in_node = weight_sum / sum(weight_sum),
                               cum_pct_weighted_obs = cumsum(weight_sum) / sum(weight_sum), 
                               pct_obs_in_node = obs_in_node / total_obs,
                               cum_pct_obs = cumsum(obs_in_node) / total_obs,
                               lift_weighted = cum_true_positive_rate_weighted / cum_pct_weighted_obs,
                               lift = cum_true_positive_rate / cum_pct_obs,
                               weighted_non_outcome_sum = weight_sum - weighted_outcome_sum,
                               non_outcome_sum = obs_in_node - outcome_positive_sum,
                               cum_false_positive_rate_weighted = cumsum(weighted_non_outcome_sum) / 
                                       sum(weighted_non_outcome_sum),
                               cum_false_positive_rate = cumsum(non_outcome_sum) / sum(non_outcome_sum))
                
        } else {

                print("training data")

                node_p_weighted_combined <- node_p_weighted_combined %>% arrange(desc(p_weighted)) %>%
                                mutate(cum_true_positive_rate_weighted = cumsum(weighted_outcome_sum) / sum(weighted_outcome_sum),
                                       cum_true_positive_rate = cumsum(outcome_positive_sum) / sum(outcome_positive_sum),
                                       pct_weighted_obs_in_node = weight_sum / sum(weight_sum),
                                       cum_pct_weighted_obs = cumsum(weight_sum) / sum(weight_sum), pct_obs_in_node = obs_in_node / total_obs,
                                       cum_pct_obs = cumsum(obs_in_node) / total_obs,
                                       lift_weighted = cum_true_positive_rate_weighted / cum_pct_weighted_obs,
                                       lift = cum_true_positive_rate / cum_pct_obs,
                                       weighted_non_outcome_sum = weight_sum - weighted_outcome_sum,
                                       non_outcome_sum = obs_in_node - outcome_positive_sum,
                                        cum_false_positive_rate_weighted = cumsum(weighted_non_outcome_sum) / sum(weighted_non_outcome_sum),
                                       cum_false_positive_rate = cumsum(non_outcome_sum) / sum(non_outcome_sum))
        }
        
        
        ########################################################
        
        
        # add flag for coefficient of variation filter
        # per reid, cv = SE / P, or [ (upper_conf_int - lower_conf_int) / (1.96 * 2) ] / min(p + .001, 1 - p + .001)
        # note we add .001 to p or 1-p to avoid a situation where the cv ends up being infinity
        node_p_weighted_combined %>% data.frame()
        node_p_weighted_combined <- node_p_weighted_combined %>% 
                mutate(se = ((upper_limit_p_weighted - lower_limit_p_weighted) / 
                        (1.96 * 2)), is_p_weighted_the_min = ifelse(p_weighted < p_weighted_complement, 1, 0), 
                       coeff_of_variation = case_when(is_p_weighted_the_min == 1 ~ se / (p_weighted + .001),
                                                      is_p_weighted_the_min == 0 ~ se / (p_weighted_complement + .001), TRUE ~ 0),
                       unreliable_p_weighted = ifelse(obs_in_node < 15 & coeff_of_variation >= .3 & 
                                                        coeff_of_variation <= .5, 1, 0),
                       suppressed_p_weighted = ifelse(obs_in_node < 15 & coeff_of_variation > .5, 1, 0)) %>%
                select(-c(se, is_p_weighted_the_min))
        
        
        #########################################################
        
        
        # add node_paths
        tree_output_terminal_nodes_subset <- tree_output_terminal_nodes %>% select(terminal_node, node_path)
        node_p_weighted_combined <- node_p_weighted_combined %>% mutate(terminal_node = as.character(terminal_node)) 
        node_p_weighted_combined <- left_join(node_p_weighted_combined, tree_output_terminal_nodes_subset, 
                                              by = c("terminal_node" = "terminal_node"))
        
        
        ########################################################
        
        
        # output node_p_weighted_combined
        node_p_weighted_combined
        
        # compile output_list
        # output_list <- list(data_updated_w_terminal_nodes_and_weights = data_updated_w_terminal_nodes_and_weights, node_p_weighted_combined = node_p_weighted_combined)
        # return(output_list)

}



###########################################################


# # inspect function output
# output <- get_node_stats(data_updated_w_terminal_nodes_and_weights = treatment_pre_interview_test_updated,
#                          tree_output_terminal_nodes = m1_denial_treatment_pre_interview_test_rpart_output,
#                          outcome_variable = outcome_variable)
# 
# output %>% data.frame()


#########################################################
##########################################################
#######################################################


# old code

# names(output)
# 
# 
# ######################################
# 
# 
# # inspect observations_updated
# output_observations_updated <- output$observations_updated
# names(output_observations_updated)[338:length(names(x))]
# 
# output_observations_updated %>% distinct(complement_of_7)
# output_observations_updated %>% group_by(terminal_node) %>% distinct(complement_of_7)
# output_observations_updated %>% group_by(terminal_node) %>% distinct(complement_of_16)
# output_observations_updated %>% group_by(terminal_node) %>% distinct(complement_of_21)
# output_observations_updated %>% group_by(terminal_node) %>% distinct(complement_of_9)
# 
# # check p_overall
# outcome_variable_dummy_sym <- sym(outcome_variable)
# 
# output_observations_updated <- output_observations_updated %>% mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight)
# 
# output_observations_updated %>% select(Case_OutcomeDenied, terminal_node, weight, weighted_outcome) 
# 
# p_overall <- output_observations_updated %>% mutate(weighted_outcome = (!!outcome_variable_dummy_sym) * weight) %>% 
#         select(Case_OutcomeDenied, terminal_node, weight, weighted_outcome) %>% 
#         summarize(p_overall = sum(weighted_outcome) / sum(weight)) %>% pull(p_overall)
# 
# p_overall
# 
# 
# ###############################################
# 
# 
# # inspect node_p_weighted_combined
# output_node_p_weighted_combined <- output$node_p_weighted_combined
# output_node_p_weighted_combined %>% data.frame()
# glimpse(output_node_p_weighted_combined)
# 
# output_node_p_weighted_combined %>% mutate(p_overall = p_overall, manual_p_diff_overall = p_weighted - p_overall) %>%
#         select(terminal_node, p_weighted, p_overall, manual_p_diff_overall, p_diff_overall)
# 
# 
# #################################################
# ##################################################
# #################################################
# 
# 
# # manually check diff btw p_weighted and p_overall
# output_observations_updated %>% select(Case_OutcomeDenied, terminal_node, weight, weighted_outcome) 
# 
# p_overall
# 
# node7_p_weighted <- output_observations_updated %>% filter(terminal_node == 7) %>% summarize(p_weighted = sum(weighted_outcome) / sum(weight))
# node7_p_weighted 
# 
# # get wilson conf. int. for domain 
# n1 <- output_observations_updated %>% filter(terminal_node == 7) %>% summarize(weight_sum = sum(weight))
# n1
# x1 <- output_observations_updated %>% filter(terminal_node == 7, (!!outcome_variable_dummy_sym) == 1) %>% summarize(weight_sum = sum(weight))
# x1
# p1 <- x1 / n1
# p1
# z <- 1.96
# 
# upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
# upper1
# 
# lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
# lower1
# 
# 
# ################################
# 
# 
# # get wilson conf. int. for complement
# n2 <- output_observations_updated %>% filter(terminal_node != 7) %>% summarize(weight_sum = sum(weight))
# n2
# x2 <- output_observations_updated %>% filter(terminal_node != 7, (!!outcome_variable_dummy_sym) == 1) %>% summarize(weight_sum = sum(weight))
# x2
# p2 <- x2 / n2
# p2
# z <- 1.96
# 
# upper2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) + (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
# upper2
# 
# lower2 <- (1 / (2 * (n2 + z^2)) ) * ( (2 * n2 * p2 + z^2) - (z * sqrt(4 * n2 * p2 * (1 - p2) + z^2) ) )
# lower2
# 
# 
# ####################################
# 
# 
# # get diff in domain vs complement using newcombe
# p1
# p2
# diff_dc <- p1 - p2
# diff_dc
# 
# diff_dc_lower_limit <- (p1 - p2) - sqrt((p1 - lower1)^2 + (upper2 - p2)^2)
# diff_dc_lower_limit 
# 
# diff_dc_upper_limit <- (p1 - p2) + sqrt((upper1 - p1)^2 + (p2 - lower2)^2)
# diff_dc_upper_limit    
# 
# 
# ##########################################
# 
# 
# # get diff in domain vs overall using reid's derivation
# node7_p_weighted 
# p_overall
# node7_p_weighted - p_overall
# 
# 
# diff_overall <- (1 - (n1 / (n1 + n2))) * diff_dc
# diff_overall
