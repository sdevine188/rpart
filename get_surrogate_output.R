library(dplyr)
library(stringr)


# function to get surrogate splits for each node
# note it will also print summary output, but only return the tbl with surrogate output
# just the base summary.rpart function updated to return the tbl w/ output
# https://github.com/cran/rpart/blob/master/R/summary.rpart.R

get_surrogate_output <-
        function(object, cp = 0, digits = getOption("digits"), file, ...)
        {
                surrogate_tbl <- tibble()
                
                if (!inherits(object, "rpart")) stop("Not a legitimate \"rpart\" object")
                
                ## rename it to "x" to save typing
                x <- object
                
                if (!missing(file)) {
                        sink(file)
                        on.exit(sink())
                }
                
                if (!is.null(x$call)) {
                        cat("Call:\n")
                        dput(x$call, control = NULL)
                }
                
                omit <- x$na.action
                n <- x$frame$n
                if (length(omit)) cat("  n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
                else cat("  n=", n[1L], "\n\n")
                
                print(x$cptable, digits = digits)
                if (!is.null(temp <- x$variable.importance)) {
                        temp <- round(100 * temp/sum(temp))
                        if (any(temp > 0)) {
                                cat("\nVariable importance\n")
                                print(temp[temp > 0])
                        }
                }
                ff <- x$frame
                ylevel <- attr(x, "ylevels")
                id <- as.integer(row.names(ff))
                parent.id <- ifelse(id == 1L, 1L, id %/% 2L)
                parent.cp <- ff$complexity[match(parent.id, id)]
                rows <- seq_along(id)[parent.cp > cp]
                rows <- if (length(rows)) rows[order(id[rows])] else 1L
                is.leaf <- ff$var == "<leaf>"
                index <- cumsum(c(1L, ff$ncompete + ff$nsurrogate + !is.leaf))
                
                if ( !all(is.leaf)) { # skip these lines for a "no splits" tree
                        sname <- rownames(x$splits)
                        cuts <- character(nrow(x$splits))
                        temp <- x$splits[ , 2L]
                        for (i in seq_along(cuts)) {
                                cuts[i] <- if (temp[i] == -1L)
                                        paste("<", format(signif(x$splits[i, 4L], digits)))
                                else if (temp[i] == 1L)
                                        paste("<", format(signif(x$splits[i, 4L], digits)))
                                else paste("splits as ",
                                           paste(c("L", "-", "R")[x$csplit[x$splits[i, 4L], 1:temp[i]]],
                                                 collapse = "", sep = ""),
                                           collapse = "")
                        }
                        
                        if (any(temp < 2L))
                                cuts[temp < 2L ] <- format(cuts[temp < 2L], justify = "left")
                        cuts <- paste0(cuts, ifelse(temp >= 2L, ",",
                                                    ifelse(temp == 1L, " to the right,", " to the left, ")))
                }
                
                tmp <- if (is.null(ff$yval2)) ff$yval[rows]
                else ff$yval2[rows , , drop = FALSE]
                tprint <-
                        x$functions$summary(tmp, ff$dev[rows], ff$wt[rows], ylevel, digits)
                
                for (ii in seq_along(rows)) {
                        i <- rows[ii]
                        nn <- ff$n[i]
                        cat("\nNode number ", id[i], ": ", nn, " observations", sep = "")
                        if (ff$complexity[i] < cp || is.leaf[i]) cat("\n")
                        else cat(",    complexity param=",
                                 format(signif(ff$complexity[i], digits)), "\n", sep = "")
                        
                        cat(tprint[ii], "\n")
                        if (ff$complexity[i] > cp && !is.leaf[i] ){
                                sons <- 2L * id[i] + c(0L, 1L)
                                sons.n <- ff$n[match(sons, id)]
                                cat("  left son=", sons[1L], " (", sons.n[1L], " obs)",
                                    " right son=", sons[2L], " (", sons.n[2L], " obs)", sep = "")
                                j <- nn - (sons.n[1L] + sons.n[2L])
                                if (j > 1L) cat(", ", j, " observations remain\n", sep = "")
                                else if (j == 1L) cat(", 1 observation remains\n")
                                else  cat("\n")
                                cat("  Primary splits:\n")
                                j <- seq(index[i], length.out = 1L + ff$ncompete[i])
                                temp <- if (all(nchar(cuts[j], "w") < 25L))
                                        format(cuts[j], justify = "left")
                                else  cuts[j]
                                cat(paste("      ", format(sname[j], justify = "left"), " ", temp,
                                          " improve=", format(signif(x$splits[j, 3L], digits)),
                                          ", (", nn - x$splits[j, 1L], " missing)", sep = ""),
                                    sep = "\n")
                                
                                
                                primary_split_part1 <- sname[j][1]
                                # print(str_c("primary_split_part1 = ", primary_split_part1))
                                primary_split_part2 <- temp[1]
                                
                                
                                if (ff$nsurrogate[i] > 0L) {
                                        cat("  Surrogate splits:\n")
                                        j <- seq(1L + index[i] + ff$ncompete[i],
                                                 length.out = ff$nsurrogate[i])
                                        agree <- x$splits[j, 3L]
                                        temp <- if (all(nchar(cuts[j], "w") < 25L))
                                                format(cuts[j], justify = "left")
                                        else cuts[j]
                                        adj <- x$splits[j, 5L]
                                        cat(paste("      ", format(sname[j], justify = "left"), " ",
                                                  temp,
                                                  " agree=", format(round(agree, 3L)),
                                                  ", adj=" , format(round(adj, 3L)),
                                                  ", (", x$splits[j, 1L], " split)", sep = ""),
                                            sep = "\n")
                                        
                                        surrogate_tbl <- bind_rows(surrogate_tbl, tibble(node_number = id[i],
                                                        primary_split_part1 = primary_split_part1, primary_split_part2 = primary_split_part2,
                                                        surrogate_split_part1 = sname[j], surrogate_split_part2 = temp,
                                                        primary_split_true_obs = sons.n[1L], primary_split_false_obs = sons.n[2L],
                                                         surrogate_split_direction_when_true = temp,
                                                        surrogate_agreement_w_primary_split = agree,
                                                        surrogate_adjusted_agreement_w_primary_split = adj,
                                                        count_of_obs_split_using_surrogate = x$splits[j, 1L]))
                                }
                        }
                }
                cat("\n")
                invisible(x)
                print(x$frame)
                
                surrogate_tbl <- surrogate_tbl %>%
                        mutate(surrogate_split_direction_when_true = ifelse(str_detect(string = surrogate_split_direction_when_true,
                                pattern = "to the right"), "to the right", "to the left"),
                               primary_split = str_c(primary_split_part1,
                                                     str_replace(string = primary_split_part2, pattern = "to the right,|to the left,",
                                                                 replacement = "")),
                               surrogate_split = str_c(surrogate_split_part1,
                                                 str_replace(string = surrogate_split_part2, pattern = "to the right,|to the left,",
                                                 replacement = "")),
                                majority_rule = ifelse(primary_split_true_obs > primary_split_false_obs, "primary_split_true",
                                                      "primary_split_false"),
                               surrogate_split_mimics = ifelse(surrogate_split_direction_when_true == "to the left",
                                                               "primary_split", "negate_primary_split")) %>%
                        select(node_number, primary_split, surrogate_split, primary_split_true_obs, primary_split_false_obs, majority_rule,
                               surrogate_split_direction_when_true, surrogate_split_mimics,
                               surrogate_agreement_w_primary_split, surrogate_adjusted_agreement_w_primary_split,
                               count_of_obs_split_using_surrogate)
                return(surrogate_tbl)
        }