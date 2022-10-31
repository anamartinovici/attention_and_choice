my_which_max <- function(f_vector) {
	index_max_values <- (f_vector == max(f_vector))*c(1:length(f_vector))
	index_max_values <- index_max_values[index_max_values > 0]
	# return the indices brands with max utility (if there are more than 1)
	# this solves ties (2+ brands with max utility) by random draw
	if(length(index_max_values) == 1) {
		pred_choice <- index_max_values	
	} else {
		pred_choice <- sample(x = index_max_values, size = 1)
	}
	
	return(pred_choice)
}

f_hit01_randomties <- function(f_list) {
	# f_list includes brand_utility as a matrix of size B x draws
	# get the position of the max utility, so the predicted brand choice
	brand_with_max_utility <- apply(f_list[["brand_utility"]], 2, my_which_max)
	# save 1 if the predicted brand choice is correct and 0 otherwise
	
	aux_result <- (brand_with_max_utility == f_list[["true_choice"]])*1
	aux_result <- c(f_list[["participant"]], aux_result)
	aux_result <- data.frame(array(aux_result, dim = c(1, length(aux_result))))
	names(aux_result) <- c("participant", paste0("draw_", 1:(length(aux_result)-1)))
	aux_result <- aux_result %>%
		mutate(pred_atQ = f_list[["pred_atQ"]],
			   choice_model = f_list[["choice_model"]]) %>%
		relocate(participant,
				 pred_atQ,
				 choice_model)
	
	return(aux_result)
}

f_prob_truechoice <- function(f_list) {
	library(matrixStats)
	
	# B x draws
	brand_utility <- f_list[["brand_utility"]]
	aux_logsumexp <- apply(brand_utility, 2, logSumExp)
	
	aux_result <- exp(brand_utility[f_list[["true_choice"]], ] - aux_logsumexp)
	aux_result <- c(f_list[["participant"]], aux_result)
	aux_result <- data.frame(array(aux_result, dim = c(1, length(aux_result))))
	names(aux_result) <- c("participant", paste0("draw_", 1:(length(aux_result)-1)))
	aux_result <- aux_result %>%
		mutate(pred_atQ = f_list[["pred_atQ"]],
			   choice_model = f_list[["choice_model"]]) %>%
		relocate(participant,
				 pred_atQ,
				 choice_model)
	
	return(aux_result)
}

f_summarise_by_mq <- function(df_results) {
	# df_results contains:
	#   for one choice model
	#       for all quarters (0 to 4 for oos predictions and only 4 for insample)
	#           for all participants j
	#               N_draws (columns) that contain the prediction performance metric
	#
	# the prediction performance metric is one of these:
	#   choice probability of the chosen brand: between 0 and 1
	#   correct brand choice prediction:  1 if the brand with max utility is chosen, 0 otherwise
	performance_mqjd <- df_results %>% pivot_longer(cols = starts_with("draw"),
													names_to = "draw_number", 
													values_to = "pred_perf")
	
	# calculate the average prediction performance per draw
	# I use this when I calculate the % participants with correctly predicted brand choice
	# mean_j -> mean 0 / 1 over participants
	performance_mqd <- performance_mqjd %>% 
		group_by(choice_model, pred_atQ, draw_number) %>% 
		summarise(perf_mqd_mean_over_j = mean(pred_perf)) %>% ungroup()
	
	# LCI is calculated over the draws
	performance_mq <- performance_mqd %>% 
		group_by(choice_model, pred_atQ) %>% 
		summarise(perf_mq_mean_over_d = mean(perf_mqd_mean_over_j),
				  perf_mq_LCI_over_d  = quantile(perf_mqd_mean_over_j, probs = c(0.025)),
				  perf_mq_UCI_over_d  = quantile(perf_mqd_mean_over_j, probs = c(0.975))) %>% ungroup()
	
	return(performance_mq)
}

f_summarise_by_mqj <- function(df_results) {
	# df_results contains:
	#   for one choice model
	#       for all quarters (0 to 4 for oos predictions and only 4 for insample)
	#           for all participants j
	#               N_draws (columns) that contain the prediction performance metric
	#
	# the prediction performance metric is one of these:
	#   choice probability of the chosen brand: between 0 and 1
	#   correct brand choice prediction:  1 if the brand with max utility is chosen, 0 otherwise
	performance_mqjd <- df_results %>% pivot_longer(cols = starts_with("draw"),
													names_to = "draw_number", 
													values_to = "pred_perf")
	
	# calculate the average prediction performance per participant
	# I need this to calculate elpd
	# mean_d -> average taken over the draws 
	# mean_d = sum(pred_perf at draw d)/N_draws 
	# there are 2000 draws used to estimate choice
	# LCI is calculated over the N_draws
	performance_mqj <- performance_mqjd %>% 
		group_by(choice_model, pred_atQ, participant) %>% 
		summarise(perf_mqj_mean_over_d = mean(pred_perf),
				  perf_mqj_LCI_over_d  = quantile(pred_perf, probs = c(0.025)),
				  perf_mqj_UCI_over_d  = quantile(pred_perf, probs = c(0.975))) %>% ungroup()
	
	return(performance_mqj)
}


# df_results is a data frame columns: participant, pred_atQ, choice_model, and draw_1 to draw_Ndraws (2000 for choice)
# I need to first change this in a tidy format and them summarize the hit rate
f_process_brand_choice_predperf <- function(f_participant_info,
											f_type_of_fold, 
											f_type_of_pred,
											f_choice_model) {
	
	if(f_type_of_fold == "fullsample") {
		f_current_Q <- 4
		load(paste(LOF_DIR, "/",  type_of_data, "/", niter_att, 
				   "/fullsample/est_choice/",
				   "/",  type_of_data, "_est_choice_",  f_choice_model, "_",  niter_att, ".RData", sep = ""))
		
		if(f_type_of_pred %in% c("predchprob")) {
			df_results <- f_calculate_chosen_perc_perdraw(f_choice_draws = stan_choice_samples,
														  f_true_choice = stan_data_choice[["est_y"]], 
														  f_X_choice = stan_data_choice[["est_X"]])
		} else {
			if(f_type_of_pred %in% c("predbrandchoice01")) {
				df_results <- f_predbrandchoice_perdraw(f_choice_draws = stan_choice_samples, 
														f_true_choice = stan_data_choice[["est_y"]], 
														f_X_choice = stan_data_choice[["est_X"]])
			}
		}
		
		colnames(df_results) <- c(paste("draw_", 1:stan_choice_samples@stan_args[[1]]$iter, sep = ""))
		df_results <- as.data.frame(df_results, stringsAsFactors = FALSE)
		df_results <- df_results %>%
			mutate(participant = c(1:N),
				   pred_atQ = c(rep(f_current_Q, N)),
				   choice_model = rep(f_choice_model, N))
		# move the columns that are not "draw_" to the front
		df_results <- df_results %>% relocate(participant, pred_atQ, choice_model)
	} else {
		load(paste(LOF_DIR, "/",  type_of_data, "/", niter_att,
				   "/", f_type_of_fold, "/pred_choice/", f_type_of_pred, "/",  f_choice_model, 
				   "/",  type_of_data, "_pred_choice_",  f_choice_model, "_",  niter_att, ".RData", sep = ""))
		f_check_df_results(type_of_pred = f_type_of_pred, df_results = df_results, choice_model = f_choice_model)
	}
	
	# df_results contains:
	#   for one choice model
	#       for all quarters (0 to 4 for oos predictions and only 4 for insample)
	#           for all participants j
	#               N_draws (columns) that contain the prediction performance metric
	#
	# the prediction performance metric is one of these:
	#   choice probability of the chosen brand: between 0 and 1
	#   correct brand choice prediction:  1 if the brand with max utility is chosen, 0 otherwise
	
	performance_mqjd <- df_results %>% pivot_longer(cols = starts_with("draw"),
													names_to = "draw_number", 
													values_to = "pred_perf")
	
	if(f_type_of_pred %in% c("predchprob")) {
		# calculate the average prediction performance per participant
		# I need this to calculate elpd
		# mean_d -> average taken over the draws 
		# mean_d = sum(pred_perf at draw d)/N_draws 
		# there are 2000 draws used to estimate choice
		# LCI is calculated over the N_draws
		performance_mqj <- performance_mqjd %>% 
			group_by(choice_model, pred_atQ, participant) %>% 
			mutate(perf_mqj_mean_over_d = mean(pred_perf),
				   perf_mqj_LCI_over_d  = quantile(pred_perf, probs = c(0.025)),
				   perf_mqj_UCI_over_d  = quantile(pred_perf, probs = c(0.975))) %>% ungroup()
		
		performance_mqj <- distinct(performance_mqj %>% 
										dplyr::select(choice_model, pred_atQ, participant, 
													  perf_mqj_mean_over_d, perf_mqj_LCI_over_d, perf_mqj_UCI_over_d))
		performance_mqj <- performance_mqj %>% 
			mutate(type_Kfold = case_when(f_type_of_fold == "K10fold" ~ "randomsplit",
										  f_type_of_fold == "K12fold" ~ "designbased",
										  f_type_of_fold == "insample" ~ "insample",
										  TRUE ~ "check_obs"))
		return(performance_mqj)
	} else {
		if(f_type_of_pred %in% c("predbrandchoice01")) {
			# calculate the average prediction performance per draw
			# I use this when I calculate the % participants with correctly predicted brand choice
			# mean_j -> mean 0 / 1 over participants
			performance_mqd <- performance_mqjd %>% 
				group_by(choice_model, pred_atQ, draw_number) %>% 
				mutate(perf_mqd_mean_over_j = mean(pred_perf)) %>% ungroup()
			performance_mqd <- distinct(performance_mqd %>% 
											dplyr::select(choice_model, pred_atQ, draw_number, 
														  perf_mqd_mean_over_j))
			
			# LCI is calculated over the draws
			performance_mq <- performance_mqd %>% 
				group_by(choice_model, pred_atQ) %>% 
				mutate(perf_mq_mean_over_d = mean(perf_mqd_mean_over_j),
					   perf_mq_LCI_over_d  = quantile(perf_mqd_mean_over_j, probs = c(0.025)),
					   perf_mq_UCI_over_d  = quantile(perf_mqd_mean_over_j, probs = c(0.975))) %>% ungroup()
			performance_mq <- distinct(performance_mq %>% 
									   	dplyr::select(choice_model, 
									   				  pred_atQ, 
									   				  perf_mq_mean_over_d, 
									   				  perf_mq_LCI_over_d, 
									   				  perf_mq_UCI_over_d))
			performance_mq <- performance_mq %>% 
				mutate(type_Kfold = case_when(f_type_of_fold == "K10fold" ~ "randomsplit",
											  f_type_of_fold == "K12fold" ~ "designbased",
											  f_type_of_fold == "insample" ~ "insample",
											  TRUE ~ "check_obs"))
			return(performance_mq)
		} else {
			cat("check this function")
		}
	} 
}

f_check_df_results <- function(type_of_pred, df_results, choice_model) {
	if(dim(df_results)[1] != (N*(Q+1))) {
		cat("There is a problem with", choice_model, "having too few observations")
		cat("\n")
		stop("Error with brand choice probabilities")
		cat("\n")
	}
	
	if(type_of_pred %in% c("predchprob")) {
		if((min(df_results$draw_1) < 0) || (max(df_results$draw_1) > 1)) {
			cat("For brand choice probabilities, all draws should be strictly between 0 and 1. This is not for", choice_model)
			cat("\n")
			stop("Error with brand choice probabilities")
			cat("\n")
		}
	}
	
	
	if(type_of_pred == "predbrandchoice01") {
		if((median(df_results$draw_1) > 0) && (median(df_results$draw_1) < 1)) {
			cat("For hit 0 1, all draws should be either 0 and 1. This is not for", choice_model)
			cat("\n")
			stop("Error with brand choice hit rate ")
			cat("\n")
		}
	}
}

f_predbrandchoice_perdraw <- function(f_choice_draws, f_true_choice, f_X_choice) {
	library(matrixStats)
	
	beta_draws <- rstan::extract(f_choice_draws, pars = c("beta"))[[1]]
	print("dimension of choice beta draws")
	print(dim(beta_draws))
	
	f_ndraws <- dim(beta_draws)[1]
	f_N <- length(f_true_choice)
	f_chbrand <- array(0, dim = c(f_N, f_ndraws))
	for(j in 1:f_N) {
		f_utility <- beta_draws %*% t(f_X_choice[j, , ])
		# f_utility is draws x B
		# now get the position of the max utility, so the predicted brand choice
		f_utility <- apply(f_utility, 1, which.max)
		# save 1 if the predicted brand choice is correct and 0 otherwise
		f_chbrand[j, ] <- (f_utility == f_true_choice[j])*1
	}
	
	return(f_chbrand)
}

f_calculate_chosen_perc_perdraw <- function(f_choice_draws, f_true_choice, f_X_choice) {
	library(matrixStats)
	
	beta_draws <- rstan::extract(f_choice_draws, pars = c("beta"))[[1]]
	print("dimension of choice beta draws")
	print(dim(beta_draws))
	
	f_ndraws <- dim(beta_draws)[1]
	f_N <- length(f_true_choice)
	f_perc_chbrand <- array(0, dim = c(f_N, f_ndraws))
	for(j in 1:f_N) {
		f_utility <- beta_draws %*% t(f_X_choice[j, , ])
		# f_utility is draws x B
		aux_logsumexp <- apply(f_utility, 1, logSumExp)
		f_perc_chbrand[j, ] <- exp(f_utility[, f_true_choice[j]] - aux_logsumexp)
	}
	
	return(f_perc_chbrand)
}
