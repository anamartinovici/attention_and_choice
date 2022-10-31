params_choice_in_tables <- tibble(coef_name = c("FE_b2", "FE_b3", "FE_b4", "FE_b5", 
												"colnum_1", "colnum_2", "colnum_3", "colnum_4",
												"br_know", "br_own", 
												"br_1st_fix",
												"nfix_int", "nfix_ls", "nfix_qs",
												"bsac_int", "bsac_ls", "bsac_qs",
												"asac_int", "asac_ls", "asac_qs",
												"nsac_int", "nsac_ls", "nsac_qs",
												"nfix_int_cplx", "nfix_ls_cplx", "nfix_qs_cplx",
												"bsac_int_cplx", "bsac_ls_cplx", "bsac_qs_cplx",
												"asac_int_cplx", "asac_ls_cplx", "asac_qs_cplx",
												"nsac_int_cplx", "nsac_ls_cplx", "nsac_qs_cplx"),
								  row_in_table = c(1:35))

params_att_in_tables <- tibble(param_name = c("intercept", "info_cond", "cat_own", "cat_know", 
											  "gender", "age", "education", "inc_noanswer", "inc_level",
											  "variance_cons",
											  "brand_B", "brand_C", "brand_D", "brand_E", 
											  "column_1", "column_2", "column_3", "column_4",
											  "br_know", "br_own", "br_chosen",
											  "variance_br"),
							   row_in_table = c(1:22))

cplx_user_in_tables <- tibble(column_name = c(paste0("low_", c("brandA", "brandB", "other", "nonSP")),
											  paste0("medium_", c("brandA", "brandB", "other", "nonSP")),
											  paste0("high_", c("brandA", "brandB", "other", "nonSP")),
											  paste0(c("low_", "medium_", "high_"), "all"),
											  paste0("all_", c("brandA", "brandB", "other", "loyal", "switcher", "nonSP", "NOTonscreen")),
											  "all_all"),
							  row_in_table = c(1:23))

brandname <- tibble(brandletter = c("A", "B", "C", "D", "E"),
					brand_name = c("Apple", "Samsung", "Nokia", "HTC", "Motorola"),
					sku_name = c("Apple iPhone 5", "Samsung Galaxy Note II", "Nokia Lumia 920", "HTC One", "Motorola Droid RAZR MAXX HD"))

# df_results is a data frame columns: participant, pred_atQ, choice_model, and draw_1 to draw_Ndraws (2000 for choice)
# I need to first change this in a tidy format and them summarize the hit rate
f_process_df_results <- function(f_df_results, group_by_this, f_participant_info) {
	# f_df_results contains:
	#   for one choice model
	#       for all quarters (0 to 4)
	#           for all participants j
	#               N_draws (columns) that contain the prediction performance metric
	#
	# the prediction performance metric is one of these:
	#   choice probability of the chosen brand: between 0 and 1
	#   correct brand choice prediction:  1 if the brand with max utility is chosen, 0 otherwise
	
	if(!sum(group_by_this %in% c("participant", "draw", "draw_partgroup"))) {
		cat("you've specified a group_by_this that doesn't exist")	
	}
	
	performance_mqjd <- f_df_results %>% pivot_longer(cols = starts_with("draw"),
													  names_to = "draw_number", values_to = "pred_perf")
	
	if(group_by_this == "participant") {
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
		return(performance_mqj)
	} 
	
	if(group_by_this == "draw") {
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
								   	dplyr::select(choice_model, pred_atQ, 
								   				  perf_mq_mean_over_d, perf_mq_LCI_over_d, perf_mq_UCI_over_d))
		return(performance_mq)
	} 
	
	if(group_by_this == "draw_partgroup") {
		# calculate the average prediction performance per partgroup and draw
		# check if partgroup is a column in f_participant_info
		if(sum((colnames(f_participant_info) == "partgroup")*1)) {
			performance_mqjd <- performance_mqjd %>% 
				left_join(f_participant_info %>% 
						  	select(participant, partgroup), 
						  by = "participant")
			
			# I use this when I calculate the % participants with correctly predicted brand choice
			# mean_j -> mean 0 / 1 over participants
			performance_mqgd <- performance_mqjd %>% 
				group_by(choice_model, pred_atQ, partgroup, draw_number) %>% 
				mutate(perf_mqgd_mean_over_j = mean(pred_perf)) %>% ungroup()
			performance_mqgd <- distinct(performance_mqgd %>% 
										 	dplyr::select(choice_model, pred_atQ, partgroup, draw_number, 
										 				  perf_mqgd_mean_over_j))
			
			# LCI is calculated over the draws
			performance_mqg <- performance_mqgd %>% 
				group_by(choice_model, pred_atQ, partgroup) %>% 
				mutate(perf_mqg_mean_over_d = mean(perf_mqgd_mean_over_j),
					   perf_mqg_LCI_over_d  = quantile(perf_mqgd_mean_over_j, probs = c(0.025)),
					   perf_mqg_UCI_over_d  = quantile(perf_mqgd_mean_over_j, probs = c(0.975))) %>% ungroup()
			performance_mqg <- distinct(performance_mqg %>% 
											dplyr::select(choice_model, pred_atQ, partgroup,
														  perf_mqg_mean_over_d, perf_mqg_LCI_over_d, perf_mqg_UCI_over_d))
			return(performance_mqg)
		} else {
			cat(" I cannot continue without partgroup ")
		}
	} 
}

f_loyal_switcher <- function(f_part) {
	if(!(sum((colnames(f_part) == "loyalty_3groups")*1))) {
		f_part$own_onscreen <- (f_part$brandletter_owned != 0)*1
		f_part$own_NOT_onscreen <- f_part$smartphone_owner - f_part$own_onscreen
		
		f_part$loyal    <- (f_part$brandletter_chosen == f_part$brandletter_owned)*1
		f_part$switcher <- f_part$own_onscreen - f_part$loyal
		f_part$nonSP    <- (!f_part$smartphone_owner)*1
		
		f_part$groupnumber <- (f_part$loyal)*1 + (f_part$switcher)*2 + (f_part$nonSP)*3 + (f_part$own_NOT_onscreen)*4
		aux_df_bis <- data.frame(groupnumber = c(1:4),
								 loyalty_3groups = c("loyal", "switcher", "other", "other"),
								 loyalty_4groups = c("loyal", "switcher", "nonSP", "NOTonscreen"),
								 stringsAsFactors = FALSE)
		f_part <- f_part %>% left_join(aux_df_bis, by = "groupnumber")
		f_part <- f_part %>% select(-groupnumber)
	}
	
	return(f_part)
}


f_compare_results <- function(f_current_results, f_original_results_path) {
	cat("\n")
	cat("Original results: ", f_original_results_path)
	cat("\n")
	cat("Comparing current and original results: ")
	
	original_results <- read.csv(paste0(f_original_results_path, ".csv"))
	original_results <- original_results[, -1]
	
	if(all.equal(f_current_results, original_results, check.attributes = FALSE)) {
		cat("all good")
	} else {
		cat(paste("Results differ!"))
	}
	cat("\n")
}

f_calc_pvalue <- function(x) {
	return ((sum(x > 0) * (mean(x) < 0) + sum(x < 0) * (mean(x > 0))) / length(x))
}

f_create_mpLU <- function(f_draws) {
	return(c(mean(f_draws), f_calc_pvalue(f_draws), quantile(f_draws, probs = c(0.025, 0.975))))
}

f_create_mSDp <- function(f_draws) {
	return(c(mean(f_draws), sd(f_draws), f_calc_pvalue(f_draws)))
}

f_create_mSDpLU <- function(f_draws) {
	return(c(mean(f_draws), sd(f_draws), f_calc_pvalue(f_draws), quantile(f_draws, probs = c(0.025, 0.975))))
}

