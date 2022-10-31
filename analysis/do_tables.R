f_summarise_obs_EM <- function(obs_eye_movements, 
							   sum_by_EM_name, sum_by_Quarter, sum_by_CPLX_cond, sum_by_type_of_brand) {
	
	aux_EM <- obs_eye_movements
	
	# keep only participants in the CPLX condition I need
	if(sum_by_CPLX_cond != "overall") {
		aux_EM <- aux_EM %>% filter(cplx_cond == sum_by_CPLX_cond)
	}
	
	if(sum_by_Quarter != 1234) {
		aux_EM <- aux_EM %>% filter(quarter == sum_by_Quarter)
	} 
	
	if(sum_by_type_of_brand == "chosen") {
		aux_EM <- aux_EM %>% filter(dummy_chosen == 1)
	}
	
	if(sum_by_type_of_brand == "notchosen") {
		aux_EM <- aux_EM %>% filter(dummy_chosen == 0)
		aux_EM <- aux_EM %>%
			group_by(participant, brandletter, EM_name) %>%
			summarise(obs_EM = sum(obs_EM_count_jigq)) %>% ungroup()
		aux_EM <- aux_EM %>%
			rename(obs_EM_count_jigq = obs_EM)
	}
	
	if(sum_by_EM_name == "nfix") {
		aux_EM <- aux_EM %>% filter(EM_name == "fix")
		
		if(sum_by_type_of_brand == "notchosen") {
			aux_EM <- aux_EM %>%
				group_by(participant, brandletter) %>%
				summarise(target_value = sum(obs_EM_count_jigq)) %>% ungroup()    
		} else {
			aux_EM <- aux_EM %>%
				group_by(participant) %>%
				summarise(target_value = sum(obs_EM_count_jigq)) %>% ungroup()    
		}
	} else {
		aux_EM <- aux_EM %>% filter(EM_name != "fix")
		
		aux_EM <- aux_EM %>%
			group_by(participant, brandletter, EM_name) %>%
			mutate(sac_jig = sum(obs_EM_count_jigq)) %>% ungroup()
		aux_EM <- distinct(aux_EM %>%
						   	select(participant, EM_name, brandletter, sac_jig))
		aux_EM <- aux_EM %>%
			group_by(participant, brandletter) %>%
			mutate(sac_ji = sum(sac_jig)) %>% ungroup()
		aux_EM <- aux_EM %>% filter(EM_name == sum_by_EM_name)
		aux_EM <- aux_EM %>%
			mutate(target_value = sac_jig / sac_ji)
	}
	
	return(c(mean_value = mean(aux_EM$target_value, na.rm = TRUE), 
			 sd_value = sd(aux_EM$target_value, na.rm = TRUE)))
}

# Enclose every value with double quotation marks and prefix an equal sign like ="00001" 
# or prepend a tab to every value. Both ways will force Excel to treat the value as text
# source: https://superuser.com/questions/307496/how-can-i-set-excel-to-always-import-all-columns-of-csv-files-as-text
f_format_pvalue <- function(f_pvalue) {
	if(f_pvalue < 0.001) {
		return(numform::f_num(0.001, digits = 3, p = '="<', s = '"'))
	} else {
		if(round(f_pvalue, digits = 2) == 0) {
			return(numform::f_num(f_pvalue, digits = 3, p = '="', s = '"'))
		} else {
			if(round(f_pvalue, 2) <= 0.04) {
				return(numform::f_num(f_pvalue, digits = 2, p = '="', s = '"'))
			} else {
				if(round(f_pvalue, 2) <= 0.05) {
					return(numform::f_num(f_pvalue, digits = 3, p = '="', s = '"'))
				} else {
					return(numform::f_num(f_pvalue, digits = 2, p = '="', s = '"'))
				}
			}
		}
		
	}
}

f_format_CI <- function(f_LCI, f_UCI, n_digits) {
	return(paste0('[', numform::f_num(f_LCI, digits = n_digits), "; ", numform::f_num(f_UCI, digits = n_digits), ']'))
}

do_table_EM_summaries <- function(f_output_filename) {
	load(here::here("data", "SNTH_procdata.RData"))
	
	obs_eye_movements <- obs_eye_movements %>% 
		left_join(participant_info %>% 
				  	select(participant, cplx_cond, brandletter_chosen), 
				  by = "participant")
	
	obs_eye_movements <- obs_eye_movements %>% 
		mutate(dummy_chosen = case_when(brandletter == brandletter_chosen ~ 1,
										TRUE ~ 0))
	obs_eye_movements <- obs_eye_movements %>% 
		rename(obs_EM_count_jigq = EM_count)
	
	sum_by_Quarter <- c(1234, 1:4)
	sum_by_EM_name <- c("nfix", "bsac", "asac", "nsac")
	sum_by_CPLX_cond <- c("overall", "low", "medium", "high")
	
	table1_results <- tibble_row(quarter = "placeholder",
								 typeEM = "placeholder",
								 group_name = "placeholder",
								 mean_value = c(0),
								 sd_value = c(0))
	for(index_Q in sum_by_Quarter) {
		for(index_EM in sum_by_EM_name) {
			
			for(index_cplx in sum_by_CPLX_cond) {
				aux_result <- f_summarise_obs_EM(obs_eye_movements = obs_eye_movements,
												 sum_by_EM_name = index_EM,
												 sum_by_Quarter = index_Q, 
												 sum_by_CPLX_cond = index_cplx, 
												 sum_by_type_of_brand = "all")
				
				table1_results <- table1_results %>%
					add_row(quarter = as.character(index_Q),
							typeEM = index_EM,
							group_name = index_cplx,
							mean_value = aux_result[["mean_value"]],
							sd_value = aux_result[["sd_value"]])
			}
		}
	}
	
	table1_results <- table1_results %>%
		filter(quarter != "placeholder") %>%
		pivot_wider(names_from = group_name,
					values_from = c(mean_value, sd_value))
	
	names(table1_results) <- str_remove_all(names(table1_results), "value_")
	
	table1_results <- table1_results %>%
		select(quarter, typeEM, 
			   mean_low, sd_low, 
			   mean_medium, sd_medium,
			   mean_high, sd_high)    
	
	row_order_table1 <- tibble(quarter = c(flatten_chr(c("1", "2", "3", "4", "1234") %>% map(rep, 4))),
							   typeEM = c(rep(c("nfix", "bsac", "asac", "nsac"), 5)),
							   rownumber = c(1:20))
	
	table1_results <- table1_results %>%
		left_join(row_order_table1, 
				  by = c("quarter", "typeEM")) %>%
		arrange(rownumber)
	
	table1_results <- table1_results %>%
		modify_at(colnames(table1_results)[str_detect(colnames(table1_results), "mean")], 
				  function(x) numform::f_num(x, digits = 2, p = '="', s = '"')) %>%
		modify_at(colnames(table1_results)[str_detect(colnames(table1_results), "sd")], 
				  function(x) numform::f_num(x, digits = 2, p = '="', s = '"'))
	
	table1_results <- table1_results %>%
		mutate(quarter = as.integer(quarter),
			   rownumber = as.integer(rownumber))
	
	write.csv(table1_results, 
			  file = paste0(DIR_current_results, "/", f_output_filename, ".csv"))
	
	note_under_table <- paste0("Total sample size for eye-fixation frequency is ",
							   nrow(participant_info), " across 5 brands (",
							   nrow(participant_info), "x 5 = ", nrow(participant_info)*5, "), with ",
							   nrow(participant_info %>% filter(cplx_cond == "low")), " (",
							   nrow(participant_info %>% filter(cplx_cond == "low"))*5, ") in low, ",
							   nrow(participant_info %>% filter(cplx_cond == "medium")), " (",
							   nrow(participant_info %>% filter(cplx_cond == "medium"))*5, ") in medium, and ",
							   nrow(participant_info %>% filter(cplx_cond == "high")), " (",
							   nrow(participant_info %>% filter(cplx_cond == "high"))*5, ") in the high information condition. ",
							   "For chosen brands n = ", nrow(participant_info), 
							   ", and for non-chosen brands n = ", nrow(participant_info)*4, " (4 x ", nrow(participant_info), ").",
							   "Average eye fixations frequency across 4 non-chosen brands shown.")
	write.csv(note_under_table,
			  file = paste0(DIR_current_results, "/", f_output_filename, "_note.csv"))
}

do_table_elpd_hit <- function(DIR_LOF, model_number_in_paper, 
							  DIR_current_results,
							  f_output_filename) {
	path_to_file <- paste0(DIR_LOF, 
						   "/K12fold", "/pred_choice")
	load(paste0(path_to_file, "/SNTH_df_probchosenbrand_mqj.RData"))
	load(paste0(path_to_file, "/SNTH_df_correctpredbrandchoice_mq.RData"))
	
	load(here::here("data", "SNTH_procdata.RData"))
	
	df_choice_K <- f_create_df_models()
	current_results <- df_probchosenbrand_mqj %>% 
		filter(choice_model %in% model_number_in_paper, pred_atQ == 4) %>%
		mutate(log_density_mqj = log(perf_mqj_mean_over_d))
	current_results <- current_results %>% 
		group_by(choice_model) %>% 
		summarise(ELPD = sum(log_density_mqj))
	current_results <- current_results %>% 
		left_join(df_choice_K %>% select(M_in_paper, n_K, has_br_FE, model_description), 
				  by = c("choice_model" = "M_in_paper"))
	current_results <- current_results %>%
		mutate(npars = n_K + (B-1)*has_br_FE) %>%
		select(choice_model, npars, ELPD, model_description)
	
	aux_df <- df_correctpredbrandchoice_mq %>% 
		filter(choice_model %in% model_number_in_paper, pred_atQ == 4) 
	current_results <- current_results %>% left_join(aux_df, by = "choice_model")
	remove(aux_df)
	
	current_results <- current_results %>% 
		select(choice_model, npars, ELPD,
			   model_description, perf_mq_mean_over_d,
			   perf_mq_LCI_over_d, perf_mq_UCI_over_d) %>%
		arrange(choice_model)
	
	current_results <- current_results %>%
		modify_at(c("ELPD"), function(x) numform::f_num(x, digits = 0, p = '="', s = '"')) %>%
		modify_at(c("perf_mq_mean_over_d"), function(x) numform::f_percent(100*x, digits = 0)) %>% 
		mutate(hit_CI = f_format_CI(100*perf_mq_LCI_over_d, 100*perf_mq_UCI_over_d, n_digits = 0))
	
	current_results <- current_results %>% select(choice_model, npars, ELPD, 
												  perf_mq_mean_over_d, hit_CI,
												  model_description)
	
	write.csv(current_results, file = paste0(DIR_current_results, "/", f_output_filename, ".csv"))
}


do_table_brand_choice_coeff <- function(DIR_LOF, choice_model, DIR_current_results, f_output_filename) {
	load(here::here(DIR_LOF, "fullsample", "est_choice",
					paste0("SNTH_est_choice_", choice_model, ".RData")))
	df_choice_K <- f_create_df_models()
	beta_choice_draws <- rstan::extract(stan_choice_samples, pars = "beta")[[1]]
	
	current_results <- t(apply(beta_choice_draws, 2, f_create_mpLU))
	current_results <- data.frame(current_results)
	colnames(current_results) <- c("mean_coef", "pvalue", "LCI", "UCI")
	
	current_results <- cbind(current_results, 
							 (df_choice_K %>% 
							  	filter(M_in_paper == choice_model) %>% 
							  	select(beta_info))[[1]][[1]])
	current_results <- current_results %>% 
		left_join(params_choice_in_tables, by = c("coef_name"))
	current_results <- current_results %>% 
		arrange(row_in_table) %>%
		select(row_in_table, coef_name, mean_coef, pvalue, LCI, UCI)
	
	current_results <- current_results %>%
		modify_at(c("mean_coef", "LCI", "UCI"), function(x) numform::f_num(x, digits = 2, p = '="', s = '"')) %>%
		mutate(pvalue = map_chr(pvalue, f_format_pvalue)) %>% 
		select(row_in_table, coef_name, mean_coef, pvalue, LCI, UCI)
	
	write.csv(current_results, file = paste0(DIR_current_results, "/", f_output_filename, ".csv"))
}

