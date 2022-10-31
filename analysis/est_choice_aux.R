f_create_df_models <- function() {
	df_choice_K <- tibble_row(M_in_paper = c("M0"),
							  do_for_SNTH_FS = c(0),
							  do_for_SNTH_K12fold = c(1),
							  has_br_FE = c(1),
							  n_K = c(4),
							  model_description = c("M0: FE, colnum"),
							  beta_info = list(tibble(coef_name = c("FE_b2", "FE_b3", "FE_b4", "FE_b5", 
							  									  "colnum_1", "colnum_2", "colnum_3", "colnum_4"),
							  						coef_beta_index = c(1:8))))
	
	df_choice_K <- df_choice_K %>% 
		add_row(M_in_paper = c("M1"),
				do_for_SNTH_FS = c(0),
				do_for_SNTH_K12fold = c(1),
				has_br_FE = c(1),
				n_K = c(5),
				model_description = c("M1: FE, own, colnum"),
				beta_info = list(tibble(coef_name = c("FE_b2", "FE_b3", "FE_b4", "FE_b5", 
													  "br_own",
													  "colnum_1", "colnum_2", "colnum_3", "colnum_4"),
										coef_beta_index = c(1:9))))
	
	df_choice_K <- df_choice_K %>% 
		add_row(M_in_paper = c("M2"),
				do_for_SNTH_FS = c(0),
				do_for_SNTH_K12fold = c(1),
				has_br_FE = c(1),
				n_K = c(6),
				model_description = c("M2: FE, own, know, colnum"),
				beta_info = list(tibble(coef_name = c("FE_b2", "FE_b3", "FE_b4", "FE_b5", 
													  "br_own", "br_know",
													  "colnum_1", "colnum_2", "colnum_3", "colnum_4"),
										coef_beta_index = c(1:10))))
	
	df_choice_K <- df_choice_K %>% 
		add_row(M_in_paper = c("M3"),
				do_for_SNTH_FS = c(0),
				do_for_SNTH_K12fold = c(1),
				has_br_FE = c(0),
				n_K = c(12),
				model_description = c("M3: theta_ilq (4 types of attention x 3 growth factors)"),
				beta_info = list(tibble(coef_name = c("nfix_int", "nfix_ls", "nfix_qs",
													  "bsac_int", "bsac_ls", "bsac_qs",
													  "asac_int", "asac_ls", "asac_qs",
													  "nsac_int", "nsac_ls", "nsac_qs"),
										coef_beta_index = c(1:12))))
	
	df_choice_K <- df_choice_K %>% 
		add_row(M_in_paper = c("PM"),
				do_for_SNTH_FS = c(1),
				do_for_SNTH_K12fold = c(1),
				has_br_FE = c(1),
				n_K = c(18),
				model_description = c("PM: FE, own, know, theta_ilq (G x 3 growth factors), colnum"),
				beta_info = list(tibble(coef_name = c("FE_b2", "FE_b3", "FE_b4", "FE_b5", 
													  "br_own", "br_know",
													  "nfix_int", "nfix_ls", "nfix_qs",
													  "bsac_int", "bsac_ls", "bsac_qs",
													  "asac_int", "asac_ls", "asac_qs",
													  "nsac_int", "nsac_ls", "nsac_qs",
													  "colnum_1", "colnum_2", "colnum_3", "colnum_4"),
										coef_beta_index = c(1:22))))
	
	df_choice_K <- df_choice_K %>% 
		mutate(reference_br_FE = 1)
	
	# check if the model numbers are unique
	if(nrow(df_choice_K) != nrow(distinct(df_choice_K %>% select(M_in_paper)))) {
		cat("There are duplicate model numbers")
		df_choice_K <- "One big error"
	}
	
	return(df_choice_K)
}

f_arrange_X_choice_perModel <- function(B, 
										df_part_info, 
										df_obs_em, 
										ar_theta_jigk, 
										choice_model, 
										df_brand_number,
										CPLX_colnum_brandname) {
	# this function doesn't have additional controls for out of sample predictions
	# that's because when calling this function, I make sure I input only data & parameters that are
	# available at the Q when I make the predictions
	# this means that df_obs_em contains eye movements only from quarters that I've observed
	# same for ar_theta_jigk - it contains theta_jigk based on eye movements from observed quarters
	
	# I need to take the participant ID into account when using the est or pred subsamples
	df_part_info <- df_part_info %>% 
		arrange(participant)
	part_IDs <- df_part_info[["participant"]]
	N   <- length(part_IDs)
	
	df_part_info <- f_create_brandnum_chosen(df_part = df_part_info, 
											 df_brand_number = df_brand_number)
	df_part_info <- f_create_brandnum_owned(df_part = df_part_info, 
											df_brand_number = df_brand_number)
	df_part_info <- df_part_info %>% arrange(participant)
	
	df_brand_number <- df_brand_number %>% 
		select(brandletter, brandnum)
	if(!(sum((colnames(df_obs_em) == "brandnum")*1))) {
		df_obs_em <- df_obs_em %>% 
			left_join(df_brand_number, 
					  by = "brandletter")	
	}
	
	brand_know <- f_create_brandnum_know(df_part = df_part_info, 
										 df_brand_number = df_brand_number)
	
	chosen_brand <- array(0, dim = c(N))
	for(j in 1:N) {
		chosen_brand[j] <- as.numeric(df_part_info %>% 
									  	filter(participant == part_IDs[j]) %>% 
									  	select(brandnum_chosen))
	}
	
	# number of explanatory variables other than brand FE
	df_choice_K <- f_create_df_models()
	all_models <- df_choice_K[["M_in_paper"]]
	
	if(sum((all_models == choice_model)*1) == 0) {
		print("you've asked for a model that is not specified")
		print("you've only specified these models:")
		print(df_choice_K)
	} else {
		print("the model you've asked is specified")
		print(df_choice_K %>% 
			  	filter(M_in_paper == choice_model))
		
		choice_K <- as.numeric(df_choice_K %>% 
							   	filter(M_in_paper == choice_model) %>% 
							   	select(n_K))
		# X_M holds all explanatory variables that are not brand FE
		if(choice_K) {
			X_M <- array(0, dim = c(N, B, choice_K))   
		}
		
		has_brand_FE <- as.numeric(df_choice_K %>% 
								   	filter(M_in_paper == choice_model) %>% 
								   	select(has_br_FE))
		if(has_brand_FE) {
			# if the model_number includes brand FE, then 
			# prepare the matrix to be added at the end, 
			# after X_M (explanatory vars other than FE) is created
			ref_br_FE <- as.numeric(df_choice_K %>% 
										filter(M_in_paper == choice_model) %>% 
										select(reference_br_FE))
			X_M_FE <- array(0, dim = c(N, B, B-1))
			for(j in 1:N) {
				X_M_FE[j, -ref_br_FE, ] <- diag(B-1)
			}
		}
		
		# Start with models that have brand ownership (it doesn't matter if they include brand FE)
		# For these models, brand ownership is the first explanatory variable
		models_w_own <- c("M1", "M2", "PM")
		if(sum((choice_model == models_w_own)*1)) {
			for(j in 1:N) {
				aux_brandnum_owned <- as.numeric(df_part_info %>% 
												 	filter(participant == part_IDs[j]) %>% 
												 	select(brandnum_owned))
				if(aux_brandnum_owned > 0) {
					X_M[j, aux_brandnum_owned, 1] <- 1
				}
			}
		}
		
		# out of these models that have brand ownership, some models also have brand knowledge
		# for models with brand ownership and brand knowledge, knowledge is the second variable
		models_w_own_know <- c("M2", "PM")
		if(sum((choice_model == models_w_own_know)*1)) {
			for(j in 1:N) {
				for(i in 1:B) {
					aux_know <- as.numeric(brand_know %>% 
										   	filter(participant == part_IDs[j], 
										   		   brandnum == i) %>% 
										   	select(brand_knowledge))
					X_M[j, i, 2] <- aux_know
				}
				X_M[j, , 2] <- X_M[j, , 2] - mean(X_M[j, , 2])
			}
		}
		
		# add theta_ilq for all G eye movements, for models that also have brand ownership and brand knowledge, so starting from position 3
		if(choice_model %in% c("PM")) {
			# PM: FE, own, know, theta_ilq (G x 3 growth factors), colnum
			
			for(j in 1:N) {
				for(i in 1:B) {
					X_M[j, i, 3]  <- ar_theta_jigk[j, i, 1, 1]
					X_M[j, i, 4]  <- ar_theta_jigk[j, i, 1, 2]
					X_M[j, i, 5]  <- ar_theta_jigk[j, i, 1, 3]
					
					X_M[j, i, 6]  <- ar_theta_jigk[j, i, 2, 1]
					X_M[j, i, 7]  <- ar_theta_jigk[j, i, 2, 2]
					X_M[j, i, 8]  <- ar_theta_jigk[j, i, 2, 3]
					
					X_M[j, i, 9]  <- ar_theta_jigk[j, i, 3, 1]
					X_M[j, i, 10] <- ar_theta_jigk[j, i, 3, 2]
					X_M[j, i, 11] <- ar_theta_jigk[j, i, 3, 3]
					
					X_M[j, i, 12] <- ar_theta_jigk[j, i, 4, 1]
					X_M[j, i, 13] <- ar_theta_jigk[j, i, 4, 2]
					X_M[j, i, 14] <- ar_theta_jigk[j, i, 4, 3]
				}
			}
		}
		
		# add theta_ilq for all G eye movements, 
		# for models WITHOUT brand ownership and brand knowledge, 
		# so starting from position 1
		if(choice_model %in% c("M3")) {
			# M3: theta_ilq (4 types of attention x 3 growth factors)
			
			for(j in 1:N) {
				for(i in 1:B) {
					X_M[j, i, 1]  <- ar_theta_jigk[j, i, 1, 1]
					X_M[j, i, 2]  <- ar_theta_jigk[j, i, 1, 2]
					X_M[j, i, 3]  <- ar_theta_jigk[j, i, 1, 3]
					
					X_M[j, i, 4]  <- ar_theta_jigk[j, i, 2, 1]
					X_M[j, i, 5]  <- ar_theta_jigk[j, i, 2, 2]
					X_M[j, i, 6]  <- ar_theta_jigk[j, i, 2, 3]
					
					X_M[j, i, 7]  <- ar_theta_jigk[j, i, 3, 1]
					X_M[j, i, 8]  <- ar_theta_jigk[j, i, 3, 2]
					X_M[j, i, 9]  <- ar_theta_jigk[j, i, 3, 3]
					
					X_M[j, i, 10] <- ar_theta_jigk[j, i, 4, 1]
					X_M[j, i, 11] <- ar_theta_jigk[j, i, 4, 2]
					X_M[j, i, 12] <- ar_theta_jigk[j, i, 4, 3]
				}
			}
		}
		
		# add the column dummies that mark where each brand was shown on display.
		# column 5 is reference
		# the start point is different, based on how many other variables enter the model
		if(choice_model %in% c("M0", "M1", "M2", "PM")) {
			# all these models include colnum, so it makes sense to have nested ifs
			
			# get the brand position on screen for each participant
			aux_df_brandpos <- f_create_brandpos_on_display(df_part = df_part_info,
															df_brand_number = df_brand_number,
															df_CPLX_colnum_brandname = CPLX_colnum_brandname)
			# the last variable is at this position in X_M
			if(choice_model %in% c("M0")) {
				last_var_at <- 0    
			}
			
			if(choice_model %in% c("M1")) {
				last_var_at <- 1
			}
			
			if(choice_model %in% c("M2")) {
				last_var_at <- 2    
			}
			
			if(choice_model %in% c("PM")) {
				last_var_at <- 14
			}
			
			for(j in 1:N) {
				# brand position in column on display
				# dummy for brand being presented in column 1, 2, 3, or 4
				for(i in 1:B) {
					brandnum_i_is_shown_in_column <- as.numeric(aux_df_brandpos %>% 
																	filter(participant == part_IDs[j], 
																		   brandnum == i) %>% 
																	select(colnum))
					# column 5 is reference
					if(brandnum_i_is_shown_in_column < 5) {
						X_M[j, i, last_var_at + brandnum_i_is_shown_in_column] <- 1    
					}
				}
			}
		}
		
		if(has_brand_FE) {
			if(choice_K) {
				X_M_complete <- array(0, dim = c(N, B, B-1 + choice_K))
				for(j in 1:N) {
					for(i in 1:B) {
						X_M_complete[j, i, 1:(B-1)] <- X_M_FE[j, i, ]
						X_M_complete[j, i, B:(B-1 + choice_K)] <- X_M[j, i, ]
					}
				}    
			} else {
				X_M_complete <- X_M_FE
			}
			
		} else {
			X_M_complete <- X_M
		}
		
		return(list(chosen_brand = chosen_brand,
					X_M = X_M_complete,
					choice_K = choice_K,
					n_par_choice = dim(X_M_complete)[3]))
	}
}


f_make_stan_data <- function(fullsample_Kfold, 
							 path_to_input_procdata,
							 path_to_theta_jigk,
							 do_choice_model) {
	
	load(path_to_input_procdata)
	load(path_to_theta_jigk)
	
	if(fullsample_Kfold == "fullsample") {
		aux_list <- f_arrange_X_choice_perModel(B = B, 
												df_part_info = participant_info, 
												df_obs_em = obs_eye_movements, 
												ar_theta_jigk = theta_jigk, 
												choice_model = do_choice_model,
												df_brand_number = brand_number,
												CPLX_colnum_brandname = CPLX_colnum_brandname)
		
		stan_data_choice <- list(B = B, 
								 n_par_choice = aux_list$n_par_choice,
								 est_N = N, 
								 est_y = as.vector(aux_list$chosen_brand), 
								 est_X = aux_list$X_M)
	} else {
		if(fullsample_Kfold == "K12fold") {
			aux_list <- f_arrange_X_choice_perModel(B = B, 
													df_part_info = est_participant_info, 
													df_obs_em = est_obs_eye_movements, 
													ar_theta_jigk = theta_jigk, 
													choice_model = do_choice_model,
													df_brand_number = brand_number,
													CPLX_colnum_brandname = CPLX_colnum_brandname)
			
			stan_data_choice <- list(B = B, 
									 n_par_choice = aux_list$n_par_choice,
									 est_N = est_N, 
									 est_y = as.vector(aux_list$chosen_brand), 
									 est_X = aux_list$X_M)
		}
	}    
	
	return(stan_data_choice)
}

f_estimate_choice <- function(fullsample_Kfold, 
							  path_to_input_procdata,
							  path_to_theta_jigk,
							  niter,
							  do_choice_model) {
	
	stan_data_choice <- f_make_stan_data(fullsample_Kfold = fullsample_Kfold, 
										 path_to_input_procdata = path_to_input_procdata,
										 path_to_theta_jigk = path_to_theta_jigk,
										 do_choice_model = do_choice_model)
	
	compiled_model <- stan_model("analysis/est_choice.stan")
	stan_choice_samples <- sampling(compiled_model, 
									data = stan_data_choice,
									chains = 2, 
									iter = niter, 
									seed = 25,
									save_warmup = FALSE)
	
	return(list(stan_choice_samples = stan_choice_samples,
				stan_data_choice = stan_data_choice))
}
