f_create_att_stan_data <- function() {
	att_K <- 3    # intercept, linear, quadratic
	X_QK  <- matrix(c(1, 0, 0,
					  1, 1, 1,
					  1, 2, 4,
					  1, 3, 9), nrow = 4, byrow = TRUE)
	return(list(att_K = att_K,
				X_QK  = X_QK))
}

f_create_att_stan_data_list <- function(input_procdata, FS_or_Kfold) {
	load(input_procdata)
	
	if(FS_or_Kfold == "K12fold") {
		N <- est_N
		participant_info  <- est_participant_info
		obs_eye_movements <- est_obs_eye_movements
	}
	
	att_K <- (f_create_att_stan_data())$att_K
	X_QK  <- (f_create_att_stan_data())$X_QK
	
	X_0j <- f_create_att_X_0j(df_participant_info = participant_info)
	X_ij <- f_create_att_X_ij(df_participant_info = participant_info, 
							  B = B, 
							  df_brand_number = brand_number, 
							  df_CPLX_colnum_brandname = CPLX_colnum_brandname)
	
	stan_data <- list(est_N = N,
					  B = B,
					  G = G,
					  Q = Q,
					  att_K = att_K,
					  att_K_0j = dim(X_0j)[2],
					  att_K_ij = dim(X_ij)[3],
					  X_QK = X_QK,
					  est_X_0j = X_0j,
					  est_X_ij = X_ij,
					  est_obs_y = f_create_lnobs_eyemovements_jigt(N, 
					  											 df_obs_EM = obs_eye_movements, 
					  											 B, 
					  											 G, 
					  											 Q, 
					  											 df_brand_number = brand_number))
	
	return(stan_data)
}

f_create_att_X_0j <- function(df_participant_info) {
	# 3 = overall mean (1), complexity (1), SP_owner (1), 
	# 4 = category knowledge (1), gender (1), age (1), education (1),
	# 2 = income_noanswer, income_level
	att_K_0j  <- 3 + 4 + 2
	
	df_participant_info <- f_create_categories_CPLX(df_participant_info)
	df_participant_info <- f_create_categories_gender(df_participant_info)
	df_participant_info <- f_create_categories_age(df_participant_info)
	df_participant_info <- f_create_categories_education(df_participant_info)
	df_participant_info <- f_create_categories_income(df_participant_info)
	
	print("att_K_0j is 9:")
	cat("\n")
	print("       1: overall mean")
	cat("\n")
	print("       2: CPLX -1(low), 0(medium), 1(high)")
	cat("\n")
	print("       3: smartphone_owner dummy")
	cat("\n")
	print("       4: know_smartphones 1 to 7 scale")
	cat("\n")
	print("       5: gender (1 = Female)")
	cat("\n")
	print("       6: age")
	print(distinct(df_participant_info %>% select(age, age_ordinal) %>% arrange(age_ordinal)))
	cat("\n")
	print("       7: education")
	print(distinct(df_participant_info %>% select(education_group, education_ordinal) %>% arrange(education_ordinal)))
	cat("\n")
	print("       8: did not disclose income")
	cat("\n")
	print("       9: income bracket")
	print(distinct(df_participant_info %>% select(HH_income_2012, inc_noanswer, inc_level)))
	
	part_IDs <- as.vector(df_participant_info$participant)
	N    <- length(part_IDs)
	X_0j <- array(0, dim = c(N, att_K_0j))
	
	for(j in 1:N) {
		X_0j[j, 1] <- 1
		X_0j[j, 2] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(IC_j))
		X_0j[j, 3] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(smartphone_owner))
		# category knowledge
		X_0j[j, 4] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(know_smartphones))
		# gender (Female = 1, Male = 0)
		X_0j[j, 5] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(gender_dummy))
		# age
		X_0j[j, 6] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(age_ordinal))
		# education 
		X_0j[j, 7] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(education_ordinal))
		# income: did not disclose
		X_0j[j, 8] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(inc_noanswer))
		# income: from -4 to 3, 0 for "no answer" and "50-75K"
		X_0j[j, 9] <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(inc_level))
	}
	
	# I can standardize and/or mean center some of the variables
	# SP_knowledge
	summary(X_0j[, 4])
	X_0j[, 4] <- X_0j[, 4] - mean(X_0j[, 4])
	summary(X_0j[, 4])
	
	return(X_0j)
}

f_create_categories_CPLX <- function(df_data) {
	# first, I should check if "IC_j" already exists in df_data
	# because if it does, then the join would add IC_j.x and .y, which leads to errors
	if(sum((colnames(df_data) == "IC_j")*1)) {
		df_data <- df_data %>% select(-IC_j)
	}
	
	df_CPLX <- data.frame(counterbalanceid = c(paste("low", 1:5, sep =""), 
											   paste("medium", 1:5, sep =""), 
											   paste("high", 1:5, sep ="")),
						  IC_j = c(rep(-1, 5), 
						  		 rep(0, 5), 
						  		 rep(1, 5)),
						  stringsAsFactors = FALSE)
	
	df_data <- df_data %>% 
		left_join(df_CPLX, by = "counterbalanceid")
	
	return(df_data)
}

f_create_categories_gender <- function(df_data) {
	# first, I should check if "gender_dummy" already exists in df_data
	# because if it does, then the join would add gender_dummy.x and .y, which leads to errors
	if(sum((colnames(df_data) == "gender_dummy")*1)) {
		df_data <- df_data %>% select(-gender_dummy)
	}
	
	df_gender <- data.frame(gender = c("Male", "Female"),
							gender_dummy = c(0, 1),
							stringsAsFactors = FALSE)
	
	df_data <- df_data %>% left_join(df_gender, by = "gender")
	
	return(df_data)
}

f_create_categories_age <- function(df_data) {
	# first, I should check if "age_ordinal" already exists in df_data
	# because if it does, then the join would add age_ordinal.x and .y, which leads to errors
	if(sum((colnames(df_data) == "age_ordinal")*1)) {
		df_data <- df_data %>% select(-age_ordinal)
	}
	
	df_age <- data.frame(age = c("18-29",
								 "30-49",
								 "50-64",
								 "65+"),
						 age_ordinal = c(0, 1, 2, 3),
						 stringsAsFactors = FALSE)
	
	df_data <- df_data %>% left_join(df_age, by = "age")
	
	return(df_data)
}

f_create_categories_education <- function(df_part) {
	# first, I should check if "education_ordinal" already exists in df_part
	# because if it does, then the join would add education_ordinal.x and .y, which leads to errors
	if(sum((colnames(df_part) == "education_ordinal")*1)) {
		df_part <- df_part %>% select(-education_ordinal)
	}
	
	df_education <- data.frame(education_group = c("graduate", "college", "associate", "highschool"),
							   education_ordinal = c(3, 2, 1, 0),
							   stringsAsFactors = FALSE)
	df_part <- df_part %>% left_join(df_education, by = "education_group")
	
	return(df_part)
}

f_create_categories_income <- function(df_part) {
	# first, I should check if "inc_noanswer" already exists in df_part
	# because if it does, then the join would add inc_noanswer.x and .y, which leads to errors
	if(sum((colnames(df_part) == "inc_noanswer")*1)) {
		df_part <- df_part %>% select(-inc_noanswer)
	}
	
	if(sum((colnames(df_part) == "inc_level")*1)) {
		df_part <- df_part %>% select(-inc_level)
	}
	
	# there are 9 categories
	df_income <- data.frame(HH_income_2012 = c("$14,999 or less", 
											   "$15,000 to $24,999",
											   "$25,000 to $34,999", 
											   "$35,000 to $49,999", 
											   "$50,000 to $74,999", 
											   "$75,000 to $99,999", 
											   "$100,000 to $149,999", 
											   "$150,000 or more", 
											   "I prefer not to answer"),
							inc_noanswer = c(rep(0, 8), 1),
							inc_level = c(-4, -3, -2, -1, 0, 1, 2, 3, 0),
							stringsAsFactors = FALSE)
	
	df_part <- df_part %>% left_join(df_income, by = "HH_income_2012")
	
	return(df_part)
}

f_create_att_X_ij <- function(df_participant_info, B, df_brand_number, df_CPLX_colnum_brandname) {
	# B-1 = brand dummies (FE, brand 1 as reference)
	# 1 = brand_owned (if the participant owns a smartphone from one of the brands on screen), 
	# 4 = brand position in column on display (4, column 5 as reference)
	# 1 = brand knowledge
	att_K_ij  <- B-1 + 1 + 4 + 1    
	
	print(paste("att_K_ij is: ", att_K_ij, sep = ""))
	cat("\n")
	print("       1-4: brand fixed effects, brand 1 as reference")
	cat("\n")
	print("       5: brand ownership dummy")
	cat("\n")
	print("       6-9: column fixed effects, column 5 as reference")
	cat("\n")
	print("       10: know_brandnum 1 to 7 scale")
	
	
	df_participant_info <- f_create_brandnum_owned(df_part = df_participant_info, df_brand_number = df_brand_number)
	brandpos_on_display <- f_create_brandpos_on_display(df_part = df_participant_info, 
														df_brand_number = df_brand_number, 
														df_CPLX_colnum_brandname = df_CPLX_colnum_brandname)
	brand_know <- f_create_brandnum_know(df_part = df_participant_info, df_brand_number = df_brand_number)
	
	part_IDs <- as.vector(df_participant_info$participant)
	N    <- length(part_IDs)
	X_ij <- array(0, dim = c(N, B, att_K_ij))
	
	for(j in 1:N) {
		# brand 1 is reference brand, so I add dummies for brands 2 to B
		for(i in 2:B) {
			X_ij[j, i, i-1] <- 1
		}
		
		aux_brandnum_owned <- as.numeric(df_participant_info %>% filter(participant == part_IDs[j]) %>% select(brandnum_owned))
		if(aux_brandnum_owned > 0) {
			X_ij[j, aux_brandnum_owned, 5] <- 1
		}
		
		# brand position in column on display
		# dummy for brand being presented in column 1, 2, 3, or 4
		for(i in 1:B) {
			brandnum_i_is_shown_in_column <- as.numeric(brandpos_on_display %>% filter(participant == part_IDs[j], brandnum == i) %>% select(colnum))
			# column 5 is reference
			if(brandnum_i_is_shown_in_column < 5) {
				X_ij[j, i, 5 + brandnum_i_is_shown_in_column] <- 1    
			}
		}
		
		# brand knowledge
		for(i in 1:B) {
			aux_know <- as.numeric(brand_know %>% filter(participant == part_IDs[j], brandnum == i) %>% select(brand_knowledge))
			X_ij[j, i, att_K_ij] <- aux_know
		}
	}
	
	# brand knowledge
	# to standardize, I could do it at the brand level: (BK_ji - (BK_0i))/sd(BK_0i)
	# or I could mean center at the participant level: BK_ji - BK_j0
	# or do it over all brands and participants: (BK_ji - BK_00)/sd(BK_00)
	# I'll go for participant level, because this makes it more similar to the other variables (brand ownership)
	for(j in 1:N) {
		X_ij[j, , att_K_ij] <- X_ij[j, , att_K_ij] - mean(X_ij[j, , att_K_ij])
	}
	
	return(X_ij)
}

f_create_brandnum_owned <- function(df_part, df_brand_number) {
	# first, I should check if "brandnum_owned" already exists in df_part
	# because if it does, then the join would add brandnum_owned .x and .y, which leads to errors
	if(sum((colnames(df_part) == "brandnum_owned")*1)) {
		df_part <- df_part %>% select(-brandnum_owned)
	}
	
	# make sure I keep only the columns I need
	df_brand_number  <- df_brand_number %>% select(brandletter, brandnum)
	
	df_part <- df_part %>% left_join(df_brand_number, by = c("brandletter_owned" = "brandletter"))
	df_part$brandnum_owned <- df_part$brandnum
	df_part$brandnum_owned <- replace_na(df_part$brandnum_owned, 0)
	df_part <- df_part %>% select(-brandnum)
	
	
	return(df_part)
}

f_create_brandnum_chosen <- function(df_part, df_brand_number) {
	# first, I should check if "brandnum_chosen" already exists in df_part
	# because if it does, then the join would add brandnum_chosen .x and .y, which leads to errors
	if(sum((colnames(df_part) == "brandnum_chosen")*1)) {
		df_part <- df_part %>% select(-brandnum_chosen)
	}
	
	# make sure I keep only the columns I need
	df_brand_number  <- df_brand_number %>% select(brandletter, brandnum)
	
	df_part <- df_part %>% left_join(df_brand_number, by = c("brandletter_chosen" = "brandletter"))
	df_part$brandnum_chosen <- df_part$brandnum
	df_part <- df_part %>% select(-brandnum)
	
	return(df_part)
}

f_create_brandpos_on_display <- function(df_part, df_brand_number, df_CPLX_colnum_brandname) {
	# make sure I keep only the columns I need
	brandpos_on_display <- df_part %>% select(participant, counterbalanceid)
	brandpos_on_display <- brandpos_on_display %>% left_join(df_CPLX_colnum_brandname, by = "counterbalanceid")
	brandpos_on_display <- brandpos_on_display %>% left_join(df_brand_number, by = "brandletter")
	brandpos_on_display <- brandpos_on_display %>% select(participant, brandnum, brandcol_number)
	colnames(brandpos_on_display) <- c("participant", "brandnum", "colnum")
	
	return(brandpos_on_display)
}

f_create_brandnum_know <- function(df_part, df_brand_number) {
	# make sure I keep only the columns I need
	df_brandnum_know <- df_part %>% select(participant, know_A, know_B, know_C, know_D, know_E)
	df_brand_number  <- df_brand_number %>% select(brandletter, brandnum)
	
	df_brandnum_know <- df_brandnum_know %>% pivot_longer(-participant, names_to = "brandletter", values_to = "brand_knowledge")
	df_brandnum_know$brandletter <- str_remove(df_brandnum_know$brandletter, "know_")
	df_brandnum_know <- df_brandnum_know %>% left_join(df_brand_number, by = "brandletter")
	df_brandnum_know <- df_brandnum_know %>% select(participant, brandnum, brand_knowledge)
	
	return(df_brandnum_know)
}

f_create_lnobs_eyemovements_jigt <- function(N, df_obs_EM, B, G, Q, df_brand_number) {
	obs_eyemovements_jigt <- array(0, dim = c(N, B, G, Q))
	
	part_IDs <- distinct(df_obs_EM %>% select(participant))
	part_IDs <- part_IDs$participant
	# sorting is needed for when I have Kfold
	part_IDs <- sort(part_IDs)
	df_brand_number  <- df_brand_number %>% select(brandletter, brandnum)
	df_obs_EM <- df_obs_EM %>% left_join(df_brand_number, by = "brandletter")
	
	for(j in 1:N) {
		for(i in 1:B) {
			for(q in 1:Q) {
				obs_eyemovements_jigt[j, i, 1, q] <- as.numeric(df_obs_EM %>% 
																	filter(participant == part_IDs[j], 
																		   brandnum == i,
																		   quarter == q,
																		   EM_name == "fix") %>% select(EM_count))
				
				obs_eyemovements_jigt[j, i, 2, q] <- as.numeric(df_obs_EM %>% 
																	filter(participant == part_IDs[j], 
																		   brandnum == i,
																		   quarter == q,
																		   EM_name == "bsac") %>% select(EM_count))
				
				obs_eyemovements_jigt[j, i, 3, q] <- as.numeric(df_obs_EM %>% 
																	filter(participant == part_IDs[j], 
																		   brandnum == i,
																		   quarter == q,
																		   EM_name == "asac") %>% select(EM_count))
				
				obs_eyemovements_jigt[j, i, 4, q] <- as.numeric(df_obs_EM %>% 
																	filter(participant == part_IDs[j], 
																		   brandnum == i,
																		   quarter == q,
																		   EM_name == "nsac") %>% select(EM_count))
			}
		}
	}
	
	lnobs_eyemovements_jigt <- log(obs_eyemovements_jigt + 1)
	
	return(lnobs_eyemovements_jigt)
}

