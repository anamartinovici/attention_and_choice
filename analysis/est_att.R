args <- commandArgs(TRUE)

if(length(args) == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	FS_or_K12fold <- args[1]
	output_dir    <- args[2]
}

library("tidyverse")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source(here::here("analysis", "est_att_aux.R"))
attention <- stan_model(here::here("analysis", "est_att.stan"))
keep_parnames <- c("beta_fix_0j", "beta_fix_ij",
				   "sd_y_fix_0j", "sd_y_fix_ij",
				   "var_theta_fix_0j", "var_theta_fix_ij",
				   "beta_sac_0j", "beta_sac_ij", 
				   "var_y_sac_Q1_0j", "var_y_sac_Q2_0j", "var_y_sac_Q3_0j", "var_y_sac_Q4_0j",
				   "var_y_sac_Q1_ij", "var_y_sac_Q2_ij", "var_y_sac_Q3_ij", "var_y_sac_Q4_ij",
				   "var_theta_sac_0j", "var_theta_sac_ij", 
				   "theta_fix_0j", "theta_sac_0j", "theta_fix_ij", "theta_sac_ij")

if(FS_or_K12fold == "fullsample") {
	input_procdata <- here::here("data", "SNTH_procdata.RData")
} else {
	K12folds <- c("brandA_low", "brandA_medium", "brandA_high",
				  "brandB_low", "brandB_medium", "brandB_high",
				  "other_low",  "other_medium",  "other_high",
				  "nonSP_low",  "nonSP_medium",  "nonSP_high")
	
	input_procdata <- character(length = length(K12folds))
	for(Kfold in 1:length(K12folds)) {
		input_procdata[Kfold] <- here::here(output_dir, FS_or_K12fold, 
											"procdata", 
											paste0("SNTH_procdata_pred_", K12folds[Kfold],  ".RData"))
	}
}

if(!dir.exists(here::here(output_dir, FS_or_Kfold, "est_att"))) {
	dir.create(here::here(output_dir, FS_or_Kfold, "est_att"),
			   recursive = TRUE)
}

for(input_item in input_procdata) {
	stan_data <- f_create_att_stan_data_list(input_procdata = input_item, 
											 FS_or_Kfold = FS_or_K12fold)
	
	fit_attention <- sampling(attention, 
							  data = stan_data, 
							  iter = 50000, 
							  chains = 2, 
							  seed = 25, 
							  save_warmup = FALSE,
							  pars = keep_parnames)	
	
	if(FS_or_K12fold == "fullsample") {
		output_filename <- "SNTH_est_att.RData"
	} else {
		output_filename <- paste0("SNTH_est_att_wo", 
								  str_sub(input_item, 
								  		start = str_locate(input_item, "_pred_")[, "end"]))
	}
	
	save(fit_attention, 
		 file = here::here(output_dir, FS_or_K12fold, "est_att", output_filename))
}


