args <- commandArgs(TRUE)

if (length(args)==0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	FS_or_Kfold <- args[1]
	output_dir  <- args[2]
}

source(here::here("analysis", "est_att_aux.R"))
source(here::here("analysis", "est_choice_aux.R"))

library("tidyverse")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(!dir.exists(here::here(output_dir, FS_or_Kfold, "est_choice"))) {
	dir.create(here::here(output_dir, FS_or_Kfold, "est_choice"),
			   recursive = TRUE)
}

if(FS_or_Kfold == "fullsample") {
	path_to_input_procdata <- here::here("data", "SNTH_procdata.RData")
	path_to_theta_jigk     <- here::here(output_dir, FS_or_Kfold,
										 "est_betatheta",
										 "SNTH_postmean_theta_jigk_att.RData")
	
	df_choice_K <- f_create_df_models() %>%
		filter(do_for_SNTH_FS == 1) %>%
		select(M_in_paper)
	
	for(choice_model in df_choice_K[["M_in_paper"]]) {
		model_output <- f_estimate_choice(fullsample_Kfold = FS_or_Kfold, 
										  path_to_input_procdata = path_to_input_procdata,
										  path_to_theta_jigk = path_to_theta_jigk,
										  niter = 2000,
										  do_choice_model = choice_model)
		
		stan_choice_samples <- model_output[["stan_choice_samples"]]
		stan_data_choice    <- model_output[["stan_data_choice"]]
		
		save(stan_choice_samples, 
			 stan_data_choice, 
			 file = here::here(output_dir, FS_or_Kfold,
			 				  "est_choice",
			 				  paste0("SNTH_est_choice_", choice_model, ".RData")))	
	}
}

if(FS_or_Kfold == "K12fold") {
	df_choice_K <- f_create_df_models() %>%
		filter(do_for_SNTH_K12fold == 1) %>%
		select(M_in_paper)
	
	K12folds <- c("brandA_low", "brandA_medium", "brandA_high",
				  "brandB_low", "brandB_medium", "brandB_high",
				  "other_low",  "other_medium",  "other_high",
				  "nonSP_low",  "nonSP_medium",  "nonSP_high")
	
	for(choice_model in df_choice_K[["M_in_paper"]]) {
		if(!dir.exists(here::here(output_dir, FS_or_Kfold, "est_choice", choice_model))) {
			dir.create(here::here(output_dir, FS_or_Kfold, "est_choice", choice_model))
		}
		
		for(Kfold in K12folds) {
			path_to_input_procdata <- here::here(output_dir, FS_or_Kfold, 
												 "procdata", 
												 paste0("SNTH_procdata_pred_", Kfold, ".RData"))
			path_to_theta_jigk     <- here::here(output_dir, FS_or_Kfold,
												 "est_betatheta",
												 paste0("SNTH_postmean_theta_jigk_att_wo_", Kfold, ".RData"))
			
			model_output <- f_estimate_choice(fullsample_Kfold = FS_or_Kfold, 
											  path_to_input_procdata = path_to_input_procdata,
											  path_to_theta_jigk = path_to_theta_jigk,
											  niter = 2000,
											  do_choice_model = choice_model)
			
			stan_choice_samples <- model_output[["stan_choice_samples"]]
			stan_data_choice    <- model_output[["stan_data_choice"]]
			
			save(stan_choice_samples, 
				 stan_data_choice, 
				 file = here::here(output_dir, FS_or_Kfold,
				 				  "est_choice", choice_model,
				 				  paste0("SNTH_est_choice_", choice_model, "_wo_", Kfold, ".RData")))
		}
	}
}


