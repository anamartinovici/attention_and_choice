args   <- commandArgs(TRUE)
n_args <- length(args)

if(n_args == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	FS_or_K12fold <- args[1]
	output_dir    <- args[2]
}

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(FS_or_K12fold == "fullsample") {
	input_fit_attention <- here::here(output_dir, FS_or_K12fold, "est_att", "SNTH_est_att.RData")
	output_filename     <- "SNTH_betadraws_att"
} else {
	K12folds <- c("wo_brandA_low", "wo_brandA_medium", "wo_brandA_high",
				  "wo_brandB_low", "wo_brandB_medium", "wo_brandB_high",
				  "wo_other_low",  "wo_other_medium",  "wo_other_high",
				  "wo_nonSP_low",  "wo_nonSP_medium",  "wo_nonSP_high")
	input_fit_attention <- character(length = length(K12folds))
	output_filename     <- character(length = length(K12folds))
	for(fold in 1:length(K12folds)) {
		input_fit_attention[fold] <- here::here(output_dir, FS_or_K12fold, 
												"est_att", 
												paste0("SNTH_est_att_", K12folds[fold],  ".RData"))
		output_filename[fold] <- paste0("SNTH_betadraws_att_", K12folds[fold])
	}
}

if(!dir.exists(here::here(output_dir, FS_or_K12fold, "est_betatheta"))) {
	dir.create(here::here(output_dir, FS_or_K12fold, "est_betatheta"),
			   recursive = TRUE)
}

for(input_fit in input_fit_attention) {
	load(input_fit)
	
	draws_beta_fix_0j <- rstan::extract(fit_attention, pars = c("beta_fix_0j"))[[1]]
	draws_beta_fix_ij <- rstan::extract(fit_attention, pars = c("beta_fix_ij"))[[1]]
	
	sd_y_fix_0j_draws <- rstan::extract(fit_attention, pars = c("sd_y_fix_0j"))[[1]]
	sd_y_fix_ij_draws <- rstan::extract(fit_attention, pars = c("sd_y_fix_ij"))[[1]]
	
	draws_var_theta_fix_0j <- rstan::extract(fit_attention, pars = c("var_theta_fix_0j"))[[1]]
	draws_var_theta_fix_ij <- rstan::extract(fit_attention, pars = c("var_theta_fix_ij"))[[1]]
	
	draws_beta_sac_0j <- rstan::extract(fit_attention, pars = c("beta_sac_0j"))[[1]]
	draws_beta_sac_ij <- rstan::extract(fit_attention, pars = c("beta_sac_ij"))[[1]]
	
	var_y_sac_Q1_0j_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q1_0j"))[[1]]
	var_y_sac_Q2_0j_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q2_0j"))[[1]]
	var_y_sac_Q3_0j_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q3_0j"))[[1]]
	var_y_sac_Q4_0j_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q4_0j"))[[1]]
	
	var_y_sac_Q1_ij_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q1_ij"))[[1]]
	var_y_sac_Q2_ij_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q2_ij"))[[1]]
	var_y_sac_Q3_ij_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q3_ij"))[[1]]
	var_y_sac_Q4_ij_draws <- rstan::extract(fit_attention, pars = c("var_y_sac_Q4_ij"))[[1]]
	
	draws_var_theta_sac_0j <- rstan::extract(fit_attention, pars = c("var_theta_sac_0j"))[[1]]
	draws_var_theta_sac_ij <- rstan::extract(fit_attention, pars = c("var_theta_sac_ij"))[[1]]
	
	save(draws_beta_fix_0j, draws_beta_fix_ij,
		 sd_y_fix_0j_draws, sd_y_fix_ij_draws,
		 draws_var_theta_fix_0j, draws_var_theta_fix_ij,
		 draws_beta_sac_0j, draws_beta_sac_ij,
		 var_y_sac_Q1_0j_draws, var_y_sac_Q2_0j_draws, var_y_sac_Q3_0j_draws, var_y_sac_Q4_0j_draws,
		 var_y_sac_Q1_ij_draws, var_y_sac_Q2_ij_draws, var_y_sac_Q3_ij_draws, var_y_sac_Q4_ij_draws,
		 draws_var_theta_sac_0j, draws_var_theta_sac_ij,
		 file = here::here(output_dir, FS_or_K12fold,
		 				  "est_betatheta", 
		 				  paste0(output_filename[which(input_fit_attention == input_fit)], ".RData")))
}
