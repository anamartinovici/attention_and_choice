args   <- commandArgs(TRUE)
n_args <- length(args)

if(n_args == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	FS_or_Kfold <- args[1]
	output_dir  <- args[2]
}

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

if(FS_or_Kfold == "fullsample") {
	input_fit_attention <- here::here(output_dir, FS_or_Kfold, "est_att", "SNTH_est_att.RData")
} else {
	K12folds <- c("wo_brandA_low", "wo_brandA_medium", "wo_brandA_high",
				  "wo_brandB_low", "wo_brandB_medium", "wo_brandB_high",
				  "wo_other_low",  "wo_other_medium",  "wo_other_high",
				  "wo_nonSP_low",  "wo_nonSP_medium",  "wo_nonSP_high")
	input_fit_attention <- character(length = length(K12folds))
	for(fold in 1:length(K12folds)) {
		input_fit_attention[fold] <- here::here(output_dir, FS_or_Kfold, 
												"est_att", 
												paste0("SNTH_est_att_", K12folds[fold],  ".RData"))
	}
}

if(!dir.exists(here::here(output_dir, FS_or_Kfold, "est_betatheta"))) {
	dir.create(here::here(output_dir, FS_or_Kfold, "est_betatheta"),
			   recursive = TRUE)
}

for(input_fit in input_fit_attention) {
	load(input_fit)
	
	theta_fix_ij_draws <- rstan::extract(fit_attention, pars = c("theta_fix_ij"))[[1]]
	theta_sac_ij_draws <- rstan::extract(fit_attention, pars = c("theta_sac_ij"))[[1]]
	
	if(FS_or_Kfold == "fullsample") {
		save(theta_fix_ij_draws, theta_sac_ij_draws,
			 file = here::here(output_dir, FS_or_Kfold,
			 				  "est_betatheta", 
			 				  "SNTH_thetadraws_ji_att.RData"))
	}
	
	# dim(theta_sac_ij) is B x 9 x N_part
	# 9 corresponds to: 
	#		int_bsac, int_asac, int_nsac, 
	#		ls_bsac, ls_asac, ls_nsac, 
	#		qs_bsac, qs_asac, qs_nsac
	
	theta_fix_ij <- apply(theta_fix_ij_draws, c(2:4), mean)
	theta_sac_ij <- apply(theta_sac_ij_draws, c(2:4), mean)
	
	B      <- dim(theta_fix_ij)[1]
	att_K  <- dim(theta_fix_ij)[2]
	N_part <- dim(theta_fix_ij)[3]
	G      <- dim(theta_sac_ij)[2] / att_K + 1
	
	theta_jigk <- array(0, dim = c(N_part, B, G, att_K))
	for (j in 1:N_part) {
		for (b in 1:B) {
			theta_jigk[j, b, 1, 1] <- theta_fix_ij[b, 1, j]
			theta_jigk[j, b, 1, 2] <- theta_fix_ij[b, 2, j]
			theta_jigk[j, b, 1, 3] <- theta_fix_ij[b, 3, j]
			
			theta_jigk[j, b, 2, 1] <- theta_sac_ij[b, 1, j]
			theta_jigk[j, b, 2, 2] <- theta_sac_ij[b, 4, j]
			theta_jigk[j, b, 2, 3] <- theta_sac_ij[b, 7, j]
			
			theta_jigk[j, b, 3, 1] <- theta_sac_ij[b, 2, j]
			theta_jigk[j, b, 3, 2] <- theta_sac_ij[b, 5, j]
			theta_jigk[j, b, 3, 3] <- theta_sac_ij[b, 8, j]
			
			theta_jigk[j, b, 4, 1] <- theta_sac_ij[b, 3, j]
			theta_jigk[j, b, 4, 2] <- theta_sac_ij[b, 6, j]
			theta_jigk[j, b, 4, 3] <- theta_sac_ij[b, 9, j]
		}
	}
	
	if(FS_or_Kfold == "fullsample") {
		output_filename <- "SNTH_postmean_theta_jigk_att.RData"
	} else {
		output_filename <- paste0("SNTH_postmean_theta_jigk_att",
								  stringr::str_sub(input_fit, 
								  				 start = stringr::str_locate(input_fit, "_wo_")[, "start"]))
	}
	
	save(theta_jigk,
		 file = here::here(output_dir, FS_or_Kfold,
		 				  "est_betatheta", 
		 				  output_filename))
}

