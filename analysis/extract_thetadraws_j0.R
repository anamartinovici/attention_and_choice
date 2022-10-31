args   <- commandArgs(TRUE)
n_args <- length(args)

if(n_args == 0) {
    stop("You need to provide arguments", call. = FALSE)
} else {
	output_dir <- args[1]
}

library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

input_fit_attention <- here::here(output_dir, "fullsample", "est_att", "SNTH_est_att.RData")

if(!dir.exists(here::here(output_dir, "fullsample", "est_betatheta"))) {
	dir.create(here::here(output_dir, "fullsample", "est_betatheta"))
}

load(input_fit_attention)

theta_fix_0j_draws <- rstan::extract(fit_attention, pars = c("theta_fix_0j"))[[1]]
theta_sac_0j_draws <- rstan::extract(fit_attention, pars = c("theta_sac_0j"))[[1]]

save(theta_fix_0j_draws, theta_sac_0j_draws,
	 file = here::here(output_dir, "fullsample",
	 				  "est_betatheta", 
	 				  "SNTH_thetadraws_j0_att.RData"))

# dim(theta_sac_0j) is 9 x N_part
# 9 corresponds to: 
#		int_bsac, int_asac, int_nsac, 
#		ls_bsac, ls_asac, ls_nsac, 
#		qs_bsac, qs_asac, qs_nsac

theta_fix_0j <- apply(theta_fix_0j_draws, c(2:3), mean)
theta_sac_0j <- apply(theta_sac_0j_draws, c(2:3), mean)

att_K  <- dim(theta_fix_0j)[1]
N_part <- dim(theta_fix_0j)[2]
G      <- dim(theta_sac_0j)[1] / att_K + 1

theta_j0gk <- array(0, dim = c(N_part, G, att_K))
for (j in 1:N_part) {
	theta_j0gk[j, 1, ] <- theta_fix_0j[, j]
	
	theta_j0gk[j, 2, 1] <- theta_sac_0j[1, j]
	theta_j0gk[j, 2, 2] <- theta_sac_0j[4, j]
	theta_j0gk[j, 2, 3] <- theta_sac_0j[7, j]
	
	theta_j0gk[j, 3, 1] <- theta_sac_0j[2, j]
	theta_j0gk[j, 3, 2] <- theta_sac_0j[5, j]
	theta_j0gk[j, 3, 3] <- theta_sac_0j[8, j]
	
	theta_j0gk[j, 4, 1] <- theta_sac_0j[3, j]
	theta_j0gk[j, 4, 2] <- theta_sac_0j[6, j]
	theta_j0gk[j, 4, 3] <- theta_sac_0j[9, j]
}

save(theta_j0gk,
	 file = here::here(output_dir, "fullsample",
	 				  "est_betatheta", 
	 				  "SNTH_postmean_theta_j0gk_att.RData"))

