args <- commandArgs(TRUE)

if(length(args) == 0) {
	stop("You need to provide arguments", call.=FALSE)
} else {
	output_dir <- args[1]
}

set.seed(25)

library("tidyverse")

source("analysis/est_att_aux.R")

K12folds <- c("brandA_low", "brandA_medium", "brandA_high",
			  "brandB_low", "brandB_medium", "brandB_high",
			  "other_low",  "other_medium",  "other_high",
			  "nonSP_low",  "nonSP_medium",  "nonSP_high")

if(!dir.exists(here::here(output_dir, "K12fold", "pred_theta"))) {
	dir.create(here::here(output_dir, "K12fold", "pred_theta"),
			   recursive = TRUE)
}

att_K <- (f_create_att_stan_data())$att_K
X_QK  <- (f_create_att_stan_data())$X_QK

for(Kfold in K12folds) {
	load(here::here(output_dir, "K12fold", "procdata",
					paste0("SNTH_procdata_pred_", Kfold, ".RData")))
	load(here::here(output_dir, "K12fold", "est_betatheta",
					paste0("SNTH_betadraws_att_wo_", Kfold, ".RData")))
	
	est_X_0j  <- f_create_att_X_0j(df_participant_info = est_participant_info)
	pred_X_0j <- f_create_att_X_0j(df_participant_info = pred_participant_info)
	
	est_X_ij <- f_create_att_X_ij(df_participant_info = est_participant_info, 
								  B = B, 
								  df_brand_number = brand_number, 
								  df_CPLX_colnum_brandname = CPLX_colnum_brandname)
	pred_X_ij <- f_create_att_X_ij(df_participant_info = pred_participant_info, 
								   B = B, 
								   df_brand_number = brand_number, 
								   df_CPLX_colnum_brandname = CPLX_colnum_brandname)
	
	# these are the same for all quarter predictions
	att_K_0j   <- dim(est_X_0j)[2]
	att_K_ij   <- dim(est_X_ij)[3]
	pred_obs_y <- f_create_lnobs_eyemovements_jigt(pred_N, 
												   df_obs_EM = pred_obs_eye_movements, 
												   B, G, Q, 
												   df_brand_number = brand_number)
	post_draws <- dim(draws_beta_fix_0j)[1]
	
	mtm_pred_theta_jigkq <- array(0, dim = c(pred_N, B, G, att_K, Q))
	
	for(current_Q in 1:Q) {
		current_att_K   <- 3
		current_X_QK    <- X_QK[1:current_Q, 1:current_att_K, drop = FALSE]
		
		# fixation specific
		X_1_fix <- kronecker(cbind(1, diag(B)), current_X_QK)
		t_X_1_fix <- t(X_1_fix)
		X_2_fix <- kronecker(array(1, dim = c(B, 1)), diag(current_Q))
		t_X_2_fix <- t(X_2_fix)
		X_3_fix <- array(0, dim = c((1+B)*current_att_K, current_att_K*(att_K_0j + att_K_ij), pred_N))
		
		# saccades specific
		X_1_sac <- kronecker(cbind(1, diag(B)), kronecker(current_X_QK, diag(G-1)))
		t_X_1_sac <- t(X_1_sac)
		X_2_sac <- kronecker(array(1, dim = c(B, 1)), kronecker(diag(current_Q), diag(G-1)))
		t_X_2_sac <- t(X_2_sac)
		X_3_sac <- array(0, dim = c((1+B)*current_att_K*(G-1), current_att_K*((G-1)*att_K_0j + (G-1)*att_K_ij), pred_N))
		
		fix_start_col <- current_att_K*att_K_0j + 1
		fix_end_col   <- current_att_K*(att_K_0j + att_K_ij)
		
		sac_start_col <- current_att_K*(G-1)*att_K_0j + 1
		sac_end_col   <- current_att_K*((G-1)*att_K_0j + (G-1)*att_K_ij)
		
		for(j in 1:pred_N) {
			X_3_fix[1:current_att_K, 1:(current_att_K*att_K_0j), j] <- 
				kronecker(diag(current_att_K), array(pred_X_0j[j, ], dim = c(1, att_K_0j)))
			
			X_3_sac[1:(current_att_K*(G-1)), 1:(current_att_K*(G-1)*att_K_0j), j] <- 
				kronecker(diag(current_att_K*(G-1)), array(pred_X_0j[j, ], dim = c(1, att_K_0j)))
			
			for(i in 1:B) {
				start_row <- i*current_att_K + 1
				end_row   <- start_row + current_att_K - 1
				X_3_fix[start_row:end_row, fix_start_col:fix_end_col, j] <- 
					kronecker(diag(current_att_K), array(pred_X_ij[j, i, ], dim = c(1, att_K_ij)))
				
				start_row <- i*current_att_K*(G-1) + 1
				end_row   <- start_row + current_att_K*(G-1) - 1
				X_3_sac[start_row:end_row, sac_start_col:sac_end_col, j] <- 
					kronecker(diag(current_att_K*(G-1)), array(pred_X_ij[j, i, ], dim = c(1, att_K_ij)))
			}
		}
		
		pred_y_fix_long <- array(0, dim = c(B*current_Q, pred_N))
		pred_y_sac_long <- array(0, dim = c(B*current_Q*(G-1), pred_N))
		
		for(j in 1:pred_N) {
			for(i in 1:B) {
				for(q in 1:current_Q) {
					pred_y_fix_long[(i-1)*current_Q + q, j] <- pred_obs_y[j, i, 1, q]   
					for(g in 1:(G-1)) {
						pred_y_sac_long[(i-1)*current_Q*(G-1) + (q-1)*(G-1) + g, j] <- pred_obs_y[j, i, g+1, q]    
					}
				}
			}
		}
		
		var_theta_fix_0j_ij <- array(0, dim = c((1+B)*current_att_K, (1+B)*current_att_K))
		var_theta_sac_0j_ij <- array(0, dim = c((1+B)*current_att_K*(G-1), (1+B)*current_att_K*(G-1)))
		var_obs_y_sac_0j    <- array(0, dim = c(current_Q*(G-1), current_Q*(G-1)))
		var_obs_y_sac_ij    <- array(0, dim = c(current_Q*(G-1), current_Q*(G-1)))
		
		z_aux_pred_theta_fix_0j_ij <- array(rnorm(post_draws*pred_N*(1+B)*current_att_K, 0, 1), 
											dim = c(post_draws, (1+B)*current_att_K, pred_N))
		aux_pred_theta_fix_ij <- array(0, dim = c(post_draws, B, current_att_K, pred_N))
		
		z_aux_pred_theta_sac_0j_ij <- array(rnorm(post_draws*pred_N*(1+B)*current_att_K*(G-1), 0, 1), 
											dim = c(post_draws, (1+B)*current_att_K*(G-1), pred_N))
		aux_pred_theta_sac_ij <- array(0, dim = c(post_draws, B, current_att_K*(G-1), pred_N))
		
		for(d in 1:post_draws) {
			# fixations
			var_theta_fix_0j_ij[1:current_att_K, 1:current_att_K] <- 
				draws_var_theta_fix_0j[d, 1:current_att_K, 1:current_att_K]
			var_theta_fix_0j_ij[(current_att_K+1):((1+B)*current_att_K), (current_att_K+1):((1+B)*current_att_K)] <- 
				kronecker(diag(B), draws_var_theta_fix_ij[d, 1:current_att_K, 1:current_att_K])
			inv_var_theta_fix_0j_ij <- solve(var_theta_fix_0j_ij)
			
			var_obs_y_fix_0j <- diag(c(sd_y_fix_0j_draws[d, 1:current_Q]^2), nrow = current_Q)
			var_obs_y_fix_ij <- diag(c(sd_y_fix_ij_draws[d, 1:current_Q]^2), nrow = current_Q)
			
			aux_mat_fix     <- t_X_1_fix %*% solve(X_2_fix %*% var_obs_y_fix_0j %*% t_X_2_fix + kronecker(diag(B), var_obs_y_fix_ij))
			aux_D_fix_0j_ij <- solve(aux_mat_fix %*% X_1_fix + inv_var_theta_fix_0j_ij)
			
			beta_fix_0j_ij <- c(as.vector(t(draws_beta_fix_0j[d, 1:current_att_K, ])), 
								as.vector(t(draws_beta_fix_ij[d, 1:current_att_K, ])))
			
			# saccades
			var_theta_sac_0j_ij[1:(current_att_K*(G-1)), 1:(current_att_K*(G-1))] <- 
				draws_var_theta_sac_0j[d, 1:(current_att_K*(G-1)), 1:(current_att_K*(G-1))]
			var_theta_sac_0j_ij[(current_att_K*(G-1)+1):((1+B)*current_att_K*(G-1)), (current_att_K*(G-1)+1):((1+B)*current_att_K*(G-1))] <- 
				kronecker(diag(B), draws_var_theta_sac_ij[d, 1:(current_att_K*(G-1)), 1:(current_att_K*(G-1))])
			inv_var_theta_sac_0j_ij <- solve(var_theta_sac_0j_ij)
			
			var_obs_y_sac_0j[1:(G-1), 1:(G-1)] <- var_y_sac_Q1_0j_draws[d, , ]
			var_obs_y_sac_ij[1:(G-1), 1:(G-1)] <- var_y_sac_Q1_ij_draws[d, , ]
			
			if(current_Q >= 2) {
				var_obs_y_sac_0j[(G-1 + 1):(2*(G-1)), (G-1 + 1):(2*(G-1))] <- var_y_sac_Q2_0j_draws[d, , ]
				var_obs_y_sac_ij[(G-1 + 1):(2*(G-1)), (G-1 + 1):(2*(G-1))] <- var_y_sac_Q2_ij_draws[d, , ]
			}
			
			if(current_Q >= 3) {
				var_obs_y_sac_0j[(2*(G-1) + 1):(3*(G-1)), (2*(G-1) + 1):(3*(G-1))] <- var_y_sac_Q3_0j_draws[d, , ]
				var_obs_y_sac_ij[(2*(G-1) + 1):(3*(G-1)), (2*(G-1) + 1):(3*(G-1))] <- var_y_sac_Q3_ij_draws[d, , ]
			}
			
			if(current_Q >= 4) {
				var_obs_y_sac_0j[(3*(G-1) + 1):(4*(G-1)), (3*(G-1) + 1):(4*(G-1))] <- var_y_sac_Q4_0j_draws[d, , ]
				var_obs_y_sac_ij[(3*(G-1) + 1):(4*(G-1)), (3*(G-1) + 1):(4*(G-1))] <- var_y_sac_Q4_ij_draws[d, , ]
			}
			
			aux_mat_sac <- t_X_1_sac %*% solve(X_2_sac %*% var_obs_y_sac_0j %*% t_X_2_sac + kronecker(diag(B), var_obs_y_sac_ij))
			aux_D_sac_0j_ij <- solve(aux_mat_sac %*% X_1_sac + inv_var_theta_sac_0j_ij)
			
			beta_sac_0j_ij <- c(as.vector(t(draws_beta_sac_0j[d, 1:(current_att_K*(G-1)), ])), 
								as.vector(t(draws_beta_sac_ij[d, 1:(current_att_K*(G-1)), ])))
			
			for(j in 1:pred_N) {
				aux_d_fix_0j_ij <- aux_mat_fix %*% pred_y_fix_long[, j, drop = FALSE] + inv_var_theta_fix_0j_ij %*% X_3_fix[, , j] %*% beta_fix_0j_ij
				aux_theta_fix_0j_ij <- aux_D_fix_0j_ij %*% aux_d_fix_0j_ij + chol(aux_D_fix_0j_ij) %*% z_aux_pred_theta_fix_0j_ij[d, , j, drop = FALSE]
				
				aux_d_sac_0j_ij <- aux_mat_sac %*% pred_y_sac_long[, j, drop = FALSE] + inv_var_theta_sac_0j_ij %*% X_3_sac[, , j] %*% beta_sac_0j_ij
				aux_theta_sac_0j_ij <- aux_D_sac_0j_ij %*% aux_d_sac_0j_ij + chol(aux_D_sac_0j_ij) %*% z_aux_pred_theta_sac_0j_ij[d, , j, drop = FALSE]
				
				for(i in 1:B) {
					start_fix <- i*current_att_K + 1;
					end_fix   <- (i+1)*current_att_K;
					aux_pred_theta_fix_ij[d, i, , j] <- aux_theta_fix_0j_ij[start_fix:end_fix]
					
					start_sac <- i*current_att_K*(G-1) + 1
					end_sac   <- (i+1)*current_att_K*(G-1)
					aux_pred_theta_sac_ij[d, i, , j] <- aux_theta_sac_0j_ij[start_sac:end_sac]
				}
			}
			
			if(d%%10000 == 0) {
				cat(d)
				timestamp()
				cat('\n')
			}
		}
		
		for(j in 1:pred_N) {
			for(i in 1:B) {
				for(k in 1:current_att_K) {
					mtm_pred_theta_jigkq[j, i, 1, k, current_Q] <- mean(aux_pred_theta_fix_ij[, i, k, j])
					for(g in 1:(G-1)) {
						mtm_pred_theta_jigkq[j, i, g+1, k, current_Q] <- mean(aux_pred_theta_sac_ij[, i, (k-1)*(G-1) + g, j])
					}
				}
			}
		}
	}
	
	save(mtm_pred_theta_jigkq, 
		 file = here::here(output_dir, "K12fold", "pred_theta",
		 				  paste0("SNTH_pred_theta_", Kfold, ".RData")))
}


