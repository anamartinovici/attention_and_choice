args <- commandArgs(TRUE)

if(length(args) == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	output_dir <- args[1]
}

library("tidyverse")

source(here::here("analysis", "est_choice_aux.R"))
source(here::here("analysis", "est_att_aux.R"))

df_choice_K <- f_create_df_models() %>%
	filter(do_for_SNTH_K12fold == 1) %>%
	select(M_in_paper)

K12folds <- c("brandA_low", "brandA_medium", "brandA_high",
			  "brandB_low", "brandB_medium", "brandB_high",
			  "other_low",  "other_medium",  "other_high",
			  "nonSP_low",  "nonSP_medium",  "nonSP_high")

if(!dir.exists(here::here(output_dir, "K12fold", "pred_utility"))) {
	dir.create(here::here(output_dir, "K12fold", "pred_utility"),
			   recursive = TRUE)
}

for(choice_model in df_choice_K[["M_in_paper"]]) {
	df_results <- vector(mode = "list", length = 5*12)
	
	for(Kfold in K12folds) {
		load(here::here(output_dir, "K12fold", "procdata",
						paste0("SNTH_procdata_pred_", Kfold, ".RData")))
		load(here::here(output_dir, "K12fold", "pred_theta",
						paste0("SNTH_pred_theta_", Kfold, ".RData")))    
		
		load(here::here(output_dir, "K12fold", "est_choice", choice_model,
						paste0("SNTH_est_choice_", choice_model, "_wo_", Kfold, ".RData")))
		
		beta_draws <- rstan::extract(stan_choice_samples, pars = c("beta"))[[1]]
		beta_draws <- t(beta_draws)
		
		for(current_Q in 0:4) {
			pred_theta_jigk <- array(0, dim = c(pred_N, B, G, 3))
			if(current_Q > 0) {
				# if Q_now is 0, then I don't have any theta so brand choice probabilities 
				# are based only on brand intercepts, brand ownership, and brand knowledge
				pred_theta_jigk <- mtm_pred_theta_jigkq[, , , , current_Q]
			}
			
			aux_list <- f_arrange_X_choice_perModel(B = B, 
													df_part_info = pred_participant_info, 
													# works also when current_Q is 0, meaning that it assigns 0 fixations
													df_obs_em = pred_obs_eye_movements %>% filter(quarter <= current_Q), 
													ar_theta_jigk = pred_theta_jigk, 
													choice_model = choice_model,
													df_brand_number = brand_number,
													CPLX_colnum_brandname = CPLX_colnum_brandname)
			
			pred_true_choice <- aux_list$chosen_brand
			pred_X_choice <- aux_list$X_M
			remove(aux_list)
			
			pred_utility <- NULL
			for(j in 1:pred_N) {
				# aux_utility is B x n_draws
				brand_utility <- pred_X_choice[j, , ] %*% beta_draws
				
				pred_utility[[j]] <- list(participant = pred_IDs[j],
										  true_choice = pred_true_choice[j],
										  pred_atQ = current_Q,
										  choice_model = choice_model,
										  brand_utility = brand_utility,
										  pred_X_choice = pred_X_choice[j, , ],
										  dim_brand_utility = "brandnum by choicedraws")
			}
			
			df_results[current_Q*12 + which(K12folds == Kfold)] <- list(pred_utility)
		}
	}
	
	df_results <- flatten(df_results)
	save(df_results,
		 file = here::here(output_dir, "K12fold", "pred_utility", 
		 				  paste0("SNTH_pred_utility_", choice_model, ".RData")))
}
