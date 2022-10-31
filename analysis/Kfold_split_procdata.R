args <- commandArgs(TRUE)

if(length(args) == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	output_dir <- args[1]
}

library("tidyverse")
load(here::here("data", "SNTH_procdata.RData"))

K12folds <- (participant_info %>% 
			 	select(K12fold) %>%
			 	distinct())[["K12fold"]]

if(!dir.exists(here::here(output_dir, "K12fold", "procdata"))) {
	dir.create(here::here(output_dir, "K12fold", "procdata"), 
			   recursive = TRUE)
}

for(Kfold in K12folds) {
	pred_participant_info <- participant_info %>% 
		filter(K12fold == Kfold)
	pred_obs_eye_movements <- obs_eye_movements %>% 
		semi_join(pred_participant_info, 
				  by = "participant")
	pred_N <- as.numeric(nrow(pred_participant_info))
	pred_IDs <- pred_participant_info[["participant"]]
	
	est_participant_info <- participant_info %>% 
		anti_join(pred_participant_info, 
				  by = "participant")
	est_obs_eye_movements <- obs_eye_movements %>% 
		semi_join(est_participant_info, 
				  by = "participant")
	est_N <- as.numeric(nrow(est_participant_info))
	est_IDs  <- est_participant_info[["participant"]]
	
	save(Kfold, 
		 B, G, Q, brand_number, CPLX_colnum_brandname,
		 est_obs_eye_movements, est_participant_info, est_N, est_IDs,
		 pred_obs_eye_movements, pred_participant_info, pred_N, pred_IDs,
		 file = here::here(output_dir, "K12fold", "procdata",
		 			       paste0("SNTH_procdata_pred_", Kfold, ".RData")))
}
