args <- commandArgs(TRUE)

if(length(args) == 0) {
	stop("You need to provide arguments", call. = FALSE)
} else {
	output_dir <- args[1]
}

library("tidyverse")

source(here::here("analysis", "est_choice_aux.R"))
source(here::here("analysis", "Kfold_pred_choice_aux.R"))

df_choice_K <- f_create_df_models() %>%
	filter(do_for_SNTH_K12fold == 1) %>%
	select(M_in_paper)

if(!dir.exists(here::here(output_dir, "K12fold", "pred_choice"))) {
	dir.create(here::here(output_dir, "K12fold", "pred_choice"),
			   recursive = TRUE)
}

df_correctpredbrandchoice_mq <- NULL
df_probchosenbrand_mqj <- NULL

for(index_model in df_choice_K[["M_in_paper"]]) {
	load(here::here(output_dir, "K12fold", "pred_utility", 
					paste0("SNTH_pred_utility_", index_model, ".RData")))
	
	# correct pred brand choice
	df_correctpredbrand <- map_df(df_results, f_hit01_randomties)
	df_correctpredbrand <- f_summarise_by_mq(df_results = df_correctpredbrand)
	df_correctpredbrand <- df_correctpredbrand %>% 
		mutate(Kfoldsplit = "K12fold") %>%
		mutate(type_Kfold = "designbased")
	df_correctpredbrandchoice_mq[[index_model]] <- df_correctpredbrand
	
	# prob chosen brand	
	df_probchosenbrand <- map_df(df_results, f_prob_truechoice)
	df_probchosenbrand <- f_summarise_by_mqj(df_results = df_probchosenbrand)
	df_probchosenbrand <- df_probchosenbrand %>% 
		mutate(Kfoldsplit = "K12fold") %>%
		mutate(type_Kfold = "designbased")
	df_probchosenbrand_mqj[[index_model]] <- df_probchosenbrand
}
df_correctpredbrandchoice_mq <- purrr::reduce(df_correctpredbrandchoice_mq, rbind)
save(df_correctpredbrandchoice_mq, 
	 file = here::here(output_dir, "K12fold", "pred_choice", 
	 				  "SNTH_df_correctpredbrandchoice_mq.RData"))

df_probchosenbrand_mqj <- purrr::reduce(df_probchosenbrand_mqj, rbind)
save(df_probchosenbrand_mqj, 
	 file = here::here(output_dir, "K12fold", "pred_choice", 
	 				  "SNTH_df_probchosenbrand_mqj.RData"))


