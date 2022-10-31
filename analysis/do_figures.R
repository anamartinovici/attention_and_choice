gg_att_share_V1 <- function(DIR_current_results) {
	load(here::here("data", "SNTH_procdata.RData"))
	
	participant_info <- f_loyal_switcher(participant_info)
	obs_eye_movements <- obs_eye_movements %>%
		left_join(participant_info %>% 
				  	select(participant, loyalty_3groups, 
				  		   brandletter_chosen, brandletter_owned), 
				  by = "participant")
	obs_eye_movements <- obs_eye_movements %>%
		mutate(is_owned = case_when(brandletter == brandletter_owned ~ "owned_brand",
									TRUE ~ "not_owned"),
			   is_chosen = case_when(brandletter == brandletter_chosen ~ "chosen_brand",
			   					  TRUE ~ "not_chosen"))
	obs_eye_movements <- obs_eye_movements %>%
		mutate(type_of_brand = paste0(is_owned, "_", is_chosen))
	
	df_plot <- obs_eye_movements %>% 
		filter(EM_name %in% c("fix", "bsac")) 
	df_plot <- df_plot %>%
		group_by(participant, EM_name, quarter) %>%
		mutate(sum_EM_count_jgt = sum(EM_count)) %>% 
		ungroup()
	df_plot <- df_plot %>%
		mutate(share_jigt = if_else(sum_EM_count_jgt > 0, EM_count/sum_EM_count_jgt, 0))
	
	df_plot %>%
		ggplot(aes(x = as.factor(quarter), y = share_jigt)) +
		geom_boxplot(width = 0.25, size = 0.5, color = "grey50") +
		scale_y_continuous("Share of Eye-movements",
						   limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
		geom_hline(aes(yintercept = 0.2), color = "red", linetype = "dotted", size = 1) + 
		stat_summary(
			fun = median, geom = "errorbar", aes(ymax = ..y.., ymin = ..y.., color = "Median"),
			width = .25, linetype = "solid", size = 1) +
		scale_colour_manual("", values = c(Median = "black"), labels = c("Median" = "Median (half of participants have a larger share)")) +
		xlab("Quarter") +
		facet_grid(factor(EM_name, 
						  levels = c("fix", "bsac"), 
						  labels = c("fix" = "Fixations", "bsac" = "Within-brand Saccades")) ~ is_chosen,
				   labeller = labeller(is_chosen = c("chosen_brand" = "Chosen Brand", 
				   								  "not_chosen" = "Not Chosen Brands"))) +
		theme_bw() +
		theme(legend.position = "bottom")
}


gg_att_share_V2 <- function(DIR_current_results) {
	load(here::here("data", "SNTH_procdata.RData"))
	
	participant_info <- f_loyal_switcher(participant_info)
	obs_eye_movements <- obs_eye_movements %>%
		left_join(participant_info %>% 
				  	select(participant, loyalty_3groups, 
				  		   brandletter_chosen, brandletter_owned), 
				  by = "participant")
	obs_eye_movements <- obs_eye_movements %>%
		mutate(is_owned = case_when(brandletter == brandletter_owned ~ "owned_brand",
									TRUE ~ "not_owned"),
			   is_chosen = case_when(brandletter == brandletter_chosen ~ "chosen_brand",
			   					  TRUE ~ "not_chosen"))
	obs_eye_movements <- obs_eye_movements %>%
		mutate(type_of_brand = paste0(is_owned, "_", is_chosen))
	
	df_plot <- obs_eye_movements %>%
		filter(EM_name == "fix") %>%
		select(participant, brandletter, quarter, EM_count, 
			   loyalty_3groups, is_chosen, is_owned)
	df_plot <- df_plot %>%
		pivot_wider(values_from = EM_count,
					names_from = quarter,
					names_prefix = "nfix_")
	df_plot <- df_plot %>%
		mutate(nfix_all_Q = nfix_1 + nfix_2 + nfix_3 + nfix_4,
			   nfix_first_half = nfix_1 + nfix_2,
			   nfix_second_half = nfix_3 + nfix_4)
	df_plot <- df_plot %>%
		group_by(participant) %>%
		mutate(sum_nfix_first_half = sum(nfix_first_half),
			   sum_nfix_second_half = sum(nfix_second_half),
			   sum_nfix_all_Q = sum(nfix_all_Q)) %>% ungroup()
	
	df_plot <- df_plot %>%
		mutate(nfix_time_change = if_else(nfix_all_Q > 0, nfix_second_half / nfix_all_Q, 0)) %>%
		mutate(share_vs_competitors = if_else(sum_nfix_first_half > 0, nfix_first_half / sum_nfix_first_half, 0))
	
	df_plot <- df_plot %>%
		mutate(is_owned_not_chosen = case_when((is_owned == "owned_brand")*(is_chosen == "not_chosen") == 1 ~ "owned_not_chosen",
											   TRUE ~ "other_type"))
	
	x_vline <- 2/4
	y_hline <- 1/5
	
	df_plot <- df_plot %>%
		mutate(pos_vs_vline = if_else(nfix_time_change < x_vline, "left_of_the_vline", "right_of_the_vline"),
			   pos_vs_hline = if_else(share_vs_competitors < y_hline, "below_the_hline", "above_the_hline"))
	df_plot <- df_plot %>%
		mutate(quadrant_number = case_when((pos_vs_hline == "above_the_hline")*(pos_vs_vline == "left_of_the_vline") == 1 ~ "UL",
										   (pos_vs_hline == "above_the_hline")*(pos_vs_vline == "right_of_the_vline") == 1 ~ "UR",
										   (pos_vs_hline == "below_the_hline")*(pos_vs_vline == "right_of_the_vline") == 1 ~ "BR",
										   (pos_vs_hline == "below_the_hline")*(pos_vs_vline == "left_of_the_vline") == 1 ~ "BL"))
	
	labels_loyalty <- c("loyal" = "Loyals",
						"switcher" = "Switchers",
						"other" = "Others")
	labels_chosen <- c("chosen_brand" = "Chosen Brand",
					   "not_chosen" = "Not Chosen Brand")
	
	f_labels <- function(x) {
		my_labels <- numform::f_num(x, digits = 2)
		my_labels[1] <- 0
		my_labels[length(my_labels)] <- 1
		return(my_labels)
	}
	
	df_plot <- df_plot
	
	df_plot %>%
		filter(loyalty_3groups %in% c("loyal", "switcher")) %>%
		filter(is_chosen %in% c("chosen_brand") | is_owned == "owned_brand") %>%
		ggplot(aes(x = share_vs_competitors, y = nfix_time_change, color = factor(is_chosen), alpha = factor(is_chosen))) +
		geom_point(size = 2) +
		scale_x_continuous("Attention Share in First Half of Task", limits = c(0, 1), labels = function(x) f_labels(x)) +
		scale_y_continuous("Attention Share in Second Half of Task", limits = c(0, 1)) +
		geom_hline(yintercept = x_vline, color = "red", linetype = "dashed") +
		geom_vline(xintercept = y_hline, color = "red", linetype = "dashed") +
		scale_color_manual(name = NULL, values = c("chosen_brand" = "black", "not_chosen" = "grey70"), 
						   labels = c("chosen_brand" = "Chosen Brand",
						   		   "not_chosen" = "Owned and Not Chosen Brand")) +
		scale_alpha_manual(name = NULL, values = c("chosen_brand" = 1, "not_chosen" = 0.75), labels = c("chosen_brand" = "Chosen Brand",
																										"not_chosen" = "Owned and Not Chosen Brand")) +
		facet_wrap(vars(loyalty_3groups), labeller = labeller(loyalty_3groups = labels_loyalty)) +
		theme_bw() +
		coord_fixed(ratio = 1) +
		theme(legend.position = "bottom")
}


gg_figure_E2 <- function(DIR_current_results) {
	load(here::here("data", "SNTH_procdata.RData"))
	
	aux_EM <- obs_eye_movements
	aux_EM <- aux_EM %>%
		filter(EM_name %in% c("bsac")) %>%
		mutate(brandname = case_when(brandletter == "A" ~ "Apple",
									 brandletter == "B" ~ "Samsung",
									 brandletter == "C" ~ "Nokia",
									 brandletter == "D" ~ "HTC",
									 brandletter == "E" ~ "Motorola",
									 TRUE ~ "check_this")) %>%
		select(participant, EM_name, brandname, quarter, EM_count)
	aux_EM <- aux_EM %>%
		pivot_wider(names_from = quarter,
					names_prefix = "EM_Q",
					values_from = EM_count)
	aux_EM <- aux_EM %>%
		arrange(participant, brandname)
	
	mean_aux_EM <- aux_EM %>%
		group_by(participant, EM_name) %>%
		summarise(mean_EM_Q1 = mean(EM_Q1),
				  mean_EM_Q2 = mean(EM_Q2),
				  mean_EM_Q3 = mean(EM_Q3),
				  mean_EM_Q4 = mean(EM_Q4)) %>% ungroup()
	
	# I select these participants to discuss in the paper
	use_participants <- data.frame(participant = c(85, 119, 174, 257, 258))
	
	# Example 1 - participant-level WBS (within-brand saccades)
	aux_df1 <- aux_EM %>%
		inner_join(use_participants, by = "participant")
	aux_df2 <- mean_aux_EM %>%
		inner_join(use_participants, by = "participant")
	
	aux_df2 %>%
		pivot_longer(cols = starts_with("mean_EM_Q"),
					 values_to = "mean_EM",
					 names_to = "quarter_number",
					 names_prefix = "mean_EM_Q") %>%
		ggplot(aes(x = quarter_number, y = mean_EM, 
				   group = participant)) +
		geom_line(aes(linetype = as.factor(participant), color = as.factor(participant)), size = 1) + 
		geom_text(data = aux_df2,
				  mapping = aes(x = 4.2, y = mean_EM_Q4, label = paste0("P ", participant)),
				  size = 3.5) +
		scale_linetype_manual("Participant number",
							  values = c("85" = "solid", "119" = "solid",  "174" = "dotted", "257" = "dotted", "258" = "dashed")) +
		scale_color_manual("Participant number",
						   values = c("85" = "black", "119" = "grey50", "174" = "black", "257" = "grey50", "258" = "black")) +
		xlab("Quarter number") +
		ylab("Average within-brand saccades") +
		theme_bw() +
		theme(legend.position = "bottom", 
			  legend.key.width = unit(1, "cm"), 
			  legend.text = element_text(size = 12),
			  axis.title = element_text(size = 12),
			  axis.text.x = element_text(size = 12),
			  legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
		guides(linetype = guide_legend(nrow = 2))
}



gg_figure_E3 <- function(DIR_current_results) {
	load(here::here("data", "SNTH_procdata.RData"))
	
	aux_EM <- obs_eye_movements
	aux_EM <- aux_EM %>%
		filter(EM_name %in% c("bsac")) %>%
		mutate(brandname = case_when(brandletter == "A" ~ "Apple",
									 brandletter == "B" ~ "Samsung",
									 brandletter == "C" ~ "Nokia",
									 brandletter == "D" ~ "HTC",
									 brandletter == "E" ~ "Motorola",
									 TRUE ~ "check_this")) %>%
		select(participant, EM_name, brandname, quarter, EM_count)
	aux_EM <- aux_EM %>%
		pivot_wider(names_from = quarter,
					names_prefix = "EM_Q",
					values_from = EM_count)
	aux_EM <- aux_EM %>%
		arrange(participant, brandname)
	
	mean_aux_EM <- aux_EM %>%
		group_by(participant, EM_name) %>%
		summarise(mean_EM_Q1 = mean(EM_Q1),
				  mean_EM_Q2 = mean(EM_Q2),
				  mean_EM_Q3 = mean(EM_Q3),
				  mean_EM_Q4 = mean(EM_Q4)) %>% ungroup()
	
	# I select these participants to discuss in the paper
	use_participants <- data.frame(participant = c(85, 119, 174, 257, 258))
	
	# Example 1 - participant-level WBS (within-brand saccades)
	aux_df1 <- aux_EM %>%
		inner_join(use_participants, by = "participant")
	aux_df2 <- mean_aux_EM %>%
		inner_join(use_participants, by = "participant")
	
	# I then take a screenshot of aux_df1 and aux_df2
	aux_df2 <- aux_df2 %>%
		left_join(participant_info %>%
				  	select(participant, cplx_cond, smartphone_owner), 
				  by = "participant")
	
	# in text, I discuss the % changes from one Q to the next
	aux_df2 <- aux_df2 %>%
		mutate(part_from_Q1_to_Q2 = numform::f_percent(100 * (mean_EM_Q2/mean_EM_Q1 - 1), digits = 0),
			   part_from_Q2_to_Q3 = numform::f_percent(100 * (mean_EM_Q3/mean_EM_Q2 - 1), digits = 0),
			   part_from_Q3_to_Q4 = numform::f_percent(100 * (mean_EM_Q4/mean_EM_Q3 - 1), digits = 0))
	
	# example 2: participant-and-brand variation in WBS
	brand_number <- brand_number %>%
		mutate(brandname = case_when(brandletter == "A" ~ "Apple",
									 brandletter == "B" ~ "Samsung",
									 brandletter == "C" ~ "Nokia",
									 brandletter == "D" ~ "HTC",
									 brandletter == "E" ~ "Motorola",
									 TRUE ~ "check_this"))
	aux_df3 <- aux_df1 %>%
		filter(participant %in% c(85, 119, 174, 258)) %>%
		left_join(brand_number %>% select(brandname, brandletter),
				  by = "brandname")
	aux_df3 <- aux_df3 %>%
		left_join(participant_info %>%
				  	select(participant, counterbalanceid, brandletter_owned),
				  by = "participant")
	aux_df3 <- aux_df3 %>%
		left_join(CPLX_colnum_brandname,
				  by = c("counterbalanceid", "brandletter"))
	aux_df3 <- aux_df3 %>%
		mutate(is_owned = case_when(brandletter == brandletter_owned ~ 1,
									TRUE ~ 0))
	aux_df3 <- aux_df3 %>%
		select(participant,
			   EM_name,
			   brandname,
			   EM_Q1, EM_Q2, EM_Q3, EM_Q4,
			   brandcol_number, is_owned) %>%
		rename(column_number = brandcol_number)
	
	aux_df4 <- aux_df3 %>%
		pivot_longer(cols = starts_with("EM_Q"),
					 values_to = "EM_count",
					 names_to = "quarter_number",
					 names_prefix = "EM_Q")
	
	aux_df5 <- aux_df4 %>%
		group_by(participant, EM_name, quarter_number) %>%
		summarise(mean_EM = mean(EM_count)) %>% 
		ungroup()
	aux_df5 <- aux_df5 %>%
		rename(EM_count = mean_EM) %>%
		mutate(brandname = "avg_brand",
			   column_number = 0,
			   is_owned = 0)
	df_plot <- rbind(aux_df4,
					 aux_df5)
	
	df_plot <- df_plot %>%
		mutate(quarter_number = as.integer(quarter_number))
	
	df_plot %>%
		filter(brandname != "avg_brand") %>%
		ggplot(aes(x = quarter_number, y = EM_count)) +
		geom_line(aes(group = brandname, 
					  size = as.factor(is_owned),
					  linetype = brandname,
					  color = brandname)) +
		geom_line(data = df_plot %>% filter(brandname == "avg_brand"),
				  mapping = aes(x = quarter_number, y = EM_count, group = "avg_line"), 
				  color = "red", size = 2, linetype = "solid", alpha = 0.50) + 
		geom_text(data = df_plot %>% 
				  	filter(brandname != "avg_brand") %>%
				  	filter(quarter_number == 4),
				  mapping = aes(x = 4.2, y = EM_count, label = paste0("C ", column_number)),
				  size = 3.5) +
		scale_size_manual(" " ,
						  values = c("1" = 1.5, "0" = 0.75),
						  labels = c("1" = "Owned", "0" = "Not owned")) +
		scale_linetype_manual(" ", values = c("Apple" = "solid", "Samsung" = "solid", 
											  "Nokia" = "dashed", "HTC" = "dashed", 
											  "Motorola" = "dotted")) +
		scale_color_manual(" ", values = c("Apple" = "black", "Samsung" = "grey50", 
										   "Nokia" = "black", "HTC" = "grey50", 
										   "Motorola" = "black")) +
		facet_wrap(vars(participant), scales = "free") +
		scale_y_continuous(limits = c(0, NA)) +
		xlab("Quarter number") +
		ylab("Within-brand saccades") +
		theme_bw() +
		theme(legend.position = "bottom", 
			  legend.key.width = unit(1, "cm"), 
			  legend.text = element_text(size = 12),
			  axis.title = element_text(size = 12),
			  axis.text.x = element_text(size = 12),
			  legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
		guides(linetype = guide_legend(nrow = 3),
			   size = guide_legend(nrow = 2)) 
}

gg_figure_F1_part1 <- function(DIR_current_results) {
	load(here::here("data", "ORIG_summaries_figureF1_part1.RData"))
	load(here::here("data", "SNTH_procdata.RData"))
	
	SNTH_em <- obs_eye_movements
	SNTH_part_info <- participant_info
	remove(obs_eye_movements, participant_info)
	
	df_plot <- SNTH_part_info %>%
		group_by(brandletter_chosen) %>%
		summarise(choice_share = n()/N) %>% ungroup() %>%
		mutate(type_of_data = "SNTH")
	
	df_plot <- df_plot %>% 
		left_join(brandname, by = c("brandletter_chosen" = "brandletter"))
	
	df_plot <- rbind(df_plot,
					 df_ORIG)
	
	df_plot %>%
		ggplot(aes(x = brand_name, y = choice_share, fill = type_of_data)) +
		geom_col(position = position_dodge()) +
		theme_bw() + 
		theme(legend.position = "bottom") +
		xlab("Chosen brand") +
		ylab("% Participants") +
		scale_fill_manual("Type of data:", 
						  values = c("ORIG" = "grey75", "SNTH" = "grey25"), 
						  labels = c("ORIG" = "Original", "SNTH" = "Simulated"))
	
	
}


gg_figure_F1_part2 <- function(DIR_current_results) {
	load(here::here("data", "ORIG_summaries_figureF1_part2.RData"))
	load(here::here("data", "SNTH_procdata.RData"))
	
	SNTH_em <- obs_eye_movements
	SNTH_part_info <- participant_info
	remove(obs_eye_movements, participant_info)
	
	SNTH_part_info <- f_loyal_switcher(SNTH_part_info)
	
	df_plot <- df_ORIG %>%
		left_join(SNTH_part_info %>% 
				  	select(participant, loyalty_4groups) %>%
				  	rename(SNTH_loyalty = loyalty_4groups),
				  by = "participant")
	
	df_plot <- df_plot %>%
		mutate(is_same_in_SNTH = case_when(ORIG_loyalty == SNTH_loyalty ~ "same",
										   TRUE ~ "different"))
	
	df_plot %>%
		ggplot(aes(x = ORIG_loyalty, fill = is_same_in_SNTH)) +
		geom_bar() +
		theme_bw() + 
		ylab("N") +
		scale_fill_manual("Same type in the Simulated dataset:", 
						  values = c("same" = "grey25", "different" = "grey75"),
						  breaks = c("same", "different"),
						  labels = c("same" = "Yes", "different" = "No")) +
		theme(legend.position = "bottom") +
		scale_x_discrete("Type of consumer in the Original dataset",
						 labels = c("loyal" = "Loyal", "switcher" = "Switcher"))
}

f_figureF2 <- function(DIR_current_results, do_this) {
	# compare beta_fix and beta_sac for brand knowledge
	# for saccades, the beta has dim draws x att_K*(G-1) x att_K_0j
	# att_K*(G-1) means that the first 3 positions are for the first growth factor (intercept) for all saccades types
	# so if I want to extract growth factors for attention type "bsac", I take 1, 4, and 7
	names_beta_0j <- c("intercept", "info_cond", "cat_own", "cat_know", "gender", "age", "education", "inc_noanswer", "inc_level")
	names_beta_ij <- c("brand_B", "brand_C", "brand_D", "brand_E", 
					   "br_own", "column_1", "column_2", "column_3", "column_4", "br_know")
	
	load(here::here(DIR_LOF, "fullsample/est_betatheta/SNTH_betadraws_att.RData"))
	
	if(do_this == "info_cond") {
		use_index <- which(names_beta_0j == do_this)
		plot_title <- "Effect of Information Condition \n on Attention:"
		load(here::here("data", "ORIG_summary_figureF2_part1.RData"))
		
		df_SNTH <- data.frame(beta_fix_0j_int = draws_beta_fix_0j[, 1, use_index],
							  beta_fix_0j_ls  = draws_beta_fix_0j[, 2, use_index],
							  beta_fix_0j_qs  = draws_beta_fix_0j[, 3, use_index],
							  
							  beta_bsac_0j_int = draws_beta_sac_0j[, 1, use_index],
							  beta_bsac_0j_ls  = draws_beta_sac_0j[, 4, use_index],
							  beta_bsac_0j_qs  = draws_beta_sac_0j[, 7, use_index],
							  
							  beta_asac_0j_int = draws_beta_sac_0j[, 2, use_index],
							  beta_asac_0j_ls  = draws_beta_sac_0j[, 5, use_index],
							  beta_asac_0j_qs  = draws_beta_sac_0j[, 8, use_index],
							  
							  beta_nsac_0j_int = draws_beta_sac_0j[, 3, use_index],
							  beta_nsac_0j_ls  = draws_beta_sac_0j[, 6, use_index],
							  beta_nsac_0j_qs  = draws_beta_sac_0j[, 9, use_index])
	}
	
	if(do_this == "br_own") {
		use_index <- which(names_beta_ij == do_this)
		plot_title <- "Effect of Brand Ownership \n on Attention:"
		load(here::here("data", "ORIG_summary_figureF2_part2.RData"))
		
		df_SNTH <- data.frame(beta_fix_ij_int = draws_beta_fix_ij[, 1, use_index],
							  beta_fix_ij_ls  = draws_beta_fix_ij[, 2, use_index],
							  beta_fix_ij_qs  = draws_beta_fix_ij[, 3, use_index],
							  
							  beta_bsac_ij_int = draws_beta_sac_ij[, 1, use_index],
							  beta_bsac_ij_ls  = draws_beta_sac_ij[, 4, use_index],
							  beta_bsac_ij_qs  = draws_beta_sac_ij[, 7, use_index],
							  
							  beta_asac_ij_int = draws_beta_sac_ij[, 2, use_index],
							  beta_asac_ij_ls  = draws_beta_sac_ij[, 5, use_index],
							  beta_asac_ij_qs  = draws_beta_sac_ij[, 8, use_index],
							  
							  beta_nsac_ij_int = draws_beta_sac_ij[, 3, use_index],
							  beta_nsac_ij_ls  = draws_beta_sac_ij[, 6, use_index],
							  beta_nsac_ij_qs  = draws_beta_sac_ij[, 9, use_index])
	}
	
	df_SNTH <- df_SNTH %>%
		pivot_longer(cols = everything(),
					 names_to = "param_name",
					 values_to = "post_draw")
	df_SNTH <- df_SNTH %>%
		left_join(df_ORIG, by = "param_name") 
	
	df_SNTH <- df_SNTH %>%
		mutate(EM_name = str_sub(param_name, start = 6, end = 9))
	df_SNTH <- df_SNTH %>%
		mutate(comp_name = str_sub(param_name, start = 13))
	table(df_SNTH$EM_name)
	table(df_SNTH$comp_name)
	df_SNTH <- df_SNTH %>%
		mutate(EM_name = str_remove(EM_name, "_"))
	df_SNTH <- df_SNTH %>%
		mutate(comp_name = str_remove(comp_name, "_"))
	table(df_SNTH$EM_name)
	table(df_SNTH$comp_name)
	df_SNTH <- df_SNTH %>%
		mutate(factor_EM_name = factor(EM_name, 
									   levels = c("fix", "bsac", "asac", "nsac"), 
									   labels = c("Quantity", "Integration", "Comparison", "Other")))
	df_SNTH <- df_SNTH %>%
		mutate(factor_comp_name = factor(comp_name,
										 levels = c("int", "ls", "qs"),
										 labels = c("Initial Level", "Linear Change", "Quadratic Change")))
	df_SNTH <- df_SNTH %>%
		group_by(param_name) %>%
		mutate(LCI = quantile(post_draw, probs = c(0.025)),
			   UCI = quantile(post_draw, probs = c(0.975))) %>% ungroup()
	
	df_CI <- distinct(df_SNTH %>%
					  	select(factor_comp_name, factor_EM_name, LCI, UCI))
	
	df_SNTH %>%
		ggplot() +
		geom_density(aes(x = post_draw), fill = "grey75") +
		geom_vline(aes(xintercept = ORIG_mean_value, color = "ORIG_mean"), size = 1) +
		geom_rect(data = df_CI, aes(xmin = LCI, xmax = UCI, ymin = 0, ymax = Inf, fill = "CI"), alpha = 0.25) +
		scale_color_manual(name = "", values = c("ORIG_mean" = "blue"), 
						   labels = c("ORIG_mean" = "True value")) +
		scale_fill_manual(name = "", values = c("CI" = "grey25"), 
						  labels = c("CI" = "95% CI")) +
		facet_wrap(vars(factor_EM_name, factor_comp_name), ncol = 3, scales = "free") +
		scale_x_continuous(name = NULL, breaks = NULL) +
		scale_y_continuous(name = NULL, breaks = NULL) +
		theme_bw() +
		theme(legend.position = "bottom") +
		ggtitle(plot_title) +
		theme(strip.text.x = element_text(size = 8))
}


