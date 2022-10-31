all: tables_and_figures.html

# test if you can use Make to compile stan code
# by writing in the terminal "make test_target"
# the test is succesful if it generates "test_setup/stan_test.rds" and "test_target"
test_target: 
	@echo "Testing if .stan code is compiled"
	Rscript test_setup/stan_test.R
	date > $@

###############################
tables_and_figures.html: output/fullsample/est_choice/SNTH_est_choice \
						 output/K12fold/pred_choice/SNTH_K12fold_pred_choice \
						 analysis/tables_and_figures.Rmd \
						 analysis/tables_and_figures_aux.R
	$(print-target-and-prereq-info)
	Rscript analysis/render_rmd.R \
			analysis/tables_and_figures.Rmd

# fullsample
output/fullsample/est_choice/SNTH_est_choice: output/fullsample/est_betatheta/SNTH_thetadraws_att \
											  analysis/est_choice.R \
											  analysis/est_choice_aux.R
	$(print-target-and-prereq-info)
	Rscript analysis/est_choice.R \
			"fullsample" \
			"output"
	date > $@

output/fullsample/est_betatheta/SNTH_thetadraws_att: output/fullsample/est_betatheta/SNTH_betadraws_att.RData \
													 analysis/extract_thetadraws_ji.R \
													 analysis/extract_thetadraws_j0.R 
	$(print-target-and-prereq-info)
	Rscript analysis/extract_thetadraws_j0.R \
                    "output"
	Rscript analysis/extract_thetadraws_ji.R \
                    "fullsample" \
                    "output"
	date > $@

output/fullsample/est_betatheta/SNTH_betadraws_att.RData: output/fullsample/est_att/SNTH_est_att \
														  analysis/extract_betadraws_att.R
	$(print-target-and-prereq-info)
	Rscript analysis/extract_betadraws_att.R \
                    "fullsample" \
                    "output"

output/fullsample/est_att/SNTH_est_att: analysis/est_att.R \
									    analysis/est_att_aux.R \
									    analysis/est_att.stan
	$(print-target-and-prereq-info)
	Rscript analysis/est_att.R \
	        $(strip $(FS_KFOLD)) \
	        "data/SNTH_procdata.RData" \
	        $(strip $(NITER_ATT)) \
	        "SNTH_est_att" \
	        "output/fullsample/est_att"
	date > $@

# K12fold
output/K12fold/pred_choice/SNTH_K12fold_pred_choice: output/K12fold/pred_utility/SNTH_K12fold_pred_utility \
													 analysis/Kfold_pred_choice.R \
													 analysis/Kfold_pred_choice_aux.R
	$(print-target-and-prereq-info)
	Rscript analysis/Kfold_pred_choice.R \
			"output"
	date > $@

output/K12fold/pred_utility/SNTH_K12fold_pred_utility: output/K12fold/pred_theta/SNTH_K12fold_pred_theta \
													   output/K12fold/est_choice/SNTH_K12fold_est_choice \
													   analysis/Kfold_pred_utility.R
	$(print-target-and-prereq-info)
	Rscript analysis/Kfold_pred_utility.R \
			"output"
	date > $@

output/K12fold/pred_theta/SNTH_K12fold_pred_theta: output/K12fold/est_betatheta/SNTH_K12fold_est_betatheta \
												   analysis/Kfold_pred_att_theta_jigkq.R
	$(print-target-and-prereq-info)
	Rscript analysis/Kfold_pred_att_theta_jigkq.R \
			"output"
	date > $@
	
output/K12fold/est_choice/SNTH_K12fold_est_choice: output/K12fold/est_betatheta/SNTH_K12fold_est_betatheta \
												   analysis/est_choice.R \
												   analysis/est_choice_aux.R
	$(print-target-and-prereq-info)
	Rscript analysis/est_choice.R \
			"K12fold" \
			"output"
	date > $@

output/K12fold/est_betatheta/SNTH_K12fold_est_betatheta: output/K12fold/est_att/SNTH_K12fold_est_att \
														 analysis/extract_betadraws_att.R \
														 analysis/extract_thetadraws_ji.R \
														 analysis/extract_thetadraws_j0.R 
	$(print-target-and-prereq-info)
	Rscript analysis/extract_betadraws_att.R \
                    "K12fold" \
                    "output"
	Rscript analysis/extract_thetadraws_ji.R \
                    "K12fold" \
                    "output"
	date > $@

output/K12fold/est_att/SNTH_K12fold_est_att: analysis/Kfold_split_procdata.R \
											 analysis/est_att.R \
											 analysis/est_att_aux.R \
											 analysis/est_att.stan
	$(print-target-and-prereq-info)
	Rscript analysis/Kfold_split_procdata.R \
			"output"
	Rscript analysis/est_att.R \
	        $(strip $(FS_KFOLD)) \
	        "data/SNTH_procdata.RData" \
	        $(strip $(NITER_ATT)) \
	        "SNTH_est_att" \
	        "output/fullsample/est_att"
	date > $@

##############################################
synthRB_N325_K12fold: FS_KFOLD = K12fold
synthRB_N325_K12fold: WO_WITH = wo_chosen
synthRB_N325_K12fold: EXCL_INCL_THETA = include_theta

synthRB_N325_K12fold: synthRB_N325_K12fold_splitdata \
					  synthRB_N325_K12fold_est_att \
					  synthRB_N325_K12fold_extract_beta \
					  synthRB_N325_K12fold_extract_theta \
					  synthRB_N325_K12fold_predict_theta \

synthRB_N325_K12fold_predict_theta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_predict_theta
$(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_predict_theta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_beta
$(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_predict_theta: $(ALL_THETA_PRED_REQ)
	$(predict-theta)

synthRB_N325_K12fold_extract_beta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_beta
$(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_beta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_est_att
ALL_EXTRACT_BETA += $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_beta

synthRB_N325_K12fold_extract_theta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_theta
$(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_theta: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_est_att
ALL_EXTRACT_THETA += $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_extract_theta

synthRB_N325_K12fold_est_att: NTH_batch = 0
synthRB_N325_K12fold_est_att: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_est_att
$(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_est_att: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_splitdata
ALL_EST_ATT += $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_est_att

synthRB_N325_K12fold_splitdata: $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_splitdata
ALL_SPLIT_DATA += $(strip $(DIR_RECEIPT))/synthRB_N325_K12fold_splitdata

######### macros ############

define print-target-and-prereq-info
	@echo "---------------------------------------------------------------"
	@printf "         Target is: $@ \n \n"
	@printf "The prerequisites for this target ($@) are: \n $(foreach wrd, $^, $(wrd)\n) \n"
	@printf "The prerequisites newer than the target ($@) are: \n $(foreach wrd, $?, $(wrd)\n) \n"
	@printf "         Starting work on target: $@ \n\n"
endef


define predict-theta
	$(print-target-and-prereq-info)
	bash analysis/Kfold_predict_theta_jigkq.sh \
		 -a $(strip $(NITER_ATT)) \
		 -b $(strip $(DIR_LOF)) \
		 -c $(strip $(TYPE_of_DATA)) \
		 -d $(strip $(FS_KFOLD)) \
		 -e $(strip $(WO_WITH))
	date > $@
	@echo ""
endef

define create-Kfolds
	$(print-target-and-prereq-info)
	mkdir -p $(strip $(DIR_LOF))/$(strip $(TYPE_of_DATA))/$(strip $(NITER_ATT))/$(strip $(FS_KFOLD))/procdata
	Rscript analysis/Kfold_split_procdata.R \
			$(strip $(TYPE_of_DATA)) \
			$(strip $(DIR_LOF))/$(strip $(TYPE_of_DATA))/$(strip $(NITER_ATT))/$(strip $(FS_KFOLD))/procdata \
			$(strip $(FS_KFOLD))
	date > $@
	@echo ""
endef


