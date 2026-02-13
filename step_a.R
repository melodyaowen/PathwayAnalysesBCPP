source("./funs_a.R")
data_full <- read.csv("./data_full.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_negative <- read.csv("./data_hiv_negative.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_negative_treated <- read.csv("./data_hiv_negative_treated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_negative_untreated <- read.csv("./data_hiv_negative_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_negative_males <- read.csv("./data_hiv_negative_males.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_negative_males_treated <- read.csv("./data_hiv_negative_males_treated.csv", 
                                            row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_positive <- read.csv("./data_hiv_positive.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_positive_treated <- read.csv("./data_hiv_positive_treated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
data_hiv_positive_untreated <- read.csv("./data_hiv_positive_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.)))
modes <- data_full %>%
  dplyr::select(marital_status, education, monthly_income) %>%
  dplyr::summarise(across(
    everything(),
    ~ names(sort(table(.[!is.na(.)]), decreasing = TRUE))[1]
  ))
data_full_complete <- data_full %>% 
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_negative_complete <- data_hiv_negative %>% 
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_negative_treated_complete <- data_hiv_negative_treated %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_negative_untreated_complete <- data_hiv_negative_untreated %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_negative_males_complete <- data_hiv_negative_males %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_negative_males_treated_complete <- data_hiv_negative_males_treated %>%
  mutate(marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_positive_complete <- data_hiv_positive %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_positive_treated_complete <- data_hiv_positive_treated %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
data_hiv_positive_untreated_complete <- data_hiv_positive_untreated %>%
  mutate(
    marital_status = ifelse(is.na(marital_status), paste0(modes$marital_status), marital_status),
    education = ifelse(is.na(education), paste0(modes$education), education),
    monthly_income = ifelse(is.na(monthly_income), paste0(modes$monthly_income), monthly_income),
    alcohol_weekly = ifelse(is.na(alcohol_weekly), 
                            mean(alcohol_weekly, na.rm = TRUE), alcohol_weekly),
    partners_12mos = ifelse(is.na(partners_12mos), 
                            mean(partners_12mos, na.rm = TRUE), partners_12mos)
  )
allCovars = c("gender", "age", "marital_status", "education", "monthly_income", "alcohol_weekly", "partners_12mos", "prop_began_infected", "prop_male", "prop_vlsupp")
allInteractions = c("T_k*gender",
                    "T_k*age",
"T_k*monthly_income","T_k*education",
                    "T_k*prop_began_infected"
                    )
allInteractionTerms <- c("gender",
                         "age",
   "monthly_income", "education","prop_began_infected"
                         )
currentRoundNumber <- 2
hiv_confounder_output <- identifyConfounding(
  myData = data_hiv_negative_complete,
  myCluster = "cluster_id",
  myOutcome = "Y1_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
hiv_confounders <- hiv_confounder_output$finalCovariates$ID
mortality_confounder_output <- identifyConfounding(
  myData = data_hiv_positive_complete,
  myCluster = "cluster_id",
  myOutcome = "Y2_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
mortality_confounders <- mortality_confounder_output$finalCovariates$ID
vmmc_confounder_output <- identifyMediatorConfounding(
  myData = data_hiv_negative_males_complete,
  myCluster = "cluster_id",
  myComponent = "X1_ik",
  myTreatment = "T_k",
  myCovariates = setdiff(allCovars, "gender"),
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
vmmc_confounders <- vmmc_confounder_output$finalCovariates$ID
htc_confounder_output <- identifyMediatorConfounding(
  myData = data_full_complete,
  myCluster = "cluster_id",
  myComponent = "X2_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
htc_confounders <- htc_confounder_output$finalCovariates$ID
art_confounder_output <- identifyMediatorConfounding(
  myData = data_hiv_positive_complete,
  myCluster = "cluster_id",
  myComponent = "X3_ik",
  myTreatment = "T_k",
  myCovariates = allCovars,
  pthresh = 0.2,
  roundNumber = currentRoundNumber)
art_confounders <- art_confounder_output$finalCovariates$ID
overall_hiv_unadjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik",
  myComponentProp = "Zany_k_hivneg",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL, 
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
  )
write_xlsx(overall_hiv_unadjusted, 
           "./out/hiv_overall_unadjusted.xlsx")
overall_hiv_adjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik", 
  myComponentProp = "Zany_k_hivneg",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = c(hiv_confounders),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
  )
write_xlsx(overall_hiv_adjusted, 
           "./out/hiv_overall_adjusted.xlsx")
overall_hiv_interaction <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik",
  myComponentProp = "Zany_k_hivneg",
  myOutcome = "Y1_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = c(hiv_confounders),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
  )
write_xlsx(overall_hiv_interaction, 
           "./out/hiv_overall_interaction.xlsx")
individual_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_males_treated_complete,
  mySubjectID = "subject_id", 
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X1_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_vmmc_unadjusted, 
           "./out/vmmc_individual_unadjusted.xlsx")
individual_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_males_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X1_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = setdiff(unique(c(hiv_confounders, vmmc_confounders)), 
                         "gender"),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_vmmc_adjusted, 
           "./out/vmmc_individual_adjusted.xlsx")
individual_vmmc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_males_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X1_ik",
  myOutcome = "Y1_ik",
  modelSelect = "Yes",
  myCovariates = setdiff(unique(c(hiv_confounders, vmmc_confounders, allInteractionTerms)), 
                         "gender"),
  myInteractions = setdiff(allInteractions, "T_k*gender"),
  myForcedTerms = setdiff(unique(c(hiv_confounders, vmmc_confounders)), 
                          "gender"),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_vmmc_interaction,
           "./out/vmmc_individual_interaction.xlsx")
spillover_vmmc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z1_k",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_vmmc_unadjusted, 
           "./out/vmmc_spillover_unadjusted.xlsx")
spillover_vmmc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k", 
  myComponent = "Z1_k",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, vmmc_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_vmmc_adjusted, 
           "./out/vmmc_spillover_adjusted.xlsx")
spillover_vmmc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k", 
  myComponent = "Z1_k",
  myOutcome = "Y1_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, vmmc_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, vmmc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_vmmc_interaction,
           "./out/vmmc_spillover_interaction.xlsx")
individual_htc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_htc_unadjusted, 
           "./out/htc_individual_unadjusted.xlsx")
individual_htc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, htc_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_htc_adjusted, 
           "./out/htc_individual_adjusted.xlsx")
individual_htc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_treated_complete, 
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y1_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, htc_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_htc_interaction, 
           "./out/htc_individual_interaction.xlsx")
spillover_htc_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_htc_unadjusted, 
           "./out/htc_spillover_unadjusted.xlsx")
spillover_htc_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, htc_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_htc_adjusted, 
           "./out/htc_spillover_adjusted.xlsx")
spillover_htc_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y1_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(hiv_confounders, htc_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(hiv_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_htc_interaction, 
           "./out/htc_spillover_interaction.xlsx")
overall_mortality_unadjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik",
  myComponentProp = "Zany_k_hivpos",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_mortality_unadjusted, 
           "./out/mortality_overall_unadjusted.xlsx")
overall_mortality_adjusted <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik",
  myComponentProp = "Zany_k_hivpos",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = c(mortality_confounders),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_mortality_adjusted, 
           "./out/mortality_overall_adjusted.xlsx")
overall_mortality_interaction <- runOverallAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Xany_ik",
  myComponentProp = "Zany_k_hivpos",
  myOutcome = "Y2_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = c(mortality_confounders),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(overall_mortality_interaction, 
           "./out/mortality_overall_interaction.xlsx")
individual_art_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X3_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_art_unadjusted, 
           "./out/art_individual_unadjusted.xlsx")
individual_art_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X3_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, art_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_art_adjusted, 
           "./out/art_individual_adjusted.xlsx")
individual_art_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X3_ik",
  myOutcome = "Y2_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, art_confounders, 
                          allInteractionTerms)), 
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, art_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_art_interaction, 
           "./out/art_individual_interaction.xlsx")
spillover_art_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, 
  myData = data_hiv_positive_untreated_complete, 
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z3_k",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_art_unadjusted, 
           "./out/art_spillover_unadjusted.xlsx")
spillover_art_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z3_k",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, art_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_art_adjusted, 
           "./out/art_spillover_adjusted.xlsx")
spillover_art_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z3_k",
  myOutcome = "Y2_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, art_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, art_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_art_interaction, 
           "./out/art_spillover_interaction.xlsx")
individual_htc_mortality_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(individual_htc_mortality_unadjusted, 
           "./out/htc_mortality_individual_unadjusted.xlsx")
individual_htc_mortality_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber, 
  myData = data_hiv_positive_treated_complete, 
  mySubjectID = "subject_id", 
  myClusterID = "cluster_id", 
  myTreatment = "T_k", 
  myComponent = "X2_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, htc_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(individual_htc_mortality_adjusted, 
           "./out/htc_mortality_individual_adjusted.xlsx")
individual_htc_mortality_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_treated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y2_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, htc_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(individual_htc_mortality_interaction, 
           "./out/htc_mortality_individual_interaction.xlsx")
spillover_htc_mortality_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(spillover_htc_mortality_unadjusted, 
           "./out/htc_mortality_spillover_unadjusted.xlsx")
spillover_htc_mortality_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, htc_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(spillover_htc_mortality_adjusted, 
           "./out/htc_mortality_spillover_adjusted.xlsx")
spillover_htc_mortality_interaction <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_untreated_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "Z2_k",
  myOutcome = "Y2_ik",
  modelSelect = "Yes",
  myCovariates = unique(c(mortality_confounders, htc_confounders, 
                          allInteractionTerms)),
  myInteractions = allInteractions,
  myForcedTerms = unique(c(mortality_confounders, htc_confounders)),
  myCutoff = 0.05,
  mySelectionCriteria = "LRT"
)
write_xlsx(spillover_htc_mortality_interaction, 
           "./out/htc_mortality_spillover_interaction.xlsx")
overall_vmmc_hiv_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_males_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X1_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(overall_vmmc_hiv_unadjusted, 
           "./out/hiv_vmmc_overall_unadjusted.xlsx")
overall_vmmc_hiv_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_males_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X1_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = setdiff(unique(c(hiv_confounders, vmmc_confounders)), 
                         "gender"),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_vmmc_hiv_adjusted, 
           "./out/hiv_vmmc_overall_adjusted.xlsx")
overall_htc_hiv_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(overall_htc_hiv_unadjusted, 
           "./out/hiv_htc_overall_unadjusted.xlsx")
overall_htc_hiv_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_negative_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y1_ik",
  modelSelect = "No",
  myCovariates = unique(c(hiv_confounders, htc_confounders)), 
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_htc_hiv_adjusted, 
           "./out/hiv_htc_overall_adjusted.xlsx")
overall_art_mortality_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X3_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(overall_art_mortality_unadjusted, 
           "./out/mortality_art_overall_unadjusted.xlsx")
overall_art_mortality_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X3_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, art_confounders)),
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_art_mortality_adjusted, 
           "./out/mortality_art_overall_adjusted.xlsx")
overall_htc_mortality_unadjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k", 
  myComponent = "X2_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = NULL,
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL
)
write_xlsx(overall_htc_mortality_unadjusted, 
           "./out/mortality_htc_overall_unadjusted.xlsx")
overall_htc_mortality_adjusted <- runMediationAnalysis(
  roundNumber = currentRoundNumber,
  myData = data_hiv_positive_complete,
  mySubjectID = "subject_id",
  myClusterID = "cluster_id",
  myTreatment = "T_k",
  myComponent = "X2_ik",
  myOutcome = "Y2_ik",
  modelSelect = "No",
  myCovariates = unique(c(mortality_confounders, htc_confounders)), 
  myInteractions = NULL,
  myForcedTerms = NULL,
  myCutoff = NULL,
  mySelectionCriteria = NULL
)
write_xlsx(overall_htc_mortality_adjusted, 
           "./out/mortality_htc_overall_adjusted.xlsx")


