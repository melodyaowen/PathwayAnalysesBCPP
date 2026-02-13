runMediationAnalysis <- function(roundNumber, myData, mySubjectID, myClusterID, myTreatment, myComponent, myOutcome, modelSelect = "No", myCovariates, myInteractions, myForcedTerms = NULL, myCutoff = NULL, mySelectionCriteria = "p"){
  rhs_total <- paste(c(myTreatment, myCovariates, myInteractions), collapse = " + ")
  formula_total <- stats::as.formula(paste(myOutcome, "~", rhs_total))
  rhs_direct <- paste(c(myTreatment, myComponent, myCovariates, myInteractions), collapse = " + ")
  formula_direct <- stats::as.formula(paste(myOutcome, "~", rhs_direct))
  modelData <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID),
                  cluster_id = all_of(myClusterID),
                  all_of(myTreatment), 
                  all_of(myComponent), 
                  all_of(myOutcome), 
                  all_of(myCovariates))
  mf_complete <- model.frame(formula_direct, data = modelData, na.action = na.omit)
  usedData <- modelData[rownames(mf_complete), , drop = FALSE]
if((is.null(myCovariates) & is.null(myInteractions)) | modelSelect == "No"){
    
    cat("Fitting model with no variable selection.\n\n")
    model_total_glmm <- glmer(update(formula_total, . ~ . + (1 | cluster_id)),
                              family = binomial(link = "logit"),
                              data = usedData)
    length_temp <- max(length(c(myCovariates, myInteractions)))
    formula_total_updated <- formula_total
    model_direct_glmm <- glmer(update(formula_direct, . ~ . + (1 | cluster_id)), family = binomial(link = "logit"),
                               data = usedData)
    formula_direct_updated <- formula_direct
  } else if((!is.null(myCovariates) | !is.null(myInteractions)) & modelSelect == "Yes"){
    if(mySelectionCriteria != "LRT" & mySelectionCriteria != "p"){
      stop("Must select selection criteria 'LRT' or 'p'.")
    }
    cat(paste0("Performing variable selection based off of ", mySelectionCriteria, "\n\n"))
    model_total_glmm_list <- backward_glmer_p(myFormulaGLMM = formula_total,
                                              myDataGLMM = usedData,
                                              clusterID = myClusterID,
                                              required_terms = c(myTreatment, 
                                                                 myForcedTerms),
                                              pCutoff = myCutoff,
                                              selectionCriteria = mySelectionCriteria)
    length_temp <- max(length(c(myCovariates, myInteractions)),
                       length(c(model_total_glmm_list[[2]])),
                       length(c(model_total_glmm_list[[3]])))
    model_total_glmm <- model_total_glmm_list[[1]]
    formula_total_updated <- update(formula(model_total_glmm_list[[1]]), 
                                    paste(". ~ . - (1|", myClusterID, ")", sep=""))
    formula_direct_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
                                     paste(myComponent, sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
                                                            paste0(formula_total_updated)), sep = " + "))
    model_direct_glmm <- glmer(update(stats::as.formula(formula_direct_updated), 
                                      . ~ . + (1 | cluster_id)),
                               family = binomial(link = "logit"),
                               data = usedData)
  }
  tidy_total_glmm_full <- broom.mixed::tidy(model_total_glmm, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Model = "GLMM", 
           Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(model_total_glmm, tolerance = 0)$ICC_adjusted[[1]])
  tidy_total_glmm <- dplyr::select(tidy_total_glmm_full, -Variance, -SE)
  
  tidy_total_glmm_or <- tidy_total_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  table_total <- tidy_total_glmm
  table_total_or <- tidy_total_glmm_or
  
  table_total_full <- tidy_total_glmm_full
tidy_direct_glmm_full <- broom.mixed::tidy(model_direct_glmm, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Model = "GLMM", 
           Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(model_direct_glmm, tolerance = 0)$ICC_adjusted[[1]])
  tidy_direct_glmm <- dplyr::select(tidy_direct_glmm_full, -Variance, -SE)
  
  tidy_direct_glmm_or <- tidy_direct_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  table_direct <- tidy_direct_glmm
  table_direct_or <- tidy_direct_glmm_or
  
  table_direct_full <- tidy_direct_glmm_full
  listIEandPM_glmm <- calcIEandPM(formula = stats::as.formula(formula_direct_updated), 
                                       exposure = myTreatment, 
                                       mediator = myComponent, 
                                       df = usedData, 
                                       cluster = myClusterID,
                                       family = binomial(link = "logit"),
                                       corstr = "independence", 
                                       conf.level = 0.95, 
                                       pres = "sep", 
                                       niealternative = "two-sided")
  de_full_info_covars <- table_direct_full %>%
    mutate(Effect = "Direct") %>%
    relocate(Model, Term, Effect)
  
  de_full_info_covars_or <- de_full_info_covars %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  de_full_info <- de_full_info_covars %>%
    dplyr::filter(Term == myTreatment)
  
  ie_full_info <- listIEandPM_glmm$indirectDat %>%
    mutate(Effect = "Indirect") %>%
    relocate(Model, Term, Effect) %>%
    mutate(ICC = NA)
  
  te_full_info_covars <- table_total_full %>%
    mutate(Effect = "Total") %>%
    relocate(Model, Term, Effect)
  
  te_full_info_covars_or <- te_full_info_covars %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    rename(`Estimate (OR)` = Estimate)
  
  te_full_info <- te_full_info_covars %>%
    dplyr::filter(Term == myTreatment)
    
  pm_full_info <- listIEandPM_glmm$pmDat %>%
    mutate(Effect = "PM") %>%
    relocate(Model, Term, Effect) %>%
    mutate(ICC = NA)
  
  full_table <- bind_rows(de_full_info,
                          ie_full_info,
                          te_full_info,
                          pm_full_info) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "PM")))
  
  iepm_test <- full_join(rename(dplyr::select(te_full_info, Model, 
                                              Term, Estimate), TE = Estimate), 
                         rename(dplyr::select(de_full_info, Model, 
                                              Term, Estimate), DE = Estimate), 
                         by = c("Model", "Term")) %>%
    mutate(`Indirect Calculated` = TE - DE,
           `PM Calculated` = `Indirect Calculated`/TE) %>%
    dplyr::select(-TE, -DE) %>%
    pivot_longer(cols = c("Indirect Calculated", "PM Calculated"), 
                 values_to = "Estimate", names_to = "Effect")
  full_table_test <- bind_rows(full_table, iepm_test) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated")),
            factor(Model, levels = c("GLM", "GLMM", "GEE")))
  full_table_or <- full_table_test %>%
    mutate(Estimate = ifelse(Effect == "PM" | Effect == "PM Calculated", Estimate, exp(Estimate)),
           Lower = ifelse(Effect == "PM" | Effect == "PM Calculated", Lower, exp(Lower)),
           Upper = ifelse(Effect == "PM" | Effect == "PM Calculated", Upper, exp(Upper))) %>%
    rename(`Estimate (OR)` = Estimate) %>%
    arrange(factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated")),
            factor(Model, levels = c("GLM", "GLMM", "GEE")))
  de_full_info_covars_return <- de_full_info_covars %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Direct Effects Model") %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  de_full_info_covars_or_return <- de_full_info_covars_or %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Direct Effects Model") %>%
    dplyr::select(Model, Term, `OR (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  te_full_info_covars_return <- te_full_info_covars %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Total Effects Model") %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  te_full_info_covars_or_return <- te_full_info_covars_or %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    mutate(Model = "Total Effects Model") %>%
    dplyr::select(Model, Term, `OR (95% CI)`, 
                  `p-value`, ICC, Variance, SE)
  full_table_return <- full_table_test %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    arrange(factor(Model, levels = c("GLM", "GLMM", "GEE")),
            factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated"))) %>%
    mutate(`Estimate (95% CI)` = paste0(`Estimate`, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, Effect, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  full_table_or_return <- full_table_or %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    arrange(factor(Model, levels = c("GLM", "GLMM", "GEE")),
            factor(Effect, levels = c("Total", "Direct", "Indirect", "Indirect Calculated",
                                      "PM", "PM Calculated"))) %>%
    mutate(`OR (95% CI)` = paste0(`Estimate (OR)`, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, Effect, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  output_list <- list(`All Estimates` = full_table_return,
                      `All Estimates (OR)` = full_table_or_return,
                      `Total Effects` = te_full_info_covars_return,
                      `Total Effects (OR)` = te_full_info_covars_or_return,
                      `Direct Effects` = de_full_info_covars_return,
                      `Direct Effects (OR)` = de_full_info_covars_or_return,
                      )
  return(output_list)
}
runOverallAnalysis <- function(roundNumber, myData, mySubjectID, myClusterID, myTreatment, myComponent, myComponentProp, myOutcome, modelSelect = "No", myCovariates, myInteractions, myForcedTerms = NULL, myCutoff = NULL, mySelectionCriteria = "p"){
  modelDataOverall <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID),
                  cluster_id = all_of(myClusterID),
                  all_of(myTreatment), 
                  all_of(myComponent),
                  all_of(myComponentProp),
                  all_of(myOutcome), 
                  all_of(myCovariates))
  modelDataIndividual <- modelDataOverall %>%
    dplyr::filter(!!sym(myComponent) == 1)
  
  modelDataSpillover <- modelDataOverall %>%
    dplyr::filter(!!sym(myComponent) == 0)
  rhs_overall <- paste(c(myTreatment, myCovariates, myInteractions), collapse = " + ")
  formula_overall <- stats::as.formula(paste(myOutcome, "~", rhs_overall))
  rhs_individual <- paste(c(myTreatment, 
                            myCovariates, myInteractions), collapse = " + ")
  formula_individual <- stats::as.formula(paste(myOutcome, "~", rhs_individual))
  rhs_spillover <- paste(c(myTreatment,
                           myCovariates, myInteractions), collapse = " + ")
  formula_spillover <- stats::as.formula(paste(myOutcome, "~", rhs_spillover))
  p1 <- sum(modelDataOverall[[myComponentProp]])/nrow(modelDataOverall)
  Zany_k_bar_X1 <- mean(modelDataIndividual[[myComponentProp]])
  Zany_k_bar_X0 <- mean(modelDataSpillover[[myComponentProp]])
  if((is.null(myCovariates) & is.null(myInteractions)) | modelSelect == "No"){
    cat("Fitting models with no variable selection.\n\n")
    model_overall_glmm <- glmer(update(formula_overall, . ~ . + (1 | cluster_id)),
                                family = binomial(link = "logit"),
                                data = modelDataOverall)
    model_individual_glmm <- glmer(update(formula_individual, . ~ . + (1 | cluster_id)),
                                   family = binomial(link = "logit"),
                                   data = modelDataIndividual)
    model_spillover_glmm <- glmer(update(formula_spillover, . ~ . + (1 | cluster_id)),
                                  family = binomial(link = "logit"),
                                  data = modelDataSpillover)
    length_temp <- max(length(c(myCovariates, myInteractions)))
    formula_overall_updated <- formula_overall
    formula_individual_updated <- formula_individual
    formula_spillover_updated <- formula_spillover
  } else if((!is.null(myCovariates) | !is.null(myInteractions)) & modelSelect == "Yes"){
    
    if(is.null(mySelectionCriteria)){
      stop("Must select selection criteria 'LRT' or 'p'.")
    } else if(mySelectionCriteria != "LRT" & mySelectionCriteria != "p"){
      stop("Must select selection criteria 'LRT' or 'p'.")
    }
    model_overall_glmm_list <- backward_glmer_p(myFormulaGLMM = formula_overall,
                                                myDataGLMM = modelDataOverall,
                                                clusterID = myClusterID,
                                                required_terms = c(myTreatment, 
                                                                   myForcedTerms),
                                                pCutoff = myCutoff,
                                                selectionCriteria = mySelectionCriteria)
    length_temp <- max(length(c(myCovariates, myInteractions)),
                       length(c(model_overall_glmm_list[[2]])),
                       length(c(model_overall_glmm_list[[3]])))
    model_overall_glmm <- model_overall_glmm_list[[1]]
    formula_overall_updated <- update(formula(model_overall_glmm_list[[1]]), 
                                      paste(". ~ . - (1|", myClusterID, ")", sep=""))
    formula_individual_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
        paste(sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
              paste0(formula_overall_updated)), sep = " + "))
    formula_spillover_updated <- paste0(paste(myOutcome, "~", myTreatment, "+ "),
        paste(sub(paste0(".*\\b", myTreatment, "\\b\\s*\\+\\s*"), "", 
              paste0(formula_overall_updated)), sep = " + "))
    model_individual_glmm <- glmer(update(stats::as.formula(formula_individual_updated), 
                                          . ~ . + (1 | cluster_id)),
                                   family = binomial(link = "logit"),
                                   data = modelDataIndividual)
    model_spillover_glmm <- glmer(update(stats::as.formula(formula_spillover_updated), 
                                         . ~ . + (1 | cluster_id)),
                                  family = binomial(link = "logit"),
                                  data = modelDataSpillover)
  }
  tidy_overall_glmm <- broom.mixed::tidy(model_overall_glmm, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Model = "GLMM", 
           Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(model_overall_glmm, tolerance = 0)$ICC_adjusted[[1]])
  overall_effect_glmm <- dplyr::filter(tidy_overall_glmm, Term == "T_k")$Estimate
  tidy_individual_glmm <- broom.mixed::tidy(model_individual_glmm, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Model = "GLMM", 
           Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(model_individual_glmm, tolerance = 0)$ICC_adjusted[[1]])
  individual_effect_glmm <- dplyr::filter(tidy_individual_glmm, Term == myTreatment)$Estimate
  tidy_spillover_glmm <- broom.mixed::tidy(model_spillover_glmm, effects = "fixed") %>%
    dplyr::select(term, estimate, std.error, p.value) %>%
    mutate(conf.low  = estimate - qnorm(0.975) * std.error,
           conf.high = estimate + qnorm(0.975) * std.error) %>%
    mutate(Model = "GLMM", 
           Term = term,
           Estimate = estimate,
           Lower = conf.low,
           Upper = conf.high,
           `p-value` = p.value,
           Variance = std.error^2,
           SE = std.error) %>%
    dplyr::select(Model, Term, Estimate, Lower, Upper, `p-value`, Variance, SE) %>%
    dplyr::filter(Term != "(Intercept)") %>%
    mutate(ICC = performance::icc(model_spillover_glmm, tolerance = 0)$ICC_adjusted[[1]])
  spillover_effect_glmm <- dplyr::filter(tidy_spillover_glmm, Term == myTreatment)$Estimate
  overall_table <- bind_rows(mutate(tidy_overall_glmm, Model = "Overall GLMM"), 
                             mutate(tidy_individual_glmm, Model = "Individual GLMM"),
                             mutate(tidy_spillover_glmm, Model = "Spillover GLMM") 
                             ) %>%
    dplyr::filter(Term == myTreatment) %>%
    mutate(or = exp(Estimate),
           or.lower = exp(Lower),
           or.upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(`Log-Odds Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    mutate(`OR (95% CI)` = paste0(or, " (", or.lower, ", ", or.upper, ")")) %>%
    dplyr::select(Model, Term, `Log-Odds Estimate (95% CI)`, `OR (95% CI)`,
                  `p-value`, Variance, SE, ICC)
  decompositionTable <- tibble(`Estimate Form` = c("Log-Odds", "OR"),
                               Overall = c(overall_effect_glmm, exp(overall_effect_glmm)),
                               Spillover = c(spillover_effect_glmm, exp(spillover_effect_glmm)),
                               Individual = c(individual_effect_glmm, exp(individual_effect_glmm)),
                               `Overall Calculated` = c(p1*individual_effect_glmm + (1-p1)*spillover_effect_glmm,
                                                        exp(p1*individual_effect_glmm + (1-p1)*spillover_effect_glmm))
  )
  
  # Formatting total model output for overall GLMM
  returnOverallTable <- tidy_overall_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Overall Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnOverallTable_or <- tidy_overall_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Overall Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  returnIndividualTable <- tidy_individual_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Individual Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnIndividualTable_or <- tidy_individual_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Individual Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  returnSpilloverTable <- tidy_spillover_glmm %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Spillover Effects Model",
           `Estimate (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `Estimate (95% CI)`, `p-value`, ICC, Variance, SE)
  returnSpilloverTable_or <- tidy_spillover_glmm %>%
    mutate(Estimate = exp(Estimate),
           Lower = exp(Lower),
           Upper = exp(Upper)) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, digits = roundNumber))) %>%
    mutate(Model = "Spillover Effects Model",
           `OR (95% CI)` = paste0(Estimate, " (", Lower, ", ", Upper, ")")) %>%
    dplyr::select(Model, Term, `OR (95% CI)`, `p-value`, ICC, Variance, SE)
  frequencyData <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponent),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                              ifelse(Treatment == 0, "Control", NA)),
           Outcome = ifelse(Outcome == 1, "Yes", ifelse(Outcome == 0, "No", "Missing")),
           Component = ifelse(Component == 1, "Yes", ifelse(Component == 0, "No", "Missing"))) %>%
    mutate(across(everything(), tidyr::replace_na, "Missing")) %>%
    
    mutate(across(everything(), 
                  ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                        "Control", "Treatment", "Missing")))) %>%
    group_by(across(everything())) %>%
    dplyr::summarize(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    pivot_wider(
      names_from = Component,
      values_from = Count
    ) %>%
    arrange(Treatment, Outcome) %>%
    mutate(across(where(is.numeric), tidyr::replace_na, 0)) %>%
    mutate(`Total` = rowSums(across(where(is.numeric) & !any_of("Missing")), 
                             na.rm = TRUE)) %>%
    relocate(any_of("Missing"), .after = last_col())
  frequencyDataTotals <- frequencyData %>%
    filter(Outcome != "Missing") %>%
    group_by(Treatment) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Outcome = "Total", .before = 1)
  frequencyOutput <- bind_rows(frequencyData, frequencyDataTotals) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  distDataProp <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                              ifelse(Treatment == 0, "Control", NA)),
           Outcome = ifelse(Outcome == 1, "Yes", ifelse(Outcome == 0, "No", "Missing"))) %>%
    mutate(across(where(is.character), tidyr::replace_na, "Missing")) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                        "Control", "Treatment", "Total", "Missing")))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     Count = n(),
                     .groups = "drop")
  distDataTotalProp <- modelDataOverall %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == 1, "Treatment", 
                              ifelse(Treatment == 0, "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", "Total")) %>%
    mutate(across(where(is.character), tidyr::replace_na, "Missing")) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = c("Yes", "No", "Female", "Began study HIV-infected", 
                                        "Control", "Treatment", "Total", "Missing")))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     Count = n(),
                     .groups = "drop") %>%
    dplyr::filter(Outcome != "Missing")
  distOutputProp <- bind_rows(distDataProp, distDataTotalProp) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  returnList <- list(`Decomposition` = decompositionTable,
                     
                     `Overall Effects` = returnOverallTable,
                     `Overall Effects (OR)` = returnOverallTable_or,
                     
                     `Individual Effects` = returnIndividualTable,
                     `Individual Effects (OR)` = returnIndividualTable_or,
                     
                     `Spillover Effects` = returnSpilloverTable,
                     `Spillover Effects (OR)` = returnSpilloverTable_or
  )
}