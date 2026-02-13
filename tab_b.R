data_full <- read.csv("./data_full.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_negative <- read.csv("./data_hiv_negative.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_negative_treated <- read.csv("./data_hiv_negative_treated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_negative_untreated <- read.csv("./data_hiv_negative_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_negative_males <- read.csv("./data_hiv_negative_males.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_negative_males_treated <- read.csv("./data_hiv_negative_males_treated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_positive <- read.csv("./data_hiv_positive.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_positive_treated <- read.csv("./data_hiv_positive_treated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
data_hiv_positive_untreated <- read.csv("./data_hiv_positive_untreated.csv", row.names = 1) %>%
  mutate(across(where(is.logical), ~ as.numeric(.))) %>%
  mutate(alcohol_weekly = as.character(alcohol_weekly),
         partners_12mos = as.character(partners_12mos))
createBaselineTable <- function(roundNumber, myData, mySubjectID = "subject_id", myClusterID = "cluster_id", myClusterSize = "cluster_size", myTreatmentGroup = "T_k", myVarList, myVarListLabels, myLevelOrder, myClusterList, myClusterLabels){
  baselineTable <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID), cluster_id = all_of(myClusterID), 
                  cluster_size = all_of(myClusterSize), treatment_group = all_of(myTreatmentGroup),
                  all_of(myVarList)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                     ifelse(treatment_group == 0, "Control",
                                            NA))) %>%
    mutate(across(
      where(~ !is.numeric(.x)),
      ~ ifelse(is.na(.x), "Missing", as.character(.x))
    ))
  subjectCount <- baselineTable %>%
    dplyr::select(subject_id, treatment_group) %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Individuals",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  clusterCount <- baselineTable %>%
    dplyr::select(cluster_id, treatment_group) %>%
    distinct() %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Clusters",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
    missingColumns <- baselineTable %>%
    dplyr::select(subject_id, treatment_group,
                  where(~ (is.character(.) && any(. == "Missing", na.rm = TRUE)) |
                          (is.numeric(.)   && any(is.na(.))))) %>%
    mutate(across(-c(subject_id, treatment_group), 
                  ~ if (is.character(.)) {
                    ifelse(. == "Missing", "Missing", "Not Missing")
                    } else if (is.numeric(.)) {
                      ifelse(is.na(.), "Missing", "Not Missing")
                      } else {
                        .
                        }
                  )) %>%
    dplyr::select(-subject_id, -treatment_group) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    count(Variable, Status) %>%
    pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
    mutate(Overall = rowSums(across(c(any_of("Not Missing"), any_of("Missing"))), na.rm = TRUE)) %>%
    mutate(`Missing (Overall)` = paste0(Missing, " (", ifelse(round((Missing/Overall)*100, roundNumber) == 0, "<1", 
                                                              round((Missing/Overall)*100, roundNumber)), "%)")) %>%
    dplyr::select(Variable, `Missing (Overall)`)
  baselineTableNumeric <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.numeric(.) ))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Value") %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value, na.rm = TRUE),
                     SD = sd(Value, na.rm = TRUE),
                     .groups = "drop") %>%
    mutate(Summary = paste0(round(Mean, 0), " (", round(SD, roundNumber), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Summary) %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level)
  baselineTableNumericOverall <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.numeric(.) ))) %>%
    pivot_longer(cols = -treatment_group, 
                 names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>%
    dplyr::summarise(Mean = mean(Value, na.rm = TRUE),
                     SD   = sd(Value, na.rm = TRUE),
                     .groups = "drop") %>%
    mutate(Overall = paste0(round(Mean, roundNumber), " (", round(SD, roundNumber), ")")) %>%
    dplyr::select(-Mean, -SD)
  baselineTableNumericFinal <- baselineTableNumeric %>%
    full_join(baselineTableNumericOverall, by = "Variable")
  baselineTableCharacter <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id) %>%
    dplyr::select(treatment_group,
                  where(~ (is.character(.) ))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, treatment_group, Level) %>%
    filter(Level != "Missing") %>%
    group_by(treatment_group, Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable, treatment_group) %>%
    group_by(treatment_group, Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Value = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                          round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  baselineTableCharacterOverall <- baselineTable %>%
    dplyr::select(-subject_id, -cluster_id, -treatment_group) %>%
    dplyr::select(where(~ (is.character(.) ))) %>%
    pivot_longer(cols = everything(),
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, Level) %>%
    filter(Level != "Missing") %>%
    group_by(Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Overall = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                            round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total)

  baselineTableCharacterFinal <- baselineTableCharacter %>%
    full_join(baselineTableCharacterOverall, by = c("Variable", "Level"))
  clusterTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup),
                  all_of(myClusterList)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control",
                                           NA))) %>%
    pivot_longer(-treatment_group, names_to = "Variable", values_to = "Value") %>%
    arrange(treatment_group, Variable) %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Value = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                 round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  clusterTableOverall <- myData %>%
    dplyr::select(all_of(myClusterList)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Overall = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                   round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD)
  clusterTableFinal <- clusterTable %>%
    full_join(clusterTableOverall, by = "Variable") %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level) %>%
    mutate("Missing (Overall)" = NA)
  varListFinal <- c("Number of Individuals", "Number of Clusters", 
                    "cluster_size", myVarList, myClusterList)
  varListLabelsFinal <- c("Number of Individuals", "Number of Clusters",
                          "Cluster Size", myVarListLabels, myClusterLabels)
  outputTable <- subjectCount %>%
    bind_rows(clusterCount) %>%
    bind_rows(baselineTableNumericFinal) %>%
    bind_rows(baselineTableCharacterFinal) %>%
    full_join(missingColumns, by = "Variable") %>%
    bind_rows(clusterTableFinal) %>%
    mutate(Variable = factor(Variable, levels = varListFinal, 
                             labels = varListLabelsFinal)) %>%
    mutate(Level = factor(Level, levels = myLevelOrder)) %>%
    arrange(Variable, Level) %>%
    mutate(Variable = as.character(Variable),
           `Missing (Overall)` = as.character(`Missing (Overall)`)) %>%
    group_by(Variable) %>%
    mutate(`Missing (Overall)` = if_else(row_number() == 1, `Missing (Overall)`, ""),
           Variable            = if_else(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    mutate(`Missing (Overall)` = ifelse(is.na(`Missing (Overall)`), "", `Missing (Overall)`))
  return(outputTable)
}
createComponentOutcomeTable <- function(roundNumber, myData, mySubjectID = "subject_id", myClusterID = "cluster_id", myTreatmentGroup = "T_k", myComponentList, myComponentLabels, myComponentPropList, myComponentPropLabels, myOutcomeList, myOutcomeLabels, myLevelOrder
                                        ){
  subjectCount <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID), 
                  treatment_group = all_of(myTreatmentGroup)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Individuals",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  clusterCount <- myData %>%
    dplyr::select(cluster_id = all_of(myClusterID), 
                  treatment_group = all_of(myTreatmentGroup)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    distinct() %>%
    group_by(treatment_group) %>%
    dplyr::summarize(n = n()) %>%
    ungroup() %>%
    pivot_wider(names_from = treatment_group, values_from = n) %>%
    mutate(Variable = "Number of Clusters",
           Level = "--") %>%
    relocate(Variable, Level) %>%
    mutate(Overall = Control + Treatment) %>%
    mutate(Control = as.character(Control),
           Treatment = as.character(Treatment),
           Overall = as.character(Overall))
  componentOutcomeTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    pivot_longer(cols = -treatment_group,
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, treatment_group, Level) %>%
    filter(Level != "Missing") %>%
    group_by(treatment_group, Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable, treatment_group) %>%
    group_by(treatment_group, Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Value = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                          round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  
  componentOutcomeOverallTable <- myData %>%
    dplyr::select(all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    pivot_longer(cols = everything(),
                 names_to = "Variable", values_to = "Level") %>%
    arrange(Variable, Level) %>%
    filter(Level != "Missing") %>%
    group_by(Variable, Level) %>%
    dplyr::summarize(n = n(), .groups = "drop") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    mutate(Total = sum(n)) %>%
    ungroup() %>%
    mutate(Overall = paste0(n, " (", ifelse(round((n/Total)*100, roundNumber) == 0, "<1", 
                                            round((n/Total)*100, roundNumber)), "%", ")")) %>%
    dplyr::select(-n, -Total)
  
  componentOutcomeTableFinal <- componentOutcomeTable %>%
    full_join(componentOutcomeOverallTable, by = c("Variable", "Level"))
  componentPropTable <- myData %>%
    dplyr::select(treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentPropList)) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control",
                                           NA))) %>%
    pivot_longer(-treatment_group, names_to = "Variable", values_to = "Value") %>%
    arrange(treatment_group, Variable) %>%
    group_by(treatment_group, Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Value = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                 round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD) %>%
    pivot_wider(names_from = treatment_group, values_from = Value)
  
  componentPropOverallTable <- myData %>%
    dplyr::select(all_of(myComponentPropList)) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
    arrange(Variable) %>%
    group_by(Variable) %>%
    dplyr::summarize(Mean = mean(Value), SD = sd(Value),
                     .groups = "drop") %>%
    mutate(Overall = paste0(ifelse(round(Mean, 2) == 0, "<0.01", 
                                   round(Mean, 2)), " (", round(SD, 2), ")")) %>%
    dplyr::select(-Mean, -SD)
  
  componentPropTableFinal <- componentPropTable %>%
    full_join(componentPropOverallTable, by = "Variable") %>%
    mutate(Level = "Mean (SD)") %>%
    relocate(Variable, Level)
  missingColumns <- myData %>%
    dplyr::select(subject_id = all_of(mySubjectID),
                  treatment_group = all_of(myTreatmentGroup), 
                  all_of(myComponentList), all_of(myOutcomeList)) %>%
    mutate(across(all_of(myOutcomeList), ~ ifelse(. == 1, "Yes", 
                                                  ifelse(. == 0, "No", NA)))) %>%
    mutate(across(where(is.character), ~replace_na(., "Missing"))) %>%
    mutate(treatment_group = ifelse(treatment_group == 1, "Treatment",
                                    ifelse(treatment_group == 0, "Control", NA))) %>%
    mutate(across(-all_of(c("subject_id", "treatment_group")),
        ~ {
          if (is.character(.x) || is.factor(.x)) {
            ifelse(is.na(.x) | .x == "Missing", "Missing", "Not Missing")
          } else { ifelse(is.na(.x), "Missing", "Not Missing") }
        }
      )) %>%
    dplyr::select(-subject_id, -treatment_group) %>%
    dplyr::select(where(~ any(.x == "Missing", na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Status") %>%
    count(Variable, Status) %>%
    pivot_wider(names_from = Status, values_from = n, values_fill = 0) %>%
    mutate(Overall = rowSums(across(c(any_of("Missing"), any_of("Not Missing"))), na.rm = TRUE)) %>%
    mutate(`Missing (Overall)` = paste0(Missing, " (", ifelse(round((Missing/Overall)*100, roundNumber) == 0, "<1", 
                                                              round((Missing/Overall)*100, roundNumber)), "%)")) %>%
    dplyr::select(Variable, `Missing (Overall)`)
  varListFinal <- c("Number of Individuals", "Number of Clusters", 
                    myComponentList, myComponentPropList,
                    myOutcomeList)
  varListLabelsFinal <- c("Number of Individuals", "Number of Clusters",
                          myComponentLabels, myComponentPropLabels,
                          myOutcomeLabels)
  outputTable <- subjectCount %>%
    bind_rows(clusterCount) %>%
    bind_rows(componentOutcomeTableFinal) %>%
    bind_rows(componentPropTableFinal) %>%
    full_join(missingColumns, by = "Variable") %>%
    mutate(Variable = factor(Variable, levels = varListFinal, 
                             labels = varListLabelsFinal)) %>%
    mutate(Level = factor(Level, levels = myLevelOrder)) %>%
    arrange(Variable, Level) %>%
    mutate(Variable = as.character(Variable),
           `Missing (Overall)` = as.character(`Missing (Overall)`)) %>%
    group_by(Variable) %>%
    mutate(`Missing (Overall)` = if_else(row_number() == 1, `Missing (Overall)`, ""),
           Variable            = if_else(row_number() == 1, Variable, "")) %>%
    ungroup() %>%
    mutate(`Missing (Overall)` = ifelse(is.na(`Missing (Overall)`), "", `Missing (Overall)`))
  return(outputTable)
  
}
var_individual_list <- c("hiv_status_current",
                         "gender",
                         "age",
                         "marital_status",
                         "education",
                         "monthly_income",
                         "alcohol_weekly",
                         "partners_12mos"
                         )
var_individual_list_names <- c("HIV Status at Study Start",
                               "Gender",
                               "Age",
                               "Marital Status",
                               "Education",
                               "Monthly Income",
                               "Weekly Alcohol Consumption",
                               "Number of Partners (Last 12 Months)"
                               )
var_village_list <- c("prop_began_infected",
                      "prop_male",
                      "prop_vlsupp")
var_village_list_names <- c("Village Proportion of HIV Infected at Study Start",
                            "Village Proportion of Males in Cluster",
                            "Village Proportion Virally Suppressed Among HIV+")
var_individual_levels_order <- c("Yes", "No",
                                 
                                 "Male", "Female", 
                                 
                                 "HIV-uninfected", "HIV-infected", 
                                 "Refused HIV testing", 
                                 
                                 "Single/never married", "Married", 
                                 "Divorced/separated", "Widowed",
                                 
                                 "Non-formal", "Primary", "Junior secondary", 
                                 "Senior secondary", "Higher than senior secondary",
                                 
                                 "Employed", "Unemployed looking for work", 
                                 "Unemployed not looking for work",
                                 
                                 "No income", "1-199 pula", "200-499 pula", 
                                 "500-999 pula", "1000-4999 pula", 
                                 "5000-10000 pula", "More than 10000 pula", 
                                 
                                 "Less than 6 months", "6 months to 12 months", 
                                 "1 to 5 years", "More than 5 years", 
                                 
                                 "Strongly disagree", "Disagree", 
                                 "Agree", "Strongly agree", 
                                 
                                 "0", "1", "2", "3", "4", "5",
                                 
                                 "Mean (SD)", "--")

componentList <- c("endpoint_coverage_mc", "endpoint_coverage_htc", 
                   "endpoint_coverage_onart", "endpoint_coverage_any")
componentLabels <- c("Component 1: VMMC", "Component 2: HTC", "Component 3: ART",
                     "Received any component")

componentPropList <- c("Z1_k", "Z2_k", "Z3_k", "Zany_k_hivneg", "Zany_k_hivpos")
componentPropLabels <- c("Village Proportion of Men who received VMMC",
                         "Village Proportion of People who received HTC",
                         "Village Proportion of HIV+ who received ART",
                         "Village Proportion of HIV- who received at least 1 component",
                         "Village Proportion of HIV+ who received at least 1 component")

outcomeList <- c("Y1_ik", "Y2_ik")
outcomeLabels <- c("Outcome 1: HIV Seroconversion (3-year period)",
                   "Outcome 2: Mortality (3-year period)")

levelOrder <- c("Yes", "No", "Female", "HIV-infected", "Mean (SD)", "--")
baseline_full <- createBaselineTable(roundNumber = 0,
                                     myData = data_full,
                                     mySubjectID = "subject_id",
                                     myClusterID = "cluster_id",
                                     myClusterSize = "cluster_size",
                                     myTreatmentGroup = "T_k",
                                     myVarList = var_individual_list,
                                     myVarListLabels = var_individual_list_names,
                                     myClusterList = var_village_list,
                                     myClusterLabels = var_village_list_names,
                                     myLevelOrder = var_individual_levels_order)
component_outcome_full <- createComponentOutcomeTable(roundNumber = 0,
                                                      myData = data_full,
                                                      mySubjectID = "subject_id",
                                                      myClusterID = "cluster_id",
                                                      myTreatmentGroup = "T_k",
                                                      myComponentList = componentList,
                                                      myComponentLabels = componentLabels,
                                                      myComponentPropList = componentPropList,
                                                      myComponentPropLabels = componentPropLabels,
                                                      myOutcomeList = outcomeList,
                                                      myOutcomeLabels = outcomeLabels,
                                                      myLevelOrder = levelOrder)
# Create appropriate folders if they don't already exist on computer before next part
write.csv(baseline_full, "./out/baseline_full_0.csv")
write.csv(component_outcome_full, "./out/component_outcome_full_0.csv")
baseline_hiv_negative <- createBaselineTable(roundNumber = 0,
                                             myData = data_hiv_negative,
                                             mySubjectID = "subject_id",
                                             myClusterID = "cluster_id",
                                             myClusterSize = "cluster_size",
                                             myTreatmentGroup = "T_k",
                                             myVarList = var_individual_list,
                                             myVarListLabels = var_individual_list_names,
                                             myClusterList = var_village_list,
                                             myClusterLabels = var_village_list_names,
                                             myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative <- createComponentOutcomeTable(roundNumber = 0,
                                                      myData = data_hiv_negative,
                                                      mySubjectID = "subject_id",
                                                      myClusterID = "cluster_id",
                                                      myTreatmentGroup = "T_k",
                                                      myComponentList = componentList,
                                                      myComponentLabels = componentLabels,
                                                      myComponentPropList = componentPropList,
                                                      myComponentPropLabels = componentPropLabels,
                                                      myOutcomeList = outcomeList,
                                                      myOutcomeLabels = outcomeLabels,
                                                      myLevelOrder = levelOrder)
write.csv(baseline_hiv_negative, "./out/baseline_hiv_negative_1.csv")
write.csv(component_outcome_hiv_negative, "./out/component_outcome_hiv_negative_1.csv")
baseline_hiv_negative_treated <- createBaselineTable(roundNumber = 0,
                                                       myData = data_hiv_negative_treated,
                                                       mySubjectID = "subject_id",
                                                       myClusterID = "cluster_id",
                                                       myClusterSize = "cluster_size",
                                                       myTreatmentGroup = "T_k",
                                                       myVarList = var_individual_list,
                                                       myVarListLabels = var_individual_list_names,
                                                       myClusterList = var_village_list,
                                                       myClusterLabels = var_village_list_names,
                                                       myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_treated <- createComponentOutcomeTable(roundNumber = 0,
                                                                        myData = data_hiv_negative_treated,
                                                                        mySubjectID = "subject_id",
                                                                        myClusterID = "cluster_id",
                                                                        myTreatmentGroup = "T_k",
                                                                        myComponentList = componentList,
                                                                        myComponentLabels = componentLabels,
                                                                        myComponentPropList = componentPropList,
                                                                        myComponentPropLabels = componentPropLabels,
                                                                        myOutcomeList = outcomeList,
                                                                        myOutcomeLabels = outcomeLabels,
                                                                        myLevelOrder = levelOrder)
write.csv(baseline_hiv_negative_treated, "./out/baseline_hiv_negative_treated_1a.csv")
write.csv(component_outcome_hiv_negative_treated, "./out/component_outcome_hiv_negative_treated_1a.csv")
baseline_hiv_negative_untreated <- createBaselineTable(roundNumber = 0,
                                                       myData = data_hiv_negative_untreated,
                                                       mySubjectID = "subject_id",
                                                       myClusterID = "cluster_id",
                                                       myClusterSize = "cluster_size",
                                                       myTreatmentGroup = "T_k",
                                                       myVarList = var_individual_list,
                                                       myVarListLabels = var_individual_list_names,
                                                       myClusterList = var_village_list,
                                                       myClusterLabels = var_village_list_names,
                                                       myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_untreated <- createComponentOutcomeTable(roundNumber = 0,
                                                                        myData = data_hiv_negative_untreated,
                                                                        mySubjectID = "subject_id",
                                                                        myClusterID = "cluster_id",
                                                                        myTreatmentGroup = "T_k",
                                                                        myComponentList = componentList,
                                                                        myComponentLabels = componentLabels,
                                                                        myComponentPropList = componentPropList,
                                                                        myComponentPropLabels = componentPropLabels,
                                                                        myOutcomeList = outcomeList,
                                                                        myOutcomeLabels = outcomeLabels,
                                                                        myLevelOrder = levelOrder)
write.csv(baseline_hiv_negative_untreated, "./out/baseline_hiv_negative_untreated_1b.csv")
write.csv(component_outcome_hiv_negative_untreated, "./out/component_outcome_hiv_negative_untreated_1b.csv")
baseline_hiv_negative_males <- createBaselineTable(roundNumber = 0,
                                                   myData = data_hiv_negative_males,
                                                   mySubjectID = "subject_id",
                                                   myClusterID = "cluster_id",
                                                   myClusterSize = "cluster_size",
                                                   myTreatmentGroup = "T_k",
                                                   myVarList = var_individual_list,
                                                   myVarListLabels = var_individual_list_names,
                                                   myClusterList = var_village_list,
                                                   myClusterLabels = var_village_list_names,
                                                   myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_males <- createComponentOutcomeTable(roundNumber = 0,
                                                              myData = data_hiv_negative_males,
                                                              mySubjectID = "subject_id",
                                                              myClusterID = "cluster_id",
                                                              myTreatmentGroup = "T_k",
                                                              myComponentList = componentList,
                                                              myComponentLabels = componentLabels,
                                                              myComponentPropList = componentPropList,
                                                              myComponentPropLabels = componentPropLabels,
                                                              myOutcomeList = outcomeList,
                                                              myOutcomeLabels = outcomeLabels,
                                                              myLevelOrder = levelOrder)
write.csv(baseline_hiv_negative_males, "./out/baseline_hiv_negative_males_1c.csv")
write.csv(component_outcome_hiv_negative_males, "./out/component_outcome_hiv_negative_males_1c.csv")
baseline_hiv_negative_males_treated <- createBaselineTable(roundNumber = 0,
                                                   myData = data_hiv_negative_males_treated,
                                                   mySubjectID = "subject_id",
                                                   myClusterID = "cluster_id",
                                                   myClusterSize = "cluster_size",
                                                   myTreatmentGroup = "T_k",
                                                   myVarList = var_individual_list,
                                                   myVarListLabels = var_individual_list_names,
                                                   myClusterList = var_village_list,
                                                   myClusterLabels = var_village_list_names,
                                                   myLevelOrder = var_individual_levels_order)
component_outcome_hiv_negative_males_treated <- createComponentOutcomeTable(roundNumber = 0,
                                                                    myData = data_hiv_negative_males_treated,
                                                                    mySubjectID = "subject_id",
                                                                    myClusterID = "cluster_id",
                                                                    myTreatmentGroup = "T_k",
                                                                    myComponentList = componentList,
                                                                    myComponentLabels = componentLabels,
                                                                    myComponentPropList = componentPropList,
                                                                    myComponentPropLabels = componentPropLabels,
                                                                    myOutcomeList = outcomeList,
                                                                    myOutcomeLabels = outcomeLabels,
                                                                    myLevelOrder = levelOrder)
write.csv(baseline_hiv_negative_males_treated, "./out/baseline_hiv_negative_males_treated_1d.csv")
write.csv(component_outcome_hiv_negative_males_treated, "./out/component_outcome_hiv_negative_males_treated_1d.csv")
baseline_hiv_positive <- createBaselineTable(roundNumber = 0,
                                             myData = data_hiv_positive,
                                             mySubjectID = "subject_id",
                                             myClusterID = "cluster_id",
                                             myClusterSize = "cluster_size",
                                             myTreatmentGroup = "T_k",
                                             myVarList = var_individual_list,
                                             myVarListLabels = var_individual_list_names,
                                             myClusterList = var_village_list,
                                             myClusterLabels = var_village_list_names,
                                             myLevelOrder = var_individual_levels_order)
component_outcome_hiv_positive <- createComponentOutcomeTable(roundNumber = 0,
                                                              myData = data_hiv_positive,
                                                              mySubjectID = "subject_id",
                                                              myClusterID = "cluster_id",
                                                              myTreatmentGroup = "T_k",
                                                              myComponentList = componentList,
                                                              myComponentLabels = componentLabels,
                                                              myComponentPropList = componentPropList,
                                                              myComponentPropLabels = componentPropLabels,
                                                              myOutcomeList = outcomeList,
                                                              myOutcomeLabels = outcomeLabels,
                                                              myLevelOrder = levelOrder)
write.csv(baseline_hiv_positive, "./out/baseline_hiv_positive_2.csv")
write.csv(component_outcome_hiv_positive, "./out/component_outcome_hiv_positive_2.csv")
baseline_hiv_positive_treated <- createBaselineTable(roundNumber = 0,
                                                       myData = data_hiv_positive_treated,
                                                       mySubjectID = "subject_id",
                                                       myClusterID = "cluster_id",
                                                       myClusterSize = "cluster_size",
                                                       myTreatmentGroup = "T_k",
                                                       myVarList = var_individual_list,
                                                       myVarListLabels = var_individual_list_names,
                                                       myClusterList = var_village_list,
                                                       myClusterLabels = var_village_list_names,
                                                       myLevelOrder = var_individual_levels_order)
component_outcome_hiv_positive_treated <- createComponentOutcomeTable(roundNumber = 0,
                                                                        myData = data_hiv_positive_treated,
                                                                        mySubjectID = "subject_id",
                                                                        myClusterID = "cluster_id",
                                                                        myTreatmentGroup = "T_k",
                                                                        myComponentList = componentList,
                                                                        myComponentLabels = componentLabels,
                                                                        myComponentPropList = componentPropList,
                                                                        myComponentPropLabels = componentPropLabels,
                                                                        myOutcomeList = outcomeList,
                                                                        myOutcomeLabels = outcomeLabels,
                                                                        myLevelOrder = levelOrder)
write.csv(baseline_hiv_positive_treated, "./out/baseline_hiv_positive_treated_2a.csv")
write.csv(component_outcome_hiv_positive_treated, "./out/component_outcome_hiv_positive_treated_2a.csv")
baseline_hiv_positive_untreated <- createBaselineTable(roundNumber = 0,
                                                       myData = data_hiv_positive_untreated,
                                                       mySubjectID = "subject_id",
                                                       myClusterID = "cluster_id",
                                                       myClusterSize = "cluster_size",
                                                       myTreatmentGroup = "T_k",
                                                       myVarList = var_individual_list,
                                                       myVarListLabels = var_individual_list_names,
                                                       myClusterList = var_village_list,
                                                       myClusterLabels = var_village_list_names,
                                                       myLevelOrder = var_individual_levels_order)
component_outcome_hiv_positive_untreated <- createComponentOutcomeTable(roundNumber = 0,
                                                                        myData = data_hiv_positive_untreated,
                                                                        mySubjectID = "subject_id",
                                                                        myClusterID = "cluster_id",
                                                                        myTreatmentGroup = "T_k",
                                                                        myComponentList = componentList,
                                                                        myComponentLabels = componentLabels,
                                                                        myComponentPropList = componentPropList,
                                                                        myComponentPropLabels = componentPropLabels,
                                                                        myOutcomeList = outcomeList,
                                                                        myOutcomeLabels = outcomeLabels,
                                                                        myLevelOrder = levelOrder)
write.csv(baseline_hiv_positive_untreated, "./out/baseline_hiv_positive_untreated_2b.csv")
write.csv(component_outcome_hiv_positive_untreated, "./out/component_outcome_hiv_positive_untreated_2b.csv")

