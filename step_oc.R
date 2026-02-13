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
threeWayComponent <- function(myData,
                              myOutcome,
                              myComponent,
                              myTreatment){
  
  levelOrder <- c("Yes", "No", "Female", "Began study HIV-infected", 
                  "Control", "Treatment", "Missing")
  
  tableData <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponent),
                  Outcome = all_of(myOutcome)
                  ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", Outcome),
           Component = ifelse(is.na(Component), "Missing", Component)) %>%
    mutate(across(everything(), 
                  ~factor(., levels = levelOrder))) %>%
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
  
  tableDataTotals <- tableData %>%
    filter(Outcome != "Missing") %>%
    group_by(Treatment) %>%
    dplyr::summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    mutate(Outcome = "Total", .before = 1)
    
  
  tableOutput <- bind_rows(tableData, tableDataTotals) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
    
  return(tableOutput)
}
threeWayComponentProp <- function(myData,
                                  myOutcome,
                                  myComponentProp,
                                  myTreatment,
                                  roundNumber = 3){
  
  levelOrder <- c("Yes", "No", "Female", "Began study HIV-infected", 
                  "Control", "Treatment", "Total", "Missing")
  
  tableData <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", Outcome)) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = levelOrder))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     Count = n(),
                     .groups = "drop")
  
  tableDataTotal <- myData %>%
    dplyr::select(Treatment = all_of(myTreatment),
                  Component = all_of(myComponentProp),
                  Outcome = all_of(myOutcome)
    ) %>%
    mutate(Treatment = ifelse(Treatment == "Intervention", "Treatment", 
                              ifelse(Treatment == "Standard of Care", "Control", NA)),
           Outcome = ifelse(is.na(Outcome), "Missing", "Total")) %>%
    mutate(across(where(is.character), 
                  ~factor(., levels = levelOrder))) %>%
    group_by(Treatment, Outcome) %>%
    dplyr::summarize(Mean = round(mean(Component), roundNumber),
                     Min = round(min(Component), roundNumber),
                     Max = round(max(Component), roundNumber),
                     SD = round(sd(Component), roundNumber),
                     Count = n(),
                     .groups = "drop") %>%
    dplyr::filter(Outcome != "Missing")
  
  tableOutput <- bind_rows(tableData, tableDataTotal) %>%
    arrange(Treatment,
            factor(Outcome, levels = c("Yes", "No", "Total", "Missing")))
  
  return(tableOutput)
}
vmmc_individual_table <- threeWayComponent(myData = data_hiv_negative_males_treated,
                                           myOutcome = "endpoint_seroconvert",
                                           myComponent = "endpoint_coverage_mc",
                                           myTreatment = "random_arm")
vmmc_spillover_table <- threeWayComponentProp(myData = data_hiv_negative_untreated,
                                              myOutcome = "endpoint_seroconvert",
                                              myComponentProp = "Z1_k",
                                              myTreatment = "random_arm",
                                              roundNumber = 2)
vmmc_list <- list(`Y1 by X1 Individual` = vmmc_individual_table,
                  `Y1 by Z1 Spillover` = vmmc_spillover_table)
htc_individual_table <- threeWayComponent(myData = data_hiv_negative_treated,
                                          myOutcome = "endpoint_seroconvert",
                                          myComponent = "endpoint_coverage_htc",
                                          myTreatment = "random_arm")
htc_spillover_table <- threeWayComponentProp(myData = data_hiv_negative_untreated,
                                             myOutcome = "endpoint_seroconvert",
                                             myComponentProp = "Z2_k",
                                             myTreatment = "random_arm",
                                             roundNumber = 2)
htc_list <- list(`Y1 by X2 Individual` = htc_individual_table,
                 `Y1 by Z2 Spillover` = htc_spillover_table)
art_individual_table <- threeWayComponent(myData = data_hiv_positive_treated,
                                          myOutcome = "endpoint_death",
                                          myComponent = "endpoint_coverage_onart",
                                          myTreatment = "random_arm")
art_spillover_table <- threeWayComponentProp(myData = data_hiv_positive_untreated,
                                             myOutcome = "endpoint_death",
                                             myComponentProp = "Z3_k",
                                             myTreatment = "random_arm",
                                             roundNumber = 2)
art_list <- list(`Y2 by X3 Individual` = art_individual_table,
                 `Y2 by Z3 Spillover` = art_spillover_table)
htc_mortality_individual_table <- threeWayComponent(myData = data_hiv_positive_treated,
                                          myOutcome = "endpoint_death",
                                          myComponent = "endpoint_coverage_htc",
                                          myTreatment = "random_arm")
htc_mortality_spillover_table <- threeWayComponentProp(myData = data_hiv_positive_untreated,
                                             myOutcome = "endpoint_death",
                                             myComponentProp = "Z2_k",
                                             myTreatment = "random_arm",
                                             roundNumber = 2)
htc_mortality_list <- list(`Y2 by X2 Individual` = htc_mortality_individual_table,
                 `Y2 by Z2 Spillover` = htc_mortality_spillover_table)
overall_negative_table <- threeWayComponent(myData = data_hiv_negative,
                                            myOutcome = "endpoint_seroconvert",
                                            myComponent = "endpoint_coverage_any",
                                            myTreatment = "random_arm")
overall_positive_table <- threeWayComponent(myData = data_hiv_positive,
                                            myOutcome = "endpoint_death",
                                            myComponent = "endpoint_coverage_any",
                                            myTreatment = "random_arm")
write_xlsx(vmmc_list, "./out/vmmc_three_way_tables.xlsx")
write_xlsx(htc_list, "./out/htc_three_way_tables.xlsx")
write_xlsx(art_list, "./out/art_three_way_tables.xlsx")
write_xlsx(htc_mortality_list, "./out/htc_mortality_three_way_tables.xlsx")
write_xlsx(overall_negative_table, "./out/hiv_overall_three_way_tables.xlsx")
write_xlsx(overall_positive_table, "./out/mortality_overall_three_way_tables.xlsx")

