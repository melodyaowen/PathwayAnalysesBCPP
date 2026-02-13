bcpp_y1_path # assign to raw data file paths
bcpp_y2_path 
bcpp_y3_path 
data_y1 <- read_sas(bcpp_y1_path)
data_y2 <- read_sas(bcpp_y2_path)
data_y3 <- read_sas(bcpp_y3_path)
names_y1 <- tibble(colname = sort(colnames(data_y1)), dataset = "data_y1")
names_y2 <- tibble(colname = sort(colnames(data_y2)), dataset = "data_y2")
names_y3 <- tibble(colname = sort(colnames(data_y3)), dataset = "data_y3")
vec <- c() # variables you want here
variable_names_all <- bind_rows(names_y1, names_y2, names_y3) %>%
  mutate(present = colname) %>%
  pivot_wider(names_from = dataset, values_from = present) %>%
  arrange(colname)
variable_names_keep <- variable_names_all %>%
  dplyr::filter(colname %in% vec
  )
data_y1_renamed <- data_y1 %>% rename_with(~ paste0(., "_y1"), -de_subj_idC)
data_y2_renamed <- data_y2 %>% rename_with(~ paste0(., "_y2"), -de_subj_idC)
data_y3_renamed <- data_y3 %>% rename_with(~ paste0(., "_y3"), -de_subj_idC)
all_variables_list <- tibble(vars = c(paste0(variable_names_keep$data_y1, "_y1"),
                                      paste0(variable_names_keep$data_y2, "_y2"),
                                      paste0(variable_names_keep$data_y3, "_y3"))) %>%
  filter(vars != "de_subj_idC_y1", vars != "de_subj_idC_y2", vars != "de_subj_idC_y3") %>%
  arrange(vars) %>%
  filter(!str_detect(vars, "NA_"))
data_full <- full_join(data_y1_renamed, data_y2_renamed, by = "de_subj_idC") %>%
  full_join(data_y3_renamed, by = "de_subj_idC") %>%
  dplyr::select(de_subj_idC, all_variables_list$vars) %>%
  mutate(across(where(~ !is.numeric(.)), ~ na_if(., ""))) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(random_arm = ifelse(!is.na(random_arm_y1), random_arm_y1,
                             ifelse(!is.na(random_arm_y2), random_arm_y2,
                                    random_arm_y3))) %>%
  dplyr::select(-random_arm_y1, -random_arm_y2, -random_arm_y3) %>%
  mutate(community = ifelse(!is.na(community_y1), community_y1,
                            ifelse(!is.na(community_y2), community_y2,
                                   community_y3))) %>%
  dplyr::select(-community_y1, -community_y2, -community_y3) %>%
  mutate(location = community) %>%
  mutate(age = ifelse(!is.na(age_at_interview_y1), age_at_interview_y1,
                            ifelse(!is.na(age_at_interview_y2), age_at_interview_y2 - 1,
                                   ifelse(!is.na(age_at_interview_y3), age_at_interview_y3 - 2, NA)))) %>%
  dplyr::select(-age_at_interview_y1, -age_at_interview_y2, -age_at_interview_y3) %>%
  mutate(gender = ifelse(!is.na(gender_y1), gender_y1,
                         ifelse(!is.na(gender_y2), gender_y2,
                                ifelse(!is.na(gender_y3), gender_y3, NA)))) %>%
  dplyr::select(-gender_y1, -gender_y2, -gender_y3) %>%
  mutate(hiv_status_current = ifelse(!is.na(hiv_status_current_y1), hiv_status_current_y1,
                                     ifelse(!is.na(hiv_status_current_y2), hiv_status_current_y2,
                                            ifelse(!is.na(hiv_status_current_y3), hiv_status_current_y3, NA)))) %>%
  mutate(hiv_status_current = case_when(hiv_status_current == "Refused HIV testing" & hiv_status_current_y3 == "HIV-uninfected" ~ "HIV-uninfected",
                                        hiv_status_current == "Refused HIV testing" & is.na(hiv_status_current_y3) & hiv_status_current_y2 == "HIV-uninfected" ~ "HIV-uninfected",
                                        TRUE ~ hiv_status_current)) %>%
  dplyr::select(-hiv_status_current_y1, -hiv_status_current_y2, -hiv_status_current_y3) %>%
  mutate(across(contains("arv_status"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(arv_status_current = ifelse(arv_status_current_y1 == "On ARVs" | arv_status_current_y2 == "On ARVs" | arv_status_current_y3 == "On ARVs", "On ARVs", 
                                     ifelse(arv_status_current_y1 == "ARV defaulter" | arv_status_current_y2 == "ARV defaulter" | arv_status_current_y3 == "ARV defaulter", "ARV defaulter",
                                            ifelse(arv_status_current_y1 == "ARV naive" | arv_status_current_y2 == "ARV naive" | arv_status_current_y3 == "ARV naive", "ARV naive", NA)))) %>%
  dplyr::select(-arv_status_current_y1, -arv_status_current_y2, -arv_status_current_y3) %>%
  mutate(across(contains("circumcised"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(circumcised = ifelse(gender == "F", "Female",
                              ifelse(circumcised_y1 != "0", circumcised_y1,
                                     ifelse(circumcised_y2 != "0", circumcised_y2,
                                            ifelse(circumcised_y3 != "0", circumcised_y3, NA))))) %>%
  dplyr::select(-circumcised_y1, -circumcised_y2, -circumcised_y3) %>%
  mutate(circumcision_days = ifelse(!is.na(circumcision_days_y1), circumcision_days_y1,
                                    ifelse(!is.na(circumcision_days_y2), circumcision_days_y2,
                                           ifelse(!is.na(circumcision_days_y3), circumcision_days_y3, NA)))) %>%
  dplyr::select(-circumcision_days_y1, -circumcision_days_y2, -circumcision_days_y3) %>%
  mutate(across(contains("endpoint_coverage_mc"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "F", "Female",
                                       ifelse(endpoint_coverage_mc_y1 == "Yes" | endpoint_coverage_mc_y2 == "Yes" | endpoint_coverage_mc_y3 == "Yes", "Yes",
                                              ifelse(endpoint_coverage_mc_y1 == "No" | endpoint_coverage_mc_y2 == "No" | endpoint_coverage_mc_y3 == "No", "No", NA)))) %>%
  dplyr::select(-endpoint_coverage_mc_y1, -endpoint_coverage_mc_y2, -endpoint_coverage_mc_y3) %>%
  mutate(endpoint_coverage_mc = case_when(circumcised == "Yes" & circumcision_days >= 0 ~ "Yes",
                                              TRUE ~ endpoint_coverage_mc)) %>%
  mutate(endpoint_coverage_htc = endpoint_coverage_htc_y1) %>%
  dplyr::select(-endpoint_coverage_htc_y1) %>%
  mutate(across(contains("endpoint_coverage_onart"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_onart = ifelse(endpoint_coverage_onart_y1 == "Yes" | endpoint_coverage_onart_y2 == "Yes" | endpoint_coverage_onart_y3 == "Yes", "Yes",
                                       ifelse(endpoint_coverage_onart_y1 == "No" | endpoint_coverage_onart_y2 == "No" | endpoint_coverage_onart_y3 == "No", "No", NA))) %>%
  dplyr::select(-endpoint_coverage_onart_y1, -endpoint_coverage_onart_y2, -endpoint_coverage_onart_y3) %>%
  mutate(across(contains("endpoint_coverage_vlsupp"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(endpoint_coverage_vlsupp = ifelse(endpoint_coverage_vlsupp_y1 == "Yes" | endpoint_coverage_vlsupp_y2 == "Yes" | endpoint_coverage_vlsupp_y3 == "Yes", "Yes",
                                           ifelse(endpoint_coverage_vlsupp_y1 == "No" | endpoint_coverage_vlsupp_y2 == "No" | endpoint_coverage_vlsupp_y3 == "No", "No", NA))) %>%
  dplyr::select(-endpoint_coverage_vlsupp_y1, -endpoint_coverage_vlsupp_y2, -endpoint_coverage_vlsupp_y3) %>%
  mutate(endpoint_seroconvert = endpoint_seroconvert_y1) %>%
  dplyr::select(-endpoint_seroconvert_y1) %>%
  mutate(endpoint_death = endpoint_death_y1) %>%
  dplyr::select(-endpoint_death_y1) %>%
  mutate(overall_access = overall_access_y1) %>%
  dplyr::select(-overall_access_y1) %>%
  mutate(across(contains("exchange_12mos"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(exchange_12mos = ifelse(exchange_12mos_y1 == "Yes" | exchange_12mos_y2 == "Yes" | exchange_12mos_y3 == "Yes", "Yes",
                                          ifelse(exchange_12mos_y1 == "No" | exchange_12mos_y2 == "No" | exchange_12mos_y3 == "No", "No", NA))) %>%
  dplyr::select(-exchange_12mos_y1, -exchange_12mos_y2, -exchange_12mos_y3) %>%
  mutate(across(contains("condom_lastsex"),~ ifelse(is.na(.) | . == "", "0", .))) %>%
  mutate(condom_lastsex = ifelse(condom_lastsex_y1 == "No" | condom_lastsex_y2 == "No" | condom_lastsex_y3 == "No", "No",
                                 ifelse(condom_lastsex_y1 == "Yes" | condom_lastsex_y2 == "Yes" | condom_lastsex_y3 == "Yes", "Yes", NA))) %>%
  dplyr::select(-condom_lastsex_y1, -condom_lastsex_y2, -condom_lastsex_y3) %>%
  mutate(partners_lifetime = pmax(partners_lifetime_y1, partners_lifetime_y2, 
                                  partners_lifetime_y3, na.rm = TRUE)) %>%
  dplyr::select(-partners_lifetime_y1, -partners_lifetime_y2, -partners_lifetime_y3) %>%
  mutate(across(contains("partners_12mos"), 
                ~ case_when(. == "1 partner" ~ 1L,
                            . == "2 partners" ~ 2L,
                            . == "3 partners" ~ 3L,
                            . == "4 or more partners" ~ 4L,
                            . == "None" ~ 0L,
                            TRUE ~ NA_integer_)
                )
         ) %>%
  mutate(partners_12mos = pmax(partners_12mos_y1, partners_12mos_y2, 
                               partners_12mos_y3, na.rm = TRUE)) %>%
  dplyr::select(-partners_12mos_y1, -partners_12mos_y2, -partners_12mos_y3) %>%
  mutate(length_residence = ifelse(!is.na(length_residence_y1), length_residence_y1,
                                    ifelse(!is.na(length_residence_y2), length_residence_y2,
                                           ifelse(!is.na(length_residence_y3), length_residence_y3, NA)))) %>%
  dplyr::select(-length_residence_y1, -length_residence_y2, -length_residence_y3) %>%
  mutate(monthly_income = ifelse(!is.na(monthly_income_y1), monthly_income_y1,
                                 ifelse(!is.na(monthly_income_y2), monthly_income_y2,
                                        ifelse(!is.na(monthly_income_y3), monthly_income_y3, NA)))) %>%
  dplyr::select(-monthly_income_y1, -monthly_income_y2, -monthly_income_y3) %>%
  mutate(employment_status = ifelse(!is.na(employment_status_y1), employment_status_y1,
                                    ifelse(!is.na(employment_status_y2), employment_status_y2,
                                           ifelse(!is.na(employment_status_y3), employment_status_y3, NA)))) %>%
  dplyr::select(-employment_status_y1, -employment_status_y2, -employment_status_y3) %>%
  mutate(education = ifelse(!is.na(education_y1), education_y1,
                            ifelse(!is.na(education_y2), education_y2,
                                   ifelse(!is.na(education_y3), education_y3, NA)))) %>%
  dplyr::select(-education_y1, -education_y2, -education_y3) %>%
  mutate(marital_status = ifelse(!is.na(marital_status_y1), marital_status_y1,
                                 ifelse(!is.na(marital_status_y2), marital_status_y2,
                                        ifelse(!is.na(marital_status_y3), marital_status_y3, NA)))) %>%
  dplyr::select(-marital_status_y1, -marital_status_y2, -marital_status_y3) %>%
  mutate(alcohol_12mos = alcohol_y1) %>%
  dplyr::select(-alcohol_y1) %>%
  mutate(alcohol_weekly = case_when(alcohol_12mos == "2 to 3 times a week" ~ "3",
                                    alcohol_12mos == "Less then once a week" ~ "1",
                                    alcohol_12mos == "more than 3 times a week" ~ "4",
                                    alcohol_12mos == "Never" ~ "0",
                                    alcohol_12mos == "Once a week" ~ "2",
                                    TRUE ~ NA_character_),
         alcohol_weekly = as.numeric(alcohol_weekly)
         ) %>%
  mutate(prob_alcohol_drug = ifelse(!is.na(prob_alcohol_drug_y1), prob_alcohol_drug_y1,
                                 ifelse(!is.na(prob_alcohol_drug_y2), prob_alcohol_drug_y2, NA))) %>%
  dplyr::select(-prob_alcohol_drug_y1, -prob_alcohol_drug_y2) %>%
  mutate(prob_healthcare = ifelse(!is.na(prob_healthcare_y1), prob_healthcare_y1,
                                    ifelse(!is.na(prob_healthcare_y2), prob_healthcare_y2, NA))) %>%
  dplyr::select(-prob_healthcare_y1, -prob_healthcare_y2) %>%
  mutate(prob_hiv = ifelse(!is.na(prob_hiv_y1), prob_hiv_y1,
                           ifelse(!is.na(prob_hiv_y2), prob_hiv_y2, NA))) %>%
  dplyr::select(-prob_hiv_y1, -prob_hiv_y2) %>%
  mutate(prob_schools = ifelse(!is.na(prob_schools_y1), prob_schools_y1,
                               ifelse(!is.na(prob_schools_y2), prob_schools_y2, NA))) %>%
  dplyr::select(-prob_schools_y1, -prob_schools_y2) %>%
  mutate(prob_housing = ifelse(!is.na(prob_housing_y1), prob_housing_y1,
                               ifelse(!is.na(prob_housing_y2), prob_housing_y2, NA))) %>%
  dplyr::select(-prob_housing_y1, -prob_housing_y2) %>%
  relocate(de_subj_idC, community, random_arm, contains("current"), 
           contains("endpoint"), gender, age) %>%
  arrange(community, de_subj_idC) %>%
  mutate(subject_id = row_number()) %>%
  group_by(community) %>%
  mutate(cluster_id = cur_group_id()) %>%
  mutate(subject_cluster_id = row_number()) %>%
  ungroup() %>%
  add_count(cluster_id, name = "cluster_size") %>%
  relocate(subject_id, subject_cluster_id, cluster_id, cluster_size) %>%
  mutate(gender = case_when(gender == "F" ~ "Female",
                            gender == "M" ~ "Male",
                            TRUE ~ NA_character_)) %>%
  mutate(circumcised = ifelse(gender == "Female", "Female", circumcised)) %>%
  mutate(endpoint_coverage_mc = ifelse(gender == "Female", "Female", endpoint_coverage_mc)) %>%
  mutate(endpoint_seroconvert = ifelse(hiv_status_current == "HIV-infected" & 
                                         is.na(endpoint_seroconvert), 
                                       "Began study HIV-infected", endpoint_seroconvert)) %>%
  mutate(T_k = ifelse(random_arm == "Intervention", 1, 0)) %>%
  mutate(X1_ik = ifelse(endpoint_coverage_mc == "Yes", 1,
                        ifelse(endpoint_coverage_mc == "No", 0, NA))) %>%
  mutate(X2_ik = ifelse(endpoint_coverage_htc == "Yes", 1,
                        ifelse(endpoint_coverage_htc == "No", 0, NA))) %>%
  mutate(X3_ik = ifelse(endpoint_coverage_onart == "Yes", 1,
                        ifelse(endpoint_coverage_onart == "No", 0, NA))) %>%
  mutate(Xany_ik = ifelse(gender == "Male" & hiv_status_current == "HIV-uninfected",
                          ifelse(X1_ik == 1 | X2_ik == 1, 1, 0),
                          ifelse(gender == "Female" & hiv_status_current == "HIV-uninfected", 
                                 X2_ik, NA)),
         Xany_ik = replace_na(Xany_ik, 0)) %>%
  mutate(Xany_ik = ifelse(hiv_status_current == "HIV-infected",
                          ifelse(X3_ik == 1 | X2_ik == 1, 1, 0), Xany_ik),
         Xany_ik = replace_na(Xany_ik, 0)) %>%
  mutate(Y1_ik = ifelse(endpoint_seroconvert == "Yes", 1, 
                        ifelse(endpoint_seroconvert == "No", 0, NA))) %>%
  mutate(Y2_ik = ifelse(endpoint_death == "Yes", 1, 0)) %>%
  mutate(Y3_ik = ifelse(endpoint_coverage_vlsupp == "Yes", 1, 
                        ifelse(endpoint_coverage_vlsupp == "No", 0, NA))) %>%
  mutate(endpoint_coverage_any = ifelse(Xany_ik == 1, "Yes",
                                        ifelse(Xany_ik == 0, "No", NA))) %>%
  group_by(cluster_id) %>% mutate(male_count = sum(gender == "Male", na.rm = TRUE),
         began_hiv_infected_count = sum(hiv_status_current == "HIV-infected", na.rm = TRUE),
         began_hiv_uninfected_count = sum(hiv_status_current == "HIV-uninfected", na.rm = TRUE),
         refused_hiv_testing_count = sum(hiv_status_current == "Refused HIV testing", na.rm = TRUE),
         vmmc_count = sum(endpoint_coverage_mc == "Yes", na.rm = TRUE),
         htc_count = sum(endpoint_coverage_htc == "Yes", na.rm = TRUE),
         art_count = sum(endpoint_coverage_onart == "Yes", na.rm = TRUE),
         
         any_count_hivpos = sum(Xany_ik == 1 & hiv_status_current == "HIV-infected", na.rm = TRUE),
         any_count_hivneg = sum(Xany_ik == 1 & hiv_status_current == "HIV-uninfected", na.rm = TRUE),
         
         vlsupp_count = sum(endpoint_coverage_vlsupp == "Yes", na.rm = TRUE)
  ) %>% ungroup() %>%
  mutate(Z1_k = vmmc_count/male_count,
         Z2_k = htc_count/cluster_size,
         Z3_k = art_count/began_hiv_infected_count,
         Zany_k_hivpos = any_count_hivpos/began_hiv_infected_count,
         Zany_k_hivneg = any_count_hivneg/began_hiv_uninfected_count,
         prop_male = male_count/cluster_size,
         prop_vlsupp = vlsupp_count/began_hiv_infected_count,
         prop_began_infected = began_hiv_infected_count/cluster_size,
         hiv_refused_testing_prop = refused_hiv_testing_count/cluster_size
         ) %>%
  dplyr::select(-ends_with("_count"))
data_hiv_negative <- data_full %>% filter(hiv_status_current == "HIV-uninfected")
data_hiv_negative_treated <- data_full %>% filter(hiv_status_current == "HIV-uninfected", Xany_ik == 1)
data_hiv_negative_untreated <- data_full %>% filter(hiv_status_current == "HIV-uninfected", Xany_ik == 0)
data_hiv_negative_males <- data_full %>% filter(hiv_status_current == "HIV-uninfected", gender == "Male")
data_hiv_negative_males_treated <- data_full %>% filter(hiv_status_current == "HIV-uninfected", gender == "Male") %>% filter(Xany_ik == 1)
data_hiv_positive <- data_full %>% filter(hiv_status_current == "HIV-infected")
data_hiv_positive_treated <- data_full %>% filter(hiv_status_current == "HIV-infected", Xany_ik == 1)
data_hiv_positive_untreated <- data_full %>% filter(hiv_status_current == "HIV-infected", Xany_ik == 0)
# Create appropriate folders if they don't already exist on computer before next part
write.csv(data_full, "./data_full.csv")
write.csv(data_hiv_negative, "./data_hiv_negative.csv")
write.csv(data_hiv_negative_treated, "./data_hiv_negative_treated.csv")
write.csv(data_hiv_negative_untreated, "./data_hiv_negative_untreated.csv")
write.csv(data_hiv_negative_males, "./data_hiv_negative_males.csv")
write.csv(data_hiv_negative_males_treated, "./data_hiv_negative_males_treated.csv")
write.csv(data_hiv_positive, "./data_hiv_positive.csv")
write.csv(data_hiv_positive_treated, "./data_hiv_positive_treated.csv")
write.csv(data_hiv_positive_untreated, "./data_hiv_positive_untreated.csv")




