# This is code recodes and sorts all factors to run all analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 1.0 # 2023-01-02, first commit

# Recoded variables:
# 1. Prefered place of death
# 2. Place of care 
# 3. Religious affilitaion 
# 4. Nationality 
# 5. Marital status 
# 6. Housing situation 
# 7. Education
# 8. Residential location
# 9. Knowledge about palliative care
# 10. Knowledge about hospice
# 11. Power of attorney
# 12. Advance_directive 
# 13. Nursing_support
# 14. POD_family_friends 
# 15. POD_GP
# 16. POD_AD
# 17. Thoughts_about_end_of_life_wishes
# 18. People to which pod has been communicated 
# 19. Charlson comorbidity index 
# 20. Principal component of disease severity?


# ========================================================================================================================= #
# 1. Prefered place of death
row_temp <- dataframe_codes %>%
  filter(Factor == "prefered_place_of_death") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.prefered_place_of_death <- dataframe_codes %>%
  filter(Factor == "prefered_place_of_death") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "prefered_place_of_death" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.prefered_place_of_death = factor(prefered_place_of_death, levels = as.factor(factor.prefered_place_of_death$row_number)))  %>%
  mutate(home_death = ifelse(prefered_place_of_death == "0", "yes", "no"))
levels(eol_dataframe$cat.prefered_place_of_death) = as.factor(factor.prefered_place_of_death$.)

# ========================================================================================================================= #
# 2. Place of care 
row_temp <- dataframe_codes %>%
  filter(Factor == "prefered_place_of_care") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.prefered_place_of_care <- dataframe_codes %>%
  filter(Factor == "prefered_place_of_care") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "prefered_place_of_care" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.prefered_place_of_care = factor(prefered_place_of_care, levels = as.factor(factor.prefered_place_of_care$row_number)))  %>%
  mutate(home_care = fct_collapse(as.factor(prefered_place_of_care),
								  "home" = c("0", "5"),
								  "institution" = c("1", "2", "4"),
								  "other" = c("3", "6")))
levels(eol_dataframe$cat.prefered_place_of_care) = as.factor(factor.prefered_place_of_care$.)

#  Because of multicollinearity, this value had to be change home_care: yes vs. no to three factors (home, institution, other)
#   mutate(home_care = ifelse(prefered_place_of_care == "0", "yes", "no")) # original line
# ========================================================================================================================= #
# 3. Religious affilitaion 
row_temp <- dataframe_codes %>%
  filter(Factor == "religion_belief_worldview") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.religious_affiliation <- dataframe_codes %>%
  filter(Factor == "religion_belief_worldview") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "religion_belief_worldview" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.religious_affiliation = factor(religion_belief_worldview, levels = as.factor(factor.religious_affiliation$row_number)))  %>%
  mutate(religious_affiliation = ifelse(religion_belief_worldview == "8", "no", "yes"))
levels(eol_dataframe$cat.religious_affiliation) = as.factor(factor.religious_affiliation$.)

# ========================================================================================================================= #
# 4. Nationality 
row_temp <- dataframe_codes %>%
  filter(Factor == "nationality") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.nationality <- dataframe_codes %>%
  filter(Factor == "nationality") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "nationality" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.nationality = factor(nationality, levels = as.factor(factor.nationality$row_number)))  %>%
  mutate(german = ifelse(nationality == "0", "yes", "no"))
levels(eol_dataframe$cat.nationality) = as.factor(factor.nationality$.)

# ========================================================================================================================= #
# 5. Marital status 
row_temp <- dataframe_codes %>%
  filter(Factor == "marital_status") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.marital_status <- dataframe_codes %>%
  filter(Factor == "marital_status") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "marital_status" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.marital_status = factor(marital_status, levels = as.factor(factor.marital_status$row_number))) %>%
  mutate(married = fct_collapse(as.factor(marital_status), "no" = c("0", "3", "4", "5"), "yes" = c("1", "2"))) 
levels(eol_dataframe$cat.marital_status) = as.factor(factor.marital_status$.)

# ========================================================================================================================= #
# 6. Housing situation 
row_temp <- dataframe_codes %>%
  filter(Factor == "Housing_situation") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.housing_situation <- dataframe_codes %>%
  filter(Factor == "Housing_situation") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Housing_situation" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.independent_living = factor(Housing_situation, levels = as.factor(factor.housing_situation$row_number)))  %>%
  mutate(independent_living = ifelse(Housing_situation == "0", "yes", "no"))
levels(eol_dataframe$cat.independent_living) = as.factor(factor.housing_situation$.)

# ========================================================================================================================= #
# 7. Education 
row_temp <- dataframe_codes %>%
  filter(Factor == "Professional_education") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.education <- dataframe_codes %>%
  filter(Factor == "Professional_education") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Professional_education" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.education = factor(Professional_education, levels = as.factor(factor.education$row_number))) %>%
  mutate(professional_education = fct_collapse(as.factor(Professional_education), 	"0" = c("0"),
											   										"1" = c("1"),
      																				"2" = c("2", "3"),
      																				"other" = c("4")))
levels(eol_dataframe$cat.education) = as.factor(factor.education$.)

# ========================================================================================================================= #
# 8. Residential location #QUERY: There are some subjects coded as "4" which doesn't appear in the codes!
row_temp <- dataframe_codes %>%
  filter(Factor == "Residential_location") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.residential_location <- dataframe_codes %>%
  filter(Factor == "Residential_location") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Residential_location" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.residential_location = factor(Residential_location, levels = as.factor(factor.residential_location$row_number)))  %>%
  mutate(rurality = fct_collapse(as.factor(Residential_location), 	"no" = c("0"),
											   							"no" = c("1"),
											   							"yes" = c("2"),
																		"yes" = c("3"),
       																	"yes" = c("4")))

levels(eol_dataframe$cat.residential_location) = as.factor(factor.residential_location$.)

# ========================================================================================================================= #
# 9. Knowledge about palliative care
row_temp <- dataframe_codes %>%
  filter(Factor == "palliative_care_knowledge") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.palliative_care_knowledge <- dataframe_codes %>%
  filter(Factor == "palliative_care_knowledge") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "palliative_care_knowledge" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.palliative_care_knowledge = factor(palliative_care_knowledge, levels = as.factor(factor.palliative_care_knowledge$row_number)))  %>%
  mutate(informed_about_palliative_care = ifelse(palliative_care_knowledge == "0", "no", "yes"))
levels(eol_dataframe$cat.palliative_care_knowledge) = as.factor(factor.palliative_care_knowledge$.)

# ========================================================================================================================= #
# 10. Knowledge about hospice
row_temp <- dataframe_codes %>%
  filter(Factor == "hospice_knowledge") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.hospice_knowledge <- dataframe_codes %>%
  filter(Factor == "hospice_knowledge") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "hospice_knowledge" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.hospice_knowledge = factor(hospice_knowledge, levels = as.factor(factor.hospice_knowledge$row_number)))  %>%
  mutate(informed_about_hospice = ifelse(hospice_knowledge == "0", "no", "yes"))
levels(eol_dataframe$cat.hospice_knowledge) = as.factor(factor.hospice_knowledge$.)

# ========================================================================================================================= #
# 11. Power of attorney 
row_temp <- dataframe_codes %>%
  filter(Factor == "Power_of_attorney") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.power_of_attorney <- dataframe_codes %>%
  filter(Factor == "Power_of_attorney") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Power_of_attorney" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.power_attorney = factor(Power_of_attorney, levels = as.factor(factor.power_of_attorney$row_number))) %>%
  mutate(attorney_power = fct_collapse(as.factor(Power_of_attorney), 	"no" = c("0"), 
											   							"yes" = c("1"),
      																	"no" = c("2")))
#                                        "maybe" = c("2")))  # being recoded as 0 in case of maybe!
levels(eol_dataframe$cat.power_attorney) = as.factor(factor.power_of_attorney$.)

# ========================================================================================================================= #
# 12. Advance_directive 
row_temp <- dataframe_codes %>%
  filter(Factor == "Advance_directive") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.advance_directive <- dataframe_codes %>%
  filter(Factor == "Advance_directive") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Advance_directive" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.advance_directive = factor(Advance_directive, levels = as.factor(factor.advance_directive$row_number))) %>%
  mutate(existence_advance_directive = fct_collapse(as.factor(cat.advance_directive),"no" = c("0"), 
											   										"yes" = c("1"),
      																	"no" = c("2")))
#                                        "maybe" = c("2")))  # being recoded as 0 in case of maybe!
levels(eol_dataframe$cat.advance_directive) = as.factor(factor.advance_directive$.)

# ========================================================================================================================= #
# 13. Nursing_support 
row_temp <- dataframe_codes %>%
  filter(Factor == "Nursing_support") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.nursing_support <- dataframe_codes %>%
  filter(Factor == "Nursing_support") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "Nursing_support" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.nursing_support = factor(Nursing_support, levels = as.factor(factor.nursing_support$row_number))) %>%
  mutate(receiving_nursing_support = fct_collapse(as.factor(cat.nursing_support),"no" = c("0"), 
											   										"informal" = c("1"),
      																				"formal" = c("2", "3", "4")))
levels(eol_dataframe$cat.nursing_support) = as.factor(factor.nursing_support$.)

# ========================================================================================================================= #
# 14. POD_familiy_friends 
row_temp <- dataframe_codes %>%
  filter(Factor == "POD_familiy_friends") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.pod_family_friends <- dataframe_codes %>%
  filter(Factor == "POD_familiy_friends") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "POD_familiy_friends" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.pod_family_friends = factor(POD_familiy_friends, levels = as.factor(factor.pod_family_friends$row_number))) %>%
  mutate(pod.family_friends = fct_collapse(as.factor(cat.pod_family_friends),"no" = c("0"), 
											   										"yes" = c("1"),
      																	"no" = c("2")))
#                                        "maybe" = c("2")))  # being recoded as no in case of maybe!
levels(eol_dataframe$cat.pod_family_friends) = as.factor(factor.pod_family_friends$.)

# ========================================================================================================================= #
# 15. POD_GP 
row_temp <- dataframe_codes %>%
  filter(Factor == "POD_GP") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.pod_GP <- dataframe_codes %>%
  filter(Factor == "POD_GP") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "POD_GP" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.pod_GP = factor(POD_GP, levels = as.factor(factor.pod_GP$row_number))) %>%
  mutate(pod.GP = fct_collapse(as.factor(cat.pod_GP),"no" = c("0"), 
											   										"yes" = c("1"),
      																	"no" = c("2")))
#                                        "maybe" = c("2")))  # being recoded as no in case of maybe!
levels(eol_dataframe$cat.pod_GP) = as.factor(factor.pod_GP$.)

# ========================================================================================================================= #
# 15. POD_neurologist 
row_temp <- dataframe_codes %>%
  filter(Factor == "POD_neurologist") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.pod_neurologist <- dataframe_codes %>%
  filter(Factor == "POD_neurologist") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "POD_neurologist" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.pod_neurologist = factor(POD_neurologist, levels = as.factor(factor.pod_neurologist$row_number))) %>%
  mutate(pod.neurologist = fct_collapse(as.factor(cat.pod_neurologist),"no" = c("0"), 
											   										"yes" = c("1"),
      																				"no" = c("2")))
levels(eol_dataframe$cat.pod_neurologist) = as.factor(factor.pod_neurologist$.)

# ========================================================================================================================= #
# 16. POD_AD 
row_temp <- dataframe_codes %>%
  filter(Factor == "POD_AD") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.pod_AD <- dataframe_codes %>%
  filter(Factor == "POD_AD") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "POD_AD" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.pod_AD = factor(POD_AD, levels = as.factor(factor.pod_AD$row_number))) %>%
  mutate(pod.AD = fct_collapse(as.factor(cat.pod_AD),"no" = c("0"), 
											   										"yes" = c("1"),
      																				"no" = c("2")))
#                                        "maybe" = c("2")))  # being recoded as no in case of maybe!
levels(eol_dataframe$cat.pod_AD) = as.factor(factor.pod_AD$.)

# ========================================================================================================================= #
# 17. thoughts_about_end_of_life_wishes 
row_temp <- dataframe_codes %>%
  filter(Factor == "thoughts_about_end_of_life_wishes") %>% select(-c(Unit, Factor))

# Extract the row, flatten the matrix into a vector, and remove the NAs
factor.thoughts_EOLwishes <- dataframe_codes %>%
  filter(Factor == "thoughts_about_end_of_life_wishes") %>% 
  select(-c(Unit, Factor)) %>% 
  unlist(t(as.matrix(row_temp))) %>% 
  na.omit() %>% 
  data.frame() %>% 
  mutate(row_number = row_number()-1)  # Add a row number column

# Mutate the values in the "thoughts_about_end_of_life_wishes" column using the row number as the levels and recode to binary data
eol_dataframe <- eol_dataframe %>%
  mutate(cat.thoughts_EOLwishes = factor(thoughts_about_end_of_life_wishes, levels = as.factor(factor.thoughts_EOLwishes$row_number))) %>%
  mutate(oftenEOLwishes_thoughts = fct_collapse(as.factor(cat.thoughts_EOLwishes),
                                               	"no" = c("0"),
												"yes" = c("1", "2", "3", "4")))
levels(eol_dataframe$cat.thoughts_EOLwishes) = as.factor(factor.thoughts_EOLwishes$.)

# ========================================================================================================================= #
# 18. People to which pod has been communicated 

df_temp <- eol_dataframe %>% select(starts_with("pod.")) %>%
  mutate(total_pod = rowSums(sapply(., function(col) str_count(col, "yes"))))

# add column to the original data frame
eol_dataframe$total_pod <- df_temp$total_pod

# ========================================================================================================================= #
# 19. Charlson comorbidity index, i.e. the probability of mortality

eol_dataframe <- eol_dataframe  %>% mutate(CCI = .983^exp(1)^(Charlson_withage * .9))


# ========================================================================================================================= #
# 20. # Principal component of disease severity:

pca_severity = c("UPDRS_sum", "Hoehn_Yahr", "LEDD")
flag = FALSE
if (flag){
	correlation_matrix <- cor(eol_dataframe  %>% select(all_of(pca_severity)), method="spearman")
	corrplot(correlation_matrix) # plot correlation coefficients
}

df_temp <- eol_dataframe  %>% select(all_of(pca_severity))
pca <- prcomp(df_temp, retx=TRUE, scale=TRUE, center = TRUE)
df_temp_transformed <- pca$x
vars_transformed <- apply(df_temp_transformed, 2, var)
vars_transformed/sum(vars_transformed) # <- just for illustration, the first PC picks up most of the variance of all 5 columns!
eol_dataframe$disease_severityPC <- c(pca$x[,1])