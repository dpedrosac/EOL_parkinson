


# 1. Homecare
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
  mutate(home_care = ifelse(prefered_place_of_care == "0", "yes", "no"))
levels(eol_dataframe$cat.prefered_place_of_care) = as.factor(factor.prefered_place_of_care$.)



# Convert the vector to a data frame
prefered_place_of_death_factors <- data.frame(prefered_place_of_death_factors)

eol_xls <- eol_xls %>%
  mutate(
    homedeath = fct_collapse(
      as.factor(prefered_place_of_death),
      "no" = c("1", "2", "3", "4", "5", "7"),
      "yes" = c("0"),
      "not important" = c("7")))

eol_xls <- eol_xls %>%
  mutate(
    homecare = fct_collapse(
      as.factor(prefered_place_of_care),
      "no" = c("1", "2", "3", "4", "5", "7"),
      "yes" = c("0"),
      "not important" = c("7")))

eol_xls <- eol_xls %>%
  mutate(
    married = fct_collapse(
      as.factor(marital_status),
      "no" = c("0", "3", "4", "5"),
      "yes" = c("1", "2")))

eol_xls <- eol_xls %>%
  mutate(
    German = fct_collapse(
      as.factor(nationality),
      "no" = c("1", "2", "3", "4","5", "6", "7", "8", "9", "10", "11" ),
      "yes" = c("0")))

eol_xls <- eol_xls %>%
  mutate(
    Religious_affiliation = fct_collapse(
      as.factor(religion_belief_worldview),
      "no" = c( "8"),
      "yes" = c("0", "1", "2", "3", "4","5", "6", "7")))

eol_xls <- eol_xls %>%
  mutate(
    independent_living = fct_collapse(
      as.factor(LivingSituation),
      "no" = c("1", "2", "3", "4"),
      "yes" = c("0")))

eol_xls <- eol_xls %>%
  mutate(
    nursingcare = fct_collapse(
      as.factor(Nursing_support),
      "no" = c("0"), 
      "informal" = c("1"),
      "formal"= c("2", "3", "4")))

eol_xls <- eol_xls %>%
  mutate(
    rurality = fct_collapse(
      as.factor(Residential_location),
      "no" = c("0", "1","2","3"),
      "yes" = c("4")))

eol_xls <- eol_xls %>%
  mutate(
    ProfessionalEducation = fct_collapse(
      as.factor(Professional_education),
      "none" = c("0"),
      "Ausbildung" = c("1"),
      "university degree" = c("2", "3"),
      "other" = c("4")))

eol_xls <- eol_xls %>%
  mutate(
    AdvanceDirective = fct_collapse(
      as.factor(Advance_directive),
      "no" = c("0"),
      "yes" = c("1"),
      "maybe" = c("2")))

eol_xls <- eol_xls %>%
  mutate(
    PowerOfAttorney = fct_collapse(
      as.factor(Power_of_attorney),
      "no" = c("0"),
      "yes" = c("1"),
      "maybe" = c("2")))

eol_xls <- eol_xls %>%
  mutate(
    knowledge_PC = fct_collapse(
      as.factor(palliative_care-knowledge),
      "no" = c("0"),
      "yes" = c("1")))

eol_xls <- eol_xls %>%
  mutate(
    knowledge_hospice = fct_collapse(
      as.factor(hospice_knowledge),
      "no" = c("0"),
      "yes" = c("1")))

eol_xls <- eol_xls %>%
  mutate(
    familyfriends_POD = fct_collapse(
      as.factor(POD_familiy_friends),
      "yes" = c("1"),
      "no" = c("0"),
      "maybe" = c("2"))) 

eol_xls <- eol_xls %>%
  mutate(
    GP_POD = fct_collapse(
      as.factor(POD_GP),
      "yes" = c("1"),
      "no" = c("0"),
      "maybe" = c("2"))) 

eol_xls <- eol_xls %>%
  mutate(
    Neurologist_POD = fct_collapse(
      as.factor(POD_neurologist),
      "yes" = c("1"),
      "no" = c("0"),
      "maybe" = c("2")))

eol_xls <- eol_xls %>%
  mutate(
    AD_POD = fct_collapse(
      as.factor(POD_AD),
      "yes" = c("1"),
      "no" = c("0"),
      "maybe" = c("2")))