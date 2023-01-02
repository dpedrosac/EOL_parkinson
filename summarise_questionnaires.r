# This is code recodes and sorts all factors to run all analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 1.0 # 2023-01-03, first commit

# ==================================================================================================
# Create Sum scores for UPDRS parts I-IV, PDQ and MOCA

updrs_temp <- eol_dataframe %>% select(matches("^UPDRS_1")) %>% 
  select(-matches("source")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(updrs_I = updrs_temp)

updrs_temp <- eol_dataframe %>% select(matches("^UPDRS_2")) %>% 
  select(-matches("source")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(updrs_II = updrs_temp)

updrs_temp <- eol_dataframe %>% select(matches("^UPDRS_3")) %>% 
  select(-matches("source")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(updrs_III = updrs_temp)

updrs_temp <- eol_dataframe %>% select(matches("^UPDRS_4")) %>% 
  select(-matches("source")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(updrs_IV = updrs_temp)

# Select the columns "updrs_I", "updrs_II", "updrs_III", and "updrs_IV"
updrs_temp <- eol_dataframe %>% select(updrs_I, updrs_II, updrs_III, updrs_IV) %>% 
	as.matrix() %>% rowSums()

# Replace the values in the "updrs_sum" column with the new values and rename the column to "updrs_total"
eol_dataframe <- eol_dataframe %>% 
  mutate(updrs_sum = updrs_temp) %>% 
  rename(updrs_total = updrs_sum)

moca_temp <- eol_dataframe %>% select(matches("^MOCA")) %>% 
  select(-matches("score")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(MOCA_score = moca_temp)

pdq_temp <- eol_dataframe %>% select(matches("^PDQ")) %>% # This is a wrong way of adding the PDQ; It should be modified, as it is not a sum but a quotient.
  select(-matches("score")) %>% 
  as.matrix() %>% 
  rowSums()
eol_dataframe <- eol_dataframe %>% mutate(PDQ_score = pdq_temp)



