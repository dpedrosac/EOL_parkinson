# This is code to run analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 2.1 # 2023-02-23, new data matrix with some input errors added. Removed minor mistakes from code.

## First, specify the packages of interest
packages = c("readxl", "tableone", "ggplot2", "tidyverse", "lemon", "openxlsx", "caret", "corrplot", "gridExtra",
			 "psych", "DescTools", "jtools", "rstatix", "ggpubr", "dplyr", "precrec", "MLmetrics")
source("load_packages.r") 							# all defined [packages] are loaded - helper file
source("functionsUsed.R")

## In case of multiple people working on one project, this creates automatic script
username = Sys.info()["login"]

if (username == "dpedr") {
	wdir 		<- "D:/EOL_parkinson/"
	data_dir 	<-file.path(wdir, 'data')
} else if (username == "david") {
	wdir 		<-  "/media/storage/EOL_parkinson/"
	data_dir 	<-file.path(wdir, 'data')
} else {
	wdir = getwd()
	data_dir 	<-file.path(wdir)
}
setwd(wdir)


# ==================================================================================================
# Read data from excel spreadsheet
eol_dataframe 	<- read_xlsx(file.path(data_dir, "Matrix_EOL_PD.v1.2.xlsx"))

# Read and convert coding/explanations from xlsx-file
dataframe_codes <- read_excel(file.path(data_dir, "Matrix_EOL_PD_.xlsx"), sheet = "explanations")
dataframe_codes_clean <- dataframe_codes %>%
	drop_na(starts_with("0")) %>% 				# Remove rows with NAs in columns 3 and beyond
	select(-Unit)  								# Drop the "Unit" column


# ==================================================================================================
# Summarise questionnaires and recode variables
source("summarise_questionnaires.r")  			# questionnaires (UPDRS, PDQ, MoCA)
source("recode_dataframe.r") 	# data is recoded and structured according to labels
eol_dataframe$Cohabitation <- as.numeric(eol_dataframe$Cohabitation)
eol_dataframe$Cohabitation[eol_dataframe$Cohabitation>6] = NA 								# Outliers removed!
eol_dataframe$cat.education[eol_dataframe$cat.education=='other'] = NA 						# Subj. claiming 'Other' discarded!
eol_dataframe$professional_education[eol_dataframe$professional_education=='other'] = NA 	# Subj. claiming 'Other' discarded!


# ==================================================================================================
# Create TableOne with all data

allVars <- c(	"gender", "age", "age_at_diagnosis", "duration", "marital_status", "cat.education",
				 "religious_affiliation", "cat.independent_living", "Cohabitation", "cat.nursing_support",
				 "cat.residential_location", "cat.education", "cat.advance_directive", "cat.power_attorney",
				 "cat.palliative_care_knowledge", "cat.hospice_knowledge", "cat.thoughts_EOLwishes",
				 "Sharing_of_thoughts", "Thoughts_dicussed_with",
				 "asked_about_end_of_life_wishes", "asked_by_whom", "cat.prefered_place_of_care",
				 "cat.prefered_place_of_death", "cat.pod_family_friends", "cat.pod_GP", "cat.pod_neurologist",
				 "cat.pod_AD", "LEDD", "Hoehn_Yahr", "PDQ_score", "UPDRS_sum", "bdi_score", "MOCA_score",
				 "Charlson_withoutage", "Charlson_withage")

catVars <- c(	"gender", "german", "cat.marital_status", "religious_affiliation",
				 "cat.independent_living", "cat.nursing_support", "cat.residential_location", "cat.education",
				 "cat.advance_directive", "cat.power_attorney", "cat.palliative_care_knowledge",
				 "cat.hospice_knowledge", "cat.thoughts_EOLwishes", "Sharing_of_thoughts",
				 "Thoughts_dicussed_with", "asked_about_end_of_life_wishes", "asked_by_whom",
				 "cat.prefered_place_of_care", "cat.prefered_place_of_death", "cat.pod_family_friends",
				 "cat.pod_GP", "cat.pod_neurologist", "cat.pod_AD", "Hoehn_Yahr")

NumVars <- c(	"age", "age_at_diagnosis", "duration", "LEDD", "PDQ_score","updrs_sum", "bdi_score", "MOCA_score",
				 "Charlson_withoutage","Charlson_withage", "Cohabitation")

tab2 <- CreateTableOne(vars = allVars, data = eol_dataframe, factorVars = catVars)
print(tab2)
write.csv(print(tab2, quote = FALSE,
				noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE),
		  file = file.path(wdir, "results", "table1_demographicsEOL.csv"))


# ==================================================================================================
# Some basic stats (non-parametric and parametric tests)

stat.test.np <- eol_dataframe %>% select(all_of(NumVars), home_death) %>%
	summarise(across(!home_death, ~wilcox.test(.x ~ home_death)$p.value))

stat.test.param <- eol_dataframe %>% select(all_of(NumVars), home_death) %>%
	gather(key = variable, value = value, -home_death) %>%
	group_by(home_death, variable) %>%
	summarise(value = list(value)) %>%
	spread(home_death, value) %>%
	group_by(variable) %>%
	mutate(p_value = t.test(unlist(yes), unlist(no))$p.value,
		   t_value = t.test(unlist(yes), unlist(no))$statistic)


# ==================================================================================================
# Different linear regression models are applied in order to reduce dimensionality/extract the most meaningful predictors

factors_regression = c("gender", "age", "age_at_diagnosis", "duration", "german", "married",
					   "religious_affiliation","receiving_nursing_support", "rurality",
					   "professional_education", "existence_advance_directive",
					   "attorney_power", "palliative_care_knowledge", "hospice_knowledge",
					   "oftenEOLwishes_thoughts", "home_care", "Charlson_withage", "PDQ_score",
					   "bdi_score", "MOCA_score", "disease_severityPC")

data_full_glm <- eol_dataframe %>% select(all_of(factors_regression), home_death) %>%
	mutate(across(c(1,5:9, 11:16, 22), as.factor)) # convert to factors whereever needed
data_full_glm$bdi_score[59] = 9 #ToDo: should be confirmed
data_full_glm <- data_full_glm %>%
	mutate(across(10, as.numeric)) # convert to factors

data_full_glm <- data_full_glm %>%
	mutate(across(16, as.factor)) # convert to factors
data_full_glm <- droplevels(data_full_glm) %>% drop_na()

# ==================================================================================================
## GLM analyses, that is full model vs. model w/ stepwise reduction (glmStepAIC) and ElasticNet penalised
# regressionfrom {caret} package; analyses in parts adapted from: https://rpubs.com/mpfoley73/625323

# Separate into train and test dataset
index 		<- createDataPartition(data_full_glm$home_death, p = 0.8, list = FALSE) # split w/ balance for home_death
train_data 	<- data_full_glm[index,]
test_data 	<- data_full_glm[-index,]

train_control <- trainControl(method 			= "repeatedcv",
							  number 			= 5,
							  repeats			= 10,
							  summaryFunction 	= mnLogLoss,
							  savePredictions 	= "final",
							  classProbs 		= TRUE,
							  verboseIter 		= TRUE)

# ==================================================================================================
# a) Estimate the distinct models; (results_model) is defined in functionsUsed.R and basically
# estimated a GLM based on the input and taking advantage of the {caret}-package

mdl_full = results_model(method = 'glm',  data = data_full_glm, train_control = train_control, tunegrid = NULL,
						 test_data = test_data, model_name= 'Full GLM')
annotation_full <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_full[[3]]$overall[[1]],
														mdl_full[[3]]$overall[[3]],
														mdl_full[[3]]$overall[[4]]))


mdl_step = results_model(method = 'glmStepAIC', data = data_full_glm, train_control = train_control, tunegrid = NULL,
						 test_data = test_data, model_name= 'Stepwise reduced GLM')
annotation_step <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_step[[3]]$overall[[1]],
														mdl_step[[3]]$overall[[3]],
														mdl_step[[3]]$overall[[4]]))


lambda.grid <- seq(0.0001, 1, length = 100) #seq(0, 100)
alpha.grid <- seq(0, 1, length = 11) #1
grid_total <- expand.grid(alpha = alpha.grid,
						  lambda = lambda.grid)
mdl_pen = results_model(method = 'glmnet',  data = data_full_glm, train_control = train_control, tunegrid = grid_total,
						test_data = test_data, model_name= 'ElasticNet regularization')
annotation_pen <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
													   mdl_pen[[3]]$overall[[1]],
													   mdl_pen[[3]]$overall[[3]],
													   mdl_pen[[3]]$overall[[4]]))

# ==================================================================================================
# Print results in separate subfigures (deprecated)

fig2a <- print_AUC(mdl_full[[1]], test_data = test_data, annotation = annotation_full,
				   subtitle="Full GLM")
fig2b <- print_AUC(mdl_step[[1]], test_data = test_data, annotation = annotation_step,
				   subtitle="Stepwise reduced GLM (AIC)")
fig2c <- print_AUC(mdl_pen[[1]], test_data = test_data, annotation = annotation_pen,
				   subtitle="ElasticNet regularization")

# ==================================================================================================
# Bootstrap confidence intervals for the models

file2save_bootstrap <- file.path(wdir, "results", "CIdataBootstrap.v2.1.Rdata")

if (!file.exists(file2save_bootstrap)){ # loads data if existent
	nboot = 1000
	CI_full <- results_bootstrap(method='glm', data=data_full_glm, test_data=test_data,
								 model_name='FULL', nboot = nboot)
	CI_step <- results_bootstrap(method='glmStepAIC', data=data_full_glm, test_data=test_data,
								model_name='STEP', nboot = nboot)
	CI_pen <- results_bootstrap(method='glmnet', data=data_full_glm, test_data=test_data,
							   model_name='PEN', nboot = nboot)
	save(list = c("CI_full", "CI_step", "CI_pen"),
		 file = file2save_bootstrap)
} else {
	load(file2save_bootstrap)
}

# ==================================================================================================
# Plot metrics for distinct models
data2plotwCI 	<- 	bind_rows(CI_full[[1]], CI_step[[1]], CI_pen[[1]], .id="model_name") %>%
	mutate(model_name = recode(model_name, "1" = "Full GLM", "2" = "Stepwise GLM", "3" = "ElasticNet regularization")) %>%
	select(model_name, AUC, LogLoss, Accuracy) %>%
	group_by(model_name) %>%
	summarize_all(funs(mean, sd, se=sd(.)/sqrt(n())))

WhiskerDataTemp <- data2plotwCI %>% select(., c(model_name, matches("sd"), -matches("LogLoss"))) %>%
	pivot_longer(!model_name, names_to="metric")

AUC_Accuracy <- data2plotwCI %>% select(., c(model_name, matches("mean"), -matches("LogLoss"))) %>%
	pivot_longer(!model_name, names_to="metric") %>% add_column(val_whis=WhiskerDataTemp$value) %>%
	mutate(metric = recode(metric, "AUC_mean" = "AUC", "Accuracy_mean" = "Accuracy"))

p_comparison_models <- AUC_Accuracy %>%
	ggplot(aes(fill = model_name, y = value, x = metric)) +
	geom_bar(position = position_dodge(.9), stat = "identity") +
	geom_errorbar(aes(ymin=value-val_whis, ymax=value+val_whis), width=.1, position = position_dodge(.9))  +
	#	 scale_x_discrete(limits = rep(c("Full GLM", "Stepwise reduced GLM", "ElasticNet penalisation"), 3)) +
	#scale_fill_manual(values = c("#7A8B99", "#A9DDD6")) +
	theme_minimal() +
	theme(text = element_text(size = 20),
		  plot.caption = element_text(hjust = .7, face="italic"),
		  legend.title = element_text(hjust = .5, color = "black", size = 20, face = "bold"),
		  axis.text = element_text(size = 20),
		  legend.position = "none", #c(0.86, 0.9),
		  plot.margin = margin(t = 10, unit = "pt")) +
	ylim(0,max(select_if(AUC_Accuracy, is.numeric))+ .4) +
	scale_fill_brewer(palette = 1) +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "",
		caption = "") +
	geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 5,
			  position = position_dodge(width= 0.9)) #+
p_comparison_models

WhiskerDataTemp <- data2plotwCI %>% select(., c(model_name, matches("LogLoss"))) %>%
	select(., c(model_name, matches("sd"))) %>%
	pivot_longer(!model_name, names_to="metric")

LogLoss <- data2plotwCI %>% select(., c(model_name, matches("LogLoss_mean"))) %>%
	pivot_longer(!model_name, names_to="metric") %>% add_column(val_whis=WhiskerDataTemp$value) %>%
	mutate(metric = recode(metric, "LogLoss_mean" = "logLoss"))

p_comparison_models2 <- LogLoss %>%
	ggplot(aes(fill = model_name, y = value, x = metric)) +
	geom_bar(position = position_dodge(.9), stat = "identity") +
	geom_errorbar(aes(ymin=value-val_whis, ymax=value+val_whis), width=.1, position = position_dodge(.9))  +
	#	 scale_x_discrete(limits = rep(c("Full GLM", "Stepwise reduced GLM", "ElasticNet penalisation"), 3)) +
	#scale_fill_manual(values = c("#7A8B99", "#A9DDD6")) +
	theme_minimal() +
	theme(text = element_text(size = 20),
		  plot.caption = element_text(hjust = .7, face="italic"),
		  legend.title = element_text(hjust = .5, color = "black", size = 20, face = "bold"),
		  axis.text = element_text(size = 20),
		  legend.position = c(0.54, 0.9),
		  plot.margin = margin(t = 10, unit = "pt")) +
	ylim(0,max(select_if(LogLoss,is.numeric) * 2.2)) +
	scale_fill_brewer(palette = 1) +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "",
		caption = "") +
	geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 5,
			  position = position_dodge(width= 0.9)) #+
# coord_capped_cart()
p_comparison_models2

pdf(file = file.path(wdir, "results", "Figure1.model_comparison.v1.0.pdf"))
grid.arrange(p_comparison_models, p_comparison_models2, nrow = 1,
			 top = text_grob("Comparing full model with \nstepwise reduced and regularised model",
							 size = 16, face = "bold"),
			 bottom = text_grob(label=
									"Higher values indicate better performance: Accuracy and AUC\nLower values indicate better performance: logLoss",
								size = 9, face = "italic", hjust = 0))
dev.off()

# ==================================================================================================
# Plot confidence intervals from the penalised model for all factors

pdf(file = file.path(wdir, "results", "Suppl.Figure1.coefsBootstrapAll.v1.0.pdf"))
coefs = data.frame(as.matrix(coef(mdl_pen[[1]]$finalModel, mdl_pen[[1]]$bestTune$lambda)))
CIpen2plot <- CI_pen[[2]] %>% drop_na() %>% select(2:dim(CI_pen[[2]])[2])
r = colSums(CIpen2plot == 0)

ggplot(stack(CI_pen[[2]]), aes(x = ind, y = values)) +
	geom_boxplot() +
	geom_jitter(width=0.15, alpha=0.1) +
	theme_minimal() +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) ",
		caption = "") +
	#stat_summary(fun.y="median", colour="red", geom="text", show_guide = FALSE, position = position_dodge(width = .75),
	#       aes( label=round(..y.., digits=2)))
	theme(plot.title = element_text(size=22)) +
	scale_color_brewer(palette = 1) +
	ylim(c(-2,2))
dev.off()

pdf(file = file.path(wdir, "results", "Figure2.coefsBootstrapPenalisedModel.v1.0.pdf"))
idx_CIpen <- rownames(coefs)[which(coefs!=0)]
data2plot <- CI_pen[[2]] %>% select(all_of(idx_CIpen)) %>% select(2:5,)
colnames(data2plot) <- c("Receiving informal \nsupport", "Professional \neducation",
						 "Prefered place \nof care in an \ninstitution",
						 "Prefered place \nof care at \nother place")
ggplot(stack(data2plot), aes(x = ind, y = values)) +
	geom_boxplot() +
	ylim(c(-2,2)) +
	scale_x_discrete(labels = colnames(data2plot)) +
	theme_minimal() +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) ",
		caption = "") +
	#stat_summary(fun.y="median", colour="red", geom="text", show_guide = FALSE, position = position_dodge(width = .75),
	#       aes( label=round(..y.., digits=2)))
	theme(plot.title = element_text(size=22)) +
	scale_fill_brewer(palette = 1)
dev.off()

# ==================================================================================================
# Create table for penalized reduced model
mdl_pen_final 	<- mdl_pen[[1]]
coefs <- data.frame(as.matrix(coef(mdl_pen_final$finalModel, mdl_pen_final$bestTune$lambda)))
sig_predictors 	<- which(coefs != 0)
mdl_pen_sig 	<- data.frame(predictor = c("(Intercept)", "Receiving informal support", "Professional education",
								   "Prefered place of care at institution",
									"Prefered place of care at other place"), coef=coefs[sig_predictors,])
write.csv(mdl_pen_sig, file.path(wdir, "results", "table2.ResultsElasticNet_model.v1.0.csv"), row.names = T) # csv-file may be easily imported into text processing software


# ==================================================================================================
# Create table for stepwise reduced model
mdl_step_final 	<- mdl_step[[1]]
sig_predictors 	<- attr(which(summary(mdl_step_final)$coef[idx_coefs,4] <= .05), "names")
mdl_step_sig 	<- data.frame(summary(mdl_step_final)$coef)
mdl_step_sig 	<- mdl_step_sig[row.names(mdl_step_sig) %in% sig_predictors, ]
rownames(mdl_step_sig) <- c("Age", "Receiving informal support", "Prefered place of care at other place",
                            "Charlson comorbidity score including age", "PDQ-39 score")
write.csv(mdl_step_sig, file.path(wdir, "results", "table3.ResultsStepWiseReduced_model.v1.0.csv"), row.names = T) # csv-file may be easily imported into text processing software

