# Code developed by Anna and David Pedrosa

# Version 2.2 # 2023-02-26, added analyses with HOMECARE

## First, specify the packages of interest
packages = c("readxl", "tableone", "ggplot2", "tidyverse", "lemon", "openxlsx", "caret", "corrplot", "gridExtra",
			 "psych", "DescTools", "jtools", "rstatix", "ggpubr", "dplyr", "precrec", "MLmetrics", "labelled")
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


# This is code to run analyses for the palliative care project;
# ==================================================================================================
# Summarise questionnaires and recode variables
source("summarise_questionnaires.r")  			# questionnaires (UPDRS, PDQ, MoCA)
source("recode_dataframe.r") 	# data is recoded and structured according to labels


# ==================================================================================================
# Create TableOne with all data

allVars <- c(	"gender", "age", "duration", "cat.marital_status", "cat.education",
				 "religious_affiliation", "cat.independent_living", "Cohabitation", "cat.nursing_support",
				 "cat.residential_location", "cat.advance_directive", "cat.power_attorney",
				 "cat.palliative_care_knowledge", "cat.hospice_knowledge",
				 "cat.prefered_place_of_care", "pod.family_friends",
				 "pod.GP", "pod.neurologist", "pod.AD", "LEDD", "Hoehn_Yahr", "PDQ_score",
				 "updrs_sum", "bdi_score", "MOCA_score", "Charlson_withage")

catVars <- c(	"gender", "cat.marital_status", "cat.education", "religious_affiliation",
				 "cat.independent_living", "cat.nursing_support", "cat.residential_location",
				 "cat.advance_directive", "cat.power_attorney", "cat.palliative_care_knowledge",
				 "cat.hospice_knowledge", "cat.prefered_place_of_care", "pod.family_friends",
				 "pod.GP", "pod.neurologist", "pod.AD", "Hoehn_Yahr")

NumVars <- c(	"age", "duration", "Cohabitation", "LEDD", "PDQ_score","updrs_sum",
				 "bdi_score", "MOCA_score", "Charlson_withage")

renameTab1 <- list(gender = "Male",
                       age = "Age, years",
                       duration = "Time since diagnosis, years",
                       cat.marital_status = "Marital status",
                       cat.education = "Professional education",
                       religious_affiliation = "Religious/spiritual affiliation",
                       cat.independent_living = "Living situation",
                       Cohabitation = "No. of household members",
                       cat.nursing_support = "Nursing support",
                       cat.residential_location = "Residential location",
                       cat.advance_directive = "Advance directive (AD)",
                       cat.power_attorney = "Power of attorney",
                       cat.palliative_care_knowledge = "Palliative care knowledge",
                       cat.hospice_knowledge = "Hospice knowledge",
                       cat.prefered_place_of_care = "Preferred place of care",
                       pod.family_friends = " pPOD shared with family/friends",
                       pod.GP = "pPOD shared with GP",
                       pod.neurologist = "pPOD shared with neurologist",
                       pod.AD = "pPOD documented in AD",
                       Hoehn_Yahr = "Hoehn & Yahr",
                       PDQ_score = "PDQ-39",
                       updrs_sum = "MDS-UPDRS",
                       bdi_score = "BDI-II",
                       MOCA_score = "MoCA",
                       Charlson_withage = "CCI")

tab1 <- CreateTableOne(vars = allVars, strata = c("home_death"),
					   data = eol_dataframe,
					   factorVars = catVars, addOverall = TRUE)
print(tab1)
write.csv(print(tab1, quote = FALSE, test=FALSE, contDigits = 1,
				noSpaces = TRUE, printToggle = FALSE, showAllLevels = FALSE),
		  file = file.path(wdir, "results", "table1a_demographicsEOL.csv"))

tab1mod <- CreateTableOne(vars = allVars, #c(setdiff(allVars, 'cat.prefered_place_of_care'), 'cat.prefered_place_of_death'),
						  strata = c("home_care"),
						  data = eol_dataframe, factorVars = catVars, addOverall = FALSE)
write.csv(print(tab1mod, quote = FALSE, test=FALSE, contDigits = 1,
				noSpaces = TRUE, printToggle = FALSE, showAllLevels = FALSE),
		  file = file.path(wdir, "results", "table1b_demographicsEOLmod.csv"))

## START ANALYSES FOR HOME DEATH
# ==================================================================================================
# Some basic stats (non-parametric and parametric tests)

stat.test.npHOMEDEATH <- eol_dataframe %>% select(all_of(NumVars), home_death) %>%
	summarise(across(!home_death, ~wilcox.test(.x ~ home_death)$p.value))

stat.test.paramHOMEDEATH <- eol_dataframe %>% select(all_of(NumVars), home_death) %>%
	gather(key = variable, value = value, -home_death) %>%
	group_by(home_death, variable) %>%
	summarise(value = list(value)) %>%
	spread(home_death, value) %>%
	group_by(variable) %>%
	mutate(p_value = t.test(unlist(yes), unlist(no))$p.value,
		   t_value = t.test(unlist(yes), unlist(no))$statistic)


# ==================================================================================================
# Linear regression models to reduce dimensionality/extract the most meaningful predictors (HOMEDEATH)

factors_regression = c("gender", "age", "age_at_diagnosis", "duration", "german", "married",
					   "religious_affiliation","receiving_nursing_support", "rurality",
					   "professional_education", "existence_advance_directive", "attorney_power",
					   "palliative_care_knowledge", "hospice_knowledge", "oftenEOLwishes_thoughts",
					   "home_care",  																					 # only change w/ respect to HOMECARE below
					   "Charlson_withage", "PDQ_score", "bdi_score", "MOCA_score", "disease_severityPC")

data_full_glmHOMEDEATH <- eol_dataframe %>% select(all_of(factors_regression), home_death) %>%
	mutate(across(c(1,5:9, 11:16, 22), as.factor)) # convert to factors whereever needed
data_full_glmHOMEDEATH <- data_full_glmHOMEDEATH %>%
	mutate(across(10, as.numeric)) # convert to factors

data_full_glmHOMEDEATH <- data_full_glmHOMEDEATH %>%
	mutate(across(16, as.factor)) # convert to factors
data_full_glmHOMEDEATH <- droplevels(data_full_glmHOMEDEATH) %>% drop_na()

# ==================================================================================================
## GLM analyses, that is full model vs. model w/ stepwise reduction (glmStepAIC) and ElasticNet penalised
# regressionfrom {caret} package; analyses in parts adapted from: https://rpubs.com/mpfoley73/625323

# Separate into train and test dataset
index 		<- createDataPartition(data_full_glmHOMEDEATH$home_death, p = 0.8, list = FALSE) # split w/ balance for home_death
train_data 	<- data_full_glmHOMEDEATH[index,]
test_data 	<- data_full_glmHOMEDEATH[-index,]

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

mdl_fullHOMEDEATH = results_modelHOMEDEATH(method = 'glm',
						 data = data_full_glmHOMEDEATH,
						 train_control = train_control,
						 tunegrid = NULL,
						 test_data = test_data, model_name= 'Full GLM')
annotation_fullHOMEDEATH <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_fullHOMEDEATH[[3]]$overall[[1]],
														mdl_fullHOMEDEATH[[3]]$overall[[3]],
														mdl_fullHOMEDEATH[[3]]$overall[[4]]))


mdl_stepHOMEDEATH = results_modelHOMEDEATH(method = 'glmStepAIC',
						 data = data_full_glmHOMEDEATH,
						 train_control = train_control,
						 tunegrid = NULL,
						 test_data = test_data, model_name= 'Stepwise reduced GLM')
annotation_stepHOMEDEATH <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_stepHOMEDEATH[[3]]$overall[[1]],
														mdl_stepHOMEDEATH[[3]]$overall[[3]],
														mdl_stepHOMEDEATH[[3]]$overall[[4]]))


lambda.grid <- seq(0.0001, 1, length = 100) #seq(0, 100)
alpha.grid <- seq(0, 1, length = 11) #1
grid_total <- expand.grid(alpha = alpha.grid,
						  lambda = lambda.grid)
mdl_penHOMEDEATH = results_modelHOMEDEATH(method = 'glmnet',
						data = data_full_glmHOMEDEATH,
						train_control = train_control,
						tunegrid = grid_total,
						test_data = test_data, model_name= 'ElasticNet regularization')
annotation_penHOMEDEATH <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
													   mdl_penHOMEDEATH[[3]]$overall[[1]],
													   mdl_penHOMEDEATH[[3]]$overall[[3]],
													   mdl_penHOMEDEATH[[3]]$overall[[4]]))

# ==================================================================================================
# Print results in separate subfigures (not really informative, therefore deprecated)

fig99a <- print_AUC(mdl_fullHOMEDEATH[[1]], test_data = test_data, annotation = annotation_fullHOMEDEATH,
				   subtitle="Full GLM")
fig99b <- print_AUC(mdl_stepHOMEDEATH[[1]], test_data = test_data, annotation = annotation_stepHOMEDEATH,
				   subtitle="Stepwise reduced GLM (AIC)")
fig99c <- print_AUC(mdl_penHOMEDEATH[[1]], test_data = test_data, annotation = annotation_penHOMEDEATH,
				   subtitle="ElasticNet regularization")

# ==================================================================================================
# Bootstrap confidence intervals for the models (HOME DEATH); this takes a while and so results are saved once locally

file2save_bootstrap <- file.path(wdir, "results", "CIdataBootstrapHOMEDEATH.v2.1.Rdata")
if (!file.exists(file2save_bootstrap)){ # loads data if existent
	nboot = 1000
	CI_full <- results_bootstrap(method='glm', data=data_full_glmHOMEDEATH, test_data=test_data,
								 model_name='FULL', nboot = nboot, predictor = 'HOMEDEATH')
	CI_step <- results_bootstrap(method='glmStepAIC', data=data_full_glmHOMEDEATH, test_data=test_data,
								model_name='STEP', nboot = nboot, predictor = 'HOMEDEATH')
	CI_pen <- results_bootstrap(method='glmnet', data=data_full_glmHOMEDEATH, test_data=test_data,
							   model_name='PEN', nboot = nboot, predictor = 'HOMEDEATH')
	save(list = c("CI_full", "CI_step", "CI_pen"),
		 file = file2save_bootstrap)
} else {
	load(file2save_bootstrap)
}

# ==================================================================================================
# Plot metrics for distinct models
plot_results_withCI(CI_full, CI_step, CI_pen, "Home Death", "Figure1.model_comparisonHOMEDEATH.v1.0.pdf")

# ==================================================================================================
# Plot confidence intervals from the penalised model for all factors
pdf(file	<- file.path(wdir, "results", "Suppl.Figure1.coefsBootstrapHOMEDEATH.v1.0.pdf"))
coefs 		<- data.frame(as.matrix(coef(mdl_penHOMEDEATH[[1]]$finalModel,
								  mdl_penHOMEDEATH[[1]]$bestTune$lambda))) # extracts all coefficients from the  penalised model with all data
CIpen2plot 				<- CI_pen[[2]] %>% drop_na() %>% select(2:dim(CI_pen[[2]])[2])
# r 					<- colSums(CIpen2plot == 0)
names_predictors		<- colnames(CI_pen[[2]]) 	# extracts the predictors to plot later as a sort of "legend"
colnames(CI_pen[[2]]) 	<- 1:length(CI_pen[[2]])  	# replaces predictors with numbers to make plot easier to read
ggplot(stack(CI_pen[[2]]), aes(x = ind, y = values)) +
	geom_boxplot() +
	geom_jitter(width=0.15, alpha=0.1) +
	theme_minimal() +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) - HOME DEATH ",
		caption = "") +
	theme(plot.title = element_text(size=22)) +
	scale_color_brewer(palette = 1) +
	ylim(c(-2,2))
dev.off()

# ==================================================================================================
# Plot confidence intervals from the penalised model for all factors
pdf(file = file.path(wdir, "results", "Figure2.coefsBootstrapPenalisedModelHOMEDEATH.v1.0.pdf"))
idx_CIpen <- rownames(coefs)[which(coefs!=0)]
colnames(CI_pen[[2]]) <- names_predictors
data2plot <- CI_pen[[2]] %>% select(all_of(idx_CIpen)) %>% select(-"(Intercept)")
colnames(data2plot) <- c(	"Disease duration", "Religious affiliation",
							"Receiving informal \n nursingsupport",
							 "Often end of life \nwishes or thoughts",
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
		title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) \nDependent variable: Home death ",
		caption = "") +
	#stat_summary(fun.y="median", colour="red", geom="text", show_guide = FALSE, position = position_dodge(width = .75),
	#       aes( label=round(..y.., digits=2)))
	theme(plot.title = element_text(size=22)) +
	scale_fill_brewer(palette = 1)
dev.off()

# ==================================================================================================
# Create table for penalized reduced model
mdl_pen_final 	<- mdl_penHOMEDEATH[[1]]
coefs <- data.frame(as.matrix(coef(mdl_pen_final$finalModel, mdl_pen_final$bestTune$lambda)))
sig_predictors 	<- which(coefs != 0)
mdl_pen_sig 	<- data.frame(predictor =
								 c(	"(Intercept)", "Disease duration", "Religious affiliation",
									   "Receiving informal \n nursingsupport",
									   "Often end of life \nwishes or thoughts",
									 	"Prefered place \nof care in an \ninstitution",
										"Prefered place \nof care at \nother place"),
								 coef=coefs[sig_predictors,])
write.csv(mdl_pen_sig, file.path(wdir, "results", "table2.ResultsElasticNet_modelHOMEDEATH.v1.0.csv"),
		  row.names = T) # csv-file may be easily imported into text processing software


# ==================================================================================================
# Create table for stepwise reduced model
mdl_step_final 	<- mdl_stepHOMEDEATH[[1]]
# sig_predictors 	<- attr(which(summary(mdl_step_final)$coef[,4] <= .05), "names")
mdl_step_sig 	<- data.frame(summary(mdl_step_final)$coef)
sig_predictors <- which(mdl_step_sig[,4]<.05 | mdl_step_sig[,4]>.95)
mdl_step_sig 	<- mdl_step_sig[sig_predictors, ]
rownames(mdl_step_sig) <- c("(Intercept)", "Age", "Religious affiliation", "Receiving informal support",
							"Prefered place of care in an institution",
                            "Charlson comorbidity score including age", "PDQ-39 score")
write.csv(mdl_step_sig, file.path(wdir, "results", "table3.ResultsStepWiseReduced_modelHOMEDEATH.v1.0.csv"),
		  row.names = T) # csv-file may be easily imported into text processing software


## START ANALYSES FOR HOME CARE
# ==================================================================================================
# Some basic stats (non-parametric and parametric tests)

stat.test.npHOMECARE <- eol_dataframe %>% select(all_of(NumVars), home_careBINARY) %>%
	summarise(across(!home_careBINARY, ~wilcox.test(.x ~ home_careBINARY)$p.value))

stat.test.paramHOMECARE <- eol_dataframe %>% select(all_of(NumVars), home_careBINARY) %>%
	gather(key = variable, value = value, -home_careBINARY) %>%
	group_by(home_careBINARY, variable) %>%
	summarise(value = list(value)) %>%
	spread(home_careBINARY, value) %>%
	group_by(variable) %>%
	mutate(p_value = t.test(unlist(yes), unlist(no))$p.value,
		   t_value = t.test(unlist(yes), unlist(no))$statistic)

# ==================================================================================================
# Linear regression models to reduce dimensionality/extract the most meaningful predictors (HOMECARE)

factors_regression = c("gender", "age", "age_at_diagnosis", "duration", "german", "married",
					   "religious_affiliation","receiving_nursing_support", "rurality",
					   "professional_education", "existence_advance_directive", "attorney_power",
					   "palliative_care_knowledge", "hospice_knowledge", "oftenEOLwishes_thoughts",
					   "home_death",  																					 # only change w/ respect to HOMEDEATH above
					   "Charlson_withage", "PDQ_score", "bdi_score", "MOCA_score", "disease_severityPC")

data_full_glmHOMECARE <- eol_dataframe %>% select(all_of(factors_regression), home_careBINARY) %>%
	mutate(across(c(1,5:9, 11:16, 22), as.factor)) # convert to factors whereever needed
data_full_glmHOMECARE <- data_full_glmHOMECARE %>%
	mutate(across(10, as.numeric)) # convert to factors

data_full_glmHOMECARE <- data_full_glmHOMECARE %>%
	mutate(across(16, as.factor)) # convert to factors
data_full_glmHOMECARE <- droplevels(data_full_glmHOMECARE) %>% drop_na()


# ==================================================================================================
## GLM analyses, that is full model vs. model w/ stepwise reduction (glmStepAIC) and ElasticNet penalised
# see above for further details

# Separate into train and test dataset
index 		<- createDataPartition(data_full_glmHOMECARE$home_careBINARY, p = 0.8, list = FALSE) # split w/ balance for home_careBINARY
train_data 	<- data_full_glmHOMECARE[index,]
test_data 	<- data_full_glmHOMECARE[-index,]

# train_control is used in the identical way as [HOMEDEATH] model above


# ==================================================================================================
# a) Estimate the distinct models; (results_model) is defined in functionsUsed.R and basically
# estimated a GLM based on the input and taking advantage of the {caret}-package

mdl_fullHOMECARE = results_modelHOMECARE(method = 'glm',
						 data = data_full_glmHOMECARE,
						 train_control = train_control,
						 tunegrid = NULL,
						 test_data = test_data, model_name= 'Full GLM')
annotation_fullHOMECARE <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_fullHOMECARE[[3]]$overall[[1]],
														mdl_fullHOMECARE[[3]]$overall[[3]],
														mdl_fullHOMECARE[[3]]$overall[[4]]))


mdl_stepHOMECARE = results_modelHOMECARE(method = 'glmStepAIC',
						 data = data_full_glmHOMECARE,
						 train_control = train_control,
						 tunegrid = NULL,
						 test_data = test_data, model_name= 'Stepwise reduced GLM')
annotation_stepHOMECARE <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_stepHOMECARE[[3]]$overall[[1]],
														mdl_stepHOMECARE[[3]]$overall[[3]],
														mdl_stepHOMECARE[[3]]$overall[[4]]))


lambda.grid <- seq(0.0001, 1, length = 100) #seq(0, 100)
alpha.grid <- seq(0, 1, length = 11) #1
grid_total <- expand.grid(alpha = alpha.grid,
						  lambda = lambda.grid)
mdl_penHOMECARE = results_modelHOMECARE(method = 'glmnet',
						data = data_full_glmHOMECARE,
						train_control = train_control,
						tunegrid = grid_total,
						test_data = test_data, model_name= 'ElasticNet regularization')
annotation_penHOMECARE <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
													   mdl_penHOMECARE[[3]]$overall[[1]],
													   mdl_penHOMECARE[[3]]$overall[[3]],
													   mdl_penHOMECARE[[3]]$overall[[4]]))

# ==================================================================================================
# Print results in separate subfigures (not really informative, therefore deprecated)

fig98a <- print_AUC(mdl_fullHOMECARE[[1]], test_data = test_data, annotation = annotation_fullHOMECARE,
				   subtitle="Full GLM")
fig98b <- print_AUC(mdl_stepHOMECARE[[1]], test_data = test_data, annotation = annotation_stepHOMECARE,
				   subtitle="Stepwise reduced GLM (AIC)")
fig98c <- print_AUC(mdl_penHOMECARE[[1]], test_data = test_data, annotation = annotation_penHOMECARE,
				   subtitle="ElasticNet regularization")

# ==================================================================================================
# Bootstrap confidence intervals for the models (HOME CARE); this takes a while and so results are saved once locally

file2save_bootstrap <- file.path(wdir, "results", "CIdataBootstrapHOMECARE.v2.0.Rdata")
if (!file.exists(file2save_bootstrap)){ # loads data if existent
	nboot = 1000
	CI_full <- results_bootstrap(method='glm', data=data_full_glmHOMECARE, test_data=test_data,
								 model_name='FULL', nboot = nboot, predictor = 'HOMECARE')
	CI_step <- results_bootstrap(method='glmStepAIC', data=data_full_glmHOMECARE, test_data=test_data,
								model_name='STEP', nboot = nboot, predictor = 'HOMECARE')
	CI_pen <- results_bootstrap(method='glmnet', data=data_full_glmHOMECARE, test_data=test_data,
							   model_name='PEN', nboot = nboot, predictor = 'HOMECARE')
	save(list = c("CI_full", "CI_step", "CI_pen"),
		 file = file2save_bootstrap)
} else {
	load(file2save_bootstrap)
}

# ==================================================================================================
# Plot metrics for distinct models
plot_results_withCI(CI_full, CI_step, CI_pen, "Home Care", "Figure3.model_comparisonHOMECARE.v1.0.pdf")

# ==================================================================================================
# Plot confidence intervals from the penalised model for all factors
pdf(file	<- file.path(wdir, "results", "Suppl.Figure1.coefsBootstrapHOMECARE.v1.0.pdf"))
coefs 		<- data.frame(as.matrix(coef(mdl_penHOMECARE[[1]]$finalModel,
								  mdl_penHOMECARE[[1]]$bestTune$lambda))) # extracts all coefficients from the  penalised model with all data
CIpen2plot <- CI_pen[[2]] %>% drop_na() %>% select(2:dim(CI_pen[[2]])[2])
# r = colSums(CIpen2plot == 0)
names_predictors <- colnames(CI_pen[[2]]) 		# extracts the predictors to plot later as a sort of "legend"
colnames(CI_pen[[2]]) <- 1:length(CI_pen[[2]])  # replaces predictors with numbers to make plot easier to read
ggplot(stack(CI_pen[[2]]), aes(x = ind, y = values)) +
	geom_boxplot() +
	geom_jitter(width=0.15, alpha=0.1) +
	theme_minimal() +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "Bootstrapped coefficients for the penalised\n regression model (ElasticNet) - HOME CARE ",
		caption = "") +
	theme(plot.title = element_text(size=22)) +
	scale_color_brewer(palette = 1) +
	ylim(c(-2,2))
dev.off()
