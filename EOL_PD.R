# This is code to run analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 1.2 # 2023-02-05, added confidence interval estimation via bootrstrap

## First specify the packages of interest
packages = c("readxl", "tableone", "ggplot2", "tidyverse", "lemon", "openxlsx", "caret", "corrplot",
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
eol_dataframe 	<- read_xlsx(file.path(data_dir, "Matrix_EOL_PD_.xlsx"))

# Read and convert coding/explanations from xlsx-file
dataframe_codes <- read_excel(file.path(data_dir, "Matrix_EOL_PD_.xlsx"), sheet = "explanations")
dataframe_codes_clean <- dataframe_codes %>%
	drop_na(starts_with("0")) %>% 				# Remove rows with NAs in columns 3 and beyond
	select(-Unit)  								# Drop the "Unit" column


# ==================================================================================================
# Recode variables
source("recode_dataframe.r") 	# data is recoded and structured according to labels
eol_dataframe$Cohabitation[eol_dataframe$Cohabitation>6] = NA 								# Outliers removed!
eol_dataframe$cat.education[eol_dataframe$cat.education=='Other'] = NA 						# Subj. claiming 'Other' discarded!
eol_dataframe$professional_education[eol_dataframe$professional_education=='other'] = NA 	# Subj. claiming 'Other' discarded!


# ==================================================================================================
# Recode variables
source("summarise_questionnaires.r")  			# questionnaires (UPDRS, PDQ, MoCA)


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

NumVars <- c(	"age", "age_at_diagnosis", "duration", "LEDD", "PDQ_score","UPDRS_sum", "bdi_score", "MOCA_score",
				 "Charlson_withoutage","Charlson_withage", "Cohabitation")

tab2 <- CreateTableOne(vars = allVars, data = eol_dataframe, factorVars = catVars)
print(tab2)
write.csv(print(tab2, quote = FALSE,
				noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE),
		  file = file.path(wdir, "results", "TableOne_EOL.csv"))


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
model_est 	<- data.frame(model_name=c("Full GLM", "Stepwise reduced GLM", "ElasticNet regularization"),
						   AUC=c(NA, NA, NA), LogLoss=c(NA, NA, NA), Accuracy=c(NA, NA, NA))

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
# Print results in separate subfigures

fig2a = print_AUC(mdl_full[[1]], test_data = test_data, annotation = annotation_full,
				  subtitle="Full GLM")
fig2b = print_AUC(mdl_step[[1]], test_data = test_data, annotation = annotation_step,
				  subtitle="Stepwise reduced GLM (AIC)")
fig2c = print_AUC(mdl_pen[[1]], test_data = test_data, annotation = annotation_pen,
				  subtitle="ElasticNet regularization")

# ==================================================================================================
# Bootstrap confidence intervals for the models
nboot = 1000
CI_full = results_bootstrap(method='glm', data=data_full_glm, test_data=test_data,
							model_name='FULL', nboot = nboot)
CI_step = results_bootstrap(method='glmStepAIC', data=data_full_glm, test_data=test_data,
							model_name='STEP', nboot = nboot)
CI_pen = results_bootstrap(method='glmnet', data=data_full_glm, test_data=test_data,
						   model_name='PEN', nboot = nboot)
save(list = c("CI_full", "CI_step", "CI_pen"),
	 file = file.path(wdir, "results", "CIdataBootstrap.v2.0.Rdata"))

# ==================================================================================================
# PLoit results of different models



coefs = data.frame(as.matrix(coef(mdl_pen[[1]]$finalModel, mdl_pen[[1]]$bestTune$lambda)))


# ==================================================================================================
# b) stepwise regression using {caret}-package
# train_data = data_full_glm
mdl_step 	<- train(as.formula(paste( 'home_death', '~', '.')),
					 data=train_data,
					 method = 'glmStepAIC',
					 preProcess = c("nzv", "center", "scale"),
					 tuneLength = 25, #"ROC",
					 family="binomial",
					 trControl = train_control,
					 metric = "logLoss")

# Interpreting results {mdl_step}:
mdl_step
varImp(mdl_step$finalModel) # Factors most contributing to data are listed here

predicted_classes 	<- predict(mdl_step, newdata = test_data)
predicted_probs 	<- predict(mdl_step, newdata = test_data, type = "prob")
mdl_step_matrix 	<- confusionMatrix(predicted_classes, test_data$home_death, positive = "yes") # predictions on (independent) test data

mdl_step_preds <- predict(mdl_step, newdata = test_data, type = "prob")
(mdl_step_eval <- evalmod(
	scores = mdl_step_preds$yes,
	labels = test_data$home_death
))

mdl_step_prob <- predict(mdl_step, newdata = test_data, type = "prob") %>%
	bind_cols(predict(mdl_step, test_data)) %>%
	bind_cols(select(test_data, home_death))

trueLabelsBinary_step <- ifelse(mdl_step_prob$home_death=="yes", 1, 0)
predictedLabelsBinary <- ifelse(mdl_step_prob$`...3`=="yes", 1, 0)
model_est$LogLoss[2] = LogLoss(mdl_step_prob$yes, trueLabelsBinary_step)
model_est$AUC[2] = AUC(mdl_step_prob$yes, trueLabelsBinary_step)
model_est$Accuracy[2] = mdl_step_matrix$overall[[1]]

annotation_step <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
														mdl_step_matrix$overall[[1]],
														mdl_step_matrix$overall[[3]],
														mdl_step_matrix$overall[[4]]))

# Print AUC for the stepwise reduced model
options(yardstick.event_first = FALSE)  # set the second level as success
fig3b <- data.frame(pred = mdl_step_preds$yes, obs = test_data$home_death) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction of the model in the test dataset",
		 subtitle = "Stepwise reduced GLM") +
	geom_text(data=annotation_step, aes(x=x, y=y, label=label), color="black", fontface="bold") +
	coord_equal() +
	xlab("1 - specificity") + ylab("sensitivity")
fig3b

# ==================================================================================================
# c) penalized regression using {caret}-package"
# Formula for "Brier" cf. https://stackoverflow.com/questions/61014688/r-caret-package-brier-score?rq=1

# train_data = data_full_glm
lambda.grid <- seq(0.0001, 1, length = 100) #seq(0, 100)
alpha.grid <- seq(0, 1, length = 11) #1
grid_total <- expand.grid(alpha = alpha.grid,
						  lambda = lambda.grid)

mdl_pen 	<- train(as.formula(paste( 'home_death', '~', '.')),
					data=train_data,
					method = 'glmnet',
					preProcess = c("nzv", "center", "scale"),
					tuneLength = 25, #"ROC",
					family="binomial",
					trControl = train_control,
					metric = "logLoss", #"Brier",
					tuneGrid = grid_total )

# Interpreting results {mdl_pen}:
mdl_pen

# TODO: Save suppl. figure 1
plot(mdl_pen)
coefs = data.frame(as.matrix(coef(mdl_pen$finalModel, mdl_pen$bestTune$lambda)))


varImp(mdl_pen$finalModel) # Factors most contributing to data are listed here

predicted_classes 	<- predict(mdl_pen, newdata = test_data)
predicted_probs 	<- predict(mdl_pen, newdata = test_data, type = "prob")
mdl_pen_matrix 	<- confusionMatrix(predicted_classes, test_data$home_death, positive = "yes") # predictions on (independent) test data

mdl_pen_preds <- predict(mdl_pen, newdata = test_data, type = "prob")
(mdl_pen_eval <- evalmod(
	scores = mdl_pen_preds$yes,
	labels = test_data$home_death
))

mdl_pen_prob <- predict(mdl_pen, newdata = test_data, type = "prob") %>%
	bind_cols(predict(mdl_pen, test_data)) %>%
	bind_cols(select(test_data, home_death))

trueLabelsBinary_pen <- ifelse(mdl_pen_prob$home_death=="yes", 1, 0)
predictedLabelsBinary <- ifelse(mdl_pen_prob$`...3`=="yes", 1, 0)
model_est$LogLoss[3] = LogLoss(mdl_pen_prob$yes, trueLabelsBinary_pen)
model_est$AUC[3] = AUC(mdl_pen_prob$yes, trueLabelsBinary_step)
model_est$Accuracy[3] = mdl_pen_matrix$overall[[1]]

annotation_pen <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]",
													   mdl_pen_matrix$overall[[1]],
													   mdl_pen_matrix$overall[[3]],
													   mdl_pen_matrix$overall[[4]]))

# Print AUC for the FULL model
options(yardstick.event_first = FALSE)  # set the second level as success
fig3c <- data.frame(pred = mdl_pen_preds$yes, obs = test_data$home_death) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction of the model in the test dataset",
		 subtitle = "ElasticNet regularization") +
	geom_text(data=annotation_pen, aes(x=x, y=y, label=label), color="black", fontface="bold") +
	coord_equal() +
	xlab("1 - specificity") + ylab("sensitivity")
fig3c
# TODO: Save figure 2a/b/c


p_comparison_models <- model_est %>% pivot_longer(!model_name, names_to="metric") %>%
	# Compare models
	ggplot(aes(fill = model_name, y = value, x = metric)) +
	geom_bar(position = "dodge", stat = "identity") +
	#	 scale_x_discrete(limits = rep(c("Full GLM", "Stepwise reduced GLM", "ElasticNet penalisation"), 3)) +
	#scale_fill_manual(values = c("#7A8B99", "#A9DDD6")) +
	theme_minimal() +
	theme(text = element_text(size = 20),
		  plot.caption = element_text(hjust = .7, face="italic"),
		  legend.title = element_text(hjust = .5, color = "black", size = 20, face = "bold"),
		  axis.text = element_text(size = 20),
		  legend.position = c(0.86, 0.9),
		  plot.margin = margin(t = 10, unit = "pt")) +
	ylim(0,max(select_if(model_est, is.numeric))+ .4) +
	scale_fill_brewer(palette = 1) +
	labs(
		y = "",
		x = "",
		fill = NULL,
		title = "Comparing full model with stepwise reduced and regularised model",
		caption = "Higher values indicate better performance: Accuracy and AUC\nLower values indicate better performance: logLoss") +
	geom_text(aes(label = round(value, 3)), vjust = -0.5, size = 5, position = position_dodge(width= 0.9)) #+
# coord_capped_cart()
p_comparison_models


# ==================================================================================================
# Create table for stepwise reduced model #TODO: this is not correct and MUST be changed!
summary_mdl_step <- data.frame(	Terms=attr(summary(mdl_step)$terms , "term.labels")[summary(mdl_step)$coef[,4] <= .05],
								   Estimate=sprintf(summary(mdl_step)$coef[summary(mdl_step)$coef[,4] <= .05, 1], fmt="%#.2f"),
								   Std.Error=sprintf(summary(mdl_step)$coef[summary(mdl_step)$coef[,4] <= .05, 2], fmt="%#.2f"),
								   zvalue=sprintf(summary(mdl_step)$coef[summary(mdl_step)$coef[,4] <= .05, 3], fmt="%#.2f"),
								   p=sprintf(summary(mdl_step)$coef[summary(mdl_step)$coef[,4] <= .05, 4], fmt="%#.3f")
)
write.csv(summary_mdl_step, file.path(wdir, "results", "table_stepwise_model.csv"), row.names = F) # csv-file may be easily imported into text processing software
# TODO: Some manual refinement of the table necessary (include "(Intercept", tidy up the predictors, etc.)

# ==================================================================================================
# Summary of all results from the stepwise reduced regression and plots to show the results for the
# identified predictors
summary(mdl_step) # No function was traceable to get that into a table, so it has to be done manually!

#TODO: Supplementary data which displeays the levels of the answers on the x-Axis and the means for 'yes' and 'no' on the y axis for the
# supplementary data. For that we need a list of factors (-> just copy the entore list and remove what is not needed) and the ggplot routines in a loop ; )




# TODO David: There are two things to do: 
# 1. find out whether pca is meaningful to reduce data complexity in a first place
# 2. create a for loop with a bootstrap ot get confidence intervals in the model.
# 3- run GLM with the resulting valus for the estimates of the predictors (OR, etc.)




# TODO: For sanity checks one may plot results like this:
# data_full_glm %>% 
#	ggplot(aes(x=home_death, y=Charlson_withage)) + 
#	geom_boxplot() + 
#	geom_jitter(width=0.1, alpha=0.2)



# Interpreting results of stepwise reduced GLM:
predicted_classes 	<- predict(mdl_step, newdata = test_data)
predicted_probs	 	<- predict(mdl_step, newdata = test_data, type = "prob")
mdl_step_matrix 	<- confusionMatrix(predicted_classes, test_data$home_death, positive = "yes") # predictions on (independent) test data

mdl_step_preds <- predict(mdl_step, newdata = test_data, type = "prob")
(mdl_step_eval <- evalmod(
	scores = mdl_step_preds$yes,
	labels = test_data$home_death
))

mdl_step_prob <- predict(mdl_step, newdata = test_data, type = "prob") %>%
	bind_cols(predict(mdl_step, test_data)) %>%
	bind_cols(select(test_data, home_death))

trueLabelsBinary <- ifelse(mdl_step_prob$home_death=="yes", 1, 0)
predictedLabelsBinary <- ifelse(mdl_step_prob$`...3`=="yes", 1, 0)
model_est$LogLoss[2] = LogLoss(mdl_step_prob$yes, trueLabelsBinary)
model_est$AUC[2] = AUC(mdl_step_prob$yes, trueLabelsBinary)
model_est$Accuracy[2] = mdl_step$results[[2]]

# Print AUC for the FULL model
annotation <- data.frame(x=.8, y=.6, label=sprintf("AUC = %.2f [%.2f; %.2f]", mdl_step_matrix$overall[[1]],  mdl_step_matrix$overall[[3]], mdl_step_matrix$overall[[4]]))
options(yardstick.event_first = FALSE)  # set the second level as success
fig3b <- data.frame(pred = mdl_step_preds$yes, obs = test_data$home_death) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction that healthcare was needed but wasn't received",
		 subtitle = "GLM model after AIC-based stepwise reduction ") +
	geom_text(data=annotation, aes(x=x, y=y, label=label), color="black", fontface="bold") +
	coord_equal() +
	xlab("1 - specificity") + ylab("sensitivity")


scores_list <- join_scores(
	predict(mdl_full, newdata = train_data, type = "prob")$yes,
	predict(mdl_step, newdata = train_data, type = "prob")$yes)

labels_list <- join_labels(
	train_data$home_death,
	train_data$home_death)

pe <- evalmod(
	scores = scores_list,
	labels = labels_list,
	modnames = c("Full", "glmStepAIC"),
	posclass = "yes")





# ==================================================================================================
# Run regression analyses
colnames(eol_dataframe) # list of column names which may be used to define the factors of interest

factorsOR1 = c("gender", "age", "age_at_diagnosis", "duration", "german", "married", "religious_affiliation", "independent_living",
			   "receiving_nursing_support", "residential_location", "professional_education", "existence_advance_directive",
			   "attorney_power", "palliative_care_knowledge", "hospice_knowledge",
			   "cat.thoughts_EOLwishes", "Sharing_of_thoughts", "Thoughts_dicussed_with", "asked_about_end_of_life_wishes", #TODO Anna: the last three items in this row are weird!
			   "asked_by_whom", "cat.prefered_place_of_care", "home_care", "Charlson_withage",
			   "pod.family_friends", "pod.GP", "pod.neurologist", "pod.AD", "Hoehn_Yahr", "dbs")

factors_regression = c("gender", "age", "age_at_diagnosis", "duration", "german", "married",
					   "religious_affiliation","receiving_nursing_support", "rurality",
					   "professional_education", "existence_advance_directive",
					   "attorney_power", "palliative_care_knowledge", "hospice_knowledge",
					   "oftenEOLwishes_thoughts", "Sharing_of_thoughts",
					   "asked_about_end_of_life_wishes",
					   "home_care", "Charlson_withage", "Hoehn_Yahr", "LEDD",
					   "PDQ_score", "UPDRS_sum", "bdi_score", "MOCA_score", "disease_severityPC")


# "pod.neurologist removed as there was not much information inside, "asked_by_whom" removed

factorsOR1 <- factors_regression
results_homedeath = c()
for (fac in factorsOR1) { # for loop over factors of interest
	mod <- as.formula(sprintf("I(home_death=='yes') ~ %s", fac)) # formula for (unadjusted) GLM
	fit_temp = glm(mod, data=eol_dataframe, family="binomial") # estimate model
	results_homedeath = rbind(	results_homedeath, c(exp(coef(fit_temp)[2]), 	# OR
													   exp(confint.default(fit_temp)[2]),  	# lower CI
													   exp(confint.default(fit_temp)[4]),  	# upper CI
													   summary(fit_temp)$coefficients[2,4])) 	# significance value
}

results_OR1 <- data.frame(yAxis = length(factorsOR1):1,
						  factors=factorsOR1,
						  # factors_group=group,
						  boxOdds = results_homedeath[,1],
						  boxCILow = results_homedeath[,2],
						  boxCIHigh = results_homedeath[,3],
						  pvalue=results_homedeath[,4])
results_OR1 <- results_OR1 %>% mutate(significance = case_when(pvalue <= .001 ~ 'p < .001',
															   (pvalue < .05 & pvalue >.001) ~ 'p < .05',
															   pvalue > .05 ~ 'ns')) %>%
	mutate(dot_color = case_when(pvalue <= .001 ~ "#6B340D",
								 (pvalue < .05 & pvalue >.001) ~ "#B8A31F",
								 pvalue > .05 ~ "#08306B"))
results_OR1 <- results_OR1 %>% mutate(label = sprintf("%.2f; [%.2f, %.2f]", boxOdds, boxCILow, boxCIHigh))



tabOR <- results_OR1 %>% select(2:5) %>% mutate(across(where(is.numeric), ~ round(.,2))) #<- table with odds ratios
write.xlsx(tabOR, file = file.path(data_dir, "results", "TableOR.xlsx"), overwrite=TRUE)

results_OR1 %>%
	dplyr::arrange(boxOdds) %>%
	mutate(factors=factor(factors, levels=factors)) %>%  # This trick update the factor levels
	ggplot(aes(x = boxOdds, y = factors, label=label)) +
	geom_vline(aes(xintercept = 1), size = .75, linetype = "dashed", color="grey") +
	geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
	geom_point(aes(x = boxOdds, y = factors, color = significance), size = 2, show.legend=TRUE) +
	guides(colour = guide_legend(reverse=TRUE)) +
	scale_colour_manual(values=c("#C6DBEF", "#4292C6", "#08306B")) +
	theme_minimal() +
	theme(text = element_text(size = 12),
		  panel.grid.minor = element_blank(),
		  legend.position = c(0.75, 0.1), legend.title=element_blank(), legend.box.background = element_rect(colour = "black"),
		  legend.background = element_rect(colour = "black", fill="white"),
		  plot.title = element_text(vjust = 4, hjust = .5, color = "black", size = 12, face = "bold"),
		  axis.line.x = element_line(size = .25, colour = "black"),
		  axis.ticks.x = element_line(size = .25, colour = "black"),
		  axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
		  axis.text = element_text(size = 12),
		  plot.caption = element_text(hjust = 0, face="italic")) +
	coord_capped_cart(bottom='right') +
	scale_x_log10(breaks = c(.01, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10, 20, 50, 100), limits=c(.01, 100), expand=c(0.2, 0)) +
	#geom_hline(yintercept=hlines[1:length(hlines)-1], linetype="dotted", size = .01) +
	geom_text(
		aes(x = 20, y = factors, label = label), # aes(x = start, y = 2500, label = name)
		#data = results_OR1,
		hjust = -0.5, size = 4,
		position = position_dodge(width = 1),
		#inherit.aes = TRUE
	) +
	ylab("") +
	xlab("Odds ratio (log scale)") +
	# ggtitle("iPS-patients' odds of preferred home death") +
	labs(title = "Patients' odds of preferred home death",
		 caption = sprintf("(n = %s patients)", dim(eol_dataframe)[1]))
#sprintf("%.2f - [%.2f ; %.2f]", results_OR1$boxOdds[1], results_OR1$boxCILow[1], results_OR1$boxCILow[1])


# Run statistical analyses
eol_dataframe %>%
	select(all_of(NumVars), home_death) %>%
	gather(key=variable, value=value,  -home_death) %>%
	group_by(home_death, variable) %>%
	summarize(value=list(value)) %>%
	spread(home_death, value) %>%
	group_by(variable) %>%
	mutate(p_value = wilcox.test(unlist(yes), unlist(no))[['p.value']],
		   t_value =  wilcox.test(unlist(yes), unlist(no))[['statistic']])

sw_test_results <- eol_dataframe %>%
	select(all_of(NumVars), home_death) %>%
	gather(key=variable, value=value,  -home_death) %>%
	group_by(home_death, variable) %>%
	do(tidy(shapiro.test(.$value)))


factors_eol_dataframe <- eol_dataframe %>%
	select(c(catVars, "home_death"))

df2 <- t(round(cbind(apply(factors_eol_dataframe %>% select(-home_death), 2, function(x) {
	ch <- chisq.test(factors_eol_dataframe$home_death, x)
	c(unname(ch$statistic), ch$parameter, ch$p.value )})), 3))





BigSummary <- function (data, lev = NULL, model = NULL) {
	pr_auc <- try(MLmetrics::PRAUC(data[, lev[1]],
								   ifelse(data$obs == lev[1], 1, 0)),
				  silent = TRUE)
	brscore <- try(mean((data[, lev[1]] - ifelse(data$obs == lev[1], 1, 0)) ^ 2),
				   silent = TRUE)
	rocObject <- try(pROC::roc(ifelse(data$obs == lev[1], 1, 0), data[, lev[1]],
							   direction = "<", quiet = TRUE), silent = TRUE)
	if (inherits(pr_auc, "try-error")) pr_auc <- NA
	if (inherits(brscore, "try-error")) brscore <- NA
	rocAUC <- if (inherits(rocObject, "try-error")) {
		NA
	} else {
		rocObject$auc
	}
	tmp <- unlist(e1071::classAgreement(table(data$obs,
											  data$pred)))[c("diag", "kappa")]
	out <- c(Acc = tmp[[1]],
			 Kappa = tmp[[2]],
			 AUCROC = rocAUC,
			 AUCPR = pr_auc,
			 Brier = brscore,
			 Precision = caret:::precision.default(data = data$pred,
												   reference = data$obs,
												   relevant = lev[1]),
			 Recall = caret:::recall.default(data = data$pred,
											 reference = data$obs,
											 relevant = lev[1]),
			 F = caret:::F_meas.default(data = data$pred, reference = data$obs,
										relevant = lev[1]))
	out
}




