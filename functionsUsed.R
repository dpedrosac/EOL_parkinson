# This code is intended to provide the functions used in the main file "EOL_PD.R"
# Code developed by Anna and David Pedrosa

# Version 1.0 # 2023-02-06, first commit

# 1. (results_model) -> estimate GLM w/ "home_death" as dependent variable; inputs define possible options
# 2. (print_AUC) -> Generic function to print AUC for the estimated models (deprecated in final analyses)
# 3. (results_bootstrap) -> Bootstrap CI for estimated models

# ==================================================================================================
# Generic {caret}-formula, which may be used for all models w/ different input

results_model <- function(method, data, train_control, tunegrid, test_data, model_name) {

	mdl_caret 			<- train(as.formula(paste( 'home_death', '~', '.')),
							  data=data,
							  method = method,
							  preProcess = c("nzv", "center", "scale"),
							  family="binomial",
							  trControl = train_control,
							  tuneLength = 25,
							  metric = "logLoss",
							  tuneGrid = tunegrid)
	estimated_model 	<- data.frame(model_name=model_name,
								  AUC=NA, LogLoss=NA, Accuracy=NA)

	predicted_classes 	<- predict(mdl_caret, newdata = test_data)
	mdl_caret_matrix 	<- confusionMatrix(predicted_classes, test_data$home_death, positive = "yes") # predictions on (independent) test data

	mdl_caret_preds 	<- predict(mdl_caret, newdata = test_data, type = "prob")
	mdl_caret_prob 		<- predict(mdl_caret, newdata = test_data, type = "prob") %>%
		cbind(., caret_results=predict(mdl_caret, test_data), home_death = test_data$home_death)

	trueLabelsBinary_full 		<- ifelse(mdl_caret_prob$home_death=="yes", 1, 0)
	predictedLabelsBinary 		<- ifelse(mdl_caret_prob$caret_results=="yes", 1, 0)
	estimated_model$LogLoss[1] 	<- LogLoss(mdl_caret_prob$yes, trueLabelsBinary_full)
	estimated_model$AUC[1] 		<- AUC(mdl_caret_prob$yes, trueLabelsBinary_full)
	estimated_model$Accuracy[1] <- mdl_caret_matrix$overall[[1]]

	return(list(mdl_caret, estimated_model, mdl_caret_matrix))
}

# ==================================================================================================
# 2. Generic function to print AUC for the estimated models

print_AUC <- function(mdl, test_data, annotation, subtitle) {

	# Print AUC for the FULL model
	options(yardstick.event_first = FALSE)  # set the second level as success
	fig <- data.frame(pred = predict(mdl, newdata = test_data, type = "prob")$yes, obs = test_data$home_death) %>%
	yardstick::roc_curve(obs, pred) %>%
	autoplot() +
	theme_bw() +
	labs(title = "Prediction of the model in the test dataset",
		 subtitle = subtitle) +
	geom_text(data=annotation, aes(x=x, y=y, label=label), color="black", fontface="bold") +
	coord_equal() +
	xlab("1 - specificity") + ylab("Sensitivity")

	return(fig)
}


# ==================================================================================================
# Bootstrap CI for estimated models
results_bootstrap <- function(method, data, test_data, model_name, nboot) {

	if (method == 'glmnet'){
		lambda.grid		<- seq(0.0001, 1, length = 50) #seq(0, 100)
		alpha.grid 		<- seq(0, 1, length = 11) #1
		grid_total 		<- expand.grid(alpha = alpha.grid,
								  lambda = lambda.grid)
		method_train 	<- "repeatedcv"
	} else {
		grid_total 		<- NULL
		method_train	<- "none"
	}

	tcBoot <- trainControl(method			= method_train,
						   summaryFunction 	= mnLogLoss,
						   savePredictions 	= "final",
						   verboseIter 		= FALSE,
						   classProbs 		= TRUE)

	model_est_boot  		<- as.data.frame(matrix(data=NA, nrow=nboot, ncol=6))
	colnames(model_est_boot)<- c('k', 'AUC', 'LogLoss', 'Accuracy', 'alpha', 'lambda')

	pb 						<- txtProgressBar(min = 0, max = nboot, style = 3)
	options(warn=-1)
	for (k in 1:nboot) {
		setTxtProgressBar(pb, k)
		model_est_boot[k,1] <- k
		boot_idx 			<- sample(dim(data)[1], replace=TRUE)
		boot_data 			<- data[boot_idx,]
		test_idx 			<- setdiff(1:dim(data)[1], boot_idx)
		test_data 			<- data[test_idx,]
		mdl_boot 			<- results_model(method = method, data = boot_data, train_control = tcBoot,
									 tunegrid= grid_total, test_data = test_data, model_name = model_name)

		if (method == 'glmnet'){
			model_est_boot[k,2:4] 	<- mdl_boot[[2]][1,2:4]
			model_est_boot[k,5] 	<- mdl_boot[[1]]$bestTune$alpha
			model_est_boot[k,6] 	<- mdl_boot[[1]]$bestTune$lambda
		} else {
			model_est_boot[k,2:4] 	<- mdl_boot[[2]][1,2:4]
		}
	}
	options(warn=0)
	close(pb)
	return (model_est_boot)
}

