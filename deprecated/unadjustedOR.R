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


