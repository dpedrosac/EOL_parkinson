# This is code to run analyses for the palliative care project;
# Code developed by Anna and David Pedrosa

# Version 1.2 # 2022-06-16, added further analyses

## First specify the packages of interest
packages = c("readxl", "tableone", "ggplot2", "tidyverse", "lemon", "openxlsx",
             "psych", "DescTools", "jtools", "rstatix", "ggpubr")

## In case of multiple people working on one project, this helps to create an automatic script
username = Sys.info()["login"]

if (username == "dpedr") {
  wdir = "D:/anna_projects"
} else if (username == "david") {
  wdir = "/media/storage/anna_projects/"
} else {
  wdir = "/Users/annapedrosa/Documents/dokumente/Arbeit_Anna/Aktuelle Projekte/StudiePoD_PD/" # <- wenn ich nicht dran arbeite, arbeitest Du dran und dann ist das der Ordner, in dem er sucht
}
setwd(wdir)

## Now load or install & load all iff necessary
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# ==================================================================================================
# Read data from excel spreadsheet
eol_xls <- read_excel(file.path(wdir, "Matrix_EOL_PD_.xlsx"))

# ==================================================================================================

# Recode variables

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



allVars <- c("gender", "age", "age_at_diagnosis", "duration", "nationality", "marital_status",
            "Religious_affiliation", "Housing_situation", "Cohabitation", "Nursing_support", "Residential_location",
            "School_graduation", "Professional_education", "Advance_directive", "Power_of_attorney", "other_document",
            "palliative_care_knowledge", "hospice_knowledge", "thoughts_about_end_of_life_wishes", "Sharing_of_thoughts",
            "Thoughts_dicussed_with", "asked_about_end_of_life_wishes", "asked_by_whom", "prefered_place_of_care",
            "prefered_place_of_death", "POD_familiy_friends", "POD_GP", "POD_neurologist", "POD_AD", "LEDD", "Hoehn_Yahr",
            "PDQ_score", "UPDRS_sum", "bdi_score", "MOCA_score", "Charlson_withoutage", "Charlson_withage", "dbs")

catVars <- c("gender", "nationality", "marital_status", "religion_belief_worldview", "Housing_situation", "Cohabitation",
             "Nursing_support", "Residential_location", "School_graduation", "Professional_education", "Advance_directive",
             "Power_of_attorney", "other_document", "palliative_care_knowledge", "hospice_knowledge",
             "thoughts_about_end_of_life_wishes", "Sharing_of_thoughts", "Thoughts_dicussed_with", "asked_about_end_of_life_wishes",
             "asked_by_whom", "prefered_place_of_care", "prefered_place_of_death", "POD_familiy_friends", "POD_GP",
             "POD_neurologist", "POD_AD", "Hoehn_Yahr", "dbs")

NumVars <- c("age", "age_at_diagnosis", "duration", "LEDD", "PDQ_score","UPDRS_sum", "bdi_score", "MOCA_score", "Charlson_withoutage",
             "Charlson_withage")

tab2 <- CreateTableOne(vars = allVars, data = eol_xls, factorVars = catVars)
print(tab2)
write.csv(print(tab2, quote = FALSE, 
                noSpaces = TRUE, printToggle = FALSE, showAllLevels = TRUE), file = "TableOne_EOL.csv")

# Run regression analyses
colnames(eol_xls) # list of column names which may be used to define the factors of interest

factorsOR1 = c("gender", "age", "age_at_diagnosis", "duration", "German", "married",
               "religious", "independent_living", "Cohabitation", "nursingcare", "rurality",
                "ProfessionalEducation", "AdvanceDirective", "Power_of_attorney",
               "knowledge_PC", "knowlege_hospice", "thoughts_about_end_of_life_wishes", "Sharing_of_thoughts",
               "Thoughts_dicussed_with", "asked_about_end_of_life_wishes", "asked_by_whom", "homecare",
              "LEDD", "Hoehn_Yahr", "PDQ_score", "UPDRS_sum", "bdi_score", "MOCA_score", "Charlson_withoutage", "Charlson_withage", "dbs")

results_homedeath = c()
for (fac in factorsOR1) { # for loop over factors of interest
  mod <- as.formula(sprintf("I(homedeath=='yes') ~ %s", fac)) # formula for (unadjusted) GLM
  fit_temp = glm(mod, data=data_xls, family="binomial") # estimate model
  results_homedeath = rbind(	results_homedeath, c(exp(coef(fit_temp)[2]), 	# OR
                                                  exp(confint.default(fit_temp)[2]),  	# lower CI
                                                  exp(confint.default(fit_temp)[4]),  	# upper CI
                                                  summary(fit_temp)$coefficients[2,4])) 	# significance value
  
  results_OR1 <- data.frame(yAxis = length(factorsOR1):1, 
                            factors=factorsOR1,
                            # factors_group=group,
                            boxOdds = results_interest1[,1], 
                            boxCILow = results_interest1[,2],
                            boxCIHigh = results_interest1[,3],
                            pvalue=results_interest1[,4])
  results_OR1 <- results_OR1 %>% mutate(significance = case_when(pvalue <= .001 ~ 'p < .001', 
                                                                 (pvalue < .05 & pvalue >.001) ~ 'p < .05',
                                                                 pvalue > .05 ~ 'ns')) %>% 
    mutate(dot_color = case_when(pvalue <= .001 ~ "#6B340D", 
                                 (pvalue < .05 & pvalue >.001) ~ "#B8A31F",
                                 pvalue > .05 ~ "#08306B"))
  results_OR1 <- results_OR1 %>% mutate(label = sprintf("%.2f; [%.2f, %.2f]", boxOdds, boxCILow, boxCIHigh))
  
  

tabOR <- results_OR1 %>% select(2:5) %>% mutate(across(where(is.numeric), ~ round(.,2))) #<- table with odds ratios
write.xlsx(tabOR, file = "TableOR.xlsx", overwrite=TRUE)

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
  scale_x_log10(breaks = c(.01, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10), limits=c(.01, 10), expand=c(0.2, 0)) + 
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
       caption = sprintf("(n = %s patients)", dim(data_xls)[1]))
#sprintf("%.2f - [%.2f ; %.2f]", results_OR1$boxOdds[1], results_OR1$boxCILow[1], results_OR1$boxCILow[1])
