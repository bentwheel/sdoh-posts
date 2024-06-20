# Libraries

# Invoke required libraries - install them in the User package library if not already installed.
if(!require(devtools)) { install.packages("devtools"); library(devtools) }
if(!require(survey)) { install.packages("survey"); library(survey) }
if(!require(srvyr)) { install.packages("srvyr"); library(srvyr) }
if(!require(foreign)) { install.packages("foreign"); library(foreign) }
if(!require(haven)) { install.packages("haven"); library(haven) }
if(!require(tidyverse)) { install.packages("tidyverse"); library(tidyverse) }
if(!require(flextable)) { install.packages("flextable"); library(flextable) }
if(!require(viridis)) { install.packages("viridis"); library(viridis) }
if(!require(scales)) { install.packages("scales"); library(scales) }
if(!require(here)) { install.packages("here"); library(here) }

# Use devtools to download custom MEPS dataset interface package from Github if it is not already installed.
# Fore more information on the MEPS package and how to use it, refer to the following URL:
# https://github.com/HHS-AHRQ/MEPS/tree/master/R#all-data-years-using-the-meps-package
if(!require(MEPS)) { devtools::install_github("e-mitchell/meps_r_pkg/MEPS"); library(MEPS) }

# Table options
init_flextable_defaults()

set_flextable_defaults(
  font.size = 8, font.family = "Arial",
  background.color = "white",
  font.color = "#333333",
  table.layout = "autofit",
  border.color = "gray",
  padding.top = 3, padding.bottom = 3,
  padding.left = 4, padding.right = 4,
  digits = 0,
  decimal.mark = ".",
  big.mark = ",")

# Set options to deal with lonely PSUs. A PSU is a Primary Sampling Unit. Primary Sampling Units are divided among several sampling strata. The MEPS survey design package provides appropriate sampling weights for each strata in order for each sampling unit to be reweighted in proportion to the POI (Population of Interest - in this case ,the entire US).

# In some cases, our analysis might necessitate drilling down to very small subpopulations, which will whittle down the membership of some strata to 1 or fewer members. In this case, we might encounter some strata with a single member - a "lonely PSU" - at which point the Survey package will error out when computing the sample variance to determine the standard error for a particular statistic (mean, total sum, etc.) in our analyses. Setting this option will adjust the data in the single-PSU stratum so that it is centered at the entire sample mean instead of the particular stratum mean, which tends to be a more conservative computation of the variance and has the effect of contributing to a wider standard error estimate for any statistic of interest in the analysis.

# In short, the following line will conservatively recenter certain data points so that a standard error for statistics of interest (mean, total sum, etc.) are computable for all strata - even those containing a single PSU, with the tradeoff of a larger (and more conservative) magnitude of standard error.

# An excellent article that goes into more detail about this process (and expresses some concern about the magnitude of overconservatism that R's survey package employs in recentering the lonely PSU mean) can be read here:
# https://www.practicalsignificance.com/posts/bugs-with-singleton-strata/

options(survey.lonely.psu='adjust')

# The following lines of code make use of the custom MEPS R package developed by the MEPS staff. This package is not on CRAN and must be downloaded via Github using the "devtools" package, which is done in the "Initial_Setup" chunk of code in the 'README.Rmd' markdown file.

# Due to the desire to have statistically meaningful results on display in later sections of this analysis, I have decided to pull MEPS data files spanning the full years 2014 through 2019. Ideally I would have also pulled 2020, but since this project consists of an analysis of differences in healthcare spending patterns between demographic groups, and since these same spending patterns are likely to have been influenced by varying degrees regional differences in public attitudes/lockdowns/"pent-up" utilization patterns/etc. during the height of the pandemic, 2020 year data was not included.

# Download the 2021 Full Year Consolidated Data File
# https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-216
fyc21 = MEPS::read_MEPS(year = 2021, type = "FYC")

fyc21_extended <- fyc21 %>% 
  # In some cases, respondents do not include age and it cannot be imputed using other data sources. These respondents are coded with a -1 for age. We do not wish to include these responses in our analysis.
  filter(AGE21X > 0 & AGE42X > 0) %>% 
  #  Now will define some categorical variables for labeling visualizations and crosstabbing in natural language
  mutate(AGE_GRP_2 = if_else(AGE21X >= 65, "65 and over", "0 - 64")) %>% 
  mutate(AGE_GRP_3 = case_when(AGE21X < 18 ~ "Under 18",
                               AGE21X >= 18 & AGE21X < 65 ~ "18 - 64",
                               AGE21X >= 65 ~ "65 and over",
                               T ~ as.character(AGE21X))) %>%
  mutate(AGE_GRP_5 = case_when(AGE21X < 5 ~ "Under 5",
                               AGE21X >= 5 & AGE21X <= 17 ~ "5 - 17",
                               AGE21X >= 18 & AGE21X <= 44 ~ "18 - 44",
                               AGE21X >= 45 & AGE21X <= 64 ~ "45 - 64",
                               AGE21X >= 65 ~ "65 and over",
                               T ~ as.character(AGE21X))) %>%
  mutate(AGE_GRP_9 = case_when(AGE21X < 5 ~ "Under 5",
                               AGE21X >= 5 & AGE21X <= 17 ~ "5 - 17",
                               AGE21X >= 18 & AGE21X <= 29 ~ "18 - 29",
                               AGE21X >= 30 & AGE21X <= 39 ~ "30 - 39",
                               AGE21X >= 40 & AGE21X <= 49 ~ "40 - 49",
                               AGE21X >= 50 & AGE21X <= 59 ~ "50 - 59",
                               AGE21X >= 60 & AGE21X <= 69 ~ "60 - 69",
                               AGE21X >= 70 & AGE21X <= 79 ~ "70 - 79",
                               AGE21X >= 80 ~ "80 and over",
                               T ~ as.character(AGE21X))) %>%
  arrange(AGE21X) %>% 
  mutate(AGE_GRP_9 = forcats::fct_inorder(AGE_GRP_9),
         AGE_GRP_5 = forcats::fct_inorder(AGE_GRP_5),
         AGE_GRP_3 = forcats::fct_inorder(AGE_GRP_3),
         AGE_GRP_2 = forcats::fct_inorder(AGE_GRP_2)) %>% 
  # Age variable - exact age is AGE21X, where for SAQ the age variable is AGE42X as the SAQ is only administered during rounds 2 and 4 of certain panels, respectively.
  mutate(AGE_GRP_2_SAQ = if_else(AGE42X >= 65, "65 and over", "0 - 64")) %>% 
  mutate(AGE_GRP_3_SAQ = case_when(AGE42X < 18 ~ "Under 18",
                                   AGE42X >= 18 & AGE42X < 65 ~ "18 - 64",
                                   AGE42X >= 65 ~ "65 and over",
                                   T ~ as.character(AGE42X))) %>%
  mutate(AGE_GRP_5_SAQ = case_when(AGE42X < 5 ~ "Under 5",
                                   AGE42X >= 5 & AGE42X <= 17 ~ "5 - 17",
                                   AGE42X >= 18 & AGE42X <= 44 ~ "18 - 44",
                                   AGE42X >= 45 & AGE42X <= 64 ~ "45 - 64",
                                   AGE42X >= 65 ~ "65 and over",
                                   T ~ as.character(AGE42X))) %>%
  mutate(AGE_GRP_9_SAQ = case_when(AGE42X < 5 ~ "Under 5",
                                   AGE42X >= 5 & AGE42X <= 17 ~ "5 - 17",
                                   AGE42X >= 18 & AGE42X <= 29 ~ "18 - 29",
                                   AGE42X >= 30 & AGE42X <= 39 ~ "30 - 39",
                                   AGE42X >= 40 & AGE42X <= 49 ~ "40 - 49",
                                   AGE42X >= 50 & AGE42X <= 59 ~ "50 - 59",
                                   AGE42X >= 60 & AGE42X <= 69 ~ "60 - 69",
                                   AGE42X >= 70 & AGE42X <= 79 ~ "70 - 79",
                                   AGE42X >= 80 ~ "80 and over",
                                   T ~ as.character(AGE42X))) %>%
  arrange(AGE42X) %>% 
  mutate(AGE_GRP_9_SAQ = forcats::fct_inorder(AGE_GRP_9_SAQ),
         AGE_GRP_5_SAQ = forcats::fct_inorder(AGE_GRP_5_SAQ),
         AGE_GRP_3_SAQ = forcats::fct_inorder(AGE_GRP_3_SAQ),
         AGE_GRP_2_SAQ = forcats::fct_inorder(AGE_GRP_2_SAQ)) %>% 
  mutate(RACETHX_DSC = case_when(RACETHX == 1 ~ "Hispanic",
                                 RACETHX == 2 ~ "Non-Hispanic White Only",
                                 RACETHX == 3 ~ "Non-Hispanic Black Only",
                                 RACETHX == 4 ~ "Non-Hispanic Asian Only",
                                 RACETHX == 5 ~ "Non-Hispanic Other Race or Multiple Race",
                                 T ~ as.character(RACETHX))) %>% 
  arrange(RACETHX) %>% 
  mutate(RACETHX_DSC = forcats::fct_inorder(RACETHX_DSC)) %>%
  mutate(POVLEV_DSC = case_when(POVLEV21 < 100 ~ "Less than 100%",
                                POVLEV21 >= 100 & POVLEV21 < 138 ~ "100% to less than 138%",
                                POVLEV21 >= 138 & POVLEV21 < 150 ~ "138% to less than 150%",
                                POVLEV21 >= 150 & POVLEV21 < 400 ~ "150% to less than 400%",
                                POVLEV21 >= 400 ~ "400% or more",
                                T ~ as.character(POVLEV21))) %>% 
  arrange(POVLEV21) %>% 
  mutate(POVLEV_DSC = forcats::fct_inorder(POVLEV_DSC, ordered=T)) %>%   
  mutate(HDDX_DSC = case_when(CHDDX == 1 | ANGIDX == 1 | MIDX == 1 | OHRTDX == 1 ~ "Diagnosed with Heart Disease", 
                              CHDDX > 0  | ANGIDX > 0 | MIDX > 0 | OHRTDX > 0 ~ "Not Diagnosed with Heart Disease", 
                              T ~ "Unknown or Inapplicable")) %>% 
  mutate(HDDX_DSC = forcats::fct_inorder(HDDX_DSC)) %>%   
  mutate(DIABDX_DSC = case_when(DIABDX_M18 == 1 ~ "Diagnosed with Diabetes", 
                                DIABDX_M18 == 2 ~ "Not Diagnosed with Diabetes", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(DIABDX_DSC = forcats::fct_inorder(DIABDX_DSC)) %>%   
  mutate(STRKDX_DSC = case_when(STRKDX == 1 ~ "Diagnosed with having had a Stroke", 
                                STRKDX == 2 ~ "Not Diagnosed with having had a Stroke", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(STRKDX_DSC = forcats::fct_inorder(STRKDX_DSC)) %>%   
  mutate(HIBPDX_DSC = case_when(HIBPDX == 1 ~ "Diagnosed with High Blood Pressure", 
                                HIBPDX == 2 ~ "Not Diagnosed with High Blood Pressure", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(HIBPDX_DSC = forcats::fct_inorder(HIBPDX_DSC)) %>%   
  mutate(CHOLDX_DSC = case_when(CHOLDX == 1 ~ "Diagnosed with High Cholesterol", 
                                CHOLDX == 2 ~ "Not Diagnosed with High Cholesterol", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(CHOLDX_DSC = forcats::fct_inorder(CHOLDX_DSC)) %>%   
  mutate(ASTHDX_DSC = case_when(ASTHDX == 1 ~ "Diagnosed with Asthma", 
                                ASTHDX == 2 ~ "Not Diagnosed with Asthma", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(ASTHDX_DSC = forcats::fct_inorder(ASTHDX_DSC)) %>% 
  mutate(ARTHDX_DSC = case_when(ARTHDX == 1 ~ "Diagnosed with Arthritis", 
                                ARTHDX == 2 ~ "Not Diagnosed with Arthritis", 
                                T ~ "Unknown or Inapplicable")) %>% 
  mutate(ARTHDX_DSC = forcats::fct_inorder(ARTHDX_DSC)) %>% 
  mutate(CANCERDX_DSC = case_when(CANCERDX == 1 ~ "Diagnosed with Cancer", 
                                  CANCERDX == 2 ~ "Not Diagnosed with Cancer", 
                                  T ~ "Unknown or Inapplicable")) %>% 
  mutate(CANCERDX_DSC = forcats::fct_inorder(CANCERDX_DSC)) %>% 
  mutate(ATLEASTONE_CHRONIC_DSC = case_when((CHDDX == 1 | ANGIDX == 1 | MIDX == 1 | OHRTDX == 1 | DIABDX_M18 == 1 | HIBPDX == 1 | CHOLDX == 1 | ASTHDX == 1 | ARTHDX == 1 | CANCERDX == 1 | STRKDX == 1) ~ "Diagnosed with one or more of these conditions", 
                                            T ~ "Not diagnosed with one or more of these conditions, or Unknown/Inapplicable")) %>% 
  mutate(CANCERDX_DSC = forcats::fct_inorder(CANCERDX_DSC)) %>% 
  mutate(SEX_DSC = if_else(SEX == 1, "Male", if_else(SEX == 2, "Female", "Unknown"))) %>% 
  mutate(SEX_DSC = forcats::fct_inorder(SEX_DSC)) %>% 
  # Highest levl of education attained
  mutate(HIDEG_DSC = case_when(HIDEG == 1 ~ "Did not complete high school",
                               HIDEG >= 2 & HIDEG <= 7 ~ "Completed GED or high school",
                               HIDEG == 8 ~ "Inapplicable - Under 16",
                               .default = "N/A, Don't Know, or Undeterminable")) %>% 
  arrange(HIDEG) %>% 
  mutate(HIDEG_DSC = forcats::fct_inorder(HIDEG_DSC)) %>% 
  # for SDOH responses
  mutate(SDPUBTRANS_DSC = case_when(SDPUBTRANS == 1 ~ "Excellent",
                                    SDPUBTRANS == 2 ~ "Very good",
                                    SDPUBTRANS == 3 ~ "Good",
                                    SDPUBTRANS == 4 ~ "Fair",
                                    SDPUBTRANS == 5 ~ "Poor",
                                    .default = "N/A or Undeterminable")) %>% 
  arrange(SDPUBTRANS) %>% 
  mutate(SDPUBTRANS_DSC = forcats::fct_inorder(SDPUBTRANS_DSC)) %>% 
  mutate(SDPUBTRANS_DSC_2 = case_when(SDPUBTRANS == 1 | SDPUBTRANS == 2 | SDPUBTRANS == 3 ~ "Rated neighborhood access to public transportation as \"Excellent\", \"Very good\", or \"Good\"",
                                      SDPUBTRANS == 4 | SDPUBTRANS == 5 ~ "Rated neighborhood access to public transportation as \"Fair\" or \"Poor\"",
                                      .default = "N/A or Undeterminable")) %>% 
  arrange(SDPUBTRANS) %>% 
  mutate(SDPUBTRANS_DSC_2 = forcats::fct_inorder(SDPUBTRANS_DSC_2)) %>% 
  mutate(SDAFRDHOME_DSC = case_when(SDAFRDHOME == 1 ~ "Excellent",
                                    SDAFRDHOME == 2 ~ "Very good",
                                    SDAFRDHOME == 3 ~ "Good",
                                    SDAFRDHOME == 4 ~ "Fair",
                                    SDAFRDHOME == 5 ~ "Poor",
                                    .default = "N/A or Undeterminable")) %>% 
  arrange(SDAFRDHOME) %>% 
  mutate(SDAFRDHOME_DSC = forcats::fct_inorder(SDAFRDHOME_DSC)) %>% 
  mutate(SDAFRDHOME_DSC_2 = case_when(SDAFRDHOME == 1 | SDAFRDHOME == 2 | SDAFRDHOME == 3 ~ "\"Excellent\", \"Very good\", or \"Good\"",
                                      SDAFRDHOME == 4 | SDAFRDHOME == 5 ~ "\"Fair\" or \"Poor\"",
                                      .default = "N/A or Undeterminable")) %>% 
  arrange(SDAFRDHOME) %>% 
  mutate(SDAFRDHOME_DSC_2 = forcats::fct_inorder(SDAFRDHOME_DSC_2)) %>% 
  mutate(SDNOTRANS_DSC = case_when(SDNOTRANS == 1 ~ "Yes",
                                   SDNOTRANS == 2 ~ "No",
                                   .default = "N/A or Undeterminable")) %>% 
  arrange(SDNOTRANS) %>% 
  mutate(SDNOTRANS_DSC = forcats::fct_inorder(SDNOTRANS_DSC)) %>% 
  # Now the General health question from SAQ
  mutate(ADGENH42_DSC = case_when(ADGENH42 == 1 ~ "Excellent",
                                  ADGENH42 == 2 ~ "Very good",
                                  ADGENH42 == 3 ~ "Good",
                                  ADGENH42 == 4 ~ "Fair",
                                  ADGENH42 == 5 ~ "Poor",
                                  .default = "N/A or Undeterminable")) %>% 
  arrange(ADGENH42) %>% 
  mutate(ADGENH42_DSC = forcats::fct_inorder(ADGENH42_DSC)) %>% 
  mutate(GENHLTH_SIMPLE = case_when(ADGENH42 == 1 | ADGENH42 == 2 | ADGENH42 == 3 ~ "Self-reported general health status on VR-12 is \"Excellent\", \"Very good\", or \"Good\"",
                                    ADGENH42 == 4 | ADGENH42 == 5 ~ "Self-reported general health status on VR-12 is \"Fair\" or \"Poor\"",
                                    .default = "N/A or Undeterminable")) %>% 
  arrange(ADGENH42) %>% 
  mutate(GENHLTH_SIMPLE = forcats::fct_inorder(GENHLTH_SIMPLE))

# Sample size check

sample_size_check_housing <-  fyc21_extended %>% 
  filter(SAQWT21F > 0 & AGE42X >= 16) %>% 
  group_by(SDAFRDHOME_DSC_2, RACETHX_DSC) %>% 
  summarize(sample_n = n()) %>%
  write_csv(file="./outputs/data/sample_size_check_housing.csv")


### Affordable Housing

housing_vs_genhealth_all <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = SAQWT21F,
    nest = T) %>% 
  filter(SAQWT21F > 0 & AGE42X >= 16) %>% 
  group_by(SDAFRDHOME_DSC_2, GENHLTH_SIMPLE) %>% 
  summarize(n = survey_prop(vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  mutate(RACETHX_DSC = "All Race/Ethnicity Groups") %>%
  relocate(RACETHX_DSC, .before=SDAFRDHOME_DSC_2)  

housing_vs_genhealth.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = SAQWT21F,
    nest = T) %>% 
  filter(SAQWT21F > 0 & AGE42X >= 16) %>% 
  group_by(RACETHX_DSC, SDAFRDHOME_DSC_2, GENHLTH_SIMPLE) %>% 
  summarize(n = survey_prop(vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  union_all(housing_vs_genhealth_all) %>%
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>% 
  filter(GENHLTH_SIMPLE != "N/A or Undeterminable") %>%
  write_csv(file="./outputs/data/housing_vs_genhealth.csv")

housing_vs_genhealth.table <- housing_vs_genhealth.data %>% 
  arrange(SDAFRDHOME_DSC_2, RACETHX_DSC) %>% 
  flextable() %>% 
  bold(part = "header", bold = TRUE) %>% 
  theme_box()

housing_vs_genhealth.table

# SDOH: SDAFRDHOME - Rate neighborhood availability of affordable housing

housing_vs_genhealth.plot <- housing_vs_genhealth.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = SDAFRDHOME_DSC_2,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  facet_grid(GENHLTH_SIMPLE ~ RACETHX_DSC, scales="fixed", labeller = label_wrap_gen(width = 20, multi_line = TRUE)) +
  labs(title="Self-Reported General Health Status vs. Rated Access to Affordable Housing",
       subtitle="By Race and Ethnicity",
       y = "Percent of US Population Ages 16+",
       x = "Rated neighborhood access to affordable housing",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

housing_vs_genhealth.plot %>%
  ggsave(file = "./outputs/charts/housing_vs_genhealth.png", width = 11, height = 8.5)

## Transit hardships vs. ED / IP utils
housing_vs_ed_all <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(ERTOT21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  mutate(RACETHX_DSC = "All Race/Ethnicity Groups") %>%
  relocate(RACETHX_DSC, .before=SDAFRDHOME_DSC_2)   

housing_vs_ed.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(RACETHX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(ERTOT21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  union_all(housing_vs_ed_all) %>%
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  filter(RACETHX_DSC != "Non-Hispanic Asian Only") %>%
  write_csv(file="./outputs/data/housing_vs_ed.csv")

housing_vs_ed.plot <- housing_vs_ed.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = RACETHX_DSC,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~ RACETHX_DSC, ncol = 3, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title="Average Annual ER Visits",
       subtitle="By Race/Ethnicity and Access to Affordable Housing",
       y = "Average annual ER visits per person",
       x = "Access to Affordable Housing",
       fill = "Race/Ethnicity",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") + 
  guides(fill = "none")

housing_vs_ed.plot %>%
  ggsave(file = "./outputs/charts/housing_vs_ed.png", width = 11, height = 8.5)

## Transit hardships vs. ED utils by select Chronic Cond. DX

housing_vs_ed_CC.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(HDDX_DSC, DIABDX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(ERTOT21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  filter(HDDX_DSC != "Unknown or Inapplicable" & DIABDX_DSC != "Unknown or Inapplicable") %>%
  write_csv(file="./outputs/data/housing_vs_ed_CC.csv")

housing_vs_ed_CC.plot <- housing_vs_ed_CC.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = SDAFRDHOME_DSC_2,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  facet_grid(HDDX_DSC~DIABDX_DSC, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  labs(title="Average Annual ER Visits",
       subtitle="By Select Chronic Condition Diagnosis and Access to Affordable Housing",
       y = "Average annual ER visits per person",
       x = "Access to Affordable Housing",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") + 
  guides(fill = "none")

housing_vs_ed_CC.plot %>%
  ggsave(file = "./outputs/charts/housing_vs_ed_CC.png", width = 11, height = 8.5)

## Housing vs. IP admits
housing_vs_ip_all <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(IPNGTD21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  mutate(RACETHX_DSC = "All Race/Ethnicity Groups") %>%
  relocate(RACETHX_DSC, .before=SDAFRDHOME_DSC_2)   

housing_vs_ip.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(RACETHX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(IPNGTD21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  union_all(housing_vs_ip_all) %>%
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  filter(RACETHX_DSC != "Non-Hispanic Asian Only") %>%
  write_csv(file="./outputs/data/housing_vs_ip.csv")

housing_vs_ip.plot <- housing_vs_ip.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = RACETHX_DSC,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  facet_wrap(~ RACETHX_DSC, ncol = 4, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  labs(title="Average Number of Days Spent in an Inpatient Facility",
       subtitle="By Race/Ethnicity and Access to Affordable Housing",
       y = "Average inpatient days per person",
       x = "Access to Affordable Housing",
       fill = "Race/Ethnicity",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

housing_vs_ip.plot  %>%
  ggsave(file = "./outputs/charts/housing_vs_ip.png", width = 11, height = 8.5)

## Housing vs. IP admits by Select Chronic Cond. DX
housing_vs_ip_CC.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(HDDX_DSC, DIABDX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_mean(IPNGTD21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>% 
  filter(rse <= .3) %>% 
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  filter(HDDX_DSC != "Unknown or Inapplicable" & DIABDX_DSC != "Unknown or Inapplicable") %>%
  write_csv(file="./outputs/data/housing_vs_ip_CC.csv")

housing_vs_ip_CC.plot <- housing_vs_ip_CC.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = SDAFRDHOME_DSC_2,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  facet_grid(HDDX_DSC~ DIABDX_DSC, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  labs(title="Average Number of Days Spent in an Inpatient Facility",
       subtitle="By Select Chronic Condition Diagnosis and Access to Affordable Housing",
       y = "Average inpatient days per person",
       x = "Access to Affordable Housing",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

housing_vs_ip_CC.plot  %>%
  ggsave(file = "./outputs/charts/housing_vs_ip_CC.png", width = 11, height = 8.5)

# Housing vs. Total Cost of Care
housing_vs_totexp_all <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_median(TOTEXP21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>%
  filter(rse <= .3) %>% 
  mutate(RACETHX_DSC = "All Race/Ethnicity Groups") %>%
  relocate(RACETHX_DSC, .before=SDAFRDHOME_DSC_2)     

housing_vs_totexp.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(RACETHX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_median(TOTEXP21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>%
  filter(rse <= .3) %>% 
  union_all(housing_vs_totexp_all) %>%
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  write_csv(file="./outputs/data/housing_vs_totexp.csv")

housing_vs_totexp.plot <- housing_vs_totexp.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = RACETHX_DSC,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  facet_wrap(~ RACETHX_DSC, ncol = 3, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  labs(title="Median Annual Individual Healthcare Expenditures",
       subtitle="By Race/Ethnicity and Access to Affordable Housing",
       y = "Median annual healthcare expenditures",
       x = "Access to Affordable Housing",
       fill = "Race/Ethnicity",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

housing_vs_totexp.plot %>%
  ggsave(file = "./outputs/charts/housing_vs_totexp.png", width = 11, height = 8.5)

# Housing vs. Total Cost of Care by select CC DX
housing_vs_totexp_CC.data <- fyc21_extended %>% 
  as_survey_design(
    ids = VARPSU,
    strata = VARSTR,
    weights = PERWT21F,
    nest = T) %>% 
  filter(PERWT21F > 0 & AGE21X >= 16) %>% 
  group_by(HDDX_DSC, DIABDX_DSC, SDAFRDHOME_DSC_2) %>% 
  summarize(n = survey_median(TOTEXP21, vartype = c("se", "ci"), level = 0.95)) %>% 
  mutate(rse = n_se / n) %>%
  filter(rse <= .3) %>% 
  filter(SDAFRDHOME_DSC_2 != "N/A or Undeterminable") %>%
  write_csv(file="./outputs/data/housing_vs_totexp_CC.csv")

housing_vs_totexp_CC.plot <- housing_vs_totexp_CC.data %>% 
  ggplot(mapping = aes(x = SDAFRDHOME_DSC_2,
                       fill = SDAFRDHOME_DSC_2,
                       y = n)) +
  scale_fill_viridis_d(begin = 0.75, end = 0) +
  geom_bar(stat="identity", position="dodge", alpha = .75) +
  geom_errorbar(aes(ymin=n_low, ymax=n_upp), width=.2,
                position=position_dodge(.9)) +
  theme_bw() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  facet_grid(HDDX_DSC ~ DIABDX_DSC, scales="fixed", labeller = label_wrap_gen(width = 36, multi_line = TRUE)) +
  labs(title="Median Annual Individual Healthcare Expenditures",
       subtitle="By Select Chronic Condition Diagnosis and Access to Affordable Housing",
       y = "Median annual healthcare expenditures",
       x = "Access to Affordable Housing",
       caption = "Source: MEPS 2021 Full Year Consolidated Data File (https://meps.ahrq.gov/).\n
                  Error bars denote a 95% confidence interval around the corresponding estimate.") +
  theme(legend.position = "bottom") +
  guides(fill = "none")

housing_vs_totexp_CC.plot %>%
  ggsave(file = "./outputs/charts/housing_vs_totexp_CC.png", width = 11, height = 8.5)

