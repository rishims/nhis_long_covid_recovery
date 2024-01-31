# Factors Associated with Long COVID Recovery among US Adults
# Data source is the National Health Interview Survey (NHIS) from the year 2022
# Data can be downloaded from here: https://www.cdc.gov/nchs/nhis/2022nhis.htm
# Rishi Shah
# December 24, 2023

# load necessary libraries
library(data.table)
library(foreign)
library(dplyr)
library(survey)
library(srvyr)
library(data.table)
library(epiDisplay)
library(ggplot2)
library(gtsummary)
library(gt)
library(aod)

# read in 2022 NHIS survey data
df_orig <- fread('adult22.csv')
names(df_orig) <- tolower(names(df_orig))

df_orig$had_covid <- 0
df_orig$had_covid[df_orig$postest_a == 1 | df_orig$cvddiag_a == 1] <- 1

df_orig$ever_lc <- 0
df_orig$ever_lc[df_orig$had_covid == 1 & df_orig$longcvd_a == 1] <- 1

################################
#### 1. DATA PRE-PROCESSING ####
################################

# set the seed for reproducibility
set.seed(12242023)

# subset to only include those that responded yes/no to covid-related questions
df <- df_orig %>%
  filter((postest_a %in% c(1, 2) |
            cvddiag_a %in% c(1, 2)) & 
           longcvd_a %in% c(1, 2) & 
           sympnow_a %in% c(1, 2))

# set survey
design_1 <- svydesign(
  ids = ~ppsu,
  strata = ~pstrat,
  weights = ~wtfa_a,
  data = df,
  nest = TRUE
)

# make % infected by month plot
df$had_covid <- 0
df$had_covid[df$postest_a == 1 | df$cvddiag_a == 1] <- 1

df$ever_lc <- 0
df$ever_lc[df$had_covid == 1 & df$longcvd_a == 1] <- 1

df$current_lc <- 0
df$current_lc[df$ever_lc == 1 & df$sympnow_a == 1] <- 1

df$recovered_lc <- 0
df$recovered_lc[df$ever_lc == 1 & df$current_lc == 0] <- 1
levels <- c("Not Recovered", "Recovered")
labels <- c(0, 1)
df$recovered_lc <- factor(df$recovered_lc, levels = labels, labels = levels)

# covariates 

# race/ethnicity
df$race_simple <- NA
df$race_simple[df$hispallp_a == 1] <- 1
df$race_simple[df$hispallp_a == 2] <- 2
df$race_simple[df$hispallp_a == 3] <- 3
df$race_simple[df$hispallp_a == 4] <- 4
df$race_simple[df$hispallp_a >= 5 & df$hispallp_a <= 7] <- 5

levels <- c("Hispanic", "NH White", "NH Black", "NH Asian", "NH Other")
labels <- c(1, 2, 3, 4, 5)
df$race_simple <- factor(df$race_simple, levels = labels, labels = levels)

# binarized races
df$white <- ifelse(df$race_simple == "NH White", 1, 0)
df$black <- ifelse(df$race_simple == "NH Black", 1, 0)
df$asian <- ifelse(df$race_simple == "NH Asian", 1, 0)
df$otherrace <- ifelse(df$race_simple == "NH Other", 1, 0)
df$hispanic <- ifelse(df$race_simple == "Hispanic", 1, 0)

# binarize sex
# 1 = female
df$female <- 0
df$female[df$sex_a == 2] <- 1

levels <- c("Male", "Female")
labels <- c(0, 1)
df$female <- factor(df$female, levels = labels, labels = levels)

# binarize less than or greater than high school education
# 1 = some college or higher
df$educ_binary <- NA
df$educ_binary[df$educp_a >= 1 & df$educp_a < 3] <- 0
df$educ_binary[df$educp_a >= 3 & df$educp_a < 99] <- 1

levels <- c("Less HS", "More HS")
labels <- c(0, 1)
df$educ_binary <- factor(df$educ_binary, levels = labels, labels = levels)

# binarize low and middle/high family income
# low = < 200% of the federal poverty limit = 0
# middle/high = >= 200% of the federal poverty limit = 1
df$inc_binary <- NA
df$inc_binary[df$ratcat_a >= 1 & df$ratcat_a <= 7] <- 0
df$inc_binary[df$ratcat_a >= 8 & df$ratcat_a <= 14] <- 1

levels <- c("Low income", "High income")
labels <- c(0, 1)
df$inc_binary <- factor(df$inc_binary, levels = labels, labels = levels)

# binarize sexual orientation
# 0 = not-straight, 1 = straight
df$orient_binary <- NA
df$orient_binary[df$orient_a == 1 | df$orient_a == 3 | df$orient_a == 4] <- 0
df$orient_binary[df$orient_a == 2] <- 1

levels <- c("Not straight", "Straight")
labels <- c(0, 1)
df$orient_binary <- factor(df$orient_binary, levels = labels, labels = levels)

# make three age categories
df$agecat <- NA
df$agecat[df$agep_a < 40] <- 1
df$agecat[df$agep_a >= 40 & df$agep_a < 65] <- 2
df$agecat[df$agep_a >= 65 & df$agep_a < 99] <- 3

levels <- c("Young", "Mid", "Old")
labels <- c(1, 2, 3)
df$agecat <- factor(df$agecat, levels = labels, labels = levels)

# binarized age categories
df$young <- ifelse(df$agecat == 1, 1, 0)
df$mid <- ifelse(df$agecat == 2, 1, 0)
df$old <- ifelse(df$agecat == 3, 1, 0)

# make binary citizenship status
# 0 = not citizen, 1 = citizen
df$citizen_binary <- NA
df$citizen_binary[df$citznstp_a == 1] <- 0
df$citizen_binary[df$citznstp_a == 2] <- 1

levels <- c("Not citizen", "Citizen")
labels <- c(0, 1)
df$citizen_binary <- factor(df$citizen_binary, levels = labels, labels = levels)

# make binary variables for each region of the US
df$neast <- ifelse(df$region == 1, 1, 0)
df$midwest <- ifelse(df$region == 2, 1, 0)
df$south <- ifelse(df$region == 3, 1, 0)
df$west <- ifelse(df$region == 4, 1, 0)

levels <- c("Northeast", "Midwest", "South", "West")
labels <- c(1, 2, 3, 4)
df$region <- factor(df$region, levels = labels, labels = levels)

# make binary variables for each urban/rural classification
df$lrge_cent_metr <- ifelse(df$urbrrl == 1, 1, 0)
df$lrge_frin_metr <- ifelse(df$urbrrl == 2, 1, 0)
df$med_sml_metr <- ifelse(df$urbrrl == 3, 1, 0)
df$non_metr <- ifelse(df$urbrrl == 4, 1, 0)

df$urbrur <- ifelse(df$urbrrl %in% c(1, 2, 3), 1, ifelse(df$urbrrl == 4, 0, NA))
levels <- c("Nonmetropolitan", "Metropolitan")
labels <- c(0, 1)
df$urbrur <- factor(df$urbrur, levels = labels, labels = levels)


levels <- c("Large central metro", "Large fringe metro", "Medium and small metro", "Nonmetropolitan")
labels <- c(1, 2, 3, 4)
df$urbrrl <- factor(df$urbrrl, levels = labels, labels = levels)

# binarize insurance status
df$public <- ifelse(df$insurancecat == 1, 1, 0)
df$private <- ifelse(df$insurancecat == 2, 1, 0)
df$uninsured <- ifelse(df$insurancecat == 3, 1, 0)

# binarize employment status
df$empstat <- ifelse(df$emplastwk_a == 1, 1, ifelse(df$emplastwk_a == 2, 0, NA))

levels <- c("Unemployed", "Employed")
labels <- c(0, 1)
df$empstat <- factor(df$empstat, levels = labels, labels = levels)

# covid severity
df$cvdsev_a <- ifelse(df$cvdsev_a %in% c(7, 9), NA, df$cvdsev_a)
levels <- c("Mild symptoms", "Moderate symptoms", "Severe symptoms")
labels <- c(2, 3, 4)
df$cvdsev_a <- factor(df$cvdsev_a, levels = labels, labels = levels)

# covid vaccinations
df$num_covvax <- NA
df$num_covvax <- ifelse(df$shtcvd191_a == 1, df$shtcvd19nm_a, 0)
df$num_covvax <- ifelse(df$num_covvax == 7 | df$num_covvax == 9, 0, df$num_covvax)

# health status
df$healthstat <- df$phstat_a
df$healthstat <- ifelse(df$healthstat %in% c(7, 9), NA, df$healthstat)

levels <- c("Excellent", "Very Good", "Good", "Fair", "Poor")
labels <- c(1, 2, 3, 4, 5)
df$healthstat <- factor(df$healthstat, levels = labels, labels = levels)

# comorbidities

# diabetes (type II)
df$diabetes <- ifelse(df$dibev_a %in% c(7, 9), NA, df$dibev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$diabetes <- factor(df$diabetes, levels = labels, labels = levels)

# asthma
df$asthma <- ifelse(df$asev_a %in% c(7, 9), NA, df$asev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$asthma <- factor(df$asthma, levels = labels, labels = levels)

# chronic obstructive pulmonary disease
df$copd <- ifelse(df$copdev_a %in% c(7, 9), NA, df$copdev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$copd <- factor(df$copd, levels = labels, labels = levels)

# anxiety
df$anxiety <- ifelse(df$anxev_a %in% c(7, 9), NA, df$anxev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$anxiety <- factor(df$anxiety, levels = labels, labels = levels)

# depression
df$depression <- ifelse(df$depev_a %in% c(7, 9), NA, df$depev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$depression <- factor(df$depression, levels = labels, labels = levels)

# chronic fatigue syndrome
df$chronicfatigue <- ifelse(df$cfsev_a %in% c(7, 9), NA, df$cfsev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$chronicfatigue <- factor(df$chronicfatigue, levels = labels, labels = levels)

# high cholesterol
df$hcholesterol <- ifelse(df$chlev_a %in% c(7, 9), NA, df$chlev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$hcholesterol <- factor(df$hcholesterol, levels = labels, labels = levels)

# hypertension
df$hypertension <- ifelse(df$hypev_a %in% c(7, 9), NA, df$hypev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$hypertension <- factor(df$hypertension, levels = labels, labels = levels)

# cancer
df$cancer <- ifelse(df$canev_a %in% c(7, 9), NA, df$canev_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$cancer <- factor(df$cancer, levels = labels, labels = levels)

# obsesity (BMI > 30)
df$obese <- ifelse(df$bmicat_a == 4, 1, 0)
levels <- c("Yes", "No")
labels <- c(1, 0)
df$obese <- factor(df$obese, levels = labels, labels = levels)

# disability (Washington Group Composite Disability Indicator)
df$disability <- ifelse(df$disab3_a == 9, NA, df$disab3_a)
levels <- c("Yes", "No")
labels <- c(1, 2)
df$disability <- factor(df$disability, levels = labels, labels = levels)

# convert "Yes" and "No" to 1 and 0
binary_convert <- function(var) {
  ifelse(var == "Yes", 1, 0)
}

# convert comorbidity variables to binary numeric
df$diabetes <- binary_convert(df$diabetes)
df$asthma <- binary_convert(df$asthma)
df$copd <- binary_convert(df$copd)
df$hcholesterol <- binary_convert(df$hcholesterol)
df$hypertension <- binary_convert(df$hypertension)
df$cancer <- binary_convert(df$cancer)
df$obese <- binary_convert(df$obese)

# calculate total comorbidities for each person
df$tot_comorb <- rowSums(df[, c("diabetes", "asthma", "copd", "hcholesterol", "hypertension", "cancer", "obese")], na.rm = TRUE)
df$tot_comorb <- cut(df$tot_comorb, breaks = c(-1, 0, 1, Inf), labels = c("0", "1", ">=2"), include.lowest = TRUE)
df$tot_comorb <- factor(df$tot_comorb, levels = c("0", "1", ">=2"), ordered = TRUE)

# healthcare access, affordability, and utilization

# insurance coverage
# make three insurance status categories (public, private, uninsured)
# 1 = public, 2 = private, 3 = uninsured
df$insurancecat <- NA
df$insurancecat[df$medicare_a == 1 | df$medicare_a == 2 | df$medicaid_a == 1 | df$medicaid_a == 2 | df$chip_a == 1 | df$chip_a == 2 | df$othpub_a == 1 | df$othpub_a == 2 | df$othgov_a == 1 | df$othgov_a == 2 | df$military_a == 1 | df$military_a == 2] <- 1
df$insurancecat[df$private_a == 1 | df$private_a == 2] <- 2
df$insurancecat[df$notcov_a == 1] <- 3

levels <- c("Public", "Private", "Uninsured")
labels <- c(1, 2, 3)
df$insurancecat <- factor(df$insurancecat, levels = labels, labels = levels)

# foregone or delayed medical care due to cost (affordability)
df$nocarecost <- ifelse(df$medng12m_a == 1, 1, ifelse(df$medng12m_a == 2, 0, NA))
df$latecarecost <- ifelse(df$meddl12m_a == 1, 1, ifelse(df$meddl12m_a == 2, 0, NA))
df$nomedscost <- ifelse(df$rxdg12m_a == 1, 1, ifelse(df$rxdg12m_a == 2, 0, NA))

df$badcarecost <- ifelse(df$nocarecost == 0 & df$latecarecost == 0 & df$nomedscost == 0, 0, 1)

levels <- c("Not forgoing or delaying care due to cost, past yr", "Forgoing or delaying care due to cost, past yr")
labels <- c(0, 1)
df$badcarecost <- factor(df$badcarecost, levels = labels, labels = levels)

# no usual source of care
df$noplace <- ifelse(df$usualpl_a %in% c(1, 3), 0, ifelse(df$usualpl_a == 2, 1, NA))

levels <- c("With usual source of care", "Without usual source of care")
labels <- c(0, 1)
df$noplace <- factor(df$noplace, levels = labels, labels = levels)

# not seen/talked to a health care professional in the past year
df$novisit <- ifelse(df$lastdr_a == 1, 0, ifelse(df$lastdr_a %in% c(0, 2, 3, 4, 5, 6), 1, NA))

levels <- c("Seen or talked to a health professional, past yr", "Not seen or talked to a health professional, past yr")
labels <- c(0, 1)
df$novisit <- factor(df$novisit, levels = labels, labels = levels)

# telehealth use
df$telehealth <- ifelse(df$virapp12m_a == 1, 1, ifelse(df$virapp12m_a == 2, 0, NA))

levels <- c("Did not use telemedicine, past yr", "Used telemedicine, past yr")
labels <- c(0, 1)
df$telehealth <- factor(df$telehealth, levels = labels, labels = levels)

# access barriers to care

# delayed because of insurance not being taken, appointments not being available, and transportation/time barriers
df$insnottaken <- ifelse(df$abinsur_a == 1, 1, ifelse(df$abinsur_a == 2, 0, NA))
df$apptnotavail <- ifelse(df$abavail_a == 1, 1, ifelse(df$abavail_a == 2, 0, NA))
df$unablewhenopen <- ifelse(df$abopen_a == 1, 1, ifelse(df$abopen_a == 2, 0, NA))
df$takestoolong <- ifelse(df$abtoolong_a == 1, 1, ifelse(df$abtoolong_a == 2, 0, NA))
df$busywithwork <- ifelse(df$abtime_a == 1, 1, ifelse(df$abtime_a == 2, 0, NA))

df$altdelaycare <- ifelse(df$insnottaken == 0 & df$apptnotavail == 0 & df$unablewhenopen == 0
                          & df$takestoolong == 0 & df$busywithwork == 0, 0, 1)

levels <- c("Not forgoing or delaying care due to other reason, past yr", "Forgoing or delaying care due to other reason, past yr")
labels <- c(0, 1)
df$altdelaycare <- factor(df$altdelaycare, levels = labels, labels = levels)

# financial burden
df$difficultbills <- ifelse(df$paybll12m_a == 1, 1, ifelse(df$paybll12m_a == 2, 0, NA))
df$worryabtbills <- ifelse(df$payworry_a %in% c(1, 2), 1, ifelse(df$payworry_a == 3, 0, NA))

levels <- c("No difficulty paying medical bills, past yr", "Difficulty paying medical bills, past yr")
labels <- c(0, 1)
df$difficultbills <- factor(df$difficultbills, levels = labels, labels = levels)

levels <- c("Not worried about paying medical bills", "Worried about paying medical bills")
labels <- c(0, 1)
df$worryabtbills <- factor(df$worryabtbills, levels = labels, labels = levels)

################################
####    2. DATA ANALYSIS    ####
################################

# extrapolate to population level estimates
# to get estimates, multiply means and SEs by 18 million (population of LC pts in US, annually)
# source: https://www.cbsnews.com/news/long-covid-americans-new-cdc-survey-data/
data <- as_survey(df, id = ppsu, weight = wtfa_a, strata = pstrat, nest = TRUE)

# what are the characteristics/covariates associated with LC recovery?

# subset data to only patients with LC
df_small <- df[df$ever_lc == 1,]

# survey-ify data
data2 <- as_survey(df_small, 
                   id = ppsu, 
                   weight = wtfa_a,
                   strata = pstrat, nest = TRUE)

# make summary table with national estimates (abridged)
tbl_svysummary(data2, by = recovered_lc, 
               include = c(agecat, female, race_simple, empstat,
                           insurancecat, educ_binary, urbrur, cvdsev_a)
) %>% add_p()

# make summary table with national estimates (full)
tbl_svysummary(data2, by = recovered_lc,
               include = c(agecat, female, race_simple, inc_binary, 
                           educ_binary, orient_binary, citizen_binary, 
                           insurancecat, region, cvdsev_a, num_covvax,
                           diabetes, asthma, copd, anxiety, depression,
                           chronicfatigue, hcholesterol, hypertension, obese,
                           public, private, uninsured, empstat, healthstat,
                           difficultbills, worryabtbills)
) %>% add_p()

# make summary table with sample estimates
t1_all <- (Table1_all_format <- tbl_summary(
  data = df_small, 
  by = 'recovered_lc',
  statistic = list(
    female ~ "{n} ({p})",
    agecat ~ "{n} ({p})",
    race_simple ~ "{n} ({p})",
    empstat ~ "{n} ({p})",
    insurancecat ~ "{n} ({p})",
    inc_binary ~ "{n} ({p})",
    difficultbills ~ "{n} ({p})",
    worryabtbills ~ "{n} ({p})",
    orient_binary ~ "{n} ({p})",
    citizen_binary ~ "{n} ({p})",
    educ_binary ~ "{n} ({p})",
    region ~ "{n} ({p})",
    urbrur ~ "{n} ({p})",
    hypertension ~ "{n} ({p})",
    hcholesterol ~ "{n} ({p})",
    obese ~ "{n} ({p})",
    copd ~ "{n} ({p})",
    diabetes ~ "{n} ({p})",
    cancer ~ "{n} ({p})",
    asthma ~ "{n} ({p})",
    depression ~ "{n} ({p})",
    anxiety ~ "{n} ({p})",
    chronicfatigue ~ "{n} ({p})",
    disability ~ "{n} ({p})",
    healthstat ~ "{n} ({p})",
    cvdsev_a ~ "{n} ({p})",
    num_covvax ~ "{n} ({p})",
    badcarecost ~ "{n} ({p})",
    novisit ~ "{n} ({p})",
    telehealth ~ "{n} ({p})",
    noplace ~ "{n} ({p})",
    altdelaycare ~ "{n} ({p})"),
  missing_text = "Missing",
  include = c(female, agecat, race_simple, empstat, insurancecat, inc_binary, difficultbills,
              worryabtbills, orient_binary, citizen_binary, educ_binary, region, urbrur,
              hypertension, hcholesterol, obese, copd, diabetes, cancer, asthma, depression,
              anxiety, chronicfatigue, disability, healthstat, cvdsev_a, num_covvax, badcarecost, 
              novisit, telehealth, noplace, altdelaycare)
)) %>% add_p(pvalue_fun = function(x) style_pvalue(x, digits = 3))



# multivariate logistic regression model to examine which factors are associated with CRN.
svy_design <- svydesign(ids = ~ppsu, strata = ~pstrat, 
                        weights = ~wtfa_a, data = df_small, nest = T)


# fit logistic regression model
fit <- svyglm(recovered_lc ~ 
                       relevel(agecat, ref = "Young") +
                       relevel(female, ref = "Male") +
                       relevel(race_simple, ref = "NH White") + 
                       relevel(urbrur, ref = "Nonmetropolitan") +
                       relevel(educ_binary, ref = "Less HS") +
                       relevel(cvdsev_a, ref = "Mild symptoms") +
                       relevel(citizen_binary, ref = "Citizen"),
                     design = svy_design, 
                     data = df_small,
                     family = quasibinomial(link = 'logit'))


# wald tests for the categorical variables with more than 2 levels
# age
wtest_age <- wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 2:3)    
wPvalue_age <- wtest_age$result$chi2[3]   # p = 0.00956

# race
wtest_race <- wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 5:8)    
wPvalue_race <- wtest_race$result$chi2[3]  # p = 0.0385

# symptom severity
wtest_ss <- wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 11:12)    
wPvalue_ss <- wtest_ss$result$chi2[3]  # p = 0.00599

# get odds ratios and confidence intervals
logistic.display(fit)

# alternate plot, with variables named
new_plot_data <- fread('full_fig_results.csv')

new_plot_data <- new_plot_data %>%
  mutate(Significance = case_when(
    P < 0.05 ~ 'Significant',
    OR == 1 ~ 'OR1',
    TRUE ~ 'Not Significant'
  ))


plot_data$characteristic <- factor(new_plot_data$characteristic, levels = rev(unique(new_plot_data$characteristic)))

or_plot <- ggplot(plot_data, aes(x = OR, y = characteristic, xmin = lower95ci, xmax = upper95ci, color = factor(Significance))) +
  geom_point(size = 3.25) +
  geom_errorbar(width = 0.5, color = '#80796B') +
  geom_vline(xintercept = 1, color = 'gray50', linetype = 'dashed') + 
  labs(x = 'Odds Ratio [95% CI]', y = '', color = 'Significance') +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_color_manual(values = c('Not Significant' = '#374e55', 'Significant' = '#df8f44', 'OR1' = '#6A6599', 'SPACE' = 'white')) +
  theme_pubr() + theme(legend.position = 'none')
