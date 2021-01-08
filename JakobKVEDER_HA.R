### ASSIGNMENT 1 ###

#setwd("~/Desktop/Lund University/PSYP14 - Advanced Scientific Methods in Psychology/Subcourse 1/Home Assignments")

library("tidyverse")

data_HA_1 = read_csv("https://tinyurl.com/ha-dataset1")
View(data_HA_1)

library("psych")

data_HA_1 %>% group_by(sex) %>% count() #I create a count of participants based on the sex variable to see if everyone got assigned either male or female (and no accidental cases for example "mlae" or "femlae")

data_HA_1 %>% 
  select(pain, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, IQ, household_income) %>%
  describe() 
#at first glance, there seem to be a few coding errors in the tibble
  #a) STAI_trait column has a min value of 3.90 (which should not be possible, as the measure scores range from 20 to 80)
  #b) age column has a max value of 444.00 (which should not be possible, as there are no documented cases of a human being living longer than 130 years)
  #c) household income has a min value of -3732.00 $
  #d) IQ column has a min value of 49.00 and a max value of 149.00, which are statistically extremely rare values (this raises some concern, as I ask myself whether a person with an IQ score of 49 would even be able to fill in the measures in an informative way)

data_HA_1 %>%
  ggplot() + 
  aes(x = STAI_trait) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(breaks = seq(0, 160, 1)) 
#I create a histogram for every variable with suspicious values, so I can determine the number of participants that I need to look closely at and decide whether to exclude them or not 
#histogram shows there is 1 participant with an impossible value

data_HA_1 %>%
  ggplot() + 
  aes(x = age) +
  geom_histogram(binwidth = 1) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there is 1 participant with an impossible value

data_HA_1 %>%
  ggplot() + 
  aes(x = household_income) +
  geom_histogram(binwidth = 5000) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there is 1 participant with an impossible value

data_HA_1 %>%
  ggplot() + 
  aes(x = IQ) +
  geom_histogram(binwidth = 2) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there are 2 participants with extreme values

which.min(data_HA_1$STAI_trait) #I tried hard to find a way for R to add participant ID for a specific set of values (unusual values for each plotted variable) on top of the histogram bars representing them, but to no avail; I ended up resorting to these functions instead 
which.max(data_HA_1$age)

#Household income will not be a predictor in any of the models of assignment 1, so I decide not to exclude the participant with the unusual value on this variable 
#I do not think it wise to speculate about the STAI_trait score of participant ID 150 and age of participant ID 93, even though the reported data might be simple entry errors (like a score of 39.00 and not 3.90 on STAI_trait, or age 44 instead of 444). I decide to exclude these two participants from the analysis for assignment 1.
#I also exclude the person with the IQ value below 50, as I deem that the measures used in this study were not adequately adapted to their present mental functioning level, causing us to lose valuable information  (diagnostically speaking, this person is characterized as having a moderate intellectual disability, and there are scales to measure constructs such as anxiety and mindfulness adapted for their population, e. g. Mindham & Espie, 2003). I decide to keep participants diagnostically categorized as having a mild intellectual disability (IQ above 60 and below 70), as a lower-than-average IQ does in itself indicate a significant limitation in intellectual functioning of the individual, but isn't enough to think of a person as disabled.   
  #to better understand my decision, see: 
    #1. Mindham, J., & Espie, C. A. (2003). Glasgow Anxiety Scale for people with an Intellectual Disability (GAS‐ID): development and psychometric properties of a new measure for use with people with mild intellectual disability. Journal of Intellectual Disability Research, 47(1), 22-30.
    #2. Boat, T. F., & Wu, J. T. (2015). Mental disorders and disabilities among low-income children.
    #3. https://www.psychiatry.org/patients-families/intellectual-disability/what-is-intellectual-disability

data_HA_1_excluded <- 
  data_HA_1 %>%
  filter(age < 130.01, STAI_trait > 19.99, IQ > 59.99)

mod_HA_1 <- lm(pain ~ age + sex, data = data_HA_1_excluded)
summary(mod_HA_1)

mod_HA_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_HA_1_excluded)
summary(mod_HA_2)

#I create graphical representations of the relationship of pain, the predicted variable, and the different predictors
data_HA_1_excluded %>% ggplot() + aes(x = age, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()
data_HA_1_excluded %>% ggplot() + aes(x = sex, y = pain, label = row.names(data_HA_1_excluded)) + geom_boxplot()
data_HA_1_excluded %>% ggplot() + aes(x = STAI_trait, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()
data_HA_1_excluded %>% ggplot() + aes(x = pain_cat, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()
data_HA_1_excluded %>% ggplot() + aes(x = mindfulness, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()
data_HA_1_excluded %>% ggplot() + aes(x = cortisol_serum, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()
data_HA_1_excluded %>% ggplot() + aes(x = cortisol_saliva, y = pain, label = row.names(data_HA_1_excluded)) + geom_point()

mod_HA_1 %>% plot(which=4)
#2 cases with high residual error and high leverage identified: rows 98, 126, 139 (ID-100, ID-128, ID-141: Cook's distance e. g. for participant in row 99, but I excluded some cases so participant ID and row number don't match necessarily ...)
mod_HA_2 %>% plot(which=4)
#3 cases with high residual error and high leverage identified: rows 67, 98, 112 (ID-68, ID-100, ID-114) 

slice_1_2 <- data_HA_1_excluded %>% slice(c(67, 98, 112, 126, 139))
View(slice_1_2)

describe(residuals(mod_HA_1))
describe(residuals(mod_HA_2))
#skewness and kurtosis for both models between -1 and 1; the residuals of both models seem to be normally distributed

library("car")

mod_HA_1 %>% residualPlots()
mod_HA_2 %>% residualPlots()
#the non-linearity test returns no significant test statistics; the relationship of the predictors (apart from sex) and the outcome variable seems to be linear

library("lmtest")

mod_HA_1 %>% bptest()
mod_HA_2 %>% bptest() 
#the Breusch-Pagan test returns no significant test statistics; the standard deviations of the residuals of both models seem to be constant

mod_HA_1 %>% vif()
mod_HA_2 %>% vif()
#in model 2, cortisol_serum and cortisol_saliva have variance inflation factors that exceed the value of 3, which indicates problematic multicollinearity 
#in line with previous research, I decide to deal with this issue by removing carotisol_saliva from model 2, as cortisol_serum is thought to be a more reliable biological indicator of stress between the two

mod_HA_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_HA_1_excluded)
summary(mod_HA_3)
mod_HA_3 %>% plot(which=4) #participants in rows 94, 98 and 112 have the largest Cook's distances
slice_3 <- data_HA_1_excluded %>% slice(c(94, 98, 112))
View(slice_3)
describe(residuals(mod_HA_3))
mod_HA_3 %>% residualPlots()
mod_HA_3 %>% bptest()
mod_HA_3 %>% vif

#Model comparison
#even though sex isn't a significant predictor of pain in Model 1, I decide to leave it in, because previous research shows that sex is a predictor of pain, dependent on the type of the medical procedure participants undergo (this gives us valuable information that at the dentist's office this might not be the case: https://www.theanalysisfactor.com/insignificant-effects-in-model/)
#for the same reason, I decide to leave all predictors from model 2, apart from cortisol_saliva, in model 3

AIC(mod_HA_1)
AIC(mod_HA_3)
anova(mod_HA_1, mod_HA_3)
#both the AIC and anova functions show that model 1 and 3 differ in their model fit significantly, with model 3 having better fit, meaning we gained substantial information about postoperative pain by adding the state and trait psychological variables

library(lm.beta)

confint(mod_HA_1)
confint(mod_HA_3)
lm.beta(mod_HA_1)
lm.beta(mod_HA_3)

### ASSIGNMENT 2 ###

data_HA_2_excluded <- 
  data_HA_1 %>%
  filter(age < 130.01, STAI_trait > 19.99, IQ > 59.99, household_income > 0.00) #For this assignment, I exclude an additional participants from my sample: the person with the household_income value below zero. 

data_HA_2_excluded %>% ggplot() + aes(x = weight, y = pain, label = row.names(data_HA_2_excluded)) + geom_point()
data_HA_2_excluded %>% ggplot() + aes(x = IQ, y = pain, label = row.names(data_HA_2_excluded)) + geom_point()
data_HA_2_excluded %>% ggplot() + aes(x = household_income, y = pain, label = row.names(data_HA_2_excluded)) + geom_point()

mod_HA_r_initial <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_HA_2_excluded)
summary(mod_HA_r_initial)

mod_HA_r_initial %>% plot(which=4)
#3 cases with high residual error and high leverage identified: participants in row 3, 98, 111 (ID-3, ID-100, and ID-114)

slice_initial <- data_HA_2_excluded %>% slice(c(3, 98, 111))
View(slice_initial)

describe(residuals(mod_HA_r_initial))
#skewness and kurtosis between -1 and 1; the residuals of the model seem to be normally distributed

mod_HA_r_initial %>% residualPlots()
#the non-linearity test returns no significant test statistics; the relationship of the predictors (apart from sex) and the outcome variable seems to be linear

mod_HA_r_initial %>% bptest() 
#the Breusch-Pagan test doesn't return a significant test statistic; the standard deviations of the residuals of the model seem to be constant

mod_HA_r_initial %>% vif()
#no variance inflation factor exceeds the threshold (3); the assumption of no multicollinearity isn't violated

step(mod_HA_r_initial, direction = "backward")
mod_HA_r_backward <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_HA_2_excluded) #backward model
summary(mod_HA_r_backward)
mod_HA_3_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_HA_2_excluded) #theory-based model 
summary(mod_HA_3_2) 

summary(mod_HA_r_initial)$adj.r.squared
summary(mod_HA_r_backward)$adj.r.squared #0.480
summary(mod_HA_3_2)$adj.r.squared #0.0.473

AIC(mod_HA_r_initial)
AIC(mod_HA_r_backward)
AIC(mod_HA_3_2) #lowest AIC for the backwards model, 2.1 lower than AIC for the theory-based model

data_HA_2_additional = read_csv("https://tinyurl.com/ha-dataset2") #I load the new data collected specifically to test the theory-based and backward models 
View(data_HA_2_additional)

#I check the new data for coding errors
data_HA_2_additional %>% group_by(sex) %>% count()
data_HA_2_additional %>% 
  select(pain, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, IQ, household_income) %>%
  describe() 
#at first glance, there seems to be a coding error in the tibble
  #a) the max value for the mindfulness scale is 7.17, which is not possible as the highest possible score is 6

data_HA_2_additional %>%
  ggplot() + 
  aes(x = mindfulness) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 6, 0.5)) + 
  scale_y_continuous(breaks = seq(0, 160, 1)) 

data_HA_2_additional_excluded <- 
  data_HA_2_additional %>%
  filter(mindfulness < 6.01) #I excluded one participant with an impossible score on the mindfulness scale
View(data_HA_2_additional_excluded)

theory_based_pred <- predict(mod_HA_3_2, data_HA_2_additional_excluded) #predicting pain for subjects in the second data set, based on the theory based model trained on the first data set
backward_pred <- predict(mod_HA_r_backward, data_HA_2_additional_excluded) #predicting pain for subjects in the second data set, based on the backward model trained on the first data set

RSS_theory_based <- sum((data_HA_2_additional_excluded[, "pain"] - theory_based_pred)^2) #calculating the sum of squared differences between reported and predicted pain values in testing the fit of the theory-based model on new data 
RSS_backward <- sum((data_HA_2_additional_excluded[, "pain"] - backward_pred)^2) #calculating the sum of squared differences between reported and predicted pain values in testing the fit of the backward model on new data
  
RSS_theory_based #lower than the backward RSS for 10 points
RSS_backward

confint(mod_HA_r_backward)
lm.beta(mod_HA_r_backward)

### ASSIGNMENT 3 ###

data_HA_3 = read_csv("https://tinyurl.com/ha-dataset3")
View(data_HA_3)

data_HA_3 %>% group_by(sex) %>% count() #I detect one coding error, a participant being mistakenly assigned "femlae" as sex
#detach("package:car", unload = FALSE, force = TRUE) #I am about to fix the above coding error using the recode function from tidyverse, which is masked by the one in the car package (R warned me about it earlier), so I detach it for the time being 
data_HA_3_corr <- data_HA_3 %>% dplyr::na_if("femlae")
data_HA_3_corr <- data_HA_3_corr %>% dplyr::mutate(sex = replace_na(sex, "female")) 
data_HA_3_corr %>% group_by(sex) %>% count()

data_HA_3_corr %>% 
  select(pain, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, IQ, household_income) %>%
  describe()
#at first glance, there seems to be a coding error in the tibble 
  #a) the min value for household_income is below zero, which shouldn't be possible
#the min value for IQ is below 60, which was a criterion for exclusion in earlier data sets, that I will keep in this assignment as well 

data_HA_3_corr %>%
  ggplot() + 
  aes(x = household_income) +
  geom_histogram(binwidth = 5000) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there is 1 participant with an impossible value; I do not exclude them from the data set, however, as I will not use household_income as a predictor in any of the Assignment 3 models 

data_HA_3_corr %>%
  ggplot() + 
  aes(x = IQ) +
  geom_histogram(binwidth = 2) +
  scale_x_continuous(breaks = seq(0,150,2)) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there is 1 participant with a value below 60 

data_HA_3_corr_excluded <- 
  data_HA_3_corr %>%
  filter(IQ > 59.99)

data_HA_3_corr_excluded %>% group_by(hospital) %>% count() #getting an idea of how sample distributes across hospitals; seems to be a rectangular distribution 

library("cAIC4")
library("r2glmm")
library("lme4")
library("lmerTest")
library("MuMIn")
library("optimx")

summary(mod_HA_3) #summary of the final model from Assignment 1
mod_HA_random_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_HA_3_corr_excluded) #building a random intercept linear mixed model, accounting for the clustering of data at different hospitals   
summary(mod_HA_random_int)
confint(mod_HA_random_int)

AIC(mod_HA_3)
cAIC(mod_HA_random_int)

r2beta(mod_HA_random_int, method = "nsj", data = data_HA_3_corr_excluded) #calculating the  marginal R squared and the 95% confidence interval for the model and each fixed effect predictor
#the function identifies that the 95% confidence intervals of explained outcome variance of 4 predictors, namely mindfulness, pain_cat, sex, and STAI_trait contain 0, which means that they bring no significant explanatory value to the model   
r.squaredGLMM(mod_HA_random_int) #calculating the conditional R squared for the random intercept model 

data_HA_4 = read_csv("https://tinyurl.com/ha-dataset4")
View(data_HA_4)

data_HA_4 %>% group_by(sex) %>% count()

data_HA_4 %>% 
  select(pain, age, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness, weight, IQ, household_income) %>%
  describe()
#at first glance, there seem to be a few impossible or extreme values in the tibble
  #a) the max value for mindfulness is 6.05, which should be impossible as composite scores range between 1 and 6
  #b) the min value for IQ is 58.00, which is below the previously established threshold of 60.00
  #c) the min value for household_income is below zero, which should be impossible 

data_HA_4 %>%
  ggplot() + 
  aes(x = mindfulness) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(breaks = seq(1, 6, 1)) + 
  scale_y_continuous(breaks = seq(0, 160, 2))
#histogram shows there is 1 participant with an impossible value; I decide to exclude them from any further analyses

data_HA_4 %>%
  ggplot() + 
  aes(x = household_income) +
  geom_histogram(binwidth = 5000) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there are 2 participants with an impossible value; I do not exclude them from the data set, however, as I will not use household_income as a predictor in any of the Assignment 3 models 

data_HA_4 %>%
  ggplot() + 
  aes(x = IQ) +
  geom_histogram(binwidth = 2) +
  scale_x_continuous(breaks = seq(0,150,2)) +
  scale_y_continuous(breaks = seq(0, 160, 1))
#histogram shows there is 1 participant with a value below 60; I decide to exclude them from the data set before performing any further analyses on it  

data_HA_4_excluded <- data_HA_4 %>%
  filter(mindfulness < 6.01, IQ > 59.99)
View(data_HA_4_excluded)

random_int_pred <- predict(mod_HA_random_int, newdata = data_HA_4_excluded, re.form = NULL, allow.new.levels = TRUE) #predicting pain for subjects in the fourth data set, based on the random intercept model trained on the third data set
#the fourth data set contains data collected at hospital sites different from those in data set three, which is why I am allowing the predict function to use new levels of this variable; in this case, the prediction uses the unconditional (population-level) values for variables with previously unobserved levels
  #in other words, the function relies on estimates of global distributions of population means in the prediction of outcome values for the average group, and is doing so by using the means from the distributions of variables I used to build our random effect model  

#computing how much outcome variance the random intercept model explains on the fourth data set; will be using the following formula 1-(RSS/TSS), where RSS stands for residual sum of squares, and TSS for total sum of squares
RSS_random_int <- sum((data_HA_4_excluded$pain - random_int_pred)^2)
TSS_random_int <- sum((data_HA_4_excluded$pain - mean(data_HA_4_excluded$pain))^2)
R2_random_int <- 1 - (RSS_random_int/TSS_random_int)

#building a random intercept and random slope linear mixed model predicting pain based on cortisol_serum (most influential predictor from the previous model) values on the third data set 
mod_HA_random_int_and_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_HA_3_corr_excluded)
summary(mod_HA_random_int_and_slope)

#visualizing fitted regression lines for each individual hospital site 
random_int_and_slope_pred <- predict(mod_HA_random_int_and_slope)
data_HA_3_corr_excluded %>%
  ggplot() + 
  aes(y = pain, x = cortisol_serum, group = hospital) + 
  geom_point(aes(color = hospital), size = 3) + 
  geom_line(color = 'red', aes(y = random_int_and_slope_pred, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 5)
