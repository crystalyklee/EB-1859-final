# #Questions to Answer
# 1. What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
# 2. What is the impact and relationship of sleep disturbance with health related quality of life in these patients

# # Steps
# 1. Assign roles, including code reviewer
# 2. Clean data, focus on relevant variables
# NAs appear to be listed
RawData <- read.csv("project_data.csv")

# 3. Describe relevant data: some are categorical, some are numeric
# Can use summary functions, convert others to factors
RelevantData <- RawData[,c(2,3,9,16,18,19,20,24,42,46,58,70,75,77,85,86,87)]
RelevantData$Gender <- factor(RelevantData$Gender,levels = c(1,2), labels = c("male","female"))
RelevantData$Liver.Diagnosis <- factor(RelevantData$Liver.Diagnosis,levels = c(1:5), labels = c("Hep C","Hep B","PSC/PBC/AHA","alcohol","other"))
RelevantData$Recurrence.of.disease <- factor(RelevantData$Recurrence.of.disease,levels = c(1,0), labels = c("yes","no"))
RelevantData$Rejection.graft.dysfunction <- factor(RelevantData$Rejection.graft.dysfunction,levels = c(1,0), labels = c("yes","no"))
RelevantData$Any.fibrosis <- factor(RelevantData$Any.fibrosis,levels = c(1,0), labels = c("yes","no"))
RelevantData$Renal.Failure <- factor(RelevantData$Renal.Failure,levels = c(1,0), labels = c("yes","no"))
RelevantData$Depression <- factor(RelevantData$Depression,levels = c(1,0), labels = c("yes","no"))
RelevantData$Corticoid <- factor(RelevantData$Corticoid,levels = c(1,0), labels = c("yes","no"))
#RelevantData$Berlin.Sleepiness.Scale <- factor(RelevantData$Berlin.Sleepiness.Scale,levels = c(1,0), labels = c("high likelihood of sleep disordered breathing","low likelihood of sleep disordered breathing"))

summary(RelevantData)

#-------------------------------------------------------------------------------
# 4. Estimate the prevalence of sleep disturbance (Q1)
#    Need to examine the various sleep scores - literature review
#    -> Berlin.Sleepiness.Scale is binary (value of 1 indicates disturbance)
#    -> Pittsburgh Sleep Quality Index (PSQI), the Epworth sleepiness scale (ESS),
#       and the Athens insomnia scale (AIS) are numeric with higher values
#       indicating worse sleep.
#    Collate these different measures into a new column of the data frame using
#    the mutate function

library(dplyr)

# Code that can ennumerate the number of sleep disturbances, defined as "disorders of initiating and maintaining sleep (DIMS, insomnias), disorders of excessive somnolence (DOES), disorders of sleep–wake schedule, and dysfunctions associated with sleep, sleep stages, or partial arousals (parasomnias)." - https://www.ncbi.nlm.nih.gov/books/NBK401/#:~:text=Sleep%20disturbances%20encompass%20disorders%20of,or%20partial%20arousals%20(parasomnias).
library(forcats)
RelevantData <- RelevantData %>%
  mutate(Disordered_EpWorth = case_when(Epworth.Sleepiness.Scale >10 ~ 1, .default = 0)) %>%  # >10
  mutate(Disordered_Pittsburgh = case_when(Pittsburgh.Sleep.Quality.Index.Score >4 ~ 1, .default = 0)) %>%  # >10
  mutate(Disordered_Athens = case_when(Athens.Insomnia.Scale >5 ~ 1, .default = 0)) # >10

RelevantData <- mutate(RelevantData,Sleep_Disturbance = case_when(
    Berlin.Sleepiness.Scale == 1 | 
    Epworth.Sleepiness.Scale >10 |
    Pittsburgh.Sleep.Quality.Index.Score >4 |
    Athens.Insomnia.Scale >5 ~ as.factor(1),
    .default = as.factor(0)
))

Sleep_Disturbance_Prevalence <- sum(as.numeric(RelevantData$Sleep_Disturbance))/length(RelevantData$Sleep_Disturbance)

#-------------------------------------------------------------------------------
# 5. Identify predictors that are associated with sleep disturbance (Q1)
#    Logistic regression due to the binomial nature of the outcome (sleep disturbance)
#    True predictors will have larger coefficients and smaller p-values
#    Can compare different suites of predictors using ANOVAs with nested models

# examine the correlation of the predictors
cor(RawData[,c(2,3,9,16,18,19,20,24,42,46,58)], use = "pairwise")
# Recurrence.of.disease and Liver.Diagnosis have a correlation of -0.53783662
# Recurrence.of.disease and Rejection.graft.dysfunction have a correlation of 0.35072921
# Recurrence.of.disease and Any.fibrosis have a correlation of 0.55298669
# Rejection.graft.dysfunction and Any.fibrosis have a correlation of 0.614728595
# Liver.Diagnosis and Any.fibrosis have a correlation of -0.31888546

# should not include the sleep scales in the model, but of the remaining predictors:
# Gender
# Age: increasing age can correlate with difficulty sleeping - https://pubmed.ncbi.nlm.nih.gov/29878472/
# BMI: direct link between obesity and sleep disorders - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10367528/
# Time.from.transplant
# Liver.Diagnosis
# Recurrence.of.disease - may choose to focus on disease recurrance rather than liver diagnosis, Rejection.graft.dysfunction, and Any.fibrosis, given the correlation > 0.35
# Rejection.graft.dysfunction
# Any.fibrosis
# Renal.Failure
# Depression: depression and sleep disorders are highly linked - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3181883/
# Corticoid: steroids can cause sleep disturbance - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7357890/

# create a model which uses all the predictors
model1 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(model1)
# Create more restricted models that use different combinations of the liver health predictors
model2 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Depression+Corticoid, data=RelevantData,family = binomial)
model3 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Recurrence.of.disease+Depression+Corticoid, data=RelevantData,family = binomial)
model4 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Rejection.graft.dysfunction+Depression+Corticoid, data=RelevantData,family = binomial)
model5 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Any.fibrosis+Depression+Corticoid, data=RelevantData,family = binomial)
model6 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)

AIC(model1) #267.383
AIC(model2) #270.4803
AIC(model3) #258.8506 - as predicted, Recurrence.of.disease is able to explain much of the variation for the liver health variables
AIC(model4) #267.9753
AIC(model5) #267.7453
AIC(model6) #267.2704

# test to see what happens if renal failure is added, given it had a lower correlation with Recurrence.of.disease compared to the other predictors
model7 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Recurrence.of.disease+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model7) #258.6184 - including renal failure does not improve the model

# remove the predictors that have very small beta coefficients (<0.01)
model8 <- glm(Sleep_Disturbance ~ BMI+Time.from.transplant+Recurrence.of.disease+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model8) #255.6975 - can cut out gender and age and still the model performs well
summary(model8)

# remove the predictors that have small beta coefficients (<0.1)
model9 <- glm(Sleep_Disturbance ~ Recurrence.of.disease+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model9) #283.9534 - does worse, BMI and Time.from.transplant may be key influences on the model and can't be removed

# create a model that uses only the predictors with the lowest p-values
model10 <- glm(Sleep_Disturbance ~BMI+Recurrence.of.disease+Depression, data=RelevantData,family = binomial)
AIC(model10) #254.2909 - a tiny bit better AIC value than model 8
summary(model10)

# create a model that includes only the predictors with significant p-values
model11 <- glm(Sleep_Disturbance ~Recurrence.of.disease+Depression, data=RelevantData,family = binomial)
AIC(model11) #282.1825 - also does worse than model 8 when it comes to AIC, but all of the predictors have a p-value less than 0.05
summary(model11)

# compare the models
NARemovedRelevantData <- na.omit(RelevantData[c("BMI","Time.from.transplant","Recurrence.of.disease","Renal.Failure","Depression","Corticoid","Sleep_Disturbance")]) #test the models on the same data
OmittedModel8 <- glm(Sleep_Disturbance ~ BMI+Time.from.transplant+Recurrence.of.disease+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
OmittedModel11 <- glm(Sleep_Disturbance ~Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
anova(OmittedModel11,OmittedModel8, test = "Chisq")
# not statistically significant in difference
# the most important predictors for sleep disturbance are Recurrence.of.disease and Depression
# but! Some data has been lost, 23 observations, though it is only ~8.58% missingness in the dataset


AllNARemovedRelevantData <- na.omit(RelevantData) #test the models on the same data
# try the backwards stepping procedure
library(ISLR)
library(MASS)
model0 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
glm.step.back <- stepAIC(model0,trace = F)
# here is the selected model
summary(glm.step.back)
# the p-values are not accurate
# Forward stepwise selection using AIC
# here is the selected model
glm.step.back$coefficients
# produces the same result as the reduced model 11


# Create models using the backwards stepping for each individual test

EpworthMode1 <- glm(Disordered_EpWorth ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Epworthglm.step.back <- stepAIC(EpworthMode1,trace = F)
# here is the selected model
summary(Epworthglm.step.back)

PittsburghModel <- glm(Disordered_Pittsburgh ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Pittsburghglm.step.back <- stepAIC(PittsburghModel,trace = F)
# here is the selected model
summary(Pittsburghglm.step.back)

AthensModel <- glm(Disordered_Athens ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Athensglm.step.back <- stepAIC(AthensModel,trace = F)
# here is the selected model
summary(Athensglm.step.back)

BerlinModel <- glm(Berlin.Sleepiness.Scale ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Berlinglm.step.back <- stepAIC(BerlinModel,trace = F)
# here is the selected model
summary(Berlinglm.step.back)

#-------------------------------------------------------------------------------
# 6. Evaluate the relationship between sleep disturbance and quality of life (Q2)
#    Health Related Quality of Life (QOL) is measured by SF36 PCS and SF36 MCS:
#    -> SF36 PCS measures the physical component of QOL
#    -> SF36 MCS measures the mental component of QOL
#    They both take numeric values, with larger values indicating better QOL
#    Logistic regression due to the binomial nature of the outcome (sleep disturbance)

# 7. Write up the answers to Q1 and Q2

# 8. Create presentation on the answers to Q1 and Q2
