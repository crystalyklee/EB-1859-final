# #Questions to Answer
# 1. What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
# 2. What is the impact and relationship of sleep disturbance with health related quality of life in these patients


# Examine the data visually
# NAs appear to be listed
RawData <- read.csv("project_data.csv")

# Load packages
library(dplyr)
library(funModeling) 
library(Hmisc)
library(car)
library(forcats)
library(pscl)
library(ISLR)
library(MASS)
library(ggplot2)
library(tidyr)

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

# EDA using Data Science Heroes' Template
summary(RelevantData)
glimpse(RelevantData)
print(status(RelevantData))
freq(RelevantData) 
print(profiling_num(RelevantData))
plot_num(RelevantData)
describe(RelevantData)

#-------------------------------------------------------------------------------
# 4. Estimate the prevalence of sleep disturbance (Q1)
#    Need to examine the various sleep scores - literature review
#    -> Berlin.Sleepiness.Scale is binary (value of 1 indicates disturbance)
#    -> Pittsburgh Sleep Quality Index (PSQI), the Epworth sleepiness scale (ESS),
#       and the Athens insomnia scale (AIS) are numeric with higher values
#       indicating worse sleep.
#    Collate these different measures into a new column of the data frame using
#    the mutate function

# Code that can ennumerate the number of sleep disturbances, defined as "disorders of initiating and maintaining sleep (DIMS, insomnias), disorders of excessive somnolence (DOES), disorders of sleep–wake schedule, and dysfunctions associated with sleep, sleep stages, or partial arousals (parasomnias)." - https://www.ncbi.nlm.nih.gov/books/NBK401/#:~:text=Sleep%20disturbances%20encompass%20disorders%20of,or%20partial%20arousals%20(parasomnias).
RelevantData <- RelevantData %>%
  mutate(Disordered_EpWorth = case_when(Epworth.Sleepiness.Scale >10 ~ 1, .default = 0)) %>%  # >10
  mutate(Disordered_Pittsburgh = case_when(Pittsburgh.Sleep.Quality.Index.Score >4 ~ 1, .default = 0)) %>%  # >10
  mutate(Disordered_Athens = case_when(Athens.Insomnia.Scale >5 ~ 1, .default = 0)) %>% # >10
  mutate(Disordered_Berlin = case_when(Berlin.Sleepiness.Scale == 1 ~ 1, .default = 0))

RelevantData <- mutate(RelevantData,Sleep_Disturbance = case_when( #the case when will check to see if a participant has ANY sleep disorders
    Berlin.Sleepiness.Scale == 1 |
    Epworth.Sleepiness.Scale >10 |
    Pittsburgh.Sleep.Quality.Index.Score >4 |
    Athens.Insomnia.Scale >5 ~ as.factor(1),
    .default = as.factor(0)
))

Sleep_Disturbance_Prevalence <- sum(as.numeric(as.character(RelevantData$Sleep_Disturbance)))/length(RelevantData$Sleep_Disturbance)

# Reshape the data for plotting
plot_data <- RelevantData %>%
  pivot_longer(cols = starts_with("Disordered_"),
               names_to = "Disorder_Type",
               values_to = "Presence") %>%
  group_by(Disorder_Type, Presence) %>%
  summarise(Count = n(), .groups = 'drop') # Ensure grouping is done before summarizing

# Create the combined bar graph
ggplot(plot_data, aes(x = Disorder_Type, y = Count, fill = factor(Presence))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  labs(title = "Frequency of Participants with and without Each Sleep Disorder",
       x = "Disorder Type",
       y = "Count",
       fill = "Disorder Present (1) or Absent (0)") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "salmon")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

# plot the response variable against the predictor variables to see if there is any clear relationships
ggplot(RelevantData,aes(x=Gender, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Age, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=BMI, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Time.from.transplant, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Liver.Diagnosis, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Recurrence.of.disease, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Rejection.graft.dysfunction, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Any.fibrosis, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Renal.Failure, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association, almost no renal failure
ggplot(RelevantData,aes(x=Depression, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Corticoid, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=SF36.PCS, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=SF36.MCS, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #some difference in distribution

#-------------------------------------------------------------------------------

# create a model for sleep disturbance which uses all the predictors
model1 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(model1)

# examine collinearity
vif(model1)
# all GVIF values are below 4

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

# test to see what happens if renal failure is added to model3, given it had a lower correlation with Recurrence.of.disease compared to the other predictors
model7 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Recurrence.of.disease+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model7) #258.6184 - including renal failure does not improve the model

# remove the predictors that have very small beta coefficients (<0.01)
model8 <- glm(Sleep_Disturbance ~ BMI+Time.from.transplant+Recurrence.of.disease+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model8) #255.8929 - can cut out gender and age and still the model performs well
summary(model8)

# remove the predictors that have small beta coefficients (<0.1)
model9 <- glm(Sleep_Disturbance ~ Recurrence.of.disease+Depression+Corticoid, data=RelevantData,family = binomial)
AIC(model9) #284.0726 - does worse, BMI and Time.from.transplant may be key influences on the model and can't be removed
summary(model9)

# create a model that uses only the predictors with the lowest p-values
model10 <- glm(Sleep_Disturbance ~BMI+Recurrence.of.disease+Depression, data=RelevantData,family = binomial)
AIC(model10) #254.2909 - a tiny bit better AIC value than model 8
summary(model10)

# create a model that includes only the predictors with significant p-values
model11 <- glm(Sleep_Disturbance ~Recurrence.of.disease+Depression, data=RelevantData,family = binomial)
AIC(model11) #282.1825 - also does worse than model 8 when it comes to AIC, but all of the predictors have a p-value less than 0.05
summary(model11)

# compare the models
NARemovedRelevantData <- na.omit(RelevantData[c("Sleep_Disturbance","Gender","Age","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid")]) #test the models on the same data
OmittedModel1 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
OmittedModel11 <- glm(Sleep_Disturbance ~Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
OmittedModel10 <- glm(Sleep_Disturbance ~BMI+Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
anova(OmittedModel1,OmittedModel10, test = "Chisq")
anova(OmittedModel1,OmittedModel11, test = "Chisq")
anova(OmittedModel10,OmittedModel11, test = "Chisq")

NARemovedRelevantData <- na.omit(RelevantData[c("Sleep_Disturbance","Gender","Age","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid")]) #test the models on the same data
OmittedModel1 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
OmittedModel11 <- glm(Sleep_Disturbance ~Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
OmittedModel10 <- glm(Sleep_Disturbance ~BMI+Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
anova(OmittedModel1,OmittedModel10, test = "Chisq")
anova(OmittedModel1,OmittedModel11, test = "Chisq")
anova(OmittedModel10,OmittedModel11, test = "Chisq")
# not statistically significant in difference
# the most important predictors for sleep disturbance are Recurrence.of.disease and Depression
# but! Some data has been lost, 23 observations, though it is only ~8.58% missingness in the dataset

# try the backwards stepping procedure
AllNARemovedRelevantData <- na.omit(RelevantData) #test the models on the same data
model12 <- glm(Sleep_Disturbance ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
model13 <- stepAIC(model12,trace = F)
# here is the selected model
summary(model13)
# the p-values are not accurate
model13$coefficients
# produces the same result as the reduced model 11

# summarize the models
ModelAICSummary <- data.frame(NumberOfCoefficients = c((length(model1$coefficients)),(length(model2$coefficients)),(length(model3$coefficients)),(length(model4$coefficients)),(length(model5$coefficients)),(length(model6$coefficients)),(length(model7$coefficients)),(length(model8$coefficients)),(length(model9$coefficients)),(length(model10$coefficients)),(length(model11$coefficients)),(length(model12$coefficients)),(length(model13$coefficients))
                                                  ),
                              ModelAIC = c(AIC(model1),AIC(model2),AIC(model3),AIC(model4),AIC(model5),AIC(model6),AIC(model7),AIC(model8),AIC(model9),AIC(model10),AIC(model11),AIC(model12),AIC(model13)),
                              NumberOfObservations = c((length(model1$fitted.values)),(length(model2$fitted.values)),(length(model3$fitted.values)),(length(model4$fitted.values)),(length(model5$fitted.values)),(length(model6$fitted.values)),(length(model7$fitted.values)),(length(model8$fitted.values)),(length(model9$fitted.values)),(length(model10$fitted.values)),(length(model11$fitted.values)),(length(model12$fitted.values)),(length(model13$fitted.values)))
                              )
ModelAICSummary <- mutate(ModelAICSummary,AIC2ObservationRatio = ModelAIC/NumberOfObservations)
#Model NumberOfCoefficients  ModelAIC NumberOfObservations  AIC2ObservationRatio
# 13          3             143.4618         151                  0.9500781
# 12         15             156.6485         151                  1.0374070
# 10          4             254.2909         245                  1.0379221

#-------------------------------------------------------------------------------
# Create models using the backwards stepping for each individual test
NAOmittedEpworthMode1 <- glm(Disordered_EpWorth ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Epworthglm.step.back <- stepAIC(NAOmittedEpworthMode1,trace = F)
# here is the selected model
summary(Epworthglm.step.back) #glm(formula = Disordered_EpWorth ~ Liver.Diagnosis + Renal.Failure + Corticoid, family = binomial, data = AllNARemovedRelevantData)

NAOmittedPittsburghModel <- glm(Disordered_Pittsburgh ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Pittsburghglm.step.back <- stepAIC(NAOmittedPittsburghModel,trace = F)
# here is the selected model
summary(Pittsburghglm.step.back) #glm(formula = Disordered_Pittsburgh ~ Gender + Age + Recurrence.of.disease + Renal.Failure + Depression, family = binomial, data = AllNARemovedRelevantData)

NAOmittedAthensModel <- glm(Disordered_Athens ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Athensglm.step.back <- stepAIC(NAOmittedAthensModel,trace = F)
# here is the selected model
summary(Athensglm.step.back) #glm(formula = Disordered_Athens ~ Age + Liver.Diagnosis + Depression + Corticoid, family = binomial, data = AllNARemovedRelevantData)

NAOmittedBerlinModel <- glm(Berlin.Sleepiness.Scale ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=AllNARemovedRelevantData,family = binomial)
# backward stepwise selection using AIC
Berlinglm.step.back <- stepAIC(NAOmittedBerlinModel,trace = F)
# here is the selected model
summary(Berlinglm.step.back) #glm(formula = Berlin.Sleepiness.Scale ~ BMI + Renal.Failure, family = binomial, data = AllNARemovedRelevantData)

# Create a model that includes all of the simulated predictors from the stepping back procedure
CumulativeModel.step.back <- glm(Sleep_Disturbance ~ Gender+Age+Recurrence.of.disease+Depression+Liver.Diagnosis+Corticoid+BMI+Renal.Failure, data=RelevantData, family=binomial)
summary(CumulativeModel.step.back)

#-------------------------------------------------------------------------------

# Epworth
# plot the response variable against the predictor variables to see if there is any clear relationships
ggplot(RelevantData,aes(x=Gender, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Age, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=BMI, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Time.from.transplant, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Liver.Diagnosis, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Recurrence.of.disease, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Rejection.graft.dysfunction, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #some asymmetry across no rejection and disordered Epworth sleep
ggplot(RelevantData,aes(x=Any.fibrosis, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #some asymmetry across some fibrosis and disordered Epworth sleep
ggplot(RelevantData,aes(x=Renal.Failure, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association, almost no renal failure
ggplot(RelevantData,aes(x=Depression, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #some asymmetry across depression and disordered Epworth sleep
ggplot(RelevantData,aes(x=Corticoid, y=Disordered_EpWorth)) + geom_jitter(height=0.05,alpha=0.5) #no strong association

# Pittsburgh
# plot the response variable against the predictor variables to see if there is any clear relationships
ggplot(RelevantData,aes(x=Gender, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Age, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=BMI, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Time.from.transplant, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Liver.Diagnosis, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Recurrence.of.disease, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Rejection.graft.dysfunction, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Any.fibrosis, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Renal.Failure, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association, almost no renal failure
ggplot(RelevantData,aes(x=Depression, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Corticoid, y=Disordered_Pittsburgh)) + geom_jitter(height=0.05,alpha=0.5) #no strong association

# Athens
# plot the response variable against the predictor variables to see if there is any clear relationships
ggplot(RelevantData,aes(x=Gender, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Age, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=BMI, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Time.from.transplant, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Liver.Diagnosis, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Recurrence.of.disease, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Rejection.graft.dysfunction, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Any.fibrosis, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Renal.Failure, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Depression, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Corticoid, y=Disordered_Athens)) + geom_jitter(height=0.05,alpha=0.5) #no strong association

# Berlin
# plot the response variable against the predictor variables to see if there is any clear relationships
ggplot(RelevantData,aes(x=Gender, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Age, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=BMI, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #some association
ggplot(RelevantData,aes(x=Time.from.transplant, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Liver.Diagnosis, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Recurrence.of.disease, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Rejection.graft.dysfunction, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Any.fibrosis, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Renal.Failure, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Depression, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=Corticoid, y=Berlin.Sleepiness.Scale)) + geom_jitter(height=0.05,alpha=0.5) #no strong association

#-------------------------------------------------------------------------------

# Create models by selecting only the most statistically significant predictors
NARemovedRelevantData <- na.omit(RelevantData[c("Gender","Age","BMI","Time.from.transplant","Liver.Diagnosis","Recurrence.of.disease","Rejection.graft.dysfunction","Any.fibrosis","Renal.Failure","Depression","Corticoid","Sleep_Disturbance","Disordered_EpWorth","Disordered_Pittsburgh","Disordered_Athens","Berlin.Sleepiness.Scale","Sleep_Disturbance")]) #test the models on the same data

EpworthModel1 <- glm(Disordered_EpWorth ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(EpworthModel1)
EpworthModel2 <- glm(Disordered_EpWorth ~ Liver.Diagnosis+Corticoid, data=RelevantData,family = binomial)
summary(EpworthModel2)
NAOmittedEpworthModel1 <- glm(Disordered_EpWorth ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
NAOmittedEpworthModel2 <- glm(Disordered_EpWorth ~ Liver.Diagnosis+Corticoid, data=NARemovedRelevantData,family = binomial)
anova(NAOmittedEpworthModel1,NAOmittedEpworthModel2,test="Chisq")

PittsburghModel1 <- glm(Disordered_Pittsburgh ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(PittsburghModel1)
PittsburghModel2 <- glm(Disordered_Pittsburgh ~ BMI+Liver.Diagnosis+Recurrence.of.disease, data=RelevantData,family = binomial)
summary(PittsburghModel2)
NAOmittedPittsburghModel1 <- glm(Disordered_Pittsburgh ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
NAOmittedPittsburghModel2 <- glm(Disordered_Pittsburgh ~ BMI+Liver.Diagnosis+Recurrence.of.disease, data=NARemovedRelevantData,family = binomial)
anova(NAOmittedPittsburghModel1,NAOmittedPittsburghModel2,test="Chisq") #the reduced model explains significantly less variance than the larger model (310.36 vs 290.40)

AthensModel1 <- glm(Disordered_Athens ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(AthensModel1)
AthensModel2 <- glm(Disordered_Athens ~ Recurrence.of.disease+Depression, data=RelevantData,family = binomial)
summary(AthensModel2)
NAOmittedAthensModel1 <- glm(Disordered_Athens ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
NAOmittedAthensModel2 <- glm(Disordered_Athens ~ Recurrence.of.disease+Depression, data=NARemovedRelevantData,family = binomial)
anova(NAOmittedAthensModel1,NAOmittedAthensModel2,test="Chisq") #the reduced model explains significantly less variance than the larger model

BerlinModel1 <- glm(Berlin.Sleepiness.Scale ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=RelevantData,family = binomial)
summary(BerlinModel1)
BerlinModel2 <- glm(Berlin.Sleepiness.Scale ~ BMI, data=RelevantData,family = binomial)
summary(BerlinModel2)
NAOmittedBerlinModel1 <- glm(Berlin.Sleepiness.Scale ~ Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid, data=NARemovedRelevantData,family = binomial)
NAOmittedBerlinModel2 <- glm(Berlin.Sleepiness.Scale ~ BMI, data=NARemovedRelevantData,family = binomial)
anova(NAOmittedBerlinModel1,NAOmittedBerlinModel2,test="Chisq")

PartiallyReducedRelevantData <- na.omit(RelevantData[c("BMI","Liver.Diagnosis","Corticoid","Recurrence.of.disease","Depression","Sleep_Disturbance")])
CumulativeModel <- glm(Sleep_Disturbance ~ BMI+Liver.Diagnosis+Corticoid+Recurrence.of.disease+Depression, data=PartiallyReducedRelevantData,family = binomial)
summary(CumulativeModel)
ModifiedModel11 <- glm(Sleep_Disturbance ~ Recurrence.of.disease+Depression, data=PartiallyReducedRelevantData,family = binomial)
summary(ModifiedModel11)
ModifiedModel10 <- glm(Sleep_Disturbance ~ BMI+Recurrence.of.disease+Depression, data=PartiallyReducedRelevantData,family = binomial)
summary(ModifiedModel10)
anova(CumulativeModel,ModifiedModel11,test="Chisq")
anova(CumulativeModel,ModifiedModel10,test="Chisq")
anova(ModifiedModel11,ModifiedModel10,test="Chisq")

#' SUMMARY
#' Argument for Model 11 given it has the fewest parameters and explains a comparable amount of variation for the other models
#' If not model 11, then model 10 may be an appropriate choice as it includes BMI, which can be considered clinically relevant
#' The anova test between model 10 and 11 indicates no real difference in explanatory variation

# Testing the model
library(survey) #perform the Wald Test
# Model 1
regTermTest(model1, "Gender") #p= 0.50433 
regTermTest(model1, "Age") #p= 0.95265
regTermTest(model1, "BMI") #p= 0.1457
regTermTest(model1, "Time.from.transplant") #p= 0.33853
regTermTest(model1, "Liver.Diagnosis") #p= 0.60299
regTermTest(model1, "Recurrence.of.disease") #p= 0.0084884 <- significant#######
regTermTest(model1, "Rejection.graft.dysfunction") #p= 0.86507
regTermTest(model1, "Any.fibrosis") #p= 0.4399
regTermTest(model1, "Renal.Failure") #p= 0.98911
regTermTest(model1, "Depression") #p= 0.052629 <- almost significant############
regTermTest(model1, "Corticoid") #p= 0.73567

#EpworthModel1
regTermTest(EpworthModel1, "Gender") #p= 0.47833 
regTermTest(EpworthModel1, "Age") #p= 0.71654
regTermTest(EpworthModel1, "BMI") #p= 0.63327
regTermTest(EpworthModel1, "Time.from.transplant") #p= 0.89123
regTermTest(EpworthModel1, "Liver.Diagnosis") #p= 0.16057
regTermTest(EpworthModel1, "Recurrence.of.disease") #p= 0.98683
regTermTest(EpworthModel1, "Rejection.graft.dysfunction") #p= 0.14871
regTermTest(EpworthModel1, "Any.fibrosis") #p= 0.75146
regTermTest(EpworthModel1, "Renal.Failure") #p= 0.98349
regTermTest(EpworthModel1, "Depression") #p= 0.16182 
regTermTest(EpworthModel1, "Corticoid") #p= 0.035536 <- significant#############

# PittsburghModel1
regTermTest(PittsburghModel1, "Gender") #p= 0.17023 
regTermTest(PittsburghModel1, "Age") #p= 0.079494
regTermTest(PittsburghModel1, "BMI") #p= 0.027877 <- significant################
regTermTest(PittsburghModel1, "Time.from.transplant") #p= 0.55805
regTermTest(PittsburghModel1, "Liver.Diagnosis") #p= 0.25924
regTermTest(PittsburghModel1, "Recurrence.of.disease") #p= 0.017526 <- significant
regTermTest(PittsburghModel1, "Rejection.graft.dysfunction") #p= 0.98183
regTermTest(PittsburghModel1, "Any.fibrosis") #p= 0.72516
regTermTest(PittsburghModel1, "Renal.Failure") #p= 0.084025
regTermTest(PittsburghModel1, "Depression") #p= 0.084025 
regTermTest(PittsburghModel1, "Corticoid") #p= 0.15107

# AthensModel1
regTermTest(AthensModel1, "Gender") #p= 0.77682 
regTermTest(AthensModel1, "Age") #p= 0.25239
regTermTest(AthensModel1, "BMI") #p= 0.7904
regTermTest(AthensModel1, "Time.from.transplant") #p= 0.23713
regTermTest(AthensModel1, "Liver.Diagnosis") #p= 0.25618
regTermTest(AthensModel1, "Recurrence.of.disease") #p= 0.039342 <- significant##
regTermTest(AthensModel1, "Rejection.graft.dysfunction") #p= 0.25025
regTermTest(AthensModel1, "Any.fibrosis") #p= 0.50026
regTermTest(AthensModel1, "Renal.Failure") #p= 0.40938
regTermTest(AthensModel1, "Depression") #p= 0.015157 <- significant#############
regTermTest(AthensModel1, "Corticoid") #p= 0.23401

# BerlinModel1
regTermTest(BerlinModel1, "Gender") #p= 0.73623 
regTermTest(BerlinModel1, "Age") #p= 0.30793
regTermTest(BerlinModel1, "BMI") #p= 3.3809e-7 <- significant###################
regTermTest(BerlinModel1, "Time.from.transplant") #p= 0.44244
regTermTest(BerlinModel1, "Liver.Diagnosis") #p= 0.70132
regTermTest(BerlinModel1, "Recurrence.of.disease") #p= 0.070461
regTermTest(BerlinModel1, "Rejection.graft.dysfunction") #p= 0.62609
regTermTest(BerlinModel1, "Any.fibrosis") #p= 0.462
regTermTest(BerlinModel1, "Renal.Failure") #p= 0.98336
regTermTest(BerlinModel1, "Depression") #p= 0.85641
regTermTest(BerlinModel1, "Corticoid") #p= 0.74349

pR2(model1) #McFadden result of 0.1033430, the highest of any of the models, indicates poor predictive power for the models
pR2(model10) #McFadden result of 0.07342024, approximately equal to the other models
pR2(model11) #McFadden result of 0.06264110, approximately equal to the other models

# create a subset of the data
set.seed(123)
RelevantDataSubsetIndices <- sample(1:nrow(RelevantData),nrow(RelevantData)/2)
RelevantDataSubset <- RelevantData[RelevantDataSubsetIndices,]
RemainingDataSubset <- RelevantData[-RelevantDataSubsetIndices,]
RemainingDataSubsetRelevantVariables <- RemainingDataSubset[,c(3,6,10)]

model10New <- glm(Sleep_Disturbance ~BMI+Recurrence.of.disease+Depression, data=RelevantDataSubset,family = binomial)
PredictedSleepDisturbance <- predict(model10New, newdata = RemainingDataSubsetRelevantVariables, type = "response")
sum(PredictedSleepDisturbance>0.5, na.rm=T) # does not predict any disordered sleep

#-------------------------------------------------------------------------------
# 6. Evaluate the relationship between sleep disturbance and quality of life (Q2)
#    Health Related Quality of Life (QOL) is measured by SF36 PCS and SF36 MCS:
#    -> SF36 PCS measures the physical component of QOL
#    -> SF36 MCS measures the mental component of QOL
#    They both take numeric values, with larger values indicating better QOL
#    Logistic regression due to the binomial nature of the outcome (sleep disturbance)

ggplot(RelevantData,aes(x=SF36.PCS, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #no strong association
ggplot(RelevantData,aes(x=SF36.MCS, y=Sleep_Disturbance)) + geom_jitter(height=0.05,alpha=0.5) #some difference in distribution

NewModel3 <- glm(Sleep_Disturbance ~ SF36.PCS+SF36.MCS, data=RelevantData,family = binomial)
summary(NewModel3)
#Null Deviance:	    259.4 
#Residual Deviance: 197.4 
# Thus, the model explains some of the deviance in the data, being the two predictors that explain the most deviance when compared to predictors such as Age or BMI
AIC(NewModel3) # AIC = 203.3916
pR2(NewModel3) #McFadden value = 0.2391312


#Linear model with QOL serving as the response
# Physical
SF36.PCSModel1 <- lm(SF36.PCS~Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid+Epworth.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score+Athens.Insomnia.Scale+Berlin.Sleepiness.Scale,RelevantData)
summary(SF36.PCSModel1)
vif(SF36.PCSModel1) #all below 5
pR2(SF36.PCSModel1) #McFadden value = 0.0426612

SF36.PCSModel2 <- lm(SF36.PCS~Recurrence.of.disease+Epworth.Sleepiness.Scale+Athens.Insomnia.Scale,RelevantData)
summary(SF36.PCSModel2)
pR2(SF36.PCSModel2) #McFadden value = 0.02730669

# Further analysis on SF36 PCS simpler model 
# Calculate Pearson's r for simpler model
# Remove NAs so observed and fitted data are the same length
SF36PCS_cleandata <- na.omit(RelevantData[, c("SF36.PCS", "Recurrence.of.disease", "Epworth.Sleepiness.Scale", "Athens.Insomnia.Scale")])

# Fit model with clean data
SF36.PCSModel2_clean <- lm(SF36.PCS ~ Recurrence.of.disease + Epworth.Sleepiness.Scale + Athens.Insomnia.Scale, data = SF36PCS_cleandata)

# Calculate observed and fitted data
observed_valuesPCS <- SF36PCS_cleandata$SF36.PCS
fitted_valuesPCS <- SF36.PCSModel2_clean$fitted.values

# Calculate Pearson's correlation coefficient
SF36PCS_pearson_r <- cor(observed_valuesPCS, fitted_valuesPCS)
SF36PCS_pearson_r

# Create residual plot for the model
plot(fitted_valuesPCS, resid(SF36.PCSModel2_clean),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# QQ plot to assess residuals for normality
qqnorm(resid(SF36.PCSModel2_clean), main = "Q-Q Plot of Residuals")
qqline(resid(SF36.PCSModel2_clean), col = "red")

---
# Mental
SF36.MCSModel1 <- lm(SF36.MCS~Gender+Age+BMI+Time.from.transplant+Liver.Diagnosis+Recurrence.of.disease+Rejection.graft.dysfunction+Any.fibrosis+Renal.Failure+Depression+Corticoid+Epworth.Sleepiness.Scale+Pittsburgh.Sleep.Quality.Index.Score+Athens.Insomnia.Scale+Berlin.Sleepiness.Scale,RelevantData)
summary(SF36.MCSModel1)
vif(SF36.MCSModel1) #all below 5
pR2(SF36.MCSModel1) #McFadden value = 0.08239649

SF36.MCSModel2 <- lm(SF36.MCS~Age+Depression+Pittsburgh.Sleep.Quality.Index.Score,RelevantData)
summary(SF36.PCSModel2)
pR2(SF36.MCSModel2) #McFadden value = 0.06826574

# Further analysis on SF36 MCS simpler model 
# Calculate Pearson's r for simpler model
# Remove NAs so observed and fitted data are the same length
SF36MCS_cleandata <- na.omit(RelevantData[, c("SF36.MCS", "Age", "Depression", 
                                              "Pittsburgh.Sleep.Quality.Index.Score")])

# Fit model with clean data
SF36.MCSModel2_clean <- lm(SF36.MCS ~ Age + Depression + Pittsburgh.Sleep.Quality.Index.Score, data = SF36MCS_cleandata)

# Calculate observed and fitted data
observed_valuesMCS <- SF36MCS_cleandata$SF36.MCS
fitted_valuesMCS <- SF36.MCSModel2_clean$fitted.values

# Calculate pearson's R
SF36MCS_pearson_r <- cor(observed_valuesMCS, fitted_valuesMCS)
SF36MCS_pearson_r

# Create residual plot for simpler model
plot(SF36.MCSModel2_clean$fitted.values, resid(SF36.MCSModel2_clean),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")

# QQplot to assess for normality 
qqnorm(resid(SF36.MCSModel2_clean), main = "Q-Q Plot of Residuals")
qqline(resid(SF36.MCSModel2_clean), col = "red")

