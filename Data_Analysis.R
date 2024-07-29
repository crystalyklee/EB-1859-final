# #Questions to Answer
# 1. What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
# 2. What is the impact and relationship of sleep disturbance with health related quality of life in these patients
# 
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
RelevantData$Berlin.Sleepiness.Scale <- factor(RelevantData$Berlin.Sleepiness.Scale,levels = c(1,0), labels = c("high likelihood of sleep disordered breathing","low likelihood of sleep disordered breathing"))

summary(RelevantData)

# 4. Estimate the prevalence of sleep disturbance (Q1)
#    Need to examine the various sleep scores - literature review
#    -> Berlin.Sleepiness.Scale is binary (value of 1 indicates disturbance)
#    -> Pittsburgh Sleep Quality Index (PSQI), the Epworth sleepiness scale (ESS),
#       and the Athens insomnia scale (AIS) are numeric with higher values
#       indicating worse sleep.
#    Collate these different measures into a new column of the data frame using
#    the mutate function


# 5. Identify predictors that are associated with sleep disturbance (Q1)
#    Logistic regression due to the binomial nature of the outcome (sleep disturbance)
#    True predictors will have larger coefficients and smaller p-values
#    Can compare different suites of predictors using ANOVAs with nested models

# 6. Evaluate the relationship between sleep disturbance and quality of life (Q2)
#    Health Related Quality of Life (QOL) is measured by SF36 PCS and SF36 MCS:
#    -> SF36 PCS measures the physical component of QOL
#    -> SF36 MCS measures the mental component of QOL
#    They both take numeric values, with larger values indicating better QOL
#    Logistic regression due to the binomial nature of the outcome (sleep disturbance)

# 7. Write up the answers to Q1 and Q2

# 8. Create presentation on the answers to Q1 and Q2
