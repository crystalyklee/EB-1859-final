plan
another plan 

#Data
1. Sleep data found in "project_data.csv"
2. Key variables and data definitions in "Data_dictionary.docx"

#Questions to Answer
1. What is the prevalence of subjective sleep disturbance and what are its predictors in post liver transplant recipients
2. What is the impact and relationship of sleep disturbance with health related quality of life in these patients

# Steps
1. Assign roles, including code reviewer
2. Clean data, focus on relevant variables
   NAs appear to be listed
3. Describe relevant data: some are categorical, some are numeric
   Can use table functions, convert others to factors
4. Estimate the prevalence of sleep disturbance (Q1)
   Need to examine the various sleep scores - literature review
   -> Berlin.Sleepiness.Scale is binary (value of 1 indicates disturbance)
   -> Pittsburgh Sleep Quality Index (PSQI), the Epworth sleepiness scale (ESS),
   and the Athens insomnia scale (AIS) are numeric with higher values
   indicating worse sleep.
   Collate these different measures into a new column of the data frame using
   the mutate function
5. Identify predictors that are associated with sleep disturbance (Q1)
   Logistic regression due to the binomial nature of the outcome (sleep   
   disturbance)
   True predictors will have larger coefficients and smaller p-values
   Can compare different suites of predictors using ANOVAs with nested models
6. Evaluate the relationship between sleep disturbance and quality of life (Q2)
   Health Related Quality of Life (QOL) is measured by SF36 PCS and SF36 MCS:
   -> SF36 PCS measures the physical component of QOL
   -> SF36 MCS measures the mental component of QOL
   They both take numeric values, with larger values indicating better QOL
   Logistic regression due to the binomial nature of the outcome (sleep 
   disturbance)
7. Write up the answers to Q1 and Q2
8. Create presentation on the answers to Q1 and Q2
