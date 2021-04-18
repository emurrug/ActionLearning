# This will give me a basic breakdown of my study's demographics (i.e. age, sex, race, number of participants).

library (readr)
library (kableExtra)
library (pastecs)
library (dplyr)
library (ggplot2)
library(psych)

#### IMPORTING DATA ####
demo <- "~/Documents/Most/SLA_Study/GitHub/ActionLearning/Data/SLA_Official/Concatenated/Demographics_All.csv"
dfdemo <- read_csv(demo)

dfdemo<-dfdemo[!(dfdemo$Finished=="0"),]

### LABELING DATA ###
# Variable checks:
str(dfdemo)

is.factor(dfdemo$variable)
is.numeric(dfdemo$variable)

dfdemo$Statistical_Organization <- as.factor(dfdemo$Statistical_Organization)
dfdemo$Finished <- as.factor(dfdemo$Finished)
dfdemo$Survey <- as.factor(dfdemo$Survey)
dfdemo$Condition <- as.factor(dfdemo$Condition)
dfdemo$Education <- as.factor(dfdemo$Education)
dfdemo$Gender <- as.factor(dfdemo$Gender)
dfdemo$Race <- as.factor(dfdemo$Race)

# Not sure if I want to keep this change.
dfdemo$Birth_Year <- as.numeric(dfdemo$Birth_Year)

str(dfdemo)

#### DESCRIPTIVE STATISTICS, VISUALISING THE DATA ####
# Gives basic totals for different variables.
summary(dfdemo)
summary(dfdemo$Race)
summary(dfdemo$Education)
summary(dfdemo$Gender)
summary(dfdemo$Occupation)

# Gives basic totals for the data set, sorting by a particular variable.
by(dfdemo, dfdemo$Statistical_Organization, summary)
by(dfdemo, dfdemo$Condition, summary)

table(dfdemo$Statistical_Organization, dfdemo$Condition)

# Gives descriptive statistics for each variable. Can do this by group.
describe(dfdemo)
describeBy(dfdemo, dfdemo$Condition)
describeBy(dfdemo, dfdemo$Statistical_Organization)
