# MC: I'm creating this as a slightly more abbreviated file than statistical_action_learning_script.R.
# I hope to consolidate the most relevant code for Likert scores here, and will probably be doing cleaning and
# editing in this file from now on. (30 March 2021)

# The first two sections are exactly the same as sla_script_tests.R.

#### LOADING PACKAGES ####
library(readr)   #reading CSV's
library(kableExtra)   #formatting table
library(pastecs) #describes data
library(dplyr)   #manipulating data to view it
library(ggplot2) #visualizing data with graphs (we don't talk about the first "ggplot")
library(emmeans) #post hoc analyses (contrasts!)
library(magrittr) #not sure if I need this one?

#### IMPORTING & LABELLING DATA ####
# Import the file directly from Github:
myfile <- "~/Documents/Most/SLA_Study/GitHub/ActionLearning/Data/SLA_Official/Concatenated/SLA_Data_All.csv"

# Create dataframe from file:
df <- read_csv(myfile)

# Getting rid of NA's:
df <- na.omit(df) #removes rows with NAs in them
df<-df[!(df$Finished=="0"),] #removes rows based on unfinished surveys (not NAs)


### LABELLING DATA ###

# Check how all of your variables are labeled:
str(df)

# Formula for checking individual variables:
is.factor(df$variable) #OR
is.numeric(df$variable)

# Formula for changing the variable type permanently, in case R is reading a variable as the incorrect type:
df$variable <- as.factor(df$variable) #to make it categorical
df$variable <- as.numeric(df$variable) #to make it continuous

# Convert all "character" columns to factors at once:
df <- df %>% mutate_if(is.character,as.factor)

# Then convert select columns to numeric, so you can take averages:
num.columns <- c('T-1_Likert', 'T-2_Likert', 'T-3_Likert', 'T-4_Likert', 'UT-1_Likert', 'UT-2_Likert', 'UT-3_Likert', 'UT-4_Likert')
num.columns <- c('T-1_Recognition', 'T-2_Recognition', 'T-3_Recognition', 'T-4_Recognition', 'UT-1_Recognition', 'UT-2_Recognition', 'UT-3_Recognition', 'UT-4_Recognition')
num.columns <- c('T-1c_Likert', 'T-2c_Likert', 'T-3c_Likert', 'T-4c_Likert', 'Correct_Mvt_T-1', 'Correct_Mvt_T-2', 'Correct_Mvt_T-3', 'Correct_Mvt_T-4')
df[num.columns] <- sapply(df[num.columns], as.numeric)

# Then convert Statistical_Organization to a factor:
df$Statistical_Organization <- as.factor(df$Statistical_Organization)
df$Condition <- as.factor(df$Condition)
df$Finished <- as.factor(df$Finished)
df$Survey <- as.factor(df$Survey)

# Check that it looks good:
str(df)

# The original sheet deals with creating new columns in R.
# I'm just creating them in the sheet because it's more efficient for where I'm at.

#### DESCRIPTIVE STATISTICS, VISUALISING THE DATA ####
# Get some initial stats to make sure the numbers make sense:
stat.desc(df$variable)

# Check your residual distributions to make sure you aren't violating any statistical assumptions
# (e.g. normality, linearity, homogeneity, etc.).

# Let's visualize the shape of the data: 
ggplot(df, aes(x = df$variable, na.rm = TRUE)) + #specifies your x,y of interest (no Y here since it is a histogram)
  geom_bar(color = "black", fill = "white", stat = "count") #tinker with color and size

# How are the variables correlated with one another? Haven't used this one yet, but included it just in case.
> pairs(~var1+var2+var3+var4+varetc,data=df, + main="Simple Scatterplot Matrix")

# Histogram of the distribution of summed confidence scores:
confidence_hist <- ggplot(df, aes(x = `Total_Likert`, na.rm = TRUE))
confidence_hist + geom_bar(color = "black", fill = "white", stat = "count")

# Histogram of the distribution of confidence for familiar trigrams, comprehension:
confidence_f_hist <- ggplot(df, aes(x = `Familiar_Likert`, na.rm = TRUE))
confidence_f_hist + geom_bar(color = "black", fill = "white", stat = "count")

# Histogram of the distribution of confidence for unfamiliar trigrams, comprehension:
confidence_u_hist <- ggplot(df, aes(x = `Unfamiliar_Likert`, na.rm = TRUE))
confidence_u_hist + geom_bar(color = "black", fill = "white", stat = "count")

# Do some people always pick the same answer?
# The analysis above mostly answers that question, I just have to calculate means for Recognition questions, too.
# ggplot(df, aes(x = `RECOGNITIONMEAN`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")


# In which of the six conditions were the participants the most confident?

# Were participants more confident in the statistical learning vs. random condition? SL = 1, R = 0
# Look for effects in both comprehension and production questions.

# Histogram of the distribution of summed confidence scores, color-coded (SL vs. R):
ggplot(df, aes(Total_Likert, fill = `Statistical_Organization`)) + geom_histogram(binwidth = 1, position = dodge)


# Bar graph showing the averages of the total Likert scores for R vs. SL:
confidence_condition_bar <- ggplot(df, aes(`Statistical_Organization`, `Total_Likert`))
confidence_condition_bar + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "Red")

# Game plan: find a way to differentiate between SL and R, then do both histograms and correlative measures.

# Were participants more confident in the action vs. stationary (vs. control) condition?
# Look for effects in both comprehension and production questions.
# Pay special attention to how the control condition compares to the action condition. Are they significantly different?
  


#### ANOVA TESTS ####
# Formula for the ANOVA test: 
model <- aov(df$DV ~ df$IV1 * df$IV2)
summary(model)

# The asterisk here is testing for an interaction effect. If you only want to look at main effects without
# the interaction, you should add a "+" instead. 

# Is there a correlation between confidence and correctness?
# That is, were people more confident when they were correct?

# ANOVA test for whether confidence predicts correctness on production.
# THIS IS A BETTER TEST BECAUSE CORRECTNESS IS DOING ALL THE PREDICTIVE WORK (AS EVIDENCED BY THE TEST BELOW)
model_productionL <- aov(df$Mvt_Likert ~ df$Correct_Mvt_Scores)
summary(model_productionL)

# ANOVA test for the interaction between correctness and condition to predict confidence:
model_productionLCC <- aov(df$Mvt_Likert ~ df$Correct_Mvt_Scores * df$Condition)
summary(model_productionLCC)

# Same ANOVA test with + (instead of *). HAVEN'T UPDATED YET -- WAITING TO SEE IF THE ABOVE CODE IS RIGHT
model_productionL2 <- aov(df$Correct_Mvt_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_production2)


# ANOVA test for whether confidence predicts correctness on comprehension.
model_comprehensionL <- aov(df$Total_Likert ~ df$Correct_Trigram_Scores)
summary(model_comprehensionL)

# Same ANOVA test with + (instead of *). HAVEN'T UPDATED YET -- WAITING TO SEE IF THE ABOVE CODE IS RIGHT
model_comprehensionL2 <- aov(df$Correct_Trigram_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_comprehension2)


# Scatterplot for comprehension and confidence -- HAVEN'T UPDATED YET FROM EXPERIENCE CODE
# experience_comprehension <- ggplot(df2, aes(df2$Experience_Total, df$Correct_Trigram_Scores))
# experience_comprehension + stat_summary(geom = "point")


# This is interesting by I haven't figured out wtf this means...
model_productionLi <- aov(df$Mvt_Likert ~ df$Statistical_Organization * df$Condition)
summary(model_productionLi)


#### POST HOC ANALYSES ####
# Post hoc analysis for comprehension scores (RENAME THIS MODEL):
model_comprehension_ph.emm.s <- emmeans(model_comprehension2, "Condition")
pairs(model_comprehensions_ph.emm.s)

# The linear regression test (notice the similarity to the ANOVA - there's a theoretical reason for that!):
model2 <- lm(df$DV ~ df$IV1 * df$IV2)
summary(model2)

### FOLLOW UP ANALYSES ####
# Does movement experience predict performance? Doesn't look like it (see Roam).
# Regression for production as predicted by experience:
model_experience_production <- lm(df2$Correct_Mvt_Scores ~ df2$Experience_Total)
summary(model_experience_production)

# Scatterplot for production and experience:
experience_production <- ggplot(df2, aes(df2$Experience_Total, df$Correct_Mvt_Scores))
experience_production + stat_summary(geom = "point")


# Does movement experience predict comprehension? Doesn't look like it (see Roam).
# Regression for comprehension as predicted by experience:
model_experience_comprehension <- lm(df2$Correct_Trigram_Scores ~ df2$Experience_Total)
summary(model_experience_comprehension)

# Scatterplot for comprehension and experience:
experience_comprehension <- ggplot(df2, aes(df2$Experience_Total, df$Correct_Trigram_Scores))
experience_comprehension + stat_summary(geom = "point")

#### GRAPHING ####
# Template for making a bar graph:
# myGraph <- ggplot(myData, aes(variable for x axis, variable for y axis, fill = independent variable))

# Actual code for bar graph with action conditions (x) and production scores (y):
barProduction <- ggplot(df, aes(Condition, Correct_Mvt_Scores, fill = `Statistical_Organization`))
barProduction + stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Action Conditions", y = "Production Score", fill = df$`Statistical_Organization`) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)

# Here's the same bar graph with action conditions (x) and comprehension scores (y):
barComprehension <- ggplot(df, aes(Condition, Correct_Trigram_Scores, fill = `Statistical_Organization`))
barComprehension + stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Action Conditions", y = "Comprehension Score", fill = df$`Statistical_Organization`) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)