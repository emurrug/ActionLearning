# MC: I'm creating this as a slightly more abbreviated file than statistical_action_learning_script.R.
# I hope to consolidate the most relevant comprehension and productions tests code here, and will probably be
# doing cleaning and editing in this file from now on. (4 March 2021)

# The first two sections are exactly the same as sla_script_likert.R.

#### LOADING PACKAGES ####
library(readr)   #reading CSV's
library(kableExtra)   #formatting table
library(pastecs) #describes data
library(dplyr)   #manipulating data to view it
library(ggplot2) #visualizing data with graphs (we don't talk about the first "ggplot")
library(ggsignif) #for significance asterisks on graphs
library(ggpub) #for significance asterisks on graphs
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

# Then convert Statistical_Organization, Condition, Finished, Survey to a factor:
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

# To visualize the distribution of confidence, I'll plot MeanLikert on a histogram.
ggplot(df, aes(x = `MeanLikert`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")

# Do some people always pick the same answer?
# The analysis above mostly answers that question, I just have to calculate means for Recognition questions, too.
# ggplot(df, aes(x = `RECOGNITIONMEAN`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")

#### ANOVA TESTS ####
# Formula for the ANOVA test: 
model <- aov(df$DV ~ df$IV1 * df$IV2)
summary(model)

# The asterisk here is testing for an interaction effect. If you only want to look at main effects without
# the interaction, you should add a "+" instead. 

# ANOVA test for the interaction between Statistical_Organization and action condition on performance.
model_production <- aov(df$Correct_Mvt_Scores ~ df$Statistical_Organization * df$Condition)
summary(model_production)

# Same ANOVA test with + (instead of *).
model_production2 <- aov(df$Correct_Mvt_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_production2)

# ANOVA test for the interaction between Statistical_Organization and action condition on comprehension.
model_comprehension <- aov(df$Correct_Trigram_Scores ~ df$Statistical_Organization * df$Condition)
summary(model_comprehension)

# Same ANOVA test with + (instead of *).
model_comprehension2 <- aov(df$Correct_Trigram_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_comprehension2)

#### POST HOC ANALYSES ####
# Pairwise analysis for comprehension scores by condition:
model_comprehension_ph <- emmeans(model_comprehension2, "Condition")
pairs(model_comprehensions_ph)

# Do I need confidence intervals? I'm not even sure what these would be for...
model_comprehension_ph %>% confint()
model_comprehension_ph %>% summary(infer = TRUE)

# Pairwise analysis for production scores by condition:
model_production_ph <- emmeans(model_production2, "Condition")
pairs(model_production_ph)

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


# My attempt at testing whether comprehension accuracy predicted production accuracy:
model_comp_prod <- lm(df$Correct_Mvt_Scores ~ df$Correct_Trigram_Scores)
summary(model_comp_prod)

# Scatterplot for comprehension and production:
comp_prod <- ggplot(df, aes(df$Correct_Trigram_Scores, df$Correct_Mvt_Scores))
comp_prod + geom_point()


#### GRAPHING ####
# Template for making a bar graph:
# myGraph <- ggplot(myData, aes(variable for x axis, variable for y axis, fill = independent variable))

# Actual code for bar graph with action conditions (x) and production scores (y):
barProduction <- ggplot(df, aes(x = Statistical_Organization, y = Correct_Mvt_Scores))
barProduction + stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "chartreuse4") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) +
  labs(x = "Statistical Organization", y = "Production Score", title = "Production") +
  scale_x_discrete(breaks=c("0","1"), labels=c("Random", "Statistical Learning")) +
  stat_compare_means(comparisons = list(c("0", "1")), aes(label = ..p.signif..), label.y = 5.5, method = "t.test") +
  theme_classic()

# Used to have:
#barProduction <- ggplot(df, aes(Condition, Correct_Mvt_Scores, fill = Statistical_Organization))
#barProduction + stat_summary(fun = mean, geom = "bar", position = "dodge") +
#  labs(x = "Action Conditions", y = "Production Score", fill = df$Statistical_Organization) +
#  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)

# Here's the same bar graph with action conditions (x) and comprehension scores (y):
barComprehension <- ggplot(df, aes(x = Statistical_Organization, y = Correct_Trigram_Scores))
barComprehension + stat_summary(fun = mean, geom = "bar", position = "dodge", fill = "steelblue3") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) +
  labs(x = "Statistical Organization", y = "Comprehension Score", title = "Comprehension") +
  scale_x_discrete(breaks=c("0","1"), labels=c("Random", "Statistical Learning")) +
  stat_compare_means(comparisons = list(c("0", "1")), aes(label = ..p.signif..), label.y = 5.5, method = "t.test") +
  theme_classic()

# Used to have:
#barComprehension <- ggplot(df, aes(Condition, Correct_Trigram_Scores, fill = Statistical_Organization))
#barComprehension + stat_summary(fun = mean, geom = "bar", position = "dodge") +
#  labs(x = "Action Conditions", y = "Comprehension Score", fill = df$Statistical_Organization) +
#  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)

# For significance asterisk thingies:
geom_signif(comparisons = list(c("versicolor", "virginica")), 
            map_signif_level=TRUE)
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons, label.y = c(29, 35, 40))+
  stat_compare_means(label.y = 45)


# Distribution of accuracy on comprehension:
comp_distr <- hist(df$Correct_Trigram_Scores)

# Distribution of accuracy on production:
prod_distr <- hist(df$Correct_Mvt_Scores)
