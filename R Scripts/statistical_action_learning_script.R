#### READ ME ####

#08.01.2020

#This file is meant to serve as a collaborative introduction to using R for 
#Mica's honor's thesis on Statistical Action Learning. The people writing this code are: 
#Mica Carroll (mbc225@cornell.edu) and Emma Murrugarra (eam422@cornell.edu). 

#To make life easier, there will be a lot of notes in this file. 
#This will include theoretical notes about this particular project, practical notes
#about using R, and some content/resources about statistical analysis methods broadly. 

#All the files related to this project are currently being privately hosted on Github
# at https://github.com/emurrug/ActionLearning.git. 
#I recommend that you download GitHub Desktop and work directly from the R files 
#already in the GitHub repository (rather than downloading/re-uploading).



#### PROJECT OUTLINE (bird's eye view) ####

##What are the independent variables (IVs)?##
#Action learning [AL] (Action vs. Control; categorical binary)
#Statistical Regularity [SR] (Statistical vs Random; Categorical binary)


##What are the dependent variables (DVs)?##
#Recognition of correct pattern (Categorical binary)
#Confidence in recognition (Continuous; likert 1-7)
#Recall of correct pattern (Categorical binary)
#Confidence of recall (Continuous likert 1-7)


##How does the study design impact the test?##
#The project was ran in a 2x2 design of statistical patterns stimuli X action learning.
#Each participant did 6 trials. 
#Other relevant demos collected were education, gender, age, race, and experience with action

##Questions##
#How does AL and SR predict recognition?
#How does AL and SR predict recall? 
#Do AL and SR interact to predict recognition/recall?
#Does confidence correlate with correct recall/recognition?
#Do the demographic variables impact prediction of our DVs?


#### GETTING STARTED ####

#First thing's first, if you are using RStudio (which you better be)
#look at the line above the console and below the script. It should have the
#section titled (e.g. "GETTING STARTED"). Click on this. This should help you 
#scroll through sections faster. You can also collapse sections that you don't want to see.

#The RStudio window has a lot of features that you should take the time to explore. 
#Also if you ever have any doubt about new code, go to the documentation! 
#The easiest way to do this is to type "?" in front of a new function (e.g. "?dplyr")



#First, let's import any relevant packages we will use for our study. 
#That way we don't have to write new functions for all of our needs!
#The first time you use the package, you will have to use the function "install.packages("PackageName")"
#After that, just call the package with the "library" function

library (readr)   #reading CSV's
library (kableExtra)   #formatting table
library (pastecs) #describes data
library (dplyr)   #manipulating data to view it
library (ggplot2) #visualizing data with graphs (we don't talk about the first "ggplot")




#### IMPORTING DATA ####

#The benefit to using file sharing accounts like GitHub or Dropbox is that everyone
#has access to the most updated CSV of the data. In R, you are also able to import CSV files directly from github:

myfile <- "~/Documents/Most/-SLA_Study/GitHub/ActionLearning/Data/SLA Official Data/Concatenated Data/SLA Data All.csv"
#You can also import the file directly from your computer. To do this, you just put your path
#directory instead of a URL. 
#Changes to sheet:
#Created "Survey" column to keep track of which survey each data point is from (possible inputs: RS-1, 2, 3; SLS-1, 2, 3).
#Created "Statistical_Organization" column (possible inputs: random = 0, statistical = 1).
#Created "Correct_Mvt_Scores" columns. Each box is the sum of each participant's performance scores.
#Changed condition column values to factor (possible inputs: action = 0, stationary = 1, control = 2).
#Deleted all "Date" columns.
#Renamed columns so that they were not specific to any of the six surveys.
df <- read_csv(myfile)
# df <- df[-1,] #EM: removes the first row of the data (since this is a Qualtrics file w/ two headers)
#MC: I manually removed the second header row so this isn't needed.

# "df" is a standard shorthand for "dataframe"

#to view your first few lines you do 
head(df)
#or to view the whole table at once 
View(df)



#### CLEANING & VISUALIZING DATA ####

#I don't care if it's just a quick peek, you should always do these steps!


### LABELLING YOUR VARIABLES

df <- na.omit(df) #EM: removes rows with NAs in them. 
#If you want to remove them based on unfinished surveys (not NAs), I recommend this instead: 
df<-df[!(df$Finished=="0"),]


#You remember NOIR? (Nominal, Ordinal, Interval, Ratio)
#The first step is to make sure that your variables are labelled appropriately.
#R will try to do some of this automatically, but a mislabelled variable can lead 
#to very erroneous and confusing results.
#Here's one way to check how all of your variables are labelled: 

str(df)

#to look at individual variables you can type in something like
is.factor(df$variable) #OR
is.numeric(df$variable)

#to change the variable type permanently, in case R is reading a variable as the incorrect type
df$variable <- as.factor(df$variable) #to make it categorical
df$variable <- as.numeric(df$variable) #to make it continuous

#EM: You'll notice this dataset is labelled as all characters. To make our lives easier, let's go ahead
#and convert all the "character" columns into factors all at once (rather than one at a time)

df <- df %>% mutate_if(is.character,as.factor)

#EM: if you want to take the Likert scales and make these numeric (so you can get an average, for example)
#you can select specific columns and make only these numeric
num.columns <- c('T-1_Likert', 'T-2_Likert', 'T-3_Likert', 'T-4_Likert', 'UT-1_Likert', 'UT-2_Likert', 'UT-3_Likert', 'UT-4_Likert')
num.columns <- c('T-1_Recognition', 'T-2_Recognition', 'T-3_Recognition', 'T-4_Recognition', 'UT-1_Recognition', 'UT-2_Recognition', 'UT-3_Recognition', 'UT-4_Recognition')
num.columns <- c('T-1c_Likert', 'T-2c_Likert', 'T-3c_Likert', 'T-4c_Likert', 'Correct_Mvt_T-1', 'Correct_Mvt_T-2', 'Correct_Mvt_T-3', 'Correct_Mvt_T-4')
df[num.columns] <- sapply(df[num.columns], as.numeric)

df$Finished <- as.factor(df$Finished)
df$`Statistical_Organization` <- as.factor(df$`Statistical_Organization`)
df$Condition <- as.factor(df$Condition)
df$Statistical_Organization <- df$`Statistical Organization`

#always double check it looks good: 
str(df)


### CREATING NEW VARIABLES

#EM: Lets say we want to take the average several related variables, and create a new column from this
#there's so many ways to do this, but lets go ahead and keep using dplyr.
#here is a good resource for examples of how to create/modify variables https://dplyr.tidyverse.org/reference/mutate.html

#Familiar vs. unfamiliar trigrams: calculates means of Likert scores for each trigram by row and
#creates one new column for the familiar means, one for unfamiliar means.

df <- df %>% 
  rowwise() %>%
  mutate(MeanLikertT = mean(c(`T-1_Likert_1`, `T-2_Likert_1`, `T-3_Likert_1`,`T-4_Likert_1`)))

df <- df %>% 
  rowwise() %>%
  mutate(MeanLikertUT = mean(c(`UT-1_Likert_1`, `UT-2_Likert_1`, `UT-3_Likert_1`,`UT-4_Likert_1`)))


#Statistical learning vs. random stimuli:
  #This chunk differentiated between the random (0) and SL (1) stimuli by creating a new column.
df <- df %>%
  mutate(Paradigm = recode(condition, "active - RS-1" = 0, "passive - RS-2" = 0,
                           "passive - RS-3" = 0, "active - SLS-1" = 1, "active - SLS-2" = 1,
                           "active - SLS-3" = 1, "passive - SLS-2" = 1))
  
  #This chunk created composite scores for random vs. SL Likert scores by first grouping the data by
  #Paradigm (0=random, 1=SL), then finding the mean of all MeanLikertT scores, then all
  #MeanLikertUT scores, still separated by random vs. SL. The output is a 4x4 tibble.
  #The lowercase "p" in the variable names distinguishes between the means for Paradigm vs. Condition.
df %>%
  group_by(Paradigm) %>%
  summarise("MeanTp" = mean(MeanLikertT), "MeanUTp" = mean(MeanLikertUT))


#Active vs. passive stimuli:
  #This chunk differentiated between the passive (0) and active (1) stimuli by creating a new column.
df <- df %>%
  mutate(Condition = recode(df$`condition`, "active - RS-1" = 1, "passive - RS-2" = 0,
                            "passive - RS-3" = 0, "active - SLS-1" = 1, "active - SLS-2" = 1,
                            "active - SLS-3" = 1, "passive - SLS-2" = 0))
  
  #This chunk created composite scores for active vs. passive Likert scores by first grouping the
  #data by Condition (0=passive, 1=active), then finding the mean of all MeanLikertT scores,
  #then all MeanLikertUT scores, still separated by active vs. passive. The output is a 4x4 tibble.
  #The lowercase "c" in the variable names distinguishes between the means for Paradigm vs. Condition.
df %>%
  group_by(Condition) %>%
  summarise("MeanTc" = mean(MeanLikertT), "MeanUTc" = mean(MeanLikertUT))


#Were some people more confident than others? To find out, I've created a new column (MeanLikert)
#that averages MeanLikertT and MeanLikertUT to get an overall score per subject.
df <- df %>% 
  rowwise() %>%
  mutate(MeanLikert = mean(c(`MeanLikertT`, `MeanLikertUT`)))

#To visualize the distribution of confidence, I'll plot MeanLikert on a histogram.
ggplot(df, aes(x = `MeanLikert`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")


#Do some people always pick the same answer? The analysis above mostly answers that question, I just
#have to calculate means for not only Likerts, but Recognition questions, too.
df <- df %>% 
  rowwise() %>%
  mutate(MeanRecognitionT = mean(c(`T-1_Recognition`, `T-2_Recognition`, `T-3_Recognition`, `T-4_Recognition`)))

df <- df %>% 
  rowwise() %>%
  mutate(MeanRecognitionUT = mean(c(`UT-1_Recognition`, `UT-2_Recognition`, `UT-3_Recognition`, `UT-4_Recognition`)))

df <- df %>% 
  rowwise() %>%
  mutate(MeanRecognition = mean(c(`MeanRecognitionT`, `MeanRecognitionUT`)))

#To visualize the distribution of recognition, I'll plot MeanRecognitionT and MeanRecognitionUT.
ggplot(df, aes(x = `MeanRecognitionT`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")

ggplot(df, aes(x = `MeanRecognitionUT`, na.rm = TRUE)) + geom_bar(color = "black", fill = "white", stat = "count")


if('T-1_Recognition' = 1){x = 1}

if('T-1_Recognition' = 2){x = 0}

df <- df %>% 
  rowwise() %>%
  mutate(`T-1_Correct` = x)



### VIEWING DESCRIPTIVES

#The next thing to do is look at the descriptive statistics for your variables (e.g. range, central tendency, & variabilility)
#Make sure that these numbers make sense (e.g., is there an impossible age? is the SD ridiculously high?)
#There are a lot of good ways to do this: https://www.statmethods.net/stats/descriptives.html

stat.desc(df$variable)

#Finally make sure you are checking your residual distributions so that you know if you
#are/aren't violating any statistical assumptions (e.g. normality, linearity, homogeneity, etc.)

#first, let's visualize the shape of the data: 
ggplot(df, aes(x = df$variable, na.rm = TRUE)) + #specifies your x,y of interest (no Y here since it is a histogram)
  geom_bar(color = "black", fill = "white", stat = "count") #tinker with color and size
  
#how are the variables correlated with one another? 
> pairs(~var1+var2+var3+var4+varetc,data=df, +
          main="Simple Scatterplot Matrix")

#now let's check the residuals to see if a linear model is appropriate for testing variables (DVs and IVs of interest)
#generate a model where two variables (X & Y) are correlated (i.e. Y ~ X)
model <-lm (Y~X)

plot(model)
#here is a guide on how to read the plots https://www.theanalysisfactor.com/linear-models-r-diagnosing-regression-model/
#this will tell you if you are okay using general linear models (e.g. ANOVA or regression)


#Follow up tests and other considerations will depend on the shape of your data and the tests you will run..
#I won't go into that here, since there is too much to cover.

### FINDING THE AVERAGES OF ROWS
rowMeans(df)


#### STATISTICAL TECHNIQUES FOR GOALS (theoretical) ####
#Looking back to the PROJECT GOALS, we see that our main IVs and main DVs are mostly categorical. 
#Generally, ANOVAs are best to use when you are interested in comparing means between two or more
#categorical groups. However, binary DVs automatically violate the assumptions needed for a linear model. 
#Therefore we will have to get a bit more creative...

#To run a test where the IV is categorical and the DV is categorical, you will need a 2x2 Chi-Square test. 
#Instead of measuring means, Chi-squares compares the frequency of results (e.g., % of responses that were correct vs. incorrect)
#to the expected frequency of results if it were random. Funny enough, this is exactly what a test of homogenity is.
#You will then have to follow this up with a odds-ratio test to see if there is an interaction 
#(the wikipedia page for "Odds ratio" has a lot of useful examples for reference).

#Note: There is some debate about whether a likert scale item should be treated as a continuous or a categorical variable. 
#I also have opinions here. Therefore, I will include the code to do an ANOVA (treating it categorically) and 
#a linear regression (treating it as continuous). 



#### STATISTICAL ANALYSIS (implmented) ####

#Format for odds-ratio test: 
model1 <- oddsratio(a, b, c, d, conf.level = 0.95, p.calc.by.independence = TRUE)
print(model1)
#a-d corresponds to each of the cells in a 2x2 matrix of action/control X statistical/random.
#the numbers that go in here should be the number of correct responses in either the recall or recognition task

#The ANOVA test: 
model2 <- aov(df$LikertDV ~ df$actionIV * df$statisticalIV)
summary(model2)
# The asterisk here is testing for an interaction effect. If you only want to look at main effects without
# the interaction, you should add a "+" instead. 

#MC: ANOVA test that tests the interaction between Statistical_Organization and action condition on performance.
model_production <- aov(df$Correct_Mvt_Scores ~ df$`Statistical_Organization` * df$Condition)
summary(model_production)
                                            Df Sum Sq Mean Sq F value Pr(>F)  
df$`Statistical_Organization`               1   5.16   5.164   2.949 0.0909 .
df$Condition                                2   2.89   1.444   0.825 0.4431  
df$`Statistical_Organization`:df$Condition  2   0.26   0.130   0.074 0.9285  
Residuals                                  62 108.57   1.751                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MC: ANOVA test with + (instead of *).
model_production2 <- aov(df$Correct_Mvt_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_production2)
                              Df Sum Sq Mean Sq F value Pr(>F)  
df$`Statistical_Organization`  1   5.27   5.266   3.144 0.0809 .
df$Condition                   2   2.84   1.422   0.849 0.4325  
Residuals                     65 108.88   1.675                 
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MC: ANOVA test that tests the interaction between Statistical_Organization and action condition on comprehension.
model_comprehension <- aov(df$Correct_Trigram_Scores ~ df$Statistical_Organization * df$Condition)
summary(model_comprehension)
                                            Df Sum Sq Mean Sq F value  Pr(>F)   
df$`Statistical_Organization`               1  18.37  18.366   9.451 0.00312 **
df$Condition                                2  14.61   7.306   3.760 0.02868 * 
df$`Statistical_Organization`:df$Condition  2   5.81   2.906   1.495 0.23204   
Residuals                                  63 122.43   1.943                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#MC: ANOVA test with + (instead of *).
model_comprehension2 <- aov(df$Correct_Trigram_Scores ~ df$Statistical_Organization + df$Condition)
summary(model_comprehension2)
                              Df Sum Sq Mean Sq F value Pr(>F)   
df$`Statistical_Organization`  1  18.37  18.366   9.309 0.0033 **
df$Condition                   2  14.61   7.306   3.703 0.0300 * 
Residuals                     65 128.24   1.973                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Post hoc analysis for comprehension scores (rename this model):
model_comprehension_ph.emm.s <- emmeans(model_comprehension2, "Condition")
pairs(model_comprehensions_ph.emm.s)

#The linear regression test: 
model3 <- lm(df$LikertDV ~ df$actionIV * df$statisticalIV) #notice the similarity to the ANOVA. There's a theoretical reason for that!
summary(model3)

#Now that you have the basic format of each of the tests that you need, try it out with the demographic variables
#and see if they predict any of the dependent variables of interest.


#### GRAPHING ####
#ggplot2 is a really cool tool because it works by adding layers to an image
#for example, first you add the variables (dictating the scale and axes), then you add the graph type (the pretty part), 
#and then finally you add the details like labels and colors. This means there's a lot of ways to find the graph
#that makes the data easiest to digest to the readers. Focus on your audience when you create a graph -- it's easy
#to get carried away!

#Here is an example I found for odds-ratio (this may or may not be the best method):
#https://stackoverflow.com/questions/47085514/simple-way-to-visualise-odds-ratios-in-r
#here is another package that's based on ggplot and might help: 
#https://finalfit.org/reference/or_plot.html

#MC: used this template for making a bar graph with action conditions (x) and production scores (y)
myGraph <- ggplot(myData, aes(variable for x axis, variable for y axis, fill = independent variable))
barProduction <- ggplot(df, aes(Condition, Correct_Mvt_Scores, fill = `Statistical_Organization`))
barProduction + stat_summary(fun = mean, geom = "bar", position = "dodge") +
labs(x = "Action Conditions", y = "Production Score", fill = df$`Statistical_Organization`) +
stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)

#MC: here's the same bar graph with action conditions (x) and comprehension scores (y)
barComprehension <- ggplot(df, aes(Condition, Correct_Trigram_Scores, fill = `Statistical_Organization`))
barComprehension + stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Action Conditions", y = "Comprehension Score", fill = df$`Statistical_Organization`) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)


#### SUMMARIZING RESULTS####

# APA format! 
# Example: "Memory recognition was significantly predicted by the interaction of action learning and statistical presentation 
# such that individuals who were in the action learning condition were more likely to correctly recognize the next action sequence
# in a trigram, but only when exposed to statistically predictable stimuli, (R^3 = , CI = [], p = )."

# The above statement is pretty standard issue when doing regressions/ANOVAs. However, odds ratios are a bit more complex.
# You will have to present in probabilities. Here is a good breakdown of how to describe it: 
# https://www.theanalysisfactor.com/why-use-odds-ratios/#:~:text=Odds%20ratios%20are%20one%20of,to%20wrap%20your%20head%20around.&text=For%20example%2C%20in%20logistic%20regression,phrase%20here%20is%20constant%20effect.


###Good luck! You got this!

###Thanks, Emma. :)
