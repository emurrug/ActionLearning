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
library (kable)   #formatting table
library (pastecs) #describes data
library (dplyr)   #manipulating data to view it
library (ggplot2) #visualizing data with graphs (we don't talk about the first "ggplot")




#### IMPORTING DATA ####

#The benefit to using file sharing accounts like GitHub or Dropbox is that everyone
#has access to the most updated CSV of the data. In R, you are also able to import CSV files directly from github:

myfile <- "https://raw.github.com/user/repository/branch/file.name"  
df <- read_csv(myfile, col_names = TRUE, na = c("", "NA"))  

in# "df" is a standard shorthand for "dataframe"
#'col_names' lets the reader know that the first line of the data is variable names and not data
#'na = c(...)' is saying: "cells with the following entries should be considered NA in the data file


#You can also import the file directly from your computer. To do this, you just put your path
#directory instead of a URL. 

#to view your first few lines you do 
kable(head(df))
#or to view the whole table at once 
View(df)



#### CLEANING & VISUALIZING DATA ####

#I don't care if it's just a quick peek, you should always do these steps!

#You remember NOIR? (Nominal, Ordinal, Interval, Ratio)
#The first step is to make sure that your variables are labelled appropriately.
#R will try to do some of this automatically, but a mislabelled variable can lead 
#to very erroneous and confusing results.
#Here's one way to check how all of your variables are labelled: 

str(df)

#to look at individual variables you can type in something like
is.factor(df$variable) #OR
is.numeric(df$variable)

#The next thing to do is look at the descriptive statstics for your variables (e.g. range, central tendency, & variablility)
#Make sure that these numbers make sense (e.g., is there an impossible age? is the SD ridiculously high?)
#There are a lot of good ways to do this: https://www.statmethods.net/stats/descriptives.html

stat.desc(df$variable)

#Finally make sure you are checking your residual distributions so that you know if you
#are/aren't violating any statistical assumptions (e.g. normality, linearity, homogeneity, etc.)

#first, let's visualize the shape of the data: 
ggplot(df, aes(x = df$variable)) + #specifies your x,y of interest (no Y here since it is a histogram)
  geom_histogram(binwidth = 1, color = "black", fill = "white") #tinker with color and size
  
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



#### STATISTICAL TECHNIQUES FOR GOALS (theoretical) ####
#Looking back to the PROJECT GOALS, we see that our main IVs and main DVs are mostly categorical. 
#Generally, ANOVAs are best to use when you are interested in comparing means between two or more
#categorical groups. However, binary DVs automatically violate the assumptions needed for a linear model. 
#Therefore we will ahve to get a bit more creative...

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

#The linear regression test: 
model3 <- lm(df$LikertDV ~ df$actionIV * df$statisticalIV) #notice the similarity to the ANOVA. There's a theoretical reason for that!
summary(model3)


#Now that you have the basic format of each of the tests that you need, try it out with the demographic variables
#and see if they predict any of the dependenct variables of interest.


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
