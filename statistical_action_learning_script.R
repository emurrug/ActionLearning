#### READ ME ####

#08.01.2020

#This file is meant to serve as a collaborative introduction to using R for 
#Mica's honor's thesis on Statistical Action Learning. The people writing this code are: 
#Mica Carroll (mbc225@cornell.edu) and Emma Murrugarra (eam422@cornell.edu). 

#To make life easier, there will be a lot of notes in this file. 
#This will include theoretical notes about this particular project, practical notes
#about using R, and some content/resources about statistical analysis methods broadly. 

#All the files related to this project are currently being privately hosted on Github
# at https://github.com/emurrug/ActionLearning.git



#### PROJECT GOALS (bird's eye view) ####

##What are the independent variables (IVs)?##
#Action learning [AL] (Action vs. Control; categorical binary)
#Statisticl Regularity [SR] (Statiscal vs Random; Categorical binary)


##What are the dependent variables (DVs)?##
#Recognition of correct pattern (Categorical binary)
#Confidence in recognition (Continuous; likert 1-7)
#Recall of correct pattern (Categorical binary)
#Confidence of recall (Continuouslikert 1-7)


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
#scroll through sections faster. You can also collpase sections that you don't want to see.

#The RStudio window has a lot of features that you should take the time to explore. 
#Also if you ever have any doubt about new code, go to the documentation! 
#The easiest way to do this is to type "?" in front of a new function (e.g. "?dplyr")



#First, let's import any relevant packages we will use for our study. 
#TThat way we don't have to write new functions for all of our needs!
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

# "df" is a standard shorthand for "dataframe"
#'col_names' lets the reader know that the first line of the data is variable names and not data
#'na = c(...)' is saying: "cells with the following entries should be considered NA in the datafile


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
#This means you can use an ANOVA for the likert scale DV.

#To run a test where the IV is categorical and the DV is categorical, you will need a 2x2 Chi-Square test. 
#Instead of measuring means, Chi-squares compares the frequency of results (e.g., % of responses that were correct vs. incorrect)
#to the expected frequency of results if it were random. Funny enough, this is exactly what a test of homogenity is.
#You will then have to follow this up with a odds-ratio test to see if there is an interaction.
https://www-ncbi-nlm-nih-gov.proxy.library.cornell.edu/pmc/articles/PMC2797398/
  
  





#### STATISTICAL ANALYSIS (implmened) ####

#### GRAPHING ####

#### SUMMARIZING RESULTS####

# APA format!




