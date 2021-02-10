#MC: I created this script for fiddling around with experience scores.

library (readr)
library (kableExtra)
library (pastecs)
library (dplyr)
library (ggplot2)

#file2 is a new dataframe that combines the experience scores with comprehension and production data.
file2 <- "~/Documents/Most/-SLA_Study/GitHub/ActionLearning/Data/SLA Official Data/Concatenated Data/Data + Experience All.csv"
df2 <- read_csv(file2)
View(df2)
str(df2)
df2<-df2[!(df2$Finished=="0"),]

#Coverting columns to desired variable type.
num.columns <- c('T-1_Likert', 'T-2_Likert', 'T-3_Likert', 'T-4_Likert', 'UT-1_Likert', 'UT-2_Likert', 'UT-3_Likert', 'UT-4_Likert')
num.columns <- c('T-1_Recognition', 'T-2_Recognition', 'T-3_Recognition', 'T-4_Recognition', 'UT-1_Recognition', 'UT-2_Recognition', 'UT-3_Recognition', 'UT-4_Recognition')
num.columns <- c('T-1c_Likert', 'T-2c_Likert', 'T-3c_Likert', 'T-4c_Likert', 'Correct_Mvt_T-1', 'Correct_Mvt_T-2', 'Correct_Mvt_T-3', 'Correct_Mvt_T-4')
df[num.columns] <- sapply(df[num.columns], as.numeric)

df2$Finished <- as.factor(df2$Finished)
df2$`Statistical Organization` <- as.factor(df2$`Statistical Organization`)
df2$Condition <- as.factor(df2$Condition)

#MC: histogram of total experience scores.
ggplot(df2, aes(x = `Experience_Total`, na.rm = TRUE)) +
  geom_histogram(color = "black", fill = "white", stat = "bin", binwidth = 5)


#MC: regression for comprehension as predicted by experience.
model_experience_comprehension <- lm(df2$Correct_Trigram_Scores ~ df2$Experience_Total)
summary(model_experience_comprehension)

#MC: scatterplot for comprehension and experience
experience_comprehension <- ggplot(df2, aes(df2$Experience_Total, df$Correct_Trigram_Scores))
experience_comprehension + stat_summary(geom = "point")


#MC: regression for production as predicted by experience.
model_experience_production <- lm(df2$Correct_Mvt_Scores ~ df2$Experience_Total)
summary(model_experience_production)

#MC: scatterplot for production and experience
experience_production <- ggplot(df2, aes(df2$Experience_Total, df$Correct_Mvt_Scores))
experience_production + stat_summary(geom = "point")
