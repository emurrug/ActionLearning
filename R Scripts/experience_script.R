#MC: I created this script for fiddling around with experience scores.

library (readr)
library (kableExtra)
library (pastecs)
library (dplyr)
library (ggplot2)

myfile <- "~/Documents/Most/-SLA_Study/GitHub/ActionLearning/Data/SLA Official Data/Concatenated Data/Experience All Totals.csv"
df2 <- read_csv(myfile)
View(df2)
str(df2)
df2<-df2[!(df2$Finished=="0"),]

#MC: histogram of total experience scores.
ggplot(df2, aes(x = `Experience_Total`, na.rm = TRUE)) +
  geom_histogram(color = "black", fill = "white", stat = "bin", binwidth = 5)


#MC: regressions for comprehension and production as predicted by experience.
#MC: still need to combine data frames to be absolutely sure of what's going on here. Check out cbind.
model_experience_comprehension <- lm(df$Experience_Total ~ df2$Experience_Total)
summary(model_experience_comprehension)


model_experience_production <- lm(df$Correct_Mvt_Scores ~ df2$Experience_Total)
summary(model_experience_production)
