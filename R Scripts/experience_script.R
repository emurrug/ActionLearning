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

#MC: histogram of total experience scores.
ggplot(df2, aes(x = `Experience_Total`, na.rm = TRUE)) +
  geom_histogram(color = "black", fill = "white", stat = "bin", binwidth = 5)
