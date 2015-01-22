# Descriptive statistics for the sample of individuals
library(xtable)

# Load the data
load("./MainData/MainData13.RData")

# Education time and level - crosstabulation
edutab <- table(D$edutime, D$edu)