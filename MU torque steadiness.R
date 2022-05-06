
library(readxl)
library(tidyverse)
library(emmeans)
library(ggplot2)


#Coefficient of variation of torque analysis - 10% MVIC

#dataset 10% MVIC----
torque_steadiness_10 <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/torque steadiness 10.xlsx")
View(torque_steadiness_10)

#Shapiro wilk - normality test- 
torque_steadiness_10 %>% shapiro_test(Covis)

#Student T-test
t10 <- t.test(Covis ~ Groups, torque_steadiness_10, var.equal=TRUE)
t10

# * 95% ci---
transform(torque_steadiness_10, Groups= as.factor(Groups))
transform(torque_steadiness_10, Covis= as.factor(Covis))
summarySE(torque_steadiness_10, measurevar="Covis", groupvars="Groups")

#Coefficient of variation of torque analysis - 20% MVIC

#dataset 20% MVIC----
torque_steadiness_20 <- read_excel("C:/Users/gabep/Desktop/Study 3/HDEMG paper/paper/final/github/torque steadiness 20.xlsx")
View(torque_steadiness_20)

#Shapiro wilk - normality test- 
torque_steadiness_20 %>% shapiro_test(Covis)

#Student T-test
t20 <- t.test(Covis ~ Groups, torque_steadiness_20, var.equal=TRUE)
t20


# * 95% ci---
transform(torque_steadiness_20, Groups= as.factor(Groups))
transform(torque_steadiness_20, Covis= as.factor(Covis))
summarySE(torque_steadiness_20, measurevar="Covis", groupvars="Groups")


          