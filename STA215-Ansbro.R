## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data (3).csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################
# EXAMINE QUANT_VAR1
table(data$Length_of_Conversation_in_Seconds)
mean(data$Length_of_Conversation_in_Seconds)
sd(data$Length_of_Conversation_in_Seconds)
summary(data$Length_of_Conversation_in_Seconds)

# EXAMINE QUANT_VAR2
table(data$Number_of_Humorous_Statements)
mean(data$Number_of_Humorous_Statements)
sd(data$Number_of_Humorous_Statements)
summary(data$Number_of_Humorous_Statements)

# EXAMINE QUANT_VAR3
table(data$Number_of_Stats_from_Reid)
mean(data$Number_of_Stats_from_Reid)
sd(data$Number_of_Stats_from_Reid)
summary(data$Number_of_Stats_from_Reid)

# EXAMINE QUAL_VAR1
table(data$Relation_to_the_Case)

# EXAMINE QUAL_VAR2
table(data$Audience_Emotion)

# EXAMINE QUAL_VAR3
table(data$Character_Reaction)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################
table(data$Audience_Emotion,data$Character_Reaction)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################
chisq.test(data$Audience_Emotion,data$Character_Reaction)

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova <- aov(Number_of_Humorous_Statements ~ Character_Reaction, data = data)
summary(anova)

##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
cor(data$Length_of_Conversation_in_Seconds,data$Number_of_Humorous_Statements)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Length_of_Conversation_in_Seconds ~ Number_of_Humorous_Statements, data = data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Number_of_Humorous_Statements,data$Length_of_Conversation_in_Seconds)
abline(linear_relationship, col = "lightblue")
mean(data$Number_of_Humorous_Statements)
mean(data$Length_of_Conversation_in_Seconds)
abline(a=NULL, b=NULL, h=49.94444, v=0.6296296, col = "pink")
##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Number_of_Humorous_Statements,residuals(linear_relationship))
abline(h=0, col = "lightgreen")
