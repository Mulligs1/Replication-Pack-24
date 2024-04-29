## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
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
data <- read_delim("raw_data.csv")


data <- read_delim("raw_data.csv")


mean(data$emotional, na.rm=TRUE)
mean(data$importance, na.rm=TRUE)

11/20
1/20
6/20
2/20

13/20
7/20

sd(data$emotional, na.rm=T)
sd(data$importance, na.rm=T)

data <- read_delim("raw_data.csv")

table(data$violence, data$positive_vs_negative)
chisq.test(data$violence, data$positive_vs_negative)

#### box plots ############
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
library(readxl)


ggplot(raw_data, aes(x = data$violence, y = data$importance)) +
  geom_boxplot() +
  labs(title = "Box Plot of Violence by Importance Level",
       x = "Violence",
       y = "Importance Level") +
  theme_classic()


# Perform ANOVA
anova_adapted <- aov(importance ~ violence, data = data)
# Summarize ANOVA results
summary(anova_adapted)

anova_adapted <- aov(importance ~ violence, data = data)

#####LINEAR REGRESSION#####
plot(data$emotional, data$importance)

linear_relationship <- lm(data$importance ~ data$emotional, data = raw_data)
summary(linear_relationship)       

abline(linear_relationship, col = "red")

#Linear regression plot#
plot(raw_data$emotional, residuals(linear_relationship))

abline(h = 0, col = "red")

meany <- mean(raw_data$importance)
meanx <- mean(raw_data$emotional)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")