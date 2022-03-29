library(mice)
library(modelsummary)
library(tidyverse)
library(sqldf)

setwd("C:/Jwang/00-OKU/Coursework/2022H1-ECON 5253/DScourseS22-MyFork/ProblemSets/PS7")
wage<-read.csv("wages.csv")

wage<-drop_na(wage,hgc, tenure)

datasummary_skim(wage, output= "Skim.tex")
datasummary_skim(wage, type="categorical", output= "Skim.tex")

wage1<-drop_na(wage, logwage)
ylm1<-lm(logwage~hgc + college + tenure + I(tenure^2) +age + married, data=wage1)

mlogwage<-mean(wage1$logwage)
wage2<-wage %>% 
  mutate(logwage = ifelse(is.na(logwage), mlogwage, logwage))
ylm2<-lm(logwage~hgc + college + tenure + I(tenure^2) +age + married, data=wage2)

wage3<-wage %>% 
  mutate(pwage =predict(ylm1,wage2))
library(dplyr)
wage3<-wage3 %>% mutate(logwage=coalesce(logwage,pwage))
ylm3<-lm(logwage~hgc + college + tenure + I(tenure^2) +age + married, data=wage3)

wage4 <- complete(mice(wage, m = 5, printFlag = FALSE),1)
ylm4<-lm(logwage~hgc + college + tenure + I(tenure^2) +age + married, data=wage4)

mod <- list(
  "MCAR" = ylm1,
  "Mean Filling" = ylm2,
  "MAR" = ylm3,
  "mice" = ylm4
)

modelsummary(mod, output= "latex")
