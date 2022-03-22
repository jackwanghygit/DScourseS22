library(tidyverse)
library(sqldf)
library(ggplot2)

setwd("C:/Jwang/00-OKU/Coursework/2022H1-ECON 5253/DScourseS22-MyFork/ProblemSets/PS6")

arc<-as_tibble(read.csv(file = "arc.csv"))
ng<-as_tibble(read.csv(file = "ng.csv"))
work<-as_tibble(read.csv(file = "work.csv"))

work1<-sqldf("select a.*, b.mgr_exclude, b.mgr_ng_eps
             from work a left join ng b
             on a.gvkey=b.ngvkey and a.datadate=b.date")

arc1 <- distinct(arc, cik, Fiscal_Period_End, .keep_all = TRUE)
work2<-sqldf("select a.*, b.arc
             from work1 a left join arc1 b
             on a.cik=b.cik and a.datadate=b.fiscal_period_end")

work3<-work2 %>% 
  mutate(mgr_exclude = ifelse(is.na(mgr_exclude), 0, mgr_exclude))

work3<-rename(work3, ng=mgr_exclude, eps=MGR_NG_EPS)

# plot histogram to check the distribution of Non-Gaap disclosure

f <- ggplot(work3, aes(fyear, ng))
f+geom_col()

# To check the trend Non-Gaap eps exclusion
f <- ggplot(work3, aes(fyear, eps))
f+geom_point()

# To visualize the relation between ARC (accounting reporting complexity) and Non-Gaap disclosure
work5<-work3%>% mutate(arc)%>%as.numeric

qplot(work3$arc, eps, data=work3)
hist(work3$arc)



