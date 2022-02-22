library(sparklyr)
library(tidyverse)

# Set up a connection to Spark
spark_install(version="3.0.0")
sc<-spark_connect(master = "local")

df1<-as_tibble(iris)

# Copy df1 into Spark
df<-copy_to(sc,df1)

# Verify dataframe
class(df1)
class(df)

# Selection operation
df %>% 
  select(Sepal_length, Species) %>%
  head %>%
  print

# Filter operation
df %>%
  filter(Sepal_length>5.5) %>%
  head %>%
  print

df2<-df %>%
    group_by(Species) %>%
    summarize(mean=mean(Sepal_length),count=n()) %>%
    head %>%
    print

# Assigning df2 to the output
df<-copy_to(sc,df2)

df2 %>% arrange(Species) %>% head %>% print

