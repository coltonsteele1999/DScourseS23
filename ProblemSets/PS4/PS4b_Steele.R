library(sparklyr)
library(tidyverse)

spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

df1 <- iris %>% as_tibble()

df <- copy_to(sc, df1)

class(df1)
class(df)
names(df)
names(df1)

df %>% select(Sepal_Length, Species) %>% head() %>% print()


df %>% filter(Sepal_Length > 5.5) %>% head() %>% print()

df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head() %>% print()

df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head() %>% print()

df2 <- df2 %>% arrange(Species) 

print(head(df2,6))


