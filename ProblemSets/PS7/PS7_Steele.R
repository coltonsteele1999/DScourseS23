library(modelsummary)
library(mice)
library(tidyverse)

#4
df<-data.frame(read.csv("C:/Users/colto/Downloads/wages.csv"))

#5
df2<-df %>% filter(!is.na(tenure) & !is.na(hgc))
df2$tenure_sq<-df2$tenure^2

#6
datasummary_skim(data=df2,output="summary.tex",histogram=FALSE)


#7
#a
df_complete<-df2 %>% filter(!is.na(logwage))
est_complete<-lm(logwage~hgc+college+tenure+tenure_sq+age+married,data=df_complete)

#b
df_mean<-df2
logwage_mean<-mean(df_mean$logwage,na.rm=TRUE)
df_mean$logwage[is.na(df_mean$logwage)]<-logwage_mean
est_mean<-lm(logwage~hgc+college+tenure+tenure_sq+age+married,data=df_mean)

#c
df_predicted<-df2
df_predicted$logwage[is.na(df2$logwage)]<-predict(est_complete,newdata=df_predicted)[is.na(df_predicted$logwage)]
est_predicted<-lm(logwage~hgc+college+tenure+tenure_sq+age+married,data=df_predicted)

#d
imputed_data <- mice(df2, m = 5, method = "pmm", seed = 123)
est_mice <- with(imputed_data, lm(logwage ~ hgc+college+tenure+tenure_sq+age+married))

models<-list("Complete"=est_complete,"Mean"=est_mean,"Predicted"=est_predicted,"Mice"=est_mice)
modelsummary(models,output="models.tex",stars=TRUE)

