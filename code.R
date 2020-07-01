data<-DataSheet30_6
PQ1<-data$`10. How often have you been upset because of something that happened unexpectedly?`
PQ2<-data$`11. How often have you felt that you were unable to control the important things in your life?`
PQ3<-data$`12. How often have you felt nervous and stressed?`
PQ4<-data$`13. How often have you felt confident about your ability to handle your personal problems?`
PQ5<-data$`14. How often have you felt that things were going your way?`
PQ6<-data$`15. How often have you found that you could 0t cope with all the things that you had to do?`
PQ7<-data$`16. How often have you been able to control irritations in your life?`
PQ8<-data$`17. How often have you felt that you were on top of things?`
PQ9<-data$`18. How often have you been angered because of things that were outside of your control?`
PQ10<-data$`19. How often have you felt difficulties were piling up so high that you could 0t overcome them?`


library(dplyr)
df1= data.frame(PQ1,PQ2,PQ3,PQ6,PQ9,PQ10)

Factor1<- df1 %>%
  replace(is.na(.), 0) %>%
  mutate(factor1= rowSums(.[1:6]))

df2= data.frame(PQ4,PQ5,PQ7,PQ8)

Factor2<- df2 %>%
  replace(is.na(.), 0) %>%
  mutate(factor2= rowSums(.[1:4]))

DFSum<-data.frame(PQ1,PQ2,PQ3,PQ6,PQ9,PQ10,PQ4,PQ5,PQ7,PQ8)

TotalSum<- DFSum %>%
  replace(is.na(.), 0) %>%
  mutate(sum= rowSums(.[1:10]))



TotalSum$sum
#low stress
sum(TotalSum$sum >=0 & TotalSum$sum < 14)
#Moderate Stress
sum(TotalSum$sum >=14 & TotalSum$sum <=26)
#High Perceived Stress
sum(TotalSum$sum >=27 & TotalSum$sum <= 40)

#Spearman Corelation Coefficient
corr <- cor.test(x=TotalSum$PQ1, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ2, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ3, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ4, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ5, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ6, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ7, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ8, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ9, y=TotalSum$sum, method = 'spearman',exact=F)
cor.test(x=TotalSum$PQ10, y=TotalSum$sum, method = 'spearman',exact=F)



#Disribution of option scores in %

(sum(TotalSum$PQ1 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ1 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ2 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ2 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ3 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ3 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ4 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ4 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ5 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ5 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ6 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ6 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ7 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ7 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ8 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ8 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ9 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ9 == 4)/nrow(TotalSum))*100

(sum(TotalSum$PQ10 == 0)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 1)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 2)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 3)/nrow(TotalSum))*100
(sum(TotalSum$PQ10 == 4)/nrow(TotalSum))*100

#**************************
library(psych)
#Communality --- h2 will be the Communality
communalityTotal<-factanal(DFSum, factors=2, rotation = "varimax")
Communality<- apply(communalityTotal$loadings^2,1,sum)

#****************
library(REdaS)
#Kaiser-Meyer-Olkin KMO
KMOS(DFSum, use = "pairwise.complete.obs")
#****************

bartlett.test(DFSum)


#Determinet Score
summary(DFSum)$r.squared 


#Confirmatory Factor Analysis (CFA)


library(cfa)
HS.model <-'question = ~ PQ1 + PQ2 + PQ3 + PQ6 + PQ9 + PQ10 + PQ4 + PQ5 + PQ7 + PQ8'
fit <- cfa(HS.model,DFSum)
summary(fit, fit.measures=TRUE)
