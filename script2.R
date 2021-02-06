dat<-read.csv("winequality-red.csv")
sum(is.na(dat))
str(dat)
summary(dat)

dat$verdict<-ifelse(dat$quality>5,'good','bad')
class(dat$verdict)


dat$verdict = factor(dat$verdict,
                       levels = c('bad', 'good'),
                       labels = c(0,1))
class(dat$quality)

library(caTools)
set.seed(123)
split = sample.split(dat$quality, SplitRatio = 0.75)
training_set = subset(dat, split == TRUE)
test_set = subset(dat, split == FALSE)


training_set[,1:11] = scale(training_set[1:11])
test_set[,1:11] = scale(test_set[,1:11])

c<-table(colnames(dat))
c

classifier = glm(formula = verdict ~  alcohol+chlorides+citric.acid + density
                 +fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates 
                 + total.sulfur.dioxide+ volatile.acidity,
                 family = binomial,
                 data = training_set)


library(stats)
library(dplyr)
df_wine<- select(dat,alcohol,chlorides,citric.acid,density
                 ,fixed.acidity,free.sulfur.dioxide,pH,residual.sugar,sulphates 
                 ,total.sulfur.dioxide,volatile.acidity)
c<-cor(df_wine)
corrplot(c)
vif(classifier)

#NEWCLASSIFIER
classifier = glm(formula = verdict ~  alcohol+chlorides+citric.acid + density
                 +free.sulfur.dioxide+pH+residual.sugar+sulphates 
                 + total.sulfur.dioxide+ volatile.acidity,
                 family = binomial,
                 data = training_set)

prob_pred = predict(classifier, type = 'response', newdata = test_set[,1:11])
prob_pred
y<-ifelse(prob_pred>0.5,1,0)
cm<-table(test_set[,13],y)
cm
sum(diag(cm))/nrow(test_set)


 

