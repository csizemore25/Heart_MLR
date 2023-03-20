library(tidyverse)
data=read.csv("heart.data.csv")
view(data)
glimpse(data)
length(data)
names(data)
summary(data)

#missing values
colSums(is.na(data))
ggplot(data=data, aes(biking))+geom_histogram()
biking_median=median(data$biking, na.rm=TRUE)
data$biking=ifelse(is.na(data$biking), biking_median, data$biking)
colSums(is.na(data))

ggplot(data=data, aes(smoking))+geom_histogram()
smoking_median=median(data$smoking, na.rm=TRUE)
data$smoking=ifelse(is.na(data$smoking), smoking_median, data$smoking)
colSums(is.na(data))

ggplot(data=data, aes(heart.disease))+geom_histogram()
heart_median=median(data$heart.disease, na.rm=TRUE)
data$heart.disease=ifelse(is.na(data$heart.disease), heart_median, data$heart.disease)
colSums(is.na(data))

#split data
library(caTools)
set.seed(100)
split=sample.split(data$heart.disease, SplitRatio = 0.8) #80% Train
train=subset(data, split=TRUE)
test=subset(data, split=FALSE)

#MLR
names(data)
mlr=lm(formula = heart.disease~., data=train)
summary(mlr)

#formula
#14.958560-0.200119(biking) + 0.179512(smoking)

#all p-values are significant - both less than .05 

#MSE
summ=summary(mlr)
mse=(mean(summ$residuals^2))
paste("Mean Squared Error: ", mse)

#R-Square
summary(mlr)
paste("R-squared value is 97.65% which means this is a good model")
#value less than 50% is not a good model 

#Test set prediction 
y_pred=predict(mlr, newdata=test)
dataset=data.frame(test$heart.disease, y_pred)
head(dataset)

#Validation
new=read.csv('Heart_validation.csv')
head(new)
data.frame(new[c(3)], predict(mlr, newdata=new))
