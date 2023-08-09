#loading packages
pacman::p_load(e1071, caret, tidyverse,rio)
data<-import("diabetes_binary_health_indicators_BRFSS2015.csv")[1:1000,]

#dimension of data
dim(data)
head(data)
str(data)

#splitting data
set.seed(100)
training.samples <- data$Diabetes_binary %>%createDataPartition(p = 0.8,list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#model fitting with SVM
classifier<-svm(formula = Diabetes_binary ~ .,data = train.data,
                type="C-classification",kernel = 'linear')
summary(classifier)

#prediction test data
pred.y<-predict(classifier, newdata = test.data)
pred.y

#create confusion matrix
m<-confusionMatrix(factor(test.data$Diabetes_binary),pred.y)
m$table
m$byClass
