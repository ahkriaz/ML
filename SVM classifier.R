data<-read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")[1:1000,]
#dimension of data
dim(data)

head(data)

str(data)

#loading packages
pacman::p_load(e1071, caret, tidyverse)

#splitting data
set.seed(100)
training.samples <- data$Diabetes_binary %>%createDataPartition(p = 0.8,list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#e1071 for SVM
#model fitting with SVM
classifier<-svm(formula = Diabetes_binary ~ .,data = train.data,
                type="C-classification",kernel = 'linear')
summary(classifier)

#prediction test data
pred.y<-predict(classifier, newdata = test.data)
pred.y

cm<-table(test.data$Diabetes_binary,pred.y)
cm

m<-confusionMatrix(factor(test.data$Diabetes_binary),pred.y)
m$table

m$byClass

#checking accuracy of model with significant values only
classifier_0.05<-svm(formula = Diabetes_binary ~ HighBP + BMI + Stroke + Veggies + GenHlth + Age,data = train.data,
                     type="C-classification",kernel = 'linear')
summary(classifier_0.05)

#prediction test data
pred.y_0.05<-predict(classifier_0.05, newdata = test.data)
pred.y_0.05

model_0.05<-glm(formula = Diabetes_binary ~ HighBP + BMI + Stroke + Veggies + GenHlth + Age, family=binomial, data= train.data)
summary(model_0.05)

m_0.05<-confusionMatrix(factor(test.data$Diabetes_binary), pred.y_0.05)
m_0.05$table
m_0.05$byClass


#checking accuracy of model with p-value <0.6
classifier_0.60<-svm(formula = Diabetes_binary ~ HighBP + BMI + Stroke + HeartDiseaseorAttack + PhysActivity + Veggies + AnyHealthcare + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age,data = train.data,
                type="C-classification",kernel = 'linear')
summary(classifier_0.60)

#prediction test data
pred.y_0.60<-predict(classifier_0.60, newdata = test.data)
pred.y_0.60


model_0.60<-glm(formula = Diabetes_binary ~ HighBP + BMI + Stroke + HeartDiseaseorAttack + PhysActivity + Veggies + AnyHealthcare + GenHlth + MentHlth + PhysHlth + DiffWalk + Sex + Age, family=binomial, data= train.data)
summary(model_0.60)

m_0.60<-confusionMatrix(factor(test.data$Diabetes_binary), pred.y_0.60)
m_0.60$table
m_0.60$byClass

formula_string

##specific condition for regression

# p-value threshold for variable inclusion
p_value_threshold <- 0.05

# Get the p-values for all coefficients in the model
p_values <- summary(model)$coefficients[, "Pr(>|z|)"]

# Find the names of variables to include (p-value < 0.05)
variables_to_include <- names(p_values[p_values < p_value_threshold])

# Create a new formula string with the selected variables
formula_string <- paste("Diabetes_binary ~", paste(variables_to_include, collapse = " + "))

# Update the model with the selected variables
selected_model_0.60 <- update(model, as.formula(formula_string))

summary(selected_model_0.60)
summary(model)

exp(cbind(OR=coef(selected_model), confint(selected_model)))


m$byClass
m_0.05$byClass
m_0.60$byClass
