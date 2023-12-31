#importing data set
data <- iris

#checking data set
head(iris)
table(data$Species)
dim(data)
#scaling data except col 5
data[,-5]<-scale(data[,-5])
head(data)

#load necessary libraries
pacman::p_load(caret,caTools,class,tidyverse)

#splitting data
set.seed(100)
train_samp <- data$Species %>%createDataPartition(p=0.7, list=FALSE)
train_data <- data[train_samp,]
test_data <- data[-train_samp,]

# KNN formula
knnm<-knn(train=train_data[,-5],test=test_data[,-5],cl=train_data$Species,k=1)
###########################
#create empty list for knn and confusion matrix
cm_predictions<-list()
knn_prediction <-list()
#create k values from 1 to 150
k_values<- 1:150

#fill the empty lists with corresponding k values
for (k in k_values){
    knn_prediction[[k]]<-knn(train=train_data[,-5],test=test_data[,-5],cl=train_data$Species,k=k)
    cm_predictions[[k]]<-confusionMatrix(data = knn_prediction[[k]], reference = test_data$Species)
}

#un-listing a list into a vector
knn_vector<-unlist(knn_prediction)

#creating a vector with 150 "Zero" elements for overall accuracy and by class specifications
accuracy<- numeric(150)
sens_setosa<-numeric(150)
spec_setosa<-numeric(150)
pos_pred_setosa<-numeric(150)
neg_pred_setosa<-numeric(150)
bal_acc_setosa<-numeric(150)


#replacing "zero values"  in the vector with actual values
for (i in k_values){
  accuracy[i]<-cm_predictions[[i]]$overall['Accuracy']
  sens_setosa[i]<-cm_predictions[[i]]$byClass[1,1] ##[class,Metrics] [1=setosa][1=sensitivity,2=specificity,3=pos pred,4=neg pred,11=bal_acc]
  spec_setosa[i]<-cm_predictions[[i]]$byClass[1,2]
  pos_pred_setosa[i]<-cm_predictions[[i]]$byClass[1,3]
  neg_pred_setosa[i]<-cm_predictions[[i]]$byClass[1,4]
  bal_acc_setosa[i]<-cm_predictions[[i]]$byClass[1,11]
}

#checking structure of CM lists
str(cm_predictions)

#plotting line graph: k-values vs accuracy
ovr_model<-ggplot(data = data.frame(x = k_values, y = accuracy), aes(x = x, y = y)) +
  geom_line() +
  labs(x = "K Values", y = "Accuracy", title = "K value vs Model Accuracy")

#plotting lime graph for multiple lines including legends
setosa<-ggplot(data = data.frame(x = k_values), aes(x = k_values)) +
  geom_line(aes(y=accuracy,color="Model accuracy"))+
  geom_line(aes(y = sens_setosa, color = "Sensitivity")) +
  geom_line(aes(y = spec_setosa, color = "Specificity")) +
  geom_line(aes(y = pos_pred_setosa, color = "Positive Pred Value")) +
  geom_line(aes(y=neg_pred_setosa,color="Negative Pred Value"))+
  geom_line(aes(y=bal_acc_setosa,color="Balanced accuracy"))+
  labs(x = "K-values", y = "Performance Metrics", title = "K-value vs KNN metrics (Setosa)")+
  scale_color_manual(values = c("Model accuracy" = "black", "Sensitivity" = "skyblue", 
                                "Specificity" = "blue","Positive Pred Value"= "orange", 
                                "Negative Pred Value" = "brown","Balanced accuracy" = "purple"))+ 
  theme(legend.box = "Vertical") 



##VERSICOLOR
sens_vers<-numeric(150)
spec_vers<-numeric(150)
pos_pred_vers<-numeric(150)
neg_pred_vers<-numeric(150)
bal_acc_vers<-numeric(150)


#replacing "zero values"  in the vector with actual values
for (i in k_values){
  accuracy[i]<-cm_predictions[[i]]$overall['Accuracy']
  sens_vers[i]<-cm_predictions[[i]]$byClass[2,1] ##[class,Metrics] [1=setosa][1=sensitivity,2=specificity,3=pos pred,4=neg pred,11=bal_acc]
  spec_vers[i]<-cm_predictions[[i]]$byClass[2,2]
  pos_pred_vers[i]<-cm_predictions[[i]]$byClass[2,3]
  neg_pred_vers[i]<-cm_predictions[[i]]$byClass[2,4]
  bal_acc_vers[i]<-cm_predictions[[i]]$byClass[2,11]
}

vers<-ggplot(data = data.frame(x = k_values), aes(x = k_values)) +
  geom_line(aes(y=accuracy,color="Model accuracy"))+
  geom_line(aes(y = sens_vers, color = "Sensitivity")) +
  geom_line(aes(y = spec_vers, color = "Specificity")) +
  geom_line(aes(y = pos_pred_vers, color = "Positive Pred Value")) +
  geom_line(aes(y=neg_pred_vers,color="Negative Pred Value"))+
  geom_line(aes(y=bal_acc_vers,color="Balanced accuracy"))+
  labs(x = "K-values", y = "Performance Metrics", title = "K-value vs KNN metrics (Versicolor)")+
  scale_color_manual(values = c("Model accuracy" = "black", "Sensitivity" = "skyblue", 
                                "Specificity" = "blue","Positive Pred Value"= "orange", 
                                "Negative Pred Value" = "brown","Balanced accuracy" = "purple"))+ 
  theme(legend.box = "Vertical") 

##VIRGINICA
sens_vrg<-numeric(150)
spec_vrg<-numeric(150)
pos_pred_vrg<-numeric(150)
neg_pred_vrg<-numeric(150)
bal_acc_vrg<-numeric(150)


#replacing "zero values"  in the vector with actual values
for (i in k_values){
  accuracy[i]<-cm_predictions[[i]]$overall['Accuracy']
  sens_vrg[i]<-cm_predictions[[i]]$byClass[3,1] ##[class,Metrics] [1=setosa][1=sensitivity,2=specificity,3=pos pred,4=neg pred,11=bal_acc]
  spec_vrg[i]<-cm_predictions[[i]]$byClass[3,2]
  pos_pred_vrg[i]<-cm_predictions[[i]]$byClass[3,3]
  neg_pred_vrg[i]<-cm_predictions[[i]]$byClass[3,4]
  bal_acc_vrg[i]<-cm_predictions[[i]]$byClass[3,11]
}

vrg<-ggplot(data = data.frame(x = k_values), aes(x = k_values)) +
  geom_line(aes(y=accuracy,color="Model accuracy"))+
  geom_line(aes(y = sens_vrg, color = "Sensitivity")) +
  geom_line(aes(y = spec_vrg, color = "Specificity")) +
  geom_line(aes(y = pos_pred_vrg, color = "Positive Pred Value")) +
  geom_line(aes(y=neg_pred_vrg,color="Negative Pred Value"))+
  geom_line(aes(y=bal_acc_vrg,color="Balanced accuracy"))+
  labs(x = "K-values", y = "Performance Metrics", title = "K-value vs KNN metrics (Virginica)")+
  scale_color_manual(values = c("Model accuracy" = "black", "Sensitivity" = "skyblue", 
                                "Specificity" = "blue","Positive Pred Value"= "orange", 
                                "Negative Pred Value" = "brown","Balanced accuracy" = "purple"))+ 
  theme(legend.box = "Vertical") 


pacman::p_load(ggplot2,ggpubr)
fig<- ggarrange(ovr_model,setosa,vers,vrg,
                ncol = 2, nrow=2)

fig

