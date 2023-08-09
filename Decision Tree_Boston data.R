#loading libraries and data
pacman::p_load(MASS, tidymodels, cli, tidyr, caret, tidyverse, rpart.plot, vip)
data<-Boston

dim(data)
colSums(is.na(data))
str(data)
head(data)
#####

##splitting data
set.seed(100)
training.samples <- data$crim %>%createDataPartition(p = 0.8,list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

#decision tree specification
tree_spec <- decision_tree() %>%set_engine("rpart") %>%set_mode("regression")
#model fitting
tree_fit <- tree_spec %>% fit(medv ~ ., data = train.data)
summary(tree_fit)

predictions <- tree_fit %>%predict(test.data) %>%pull(.pred)
predictions

metrics <- metric_set(rmse, rsq)
model_performance <-test.data %>%mutate(predictions = predictions) %>%
  metrics(truth = medv, estimate = predictions)

print(model_performance)

rules <- rpart.rules(tree_fit$fit)
print(rules)


#extra
plot(data)

rpart.plot(tree_fit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, 
           box.palette = "auto")

var_importance <-vip(tree_fit, num_features = 10)
print(var_importance)


