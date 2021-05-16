library(Amelia)
library(tree)

setwd("/users/Kenechukwu/Documents/DMP")

online_intention <- read.csv("online_shoppers_intention.csv")

view(online_intention)

online_intention$Revenue = as.factor(online_intention$Revenue)

sapply(online_intention,function(x) sum(is.na(x)))

sapply(online_intention, function(x) length(unique(x)))

missmap(online_intention, main = "Missing values vs observed")

tree.online_intention = tree(online_intention$Revenue~., online_intention)

summary(tree.online_intention)
tree.online_intention

plot(tree.online_intention)

text(tree.online_intention,pretty=0)

# create training and test sets
## Performance evaluation

size <- nrow(online_intention) * 0.8

set.seed(123)

train=sample (1: nrow(online_intention), size)

online_intention.test=online_intention[-train,]

tree.intention=tree(as.factor(Revenue)~.,online_intention, subset=train)

tree.pred=predict(tree.intention,online_intention.test,type="class")


actual <- as.factor(online_intention.test$Revenue)
confusion.matrix <- table(tree.pred, actual)

accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]

accuracy

confusion.matrix

(1942+235)/size

## Cross-validation generated decision tree

set.seed(3)
cv.online_intention=cv.tree(tree.online_intention,FUN=prune.misclass)
names(cv.online_intention)
cv.online_intention

par(mfrow=c(2,2))

plot(cv.online_intention$size ,cv.online_intention$dev ,type="b")
plot(cv.online_intention$k ,cv.online_intention$dev ,type="b")

prune.online_intention=prune.misclass(tree.online_intention,best=3)
plot(prune.online_intention)
text(prune.online_intention,pretty=0)

# run the method on test data
tree.pred1=predict(prune.online_intention,online_intention.test,type="class")

# create the confusion matrix for test set
table(tree.pred1,actual)

(2023+170)/size

#NAIVE BAYES

library(e1071)

setwd("/users/Kenechukwu/Documents/DMP")

naive_online_intention <- read.csv("online_shoppers_intention.csv")

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(naive_online_intention$Revenue~., data=naive_online_intention)

#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,naive_online_intention)

#Confusion matrix to check accuracy
table(NB_Predictions,naive_online_intention$Revenue)

#Getting started with Naive Bayes in mlr
#Install the package
#install.packages("mlr")
#Loading the library
library(mlr)

naive_online_intention$Weekend <- NULL

#Create a classification task for learning on Online Shoppers Dataset and specify the target feature
task = makeClassifTask(data = naive_online_intention, target = "Revenue")
#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")
#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned 
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = naive_online_intention[,1:3]))

##Confusion matrix to check accuracy
table(predictions_mlr[,1],naive_online_intention$Revenue)