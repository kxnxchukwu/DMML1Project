library(Amelia)
library(pscl)
library(ROCR)

bank_marketing <- read.csv2(file.choose())

view(bank_marketing)

sapply(bank_marketing,function(x) sum(is.na(x)))

sapply(bank_marketing, function(x) length(unique(x)))

missmap(bank_marketing, main = "Missing values vs observed")       

data <- subset(bank_marketing,select=c(2,3,4,5,7,10,13,14,15,16,17))

train <- data[1:36168,]
test <- data[36169:45211,]

model <- glm(y ~.,family=binomial(link='logit'),data=train)

summary(model)

anova(model, test="Chisq")

pR2(model)

fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10,11)),type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$y)

print(paste('Accuracy',1-misClasificError))

misClasificError

p <- predict(model, newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10,11)), type="response")

pr <- prediction(p, test$y)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")

plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]

#DECISION TREES

# read the bank data file
bank_marketing <- read.csv2(file.choose())

# View bank data
View(bank_marketing)

# view the sumamry of bank data
summary(bank_marketing)

#Data Cleaning
bank_marketing$age <- as.numeric(bank_marketing$age)
bank_marketing$balance <- as.numeric(bank_marketing$balance)
bank_marketing$day <- as.numeric(bank_marketing$day)
bank_marketing$duration <- as.numeric(bank_marketing$duration)
bank_marketing$campaign <- as.numeric(bank_marketing$campaign)
bank_marketing$pdays <- as.numeric(bank_marketing$pdays)
bank_marketing$previous <- as.numeric(bank_marketing$previous)

## Generate a decision tree
tree.bank =tree(y~., bank_marketing)

summary(tree.bank)

tree.bank

plot(tree.bank)

text(tree.bank,pretty=0)

# create training and test sets
## Performance evaluation

size <- nrow(bank_marketing) * 0.8

set.seed(123)

train=sample (1: nrow(bank_marketing), size)

bank.test=bank_marketing[-train,]

tree.bank=tree(y~.,bank_marketing, subset=train)

tree.pred=predict(tree.bank,bank.test,type="class")

actual <- as.factor(bank.test$y)
confusion.matrix <- table(tree.pred, actual)

accuracy <- confusion.matrix[1,1] + confusion.matrix[2,2]

accuracy

confusion.matrix

(7548+501)/size

(7630+448)/9043

## Cross-validation generated decision tree

set.seed(3)
cv.bank=cv.tree(tree.bank,FUN=prune.misclass)
names(cv.bank)
cv.bank

plot(cv.bank$size ,cv.bank$dev ,type="b")
plot(cv.bank$k ,cv.bank$dev ,type="b")

prune.bank=prune.misclass(tree.bank,best=3)
plot(prune.bank)
text(prune.bank,pretty=0)

# run the method on test data
tree.pred1=predict(prune.bank,bank.test,type="class")

# create the confusion matrix for test set
table(tree.pred1,actual)

(7942+142)/9043

