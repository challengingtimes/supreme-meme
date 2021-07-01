#--------Part 3---------------------------------------------------------------
library(caTools)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(MASS)
library(ROCR)
library(tree)
library(randomForest)
library(kableExtra)

setwd('C:/Users/User/Documents/Machine Learning/Coursework')
bank.data1 <- read.csv("Part 3 bank.csv", sep = ';', stringsAsFactors=T)
str(bank.data1)
bank.data2 <- bank.data1[, !(names(bank.data1) %in% c("duration"))]
bank.data2$pdays[bank.data2$pdays == -1] <- 0

#-----Data Exploration------------------------------------------------
summary(bank.data2)

plot1 <- ggplot(bank.data2) + geom_histogram(aes(x = age),color = "purple1", fill = "#C994C7", binwidth = 5) +
  ggtitle('Age Distribution') + ylab('Count') + xlab('Age') + 
  geom_vline(aes(xintercept = mean(age), color = "#E41A1C")) + scale_x_continuous(breaks = seq(0, 100, 5)) +
  theme(legend.position = "none")
plot2 <- ggplot(bank.data2) + geom_boxplot(aes(x = '', y = age)) + ggtitle('Age Boxplot') + ylab('Age')
grid.arrange(plot1, plot2, ncol = 2)

ggplot(bank.data2, aes(x = age, fill = marital)) + geom_histogram(binwidth = 2, alpha = 0.7) +
  facet_grid(cols = vars(y)) + expand_limits(x = c(0, 100)) + scale_x_continuous(breaks = seq(0, 100, 10)) +
  ggtitle("Age Distribution by Marital Status") + scale_fill_manual(values = c("#E41A1C", "#762A83", "#9970AB"))

m <- bank.data2 %>% group_by(y) %>% summarize(grp.mean = mean(age))
ggplot (bank.data2, aes(x=age)) + geom_histogram(color = "purple1", fill = "#C994C7", binwidth = 5) + 
  facet_grid(cols = vars(y)) + ggtitle("Age Distribution by Subscription") + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0, 100, 5)) + geom_vline(data=m, aes(xintercept = grp.mean), color = "red")

ggplot(data = bank.data2, aes(x = education, fill = y)) + geom_bar(position = "dodge") +
  ggtitle("Subscription based on Education Level") + xlab(" Education Level") +
  guides(fill = guide_legend(title = "Subscription of Term Deposit")) +
  scale_fill_manual(values = c("#762A83", "#9970AB"))

ggplot(bank.data2, aes(x = campaign)) + geom_density(aes(color = y)) +
  ggtitle("Subscription based on Number of Contact during the Campaign") +
  xlab("Number of Contact during the Campaign") + scale_colour_manual(values = c("#E41A1C", "#3690C0"))

#---------Logistic Regression--------------------------------------------------------
levels(bank.data2$y)
m1 <- glm(y ~ ., family = binomial, data = bank.data2)
summary(m1)
# age, education, default, balance, housing, day, pdays, previous are insignificant

prob1 <- predict(m1, type = 'response')

threshold1 <- 0.5

y.hat.1 <- ifelse(prob1 > threshold1, "yes", "no")

table1 <- table(y.hat.1, bank.data2$y)
table1  # This is the confusion matrix
# 438 cases false positive, 45 cases false negative

# Overall Accuracy
acc.lr.m1 <- mean(y.hat.1 == bank.data2$y)  # 0.8931652

m2 <- glm(y ~ . - age - education - default - balance - housing - day - pdays - previous, family = binomial, data = bank.data2)
summary(m2)

prob2 <- predict(m2, type = 'response')
y.hat.2 <- ifelse(prob2 > threshold1, "yes", "no")
table2 <- table(y.hat.2, bank.data2$y)
table2
# 438 cases false positive, 47 cases false negative

# Overall Accuracy
acc.lr.m2 <- mean(y.hat.2 == bank.data2$y)  # 0.8927228

# Train-Test split -------------------------------------
set.seed(2021)
train <- sample.split(Y = bank.data2$y, SplitRatio = 0.7)
trainset <- subset(bank.data2, train == T)
testset <- subset(bank.data2, train == F)

m3 <- glm(y ~ . , family = binomial, data = trainset)
summary(m3)

# Confusion Matrix on Trainset
prob3.train <- predict(m3, type = 'response')
predict3.y.train <- ifelse(prob3.train > threshold1, "yes", "no")
table3 <- table(trainset$y, predict3.y.train)
table3  

# Overall Accuracy
acc.lr.m3.train <- mean(predict3.y.train == trainset$y)    #0.8951027

# Confusion Matric on Testset
prob3.test <- predict(m3, newdata = testset, type = 'response')
predict3.y.test <- ifelse(prob3.test > threshold1, "yes", "no")
table4 <- table(testset$y, predict3.y.test)
table4

# Overall Accuracy
acc.lr.m3.test <- mean(predict3.y.test == testset$y)      #0.8960177

m4 <- glm(y ~ . - age - education - default - balance - housing - day - pdays - previous, family = binomial, data = trainset)
summary(m4)

# Confusion Matrix on Trainset
prob4.train <- predict(m4, type = 'response')
predict4.y.train <- ifelse(prob4.train > threshold1, "yes", "no")
table5 <- table(trainset$y, predict4.y.train)
table5

# Overall Accuracy
acc.lr.m4.train <- mean(predict4.y.train == trainset$y)    #0.8919431

# Confusion Matric on Testset
prob4.test <- predict(m4, newdata = testset, type = 'response')
predict4.y.test <- ifelse(prob4.test > threshold1, "yes", "no")
table6 <- table(testset$y, predict4.y.test)
table6

# Overall Accuracy
acc.lr.m4.test <- mean(predict4.y.test == testset$y)      #0.8982301

#---Linear Discriminant Analysis--------------------------------------
lda.m1 <- lda(y ~ . - age - education - default - balance - housing - day - pdays - previous, data = trainset)
lda.m1

lda.predict <- predict(lda.m1, testset)
lda.class <- lda.predict$class
table1 <- table(lda.class, testset$y)
table1

acc.lda <- mean(lda.class == testset$y)

#-----QDA-----------------------------------------------------------------
qda.m1 <- qda(y ~ . - age - education - default - balance - housing - day - pdays - previous, data = trainset)
qda.m1

qda.predict <- predict(qda.m1, testset)
qda.class <- qda.predict$class
table2 <- table(qda.class, testset$y)
table2

acc.qda <- mean(qda.class == testset$y)

#---------Classification Tree--------------------------------------------------------------------
tree.m1 <- tree(y ~ ., data = bank.data2)
summary(tree.m1)
plot(tree.m1)
text(tree.m1, pretty = 0)
tree.m1

tree.m2 <- tree(y ~ ., data = trainset)
tree.predict <- predict(tree.m2, testset, type = "class")
table3 <- table(tree.predict, testset$y)
table3

acc.tree <- mean(tree.predict == testset$y)   #0.899705

set.seed(2021)
cv.tree <- cv.tree(tree.m2, FUN = prune.misclass)
cv.tree
par(mfrow = c(1, 2))
plot(cv.tree$size, cv.tree$dev, type = "b")
plot(cv.tree$k, cv.tree$dev, type = "b")

m3.prune.tree <- prune.misclass(tree.m2, best = 2)
plot(m3.prune.tree)
text(m3.prune.tree, pretty = 0)

tree.predict2 <- predict(m3.prune.tree, testset, type = "class")
table4 <- table(tree.predict2, testset$y)
table4

acc.tree2 <- mean(tree.predict2 == testset$y)

#--------Random Forest-------------------------------------------------------
set.seed(2021)
rf.m1 <- randomForest(y ~ ., data = trainset, mtry = 15, importance = TRUE)
rf.m1

varImpPlot(rf.m1)

rf.predict <- predict(rf.m1, testset)
acc.rf <- mean(rf.predict == testset$y) #0.887905

features <- setdiff(names(trainset), "y")
set.seed(2021)
rf.m9 <- tuneRF(x = trainset[features], y = trainset$y, ntreeTry = 500, mtryStart = 5,
                stepFactor = 1.5, improve = 0.01, trace = FALSE)

set.seed(2021)
rf.m2 <- randomForest(y ~ ., data = trainset, mtry = 3, importance = TRUE)
rf.m2
rf.predict2 <- predict(rf.m2, testset)
acc.rf2 <- mean(rf.predict2 == testset$y) #0.8893805

varImpPlot(rf.m2)

#----Comparing Models--------------------------------------------------------------
compare6 <- data.frame(Method = c('Accuracy'), Logistic.Regression = NA, LDA = NA, QDA = NA,
                       Classification.Tree = NA, Random.Forest = NA)
compare6$Logistic.Regression <- c(acc.lr.m4.test)
compare6$LDA <- c(acc.lda)      
compare6$QDA <- c(acc.qda)
compare6$Classification.Tree <- c(acc.tree)
compare6$Random.Forest <- c(acc.rf2)
kable_styling(kable(compare6, col.names = gsub("[.]", "", names(compare6)), align = 'lccccc'), 
              c("striped","bordered"), full_width = F)
