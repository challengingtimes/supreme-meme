#----Part 2-------------------------------------------------------------------------
library(tidyverse)
library(gridExtra)
library(caTools)
library(kableExtra)
library(glmnet)
library(randomForest)

setwd('C:/Users/User/Documents/Machine Learning/Coursework')
math1 <- read.csv("Part 2 student-mat.csv", sep = ';', header = TRUE, stringsAsFactors=T)
math2 <- math1[, !(names(math1) %in% c("G1", "G2"))]
por1 <- read.csv("Part 2 student-por.csv", sep = ';', header = TRUE, stringsAsFactors=T)
por2 <- por1[, !(names(por1) %in% c("G1", "G2"))]
schools <- merge(math1, por1, by = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu",
                                     "Mjob", "Fjob", "reason", "nursery", "internet"))
str(schools)
schools2 <- schools[, !(names(schools) %in% c("G1.x", "G1.y", "G2.x", "G2.y"))]

schools %>% gather(`G3.x`, `G3.y`, key = "subject", value = "grade") %>% ggplot() +
  geom_bar(aes(x = grade, fill = subject), position = "dodge") + 
  ggtitle("Final Grades of Math and Portuguese Subjects") +
  scale_fill_manual(name = "Subject", labels = c("Math", "Portuguese"), values = c("#810F7C", "#3690C0")) 

compare1 <- data.frame(Students = c('Those that took both', 'From the individual subjects'), 
                      Math = NA, Portuguese = NA)
compare1$Math <- c(mean(schools$G3.x), mean(math1$G3))
compare1$Portuguese <- c(mean(schools$G3.y), mean(por1$G3))
kable_styling(kable(compare1, align = 'lcc'),c("striped","bordered"), full_width = F) %>%
  add_header_above(c("", "Mean" = 2))

compare2 <- data.frame(Math = c('All students', 'All students that took both subjects'), 
                      G1 = NA, G2 = NA, G3 = NA)
compare2$G1 <- c(mean(math1$G1), mean(schools$G1.x))
compare2$G2 <- c(mean(math1$G2), mean(schools$G2.x))
compare2$G3 <- c(mean(math1$G3), mean(schools$G3.x))
kable_styling(kable(compare2, align = 'lccc'),c("striped","bordered"), full_width = F) %>%
  add_header_above(c("", "Mean" = 3))

compare3 <- data.frame(Portuguese = c('All students', 'All students that took both subjects'), 
                       G1 = NA, G2 = NA, G3 = NA)
compare3$G1 <- c(mean(por1$G1), mean(schools$G1.y))
compare3$G2 <- c(mean(por1$G2), mean(schools$G2.y))
compare3$G3 <- c(mean(por1$G3), mean(schools$G3.y))
kable_styling(kable(compare3, align = 'lccc'),c("striped","bordered"), full_width = F) %>%
  add_header_above(c("", "Mean" = 3))

mathgrades <- math1 %>% gather(`G1`, `G2`, `G3`, key = "semester", value = "grade") %>% ggplot() +
  geom_bar(aes(x = grade, fill = semester), position = "dodge") + ggtitle("Distribution of Math Grades") +
  scale_fill_manual(values = c("#40004B", "#762A83", "#9970AB"))
porgrades <- por1 %>% gather(`G1`, `G2`, `G3`, key = "semester", value = "grade") %>% ggplot() +
  geom_bar(aes(x = grade, fill = semester), position = "dodge") + ggtitle("Distribution of Portuguese Grades") +
  scale_fill_manual(values = c("#40004B", "#762A83", "#9970AB"))
grid.arrange(mathgrades, porgrades)

mathgrades2 <- ggplot(math1) + geom_bar(aes(x = school, fill = as.factor(G3)), position = "dodge") +
  ggtitle("Distribution of Math Grades by School") + theme(legend.position = "none")
porgrades2 <- ggplot(por1) + geom_bar(aes(x = school, fill = as.factor(G3)), position = "dodge") +
  ggtitle("Distribution of Portuguese Grades by School") + theme(legend.position = "none")
grid.arrange(mathgrades2, porgrades2)

math.address <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = address)) +
  ggtitle("Distribution of Math Students' Grades by Address") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
por.address <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = address)) +
  ggtitle("Distribution of Portuguese Students' Grades by Address") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
grid.arrange(math.address, por.address)

math.mjob <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = Mjob)) + 
  ggtitle("Distribution of Math Students' Grades by Mother's Job") +
  scale_colour_brewer(palette = "Dark2")
por.mjob <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = Mjob)) + 
  ggtitle("Distribution of Portuguese Students' Grades by Mother's Job") +
  scale_colour_brewer(palette = "Dark2")
grid.arrange(math.mjob, por.mjob)

math.fjob <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = Fjob)) + 
  ggtitle("Distribution of Math Students' Grades by Father's Job") + 
  scale_colour_brewer(palette = "Dark2")
por.fjob <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = Fjob)) + 
  ggtitle("Distribution of Portuguese Students' Grades by Father's Job") +
  scale_colour_brewer(palette = "Dark2")
grid.arrange(math.fjob, por.fjob)

math.famrel <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = as.factor(famrel))) +
  ggtitle("Distribution of Math Students' Grades by Family Relationships") +
  scale_colour_manual(values = c("#E41A1C", "#FF7F00", "#4DAF4A", "#377EB8", "#984EA3"))
por.famrel <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = as.factor(famrel))) +
  ggtitle("Distribution of Portuguese Students' Grades by Family Relationships") +
  scale_colour_manual(values = c("#E41A1C", "#FF7F00", "#4DAF4A", "#377EB8", "#984EA3"))
grid.arrange(math.famrel, por.famrel)

math.traveltime <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = as.factor(traveltime))) +
  ggtitle("Distribution of Math Students' Grades by Traveltime") 
por.traveltime <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = as.factor(traveltime))) +
  ggtitle("Distribution of Portuguese Students' Grades by Traveltime") 
grid.arrange(math.traveltime, por.traveltime)

math.studytime <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = as.factor(studytime))) +
  ggtitle("Distribution of Math Students' Grades by Studytime")
por.studytime <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = as.factor(studytime))) +
  ggtitle("Distribution of Portuguese Students' Grades by Studytime")
grid.arrange(math.studytime, por.studytime)

math.pfail <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = as.factor(failures))) +
  ggtitle("Distribution of Math Students' Grades by Past Failures") 
por.pfail <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = as.factor(failures))) +
  ggtitle("Distribution of Portuguese Students' Grades by Past Failures")
grid.arrange(math.pfail, por.pfail)

math.higher <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = higher)) + 
  ggtitle("Distribution of Math Students' Grades by Intention of Pursuing Higher Education") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
por.higher <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = higher)) + 
  ggtitle("Distribution of Portuguese Students' Grades by Intention of Pursuing Higher Education") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
grid.arrange(math.higher, por.higher)

math.schsup <- ggplot(math1, aes(x = G3)) + geom_density(aes(color = schoolsup)) + 
  ggtitle("Distribution of Math Students' Grades by Extra Educational Support") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
por.schsup <- ggplot(por1, aes(x = G3)) + geom_density(aes(color = schoolsup)) + 
  ggtitle("Distribution of Portuguese Students' Grades by Extra Educational Support") +
  scale_colour_manual(values = c("#810F7C", "#3690C0"))
grid.arrange(math.schsup, por.schsup)

#----Linear Regression Prediction-------------------------------------------------------------
math.model <- lm(G3 ~ ., data = math2)
summary(math.model)
RMSE.math.lin.model <- sqrt(mean(residuals(math.model)^2))   #3.8944

por.model <- lm(G3 ~ ., data = por2)
summary(por.model)
RMSE.por.lin.model <- sqrt(mean(residuals(por.model)^2))  #  2.5818

set.seed(2021)
split.math.lin <- sample.split(Y = math2$G3, SplitRatio = 0.7)
split.por.lin <- sample.split(Y = por2$G3, SplitRatio = 0.7)
trainset.math <- subset(math2, split.math.lin == T)
testset.math <- subset(math2, split.math.lin == F)
trainset.por <- subset(por2, split.por.lin == T)
testset.por <- subset(por2, split.por.lin == F)

math.lin.train <- lm(G3 ~ ., data = trainset.math)
summary(math.lin.train)    # Adjusted R-squared = 0.2095
RMSE.math.lin.train <- sqrt(mean(residuals(math.lin.train)^2))   # RMSE of trainset = 3.790

predict.math.lin <- predict(math.lin.train, newdata = testset.math)
testset.error.math <- testset.math$G3 - predict.math.lin
RMSE.math.lin.test <- sqrt(mean(testset.error.math^2))   # RMSE of testset = 4.4531

por.lin.train <- lm(G3 ~ ., data = trainset.por)
summary(por.lin.train)    # Adjusted R-squared = 0.2988
RMSE.por.lin.train <- sqrt(mean(residuals(por.lin.train)^2))   # RMSE of trainset = 2.5683

predict.por.lin <- predict(por.lin.train, newdata = testset.por)
testset.error.por <- testset.por$G3 - predict.por.lin
RMSE.por.lin.test <- sqrt(mean(testset.error.por^2))   # RMSE of testset = 2.7071

#---Lasso Regression------------------------------------------------
math.x <- model.matrix(G3 ~ ., data = math2)[, -1]
math.y <- math2$G3 

por.x <- model.matrix(G3 ~ ., data = por2)[, -1]
por.y <- por2$G3 

set.seed(2021)
math.train <- sample(1:nrow(math.x), nrow(math.x)/2)
math.test <- (-math.train)
math.y.test <- math.y[math.test]

por.train <- sample(1:nrow(por.x), nrow(por.x)/2)
por.test <- (-por.train)
por.y.test <- por.y[por.test]

grid <- 10^seq(10, -2, length = 100)

math.lasso <- glmnet(math.x[math.train,], math.y[math.train], alpha = 1, lambda = grid)
plot(math.lasso)

set.seed(2021)
cv.out.math.lasso <- cv.glmnet(math.x[math.train,], math.y[math.train], alpha=1)
plot(cv.out.math.lasso)
bestlam.math.lasso <- cv.out.math.lasso$lambda.min
bestlam.math.lasso

pred.math.lasso <- predict(math.lasso, s = bestlam.math.lasso, newx = math.x[math.test,])
RMSE.math.lasso <- sqrt(mean((pred.math.lasso - math.y.test)^2))
RMSE.math.lasso 

out.math.lasso <- glmnet(math.x, math.y, alpha = 1, lambda = grid)
coef.math.lasso <- predict(out.math.lasso, type = "coefficients", s = bestlam.math.lasso)
coef.math.lasso [-1, 1]

por.lasso <- glmnet(por.x[por.train,], por.y[por.train], alpha = 1, lambda = grid)
plot(por.lasso)

set.seed(2021)
cv.out.por.lasso <- cv.glmnet(por.x[por.train,], por.y[por.train], alpha=1)
plot(cv.out.por.lasso)
bestlam.por.lasso <- cv.out.por.lasso$lambda.min
bestlam.por.lasso 

pred.por.lasso <- predict(por.lasso, s = bestlam.por.lasso, newx = por.x[por.test,])
RMSE.por.lasso <- sqrt(mean((pred.por.lasso - por.y.test)^2))
RMSE.por.lasso  

out.por.lasso <- glmnet(por.x, por.y, alpha = 1, lambda = grid)
coef.por.lasso <- predict(out.por.lasso, type = "coefficients", s = bestlam.por.lasso)
coef.por.lasso [-1, 1]

#---Random Forests--------------------------------
set.seed(2021)
math_rf <- randomForest(G3 ~ ., data = trainset.math)
math_rf
plot(math_rf)
# no. of trees with lowest MSE
which.min(math_rf$mse)  # 202
# RMSE of this optimal random forest
sqrt(math_rf$mse[which.min(math_rf$mse)])  #3.7848

features <- setdiff(names(trainset.math), "G3")
set.seed(2021)
math_rf2 <- tuneRF(x = trainset.math[features], y = trainset.math$G3, ntreeTry = 500, mtryStart = 5,
                   stepFactor = 1.5, improve = 0.01, trace = FALSE)
math_rf2

set.seed(2021)
math.rf.final <- randomForest(G3 ~ ., data = trainset.math, ntree = 500, mtry = 15, importance = TRUE)
math.rf.final
# no. of trees with lowest MSE
which.min(math.rf.final$mse)   # 480
# RMSE of this optimal RF
sqrt(math.rf.final$mse[which.min(math.rf.final$mse)])  # 3.7847

importance(math.rf.final)
varImpPlot(math.rf.final)

pred.m.rf <- predict(math.rf.final, newdata = testset.math)
rmse.math.rf <- sqrt(mean((testset.math$G3 - pred.m.rf)^2))
rmse.math.rf

set.seed(2021)
por_rf <- randomForest(G3 ~ ., data = trainset.por)
por_rf
plot(por_rf)
# no. of trees with lowest MSE
which.min(por_rf$mse)  # 52
# RMSE of this optimal random forest
sqrt(por_rf$mse[which.min(por_rf$mse)])  #2.6440

features <- setdiff(names(trainset.por), "G3")

set.seed(2021)
por_rf2 <- tuneRF(x = trainset.por[features], y = trainset.por$G3, ntreeTry = 500, mtryStart = 5,
                   stepFactor = 1.5, improve = 0.01, trace = FALSE)
por_rf2

por.rf.final <- randomForest(G3 ~ ., data = trainset.por, ntree = 500, mtry = 7, importance = TRUE)
por.rf.final
# no. of trees with lowest MSE
which.min(por.rf.final$mse)   #188  
# RMSE of this optimal RF
sqrt(por.rf.final$mse[which.min(por.rf.final$mse)])   # 2.6513  

importance(por.rf.final)
varImpPlot(por.rf.final)

pred.p.rf <- predict(por.rf.final, newdata = testset.por)
rmse.por.rf <- sqrt(mean((testset.por$G3 - pred.p.rf)^2))
rmse.por.rf

#-------Comparison table-------------------------------
compare4 <- data.frame(Technique = c('RMSE'), Linear.Regression = NA, Lasso.Regression = NA, Random.Forest = NA)
compare4$Linear.Regression <- c(RMSE.math.lin.test)
compare4$Lasso.Regression <- c(RMSE.math.lasso)
compare4$Random.Forest <- c(rmse.math.rf)              
kable_styling(kable(compare4, col.names = gsub("[.]", "", names(compare4)), align = 'lccc'), 
              c("striped","bordered"), full_width = F) %>% add_header_above(c("", "Mathematics" = 3))

compare5 <- data.frame(Technique = c('RMSE'), Linear.Regression = NA, Lasso.Regression = NA, Random.Forest = NA)
compare5$Linear.Regression <- c(RMSE.por.lin.test)
compare5$Lasso.Regression <- c(RMSE.por.lasso)      
compare5$Random.Forest <- c(rmse.por.rf)
kable_styling(kable(compare5, col.names = gsub("[.]", "", names(compare5)), align = 'lccc'), 
              c("striped","bordered"), full_width = F) %>% add_header_above(c("", "Portuguese Language" = 3))
