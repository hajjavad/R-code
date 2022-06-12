Google_Play <- read.csv("FINALLY.csv",stringsAsFactors = T)
Google_Play_App <- na.omit(Google_Play)
View(Google_Play_App)
str(Google_Play_App)
summary(Google_Play_App)

# Install Package
install.packages("caret")
install.packages("e1071")
install.packages("pRoc")

#library
library(caTools)
library(ROSE)
library(randomForest)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ROCR)
library(caret)
library(e1071)
library(pROC)

#ggpolt
ggplot(Google_Play_App , aes(x = log(Installs), y =log(Reviews), color = Category )) + geom_boxplot()
ggplot(Google_Play_App , aes(x = log(Installs), y =log(Reviews), color = Category )) + geom_point() + stat_smooth(method = "lm", level = 0.99) + facet_grid("Category")
ggplot(Google_Play_App , aes(y = log(Installs), x = Rating2, color = Category)) + geom_boxplot() + facet_grid("Category")
ggplot(Google_Play_App , aes(y = log(Installs) , x = Category , color = Rating2)) + geom_boxplot()+ facet_grid("Rating2")
ggplot(Google_Play_App , aes(y = log(Installs) , x = Current.Version )) + geom_boxplot()+ facet_grid("Category")
ggplot(Google_Play_App , aes(y = log(Installs) , x = Android.Version )) + geom_boxplot() + facet_grid("Category")

ggplot(Google_Play_App , aes(y = Installs_Cat, x =log(Reviews), color = Category )) + geom_boxplot()
ggplot(Google_Play_App , aes(y = Installs_Cat, x =log(Reviews), color = Category )) + geom_boxplot() + facet_grid("Category")
ggplot(Google_Play_App , aes(y = Installs_Cat, x = Rating2, color = Category)) + geom_boxplot()
ggplot(Google_Play_App , aes(y = Installs_Cat , x = Category , color = Rating2)) + geom_boxplot()

ggplot(Google_Play_App , aes(x = Installs_Cat_2, y =log(Reviews), color = Category )) + geom_boxplot()
ggplot(Google_Play_App , aes(x = Installs_Cat_2, y = Rating2, color = Category)) + geom_boxplot()
ggplot(Google_Play_App , aes(y = Installs_Cat_2, x = Category , color = Rating2)) + geom_boxplot()

#Histogram
ggplot(Google_Play_App , aes(x = Category)) + geom_histogram(fill="blue",stat = "count")
ggplot(Google_Play_App , aes(x = Installs_Cat)) + geom_histogram(fill="blue",stat = "count")

#build Train and test subset
set.seed(1234)
split <- sample.split(Google_Play_App$Installs,SplitRatio = 0.75)
train <- subset(Google_Play_App,split==T)
test <- subset(Google_Play_App,split==F)

#GLM Model
logmodel <- glm(Installs_Cat ~ .-Installs -Installs_Cat_2- Year.Of.Update - Month.Of.Update   ,data=train, family="binomial")
summary(logmodel)
pred.log <- predict(logmodel,newdata=test,type="response")
table(pred.log > 0.7,test$Installs)
multiclass.roc(test$Installs, pred.log)


#Cross Validation 
numFolds <- trainControl(method="cv", number=10) 
cpGrid <- expand.grid(.cp=seq(0.001, 0.2, 0.001)) 
train(Installs_Cat ~ .-Installs-Installs_Cat_2 -Year.Of.Update - Month.Of.Update , data=train, method="rpart", trControl= numFolds, tuneGrid= cpGrid) 
CV.tree <- rpart(Installs_Cat ~ .-Installs- Year.Of.Update - Month.Of.Update  , data=train, method="class", cp=0.001) 
prp(CV.tree) 
pred.CV.tree <- predict(CV.tree, newdata=test, type="class") 
table(test$Installs_Cat , pred.CV.tree) 
multiclass.roc(test$Installs_Cat,as.numeric(pred.CV.tree) )

#Random Forest Model
RandomForest_mod <- randomForest(Installs_Cat ~ .-Installs-Installs_Cat_2-Year.Of.Update - Month.Of.Update   , data = train ,  nodsize=3, ntree=400)
RandomForest_mod
Predict_RandomForest_mod <- predict(RandomForest_mod, newdata = test)
Predict_RandomForest_mod
predict(RandomForest_mod, newdata = test)
multiclass.roc(test$Installs_Cat, as.numeric(Predict_RandomForest_mod))

#Regression
regmod <- lm(Installs ~ .-Installs_Cat -Installs_Cat_2- Year.Of.Update - Month.Of.Update   ,data=train)
summary(regmod)
plot(regmod)

