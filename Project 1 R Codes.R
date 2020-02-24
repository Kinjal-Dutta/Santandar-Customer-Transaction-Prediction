rm(list=ls(all=T))
setwd("C:/Users/KINJAL/Desktop/EdWisor/Project1")

## Read the data
train_data = read.csv("train.csv", header = TRUE)
test_data = read.csv("test.csv", header = TRUE)





#check dimesions ( number of row & columns) in data set
dim(train_data)

dim(test_data)

str(train_data)

table(is.na(train_data))

colSums(is.na(train_data))

summary(train_data)



#########################Graphical Representation##############################

ggplot(train_data, aes(x= var_10, y = target)) + geom_point(size = 2.5, color="navy") + xlab("var_10") + ylab("target") + ggtitle("var_10 vs target")




############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(train_data,is.numeric) #selecting only numeric

numeric_data = train_data[,numeric_index]

cnames = colnames(numeric_data)



##################################Feature Selection################################################
install.packages("corrgram")
library("corrgram")



## Correlation Plot 
corrgram(train_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(train_data,is.factor)
factor_data = train_data[,factor_index]


for (i in 1:10)
{
   print(names(factor_data)[0])
   print(chisq.test(table(factor_data$target,factor_data[,i])))
}

## Dimension Reduction
train_data = subset(train_data, 
                         select = -c(var_10, var_15, var_25, var_30, var_40))


##################################Feature Scaling################################################
#Normality check
qqnorm(train_data$var_20)
hist(train_data$target)

#Normalisation
cnames = c("var_10","var_15","var_20","var_30","var_40","var_45","var_50")

for(i in cnames){
   print(i)
   train_data[,i] = (train_data[,1] - min(train_data[,1]))/
      (max(train_data[,1] - min(train_data[,1])))
}

# #Standardisation
 for(i in cnames){
  print(i)
 train_data[,i] = (train_data[,1] - mean(train_data[,1]))/
                                 sd(train_data[,1])
}


#################################Logistic Regression######################################
logit_model = glm(target ~ ., data = train_data, family = "binomial")

#summary of the model
summary(logit_model)

#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test_data, type = "response")

logit_Predictions



#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)
logit_Predictions

##Evaluate the performance of classification model
ConfMatrix_RF = table(test_data$target, logit_Predictions)
ConfMatrix_RF

#False Negative rate
FNR = FN/FN+TP 

11577+12
11577+12+140+1270

11589/12999

1270/(1270+12)

#Accuracy: 89%
#FNR: 99%


###########Decision Tree#############################

install.packages("rpart")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("caret")
install.packages("DataCombine")
install.packages("C50")
install.packages("Metrics")


library(Metrics)
library(rpart)
library(e1071)
library(rpart.plot)
library("caret")
library("ggplot2")
library("DataCombine")
library("C50")
library("ggplot2")

train_data$target <- as.factor(train_data$target)
 
 ##Decision tree for classification
 #Develop Model on training data
 ###################################Decision Tree#######################################
 #Clean the environment
 rmExcept("train_data")
 
 #Divide data into train and test using stratified sampling method
 set.seed(1234)
 train.index = createDataPartition(train_data$target, p = .80, list = FALSE)
 train = train_data[ train.index,]
 test  = train_data[-train.index,]
 
 ##Decision tree for classification
 #Develop Model on training data
 C50_model = C5.0(target ~., train, trials = 100, rules = TRUE)
 
 #Summary of DT model
 summary(C50_model)
 
 #write rules into disk
 write(capture.output(summary(C50_model)), "c50Rules.txt")
 
 #Lets predict for test cases
 C50_Predictions = predict(C50_model, test[,-1], type = "class")
 
 ##Evaluate the performance of classification model
 ConfMatrix_C50 = table(test$target, C50_Predictions)
 confusionMatrix(ConfMatrix_C50)
 
 confusionMatrix
 
 
 #False Negative rate
 FNR = FN/FN+TP 
 
 #Accuracy = 90.23%
 #FNR = 98.04%
 
 
 
 
 #################################Random Forest############################################
 install.packages("randomForest")
 library("randomForest")
 install.packages("inTrees")
 library("inTrees")
 
 RF_model = randomForest(target ~ ., train_data, importance = TRUE, ntree = 500)
 
 #Extract rules fromn random forest
 #transform rf object to an inTrees' format
   treeList = RF2List(RF_model)  
 # 
 # #Extract rules
   exec = extractRules(treeList, train[,-17])  # R-executable conditions
 # 
 # #Visualize some rules
 exec[1:2,]
 # 
 # #Make rules more readable:
  readableRules = presentRules(exec, colnames(train_data))
  readableRules[1:2,]
 # 
 # #Get rule metrics
 ruleMetric = getRuleMetric(exec, train_data[,-1], train_data$target)  # get rule metrics
 # 
 # #evaulate few rules
 ruleMetric[1:2,]
 
 #Presdict test data using random forest model
 RF_Predictions = predict(RF_model, test[,-1])
 
 ##Evaluate the performance of classification model
 ConfMatrix_RF = table(test$target, RF_Predictions)
 confusionMatrix(ConfMatrix_RF)
 confusionMatrix
 
 
 #False Negative rate
 FNR = FN/FN+TP 
 
 #Accuracy = 100%
 #FNR = 0
 
 
 
 ################################naive Bayes############################################
 library(e1071)
 
 #Develop model
 NB_model = naiveBayes(target ~ ., data = train_data)
 
 #predict on test cases #raw
 NB_Predictions = predict(NB_model, test_data[,2:100], type = 'class')

 
 #Look at confusion matrix
 Conf_matrix = table(observed = test_data[,1], predicted = NB_Predictions)
 confusionMatrix(Conf_matrix)

 
 #Accuracy = 88.76%
 #FNR = 97.89%