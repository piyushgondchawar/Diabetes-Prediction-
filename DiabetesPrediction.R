---
  title: "Predict the onset of diabetes based on diagnostic measures"
author: "Parthasarathi Samantaray"
date: "16/03/2019"
output: word_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

### Dataset

This dataset is originally from the National Institute of Diabetes and Digestive and Kidney Diseases. The objective of the dataset is to diagnostically predict whether or not a patient has diabetes, based on certain diagnostic measurements included in the dataset. Several constraints were placed on the selection of these instances from a larger database. In particular, all patients here are females at least 21 years old of Pima Indian heritage.

Columns

Pregnancies - Number of times pregnant
Glucose - Plasma glucose concentration a 2 hours in an oral glucose tolerance test
BloodPressure - Diastolic blood pressure (mm Hg)
SkinThickness - Triceps skin fold thickness (mm)
Insulin - 2-Hour serum insulin (mu U/ml)
BMI - Body mass index (weight in kg/(height in m)^2)
Diabetes -PedigreeFunction - Diabetes pedigree function
Age - Age (years)
Outcome - Class variable (0 or 1) 268 of 768 are 1, the others are 0


```{r,echo=FALSE}
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(Metrics) # for rmse calculation
library(gbm)
library(randomForest)
library(caTools)
library(caret)
```

```{r}
diabetes <- read_csv("diabetes.csv")
```
```{r}
diabetes %>% summary()
```
```{r}
diabetes$Outcome %>% table()
```

Observations:-
  * Outcome is not a factor
* There are 0 values for BMI , BloodPressure, Glucose and SkinThickness
Removing only records with zero BMI and BloodPressure
For 11 records the BMI is zero and 35 records the BloodPressure is zero

```{r}
diabetes$Outcome <- as.factor(diabetes$Outcome)
```
```{r}
diabetes %>% filter(BMI ==0) %>% count()
diabetes %>% filter(BloodPressure ==0) %>% count()
diabetes %>% filter(SkinThickness ==0) %>% count()
```
```{r}
diabetes<-diabetes %>% filter(BMI !=0)
diabetes<-diabetes %>% filter(BloodPressure !=0)
#diabetes<-diabetes %>% filter(SkinThickness !=0)
```
```{r}
diabetes%>% summary()
```

```{r}
diabetes$Outcome %>% table()
```
The summary don't show any missing values. After explicite checking for NA values,I didn't got any missing values.

```{r}
diabetes %>% is.na()%>% sum()
```
```{r}
diabetes %>% dim()
```
```{r}
# To make th eoutputs reproducable 
set.seed(101)
```

I have used the split fucntion of caTools package and split the diabetes data set with 70% to train and 30% to test.

```{r}
samplelist <- sample.split(diabetes, SplitRatio = 0.7)
train <- subset(diabetes,samplelist == T)
test <- subset(diabetes , samplelist ==F)
```
Dimension check 
```{r}
train %>% dim()
test %>% dim()
```

### Logistic Regression

```{r, echo=FALSE}
glmmodel <- glm(Outcome ~. , data = train, family = "binomial")
```
```{r}
glmmodel %>% summary()
```

*Accurarcy Test* 
  
  ```{r, echo=FALSE}
glmpred<- predict(glmmodel,newdata = test, type = "response")
```

```{r}
plot(glmpred, col= test$Outcome , main = "Comparision of Logistic probability vs actual outcome")
```
By visual inspection, one can see the red colors are with diabetes.It seems logistic regression has a high Type I and Type II error.

```{r}
confusionMatrix(factor(ifelse(glmpred>0.5,1,0)), test$Outcome)
```

### Decision tree

```{r}
dtmodel<-rpart(Outcome ~., train, method = "class")
```

```{r}
rpart.plot(x=dtmodel, yesno=2, type=0, extra = 0)
#rpart.plot(x=dtmodel,yesno=2, type = 0)
```
*Prediction & Confusion Matrix*
  ```{r}
dtpred<- predict(dtmodel,test, type = "class")
```

```{r}
confusionMatrix(dtpred,test$Outcome)
```
*Post pruning*
  ```{r}
plotcp(dtmodel)
print(dtmodel$cptable)
```
xerror is lowest for cp =0.01
```{r}
dt_opt <- prune(tree = dtmodel, cp=0.01)
rpart.plot(x=dt_opt, yesno=2, type=0, extra = 0)
confusionMatrix(predict(dt_opt,test, type = "class"),test$Outcome)
```

Pruning has not resulted in any improvement in accurarcy.

### Random Forest 

```{r}
rfmodel<- randomForest(Outcome~. , data = diabetes)
```
*Accurarcy*
  ```{r}
err<- rfmodel$err.rate
oob_err <- err[nrow(err),"OOB"]
paste0("The OOB Accurarcy is :", 1-oob_err)
```


```{r}
plot(rfmodel)
legend(x="right", legend = colnames(err),fill=1:ncol(err))
```

Random Forest is the most accurate model compared to logistic and decision tree modelswith an accurarcy of 77.23%

## Minor Assignment

*Function for RMSE*
  ```{r}
rmsefun <- function (x, y){
  return(sqrt(mean((x-y)^2)))
}
```
```{r}
rmse(diabetes$BMI,diabetes$Age)
rmsefun(diabetes$BMI,diabetes$Age)
```


*Function for coefficient of variation*
  ```{r}
library(stats)
```


```{r}
coefvariation <- function(x){
  return (sd(x,na.rm = TRUE)/mean(x,na.rm = TRUE))
} 
```

```{r}
coefvariation(diabetes$BMI)
```


*Function Z-Score *
  ```{r}
zscore <- function(x, mean=0,std=1 ){
  return((x-mean)/std)
}
```
```{r}
zscore(1)
zscore(5,3,1) # Z score should be 2 
```