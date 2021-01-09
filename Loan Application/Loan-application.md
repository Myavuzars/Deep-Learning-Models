---
title: "Loan approval prediction"
author: "Mustafa Arslan"
date: "1/5/2021"
output:  
   html_document:
     keep_md: true
---





```r
library(keras)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyverse)
```

```
## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
```

```
## v ggplot2 3.3.2     v purrr   0.3.4
## v tibble  3.0.4     v stringr 1.4.0
## v tidyr   1.1.2     v forcats 0.5.0
## v readr   1.4.0
```

```
## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(recipes)
```

```
## 
## Attaching package: 'recipes'
```

```
## The following object is masked from 'package:stringr':
## 
##     fixed
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
library(randomForest)
```

```
## randomForest 4.6-14
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
```

```
## 
## Attaching package: 'pROC'
```

```
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```



```r
df<- read.csv("loan_train.csv")
head(df)
```

```
##   MIS_Status LoanNr_ChkDgt                           Name            City State
## 1      P I F    1122875008  Patty Schwartzkopf Properties   MAMMOTH LAKES    CA
## 2      P I F    4549584000 HAM DELLES COMPANY INCORPORATE      SANTA ROSA    CA
## 3      P I F    7408914003 MEGA CORPORATION GROUP,A CALIF          WALNUT    CA
## 4      P I F    4579774010    STICKS-N-STONES ENTERPRISES      SAN CARLOS    CA
## 5      P I F    6266254005        PREMIERE PROPERTIES INC      BURLINGAME    CA
## 6     CHGOFF    2735566002       D BOOKER ENTERPRISES INC SAN LUIS OBISPO    CA
##     Zip                           Bank BankState  NAICS ApprovalDate ApprovalFY
## 1 93546     BANK OF AMERICA NATL ASSOC        NC 531210        16413       2005
## 2 95401                    UMPQUA BANK        OR 531312        15126       2001
## 3 91789     BANK OF AMERICA NATL ASSOC        NC 532230        16195       2004
## 4 94070     WACHOVIA SBA LENDING, INC.        CA 531312        15141       2001
## 5 94010 U.S. BANK NATIONAL ASSOCIATION        OH 531210        15818       2003
## 6 93401 JPMORGAN CHASE BANK NATL ASSOC        IL 531210        17254       2007
##   Term NoEmp NewExist CreateJob RetainedJob FranchiseCode UrbanRural RevLineCr
## 1   84     2        1         0           0             1          2         Y
## 2  300     7        1         0           7             1          1         0
## 3   84     3        1         0           3             1          1         Y
## 4  300    10        1         0           0             1          1         N
## 5  300     2        1         0           2             1          1         0
## 6   46     1        1         1           1             1          2         Y
##   LowDoc DisbursementDate DisbursementGross BalanceGross  GrAppv SBA_Appv New
## 1      N            16587             11000            0   11000     5500   0
## 2      N            15156            866800            0  866800   650100   0
## 3      N            16222             77377            0   85000    42500   0
## 4      N            15218            800100            0  810000   607500   0
## 5      N            15917           1054200            0 1056200   766917   0
## 6      N            17256             98180            0   50000    25000   0
##   RealEstate   Portion Recession daysterm    xx
## 1          0 0.5000000         0     2520 19107
## 2          1 0.7500000         0     9000 24156
## 3          0 0.5000000         0     2520 18742
## 4          1 0.7500000         0     9000 24218
## 5          1 0.7261096         0     9000 24917
## 6          0 0.5000000         0     1380 18636
```



```r
# Data Cleaning 1

#Assigning Missing values to Median values within the observation
median1<- median(df[,"DisbursementDate"], na.rm=T)
median2<- median(df[,"xx"], na.rm=T)
df[is.na(df$DisbursementDate),"DisbursementDate" ] <- median1
df[is.na(df$xx),"xx" ] <- median2 
df[is.na(df$NewExist),"NewExist"] <- 0
```



```r
#Data Cleaning 2
str(df)
```

```
## 'data.frame':	1102 obs. of  31 variables:
##  $ MIS_Status       : chr  "P I F" "P I F" "P I F" "P I F" ...
##  $ LoanNr_ChkDgt    : num  1.12e+09 4.55e+09 7.41e+09 4.58e+09 6.27e+09 ...
##  $ Name             : chr  "Patty Schwartzkopf Properties" "HAM DELLES COMPANY INCORPORATE" "MEGA CORPORATION GROUP,A CALIF" "STICKS-N-STONES ENTERPRISES" ...
##  $ City             : chr  "MAMMOTH LAKES" "SANTA ROSA" "WALNUT" "SAN CARLOS" ...
##  $ State            : chr  "CA" "CA" "CA" "CA" ...
##  $ Zip              : int  93546 95401 91789 94070 94010 93401 90201 93638 95765 91360 ...
##  $ Bank             : chr  "BANK OF AMERICA NATL ASSOC" "UMPQUA BANK" "BANK OF AMERICA NATL ASSOC" "WACHOVIA SBA LENDING, INC." ...
##  $ BankState        : chr  "NC" "OR" "NC" "CA" ...
##  $ NAICS            : int  531210 531312 532230 531312 531210 531210 531210 531210 532490 531120 ...
##  $ ApprovalDate     : int  16413 15126 16195 15141 15818 17254 16161 17066 14868 15895 ...
##  $ ApprovalFY       : int  2005 2001 2004 2001 2003 2007 2004 2006 2000 2003 ...
##  $ Term             : int  84 300 84 300 300 46 294 289 60 120 ...
##  $ NoEmp            : int  2 7 3 10 2 1 3 5 5 5 ...
##  $ NewExist         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ CreateJob        : int  0 0 0 0 0 1 3 2 0 5 ...
##  $ RetainedJob      : int  0 7 3 0 2 1 3 5 5 5 ...
##  $ FranchiseCode    : int  1 1 1 1 1 1 1 68905 1 1 ...
##  $ UrbanRural       : int  2 1 1 1 1 2 1 1 1 1 ...
##  $ RevLineCr        : chr  "Y" "0" "Y" "N" ...
##  $ LowDoc           : chr  "N" "N" "N" "N" ...
##  $ DisbursementDate : num  16587 15156 16222 15218 15917 ...
##  $ DisbursementGross: int  11000 866800 77377 800100 1054200 98180 270000 1480000 64270 200000 ...
##  $ BalanceGross     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ GrAppv           : int  11000 866800 85000 810000 1056200 50000 270000 1493000 50000 200000 ...
##  $ SBA_Appv         : int  5500 650100 42500 607500 766917 25000 202500 1119750 25000 100000 ...
##  $ New              : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ RealEstate       : int  0 1 0 1 1 0 1 1 0 0 ...
##  $ Portion          : num  0.5 0.75 0.5 0.75 0.726 ...
##  $ Recession        : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ daysterm         : int  2520 9000 2520 9000 9000 1380 8820 8670 1800 3600 ...
##  $ xx               : num  19107 24156 18742 24218 24917 ...
```

```r
df1 <- df %>%
  select(-c(LoanNr_ChkDgt, City, State, Zip, Name, Bank, BankState,NAICS, BalanceGross,NoEmp)) %>%
  mutate_at(vars(MIS_Status,Recession,UrbanRural,RevLineCr,
                 LowDoc,New,RealEstate,NewExist), list(factor))
```



```r
#Data Preparation1
#Set levels for RevLineCr  & LowDoc & MIS_STATUS
levels(df1$RevLineCr) <- c("0","0","N","T","Y")
levels(df1$LowDoc)<-c("N","N","N","N","S","Y")
levels(df1$MIS_Status) <-c("No","Yes")
```



```r
#Data Preparation 2
hist(df1$Portion)
```

![](README_figs/README-unnamed-chunk-7-1.png)<!-- -->

```r
df1$Portion <- cut(x = df1$Portion, breaks = c(0,0.51,0.74,0.84,0.86,0.92,1.1),
                   labels = c("A1","A2", "A3","A4","A5", "A6") )
hist(df1$FranchiseCode)
```

![](README_figs/README-unnamed-chunk-7-2.png)<!-- -->

```r
df1$FranchiseCode <- cut(x = df1$FranchiseCode, breaks = c(-.5,0.5,1.5,Inf),
                         labels = c("L1","L2", "L3") )
hist(df1$ApprovalFY)
```

![](README_figs/README-unnamed-chunk-7-3.png)<!-- -->

```r
df1$ApprovalFY <- cut(df1$ApprovalFY, 4, include.lowest=TRUE, 
                      labels=c("A1","A2", "A3","A4") )
hist(df1$Term)
```

![](README_figs/README-unnamed-chunk-7-4.png)<!-- -->

```r
df1$Term <- cut(df1$Term, breaks = c(0,35,37,59,61,83,85,95,97,119,Inf),
                include.lowest=TRUE,labels=c(1:10))
hist(df1$CreateJob)
```

![](README_figs/README-unnamed-chunk-7-5.png)<!-- -->

```r
df1$CreateJob <- cut(df1$CreateJob, breaks= c(0,0.5,3,Inf), include.lowest=TRUE, 
                     labels=c("A1","A2", "A3") )
hist(df1$RetainedJob)
```

![](README_figs/README-unnamed-chunk-7-6.png)<!-- -->

```r
df1$RetainedJob <- cut(df1$RetainedJob, breaks= c(0,0.5,Inf), include.lowest=TRUE, 
                       labels=c("A1","A2") )

hist(df1$daysterm)
```

![](README_figs/README-unnamed-chunk-7-7.png)<!-- -->

```r
df1$daysterm <- cut(df1$daysterm, 
                    breaks = c(0,1070,1090,1640,1771,1810,2510,2530,3500,7000,Inf), 
                    include.lowest=TRUE, labels = c(1:10))

hist(df1$DisbursementDate)
```

![](README_figs/README-unnamed-chunk-7-8.png)<!-- -->

```r
df1$DisbursementDate <- cut(df1$DisbursementDate, 
                            breaks = c(0,16420,Inf),labels = c("A1","A2"),
                            include.lowest=TRUE)
hist(df1$ApprovalDate)
```

![](README_figs/README-unnamed-chunk-7-9.png)<!-- -->

```r
df1$ApprovalDate <- cut(df1$ApprovalDate, 
                        breaks = c(0,11320,11925,16045,16282,Inf), 
                        labels= c("A1","A2","A3","A4","A5"),include.lowest=TRUE)
hist(df1$xx)
```

![](README_figs/README-unnamed-chunk-7-10.png)<!-- -->

```r
df1$xx <- cut(df1$xx, breaks = c(0,16750,18153,Inf), 
              labels= c("A1","A2","A3"),include.lowest=TRUE)
hist(df1$DisbursementGross)
```

![](README_figs/README-unnamed-chunk-7-11.png)<!-- -->

```r
df1$DisbursementGross<- cut(df1$DisbursementGross,
                            breaks = c(0,9450,35100,42480,49600,140000,Inf), 
                            labels= c("A1","A2","A3","A4","A5","A6"),include.lowest=TRUE)
hist(df1$SBA_Appv)
```

![](README_figs/README-unnamed-chunk-7-12.png)<!-- -->

```r
df1$SBA_Appv<- cut(df1$SBA_Appv,breaks = c(0,2600,17400,55000,Inf), 
                   labels= c("A1","A2","A3","A4"),include.lowest=TRUE)
hist(df1$GrAppv)
```

![](README_figs/README-unnamed-chunk-7-13.png)<!-- -->

```r
df1$GrAppv<- cut(df1$GrAppv,breaks = c(0,5100,16800,24900,34000,61000,99000,120000,Inf), 
                 labels= c("A1","A2","A3","A4","A5","A6","A7","A8"),
                 include.lowest=TRUE)
```



```r
#  Data Partition
train_index =createDataPartition(df1$MIS_Status, p=.8,
                                 times=1,
                                 list=FALSE)
df1.train <- df1[train_index,]
df1.test  <- df1[-train_index,]

dim(df1)
```

```
## [1] 1102   21
```

```r
dim(df1.train)
```

```
## [1] 882  21
```

```r
dim(df1.test)
```

```
## [1] 220  21
```



```r
#Preprocess the Data with Recipe Library
rec_obj <- recipe(MIS_Status ~ ., data = df1.train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(data = df1.train)
rec_obj
```

```
## Data Recipe
## 
## Inputs:
## 
##       role #variables
##    outcome          1
##  predictor         20
## 
## Training data contained 882 data points and no missing data.
## 
## Operations:
## 
## Dummy variables from ApprovalDate, ApprovalFY, Term, NewExist, ... [trained]
```

```r
# Predictors
x_train <- bake(rec_obj, new_data = df1.train) %>% select(- MIS_Status)
x_test  <- bake(rec_obj, new_data = df1.test)  %>% select(- MIS_Status)

#Target
y_train <- ifelse(pull(df1.train, MIS_Status) == "Yes", 1, 0)
y_test  <- ifelse(pull(df1.test,  MIS_Status) == "Yes", 1, 0)
```



```r
#Create Model
model1 <- keras_model_sequential()
```

```
## Warning in normalizePath(path.expand(path), winslash, mustWork): path[1]="C:
## \Users\myavu\Anacon\envs\rstudio-R/python.exe": The system cannot find the file
## specified

## Warning in normalizePath(path.expand(path), winslash, mustWork): path[1]="C:
## \Users\myavu\Anacon\envs\rstudio-R/python.exe": The system cannot find the file
## specified
```

```r
#this variable(model) contains instructions below
model1 %>%
  layer_dense(units= 16,
              activation='relu',
              kernel_initializer = 'uniform',
              input_shape = ncol(x_train)) %>% # input layer
  
  layer_dropout(rate = 0.2) %>%         

  # layer_dense(units= 64,
  #             kernel_initializer = 'uniform',
  #             activation='relu') %>%               # 2nd hidden layer
  
  layer_dropout(rate = 0.4) %>%                    # Hidden layer for preventing overfitting
  
  layer_dense(units= 8,
              kernel_initializer = 'uniform',
              activation='relu') %>%               # 2nd hidden layer
  
  layer_dense(units=1,
              activation = 'sigmoid') %>%          # output layer
  # sigmoid is common for binary classification
  
  #Compile Model
  
  compile(optimizer =optimizer_adam(lr=0.003),
          loss = 'binary_crossentropy',
          metrics=c('accuracy'))

model1
```

```
## Model
## Model: "sequential"
## ________________________________________________________________________________
## Layer (type)                        Output Shape                    Param #     
## ================================================================================
## dense_2 (Dense)                     (None, 16)                      1040        
## ________________________________________________________________________________
## dropout_1 (Dropout)                 (None, 16)                      0           
## ________________________________________________________________________________
## dropout (Dropout)                   (None, 16)                      0           
## ________________________________________________________________________________
## dense_1 (Dense)                     (None, 8)                       136         
## ________________________________________________________________________________
## dense (Dense)                       (None, 1)                       9           
## ================================================================================
## Total params: 1,185
## Trainable params: 1,185
## Non-trainable params: 0
## ________________________________________________________________________________
```



```r
# Fit the keras model to the training data
history <- fit(
  object           = model1, 
  x                = as.matrix(x_train), 
  y                = y_train,
  batch_size       = 50, 
  epochs           = 30,
  validation_split = 0.30
)

plot(history)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](README_figs/README-unnamed-chunk-11-1.png)<!-- -->

```r
print(history)
```

```
## 
## Final epoch (plot to see history):
##         loss: 0.3808
##     accuracy: 0.8606
##     val_loss: 0.4689
## val_accuracy: 0.8226
```



```r
#Predicting the Test Data

#Probability prediction

x_test_prob <- predict_proba(object = model1, x = as.matrix(x_test)) %>%
  as.vector()

#Class prediction
x_test_class<- predict_classes(object = model1, x = as.matrix(x_test)) %>%
  as.vector()



y_test_class<-as.factor(y_test)
x_test_class <- as.factor(x_test_class)


Pred_Table<- table(y_test_class,x_test_class)
Pred_Table
```

```
##             x_test_class
## y_test_class   0   1
##            0  61  23
##            1  21 115
```

```r
confusionMatrix(x_test_class,y_test_class)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction   0   1
##          0  61  21
##          1  23 115
##                                          
##                Accuracy : 0.8            
##                  95% CI : (0.741, 0.8508)
##     No Information Rate : 0.6182         
##     P-Value [Acc > NIR] : 4.946e-09      
##                                          
##                   Kappa : 0.5744         
##                                          
##  Mcnemar's Test P-Value : 0.8802         
##                                          
##             Sensitivity : 0.7262         
##             Specificity : 0.8456         
##          Pos Pred Value : 0.7439         
##          Neg Pred Value : 0.8333         
##              Prevalence : 0.3818         
##          Detection Rate : 0.2773         
##    Detection Prevalence : 0.3727         
##       Balanced Accuracy : 0.7859         
##                                          
##        'Positive' Class : 0              
## 
```



```r
#RANDOM FOREST


model2 <- randomForest(MIS_Status~.,data=df1.train, importance=T, mtry=2)
model2
```

```
## 
## Call:
##  randomForest(formula = MIS_Status ~ ., data = df1.train, importance = T,      mtry = 2) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 16.78%
## Confusion matrix:
##      No Yes class.error
## No  238  98  0.29166667
## Yes  50 496  0.09157509
```

```r
prediction1 <- predict(model2, df1.test)
confusionMatrix(prediction1, df1.test$MIS_Status)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  No Yes
##        No   56  14
##        Yes  28 122
##                                           
##                Accuracy : 0.8091          
##                  95% CI : (0.7508, 0.8588)
##     No Information Rate : 0.6182          
##     P-Value [Acc > NIR] : 7.514e-10       
##                                           
##                   Kappa : 0.5823          
##                                           
##  Mcnemar's Test P-Value : 0.04486         
##                                           
##             Sensitivity : 0.6667          
##             Specificity : 0.8971          
##          Pos Pred Value : 0.8000          
##          Neg Pred Value : 0.8133          
##              Prevalence : 0.3818          
##          Detection Rate : 0.2545          
##    Detection Prevalence : 0.3182          
##       Balanced Accuracy : 0.7819          
##                                           
##        'Positive' Class : No              
## 
```



```r
#LOGISTIC REGRESSION
str(df1.train)
```

```
## 'data.frame':	882 obs. of  21 variables:
##  $ MIS_Status       : Factor w/ 2 levels "No","Yes": 2 2 2 1 2 1 2 2 2 2 ...
##  $ ApprovalDate     : Factor w/ 5 levels "A1","A2","A3",..: 5 3 3 5 4 5 3 3 5 5 ...
##  $ ApprovalFY       : Factor w/ 4 levels "A1","A2","A3",..: 3 3 3 4 3 3 3 2 4 4 ...
##  $ Term             : Factor w/ 10 levels "1","2","3","4",..: 6 10 10 3 10 10 6 10 4 6 ...
##  $ NewExist         : Factor w/ 2 levels "1","2": 1 1 1 1 1 1 1 1 1 2 ...
##  $ CreateJob        : Factor w/ 3 levels "A1","A2","A3": 1 1 1 2 2 2 1 2 1 1 ...
##  $ RetainedJob      : Factor w/ 2 levels "A1","A2": 1 2 1 2 2 2 1 2 2 2 ...
##  $ FranchiseCode    : Factor w/ 3 levels "L1","L2","L3": 2 2 2 2 2 3 2 3 2 1 ...
##  $ UrbanRural       : Factor w/ 3 levels "0","1","2": 3 2 2 3 2 2 2 2 2 2 ...
##  $ RevLineCr        : Factor w/ 4 levels "0","N","T","Y": 4 1 2 4 1 1 2 2 1 4 ...
##  $ LowDoc           : Factor w/ 3 levels "N","S","Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ DisbursementDate : Factor w/ 2 levels "A1","A2": 2 1 1 2 1 2 1 1 2 2 ...
##  $ DisbursementGross: Factor w/ 6 levels "A1","A2","A3",..: 2 6 6 5 6 6 6 6 2 6 ...
##  $ GrAppv           : Factor w/ 8 levels "A1","A2","A3",..: 2 8 8 5 8 8 8 8 4 8 ...
##  $ SBA_Appv         : Factor w/ 4 levels "A1","A2","A3",..: 2 4 4 3 4 4 4 4 2 4 ...
##  $ New              : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 2 ...
##  $ RealEstate       : Factor w/ 2 levels "0","1": 1 2 2 1 2 2 1 2 1 1 ...
##  $ Portion          : Factor w/ 6 levels "A1","A2","A3",..: 1 3 3 1 3 3 3 6 1 1 ...
##  $ Recession        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 1 1 1 ...
##  $ daysterm         : Factor w/ 10 levels "1","2","3","4",..: 7 10 10 3 10 10 7 10 5 7 ...
##  $ xx               : Factor w/ 3 levels "A1","A2","A3": 3 3 3 3 3 3 2 3 3 3 ...
```

```r
df1.train<- df1.train%>%
  select(-c(LowDoc))
df1.test<- df1.test%>%
  select(-c(LowDoc))


model3 <- glm(MIS_Status~.,data=df1.train, family= binomial(logit))
model3
```

```
## 
## Call:  glm(formula = MIS_Status ~ ., family = binomial(logit), data = df1.train)
## 
## Coefficients:
##         (Intercept)       ApprovalDateA2       ApprovalDateA3  
##           16.232096           -18.247490           -16.130342  
##      ApprovalDateA4       ApprovalDateA5         ApprovalFYA2  
##          -16.286255           -17.212429             0.645783  
##        ApprovalFYA3         ApprovalFYA4                Term2  
##           -0.652213            -1.079699             1.066759  
##               Term3                Term4                Term5  
##            0.080183             0.195802             0.572928  
##               Term6                Term7                Term8  
##            2.204008             0.108713             0.362853  
##               Term9               Term10            NewExist2  
##            0.294528             2.154498             0.106878  
##         CreateJobA2          CreateJobA3        RetainedJobA2  
##           -0.591030             0.001355            -0.284686  
##     FranchiseCodeL2      FranchiseCodeL3          UrbanRural1  
##            0.287385            -0.815125             0.690567  
##         UrbanRural2           RevLineCrN           RevLineCrT  
##            1.564749             0.199494             0.325765  
##          RevLineCrY   DisbursementDateA2  DisbursementGrossA2  
##            0.110107             0.149354            -0.808817  
## DisbursementGrossA3  DisbursementGrossA4  DisbursementGrossA5  
##           -0.572368             0.141640            -0.835640  
## DisbursementGrossA6             GrAppvA2             GrAppvA3  
##           -0.140761             0.007144            -0.664099  
##            GrAppvA4             GrAppvA5             GrAppvA6  
##            0.295839            -0.224242            -0.031670  
##            GrAppvA7             GrAppvA8           SBA_AppvA2  
##            0.075904             0.687846            -0.298990  
##          SBA_AppvA3           SBA_AppvA4                 New1  
##           -0.336600             0.537460                   NA  
##         RealEstate1            PortionA2            PortionA3  
##           -0.166631            13.952689            -1.218193  
##           PortionA4            PortionA5            PortionA6  
##           -0.466925             0.125174            -0.423632  
##          Recession1            daysterm2            daysterm3  
##           -0.415787             0.277854             0.337567  
##           daysterm4            daysterm5            daysterm6  
##                  NA             3.461472                   NA  
##           daysterm7            daysterm8            daysterm9  
##            1.668918                   NA                   NA  
##          daysterm10                 xxA2                 xxA3  
##                  NA            -0.348070            -0.037581  
## 
## Degrees of Freedom: 881 Total (i.e. Null);  825 Residual
## Null Deviance:	    1172 
## Residual Deviance: 658.3 	AIC: 772.3
```

```r
p2 <- predict(model3,df1.test,  type='response')
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
## prediction from a rank-deficient fit may be misleading
```

```r
glm.pred =rep("No",220)
glm.pred[p2>=0.5]="Yes"
glm.pred <- as.factor(glm.pred)

length(df1.test$MIS_Status)
```

```
## [1] 220
```

```r
length(glm.pred)
```

```
## [1] 220
```

```r
confusionMatrix(glm.pred,df1.test$MIS_Status)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  No Yes
##        No   60  17
##        Yes  24 119
##                                           
##                Accuracy : 0.8136          
##                  95% CI : (0.7558, 0.8628)
##     No Information Rate : 0.6182          
##     P-Value [Acc > NIR] : 2.806e-10       
##                                           
##                   Kappa : 0.5988          
##                                           
##  Mcnemar's Test P-Value : 0.3487          
##                                           
##             Sensitivity : 0.7143          
##             Specificity : 0.8750          
##          Pos Pred Value : 0.7792          
##          Neg Pred Value : 0.8322          
##              Prevalence : 0.3818          
##          Detection Rate : 0.2727          
##    Detection Prevalence : 0.3500          
##       Balanced Accuracy : 0.7946          
##                                           
##        'Positive' Class : No              
## 
```



```r
#AUC & ROC 
roc(df1.train$MIS_Status,model3$fitted.values , plot=T)
```

```
## Setting levels: control = No, case = Yes
```

```
## Setting direction: controls < cases
```

![](README_figs/README-unnamed-chunk-15-1.png)<!-- -->

```
## 
## Call:
## roc.default(response = df1.train$MIS_Status, predictor = model3$fitted.values,     plot = T)
## 
## Data: model3$fitted.values in 336 controls (df1.train$MIS_Status No) < 546 cases (df1.train$MIS_Status Yes).
## Area under the curve: 0.9039
```

