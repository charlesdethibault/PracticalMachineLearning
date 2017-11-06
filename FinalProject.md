``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(randomForest)
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
library(rpart)
library(rpart.plot)
set.seed(2017)
```

download train data
===================

``` r
urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
filename <- "pml-training.csv"
tmp <- tempfile()
if(!file.exists(filename))download.file(urltrain, destfile = filename)
#data_train <- read.csv(filename)
data_train <- read.csv(filename, na.strings=c("", "NA", "#DIV/0!"), row.names = 1)
```

``` r
#download test data
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filename <- "pml-testing.csv"
tmp <- tempfile()
if(!file.exists(filename))download.file(urltest, destfile = filename)
data_test <- read.csv(filename, na.strings=c("", "NA", "#DIV/0!"), row.names = 1)
```

``` r
# remove na
data_train_clean <- data_train[,!sapply(data_train,function(x) any(is.na(x)))]
data_test_clean <- data_test[,!sapply(data_test,function(x) any(is.na(x)))]
```

``` r
# remove personal info from users which are not predicators
data_train_clean <-data_train_clean[,-c(1:6)]
data_test_clean <-data_test_clean[,-c(1:6)]
```

``` r
#partiction a testing dataset so the acutal data_test can be used as validation test.
inTrain <- createDataPartition(y=data_train_clean$classe, p=0.75, list=FALSE)
Training <- data_train_clean[inTrain, ] 
Testing <- data_train_clean[-inTrain, ]
```

``` r
# predcit using Decision tree
dt <- train(classe ~ .,method="rpart",data=Training)
print(dt$finalModel)
```

    ## n= 14718 
    ## 
    ## node), split, n, loss, yval, (yprob)
    ##       * denotes terminal node
    ## 
    ##  1) root 14718 10533 A (0.28 0.19 0.17 0.16 0.18)  
    ##    2) roll_belt< 130.5 13488  9314 A (0.31 0.21 0.19 0.18 0.11)  
    ##      4) pitch_forearm< -33.95 1176     6 A (0.99 0.0051 0 0 0) *
    ##      5) pitch_forearm>=-33.95 12312  9308 A (0.24 0.23 0.21 0.2 0.12)  
    ##       10) magnet_dumbbell_y< 439.5 10410  7457 A (0.28 0.18 0.24 0.19 0.11)  
    ##         20) roll_forearm< 120.5 6365  3744 A (0.41 0.18 0.18 0.16 0.06) *
    ##         21) roll_forearm>=120.5 4045  2736 C (0.082 0.18 0.32 0.23 0.18) *
    ##       11) magnet_dumbbell_y>=439.5 1902   932 B (0.027 0.51 0.044 0.23 0.19) *
    ##    3) roll_belt>=130.5 1230    11 E (0.0089 0 0 0 0.99) *

``` r
plot(dt$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(dt$finalModel, use.n=TRUE, all=TRUE, cex=.8)
```

![](FinalProject_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
library(rattle)
```

    ## Rattle: A free graphical interface for data science with R.
    ## Version 5.1.0 Copyright (c) 2006-2017 Togaware Pty Ltd.
    ## Type 'rattle()' to shake, rattle, and roll your data.

    ## 
    ## Attaching package: 'rattle'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     importance

``` r
fancyRpartPlot(dt$finalModel)
```

![](FinalProject_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
prediction_dt <- predict(dt,newdata=Testing)
confusionMatrix(prediction_dt,Testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1270  386  408  364  142
    ##          B   30  316   25  138  118
    ##          C   92  247  422  302  229
    ##          D    0    0    0    0    0
    ##          E    3    0    0    0  412
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.4935          
    ##                  95% CI : (0.4794, 0.5076)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.3376          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9104  0.33298  0.49357   0.0000  0.45727
    ## Specificity            0.6295  0.92137  0.78513   1.0000  0.99925
    ## Pos Pred Value         0.4942  0.50399  0.32663      NaN  0.99277
    ## Neg Pred Value         0.9464  0.85200  0.88012   0.8361  0.89107
    ## Prevalence             0.2845  0.19352  0.17435   0.1639  0.18373
    ## Detection Rate         0.2590  0.06444  0.08605   0.0000  0.08401
    ## Detection Prevalence   0.5241  0.12785  0.26346   0.0000  0.08462
    ## Balanced Accuracy      0.7700  0.62717  0.63935   0.5000  0.72826
