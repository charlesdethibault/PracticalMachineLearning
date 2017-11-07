Summary
=======

In this research we attempt to compare the difference of performance between a decision tree and a random forest model.

For this research we use the HAR dataset for benchmarking.

We use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants and define if the exercise has been properly done. The testing data rate the execution as below:

\*exactly according to the specification (A)

\*throwing elbows to the front (B)

\*lifting the dumbbell only halfway (C)

\*lowering the dumbbell only halfway (D)

\*throwing the hips to the front (E)

We split the train data into 75%/25% to train our models on the first 75% and test our models on the last 25%.

The random forest performed much better as the accuracy was almost twice higher and the Kappa almost 3 times higher than the decision tree model.

load libraries
==============

``` r
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
set.seed(2017)
```

download train data
===================

``` r
urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
filename <- "pml-training.csv"
tmp <- tempfile()
if(!file.exists(filename))download.file(urltrain, destfile = filename)
data_train <- read.csv(filename, na.strings=c("", "NA", "#DIV/0!"), row.names = 1)
```

download test data
==================

``` r
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
filename <- "pml-testing.csv"
tmp <- tempfile()
if(!file.exists(filename))download.file(urltest, destfile = filename)
data_test <- read.csv(filename, na.strings=c("", "NA", "#DIV/0!"), row.names = 1)
```

remove na
=========

``` r
data_train_clean <- data_train[,!sapply(data_train,function(x) any(is.na(x)))]
data_test_clean <- data_test[,!sapply(data_test,function(x) any(is.na(x)))]
```

remove personal info from users which are not predicators
=========================================================

``` r
data_train_clean <-data_train_clean[,-c(1:6)]
data_test_clean <-data_test_clean[,-c(1:6)]
```

partiction a testing dataset so the acutal data\_test can be used as validation test.
=====================================================================================

``` r
inTrain <- createDataPartition(y=data_train_clean$classe, p=0.75, list=FALSE)
Training <- data_train_clean[inTrain, ] 
Testing <- data_train_clean[-inTrain, ]
```

predict using Decision tree
===========================

The decision tree does not provide great results as the accuracy is .4935 and the Kappa is .3376.

``` r
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
#plot(dt$finalModel, uniform=TRUE, main="Classification Tree")
#text(dt$finalModel, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(dt$finalModel)
```

![](FinalProject_files/figure-markdown_github/unnamed-chunk-7-1.png)

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

predict randomforest
====================

The model is loaded as it was saved earlier and it avoids high computation time. The file can be found in the repo.

As we can see the random forest provides much better prediction as the accuracy is .9898 and the Kappa is .9871 while for the decision tree, we had an accuracy of .4935 and a Kappa of .3376

``` r
#rf <- train(classe ~ .,method="rf",data=Training)
load(file = "rf.rda")
print(rf$finalModel)
```

    ## 
    ## Call:
    ##  randomForest(x = x, y = y, mtry = param$mtry) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 27
    ## 
    ##         OOB estimate of  error rate: 0.69%
    ## Confusion matrix:
    ##      A    B    C    D    E class.error
    ## A 4180    4    1    0    0 0.001194743
    ## B   21 2820    6    1    0 0.009831461
    ## C    0   15 2541   11    0 0.010128555
    ## D    0    0   29 2381    2 0.012852405
    ## E    0    1    4    7 2694 0.004434590

``` r
#plot(rf$finalModel, uniform=TRUE, main="Random forest")
#text(rf$finalModel, use.n=TRUE, all=TRUE, cex=.8)


varimp <- varImp(rf, varImp.train=FALSE)
plot(varimp, top=20)
```

![](FinalProject_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
prediction_rf <- predict(rf,newdata=Testing)
confusionMatrix(prediction_rf,Testing$classe)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    A    B    C    D    E
    ##          A 1394   12    0    0    0
    ##          B    0  935    5    1    1
    ##          C    0    2  847   14    3
    ##          D    0    0    3  789    8
    ##          E    1    0    0    0  889
    ## 
    ## Overall Statistics
    ##                                           
    ##                Accuracy : 0.9898          
    ##                  95% CI : (0.9866, 0.9924)
    ##     No Information Rate : 0.2845          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.9871          
    ##  Mcnemar's Test P-Value : NA              
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: A Class: B Class: C Class: D Class: E
    ## Sensitivity            0.9993   0.9852   0.9906   0.9813   0.9867
    ## Specificity            0.9966   0.9982   0.9953   0.9973   0.9998
    ## Pos Pred Value         0.9915   0.9926   0.9781   0.9862   0.9989
    ## Neg Pred Value         0.9997   0.9965   0.9980   0.9963   0.9970
    ## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
    ## Detection Rate         0.2843   0.1907   0.1727   0.1609   0.1813
    ## Detection Prevalence   0.2867   0.1921   0.1766   0.1631   0.1815
    ## Balanced Accuracy      0.9979   0.9917   0.9930   0.9893   0.9932

References
==========

Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements. Proceedings of 21st Brazilian Symposium on Artificial Intelligence. Advances in Artificial Intelligence - SBIA 2012. In: Lecture Notes in Computer Science. , pp. 52-61. Curitiba, PR: Springer Berlin / Heidelberg, 2012. ISBN 978-3-642-34458-9. DOI: 10.1007/978-3-642-34459-6\_6.
