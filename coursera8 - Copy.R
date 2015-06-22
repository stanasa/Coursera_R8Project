#'---
#' title: "Practical Machine Learning Coursera (R8)"
#' author: "Serban Tanasa"
#' date: "June 21, 2015"
#' output: html_document
#'---

#' ##Coursera JHBSPH Practical Machine Learning Course
#' 
#' The purpose of the course project was to learn the basics of machine learning
#' in R. In the context of this course this was accomplished throught the use of 
#' the `caret` wrapper package that allows coding simple calls to complex machine 
#' learning algorithms such as Naive Bayes and Random Forests. 
#' 
#' ##

#' Load the datasets and tables
setwd("/Data/Coursera8")
library(data.table)
library(caret)

list.files()
mydat <- fread("pml-training.csv", na.strings=c("NA","", "#DIV/0!"))
mytestdat <- fread("pml-testing.csv", na.strings=c("NA","", "#DIV/0!"))

#' Create a muging function, this cleans up variables we don't need,
#' removes those that have too many NAs. Make sure the test and validation data
#' have the same structure. 
mungit <- function(my_dat)
     {
     my_dat <- as.data.frame(my_dat)
     my_dat$num_window <- NULL
     my_dat$raw_timestamp_part_1 <- NULL
     my_dat$raw_timestamp_part_2 <- NULL
     nums <- sapply(my_dat, is.numeric)
     mydat2 <- cbind(my_dat[, nums], classe=my_dat$classe)
     filter2 <- colSums(!is.na(mydat2))>19000
     mydat3 <- mydat2[, filter2]
     return(mydat3)
     }

mydat3 <-  mungit(mydat)
mytestdat2 <- as.data.frame(mytestdat)[,names(mydat3)[-53]]

#' Create the testing data
set.seed(1233242)
inTrain <- createDataPartition(mydat3$classe, p=0.25, list=FALSE)
train <- mydat3[inTrain,]
test <- mydat3[-inTrain,]

#' We generate the model using random forests on 25% of the sample data.
#' Tried several other methods on 5% data samples, but random forest empirically
#' blew the others out of the water and required little tweaking. 
#' 
#' Overall it's amazing how such a powerful method can be written in one line of
#'  code below: :-)
if(!"modelfitted.RData" %in% list.files())
{
     modelFit <- train(classe~., data=train, method="rf",
               preProcess="BoxCox")
     save(modelFit, file="modelfitted.RData")
} else load("modelfitted.RData")

#' Let's take a look at the training results
modelFit$modelInfo$tags
modelFit$results
modelFit$finalModel$confusion

#' Good Kappa and Mcnemar's P, Overall accuracy 97.8%. Could be tweaked further,
#' especially for classed B and D, with further tweaking of selected var, cleaner
#' data using preProcess to center and normalize (BoxCox) ... but since neither you
#' (the reader) nor I the writer are getting paid for this,
#' I suggest we stop here. 
#' The results can be deemed acceptable, let's move on the the validation dataset:

predictions <- predict(modelFit, newdata=test)
confusionMatrix(predictions, test$classe)
plot(predictions, test$classe)


#' Acceptable results. Let's move on to the test samples!

pml_write_files = function(x){
     n = length(x)
     for(i in 1:n){
          filename = paste0("./answers/problem_id_",i,".txt")
          write.table(x[i],file=filename,quote=FALSE,
                      row.names=FALSE,col.names=FALSE)
     }
}


answers <- (predict(modelFit, newdata = mytestdat2))
if(length(list.files("./answers")<20)){
     pml_write_files(answers)
} else print("Results have been stored.")

#' Submission accuracy for the 20 test samples was 20/20, or 100%. YAy.

