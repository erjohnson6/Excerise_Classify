library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)

## have.data <- FALSE

if (!have.data) {
    all.data <- read.csv("./pml-training.csv", na.strings = c("NA", "#DIV/0!"))
    ## set Class type for all.data
    c_class <- NULL
    all.data$user_name <- as.factor(all.data$user_name)
    all.data$cvtd_timestamp <- dmy_hm(all.data$cvtd_timestamp)
    all.data$new_window <- as.factor(all.data$new_window)
    all.data$classe <- as.factor(all.data$classe)
    have.data <- TRUE
}
set.seed(8178)
split.index <- createDataPartition(y=all.data$classe, p=0.8, list=FALSE)
train.data <- all.data[split.index, ]
verify.data <- all.data[-split.index, ]

sparse <- function(x) { sum(is.na(x)) }

percent.na <- round(apply(train.data, 2, sparse)/dim(train.data)[1],2)
include.col <- percent.na < 0.1
train.data <- train.data[,include.col]

mod.pca <- train(classe ~ ., method = "rf", data = train.data)

## var_yaw_belt has 2 data that are significant outliers.  Impact is TBD