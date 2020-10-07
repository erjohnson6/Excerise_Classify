library(dplyr)
library(ggplot2)
library(caret)
library(lubridate)

have.data <- FALSE

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
sparse <- function(x) { sum(is.na(x)) }
percent.na <- round(apply(all.data, 2, sparse)/dim(all.data)[1],2)
include.col <- percent.na < 0.1
all.data <- all.data[ , include.col]

set.seed(8178)

split.index <- createDataPartition(y = all.data$classe, p = 0.8, list = FALSE)
train.data <- all.data[split.index, ]
split.index.small <- createDataPartition(y = train.data$classe, p = 1/8, list = FALSE)
train.data.small <- train.data[split.index.small, ]

verify.data <- all.data[-split.index, ]

OOS.Accuracy <- NULL

if(!exists("rf.model.small")) {
    system.time(rf.model.small <- train(classe ~ ., method = "rf", 
                                        data = train.data.small), 
                                        trControl = trainControl(method="cv"), number=3)
}
rf.pred <- predict(rf.model.small, train.data[-split.index.small, ])
OOS.Accuracy$rf <- mean(rf.pred == train.data[-split.index.small,"classe"])
table(rf.pred, train.data[-split.index.small,"classe"])

if(!exists("rpart.model.small")) {
    system.time(rpart.model.small <- train(classe ~ ., method = "rpart", 
                                           data = train.data.small))
}
rpart.pred <- predict(rpart.model.small, train.data[-split.index.small, ])
OOS.Accuracy$rpart <- mean(rpart.pred == train.data[-split.index.small,"classe"])
table(rpart.pred, train.data[-split.index.small,"classe"])

if(!exists("lda.model.small")) {
    system.time(lda.model.small <- train(classe ~ ., method = "lda", 
                                           data = train.data.small))
}
lda.pred <- predict(lda.model.small, train.data[-split.index.small, ])
OOS.Accuracy$lda <- mean(lda.pred == train.data[-split.index.small,"classe"])
table(lda.pred, train.data[-split.index.small,"classe"])

if(!exists("nb.model.small")) {
    system.time(nb.model.small <- train(classe ~ ., method = "nb", 
                                           data = train.data.small))
}
nb.pred <- predict(nb.model.small, train.data[-split.index.small, ])
OOS.Accuracy$nb <- mean(nb.pred == train.data[-split.index.small,"classe"])
table(nb.pred, train.data[-split.index.small,"classe"])

if(!exists("gbm.model.small")) {
    system.time(gbm.model.small <- train(classe ~ ., method = "gbm", 
                                           data = train.data.small, verbose = FALSE))
}
gbm.pred <- predict(gbm.model.small, train.data[-split.index.small, ])
OOS.Accuracy$gbm <- mean(gbm.pred == train.data[-split.index.small,"classe"])
table(gbm.pred, train.data[-split.index.small,"classe"])

## rf.model <- train(classe ~ ., method = "rf", data = train.data)

## var_yaw_belt has 2 data that are significant outliers.  Impact is TBD