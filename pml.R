library(caret)
library(randomForest)
    
set.seed(1000)
    
training <- read.csv("pml-training.csv", na.strings=c("NA", "", "#DIV/0!"))
test <- read.csv("pml-testing.csv", na.strings=c("NA", "", "#DIV/0!"))
head(training)
    
#print(dim(training))
    
training <- training[colSums(is.na(training)) < 0.1 * nrow(training)]
test <- test[colSums(is.na(test)) < 0.1 * nrow(test)]
#print(dim(training))
    
#head(training)
    
removeVar <- function(set) {
    set$X <- NULL
    set$user_name <- NULL
    set$raw_timestamp_part_1 <- NULL
    set$raw_timestamp_part_2 <- NULL
    set$cvtd_timestamp <- NULL
    set$new_window <- NULL
    set$num_window <- NULL
    set
}

training <- removeVar(training)
    
#print(dim(training))
   
test <- removeVar(test)
 
#test$X <- NULL
#test$user_name <- NULL
#test$raw_timestamp_part_1 <- NULL
#test$raw_timestamp_part_2 <- NULL
#test$cvtd_timestamp <- NULL
#test$new_window <- NULL
#test$num_window <- NULL
    
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
myTrain <- training[inTrain, ]; 
myCV <- training[-inTrain, ]
    
modFit <- randomForest(classe ~. , data=myTrain)
predictions <- predict(modFit, myCV)
#print(confusionMatrix(predictions, myCV$classe))

result <- predict(modFit, test[, -length(names(test))])
print(result)

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(result)

    