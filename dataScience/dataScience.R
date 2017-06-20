require(rpart)
require(rpart.plot)
library(randomForest)
#recall
calSensitivity <- function(confusionMatrix){ 
  return (confusionMatrix[4]/(confusionMatrix[4]+confusionMatrix[1]))
}

calSpecificity <- function(confusionMatrix){
  return (confusionMatrix[2]/(confusionMatrix[2]+confusionMatrix[3]))
}

calPrecision <- function(confusionMatrix){
  return (confusionMatrix[1]/(confusionMatrix[1]+confusionMatrix[2]))
}
processData <- function(){
  allYes <- read.csv("C:\\Program Files\\R\\R-3.3.1\\bin\\data\\allYes.csv",header = FALSE)
  allNo <- read.csv("C:\\Program Files\\R\\R-3.3.1\\bin\\data\\allNo.csv",header = FALSE)
  trainYes.index <- sample(x=1:nrow(allYes), size=ceiling(0.7*nrow(allYes) ))
  trainNo.index <- sample(x=1:nrow(allNo), size=ceiling(0.5*nrow(allNo) ))
  train <- rbind(allYes[trainYes.index, ],allNo[trainNo.index,])
  test <- rbind(allYes[-trainYes.index, ],allNo[-trainNo.index,])
  return (list("training"= train,"testing"=test))
}

resultList <- c()
dataList <- processData()
train <- dataList$training
test <- dataList$testing

# Decision Tree 模型
modelDT<- rpart(V15 ~ V2+V3+V5+V7+V11+V13 , data=train)
print(prp(modelDT,         # 模型
          faclen=0,           # 呈現的變數不要縮寫
          fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
          shadow.col="gray",  # 最下面的節點塗上陰影
          # number of correct classifications / number of observations in that node
          extra="auto")  )
predDT <- predict(modelDT, newdata=test, type="vector")
cmDT <- table(real=test$V15, predict=predDT)
accuracyDT <- sum(diag(cmDT))/sum(cmDT) 
precisionDT <- calPrecision(cmDT)
recallDT <- calSensitivity(cmDT)
DTresultList <- list("cm"=cmDT,"accuracy"=accuracyDT)
resultList <- list(resultList,"DTresultList"=DTresultList)
print(paste("accuracy = ",accuracyDT,"  precision = ",precisionDT,"  recall = ",recallDT))
print(resultList$DTresultList)

# Random Forest 模型
modelRF <-randomForest(V15 ~ V2+V3+V5+V7+V11+V13, data=train, importance=TRUE, ntree=40)  
predRF <- predict(modelRF, newdata=test)
cmRF <- table(observed = test, predicted = predRF)
accuracyRF <- sum(diag(cmRF))/sum(cmRF) 
precisionRF <- calPrecision(cmRF)
recallRF <- calSensitivity(cmRF)
RFresultList <- list("cm"=cmRF,"accuracy"=accuracyRF)
resultList <- list(resultList,"RFresultList"=RFresultList)
print(paste("accuracy = ",accuracyRF,"  precision = ",precisionRF,"  recall = ",recallRF))
print(resultList$RFresultList)