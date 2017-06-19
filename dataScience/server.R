library(shiny)
library(ggplot2)
library(randomForest)
require(rpart)
require(rpart.plot)


# calculate confusion matrix
calCM <- function(predictions,references,target){
  confusionMatrix <- table(truth = c(predictions==references), prediction = c(predictions==target))
  return (confusionMatrix)
}

calSensitivity <- function(confusionMatrix){
  return (confusionMatrix[4]/(confusionMatrix[4]+confusionMatrix[1]))
}

calSpecificity <- function(confusionMatrix){
  return (confusionMatrix[2]/(confusionMatrix[2]+confusionMatrix[3]))
}

processData <- function(){
  allYes <- read.csv("./data/allYes.csv",header = FALSE)
  allNo <- read.csv("./data/allNo.csv",header = FALSE)
  trainYes.index <- sample(x=1:nrow(allYes), size=ceiling(0.7*nrow(allYes) ))
  trainNo.index <- sample(x=1:nrow(allNo), size=ceiling(0.5*nrow(allNo) ))
  train <- rbind(allYes[trainYes.index, ],allNo[trainNo.index,])
  test <- rbind(allYes[-trainYes.index, ],allNo[-trainNo.index,])
  return (list("training"= train,"testing"=test))
}

changeStrToFeature <- function(feature){
  len <- length(feature)
  features <- c()
  for(x in c(1:len)){
    if(x==1){
      features <- feature[x]
    }else{
      features <- paste(features,"+",feature[x])
    }
  }
  return(features)
}

resultList <- list()

shinyServer(function(input, output) {
  
  output$distDTPlot <- renderPlot({
    
    feature <- input$siFeature
    #features <- data.frame()
    #if(is.null(feature)) {
    #  return()
    #}else{
    #  feature <- changeStrToFeature(feature)
    #}
    #print(feature)
    
    #read files
    dataList <- processData()
    train <- dataList$training
    test <- dataList$testing
    
    # CART的模型
    feature <- sort(feature)
    model<- rpart(V15 ~ . , data=train)
    
    print(prp(model,         # 模型
        faclen=0,           # 呈現的變數不要縮寫
        fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
        shadow.col="gray",  # 最下面的節點塗上陰影
        # number of correct classifications / number of observations in that node
        extra="auto")  )
    
    pred <- predict(model, newdata=test, type="vector")
    # 用table看預測的情況
    table(real=test$V15, predict=pred)
    
    # 計算預測準確率 = 對角線的數量/總數量
    cm <- table(real=test$V15, predict=pred)
    accuracy <- sum(diag(cm))/sum(cm) # 對角線的數量/總數量
    DTresultList <- list("cm"=cm,"accuracy"=accuracy)
    resultList <- list(resultList,"DTresultList"=DTresultList)
    print(resultList$DTresultList)
  })
  output$distRFPlot <- renderPlot({
    
    feature <- input$siFeature
    query_m <- input$rbTarget
    files <- input$cbgMethod
    
    #features <- data.frame()
    #if(is.null(feature)) {
    #  return()
    #}else{
    #  feature <- changeStrToFeature(feature)
    #}
    #print(feature)
    
    #read files
    dataList <- processData()
    train <- dataList$training
    test <- dataList$testing
    
    # CART的模型
    feature <- sort(feature)
    model <-randomForest(V15 ~ V2+V3+V5+V7+V8+V9+V11+V12+V13, data=train, importance=TRUE, ntree=100)  
    
    pred <- predict(model, newdata=test, type="response")
    # 用table看預測的情況
    table(real=test$V15, predict=pred)
    
    # 計算預測準確率 = 對角線的數量/總數量
    cm <- table(real=test$V15, predict=pred)
    accuracy <- sum(diag(cm))/sum(cm) # 對角線的數量/總數量
    RFresultList <- list("cm"=cm,"accuracy"=accuracy)
    resultList <- list(resultList,"RFresultList"=RFresultList)
    print(resultList$RFresultList)

  })
  output$showResult <- renderPrint({
    paste("Decision tree cm and accuracy : ", resultList$RFresultList," Random forest cm and accuracy : ",resultList$DTresultList)
  })
})