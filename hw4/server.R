library(shiny)
library(ggplot2)

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

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    query_m <- input$rbTarget
    files <- input$cbgMethod

    if(is.null(files)) {
      return()
    }
    
    # read files
    sensitivitys <- c()
    specificitys <- c()
    
    for(file in files)
    {
      d<-read.table(paste0("./methods/",file), header=T,sep=",")  
      
      cm <- calCM(d$prediction,d$reference,query_m)
      sensitivity <- round(calSensitivity(cm),digits=2)
      specificity <- round(calSpecificity(cm),digits=2)
      
      sensitivitys <- c(sensitivitys,sensitivity)
      specificitys <- c(specificitys,specificity)
    }
    
    x <- sensitivitys
    y <- specificitys
    data <- data.frame(sensitivitys,specificitys)
    qplot(x,y,data = data)
    
  })
})