library('ROCR')

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

calPrecision <- function(confusionMatrix){
	return (confusionMatrix[4]/(confusionMatrix[4]+confusionMatrix[3]))
}

calF1 <- function(confusionMatrix){
	recall <- calSensitivity(confusionMatrix)
	precision <- calPrecision(confusionMatrix)
	return (2*precision*recall)/(precision+recall)
}

calAUC <- function(predscore, reference) {
  evalu <- prediction(predscore, reference)
  auc <- attributes(performance(evalu, 'auc'))$y.values[[1]]
  return (auc)
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_105753005.R --target male --target male --files method1.csv method2.csv method3.csv method4.csv method5.csv method6.csv method7.csv method8.csv method9.csv method10.csv --out result.csv", call.=FALSE)
}
#Rscript hw2_105753005.R --target male --target male --files method1.csv method2.csv --out result.csv
# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    query_m<-args[i+1]
    i<-i+1
  }else if(args[i] == "--files"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

print("PROCESS")
print(paste("query mode :", query_m))
print(paste("output file:", out_f))
print(paste("files      :", files))

# read files
methods<-c()
sensitivitys <- c()
specificitys <- c()
F1s <- c()
AUCs <- c()

for(file in files)
{
  #method<-gsub(".csv", "", basename(file))
  method <- file
  d<-read.table(file, header=T,sep=",")
  print(d)
  cm <- calCM(d$prediction,d$reference,query_m)
  print(cm)
  sensitivity <- round(calSensitivity(cm),digits=2)
  specificity <- round(calSpecificity(cm),digits=2)
  F1 <- round(calF1(cm),digits=2)
  AUC <- round(calAUC(d$pred.score,d$reference),digits=2)
  
  methods<-c(methods,method)
  sensitivitys <- c(sensitivitys,sensitivity)
  specificitys <- c(specificitys,specificity)
  F1s <- c(F1s,F1)
  AUCs <- c(AUCs,AUC)
}

out_data <- data.frame(method=methods, sensitivity=sensitivitys, specificity=specificitys, F1 = F1s, AUC = AUCs, stringsAsFactors = F)
#find F1 max and the second file name
highest <- c("highest")
for(x in c(2:5)){
	if(x==4){
		outDataSorted <- out_data[order(out_data[[x]]),]#get the sorted vector
		len<-nrow(out_data)
		
		maxIndexName <- outDataSorted[len,1]###This is the max
		secondMaxIndexName<-outDataSorted[len-1,1]###the value smaller only than max		
		
		maxFileData <- read.table(maxIndexName, header=T, sep=",")
		secondFileData <- read.table(secondMaxIndexName, header=T, sep=",")
		
		ct <- table(maxFileData$prediction, secondFileData$prediction)
	
		# the null hypothesis : conversion is independent of group
		print(fisher.test(ct)$p.value)
		maxIndexName <- gsub(".csv", "", basename(maxIndexName))
		if(fisher.test(ct)$p.value<0.05){			
			highest <- c(highest,paste(maxIndexName,"*",sep=""))
		}else{
			highest <- c(highest,maxIndexName)
		}		
	}else{
		index<-apply(out_data[x], 2, which.max)
		methods[index] <- gsub(".csv", "", basename(methods[index]))		
		highest <- c(highest,methods[index])
	}	
}

# output file
out_data<-rbind(out_data,highest)
out_data$method <- gsub(".csv", "", basename(out_data$method))
write.table(out_data, file=out_f, row.names = F, sep = ",", quote = F)
out_data
