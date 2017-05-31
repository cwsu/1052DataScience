library('rpart')

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw5_105753005.R -fold n -out performance.csv", call.=FALSE)
}
#Rscript hw5_105753005.R -fold 5 -out performance.csv

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "-fold"){
    n_folds <-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

n_folds <- as.numeric(n_folds)
d <- read.csv("Archaeal_tfpssm.csv",header=F)
adm_data<-as.data.frame(d)

set.seed(1117)
trains <- c()
calibrations <- c()
tests <- c()

for(k in 1:n_folds){
	trainingSize = n_folds-2
	
	# Compute sample sizes.
	sampleSizeTraining    <- floor((trainingSize/n_folds) * nrow(adm_data))
	sampleSizeCalibration <- floor((1/n_folds)            * nrow(adm_data))
	sampleSizeTest        <- floor((1/n_folds)            * nrow(adm_data))
	
	# Create the randomly-sampled indices for the dataframe. Use setdiff() to avoid overlapping subsets of indices.
	indicesTraining    <- sort(sample(seq_len(nrow(adm_data)), size=sampleSizeTraining))
	indicesNotTraining <- setdiff(seq_len(nrow(adm_data)), indicesTraining)
	indicesCalibration <- sort(sample(indicesNotTraining, size=sampleSizeCalibration))
	indicesTest        <- setdiff(indicesNotTraining, indicesCalibration)

	# Finally, output the three dataframes for training, validation and test.
	trainingSet    <- adm_data[indicesTraining, ]
	calibrationSet <- adm_data[indicesCalibration, ]
	testingSet     <- adm_data[indicesTest, ] 
	
	#run decision tree
	mymodel <- rpart(formula = trainingSet$V2 ~., data = trainingSet, control = rpart.control(cp = 0.001))
	
	#calculate accuracy score
	train <- length(which(predict(mymodel, trainingSet[,-2], type="class") == trainingSet[, 2] )) / nrow(trainingSet)
	calibration <- length(which(predict(mymodel, calibrationSet[,-2], type="class") == calibrationSet[, 2] )) / nrow(calibrationSet)
	test <- length(which(predict(mymodel, testingSet[,-2], type="class") == testingSet[, 2] )) / nrow(testingSet)	
	
	# add to trainings, calibrations, tests set
	trains <- c(trains, train)
	calibrations <- c(calibrations, calibration)
	tests <- c(tests, test)
}

accuracy <- round((c(mean(trains), mean(calibrations), mean(tests))), digits = 2)
out_data <- cbind(c("training", "calibration", "test"),accuracy)
colnames(out_data) <- c("set", "accuracy")
write.table(out_data, file=out_f, row.names = F, sep = ",", quote = F)
