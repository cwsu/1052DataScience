# run : Rscript hw1_105753005.R -files test.1.csv -out result.csv
#get the input file name and output file name
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_105753005.R -files test.1.csv -out result.csv", call.=FALSE)
} 
i<-1 
while(i < length(args))
{
  if(args[i] == "-files"){
    i_f<-args[i+1]
    i<-i+1
  }else if(args[i] == "-out"){
    o_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

#read an CSV file and 
i_f <- read.csv("test.1.csv",header=TRUE)

#calculate max value of weight and height
maxW <- round(max(i_f[2]),digits=2)
maxH <- round(max(i_f[3]),digits=2)

#output a CSV file 
data = data.frame(set = i_f, weight = maxW, height = maxH)
write.csv(data , file = o_f , row.names = FALSE)
