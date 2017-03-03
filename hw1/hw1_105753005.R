#get the file name
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_105753005.R input", call.=FALSE)
} else if (length(args)==1) {
  i_fName <- args[1] 
}

#read an CSV file and 
i_f <- read.csv("test.1.csv",header=TRUE)

#calculate max value of weight and height
maxW <- round(max(i_f[2]),digits=2)
maxH <- round(max(i_f[3]),digits=2)

#output a CSV file 
data = data.frame(set = i_fName, weight = maxW, height = maxH)
write.csv(data , file = "result.csv" , row.names = FALSE)