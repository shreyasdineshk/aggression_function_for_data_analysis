# Question 1 - Understand the data
forest_data<- as.matrix(read.table("Forest718.txt", header = FALSE, sep = "", dec = "."))

#Taking sample data
sampleforest_data<- forest_data[sample(1:517,200),c(1:13)]

#Choosing 4 different variables - x5,x9,x10,x11
plot(x = sampleforest_data[,5], y = sampleforest_data[,13])
plot(x = sampleforest_data[,9], y = sampleforest_data[,13])
plot(x = sampleforest_data[,10], y = sampleforest_data[,13])
plot(x = sampleforest_data[,11], y = sampleforest_data[,13])
hist(sampleforest_data[,5])
hist(sampleforest_data[,9])
hist(sampleforest_data[,10])
hist(sampleforest_data[,11])
hist(sampleforest_data[,13])

#Question 2 - Transform the data
FFMC <- sampleforest_data[,5]/100
temp <- sampleforest_data[,9]/100
RH <- sampleforest_data[,10]/100
wind <- sampleforest_data[,11]/100
area <- sampleforest_data[,13]/100

tdata<- cbind(FFMC,temp,RH,wind,area)
#writing the data into table
write.table(tdata, "shreyas-transformed.txt")

#Question 3
source("AggWaFit718.R")

#Fitting the functions
fit.QAM(tdata, output.1 = "outputQAM.txt", stats.1 = "statsQAM.txt", g = AM, g.inv = invAM)
fit.QAM(tdata, output.1 = "outputPM05.txt", stats.1 = "statsPM05.txt", PM05, invPM05)
fit.QAM(tdata, output.1 = "outputPM2.txt", stats.1 = "statsPM2.txt", QM, invQM)
fit.OWA(tdata, output.1 = "outputOWA.txt", stats.1 = "statsOWA.txt")
fit.choquet(tdata, output.1 = "outputchoquet.txt", stats.1 = "statschoquet.txt", kadd = (ncol(tdata)-1))

#Question 4

prediction <- function() {
  #initializing the values form the above obtained result.
  weights = c(0,0,0,0,0,0,0,0,0,1.00000000000001,1.00000000000001,0,0,1.00000000000001,1.00000000000001)
  x5 = 91.6
  x9 = 24.6
  x10 = 44
  x11 = 4
  
  #normalize
  normx5 = 91.6/100
  normx9 = 24.6/100
  normx10 = 44/100
  normx11 = 4/100
  
  values<- rbind(normx5,normx9, normx10, normx11)
  
  #predicting output
  predictionchoq<- choquet(values, weights)
  
  #original data present in data set
  origdata <- as.matrix(read.table("Forest718.txt"))
  
  finalvalues <- (predictionchoq*(max(origdata[,13]) - min(origdata[,13])) + min(origdata[,13]))
  return(finalvalues)
}
prediction()
