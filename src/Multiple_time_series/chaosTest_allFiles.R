#To install the necessary packages
packages_list = c("Chaos01","minpack.lm","nonlinearTseries","tseriesChaos", "fractal")
for (package in packages_list) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

fileNames <- list.files(paste0(getwd(),"/timeSeriesFiles_60"), pattern="*.csv", full.names=TRUE)
n=length(fileNames)

Kc_values=numeric(0)
count=0
chaotic_threshold= 0.7

for(i in 1:n){
  print(paste0("Processing file : ",i))
  seriesFile <- read.csv(file=fileNames[i],head=FALSE,sep=",")
  series <- seriesFile$V1
  
  #print(fileNames[i])
  # vec.x <- series
  
  res1 <- testChaos01(series,out=TRUE,c.int=c(0,2*pi),c.rep=1000)
  res2 <- testChaos01(series,out=FALSE,c.int=c(0,2*pi),c.rep=1000)
  
  if(is.na(res2) != TRUE){
    Kc_values <- c(Kc_values,res2)
  
    print(res2)
    plot(res1, ylim = c(0,1))
    
    if(res2 > chaotic_threshold){
      count=count+1
      #write(fileNames[i], file = "/home/shyam/Documents/BTech/7th sem/Projects/TSA/TSA/Codebase/R files/FirstShot/chaoticSeriesFiles.csv", append=TRUE, sep="\n")
      file.copy(from = fileNames[i],paste0(getwd(),"/chaoticSeriesFiles"), copy.mode=TRUE)  
    }
  }
}

hist(Kc_values, breaks = 10, col = "blue")

print(paste0("Total number of time series processed : ",n))
print(paste0("Number of chaotic series (for threshold ",chaotic_threshold,") : ",count))
print(paste0("Percentage of chaotic series : ",count/n))
