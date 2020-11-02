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

seriesFile <- read.csv(file="20338_mkpfcBeWtW.csv",head=FALSE,sep=",")

series <- seriesFile$V1
n=length(series)

res <- testChaos01(series,out=TRUE,c.int=c(0,2*pi),c.rep=1000)
plot(res,ylim = c(0,1))

res2 <- testChaos01(series,out=FALSE,c.int=c(0,2*pi),c.rep=1000)
print(res2)
