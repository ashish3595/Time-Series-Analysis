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

regressOn <- function(fileName){
  tryCatch({
    seriesFile <- read.csv(fileName,head=FALSE,sep=",")
    series <- seriesFile$V1
    original_series <- series
    
    n=length(series)
    series <- series[1:(n*0.7)]
    
    n=length(series)
    max_tau=400
    tau=nonlinearTseries::timeLag(series, technique = "ami",lag.max = max_tau, do.plot = FALSE,selection.method="first.minimum")
    
    emb_dim=7
    m=emb_dim
    
    matrix=tseriesChaos::embedd(series,d=tau,m=emb_dim)
    x=matrix[1:(nrow(matrix)-1),]
    y=matrix[2:(nrow(matrix)),]
    
    x <- data.frame(x)
    y <- data.frame(y)
    
    columnNames=vector(mode="list",length=m+1)
    columnNames[1]="y"
    for(i in 2:(m+1)){
      columnNames[i]=paste("x",i-1,sep="")
    }
    
    #train all the models
    models=vector(mode="list",length=m)
    for(i in 1:m){
      y_ <- y[,i]
      x_y <- cbind(y_,x)
      colnames(x_y) <- columnNames
      models[[i]] <- lm(y ~ . , x_y)
    }
    
    M=nrow(x)
    
    #predict the time series using the trained models
    series <- original_series
    n=length(series)
    
    X_now <- data.frame(x[1,])
    colnames(X_now) <- columnNames[2:(m+1)]
    
    predicted_time_series=vector(mode="list", length=n)
    predicted_time_series[1]=X_now[1,1]
    
    for(i in 2:n){
      X_next=X_now
      for(j in 1:m){
        X_next[1,j] = predict(models[[j]],newdata=X_now)
      }
      predicted_time_series[i]=X_next[1,1]
      X_now=X_next
    }
    #print("predicted : ")
    #print(predicted_time_series)
    
    retweet_count = cbind(series, predicted_time_series)
    matplot(retweet_count,pch=c(1,2),col=c("green","red"), ylim=c(0,10000))
    
    MAPE=0
    for(i in 1:n){
      if(series[i] > 0){
        MAPE=MAPE+abs(predicted_time_series[[i]] - series[i])/series[i]  
      }
    }
    MAPE=MAPE/n
    return(MAPE)
  }, warning = function(war){
    #print(paste0("WARNING ::  file : ",fileName,"\n",war))
    return(-1)
  }, error = function(err){
    #print(paste0("ERROR ::  file : ",fileName,"\n",err))
    return(-1)
  }, finally = {
    
  })
}

listOfChaoticSeriesFiles <- list.files("chaoticSeriesFiles", pattern="*.csv", full.names=TRUE)
numberOfFiles <- length(listOfChaoticSeriesFiles)

mape=0
faultCount=0
mape_list = 0
for(i in 1:numberOfFiles){
  t_mape = regressOn(listOfChaoticSeriesFiles[i])
  
  if(t_mape < 0 || t_mape > 10000){
    faultCount = faultCount + 1
  }
  else{
    mape = mape + t_mape
    mape_list = c(mape_list, t_mape)
    }
}
#print(mape)
print(paste0("Average Mape is : ",mape/(numberOfFiles - faultCount)))
plot(mape_list,ylim=c(0,200),pch=20, col="blue", ylab="MAPE", xlab="time series")
