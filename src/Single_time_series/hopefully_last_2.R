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

seriesFile <- read.csv("20338_mkpfcBeWtW.csv",head=FALSE,sep=",")
series <- seriesFile$V1
original_series <- series

n=length(series)
series <- series[1:(n*0.7)]

n=length(series)
max_tau=400
tau=nonlinearTseries::timeLag(series, technique = "ami",lag.max = max_tau, do.plot = TRUE,selection.method="first.minimum")

#emb_dim=fractal::corrDim(series, dimension=3, olag=0, resolution=2)
#plot(emb_dim)
emb_dim=3
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

#plot(series)
#points(predicted_time_series)
retweet_count = cbind(series, predicted_time_series)
matplot(retweet_count,pch=c(1,2),col=c("green","red"), ylim=c(0,10000))

print("MAPE : ")
MAPE=0
for(i in 1:n){
  if(series[i] > 0){
    MAPE=MAPE+abs(predicted_time_series[[i]] - series[i])/series[i]  
  }
}
MAPE=MAPE/n
print(MAPE)
