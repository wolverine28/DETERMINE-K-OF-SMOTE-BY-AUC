library("FNN")
x<- rnorm(200,0.5,0.05)
y<- rnorm(200,0.5,0.02)

maj <- cbind(x,y,0)


a <- c(rnorm(5,0.4,0.008),rnorm(3,0.5,0.005),0.55)
b <- c(rnorm(5,0.52,0.008),rnorm(3,0.46,0.005),0.49)


min <- cbind(a,b,1)


data <- rbind(min,maj)


# data <- rbind(min,maj,cbind(tmp$x,tmp$y,1))
data <- as.data.frame(data)

colnames(data) <- c("X1","X2")
#############################################################
data <- read.csv("sample.csv")
col <- ncol(data)
colnames(data) <- c("X1","X2")

par(mar=c(5.1,4.1,4.1,2.1))
par(mar=c(3.5,3.5,2,1.5))

plot(data[,-col],col="gray50",type='n',xlab="",ylab="")
points(data[data[,col]==0,-col],col="gray50")
points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)

title(main = "1",line = 0.5)
mtext(side = 1, text = "X1", line = 2.2)
mtext(side = 2, text = "X2", line = 2.2)

#############################################################
class.column <- 3
minor <- data[data[,class.column]==1,-class.column]
major <- data[data[,class.column]==0,-class.column]
knn <- get.knn(minor,nrow(minor)-1,algorithm = "kd_tree")

m <-1

near <- knn$nn.index[m,]


for(i in 1:length(near)){
  setEPS()
  
  postscript(paste("minor",as.character(m),as.character(i),"stnighbor.eps",sep=""))

  par(mar=c(3.5,3.5,2,1.5))
  # png(paste("minor ",as.character(m),"- ",as.character(i),"st nighbor.png",sep=""))
  plot(data[,-3],col="gray50",type="n",xlab="",ylab="")
  mtext(side = 1, text = "X1", line = 2.2)
  mtext(side = 2, text = "X2", line = 2.2)
  for(j in 1:i){
    # rect(minor[m,][1],minor[m,][2],minor[near[j],][1],minor[near[j],][2],col="skyblue",border = "transparent")
    rect(minor[m,][1],minor[m,][2],minor[near[j],][1],minor[near[j],][2],col="skyblue",border="transparent")
  }
  points(data[data[,3]!=0,],col=1,pch=rownames(data[data[,col]!=0,]),cex=1.5)
  # points(data[data[,3]!=0,],col=2,pch="*",cex=2.5)
  points(data[data[,3]==0,],col=3)
  if(i==1){
    title(main = paste("Minor ",as.character(m),"- ",as.character(i),"st Nighbor",sep=""),line = 0.5)
  } else if(i ==2){
    title(main = paste("Minor ",as.character(m)," - ",as.character(i),"nd Nighbor",sep=""),line = 0.5)
  } else {
    title(main = paste("Minor ",as.character(m)," - ",as.character(i),"th Nighbor",sep=""),line = 0.5)
  }
  dev.off()
  
}
#############################################################
SMOTEK <- determineK(data,3)
res <- doSMOTE(data,class.column = 3,N = 500,SMOTEK=SMOTEK)
plot(data[,-col],col="gray50",type='n')
points(data[data[,col]==0,-col],col="gray50")
points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)
points(res[,-col],col=3,pch="*",cex=2.5)
