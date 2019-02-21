x<- rnorm(1000,5,1)
y<- rnorm(1000,5,2)

maj <- cbind(x,y,0)


a <- c(rnorm(30,2.5,0.5),5.5,4.5)
b <- c(rnorm(30,5,0.5),4.5,6)


min <- cbind(a,b,1)


data <- rbind(min,maj)
data2 <- rbind(maj,min)

data2 <-as.data.frame(data2)

# data <- rbind(min,maj,cbind(tmp$x,tmp$y,1))
data <- as.data.frame(data)

plot(data[,-3],xlim=c(0,10),ylim=c(0,10),col="gray50")
points(data[data[,3]!=0,],col=2,pch="*",cex=2.5)
#############
col <- 3
N <- (sum(!data[,col])-sum(data[,col]))/sum(data[,col])*100
SMOTEK <- determineK(data = data,class.column = 3)
SMOTEK


res <- doSMOTE(data,class.column = 3,N = 200,SMOTEK=SMOTEK)
res <- doBSMOTE1(data,class.column = 3,SMOTEK=SMOTEK,s=round(N/100))

plot(data[,-col],xlim=c(0,10),ylim=c(0,10),col="gray50")
points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)
points(res[,-col],col=3,pch="*",cex=2.5)
#############
res <- ubSMOTE(data[,-col], as.factor(data[,col]), perc.over = N, k=80,perc.under = 0)
res <- cbind(res$X,res$Y)
res[,col] <- as.numeric(res[,col])-1
colnames(res) <- colnames(data)
res <- rbind(data[data[,3]==0,],res)
plot(res[,-col],col="gray50")
points(res[res[,3]!=0,],col=2,pch="*",cex=2.5)
title(main="k=5")
#############
result <- BSMOTE1(data = data, target.column = 3, m = round(length(which(data[,col]==1))/2), k = 5, s = 5)
plot(data[,-3],xlim=c(0,10),ylim=c(0,10),col="gray50")
points(data[data[,3]!=0,],col=2,pch="*",cex=2.5)
points(result$d.synTotal,col=3,pch="*",cex=2.5)
#############
result <- SLSMOTE(data = data, target.column = 3, k = 5)
plot(data[,-3],xlim=c(0,10),ylim=c(0,10),col="gray50")
points(data[data[,3]!=0,],col=2,pch="*",cex=2.5)
points(result$d.synTotal,col=3,pch="*",cex=2.5)
