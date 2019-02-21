setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/synthetic")
library(unbalanced)
data <- read.csv("03subcl5-800-7-0-BI.dat.csv")
data[,3] <- as.numeric(!data[,3])
col <-3

par(mar=c(6,3.5,2,1.5))

setEPS()
postscript("subcluster.eps",family = "Times", width = 6.0, height = 6.0,)
par(mar=c(6,3.5,2,1.5))
plot(data[,-col],col="gray50",type='n',xlab="",ylab="")
points(data[data[,col]==0,-col],col="gray50")
points(data[data[,col]!=0,],col="black",pch=16,cex=1.5)
mtext(side = 1, text = "X1", line = 2.2)
mtext(side = 2, text = "X2", line = 2.2)
mtext(side = 1, text = "(a) Original", line = 4.5, cex = 2)

dev.off()




N <- (sum(!data[,col])-sum(data[,col]))/sum(data[,col])*100
SMOTEK <- determineK(data = data,class.column = 3)
SMOTEK
res <- doSMOTE(data,class.column = 3,N = N,SMOTEK=SMOTEK)

setEPS()
postscript("subcluster_AUS.eps",family = "Times")
# # png("subcluster_AUS.png")
par(mar=c(6,3.5,2,1.5))
plot(data[,-col],col="gray50",type='n',xlab="",ylab="")
mtext(side = 1, text = "X1", line = 2.2)
mtext(side = 2, text = "X2", line = 2.2)
points(data[data[,col]==0,-col],col="gray50")
points(res[,-col],col="black",pch=16,cex=1.5)
mtext(side = 1, text = "(b) AND-SMOTE", line = 4.5, cex = 2)

dev.off()

# points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)


res <- ubSMOTE(data[,-col], as.factor(data[,col]), perc.over = N, k=30,perc.under = 0)
res <- cbind(res$X,res$Y)
res[,col] <- as.numeric(res[,col])-1
colnames(res) <- colnames(data)
# res <- rbind(data[data[,3]==0,],res)

setEPS()
postscript("subcluster_SMOTE.eps",family = "Times")
# png("subcluster_SMOTE.png")
par(mar=c(6,3.5,2,1.5))
plot(data[,-col],col="gray50",type='n',xlab="",ylab="")
mtext(side = 1, text = "X1", line = 2.2)
mtext(side = 2, text = "X2", line = 2.2)
points(data[data[,col]==0,-col],col="gray50")
points(res[,-col],col="black",pch=16,cex=1.5)
mtext(side = 1, text = "(b) SMOTE", line = 4.5, cex = 2)

dev.off()
# points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)

