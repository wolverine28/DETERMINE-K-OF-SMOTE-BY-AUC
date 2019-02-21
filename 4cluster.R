x <- runif(1500,0,10)
y <- runif(1500,0,10)

maj <- cbind(x,y,0)


ax <- c(runif(50,5.5,9.3),runif(25,1.7,4.3),runif(20,1.7,4.3),runif(10,6.7,8.3))
ay <- c(runif(50,5.5,9.3),runif(25,6.7,9.3),runif(20,1.7,4.3),runif(10,1.7,4.3))

min <- cbind(ax,ay,1)

reg <- c(which((maj[,1]%btwn%c(6,9))&(maj[,2]%btwn%c(6,9))==T),
         which((maj[,1]%btwn%c(2,4))&(maj[,2]%btwn%c(7,9))==T),
         which((maj[,1]%btwn%c(2,4))&(maj[,2]%btwn%c(2,4))==T),
         which((maj[,1]%btwn%c(7,8))&(maj[,2]%btwn%c(2,4))==T))




maj2 <- maj[-reg,]


data <- rbind(maj2,min)
plot(data[,-3],col=data[,3]+1)
colnames(data) <- c("X1","X2","Y")
data <- as.data.frame(data)


###
col <- 3
N <- (sum(!data[,col])-sum(data[,col]))/sum(data[,col])*100
SMOTEK <- determineK(data = data,class.column = 3)
SMOTEK
res <- doSMOTE(data,class.column = 3,N = 200,SMOTEK=SMOTEK)

plot(data[,-col],xlim=c(0,10),ylim=c(0,10),col="gray50")
points(data[data[,col]!=0,],col=2,pch="*",cex=2.5)
points(res[,-col],col=3,pch="*",cex=2.5)
