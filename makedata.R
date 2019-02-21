l <- dir()
n <- l[grep("win",l)]
data <- read.csv(n,h=T)
col <- ncol(data)

data <- data[data[,col]%in%c(1,0,2),]

data[data[,col]==0,col] <- "pos"
data[data[,col]!="pos",col] <- "neg"

data[data[,col]=="pos",col] <- 1
data[data[,col]!=1,col] <- 0
data[,col] <- as.numeric(data[,col])
table(data[,col])

names <- strsplit(n,5)[[1]][1]
write.csv(data,paste(names,"0vs1,2.csv",sep=""))
