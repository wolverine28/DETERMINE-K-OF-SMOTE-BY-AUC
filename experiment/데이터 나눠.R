l <- dir()
l <- l[grep(".csv",l)]


dat.inx <- list(
  c(0,-1,1,2),
  c(1,-1,0,7),
  c(-1),
  c(-1),
  c(0,-1,1),
  c(4,-1,0,1),
  c(5,-1,6,8),
  c(-1),
  c(-1),
  c(5,-1,0,4),
  c(2,-1,0,1,3),
  c(-1),
  c(-1),
  c(0,-1,1,2),
  c(-1),
  c(1,-1,0),
  c(0,-1,1,2))

for(i in 1:(length(l)-1)){
  data <- read.csv(l[i],h=T)
  print(l[i])
  col <- ncol(data)
  
  ind <- dat.inx[[i]]
  minus <- which(ind==-1)
  if(minus!=1){
    min <- ind[1:(minus-1)]
    maj <- ind[(minus+1):length(ind)]
    
    data[,col][data[,col]%in%maj] <- -1
    data[,col][data[,col]%in%min] <- -2
    
    data <- data[data[,col] < 0,]
    
    data[,col] <- -data[,col]-1
    
    strsplit(l[i],"-")[[1]][1]
    write.csv(data,paste("./new/",strsplit(l[i],"-")[[1]][1],paste(maj,sep="",collapse = ""),"vs",paste(min,sep="",collapse = ""),"_20%.csv",sep=""),row.names = F)
  } else{
    next
  }
  
}


yeast <- list(
  c(0,-1,2),
  c(0,-1,4),
  c(0,-1,5),
  c(0,-1,6),
  c(0,-1,8),
  c(0,-1,9),
  c(0,7,-1,4,5),
  c(0,7,-1,2,4,5),
  c(7,-1,2),
  c(7,-1,4),
  c(7,-1,5),
  c(7,-1,6),
  c(7,-1,8),
  c(7,-1,9),
  c(2,4,-1,3),
  c(0,6,7,-1,2,4,5,9))



data <- read.csv(l[18],h=T)
print(l[18])
col  <- ncol(data)

for(i in 1:length(yeast)){
  
  data <- read.csv(l[18],h=T)
  print(l[18])
  col  <- ncol(data)
  
  
  ind <- yeast[[i]]
  minus <- which(ind==-1)
  maj <- ind[1:(minus-1)]
  min <- ind[(minus+1):length(ind)]
  
  data[,col][data[,col]%in%maj] <- -1
  data[,col][data[,col]%in%min] <- -2
  
  data <- data[data[,col] < 0,]
  
  data[,col] <- -data[,col]-1
  

  write.csv(data,paste("./new/","yeast",paste(maj,sep="",collapse = ""),"vs",paste(min,sep="",collapse = ""),"_20%.csv",sep=""),row.names = F)
}
