l <- dir()
lists <- l[grep(".dat",l)]
tst <- lists[grep("1tst",lists)]
tra <- lists[grep("1tra",lists)]

lists <- c(tst,tra)
m <- matrix(lists,ncol=2)

bind <- function(names){
  name <- readLines(names)
  print(names)
  dats <- name[(grep("@data",name)+1):length(name)]
  tmp <- t(sapply(dats,function(x){unlist(strsplit(x,","))}))
  rownames(tmp) <- rep(NA,nrow(tmp))
  
  
  att <- tmp[,-ncol(tmp)]
  cls <- tmp[,ncol(tmp)]
  class(att) <- "numeric"
  cls[grep("pos",cls)] <-"positive"
  cls[grep("neg",cls)] <-"negative"
  cls <- as.numeric(as.factor(cls))-1
  
  data <- cbind(att,cls)
  
  colnames(data) <- c(paste("X",seq(1:(ncol(data)-1)),sep=""),"Y")
  return(data)
}

for(i in 1:length(lists)){
  
  # data <- rbind(bind(m[i,1]),bind(m[i,2]))
  data <- bind(lists[i])
  
  write.csv(data,paste("../",lists[i],".csv",sep=""),row.names = F)
}