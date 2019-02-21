l <- dir()
m <- matrix(l[grep(".csv",l)],ncol=2,byrow=T)
m
for(i in 1:nrow(m)){
  tra <- read.csv(m[i,1],h=T)
  tst <- read.csv(m[i,1],h=T)
  data <- rbind(tra,tst)
  name <- m[i,1]
  name <- strsplit(name,"-")[[1]][1]
  write.csv(data,paste("./merged/",name,"5%cn",".csv",sep=""),row.names = F)
}
