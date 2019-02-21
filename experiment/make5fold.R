setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/ClassNoise_00%")

set <- dir()


for(s in 1:length(set)){
  filename <- set[s]
  data <- read.csv(file = filename,h=T)
  print(filename)
  colnames(data)<-c(paste("X",1:(ncol(data)-1),sep=""),"Y")

  print(set[s])
  col <- ncol(data)
  
  k = 5 #Folds
  
  minor <- data[data[,col]==1,]
  nrow(minor)
  major <- data[data[,col]==0,]
  nrow(major)
  
  minor$id <- sample(1:nrow(minor))%%k+1
  major$id <- sample(1:nrow(major))%%k+1
  list <- 1:k
  
  for (i in 1:k){
    trainingset <- rbind(subset(minor, id %in% list[-i])[,-ncol(minor)],subset(major, id %in% list[-i])[,-ncol(major)])
    testset     <- rbind(subset(minor, id %in% c(i))[,-ncol(minor)]    ,subset(major, id %in% c(i))[,-ncol(major)])
    write.csv(trainingset,paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_00%/",filename,"_",as.character(i),"th_train.csv",sep=""),row.names = F)
    write.csv(testset,paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_00%/",filename,"_",as.character(i),"th_test.csv",sep=""),row.names = F)
  }
}