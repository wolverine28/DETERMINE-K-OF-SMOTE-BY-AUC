source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/experiment/Determinine and SMOTE.R', echo=F)
library("unbalanced")
library("FNN")
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/fold_keel/AUSK")
kfolder <- "Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/fold_keel/AUSK"
ks <- dir()
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/keel")
set <- dir()
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/fold_keel")
l <- dir()


for(s in 1:length(set)){
  setname <- set[s]
  folds <- matrix(l[grep(setname,l)],ncol = 2,byrow = T)
  
  trainingsets <- list()
  testsets <- list()
  ksets <- list()
  
  for(i in 1:nrow(folds)){
    trainingsets[[i]] <- read.csv(folds[i,2])
    testsets[[i]] <- read.csv(folds[i,1])
    ksets[[i]] <-  read.csv(paste(kfolder,"/",ks[grep(setname,ks)][i],sep=""))
  }
  
  col <- ncol(trainingsets[[1]])
  if(sum(trainingsets[[1]][,col])>500)
    next
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      t <- table(trainingsets[[i]][,col])
      N<-floor(max(t)/min(t))*100
      
      res <- doSMOTE(data = trainingsets[[i]],class.column = col,N = N,SMOTEK = as.numeric(t(ksets[[i]])))
      res <- rbind(trainingsets[[i]],res)
      
      AUC <- ONENN(res,testsets[[i]])
      print(paste(as.character(i),"th folds AUS",as.character(j),"th measure :",as.character(AUC)))
      ser_sm <- rbind(ser_sm,AUC)
      
    }
    smote_result[,j,] <- ser_sm
  }
  perf <- c("AUCROC","AUCPR","Fmeasure","Gmean")
  for(m in 1:4){
    write.csv(smote_result[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/fold_keel/AUS/",setname,"_OneNN_AUS_",perf[m],".csv",sep=""),row.names=F)
  }
}