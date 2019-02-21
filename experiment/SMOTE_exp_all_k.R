source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/Determinine and SMOTE.R', echo=F)
library("unbalanced")
library("FNN")
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/keel")
set <- dir()
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/fold_keel")
l <- dir()


for(s in 1:length(set)){
  setname <- set[s]
  folds <- matrix(l[grep(setname,l)],ncol = 2,byrow = T)
  
  trainingsets <- list()
  testsets <- list()
  
  for(i in 1:nrow(folds)){
    trainingsets[[i]] <- read.csv(folds[i,2])
    testsets[[i]] <- read.csv(folds[i,1])
  }
  
  col <- ncol(trainingsets[[1]])
  if(sum(trainingsets[[1]][,col])>500)
    next
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  # cov <- round(log(x = 0.1,base = (sum(trainingsets[[1]][,col])-1)/sum(trainingsets[[1]][,col])))
  
  smote_result <- array(NA,dim = c(5,30,4))
  
    for(j in 1:30){
    ser_sm <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(sum(trainingsets[[1]][,col])-1,1)
    for(i in 1:nrow(folds)){
      t <- table(trainingsets[[i]][,col])
      N<-floor(max(t)/min(t))*100
      #SMOTE
      res <- ubSMOTE(trainingsets[[i]][,-col], as.factor(trainingsets[[i]][,col]), perc.over = N, k=kn,perc.under = 0)
      # res[,3] <- as.factor(res[,3])
      res <- cbind(res$X,res$Y)
      colnames(res) <- colnames(trainingsets[[i]])
      res <- rbind(trainingsets[[i]][trainingsets[[i]][,col]==0,],res)
      res[,col] <- as.numeric(res[,col])
      
      AUC <- ONENN(res,testsets[[i]])
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn),as.character(j),"th measure :",as.character(AUC[1])))
      ser_sm <- rbind(ser_sm,AUC)
      
    }
    smote_result[,j,] <- ser_sm
  }
  perf <- c("AUCROC","AUCPR","Fmeasure","Gmean")
  for(m in 1:4){
    write.csv(smote_result[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/fold_keel/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}
