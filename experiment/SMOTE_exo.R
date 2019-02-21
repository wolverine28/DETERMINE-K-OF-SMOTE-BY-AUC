source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/experiment/Determinine and SMOTE.R', echo=F)

library("unbalanced")
library("FNN")

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/ClassNoise_00%")
set <- dir()
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_00%")
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
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(sum(trainingsets[[1]][,col])-1,1)
    for(i in 1:nrow(folds)){
      print(paste(setname,as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N<-floor(max(t)/min(t))*100
      #SMOTE
      res <- ubSMOTE(trainingsets[[i]][,-col], as.factor(trainingsets[[i]][,col]), perc.over = N, k=kn,perc.under = 0)
      # res[,3] <- as.factor(res[,3])
      res <- cbind(res$X,res$Y)
      colnames(res) <- colnames(trainingsets[[i]])
      res <- rbind(trainingsets[[i]][trainingsets[[i]][,col]==0,],res)
      res[,col] <- as.numeric(res[,col])
      
      
      ser_sm_NN <- rbind(ser_sm_NN,ONENN(res,testsets[[i]]))
      ser_sm_CART <- rbind(ser_sm_CART,CART(res,testsets[[i]]))
      
    }
    smote_result_NN[,j,] <- ser_sm_NN
    smote_result_CART[,j,] <- ser_sm_CART
  }
  perf <- c("AUCROC","AUCPR","Fmeasure","Gmean")
  for(m in 1:4){
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/SMOTE/",setname,"_CART__SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}
