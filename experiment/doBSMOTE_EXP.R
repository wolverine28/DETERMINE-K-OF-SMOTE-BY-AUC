source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/experiment/Determinine and SMOTE.R', echo=F)
library("unbalanced")
library("FNN")

ppp <- 20
no<- sprintf("%02d",ppp)

setwd(paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_",no,"%/AUSK",sep=""))
kfolder <- paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_",no,"%/AUSK",sep="")
ks <- dir()

setwd(paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_",no,"%",sep=""))
l <- dir()

set <- paste(unique(sapply(ks,function(x){strsplit(x,".csv")[[1]][1]})),".csv",sep="")


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
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    for(i in 1:nrow(folds)){
      print(paste(setname,as.character(j),"--",as.character(i),"th folds doBSMOTE"))
      t <- table(trainingsets[[i]][,col])
      N<-floor(max(t)/min(t))*100
      
      res <- doBSMOTE1(data = trainingsets[[i]],class.column = col,SMOTEK = as.numeric(t(ksets[[i]])),s=N/100)
      colnames(res) <- colnames(trainingsets[[i]])
      res <- rbind(trainingsets[[i]],res)
      
      ser_sm_NN <- rbind(ser_sm_NN,ONENN(res,testsets[[i]]))
      ser_sm_CART <- rbind(ser_sm_CART,CART(res,testsets[[i]]))
      
    }
    smote_result_NN[,j,] <- ser_sm_NN
    smote_result_CART[,j,] <- ser_sm_CART
  }
  perf <- c("AUCROC","AUCPR","Fmeasure","Gmean")
  for(m in 1:4){
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_",no,"%/doBSMOTE/",setname,"_OneNN_doBSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_",no,"%/doBSMOTE/",setname,"_CART__doBSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}