source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/Determinine and SMOTE.R', echo=F)
setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/fold")
library("unbalanced")
library("FNN")

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
  
  
  smote_result <- array(NA,dim = c(k,20,4))
  auto_result   <- array(NA,dim = c(k,20,4))
  
  # N <- (sum(!data[,col])-sum(data[,col]))/sum(data[,col])*100
  N <- 200
  
  for(j in 1:20){
    ser_sm <- matrix(NA,nrow=0,ncol=4)
    ser_as <- matrix(NA,nrow=0,ncol=4)
    kn <- sample(floor(nrow(minor)/5),1)
   
        
      #SMOTE
      res <- ubSMOTE(trainingset[,-col], as.factor(trainingset[,col]), perc.over = N, k=kn,perc.under = 0)
      # res[,3] <- as.factor(res[,3])
      res <- cbind(res$X,res$Y)
      colnames(res) <- colnames(trainingset)
      res <- rbind(trainingset[trainingset[,col]==0,],res)
      res[,col] <- as.numeric(res[,col])
      
      AUC <- ONENN(res,testset)
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(j),"th measure :",as.character(AUC[1])))
      ser_sm <- rbind(ser_sm,AUC)
      
      
      
      #METHOD
      
#       out <- determineK(trainingset,col)
#       
#       res <- doSMOTE(trainingset,class.column = col,N = N,out)
#       res <- rbind(trainingset,res)
#       
#       AUC <- ONENN(res,testset)
#       print(paste(as.character(i),"th folds METHOD",as.character(j),"th measure :",as.character(AUC)))
#       ser_as <- rbind(ser_as,AUC)
#       
#       
      
    }
    smote_result[i,,] <- ser_sm
    # auto_result[i,,] <- ser_as
  }
  
  perf <- c("AUCROC","AUCPR","Fmeasure","Gmean")
  for(m in 1:4){
    write.csv(smote_result[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/",filename,"_OneNN_SMOTE_",perf[m],".csv",sep=""))
    # write.csv( auto_result[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/new/",filename,"_OneNN_AUTOS_",perf[m],".csv",sep=""))
  }
  
  
  
  
#   #k값의 변화에 따른 SMOTE AUC 변화
#   for(m in 1:4){
#     plot(apply(smote_result[,,m],2,mean),type='l',ylim=c(0,1))
#     abline(h=mean(apply(auto_result[,,m],2,mean)),lty=2,lwd=2)
#     title(main = perf[m])
#   }
}
