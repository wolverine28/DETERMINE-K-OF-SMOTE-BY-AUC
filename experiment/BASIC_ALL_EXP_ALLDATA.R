source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/experiment/Determinine and SMOTE.R', echo=F)

library("unbalanced")
library("FNN")

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/ClassNoise_00%")
set <- dir()

IR  <-c()
min <- c()

for(i in 1:length(set)){
  data <- read.csv(set[i])
  col <- ncol(data)
  ttt <- table(data[,col])
  IR <-c(IR,ttt[2]/sum(ttt))
  min <- c(min,sum(data[,col]))
}
mmm <- data.frame(set,IR,min)
set <- set[which(mmm[,3]<1000 & mmm[,2]<0.4)]
set <- sapply(set,function(x){strsplit(x,"_")}[[1]])[1,]
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      # t <- table(trainingsets[[i]][,col])
      # N<-floor(t[1]/t[2])*100
      #SMOTE
      res <- ADASYN(data = trainingsets[[i]],class.column = col,dth = 0.75,beta = 1,k = kn)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/ADASYN/",setname,"_OneNN_ADASYN_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/ADASYN/",setname,"_CART__ADASYN_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)

  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N <- floor(t[1]/t[2])
      #B-SMOTE
      res <- cbind(BSMOTE1(data = trainingsets[[i]],target.column = col,m = round(sum(trainingsets[[i]][,col])/2),k = kn,s = N)$d.synTotal,1)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/BSMOTE/",setname,"_OneNN_BSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_00%/BSMOTE/",setname,"_CART__BSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_05%")
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/SMOTE/",setname,"_CART__SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      # t <- table(trainingsets[[i]][,col])
      # N<-floor(t[1]/t[2])*100
      #SMOTE
      res <- ADASYN(data = trainingsets[[i]],class.column = col,dth = 0.75,beta = 1,k = kn)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/ADASYN/",setname,"_OneNN_ADASYN_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/ADASYN/",setname,"_CART__ADASYN_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N <- floor(t[1]/t[2])
      #B-SMOTE
      res <- cbind(BSMOTE1(data = trainingsets[[i]],target.column = col,m = round(sum(trainingsets[[i]][,col])/2),k = kn,s = N)$d.synTotal,1)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/BSMOTE/",setname,"_OneNN_BSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_05%/BSMOTE/",setname,"_CART__BSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_10%")
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/SMOTE/",setname,"_CART__SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      # t <- table(trainingsets[[i]][,col])
      # N<-floor(t[1]/t[2])*100
      #SMOTE
      res <- ADASYN(data = trainingsets[[i]],class.column = col,dth = 0.75,beta = 1,k = kn)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/ADASYN/",setname,"_OneNN_ADASYN_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/ADASYN/",setname,"_CART__ADASYN_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N <- floor(t[1]/t[2])
      #B-SMOTE
      res <- cbind(BSMOTE1(data = trainingsets[[i]],target.column = col,m = round(sum(trainingsets[[i]][,col])/2),k = kn,s = N)$d.synTotal,1)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/BSMOTE/",setname,"_OneNN_BSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_10%/BSMOTE/",setname,"_CART__BSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_15%")
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/SMOTE/",setname,"_CART__SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      # t <- table(trainingsets[[i]][,col])
      # N<-floor(t[1]/t[2])*100
      #SMOTE
      res <- ADASYN(data = trainingsets[[i]],class.column = col,dth = 0.75,beta = 1,k = kn)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/ADASYN/",setname,"_OneNN_ADASYN_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/ADASYN/",setname,"_CART__ADASYN_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N <- floor(t[1]/t[2])
      #B-SMOTE
      res <- cbind(BSMOTE1(data = trainingsets[[i]],target.column = col,m = round(sum(trainingsets[[i]][,col])/2),k = kn,s = N)$d.synTotal,1)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/BSMOTE/",setname,"_OneNN_BSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_15%/BSMOTE/",setname,"_CART__BSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

setwd("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_20%")
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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/SMOTE/",setname,"_OneNN_SMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/SMOTE/",setname,"_CART__SMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      # t <- table(trainingsets[[i]][,col])
      # N<-floor(t[1]/t[2])*100
      #SMOTE
      res <- ADASYN(data = trainingsets[[i]],class.column = col,dth = 0.75,beta = 1,k = kn)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/ADASYN/",setname,"_OneNN_ADASYN_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/ADASYN/",setname,"_CART__ADASYN_",perf[m],".csv",sep=""),row.names=F)
  }
}

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
  ttt <- table(trainingsets[[1]][,col])
  print(setname)
  
  print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
  
  smote_result_NN <- array(NA,dim = c(5,30,4))
  smote_result_CART <- array(NA,dim = c(5,30,4))
  
  for(j in 1:30){
    ser_sm_NN <- matrix(NA,nrow=0,ncol=4)
    ser_sm_CART <- matrix(NA,nrow=0,ncol=4)
    
    kn <- sample(3:(sum(trainingsets[[1]][,col])-3),1)
    for(i in 1:nrow(folds)){
      print(paste(as.character(j),"--",as.character(i),"th folds SMOTE",as.character(kn)))
      t <- table(trainingsets[[i]][,col])
      N <- floor(t[1]/t[2])
      #B-SMOTE
      res <- cbind(BSMOTE1(data = trainingsets[[i]],target.column = col,m = round(sum(trainingsets[[i]][,col])/2),k = kn,s = N)$d.synTotal,1)
      
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
    write.csv(smote_result_NN[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/BSMOTE/",setname,"_OneNN_BSMOTE_",perf[m],".csv",sep=""),row.names=F)
    write.csv(smote_result_CART[,,m],paste("Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_20%/BSMOTE/",setname,"_CART__BSMOTE_",perf[m],".csv",sep=""),row.names=F)
  }
}
