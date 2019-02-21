DO <- function(ppp){
  no<- sprintf("%02d",ppp)
  source('Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/experiment/Determinine and SMOTE.R', echo=TRUE)
  library("unbalanced")
  library("FNN")
  
  
  setwd("Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/ClassNoise_00%")
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
  
  setwd(paste("Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_",no,"%",sep=""))
  l <- dir()
  
  
  for(s in 2:length(set)){
    setname <- set[s]
    folds <- matrix(l[grep(setname,l)],ncol = 2,byrow = T)
    
    trainingsets <- list()
    testsets <- list()
    
    for(i in 1:nrow(folds)){
      trainingsets[[i]] <- read.csv(folds[i,2])
    }
    col <- ncol(trainingsets[[1]])

    print(setname)
    print(c(sum(trainingsets[[1]][,col]),ncol(trainingsets[[1]])))
    
    out <- list()
    for(i in 1:nrow(folds)){
      out[[i]] <- determineK(trainingsets[[i]],col)
      write.csv(out[[i]],paste("Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/dataset/Final Data Sets/Fold sets/ClassNoise_",no,"%/AUSK/",setname,"_",as.character(i),"th_train_AUSK.csv",sep=""),row.names = F)
    }
    
  }
}

