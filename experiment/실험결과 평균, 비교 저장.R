for(ppp in c(0,5,10,15,20)){
  no <- sprintf("%02d",ppp)
  setwd(paste("Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/results/Final/ClassNoise_",no,"%",sep=""))
  sm <- dir("./BSMOTE")
  au <- dir("./doBSMOTE")
  

    mea <- c("ROC","PR","Fm","Gm")
    fin <- list()
    for(j in 1:4){
      l <- cbind(sm[grep(mea[j],sm)],au[grep(mea[j],au)])
      l <- matrix(l[grep(pred[k],l)],ncol=2)
      
      result <- matrix(NA,ncol=2,nrow=0)
      for(i in 1:nrow(l)){
        smd <- read.csv(paste("./BSMOTE/",l[i,1],sep=""))
        aud <- read.csv(paste("./doBSMOTE/"  ,l[i,2],sep=""))
        
        result <- rbind(result,c(mean(apply(smd,2,function(x){mean(x,na.rm = T)})),mean(apply(aud,2,function(x){mean(x,na.rm = T)}))))
      }
      rownames(result) <- matrix(sapply(l,function(x){strsplit(x,"_")}[[1]][1]),ncol=2)[,1]
      fin[[j]] <- result
    }
  write.csv(cbind(fin[[1]],fin[[2]],fin[[3]],fin[[4]]),paste("Z:/°³ÀÎ»ç¿ëÀÚ/À±Àç¼·/DETERMINE K OF SMOTE BY AUC/results/Final/tables/_",no,"%_BSMOTEvsdoBSMOTE.csv",sep=""))
}