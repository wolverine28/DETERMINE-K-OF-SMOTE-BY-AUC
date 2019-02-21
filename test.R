source('Z:/개인사용자/윤재섭/DETERMINE K OF SMOTE BY AUC/DETERMINE K OF SMOTE BY AUC_fester_hypercube.R', encoding = 'UTF-8', echo=F)
source('D:/Jaesub/Dropbox/Datamining/R CODE/datamining/STRPart 최종2.R', echo=F)

data <- read.csv("dataset5.csv",h=T)
# data[,3] <- data[,3]-1
col <- ncol(data)
plot(data[,-col],col=data[,col]+1)

part <- STRPart(data,col,0.50)

training <- part[[1]]
plot(training[,-col],col=training[,col])
testing <- part[[2]]
points(testing[,-col],col=testing[,col],pch=16)

fo <- formula(paste(colnames(data)[col],"~."))

fit <- rpart(fo,training)
tab <- print(table(predict(fit,testing[,-col],type="class"),testing[,col]))
TPR <- tab[1,1]/sum(tab[,1])
FPR <- tab[1,2]/sum(tab[,2])
AUC1 <- print(0.5+TPR/2-FPR/2)

#######################################################################################
N <- (sum(!data[,col])-sum(data[,col]))/sum(data[,col])*100

# training[,3] <- as.numeric(as.character(training[,3]))
# training[,3] <- training[,3]-1
res <- AUCSMOTE(training,class.column = col,N = N)
# res[,3] <- as.factor(res[,3])

fit <- rpart(fo,res)
tab <- print(table(predict(fit,testing[,-col],type="class"),testing[,col]))
TPR <- tab[1,1]/sum(tab[,1])
FPR <- tab[1,2]/sum(tab[,2])
AUC2 <- print(0.5+TPR/2-FPR/2)

plot(res[,-col],col=res[,col])



#######################################################################################

# training[,3] <- as.numeric(as.character(training[,3]))
# training[,3] <- training[,3]-1
ser <- c()
for(k in 1:(sum(training[,col]==1)-1)){
  res <- ubSMOTE(training[,-col], as.factor(training[,col]), perc.over = N, k=20,perc.under = 0)
  # res[,3] <- as.factor(res[,3])
  res <- cbind(res$X,res$Y)
  colnames(res) <- colnames(training)
  res <- rbind(training,res)
  
  fit <- rpart(fo,res)
  tab <- print(table(predict(fit,testing[,-col],type="class"),testing[,col]))
  TPR <- tab[1,1]/sum(tab[,1])
  FPR <- tab[1,2]/sum(tab[,2])
  AUC3 <- print(0.5+TPR/2-FPR/2)
  
  plot(res[,-col],col=res[,col])
  ser <- c(ser,AUC3)
}
#######################################################################################

# training[,3] <- as.numeric(as.character(training[,3]))
# training[,3] <- training[,3]-1
res <- BSMOTE1(training, col, m=20, k=6,s = 2)

res <- cbind(res$d.synTotal,1)
colnames(res) <- colnames(training)
res <- rbind(training,res)
# res[,3] <- as.factor(res[,3])
# res <- cbind(res$X,res$Y)
# 
# res <- rbind(training,res)

fit <- rpart(fo,res)
tab <- print(table(predict(fit,testing[,-col],type="class"),testing[,col]))
TPR <- tab[1,1]/sum(tab[,1])
FPR <- tab[1,2]/sum(tab[,2])
AUC4 <- print(0.5+TPR/2-FPR/2)

plot(res[,-col],col=res[,col])
