
evaluateModels <- function(fname,m) {
  
  require(tseries)
  require(nnet)
  require(xtable)
  require(pracma)
  require(matrixStats)
  
  CurWd <- getwd()
  
  setwd(CurWd)
  
  
  # first create split in training and testing set, just once, for basic analysis
  ###set.seed(1000+1)
  
  set.seed(1000+m)
  
  idx <- 1:1183
  randpermVec <- randperm(idx)
  idx <- randpermVec
  
  N1 <- floor(1183*0.5)
  N2 <- 1183 -N1
  N  <- N1 + N2
  
  idx1 <- idx[1:N1]
  idx2 <- idx[(N1+1):(N)]
  
  
  #first pool data for all time points together
  dataMatALL <- matrix(NA,0,7)
  cp=0
  
  #fname <- paste("data_neural_MX",".txt",sep="")
  data <- scan(file=fname,what=numeric())
  dataMat <- matrix(data,nrow=length(data)/7,ncol=7,byrow=T)
  
  
  dataFram <- as.data.frame(dataMat)
  
  #names(dataFram) <- c("TUMagneticFlux","Hel1","Hel2","Hel3","Hel4","Hel5","Hel6",
  #                     "Hel7","Hel8","Hel9","Hel10","Hel11","Hel12",
  #                     "FreeEn1","FreeEn2","FreeEn3","FreeEn4","FreeEn5","FreeEn6",
  #                     "FreeEn7","FreeEn8","FreeEn9","FreeEn10","FreeEn11","FreeEn12",
  #                     "TotalNoMagneticNullPoints","MFluxNearMPolarityILines","yGTM1","yIN-C1-C9")
  names(dataFram) <- c("BEff","IsingEn1","IsingEn2","ImbalanceRatio","GlobalNonNeutralityF","NetNonNeutralizedC",
                       "yGTM1")
  
  names(dataMat) <- c("BEff","IsingEn1","IsingEn2","ImbalanceRatio","GlobalNonNeutralityF","NetNonNeutralizedC",
                      "yGTM1")
  
  
  dataMatf <- as.matrix(dataFram)
  
  
  #dependent variables
  
  y_M1_flare <- y_C1_C9_flare <- numeric(dim(dataMatf)[1])
  for (i in 1:dim(dataMatf)[1]) {
    y_M1_flare[i]    <- as.numeric(dataMatf[i,7] )
    #y_C1_C9_flare[i] <- as.numeric(dataMatf[i,29] )
  }
  
  
  #now proceed to nnet
  
  #M class flare
  y<- y_M1_flare
  #C class flare
  ###y<- y_C1_flare
  #Χ class flare
  ###y<- y_X1_flare
  
  
  te <- dataFram
  teRaw <- te
  
  
  #max elements of columns
  #colMaxs(as.matrix(dataFram))
  #[[1] 5139.67000 3508.14000    6.86299 2862.89000    1.00000  147.83100    1.00000
  #min elements of columns
  #colMins(as.matrix(dataFram))
  #[1] 0.0000000 4.7928200 0.1412310 0.0082675 0.0173851 0.2788020 0.0000000
  
  #re-scaling
  #col. 1,  divide by 1000
  #col. 2,  divide by 1000
  #col. 3,  divide by 1, do nothing
  #col. 4,  divide by 1000
  #col. 5,  divide by 1, do nothing
  #col. 6,  divide by 100
  
  te$BEff <-     te$BEff / 1000
  te$IsingEn1 <- te$IsingEn1 / 1000
  te$IsingEn2 <- te$IsingEn2 / 1
  te$ImbalanceRatio <- te$ImbalanceRatio / 1000
  te$GlobalNonNeutralityF <- te$GlobalNonNeutralityF / 1
  te$NetNonNeutralizedC <- te$NetNonNeutralizedC / 100
  
  dataMatf1 <- dataMatf[idx1,]
  te1       <- te[idx1,]
  
  dataMatf2 <- dataMatf[idx2,]
  te2       <- te[idx2,]
  
  #now on:
  te <- te1  # for training
  teTst <- te2
  
  #lm
  #m1 <- lm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7,data=te)
  m1 <- lm( yGTM1 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
              GlobalNonNeutralityF+NetNonNeutralizedC,data=te)
  
  summary(m1)
  
  jpeg(paste("linear_model_combined_YANNIS_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yGTM1,xlab="observation",ylab="value of y",main=paste("lm model training original split",sep=""))
  lines(m1$fitted.values,col=2)
  dev.off()
  
  
  #xtable
  modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m1)[4]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(7)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\BEff$"
  mychar[3]  <- "$\\IsingEn1$"
  mychar[4]  <- "$\\IsingEn2$"
  mychar[5]  <- "$\\ImbalanceRatio$"
  mychar[6]  <- "$\\GlobalNonNeutralityF$"
  mychar[7] <- "$\\NetNonNeutralizedC$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:7],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Linear model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("linear_model_original_split","_train.tex",sep=""))
  #end xtable
  
  #glm
  #m2 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(probit))
  m2 <- glm( yGTM1 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
               GlobalNonNeutralityF+NetNonNeutralizedC,data=te,family=binomial(probit))
  
  summary(m2)
  
  jpeg(paste("probit_model_combined_YANNIS_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yGTM1,xlab="observation",ylab="value of y",main=paste("glm training with probit link original split ",sep=""))
  lines(m2$fitted.values,col=3)
  dev.off()
  
  
  #xtable
  #modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m2)[12]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(7)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\BEff$"
  mychar[3]  <- "$\\IsingEn1$"
  mychar[4]  <- "$\\IsingEn2$"
  mychar[5]  <- "$\\ImbalanceRatio$"
  mychar[6]  <- "$\\GlobalNonNeutralityF$"
  mychar[7] <- "$\\NetNonNeutralizedC$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:7],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Probit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("probit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #glm
  #m3 <- glm( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te,family=binomial(logit))
  
  m3 <- glm( yGTM1 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
               GlobalNonNeutralityF+NetNonNeutralizedC,data=te,family=binomial(logit))
  
  summary(m3)
  
  
  jpeg(paste("logit_model_combined_YANNIS_1st_trn_orig_split",".jpeg",sep=""))
  plot(te$yGTM1,xlab="observation",ylab="value of y",main=paste("glm training with logit link original split ",sep=""))
  lines(m3$fitted.values,col=4)
  dev.off()
  
  
  #xtable
  #modelFit <- as.data.frame( round(coef(m1),6))
  modelFit <- as.data.frame( round(summary(m3)[12]$coefficients,6))
  outD <- modelFit
  outD <- xtable(modelFit)
  #outD <- cbind("Param" = rep(paste("$\\theta_", 1:55, "$", sep = ""), len = nrow(outD)), outD)
  mychar <- character(7)
  mychar[1]  <- "$\\INTCPT$"
  mychar[2]  <- "$\\BEff$"
  mychar[3]  <- "$\\IsingEn1$"
  mychar[4]  <- "$\\IsingEn2$"
  mychar[5]  <- "$\\ImbalanceRatio$"
  mychar[6]  <- "$\\GlobalNonNeutralityF$"
  mychar[7] <- "$\\NetNonNeutralizedC$"
  
  
  outD <- cbind("Param" = rep(paste(mychar[1:7],sep = " "), len = nrow(outD)), outD)
  
  cap <- paste("Logit model original split", "training results", sep = " ")
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x)
  print(xtable(outD, caption = cap,digits=c(4)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("logit_model_original_split","_train.tex",sep=""))
  #end xtable
  
  
  #bayesQR
  require(bayesQR)    
  #m4 <- bayesQR( y ~ sunspot_Area + dMW2 + dMW3+ dMW4+ dMW5+ dMW6+ dMW7, data=te, quantile=seq(.1,.9,.1), ndraw=4000)
  ##m4 <- bayesQR( yGTM1 ~ TUMagneticFlux + Hel1 + Hel2+ Hel3+ Hel4+ Hel5+ Hel6 +
  ##                 Hel7+Hel8+Hel9+Hel10+Hel11+Hel12 +
  ##                 FreeEn1+FreeEn2+FreeEn3+FreeEn4+FreeEn5+FreeEn6 +
  ##                 FreeEn7+FreeEn8+FreeEn9+FreeEn10+FreeEn11+FreeEn12 +              
  ##                 TotalNoMagneticNullPoints + MFluxNearMPolarityILines,data=te,quantile=seq(.1,.9,.1), ndraw=4000)
  #m4 <- bayesQR( yGTM1 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
  #                 GlobalNonNeutralityF+NetNonNeutralizedC,data=te,quantile=seq(.1,.7,0.6/8), ndraw=4000)
  ###m4 <- bayesQR( yGTM1 ~ BEff+IsingEn1+IsingEn2+ImbalanceRatio+
  ###                 GlobalNonNeutralityF+NetNonNeutralizedC,data=te,quantile=seq(.1,.9,0.1), ndraw=4000)
  
  
  ###summary(m4)[5]
  
  require(matrixStats)
  
  
  #svm
  require(e1071)    
  
  p.svm <- svm(cbind(te$BEff,te$IsingEn1,te$IsingEn2,te$ImbalanceRatio,te$GlobalNonNeutralityF,te$NetNonNeutralizedC),
               te$yGTM1,probability=TRUE)
  
  jpeg(paste("SVM_model_combined_YANNIS_trn_orig_split",".jpeg",sep=""))
  plot(te$yGTM1,xlab="observation",ylab="value of y",main=paste("SVM with probability option, combined regressors\n", "Training, Original Split",sep=" "))
  lines(p.svm$fitted, col=6)
  dev.off()
  
  
  
  #predict
  ##p.svm.predict <- predict(p.svm, newdata=teTst[,-c(28,29)])
  ##later ...
  
  require(nnet)
  
  #set.seed(1000+1)
  #set.seed(1000+2)
  #set.seed(1000+3)
  ###set.seed(1000+jj)
  ###set.seed(1000*cp+10*iNode+tries)    #suitable for parallel loops in jj, tries, iNode (segmentations
  
  #M class flare
  #y<- y_M1_flare
  
  
  for(tries in 1:1) {
    
    #for (iNode in 21:55) {
    #for (iNode in 11:21) {
    #for (iNode in 11:55) {
    for (iNode in 12:12) {      
      
      #set.seed(1000*cp+10*iNode+tries)    #suitable for parallel loops in jj, tries, iNode (segmentations
      ###set.seed(10*iNode+tries)             #suitable for parallel loops in tries, iNode (segmentations         
      set.seed(1000*m+10*iNode+tries)             #suitable for parallel loops in tries, iNode (segmentations         
      
      cat("MCiter:... ",m,"tries:... ",tries," iNode:... ",iNode,"\n") 
      
      p.nnet <- nnet(cbind(te$BEff,te$IsingEn1,te$IsingEn2,te$ImbalanceRatio,te$GlobalNonNeutralityF,te$NetNonNeutralizedC),
                     te$yGTM1,entropy=T,maxit=2000,MaxNWts=2000,size=iNode)  #binary outcome, used CML = entropy 
      #te$yGTM1,linout=TRUE, maxit=2000,MaxNWts=2000,size=iNode) # use linear outcome, to see what happens
      
      p <- p.nnet
      jpeg(paste("MLP_model_combined_YANNIS_trn_orig_split","_Tries_",tries,"_iNode_",iNode,".jpeg",sep=""))
      plot(te$yGTM1,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors\n", "Training, Original Split,","tries:",tries,"iNode:",iNode,sep=" "))
      lines(p$fitted.values,col=5)
      dev.off()
      
      save.image(paste("mlp_object_p_","orig_split","_iNode_",iNode,"_tries_",tries,"_code_",10*iNode+tries,".RData",sep=""))
      ###  }
      ###}
      
      #predict with nnet
      #newdata:
      #teTst
      
      
      #safe mode
      yTst <- teTst$yGTM1
      teTst$yGTM1 <- -999
      
      p.nnet.predict <- predict(p.nnet,newdata=teTst)
      
      #restore
      teTst$yGTM1 = yTst
      
      jpeg(paste("MLP_model_combined_YANNIS_testing_orig_split",".jpeg",sep=""))
      plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("MLP with entropy option, combined regressors,\n",
                                                                       "Testing, original split,","tries:",tries,"iNode:",iNode,sep=" "))
      #plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("MLP with linout option, combined regressors,\n",
      #                                                             "Testing, original split,","tries:",tries,"iNode:",iNode,sep=" "))
      lines(p.nnet.predict,col=5)
      dev.off()
      
      
      p.nnet.predict.01 <- numeric(length(p.nnet.predict))
      
      accuracy0_all <- numeric(101)   # mlp is "0"
      tss0_all      <- numeric(101)
      hss0_all      <- numeric(101)
      accuracy1_all <- numeric(101)   # lm is  "1"
      tss1_all      <- numeric(101)
      hss1_all      <- numeric(101)
      accuracy2_all <- numeric(101)   # probit is "2"
      tss2_all      <- numeric(101)
      hss2_all      <- numeric(101)
      accuracy3_all <- numeric(101)   # logit is "3"
      tss3_all      <- numeric(101)
      hss3_all      <- numeric(101)
      accuracy4_all <- numeric(101)   # bayesQR is "4"
      tss4_all      <- numeric(101)
      hss4_all      <- numeric(101)
      accuracy5_all <- numeric(101)   # SVM is "5"
      tss5_all      <- numeric(101)
      hss5_all      <- numeric(101)
      
      
      ccp <- 0
      accuracy <- NA
      trueSS   <- NA
      HeidkeSS <- NA
      for (thresHOLD in seq(0.00,1.00,0.01) )
      {
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        ccp <- ccp + 1
        for (i in 1:length(p.nnet.predict)) {
          response=0
          if (!is.na(p.nnet.predict[i])) {
            if (p.nnet.predict[i] > thresHOLD) {
              response=1
            } 
          }
          p.nnet.predict.01[i] <- response
        }
        
        cfmat <- table(p.nnet.predict.01, teTst$yGTM1)
        
        write.table(cfmat,paste("cfmat","_combined_YANNIS_testing_MLP_","Orig_Split_","_iNode_",iNode,"_thresHOLD_",thresHOLD,".txt",sep=""))
        
        accuracy <- NA
        trueSS   <- NA
        HeidkeSS <- NA
        
        if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
          
          accuracy <- sum(diag(cfmat)) / sum(cfmat)
          
          a=cfmat[2,2]
          d=cfmat[1,1]
          b=cfmat[2,1] 
          c=cfmat[1,2]
          
          #trueSS <- TSS.Stat(cfmat)
          
          trueSS <- (a*d-b*c) / ((a+c)*(b+d))
          
          HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
          
          accuracy0_all[ccp] <- accuracy
          tss0_all[ccp] <- trueSS
          hss0_all[ccp] <- HeidkeSS
          
          
          #The score has a range of -1 to +1, with 0 representing no skill.
          #Negative values would be associated with "perverse" forecasts,
          #and could be converted to positive skill simply by replacing
          #all the yes forecasts with no and vice-versa.
          #The KSS is also the difference between the hit rate and false alarm rate,
          #KSS=H-F. 
          
          #hit rate and false alarm
          
          H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
          F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
          
          flag = abs(trueSS - (H-F)) < 10^(-4)
          
          #cat("dataset used as test set finished:...",jj)
          ##cat("accuracy:...",accuracy,"\n")
          ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
          ##cat("verify:...",flag,"\n")
          ##cat("HSS:... ",round(HeidkeSS,6),"\n")
          
        }
        
        
        
        #predict lm
        m1.predict <- predict(m1,newdata=teTst)
        
        jpeg(paste("linear_model_model_YANNIS_testing_Orig_Split",".jpeg",sep=""))
        plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("linear model, combined regressors,\n",
                                                                         "Testing, Original Split,",sep=" "))
        
        lines(m1.predict,col=2)
        dev.off()
        
        m1.predict.01 <- numeric(length(m1.predict))
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        for (i in 1:length(m1.predict)) {
          response=0
          if (!is.na(m1.predict[i])) {
            if (m1.predict[i] > thresHOLD) {
              response=1
            } 
          }
          m1.predict.01[i] <- response
        }
        
        cfmat <- table(m1.predict.01, teTst$yGTM1)
        
        write.table(cfmat,paste("cfmat","_combined_YANNIS_testing_lm_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
        
        accuracy <- NA
        trueSS   <- NA
        HeidkeSS <- NA
        
        if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
          
          accuracy <- sum(diag(cfmat)) / sum(cfmat)
          
          a=cfmat[2,2]
          d=cfmat[1,1]
          b=cfmat[2,1] 
          c=cfmat[1,2]
          
          #trueSS <- TSS.Stat(cfmat)
          
          trueSS <- (a*d-b*c) / ((a+c)*(b+d))
          
          HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
          
          accuracy1_all[ccp] <- accuracy
          tss1_all[ccp] <- trueSS
          hss1_all[ccp] <- HeidkeSS
          
          
          #The score has a range of -1 to +1, with 0 representing no skill.
          #Negative values would be associated with "perverse" forecasts,
          #and could be converted to positive skill simply by replacing
          #all the yes forecasts with no and vice-versa.
          #The KSS is also the difference between the hit rate and false alarm rate,
          #KSS=H-F. 
          
          #hit rate and false alarm
          
          H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
          F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
          
          flag = abs(trueSS - (H-F)) < 10^(-4)
          
          #cat("dataset used as test set finished:...",jj)
          ##cat("accuracy:...",accuracy,"\n")
          ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
          ##cat("verify:...",flag,"\n")
          ##cat("HSS:... ",round(HeidkeSS,6),"\n")
          
        }
        
        
        #predict glm probit
        ##m2.predict <- predict(m2,newdata=teTst)
        m2.predict <- predict(m2,newdata=teTst,type="response")
        
        
        jpeg(paste("probit_model_model_YANNIS_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("probit model, combined regressors,\n",
                                                                         "Testing, Original Split",sep=" "))
        lines(m2.predict,col=3)
        dev.off()
        
        m2.predict.01 <- numeric(length(m2.predict))
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        for (i in 1:length(m2.predict)) {
          response=0
          if (!is.na(m2.predict[i])) {
            if (m2.predict[i] > thresHOLD) {
              response=1
            } 
          }
          m2.predict.01[i] <- response
        }
        
        cfmat <- table(m2.predict.01, teTst$yGTM1)
        
        write.table(cfmat,paste("cfmat","_combined_YANNIS_testing_probit_","Orig_Split_","_thresHOLD_",thresHOLD,".txt",sep=""))
        
        accuracy <- NA
        trueSS   <- NA
        HeidkeSS <- NA
        
        
        if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
          
          accuracy <- sum(diag(cfmat)) / sum(cfmat)
          
          a=cfmat[2,2]
          d=cfmat[1,1]
          b=cfmat[2,1] 
          c=cfmat[1,2]
          
          #trueSS <- TSS.Stat(cfmat)
          
          trueSS <- (a*d-b*c) / ((a+c)*(b+d))
          
          HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
          
          accuracy2_all[ccp] <- accuracy
          tss2_all[ccp] <- trueSS
          hss2_all[ccp] <- HeidkeSS
          
          
          #The score has a range of -1 to +1, with 0 representing no skill.
          #Negative values would be associated with "perverse" forecasts,
          #and could be converted to positive skill simply by replacing
          #all the yes forecasts with no and vice-versa.
          #The KSS is also the difference between the hit rate and false alarm rate,
          #KSS=H-F. 
          
          #hit rate and false alarm
          
          H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
          F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
          
          flag = abs(trueSS - (H-F)) < 10^(-4)
          
          #cat("dataset used as test set finished:...",jj)
          ##cat("accuracy:...",accuracy,"\n")
          ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
          ##cat("verify:...",flag,"\n")
          ##cat("HSS:... ",round(HeidkeSS,6),"\n")
          
        }
        
        
        #predict glm logit
        
        ##m3.predict <- predict(m3,newdata=teTst)
        m3.predict <- predict(m3,newdata=teTst,type="response")
        
        jpeg(paste("logit_model_model_YANNIS_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("logit model, combined regressors","\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(m3.predict,col=4)
        dev.off()
        
        m3.predict.01 <- numeric(length(m3.predict))
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        for (i in 1:length(m3.predict)) {
          response=0
          if (!is.na(m3.predict[i])) {
            if (m3.predict[i] > thresHOLD) {
              response=1
            } 
          }
          m3.predict.01[i] <- response
        }
        
        cfmat <- table(m3.predict.01, teTst$yGTM1)
        
        write.table(cfmat,paste("cfmat","_combined_YANNIS_testing_logit_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
        
        accuracy <- NA
        trueSS   <- NA
        HeidkeSS <- NA
        
        if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
          
          accuracy <- sum(diag(cfmat)) / sum(cfmat)
          
          a=cfmat[2,2]
          d=cfmat[1,1]
          b=cfmat[2,1] 
          c=cfmat[1,2]
          
          #trueSS <- TSS.Stat(cfmat)
          
          trueSS <- (a*d-b*c) / ((a+c)*(b+d))
          
          HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
          
          accuracy3_all[ccp] <- accuracy
          tss3_all[ccp] <- trueSS
          hss3_all[ccp] <- HeidkeSS
          
          
          #The score has a range of -1 to +1, with 0 representing no skill.
          #Negative values would be associated with "perverse" forecasts,
          #and could be converted to positive skill simply by replacing
          #all the yes forecasts with no and vice-versa.
          #The KSS is also the difference between the hit rate and false alarm rate,
          #KSS=H-F. 
          
          #hit rate and false alarm
          
          H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
          F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
          
          flag = abs(trueSS - (H-F)) < 10^(-4)
          
          #cat("dataset used as test set finished:...",jj)
          ##cat("accuracy:...",accuracy,"\n")
          ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
          ##cat("verify:...",flag,"\n")
          ##cat("HSS:... ",round(HeidkeSS,6),"\n")
          
        }
        
  
        #predict bayesQR
        #not done
        
        #predict svm
        ###p.svm.predict <- predict(p.svm, newdata=teTst[,-c(28,29)])
        p.svm.predict <- predict(p.svm, newdata=teTst[,-c(7)])
        jpeg(paste("SVM_model_model_YANNIS_testing_orig_split",".jpeg",sep=""))
        plot(teTst$yGTM1,xlab="observation",ylab="value of y",main=paste("SVM model, combined regressors","\n",
                                                                         "Testing, Original Split",sep=" "))
        
        lines(p.svm.predict,col=6)
        dev.off()
        
        #because svm gives probabilities <0 and >1
        #first bound left to 0 and right to 1
        p.svm.predict.new <- numeric(length(p.svm.predict))
        for (i in 1:length(p.svm.predict)) {
          p.svm.predict.new[i] <- max(0,min(p.svm.predict[i],1))
        }
        
        p.svm.predict.01 <- numeric(length(p.svm.predict.new))
        #thresHOLD <- 0.50
        #thresHOLD <- 0.25
        
        for (i in 1:length(p.svm.predict.new)) {
          response=0
          if (!is.na(p.svm.predict.new[i])) {
            if (p.svm.predict.new[i] > thresHOLD) {
              response=1
            } 
          }
          p.svm.predict.01[i] <- response
        }
        
        cfmat <- table(p.svm.predict.01, teTst$yGTM1)
        
        write.table(cfmat,paste("cfmat","_combined_YANNIS_testing_SVM_","Orig_Split","_thresHOLD_",thresHOLD,".txt",sep=""))
        
        accuracy <- NA
        trueSS   <- NA
        HeidkeSS <- NA
        
        
        if(dim(cfmat)[1]==2 && dim(cfmat)[2]==2) {
          
          accuracy <- sum(diag(cfmat)) / sum(cfmat)
          
          a=cfmat[2,2]
          d=cfmat[1,1]
          b=cfmat[2,1] 
          c=cfmat[1,2]
          
          #trueSS <- TSS.Stat(cfmat)
          
          trueSS <- (a*d-b*c) / ((a+c)*(b+d))
          
          HeidkeSS <- 2 * (a*d-b*c) / ( (a+c)*(c+d) + (a+b)*(b+d) )
          
          accuracy5_all[ccp] <- accuracy
          tss5_all[ccp] <- trueSS
          hss5_all[ccp] <- HeidkeSS
          
          
          #The score has a range of -1 to +1, with 0 representing no skill.
          #Negative values would be associated with "perverse" forecasts,
          #and could be converted to positive skill simply by replacing
          #all the yes forecasts with no and vice-versa.
          #The KSS is also the difference between the hit rate and false alarm rate,
          #KSS=H-F. 
          
          #hit rate and false alarm
          
          H = a / (a+c)    #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko2.htm
          F = (b) / (b+d)  #http://www.eumetcal.org/resources/ukmeteocal/verification/www/english/msg/ver_categ_forec/uos2/uos2_ko3.htm
          
          flag = abs(trueSS - (H-F)) < 10^(-4)
          
          #cat("dataset used as test set finished:...",jj)
          ##cat("accuracy:...",accuracy,"\n")
          ##cat("TSS:... ",paste(round(trueSS,6),round(H-F,6),sep=" "))
          ##cat("verify:...",flag,"\n")
          ##cat("HSS:... ",round(HeidkeSS,6),"\n")
          
        }
        
        
      } ### thresHOLD loop
      
      #save all wrt thresHOLD arrays
      #mlp
      write.table(cbind(1:101,seq(0,1,0.01),accuracy0_all,tss0_all,hss0_all),file=paste("mlp_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      #lm
      write.table(cbind(1:101,seq(0,1,0.01),accuracy1_all,tss1_all,hss1_all),file=paste("lm_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      #probit
      write.table(cbind(1:101,seq(0,1,0.01),accuracy2_all,tss2_all,hss2_all),file=paste("probit_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      #logit
      write.table(cbind(1:101,seq(0,1,0.01),accuracy3_all,tss3_all,hss3_all),file=paste("logit_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      #bayesQR
      #write.table(cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all),file=paste("bayesQR_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      #SVM
      write.table(cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all),file=paste("SVM_SKILLS_testing_orig_split_","_tries_",tries,"_iNode_",iNode,".out",sep=""),row.names=FALSE,col.names=FALSE)
      
      res0 <- cbind(1:101,seq(0,1,0.01),accuracy0_all,tss0_all,hss0_all)
      
      
      jpeg(paste("mlp_model_YANNIS_testing_orig_split_","_SKILLS_iNode_",iNode,"_tries_",tries,".jpeg",sep=""))
      plot(res0[,2],res0[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",main=paste("skills wrt threshold for mlp\n",
                                                                                                                 "testing, original split,","iNode:",iNode,"tries:",tries,sep=" "))
      lines(res0[,2],res0[,4],type="l",lwd=2.5,col=3)
      lines(res0[,2],res0[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res1 <- cbind(1:101,seq(0,1,0.01),accuracy1_all,tss1_all,hss1_all)
      
      
      jpeg(paste("lm_model_YANNIS_testing_orig_split_","_SKILLS_iNode_",iNode,"_tries_",tries,".jpeg",sep=""))
      plot(res1[,2],res1[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",main=paste("skills wrt threshold for linear model\n",
                                                                                                                 "testing, original split,","iNode:",iNode,"tries:",tries,sep=" "))
      lines(res1[,2],res1[,4],type="l",lwd=2.5,col=3)
      lines(res1[,2],res1[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res2 <- cbind(1:101,seq(0,1,0.01),accuracy2_all,tss2_all,hss2_all)
      
      jpeg(paste("probit_model_YANNIS_testing_orig_split_","_SKILLS_iNode_",iNode,"_tries_",tries,".jpeg",sep=""))
      plot(res2[,2],res2[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for probit\n",
                      "testing, original split,","iNode:",iNode,"tries:",tries,sep=" "))
      lines(res2[,2],res2[,4],type="l",lwd=2.5,col=3)
      lines(res2[,2],res2[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      
      dev.off()
      
      
      
      res3 <- cbind(1:101,seq(0,1,0.01),accuracy3_all,tss3_all,hss3_all)
      
      jpeg(paste("logit_model_YANNIS_testing_orig_split_","_SKILLS_iNode_",iNode,"_tries_",tries,".jpeg",sep=""))    
      plot(res3[,2],res3[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for logit\n",
                      "testing, original split,","iNode:",iNode,"tries:",tries,sep=" "))
      lines(res3[,2],res3[,4],type="l",lwd=2.5,col=3)
      lines(res3[,2],res3[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      dev.off()
      
      res4 <- cbind(1:101,seq(0,1,0.01),accuracy4_all,tss4_all,hss4_all)
      #not done
      
      res5 <- cbind(1:101,seq(0,1,0.01),accuracy5_all,tss5_all,hss5_all)
      
      jpeg(paste("SVM_model_YANNIS_testing_orig_split_","_SKILLS_iNode_",iNode,"_tries_",tries,".jpeg",sep=""))    
      plot(res5[,2],res5[,3],type="l",col=2,lwd=2.5,xlab="probability threshold",ylab="skills values",xlim=c(0,1),ylim=c(0,1),
           main=paste("skills wrt threshold for SVM\n",
                      "testing, original split,","iNode:",iNode,"tries:",tries,sep=" "))
      lines(res5[,2],res5[,4],type="l",lwd=2.5,col=3)
      lines(res5[,2],res5[,5],type="l",lwd=2.5,col=4)    
      
      legend(0.7,0.6, # places a legend at the appropriate place 
             c("acc","tss","hss"), # puts text in the legend
             lty=c(1,1,1), # gives the legend appropriate symbols (lines)
             lwd=c(2.5,2.5,2.5),col=c(2,3,4)) # gives the legend lines the correct color and width
      dev.off()
      
      
      #} ### iNode loop
      #} ### tries loop
      #} ### jj loop - crossvalidation in every year

      #ROC curves
      require(ROCR)
      
      #neural network model    
      jpeg(paste("ROC_neural_network_model_","tries_",tries,"_iNode",iNode,".jpeg",sep=""))
      pred <- prediction(p.nnet.predict,teTst$yGTM1)
      perf <- performance(pred,"tpr","fpr")
      plot(perf,main=c("ROC curve for Neural Network Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      auc.perf = performance(pred, measure = "auc")
      auc.perf@y.values
      
      
      #linear model
      jpeg("ROC_linear_model.jpeg")
      pred <- prediction(m1.predict,teTst$yGTM1)
      perf <- performance(pred,"tpr","fpr")
      plot(perf,main=c("ROC curve for Linear Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      auc.perf = performance(pred, measure = "auc")
      auc.perf@y.values
      
      
      #probit model
      jpeg("ROC_probit_model.jpeg")
      pred2 <- prediction(m2.predict,teTst$yGTM1)
      perf2 <- performance(pred2,"tpr","fpr")
      plot(perf2,main=c("ROC curve for Probit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()  
      
      auc.perf2 = performance(pred2, measure = "auc")
      auc.perf2@y.values
      
      
      
      #logit model
      jpeg("ROC_logit_model.jpeg")
      pred3 <- prediction(m3.predict,teTst$yGTM1)
      perf3 <- performance(pred3,"tpr","fpr")
      plot(perf3,main=c("ROC curve for Logit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()  
      
      auc.perf3 = performance(pred3, measure = "auc")
      auc.perf3@y.values     
      
      
      #bayesQR model
      #not done
      
      #SVM model
      jpeg("ROC_SVM_model.jpeg")
      pred5 <- prediction(p.svm.predict.new,teTst$yGTM1)
      perf5 <- performance(pred5,"tpr","fpr")
      plot(perf5,main=c("ROC curve for SVM Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)")) 
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      auc.perf5 = performance(pred5, measure = "auc")
      auc.perf5@y.values     
      
      
      #Reliability Diagrams
      
      #neural network model
      library(verification)
      
      mod0 <- verify(obs = teTst$yGTM1, pred = p.nnet.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod1, CI=TRUE)
      #plot(mod1)
      jpeg(paste("ReliabilityDiagram_Neural_Network_model","_Tries_",tries,"_iNode_",iNode,".jpeg",sep=""))
      plot(mod0, main=c("Reliability Diagram for Neural Network Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg(paste("ReliabilityDiagram_withErrorsBars_Neural_Network_model","_Tries_",tries,"_iNode_",iNode,".jpeg",sep=""))
      plot(mod0, CI=T, main=c("Reliability Diagram for Neural Network Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      
      #linear model
      library(verification)
      
      #because lm gives probabilities <0 and >1
      #first bound left to 0 and right to 1
      m1.predict.new <- numeric(length(m1.predict))
      for (i in 1:length(m1.predict)) {
        m1.predict.new[i] <- max(0,min(m1.predict[i],1))
      }
      #mod1 <- verify(obs = teTst$yGTM1, pred = m1.predict.new)
      mod1 <- verify(obs = teTst$yGTM1, pred = m1.predict.new,thresholds=seq(0,1,0.05))
      
      
      #plot(mod1, CI=TRUE)
      #plot(mod1)
      jpeg("ReliabilityDiagram_linear_model.jpeg")
      plot(mod1, main=c("Reliability Diagram for Linear Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_linear_model.jpeg")
      plot(mod1, CI=T, main=c("Reliability Diagram for Linear Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()     
      
      
      
      #probit model
      library(verification)
      
      #mod2 <- verify(obs = teTst$yGTM1, pred = m2.predict)
      mod2 <- verify(obs = teTst$yGTM1, pred = m2.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod2, CI=TRUE)
      #plot(mod2)
      jpeg("ReliabilityDiagram_probit_model.jpeg")
      plot(mod2, main=c("Reliability Diagram for Probit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_probit_model.jpeg")
      plot(mod2, CI=T, main=c("Reliability Diagram for Probit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()          
      
      
      
      #logit model
      library(verification)
      
      #mod3 <- verify(obs = teTst$yGTM1, pred = m3.predict)
      mod3 <- verify(obs = teTst$yGTM1, pred = m3.predict,thresholds=seq(0,1,0.05))
      
      #plot(mod3, CI=TRUE)
      #plot(mod3)
      jpeg("ReliabilityDiagram_logit_model.jpeg")
      plot(mod3, main=c("Reliability Diagram for Logit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      jpeg("ReliabilityDiagram_withErrorsBars_logit_model.jpeg")
      plot(mod3, CI=T, main=c("Reliability Diagram for Logit Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()               
      
      
      
      #bayesQR model
      #not done
      
      #SVM model
      library(verification)
      
      #because svm gives probabilities <0 and >1
      #first bound left to 0 and right to 1
      p.svm.predict.new <- numeric(length(p.svm.predict))
      for (i in 1:length(p.svm.predict)) {
        p.svm.predict.new[i] <- max(0,min(p.svm.predict[i],1))
      }
      #mod5 <- verify(obs = teTst$yGTM1, pred = p.svm.predict)
      #mod5 <- verify(obs = teTst$yGTM1, pred = p.svm.predict,thresholds=seq(0,1,0.05))
      mod5 <- verify(obs = teTst$yGTM1, pred = p.svm.predict.new,thresholds=seq(0,1,0.05))
      
      #plot(mod5, CI=TRUE)
      #plot(mod5)
      jpeg("ReliabilityDiagram_SVM_model.jpeg")
      plot(mod5, main=c("Reliability Diagram for SVM Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()
      
      
      jpeg("ReliabilityDiagram_withErrorsBars_SVM_model.jpeg")
      plot(mod5, CI=T, main=c("Reliability Diagram for SVM Model","\n","Original Test set Yannis Kontogiannis (private communication, 1st dataset)"))
      lines(seq(0,1,0.01),seq(0,1,0.01))
      dev.off()               
      
    } ### iNode loop
  } ### tries loop
  
  
  
  return(cbind(res0,res1,res2,res3,res4,res5))
}


calc_SDs <- function(res) {
  
  n1 <- dim(res[[1]])[1]
  n2 <- dim(res[[1]])[2]
  m  <- length(res)
  values <- numeric(m)
  
  myStds <- matrix(NA,n1,n2)
  
  for (i in 1:n1) {
    for (j in 1:n2) {
      values <- numeric(m)
      for (k in 1:m) {
        values[k] <- res[[k]][i,j]
      }
      myStds[i,j] <- sd(values)
    }
  }
  return(myStds) 
}

evaluateScore <- function(betas,X,y) {
  
  n <- length(y)
  p <- dim(X)[2]
  y <- -1+2*y
  firep <- numeric(n)
  sump  <- numeric(n)
  score <-0
  for (i in 1:n) {
    for (j in 1:p) {
      sump[i] <- sump[i] + X[i,j]*betas[j]
    }
    
    if (sump[i] > 0) {
      firep[i]=+1
    }
    if (sump[i] < 0) {
      firep[i]=-1
    }
    if (sump[i] == 0) {
      firep[i]=y[i]
    }
    score=score+firep[i] * y[i]
  }
  score <- score/n
  return(score)  
}  




















