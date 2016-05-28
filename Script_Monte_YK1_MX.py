# -*- coding: utf-8 -*-
"""
Created on Thu May 19 15:00:11 2016

@author: Kostas
"""

##cd "C:\Storage\Kostas\Research\WinPython\2016\validate-prediction-algos_1"

import rpy2

## http://rpy2.readthedocs.org/en/version_2.7.x/introduction.html#getting-started

import rpy2.robjects as robjects

# R package names
##packnames = ('ggplot2', 'hexbin')
###packnames = ('lme4','xtable','MASS')
packnames = ('tseries','nnet','xtable','pracma','matrixStats','e1071','ROCR','verification')

# import rpy2's package module
import rpy2.robjects.packages as rpackages

if all(rpackages.isinstalled(x) for x in packnames):
    have_tutorial_packages = True
else:
    have_tutorial_packages = False


if not have_tutorial_packages:
    # import R's utility package
    utils = rpackages.importr('utils')
    # select a mirror for R packages
    utils.chooseCRANmirror(ind=1) # select the first mirror in the list
    
    
if not have_tutorial_packages:
    # R vector of strings
    from rpy2.robjects.vectors import StrVector
    # file
    packnames_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
    if len(packnames_to_install) > 0:
        utils.install_packages(StrVector(packnames_to_install))    


#F = "functions_Monte_YK1_MX.py"
#execfile(F)


robjects.r('''
Script_Monte_YK1_MX <- function(fname,mspace) {
#root <- getwd()
#root  <- "C:/Storage/Kostas/Research/WinPython/2016/YK1_M1classOriginal"
 root <- "C:/Storage/Kostas/Research/WinPython/2016/validate-prediction-algos_1"
source("functions_Monte_YK1_MX.R")

Scenario <- 1

R=200

res <- vector("list", R)

#fname <- paste("data_neural_MX",".txt",sep="")

ptm <- proc.time() # Start the clock!

for (m in mspace) {
#for (m in 1:1) {
  #for (m in 1:1) {  
  
  modelFit <- try({
    # Calculate Results
    evaluateModels(fname,m)                 
  }, TRUE)
  
  
  res[[m]] <- if (!inherits(modelFit, "try-error")) modelFit else NULL
  
  cat("Data-set:", m, "finished.\n")
  
  save.image(file=paste(m,".RData"))
  
}

elapsed_time <- proc.time() - ptm  # Stop the clock


resN <- res[!sapply(res, is.null)]

out <- as.matrix(Reduce("+", resN) / length(resN))

write.table(out,"out.txt")

outD <- as.data.frame(cbind("thres" = out[,2],
                            "0ACC" = out[,3], 
                            "0TSS" = out[,4], 
                            "0HSS" = out[,5], 
                            "1ACC" = out[,8], 
                            "1TSS" = out[,9], 
                            "1HSS" = out[,10],                             
                            "2ACC" = out[,13], 
                            "2TSS" = out[,14], 
                            "2HSS" = out[,15],                             
                            "3ACC" = out[,18], 
                            "3TSS" = out[,19], 
                            "3HSS" = out[,20],                             
                            #"4ACC" = out[,23], # method 5 is now method 4
                            #"4TSS" = out[,24], 
                            #"4HSS" = out[,25],
                            "5ACC" = out[,28], # method 5 is now method 4
                            "5TSS" = out[,29], 
                            "5HSS" = out[,30]))

write.table(outD, file = paste(root, "/Results/coefTable_Scnr", Scenario, ".txt", sep = ""), row.names=F)                            


# LaTeX output
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outD <- cbind("Par" = rep(paste("$val_{", 0:100, "}$", sep = ""), len = nrow(outD)), outD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", length(mspace), " datasets: ", 
             "Using YK1 dataset: ",
             "Method 0 is Neural Network",
             "Method 1 is Linear Regression",
             "Method 2 is Probit Regression ",
             "Method 3 is Logit Regression ", 
             "Method 4 is Baysian Quantile Regression (NA) ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outD, caption = cap,digits=c(2)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)
print(xtable(outD, caption = cap,digits=c(2)), math.style.negative = TRUE, include.rownames = FALSE, 
        sanitize.text.function = function(x) x, file=paste("Monte_YK1_5methods_R200",".tex",sep=""))
      
save.image(file="Monte_YK1_5methods_R5.RData")

gc()

#return(as.matrix(outD))
return(res)
}
''')


r_Script_Monte_YK1_MX = robjects.globalenv['Script_Monte_YK1_MX']
print(r_Script_Monte_YK1_MX.r_repr())


robjects.r('''
calc_SDs <- function(res,mspace) {
  
  n1 <- dim(res[[1]])[1]
  n2 <- dim(res[[1]])[2]
  #m  <- length(res)
  m  <- length(mspace)
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
''')

r_calc_SDs = robjects.globalenv['calc_SDs']
print(r_calc_SDs.r_repr())

##########################MAIN PROGRAM#####################################
#res = r_Script_Monte_YK1_MX()
#res = r_Script_Monte_YK1_MX("data_neural_MX.txt",range(1,2))
fname="data_neural_MX.txt"
mspace=range(1,6)
res = r_Script_Monte_YK1_MX("data_neural_MX.txt",range(1,6))
##########################END MAIN PROGRAM#################################

import numpy as np

#1, average performance metrics
def avgRES(res,mspace):
    
    n=len(mspace)
    meanRES=np.zeros((101.,30.))    
    for m in mspace:
        meanRES[:,:] = meanRES[:,:] + res[m-1]
        
    meanRES=meanRES/n
    return meanRES
    
meanRES=avgRES(res,mspace)
#2, standard deviation in performance metrics

#stdRES = r_calc_SDs(res)
stdRES = r_calc_SDs(res,mspace)


#make CHARTS
#NN
import matplotlib.pyplot as plt0
plt0.clf()
line_1, = plt0.plot(meanRES[:,2],label='avg ACC')
line_2, = plt0.plot(meanRES[:,3],label='avg TSS')
line_3, = plt0.plot(meanRES[:,4], label='avg HSS')

line_4, = plt0.plot(stdRES.rx(True,3), label='sd ACC')
line_5, = plt0.plot(stdRES.rx(True,4), label='sd TSS')
line_6, = plt0.plot(stdRES.rx(True,5), label='sd HSS')
plt0.xlabel('probability threshold %')
plt0.ylabel('Skill Scores')
plt0.title('Neural Network')
plt0.legend(handles=[line_1, line_2, line_3, line_4, line_5, line_6 ],loc="best")
plt0.savefig('myfig0')

#SVM
import matplotlib.pyplot as plt5
plt5.clf()
line_1, = plt5.plot(meanRES[:,27],label='avg ACC')
line_2, = plt5.plot(meanRES[:,28],label='avg TSS')
line_3, = plt5.plot(meanRES[:,29],label='avg HSS')
line_4, = plt5.plot(stdRES.rx(True,28), label='sd ACC')
line_5, = plt5.plot(stdRES.rx(True,29), label='sd TSS')
line_6, = plt5.plot(stdRES.rx(True,30), label='sd HSS')
plt5.xlabel('probability threshold %')
plt5.ylabel('Skill Scores')
plt5.title('Support Vector Machine')
plt5.legend(handles=[line_1, line_2, line_3, line_4, line_5, line_6 ],loc=8)
plt5.savefig('myfig5')

#LM
import matplotlib.pyplot as plt1
plt1.clf()
line_1, = plt1.plot(meanRES[:,7],label='avg ACC')
line_2, = plt1.plot(meanRES[:,8],label='avg TSS')
line_3, = plt1.plot(meanRES[:,9],label='avg HSS')
line_4, = plt1.plot(stdRES.rx(True,8), label='sd ACC')
line_5, = plt1.plot(stdRES.rx(True,9), label='sd TSS')
line_6, = plt1.plot(stdRES.rx(True,10), label='sd HSS')
plt1.xlabel('probability threshold %')
plt1.ylabel('Skill Scores')
plt1.title('Linear Regression')
plt1.legend(handles=[line_1, line_2, line_3, line_4, line_5, line_6 ],loc="best")
plt1.savefig('myfig1')

#PROBIT
import matplotlib.pyplot as plt2
plt2.clf()
line_1, = plt2.plot(meanRES[:,12],label='avg ACC')
line_2, = plt2.plot(meanRES[:,13],label='avg TSS')
line_3, = plt2.plot(meanRES[:,14],label='avg HSS')
line_4, = plt2.plot(stdRES.rx(True,13), label='sd ACC')
line_5, = plt2.plot(stdRES.rx(True,14), label='sd TSS')
line_6, = plt2.plot(stdRES.rx(True,15), label='sd HSS')
plt2.xlabel('probability threshold %')
plt2.ylabel('Skill Scores')
plt2.title('Probit Regression')
plt2.legend(handles=[line_1, line_2, line_3, line_4, line_5, line_6 ],loc=8)
plt2.savefig('myfig2')

        
#LOGIT
import matplotlib.pyplot as plt3
plt3.clf()
line_1, = plt3.plot(meanRES[:,17],label='avg ACC')
line_2, = plt3.plot(meanRES[:,18],label='avg TSS')
line_3, = plt3.plot(meanRES[:,19],label='avg HSS')
line_4, = plt3.plot(stdRES.rx(True,18), label='sd ACC')
line_5, = plt3.plot(stdRES.rx(True,19), label='sd TSS')
line_6, = plt3.plot(stdRES.rx(True,20), label='sd HSS')
plt3.xlabel('probability threshold %')
plt3.ylabel('Skill Scores')
plt3.title('Logit Regression')
plt3.legend(handles=[line_1, line_2, line_3, line_4, line_5, line_6 ],loc=8)
plt3.savefig('myfig3')
