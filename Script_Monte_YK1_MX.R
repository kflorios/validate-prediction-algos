
#setwd("C:/Storage/Kostas/Research/SolarFlares_Web/Sung-Hong_1stDataset/hong_first_dataset_flares_classification/R/x-ray-classification_first_dataset/Monte_SH1_M1class")
#setwd("C:/Storage/Kostas/Research/SolarFlares_Web/Yannis_1stDataset/Monte_YK1_M1class")

#root <- getwd()

source("functions_Monte_YK1_MX.R")

Scenario <- 1

R=200

res <- vector("list", R)

fname <- paste("data_neural_MX",".txt",sep="")

ptm <- proc.time() # Start the clock!

for (m in 1:1) {
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

out <- Reduce("+", resN) / length(resN)


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

#write.table(outD, file = paste(root, "/Results/coefTable_Scnr", Scenario, ".txt", sep = ""), row.names=F)                            
write.table(outD, file = paste(root, "/coefTable_Scnr", Scenario, ".txt", sep = ""), row.names=F)                            

# LaTeX output
#outD <- cbind("Param" = rep(paste("$\\beta_", 1:4, "$", sep = ""), len = nrow(outD)), outD)
outD <- cbind("Par" = rep(paste("$val_{", 0:100, "}$", sep = ""), len = nrow(outD)), outD)
###outD <- cbind(" " = c(sapply(1:Q, function (i) c(paste("Outcome", i), 
###                                                 rep("", length(betas) - 1)))), outD)
cap <- paste("Scenario ", Scenario, ", based on ", R, " datasets: ", 
             "Using YK1 dataset: ",
             "Method 0 is Neural Network",
             "Method 1 is Linear Regression",
             "Method 2 is Probit Regression ",
             "Method 3 is Logit Regression ", 
             "Method 4 is Baysian Quantile Regression (NA) ",
             "Method 5 is Support Vector Regresion.", sep = "")
print(xtable(outD, caption = cap,digits=c(2)), math.style.negative = TRUE, include.rownames = FALSE, 
      sanitize.text.function = function(x) x)




save.image(file="Monte_YK1_5methods_R200.RData")

gc()
