# print the probabilities of the trial sequences and the prediction error
rm(list=ls())

source("helper_functions/chooseMultinom.R")
source("helper_functions/chooseBinomial.R")
source("helper_functions/softmax.R")
source("computational_model/simulateObsAllTWFsequences.R")
source("helper_functions/InitVariab.R")
source("computational_model/lik_RescorlaWagner_obsALLTWF.R")

# list the files with the trial sequences
files<-list.files(pattern=".csv")

for (n in 1:5){
  
  # read the file
  currfile<-read.csv(files[n])
  
  # simulate data
  alpha<-0.1
  beta<-2
  
  simdata<-simulate_RW_obsALLTWFwithsequences(40, mu1 =c(0.90,0.10), mu2 = c(0.60, 0.40), 
                                              alpha = alpha, beta = beta, object_cat = currfile$object_cat)
  
  # get parameters
  paramsim<-lik_RescorlaWagner_obsALLTWF(simdata, alpha  = alpha, beta = beta, print = 3, changePoints = 3)

  # print 
  print(
    # print the probability
    ggplot(paramsim, aes(x=trialN))+
      #stat_summary(fun.y="mean",geom="line")+
      geom_line(aes(y=P1), size = 1.5, color = "blue")+
      geom_line(aes(y=P2),size = 1.5, color = "darkgreen")+
      ggtitle(paste("list N", n))+
      ylim( c(0,1))
    
    # print the prediction error (observational)
    ggplot(paramsim, aes(x=trialN))+
      #stat_summary(fun.y="mean",geom="line")+
      geom_line(aes(y=DeltaObs), size = 1.5, color = "red")+
      ggtitle(paste("Observational PE, list N", n))+
      ylim( c(0,1)) 
    
    # print the prediction error (Feedback=basked)
    ggplot(paramsim, aes(x=trialN))+
      #stat_summary(fun.y="mean",geom="line")+
      geom_line(aes(y=DeltaResp), size = 1.5, color = "red")+
      ggtitle(paste("Feedback-based PE, list N", n))+
      ylim( c(-1,1)) 
    
  )
}