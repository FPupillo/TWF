# script to simulate PE and change in priors
for (i in 1){
  rm(list=ls())
  
  # retrieve all the functions
  library(ggplot2)
  library(dplyr)
  library(beepr)
  library(lme4)
  library(lmerTest)
  source("helper_functions/chooseMultinom.R")
  source("helper_functions/chooseBinomial.R")
  source("helper_functions/softmax.R")
  source("computational_model/simulateObsAll.R")
  source("helper_functions/InitVariab.R")
  source("computational_model/lik_RescorlaWagner_obsALL.R")
  
  # probability
  #mu<-c(0.80, 0.10, 0.10)
  
  
  #mu<-c(0.75, 0.25)
  mu <-c(0.70,0.15, 0.15)
  # how many scenes?
  sceneNum<-length(mu)
  
  # how many trials per scene
  T<-80
  
  # for the alpha, we could use average values
  alphaMean<-0.43
  alphaSD<-0.23
  betaMean<- 7.99
  betaSD<-7.24
  
  interceptM<-0.49
  interceptSD<-0.21
  PEcoeffM<-0.21
  PEcoeffSD<-0.08
  
  participants<- 30
  
  # simulate data first with 3 categories
  alphas<-rnorm(n=participants, mean = alphaMean, sd = alphaSD)
  # we need positive alphas
  while(any (alphas<0)){
    alphas<-rnorm(n=participants, mean = alphaMean, sd = alphaSD)
  }
  betas<-rnorm(n=participants, mean = betaMean, sd = betaSD)
  # also positive betas
  while(any (betas<0)){
    betas<-rnorm(n=participants, mean = betaMean, sd = betaSD)
  }
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=participants, style=3)
  for (n in 1: participants){
    
    intercept<-rnorm(1, mean=interceptM, sd = interceptSD)
    Pecoeff<-rnorm(1, mean=PEcoeffM, sd = PEcoeffSD)
    #print(paste("Simulating participant"), 1)
    sim1<-simulate_RW_obsALL(T, mu, alphas[n], betas[n])
    
    Data<-sim1
    estimateOBS<-lik_RescorlaWagner_obsALL(sim1, alphas[n], betas[n], 3, sceneNum )
    estimateOBS$SubNum<-n
    
    # memory perf
    estimateOBS$recogAcc<-NA
    for (t in 1: nrow(estimateOBS)){
      estimateOBS$recogAcc[t]<-1/(1+exp(-(intercept+Pecoeff*estimateOBS$Delta[t])))
    }
    
    #progress bar
    setTxtProgressBar(pb, n) 
    
    if (exists("DataAll")){
      DataAll<-rbind(DataAll, estimateOBS)
    } else{
      DataAll<-estimateOBS
    }
  }
  
  beep(8)
  
  # plot
  # ggplot(DataAll, aes(x=trialN))+
  #   
  #   geom_line(aes(y= Prob), color = "blue")+
  #   geom_line(aes(y=Qupdate), color = "green")+
  #  geom_line(aes(y= Delta), color = "red")+
  #   #facet_grid(butterfly~SubNum)
  #   facet_wrap(.~scene_cat, ncol = sceneNum)
  # 
  # # plot every probability
  # ggplot(DataAll, aes(x=trialN))+
  #   geom_line(aes(y= P1), color = "blue")+
  #   geom_line(aes(y=P2), color = "yellow")+
  #   geom_line(aes(y=P3), color = "green")+
  #   geom_line(aes(y= Delta), color = "red")+
  #   #facet_grid(butterfly~SubNum)
  #   facet_wrap(.~scene_cat, ncol = sceneNum)
  
  
  # distribution of PE
  # distribution of PE by condition
  PErespdistr<-ggplot(DataAll, aes(x= Delta))
  PErespdistr+geom_histogram(binwidth = 0.01)#+ facet_grid( ~ scene_cat)
  
  # get the median PE
  descriptives<-summary(DataAll$Delta)
  
  # it is 0.07
  DataAll$PEposNeg<-NA
  for (n in 1:nrow(DataAll)){
    if (DataAll$Delta[n]< descriptives[2] ){
      DataAll$PEposNeg[n]<- (1)
    }else if (DataAll$Delta[n]>= descriptives[2] & DataAll$Delta[n]< descriptives[5] ){
      DataAll$PEposNeg[n]<-2
    } else if (DataAll$Delta[n]>= descriptives[5] )
      DataAll$PEposNeg[n]<-3  }
  
  #summarise
  DataAll %>%
    group_by (PEposNeg) %>%
    tally()
  
  
  # relation between mem and PE
  ggplot(DataAll, aes(Delta, recogAcc))+
    geom_smooth(method="lm",formula=y~x, se=F)+aes(colour = factor(SubNum))+
    geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
    theme(strip.text.x = element_text(size = 13))+
    ylim(0,1)
  
  # analise
  model<-lmer(recogAcc~Delta+(1+Delta|SubNum), data = DataAll)
  summary(model)
  
  # calculate effect size
  effectsize<-exp(summary(model)$coefficient[2])
  print(effectsize)
}
#summarise
DataAll %>%
  group_by (PEposNeg) %>%
  summarize(mean = mean(recogAcc, na.rm=T))



# relation between update and PE
ggplot(estimateOBS, aes(Delta, Qupdate))+
  geom_smooth(method="lm",formula=y~x, se=F)+aes(colour = factor(scene_cat))+
  geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))

ggplot(estimateOBS, aes(x=Deltatminus1, y= Pupdate))+
  geom_smooth(method="lm",formula=y~poly(x, 2, raw=F), se=F)+
  aes(colour = factor(scene_cat))+
  geom_smooth(method="lm",formula=y~poly(x, 2, raw=F), colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))

ggplot(estimateOBS, aes(x=Deltatminus1, y= Pupdate))+
  geom_smooth(method="lm",formula=y~x, se=F)+
  aes(colour = factor(scene_cat))+
  #geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))
