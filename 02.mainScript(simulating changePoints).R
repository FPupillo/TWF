# script to simulate PE and change in priors
#for (i in 1){
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
source("computational_model/simulateObsAllTWF.R")
source("helper_functions/InitVariab.R")
source("computational_model/lik_RescorlaWagner_obsALLTWF.R")

# probability first block
mu1 <-c(0.90,0.10)
mu2<-c(0.60,0.40)

# how many change points?
changePoints<-3

# how many trials per block
T<-40

# for the alpha, we could use average values
alphaMean<-0.43
alphaSD<-0.23
betaMean<- 7.99
betaSD<-7.24

# we could also take intercept and slopes for relationship between PE (resp) and memory
interceptM<-0.49
interceptSD<-0.21
PEcoeffM<-0.21
PEcoeffSD<-0.08
  
participants<- 30

# simulate data - alphas
alphas<-rnorm(n=participants, mean = alphaMean, sd = alphaSD)
# we need positive alphas
while(any (alphas<0)){
  alphas<-rnorm(n=participants, mean = alphaMean, sd = alphaSD)
}

# beta
betas<-rnorm(n=participants, mean = betaMean, sd = betaSD)
# also positive betas
while(any (betas<0)){
  betas<-rnorm(n=participants, mean = betaMean, sd = betaSD)
}

# create progress bar
pb<-txtProgressBar(min=0, max=participants, style=3)

# set seed for reproducibility of simulations
set.seed(1234)
for (n in 1: participants){

# get intercepts and 
intercept<-rnorm(1, mean=interceptM, sd = interceptSD)
Pecoeff<-rnorm(1, mean=PEcoeffM, sd = PEcoeffSD)

#print(paste("Simulating participant"), 1)
sim1<-simulate_RW_obsALLTWF(T, mu1, mu2, changePoints , alphas[n], betas[n])

Data<-sim1
estimateOBS<-lik_RescorlaWagner_obsALLTWF(sim1, alphas[n], betas[n], 3, changePoints )
estimateOBS$SubNum<-n

# memory perf
estimateOBS$recogAcc<-NA
for (t in 1: nrow(estimateOBS)){
  estimateOBS$recogAcc[t]<-1/(1+exp(-(intercept+Pecoeff*estimateOBS$DeltaResp[t])))
}

#progress bar
setTxtProgressBar(pb, n) 
estimateOBS$SubNum<-rep(n, times=nrow(estimateOBS))
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
PErespdistr<-ggplot(DataAll, aes(x= DeltaResp))
PErespdistr+geom_histogram(binwidth = 0.1)+ facet_grid( ~ epoch)

# get the median PE
descriptives<-summary(DataAll$DeltaResp)

# it is 0.07
DataAll$PEposNeg<-NA
for (n in 1:nrow(DataAll)){
  if (DataAll$DeltaResp[n]< descriptives[2] ){
    DataAll$PEposNeg[n]<- (1)
  }else if (DataAll$DeltaResp[n]>= descriptives[2] & DataAll$DeltaResp[n]< descriptives[5] ){
    DataAll$PEposNeg[n]<-2
  } else if (DataAll$DeltaResp[n]>= descriptives[5] )
    DataAll$PEposNeg[n]<-3  }

#summarise
DataAll %>%
  group_by (PEposNeg) %>%
 tally()


# relation between mem and PE
ggplot(DataAll, aes(DeltaResp, recogAcc))+
  geom_smooth(method="lm",formula=y~x, se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  ylim(0,1)

# analise
model<-lmer(recogAcc~DeltaResp+(1+DeltaResp|SubNum), data = DataAll)
summary(model)

# calculate effect size
effectsize<-exp(summary(model)$coefficient[2])
print(effectsize)

#summarise
DataAll %>%
  group_by (PEposNeg) %>%
  summarize(mean = mean(recogAcc, na.rm=T))

objects<-DataAll$object_cat[DataAll$SubNum==1]

ggplot(DataAll, aes(x=Deltatminus1Resp, y= PupdateResp))+
  geom_smooth(method="lm",formula=y~poly(x, 2, raw=F), se=F)+
aes(colour = factor(epoch))+
  geom_smooth(method="lm",formula=y~poly(x, 2, raw=F), colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))

# print the probability
ggplot(DataAll[DataAll$SubNum==1,], aes(x=trialN))+
stat_summary(fun.y="mean",geom="line")+
geom_line(aes(y=P1), size = 1.5, color = "blue")+
geom_line(aes(y=P2),size = 1.5, color = "darkgreen")


# prediction error res
PErespdistr<-ggplot(DataAll, aes(x= DeltaResp))
PErespdistr+geom_histogram(binwidth = 0.1)+
   facet_grid( ~ epoch)

ggplot(DataAll[DataAll$SubNum==1,], aes(x=trialN))+
  #stat_summary(fun.y="mean",geom="line")+
  geom_line(aes(y=DeltaResp), size = 1.5, color = "red")+
  geom_vline(xintercept = c(40, 80,120))
  
########################################################################
# prediction error observation
Eobspdistr<-ggplot(DataAll, aes(x= DeltaObs))
Eobspdistr+geom_histogram(binwidth = 0.1)+ facet_grid( ~ epoch)


  ggplot(DataAll[DataAll$SubNum==1,], aes(x=trialN))+
  #stat_summary(fun.y="mean",geom="line")+
  geom_line(aes(y=DeltaObs), size = 1.5, color = "red")+
  geom_vline(xintercept = c(40, 80,120))+
  scale_x_continuous(labels=(objects))
  


# get the median PE
descriptives<-summary(DataAll$DeltaObs)

# it is 0.07
DataAll$PEobsNeg<-NA
for (n in 1:nrow(DataAll)){
  if (DataAll$DeltaObs[n]< descriptives[2] ){
    DataAll$PEobsNeg[n]<- (1)
  }else if (DataAll$DeltaObs[n]>= descriptives[2] & DataAll$DeltaObs[n]< descriptives[5] ){
    DataAll$PEobsNeg[n]<-2
  } else if (DataAll$DeltaObs[n]>= 0.80)
    DataAll$PEobsNeg[n]<-3  }

#summarise
DataAll %>%
  group_by (PEobsNeg) %>%
  tally()


# relation between mem and PE
ggplot(DataAll, aes(DeltaObs, recogAcc))+
  geom_smooth(method="lm",formula=y~x, se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  ylim(0,1)

# quadratic?
ggplot(DataAll, aes(DeltaObs, recogAcc))+
  geom_smooth(method="lm",formula=y~poly(x,2, raw=F), se=F)+aes(colour = factor(SubNum))+
  geom_smooth(method="lm",formula=y~poly(x,2, raw=F), colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))+
  ylim(0,1)


# analise
model<-lmer(recogAcc~poly(DeltaObs, 2)+(1+poly(DeltaObs)|SubNum), data = DataAll)
summary(model)

# calculate effect size
effectsize<-exp(summary(model)$coefficient[2])
print(effectsize)

#summarise
# DataAll %>%
#   group_by (PEposNeg) %>%
#   summarize(mean = mean(recogAcc, na.rm=T))

ggplot(DataAll, aes(x=Deltatminus1Obs, y= PupdateObs))+
  geom_smooth(method="lm",formula=y~x, se=F)+
  aes(colour = factor(epoch))+
  #geom_smooth(method="lm",formula=y~x, colour="black", se=T)+
  theme(strip.text.x = element_text(size = 13))



