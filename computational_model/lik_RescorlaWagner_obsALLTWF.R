lik_RescorlaWagner_obsALLTWF<-function (Data,alpha, beta,print,  changePoints){
# This function computes the likelihood of the participants
# choices conditional on the Rescorla Wagner model.  
# in this version, the values are updated depending on which category is shown in the end, regardles of the feedb.
# even when there is no response
#
# Input
#   Data: a long dataset where each row represents a trial. 
#   alpha: a candidate alpha parameter 
#   beta: a candidate beta parameter
#   print: 1: return only the negative log-likelihood; 
#          2: return 1: "Negative LogLikel"; 2:"Q1"; 3:"Q2"; 4:"Q3"
#          5: "Delta1"; 6: "Delta2"; 7: "Delta3", 8: "P1" (Probability
#           of choosing category 1), 9:"P2", 10: "P3"
#   changePoints: how many change
#
# Output:
#   Negative Log Likelihood
# --

initialQ<-matrix(0.5, ncol=length(unique(Data$response)), nrow = length(unique(Data$epoch)))
# assign trialNumber: it is the length of the row of the data file
Data$trialN<-1:nrow(Data)

# Initialize variables: Qs (we may consider using the average probability of each category, but ok for now)
Data$Q1<-NA; Data$Q2<-NA; 
# Ps (probabilities)
Data$P1<-NA; Data$P2<-NA;
# probability strong prior
Data$Prob<-NA
# Delta, prediction error
# Data$Delta<-NA
# Data$Deltatminus1<-NA
# # data update
# Data$Qupdate<-NA
# 
# Data$Pupdate<-NA

# index variables for Q, P, and Delta
Qindex<-initVariab(2, Data)$Qindex
Pindex<-initVariab(2, Data)$Pindex
Deltaindex<-initVariab(2, Data)$Deltaindex


# Counter for indicating which scene has to be updated
count<-rep(0, times = max(Data$epoch))

# initialise choice probability and counter for the choiceprobability
prob<-NA
count2<-1

# assign information about the category displayed to a vector
x<-initVariab(2, Data)$x

# loop over trials
for (t in 1: nrow(Data)){
  
  # The following loop retrieves the Q values of the scene that corresponds to the current scene (time t).
  if (count[Data$epoch[t]]==0){
    Q<-initialQ[Data$epoch[t],]  # if it is the first category of that scene, the Qs are at their initial value
    count[Data$epoch[t]]<-count[Data$epoch[t]]+1 # update the counter
  } else{
    Q<-Data[Data$epoch==Data$epoch[t],][count[Data$epoch[t]],Qindex] # if it is not the first category of a scene, retrieve the Qs of the last trial of that scene
    count[Data$epoch[t]]<-count[Data$epoch[t]]+1
  }
  
  # update choice probabilities using the softmax distribution
  p<-softmax(Q, beta)
  
  # compute Q, delta, and choice probability for actual choice
  if (Data$response[t]!=0){
    
    # probability only for the response made by participant
    prob[count2]<-unlist(p[Data$response[t]])
    
    # assign it to the dataset
    Data$Prob[t]<- prob[count2]
    
    # update the counter 
    count2<-count2+1
  } 
  
  delta <- x[t,]-Q # subtracting 1 to the objectcategory, the category shown at the end
  
  
  # update the Qs related to the response according to the rw model
  Qnew <-Q+ alpha *delta
  
  # prior update response
    Data$QupdateResp[t]<-unlist( Qnew [Data$response[t]]- Q [Data$response[t]])
    
    # prior update obs
    Data$QupdateObs[t]<-unlist( Qnew [Data$object_cat[t]]- Q [Data$object_cat[t]])
    
    
    if (Data$trialN[t]>1){
    Data$PupdateResp[t]<-unlist( p [Data$response[t-1]]- Data[t-1, Pindex][Data$response[t-1]])
    Data$PupdateObs[t]<-unlist( p [Data$object_cat[t-1]]- Data[t-1, Pindex][Data$object_cat[t-1]])
    
    Data$Deltatminus1Resp[t]<-Data$DeltaResp[t-1]
    Data$Deltatminus1Obs[t]<-Data$DeltaObs[t-1]
    
    }
    
  # assign values to the dataset
  Data[t, Qindex]<-Qnew
  Data[t, Pindex]<-p
  Data[t, Deltaindex]<-delta
  Data$DeltaResp[t]<-unlist(delta[Data$response[t]])
  Data$DeltaObs[t]<-unlist(delta[Data$object_cat[t]])
}
# take only the prob for the high prior scenes
#Probstrong<-Data$Prob[Data$scene_cat==4| Data$scene_cat==5 | Data$scene_cat==6]
# compute negative log-likelihood as the negative sum of log prob relative to the choice
#NegLL<--sum(log(Probstrong), na.rm=T)

if (print ==1){
  return(NegLL)
}else if ( print==2){
  return (list("Negative LogLikel"=NegLL, "Q1"= Data$Q1,"Q2"= Data$Q2,
               "Delta1"= Data$Delta1,"Delta2"= Data$Delta2, "P1"=Data$P1,"P2"= Data$P2 ))
} else if(print==3){
  return(Data)}
}

