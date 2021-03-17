# initialise variables depending on the number of the scenes

initVariab<-function(sceneNum, Data){
  if (sceneNum==4){
    # index variables for Q, P, and Delta
    Qindex<-c("Q1", "Q2", "Q3", "Q4")
    Pindex<-c("P1", "P2", "P3", "P4") 
    Pupdate<-c("Pu1", "Pu2", "Pu3", "Pu4")
    Deltaindex<-c("Delta1", "Delta2", "Delta3", "Delta4") 
    
    # Counter for indicating which scene has to be updated
    count<-rep(0, 4)
    
    # initialise choice probability and counter for the choiceprobability
    prob<-NA
    count2<-1
    
    # assign information about the category displayed to a vector
    x<-matrix(NA, nrow=nrow(Data), ncol=4)
    for (n in 1:length(Data$object_cat)){
      if (Data$object_cat[n]==1){
        x[n,]<-c(1,0,0, 0)
      } else if ((Data$object_cat[n]==2)){
        x[n,]<-c(0,1,0, 0)
      } else if (Data$object_cat[n]==3){
        x[n,]<-c(0,0,1, 0)
      } else if (Data$object_cat[n]==4){
        x[n,]<-c(0,0,0, 1)
      }
    }
    
  } else if (sceneNum==3){

    # index variables for Q, P, and Delta
    Qindex<-c("Q1", "Q2", "Q3")
    Pindex<-c("P1", "P2", "P3") 
    Pupdate<-c("Pu1", "Pu2", "Pu3")
    Deltaindex<-c("Delta1", "Delta2", "Delta3") 
    
    # Counter for indicating which scene has to be updated
    count<-rep(0, 3)
    
    # initialise choice probability and counter for the choiceprobability
    prob<-NA
    count2<-1
    
    # assign information about the category displayed to a vector
    x<-matrix(NA, nrow=nrow(Data), ncol=3)
    for (n in 1:length(Data$object_cat)){
      if (Data$object_cat[n]==1){
        x[n,]<-c(1,0,0)
      } else if ((Data$object_cat[n]==2)){
        x[n,]<-c(0,1,0)
      } else if (Data$object_cat[n]==3){
        x[n,]<-c(0,0,1)}
    }
    
  } else if (sceneNum == 2){
      
      # index variables for Q, P, and Delta
      Qindex<-c("Q1", "Q2")
      Pindex<-c("P1", "P2") 
      Pupdate<-c("Pu1", "Pu2")
      Deltaindex<-c("Delta1", "Delta2") 
      
      # Counter for indicating which scene has to be updated
      count<-rep(0, 2)
      
      # initialise choice probability and counter for the choiceprobability
      prob<-NA
      count2<-1
      
      # assign information about the category displayed to a vector
      x<-matrix(NA, nrow=nrow(Data), ncol=2)
      for (n in 1:length(Data$object_cat)){
        if (Data$object_cat[n]==1){
          x[n,]<-c(1,0)
        } else if ((Data$object_cat[n]==2)){
          x[n,]<-c(0,1)
        }
      }
  }
      return(list("Qindex" = Qindex, "Pindex" = Pindex, 
                  "Pupdate" = Pupdate, "Deltaindex" = Deltaindex, "x"= x)) 
  
}