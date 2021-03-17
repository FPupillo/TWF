# simulation function

simulate_RW_obsALLTWF<-function(T, mu1, mu2, changePoints, alpha, beta){
  # this function simulates the participants' behaviour at an association task
  # using the rescorla-wagner model
  # 
  # Input
  #   T<-length of the trials
  #   mu1<- probability of the association 1
  #   mu2<- probability of association 2
  #   alpha<- alpha parametere
  #   changePoints<- number of change points
  #   beta<- beta parameter
  #   initalQ<- values at which the Qs are initialised
  #
  # Output
  #   dastaframe with $response and $object_cat
  # ------------------

  changePoints<-3
  
  # number of blocks
  epochs<-changePoints+1
  
  # categories: length of the probabilities
  categories<-length(mu1)
  
  # # initialise matrix
  data<-matrix(NA, nrow= T*epochs,ncol=3)
  
  # counter for the matrix
  counter2<-0
  
  for ( s in 1:(epochs)){
    
    # # update counter at every scene
    counter1<-1+counter2
    
    if (s == 1 | s == 3){ # for epoch 1 and 3, strong prior 
    # object category, the category displayed to participants
    object_cat<-sample(seq(1:categories), T, prob=mu1, replace=T)
    } else { # for epoch 2 and 4, flat prior
    object_cat<-sample(seq(1:categories), T, prob=mu2, replace=T)
    }
    
    # initialise Q
    Q<-rep(0.50, times=length(mu1))
    
    # initialise a, the choice
    a<-NULL
    
    # initialize r
    r<-NULL
    
    # initialise a two element binary vector idicating which category is shown
    x<-matrix(NA, nrow=T, ncol=2)
    for (n in 1:length(object_cat)){
      if (object_cat[n]==1){
        x[n,]<-c(1,0)
      } else if ((object_cat[n]==2)){
        x[n,]<-c(0,1)
    }
    }

    for(t in 1:T){
      
      # compute choice probabilities
      cp<-softmax(Q, beta)
      
      # make choice according to choice probabilities
      if (categories>2){
      a[t] <- chooseMultinom(cp)
      } else {a[t] <- chooseBinomial(cp) }
      # the reinforcement here depends on the category shown, regardless of the choice
      
      # update values
      delta <- x[t,]-Q # subtracting x to the objectcategory, the category shown at the end
      
      # update the Q related to the response according to the rw model
      Q <-Q+ alpha *delta
    }

    # update the counter
    counter2<-counter2+T
    data[counter1:counter2,]<-cbind(rep(s, length=T),a, object_cat)
    counter1<-counter1+T
  }
  
  data<-data.frame(data)
  names(data)<-c("epoch", "response", "object_cat")
  
  return(data)
}
