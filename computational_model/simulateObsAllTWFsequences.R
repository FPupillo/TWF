# simulation function

simulate_RW_obsALLTWFwithsequences<-function(T, mu1, mu2, alpha, beta, object_cat){
  # this function simulates the participants' behaviour at an association task
  # using the rescorla-wagner model
  # 
  # Input
  #   T<-length of the trials
  #   mu1<- probability of the association 1
  #   mu2<- probability of association 2
  #   alpha<- alpha parametere
  #   beta<- beta parameter
  #   object_cat<- a randomly generated trial sequence
  #
  # Output
  #   dastaframe with $response and $object_cat
  # ------------------
  
  # epoch (blocks)
  epochs<-4
  
  # categories: length of the probabilities
  categories<-length(mu1)
  
  # # initialise matrix
  data<-matrix(NA, nrow= T*epochs,ncol=3)
  
  # counter for the matrix
  counter2<-0
  
  # initialise Q
  Q<-rep(0.50, times=length(mu1))
  
  # initialise a, the choice
  a<-NULL
  
  # initialize r
  r<-NULL
  
  # initialise a two element binary vector indicating which category is shown
  x<-matrix(NA, nrow=length(object_cat), ncol=2)
  for (n in 1:length(object_cat)){
    if (object_cat[n]==1){
      x[n,]<-c(1,0)
    } else if ((object_cat[n]==2)){
      x[n,]<-c(0,1)
    }
  }
  
  for(t in 1:length(object_cat)){
    
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
  data<-cbind(rep(c(1,2,3,4), each=T),a, object_cat)

data<-data.frame(data)
names(data)<-c("epoch", "response", "object_cat")

return(data)
}
