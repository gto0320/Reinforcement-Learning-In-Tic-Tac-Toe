# Reinforcement-Learning-In-Tic-Tac-Toe-in-R
Let two AIs compete each other

### Two packages are used in this project, if you have not download package, use this code to download:

I personally got some trouble when downloading "ReinforcementLearning", it might need some extra libraries to be downloaded before this package.

```
install.packages("ReinforcementLearning")
install.packages("tidyverse")
```

Load the libraries
```
library(ReinforcementLearning)
library(tidyverse)
```

To build the model, we need to have data first. Dataset already exist online cannot achieve a very high winning rate.
To increase the winning rate, i choose to create my own dataset


create table

t1=data.frame(EMA = ".",
              SMA = ".",
              State3 = ".",
              State4 = ".",
              State5 = ".",
              State6 = ".",
              State7 = ".",
              State8 = ".",
              state9 = ".",
              StateX=".........",
              ActionX="1",
              NextStateX=".........",
              RewardX=0,
              close=".",
              row=1:4000,
              stringsAsFactors = FALSE)

######################################################################################

########################################################################## attributes used in RL

apple1=apple %>%                                                                 # reducr dimension to 2/3  
  mutate(
    s1=ifelse(EMA>open,(s1="O"),(ifelse(EMA<open,(s1="X"),(s1=".")))),           # reduct dimension to 3 for EMA, compare with open, could possibly be wrong, maybe should compair with SMA
    s2=ifelse(SMA>open,(s2="O"),(ifelse(SMA<open,(s2="X"),(s2=".")))),           # reduct dimension to 3 for SMA, compare with open, same as above
    s9=ifelse(close>open,(s9="O"),(ifelse(close<open,(s9="X"),(s9="."))))        # reduce dimension to 3 for close, compare with open, same as above
  ) 

t1$EMA[4:2268]=apple1$s1
t1$SMA[4:2268]=apple1$s2
t1$close[4:2268]=apple1$s9


############################################ pretrain, let RL now all the choices first

RandomSet=c("buy in","hold","sell")   #generate random dataset used for random number selection

for (i in (1:3)) {
  t1[i,11]=RandomSet[i]
}



# reset some parameters
learningCount=0
data_unseen=0
model=0                          # reset the model 
       

for (i in 4:200) {
  
  
  ######################################################## continue from bottom, copy current progress into column StateX  
  t1[i,10]=paste(t1[i,1],t1[i,2],t1[i,3],
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  
  ######################################################## Start RL learning 
  
 
  control     <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
  model       <- ReinforcementLearning(t1[1:i-1,], s = "StateX", a = "ActionX", r = "RewardX", s_new = "NextStateX",iter = 5, control = control)                              
  data_unseen <- data.frame(State = t1[i,10], stringsAsFactors = FALSE)                 # get the data you want to import to RL
    
    
   
      
      
  options(show.error.messages = FALSE)                                                  # surpress error message. If this specific type not showing in training, it will send an error
  mtry=try(predict(model, data_unseen$State))                                           # try to predict use the model just trained
  options(show.error.messages = TRUE)                                                   # stop surpress error message. So if other error happens then ill know
      
      
      
      
  if(inherits(mtry, "try-error")){                                                      # another loop to make sure if there is error in predicting, it will pick a random number to continue the code.
    action=RandomSet[1]                                                                 # if met a unseen state, just buy it
  }else{action=mtry}                                                                    # if the number generated is correct, use that number keep going

  t1[i,11]=action
  ######################################################## learning ends, we have the predict action now
  
  
  
  
  
  
  ######################################################## if it's a buying action, count for reward
  
  if(t1[i,11]=="buy in"){
    if(t1[i,14]=="O"){t1[i,13]=1}                         # if stock price increase after by in, reward
    if(t1[i,14]=="X"){t1[i,13]=-1}                        # if stock price decrease after by in, penalty
    }
    
  
  ######################################################## if it's a holding action, count for reward. Basically doing nothing
  
  if(t1[i,11]=="hold"){
    t1[i,13]=-0.1                                           # penalty?
    
  }
  
  
  
  ######################################################## if it's a selling action, count for reward
  
  if(t1[i,11]=="sell"){
    if(t1[i,14]=="O"){t1[i,13]=-1}                        # if stock price increase after sell, penalty
    if(t1[i,14]=="X"){t1[i,13]=1}                         # if stock price decrease after sell, reward
  }
  
  
        
  
}


model$Q_hash$O...X....
predict(model, "X........")
model

