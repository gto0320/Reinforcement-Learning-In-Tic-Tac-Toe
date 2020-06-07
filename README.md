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



############################################ create table

t1=data.frame(State1 = ".",
              State2 = ".",
              State3 = ".",
              State4 = ".",
              State5 = ".",
              State6 = ".",
              State7 = ".",
              State8 = ".",
              State9 = ".",
              StateX=".........",
              ActionX="1",
              NextStateX=".........",
              RewardX=0,
              StateY=".........",
              ActionY="1",
              NextStateY=".........",
              RewardY=0,    
              row=1:8000,
              stringsAsFactors = FALSE)
############################################ pretrain, let RL now all the choices first
for (i in (1:9)) {
  
  t1[i,10:12]=as.numeric(i)
  t1[i,14:16]=as.numeric(i) 
}
modelX<- ReinforcementLearning(t1[1:9,], s = "StateX", a = "ActionX", r = "RewardX",  s_new = "NextStateX", iter = 5, control = control)
modelY<- ReinforcementLearning(t1[1:9,], s = "StateY", a = "ActionY", r = "RewardY",  s_new = "NextStateY", iter = 5, control = control) 


# reset some parameters
learningCount=0
data_unseen=0
model=0                          # reset the model for player 1
modelY=0                         # reset the model for player 2
accuracy=0
roundcount=0                     # reset the round counter 
endRound=TRUE                    # reset the endRound count
RandomSet=sample(1:9, 9)         #generate random dataset used for random number selection
randomNumber=5 ############################ first
#if(i=100){start_time3 <- Sys.time()}    #timer example
#↕



start_time1 <- Sys.time()

for (i in 2490:8000) {
  
  if(i==1000){start_time2 <- Sys.time()}
  if(i==2000){start_time3 <- Sys.time()}
  if(i==3000){start_time4 <- Sys.time()}
  if(i==4000){start_time5 <- Sys.time()}
  if(i==5000){start_time6 <- Sys.time()}
  if(i==6000){start_time7 <- Sys.time()}
  if(i==7000){start_time8 <- Sys.time()}
  if(i==8000){start_time9 <- Sys.time()}
  
  
  
  ######################################################## continue from bottom, copy current progress into column StateX  
  t1[i,10]=paste(t1[i,1],t1[i,2],t1[i,3],
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  
  ######################################################## Start RL learning 
  
  if(i>10){                                                                                 # RL loop, put it start to do training only after 2nd row to avoid getting N/A data
    
    if(endRound==TRUE){
      control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
      model<- ReinforcementLearning(t1[1:i-1,], s = "StateX", a = "ActionX", r = "RewardX", s_new = "NextStateX",iter = 5, control = control)                              
      
      
      modelY<- ReinforcementLearning(t1[1:i-1,], s = "StateY", a = "ActionY", r = "RewardY",  s_new = "NextStateY", iter = 5, control = control)         # column names are for Y now, and modelY instead of model
      
    }
    
    
    TestAvailablity=TRUE                                                                    # use loop to make sure model pick a correct number
    while (TestAvailablity) {                                                               # loop start
      
      data_unseen <- data.frame(State = t1[i,10], stringsAsFactors = FALSE)                   # get the data you want to import to RL
      options(show.error.messages = FALSE)                                                  # surpress error message. If this specific type not showing in training, it will send an error
      mtry=try(predict(model, data_unseen$State))                                           # try to predict use the model just trained
      options(show.error.messages = TRUE)                                                   # stop surpress error message. So if other error happens then ill know
      
      
      
      if(inherits(mtry, "try-error")){                                                      # another loop to make sure if there is error in predicting, it will pick a random number to continue the code.
        count1=0
        for (count1 in 1:9) {                                                               # check if this slot already be taken by someone
          ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)}
      }else{randomNumber=mtry}                                                            # if the number generated is correct, use that number keep going
      
      
      randomNumber=as.numeric(randomNumber)                                                 # rl will change data type to charater, transfer back to numeric
      
      
      
      if(t1[i,randomNumber]!="."){                                                          # important loop, assign penatly -999 to slot which already be taken, so model will not choose this number anymore.
        model$Q_hash[[t1[i,10]]][[paste(randomNumber)]]=-999
        model$Q[t1[i,10],paste(randomNumber)]=-999                                          # problem is, everytime it starts a new training, this will happen. will take a lot of time to run for every round. no idea how to solve it.
      }else{TestAvailablity=FALSE}                                                          # if everything is good, get out of the loop. Otherwise go back and predict one more time. now this time we have -999 for unavailble slots
      
    }
    
    
    
  }else{                                                                                    # use 1-9 to let RL know there are 10 possible choices
    count1=0
    for (count1 in 1:9) {                                                                   # check if this slot already be taken by someone
      ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
    }
  }
  
  ######################################################## learning ends 
  
  
  
  
  t1[i,as.numeric(randomNumber)]="X"                                      # assign X/O into slot 1 to 9, start playing
  t1[i,11]= as.character(randomNumber)                                    # also record the move in column "Action"
  ##################################fill nextstate for player Y
  t1[i,12]=paste(t1[i,1],t1[i,2],t1[i,3],                                 # this is the statement after first player place his chess. player 2 need this state record to start RL, so put it here first, it will change later in this code
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  
  
  if(!endRound){t1[i-1,16]=t1[i,12]}                                      # if this is a new round, copy this statement to the end of the last row. player 2 need this. Go check table then you will now why i do this
  endRound=FALSE                                                          # reset endRound
  
  
  if(t1[i,1]=="X" && t1[i,2]=="X" && t1[i,3]=="X" ||                      # check if player X wins
     t1[i,4]=="X" && t1[i,5]=="X" && t1[i,6]=="X" ||
     t1[i,7]=="X" && t1[i,8]=="X" && t1[i,9]=="X" ||
     t1[i,1]=="X" && t1[i,4]=="X" && t1[i,7]=="X" ||
     t1[i,2]=="X" && t1[i,5]=="X" && t1[i,8]=="X" ||
     t1[i,3]=="X" && t1[i,6]=="X" && t1[i,9]=="X" ||
     t1[i,1]=="X" && t1[i,5]=="X" && t1[i,9]=="X" ||
     t1[i,3]=="X" && t1[i,5]=="X" && t1[i,7]=="X"){ t1[i,13]=3                                                    # reward for player 1 if he wins
     t1[i-1,17]=-10                                                # penalty for player 2
     endRound=TRUE                                                 # record this round ends
     for (penalty in (1:roundcount)) {t1[(i-penalty),17]=-10}      # apply penalty for other rows in this game
     roundcount=0                                                  # reset count how many rows in this round
     print(paste("player 1 win",",","row #",i,t1[i,10]))                    # print a winning message
     next(i)                                                       # directly go to next row, start a new game
  }
  
  
  
  
  
  if(t1[i,1]!="." &&                                                                      # make sure it moves to next row if all 9 blocks has chess piese, which is a draw
     t1[i,2]!="." && 
     t1[i,3]!="." && 
     t1[i,4]!="." && 
     t1[i,5]!="." && 
     t1[i,6]!="." && 
     t1[i,7]!="." && 
     t1[i,8]!="." && 
     t1[i,9]!="."){ t1[i,13]=-10                                                          # penalty for player 1 if draw. Yes, i blame him for not winning!
     endRound=TRUE                                                         # record this round ends
     for (penalty in (1:(roundcount-1))) {t1[(i-penalty),13]=-8}          # apply penalty for other rows in this game
     for (penalty in (1:roundcount)) {t1[(i-penalty),17]=-10}      # apply penalty for other rows in this game
     roundcount=0                                                          # reset count how many rows in this round
     print(paste("DRAW",i,t1[i,10]))                    # print a winning message
     next(i)                                                               # directly go to next row, start a new game
  } 
  ######################################################################################### player Y's move
  
  ######################################################## Start RL learning for player Y, ill point out the differences
  
  if(i>10){
    
    
    #control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
    #modelY<- ReinforcementLearning(t1[1:i-1,], s = "StateY", a = "ActionY", r = "RewardY",  s_new = "NextStateY", iter = 5, control = control)         # column names are for Y now, and modelY instead of model
    #data_unseenY <- data.frame(State = t1[i,12], stringsAsFactors = FALSE)                                                                             # add a Y in data_unseenY, and the cell it calls needs to be change too!
    
    
    
    TestAvailablity=TRUE
    while (TestAvailablity) {
      
      
      data_unseenY <- data.frame(State = t1[i,12], stringsAsFactors = FALSE)
      options(show.error.messages = FALSE)
      mtry=try(predict(modelY, data_unseenY$State))                                   # use modelY to predict, so de data_unseenY
      options(show.error.messages = TRUE)
      
      
      
      if(inherits(mtry, "try-error")){
        count1=0
        for (count1 in 1:9) {
          ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
        }
      }else{randomNumber=mtry}
      
      
      
      randomNumber=as.numeric(randomNumber)                                           # rl will change data type of randomNumber to charater, transfer back to numeric
      
      
      
      if(t1[i,randomNumber]!="."){
        modelY$Q_hash[[t1[i,12]]][[paste(randomNumber)]]=-999                         # modelY
        modelY$Q[t1[i,12],paste(randomNumber)]=-999                                   # modelY
      }else{TestAvailablity=FALSE}
      
      
    }
    
    
    
  }else{
    count1=0
    for (count1 in 1:9) {                                                               # check if this slot already be taken by someone
      ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
    }
  }
  ######################################################## learning ends, if you want to replace RL with automatic generate code, disable this part. 
  
  
  
  ######################################################## if you want to replace RL for player 2 with automatic generator, here are the codes
  # randomNumber=sample(1:9, 1)
  #
  # count1=0
  # for (count1 in 1:9) {
  #   ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
  # }
  ######################################################## remember to disable this if you want RL.
  
  
  
  ################################################## fill cloumn StateY,NextState,Action 
  
  t1[i,14]=paste(t1[i,1],t1[i,2],t1[i,3],
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  t1[i,randomNumber]="O"
  t1[i,15]=as.character(randomNumber)
  
  t1[i,12]=paste(t1[i,1],t1[i,2],t1[i,3],
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  t1[i,16]=t1[i,12]
  
  ############################################################## check winning conditions, and fill reward column if wins
  if(t1[i,1]=="O" && t1[i,2]=="O" && t1[i,3]=="O" ||
     t1[i,4]=="O" && t1[i,5]=="O" && t1[i,6]=="O" ||
     t1[i,7]=="O" && t1[i,8]=="O" && t1[i,9]=="O" ||
     t1[i,1]=="O" && t1[i,4]=="O" && t1[i,7]=="O" ||
     t1[i,2]=="O" && t1[i,5]=="O" && t1[i,8]=="O" ||
     t1[i,3]=="O" && t1[i,6]=="O" && t1[i,9]=="O" ||
     t1[i,1]=="O" && t1[i,5]=="O" && t1[i,9]=="O" ||
     t1[i,3]=="O" && t1[i,5]=="O" && t1[i,7]=="O"){ t1[i,13]=-10                                                        # penalty for player x
     t1[i,17]=3                                                          # reward for player y
     endRound=TRUE                                                       # end of this round
     for (penalty in (1:(roundcount-1))) {t1[(i-penalty),13]=-10}        # applay penalty to last few steps as well. for player x
     roundcount=0                                                        # reset row count for the loop
     print(paste("player 2 win",",","row #",i))                          # print player 2 winning message
     next(i)                                                             # go to next round
  }
  
  ############################################################### winning check end
  
  
  ############################## if no one win in this round
  roundcount=roundcount+1                                               # start count how many rows in this round, if not winning
  t1[i+1,1:9]=t1[i,1:9]                                                 #copy current progress to next row
  
}
start_timeEnd <- Sys.time()


start_time2-start_time1
start_time3-start_time1
start_time4-start_time1
start_time5-start_time1
start_time6-start_time1



library(dplyr)

for (rate in (1:7)) {
  
  
  a=count(t1[(rate*1000-999):(rate*1000),],RewardX)
  
  
  
  b=count(t1[(rate*1000-999):(rate*1000),],RewardY)
  
  
  print(paste(a,b))
  
}

######


control <- list(alpha =0.5, gamma =0.5, epsilon =0.5)
model<- ReinforcementLearning(t1[1:7300,], s = "StateX", a = "ActionX", r = "RewardX",  s_new = "NextStateX", iter = 5, control = control)
model$Q_hash$O...X....	

model<- ReinforcementLearning(t1[1:5472,], s = "StateX", a = "ActionX", r = "RewardX",  s_new = "NextStateX", iter = 5, control = control)
model$Q_hash$OOX.X.O.X



######
#save(t1,file="t4.Rda")

control <- list(alpha =0.4, gamma =0.5, epsilon =0.5)
modelY<- ReinforcementLearning(t1[1:5472,], s = "StateY", a = "ActionY", r = "RewardY",  s_new = "NextStateY", iter = 5, control = control)
modelY$Q_hash$....X..OX

..O
OXX
.X.




#######


```



