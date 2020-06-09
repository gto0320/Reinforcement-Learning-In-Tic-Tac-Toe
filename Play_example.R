library(ReinforcementLearning)

 
while (TRUE) {
  
  
  t2=data.frame(State1 = ".",
                State2 = ".",
                State3 = ".",
                State4 = ".",
                State5 = "X",
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
                row=1:2,
                stringsAsFactors = FALSE)
  RandomSet=sample(1:9, 9)         #generate random dataset used for random number selection
  randomNumber=5
  
  
  cat(paste(t2[1,1],t2[1,2],t2[1,3]),paste(t2[1,4],t2[1,5],t2[1,6]),paste(t2[1,7],t2[1,8],t2[1,9]),sep ="\n")  
  endRound=TRUE
  
  
while (endRound) {
    
a=readline(prompt="Enter an integer between 1-9: ")

t2[1,as.numeric(a)]="O"
t2[1,10]=paste(t2[1,1],t2[1,2],t2[1,3],
               t2[1,4],t2[1,5],t2[1,6],
               t2[1,7],t2[1,8],t2[1,9],sep="")

################### check if player wins

if(t2[1,1]=="O" && t2[1,2]=="O" && t2[1,3]=="O" ||
   t2[1,4]=="O" && t2[1,5]=="O" && t2[1,6]=="O" ||
   t2[1,7]=="O" && t2[1,8]=="O" && t2[1,9]=="O" ||
   t2[1,1]=="O" && t2[1,4]=="O" && t2[1,7]=="O" ||
   t2[1,2]=="O" && t2[1,5]=="O" && t2[1,8]=="O" ||
   t2[1,3]=="O" && t2[1,6]=="O" && t2[1,9]=="O" ||
   t2[1,1]=="O" && t2[1,5]=="O" && t2[1,9]=="O" ||
   t2[1,3]=="O" && t2[1,5]=="O" && t2[1,7]=="O"){ 
   print(paste("Congratulation! YOU WIN!  ???(????????? ) "))                          # print player  winning message
   break
}


################# predict use trained RL


TestAvailablity=TRUE                                                                    # use loop to make sure model pick a correct number
while (TestAvailablity) {                                                               # loop start
  
  data_unseen <- data.frame(State = t2[1,10], stringsAsFactors = FALSE)                   # get the data you want to import to RL
  options(show.error.messages = FALSE)                                                  # surpress error message. If this specific type not showing in training, it will send an error
  mtry=try(predict(model, t2[1,10]))                                           # try to predict use the model just trained
  options(show.error.messages = TRUE)                                                   # stop surpress error message. So if other error happens then ill know
  
  
  
  if(inherits(mtry, "try-error")){                                                      # another loop to make sure if there is error in predicting, it will pick a random number to continue the code.
    count1=0
    for (count1 in 1:9) {                                                               # check if this slot already be taken by someone
      ifelse(t2[1,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)}
  }else{randomNumber=mtry}                                                            # if the number generated is correct, use that number keep going
  
  
  randomNumber=as.numeric(randomNumber)                                                 # rl will change data type to charater, transfer back to numeric
  
  
  
  if(t2[1,randomNumber]!="."){                                                          # important loop, assign penatly -999 to slot which already be taken, so model will not choose this number anymore.
    model$Q_hash[[t2[1,10]]][[paste(randomNumber)]]=-999
    model$Q[t2[1,10],paste(randomNumber)]=-999                                          # problem is, everytime it starts a new training, this will happen. will take a lot of time to run for every round. no idea how to solve it.
  }else{TestAvailablity=FALSE}                                                          # if everything is good, get out of the loop. Otherwise go back and predict one more time. now this time we have -999 for unavailble slots
  
}

#################

t2[1,as.numeric(randomNumber)]= "X"
t2[1,10]=paste(t2[1,1],t2[1,2],t2[1,3],
               t2[1,4],t2[1,5],t2[1,6],
               t2[1,7],t2[1,8],t2[1,9],sep="")

cat(paste(t2[1,1],t2[1,2],t2[1,3]),paste(t2[1,4],t2[1,5],t2[1,6]),paste(t2[1,7],t2[1,8],t2[1,9]),sep ="\n")     

if(t2[1,1]=="X" && t2[1,2]=="X" && t2[1,3]=="X" ||                      # check if player X wins
   t2[1,4]=="X" && t2[1,5]=="X" && t2[1,6]=="X" ||
   t2[1,7]=="X" && t2[1,8]=="X" && t2[1,9]=="X" ||
   t2[1,1]=="X" && t2[1,4]=="X" && t2[1,7]=="X" ||
   t2[1,2]=="X" && t2[1,5]=="X" && t2[1,8]=="X" ||
   t2[1,3]=="X" && t2[1,6]=="X" && t2[1,9]=="X" ||
   t2[1,1]=="X" && t2[1,5]=="X" && t2[1,9]=="X" ||
   t2[1,3]=="X" && t2[1,5]=="X" && t2[1,7]=="X"){ 
   print(paste("AI: YOU LOSE! ???(????????????)???"))                          # print AI  winning message
   break
}


if(t2[1,1]!="." &&                                                                      # make sure it moves to next row if all 9 blocks has chess piese, which is a draw
   t2[1,2]!="." && 
   t2[1,3]!="." && 
   t2[1,4]!="." && 
   t2[1,5]!="." && 
   t2[1,6]!="." && 
   t2[1,7]!="." && 
   t2[1,8]!="." && 
   t2[1,9]!="."){ 
  
  print(paste("AI: DRAW!  w(????????)w"))                          # print draw message
  break

}

}
}



