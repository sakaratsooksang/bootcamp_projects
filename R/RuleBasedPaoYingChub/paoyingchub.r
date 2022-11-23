# HW 2
# author : sakarat sooksang

# function define

pao_ying_chub <- function(user_action){
  # define initial value 
  bot <- c("rock" ,"paper" ,"scissors")
  tie <- 0
  win <- 0
  lose <- 0
  if (!(user_action %in% c("rock" ,"paper" ,"scissors"))){
    print("Wrong Input")
    return('Please input "rock" | "paper" | "scissors" in right format')
  } 
  bot_action <- sample(x = bot,size=1)
  # tie rules
  if (user_action == bot_action) {
    print(paste("Your action :",user_action,"Bot action :",bot_action))
    print("Tie")
    tie <- tie + 1 
  } else {
    if (user_action == "paper") {
      if (bot_action == "scissors") {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  lose")
        lose <- lose + 1
      } else {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  win")
        win <- win + 1
      }
    } else if (user_action == "rock") {
      if (bot_action == "paper") {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  lose")
        lose <- lose + 1
      } else {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  win")
        win <- win + 1
      }
    } else {
      if (bot_action == "rock") {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  lose")
        lose <- lose + 1
      } else {
        print(paste("Your action :",user_action,"Bot action :",bot_action))
        print("You  win")
        win <- win + 1
      }
    }
  } 
  return(c(win,lose,tie))
}
result <- c(0,0,0)
while (TRUE){
  print("type: rock / paper / scissors ")
  user_action <- readLines("stdin",n=1)
  res <- pao_ying_chub(user_action)
  result <- result + res
  print("Wanna play more? (y/n)")
  ans <- readLines("stdin",n=1)
  if (ans == "n") {
    print(paste(
      "win: ",result[1],
      "lose: ", result[2],
      "tie: ", result[3]
      )
    )
    break
  }
}
