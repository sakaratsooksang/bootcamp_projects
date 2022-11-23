## Chatbot
# Order Pizza
# 8-10 dialogs
# Author: Sakarat Sooksang

# Batch_06_Chatbot_Pizza
# data define
price <- 0
total_price <- 0
pizza_list <- c()
side_item_list <- c()
size_list <- c()
pizza_price_list <- c()
side_item_price_list <- c()
size_price_list <- c()

delivery_fee <- 20
#print(upsize_price[["S"]])
pick_up_time <- 1800 # second OR 30 minute
delivery_time <- 3600 # second OR 60 minute



# Question 2 What pizza they want
order_pizza <- function() {
  pizza = c("hawaiian","pepperoni","veggie")
  pizza_price = c(19,20,18)
  names(pizza_price) <- pizza
  size_price = c(0,5,8) 
  names(size_price) <- c("S","M","L")
  print("What kind of pizza you want?")
  print("Today we have ")
  print(paste(pizza," - ",pizza_price,"USD"))
  pizza_selected <- tolower(readLines("stdin",n=1))
  while (!(pizza_selected %in% pizza)) {
    print("Wrong type Please try again")
    print(paste(pizza," - ",pizza_price,"USD"))
    pizza_selected <- readLines("stdin",n=1)
  }
  #price <- price + pizza_price[[pizza_selected]]
  # Question 3 Which size S/M/L
  print(paste(names(size_price)," +",size_price,"USD"))
  print("Which size? (S/M/L)")
  size <- toupper(readLines("stdin",n=1))
  while (!(size %in% c("S","M","L"))) {
    print("Wrong type Please try again")
    print(paste(names(size_price)," +",size_price,"USD"))
    print("Which size? (S/M/L)")
    size <- toupper(readLines("stdin",n=1))
  }
  return(c(pizza_selected,size))
}


# Question 4 What side items they want 
order_side_item <- function() {
  side_item <- c("chicken","garlic bread")
  side_item_price <- c(10,5)
  names(side_item_price) <- side_item
  print("Do you want some side item?")
  print("Today we have ")
  print(paste(side_item," +",side_item_price,"USD"))
  print("*** If you don't want anything please type: none")
  side_item_selected <- readLines("stdin",n=1)
  
  while (!(side_item_selected %in% side_item) & side_item_selected != "none" ) {
    print("Wrong type Please try again")
    print(paste(side_item," +",side_item_price,"USD"))
    side_item_selected <- readLines("stdin",n=1)
  }
  if (side_item_selected != "none"){
      price <- price + side_item_price[[side_item_selected]]
  }
                          
  return(side_item_selected)
}

## main program
price <- 0
total_price <- 0
pizza_list <- c()
side_item_list <- c()
size_list <- c()
pizza_price_list <- c()
side_item_price_list <- c()
size_price_list <- c()

print("Welcome to our Pizza shop!")

# Question 1 Cust_name
print("What is your name?: ")
user_name <- readLines("stdin",n=1)
print(paste("Hi",user_name))

# order pizza
pizza = c("hawaiian","pepperoni","veggie")
pizza_price = c(19,20,18)
names(pizza_price) <- pizza
size_price = c(0,5,8) 
names(size_price) <- c("S","M","L")
while (TRUE){
  dummy <- order_pizza()
  pizza_selected <- dummy[1]
  size <- dummy[2]
  pizza_list <- c(pizza_list,pizza_selected)
  pizza_price_list <- c(pizza_price_list,pizza_price[[pizza_selected]])
  size_list <- c(size_list,size)
  size_price_list <- c(size_price_list,size_price[[size]])
  print("Need anything more? (y/n)")
  ans <- readLines("stdin",n=1)
  if (ans == "n") {
    break
  }
}

# order side item 
side_item <- c("chicken","garlic bread")
side_item_price <- c(10,5)
names(side_item_price) <- side_item
while (TRUE){
  side_item_selected <- order_side_item()
  if (side_item_selected != "none") {
    side_item_list <- c(side_item_list,side_item_selected)
    side_item_price_list <- c(side_item_price_list,side_item_price[[side_item_selected]])
  }
  print("Need anything more? (y/n)")
  ans <- readLines("stdin",n=1)
  if (ans == "n") {
    break
  }
}

# Question 5 Delivery or Pickup
delivery_fee <- 20
pick_up_time <- 1800 # second OR 30 minute
delivery_time <- 3600 # second OR 60 minute

print("Pickup at shop or Delivery?")
print("Delivery fee 20 USD")
print("(P/D)")
delivered_selected <- readLines("stdin",n=1)
total_price <- sum(pizza_price_list)+sum(size_price_list)+sum(side_item_price_list)

while (!(delivered_selected %in% c("P","D"))) {
  print("Wrong input Please type only P (Pick up) or D (Delivery)")
  delivered_selected <- readLines("stdin",n=1)
}
if (delivered_selected == "D") {
  total_price <- total_price + delivery_fee
  print("You choose Delivery! Please give us your address")
  address <- readLines("stdin",n=1)
}
print(paste("Total price",total_price," USD"))  
# Question 6 Payment methods
print("Which payment method you prefer (cash / credit card)")
payment <- tolower(readLines("stdin",n=1))
while (!(payment %in% c("cash","credit card"))) {
  print("Wrong input Please type only cash or credit card")
  payment <- readLines("stdin",n=1)
}
if (payment == "credit card"){
  print("Type you credit card number")
  creditcard <- readLines("stdin",n=1)
}

# Question 7 summary
print("Order done. let me summarize your order!")
print(paste("customer_name:",user_name))
print("Pizza:")
print(paste(pizza_list,"Size",size_list,"Price:",pizza_price_list+size_price_list,"USD")) ##
print("Side item:")
print(paste(side_item_list,side_item_price_list,"USD"))
print(paste("Pickup/Delivery:",delivered_selected))


if (delivered_selected == "D") {
  print(paste("Delivery at address:",address))
}

print(paste("Payment:",payment))

print(paste("Total Price:",total_price," USD"))

if (delivered_selected == "P") {
  print(paste("Your order will finish in :",Sys.time()+pick_up_time))
} else {
  print(paste("Your order will arrive in :",Sys.time()+delivery_time))
}
# Question 8 Please give us star
print("Please give us score 1-5")
service_score <- as.numeric(readLines("stdin",n=1))
print(" Thank you. Have a nice day :) ")
