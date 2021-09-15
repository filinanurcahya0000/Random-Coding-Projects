# M358K Project Two "Chuck - A - Luck"
#Problem 2.4
chuckaluck <- function(x) {
  
    gamblerswallet <- 1
    gamblerschoice <- 2
    possiblecombinations <- c(1,2,3,4,5,6)
    rolls <- sample(possiblecombinations, 3, replace = T)
    print(rolls)
    
    if ((sum(as.numeric(rolls %in% gamblerschoice))) == 1) 
    { gamblerswallet <- gamblerswallet + 1 }
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 2) 
    { gamblerswallet <- gamblerswallet + 2 }
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 3) 
    { gamblerswallet <- gamblerswallet + 3 }
    
    else ((sum(as.numeric(rolls %in% gamblerschoice))) == 0) 
    { gamblerswallet <- gamblerswallet -1 }
    
    print(gamblerswallet)
}

hundredrounds <- replicate(100,chuckaluck())
hundredrounds
sum(hundredrounds)

#Problem 2.6 

chuckaluck <- function() {
  
  gamblerswallet <- 2
  gamblerschoice <- 2
  possiblecombinations <- c(1,2,3,4,5,6)
  walletresults <- c(gamblerswallet)

  for (i in c(1:100)) {
     rolls <- sample(possiblecombinations, 3, replace = T)

  if (gamblerswallet <= 0 ) 
  { print(walletresults)
    return(walletresults)
    stop("You Lose!")  }
    
  else if (gamblerswallet >= 4)
  { print(walletresults)
    return(walletresults)
    stop("You Win!") }
    
  else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 1) 
  { gamblerswallet <- gamblerswallet + 1 
    print(rolls)
    gamblerswallet <- as.numeric(gamblerswallet)
    print(gamblerswallet) 
    walletresults = c(walletresults , gamblerswallet) }
    
  else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 2) 
  { gamblerswallet <- gamblerswallet + 2 
    print(rolls)
    gamblerswallet <- as.numeric(gamblerswallet)
    print(gamblerswallet) 
    walletresults = c(walletresults , gamblerswallet) }
    
  else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 3) 
  { gamblerswallet <- gamblerswallet + 3 
    print(rolls)
    gamblerswallet <- as.numeric(gamblerswallet)
    print(gamblerswallet) 
    walletresults = c(walletresults , gamblerswallet) }
    
  else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 0)
    { gamblerswallet <- gamblerswallet - 1 
      print(rolls)
      gamblerswallet <- as.numeric(gamblerswallet)
      print(gamblerswallet)
      walletresults = c(walletresults , gamblerswallet) }}}

vector <- c(chuckaluck())
library("ggplot2")
lengthofplot <- length(vector)
lengthofplot
ggplot() + geom_point(aes(x = seq(1:lengthofplot), y = vector), color = "red", stroke = 1) + labs(x = "Number of Games", y = "Gambler's Money") + geom_line(aes(x = seq(1:lengthofplot),y = vector), color = "red") + ggtitle("Gambler's Money throughout Rounds")

#Question 2.7 
listofplayers <- replicate(100,chuckaluck())

alltrials <- c()
winningtrials <- c()
losingtrials <- c()
winsvsloses <- c()
iteration <- 0
for (i in listofplayers) {
  
  iteration = iteration + 1
  print(paste0("Number of Player : " , iteration))
  totaltrials <- length(i)
  alltrials = c(alltrials,length(i))
  print(paste0("Number of Tries : ", length(i))) 
  
  if (i[totaltrials] == 0) {
    print("Bankrupcy!") 
    winsvsloses = c(winsvsloses,"Bankrupcy!")
    losingtrials = c(losingtrials,length(i))}
  
  else if (i[totaltrials] >= 4) {
    print("$4 in Pocket.")
    winsvsloses = c(winsvsloses,"$4 in Pocket.")
    winningtrials = c(winningtrials,length(i)) }} 

alltrials
hist(alltrials, 30, main = "Distribution of Number of Rounds", xlab = "Number of Rounds", col = "Salmon")

losingtrials
hist(losingtrials, 30, main = "Distribution of Number of Rounds \n of Losing Players", xlab = "Number of Rounds", col = "Salmon")

winningtrials
hist(winningtrials, 30, main = "Distribution of Number of Rounds \n of Winning Players", xlab = "Number of Rounds", col = "Salmon")

mean(alltrials)

winsvslosestable <- table(winsvsloses)   
prop.table(winsvslosestable)    
    

