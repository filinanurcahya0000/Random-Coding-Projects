---
title: "M359K Applied Statistics Project Two"
author: "Filina Nurcahya - Tjoa, Dalton Hamilton, Marielena Garcia, and Karen Flores"
date: "9/19/2021"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Applied Statistics (M358K) Project Two "Chuck-A-Luck"

## Question One 

What is the probability that the gambler would win?

75 / 216 probability that one dice lands on the gambler's number.
15 / 216 probability that two dice lands the gambler's number.
1 / 216 probability that three dice lands the gambler’s number. 
The total probability that the gambler would win is 91 / 216 or 42%.

## Question Two 

Who has the long term advantage in the game : the house or the gambler? By how much on the dollar?

The house has the long term advantage in the game in this case. 
The expected winnings by a gambler who bets a dollar is 
((-1 x 125) + (1 x 75) + (2 x 15) + (3 x 1)) / 216 
= $ -0.0787 

## Question Three 

How would you change the amount of the winnings in every round of Chuck - A - Luck to make the game fair? Say that doubles pay A per $1$ bet rather than $2$ and triples pay B per $1$ bet instead of $3$. What is the condition on A and B?

((-1 x 125) + (1 x 75) + (A x 15) + (B x 1)) / 216 
= $0 
If A = $3$ and B = $5$, then 
((-1 x 125) + (1 x 75) + (3 x 15) + (5 x 1)) / 216 
= $0 

Doubles would pay A = $3$ to the dollar and Triples would pay B = $5 to the dollar to have an expected fair game.

## Question Four 

Get three regular dice or use software to create simulated values of the outcomes of rounds of Chuck - A - Luck with the original reward system. Play 100 independent rounds of the game, starting with a new dollar each time. What is the gambler’s winnings in these 100 rounds?

```{r width 80%}
chuckaluck <- function() {
  
    gamblerschoice <- 6
    gamblerswallet <- c(1)
  
    possiblecombinations <- c(1,2,3,4,5,6)
    rolls <- sample(possiblecombinations, 3, replace = T)

    if ((sum(as.numeric(rolls %in% gamblerschoice))) == 0) 
    { gamblerswallet <- c(gamblerswallet, -1)}
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 1) 
    { gamblerswallet <- c(gamblerswallet, 1)}
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 2) 
    { gamblerswallet <- c(gamblerswallet, 2)}
    
    else 
    { gamblerswallet <- c(gamblerswallet, 3)}}

hundredrounds <- replicate(100,chuckaluck())
moneywon100 <- (sum(hundredrounds) - 100) 
moneywon1002 <- (sum(hundredrounds) - 100) / 100

cat("With a Hundred Trials, the gambler is going to win $", moneywon100,", which comes out to $",moneywon1002, "on the dollar")
```

## Question Five 

Take another look at your answers to problems 2.2 and 2.4. How do they relate to the Strong Law of Large Numbers? If you forgot about this theorem you studied in probability, see https://en.wikipedia.org/wiki/Law_of_large_numbers

```{r}
chuckaluck <- function() {
  
    gamblerschoice <- 6
    gamblerswallet <- c(1)
  
    possiblecombinations <- c(1,2,3,4,5,6)
    rolls <- sample(possiblecombinations, 3, replace = T)

    if ((sum(as.numeric(rolls %in% gamblerschoice))) == 0) 
    { gamblerswallet <- c(gamblerswallet, -1)}
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 1) 
    { gamblerswallet <- c(gamblerswallet, 1)}
    
    else if ((sum(as.numeric(rolls %in% gamblerschoice))) == 2) 
    { gamblerswallet <- c(gamblerswallet, 2)}
    
    else 
    { gamblerswallet <- c(gamblerswallet, 3)}}

hundredrounds <- replicate(100,chuckaluck())
moneywon100 <- (sum(hundredrounds) - 100) 
moneywon1002 <- (sum(hundredrounds) - 100) / 100
thousandrounds <- replicate(1000, chuckaluck())
moneywon1000 <- (sum(thousandrounds) - 1000)
moneywon10002 <- (sum(thousandrounds) - 1000) / 1000

cat("With a thousand rounds, it comes out to $",moneywon10002,"on the dollar while with a \n hundred rounds, it comes out to $", moneywon1002, "on the dollar.")
```
As more rounds of this game are played, the total result will converge on the expected value of how much money the gambler gains or loses per dollar bet. In this case, their average loss per dollar they bet will tend to be closer to 7.87 cents with repeated rounds of the game. We can see this by comparing the amount on the dollar that the gambler gains or loses on the dollar when playing a hundred rounds vs a thousand rounds. The loses on the dollar with the thousand rounds are closed to the expected values than with the hundred rounds. 

## Question Six

Now we are going to look at a particular gambler who arrives at the casino with $2$ in his pocket intending to play Chuck - A - Luck. At the beginning of every round of the game, he invests exactly $1$ (if he has the money). He continues to play round after round of the game until he either loses everything or his wealth reaches at least $4$. 

Again, you can use real dice or software to play the game. Keep track of the gambler’s wealth as the rounds of Chuck-a-Luck are repeated until the entire string of rounds is ended by either bankruptcy, or his wealth being at least $4$. Display the gambler’s wealth trajectory graphically.

```{r}
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
lengthofplot <- as.integer(length(vector) - 1)
lengthofplot
ggplot() + geom_point(aes(x = c(0:lengthofplot), y = vector), color = "red", stroke = 1) + labs(x = "Number of Games", y = "Gambler's Money") + geom_line(aes(x = c(0:lengthofplot),y = vector), color = "red") + ggtitle("Gambler's Money throughout Rounds") 

```
## Question Seven

Having completed the above single run, now repeat the same exercise 100 times. More precisely, you can imagine letting 100 gamblers enter the casino with $2 each and play rounds of Chuck-a-Luck as above. This time you do not need to keep track of each gambler’s wealth as the game-play progresses. 

For each gambler you should just record: 

• the number of rounds of the game it took them to either go bankrupt or emerge victorious with at least $4; 

• whether the final result was bankruptcy or not. 

What is the average number of rounds the 100 gamblers played? What is the proportion of gamblers who finished with at least $4 in their pocket? Draw the histogram of the number of rounds played for the players who went bankrupt only, the histogram of the number of rounds played for the remaining players, and the histogram of the number of rounds played for all players.
```{r}
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
    print("Bankruptcy!") 
    winsvsloses = c(winsvsloses,"Bankruptcy!")
    losingtrials = c(losingtrials,length(i))}
  
  else if (i[totaltrials] >= 4) {
    print("$4 in Pocket.")
    winsvsloses = c(winsvsloses,"$4 in Pocket.")
    winningtrials = c(winningtrials,length(i)) }} 

alltrials
hist(alltrials, 20, main = "Distribution of Number of Rounds", xlab = "Number of Rounds", col = "Salmon")

losingtrials
hist(losingtrials, 20, main = "Distribution of Number of Rounds \n of Losing Players", xlab = "Number of Rounds", col = "Salmon")

winningtrials
hist(winningtrials, 20, main = "Distribution of Number of Rounds \n of Winning Players", xlab = "Number of Rounds", col = "Salmon")

mean(alltrials)

winsvslosestable <- table(winsvsloses)   
prop.table(winsvslosestable)    

```



