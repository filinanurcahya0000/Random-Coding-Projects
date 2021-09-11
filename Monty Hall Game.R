# Function to run through Monty Hall game.
resultslist <- c()
montyhall <- function(x) {
    for(i in x) {
    chosenbyplayer <- sample(1:3,1)
    winningdoor <- sample(1:3,1)
    hostoptions <- sample(1:3)
    chosenbyhost <- setdiff(hostoptions,seq(chosenbyplayer,winningdoor))
    if (winningdoor == chosenbyplayer) {
        resultslist = c(resultslist, "Stay")}
    else {
        resultslist = c(resultslist, "Switch")}
    }
    return(resultslist)
}

# Function to run through Monty Hall game for n times.
n <- c(seq(100,10000,10))

list1 <- data.frame()
for (x in n) {
    table1 <- table(montyhall(1:x))
    proptable <- as.vector(prop.table(table1))
    proptable <- split(proptable,c(1,2))
    positions <- nrow(list1) + 1
    list1[positions,1] <- x 
    list1[positions,2:3] <- proptable }

library(ggplot2)

ggplot() + geom_line(aes(x = list1$V1, y = list1$`1`, color = "Stay")) + geom_line(aes(x = list1$V1, y = list1$`2`, color = "Switch")) + ylab("Probability of Winning") + xlab("Number of Trials") +
    scale_color_manual(name = "Player's Choice", values = c("Switch" = "turquoise1", "Stay" = "palegreen3")) + ggtitle("Monty Hall Game Simulation")

# A single run through of the game.
print(montyhall(1))

# A hundred run throughs of the game. 
hundredtrials <- replicate(100,montyhall(1))
table <- table(hundredtrials)
table <- prop.table(table)
table
