rm(list=ls())

library(xlsx)
library(stringr)

#Set Seed
#set.seed(1)
hand <- matrix(c("4","4","4","J","Q","10","5"),nrow=1)

#Remove random cards from possible cards to get opponent's hand
get.hands <- function(players,deck){
  #Data frame of hands for all the other players
  hands <- matrix(NA,ncol=4,nrow=players)
  
  #Loop for each player
  for(i in 1:players){
    player_hand <- matrix(NA,ncol=4,nrow=1)
    k <- 1
    #Loop for choosing each card
    while(k < 3){
      #Random Suit
      suit <- sample(1:4,1)
      #Random Card
      card <- sample(1:13,1)
      #Check card wasn't taken already
      if(is.na(deck[card,suit])){
        k <- k-1
      }
      #Add card to player's hand
      else{
        player_hand[2*k-1] <- suits[suit]
        player_hand[2*k] <- deck[card,suit]
        deck[card,suit] <- NA
      }
      k <- k+1
    }
    hands[i,] <- player_hand
  }
  return(list(hands,deck))
}

#Function for getting five cards(deal)
get.deal <- function(deck){
  deal <- matrix(NA,ncol=10,nrow=1)
  k <- 1
  
  #Loop for choosing each card
  while(k < 6){
    #Random Suit
    suit <- sample(1:4,1)
    #Random Card
    card <- sample(1:13,1)
    #Check card wasn't taken already
    if(is.na(deck[card,suit])){
      k <- k-1
    }
    #Add card to player's hand
    else{
      deal[2*k-1] <- suits[suit]
      deal[2*k] <- deck[card,suit]
      deck[card,suit] <- NA
    }
    k <- k+1
  }
  
  return(list(deal,deck))
}

#Checks if a pair has occurred and gives the pair card
check.pair <- function(hand){
  pair <- F
  value <- "1."
  
  #Creates a table of frequencies for the cards
  table <- table(hand)
  num <- sum(table==2)
  if(num==1){
      pair <- T
      card <- which(deck[,1]==names(table[which(table==2)]))
      value <- paste(value,substr(toString(card/14),3,4),sep="")
  }
  table <- table[-which(table==2)]
  order <- sort(match(names(table),deck[,1]),decreasing = T)[1:3]
  for(i in 1:length(order)){
    value <- paste(value,substr(toString(order[i]/14),3,4),sep="")
  }
  value <- as.numeric(value)
  return(list(pair,value))
}

#Checks if 2 pairs have occurred and gives both pair cards
check.two.pair <- function(deck,hand){
  two.pair <- F
  value <- "2."
  #Creates a table of frequencies for the cards
  table <- table(hand)
  num <- sum(table==2)
  if(num>=2){
    two.pair <- T
    cards <- sort(match(names(table)[which(table==2)],deck[,1]),decreasing=T)
    table <- table[-which(table==2)]
    order <- sort(match(names(table),deck[,1]),decreasing = T)[1] 
    for(i in 1:length(cards)){
      value <- paste(value,substr(toString(cards[i]/14),3,4),sep="")
    }
    value <- paste(value,substr(toString(order/14),3,4),sep="")
  }
  value <- as.numeric(value)
  return(list(two.pair,value))
}

#Checks if a 3 of a kind has occurred and gives the card
check.three <- function(deck,hand){
  three <- F
  value <- "3."
  #Creates a table of frequencies for the cards
  table <- table(hand)
  num <- sum(table==3)
  if(num==1){
    three <- T
    card <- match(names(table)[which(table==3)],deck[,1])
    table <- table[-which(table==3)]
    order <- sort(match(names(table),deck[,1]),decreasing = T)[1:2] 
    value <- paste(value,substr(toString(card/14),3,4),sep="")
    for(i in 1:length(order)){
      value <- paste(value,substr(toString(order[i]/14),3,4),sep="")
    }
  }
  value <- as.numeric(value)
  return(list(three,value))
}

#Checks if a straight has occurred and gives the highest card
check.straight <- function(deck,hand){
  straight <- F
  value <- "4."
  
  #Finds the numeric value for the 7 cards
  order1 <- order2 <- match(hand,deck[,1])
  
  order2[which(order2==13)]=0 #Make aces low
  
  #Sorts the cards high to low
  order1 <- sort(order1,T)
  order2 <- sort(order2,T)
  for(i in 1:(length(hand)-4)){
    val1 <- order1[i]
    val2 <- order2[i]
    if(sum(order1[i:(i+4)])==(5*order1[i]-10)){
      straight <- T
      card <- order1[i]
      value <- paste(value,substr(toString(card/14),3,4),sep="")
      value <- as.numeric(value)
      return(list(straight,value))
    }
    if(sum(order2[i:(i+4)])==(5*order2[i]-10)){
      straight <- T
      card <- order2[i]
      value <- paste(value,substr(toString(card/14),3,4),sep="")
      value <- as.numeric(value)
      return(list(straight,value))
    }
  }
  return(list(straight,value))
}

#Checks if a flush has occurred and also gives the high card
check.flush <- function(deck,hand){
  flush <- F
  value <- "5."
  
  #Check for a flush
  table <- table(hand)
  if(max(table)>=5){
    flush<-T
    hand <- hand[which(hand==names(which(table==max(table))))+1] #Remove suits and cards not with flush suit
    order <- sort(match(hand,deck[,1]),decreasing=T)[1:5]
    for(i in 1:length(order)){
      value <- paste(value,substr(toString(order[i]/14),3,4),sep="")
    }
  }
  value <- as.numeric(value)
  return(list(flush,value))
}

#Checks if a full house has occurred and gives the 3 of a kind card
#and the pair card
check.full.house <- function(deck,hand){
  full.house <- F
  value <- "6."
  #Creates a table of frequencies for the cards
  table <- table(hand)
  num3 <- sum(table==3)
  num2 <- sum(table==2)
  if((num3==1 && num2>=1) || num3==2){
    full.house <- T
    card3 <- sort(match(names(table)[which(table==3)],deck[,1]),decreasing=T)
    card2 <- sort(match(names(table)[which(table==2)],deck[,1]),decreasing=T)
    if(length(card3)>1){
      order <- sort(card3,decreasing = T)
    }else{
      order <- cbind(card3,sort(card2,decreasing = T)[1])
    }
    for(i in 1:length(order)){
      value <- paste(value,substr(toString(order[i]/14),3,4),sep="")
    }
  }
  value <- as.numeric(value)
  return(list(full.house,value))
}

#Checks if a 4 of a kind has occurred and gives the card
check.four <- function(deck,hand){
  four <- F
  value <- "7."
  
  #Creates a table of frequencies for the cards
  table <- table(hand)
  num <- sum(table==4)
  if(num==1){
    four <- T
    card <- which(deck[,1]==names(table[which(table==4)]))
    value <- paste(value,substr(toString(card/14),3,4),sep="")
    table <- table[-which(table==4)]
    order <- sort(match(names(table),deck[,1]),decreasing = T)
    value <- paste(value,substr(toString(order[1]/14),3,4),sep="")
  }
  value <- as.numeric(value)
  return(list(four,value))
}

#Checks if a straight flush has occurred and gives the high card
check.straight.flush <- function(deck,hand){
  sf <- F
  value <- "8."
  
  #Check for a flush
  table <- table(hand)
  if(max(table)>=5){
    hand <- hand[which(hand==names(which(table==max(table))))+1] #Remove suits and cards not with flush suit
    straight <- check.straight(deck,hand)
    if(straight[[1]]){
      value <- straight[[2]]+4
    }
  }
  return(list(sf,value))
}

#Function to determine hand rank
hand.rank <- function(deal,hand){
  rank <- "High Card"
  value <- "0."
  total.hand <- cbind(hand,deal)
  
  #Need to call flush with the suits still there
  flush <- check.flush(deck,total.hand)
  straight.flush <- check.straight.flush(deck,total.hand)
  
  #Removes the suits
  total.hand = total.hand[!(total.hand %in% suits)]
  
  #Create table of hand ranks
  pair <- check.pair(total.hand)
  two.pair <- check.two.pair(deck,total.hand)
  three <- check.three(deck,total.hand)
  straight <- check.straight(deck,total.hand)
  full.house <- check.full.house(deck,total.hand)
  four <- check.four(deck,total.hand)
  
  #Check for each hand rank in order
  #Royal Flush
  if(straight.flush[[1]]==T & straight.flush[[2]]>8.86){
    return(list("Royal Flush",9))
  }
  
  #Straight Flush
  if(straight.flush[[1]]==T){
    return(list("Straight Flush",straight.flush[[2]]))
  }
  
  #Four of a Kind
  if(four[[1]]==T){
    return(list("Four of a Kind",four[[2]]))
  }
  
  #Full House
  if(full.house[[1]]==T){
    return(list("Full House",full.house[[2]]))
  }
  
  #Flush
  if(flush[[1]]==T){
    return(list("Flush",flush[[2]]))
  }
  
  #Straight
  if(straight[[1]]==T){
    return(list("Straight",straight[[2]]))
  }
  
  #Three of a Kind
  if(three[[1]]==T){
    return(list("Three of a Kind",three[[2]]))
  }
  
  #Two Pair
  if(two.pair[[1]]==T){
    return(list("Two Pair",two.pair[[2]]))
  }
  
  #One Pair
  if(pair[[1]]==T){
    return(list("One Pair",pair[[2]]))
  }
  
  #High Card
  #Sort the Cards
  table <- table(total.hand)
  order <- sort(match(names(table),deck[,1]),decreasing = T)[1:5] #Get 5 highest cards in order
  
  for(i in 1:length(order)){
    value <- paste(value,substr(toString(order[i]/14),3,4),sep="")
  }
  value <- as.numeric(value)
  return(list("High Card",value))
}

#Function for simulating percentage of time an inputted hand
#will win in general against a given number of players with randomly
#chosen hands
simulate.random.players <- function(deck,your.hand,players,N,print){
  start.deck <- deck
  
  #Initialize data frame of hand results
  results <- as.data.frame(matrix(0,nrow=1,ncol=4))
  names(results)<- c("Your Wins","Opponent Wins","Ties","Win %")
  
  #Remove your hand from the deck
  for(i in 1:2){
    suit <- which(suits==your.hand[2*i-1])
    card <- which(deck[,i]==your.hand[2*i])
    start.deck[card,suit] <- NA
  }
  
  #Simulation
  for(i in 1:N){
    new.deck <- start.deck
    
    #Deal hole cards to other players
    hole.cards.output <- get.hands(players,new.deck)
    player.hands <- hole.cards.output[[1]]
    new.deck <- hole.cards.output[[2]]
    
    #Deal out five community cards
    community.cards.output <- get.deal(new.deck)
    community.cards <- community.cards.output[[1]]
    new.deck <- community.cards.output[[2]]
    
    #Evaluate each hand
    your.value <- hand.rank(community.cards,your.hand)[[2]]
    
    #Evaluate other player values
    player.values <- rep(0,players)
    for(j in 1:players){
      player.values[j]<- hand.rank(community.cards,matrix(player.hands[j,],nrow=1))[[2]]
    }
    
    if(max(player.values)<your.value){
      results[1,1] <- results[1,1]+1
    }
    if(max(player.values)>your.value){
      results[1,2] <- results[1,2]+1
    }
    if(max(player.values)==your.value){
      results[1,3] <- results[1,3]+1
    }
  }
  
  #Calculate win %
  results[1,4] <- results[1,1]/N
  
  #Prints out the results for a hand
  if(print){
    if(your.hand[1]==your.hand[3]){
      hand.string <- paste(your.hand[2],",",your.hand[4]," suited",sep="")
    }else{
      hand.string <- paste(your.hand[2],",",your.hand[4]," off suit",sep="")
    }
    
    message(sprintf("%s wins %.2f%% of the time against %i players",hand.string,100*results[1,4],players))
  }
  
  return(results)
}

#Function for running N simulations for each possible hand against 
#a given number of players
export.data <- function(create.xlsx,players,N,print){
  start.time <- Sys.time()
  
  #Initialize a data frame of all the cards
  cards <- c("A","K","Q","J","10","9","8","7","6","5","4","3","2")
  percentages <- as.data.frame(matrix(NA,nrow=13,ncol=13))
  names(percentages) <- cards
  rownames(percentages) <- cards
  #Suited Cards
  for(i in 1:12){
    for(j in (i+1):13){
        your.hand <- matrix(c("Hearts",deck[14-i,1],"Hearts",deck[14-j,1]),nrow=1)
        data <- simulate.random.players(deck,your.hand,players,N,print)
        percentages[i,j] <- data[,4]
    }
  }
  #Unsuited Cards
  for(i in 1:13){
    for(j in 1:i){
      your.hand <- matrix(c("Hearts",deck[14-i,1],"Spades",deck[14-j,1]),nrow=1)
      data <- simulate.random.players(deck,your.hand,players,N,print)
      percentages[i,j] <- data[,4]
    }
  }

  if(create.xlsx){
    write.xlsx(percentages,"Poker Percentages.xlsx",sheetName = "Range")
  }
  
  time <- Sys.time()-start.time
  print(time)
  return(percentages)
}

#Run the simulation
N <- 100 #number of simulations

#Number of other players (1 minimum, 23 max)
players <- 1

#Initialize a data frame of all the cards
deck <- as.data.frame(matrix(NA,ncol=4,nrow=13))
suits <- c("Hearts","Diamonds","Clubs","Spades")
names(deck) <- suits
for(i in 1:4){
  deck[,i] <- c("2","3","4","5","6","7","8","9","10","J","Q","K","A")
}

create.xlsx <- F
print <- F

data <- export.data(create.xlsx,players,N,print)






