# This function calculates the mode of the given vector
getmode <- function(vec) {
  temp <- unique(vec)
  temp[which.max(tabulate(match(vec, temp)))]
}

# This function increments the given element of a vector by the given argument a
increment <- function(x, a)
{
  eval.parent(substitute(x <- x + a))
}

faceValues <- c(5,1,3,7,9)

df <- data.frame(NumberOfDecs= numeric(0), FaceValues= numeric(0), n1= numeric(0),n2= numeric(0),n3= numeric(0),n4= numeric(0),n5= numeric(0),n6= numeric(0),n7= numeric(0),n8= numeric(0),n9= numeric(0),n10= numeric(0)) 

# This for loop is to set the number of decks that will be used
for(decSet in 1:2){
  # This for loop is to set the face value
  for(faceSet in faceValues){
    dec1 <- vector()
    # This for loop creates a deck of 52 cards with the selected face value 
    for(card in 1:13) {
      for(i in 1:4){
        if(card > 10)
          dec1 <- c(dec1, faceSet)
        else
          dec1 <- c(dec1, card)
      }
    }
    # Here we combine 2 decks together to get a deck of 104 cards
    dec2 <- c(dec1, dec1)
    n <- rep(0,10)
    totalCard <- 52
    # We shuffle our deck for a 10000 times
    for(trial in 1:10000){
      # This statement selects the deck that will be used according to the current setting
      if(decSet == 1)
        sufDec1 <- sample(dec1)
      else{
        sufDec1 <- sample(dec2)
        totalCard <- 104
      }
      end_indexes <- vector()
      # for each shuffle we try all starting indexes and record how many of them end on the same card
      for(index in 1:10){
        while(index <= totalCard){
          last <- index
          index <- index + sufDec1[index]
        }
        end_indexes <- c(end_indexes, last)
      }
      tempTable <- table(end_indexes)
      # Here we increment the proportion index based on the result we obtain after trying all starting indexes
      increment(n[tempTable[names(tempTable)==getmode(end_indexes)]], 0.0001)
    }
    # We keep the results in a dataframe object to clearly visualize them
    df[nrow(df)+1, ] <- c(decSet, faceSet, n[1], n[2], n[3], n[4], n[5], n[6], n[7], n[8], n[9], n[10])
  }
}

# Summary: As we increase the face value of cards, then the probability of cards ending on same index decreases
# and as we increase the total number of cards, that probability increases. This is because, the average step size is inversely
# correlated with the probability of ending on a certain card and the average number of steps to get there is positively
# correlated with that probability. Therefore, when we increase the face cards values, we increase the average step size and this
# decreses the probability of ending on a certain card. And when we increase the number of cards we use, we increase the number
# of steps it takes to finish and this increases the probability of ending on a certain card.

