FlipOnce = function() {
  HeadOrTail = sample(c("heads", "trails"), 1)
  return(HeadOrTail)
}

CoinResults = function(n) {
  return(sample(c("heads", "tails"), n, repl=T))
}

ProbHeads = function(n)
{
  coinList = CoinResults(n)
  numHeads = sum(coinList == "heads")
  return(numHeads/n)
}

MaxAndMinHeads = function(n,m)
{
  coinFlipList = c(1:m)
  {
    for (i in 1:m)
      coinFlipList[i] = ProbHeads(n)
  }
  return (c(max(coinFlipList),min(coinFlipList)))
}




RollDie = function(n) {
  dieRolls = sample(c(1,2,3,4,5,6), n, repl=T)
  dieTable = table(dieRolls)
  labB = "Distribution of outcomes of "
  labE = "die rolls"
  labB = paste(c(labB, n), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  return(barplot(dieTable, main = labB))
}

RollSomeDice = function(n, m) {
  dieRollList = c(1:m)
  {
    for (i in 1:m)
    {
      dr = sample(c(1,2,3,4,5,6), n, repl=T)
      dieRollList[i] = sum(dr == 3)
      dr = table(dr)
    }
  }
  labB = "Number of 3's obtained in rolling "
  labE = "dice"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  barplot(dieRollList, main=labB, xlab = "Sample dice", ylab="Number of 3's")
  return (table(dieRollList))
}


DrawCardsWithReplacement = function(n, m) {
  r = "red"
  b = "black"
  redCards = rep(r, 26)
  blackCards = rep(b, 26)
  deck=c(redCards,blackCards)
  
  redCardList = c(1:n)
  {
    for (i in 1:n)
    {
      dr = sample(deck, size=m, repl = T)
      redCardList[i] = sum(dr == "red")
      dr = table(dr)
    }
  }
  
  labB = "Number of red cards draw from "
  labE = "cards with replacement"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  
  return(barplot(table(redCardList), main=labB, xlab="Number of cards drawn", ylab="Number of samples"))
}


DrawCardsWithoutReplacement = function(n, m) {
  r = "red"
  b = "black"
  redCards = rep(r, 26)
  blackCards = rep(b, 26)
  deck=c(redCards,blackCards)
  
  redCardList = c(1:n)
  {
    for (i in 1:n)
    {
      dr = sample(deck, size=m, replace = FALSE)
      redCardList[i] = sum(dr == "red")
      dr = table(dr)
    }
  }
  
  labB = "Number of red cards draw from "
  labE = "cards without replacement"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  
  return(barplot(table(redCardList), main=labB, xlab="Number of cards drawn", ylab="Number of samples"))
}









