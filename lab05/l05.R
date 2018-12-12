Dice = function(m, n) {
  arr = c(1:n)
  for (i in 1:n) {
    sr = sum(sample(c(1:6), m, replace = TRUE) == 3)
    arr[i] = sr
  }
  arr = table(arr)
  barplot(arr, main="Number of 3's rolled of 10 dice", xlab="Number of 3's", ylab="frequency")
  
  return(arr)
}

Dice2 = function(m, n) {
  arr = rbinom(n,m, 1/6)
  arr = table(arr)
  barplot(arr, main="Number of 3's rolled of 10 dice", xlab="Number of 3's", ylab="frequency")
  
  return(arr)
}

Deck = function () {
  barplot(dhyper(c(0:4), 4, 48, 8), names.arg =c(0:4), ylab="Probabilities", main="Probability histogram for drawing aces")
  return(table(dhyper(c(0:4), 4, 48, 8)))
}

Aces = function (m, n) {
  arr = c(1:n)
  for (i in 1:n) {
    s = sample(c(1:52), m)
    sr = sum(s == 1)
    sr = sr + sum(s==2)
    sr = sr + sum(s==3)
    sr = sr + sum(s==4)
    arr[i] = sr
  }
  arr = table(arr)
  barplot(arr, main="Number of aces drawn from 10 cards", xlab="number of Ace's drawn", ylab="frequency")
  
  return(arr)
}

Aces2 = function(m, n) {
  arr = rhyper(c(1:n), 4, 48, m)
  arr = table(arr)
  barplot(arr, main="Number of aces drawn from 10 cards", xlab="number of Ace's drawn", ylab="frequency")
  
  return(arr)
}


