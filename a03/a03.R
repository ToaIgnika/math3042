

q1 = function() {
  options(digits = 2)
    return((80/100)*(79/99)*(78/98)*(77/97))
}


Batteries2 = function(n) {
  g = "good"
  b = "bad"
  gBox = rep(g, 80)
  bBox = rep(b, 20)
  box = c(gBox, bBox)
  
  boxList = c(1:n)
  {
    for (i in 1:n)
    {
      dr = sample(box, size=4, repl = F)
      boxList[i] = sum(dr == "good")
    }
  }
  success = sum(boxList == 4)
  
  return(success/n)
}

Batteries3a = function() {
  g = "good"
  b = "bad"
  gBox = rep(g, 80)
  bBox = rep(b, 20)
  box = c(gBox, bBox)
  dr = sample(box, size=4, repl = F)
  res = sum(dr == "good")
  tryCount = 0
  
  while (res != 4) {
    dr = sample(box, size=4, repl = F)
    res = sum(dr == "good")
    tryCount = tryCount+1
  }
  return(tryCount)
}

Batteries3b = function(n) {
  tryTable = c(1:n)
  for (i in 1:n) {
    tryTable[i] = Batteries3a()
  }
  labB = "Number of tries before success out of "
  labE = "tries"
  labB = paste(c(labB, n), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  tryTable = table(tryTable)
  barplot(tryTable, main=labB, xlab="Attepts", ylab="Number of results")
  return(tryTable)
}

GetBox = function (gNum, bNum) {
  g = "good"
  b = "bad"
  gBox = rep(g, gNum)
  bBox = rep(b, bNum)
  box = c(gBox, bBox)
  return (box)
}

Batteries4b = function() {
  g = "good"
  b = "bad"
  gNum = 80
  bNum = 20
  box = GetBox(gNum, bNum)
  dr = sample(box, size=4, repl = F)
  resG = sum(dr == "good")
  resB = sum(dr == "bad")
  tryCount = 0
  
  while (resG != 4) {
    gNum - resG
    bNum - resB
    box = GetBox(gNum, bNum)
    dr = sample(box, size=4, repl = F)
    resG = sum(dr == "good")
    resB = sum(dr == "bad")
    tryCount = tryCount+1
  }
  
  return (tryCount)
}

Batteries4c = function(n) {
  resList = c(1:n)
  for (i in 1:n) {
    resList[i] = Batteries4b()
  }
  resList = table(resList)
  barplot(resList, main="Number of times batteries are replaced", xlab="Replacements")
  return(resList)
}
