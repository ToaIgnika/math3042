confidence25 = function() {
    racetimes25=TenMileRace$net[sample(1:8636, 25)]
    t.test(racetimes25)
}

RepairConstantComputers = function(m) {
  brenda = 0
  carl = 0
  david = 0
  erica = 0
  for (i in 1:m) {
    x = sample(1:12, size = 1);
    if (x == 1) {
      david = david + 1
    } else if (x > 1 && x < 4) {
      carl = carl + 1
    } else if (x > 3 && x < 8) {
      erica = erica + 1
    } else {
      brenda = brenda + 1
    }
  }
  s = max(brenda * 0.2, carl * 0.5,  david * 1.0,  erica * 0.25)
  return (s)
}
# brenda : 0.2dpc, 5/12 
# carl   : 0.5dpc, 2/12
# david  : 1.0dpc,1/12
# erika  : 0.25dpc, 4/12

SimulateConstantRepairs = function(n,m) {
  arr = c(1:n)
  for (i in 1:n) {
    arr[i] = RepairConstantComputers(m)
  }
  labB = "Total repair times for"
  labE = "computers at Constant Computers"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  
  labM = "mean="
  labS = "sd="
  labM = paste(c(labM, format(round(mean(arr), 4), nsmall = 4)),collapse = " ")
  labM = paste(c(labM, labS), collapse = "\n")
  labM = paste(c(labM, format(round(sd(arr), 4), nsmall = 4)),collapse = " ")
  hist(arr, main=labB, xlab = labM)
  t.test(arr)
  return( t.test(arr))
}

RepairExponentialComputers = function(m) {
  kambiz = 0
  goran = 0
  harpeet = 0
  jim = 0
  for (i in 1:m) {
    x = sample(1:12, size = 1);
    if (x == 1) {
      harpeet = harpeet + 1
    } else if (x > 1 && x < 4) {
      goran = goran + 1
    } else if (x > 3 && x < 8) {
      jim = jim + 1
    } else {
      kambiz = kambiz + 1
    }
  }
  s = max(sum(rexp(kambiz, 5)), sum(rexp(goran, 2)), sum(rexp(harpeet, 1)), sum(rexp(jim, 4)))
  return (s)
}

# Kambiz   m=0.2dpc 5/12
# Goran    m=0.5dpc 2/12
# Hapreet  m=1.0dpc 1/12
# Jim      m=0.25dpc 4/12


SimulateExponentialRepairs = function(n,m) {
  arr = c(1:n)
  for (i in 1:n) {
    arr[i] = RepairExponentialComputers(m)
  }
  
  labB = "Total repair times for"
  labE = "computers at Exponential Computers"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  
  labM = "mean="
  labS = "sd="
  labM = paste(c(labM, format(round(mean(arr), 4), nsmall = 4)),collapse = " ")
  labM = paste(c(labM, labS), collapse = "\n")
  labM = paste(c(labM, format(round(sd(arr), 4), nsmall = 4)),collapse = " ")
  hist(arr, main=labB, xlab = labM)
  return( t.test(arr))
}

# Laura  m=0.2dpc 5/12
# Maryam m=0.5dpc 2/12
# Paul   m=1.0dpc 1/12
# Sandi  m=0.25dpc 4/12

RepairNormalComputers = function(m) {
  laura = 0
  maryam = 0
  paul = 0
  sandi = 0
  for (i in 1:m) {
    x = sample(1:12, size = 1);
    if (x == 1) {
      paul = paul + 1
    } else if (x > 1 && x < 4) {
      maryam = maryam + 1
    } else if (x > 3 && x < 8) {
      sandi = sandi + 1
    } else {
      laura = laura + 1
    }
  }
  rlaura = rnorm(laura, mean=0.2, sd=0.2/3)
  rmaryam = rnorm(maryam, mean=0.5, sd=0.5/3)
  rpaul = rnorm(paul, mean=1.0, sd=1.0/3)
  rsandi = rnorm(sandi, mean=0.25, sd=0.25/3)
  
  rlaura[rlaura<0] = 0
  rmaryam[rmaryam<0] = 0
  rpaul[rpaul<0] = 0
  rsandi[rsandi<0] = 0
  
  s = max(sum(rlaura), sum(rmaryam),  sum(rpaul),  sum(rsandi))
  return (s)
}

SimulateNormalRepairs = function(n,m) {
  arr = c(1:n)
  for (i in 1:n) {
    arr[i] = RepairNormalComputers(m)
  }
  
  labB = "Total repair times for"
  labE = "computers at Normal Computers"
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  
  labM = "mean="
  labS = "sd="
  labM = paste(c(labM, format(round(mean(arr), 4), nsmall = 4)),collapse = " ")
  labM = paste(c(labM, labS), collapse = "\n")
  labM = paste(c(labM, format(round(sd(arr), 4), nsmall = 4)),collapse = " ")
  hist(arr, main=labB, xlab = labM)
  return( t.test(arr))
}
