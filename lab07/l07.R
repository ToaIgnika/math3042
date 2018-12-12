DiceMeans = function(n,m) {
  # roll m dice n times
  dieSampleList = c(1:m)
  {
    for (i in 1:n)
    {
      dr = sample(c(1:6), m, repl=T)
      dieSampleList[i] = mean(dr)
      #dr = table(dr)
    }
  }
  labB = "Means for"
  labE = "rolls of"
  labC = "dice"
  labB = paste(c(labB, n), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labC), collapse = " ")
  barplot(table(dieSampleList), main=labB, xlab = "Sample dice", ylab="Number of rolls")
  return (c(mean(dieSampleList), sd(dieSampleList)))
}

NormalMeans = function(n,m) {
  normSample = c(1:n);
  for (i in 1:n) {
    arr = rnorm(m, mean=0, sd=1);
    normSample[i] = mean(arr);
  }
  labB = "Means for"
  labE = "samples of"
  labC = "samples"
  labB = paste(c(labB, n), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labC), collapse = " ")
  hist((normSample), main=labB, xlab = "Sample", ylab="Number of samples")
  return (c(mean(normSample), sd(normSample)))
}

ExponentialMeans = function(n,m) {
  normSample = c(1:n);
  for (i in 1:n) {
    arr = rexp(m, rate=1);
    normSample[i] = mean(arr);
  }
  labB = "Means for"
  labE = "samples of"
  labC = "samples"
  labB = paste(c(labB, n), collapse = " ")
  labB = paste(c(labB, labE), collapse = " ")
  labB = paste(c(labB, m), collapse = " ")
  labB = paste(c(labB, labC), collapse = " ")
  hist((normSample), main=labB, xlab = "Sample", ylab="Number of samples")
  return (c(mean(normSample), sd(normSample)))
} 