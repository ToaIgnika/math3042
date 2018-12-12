Lottery = function(n) {
  arr= c(1:n);
  for (i in 1:n) {
    count = 1;
    while (sample(c(1:5)) != 1) {
      count = count + 1;
    }
    arr[i] = count;
    }
 barplot(table(arr),main = "Table of attempts for lottery", xlab="number of attempts", ylab="number of students");
  return(table(arr));
}

Lottery2 = function(n) {
  arr = rgeom(c(1:n), 1/5);
  barplot(table(arr),main = "Table of attempts for lottery", xlab="number of attempts", ylab="number of students");
  return(table(arr));
}

Cable = function(n) {
  arr = rpois(c(1:n), 0.75);
  barplot(table(arr), main="Fiber flaw distribution per one m with 0.75%", ylab= "probability")
  return (table(arr));
}

Bus = function (n) {
  arr = runif(n, min=0, max=20);
  proc = sum(arr < 10);
  return (proc/n);
}

Battery = function (n) {
  arr = rnorm(n, mean=9.0, sd=0.05);
  hist(arr , main="Histogram of battery voltages", xlab="voltage V");
  return();
}

Battery2 = function (n) {
  arr = rnorm(n, mean=9.0, sd=0.05);
  return (sum(arr > 8.9 & arr < 9.1)/n);
}

