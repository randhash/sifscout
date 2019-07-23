library(magrittr)
library(partitions)

dsif <- function(x, size, prob, scout=11) {
  partitions <- restrictedparts(size, x)
  first <- apply(partitions, 2, function(l) choose(x-1, sum(l==0)))
  ends <- apply(partitions, 2, function(k) unique(k[k!=0]))
  probs <- numeric(sum(sapply(ends, length)))
  l <- 1
  for (i in 1:length(ends)) {
    end <- ends[[i]]
    vec <- partitions[,i]
    for (j in 1:length(end)) {
      spend <- end[j]
      newvec <- vec[-which(vec==spend)[1]]
      second <- factorial(sum(newvec!=0))/prod(factorial(table(newvec, exclude=0)))
      probs[l] <- prod(c(pbinom(spend-1, size=scout, prob=prob, lower.tail=FALSE), dbinom(newvec, size=scout, prob=prob)))*second*first[i]
      l <- l+1
    }
  }
  return(sum(probs))
}
psif <- function(x, size, prob, scout=11, lower.tail=TRUE) {
  raw <- sum(sapply(1:x, dsif, size, prob, scout))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}