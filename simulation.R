library(ggplot2)
library(dplyr)
library(magrittr)

target <- 6
rate <- 0.01

#Simulation
sim_pulls <- c()
for (i in 1:10000) {
  j <- 0
  k <- 0
  while(k<target) {
    j <- j+1
    pull <- rbinom(1, 11, prob=rate)
    k <- k+pull
  }
  sim_pulls <- c(sim_pulls, j)
}
mean(sim_pulls<=2)

#Define success as getting at least one target pull in an 11 scout
p <- 1-(1-rate)^11
plotfun <- function(x, ...) dnbinom(x-target, ...)
df <- data.frame(x=target:max(sim_pulls)) %>%
  mutate(y=plotfun(x, size=target, prob=p))

#Try this new distribution
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
dftest <- data.frame(x=1:max(sim_pulls)) %>%
  mutate(y=sapply(x, dsif, size=target, prob=rate))


ggplot()+
  geom_histogram(data=data.frame(x=sim_pulls), aes(x, ..density..), bins=180, fill="blue", alpha=0.2)+
  geom_line(aes(x, y), data=dftest, col="green")+
  geom_line(aes(x, y), data=df, col="red")+
  ggtitle(paste(target, "pulls,", rate, "rate"))+
  theme_bw()