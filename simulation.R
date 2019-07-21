library(ggplot2)
library(dplyr)
library(magrittr)

target <- 1
rate <- 0.04

#Simulation
sim_pulls <- c()
for (i in 1:6000) {
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

ggplot()+
  geom_histogram(data=data.frame(x=sim_pulls), aes(x, ..density..), bins=30, fill="blue", alpha=0.2)+
  geom_line(aes(x, y), data=df)+
  ggtitle(paste(target, "pulls,", rate, "rate"))+
  theme_bw()