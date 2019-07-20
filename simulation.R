target <- 3
rate <- 0.01

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
