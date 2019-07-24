prob <- 0.15
psp <- 0.8
gr.num <- 1

#Simulation
runs <- 10000
sim_pulls <- numeric(runs)
for (i in 1:runs) {
  pull <- rmultinom(1, size=11, prob=c(0, 0.8, 0.15, 0.04, 0.01))
  if (sum(pull[3:5,])<gr.num) {
    res <- gr.num
  } else {
    res <- pull[3,1]
  }
  sim_pulls[i] <- rbinom(1, size=res, prob=psp)
}
mean(sim_pulls>=3)