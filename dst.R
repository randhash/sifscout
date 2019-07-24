library(magrittr)
library(partitions)

x <- 0
gr <- "nothing"
size <- 11
prob <- 0.04
psp <- 1
gr.num <- 3
rv <- c(0, 80, 15, 4, 1)
input <- list(st.rare="SSR")

rarities <- c("N", "R", "SR", "SSR", "UR")
dst <- function(x, size, prob, gr="SR", gr.num=1, psp) {
  key <- x:size
  index <- ifelse(gr=="nothing", Inf, which(rarities==input$st.rare))
  grindex <- ifelse(gr=="nothing", 0, which(rarities==gr))
  raw <- dbinom(key, size, prob)
  spec <- dbinom(x, size=key, prob=psp)
  if (index>grindex) {
    return(sum(raw*spec))
  } else if (index==grindex) {
    raw[key<gr.num] <- 0
    lower <- sum(rv[1:(index-1)])/100
    higher <- 1-prob-lower
    #Partitions for the conditions that permit you to pull below guaranteed because you pulled better
    adj1v <- rep(0, times=length(raw))
    if (x<gr.num) {
      prt <- Reduce(cbind, lapply(size-intersect(1:gr.num-1, key), restrictedparts, 2))
      prt <- cbind(prt, prt[2:1,prt[1,]!=prt[2,]])
      prt <- rbind(size-colSums(prt), prt)
      prt <- matrix(prt[,(prt[1,]+prt[3,])>=gr.num], nrow=3)
      adj1 <- split(apply(prt, 2, function(k) dmultinom(k, prob=c(prob, lower, higher))), prt[1,]) %>% sapply(sum)
      adj1v[which(key %in% as.numeric(names(adj1)))] <- adj1
    }
    #Partitions for the conditions that trigger guaranteed pulls
    adj2v <- rep(0, times=length(raw))
    if (gr.num>0) {
      prts <- matrix(rep(0, 2), ncol=1, nrow=2)
      if (gr.num>1) {
        prts <- Reduce(cbind, c(list(prts), lapply(1:(gr.num-1), restrictedparts, 2)))
      }
      prts <- cbind(prts, prts[2:1,prts[1,]!=prts[2,]])
      prts <- rbind(size-colSums(prts), prts)
      adj2 <- sum(apply(prts, 2, function(k) dmultinom(k, prob=c(lower, prob, higher))))
      adj2v[which(key==gr.num)] <- adj2
    }
    return(sum((raw+adj1v+adj2v)*spec))
  } else {
    keep <- key<(size-gr.num+1)
    return(sum(raw*keep*spec))
  }
}
pst <- function(x, size, prob, gr="SR", gr.num=1, psp, lower.tail=TRUE) {
  raw <- sum(sapply(0:x, dst, size=size, prob=prob, gr=gr, gr.num=gr.num, psp=psp))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}
pst(x, 11, prob, "SSR", gr.num=0, psp=1, lower.tail = F)
#Simulation
runs <- 100000
sim_pulls <- numeric(runs)
for (i in 1:runs) {
  pull <- rmultinom(1, size=11, prob=c(0, 0.8, 0.15, 0.04, 0.01))
  if (sum(pull[4:5,])<gr.num) {
    res <- gr.num
  } else {
    res <- pull[4,1]
  }
  sim_pulls[i] <- rbinom(1, size=res, prob=psp)
}
#allcomb <- Reduce(cbind, all)
#allcomb[,allcomb[3,]==0]

sapply(0:6, dst, size=size, prob=prob, gr=gr, gr.num=gr.num, psp=psp)
sapply(0:6, function(i) mean(sim_pulls==i))

