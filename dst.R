library(magrittr)
library(partitions)

rarities <- c("N", "R", "SR", "SSR", "UR")
x <- 0
gr <- "SSR"
size <- 11
psp <- 1
gr.num <- 11
rv <- c(0, 80, 15, 4, 1)/100
target <- "SR"
prob <- rv[which(rarities==target)]

dst <- function(x, size, prob, target, gr="SR", gr.num=1, rv, psp) {
  if (x<0|x>size) return(0)
  key <- x:size
  index <- ifelse(gr=="nothing", Inf, which(rarities==target))
  grindex <- ifelse(gr=="nothing", 0, which(rarities==gr))
  raw <- dbinom(key, size, prob)
  spec <- dbinom(x, size=key, prob=psp)
  if (index>grindex) {
    return(sum(raw*spec))
  } else if (index==grindex) {
    raw[key<gr.num] <- 0
    lower <- sum(rv[1:(index-1)])
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
    adjv <- rep(0, length(key))
    higher <- sum(rv[grindex:length(rv)])
    lh <- 1-higher-prob
    depth <- size:(size-gr.num)
    if (any(depth==0)) {
      depth <- depth[depth!=0]
      prt <- Reduce(cbind, c(list(matrix(rep(0, 2), ncol=1, nrow=2)), lapply(depth, restrictedparts, 2)))
    } else {
      prt <- Reduce(cbind, lapply(depth, restrictedparts, 2))
    }
    prt <- cbind(prt, prt[2:1,prt[1,]!=prt[2,]])
    prt <- rbind(prt, size-colSums(prt))
    grp <- prt[1,]-(gr.num-prt[3,])
    grp <- ifelse(grp<0, 0, grp)
    prt <- prt[,grp>=x]
    adj <- split(apply(prt, 2, function(k) dmultinom(k, prob=c(prob, lh, higher))), grp[grp>=x]) %>% sapply(sum)
    repl <- which(key %in% as.numeric(names(adj)))
    if (length(repl)>0) adjv[repl] <- adj
    return(sum(adjv*spec))
  }
}
pst <- function(x, size, prob, target, gr="SR", gr.num=1, rv, psp, lower.tail=TRUE) {
  if (lower.tail) {
    raw <- sum(sapply(0:x, dst, size=size, prob=prob, target=target, gr=gr, gr.num=gr.num, rv=rv, psp=psp))
  } else {
    raw <- sum(sapply((x+1):size, dst, size=size, prob=prob, target=target, gr=gr, gr.num=gr.num, rv=rv, psp=psp))
  }
  return(raw)
}

#Simulation
runs <- 5000000
sim_pulls <- numeric(runs)
grindex <- which(rarities==gr)
index <- which(rarities==target)
for (i in 1:runs) {
  pull <- rmultinom(1, size=11, prob=c(0, 0.8, 0.15, 0.04, 0.01))
  if (sum(pull[grindex:5,])<gr.num) {
    res <- pull[index,1]-(gr.num-sum(pull[grindex:5,]))
    res <- ifelse(res<0, 0, res)
  } else {
    res <- pull[index,1]
  }
  sim_pulls[i] <- rbinom(1, size=res, prob=psp)
}

sapply(0:11, dst, size=size, prob=prob, gr=gr, target=target, gr.num=gr.num, rv=rv, psp=psp)
sapply(0:11, function(i) mean(sim_pulls==i))

