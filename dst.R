rarities <- c("N", "R", "SR", "SSR", "UR")
dst <- function(x, size, prob, gr="SR", gr.num=1, psp) {
  key <- x:size
  index <- which(rarities==input$st.rare)
  grindex <- which(rarities==gr)
  raw <- dbinom(key, size, prob)
  spec <- dbinom(x, size=key, prob=psp)
  if (index>grindex) {
    return(sum(raw*spec))
  } else if (index==grindex) {
    lower <- sum(rv[1:(index-1)])/100
    higher <- 1-prob-lower
    #Partitions for the conditions that permit you to pull below or equal to guaranteed because you pulled better
    adj1v <- rep(0, times=length(raw))
    if (x<gr.num) {
      prt <- Reduce(cbind, lapply(seq(to=11, length.out=gr.num), restrictedparts, 2))
      prt <- prt[,prt[2,]>=gr.num]
      prt <- cbind(prt, prt[2:1,prt[1,]!=prt[2,]])
      prt <- rbind(size-colSums(prt), prt)
      adj1 <- split(apply(prt, 2, function(k) dmultinom(k, prob=c(prob, lower, higher))), prt[1,]) %>%
        sapply(sum)
      adj1v[as.numeric(names(adj1))+1] <- adj1
    }
    #Partitions for the conditions that trigger guaranteed pulls
    prts <- matrix(rep(0, 2), ncol=1, nrow=2)
    if (gr.num>1) {
      prts <- Reduce(cbind, c(list(prts), sapply(1:(gr.num-1), restrictedparts, 2)))
    }
    prts <- cbind(prts, prts[2:1,prts[1,]!=prts[2,]])
    adj2 <- sum(apply(prts, 2, function(k) dmultinom(c(size-sum(k), k), prob=c(lower, prob, higher))))
    raw[which(key==gr.num)] <- raw[which(key==gr.num)]+adj2
    return(sum((raw+adj1v)*spec))
  } else {
    keep <- key<(size-gr.num+1)
    return(sum(raw*keep*spec))
  }
}
pst <- function(x, size, prob, gr="SR", gr.num=1, psp, lower.tail=TRUE) {
  raw <- sum(sapply(0:x, dst, size=size, prob=prob, gr=gr, gr.num=gr.num, psp=psp))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}

sapply(0:6, dst, size=11, prob=0.15, gr="SR", gr.num=1, psp=0.8)
sapply(0:6, function(i) mean(sim_pulls==i))

df <- data.frame(x=0:max(sim_pulls)) %>%
  mutate(y=sapply(x, function(l) dst(l, size=11, prob=0.15, psp=0.33)))
ggplot()+
  geom_histogram(aes(sim_pulls, ..density..), bins=5)+
  geom_line(data=df, aes(x, y))