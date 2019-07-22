p <- 0.15
rule <- "exactly equal to"
gr <- "SR"
grnum <- 1
x <- 1
if (rule=="exactly equal to") {
  raw <- dbinom(x, size=11, prob=p)
} else {
  raw <- pbinom(x-switch(rule, "at least"=1, "at most"=0), size=11, prob=p)
}
if (gr!="nothing") {
  grvec <- 1:grnum
  sel <- switch(rule,
                "at least"=is_weakly_greater_than,
                "at most"=is_weakly_less_than,
                "exactly equal to"=equals)(grvec, x)
  if (sum(sel)==0) return(raw)
  key <- grvec[sel]-1
  return(sum(c(raw, dbinom(key, size=11, prob=p))))
}