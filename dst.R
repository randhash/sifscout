dst <- function(x, size, prob, psp=NULL) {
  raw <- dbinom(x, size, prob)
}


p <- ifelse(input$st.usesp, input$stsprate/100, 1)*(rate/100)
if (input$st.rule=="exactly equal to") {
  raw <- dbinom(input$st.x, size=11, prob=p)
} else {
  raw <- pbinom(input$st.x-switch(input$st.rule, "at least"=1, "at most"=0), size=11, prob=p, lower.tail=switch(input$st.rule, "at least"=FALSE, "at most"=TRUE))
}
if (input$st.gr==input$st.rare) {
  grvec <- 1:input$st.grnum
  sel <- switch(input$st.rule,
                "at least"=is_weakly_greater_than,
                "at most"=is_weakly_less_than,
                "exactly equal to"=equals)(grvec, input$st.x)
  if (sum(sel)==0) return(raw)
  key <- grvec[sel]-1
  sel2 <- switch(input$st.rule,
                 "at least"=is_weakly_greater_than,
                 "at most"=is_weakly_less_than,
                 "exactly equal to"=equals)(key, input$st.x)
  adj <- ifelse(any(sel2), sum(dbinom(key[sel2], size=11, prob=p)), 0)
  return(sum(c(raw, dbinom(key, size=11, prob=1-(lowrate/100)), -adj)))
} else if ((which(rarities==input$st.rare)<which(rarities==input$st.gr)) & input$st.gr!="nothing") {
  grvec <- 12-(1:input$st.grnum)
  sel <- switch(input$st.rule,
                "at least"=is_weakly_greater_than,
                "at most"=is_weakly_less_than,
                "exactly equal to"=equals)(grvec, input$st.x)
  if (sum(sel)==0) return(raw)
  rev <- sum(c(raw, -dbinom(grvec[sel], size=11, prob=p)))
  return(ifelse(rev<10^(-9), 0, rev))
} else {
  return(raw)
}