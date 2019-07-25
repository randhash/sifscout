library(partitions)
library(shiny)
library(dplyr)
library(magrittr)

rarities <- c("N", "R", "SR", "SSR", "UR")
rt <- readRDS("data/rates.rds")
#Functions to calculate negative binomials
dsif <- function(x, size, prob, scout=11) {
  if (x==0) return(0)
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
  raw <- sum(sapply(0:x, dsif, size, prob, scout))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}
#Functions to calculate step ups with specific cards
dst <- function(x, size, prob, target, gr="SR", gr.num=1, rv, psp) {
  key <- x:size
  index <- ifelse(gr=="nothing", Inf, which(rarities==target))
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
pst <- function(x, size, prob, target, gr="SR", gr.num=1, rv, psp, lower.tail=TRUE) {
  raw <- sum(sapply(0:x, dst, size=size, prob=prob, target=target, gr=gr, gr.num=gr.num, rv=rv, psp=psp))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}
#Functions for getting a specific card in the Ltd UR box
dlu <- function(x, m, n, k, psp) sum(dhyper(x:k, m=m, n=n, k=k)*dbinom(x, size=x:k, prob=psp))
plu <- function(x, m, n, k, psp, lower.tail=TRUE) {
  raw <- sum(sapply(0:x, dlu, m, n, k, psp))
  return(abs(ifelse(lower.tail, 0, 1)-raw))
}

ui <- fluidPage(
  titlePanel("sifscout"),
  p("Created by", a("randhash", href="https://www.reddit.com/user/randhash", target="_blank")),
  sidebarPanel(
    tabsetPanel(type="pills", id="tab",
              tabPanel("Honor", value=1,
                       tableOutput("odds1")),
              tabPanel("Regular", value=2,
                       tableOutput("odds2")),
              tabPanel("Scouting Coupon (SR 20%, UR 80%)", value=3,
                       helpText('Note: This is identical to SSR 80%, UR 20% if you treat SR as SSR.'),
                       tableOutput("odds3")),
              tabPanel("Scouting Coupon (Supporting Members)", value=4,
                       tableOutput("odds4")),
              tabPanel("Custom", value=5,
                       numericInput("c.n", label="N rate (%)", value=0, min=0, max=100),
                       numericInput("c.r", label="R rate (%)", value=80, min=0, max=100),
                       numericInput("c.sr", label="SR rate (%)", value=15, min=0, max=100),
                       numericInput("c.ssr", label="SSR rate (%)", value=4, min=0, max=100),
                       numericInput("c.ur", label="UR rate (%)", value=1, min=0, max=100),
                       actionButton("c.auto", label="Autofill empty rates uniformly"),
                       actionButton("c.clear", label="Clear rates", inline=TRUE)
                       )
  ),
  hr(),
  uiOutput("scoutno"),
  selectInput("rare", label="Target card rarity:", choices=c("UR", "SSR", "SR", "R", "N")),
  selectInput("mode", label="What situation?", choices=c("Scouting until a certain number of a specific card (or card rarity) is obtained.",
                                                           "Set number of scouts.")),
  uiOutput("paramentry"),
  checkboxInput("usesp", label="[Optional] Use specific card rate?"),
  helpText('Note: Use this option if you are scouting for a specific card',
           'with a known pull rate. This is the "Appearance rate per member"',
           'in a specific card rarity class that may be found in the details',
           'page in the specific SIF page.'),
  uiOutput("rtentry"),
  hr(),
  uiOutput("ruleentry"),
  numericInput("x", label="this number:", value=1, min=1, step=1),
  strong("is", style="display:inline"),
  textOutput("result", inline=TRUE),
  br(),
  helpText('Tip: You usually want to set the comparison rule to "at least" for the set number of scouts because this ensures',
           'the probability applies for getting at least the specified number of cards. On the other hand, you usually want to',
           'set the comparison rule to "at most" for scouting until a particular draw is achieved because you want to limit the',
           'number of scouts.'),
  br(),
  actionButton(inputId="submit", label="Submit")
  ),
  sidebarPanel(
    h3("Limited UR Box"),
    numericInput("lu.n", label="N cards remaining", value=0, min=0, step=1),
    numericInput("lu.r", label="R cards remaining", value=80, min=0, step=1),
    numericInput("lu.sr", label="SR cards remaining", value=15, min=0, step=1),
    numericInput("lu.ssr", label="SSR cards remaining", value=4, min=0, step=1),
    numericInput("lu.ur", label="Ltd. UR cards remaining", value=1, min=0, step=1),
    actionButton("lu.rd", label="Restore defaults"),
    hr(),
    selectInput("lu.number", label="Scout:", choices=c(11, 10, 1)),
    selectInput("lu.rare", label="Target card rarity", choices=c("Ltd. UR", "SSR", "SR", "R", "N")),
    numericInput("lu.param", label="Number of scouts:", value=1, min=1, step=1),
    checkboxInput("lu.usesp", label="[Optional] Use specific card rate?"),
    helpText('Note: Use this option if you are scouting for a specific card',
             'with a known pull rate. This is the "Appearance rate per member"',
             'in a specific card rarity class that may be found in the details',
             'page in the specific SIF page.'),
    uiOutput("lu.rtentry"),
    hr(),
    selectInput("lu.rule", label="The probability that the number of the specified card(s) in your next Ltd. UR box scout(s) is", choices=c("at least", "exactly equal to", "at most")),
    numericInput("lu.x", label="this number:", value=1, min=1, step=1),
    strong("is", style="display:inline"),
    textOutput("lu.result", inline=TRUE),
    br(),
    br(),
    actionButton(inputId="lu.submit", label="Submit")
    ),
  sidebarPanel(
    h3("Step-Up"),
    numericInput("st.n", label="N rate (%)", value=0, min=0, max=100),
    numericInput("st.r", label="R rate (%)", value=80, min=0, max=100),
    numericInput("st.sr", label="SR rate (%)", value=15, min=0, max=100),
    numericInput("st.ssr", label="SSR rate (%)", value=4, min=0, max=100),
    numericInput("st.ur", label="UR rate (%)", value=1, min=0, max=100),
    actionButton("st.auto", label="Autofill empty rates uniformly"),
    actionButton("st.clear", label="Clear rates", inline=TRUE),
    hr(),
    selectInput("st.gr", label="Guaranteed rarity (or above):", choices=c("nothing", "SR", "SSR")),
    uiOutput("st.grnumentry"),
    helpText('Note: Above number is ignored if "nothing" is guaranteed.'),
    selectInput("st.rare", label="Target card rarity:", choices=c("UR", "SSR", "SR", "R", "N")),
    checkboxInput("st.usesp", label="[Optional] Use specific card rate?"),
    helpText('Note: Use this option if you are scouting for a specific card',
             'with a known pull rate. This is the "Appearance rate per member"',
             'in a specific card rarity class that may be found in the details',
             'page in the specific SIF page.'),
    uiOutput("st.rtentry"),
    hr(),
    selectInput("st.rule", label="The probability that the number of successful pulls in an 11 scout is", choices=c("at least", "exactly equal to", "at most")),
    numericInput("st.x", label="this number:", value=1, min=1, step=1),
    strong("is", style="display:inline"),
    textOutput("st.result", inline=TRUE),
    br(),
    br(),
    actionButton("st.submit", label="Submit")
  )
)

server <- function(input, output, session) {
  #First column
  #Set the correct rate vector display when inputs are submitted
  output$odds1 <- renderTable(data.frame(Rarity=rarities, `Rate (%)`=c(0, 80, 15, 4, 1), check.names=FALSE), align="l", width="100%")
  output$odds2 <- renderTable(data.frame(Rarity=rarities, `Rate (%)`=c(94, 5, 0, 1, 0), check.names=FALSE), align="l", width="100%")
  output$odds3 <- renderTable(data.frame(Rarity=rarities, `Rate (%)`=c(0, 0, 80, 0, 20), check.names=FALSE), align="l", width="100%")
  output$odds4 <- renderTable(data.frame(Rarity=rarities, `Rate (%)`=c(0, 60, 30, 0, 10), check.names=FALSE), align="l", width="100%")
  #Set the correct number of scouts
  output$scoutno <- renderUI({
    selectInput("number", label="Scout:", choices=c(switch(input$tab, "1"=11, "2"=10, "5"=11), 1))
  })
  #Selection of distribution parameters
  output$paramentry <- renderUI({
    numericInput("param", label=switch(input$mode,
                                       "Scouting until a certain number of a specific card (or card rarity) is obtained."="Number of successful pulls:",
                                       "Set number of scouts."="Number of scouts:"),
                 value=1, min=1, step=1)
    })
  #Make the specific card rate entry appear and disappear
  observe({
    if (input$usesp) {
      output$rtentry <- renderUI({
        numericInput("sprate", label="Specific card rate (%):", value=NA, min=0, max=100)
      })
    } else {
      removeUI(".shiny-input-container:has(#sprate)")
    }
  })
  #Rule for probability
  output$ruleentry <- renderUI({
    selectInput("rule", label=paste("The probability that the number of",
                                    switch(input$mode,
                                           "Scouting until a certain number of a specific card (or card rarity) is obtained."="scouts",
                                           "Set number of scouts."="successful pulls"), "is"), choices=c("at least", "exactly equal to", "at most"))
  })
  #Autofill custom rates
  observeEvent(input$c.auto, {
    rv <- c(input$c.n, input$c.r, input$c.sr, input$c.ssr, input$c.ur)
    each <- (100-sum(rv, na.rm=TRUE))/sum(is.na(rv))
    if (is.na(input$c.n)) updateNumericInput(session, "c.n", value=each)
    if (is.na(input$c.r)) updateNumericInput(session, "c.r", value=each)
    if (is.na(input$c.sr)) updateNumericInput(session, "c.sr", value=each)
    if (is.na(input$c.ssr)) updateNumericInput(session, "c.ssr", value=each)
    if (is.na(input$c.ur)) updateNumericInput(session, "c.ur", value=each)
  })
  #Clear custom rates
  observeEvent(input$c.clear, {
    updateNumericInput(session, "c.n", value=NA)
    updateNumericInput(session, "c.r", value=NA)
    updateNumericInput(session, "c.sr", value=NA)
    updateNumericInput(session, "c.ssr", value=NA)
    updateNumericInput(session, "c.ur", value=NA)
  })
  #Compute result
  dsc <- reactiveVal()
  result <- eventReactive(input$submit, {
    validate(
      need(all((c(input$param, input$x) %% 1)==0), "Number of scouts/pulls must be an integer.")
    )
    if (input$usesp) {
      validate(
        need(!is.na(input$sprate), "Fill in the required specific card rate."),
        need(input$sprate<=100, "Specific card rate must not exceed 100%.")
      )
    }
    if (input$tab==5) {
      rate <- c(input$c.n, input$c.r, input$c.sr, input$c.ssr, input$c.ur)[which(rarities==input$rare)]/100
      validate(
        need(!is.na(rate), "Fill in the required rate."),
        need(all(rate<=100), "Rates must not exceed 100%.")
      )
    } else {
      rate <- filter(rt, type==input$tab, rarity==input$rare)[1,"rate"]
    }
    p <- ifelse(input$usesp, input$sprate/100, 1)*rate
    if (input$mode=="Scouting until a certain number of a specific card (or card rarity) is obtained.") {
      if (input$rule=="exactly equal to") {
        res <- R.utils::withTimeout(dsif(input$x, size=input$param, prob=p, scout=as.numeric(input$number)), timeout=3, onTimeout="silent")
      } else {
        res <- R.utils::withTimeout(psif(input$x-switch(input$rule, "at least"=1, "at most"=0), size=input$param, prob=p, scout=as.numeric(input$number), lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)), timeout=3, onTimeout="silent")
      }
      if (!is.null(res)) {
        dsc("exact by computed distribution")
        return(res)
      }
      if ((p>0.01 & as.numeric(input$number)!=1)|input$x<input$param) {
        runs <- 2000
        sim_pulls <- numeric(runs)
        for (i in 1:runs) {
          j <- 0
          k <- 0
          while(k<input$param) {
            j <- j+1
            pull <- rbinom(1, as.numeric(input$number), prob=p)
            k <- k+pull
          }
          sim_pulls[i] <- j
        }
        dsc(paste("approximation by simulation of", runs, "scouts"))
        return(mean(switch(input$rule,
                           "at least"=is_weakly_greater_than,
                           "at most"=is_weakly_less_than,
                           "exactly equal to"=equals)(sim_pulls, input$x)))
      } else {
        dsc(ifelse(as.numeric(input$number)==1, "exact by negative binomial distribution", "approximation by asymptotics of <=1% scout rate"))
        pp <- ifelse(as.numeric(input$number)==1, p, 1-(1-p)^as.numeric(input$number))
        if (input$rule=="exactly equal to") {
          return(dnbinom(input$x-input$param, size=input$param, prob=pp))
        } else {
          return(pnbinom(input$x-input$param-switch(input$rule, "at least"=1, "at most"=0), size=input$param, prob=pp, lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)))
        }
      }
    } else {
      dsc("exact by binomial distribution")
      if (input$rule=="exactly equal to") {
        return(dbinom(input$x, size=input$param*as.numeric(input$number), prob=p))
      } else {
        return(pbinom(input$x-switch(input$rule, "at least"=1, "at most"=0), size=input$param*as.numeric(input$number), prob=p, lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)))
      }    
    }
  })
  output$result <- eventReactive(result(), {paste0(result()*100, "% (", isolate(dsc()), ")")})
  
  #Second column
  #Specific card rate
  observe({
    if (input$lu.usesp) {
      output$lu.rtentry <- renderUI({
        numericInput("lusprate", label="Specific card rate (%):", value=NA, min=0, max=100)
      })
    } else {
      removeUI(".shiny-input-container:has(#lusprate)")
    }
  })
  #Restore defaults
  observeEvent(input$lu.rd, {
    updateNumericInput(session, "lu.n", value=0)
    updateNumericInput(session, "lu.r", value=80)
    updateNumericInput(session, "lu.sr", value=15)
    updateNumericInput(session, "lu.ssr", value=4)
    updateNumericInput(session, "lu.ur", value=1)
  })
  #Compute the result
  lu.result <- eventReactive(input$lu.submit, {
    vec <- c(input$lu.n, input$lu.r, input$lu.sr, input$lu.ssr, input$lu.ur)
    validate(
      need(all((c(vec, input$lu.x) %% 1)==0), "Remaining cards and target value must be integers."),
      need((input$lu.param %% 1)==0, "Number of scouts must be an integer.")
    )
    total <- sum(vec)
    pool <- vec[which(c("N", "R", "SR", "SSR", "Ltd. UR")==input$lu.rare)]
    if (input$lu.usesp) {
      if (input$lu.rule=="exactly equal to") {
        return(dlu(input$lu.x, m=pool, n=total-pool, k=min(as.numeric(input$lu.number)*input$lu.param, total), psp=input$lusprate/100))
      } else {
        return(plu(input$lu.x-switch(input$lu.rule, "at least"=1, "at most"=0), m=pool, n=total-pool, k=min(as.numeric(input$lu.number)*input$lu.param, total), psp=input$lusprate/100, lower.tail=switch(input$lu.rule, "at least"=FALSE, "at most"=TRUE)))
      }
    }
    if (input$lu.rule=="exactly equal to") {
      return(dhyper(input$lu.x, m=pool, n=total-pool, k=min(as.numeric(input$lu.number)*input$lu.param, total)))
    } else {
      return(phyper(input$lu.x-switch(input$lu.rule, "at least"=1, "at most"=0), m=pool, n=total-pool, k=min(as.numeric(input$lu.number)*input$lu.param, total), lower.tail=switch(input$lu.rule, "at least"=FALSE, "at most"=TRUE)))
    }
  })
  output$lu.result <- reactive({paste0(lu.result()*100, "% (exact by hypergeometric distribution)")})
  
  #Third column
  #Step-Up guaranteed
  output$st.grnumentry <- renderUI({
    numericInput("st.grnum", label="Number of guaranteed pulls of above rarity:", value=switch(input$st.gr, "nothing"=0, 1), min=0, step=1)
  })
  #Autofill custom rates
  observeEvent(input$st.auto, {
    rv <- c(input$st.n, input$st.r, input$st.sr, input$st.ssr, input$st.ur)
    each <- (100-sum(rv, na.rm=TRUE))/sum(is.na(rv))
    if (is.na(input$st.n)) updateNumericInput(session, "st.n", value=each)
    if (is.na(input$st.r)) updateNumericInput(session, "st.r", value=each)
    if (is.na(input$st.sr)) updateNumericInput(session, "st.sr", value=each)
    if (is.na(input$st.ssr)) updateNumericInput(session, "st.ssr", value=each)
    if (is.na(input$st.ur)) updateNumericInput(session, "st.ur", value=each)
  })
  #Clear custom rates
  observeEvent(input$st.clear, {
    updateNumericInput(session, "st.n", value=NA)
    updateNumericInput(session, "st.r", value=NA)
    updateNumericInput(session, "st.sr", value=NA)
    updateNumericInput(session, "st.ssr", value=NA)
    updateNumericInput(session, "st.ur", value=NA)
  })
  #Specific card rate
  observe({
    if (input$st.usesp) {
      output$st.rtentry <- renderUI({
        numericInput("stsprate", label="Specific card rate (%):", value=NA, min=0, max=100)
      })
    } else {
      removeUI(".shiny-input-container:has(#stsprate)")
    }
  })
  #Compute results
  st.result <- eventReactive(input$st.submit, {
    rv <- c(input$st.n, input$st.r, input$st.sr, input$st.ssr, input$st.ur)
    validate(
      need(all(!is.na(rv)), "Fill in the rates."),
      need(all.equal(sum(abs(rv)), 100), "Rates must add to 100%."),
      need(all((c(input$st.grnum, input$st.x) %% 1)==0), "Number of guaranteed cards and target value must be integers.")
    )
    rate <- rv[which(rarities==input$st.rare)]
    p <- rate/100
    if (input$st.rule=="exactly equal to") {
      res <- dst(input$st.x, size=11, prob=p, gr=input$st.gr, target=input$st.rare, gr.num=input$st.grnum, rv=rv, psp=ifelse(input$st.usesp, input$stsprate/100, 1))
    } else {
      res <- pst(input$st.x-switch(input$st.rule, "at least"=1, "at most"=0), size=11, prob=p, target=input$st.rare, gr=input$st.gr, gr.num=input$st.grnum, rv=rv, psp=ifelse(input$st.usesp, input$stsprate/100, 1), lower.tail=switch(input$st.rule, "at least"=FALSE, "at most"=TRUE))
    }
    return(res)
  })
  output$st.result <- reactive({paste0(st.result()*100, "% (exact by binomial/computed distribution)")})
}

shinyApp(ui=ui, server=server)