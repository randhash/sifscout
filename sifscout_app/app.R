library(shiny)
library(dplyr)
library(magrittr)

rarities <- c("N", "R", "SR", "SSR", "UR")
rt <- readRDS("data/rates.rds")

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
                       numericInput("c.n", label="N rate (%)", value=NA, min=0, max=100),
                       numericInput("c.r", label="R rate (%)", value=NA, min=0, max=100),
                       numericInput("c.sr", label="SR rate (%)", value=NA, min=0, max=100),
                       numericInput("c.ssr", label="SSR rate (%)", value=NA, min=0, max=100),
                       numericInput("c.ur", label="UR rate (%)", value=NA, min=0, max=100),
                       actionButton("c.auto", label="Autofill empty rates uniformly"),
                       actionButton("c.clear", label="Clear rates", inline=TRUE))
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
    selectInput("lu.number", label="Scout:", choices=c(11, 1)),
    selectInput("lu.rare", label="Target card rarity", choices=c("Ltd. UR", "SSR", "SR", "R", "N")),
    checkboxInput("lu.usesp", label="[Optional] Use specific card rate?"),
    helpText('Note: Use this option if you are scouting for a specific card',
             'with a known pull rate. This is the "Appearance rate per member"',
             'in a specific card rarity class that may be found in the details',
             'page in the specific SIF page.'),
    uiOutput("lu.rtentry"),
    hr(),
    selectInput("lu.rule", label="The probability that the number of the specified card(s) in your next Ltd. UR box scout is", choices=c("at least", "exactly equal to", "at most")),
    numericInput("lu.x", label="this number:", value=1, min=1, step=1),
    strong("is", style="display:inline"),
    textOutput("lu.result", inline=TRUE),
    br(),
    br(),
    actionButton(inputId="lu.submit", label="Submit")
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
    if (input$tab==5) {
      rate <- c(input$c.n, input$c.r, input$c.sr, input$c.ssr, input$c.ur)[which(rarities==input$rare)]/100
      validate(
        need(!is.na(rate), "Fill in the required rate.")
      )
    } else {
      rate <- filter(rt, type==input$tab, rarity==input$rare)[1,"rate"]
    }
    dsc("exact")
    p <- ifelse(input$usesp, input$sprate/100, 1)*rate
    if (input$mode=="Scouting until a certain number of a specific card (or card rarity) is obtained.") {
      if ((p>0.01 & as.numeric(input$number)!=1)|input$x<input$param) {
        runs <- 1000
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
        dsc(ifelse(as.numeric(input$number)==1, "exact", "approximation by asymptotics of <=1% scout rate"))
        pp <- ifelse(as.numeric(input$number)==1, p, 1-(1-p)^as.numeric(input$number))
        if (input$rule=="exactly equal to") {
          return(dnbinom(input$x-input$param, size=input$param, prob=pp))
        } else {
          return(pnbinom(input$x-input$param-switch(input$rule, "at least"=1, "at most"=0), size=input$param, prob=pp, lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)))
        }
      }
    } else {
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
      need(all((vec %% 1)==0), "Remaining cards must be integers.")
    )
    total <- sum(vec)
    pool <- vec[which(c("N", "R", "SR", "SSR", "Ltd. UR")==input$lu.rare)]
    if (input$lu.rule=="exactly equal to") {
      res <- dhyper(input$lu.x, m=pool, n=total-pool, k=as.numeric(input$lu.number))
    } else {
      res <- phyper(input$lu.x-switch(input$lu.rule, "at least"=1, "at most"=0), m=pool, n=total-pool, k=min(as.numeric(input$lu.number), total), lower.tail=switch(input$lu.rule, "at least"=FALSE, "at most"=TRUE))
    }
    if (input$lu.usesp) {
      res <- res*input$lusprate/100
    }
    return(res)
  })
  output$lu.result <- reactive({paste0(lu.result()*100, "%")})
}

shinyApp(ui=ui, server=server)