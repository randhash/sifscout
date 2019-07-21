library(shiny)
library(dplyr)

rt <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\sifscout\\rates.rds")
disp <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\sifscout\\disprates.rds")

ui <- fluidPage(
  titlePanel("sifscout"),
  sidebarPanel(
    tabsetPanel(type="pills", id="tab",
              tabPanel("Honor", value=1),
              tabPanel("Regular", value=2),
              tabPanel("Scouting Coupon (SR, SSR, UR Only!)", value=3,
                       selectInput("btype", label="Type:", choices=c("SR 80%, UR 20%", "SSR 80%, UR 20%"))),
              tabPanel("Scouting Coupon (Supporting Members)", value=4)
  ),
  uiOutput("btypeentry"),
  dataTableOutput("odds"),
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

server <- function(input, output) {
  #First column
  #Set the correct rate vector display when inputs are submitted
  output$odds <- renderDataTable({
    if (input$tab!=3) {
      df <- filter(disp, type==input$tab)
    } else {
      df <- filter(disp, type==input$tab, subtype==switch(input$btype, "SR 80%, UR 20%"=1, "SSR 80%, UR 20%"=2)) #df of rates in the order N, SR, SSR, UR
    }
    select(df, Rarity, `Rate (%)`)
    }, options=list(searching=FALSE, paging=FALSE, dom="t", ordering=FALSE))
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
  output$bt <- renderUI({
    selectInput("btype", label="", choices=c())
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
  result <- eventReactive(input$submit, {
    if (input$tab==5) {
      
    }
    validate(
      need((input$param %% 1)==0, "Number of scouts/pulls must be an integer."),
      need((input$x %% 1)==0, "Number of scouts/pulls must be an integer.")
    )
    if (input$tab!=3) {
      rates <- filter(rt, type==input$tab)
    } else {
      rates <- filter(rt, type==input$tab, subtype==switch(input$btype, "SR 80%, UR 20%"=1, "SSR 80%, UR 20%"=2)) #df of rates in the order N, SR, SSR, UR
    }
    p <- ifelse(input$usesp, input$sprate/100, 1)*filter(rates, rarity==input$rare)[1,"rate"]
    if (input$mode=="Scouting until a certain number of a specific card (or card rarity) is obtained.") {
      if ((p/0.001)<10|as.numeric(input$number)==1) {
        pp <- ifelse(as.numeric(input$number)==1, p, 1-(1-p)^as.numeric(input$number))
        if (input$rule=="exactly equal to") {
          return(dnbinom(input$x-input$param, size=input$param, prob=pp))
        } else {
          return(pnbinom(input$x-input$param-switch(input$rule, "at least"=1, "at most"=0), size=input$param, prob=pp, lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)))
        }
      } else {
        sim_pulls <- c()
        for (i in 1:3000) {
          j <- 0
          k <- 0
          while(k<input$param) {
            j <- j+1
            pull <- rbinom(1, as.numeric(input$number), prob=p)
            k <- k+pull
          }
          sim_pulls <- c(sim_pulls, j)
        }
        return(mean(switch(input$rule,
                           "at least"=is_weakly_greater_than,
                           "at most"=is_weakly_less_than,
                           "exactly equal to"=equals)(sim_pulls, input$x)))
      }
    } else {
      if (input$rule=="exactly equal to") {
        return(dbinom(input$x-input$param, size=input$param, prob=p))
      } else {
        return(pbinom(input$x-input$param-switch(input$rule, "at least"=1, "at most"=0), size=input$param, prob=p, lower.tail=switch(input$rule, "at least"=FALSE, "at most"=TRUE)))
      }    
    }
  })
  output$result <- reactive({paste0(result()*100, "%")})
  
  #Second column
  observe({
    if (input$lu.usesp) {
      output$lu.rtentry <- renderUI({
        numericInput("lusprate", label="Specific card rate (%):", value=NA, min=0, max=100)
      })
    } else {
      removeUI(".shiny-input-container:has(#lusprate)")
    }
  })
  lu.result <- eventReactive(input$lu.submit, {
    validate(
      need((input$lu.n %% 1)==0, "Remaining cards must be integers."),
      need((input$lu.r %% 1)==0, "Remaining cards must be integers."),
      need((input$lu.sr %% 1)==0, "Remaining cards must be integers."),
      need((input$lu.ssr %% 1)==0, "Remaining cards must be integers."),
      need((input$lu.ur %% 1)==0, "Remaining cards must be integers.")
    )
    vec <- c(input$lu.n, input$lu.r, input$lu.sr, input$lu.ssr, input$lu.ur)
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