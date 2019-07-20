library(shiny)
library(dplyr)

rt <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\sifscout\\rates.rds")
disp <- readRDS("C:\\Users\\Bearkey\\Documents\\Anime\\sifscout\\disprates.rds")

ui <- fluidPage(
  titlePanel("SIF Scout Calculator"),
  sidebarPanel(
    p("A scouting probability tool for Love Live! School Idol Festival"),
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
  selectInput("rare", label="Card rarity:", choices=c("UR", "SSR", "SR", "R", "N")),
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
  actionButton(inputId="submit", label="Submit"),
  textOutput("result")
  )
)

server <- function(input, output) {
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
    selectInput("number", label="Scout:", choices=c(switch(input$tab, "1"=11, "2"=10), 1))
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
      removeUI(sprintf('.shiny-input-container:has(#%s)',"sprate"))
    }
  })
  #Rule for probability
  output$ruleentry <- renderUI({
    selectInput("rule", label=paste("Calculate the probability that the required number of",
                                    switch(input$mode,
                                           "Scouting until a certain number of a specific card (or card rarity) is obtained."="scouts",
                                           "Set number of scouts."="successful pulls"), "is"), choices=c("at least", "exactly equal to", "at most"))
  })
  output$result <- eventReactive(input$submit, {
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
    pfun <- eval(parse(text=paste0(ifelse(input$mode=="exactly equal to", "d", "p"),
                                   switch(input$mode, "Scouting until a certain number of a specific card (or card rarity) is obtained."="nbinom",
                                          "Set number of scouts."="binom"))))
    prob <- pfun(input$x, input$param, lower.tail=FALSE)
  })
}

shinyApp(ui=ui, server=server)