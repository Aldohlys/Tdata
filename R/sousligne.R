
########### Utilities for Subline module
#################################################


### Stand-alone: needs global.R !!!! But has to be removed otherwise as implies loops
# source("global.R")
# library(shiny)
# library(dplyr)
# library(reticulate)
# py_run_file("C:/Users/aldoh/Documents/RApplication/Tutils/inst/python/getContractValue.py")
# library(lubridate)
# library(derivmkts)

NBBO = function(sym,right,strike,expiration,currency,exchange,tradingClass) {
  if (tradingClass == "Stock") {
    display_error_message("A valid Trading Class must be provided!")
    return(NA)
  }
  if (right=="Put") right="P"
  if (right=="Call") right="C"
  expiration=format(expiration,"%Y%m%d")
  strike=as.numeric(strike)
  message("NBBO Option value Sym:",sym," Type:",right," Strike:",strike," Expiration:",expiration,
          " Currency:",currency," Exchange:",exchange," tradingClass:",tradingClass)
  val=reticulate::py$getOptValue(sym=sym,strike=strike,expiration=expiration,
                                 right=right,currency=currency,exchange=exchange,tradingClass=tradingClass)
  if (is.null(val)) val=-1
  else message("NBBO value:",val)
  return(val)
}

#'  build_sub_line function
#'
#'This utility transforms a set of arguments into a dataframe. Any missing argument gets a default value,
#'
#'@param type Put Call or Stock
#'@param sym ticker name Empty default value
#'@param strike strike value - 0 is default
#'@param expdate expiration date Empty default value
#'@param DTE number of days to expiration -NA is default
#'@param price initial option price - 0 is default
#'@param u_price underlying price - 0 is default
#'@param IV implied vol - 0 is default
#'@param mul multiplier - NA is default
#'@param delta delta value  - NA is default
#'@param deltanotional delta notional value (delta dollars)  - NA is default
#'@param gamma gamma value  - NA is default
#'@param theta theta value  - NA is default
#'@param vega vega value  - NA is default
#'@return a data frame with the following arguments:
#' * `instrument`,
#' * `type`,
#' * `strike`,
#' * `mul`,
#' * `price`,
#' * `IV`,
#' * `DTE`,
#' * `delta`,
#' * `deltanotional`
#' * `gamma`,
#' * `theta`,
#' * `vega`,
#'@examples build_sub_line()
#'@examples build_sub_line(sym="STOCK",type="Stock",price=100,
#'mul=1,delta=5,deltanotional = 5*100)
#'@examples build_sub_line(sym="STOCK",type="Put",strike=100,expdate="20240119",
#'price=1.5,mul=100,delta=50,deltanotional=50*100,
#'vega=-24,theta=12)

#'@export
build_sub_line = function(type="", sym="", strike=0, expdate="", DTE=NA, price=0, u_price=0,
                      IV=0, mul=NA, delta=NA, deltanotional=NA,
                      gamma=NA, theta=NA, vega=NA) {

  default_line=data.frame(instrument="", type="", strike=0, mul=NA, price=0, IV=0,
                           DTE=NA, delta=NA, deltanotional=NA,
                          gamma=NA,  theta=NA,  vega=NA)

  stopifnot(is.numeric(c(strike,u_price,mul,price,IV,
                         DTE, delta, gamma, theta, vega, deltanotional))
            & is.character(c(type, sym, expdate)))

  line=dplyr::if_else (type=="", default_line,
                       data.frame(
                         instrument= dplyr::case_match (type,
                                                        "Stock" ~ paste(sym,price),
                                                        c("Put","Call") ~  paste(type,sym,strike,expdate)),
                         type=type,
                         strike=strike,
                         mul=mul,
                         price=price,
                         IV= IV,
                         DTE= DTE,
                         delta=delta,
                         deltanotional= deltanotional, ##equals NA if multiplier or delta=NA
                         gamma=gamma,
                         theta=theta,
                         vega=vega
                       ))
  return(line)
}

#################################################
########################  Line Module


######################

#'   Ligne module UI
#'
#'This module provides the ability to enter parameters to an option
#'And then to output corresponding option price by using either a computation model (BS), looking at IBKR interface or manually.
#'It also allows to give the option price given a number of days and an end unerlying price (BS)
#'@param id this is used by caller to identify line and have the link with server piece
#'@export
sublineUI = function(id) {
  ns=shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("Type"),label="Type:",choices=c("Select","Stock","Put","Call"),
                       selected="Select"),
    shiny::uiOutput(ns("Controls1")),
    shiny::uiOutput(ns("Controls2")),
    shiny::actionButton(ns("Reset"),"Reset!",shiny::icon("trash"),
                        style="color: #fff; background-color: #E98088; border-color: #2e6da4"))
}

#'   Ligne server
#'
#'This module server function returns expected value of a specific option in the future using BS model.
#'
#'For computing option start price, it is possible either to retrieve current value from IBKR (NBBO option),
#'or to enter it manually (Manual option) or by giving a start IV so that a BS option value can be computed.
#'
#'For the end value, BS will be used in all cases for computation.
#'Therefore all input data necessary for BS computation
#'-including risk free interest rate, dividend yield- are expected.
#'@param id this is used by caller to identify line and have the link with server piece
#'@param sym symbol string - may be used to retrieved value from IBKR
#'@param mul multiplier - usually 100, but also 10 (ESTX50)
#'@param s_datetime start date time- currently now
#'@param i_rate value for interest rate (options BS computation)
#'@param u_price value for underlying spot price
#'@param div dividend yield used by BS option computation
#'@param currency string - "USD", "EUR" or "CHF"
#'@param exchange string usually "SMART", may be "CBOE" or "EUREX"
#'@param tradingClass string necessary to get option price from IBKR
#'@param exp_dates a vector of dates
#'@param strikes a vector of strikes
#'@return a data frame built by `build_line` function
#'@export
sublineServer = function(id, sym, mul, s_datetime, i_rate, u_price, div,
                       currency,exchange,tradingClass, exp_dates,strikes) {

  stopifnot(shiny::is.reactive(sym),shiny::is.reactive(mul),shiny::is.reactive(s_datetime),shiny::is.reactive(i_rate),
            shiny::is.reactive(u_price), shiny::is.reactive(div),
            shiny::is.reactive(currency),shiny::is.reactive(exchange),shiny::is.reactive(tradingClass),
            shiny::is.reactive(exp_dates),shiny::is.reactive(strikes))

  shiny::moduleServer(id,
                      function(input,output,session) {
    ############  Action to be defined #########
    ns=shiny::NS(id)
    #### Add strike, Exp. date and pricing methods to User Interface for calls and puts
    shiny::observeEvent(input$Type,{
      if ((shiny::req(input$Type) == "Call") | (input$Type == "Put")) {
        output$Controls1=shiny::renderUI({
          shiny::tagList(
            shiny::selectInput(ns("ExpDate"),label="Exp. date",choices=exp_dates(),selected=exp_dates()[4]),
            {if (length(strikes())>1) shiny::selectInput(ns("Strike"),label="Strike:",choices=strikes(),selected=strikes()[1])
              else shiny::numericInput(ns("Strike"),label="Strike:",value=0)
            },
            shiny::selectInput(ns("Pricing"),label="Pricing: ",choices=c("Select","Black-Scholes","NBBO","Manual"),selected="Select"),
            shiny::textOutput(ns("price"))
          )
        })
      }
      ##### {} is necessary otherwise gets an obscure error:
      ##### Error in installExprFunction: argument "expr" is missing, with no default....
      else {
        output$Controls1 = shiny::renderUI({})
        output$Controls2 = shiny::renderUI({})
      }
    })

    ##### Add field to enter price for Manual pricing method
    shiny::observeEvent(input$Pricing, {
      message("observe pricing")
      if (shiny::req(input$Pricing) == "Manual") {
        output$Controls2=shiny::renderUI(shiny::tagList(
          shiny::numericInput(ns("price"),label=" ",value="")))
      }
      else if (input$Pricing == "Black-Scholes") {
        output$Controls2=shiny::renderUI(shiny::tagList(shiny::numericInput(ns("IV"),label="Implied Vol (%): ",value="")))
      }
      else output$Controls2=shiny::renderUI({})
    })

    shiny::observeEvent(input$ExpDate,{
      message("observe ExpDate: ",sym(),"  ",currency(),"  ",exchange(),"  ",tradingClass(),"  ",input$ExpDate)
      print(strikes())
      if (length(strikes())!=1) {
        expdate=format(as.Date(input$ExpDate,"%d %b %Y"),"%Y%m%d")
        expdate_strikes=reticulate::py$getStrikesfromExpDate(sym=sym(),currency=currency(),exchange=exchange(),
                                                             tradingClass=tradingClass(),
                                                             expdate=expdate,
                                                             strikes=strikes())
        print(expdate_strikes)
        shiny::updateSelectInput(session=session,inputId="Strike",choices= expdate_strikes,
                                 selected=findNearestNumberOrDate(expdate_strikes,u_price()))
      }
    })

    shiny::observeEvent(input$Reset,{
      shiny::updateNumericInput(session, "Type", value="Select")
    })

    ### Compute line elements:
    ### By default every field set to 0 or empty
    ### Stock: take start price and end price given as inputs to server function
    ### Call/Put: compute start price according to pricing method (BS, NBBO, Manual) - end price is always BS
    ###           compute vol for start price - vol for BS is already known

    expDate=shiny::reactive({
      #message("expdate: ",input$ExpDate)
      shiny::req(input$ExpDate)
      as.Date(input$ExpDate,"%d %b %Y")
    })

    strike=shiny::reactive({
      shiny::req(input$Strike)
      #message("strike: ",input$Strike)
      as.numeric(input$Strike)
    })

    price= shiny::reactive({
      message("pricing: ",input$Pricing)
      shiny::req(input$Pricing,input$Strike)
      ### Other arguments like Type, ExpDate, s_datetime, vol and i_rate have default values or tested before (Type)
      switch(input$Pricing,
             "Manual"= shiny::req(input$price),
             "Black-Scholes"= {
               getBSOptPrice(type=input$Type, S=u_price(),K=strike(),r=i_rate(),
                             DTE=getDTE(s_datetime(),expDate()),
                             sig=IV(),div=div())
             },
             "NBBO"= {
               nbbo= NBBO(sym=sym(),right=input$Type, strike=strike(),expiration=expDate(),
                          currency=currency(),exchange=exchange(),tradingClass=tradingClass())
               if (is.na(nbbo)) 0
               else if (nbbo != -1) nbbo
               else {
                 display_error_message("Can't get an option price from IBKR!")
                 0
               }
             },
             0
      )
    })

    sDTE =shiny:: reactive({
      shiny::req(expDate)
      sdte=getDTE(s_datetime(),expDate())
      #message("sDTE: ",sdte)
      sdte
    })


    IV = shiny::reactive({
      shiny::req(input$Type,input$Strike,input$Pricing)
      if (input$Pricing== "Black-Scholes") {
        shiny::req(input$IV)
        iv=as.numeric(input$IV)/100
      } else
        iv=getImpliedVolOpt(type=input$Type,S=u_price(),K=strike(),r=i_rate(),
                                 DTE=sDTE(),price=price(),div=div())
      #message("startPriceIV: ",startiv)
      iv
    })


    delta = shiny::reactive({
      delta=NA
      if (input$Type != "Stock") {
        shiny::req(input$Strike)
          # print(paste("Delta BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
          #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
          delta=getBSOptDelta(type=input$Type,S=u_price(),K=strike(),
                              DTE=sDTE(),sig=IV(),r=i_rate(),div=div())
      }
      # print(paste("Delta:",delta))
      delta
    })

    gamma = shiny::reactive({
      gamma=0
      if (input$Type != "Stock") {
        shiny::req(input$Strike)

          # print(paste("Gamma BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
          #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
          gamma=getBSOptGamma(type=input$Type,S=u_price(),K=strike(),
                              DTE=sDTE(),sig=IV(),r=i_rate(),div=div())

      }
      # print(paste("Gamma:",gamma))
      gamma
    })


    theta = shiny::reactive({
      theta=0
      if (input$Type != "Stock") {
        shiny::req(input$Strike)

          # print(paste("theta BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
          #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
          theta=getBSOptTheta(type=input$Type,S=u_price(),K=strike(),
                              DTE=sDTE(),sig=IV(),r=i_rate(),div=div())

      }
      # print(paste("theta:",theta))
      theta
    })


    vega = shiny::reactive({
      vega=0
      if (input$Type != "Stock") {
        shiny::req(input$Strike)

          # print(paste("vega BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
          #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
          vega=getBSOptVega(type=input$Type,S=u_price(),K=strike(),
                            DTE=sDTE(),sig=IV(),r=i_rate(),div=div())

      }
      # print(paste("vega:",vega))
      vega
    })

    output$price = shiny::renderText({
      shiny::req(input$Pricing)
      if (input$Pricing == "Black-Scholes") paste0(" Price: ", round(price(),2))
      else if (input$Pricing == "Manual")  paste0(" IV: ",round(IV()*100,2),"%")
      else paste0("Price: ", round(price(),2),"  IV: ",round(IV()*100,2),"%")
    })

   return(shiny::reactive({
      stopifnot(is.numeric(c(mul(),i_rate(),u_price())))

      switch(input$Type,
             "Stock" = build_sub_line(type="Stock", sym=shiny::req(sym()), price=u_price(),
                                  mul=1,  delta=1,deltanotional=u_price()),
             "Put"=,
             "Call"= {
               deltanet=delta()*mul()
               gammanet=gamma()*mul()
               thetanet=theta()*mul()
               veganet=vega()*mul()

               build_sub_line(type=input$Type, sym=shiny::req(sym()), strike=shiny::req(strike()),
                          expdate=format(shiny::req(expDate()),"%d-%b-%Y"),
                          DTE=sDTE(), price=price(),
                          IV=IV(),
                          mul=mul(),
                          delta=deltanet,
                          deltanotional=deltanet*u_price(),
                          gamma=gammanet,
                          theta=thetanet,
                          vega=veganet
               )
             },
             build_sub_line()) ## Default value
    }))
  })
}

##print("Ligne module loaded!")

#'   ligneApp function
#'
#'This module server function returns expected value of a specific option in the future using BS model.
#'
#'For computing option start price, it is possible either to retrieve current value from IBKR (NBBO option),
#'or to enter it manually (Manual option) or by giving a start IV so that a BS option value can be computed.
#'
#'For the end value, BS will be used in all cases for computation.
#'Therefore all input data necessary for BS computation
#'-including risk free interest rate, dividend yield- are expected.
#'@param interest_rate this is used by caller to identify line and have the link with server piece
#'@export
ligneApp = function(interest_rate) {
  weekly_dates=c(lubridate::ymd("2024-01-19"),lubridate::ymd("2024-01-26"),
                 lubridate::ymd("2024-02-02"),lubridate::ymd("2024-02-09"),lubridate::ymd("2024-02-16"),lubridate::ymd("2024-02-23")
  )


  monthly_dates=c(lubridate::ymd("2024-03-15"),lubridate::ymd("2024-04-19"),lubridate::ymd("2024-05-17"),lubridate::ymd("2024-06-21"),lubridate::ymd("2024-07-19"),
                  lubridate::ymd("2024-08-16"),lubridate::ymd("2024-09-20"),lubridate::ymd("2024-10-18"),lubridate::ymd("2024-11-15"),lubridate::ymd("2024-12-20"),
                  lubridate::ymd("2025-01-17"), lubridate::ymd("2025-03-21"), lubridate::ymd("2025-06-18"), lubridate::ymd("2025-06-20"), lubridate::ymd("2025-09-19"), lubridate::ymd("2025-12-19"),
                  lubridate::ymd("2026-01-16"))
  dates_list=c(weekly_dates, monthly_dates)

  ui = shiny::fluidPage(
    shiny::h1("Ligne Demo"),
    shiny::h3(shiny::textOutput("title")),
    shiny::textInput("sym","Symbol: ",value="SPX"),
    shiny::numericInput("mul","Multiplier: ",value=100),
    shiny::selectInput("currency","Currency: ",choices=c("USD","EUR","CHF"),selected="USD"),
    shiny::selectInput("exchange","Exchange: ",choices=c("SMART","EUREX","CBOE"),selected="CBOE"),
    shiny::textInput("tradingClass","Trading Class: ",value="SPX"),
    shiny::numericInput("strikes","Strike: ",value=4900),
    shiny::numericInput("u_price","Current underlying price:",value=4920),
    #shiny::numericInput("div","Dividend yield (%):",value=0),
    shiny::hr(),
    sublineUI("Ligne"),
    shiny::tableOutput("table")
  )

  server = function(input, output, session) {

    ligne=sublineServer(id="Ligne",sym=shiny::reactive(input$sym),
                      mul=shiny::reactive(input$mul),
                      s_datetime=shiny::reactive(lubridate::now()),
                      i_rate=shiny::reactive(interest_rate),
                      u_price=shiny::reactive(input$u_price),
                      div=shiny::reactive(0),
                      currency=shiny::reactive(input$currency),
                      exchange=shiny::reactive(input$exchange),
                      tradingClass = shiny::reactive(input$tradingClass),
                      exp_dates=shiny::reactive(format(dates_list,"%d %b %Y")),
                      strikes=shiny::reactive(input$strikes)
                      #strikes=reactive(seq(150,170,2))
    )

    output$table=shiny::renderTable(ligne())
    output$title=shiny::renderText({paste0(input$sym," current price=",input$u_price,
                                            "  div yield= 0 interest_rate=",interest_rate*100,"%")})

  }
  shiny::shinyApp(ui, server)
}
