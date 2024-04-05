
########### Utilities for Line module
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

#'  build_line function
#'
#'This utility transforms a set of arguments into a dataframe. Any missing argument gets a default value.
#'All data is rounded to 2 digits (dollar value) or 3 digits (percentage)
#'
#'@param type Put Call or Stock
#'@param sym ticker name Empty default value
#'@param strike strike value - 0 is default
#'@param expdate expiration date Empty default value
#'@param DTE number of days to expiration -NA is default
#'@param startPrice initial price - 0 is default
#'@param endPrice final price - 0 is default
#'@param startPriceIV implied vol for initial price - 0 is default
#'@param endPriceIV implied vol for final price - 0 is default
#'@param pos position  - 0 is default
#'@param mul multiplier - NA is default
#'@param delta delta value  - NA is default
#'@param deltanotional delta notional (delta dollars or euros) equals `mul*pos*delta*underlying price` or simply `deltanet*underlying price`
#'@param gamma gamma value  - NA is default
#'@param theta theta value  - NA is default
#'@param vega vega value  - NA is default
#'@return a data frame with the following arguments:
#' * `instrument`,
#' * `type`,
#' * `strike`,
#' * `mul`,
#' * `pos`
#' * `startPrice`,
#' * `startPriceIV`,
#' * `endPrice`,
#' * `endPriceIV`,
#' * `change`= (endPrice-startPrice)/startPrice, Division is done only if endPrice differs from startPrice.
#' Equals to NA if both startPrice and endPrice are equal to NA.
#' * `startValue`=mul`*`pos`*`startPrice,
#' * `endValue`=mul`*`pos`*`endPrice,
#' * `unrealizedPnL`=mul`*`pos`*`(endPrice-startPrice),
#' * `DTE`,
#' * `delta`,
#' * `deltanet`=mul`*`pos`*`delta,
#' * `deltanotional`
#' * `gamma`,
#' * `theta`,
#' * `vega`,
#'
#'NB: If mul is NA then startValue, endValue, unPnL are all equal to NA.
#'@examples build_line()
#'@examples build_line(sym="STOCK",type="Stock",startPrice=100,endPrice=110,
#'mul=1,pos=5,delta=1)
#'@examples build_line(type="Put", pos=2,strike=95,expdate = 20240209,
#'startPrice=0.74,mul=100,endPrice=0.19,delta=-0.08,deltanotional=100*(-0.08)*2*102)
#'@export
build_line = function(type="", sym="", strike=NA, expdate="", DTE=NA, startPrice=0, endPrice=0,
                      startPriceIV=NA, endPriceIV=NA, pos=0, mul=NA, delta=NA, deltanotional=NA,
                      gamma=NA, theta=NA, vega=NA) {

  default_line=data.frame(instrument="", type=type, strike=strike, mul=mul, pos=pos, startPrice=startPrice,
                          startPriceIV=startPriceIV,  endPrice=endPrice, endPriceIV=endPriceIV,  change=0,
                          startValue=NA, endValue=NA, unrealizedPnL=NA, DTE=DTE, delta=delta, deltanet=NA,
                          deltanotional=NA, gamma=gamma,  theta=theta,  vega=vega)

  stopifnot(is.numeric(c(strike,pos,mul,startPrice,startPriceIV,endPrice,endPriceIV,
                         DTE, delta, deltanotional, gamma, theta, vega))
            & is.character(c(type, sym, expdate)))

  line=dplyr::if_else (type=="", default_line,
                data.frame(
                  instrument= dplyr::case_match (type,
                                          "Stock" ~ paste(sym,startPrice),
                                          c("Put","Call") ~  paste(type,sym,strike,expdate)),
                  type=type,
                  strike=strike,
                  mul=mul,
                  pos=pos,
                  startPrice=startPrice,
                  startPriceIV= startPriceIV,
                  endPrice=endPrice,
                  endPriceIV= endPriceIV,
                  change= change(startPrice,endPrice),
                  startValue=mul*pos*startPrice, ##equals NA if multiplier=NA
                  endValue=mul*pos*endPrice, ##equals NA if multiplier=NA
                  unrealizedPnL=mul*pos*(endPrice-startPrice), ##equals NA if multiplier=NA
                  DTE= DTE,
                  delta=round(delta,2),
                  deltanet=round(pos*mul*delta,3),
                  deltanotional= round(deltanotional,3), ##equals NA if multiplier or delta=NA - needs underlying price to be computed
                  gamma=round(gamma,3),
                  theta=round(theta,3),
                  vega=round(vega,3)
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
ligneUI = function(id) {
  ns=shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(ns("Type"),label="Type:",choices=c("Select","Stock","Put","Call"),
                selected="Select"),
    shiny::numericInput(ns("Qty"),label="Quantity:",value=0),
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
#'@param p_days number of days till projected date
#'@param p_u_price projected underlying price
#'@param currency string - "USD", "EUR" or "CHF"
#'@param exchange string usually "SMART", may be "CBOE" or "EUREX"
#'@param tradingClass string necessary to get option price from IBKR
#'@param exp_dates a vector of dates
#'@param strikes a vector of strikes
#'@return a data frame built by `build_line` function
#'@export
ligneServer = function(id, sym, mul, s_datetime, i_rate, u_price, div,
                       p_days, p_u_price,currency,exchange,tradingClass,
                       exp_dates,strikes
                       ) {
  stopifnot(shiny::is.reactive(sym),shiny::is.reactive(mul),shiny::is.reactive(s_datetime),shiny::is.reactive(i_rate),
            shiny::is.reactive(u_price), shiny::is.reactive(div),shiny::is.reactive(p_days),shiny::is.reactive(p_u_price),
            shiny::is.reactive(currency),shiny::is.reactive(exchange),shiny::is.reactive(tradingClass),
            shiny::is.reactive(exp_dates),shiny::is.reactive(strikes))

  shiny::moduleServer(id, function(input,output,session) {
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
            shiny::selectInput(ns("Pricing"),label="Pricing: ",choices=c("Select","Black-Scholes","Bjerksund-Stensland","NBBO","Manual"),selected="Select"),
            shiny::textOutput(ns("start_price"))
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
            shiny::numericInput(ns("startPrice"),label=" ",value=""),
            shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value=""),
            shiny::textOutput(ns("end_price"))))
        }
        else if ((input$Pricing == "Black-Scholes") || (input$Pricing == "Bjerksund-Stensland")) {
          output$Controls2=shiny::renderUI(shiny::tagList(shiny::numericInput(ns("startIV"),label="Start Implied Vol (%): ",value=""),
                                                          shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value=""),
                                                          shiny::textOutput(ns("end_price"))))
        }
        else if (input$Pricing == "NBBO") {
          output$Controls2=shiny::renderUI(shiny::tagList(shiny::numericInput(ns("p_vol"),label="End Implied Vol (%):",value=""),
                                                          shiny::textOutput(ns("end_price"))))

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

    #### As initial value copies strt vol into end vol
    shiny::observeEvent(startPriceIV(),{
      shiny::updateNumericInput(session=session,inputId="p_vol",value=input$startIV)
    },once=TRUE)

    shiny::observeEvent(input$Reset,{
      shiny::updateNumericInput(session, "Type", value="Select")
      shiny::updateNumericInput(session, "Qty", value=0)
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

     startPrice= shiny::reactive({
       message("startPrice: ",input$Pricing)
       shiny::req(input$Pricing,input$Strike)
       ### Other arguments like Type, ExpDate, s_datetime, vol and i_rate have default values or tested before (Type)
       switch(input$Pricing,
              "Manual"= shiny::req(input$startPrice),
              "Black-Scholes"= {
                getBSOptPrice(type=input$Type, S=u_price(),K=strike(),r=i_rate(),
                              DTE=getDTE(s_datetime(),expDate()),
                              sig=startPriceIV(),div=div())
              },
              "Bjerksund-Stensland" = {
                getBjSOptPrice(type=input$Type, S=u_price(),K=strike(),r=i_rate(),
                              DTE=getDTE(s_datetime(),expDate()),
                              sig=startPriceIV(),div=div())
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

     eDTE = shiny::reactive({
       shiny::req(expDate)
       edte=getDTE(s_datetime()+lubridate::ddays(as.numeric(p_days())),expDate())
       #message("eDTE: ",edte)
       edte
     })

     startPriceIV = shiny::reactive({
       shiny::req(input$Type,input$Strike,input$Pricing)
       if ((input$Pricing == "Black-Scholes") || (input$Pricing == "Bjerksund-Stensland")) {
         shiny::req(input$startIV)
         startiv=as.numeric(input$startIV)/100
        }
       else if ((input$Pricing == "Manual") || (input$Pricing == "NBBO")) {
         shiny::req(startPrice, sDTE)
         startiv=getImpliedVolOpt(type=input$Type,S=u_price(),K=strike(),r=i_rate(),
                                  DTE=sDTE(),price=startPrice(),div=div())
       }

       else startiv=0
       #message("startPriceIV: ",startiv)
       startiv
     })

     endPrice = shiny::reactive({
       shiny::req(input$Strike)
       endp=0
       ### Test for arguments that have no default value (only empty string)
       if ((p_u_price() != "") & (p_days() != "")) {
         endp=getBSOptPrice(type=input$Type,S=p_u_price(),K=strike(),r=i_rate(),
                       DTE=eDTE(),sig=endPriceIV(), div=div())
       }
       #print(paste("endPrice:",endp))
       endp
     })

     endPriceIV = shiny::reactive({
       shiny::req(input$p_vol)
       #print(paste("endPriceIV:",input$p_vol/100))
       input$p_vol/100
     })

     delta = shiny::reactive({
       delta=NA
       if (input$Type != "Stock") {
         shiny::req(input$Strike)
         if ((p_u_price() != "") & (p_days() != "")) {
           # print(paste("Delta BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
           #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
           delta=getBSOptDelta(type=input$Type,S=p_u_price(),K=strike(),
                               DTE=eDTE(),sig=endPriceIV(),r=i_rate(),div=div())
         }
       }
       # print(paste("Delta:",delta))
       delta
     })

     gamma = shiny::reactive({
       gamma=0
       if (input$Type != "Stock") {
         shiny::req(input$Strike)
         if ((p_u_price() != "") & (p_days() != "")) {
           # print(paste("Gamma BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
           #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
           gamma=getBSOptGamma(type=input$Type,S=p_u_price(),K=strike(),
                               DTE=eDTE(),sig=endPriceIV(),r=i_rate(),div=div())
         }
       }
       # print(paste("Gamma:",gamma))
       gamma
     })


     theta = shiny::reactive({
       theta=0
       if (input$Type != "Stock") {
         shiny::req(input$Strike)
         if ((p_u_price() != "") & (p_days() != "")) {
           # print(paste("theta BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
           #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
           theta=getBSOptTheta(type=input$Type,S=p_u_price(),K=strike(),
                               DTE=eDTE(),sig=endPriceIV(),r=i_rate(),div=div())
         }
       }
       # print(paste("theta:",theta))
       theta
     })


     vega = shiny::reactive({
       vega=0
       if (input$Type != "Stock") {
         shiny::req(input$Strike)
         if ((p_u_price() != "") & (p_days() != "")) {
           # print(paste("vega BS Computation:",input$Type,"S:",p_u_price(),"K:",strike(),
           #             "eDTE:",eDTE(),"Vol:",endPriceIV(),"r:",i_rate()))
           vega=getBSOptVega(type=input$Type,S=p_u_price(),K=strike(),
                               DTE=eDTE(),sig=endPriceIV(),r=i_rate(),div=div())
         }
       }
       # print(paste("vega:",vega))
       vega
     })

     output$start_price = shiny::renderText({
       shiny::req(input$Pricing)
       if ((input$Pricing == "Black-Scholes") || (input$Pricing == "Bjerksund-Stensland")) paste0("Start Price: ", round(startPrice(),2))
       else if (input$Pricing == "Manual")  paste0("Start Price IV: ",round(startPriceIV()*100,2),"%")
       else paste0("Start Price: ", round(startPrice(),2),"  Start Price IV: ",round(startPriceIV()*100,2),"%")
     })

     output$end_price = shiny::renderText(paste0("End Price: ", round(endPrice(),2)))

    return(shiny::reactive({
      stopifnot(is.numeric(c(mul(),i_rate(),u_price(),p_days(),p_u_price())))

      ### build_line 14 arguments: type="", sym="", strike=0, expdate="",
      ####                         DTE=NA, startPrice=0, endPrice=0, u_price=0, p_u_price=0,
      ####                         startPriceIV=0, endPriceIV=0, pos=0, mul=NA, delta=NA
      ### No required argument

      switch(input$Type,
             "Stock" = build_line(type="Stock", sym=shiny::req(sym()), startPrice=u_price(), endPrice=p_u_price(),
                                  pos=shiny::req(input$Qty), mul=1, delta=input$Qty,
                                  deltanotional=input$Qty*p_u_price()),
             "Put"=,
             "Call"= {
                        build_line(type=input$Type, sym=shiny::req(sym()), strike=shiny::req(strike()),
                                   expdate=format(shiny::req(expDate()),"%d-%b-%Y"),
                        DTE=eDTE(), startPrice=startPrice(), endPrice=endPrice(),
                        startPriceIV=startPriceIV(), endPriceIV=endPriceIV(),
                        pos=shiny::req(input$Qty), mul=mul(),
                        delta=delta(), deltanotional=mul()*delta()*input$Qty*p_u_price(),
                        gamma=gamma(),
                        theta=theta(),
                        vega=vega()
                        )
                      },
             build_line()) ## Default value
    }))
  })
}

##print("Ligne module loaded!")

ligneDemoApp <- function() {
  ##### List of  default expiration dates values
  weekly_dates=c(lubridate::ymd("2024-01-19"),lubridate::ymd("2024-01-26"),
                 lubridate::ymd("2024-02-02"),lubridate::ymd("2024-02-09"),lubridate::ymd("2024-02-16"),lubridate::ymd("2024-02-23")
  )


  monthly_dates=c(lubridate::ymd("2024-03-15"),lubridate::ymd("2024-04-19"),lubridate::ymd("2024-05-17"),lubridate::ymd("2024-06-21"),lubridate::ymd("2024-07-19"),
                  lubridate::ymd("2024-08-16"),lubridate::ymd("2024-09-20"),lubridate::ymd("2024-10-18"),lubridate::ymd("2024-11-15"),lubridate::ymd("2024-12-20"),
                  lubridate::ymd("2025-01-17"), lubridate::ymd("2025-03-21"), lubridate::ymd("2025-06-20"), lubridate::ymd("2025-09-19"), lubridate::ymd("2025-12-19"),
                  lubridate::ymd("2026-01-16"))
  dates_list=c(weekly_dates, monthly_dates)

  ui = shiny::fluidPage(
    shiny::h1("Ligne Demo"),
    shiny::h3(shiny::textOutput("title")),
    shiny::textInput("sym","Symbol: ",value=""),
    shiny::numericInput("mul","Multiplier: ",value=100),
    shiny::selectInput("currency","Currency: ",choices=c("USD","EUR","CHF"),selected="USD"),
    shiny::selectInput("exchange","Exchange: ",choices=c("SMART","EUREX","CBOE"),selected="SMART"),
    shiny::textInput("tradingClass","Trading Class: ",value=""),
    shiny::numericInput("strikes","Strike: ",value=100),
    shiny::numericInput("u_price","Current underlying price:",value=100),
    shiny::numericInput("p_u_price","Projected price:",value=100),
    #shiny::numericInput("div","Dividend yield (%):",value=0),
    shiny::numericInput("p_days","Number of days for projected price:",value=10),
    shiny::hr(),
    ligneUI("Ligne"),
    shiny::tableOutput("table")
  )

  server = function(input, output, session) {
    ligne=ligneServer(id="Ligne",sym=shiny::reactive(input$sym),mul=shiny::reactive(input$mul),
                      s_datetime=shiny::reactive(lubridate::now()),
                      i_rate=shiny::reactive(0.04),
                      u_price=shiny::reactive(input$u_price),
                      div=shiny::reactive(0),
                      p_days=shiny::reactive(input$p_days),
                      p_u_price=shiny::reactive(input$p_u_price),
                      currency=shiny::reactive(input$currency),
                      exchange=shiny::reactive(input$exchange),
                      tradingClass = shiny::reactive(input$tradingClass),
                      exp_dates=shiny::reactive(format(dates_list,"%d %b %Y")),
                      strikes=shiny::reactive(input$strikes)
                      #strikes=reactive(seq(150,170,2))
                      )

    output$table=shiny::renderTable(ligne())
    output$title=shiny::renderText({paste0(input$sym," current price=",input$u_price,",
                                           projected price=",input$p_u_price," in ",
                                    input$p_days," days, div yield= 0 interest_rate=4%")})

  }
  shiny::shinyApp(ui, server)
}


##ligneDemoApp()

ligneApp = function(interest_rate=0.04) {
  weekly_dates=c(lubridate::ymd("2024-01-19"),lubridate::ymd("2024-01-26"),
                 lubridate::ymd("2024-02-02"),lubridate::ymd("2024-02-09"),lubridate::ymd("2024-02-16"),lubridate::ymd("2024-02-23")
  )


  monthly_dates=c(lubridate::ymd("2024-03-15"),lubridate::ymd("2024-04-19"),lubridate::ymd("2024-05-17"),lubridate::ymd("2024-06-21"),lubridate::ymd("2024-07-19"),
                  lubridate::ymd("2024-08-16"),lubridate::ymd("2024-09-20"),lubridate::ymd("2024-10-18"),lubridate::ymd("2024-11-15"),lubridate::ymd("2024-12-20"),
                  lubridate::ymd("2025-01-17"), lubridate::ymd("2025-03-21"), lubridate::ymd("2025-06-20"), lubridate::ymd("2025-09-19"), lubridate::ymd("2025-12-19"),
                  lubridate::ymd("2026-01-16"))
  dates_list=c(weekly_dates, monthly_dates)

  ui = shiny::fluidPage(
    shiny::h1("Ligne Demo"),
    shiny::h3(shiny::textOutput("title")),
    shiny::textInput("sym","Symbol: ",value=""),
    shiny::numericInput("mul","Multiplier: ",value=100),
    shiny::selectInput("currency","Currency: ",choices=c("USD","EUR","CHF"),selected="USD"),
    shiny::selectInput("exchange","Exchange: ",choices=c("SMART","EUREX","CBOE"),selected="SMART"),
    shiny::textInput("tradingClass","Trading Class: ",value=""),
    shiny::numericInput("strikes","Strike: ",value=100),
    shiny::numericInput("u_price","Current underlying price:",value=100),
    #shiny::numericInput("div","Dividend yield (%):",value=0),
    shiny::hr(),
    ligneUI("Ligne"),
    shiny::tableOutput("table")
  )

  server = function(input, output, session, interest_rate) {
    ligne=ligneServer(id="Ligne",sym=shiny::reactive(input$sym),
                      mul=shiny::reactive(input$mul),
                      s_datetime=shiny::reactive(lubridate::now()),
                      i_rate=shiny::reactive(interest_rate),
                      u_price=shiny::reactive(input$u_price),
                      div=shiny::reactive(0),
                      p_days=shiny::reactive(0),
                      p_u_price=u_price,
                      currency=shiny::reactive(input$currency),
                      exchange=shiny::reactive(input$exchange),
                      tradingClass = shiny::reactive(input$tradingClass),
                      exp_dates=shiny::reactive(format(dates_list,"%d %b %Y")),
                      strikes=shiny::reactive(input$strikes)
                      #strikes=reactive(seq(150,170,2))
    )

    output$table=shiny::renderTable(ligne())
    output$title=shiny::renderText({paste0(input$sym," current price=",input$u_price,",
                                           projected price=",input$p_u_price," in ",
                                           input$p_days," days, div yield= 0 interest_rate=4%")})

  }
  shiny::shinyApp(ui, server)
}
