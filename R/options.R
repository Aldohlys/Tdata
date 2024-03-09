#### General purpose options library
#### Alexis Martin 16.05.2023
####
####
#### getBSCallPrice : get BS price for Call option
#### getBSPutPrice : get BS price for Put option
#### getBSOptPrice : get BS price for any option
#### getBSOptDelta : get BS price for any option or 1 for stock
#### getVol : get implied vol given a price, a price function

#library(derivmkts) ### for basic bsput, bscall and Greeks computation
# library(quantmod) ### To retrieve stock quotes from Yahoo src
# library(lubridate) ### to manage dates and time
# library(dplyr) ### Data manipulation
##library(purrr) ### Functional programing (e.g. pmap)
##library(tidyr)

############################## Black-Scholes computation ##################
### https://www.theglobeandmail.com/investing/markets/commodities/GE*1/

##### https://rdocumentation.org/packages/derivmkts/versions/0.2.5
##########  Black Scholes option pricing model
# !!!! bscall(s=410,k=410,v=0.17,r=0,tt=0,d=0) returns NaN - basically not possible to price the call -> equals 0
# Same issue for bsput
### If missing r then r= interest_rate, missing div then div= 0

######### getBSCallPrice ######################
#'  getBSCallPrice
#'
#' This function supplies an option price based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#' d1 <- (log(S/K) + (r + sig^2/2) x T) / (sig x sqrt(T))
#'
#' d2 <- d1 - sig x sqrt(T)
#' value <- S x pnorm(d1) - K x exp(-r*T) x pnorm(d2)
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSCallPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSCallPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export

getBSCallPrice <- function(S, K, r=as.numeric(config::get("interest_rate")), DTE, sig, div=0){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) {
    message(c(as.list(environment())))
    stop("getBSCallPrice: one of arguments missing!!")
  }
  # d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
  # d2 <- d1 - sig*sqrt(T)
  # value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  ## Handle special cases not handled correctly by bsput/bscall
  dplyr::if_else( ((DTE==0)&(S==K) | (DTE<0) | ((S==0)&(K==0))), 0,
           {
        T=DTE/365
        derivmkts::bscall(s=S, k=K, v=sig, r=r, tt=T, d=div)})
}

########### getBSPutPrice ###########
#'  getBSPutPrice
#'
#' This function supplies an option price based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#' d1 <- (log(S/K) + (r + sig^2/2) x T) / (sig x sqrt(T))
#'
#' d2 <- d1 - sig*sqrt(T)
#'
#' value <-  (K x exp(-rT) x pnorm(-d2) - S x pnorm(-d1))
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export

getBSPutPrice <- function(S, K, r=as.numeric(config::get("interest_rate")), DTE, sig, div=0){
  if(any(missing(S), missing(K), missing(DTE),missing(sig))) stop("getBSPutPrice: one of arguments missing!!")

  ## Handle special cases not handled correctly by bsput/bscall
  dplyr::if_else( ((DTE==0)&(S==K) | (DTE<0) | ((S==0)&(K==0))), 0,
           {
             T=DTE/365
             derivmkts::bsput(s=S, k=K, v=sig, r=r, tt=T, d=div)})
}

########### Straddle Price Put + Call
#'  getBSStraddlePrice
#'
#' This function supplies an option price based upon Black-Scholes-Merton model
#'
#' It merely sums call price + put price (getBSCallPrice and getBSPutPrice)
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSStraddlePrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSStraddlePrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export

getBSStraddlePrice = function(S,K,r=as.numeric(config::get("interest_rate")),DTE,sig,div=0) {
  getBSCallPrice(S,K,r,DTE,sig,div)+getBSPutPrice(S,K,r,DTE,sig,div)
}

### getBSOptPrice ##############
#'  getBSOptPrice
#'
#' This function supplies an option price based upon Black-Scholes-Merton model.
#'
#' It is a wrapper that calls getBSPutPrice or getBSCalPrice depending upon type value.
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, else NA is returned.
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div  annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export

getBSOptPrice = function(type,S,K,r=as.numeric(config::get("interest_rate")),DTE,sig,div=0) {
  ##print(paste("getBSOptPrice Type:",type," S:",S," K:",K," r:",r," DTE:",DTE," sig:",sig, "div:",div))
  dplyr::if_else(type=="Put", getBSPutPrice(S, K, r, DTE, sig, div),
                 dplyr::if_else(type=="Call", getBSCallPrice(S, K, r, DTE, sig, div), NA))
}

### getBSPrice ##############
#'  getBSPrice
#'
#' This function supplies a contract price, i.e. either an option price x multiplier
#' or a stock price. The option price is based upon Black-Scholes-Merton model or a stock price.
#'
#' This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns underlying price, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param mul contract multiplier - generally 100 - but equal to 10 for ESTX OESX option chains
#'@param div  annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSPutPrice(S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSPutPrice(S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export
getBSPrice = function(type,S,K,r=as.numeric(config::get("interest_rate")),DTE,sig,mul,div=0) {
  ##print(paste("getBSOptPrice Type:",type," S:",S," K:",K," r:",r," DTE:",DTE," sig:",sig, "div:",div))
  dplyr::if_else(type=="Put", mul*getBSPutPrice(S, K, r, DTE, sig, div),
                 dplyr::if_else(type=="Call", mul*getBSCallPrice(S, K, r, DTE, sig, div),
                                dplyr::if_else(type=="Stock", S, NA)))
}

### Be aware that combo price is divided by PGCD of positions!!!!
### getBSComboPrice ##############
#'  getBSComboPrice
#'
#' This function supplies a combo price, i.e. the sum of all contract prices multiplied by their ratioed
#' positions within the combo (leg price). This sum is then divided by PGCD of all leg prices.
#'
#' This ratioed price is obtained in 2 steps:
#' 1/ compute each leg's price by using \code{getBSPrice} function, i.e. multiplying their respective price by multiplier- and then multiplying the result of \code{getBSPrice} by position.
#' 2/ dividing each leg price by the PGCD of the total
#' The contract price of each leg is based upon Black-Scholes-Merton model or a stock price.
#' Multiplier has to be provided for each leg, in case there is a stock into the combo, the multiplier will make sure
#' each leg has the correct weight into the combo price.
#'
#'Regarding implied volatility (\code{sig} parameter) it may be included in the \code{data} parameter - internal IV
#'or supplied as individual parameter - external IV. In the latter case, the same IV will be applied to each leg,
#'provided it is an option of course.
#'
#' This function can be vectorized over \code{S} parameter, e.g. for providing a graph over multiple value of S.
#' Notice that it in this case the set of \code{S} parameters must be _sorted_, because these parameters will be used by \code{getBSComboPrice}
#' for grouping data, before executing \code{summarize} function.
#'
#' Last, through getBSPrice, it handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE=0 and S=K -> option price = 0
#' - DTE < 0 -> option price = 0
#' - S=K=0 -> option price = 0
#'@param data a data frame containing the mandatory following fields: pos,type,strike,DTE,mul + sig
#'@param S Spot price or current underlying price - mandatory parameter
#'@param r Interest rate - default value is defined in config.yml file
#'@param sig annualized volatility - optional parameter, may also be contained within \code{data} parameter
#'@param div  annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSComboPrice(data=cbind(data.frame(pos=c(-2,2),type=c("Put","Put"),
#'strike=c(100,95),DTE=c(5,10)),sig=0.3,r=0.05035,mul=100),S=100)
#'getBSComboPrice(data=cbind(data.frame(pos=c(200,2),type=c("Stock","Put"),
#'strike=c(NA,127),DTE=c(NA,18),sig=c(0,0.37),mul=c(1,100)),r=0.05,div=0),S=130)
#'@export
getBSComboPrice = function(data,S, r=as.numeric(config::get("interest_rate")), sig,div=0) {

  ######  data contains pos type strike DTE / sig is either in data, or external to data
  ###### - S is external to data
  if (any(missing(data),missing(S)) | (missing(sig) & !("sig" %in% names(data)))) {
    display_error_message("Missing arguments in getBSComboPrice!!")
    return(NA)
  }

  if (is.unsorted(S)) {
    display_error_message("Underlying prices list must be sorted !")
    return(NA)
  }

  ### Case where sig is to be found in data dataframe, as absent from command line
  if (missing(sig)) {
    #### This step is necessary when dealing with vectorized input data containing multiple S underlying prices
    #### However each S value will be applied to a single combo - combo cannot be vectorized
    data= dplyr::group_by(tidyr::crossing(data,S=S),S)
    price= dplyr::summarize(data, price=sum(pos*getBSPrice(type=type,S=S,K=strike,r=r,DTE=DTE,mul=mul,sig=sig))/pgcd(pos))
  }

  else {
    if (is.unsorted(sig)) {
      display_error_message("Implied vol lists must be sorted")
      return(NA)
    }
    ### In case sig also present in the data frame
    data$sig=NULL
    #### This step is necessary when dealing with vectorized input data containing multiple S underlying prices
    data= dplyr::group_by(tidyr::crossing(data,S=S,sig=sig),S,sig)
    price= dplyr::summarize(data, price=sum(pos*getBSPrice(type=type,S=S,K=strike,r=r,DTE=DTE,mul=mul,sig=sig))/pgcd(pos))
  }
  return(price$price)
}



#######  getBSOptDelta ###############
#'  getBSOptDelta
#'
#' This function supplies delta value of an option based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE = 0 -> delta = 0
#' - DTE < 0 -> delta = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSOptDelta(type="Call",S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSOptDelta(type="Put",S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export
getBSOptDelta <- function(type,S,K,DTE,sig,r=as.numeric(config::get("interest_rate")),div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  ### Test first that DTE exists (Stock case)

  dplyr::if_else (type=="Stock", 1, {
    dplyr::if_else (DTE<=0, 0, {
      T=DTE/365
      ##print(paste("getBSOptDelta: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
      as.numeric(dplyr::if_else(type=="Put", derivmkts::bsopt(S,K,sig,r,T,div)$Put["Delta",],
               derivmkts::bsopt(S,K,sig,r,T,div)$Call["Delta",]))
    })
  })
}

#######  getBSOptVega ###############
#'  getBSOptVega
#'
#' This function supplies vega value of an option based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE = 0 -> Vega = 0
#' - DTE < 0 -> Vega = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSOptVega(type="Call",S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSOptVega(type="Put",S=220,K=225,r=0.04,DTE=500,sig=0.3,div=0)
#'getBSOptVega(type="Put",S=220,K=247,r=0.04,DTE=700,sig=0.2,div=0)
#'getBSOptVega(type="Put",S=22,K=22.5,r=0.04,DTE=500,sig=0.3,div=0)
#'@export
getBSOptVega <- function(type,S,K,DTE,sig,r=as.numeric(config::get("interest_rate")),div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  ### Test first that DTE exists (Stock case)

  dplyr::if_else (type=="Stock", 1, {
    dplyr::if_else (DTE<=0, 0, {
      T=DTE/365
      ##print(paste("getBSOptVega: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
      dplyr::if_else(type=="Put", derivmkts::bsopt(S,K,sig,r,T,div)$Put["Vega",],
                     derivmkts::bsopt(S,K,sig,r,T,div)$Call["Vega",])
    })
  })
}

#######  getBSOptTheta ###############
#'  getBSOptTheta
#'
#' This function supplies theta value of an option based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE = 0 -> Theta = 0
#' - DTE < 0 -> Theta = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSOptTheta(type="Call",S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSOptTheta(type="Put",S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export
getBSOptTheta <- function(type,S,K,DTE,sig,r=as.numeric(config::get("interest_rate")),div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  ### Test first that DTE exists (Stock case)

  dplyr::if_else (type=="Stock", 1, {
    dplyr::if_else (DTE<=0, 0, {
      T=DTE/365
      ##print(paste("getBSOptTheta: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
      dplyr::if_else(type=="Put", derivmkts::bsopt(S,K,sig,r,T,div)$Put["Theta",],
                     derivmkts::bsopt(S,K,sig,r,T,div)$Call["Theta",])
    })
  })
}

#######  getBSOptGamma ###############
#'  getBSOptGamma
#'
#' This function supplies gamma value of an option based upon Black-Scholes-Merton model
#'
#' It performs the computation as per textbooks using derivmkts package:
#'
#'This function can be vectorized. It handles also special cases not handled correctly by bsput/bscall, such as:
#' - DTE = 0 -> Gamma = 0
#' - DTE < 0 -> Gamma = 0
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param sig annualized volatility
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@keywords Black-Scholes trading
#'@examples
#'getBSOptGamma(type="Call",S=100,K=100,r=0.04,DTE=5,sig=0.25,div=0)
#'getBSOptGamma(type="Put",S=22,K=22.5,r=0.04,DTE=5,sig=0.3,div=0)
#'@export
getBSOptGamma <- function(type,S,K,DTE,sig,r=as.numeric(config::get("interest_rate")),div=0){
  ## Handle special cases not handled correctly by bsput/bscall
  ### DTE=0 cannot be handled correctly
  ### Test first that DTE exists (Stock case)

  dplyr::if_else (type=="Stock", 1, {
    dplyr::if_else (DTE<=0, 0, {
      T=DTE/365
      ##print(paste("getBSOptGamma: argts S:",S," K:",K," sig:",sig," r:",r," T:",T," div:",div))
      dplyr::if_else(type=="Put", derivmkts::bsopt(S,K,sig,r,T,div)$Put["Gamma",],
                     derivmkts::bsopt(S,K,sig,r,T,div)$Call["Gamma",])
    })
  })
}

#### getVol Implied vol computation #########################
#### Also valid for different combo options
### getVol deduces implied volatility from an option price (called price)

### TO do this it computes option prices with a series of volatility numbers (starting from 400%)
#### THen dochotomia algorithm : this works ONLY if fPrice is monotonous
#### In other cases, getVol2 (brute force) must be used instead
getVol = function(price,min_vol=0,max_vol=2,iter=0,fPrice,...) {
  if (missing(price)) return(NA)
  if (iter>20) return(NA)
  sig=(min_vol+max_vol)/2
  cur_price=fPrice(...,sig=sig)
  ##message("Iter: ",iter," cur_price: ",cur_price, " min_vol: ",min_vol," max_vol: ",max_vol)
  if (is.na(cur_price)) return(NA)
  if (abs(sig-max_vol)<0.0001) {
    #  message("Iter: ",iter,"cur_price: ",cur_price, "min_vol: ",min_vol,"max_vol: ",max_vol)
      round(sig,3)
  }
  else {
      if (sign(price*(cur_price-price))<0)  getVol(price=price,min_vol=sig,max_vol=max_vol,iter=iter+1,fPrice,...)
      else getVol(price=price,min_vol=min_vol,max_vol=sig,iter=iter+1,fPrice,...)
  }
}


#### getIR Implied interest rate computation #########################
#### Also valid for different combo options
### getIR deduces interest rate from an option price (called price)

### TO do this it computes option prices with a series of interest rates (between 0 and 10%)
#### Then dochotomia algorithm : this works ONLY if fPrice is monotonous
getIR = function(price,min_ir=0,max_ir=0.1,iter=0,fPrice,...) {
  if (missing(price)) return(NA)
  if (iter>20) return(NA)
  ir=(min_ir+max_ir)/2
  cur_price=fPrice(...,r=ir)
  ##message("Iter: ",iter," cur_price: ",cur_price, " min_vol: ",min_vol," max_vol: ",max_vol)
  if (is.na(cur_price)) return(NA)
  if (abs(ir-max_ir)<0.0001) {
    #  message("Iter: ",iter,"cur_price: ",cur_price, "min_vol: ",min_vol,"max_vol: ",max_vol)
    round(ir,3)
  }
  else {
    if (sign(price*(cur_price-price))<0)  getIR(price=price,min_ir=ir,max_ir=max_ir,iter=iter+1,fPrice,...)
    else getIR(price=price,min_ir=min_ir,max_ir=ir,iter=iter+1,fPrice,...)
  }
}

#######  getImpliedVolOpt ###############
#'  getImpliedVolOpt
#'
#' This function computes the implied volatility of a give price, based upon Black-Scholes-Merton model
#'
#' It performs the computation using dichotomia algorithm. As such this requires that the price function is monotonous. Otherwise a brute force algorithm must be used.
#'
#'This function cannot be vectorized.
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@param price option price
#'@keywords Black-Scholes volatility trading
#'@examples
#'getImpliedVolOpt(type="Call",S=100,K=100,r=0.04,DTE=5,price=0.85)
#'getImpliedVolOpt(type="Put",S=22,K=22.5,r=0.04,DTE=5,price=1.1)
#'@export
getImpliedVolOpt = function(type,S,K,r=as.numeric(config::get("interest_rate")),DTE,div=0,price) {
  dplyr::if_else(is.na(price),NA,
  switch(type,
    "Stock" = 0,
    "Put" = getVol(price=price,fPrice=getBSPutPrice,S=S,K=K,r=r,DTE=DTE,div=div),
    "Call"=getVol(price=price,fPrice=getBSCallPrice,S=S,K=K,r=r,DTE=DTE,div=div)
  ))
}


#######  getImpliedIROpt ###############
#'  getImpliedIROpt
#'
#' This function computes the implied interest rate of a given price, based upon Black-Scholes-Merton model
#'
#' It performs the computation using dichotomia algorithm.
#' As such this requires that the price function is monotonous. Otherwise a brute force algorithm must be used.
#'
#'This function cannot be vectorized.
#'@param type a string, if equal to "Put" then getBSPutPrice is called, if equal to "Call" getBSCallPrice is called, if equal to "Stock" returns 1, else returns NA
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param sig Implied Volatility
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@param price option price
#'@keywords Black-Scholes volatility trading
#'@examples
#'getImpliedIROpt("Call",S=100,K=100,DTE=5,sig=0.25,div=0,price=1.19455)
#'getImpliedIROpt(type="Put",S=22,K=22.5,sig=0.4,DTE=5,price=1.1)
#'@export
getImpliedIROpt = function(type,S,K,sig,DTE,div=0,price) {
  dplyr::if_else(is.na(price),NA,
                 switch(type,
                        "Stock" = NA,
                        "Put" = getIR(price=price,fPrice=getBSPutPrice,S=S,K=K,sig=sig,DTE=DTE,div=div),
                        "Call"=getIR(price=price,fPrice=getBSCallPrice,S=S,K=K,sig=sig,DTE=DTE,div=div)
                 ))
}

#######  getImpliedVolStraddle ###############
#'  getImpliedVolStraddle
#'
#' This function computes the implied volatility of a straddle, based upon Black-Scholes-Merton model
#'
#' It performs the computation using \code{getImpliedVolOpt} function. As 30 days at-the-money option, this can be utilized as the standard implied volatility of the underlying.
#'
#'This function cannot be vectorized.
#'@param S Spot price or current underlying price
#'@param K Strike price
#'@param r Interest rate - default value is defined in config.yml file
#'@param DTE Number of days to expiration (may be a decimal number - useful as expiration may happen at 4:00pm or in the morning (index case))
#'@param div annualized dividend yield (i.e. sum of present values of upcoming dividends over 1 year divided by current spot price). Default value is 0.
#'@param price straddle price
#'@keywords Black-Scholes volatility trading
#'@examples
#'getImpliedVolStraddle(S=100,K=100,r=0.04,DTE=30,price=3.85)
#'getImpliedVolStraddle(S=22,K=22.5,r=0.04,DTE=30,price=1.1)
#'@export
getImpliedVolStraddle = function(S,K,r=as.numeric(config::get("interest_rate")),DTE,div=0,price) {
  getVol(price=price,fPrice=getBSStraddlePrice,S=S,K=K,r=r,DTE=DTE,div=div)
}

getImpliedVolCombo = function(S,data,r=as.numeric(config::get("interest_rate")),div=0,price) {
  getVol2(price=price,fPrice=getBSComboPrice,data=data,S=S,r=r,div=div)
}

### getVol2 requires fPrice to be vectorized, i.e. a vecotr of volatilities can be passed on fPrice
getVol2 = function(price,fPrice,...) {
  if (missing(price)) return(NA)
  sample_size=0.5  ### 4 Slices of 50% each, from 0 to 200%
  sample_increment=0.0001 ### to get volatility at 0.05% precision - 5'000 samples each time
  for (sig1 in seq(from=0,to=2,by=sample_size)) {
    ### Compute prices_list as a result of fPrice applied to a range of vol values
    ### from sig1 till sig1+sample_size
    prices_list=fPrice(...,sig=seq(sig1,sig1+sample_size,by=sample_increment))
    min_price_index=which.min(abs(prices_list-price))
    min_price=prices_list[min_price_index]
    ### If min_price is near enough price then returns vol at 0.01% precision
    if (abs(min_price-price)<0.001) {
      vol=sig1+(min_price_index-1)*sample_increment
      ### Limit to xx.y% format
      return(round(vol,3))
    }
  }
  ### There was no vol value that fit between 0 and 200%
  display_error_message("Could not compute a volatility from price function !!")
}

# getImpliedVolOpt2 = function(df) {
#   return(unlist(purrr::pmap(df,getImpliedVolOpt)))
# }

