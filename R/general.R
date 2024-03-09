#### 04.09.2023
#### For all programs
###############   General purpose utility programs  ###################

# library(dplyr)
# library(readr)
# library(scales) #### for label_percent() and label_dollar() function
# library(reticulate)
# py_run_file(config::get("PyContractValue"))
# print("Python functions loaded!")

###library(DescTools) ### for pgcd
###library(quantmod) ## to retrieve data from Yahoo - getSymbols
###library(stringr) ### for str_to_upper function
##library(RQuantLib) ## for isBusinessDay function
### library(lubridate) ### for today() function



##########################  General purposes utilities ##########
#'   Display message
#'
#'Within any Shiny app, this function allows to display an information message
#'The end user then has to click OK button
#'The window is modal
#'This function cannot be used outside a Shiny app
#'@param msg string to be displayed. Default to empty string
#'@returns NULL
#'@keywords Shiny
#'@export
#'@examples
#'display_message("Pi value is 3.14")

display_message = function(msg) {
  if (shiny::isRunning()) {
    shiny::showModal(shiny::modalDialog(
      title = "Message",
      msg,
      easyClose = TRUE,
      footer = NULL
    ))
  }
  else message(msg)
}

#'   Display error message
#'
#'Within any Shiny app, this function allows to display an error message
#'The end user then has to click OK button
#'The window is modal
#'This function cannot be used outside a Shiny app
#'@param error_msg string to be displayed. Default to empty string
#'@returns NULL
#'@keywords error Shiny
#'@export
#'@examples
#'display_error_message("You must provide a valid integer!")
#'display_error_message("No date available!")

display_error_message = function(error_msg) {
  if (shiny::isRunning()) {
    shiny::showModal(shiny::modalDialog(
      title = "Error message",
      error_msg,
      easyClose = TRUE,
      footer = NULL
    ))
  }
  else message("Error message: ",error_msg)
}

###' Enter numerical data
###'
###' This very simple code piece is intended to enter one numerical value and then returns it.
###' This is to be used whenever additional data must be entered that was not initially foreseen to be entered
###' (e.g. due to lack of IBKR connection).
###'
###' There are 2 cases:
###' * R is in interactive mode (e.g. from console)
###' * R is called from batch.
###' These 2 sub-cases are handled with different function calls (\code{readline} vs \code{readLines})
###' @param sym This is the name data to be filled in by end user
###' @returns numerical input by end user, if cannot be coerced to a number then NA
###' @export
enter_numerical_data = function(sym) {
    print(paste0("Enter value for ",sym))

    if (interactive()) val=readline(prompt="(interactive) ")
    else val= readLines(con="stdin", n=1)[[1]]
    return(suppressWarnings(as.double(val)))
}

#'   pgcd
#'
#'"plus grand commun diviseur"
#'GCD function from DescTools has several limitations that are addressed in this package:
#'- when there is only one value, GCD raises an error
#'- GCD does not know how to handle NA
#'@param x is a vector of integers, may include NA
#'@return numeric GCD
#'@keywords arithmetic GCD
#'@export
#'@examples
#'pgcd(c(6,4))  ### Should be equal to 2
#'pgcd(c(6,4,NA))  ### Should be equal to 2
#'pgcd(5) ### Equal to 5

pgcd = function(x) {
  x= x[!is.na(x)]
  ### This test covers both cases where length x =1 and length x=0
  if (length(x)<2) return(as.numeric(x))
  ### Small hack as is.integer does not return T for numeric that are integers
  if (!isTRUE(all.equal(x, as.integer(x)))) {
    display_error_message("All position numbers must be integers! Will use 1 as PGCD")
    return(1)
  }
  else return(DescTools::GCD(as.numeric(x)))
}

#'   change
#'
#'Computes the relative change between x and y, x is start value, y end value.
#'
#'x and y may be negative, but if x = 0, then change returns NA (float)
#'x and y may be vectors of numbers
#'@param x is a vector of integers, cannot include NA
#'@param y is a vector of integers, cannot include NA#
#'@return numeric number
#'@export
#'@examples
#'change(1,1.1)  ### Should be equal to 0.1
#'change(1,1)  ### Should be equal to 0
#'change(-1,-1.1)  ### Should be equal to -0.1
#'change(0,1) ## NA
#'change(-1,-0.9)  ### Should be equal to 0.1

change = function(x,y) {
  stopifnot(is.numeric(x),is.numeric(y))
  dplyr::if_else (
    x==0,
    NA_real_,
    sign(x)*(y-x)/x
  )
}



#'   reformat
#'
#' reformat function is to be used to convert a CSV file
#' separated by commas into a file CSV separated by semi-columns
#' The new file has an extension csv2 and the same name
#'@param file is the input CSV file
#'@keywords csv file locale
#'@export

reformat = function(file) {
  rootfile= unlist(strsplit(file, ".csv",fixed=TRUE))
  new_file=paste0(rootfile,".csv2")
  content=utils::read.delim(file,check.names = TRUE,sep=",")
  utils::write.table(content,new_file,sep=";",row.names=F,fileEncoding = "UTF-8")
}

#'   findNearestNumberOrDate
#'
#' Given a list of numbers or dates,
#' The aim is to find the nearest number or date within the list compared with target
#' The list does not need to be ordered
#' This function will proceed by using which.min applied to list - target
#'@param values_list is a list of numbers or dates
#'@param target is a date or a number
#'@keywords dates numbers minimum
#'@export
#'@examples
#'findNearestNumberOrDate(c(1,4,5),3)

findNearestNumberOrDate = function(values_list, target) {
  # nearest = numbers[1]
  # diff = abs(as.numeric(nearest - target))
  #
  # ### Without as.list this for loop does not work
  # for (number in as.list(numbers)) {
  #   current_diff = abs(as.numeric(number - target))
  #   if (current_diff < diff) {
  #     diff = current_diff
  #     nearest = number
  #   }
  #  }

  nearest_index=which.min(abs(as.numeric(values_list-target)))
  return(values_list[nearest_index])
}

##### Utilities to work on Trades.csv file or options (getDTE)  ###################
#### Utilities - no real relation with options computation

#'   getDTE
#'
#' getDTE returns the number of days (in decimal format) between a given datetime and a list (or one) of
#' expiration dates.
#'
#' Expiration dates will all be converted into a datetime by appending 4:00pm EST to them.
#' The interval between both is then converted to a duration (seconds) and converted back in days
#' by dividing by 24*3600
#'@param c_datetime is the current date&time
#'@param expdate is the expiration date (at 4:00pm)
#'@keywords date options Black-Scholes
#'@export
#'@examples
#'getDTE(as.Date("2023-12-10"),as.Date("2023-12-15"))
#'getDTE(as.Date("2023-12-10"),c(as.Date("2023-12-10"),as.Date("2023-12-15")))


getDTE = function(c_datetime,expdate) {
  dplyr::if_else(is.na(expdate),NA, {
    exp_datetime=lubridate::as_datetime(expdate)
    lubridate::hour(exp_datetime) = 16  ### Option end is at 16:00 EST usually 22:00 CET
    lubridate::tz(exp_datetime) = "EST"

    ### If difftime i.e. DTE is negative then call and put prices are equal to 0 - delta equal to 0
    ### This is not tested and ought to be tested by the option pricing model
    difftime=as.numeric(lubridate::interval(c_datetime,exp_datetime))

    difftime=as.numeric(lubridate::as.duration(difftime)) ### Convert to duration in seconds and then to a number
    difftime=round(difftime/(24*3600),2) ### and then convert duration from seconds duration to days duration - keep only 2 digits

    difftime
  })
}

##### Utilities to work on Trades.csv file or options (getDTE)  ###################
#### Utilities - no real relation with options computation

#'   buildInstrumentName
#'
#' Objective is to build a string corresponding to a specific option (using IBKR TWS).
#' The string can then be searched in any file generated by IBKR website.
#' This function performs some checks (for instance is expdate a date, is type a valid type)
#' but no extensive checks are performed.
#' <br> If type is unknown (neither Put nor Call nor P nor C) then sym is returned.
#'@param sym is the underlying name (e.g. SPY)
#'@param expdate is the expiration date (e.g. Dec 15, 2023)
#'@param strike is the strike number (e.g. 400)
#'@param type is character vector - should be one of the following: "P", "C", "Put", "Call"
#'@keywords date options Black-Scholes
#'@export
#'@examples
#'buildInstrumentName("SPY",as.Date("2023-12-15"),400.0,"Put")

buildInstrumentName=function(sym,expdate=as.Date("2023-01-01"),strike=100,type) {
  ### If type is neither P,C, Put or Call then returns sym name
  ### Both False and True outcomes will be run in any case
  ### - expdate and strike values must be initialized
  dplyr::if_else(!(type %in% c("P","C","Put","Call")), sym, {
    #### expdate must be a date
    stopifnot("expdate must be a date!" = lubridate::is.Date(expdate),
              "strike must be convertible to a float" = is.numeric(as.numeric(strike)))

    ## In case type already equals P or C, keep current value - otherwise replace by P for Put and C for Call
    right = dplyr::case_match(type, "Put" ~"P","Call" ~"C",.default = type)

    #### Instrument string is built using English - i.e. non locale date names
    #### This is necessary as Instrument name must follow IBKR format (to match strings in Trades.csv file)
    #### Apply locale used by IBKR to build instrument expiration date
    expdate= withr::with_locale(new = c("LC_TIME" = "English"),
                                code = stringr::str_to_upper(format(expdate,"%d%b%y")))
    paste(sym,expdate,strike,right)
  })
}
