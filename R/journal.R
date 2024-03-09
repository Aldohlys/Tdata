#### Journal module

# library(shiny)
# library(lubridate)
# library(DT)
# library(Tutils)
# library(readr)
# library(dplyr)

##########  Utilities

format_datatable_journal = function(dt, choice) {
  dt$mkt_price=scales::label_dollar(accuracy=0.01)(dt$mkt_price)
  dt$close=as.character(round(dt$close,2))

  if (choice!= "info") dt = dplyr::filter(dt, dt$theme == choice)
  datatable=DT::datatable(as.data.frame(dt),
            filter='none',
            options=list(
             paging=TRUE,searching=TRUE, info=FALSE,ordering=TRUE,autowidth=FALSE,
             decimal=".",thousands= "",
              columnDefs = list(
                list(targets ="theme", visible=FALSE),
                list(targets = "text",width='300px')
                ,list(targets = "text",
                      render = DT::JS('function(data, type, full) {
                  function formatColumn(data) {
                   return data.replace(/\\n/g, "<br>");
                   }
                   return $("<div/>").html(data).text();
                 }')
                 )
                )
            )
            # ,rownames=FALSE
  )
    datatable = DT::formatStyle(datatable,"theme",target="row",backgroundColor = DT::styleEqual(c("trade", "follow","info","setup"),
                                                                   c("lightgreen","lightyellow","white","lightblue")))
    datatable = DT::formatStyle(datatable, columns=names(dt),whiteSpace= "pre-wrap")
    #formatCurrency(c("close","mkt_price"),dec.mark=".",before=F)
    datatable = DT::formatDate(datatable, "date", method = "toLocaleDateString")
}

######################

#'   Journal module UI
#'
#'This module provides the ability to write log entries into a journal and to retrieve them
#'@param id this is used by caller to identify journal and have the link with server piece
#'@export
journalUI = function(id) {
  ns=shiny::NS(id)
  htmltools::tagList(
    shiny::textAreaInput(ns("inputIdee"), label = "Enter text:", value = "",width="800px",height = "200px"),
    shiny::actionButton(ns("saveIdee"), "Save", shiny::icon("paper-plane"),
                          style="color: #fff; background-color: #E96200; border-color: #2e6da4"),
    shiny::selectInput(ns("theme"), label="Theme", choices=c("trade", "follow","info","setup"), selected="info"),
    htmltools::br(),
    htmltools::br(),
    DT::DTOutput(ns("text_output"))
  )
}

#'   Journal module server
#'
#'This module provides the ability to write log entries into a journal and to retrieve them
#'@param id this is used by caller to identify journal and have the link with server piece
#'@param sym this is to filter only log entries corresponding to \code{sym}
#'@param windowDate this is to retrieve only log entries whose date is greater or equal to \code{windowdate}
#'@export
journalServer = function(id, sym, windowDate=shiny::reactive(lubridate::today()-365)) {
  message("journalServer")
  stopifnot("sym must be reactive!" = shiny::is.reactive(sym),
            "windowDate must be reactive!" = shiny::is.reactive(windowDate))

  shiny::moduleServer(id, function(input, output, session) {
     shiny::observeEvent(input$saveIdee, {
                   journalEntry(sym=sym(), theme=input$theme, text=input$inputIdee)
                   shiny::updateTextAreaInput(session,"inputIdee",value="")
                 })

    ################################  Tab Journal #################################

     output$text_output = DT::renderDT({

        # df= read.table(file=paste0(config::get("DirNewTrading"),"Journal.csv"),
        #                             header=TRUE,sep=";",dec=".")
        df= suppressMessages(read_delim(file=paste0(config::get("DirNewTrading"),"Journal.csv"),
                                    col_names=TRUE,delim=";",
                                    locale = readr::locale(decimal_mark = ".",grouping_mark = "" )))
        df$date=lubridate::dmy(df$date)
        df=dplyr::filter(df, date >= windowDate())
        if ((sym() != "STOCK") && (sym() != "All")) df=dplyr::filter(df,sym==sym())

        format_datatable_journal(df,input$theme)
     })

  })
}

#'   Journal app
#'
#'This complete app makes use of journal UI/server module.
#'
#'It can be used as a standalone application to record new entries or to read journal.
#'@export
journalApp = function() {
  ui = shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        htmltools::h1("Journal"),
        shiny::textInput("sym","Symbol:",value="STOCK"),
        shiny::sliderInput("windowDate",label="Window date: ",max=lubridate::today(),
                           min=lubridate::ymd("2022-10-01"),value=lubridate::today()-100
                           ,step = 7)
      ),
      shiny::mainPanel(journalUI("Journal"))
    )
  )

  server= function(input, output, session) {
    ##i_s_d = debounce(input$sym,3000)
    journalServer(id="Journal",sym=shiny::reactive(input$sym),
                  windowDate=shiny::reactive(lubridate::ymd(input$windowDate)))
  }
  shiny::shinyApp(ui, server)
}


#'   Journal Entry
#'
#'This function provides the ability to enter one new log individual entry into the journal
#'
#'Text contents may include special characters, but then must be quoted using \\ character
#'Also the record date is today - this may differ from the actual trade date.
#'The idea here is that the comment in the journal reflects the thinking and feeling when it is written
#'and therefore date should be correct.
#'@param sym to identify the ticker
#'@param theme to know what type of entry (e.g. "trade")
#'@param text contents - it may include \\n for new lines returns
#'@export
#'@examples
#'journalEntry("TEST","info","This is great stock!\nI must absolutely get in :-)")

journalEntry= function(sym, theme, text) {
  tickerData = getLastTickerData(sym)
  lastSPY=getLastTickerData("SPY")
  log_entry=data.frame(
    theme=theme,
    date=format(lubridate::today(),"%d.%m.%Y"),
    sym=sym,
    sym_close=tickerData$last,
    sym_close_change=tickerData$change,
    mkt_price=lastSPY$last,
    mkt_change=lastSPY$change,
    text=text)

    file= paste0(config::get("DirNewTrading"),"Journal.csv")
    if (file.exists (file)) {
      readr::write_delim(log_entry,file=file,append=TRUE,
                         col_names=FALSE,delim=";",
                         quote="needed")
    }
    else display_error_message(paste0("Journal.csv does not exists in specified pathname: ", config::get("DirNewTrading")))
}




#journalApp()

#### TESTS
# library(dplyr)
# x=tibble(chp1="gguigu",chp2="ggeg \ngegterhg",chp3=5000.34)
# x=tibble(chp1="-0.5453245",chp2="ggeg \ngegterhg",chp3=115227.45)
#
# write_delim(x,file="Essai.csv",append=TRUE,
#             col_names=FALSE,delim=";",
#             quote="needed")
#
# x=read_delim(file="Essai.csv",
#            col_names=TRUE,delim=";",
#            locale = locale(decimal_mark = ".",grouping_mark = "" ))
#
