#### Creation date: 7.09.2023

############ To be commented out if single set up
#library(shinyWidgets) ## for sliderTextInput
#  library(ggplot2)
# library(plotly)
#  library(shiny)
#  library(dplyr)
#  library(tidyr) #for crossing function
#
# ## for getBSComboPrice function - if not already sourced
#  library(derivmkts)

######################

#'   Chart module UI
#'
#'This module provides the ability to display a chart with 2 curves using the same underlying
#'@param id this is used by caller to identify chart and have the link with server piece
#'@export
chartUI = function(id) {
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::sliderInput(ns("x_zoom"),"X Zoom",min=0,max=30,step=0.5,value=10,post="%"),
    plotly::plotlyOutput(ns("distPlot"))
  )
}


#'   Chart module server
#'
#'This module provides the ability to display a chart with 2 curves using the same underlying
#'@param id this is used by caller to identify chart and have the link with server piece
#'@param data data frame containing \code{x}, \code{y1} and \code{y2} columns
#'@export
chartServer = function(id,data) {
  #### If data is NULL (no data) then do nothing
  #### If no data$y1 or no data$y2 then do nothing for y1 (resp. y2) and display only y2 (resp. y1)
  shiny::moduleServer(id, function(input,output,session) {

      output$distPlot <- plotly::renderPlotly({
        message("chart output$distplot")

        if (length(data) != 0) {
          ### Create plot_data dataframe - to be passed to ggplot function
          plot_data=data.frame(x=data$x)
          midx=stats::median(data$x)
          y1_min=NA
          y2_min=NA
          y1_max=NA
          y2_max=NA
          ind_min=which.min(abs(data$x-midx*(1-input$x_zoom/100)))
          ind_max=which.min(abs(data$x-midx*(1+input$x_zoom/100)))
          x_min=data$x[ind_min]
          x_max=data$x[ind_max]
          message(" ind_min: ",ind_min," ind_max:",ind_max)
          message(" xmin: ",x_min," xmax:",x_max)

          if (!is.null(data$y1)) {
            ### Add y1 to plot_data dataframe
            plot_data$y1=data$y1
            y1_range=data$y1[seq(ind_min,ind_max,1)]
            y1_min=min(y1_range)
            y1_max=max(y1_range)
            message("y1 min: ",y1_min," y1 max:",y1_max)
          }

          if (!is.null(data$y2)) {
            ### Add y2 to plot_data dataframe
            plot_data$y2=data$y2
            y2_range=data$y2[seq(ind_min,ind_max,1)]
            y2_min=min(y2_range)
            y2_max=max(y2_range)
            message("y2 min: ",y2_min," y2 max:",y2_max)
          }

          ### This will work even if y1_min/y2_min/y1_max/y2_max have not bee initialized
          y_max=max(y1_max,y2_max,na.rm=T)
          y_min=min(y1_min,y2_min,na.rm=T)

          plot=ggplot2::ggplot(plot_data,mapping=ggplot2::aes(x=x))+
            {if (!is.null(data$y1))  ggplot2::geom_line(ggplot2::aes(y=y1),color="red",linewidth=1)}+
            {if (!is.null(data$y2)) ggplot2::geom_line(ggplot2::aes(y=y2),color="purple",linewidth=1)}+
            ggplot2::coord_cartesian(xlim=c(x_min,x_max),ylim=c(y_min,y_max))+ ### This will zoom without removing data
            ## dodge does not work with plotly
            ggplot2::scale_x_continuous(name="Underlying prices", guide = ggplot2::guide_axis(n.dodge = 2),n.breaks= 30)+
            ggplot2::scale_y_continuous(name="Resultat uPnL",n.breaks= 20)+
            ggplot2::geom_hline(yintercept=0,color="black")+
            ggplot2::geom_vline(xintercept=midx,color="black")
          plotly::ggplotly(plot)
        }

        else {
            message("No data for chart")
            plotly::ggplotly(ggplot2::ggplot()+ggplot2::geom_blank(data=data.frame()))
        }
  })
})}


chartDemoApp <- function() {
  ui = shiny::fluidPage(
    chartUI("Chart1")
  )

  server = function(input, output, session) {
    u_prices=seq(90,110,0.1)
    df1=NULL
    df2=NULL
    df1=data.frame(pos=c(-2,2),type=c("Put","Put"),strike=c(100,100),DTE=c(5,10),sig=c(0.2,0.4),mul=c(100,100))
    df2=data.frame(pos=c(1,-1),type=c("Put","Put"),strike=c(100,95),DTE=c(10,5),mul=c(100,100))
    opt_prices1=getBSComboPrice(data=df1,S=u_prices)
    opt_prices2=getBSComboPrice(data=df2,S=u_prices,sig=0.1)
    chart_data=list(x=u_prices,y1=opt_prices1,y2=opt_prices2)
    ##chart_data=list(x=u_prices,y1=opt_prices1,y2=NULL)
    ##tib=dplyr::tibble(x=chart_data$x, y1=chart_data$y1,y2=chart_data$y2)
    chartServer("Chart1",chart_data)
  }
  shiny::shinyApp(ui, server)
}


###chartDemoApp()
