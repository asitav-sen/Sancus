#' barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barchart_ui <- function(id,ht="300px", wt="100%"){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("barplot"), height = ht, width =wt), image="./www/horse.gif", image.width = "75px")
  )
}
    
#' barchart Server Functions
#'
#' @noRd 
mod_barchart_server <- function(id,x,y,z=NULL, xtitle, ytitle, lo="h"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barplot<- renderPlotly({
      if(is.null(z)){
        plot_ly(
          x= ~x,
          y= ~y,
          type = 'bar',
          showlegend=F
        )%>%
          layout(
            xaxis = list(title = xtitle, color = "white"),
            yaxis = list(title = ytitle, color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent',
            hoverlabel=list(bgcolor="black")
          ) %>% plotly::config(displaylogo = FALSE,
                               modeBarButtonsToRemove = c(
                                 'sendDataToCloud',
                                 'autoScale2d',
                                 'resetScale2d',
                                 'hoverClosestCartesian',
                                 'hoverCompareCartesian'
                               ))
      } else {
        plot_ly(
          x= ~x,
          y= ~y,
          color= ~z,
          colors = c("dodgerblue", "cadetblue3", "cornflowerblue","deepskyblue3", "lightblue1", "darkslategray3"),
          type = 'bar',
          showlegend=T
        )%>%
          layout(
            xaxis = list(title = xtitle, color = "white"),
            yaxis = list(title = ytitle, color = "white"),
            plot_bgcolor = 'transparent',
            paper_bgcolor = 'transparent',
            legend = list(orientation = lo, font=list(color = "white")),
            hoverlabel=list(bgcolor="black")      
          ) %>% plotly::config(displaylogo = FALSE,
                               modeBarButtonsToRemove = c(
                                 'sendDataToCloud',
                                 'autoScale2d',
                                 'resetScale2d',
                                 'hoverClosestCartesian',
                                 'hoverCompareCartesian'
                               ))
      }
        
    }) %>% 
      bindCache(df, x,y,z,xtitle, ytitle, lo) 
  })
}
    
## To be copied in the UI
# mod_barchart_ui("barchart_1")
    
## To be copied in the server
# mod_barchart_server("barchart_1")
