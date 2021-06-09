#' linechart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_linechart_ui <- function(id,ht="300px"){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("lineplot"), height = ht), image="./www/horse.gif", image.width = "75px")
  )
}
    
#' linechart Server Functions
#'
#' @noRd 
mod_linechart_server <- function(id, x,y,xtitle, ytitle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$lineplot<- renderPlotly({
      plot_ly(
        x= ~x,
        y= ~y,
        type = 'scatter',
        mode = 'lines+markers'
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
    })%>% 
      bindCache(x,y,xtitle, ytitle)
  })
}
    
## To be copied in the UI
# mod_linechart_ui("linechart_1")
    
## To be copied in the server
# mod_linechart_server("linechart_1")
