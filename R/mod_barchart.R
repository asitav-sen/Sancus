#' barchart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_barchart_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("barplot")) 
  )
}
    
#' barchart Server Functions
#'
#' @noRd 
mod_barchart_server <- function(id,x,y, xtitle, ytitle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$barplot<- renderPlotly({
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
          paper_bgcolor = 'transparent'
        ) %>% plotly::config(displayModeBar = FALSE)
    })
  })
}
    
## To be copied in the UI
# mod_barchart_ui("barchart_1")
    
## To be copied in the server
# mod_barchart_server("barchart_1")
