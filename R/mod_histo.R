#' histo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
mod_histo_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(plotly::plotlyOutput(ns("histoplot")), image="./www/horse.gif", image.width = "75px")
  )
}

#' histo Server Functions
#' @import plotly
#'
#' @noRd 
mod_histo_server <- function(id,x, xt="", yt=""){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    req(length(x>0))
    output$histoplot<-renderPlotly({
      res <- hist(x, breaks = 250)
      plot_ly() %>% 
        layout(
          xaxis = list(title = xt,color = "white"),
          yaxis = list(title = yt,color = "white"),
          plot_bgcolor = 'transparent',
          paper_bgcolor = 'transparent',
          shapes = list(
            list(
              type = "rect",
              fillcolor = "red", line = list(color = "white"),
              opacity = 0.05,
              x0 = 0, x1 = as.numeric(quantile(x,0.25)), xref = "x",
              y0 = 0, y1 = max(res$count), yref = "y"
            ),
            list(
              type = "rect",
              fillcolor = "transparent", line = list(color = "white"),
              opacity = 0.1,
              x0 = as.numeric(quantile(x,0.25)), x1 = as.numeric(quantile(x,0.75)), xref = "x",
              y0 = 0, y1 = max(res$count), yref = "y"
            ),
            list(
              type = "rect",
              fillcolor = "green", line = list(color = "white"),
              opacity = 0.05,
              x0 = as.numeric(quantile(x,0.75)), x1 = 1, xref = "x",
              y0 = 0, y1 = max(res$count), yref = "y"
            )
            
          ),
          annotations= list(
            list(
              text="Low",
              showarrow=F,
              x=as.numeric(quantile(x,0.25))/2,
              y=length(x)/84,
              font=list(size=15, color="white")
            ),
            list(
              text="High",
              showarrow=F,
              x=as.numeric(quantile(x,0.75))+(1-as.numeric(quantile(x,0.25)))/2,
              y=length(x)/84,
              font=list(size=15, color="white")
            ),
            list(
              text="Medium",
              showarrow=F,
              x=as.numeric(quantile(x,0.5)),
              y=length(x)/84,
              font=list(size=15, color="white")
            )
          )
        ) %>% 
        add_histogram(x = ~x, nbinsx = 500) %>% 
        plotly::config(displayModeBar = FALSE)
    })
    
    
  })
}

## To be copied in the UI
# mod_histo_ui("histo_1")

## To be copied in the server
# mod_histo_server("histo_1")
