#' infobox UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinycssloaders withSpinner
mod_infobox_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinycssloaders::withSpinner(
      bs4ValueBoxOutput(ns("vbox"), width=12), image="./www/fish.gif", image.width = "75px"
    )
    
  )
}

#' infobox Server Functions
#'
#' @noRd 
mod_infobox_server <- function(id, val, subt, icn, clr, wd, hrf="", grd=F, elv=NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$vbox<-renderbs4ValueBox({
      valueBox(
        value=val,
        subtitle=subt,
        icon = icon(icn),
        color = clr,
        width = wd,
        href = hrf,
        footer = NULL,
        gradient = grd,
        elevation = elv
      )
    })%>% 
      bindCache(val, subt, icn, clr, wd, hrf, grd, elv)
    
  })
}

## To be copied in the UI
# mod_infobox_ui("infobox_1")

## To be copied in the server
# mod_infobox_server("infobox_1")
