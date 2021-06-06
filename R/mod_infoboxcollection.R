#' infoboxcollection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_infoboxcollection_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      mod_infobox_ui(ns("high")),
      mod_infobox_ui(ns("medium")),
      mod_infobox_ui(ns("low"))
    )

  )
}
    
#' infoboxcollection Server Functions
#'
#' @noRd 
mod_infoboxcollection_server <- function(id,gdf){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_infobox_server("high",val=paste0(gdf[gdf$kpi=="high",]$growth," %"),
                       subt="Change in High", icn="comments", clr="white", wd=4, hrf="")
    mod_infobox_server("medium",val=paste0(gdf[gdf$kpi=="medium",]$growth," %"),
                       subt="Change in Medium", icn="comments", clr="white", wd=4, hrf="")
    mod_infobox_server("low",val=paste0(gdf[gdf$kpi=="low",]$growth," %"),
                       subt="Change in Low", icn="comments", clr="white", wd=4, hrf="")
  })
}
    
## To be copied in the UI
# mod_infoboxcollection_ui("infoboxcollection_1")
    
## To be copied in the server
# mod_infoboxcollection_server("infoboxcollection_1")
