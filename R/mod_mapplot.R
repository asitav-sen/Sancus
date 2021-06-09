#' mapplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @import leaflet
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize ungroup
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_mapplot_ui <- function(id){

  ns <- NS(id)
  tagList(
    leafletOutput(ns("mapplot"))
  )
}
    
#' mapplot Server Functions
#'
#' @noRd 
mapplotServer<- function(id, df, mname="merchant1"){
  moduleServer(
    id,
    function(input, output, session){
      
      output$mapplot<- renderLeaflet({
        Icons <- icons(
          iconUrl = ifelse(df$merchant == mname,
                           "https://www.fortuna.club/wp-content/uploads/2019/10/logo-fortuna.png",
                           "https://img.pngio.com/other-png-6-png-image-other-png-2000_2412.png"
          ),
          iconWidth = 15, iconHeight = 15#,
          #iconAnchorX = 22, iconAnchorY = 94
        )
        df%>%
          leaflet()%>%
          addTiles()%>%
          addMarkers(
            lng = ~long,
            lat = ~lat,
            icon = Icons,
            #stroke = FALSE, fillOpacity = 0.5,
            label = ~ifelse(merchant==mname, mname, Segment)
          )
      }) %>% 
        bindCache(df, mname)
    }
  )
}
    
## To be copied in the UI
# mod_mapplot_ui("mapplot_1")
    
## To be copied in the server
# mod_mapplot_server("mapplot_1")
