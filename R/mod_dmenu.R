#' dmenu UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dmenu_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarMenuOutput(ns("menu"))
  )
}
    
#' dmenu Server Functions
#'
#' @noRd 
mod_dmenu_server <- function(id, merchant="admin"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$menu<-renderMenu({
      if(merchant=="admin") {
        sidebarMenu(
          menuItem(
            text="Overall Scores",
            tabName="overall",
            badgeLabel = "Exclusive",
            #status="warning",
            selected= T
          ),
          menuItem(
            text="MileStory",
            tabName="mstory",
            badgeLabel = "Exclusive",
            #status="danger",
            selected= F
          ),
          menuItem(
            text="About LaNubia",
            badgeLabel = "Supplier",
            #status="info",
            selected= F
          )
        )
      } else {
        sidebarMenu(
          menuItem(
            text="Scores",
            badgeLabel = "Merchants",
            tabName="merchant",
            status="success",
            selected= T
          ),
          menuItem(
            text="Customer Behaviour",
            badgeLabel = "Premium",
            tabName="premium",
            status="danger",
            selected= F
          ),
          menuItem(
            text="About LaNubia",
            badgeLabel = "Supplier",
            status="info",
            selected= F
          )
        )
      }
    })
 
  })
}
    
## To be copied in the UI
# mod_dmenu_ui("dmenu_1")
    
## To be copied in the server
# mod_dmenu_server("dmenu_1")
