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
mod_dmenu_server <- function(id, merchant, premium){
  req(!is.null(merchant))
  print("menu")
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$menu<-renderMenu({
      if(merchant=="admin") {
        sidebarMenu(
          id="men",
          menuItem(
            text="Overall Scores",
            tabName="overall",
            badgeLabel = "Exclusive",
            #status="warning",
            selected= T
          ),
          menuItem(
            text="About LaNubia",
            badgeLabel = "Supplier",
            #status="info",
            selected= F
          )
        )
      } else { 
        if(premium == T){
          sidebarMenu(
            id="men",
            menuItem(
              text="Basic",
              tabName="merchant",
              #status="success",
              selected= T
            ),
            menuItem(
              text="Advanced",
              badgeLabel = "Premium",
              tabName="premium",
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
            id="men",
            menuItem(
              text="Basic",
              tabName="merchant",
              #status="success",
              selected= T
            ),
            menuItem(
              text="Advanced",
              badgeLabel = "Premium",
              tabName="premiumdummy",
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
        }

      }
    })%>% 
      bindCache(merchant, premium) 
 
  })
}
    
## To be copied in the UI
# mod_dmenu_ui("dmenu_1")
    
## To be copied in the server
# mod_dmenu_server("dmenu_1")
