#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(
      header=headr(),
      sidebar= sidebr(),
      body = dashboardBody(
        tabItems(
          tabItem(
            tabName="overall",
            taboverall()
          ),
          tabItem(
            tabName="mstory",
            tabmstory()
          ),
          tabItem(
            tabName="merchant"
          ),
          tabItem(
            tabName="premium"
          )
        )
        ),
      controlbar = NULL,  
      footer = footr(),
      dark = T,
      scrollToTop = TRUE,
      fullscreen=T
      )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Loyalty'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

