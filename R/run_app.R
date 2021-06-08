#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts. 
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options 
#' @importFrom shinymanager secure_app
run_app <- function(
  onStart = NULL,
  options = list(), 
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = secure_app(app_ui, enable_admin=T,
                      tags_top = 
                        tags$div(
                          tags$h4("Brought to you by", style = "align:center"),
                          tags$img(
                            src = "https://lanubia.nl/wp-content/uploads/2018/04/Logo-LaNubia.com-340-x-156.png", width = 100
                          )
                        ),
                      tags_bottom = tags$div(
                        tags$p(
                          "For any question, please  contact ",
                          tags$a(
                            href = "mailto:aldo.silvano@lanubia.com?Subject=Shiny%20aManager",
                            target="_top", "Aldo Silvano"
                          )
                        )
                      ),
                      background  = "linear-gradient(rgba(0, 0, 255, 0.5),
                       rgba(255, 255, 0, 0.5)),
                       url('https://fortuna.club/wp-content/uploads/2019/11/fortuna-card-2.png');"),
      server = app_server,
      onStart = onStart,
      options = options, 
      enableBookmarking = enableBookmarking, 
      uiPattern = uiPattern
    ), 
    golem_opts = list(...)
  )
}
