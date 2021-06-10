#' scorer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scorer_ui <- function(id){
  ns <- NS(id)
  box(
    title = textOutput("scoretitle"),
    solidHeader = T,
    width = 12,
    headerBorder = T,
    status ="secondary",
  tagList(
    fluidRow(
      column(
        width=6,
        radioButtons(ns("ttypesel"),"Select Transaction Type", 
                     choiceNames = c("Miles Earned","Miles Redeemed"),
                     choiceValues = c(0,1), selected = 0, inline = T)
      ),
      column(
        width=6,
        radioButtons(ns("scorevalsel"),"Select score based on", 
                     choiceNames = c("Overall","Transaction Value","Transaction Frequency"),
                     choiceValues = c("valscore","repscore","milscore"), selected = "valscore", inline = T)
      )
    ),
    fluidRow(
      column(
        width = 10,
        mod_histo_ui(ns("scorehist"))
      ),
      column(
        width = 2,
        mod_infoboxcollection_ui(ns("scoredelta"))
      )
    )
  )
  )
}
    
#' scorer Server Functions
#'
#' @noRd 
mod_scorer_server <- function(id, df, merc="merchant1"){
  #req(df)
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    scored.dat<- reactive(
      {
        print("1")
        scorer.all(df,ttype=input$ttypesel)
      }
    )
    
    growth.dat<- reactive(
      {
        print("2")
        score.growth(scored.dat(), kpi=input$scorevalsel)
      }
    )
    
    x<- reactive({
      print("3")
      req(input$scorevalsel, scored.dat())
      if(input$scorevalsel=="valscore"){
        scored.dat()[scored.dat()$monthid==max(scored.dat()$monthid),]$valscore
      } else {
        if(input$scorevalsel=="repscore"){
          scored.dat()[scored.dat()$monthid==max(scored.dat()$monthid),]$repscore
        } else {
          scored.dat()[scored.dat()$monthid==max(scored.dat()$monthid),]$milscore
        }
      }
    })
    
    observeEvent( c(input$scorevalsel, input$ttypesel),
      {
        mod_histo_server("scorehist", x=x(), xt="Score", yt="Frequency")
        output$scoretitle<-renderText(paste0("Customer score for the month ",max(scored.dat()$monthid), " for ", merc))
        mod_infoboxcollection_server("scoredelta",growth.dat())
    })
    
 
  })
}
    
## To be copied in the UI
# mod_scorer_ui("scorer_1")
    
## To be copied in the server
# mod_scorer_server("scorer_1")
