#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom fst read.fst
#' @importFrom shinymanager check_credentials secure_server
#' @noRd
app_server <- function( input, output, session ) {
  
  
  res_auth <- secure_server(
    check_credentials = check_credentials(
      "./cred.sqlite",
      passphrase = "dreamcatcher"
    )
  )
  
  company<- reactive({
    req(input$shinymanager_where)
    isolate(res_auth$company)
  })
  
  observeEvent(company(),{
    if(!is.null(company())){
      mod_dmenu_server("mmenu", merchant=company())
    }
  }
  )
  
  
  df<-read.fst("./data/transactiondata.fst")
  
  
  observeEvent(c(input$valsel, input$typesel),{
    scored.all<- reactive({
      scorer.all(df,ttype=input$typesel)
    })
    growth<-reactive({
      score.growth(scored.all(), kpi=input$valsel)
      })
    x<- reactive({
      if(input$valsel=="valscore"){
        scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$valscore
      } else {
        if(input$valsel=="repscore"){
          scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$repscore
        } else {
          scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$milscore
        }
      }
      
    })
    mod_histo_server("milestype1", x=x(), xt="Score", yt="Frequency")
    output$scoretitle<-renderText(paste0("Customer score for the month ",max(scored.all()$monthid)))
    mod_infoboxcollection_server("scoredelta",growth())
  })
  
  observeEvent(c(input$valselmerove, input$typeselmerove, input$merove),{
    dtm<-reactive(df[df$merchant==input$merove,])
    scored.mer<- reactive({
      scorer.all(dtm(),ttype=input$typesel)
      
    })
    growth.mer<-reactive({
      score.growth(scored.mer(), kpi=input$valselmerove)
    })
    y<- reactive({
      if(input$valselmerove=="valscore"){
        scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$valscore
      } else {
        if(input$valselmerove=="repscore"){
          scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$repscore
        } else {
          scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$milscore
        }
      }
      
    })
    mod_histo_server("milestypemerove", x=y(), xt="Score", yt="Frequency")
    output$scoretitlemerove<-renderText(paste0("Customer score for the month ",max(scored.mer()$monthid), " for ", input$merove))
    mod_infoboxcollection_server("scoredeltamerove",growth.mer())
  })
  
  
  observeEvent(input$typeselmiles,{
    dfmiles<-reactive({
      df[df$TType==input$typeselmiles,]
    })
    mil.sum.df<- reactive(
      milesummary(dfmiles())
    )
    mod_barchart_server("milsumall",x=mil.sum.df()$monthid,y=mil.sum.df()$miles, xtitle="", ytitle="Miles")
  })
  
  
  
}
