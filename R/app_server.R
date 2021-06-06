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
  
  scored.all<-eventReactive(c(input$typesel),{
    scorer.all(df,ttype=input$typesel)
  })
  
  growth<-eventReactive(c(input$valsel, scored.all()),{
    score.growth(scored.all(), kpi=input$valsel)
  })
  
  x<- eventReactive(c(scored.all(),input$valsel),{
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
  
  observeEvent(c(scored.all(), growth(), x()),{
    mod_histo_server("milestype1", x=x(), xt="Score", yt="Frequency")
    output$scoretitle<-renderText(paste0("Customer score for the month ",max(scored.all()$monthid)))
    mod_infoboxcollection_server("scoredelta",growth())
  })
  
  
  dtm<-eventReactive(input$merove,{
    df[df$merchant==input$merove,]
  })
  
  scored.mer<-eventReactive( c(dtm(),input$typesel),
    {
      scorer.all(dtm(),ttype=input$typesel)
    }
  )
  
  growth.mer<-eventReactive(c(scored.mer(),input$valselmerove),{
    score.growth(scored.mer(), kpi=input$valselmerove)
  })
  
  y<- eventReactive(c(input$valselmerove,scored.mer()),{
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
  
  observeEvent(c(y(),growth.mer(),scored.mer()),{
    mod_histo_server("milestypemerove", x=y(), xt="Score", yt="Frequency")
    output$scoretitlemerove<-renderText(paste0("Customer score for the month ",max(scored.mer()$monthid), " for ", input$merove))
    mod_infoboxcollection_server("scoredeltamerove",growth.mer())
  })
  
  dfmiles<-eventReactive(input$typeselmiles,{
    df[df$TType==input$typeselmiles,]
  })
  
  mil.sum.df<- eventReactive(dfmiles(),{
    milesummary(dfmiles())
  }
  )
  
  observeEvent(mil.sum.df(),{
    mod_barchart_server("milsumall",x=mil.sum.df()$monthid,y=mil.sum.df()$miles, xtitle="", ytitle="Miles")
  })
  
  
  
}
