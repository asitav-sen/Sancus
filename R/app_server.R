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
    req(company()=="admin")
    scorer.all(df,ttype=input$typesel)
  }) 
  
  growth<-eventReactive(c(input$valsel, scored.all()),{
    req(company()=="admin")
    score.growth(scored.all(), kpi=input$valsel)
  })
  
  x<- eventReactive(c(scored.all(),input$valsel),{
    req(company()=="admin")
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
  
  rep.dat<-eventReactive( c(company(),input$churnlim),
    {
    if(!is.null(company()))
    repcal(df, merc=company(), lim=input$churnlim)
  })
  
  
  observeEvent( input$churnlim,
    {
      mod_barchart_server("newrep",x=rep.dat()$monthid,y=rep.dat()$count,z=rep.dat()$rep.pur, xtitle="", ytitle="# Transactions")
      mod_barchart_server("newrepmiles",x=rep.dat()$monthid,y=rep.dat()$miles,z=rep.dat()$rep.pur, xtitle="", ytitle="Amount/Miles")
      mod_barchart_server("newrepclients",x=rep.dat()$monthid,y=rep.dat()$customers,z=rep.dat()$rep.pur, xtitle="", ytitle="# Customers")
    }
  )
  
  sow.data<- eventReactive( company(),
    {
      sowcal(df, company())
    }
  )
  
  
  mod_linechart_server("sowtr",x=sow.data()$monthid,y=sow.data()$sowtr, xtitle="", ytitle="Share (%)")
  mod_linechart_server("sowmil",x=sow.data()$monthid,y=sow.data()$sowmil,xtitle="", ytitle="Share (%)")
  
  salrev.dat<- eventReactive( company(),
                              {
                                salrevcal(df,company())
                              })
  
  mod_barchart_server("salrevtr", x=salrev.dat()$monthid,y=salrev.dat()$count, xtitle="", ytitle="Transactions")
  mod_barchart_server("salrevmil", x=salrev.dat()$monthid,y=salrev.dat()$miles, xtitle="", ytitle="Amount/Miles")
  
  
  
  scored.mer.sel<-eventReactive( c(input$typeselmer),
                             {
                               scorer.all(df[df$merchant==company(),],ttype=input$typeselmer)
                             }
  )
  
  growth.mer.sel<-eventReactive(c(scored.mer.sel(),input$valselmersel),{
    score.growth(scored.mer.sel(), kpi=input$valselmersel)
  })
  
  z<- eventReactive(c(input$valselmersel,scored.mer.sel()),{
    if(input$valselmerove=="valscore"){
      scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$valscore
    } else {
      if(input$valselmerove=="repscore"){
        scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$repscore
      } else {
        scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$milscore
      }
    }
    
  })
  
  observeEvent(c(z(),growth.mer.sel(),scored.mer.sel()),{
    mod_histo_server("milestypemersel", x=z(), xt="Score", yt="Frequency")
    output$scoretitlemersel<-renderText(paste0("Customer score for the month ",max(scored.mer.sel()$monthid)))
    mod_infoboxcollection_server("scoredeltamersel",growth.mer.sel())
  })
  
  
  segment.self.dat<- eventReactive( c(company()),
    {
      segmentd(df, company())
    }
  )
  
  
  mod_barchart_server("ownsegmenttrn",x=segment.self.dat()$monthid,y=segment.self.dat()$count,
                      z=segment.self.dat()$Segment, xtitle="", ytitle="Transactions", lo="h")
  
  mod_barchart_server("ownsegmentmil",x=segment.self.dat()$monthid,y=segment.self.dat()$miles,
                      z=segment.self.dat()$Segment, xtitle="", ytitle="Amount/Miles", lo="h")
  
  
}
