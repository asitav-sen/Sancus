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
  
  premium<- reactive({
    req(input$shinymanager_where)
    isolate(res_auth$premium)
  })
  
  observeEvent(company(),{
    if(!is.null(company())){
      mod_dmenu_server("mmenu", merchant=company(), premium=premium())
    }
  }, ignoreNULL = F
  )
  
  
  df<-read.fst("./data/transactiondata.fst")
  
  scored.all<-reactive({
    req(company()=="admin")
    scorer.all(df,ttype=input$typesel)
  }) %>% 
    bindCache(input$typesel, df)
  
  growth<-reactive({
    req(company()=="admin")
    score.growth(scored.all(), kpi=input$valsel)
  }) %>% 
    bindCache(input$valsel, scored.all())
  
  x<- reactive({
    req(company()=="admin",scored.all())
    if(input$valsel=="valscore"){
      scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$valscore
    } else {
      if(input$valsel=="repscore"){
        scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$repscore
      } else {
        scored.all()[scored.all()$monthid==max(scored.all()$monthid),]$milscore
      }
    }
    
  })%>% 
    bindCache(input$valsel, scored.all())
  
  observeEvent(c(scored.all(), growth(), x()),{
    
    mod_histo_server("milestype1", x=x(), xt="Score", yt="Frequency")
    output$scoretitle<-renderText(paste0("Customer score for the month ",max(scored.all()$monthid)))
    mod_infoboxcollection_server("scoredelta",growth())
  }, ignoreNULL = F)
  
  
  dtm<-reactive({
    df[df$merchant==input$merove,]
  })%>% 
    bindCache(input$merove)
  
  scored.mer<-reactive(
    {
      
      req(input$typesel, dtm(), input$merove)
      scorer.all(dtm(),ttype=input$typesel)
    }
  )%>% 
    bindCache(dtm(),input$typesel)
  
  growth.mer<-reactive({
    req(scored.mer(),input$valselmerove)
    score.growth(scored.mer(), kpi=input$valselmerove)
  })%>% 
    bindCache(scored.mer(),input$valselmerove)
  
  y<- reactive({
    req(input$valselmerove,scored.mer())
    if(input$valselmerove=="valscore"){
      scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$valscore
    } else {
      if(input$valselmerove=="repscore"){
        scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$repscore
      } else {
        scored.mer()[scored.mer()$monthid==max(scored.mer()$monthid),]$milscore
      }
    }
    
  })%>% 
    bindCache(input$valselmerove,scored.mer())
  
  observeEvent(c(y(),growth.mer(),scored.mer()),{
    mod_histo_server("milestypemerove", x=y(), xt="Score", yt="Frequency")
    output$scoretitlemerove<-renderText(paste0("Customer score for the month ",max(scored.mer()$monthid), " for ", input$merove))
    mod_infoboxcollection_server("scoredeltamerove",growth.mer())
  })
  
  dfmiles<-reactive({
    df[df$TType==input$typeselmiles,]
  })%>% 
    bindCache(input$typeselmiles,df)
  
  mil.sum.df<- reactive({
    milesummary(dfmiles())
  }
  )%>% 
    bindCache(dfmiles())
  
  observeEvent(mil.sum.df(),{
    mod_barchart_server("milsumall",x=mil.sum.df()$monthid,y=mil.sum.df()$miles, xtitle="", ytitle="Miles")
  })
  
  rep.dat<-reactive(
    {
    if(!is.null(company()))
    repcal(df, merc=company(), lim=input$churnlim)
  }) %>% 
    bindCache(company(),input$churnlim)

  
  observeEvent( input$churnlim,
    {
      mod_barchart_server("newrep",x=rep.dat()$monthid,y=rep.dat()$count,z=rep.dat()$rep.pur, xtitle="", ytitle="# Transactions")
      mod_barchart_server("newrepmiles",x=rep.dat()$monthid,y=rep.dat()$miles,z=rep.dat()$rep.pur, xtitle="", ytitle="Amount/Miles")
      mod_barchart_server("newrepclients",x=rep.dat()$monthid,y=rep.dat()$customers,z=rep.dat()$rep.pur, xtitle="", ytitle="# Customers")
    }
  )
  
  sow.data<- reactive(
    {
      sowcal(df, company())
    }
  ) %>% 
    bindCache(company())
  
  
  
  mod_linechart_server("sowtr",x=sow.data()$monthid,y=sow.data()$sowtr, xtitle="", ytitle="Share (%)")
  mod_linechart_server("sowmil",x=sow.data()$monthid,y=sow.data()$sowmil,xtitle="", ytitle="Share (%)")
  
  salrev.dat<- reactive(
                              {
                                salrevcal(df,company())
                              }) %>% 
    bindCache(company())
  
  mod_barchart_server("salrevtr", x=salrev.dat()$monthid,y=salrev.dat()$count, xtitle="", ytitle="Transactions")
  mod_barchart_server("salrevmil", x=salrev.dat()$monthid,y=salrev.dat()$miles, xtitle="", ytitle="Amount/Miles")
  
  
  
  scored.mer.sel<-reactive(
                             {
                               req(input$typeselmer, df, company())
                               scorer.all(df[df$merchant==company(),],ttype=input$typeselmer)
                             }
  ) %>% 
    bindCache(input$typeselmer,df, company())
  
  growth.mer.sel<-reactive({
    req(scored.mer.sel(),input$valselmersel)
    score.growth(scored.mer.sel(), kpi=input$valselmersel)
  }) %>% 
    bindCache(scored.mer.sel(),input$valselmersel)
  
  z<- reactive({
    req(input$valselmerove,scored.mer.sel())
    if(input$valselmerove=="valscore"){
      scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$valscore
    } else {
      if(input$valselmerove=="repscore"){
        scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$repscore
      } else {
        scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$milscore
      }
    }
    
  }) %>% 
    bindCache(input$valselmersel,scored.mer.sel())
  
  observeEvent(c(z(),growth.mer.sel(),scored.mer.sel()),{
    
    mod_histo_server("milestypemersel", x=z(), xt="Score", yt="Frequency")
    output$scoretitlemersel<-renderText(paste0("Customer score for the month ",max(scored.mer.sel()$monthid)))
    mod_infoboxcollection_server("scoredeltamersel",growth.mer.sel())
  })
  
  
  segment.self.dat<- reactive(
    {
      segmentd(df, company())
    }
  ) %>% 
    bindCache(company(), df)
  
  segment.loc.dat<- reactive(
    {
      segmentloc(df,company())
    }
  ) %>% 
    bindCache(company(),df)
  
  mod_barchart_server("ownsegmenttrn",x=segment.self.dat()$monthid,y=segment.self.dat()$count,
                      z=segment.self.dat()$Segment, xtitle="", ytitle="Transactions", lo="h")
  
  mod_barchart_server("ownsegmentmil",x=segment.self.dat()$monthid,y=segment.self.dat()$miles,
                      z=segment.self.dat()$Segment, xtitle="", ytitle="Amount/Miles", lo="h")
  mapplotServer("ownsegmentloc",segment.loc.dat(), company())
  
  
  segment.oth.dat<- reactive(
                                    {
                                      segmentd(df, company(),a=0)
                                    }
  ) %>% 
    bindCache(company(), df)
  
  segment.loc.oth.dat<- reactive(
                                   {
                                     segmentloc(df,company(),a=0)
                                   }
  ) %>% 
    bindCache(company(),df)
  
  mod_barchart_server("othsegmenttrn",x=segment.oth.dat()$monthid,y=segment.oth.dat()$count,
                      z=segment.self.dat()$Segment, xtitle="", ytitle="Transactions", lo="h")
  
  mod_barchart_server("othsegmentmil",x=segment.oth.dat()$monthid,y=segment.oth.dat()$miles,
                      z=segment.oth.dat()$Segment, xtitle="", ytitle="Amount/Miles", lo="h")
  mapplotServer("othsegmentloc",segment.loc.oth.dat(), company())
  
  observeEvent( input$men,
    {
      if(input$men=="merchant" | input$men=="premium"){
        showModal(modalDialog(
          title = "Welcome!",
          jumbotron(
            title = "Boost your revenues!",
            lead = "Take decisions driven by data. Understand how and where your customers and prospects are spending.",
            "There are three ways to boost your revenue.",
            tags$ol(
              tags$li("Bring in more customers"), 
              tags$li("Bring in existing customers more frequently"), 
              tags$li("Increase sales per visit")
            ),
            "This dashboard has substantial information to help you with these three. Want to know more? Click below",
            status = "info",
            href = "http://www.lanubia.com",
            btnName = "Yes! I want to boost revenue!"
          ),
          
          size="l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
      
      if(input$men=="premiumdummy"){
        showModal(modalDialog(
          title = "Oops!",
          jumbotron(
            title = "Sorry. You are not authorised.",
            lead = "You need to activate our premium services to activate this.",
            "You will have access to information about where and how are your customers and your competitor's customers spending.
            Want to access? Click below.",
            status = "info",
            href = "http://www.lanubia.com",
            btnName = "Yes! I want to activate premium."
          ),
          size="l",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
  )
  
  
}
