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
  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })
  
  
  company<- reactive({
    req(input$shinymanager_where)
    print(res_auth$company)
    res_auth$company
  })
  
  premium<- reactive({
    req(input$shinymanager_where)
    res_auth$premium
  })
  
  observeEvent(company(),{
    req(company())
    if(!is.null(company())){
      mod_dmenu_server("mmenu", merchant=company(), premium=premium())
    }
  }
  )
  
  
  df<-read.fst("./data/transactiondata.fst")
  
  dtm<- eventReactive(c(input$merc,company()),{
    print(company())
    req(company())
    if(company()=="admin"){
      df[df$merchant==input$merc,]
    } else {
      df[df$merchant==company(),]
    }
    
  })
  
  observeEvent(input$merc,
    {
      req(company(), dtm(), input$merc)
      mod_scorer_server("overall",dtm(),input$merc)
    }
  )
  
  
  
  rep.dat<-reactive(
    {
      req(company(), df, input$churnlim)
    if(!is.null(company()))
    repcal(df, merc=company(), lim=input$churnlim)
  }) %>% 
    bindCache(company(),input$churnlim)

  
  observeEvent( input$churnlim,
    {
      req(company(),rep.dat())
      mod_barchart_server("newrep",x=rep.dat()$monthid,y=rep.dat()$count,z=rep.dat()$rep.pur, xtitle="", ytitle="# Transactions")
      mod_barchart_server("newrepmiles",x=rep.dat()$monthid,y=rep.dat()$miles,z=rep.dat()$rep.pur, xtitle="", ytitle="Amount/Miles")
      mod_barchart_server("newrepclients",x=rep.dat()$monthid,y=rep.dat()$customers,z=rep.dat()$rep.pur, xtitle="", ytitle="# Customers")
    }
  )
  
  sow.data<- reactive(
    {
      req(df, company())
      sowcal(df, company())
    }
  ) %>% 
    bindCache(company())
  
  
  
  mod_linechart_server("sowtr",x=sow.data()$monthid,y=sow.data()$sowtr, xtitle="", ytitle="Share (%)")
  mod_linechart_server("sowmil",x=sow.data()$monthid,y=sow.data()$sowmil,xtitle="", ytitle="Share (%)")
  
  salrev.dat<- reactive(
                              {
                                req(df, company())
                                salrevcal(df,company())
                              }) %>% 
    bindCache(company())
  
  mod_barchart_server("salrevtr", x=salrev.dat()$monthid,y=salrev.dat()$count, xtitle="", ytitle="Transactions")
  mod_barchart_server("salrevmil", x=salrev.dat()$monthid,y=salrev.dat()$miles, xtitle="", ytitle="Amount/Miles")
  
  
  
  # scored.mer.sel<-reactive(
  #                            {
  #                              req(input$typeselmer, df, company())
  #                              scorer.all(df[df$merchant==company(),],ttype=input$typeselmer)
  #                            }
  # ) %>% 
  #   bindCache(input$typeselmer,df, company())
  # 
  # growth.mer.sel<-reactive({
  #   req(scored.mer.sel(),input$valselmersel)
  #   score.growth(scored.mer.sel(), kpi=input$valselmersel)
  # }) %>% 
  #   bindCache(scored.mer.sel(),input$valselmersel)
  # 
  # z<- reactive({
  #   req(input$valselmersel,scored.mer.sel())
  #   if(input$valselmersel=="valscore"){
  #     scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$valscore
  #   } else {
  #     if(input$valselmersel=="repscore"){
  #       scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$repscore
  #     } else {
  #       scored.mer.sel()[scored.mer.sel()$monthid==max(scored.mer.sel()$monthid),]$milscore
  #     }
  #   }
  #   
  # }) %>% 
  #   bindCache(input$valselmersel,scored.mer.sel())
  # 
  # observeEvent(c(z(),growth.mer.sel(),scored.mer.sel()),{
  #   
  #   mod_histo_server("milestypemersel", x=z(), xt="Score", yt="Frequency")
  #   output$scoretitlemersel<-renderText(paste0("Customer score for the month ",max(scored.mer.sel()$monthid)))
  #   mod_infoboxcollection_server("scoredeltamersel",growth.mer.sel())
  # })
  
  # dtsm<- reactive({
  #   print(company())
  #   req(input$shinymanager_where)
  #   print(company())
  #   df[df$merchant==company(),]
  # })
  
  #observeEvent(dtsm(),{
    mod_scorer_server("companyselected",dtm(),merc=company())
  #})
  
  
  segment.self.dat<- reactive(
    {
      req(company(),df)
      segmentd(df, company())
    }
  ) %>% 
    bindCache(company(), df)
  
  segment.loc.dat<- reactive(
    {
      req(company(),df)
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
                                      req(company(),df)
                                      segmentd(df, company(),a=0)
                                    }
  ) %>% 
    bindCache(company(), df)
  
  segment.loc.oth.dat<- reactive(
                                   {
                                     req(company(),df)
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
  
  observeEvent(input$newrephelp,
               {
                 showModal(
                   modalDialog(
                     title = "New vs Old",
                     size = "l",
                     easyClose = T,
                    "This gives you an idea about number of transactions (and/or number of customers and/or amount) in your business from new customers vis-a-vis old customers. The intention is to increase the business from repeat purchase. This is because acquiring new customer is far more expensive. This also gives you an idea of progess MoM so that you can track progress."
                   )
                 )
               }
               )
  
  observeEvent(input$sowhelp,
               {
                 showModal(
                   modalDialog(
                     title = "Share of wallet",
                     size = "l",
                     easyClose = T,
                     "Business can be enhanced by increasing the amount customers spend in your product/sevice. Share of wallet gives you an idea about what percent of monthly spend does your customer spend on you. A lower share of wallet indicates opportunity loss. And decreasing share of wallet can be an indicator of some major underlying issue."
                   )
                 )
               }
  )
  
  observeEvent(input$scorehelp,
               {
                 showModal(
                   modalDialog(
                     title = "Score distribution",
                     size = "l",
                     easyClose = T,
                     "According to one of ouur clients, this is the most valuable information/insight they ever came across, related to customer behaviour. Consider share of wallet; assume that average spend per transaction is $100 in your shop. Does that mean everyone is spending $100 per transaction? Most likely, no. Some are spending probably $10, whereas some may be spending $250. If you build your strategy based on the average of $100, it will never cater to the needs of those who are spending $10 or $250. You must, divide them into groups and build different strategies for each group. So, very low paying groups may need a specific strategy for them to increase their spend per transaction.
                     The distribution show here serves similar purpose. You can see how many customers are low scoring and how many are high or medium. Thereby giving you opportunity to formulate more targetted strategy."
                   )
                 )
               }
  )
  
  observeEvent(input$selfbeh,
               {
                 showModal(
                   modalDialog(
                     title = "Behaviour of your customer",
                     size = "l",
                     easyClose = T,
                     "If you want your customers to come more often or spend more money in your business, you need to know their behaviour. May be they are not coming because of your geographic location. In one of the visuals here, you will be able to check where are they mostly shopping from to verify your assumption and formulate your strategy.
                     Or, you may want to find out in which segment are they spending more or in which segment are they making more transactions. Then you may formulate your strategy accordingly."
                   )
                 )
               }
  )
  
  observeEvent(input$othbeh,
               {
                 showModal(
                   modalDialog(
                     title = "Behaviour of your customer",
                     size = "l",
                     easyClose = T,
                     "If you want to bring in new customers, then also, much like your existing customers, you need to understand where and how are they spending. Perhaps, to launch new offer or launch a new marketing strategy or tie-up with other businesses where they are already going!"
                   )
                 )
               }
  )
  
}
