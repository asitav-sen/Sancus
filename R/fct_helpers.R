#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
headr<-function(){
  dashboardHeader(
    title=dashboardBrand(
      title = "Sancus",
      color = "gray-dark",
      href = "https://www.fortuna.club/",
      image = "./www/logo-fortuna-color.png"
    ),
    skin="dark"
  )

}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
sidebr<-function(){
  dashboardSidebar(
    collapsed=T,
    id="sidebar",
    mod_dmenu_ui("mmenu")
    
    )
}

#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
footr<-function(){
  dashboardFooter(
    left = a(
      href = "https://asitavsen.com",
      target = "_blank", "Build a dashboard like this"
    ),
    right = a(
      href = "https://github.com/asitav-sen/Sancus",
      target = "_blank", "Code in Github. MIT license"
    )
  )
}

#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
scaler <- function(x){(x-min(x))/(max(x)-min(x))}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number arrange
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
scorer.all<-function(df, ttype=0){
  req(df)
  ms=c("Giving flying lessons to Superman", "Tickling Genghis Khan", "Burning some buildings", 
       "Dealing in Dark-sided stuff", "Scanning for virus", "Scanning for covid virus", "Discussing new car design with Musk",
       "Spraying sanitizer in your room", "In a planchet session with Hitler")
  withProgress(
    message=sample(ms,1,T),
    detail="You probably don't wanna know how",
    value=0,
    {
      setProgress(value=1,message=sample(ms,1,T))
      scores.overall<-
        df%>%
        filter(TType==ttype)%>%
        mutate(TTime=ymd_hms(TTime)) %>% 
        group_by(CustomerId) %>%
        mutate(firstdate=min(TTime)) %>%
        mutate(months= ceiling(as.integer(difftime(TTime,firstdate, units = "days"), units="days")/30)) %>%
        mutate(Tranno=row_number(), Totmiles=cumsum(miles)) %>%
        mutate(tranpermonth=ifelse(months==0,0,Tranno/months), milespermonth=ifelse(months==0,0,Totmiles/months)) %>%
        ungroup() %>%
        mutate(monthid=ceiling_date(TTime,"month")) %>%
        group_by(CustomerId,monthid) %>%
        filter(TTime==max(TTime)) %>%
        ungroup() %>% 
        group_by(monthid) %>%
        mutate(repscore=scaler(tranpermonth), milscore=scaler(milespermonth))%>%
        mutate(valscore=scaler(repscore+milscore)) %>% 
        filter(!is.na(repscore)) %>% 
        filter(!is.na(milscore)) %>% 
        filter(!is.na(valscore)) %>% 
        filter(!is.nan(repscore)) %>% 
        filter(!is.nan(milscore)) %>% 
        filter(!is.nan(valscore))
      
      setProgress(value=2,message=sample(ms,1,T))
      
      ar<-as.numeric(quantile(scores.overall$repscore,0.75))
      br<-as.numeric(quantile(scores.overall$repscore,0.25))
      
      am<-as.numeric(quantile(scores.overall$milscore,0.75))
      bm<-as.numeric(quantile(scores.overall$milscore,0.25))
      
      av<-as.numeric(quantile(scores.overall$valscore,0.75))
      bv<-as.numeric(quantile(scores.overall$valscore,0.25))
      
      setProgress(value=3,message="Oh. Your pet is dead!")
      
      scores.overall<-
        scores.overall %>% 
        mutate(repcat=ifelse(repscore<=br, "low", ifelse(repscore<=ar,"medium","high")), 
               valcat=ifelse(valscore<=bv, "low", ifelse(valscore<=av,"medium","high")), 
               milcat=ifelse(milscore<=bm, "low", ifelse(milscore<=am,"medium","high")))
      
      setProgress(value=4,message="Sorry, gotta go")
    }
  )
  print(scores.overall)
  scores.overall
}

#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
score.growth<-function(df, kpi="valcat"){
  req(df)
  withProgress(
    message = "He he..",
    detail = "Peek-a-boo",
    value = 0,
    {
      setProgress(value=1,message="Aah! I got it now. You don't have friends!")
      
      if(kpi=="valscore"){
        gr<-df %>% 
          group_by(valcat,monthid) %>% 
          summarise(count=n()) %>% 
          mutate(count.lag=lag(count,1)) %>% 
          mutate(growth=round((count-count.lag)*100/count.lag,2)) %>% 
          mutate(growth=ifelse(is.na(growth),0,growth)) %>% 
          ungroup() %>% 
          filter(monthid==max(monthid)) %>% 
          select(1,5)
      } else {
        if(kpi=="repscore") {
          gr<-df %>% 
            group_by(repcat,monthid) %>% 
            summarise(count=n()) %>% 
            mutate(count.lag=lag(count,1)) %>% 
            mutate(growth=round((count-count.lag)*100/count.lag,2)) %>% 
            mutate(growth=ifelse(is.na(growth),0,growth)) %>% 
            ungroup() %>% 
            filter(monthid==max(monthid)) %>% 
            select(1,5)
        } else {
          gr<-df %>% 
            group_by(milcat,monthid) %>% 
            summarise(count=n()) %>% 
            mutate(count.lag=lag(count,1)) %>% 
            mutate(growth=round((count-count.lag)*100/count.lag,2)) %>% 
            mutate(growth=ifelse(is.na(growth),0,growth)) %>% 
            ungroup() %>% 
            filter(monthid==max(monthid)) %>% 
            select(1,5)
        }
      }
      setProgress(value=2,message="Happens.")
    }
  )

  names(gr)[1]<-"kpi"
  gr
}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
taboverall<- function(){
  df<-read.fst("./data/transactiondata.fst")
  merc<-unique(df$merchant)
  tagList(
    h5("Scores by Merchant"),
  fluidRow(
    selectizeInput("merc", "Select Merchant",choices=merc, selected="merchant1"),
    mod_scorer_ui("overall")
  )
  )
}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
repcal<- function(df,merc="merchant1", lim=2){
  df %>% 
    filter(merchant==merc) %>% 
    mutate(TTime=ymd_hms(TTime)) %>% 
    group_by(CustomerId) %>% 
    mutate(TTimelag=lag(TTime)) %>% 
    mutate(time.pur=as.integer(difftime(TTime,TTimelag, units = "days"))) %>% 
    mutate(rep.pur= ifelse(is.na(time.pur) | time.pur>lim,"New","Repeat") ) %>% 
    mutate(monthid=ceiling_date(TTime,"month")) %>% 
    group_by(monthid,rep.pur) %>% 
    summarize(count=n(), miles=sum(miles), customers=length(unique(CustomerId)))
}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
tabmhome<- function(){
  fluidRow(
    box(
      title = "Sales/Revenue",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      fluidRow(
        column(
          width = 6,
          "Transactions",
          mod_barchart_ui("salrevtr")
        ) %>%  tooltip(placement="top", title="Transactions by month. Click and drag to zoom. Hover for info"),
        column(
          width = 6,
          "Revenue/Miles",
          mod_barchart_ui("salrevmil")
        ) %>%  tooltip(placement="top", title="Miles by month. Click and drag to zoom. Hover for info")
      )
    ),
    box(
      title = "New vs. Returning",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      fluidRow(
        sliderInput("churnlim","Expected gap between purchase", min=1,max=400, value=30) %>% 
          tooltip(placement="top", title="If a customer does not return withing these many days, you consider her churned/lost.")
      ),
      fluidRow(
        column(
          width = 4,
          h5("Number of transactions"),
          mod_barchart_ui("newrep")
        )%>%  tooltip(placement="top", title="Click and drag to zoom. Hover for info"),
        column(
          width = 4,
          h5("Amount/Miles"),
          mod_barchart_ui("newrepmiles")
        )%>%  tooltip(placement="top", title="Click and drag to zoom. Hover for info"),
        column(
          width = 4,
          h5("Customers"),
          mod_barchart_ui("newrepclients")
        )%>%  tooltip(placement="top", title="Click and drag to zoom. Hover for info"),
        actionButton("newrephelp", "How does it help?")
      )
    ),
    box(
      title = "Share of Wallet",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      fluidRow(
        column(
          width = 6,
          "By Transaction",
          mod_linechart_ui("sowtr")
        )%>%  tooltip(placement="top", title="This is no. of transactions your customers 
                      makes at your outlet out of every 100 transaction s/he makes in a month."),
        column(
          width = 6,
          "By Amount/Miles",
          mod_linechart_ui("sowmil")
        )%>%  tooltip(placement="top", title="This is the amount your customer spends out of every 
                      $100 s/he spends every month.")
      ),
      actionButton("sowhelp", "How does it help?")
    ),
    mod_scorer_ui("companyselected"),
    actionButton("scorehelp", "How does it help?"),
    salescard()
  )
}



#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag case_when summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd

sowcal<-function(dff, merc="merchant1"){
  
  clist<- unique(dff[dff$merchant==merc,]$CustomerId)
  
  dff %>% 
    filter(CustomerId %in% clist) %>% 
    mutate(TTime=ymd_hms(TTime)) %>% 
    mutate(monthid=ceiling_date(TTime,"month"), self=ifelse(merchant==merc, "your","other")) %>% 
    group_by(monthid, self) %>% 
    summarize(miles=sum(miles), count=n()) %>% 
    mutate(sowmil=round(miles*100/sum(miles),2), sowtr=round(count*100/sum(count),2)) %>% 
    filter(self=="your")
  
}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag case_when summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd

salrevcal<-function(dff, merc="merchant1"){
  
  dff %>% 
    filter(merchant==merc) %>% 
    mutate(TTime=ymd_hms(TTime)) %>% 
    mutate(monthid=ceiling_date(TTime,"month")) %>% 
    group_by(monthid) %>% 
    summarize(count=n(), miles=sum(miles))
  
}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag case_when summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date ymd
#' @importFrom forcats fct_lump
#'
#' @noRd

segmentd<-function(dff, merc="merchant1", a=1){
  
  if(a==1){
    clist<- unique(dff[dff$merchant==merc,]$CustomerId)
  } else {
    clist<- unique(dff[dff$merchant!=merc,]$CustomerId)
  }
  
  dff %>% 
    filter(CustomerId %in% clist) %>% 
    mutate(TTime=ymd_hms(TTime)) %>% 
    filter(merchant!=merc) %>% 
    mutate(monthid=ceiling_date(TTime,"month")) %>% 
    mutate(Segment = fct_lump(Segment, n = 19)) %>% 
    group_by(monthid,Segment) %>% 
    summarize(count=n(), miles=sum(miles)) %>% 
    filter(monthid>ymd("2015-01-01"))
    
  
}



#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
tabpremium<- function(){
  fluidRow(
    box(
      title = "Our Customer's Behaviour",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      fluidRow(
          h5("Transactions by segment"),
          mod_barchart_ui("ownsegmenttrn", ht="400px", wt="1200px")) %>%  
        tooltip(placement="top", title="Which segments are your customers making transactions?"),
      fluidRow(
        h5("Amount/miles by segment"),
        mod_barchart_ui("ownsegmentmil", ht="400px", wt="1200px")
      )  %>%  
        tooltip(placement="top", title="How much are your customers spending in different segments?"),
      fluidRow(
        h5("Shopping Locations"),
        mod_mapplot_ui("ownsegmentloc")
      ) %>%  
        tooltip(placement="top", title="Where are your customers shopping from?"),
      actionButton("selfbeh","How Does this help?")
      ),
    box(
      title = "Our Prospect's Behaviour",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      fluidRow(
        h5("Transactions by segment"),
        mod_barchart_ui("othsegmenttrn", ht="400px", wt="1200px")) %>%  
        tooltip(placement="top", title="Which segments are your prospects (Those who do not buy from you) making transactions?"),
      fluidRow(
        h5("Amount/miles by segment"),
        mod_barchart_ui("othsegmentmil", ht="400px", wt="1200px")
      )%>%  
        tooltip(placement="top", title="How much are your prospects (Those who do not buy from you) spending in different segments?"),
      fluidRow(
        h5("Shopping Locations"),
        mod_mapplot_ui("othsegmentloc")
      )%>%  
        tooltip(placement="top", title="Where are your prospects (Those who do not buy from you) shopping from?"),
      actionButton("othbeh","How Does this help?")
    ),
    salescard()
    )
}


#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag case_when summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date ymd
#' @importFrom forcats fct_lump
#'
#' @noRd

segmentloc<-function(dff, merc="merchant1", a="1"){

  if(a=="1"){
    clist<- unique(dff[dff$merchant==merc,]$CustomerId)
  } else {
    dff<-dff[dff$TTime >="2020-06-01",]
    clista<- unique(dff[dff$merchant==merc,]$CustomerId)
    clistb<- unique(dff$CustomerId)
    clist<- setdiff(clistb,clista)
  }
  
  dff %>% 
    filter(CustomerId %in% clist) %>% 
    select(retailers, merchant, Segment, lat, long) %>% 
    unique() 

}
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag case_when summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date ymd
#' @importFrom forcats fct_lump
#'
#' @noRd

salescard<-function(){
  bs4UserCard(
    title = userDescription(
      title = "O-Ren Ishii",
      subtitle = "Poaching guru",
      type = 2,
      image = "https://i0.wp.com/dangerouslee.biz/wp-content/uploads/2019/04/Lexi-Montgomery.jpeg?resize=128%2C128&ssl=1",
    ),
    footer = actionButton("salescontact","Yes! I want to boost my revenues."),
    status = NULL,
    background = NULL,
    width = 12,
    height = NULL,
    collapsible = TRUE,
    collapsed = FALSE,
    closable = FALSE,
    maximizable = FALSE,
    gradient = FALSE,
    boxToolSize = "sm",
    elevation = NULL,
    headerBorder = TRUE,
    label = NULL,
    dropdownMenu = NULL,
    sidebar = NULL,
    id = NULL,
      h5("I can help you by\n"),
      tags$ol(
        tags$li("Sending targetted offers to your existing customers with low scores."), 
        tags$li("Identifying best location for your next outlet, based on shopping behaviour of your prospects."), 
        tags$li("Sending tagetted offers to your prospects (Those who are not your customers)."),
        tags$li("And a lot more.....\n")
      ),
      h5("Want to explore?")
  )
}
