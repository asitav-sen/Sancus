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
      color = "primary",
      href = "https://adminlte.io/themes/v3",
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
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
      href = "https://github.com/asitav-sen/fishstat2020",
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
  withProgress(
    message=sample(c("Learning to play guitar", "Smoking a cigar", "Burning some buildings", "Dealing in Dark-sided stuff", "Pretending to do a lot of work"),1,T,prob = c(0.20,0.20,0.20,0.20,0.20)),
    detail="You probably don't wanna know how",
    value=0,
    {
      setProgress(value=1,message="Finding wierd stuff in the internet")
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
      
      setProgress(value=2,message="Gained access!")
      
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
  fluidRow(
    h5("Overall Score"),
    box(
      title = textOutput("scoretitle"),
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
        fluidRow(
          column(
            width=6,
            radioButtons("typesel","Select Transaction Type", 
                         choiceNames = c("Miles Earned","Miles Redeemed"),
                         choiceValues = c(0,1), selected = 0, inline = T)
          ),
          column(
            width=6,
            radioButtons("valsel","Select score based on", 
                         choiceNames = c("Overall","Transaction Value","Transaction Frequency"),
                         choiceValues = c("valscore","repscore","milscore"), selected = "valscore", inline = T)
          )
        ),
        fluidRow(
          column(
            width = 10,
            mod_histo_ui("milestype1")
          ),
          column(
            width = 2,
            mod_infoboxcollection_ui("scoredelta")
          )
        )
    )
  ),
  fluidRow(
    h5("Scores by Merchant"),
    box(
      title = textOutput("scoretitlemerove"),
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
      tagList(
        fluidRow(
          column(
            width = 4,
            selectizeInput("merove", "Select Merchant",choices=merc, selected="merchant1")
          ),
          column(
            width=4,
            radioButtons("typeselmerove","Select Transaction Type", 
                         choiceNames = c("Miles Earned","Miles Redeemed"),
                         choiceValues = c(0,1), selected = 0, inline = T)
          ),
          column(
            width=4,
            radioButtons("valselmerove","Select score based on", 
                         choiceNames = c("Overall","Transaction Value","Transaction Frequency"),
                         choiceValues = c("valscore","repscore","milscore"), selected = "valscore", inline = T)
          )
        ),
        fluidRow(
          column(
            width = 10,
            mod_histo_ui("milestypemerove")
          ),
          column(
            width = 2,
            mod_infoboxcollection_ui("scoredeltamerove")
          )
        )
      )
    )
    
  )
  )
}


#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @importFrom dplyr filter mutate group_by ungroup row_number n arrange lag summarize
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms ceiling_date
#'
#' @noRd
milesummary<- function(df){
  df %>% 
    mutate(TTime=ymd_hms(TTime)) %>% 
    mutate(monthid=ceiling_date(TTime,"month")) %>% 
    group_by(monthid) %>% 
    summarize(miles=sum(miles)) %>% 
    filter(miles>1000000)
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
tabmstory<- function(){
  fluidRow(
    box(
      title = "Miles by month",
      solidHeader = T,
      width = 12,
      headerBorder = T,
      status ="secondary",
        fluidRow(
            radioButtons("typeselmiles","Select Transaction Type", 
                         choiceNames = c("Miles Earned","Miles Redeemed"),
                         choiceValues = c(0,1), selected = 0, inline = T),
            mod_barchart_ui("milsumall")
        )
    )
  )
}







