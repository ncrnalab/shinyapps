library (shiny)
library (plotly)
library (tidyverse)
library (shinydashboard)
library (stringi)

  if (rstudioapi::isAvailable()) {
  setwd (dirname (rstudioapi::getActiveDocumentContext()$path))
}
  
source ("mapDK.R")

load ("data/municipality.rda")


ui <- dashboardPage(
  title = "Corona in DK",
  
  dashboardHeader(
    title = span(img(src = "covid19_transparent.png", height = 35), "Corona in DK")
  ),
  
  dashboardSidebar(width=0),
  
  dashboardBody(
  
    tags$head(tags$script("$( document ).ready(function() { $('#notification').html ('fetching data from SSI...'); })")),
    
    tags$head(tags$style(type="text/css", ".sidebar-toggle {visibility: hidden;}")),
    

    fluidRow(
      box (width=10,
        HTML (   
          paste ("<font size=5>National and regional data on corona spread from <a href='https://www.ssi.dk/sygdomme-beredskab-og-forskning/sygdomsovervaagning/c/covid19-overvaagning/arkiv-med-overvaagningsdata-for-covid19'>SSI</a>.</font><br/> ",
                       "<font size=4>Simply click the map for local COVID-19 test data. The data is updated automatically (typically released from SSI around 2pm (CET) on weekdays). ", 
                       "P-values are calculated by simple one-sided binomial tests with number of positives (successes), ", 
                       "number of tests (trials), and 1-specificity as the probability of success. ",
                       "Note: P-values have not been corrected for multiple testing. ",
                       "Code available at <a href='https://github.com/ncrnalab/shinyapps'>github</a></font>", sep=""))
          
      ),

      box (width=2,
           
           sliderInput("specificity", "Test specificity", min = 0.9, max = 1, value=0.995, step=0.001)
           
      )
    ),
    fluidRow (
      box (width=12,
      
        htmlOutput ("notification")
        
      )
      
    ),
    
    fluidRow(
     
      box (width = 6,
           
         htmlOutput ("regional_text"),
         
         plotlyOutput("map", height=600) #plotlyOutput("national", height=600)
         
         
      ),
      
      box (width=6,

         plotlyOutput("burden", height=632)
          

      )
    )
  )
)

# 
server <- shinyServer(function(input, output, session) {

  
  
  # hack to avoid constant reconnect...
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  
  traces_map <- c()
  
  region <- reactiveVal ("Denmark")
  
  
  # theme for ggplot
  ggtheme <- theme_bw() +
    theme (
      panel.background = element_rect(fill = "white", color="grey", size=1),
      axis.title  = element_text(size = 15, color = "black"),
      axis.text.y = element_text(size = 13, color = "black"),
      axis.text.x = element_text(size = 13, color = "black"),
      strip.text  = element_text(size = 15, color = "black"),
      legend.title = element_text(size = 14, color = "black"),
      legend.text = element_text(size = 13, color = "black"),
      legend.background = element_rect(fill = "transparent", colour = "transparent"),
      strip.background = element_blank()
      
    )
  
 
  labeller_general <- c(pct_pos = "Fraction of positive tests", ntested = "Number of tests", npositive = "Number of new positives")
  plegend <- c("TRUE" = "Corona detected (p<0.05)", "FALSE" = "Maybe just false positives", "NA" = "NA")
  pcolor <- structure (c("#d62728", "#2ca02c", "grey"), names = as.character (plegend))
  
  
  char_fix <- function (x) {
  
    x <- gsub ("Ã.", "ø", x)
    x <- gsub ("ørø", "Ærø", x)
    x <- gsub ("Holbøk", "Holbæk", x)
    x <- gsub ("Halsnøs", "Halsnæs", x)
    x <- gsub ("Løsø", "Læsoe", x)
    x <- gsub ("Lyngby.Taarbøk", "Lyngby.Taarbæk", x)
    x <- gsub ("Nøstved", "Næstved", x)
    x <- gsub ("Tørnby", "Tårnby", x)
    x <- gsub ("Vallensbøk", "Vallensbæk", x)
    x <- gsub ("Copenhagen", "København", x)

    x
    
  }
  
  
  # one-sided binomial test
  b.test <- function (x, n, p) {
    
    out <- apply (cbind (x, n), 1, FUN=function (b) {
      
      if (is.na (b['x']) || b['x']<=0 || is.na (b['n']) || b['n'] <= 0) {
        1
      } else {
        binom.test (b['x'], b['n'], p=p, alternative="greater")$p.value
      }
      
    })
    out
    
  }
  

  get_report_date <- function (result) {

    suppressWarnings (unlist(sapply (strsplit (result, "-|_"), function (x) {

       x[which (nchar(x) == 8 & sapply (x, function (y) !is.na(as.numeric(y))))]
      
    })))
    
  }
  
  # Retrieving data from SSI homepage
  get_data <- reactive({
    
    print ("get_data")
    
    
    withProgress(message = 'Downloading data', value = 0, {
      
      
      # getting url for data
      
      thepage = readLines('https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata')
      thepage <- unlist (strsplit (thepage, "</a><a"))
      
      lines <- grepl ("https", thepage) & 
               grepl ("overvagning", tolower(thepage)) & 
               (grepl ("data-epidemi", tolower(thepage)) |
                grepl ("overvaagningsdata-covid19", tolower(thepage)))
      
      
      datalines <- thepage[lines]
      
      getexpr = function(s,g) substring(s,g,g+attr(g,'match.length')-1)
      
      mypattern <- 'href="(.*?)"(.*)'
      
      gg = gregexpr(mypattern,datalines)
      
      matches = mapply(getexpr, datalines, gg)
      
      result = as.character(gsub(mypattern,'\\1',matches))
      result <- result[result != ""]

      print ("downloading data")
      
      report_date <- get_report_date (result)
      
      # remove duplicated entries
      result <- result[!duplicated (report_date)]
      report_date <- report_date[!duplicated (report_date)]
      
      
      # sort entries with most recent entry first
      result <- result[order (as.Date(report_date, format="%d%m%Y"), decreasing = T)]
      
      map (result[1], function (x) {
        
        print (x)
        url <- x #paste ("https://files.ssi.dk/Data", x, ".zip", sep="")
        
        url[!grepl (".zip", url)] <- paste (url[!grepl (".zip", url)], ".zip", sep="") 
        
        url <- gsub ("\\?la=da", "", url)  # adaptation for the constant changes in data from SSI...
        
        url <- gsub ("\t", "", url)         # now SSI also includes tabs in url...
        
        report_date <- get_report_date (x)
        
        data <- list()
        data$date <- report_date
        data$valid_date <- nchar(report_date) == 8
        
        dest <- paste ("data/data", report_date, ".zip", sep="")
        
        data$file <- dest
        
        if (!file.exists (dest)) {  # only download if not downloaded before
          
          cat(file=stderr(), "downloading ", dest, "\n")
          
          tryCatch({
            download.file(url, dest)
            data$found = T
            
          }, error=function (e) {
            print (e)
            data$found = F
            
          })
        }
        
        data
        
      })
      
    })
    
  })
  
  #Extract national info from the most recent data
  
  get_national <- reactive({
    
    print ("get_national")
    
    withProgress(message = 'Preparing national data', value = 0.3, {
      
      ld <- get_data ()
      
      data <- ld[[1]] # most recent data
      
      tryCatch ({
      
          df.tests <- read.table(unz(data$file, "Test_pos_over_time.csv"), sep=";", header=T, dec=",", colClasses="character")
          
          colnames (df.tests) <- c("date", "new_pos", "not_prev_pos", "pos_pct", "prev_pos", "ntested", "cumulative_ntested")
          
          df.tests$date <- as.Date(df.tests$date)
          df.tests <- df.tests %>% filter (!is.na (date))
          
          suppressWarnings (df.tests[,-1] <- apply (df.tests[,-1], 2, function (x) as.numeric(gsub (",", ".", gsub ("\\.", "", x)))))
          
          df.tests <- df.tests %>% group_by (date) %>%
            mutate (npositive = new_pos,
                    pct_pos = npositive / ntested,
                    pct_prev_pos = prev_pos / ntested)
          
          return (df.tests)
          
        }, error=function(cond) {
          
          print (cond)
          return (NULL)
          
        }, finally={}
      )
    })
  })

  
  
  
  get_regional <- reactive({
    
    ld <- get_data ()
    
    data <- ld[[1]] # most recent data
    
    df.regional <- tryCatch ({
      
      df.tests <- read.table(unz(data$file, "Municipality_tested_persons_time_series.csv"), sep=";", header=T, dec=",", colClasses="character")
      #df.tests <- read.table(unz(data$file, "Municipality_tested_persons_time_series.csv"), sep=";", header=T, dec=",")
      
      
      colnames (df.tests)[1] <- "date"
      df.tests$date <- as.Date (df.tests$date)
      
      colnames (df.tests) <- char_fix (colnames (df.tests))
      
      df.tests.m <- df.tests %>%
        gather (municipal_name, ntested, -1)  %>%
        mutate (ntested = as.integer (ntested))#"PrDate_adjusted")
      
      
      df.pos <- read.table(unz(data$file, "Municipality_cases_time_series.csv"), sep=";", header=T, dec=",", colClasses="character")
      
      colnames (df.pos)[1] <- "date"
      df.pos$date <- as.Date (df.pos$date)
      
      colnames (df.pos) <- char_fix (colnames (df.pos))
      
      
      df.pos.m <- df.pos %>%
        gather (municipal_name, npositive, -1) %>%
        mutate (npositive = as.integer (npositive)) #"PrDate_adjusted")
      
      df <- merge (df.tests.m, df.pos.m, by=c("date", "municipal_name")) 
      
      #df$municipal_name <- remove_dk (df$municipal_name) 
      df$pct_pos <- df$npositive / df$ntested
      
      df
      
    }, error=function(cond) {
      
      print (cond)
      return (NULL)
      
    }, finally={})
    
    
    df.regional
    
    
  })
  
  
  get_regional_old <- reactive({
    
    print ("get_regional")
    
    
    cat(file=stderr(), "process regional data....", "\n")
    
    withProgress(message = 'Preparing regional data', value = 0.6, {
      
      ld <- map (get_data (), function (data) {
      
        tryCatch ({
          
            df.regional <- read.table(unz(data$file, "Municipality_test_pos.csv"), sep=";", header=T, dec=",", colClasses="character", encoding="UTF-8")
            
            suppressWarnings (df.regional[,-2] <- apply (df.regional[,-2], 2, function (x) as.numeric(gsub (",", ".", gsub ("\\.", "", x)))))
            
            colnames (df.regional) <- c("municipal_id", "municipal_name", "cumulative_ntests", "cumulative_npositive", "population", "cumulative_incidence")
            
            df.regional[is.na (df.regional)] <- 0
            
            df.regional$date <- as.Date(as.character(data$date), format = c("%d%m%Y"))
           
            data$df.regional <- df.regional
            
            return (data)
          
          }, error=function (cond) {
            print (cond)
            return (NULL)
          }, finally={}
        )
      
      })
      
      # bind into one data.frame
      
      df.regional <-
        map (ld, function (data) {
          return (data$df.regional)
          
        }) %>% bind_rows()
      
      
      # Get the incremental numbers from cumulative data
      
      df.regional <- 
        df.regional %>% arrange (date) %>% group_by (municipal_name) %>%
        mutate (ntested = cumulative_ntests - lag (cumulative_ntests),
                npositive = cumulative_npositive - lag (cumulative_npositive),
                pct_pos = npositive / ntested)
     
      df.regional
      
    }) 
  })
  
  
  # Get the most recent regional data for alert
  
  get_recent_regional <- reactive({
    
    print ("get_recent_regional")
    
    df.regional <- get_regional ()
    
    # sum the last 5 days
    recent_dates <- unique (df.regional$date) %>% tail (5)
    print (recent_dates)
    
    df.recent <- df.regional %>% filter (date %in% recent_dates) %>%
      group_by (municipal_name) %>%
      summarize (ntested = sum(ntested),
                 npositive = sum (npositive),
                 date = recent_dates %>% tail(1)) %>%
      mutate (pct_pos = npositive / ntested,
              date = recent_dates %>% tail(1))
    
    df.recent
    
    
  })
  
  get_recent_regional_p <- reactive ({
    
    print ("get_recent_regional_p")
    
    df.recent <- get_recent_regional ()
    
    df.recent %>% mutate (p = b.test (npositive, ntested, 1-input$specificity))
    
    
  })
  
 
  output$burden <- renderPlotly ({
    
    print ("PLOTTING: burden")
  
    if (region () == "Denmark") {
      df.region <- get_national()
      df.region$municipal_name <- "Denmark"
    } else {
       df.region <- get_regional() %>% filter (tolower(municipal_name) == tolower(region()))
    }
    
    if (nrow (df.region) == 0) {
      return ()
    }
    
    data.m <- df.region %>%
      group_by (date) %>%
      mutate (p = b.test (npositive, ntested, 1-input$specificity), npos=npositive) %>%
      gather (var, val, c("npositive", "pct_pos", "ntested"))
    
    data.m$var <- factor (data.m$var, levels = c("npositive", "pct_pos", "ntested"))
    data.m$legend <- factor (plegend[as.character (data.m$p < 0.05)], levels = as.character (plegend))
    
    # Compare with last year...
    
    sep_dates <- separate(data.m, "date", c("Year", "Month", "Day"), sep = "-")
    data.m$year <- sep_dates$Year
    data.m$day <- as.Date (paste (sep_dates$Month, sep_dates$Day, sep="-"), format="%m-%d") 
    
    
    g <- ggplot () + 
      geom_line(data=data.m %>% filter (var != "ntested"), aes (x=day, y=val, group=paste (municipal_name, year)), color="black", alpha=0.3) + 
      geom_point(data=data.m %>% filter (var != "ntested"), aes (x=day, y=val, text=paste ("Number of positives:", npos), color = legend, alpha=year)) + 
      scale_color_manual(values = pcolor) + 
      geom_line(data=data.m %>% filter (var == "ntested"), aes (x=day, y=val, alpha=year), color="black") + 
      facet_wrap (~var, nrow=3, scales="free_y", labeller = as_labeller(labeller_general)) +
      labs (x="", y="", color="") +
      ggtheme + theme (legend.position = "none")
    
    
    ggplotly (g) 
    
    
  })
  
  
  output$notification <- renderText ({
    
    print ("notification...")

    df.recent.regional <- get_recent_regional_p () 
  
    if (nrow (df.recent.regional) == 0) {
      return ()
    }
    
    recent_date <- unique (df.recent.regional$date)
    
    text <- paste ("<font size=4>Latest report: <b>", recent_date, ". </b></font>", sep="")
    
    df.recent.regional <- df.recent.regional %>% filter (p < 0.05)
    
    alert <- ""
    
    image <- "<img src='alert.png' height='15px' valign='top' style='position: relative; top: -3px;'/>"
    
    if (nrow (df.recent.regional) > 0) {
      
      alert <- paste (image, "<font size=4><b>Alert</b>: Corona recently detected in", paste (sort(df.recent.regional$municipal_name), collapse=", "), "</font>")
      
    } 
    
    
    return (ifelse (alert == "", text, paste (text, alert, sep=" ")))
    
    
  })
  
  
  output$regional_text <- renderText ({
    
     paste ("<font size=5>Selected region: <b>", region (), "</b></font>")
    
  })
  
  
  
  get_map_data <- reactive ({
    
    print ("get_map")
    municipality
    
    
  })

    
    
  output$map <- renderPlotly({
    
    withProgress(message = 'Drawing map', value = 0.3, {
      
      
      print ("PLOTTING: map")
      
      req (get_recent_regional (), get_map_data ())
      
      df.recent <- get_recent_regional ()
      df.map <- get_map_data ()
      
      g <- mapDK (df.map, id="municipal_name", values = "pct_pos", data = df.recent)
      
      
      ggplotly(g, tooltip="key")
    })
    
  })
  
  observe({
    
    d <- event_data("plotly_click")

    if (!is.null(d)) {
  
      if ("key" %in% colnames (d)) {
        
         print ("event click")
        
         # set region
         region (d$key)
         
         df.map <- get_map_data ()
         
         plot <- plotlyProxy("map", session) 
         
         
         if (length (traces_map) > 0) {
           
           ntraces <- nrow (df.map %>% group_by (id) %>% summarize (n=n())) + 2
           
           plot <- plot %>% plotlyProxyInvoke ("deleteTraces", ntraces)
         
           traces_map <<- head (traces_map, -1)
           
         }
         
         shapedata <- df.map %>% filter (onlyChar (id) == onlyChar (d$key)) %>% mutate (municipality=d$key)
         
         if (nrow (shapedata) > 0) {
           
           shapedata$municipality <- d$key
             
           gg <- ggplot (shapedata, aes (x=long, y=lat, group=group, key=municipality)) + 
             geom_polygon(color = "red", fill=NA)
             
           plot <- plot %>%
             plotlyProxyInvoke("addTraces", plotly_build(gg)$x$data)
           
           
           traces_map <<- c(traces_map, structure (length (traces_map)+1, names=d$key))
           
         }
         
         print (traces_map)
         
         plot
        
      } 
    }
    
  })
  
})


shinyApp(ui = ui, server = server)

