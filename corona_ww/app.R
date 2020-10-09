library (plotly)
library (shiny)
library (ggplot2)
library (data.table)
library (dplyr)
library (tidyr)
library (RCurl)
library (minpack.lm)
library (DT)
library (shinydashboard)


# 
ui <- dashboardPage(
  title = "The corona pandemic of 2020",
  
  
  dashboardHeader(
  
    
    
    title = span(img(src = "covid19_transparent.png", height = 35), "COVID-19"),
    titleWidth = 400,
    dropdownMenu(
      #width =  400,
      type = "messages", 
      headerText = HTML(""), 
      icon = icon("question"), 
      badgeStatus = NULL,
      messageItem(
        from="ncrnapps.dk",
        message = tags$div("With a div tag and display: inline-block",
                                  tags$br(),
                                  "it appears to the right.",
                                  style = "display: inline-block; vertical-align: middle;")
      )
  
    )
  ),
  
  dashboardSidebar(
    
    width = 400,
    
    sidebarMenu(
      
        menuItem  (
          "WORLD",
          tabName = "ww"
          
        ),
        
        menuItem  (
          "USA",
          tabName = "us"
        ),
        
        id = "tabs"
        
    ),
    
    sidebarMenu (
      
        menuItem  (
          "GROUPS",
          checkboxGroupInput("groups", label=h4("Groups"),choices=c("cases", "death"), selected = c("cases", "death"))
          
        
        ),
        
        menuItem  (
          "SETTINGS",
          selectInput("axis", label = h4("Set axis as..."), 
                      choices = c("linear", "log10"), selected = "linear"),
          
          selectInput("relative", label = h4("Counts/Per capita..."), 
                      choices = c("Counts", "Per Capita"), selected = "Counts"),
          
          selectInput("plot", label = h4("Plot countries..."), 
                      choices = c("separated", "combined"), selected = "separated"),
          
          selectInput("cases", label = h4("Treat cases..."), 
                      choices = c("cumulative", "cases/day"), selected = "cumulative"),
          
          selectInput("date", label = h4("Set date... "), 
                      choices = c("absolute", "relative to day100cases"), selected = "absolute"),
          
          selectInput("regression", label = h4("Regression... "), 
                      choices = c("logistic", "exponential"), selected = "logistic"),
          
          
          uiOutput("dates_ui")
        
      )
    )
  ),
  
  dashboardBody(height=950,
    tags$head(
      tags$style(type="text/css", ".navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:800px;}")
    ),
    tags$head(
      tags$style(type="text/css", ".dataTables_filter {display: none;    }"
    )),

    fluidRow(
       box (width=12,
           
           HTML (paste ("<font size=3>Simply select countries and groups for analysis.", 
                                    "Also, in settings, choose date-interval of interest (or project into future), and/or select different types of analysis/visualization.", 
                                     "This is NOT made by a professional statistician or epidemiologist, and should not be a source of trust-worthy info.",
                                     "The raw data on the other hand is from <a href='https://github.com/CSSEGISandData/COVID-19'>Johns Hopkins CSSE</a>. And code is available from <a href='https://github.com/ncrnalab/shinyapps/tree/master/corona_ww'> GitHub</a>",
                                     "Suggestions and comments are welcomed at tbh@mbg.au.dk or @ncrnalab on twitter</font>"))
      )
      
    ),
    
    
    
    fluidRow(
      box (width=12,
       
        HTML (paste ("<b>Current stats.</b> Find for each country/state,",
                     "the number of total cases and deaths, cases and death per million capita (pmc)",
                     "and the daily increment of cases/death for the last 5 days per million capita.")),  
        DT::DTOutput("mytable"), 
        style = "height:500px; width:100%; overflow-y: scroll;overflow-x: scroll;" 
      )
      
    ),
       
    fluidRow (box (
       width=12,
       
       htmlOutput ("timecurve_text"),
       
       plotlyOutput("timecurve", height=600)
    )),
    
    fluidRow (
       box (
         
         htmlOutput ("mortal_text"),
         
         plotlyOutput("mortal", height=300)
         
       ),
       box (
         
         htmlOutput ("doubling_text"),
         
         plotlyOutput("doubling", height=300)
         
       )
    )
  )  
)
     
  
# options (shiny.trace=F)

# theme for ggplot
ggtheme <- theme_bw() +
  theme (
    panel.background = element_rect(fill = "white", color="grey", size=1),
    axis.title  = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    strip.text  = element_text(size = 12, color = "black"),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = "transparent", colour = "transparent"),
    strip.background = element_blank()
    
  )







# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  
  # prepare current population to match data from Johns Hopkins...  
  current.population <- population %>% arrange (-year) %>% group_by (country) %>% top_n (year, n=1)
  
  current.population$country <- gsub("Brunei Darussalam", "Brunei", current.population$country)
  current.population$country <- gsub("Viet Nam", "Vietnam", current.population$country)
  current.population$country <- gsub("Russian Federation", "Russia", current.population$country)
  current.population$country <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", current.population$country)
  current.population$country <- gsub("Iran (Islamic Republic of)", "Iran", current.population$country)
  current.population$country <- gsub("Congo", "Congo (Kinshasa)", current.population$country)
  current.population$country <- gsub("United States of America", "US", current.population$country)
  current.population$country <- gsub("Republic of Korea", "Korea, South", current.population$country)
  current.population$country <- gsub("West Bank and Gaza Strip", "West Bank and Gaza", current.population$country)
  current.population$country <- gsub("Czech Republic", "Czechia", current.population$country)
  current.population$country <- gsub("Iran (Islamic Republic of)", "Iran", current.population$country)
  
  
  
  
  
  
  
  
  
  spop <- structure (current.population$population, names = current.population$country)
  
  # hack to avoid constant reconnect...
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  selection <- list (
    ww = c("Denmark", "Sweden"),
    us = c("New York")
  )
  
  
  grps <- reactive({ input$groups }) %>% debounce(800)
  
  
  output$dates_ui <- renderUI({
    
     dates <- get_dates ()
     
     print (dates)
  
     sliderInput("dates", h4("Dates:"),
                 min = dates[1],
                 max = dates[3],
                 value = dates[1:2],
                 timeFormat="%Y-%m-%d")

    
  })
  
  get_dates <- reactive ({

    
    df.data.m <- get_data ()

    c(as.Date(min (df.data.m$date),"%Y-%m-%d"), 
      as.Date(max (df.data.m$date),"%Y-%m-%d"),
      as.Date(max (df.data.m$date+40),"%Y-%m-%d"))

    
  })
  
  
  
  get_data <- reactive ({
    
    print ("get_data")

    withProgress(message = 'Retrieving data...', value = 0, {
        
      
      if (input$tabs == "ww") {
      
        df.cases <- as.data.frame (fread(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")))
        incProgress(0.25)
        df.death <- as.data.frame (fread(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")))
        incProgress(0.25)
        
        
      } else if (input$tabs == "us") {
      
        
        df.cases <- as.data.frame (fread(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")))
        incProgress(0.25)
        df.death <- as.data.frame (fread(getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))
        incProgress(0.25)
        
        
        colnames (df.cases)[7] <- "Country/Region"
        colnames (df.death)[7] <- "Country/Region"
      
        df.pop <- df.death %>% group_by (`Country/Region`) %>%
          summarize (pop = sum (Population))
        
        spop[df.pop$`Country/Region`] <<- df.pop$pop
        
        
      }
      
      
      df.cases.m <- melt_cases (df.cases, "cases")
      df.death.m <- melt_cases (df.death, "death")
      
      incProgress(0.25)
      
  
      df.data <- rbind (df.cases.m, df.death.m) %>%
        mutate (date    = as.Date (time, format = "%m/%d/%y"),
                reldate = as.numeric (date - min (date)),
                type    = factor (type, levels=c("cases", "death")))
      
      incProgress(0.25)
      
      df.data
    }) 
  })

  
  
  melt_cases <- function (df, type="cases") {
    
    df %>% 
      gather (time, cases, contains ("/20")) %>%
      group_by (country=`Country/Region`, time) %>%
      summarize (cases = sum(cases), type=type) 
    
  } 
  

  # Function for fitting model  
  fitmodel <- function (df.fitdata, lower_A = 0, upper_A = Inf, lower_B=-Inf, upper_B = Inf, lower_C = 0.1, upper_C = Inf, exponential = F) {
    
    print (paste ("fitmodel", nrow (df.fitdata)))
    
    lreg <- list (model=NA, 
                  data=data.frame (intercept = NA, intercept_std <- NA, slope = NA, 
                                   A = NA, A_std = NA, 
                                   B = NA, B_std = NA,
                                   C = NA, C_std = NA,
                                   D = NA, D_std = NA,
                                   xintercept = NA))
    
    if (nrow (df.fitdata) >= 5) {
      
      weights = (df.fitdata$reldate - min (df.fitdata$reldate))
      
      model <- nlsLM (cases ~ B / (1+2^((A-reldate)/C)), data = df.fitdata, control = nls.lm.control (maxiter = 200),
                      start = list(A=max(df.fitdata$reldate), B=max (df.fitdata$cases)*2, C=3),
                      #weights = weights,
                      lower = c(lower_A, lower_B, lower_C),
                      upper = c(upper_A, upper_B, upper_C))

      
      # A: infliction
      # B: Max
      # C: Doubling rate
      
      # Save the coefficients
      
      sm <- summary (model)
      
      lreg$data$A <- sm$coefficients["A",1]
      lreg$data$A_std <- sm$coefficients["A",2]
      
      lreg$data$B <- sm$coefficients["B",1]
      lreg$data$B_std <- sm$coefficients["B",2]
      
      lreg$data$C <- sm$coefficients["C",1]
      lreg$data$C_std <- sm$coefficients["C",2]
      
      lreg$model <- model
      
      if (exponential) {
        
        df.expdata <- df.fitdata %>% filter (reldate <= lreg$data$A)
        model_exp <- nlsLM (cases ~ D * 2^(reldate/E), data = df.expdata, control = nls.lm.control (maxiter = 100),
                            start = list(E=lreg$data$C, D=min (df.expdata$cases)))
        
        sm <- summary (model_exp)
        
        lreg$data$D <- sm$coefficients["D",1]
        lreg$data$E <- sm$coefficients["E",1]
        lreg$model <- model_exp
      }
    }
    
    return (lreg)
    
  }
  
  paste_formula <- function (lreg) {
    
    if (input$regression == "exponential") {
      paste ("~ ", round (lreg$data$D, 1), " 2^(x/", round(lreg$data$E, 1), ")", sep="")
      
    } else {
      paste ("~ ", round (lreg$data$B, 1), " / (1+2^((", round (lreg$data$A,1), "-x)/", round(lreg$data$C, 1), ")", sep="")
    }
  }
  
  
  # Main processing of data
  proc_data <- reactive({
    
    req (countries())
    
    sel <- countries () 
    
    print (paste ("countries", sel))
    
    if (length (sel) == 0) {
      return (NULL)
    }
    
    df.plot <- get_data () %>% filter (country %in% sel) #, date >= as.Date(input$dates[1])) #, date <= as.Date (input$dates[2]))
    
    
    if (nrow (df.plot) == 0) {
      return (NULL)
    }
    
    # save most recent selection
    
    selection[[input$tabs]] <<- sel
    
    withProgress(message = 'Processing data', value = 0, {
      
      # Process data - filter countries, fit models, and 
      print ("proc_data")
      
      dates <- get_dates ()
      
      if (!is.null(input$dates)) {
        dates <- input$dates
      }
      
      print (dates)
      
      df.date <- as.data.frame (expand.grid (date=seq (as.Date(dates[1]), as.Date(dates[2]), 1),
                                             country=as.character (unique (df.plot$country)), type = as.character(unique (df.plot$type))))
      
      
      
      df.plot <- merge (df.plot, df.date, by=c("date", "country", "type"), all.y=T)
      
      df.plot$reldate <- as.numeric (df.plot$date - min (df.plot$date))
      
      
      if (input$date == "relative to day100cases") {
        
        df.plot <- df.plot %>% 
          mutate (cases100 = ifelse (cases < 100 | type != "cases", NA, cases)) %>%
          group_by (country) %>%
          mutate (x = reldate - reldate[which.min (cases100)])
        
        df.plot$reldate <- as.numeric (df.plot$x)
        
      } else {
        
        df.plot$x <- df.plot$date
        
      }
      
      
      if (input$relative == "Per Capita") {
         df.plot$cases <- df.plot$cases / spop[df.plot$country]
      }
      
      
      if (input$plot == "combined") {
        
        if (input$date == "relative to day100cases") { # only combine up to where all countries have data
          
          df.maxdate <- df.plot %>%
            group_by (country) %>%
            summarize (md = max (reldate)) 
          
          df.plot <- df.plot %>% filter (reldate < min(df.maxdate$md))
          
        }
        
        df.plot <- df.plot %>%
          group_by (type, x, reldate) %>%
          summarize (cases = mean(cases), country ="combined cases") 
        
      }
      
      
      
      
      nprocesses <- length (unique (df.plot$country)) + 1
      incProgress(1/nprocesses)
      
      
      print ("fitting curve")
      # fitting logistic/exponential curve
      df.fit <- data.frame ()
      df.plot$pred <- 0
      
      
      for (c in unique (df.plot$country)) {
        
        lreg_cases <- fitmodel (df.plot %>% filter (type=="cases", !is.na (cases), cases > 0, country == c))
        
        
        df.plot[df.plot$country==c & df.plot$type=="cases", "pred"] <- predict (lreg_cases$model, newdata=df.plot %>% filter (country==c, type=="cases"))
        
        df.plot[df.plot$country==c & df.plot$type=="cases", "formula"] <- paste_formula (lreg_cases)
        
        # death
        lreg_death <- fitmodel (df.plot %>% filter (type=="death", !is.na (cases), cases > 0, country == c), 
                                lower_C=lreg_cases$data$C,
                                upper_C=lreg_cases$data$C,
                                lower_A=lreg_cases$data$A,
                                upper_A=lreg_cases$data$A+10,
                                upper_B=lreg_cases$data$B,
                                exponential = input$regression == "exponential")
        
        
        df.plot[df.plot$country==c & df.plot$type=="death", "pred"] <- predict (lreg_death$model, newdata=df.plot %>% filter (country==c, type=="death"))
        df.plot[df.plot$country==c & df.plot$type=="death", "formula"] <- paste_formula (lreg_death)
        
        df.cases <- lreg_cases$data
        df.death <- lreg_death$data
        
        colnames (df.cases) <- paste ("cases", colnames (df.cases), sep="_")
        colnames (df.death) <- paste ("death", colnames (df.death), sep="_")
        
        df.fit <- rbind (df.fit, cbind (country=c, df.cases, df.death))
      
        incProgress(1/nprocesses)
        
        
      }
      
      df.fit$ttd = df.fit$death_A - df.fit$cases_A
      df.fit$rate <- df.fit$death_B / df.fit$cases_B
      
      df.fit$stderr <- sqrt ((df.fit$death_B_std / df.fit$death_B)^2 + (df.fit$cases_B_std / df.fit$cases_B)^2) * df.fit$rate
      df.fit$ttd_rate <- df.fit$ttd# + log2(df.fit$rate)
      
    })
    
    return (list ("df.plot"=df.plot, "df.fit"=df.fit))
    
  })
  
  
  # output renders
  
  output$timecurve_text <- renderText({
    
    # if (is.null (countries()) || length (countries () == 0)) {
    #   return ("Generating data, please wait...");
    # }
    # 
    
    req (countries ())
    req (proc_data())
    
    regtext <- paste ("The semi-transparent lines reflect logistic regression using formula: y ~ L / (1 + 2^((x0-x)/k),",
                      "where L is the asymptote maxpoint, k is the doubling rate, and x0 is the infliction point.")
    
    if (input$regression == "exponential") {
      regtext <- paste ("The semi-transparent lines reflect exponential regression using formula: y ~ A * 2^(x/k),",
                        "where k is the doubling rate.")
      
      
    }
    
    paste ("<b>Time-series analysis</b> based on selected countries and settings.", 
           regtext,
           "When fitting regression for death events, the doubling rate is pre-set to match cases")
    
  })
  
  output$timecurve <- renderPlotly({
    
    #print (countries())
    
    # cts <- countries()
    # 
    # if (is.null (cts) || length (cts) == 0) {
    #   return ();
    # }
    # 
    ldata <- proc_data ()
    
    req (ldata)
    
    df.plot <- ldata$df.plot
    
    df.plot <- df.plot %>% filter (type %in% grps())
    
    
    if (input$cases == "cases/day") {
      df.plot <- df.plot %>% 
        arrange (reldate) %>%
        group_by (type, country) %>%
        mutate(cases = cases - lag(cases), pred = pred - lag(pred))
    }
    
    ypostfix <- ""
    logmin <- 1
    if (input$relative == "Per Capita") {
      ypostfix <- "per capita"
      logmin <- 1e-9
    }
    
    
    gg <- ggplot (df.plot) + 
      geom_line (aes (x=x, y=cases, color=country, linetype=type)) + 
      geom_line (aes (x=x, y=pred, color=country, linetype=type, text=formula), alpha=0.4) +
      ggtheme + labs (x="", y=ifelse (input$cases == "cases/day", paste ("Cases per day", ypostfix), paste ("Total cases", ypostfix)), color="", linetype="")
    
    if (input$axis == "log10") {
      
      
      gg <- gg + scale_y_log10(limits=c(logmin,max (df.plot$cases)))
      
    }
    
    
    ggplotly (gg)
    
    
  })
  
  
  output$mortal_text <- renderText({
    
    req (proc_data())
    
    paste ("<b>mortality rate</b> based on the ratio of max-points (L) from logistic regression on cases and death")
    
    
  })
  
  output$mortal <- renderPlotly({
    
    req (proc_data())
    
    ldata<- proc_data ()
    df.fit <- ldata$df.fit
    
    df.fit$rate[df.fit$stderr > df.fit$rate] <- NA  
    
    df.fit$text <- ifelse (is.na (df.fit$rate), "NA", "")
    
    gg <- ggplot (df.fit, aes (x=country, y=rate*100, fill=country)) + 
      geom_col() + geom_errorbar(aes(ymin=rate*100-stderr*100, ymax=rate*100+stderr*100), width=.2, position=position_dodge(.9)) +
      geom_text (aes (y=0, label=text)) + 
      labs (x="", y="Mortality rate (%)") + 
      ggtheme + theme (legend.position = "none")
    
    ggplotly (gg)
    
    
    
  })
  
  output$doubling_text <- renderText ({
    
    req (proc_data())
    
    paste ("<b>Doubling rate</b>, the k-value from the logistic regression")
    
  })
  
  output$doubling <- renderPlotly({
    
    req (proc_data())
    
    print (countries())
    
    ldata <- proc_data ()
    
    req (ldata)
    
    df.fit <- ldata$df.fit
    
    print (nrow (df.fit))
    
    gg <- ggplot (df.fit, aes (x=country, y=cases_C, fill=country)) + 
      geom_col() + 
      geom_errorbar(aes(ymin=cases_C-cases_C_std, ymax=cases_C+cases_C_std), width=.2,
                    position=position_dodge(.9)) +
      labs (x="", y="Doubling rate (days)") + 
      ggtheme + theme (legend.position = "none")
    
    
    ggplotly (gg)
    
    
    
  })
  
  output$delay_text <- renderText({
    
    
    req (proc_data())
    
    paste ("<b>Delay</b>, the difference in infliction points between cases and death.")
    
  })
  
  
  output$delay <- renderPlotly({
    
    req (proc_data())
    
    
    ldata<- proc_data ()
    
    req (ldata)
    
    
    df.fit <- ldata$df.fit
    
    gg <- ggplot (df.fit, aes (x=country, y=ttd_rate, fill=country)) + 
      geom_col() +
      labs (x="", y="Delay (days)") + 
      ggtheme + theme (legend.position = "none")
    
    
    ggplotly (gg)
    
  })
  
  
  
  
  get_table_data <- reactive ({
    
    req (get_data())
    
    df.data <- get_data ()
    
    df.data %>% 
      select (reldate, type, country, cases) %>%
      spread (type, cases) %>%
      #filter (type== "cases") %>% 
      arrange (reldate) %>% 
      group_by (country) %>%
      mutate (inc_cases = cases - lag (cases), inc_cases_pmc = inc_cases / (spop[country]/1e6),
              inc_death = death - lag (death), inc_death_pmc = inc_death / (spop[country]/1e6)) %>%
      top_n (reldate, n=5) %>% 
      group_by (country) %>%
      summarize (daily_cases_pmc = round(mean (inc_cases_pmc), 2), 
                 daily_death_pmc = round(mean (inc_death_pmc), 2), 
                 total_cases = max(cases), 
                 total_death = max (death),
                 total_cases_pmc = round (max(cases) / (spop[unique(country)]/1e6), 1),
                 total_death_pmc = round (max(death) / (spop[unique(country)]/1e6), 1)) %>%
      dplyr::select (country, total_cases, total_death, 
                     total_cases_pmc, total_death_pmc, 
                     daily_cases_pmc, daily_death_pmc) %>%
      arrange (country) 
    
  })
  
  output$mytable = DT::renderDT({
    
    
    withProgress(message = 'Preparing table with stats', value = 0, {
        
      
      df <- get_table_data ()
      
      
      incProgress(0.5)
      
      preselect <- which (df$country %in% selection[[input$tabs]])
      
      print (preselect)
      
      
      datatable(df, rownames = F, 
                 selection = list(mode = 'multiple', selected = preselect, target = 'row'), 
                 escape=F, 
                 options = list(paging = FALSE)) %>%
        formatStyle(
          'country',
          backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
        )
    })
  })
  
  
  countries <- reactive({ 
    
    print ("table selection")
    
    # if (is.null(input$mytable_rows_selected) && !user_selection ()) {
    #   
    #   return (c("Denmark", "Sweden"))
    #   
    # }
    # 
    df <- get_table_data ()
    
    print (input$mytable_rows_selected)
    
    sel <- df[input$mytable_rows_selected,] 
    
    print (sel)
    
    sel %>% .$country %>% as.vector ()
    
  }) %>% debounce(800)

})


shinyApp(ui = ui, server = server)