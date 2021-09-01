library(shiny)
library(plotly)
library(dplyr)
library(data.table)
options(shiny.maxRequestSize=100*1024^2)
getOption("digits")
options("digits" = 15)
library(data.table)
library(RColorBrewer)
options(scipen=1000)
library(rsconnect)
library(tidyverse)
library(bit64)
library(shinyFiles)
library(xlsx)
library(writexl)
Sys.setlocale("LC_TIME","english")

ui <- fluidPage(  ##desing UI
  navbarPage("EZ composer:",  #title of application
             ###Recalibration tab ui============
             tabPanel("Recalibration",  ###open first tab panel
                      titlePanel("Recalibartion processing"), ##title of tap panel
                      sidebarLayout( ##design side bar (location of user select value)
                        sidebarPanel(width=3, ##set width of sider panel 
                                     fileInput("file1", "Choose File", ##start file input UI
                                               buttonLabel=list(icon("folder"),"Browse"), ##image of file browse icon
                                               multiple = TRUE, ##option to multiple choice but i don't know how is it work
                                               accept = c("text/csv",   
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),##set available format of input file
                                     textInput("text", label = h4("Sample name"), value = ""), ##input sample name for saving file without chaing file format
                                     sliderInput(inputId = "slider", ##slider to adjust y axis of Error ppm vs Cal m/z plot
                                                 label = "Error ppm range", ##title of UI
                                                 min = -10, #minimum value of slider bar
                                                 max = 10, #maximum value of slider bar
                                                 sep = "",#value separate unit 'none'
                                                 step = 0.5, #gap of each slide point
                                                 value = c(-4.5, 4.5)), ##initial value of slider range
                                     sliderInput(inputId = "sliderx",  ##slider to adjust x axis of Error ppm vs Cal m/z plot
                                                 label = "m/z range", ##title of UI
                                                 min = 0, 
                                                 max = 1000, 
                                                 sep = "",
                                                 step = 100,
                                                 value = c(100, 1000)),
                                     tags$style(HTML(".radio-inline {margin-right: 38px;}")), ##gap between radio buttom
                                     radioButtons("Modeselect", label = h4("+/- mode"), ##selecting option for analysis condition (Positive and Negative mode)
                                                  inline = T, ##radio button directio inline==same line 
                                                  choices = list("Positive (+)" = 1, "Negative (-)" = -1),selected = 1),##Positive button indicated +1 value, this value used at ref file with +1 value, meanwhile negative button showed -1 charge at ref file
                                     tags$style(HTML(".radio-inline {margin-right: 12px;}")), 
                                     radioButtons("checkRCstep", label = h4("Recalibration step"),
                                                  inline = T,
                                                  choices = list("CHO" = "CHO", "CHON1S1" = "CHON1S1", "CHON4S2" = "CHON4S2",
                                                                 "usermam" = "user"),selected = "CHO"),
                                     conditionalPanel('input.checkRCstep == "user"',
                                                      textInput("inText", "RCstep", value = "")),
                                     radioButtons("selection", label = h4("Select TRUE/FALSE"),
                                                  inline = T,
                                                  choices = list("TRUE" = T, "False" = F),selected = T),
                                     #tags$hr(),
                                     #textInput("text", label = h4("Sample name"), value = ""),
                                     tags$hr(),
                                     downloadButton("ref","Export as ref",class = "btn btn-secondary btn-sm"),
                                     downloadButton("download","Download as table", class = "btn btn-secondary btn-sm"),
                                     downloadButton("matlab","Export for matlab file", class = "btn btn-secondary btn-sm"),
                                     tags$hr(),
                                     selectInput(inputId ="dist", h4("Histrogram dist"), 
                                                 choices = list("10" = 10, "20" = 20, "25"=25,
                                                                "50" = 50, "100"=100), selected = 50),
                                     
                        ),
                        
                        
                        mainPanel(
                          #verbatimTextOutput("summary"),
                          #DT::dataTableOutput('summary'),
                          #plotlyOutput('Masslist',  width = "400px", height = "400px"),
                          #tags$hr(),
                          splitLayout(style = "border: 1px solid silver:", cellWidths = c(900,450), 
                                      plotlyOutput('Masslist',  width = "500px", height = "500px"),
                                      DT::dataTableOutput('summary',width = "400px", height = "400px")),
                          
                          splitLayout(style = "border: 1px solid silver:", cellWidths = c(800,600), 
                                      plotlyOutput('massspec',width = "400px", height = "400px"),
                                      plotOutput('masshisto',width = "600px", height = "400px")
                                      
                          )
                          
                        )
                      )
             ),
             ###FT analysis================
             tabPanel("FT distribution ",
                      titlePanel("FT-info"),
                      #sidebarLayout(
                      mainPanel(
                        
                        
                        #verbatimTextOutput("summary"),
                        #DT::dataTableOutput('summary'),
                        #plotlyOutput('Masslist',  width = "400px", height = "400px"),
                        #tags$hr(),
                        splitLayout(style = "border: 1px solid silver:", cellWidths = c(800,1100), 
                                    plotlyOutput('vk1',  width = "550px", height = "550px"),
                                    plotlyOutput('vk2',  width = "550px", height = "550px")
                        ),
                        tags$hr(),

                        splitLayout(style = "border: 1px solid silver:", cellWidths = c(800,800), 
                                    plotlyOutput('pie_freq',width = "550px", height = "550px"),
                                    plotlyOutput('pie_inty',width = "550px", height = "550px")
                                    
                        ),
                        
                      )
                       
             ),
             ###blank removal tap ui======
             tabPanel("Blank processing",
                      titlePanel("Blank removal"),
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("blank","Select blank",
                                    buttonLabel = list(icon("folder"),"Browse"),
                                    multiple = TRUE),
                          radioButtons("Ionization mode", label = h3("ESI/APPI mode"),
                                       inline = T,
                                       choices = list("ESI"="ESI" , "APPI" = "APPI"),selected = "ESI"),
                          actionButton("action", "Select filefolder&Blank removal")
                        ),
                        mainPanel(
                          DT::dataTableOutput('blanktable'),
                          verbatimTextOutput("text", placeholder = TRUE),
                          DT::dataTableOutput('sampletable'),
                          
                          
                        )
                      )
             )
  )
)


server <- function(input, output, session) {
  
  observe({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    
    y <- input$file1
    get_filename <- stringi::stri_extract_first(str = y$name, regex = ".*(?=\\.)")
    
    # This will change the value of input$inText, based on x
    updateTextInput(session, "text", value = get_filename)
    
    # Can also set the label, this time for input$inText2
  })
  
  test <- reactive({
    testcsv <- data.table()
    testcsv <- fread(input$file1$datapath)
    testcsv$Selected <- T
    testcsv$Nrule=(testcsv$`#H`+ testcsv$`#N`)%%2
    testcsv$CNHratio=ifelse((2*testcsv$`#C`+ testcsv$`#N`+2)>=testcsv$`#H`,0,1)
    testcsv$OCratio=ifelse(testcsv$`#O`/testcsv$`#C`<=1,0,1)
    testcsv$HCratio=ifelse(testcsv$`#H`/testcsv$`#C`>0.3,0,1)
    testcsv$NCratio=ifelse(testcsv$`#N`/testcsv$`#C`<=1,0,1)
    testcsv$filteringIndex=testcsv$Nrule+testcsv$CNHratio+testcsv$OCratio+testcsv$HCratio+testcsv$NCratio
    #testcsv=testcsv %>% filter(testcsv$filteringIndex==0)
    
    setorder(testcsv, `ppm Error`)
    
    return(testcsv)
  })
  
  Mass_selection <- reactive({
    parsToChange2 <- vector()
    eventData <- event_data("plotly_selected", source = 'Masslist_source', session)
    #parsToChange <- eventData$y
    parsToChange2 <- eventData$x
    test()[`Calc m/z` %in% parsToChange2, Selected := !Selected]
    #test()[`ppm Error` %in% parsToChange, Selected := !Selected]
    test()
  })
  
  
  output$Masslist <- renderPlotly({
    
    if (is.null(input$file1)) { return(NULL)
    }else{
    req(Mass_selection())
    plotheight <- 500
    
    colors <- if (length(unique(Mass_selection()$Selected)) > 1) {
      c('#F0F0F0', '#F0F0F0')
    } else {
      if (unique(Mass_selection()$Selected)) {
        '#F0F0F0'
      } else {
        '#F0F0F0'
      }
    }
    
    stroke <- if (length(unique(Mass_selection()$Selected)) > 1) {
      c('#191919', '#191919')
    } else {
      if (unique(Mass_selection()$Selected)) {
        '#191919'
      } else {
        '#191919'
      }
    }
    
    symbols <-
      if (length(unique(Mass_selection()$Selected)) > 1) {
        c('x', 'circle')
      } else {
        if (unique(Mass_selection()$Selected)) {
          'circle'
        } else {
          'x'
        }
      }
    
  
    p <- plot_ly(data = Mass_selection(),
                 source = 'Masslist_source',
                 height = plotheight,
                 width = 900) %>%
      add_trace(x = ~`Calc m/z`,
                y = ~`ppm Error`,
                type = 'scatter',
                mode = 'markers',
                color = ~factor(Selected),
                colors = colors,
                symbol = ~factor(Selected),
                symbols = symbols,
                alpha = 0.2,
                stroke = ~factor(Selected),
                strokes= stroke,
                marker = list(size  = 6),
                hoverinfo = "text",
                text = ~paste('<br>',  'Formula: ', `Formula`,
                              '<br>', 'Mass: ', `Calc m/z`,
                              '<br>',  'Error ppm: ', `ppm Error`,
                              sep = '')) %>%
      
      add_lines(x= seq(100,1000,5), y= rep(0.5, 181) , name = "Error ppm:+0.5",
                line = list(color= "grey", widthh=0.5, dash="dot")) %>%
      add_lines(x= seq(100,1000,5), y= rep(-0.5, 181), name = "Error ppm:-0.5",
                line = list(color= "grey", widthh=0.5, dash="dot")) %>% 
      layout(
        title=paste(input$text,ifelse(input$checkRCstep=="user",input$inText,input$checkRCstep),sep = "_"),
        margin = list(l = 50, r= 20, b = 00, t = 50),
        hoverlabel = list(font=list( color = '#1b73c1'), bgcolor='#f7fbff'),
        xaxis =  list(title = 'mz',
                      showgrid = F,
                      showline = T,
                      zeroline = F,
                      nticks = 5,
                      font = list(size = 8),
                      ticks = "outside",
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("black"),
                      range= c(input$sliderx[1],input$sliderx[2])
        ),
        yaxis =  list(categoryarray = ~`ppm Error`,
                      title = 'Error ppm',
                      autorange = F,
                      showgrid = F,
                      showline = T,
                      autotick = T,
                      font = list(size = 8),
                      ticks = "outside",
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("black"),
                      range= c(input$slider[1],input$slider[2])
                      #range= c(ifelse(input$checkRCstep=="CHO_preRC",-4.0,-2.0),ifelse(input$checkRCstep=="CHO_preRC",4.0,2.0))
        ),
        #dragmode =  "select"
        dragmode =  "lasso"
      )
    
    p <- p %>% config(
      modeBarButtonsToRemove = list(
        "zoom2d",
        "pan2d",
        #"zoomIn2d",
        "zoomOut2d",
        "autoScale2d",
        #"resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "sendDataToCloud",
        "toggleHover",
        "resetViews",
        "toggleSpikelines",
        "resetViewMapbox"
      ),
      displaylogo = FALSE
    )
    p
    }

 
  })
  
  
  output$massspec <- renderPlotly({
    plotheight <- 600
    if (is.null(input$file1)) { return(NULL)
    }else{
    fig <- plot_ly(df_subset(), color = I("gray20"),
                   width = 780) %>% add_segments(x = ~`Calc m/z`, xend = ~`Calc m/z`, 
                                          y = 0,yend = ~`Mono Abund`, showlegend = T,
                                          hoverinfo = "text", text = ~paste('<br>',  'Formula: ', `Formula`,
                                                                            '<br>', 'Mass: ', `Calc m/z`,
                                                                            '<br>',  'Error ppm: ', `ppm Error`,
                                                                            sep = '')) %>% 
      layout(
        title="",
        margin = list(l = 50, r= 20, b = 70, t = 50),
        xaxis =  list(title = 'mz',
                      showgrid = F,
                      showline = T,
                      zeroline = F,
                      nticks = 5,
                      font = list(size = 8),
                      ticks = "outside",
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("black"),
                      range= c(input$sliderx[1],input$sliderx[2])
        )
      )
      
    
    fig <- fig %>% config(
      displaylogo = FALSE
    )
    
    fig
    }
    
  })
  
  output$masshisto <- renderPlot({
    if (is.null(input$file1)) { return(NULL)
    }else{
    x    <- test()$`Calc m/z`
    
    bins <- seq(0, 1000, length.out = 1000/as.numeric(input$dist) + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "",
         xlim = c(input$sliderx[1],input$sliderx[2]),
         main = "Histogram of assigned formula")
    }
    
  })
  
  df_subset <- reactive({
    a <- subset(Mass_selection(), Mass_selection()$Selected == input$selection)
    #a <- subset(Mass_selection(), Selected == TRUE)
    a=a %>% filter(a$filteringIndex==0)
    return(a)
  })
  
  
  df_sort <- reactive({
    mass_sort <- data.table(Mass_selection()[["Index"]],Mass_selection()[["Formula"]],Mass_selection()[["Adduct"]],Mass_selection()[["Exp m/z"]],
                            Mass_selection()[["Mono Abund"]],Mass_selection()[["Total Abund"]],Mass_selection()[["Calc m/z"]],Mass_selection()[["ppm Error"]],
                            Mass_selection()[["#C"]],Mass_selection()[["#H"]],Mass_selection()[["#N"]],Mass_selection()[["#O"]],Mass_selection()[["#S"]])
    colnames(mass_sort) <- c("Index","Formula", "Adduct","Expt m/z","Mono\nInty","Total Inty", 
                     "Calc m/z","ppm Error","C#","H#","N#","O#","S#")
    return(mass_sort)
  })
  
  output$download <- downloadHandler(
    
  filename = function() {
    paste(paste(paste0(format(Sys.time(), "%y"),format(Sys.time(), "%m"),format(Sys.time(), "%d")),ifelse(input$Modeselect==1,"P","N"),input$text,input$checkRCstep,"sort_sel",sep = "_"),".csv", sep = ".csv")
  },
  content = function(file) {
    fwrite(test(), file, row.names = FALSE)
    #fwrite(df_sort(), file, row.names = FALSE)
    }
  )
  
  ##v2.0 export ref file
  dat2 <- reactive({
    test <-  data.table(df_subset()[["Formula"]], df_subset()[["Calc m/z"]])
    test$Charge=input$Modeselect
    colnames(test) <- c("Formula", "Calc m/z","Charge")
    return(test)
  })
  
  output$ref <- downloadHandler(
    filename = function() {
      paste(paste(paste0(format(Sys.time(), "%y"),format(Sys.time(), "%m"),format(Sys.time(), "%d")),ifelse(input$Modeselect==1,"P","N"),input$text,ifelse(input$checkRCstep=="user",input$inText,input$checkRCstep),sep = "_"), ".ref", sep = "")
    },
    content = function(file) {
      write.table(dat2(),file, row.names = FALSE, quote = F)
    }
  )
  
 
  ##v3.0 export as matlab
  
  df_filter <- reactive({
    
    
    f <- data.table(df_subset()[["Index"]],df_subset()[["Formula"]],df_subset()[["Adduct"]],df_subset()[["Exp m/z"]],
                    df_subset()[["Mono Abund"]],df_subset()[["Total Abund"]],df_subset()[["Calc m/z"]],df_subset()[["ppm Error"]],
                    df_subset()[["#C"]],df_subset()[["#H"]],df_subset()[["#N"]],df_subset()[["#O"]],df_subset()[["#S"]])
    colnames(f) <- c("Index","Formula", "Adduct","Expt m/z","Mono\nInty","Total Inty", 
                        "Calc m/z","ppm Error","C#","H#","N#","O#","S#")
    f= f %>% add_column("Bromo Inty"=f$`Mono\nInty`,.after =5 )
    
    f$Charge=input$Modeselect
    f$DBE=f$`C#`-f$`H#`/2+f$`N#`/2+1
    f$`O/C`=f$`O#`/f$`C#`
    f$`H/C`=f$`H#`/f$`C#`
    f$OSc=2*(f$`O#`/f$`C#`)-(f$`H#`/f$`C#`)
    f$`N/C`=f$`N#`/f$`C#`
    f$`N/O`=f$`N#`/f$`O#`
    f$`S/C`=f$`S#`/f$`C#`
    f$`OM/OC`=(1+(16*f$`O#`+14*f$`N#`+f$`H#`+32*f$`S#`)/(12*f$`C#`))
    f$`Rel.\nIntensity`=f$`Bromo Inty`/max(f$`Bromo Inty`)*100
    f$`Ral. OM/OC`=f$`OM/OC`*f$`Rel.\nIntensity`
    f$CAI=f$`C#`-f$`N#`-f$`O#`-f$`S#`
    f$DBEAI=1+f$`C#`-f$`O#`-f$`S#`-f$`H#`/2-f$`N#`/2
    f$AI=ifelse(f$CAI<=0,0,ifelse(f$DBEAI<0,0,f$DBEAI/f$CAI))
    
    f$`O/C filter`=ifelse(f$`O/C`>=2,1,0)
    f$`H/C filter`=ifelse(f$`H/C`>=2.2,1,0)
    f$`N/C filter`=ifelse(f$`N/C`>=0.5,1,0)
    f$`S/C filter`=ifelse(f$`S/C`>=0.2,1,0)
    f$`DBE filter`=ifelse(f$DBE<0,1,0)
    
    f$filteringIndex=f$`O/C filter`+f$`H/C filter`+f$`N/C filter`+f$`S/C filter`+f$`DBE filter`
    #f=f %>% filter(f$filteringIndex==0)
    f$`Rel.\nIntensity`=f$`Bromo Inty`/max(f$`Bromo Inty`)*100
    
    #fsel=f[,-c(29:34)]
    return(f)
  })
  
  matlab_subset <- reactive({
    matlab <- df_filter()
    matlab=matlab %>% filter(matlab$filteringIndex==0)
    matlab_sel=matlab[,-c(29:34)]
    return(matlab_sel)
  })
  
  
  output$matlab <- downloadHandler(
    filename = function() {
      paste(paste(format(Sys.time(), "%y%m%d"),ifelse(input$Modeselect==1,"P","N"),input$text,"matlab",sep = "_"), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(matlab_subset(),file, row.names = FALSE)
    }
  )
  
  
  output$summary = DT::renderDataTable(
    if (is.null(input$file1)) { return(NULL)
    }else{
    DT::datatable(t(data.table(`Input formula`=nrow(test()),`Assigned formula`=nrow(matlab_subset()),
                             Filtered=nrow(test())-nrow(matlab_subset()),
                             `N rule`=sum(test()$Nrule),`CNH (2C+N+2>H)`=sum(test()$CNHratio),
                             `O/C (>1)`=sum(test()$OCratio),
                             `H/C (<0.3)`=sum(test()$HCratio),
                             `N/C (>0.5)`=sum(test()$NCratio),
                             `Matlab (H/C>2.2+N/C>0.5+S/C>0.2+DBE<0)`=paste0(nrow(df_filter())-nrow(matlab_subset()),
                                                  " (",sum(df_filter()$`H/C filter`),"+",
                                                  sum(df_filter()$`N/C filter`),"+",
                                                  sum(df_filter()$`S/C filter`),"+",
                                                  sum(df_filter()$`DBE filter`),")"),
                             `Multi filtering (-)`=sum(test()$Nrule)+sum(test()$CNHratio)+sum(test()$OCratio)+sum(test()$HCratio)
                                     +sum(test()$NCratio)+nrow(df_filter())-nrow(test()))), colnames ="value" )},
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )
  
  blank_h <- reactive({
    testxl_h <- data.table()
    testxl_h <- as.data.table(read.xlsx2(input$blank$datapath, sheetIndex = 1))
    testxl_h=subset(testxl_h, testxl_h$Adduct=="H")
    
    return(testxl_h)
  })
  
  blank_rad <- reactive({
    testxl_rad <- data.table()
    testxl_rad <- as.data.table(read.xlsx2(input$blank$datapath, sheetIndex = 1))
    testxl_rad=subset(testxl_rad, testxl_rad$Adduct!="H")
    return(testxl_rad)
  })
  
  output$blanktable =  DT::renderDataTable(
    if (is.null(input$file1)) { return(NULL)
    }else{
    DT::datatable(data = rbind(blank_h(),blank_rad()))
    },
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )
  
  observeEvent(input$action, {
    fdir=setwd(choose.dir())
    flist=list.files(path = ".",pattern = ".xlsx")
    output$text <- renderText(flist)
    dir.create("blankRM")

        for (i in 1:length(flist)) {
      temp=read.xlsx2(file = flist[i], sheetIndex = 1)
      temp_h=subset(temp,temp$Adduct=="H")
      temp_rad=subset(temp,temp$Adduct!="H")
      
      temp2_h=temp_h %>% filter(! Formula%in%blank_h()$Formula)
      temp2_rad=temp_rad %>% filter(! Formula%in%blank_rad()$Formula)
      
      temp2=rbind(temp2_rad,temp2_h)
      
      colnames(temp2) <- c("Index","Formula","Adduct","Expt m/z","Mono\nInty","Bromo Inty","Total Inty", 
                           "Calc m/z","ppm Error","C#","H#","N#","O#","S#","Charge","DBE","O/C","H/C","OSc",
                           "N/C","N/O","S/C","OM/OC","Rel.\nIntensity","Ral. OM/OC","CAI","DBEAI","AI")
      
      output$sampletable =  DT::renderDataTable(
        
        DT::datatable(data = temp2
                      
        ),
        filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
      )

      cols.num <- c("Index","Expt m/z","Mono\nInty","Bromo Inty","Total Inty", 
                    "Calc m/z","ppm Error","C#","H#","N#","O#","S#","Charge","DBE","O/C","H/C","OSc",
                    "N/C","N/O","S/C","OM/OC","Rel.\nIntensity","Ral. OM/OC","CAI","DBEAI","AI")
      
      temp2[cols.num] <- sapply(temp2[cols.num],as.numeric)
      
      write_xlsx(temp2, path = paste0("blankRM/",tools::file_path_sans_ext(flist[i]),"_blankRM.xlsx"))
      
    }

    
  })
  
  ft_vk <- reactive({
    
    ft_data=matlab_subset()
    ft_data=ft_data %>% mutate(C=ifelse(`C#`>0,"C","")) %>% mutate(H=ifelse(`H#`>0,"H","")) %>% mutate(O=ifelse(`O#`>0,"O","")) %>% 
      mutate(N=ifelse(`N#`>0,"N","")) %>% mutate(S=ifelse(`S#`>0,"S",""))
    ft_data=ft_data %>% unite("Chemicalcomposition",colnames(ft_data[,c("C","H","O","N","S")]),sep = "") ##column of atomic comp
    
    ft_data$Chemicalcomposition=factor(ft_data$Chemicalcomposition, levels = c("CHO","CHON","CHOS","CHONS","CH","CHN","CHS","CHNS"))
    
    ft_data$lipid=ifelse(ft_data$`O/C`>=0&ft_data$`O/C`<0.3,
                         ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.0, "Lipids",""),"")
    ft_data$protein=ifelse(ft_data$`O/C`>=0.3&ft_data$`O/C`<=0.67,
                           ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.2,
                                  ifelse(ft_data$`N/C`>=0.05,"Proteins","") ,""),"")
    ft_data$carbo=ifelse(ft_data$`O/C`>0.67&ft_data$`O/C`<2.0,
                         ifelse(ft_data$`H/C`>1.5&ft_data$`H/C`<2.3, "Carbohydrates",""),"")
    ft_data$Unhydro=ifelse(ft_data$`O/C`>0&ft_data$`O/C`<0.1,
                           ifelse(ft_data$`H/C`>=0.7&ft_data$`H/C`<=1.5, "Unsaturated hydrocarbons",""),"")
    ft_data$lignin=ifelse(ft_data$`O/C`>=0.1&ft_data$`O/C`<=0.67,
                          ifelse(ft_data$`H/C`>=0.7&ft_data$`H/C`<=1.5, 
                                 ifelse(ft_data$AI<0.67, "Lignins",""),""),"")
    ft_data$tannin=ifelse(ft_data$`O/C`>0.67&ft_data$`O/C`<2.0,
                          ifelse(ft_data$`H/C`>=0.5&ft_data$`H/C`<=1.5, 
                                 ifelse(ft_data$AI<0.67, "Tannins",""),""),"")
    ft_data$conaro=ifelse(ft_data$`O/C`>0&ft_data$`O/C`<=0.67,
                          ifelse(ft_data$`H/C`>=0.2&ft_data$`H/C`<0.67, 
                                 ifelse(ft_data$AI>=0.67, "Condensed aromatics",""),""),"")
    
    ft_data=ft_data %>%  unite("Molecularclass",c("lipid","protein","carbo","Unhydro","lignin","tannin","conaro"), sep = "")
    ft_data$Molecularclass=ifelse(ft_data$Molecularclass=="","Unassigned", ft_data$Molecularclass) ###colum of molecular class
    
    ft_data$`Combusetion derived`=ifelse(ft_data$AI>0.66, "Combusetion derived polycyclic aromatic structures","")
    ft_data$`Soil derived PCA`=ifelse(ft_data$AI<=0.66,
                                      ifelse(ft_data$AI>=0.50,"Soil derived PCA with aliphatic chain",""),"")
    
    ft_data$`Soil derived humics`=ifelse(ft_data$AI<0.5,
                                         ifelse(ft_data$`H/C`<1.50,"Soil derived humics & highly unsaturated",""),"")
    
    ft_data$`Unsaturated aliphatic`=ifelse(ft_data$AI<0.5,
                                           ifelse(ft_data$`H/C`>=1.50,
                                                  ifelse(ft_data$`H/C`<2.0,"Unsaturated aliphatic compounds",""),""),"")
    
    ft_data$`fatty & sulfonic acids`=ifelse(ft_data$AI<0.5,
                                            ifelse(ft_data$`H/C`>=2.0,"fatty & sulfonic acids",""),"")
    
    ft_data=ft_data %>%  unite("Originclass",c("Combusetion derived","Soil derived PCA","Soil derived humics","Unsaturated aliphatic","fatty & sulfonic acids"), sep = "")
    
    ft_data$Originclass=ifelse(ft_data$Originclass=="","Unassigned", ft_data$Originclass) ###colum of molecular class
    
    return(ft_data)
  })
  
  output$vk1 <- renderPlotly({
    plotheight <- 600
    if (is.null(input$file1)) { return(NULL)
    }else{
      #pal <- c("red", "blue", "green","purple","grey70")
      p1 <- plot_ly(data =ft_vk(), x=~`O/C`,y=~`H/C`,color = ~(Chemicalcomposition), type="scatter",
                    #colors = pal,
                    alpha = 0.3,width = 780,hoverinfo = 'text',
                    text = ~paste('<br>',  'Formula: ', `Formula`,
                                  '<br>', 'Mass: ', `Calc m/z`,
                                  '<br>',  'Chemical composition:', `Chemicalcomposition`,
                                  '<br>',  'Molecular class:', `Molecularclass`,
                                  sep = '')) %>% 
        layout(title = 'Test',
               shapes = list(
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "fc913a"), opacity = 0.5,
                      x0 = 0, x1 = 0.295, xref = "x",
                      y0 = 1.505, y1 = 2.0, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "#f9320c"), opacity = 0.5,
                      x0 = 0.305, x1 = 0.675, xref = "x",
                      y0 = 1.505, y1 = 2.2, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "#9055A2"), opacity = 0.5,
                      x0 = 0.685, x1 = 1.5, xref = "x",
                      y0 = 1.505, y1 = 2.2, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "#49010F"), opacity = 0.5,
                      x0 = 0, x1 = 0.095, xref = "x",
                      y0 = 0.675, y1 = 1.495, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "royalblue"), opacity = 0.5,
                      x0 = 0.105, x1 = 0.675, xref = "x",
                      y0 = 0.675, y1 = 1.495, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "#56A902"), opacity = 0.5,
                      x0 = 0.685, x1 = 1.5, xref = "x",
                      y0 = 0.5, y1 = 1.495, yref = "y"),
                 list(type = "rect",
                      fillcolor = NULL, line = list(color = "darkgoldenrod3"), opacity = 0.5,
                      x0 = 0, x1 = 0.675, xref = "x",
                      y0 = 0.2, y1 = 0.660, yref = "y")
                 
               )
        ) %>% 
        layout(
          margin = list(l = 50, r= 20, b = 00, t = 50),
          hoverlabel = list(font=list( color = '#1b73c1'), bgcolor='#f7fbff'),
          xaxis =  list(title = 'O/C',
                        showgrid = T,
                        showline = T,
                        zeroline = F,
                        nticks = 10,
                        font = list(size = 8),
                        ticks = "outside",
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        range= c(0,1.2)
          ),
          yaxis =  list(title = 'H/C',
                        autorange = F,
                        showgrid = T,
                        showline = T,
                        autotick = T,
                        font = list(size = 8),
                        ticks = "outside",
                        ticklen = 5,
                        tickwidth = 2,
                        tickcolor = toRGB("black"),
                        range= c(0,2.5)
          )
        )
      p1
    }
    
  })
  
  output$vk2 <- renderPlotly({
    plotheight <- 600
    if (is.null(input$file1)) { return(NULL)
    }else{
      
      p2 <- plot_ly(data =ft_vk(),width = 1000) %>% 
        add_lines(x= seq(0,1.2,0.01), y= ~(1.1-seq(0,1.2,0.01)) , name = "Soil derived PCA with aliphatic chain",hoverinfo = 'text',text = ~paste("AI: ", 0.5),
                  line = list(color= "grey", widthh=0.5, dash="dot"),showlegend = F) %>% 
        add_lines(x= seq(0,1.2,0.01), y= ~(0.76-0.76*seq(0,1.2,0.01)) , name = "Combusetion derived polycyclic aromatic structures",hoverinfo = 'text',text = ~paste("AI: ", 0.66),
                  line = list(color= "grey", widthh=0.5, dash="dot"),showlegend = F) %>% 
        add_lines(x= seq(0,1.2,0.01), y=rep(1.5,121), name = "Soil derived humics & highly unsaturated",hoverinfo = 'text',
                  line = list(color= "grey", widthh=0.5, dash="dot"),showlegend = F) %>% 
        add_lines(x= seq(0,1.2,0.01), y= rep(2.0,121) , name = "Unsaturated aliphatic compounds",hoverinfo = 'text',
                  line = list(color= "grey", widthh=0.5, dash="dot"),showlegend = F) %>% 
        add_trace(data =ft_vk(),x=~`O/C`,y=~`H/C`,color = ~Originclass, type="scatter",hoverinfo = 'text',
                  text = ~paste('<br>',"Formula: ", Formula,
                                '<br>', 'Mass: ', `Calc m/z`,
                                '<br>', 'Origin class: ', `Originclass`),alpha = 0.4) %>% 
        layout(showlegend = T,legend = list(orientation = 'v'),
               margin = list(l = 50, r= 20, b = 00, t = 50),
               #hoverlabel = list(font=list( color = '#1b73c1'), bgcolor='#f7fbff'),
               xaxis =  list(title = 'O/C',
                             showgrid = T,
                             showline = T,
                             zeroline = F,
                             nticks = 10,
                             font = list(size = 8),
                             ticks = "outside",
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black"),
                             range= c(0,1.2)
               ),
               yaxis =  list(title = 'H/C',
                             autorange = F,
                             showgrid = T,
                             showline = T,
                             autotick = T,
                             font = list(size = 8),
                             ticks = "outside",
                             ticklen = 5,
                             tickwidth = 2,
                             tickcolor = toRGB("black"),
                             range= c(0,2.5)
               )
        )
      p2
    }
    
  })
  
  ft_comp <- reactive({
    ft_data=ft_vk()
    ft_comp=count(ft_data, Chemicalcomposition)
    ft_comp <- ft_comp %>% slice(match(c("CHO","CHON","CHOS","CHONS","Remainders"), Chemicalcomposition))
    
    return(ft_comp)
  })
  
  ft_mole <- reactive({
    ft_data=ft_vk()
    ft_mole=count(ft_data, Molecularclass)
    ft_mole <- ft_mole %>% slice(match(c("Lignins","Tannins","Lipids","Proteins","Condensed aromatics",
                                         "Unsaturated hydrocarbons","Carbohydrates","Unassigned"), Molecularclass))
    
    return(ft_mole)
  })
  
  ft_Origin <- reactive({
    ft_data=ft_vk()
    ft_Origin=count(ft_data, Originclass)
    ft_Origin <- ft_Origin %>% slice(match(c("Combusetion derived polycyclic aromatic structures",
                                             "Soil derived PCA with aliphatic chain",
                                             "Soil derived humics & highly unsaturated",
                                             "Unsaturated aliphatic compounds",
                                             "fatty & sulfonic acids"), Originclass))
    
    return(ft_Origin)
  })
  
  
  output$pie_freq <- renderPlotly({
    plotheight <- 600
    if (is.null(input$file1)) { return(NULL)
    }else{
      
      
      pie_freq <- plot_ly(width = 780)
      pie_freq <- pie_freq %>% add_pie(data =ft_comp(), labels = ~Chemicalcomposition, values = ~n,
                                       name = "",title="Chemical composition \n ",
                                       sort = FALSE, domain = list(x = c(0, 0.4), y = c(0.4, 0.9)))
      
      pie_freq <- pie_freq %>% add_pie(data = ft_mole(), labels = ~Molecularclass, values = ~n,sort = FALSE,
                                       name = "",title="Molecular class\n ", domain = list(x = c(0.25, 0.75), y = c(0, 0.5)))
      
      pie_freq <- pie_freq %>% add_pie(data = ft_Origin(), labels = ~Originclass, values = ~n,sort = FALSE,
                                       name = "",title="Derived class\n ", domain = list(x = c(0.6, 1), y = c(0.4, 0.9)))
      
      pie_freq <- pie_freq %>% layout(title = "Frequency based", showlegend = F,
                                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      pie_freq
    }
    
  })
  
  
  it_comp <- reactive({
    ft_data=ft_vk()
    
    it_comp=as.data.table(aggregate(ft_data$`Bromo Inty`, by=list(Chemicalcomposition=ft_data$Chemicalcomposition), FUN=sum))
    it_comp <- it_comp %>% slice(match(c("CHO","CHON","CHOS","CHONS"), Chemicalcomposition))
    return(it_comp)
  })
  
  it_mole <- reactive({
    ft_data=ft_vk()
    
    it_mole=as.data.table(aggregate(ft_data$`Bromo Inty`, by=list(Molecularclass=ft_data$Molecularclass), FUN=sum))
    it_mole <- it_mole %>% slice(match(c("Lignins","Tannins","Lipids","Proteins","Condensed aromatics",
                                             "Unsaturated hydrocarbons","Carbohydrates","Unassigned"), Molecularclass))
    return(it_mole)
  })
  
  it_Origin <- reactive({
    ft_data=ft_vk()
    
    it_Origin=as.data.table(aggregate(ft_data$`Bromo Inty`, by=list(Originclass=ft_data$Originclass), FUN=sum))
    it_Origin <- it_Origin %>% slice(match(c("Combusetion derived polycyclic aromatic structures",
                                                 "Soil derived PCA with aliphatic chain",
                                                 "Soil derived humics & highly unsaturated",
                                                 "Unsaturated aliphatic compounds",
                                                 "fatty & sulfonic acids"), Originclass))
    return(it_Origin)
  })
  
  
  output$pie_inty <- renderPlotly({
    plotheight <- 600
    if (is.null(input$file1)) { return(NULL)
    }else{
      
      
      pie_inty <- plot_ly(width = 780)
      pie_inty <- pie_inty %>% add_pie(data =it_comp(), labels = ~Chemicalcomposition, values = ~x,
                                       name = "",title="Chemical composition \n ", 
                                       sort = FALSE, domain = list(x = c(0, 0.4), y = c(0.4, 0.9)))
      
      pie_inty <- pie_inty %>% add_pie(data = it_mole(), labels = ~Molecularclass, values = ~x,sort = FALSE,
                                       name = "",title="Molecular class\n ", domain = list(x = c(0.25, 0.75), y = c(0, 0.5)))
      
      pie_inty <- pie_inty %>% add_pie(data = it_Origin(), labels = ~Originclass, values = ~x,sort = FALSE,
                                       name = "",title="Derived class\n ", domain = list(x = c(0.6, 1), y = c(0.4, 0.9)))
      
      pie_inty <- pie_inty %>% layout(title = "Intensity based", showlegend = F,
                                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      pie_inty
    }
    
  })
  
}

shinyApp(ui, server)

##patch note====
##1.6 download selected formula (bug fixed at v1.8)
##v1.7 xlimit setting (200 - 1000) & extend error line
##v1.8 add function 'export for matlab'
##v1.8.2 adjust y axis
##v1.8.3 change charge calculation to manual
##v1.9 add RC step button and saved file
##v1.9.1 y-axis adjust at preRC as -4 to 4, 1st,2ndRC: -2 to 2/ default color chaged as white
##v1.9.2 y-axis adjust method chage: limits adjust by slider value/ change year format yy
##v1.9.3 x-axis adjust method chage: limits adjust by slider value
##v1.9.4 filtering index add
##2.0.1: export ref file
##2.1.1: export matlab file (no filtering)
##2.2.1: add matlab filtering
##2.2.2: bug fix
##2.2.3: Add selection buttion (positive/negative)
##2.2.4: Add export recalibration figure 
##2.2.5: change algorithm of rel. abundance&AI: max to 100%/ if AI less than 0 -> AI=0, bugfix in DBEAI method
##2.2.6: Add service unfiltered data table, add annotation of formula name at error ppm plot, H/C filter debug
##3.0.0: Display filtering statue
##3.1.0: Add lasso selection/ Add save error ppm plot
##3.1.1: event selection based on x (before y)
##3.2.1: add service: export older version format
##3.2.2: add annotation about Positive and negative mode in export file
##3.2.3: add conditional text inRCstep
##4.0.0: add blank removal tab (only tab)
##4.0.1: add blank file selection
##4.0.2: add event button: click the button and then blank removal in selected directory
##4.1.0: complete function for blank remove (only esi mode)
##4.1.1: add ionization source selection
##4.2.0: algorithm fixation of blank removal function. now this function can apply for APPI source 
##4.2.1: re assign colname for matlab format & directly apply for matlab analysis
##4.2.2: diplay plot(error vs m/z) title according to working step
##4.3.0: add intensity (mono inty) and histogram of assigned formula plot
##4.3.1: change location of filtering table
##4.4.0: sample name was automatically updated from file name
##4.4.1: adjust text in recalibration step 
##4.4.2-3: UI replacement
##4.4.4-5: Matlab filtering condition change (DBE<=0 -> DBE>0), show details matlab filtering option
##4.4.6: Hide error message
##5.0.0: display distribution of formula  (chemical composition, molecular class, origin class)
##5.1.0: add pie chart interact with selected formula  (chemical composition, molecular class, origin class)
##5.1.1-2: modify hover option, now hoverinfo showed formula, m/z and chemical/molecular class 
##5.1.3: adjust default y axis range -4 to 4 -> -4.5 to 4.5 / massspectrum data was changed to selected data

#further====
#1. layout adjustment detail
#2. select option treated as remainders on/off molecular division box
#3. annotation of molecular class and origin class in vkplot