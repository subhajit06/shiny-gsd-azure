############ Shiny UI interface #################
############ for generating JSON object #########
#################################################

## Only run this example in interactive R sessions
######### Nov 03, 2020 ###############
######## @Subhajit Sengupta

#rm(list = ls())
#setwd("D:/R_test/Azure/")

library(shiny)
library(shinydashboard)

library(jsonvalidate)
library(readr)

library(gsDesign)


######################################

source("./queueLib.R")

###################
dH <- dashboardHeader(title = "gsDesign-AzureQstor")
  
dS <- dashboardSidebar(width = '350px',
    
    sidebarMenu( withMathJax(),
                selectInput("functions", "Choose the function",
                            c("nBinomial"   = "nB",
                              "testBinomial"   = "tB",
                              "simBinomial" = "sB")),
                
                
                htmlOutput('ST1'),
                
                conditionalPanel(
                  condition = "input.functions == 'nB'",
                  fluidRow(column(5, 
                                  numericInput('p1', '\\(p_1\\)', value = 0.1, min = 0.0, max=1.0),
                                  numericInput('alpha', '\\(\\alpha\\)', value = 0.025, min = 0.0, max=1.0),
                                  numericInput('delta0','\\(\\delta_0\\)', value = 0, min = -1.0, max=1.0),
                                  selectInput('sided', 'sided', choices = list("one" = 1, "two" =  2)),
                                  numericInput('n', '\\(n\\)', value=-1)),
                           column(5, 
                                  numericInput('p2', '\\(p_2\\)', value = 0.5, min = 0.0, max=1.0),
                                  numericInput('beta', '\\(\\beta\\)',  value = 0.2, min = 0.0, max=1.0),
                                  numericInput('ratio', 'ratio', value = 1),
                                  selectInput('scale', 'scale', choices = list("Difference", "RR", "OR"), selected = "Difference"),
                                  checkboxInput('comp_power', 'Compute power', value = FALSE)))),
                
                conditionalPanel(
                  condition = "input.functions == 'tB'",
                  fluidRow(column(5, 
                                  numericInput('x1', '\\(x_1\\)', value = 100),
                                  numericInput('n1', '\\(n_1\\)', value = 1000),
                                  numericInput('delta0','\\(\\delta_0\\)', value = 0, min = -1.0, max=1.0),
                                  checkboxInput('adj', 'adjusted', value = FALSE),
                                  checkboxInput('chisq', label = '\\(\\chi^2\\)', value = FALSE)),
                           column(5, 
                                  numericInput('x2', '\\(x_2\\)', value = 500),
                                  numericInput('n2', '\\(n_2\\)',  value = 1000),
                                  selectInput('scale', 'scale', choices = list("Difference", "RR", "OR"), selected = "Difference"),
                                  numericInput('tol','tolerance', value = 1e-11)))),
                
                conditionalPanel(
                  condition = "input.functions == 'sB'",
                  fluidRow(column(5, 
                                  numericInput('p1', '\\(p_1\\)', value = 0.1, min = 0.0, max=1.0),
                                  numericInput('n1', '\\(n_1\\)', value = 1000),
                                  numericInput('delta0','\\(\\delta_0\\)', value = 0, min = -1.0, max=1.0),
                                  checkboxInput('adj', 'adjusted', value = FALSE),
                                  checkboxInput('chisq', label = '\\(\\chi^2\\)', value = FALSE)),
                           column(5, 
                                  numericInput('p2', '\\(p_2\\)', value = 0.5, min = 0.0, max=1.0),
                                  numericInput('n2', '\\(n_2\\)',  value = 1000),
                                  selectInput('scale', 'scale', choices = list("Difference", "RR", "OR"), selected = "Difference"),
                                  numericInput('nsim','N-Simulations', value = 100, min=1, max=10000)))),
                  hr()           
    )
)
  
dB <- dashboardBody(
  mainPanel(width = 10,
            hr(),
            fluidRow(column(6,
                      actionButton("init" ,"Intialize I/O Queues", icon("asterix"), class = "btn-primary")),
                     column(6,
                      actionButton("upload" ,"Upload Function to I-Queue", icon("cloud-upload"), class = "btn-primary"))),
            hr(),
            htmlOutput("msgCount"),
            #actionButton("showAll" ,"Show Q", icon("border-all"), class = "btn-primary"),
            htmlOutput('allMsgStr'),
            hr(),
            #dataTableOutput('paramsTable'),
            #hr(),
            fluidRow(column(4,
                            actionButton("runAll" ,"Run All", icon("rotate-right	"), class = "btn-primary")),
                     column(4,
                            actionButton("done" ,"Not Started", icon("hourglass"), class = "btn-warning")),
                     column(4,
                            actionButton("showAll" ,"Show Results (from O-Queue)", icon("cloud-download"), class = "btn-primary"))),
            hr(),
            htmlOutput('allResStr')
  )
)

ui <- dashboardPage(header=dH, sidebar=dS, body=dB)

server <- function(input, output,session) {
  
########## reactive variables ##################  
  qStruct <- reactiveValues(func=NULL)
  qMsg    <- reactiveValues(nCnt = 0)
  qAllMsg    <- reactiveValues(tmpStr = "")
  results <- reactiveValues(tmpStr = "<h4> <b> Results: </b></h4>")
  output$ST1 <- renderText({
    paste0("<h4>&nbsp&nbsp&nbsp Enter parameters</h4>")
  })
################ init part ###################
  observeEvent(input$init,{
    
    in_qname = "input-test-01"
    in_q = create_queue_by_name(in_qname)
    out_qname = "output-test-01"
    out_q = create_queue_by_name(out_qname)
    
    #### clearing previous runs
    in_q$clear()
    out_q$clear()
    
    qStruct$in_q = in_q
    qStruct$in_qname = in_qname
    qStruct$out_q = out_q
    qStruct$out_qname = out_qname
    
    cat("** setQ_params: ", qStruct$in_qname,"/",qStruct$out_qname," **\n")
    
    output$msgCount <- renderText({
      paste("<h4><b>No message in Queue</b></h4>")
    })
  
  })
  
  
  ###########################
  
    
  ############# show result part ###############
  observeEvent(input$showAll,{
    
    for(i in 1:qMsg$nCnt){
      res = get_params_from_queue(qStruct$out_q)
      func = res[[1]]
      if(func == "nB") 
        func_str = "Sample size: "
      if (func == 'power')
        func_str = "Power: "
      if(func == "tB")
        func_str = "Test statistic: "
      if(func == "sB")
        func_str = "Simulated test statistic: "
      
          
      s3 = paste0("(",i,") ","<b>",func_str, "</b>", toString(res[[2]]),"<br><br>")
      results$tmpStr = paste0(results$tmpStr, s3)
    }
    
    output$allResStr <- renderText({ 
      cat("** result table"," **\n")
      results$tmpStr
    })
    
  })
  ################# upload part ##########
  
  observeEvent(input$upload,{
    
    dT = NULL
    in_q_msg = NULL
    cat("** getInputParams(): ", qStruct$in_qname," / ", qStruct$out_qname, " **\n")
    
    output$msgCount <- renderText({
      paste("Design Queue Message Count: ", "<b>",qMsg$nCnt, "</b> <br> <br><h4>Input Queue: </h4>")
    })
    
    if(input$functions == 'nB'){
      
      dT$func = input$functions
      dT$p1 = input$p1
      dT$p2 = input$p2
      dT$alpha = input$alpha
      dT$beta = input$beta
      dT$delta0 = input$delta0
      dT$ratio = input$ratio
      dT$sided = ifelse(input$sided=="1",1,2)
      dT$scale = input$scale
      dT$n = ifelse(input$comp_power == FALSE, -1, input$n) 
      
      
      nB_json = NULL
      nB_json$functions = dT$func
      nB_json$parameters$p1 = dT$p1
      nB_json$parameters$p2 = dT$p2
      nB_json$parameters$alpha = dT$alpha
      nB_json$parameters$beta = dT$beta
      nB_json$parameters$delta0 = dT$delta0
      nB_json$parameters$ratio = dT$ratio
      nB_json$parameters$sided = dT$sided
      nB_json$parameters$scale = dT$scale
      nB_json$parameters$n = dT$n

      in_q_msg = nB_json
      s1 = paste0("<b>Function: </b> nBinomial; <b>Parameters: </b>", toString(in_q_msg$parameters))
      
      gsDesignInput = jsonlite::toJSON(nB_json, pretty = TRUE)
      con = file("./json/gsDesign_Input_nB_test.json")
      writeLines(gsDesignInput, con) 
      close(con=con)
      
      gsDesignInputSchemaStr = readr::read_file("./json/gsDesign_Input_Schema_nB.json")
      gsDesignInputStr       = readr::read_file("./json/gsDesign_Input_nB_test.json")

      validFormat = jsonvalidate::json_validate(gsDesignInputSchemaStr, gsDesignInputStr, verbose = TRUE)
      cat("[nB] validFormat = ", validFormat,"\n")

     
    }
    if(input$functions == 'tB'){
      dT$func = input$functions
      dT$x1 = input$x1
      dT$x2 = input$x2
      dT$n1 = input$n1
      dT$n2 = input$n2
      dT$delta0 = input$delta0
      dT$scale = input$scale
      dT$adj = ifelse(input$adj == FALSE, 0, 1)
      dT$chisq = ifelse(input$chisq == FALSE, 0, 1)
      dT$tol = input$tol
      
      tB_json = NULL
      tB_json$functions = dT$func
      tB_json$parameters$x1 = dT$x1
      tB_json$parameters$x2 = dT$x2
      tB_json$parameters$n1 = dT$n1
      tB_json$parameters$n2 = dT$n2
      tB_json$parameters$delta0 = dT$delta0
      tB_json$parameters$scale = dT$scale
      tB_json$parameters$adj = dT$adj
      tB_json$parameters$chisq = dT$chisq
      tB_json$parameters$tol = dT$tol
      
      in_q_msg = tB_json
      s1 = paste0("<b>Function: </b> testBinomial; <b>Parameters: </b>", toString(in_q_msg$parameters))
      
      gsDesignInput = jsonlite::toJSON(tB_json, pretty = TRUE)
      con = file("./json/gsDesign_Input_tB_test.json")
      writeLines(gsDesignInput, con) 
      close(con=con)
      
      gsDesignInputSchemaStr = readr::read_file("./json/gsDesign_Input_Schema_tB.json")
      gsDesignInputStr       = readr::read_file("./json/gsDesign_Input_tB_test.json")
      
      validFormat = jsonvalidate::json_validate(gsDesignInputSchemaStr, gsDesignInputStr, verbose = TRUE)
      cat("[tB] validFormat = ", validFormat,"\n")
      
      
    }
    if(input$functions == 'sB'){
      dT$func = input$functions
      dT$p1 = input$p1
      dT$p2 = input$p2
      dT$n1 = input$n1
      dT$n2 = input$n2
      dT$delta0 = input$delta0
      dT$scale = input$scale
      dT$adj = ifelse(input$adj == FALSE, 0, 1)
      dT$chisq = ifelse(input$chisq == FALSE,0, 1) 
      dT$nsim = input$nsim
      
      sB_json = NULL
      sB_json$functions = dT$func
      sB_json$parameters$p1 = dT$p1
      sB_json$parameters$p2 = dT$p2
      sB_json$parameters$n1 = dT$n1
      sB_json$parameters$n2 = dT$n2
      sB_json$parameters$delta0 = dT$delta0
      sB_json$parameters$scale = dT$scale
      sB_json$parameters$adj = dT$adj
      sB_json$parameters$chisq = dT$chisq
      sB_json$parameters$nsim = dT$nsim
      
      in_q_msg = sB_json
      s1 = paste0("<b>Function: </b> simBinomial; <b>Parameters: </b>", toString(in_q_msg$parameters))
      
      gsDesignInput = jsonlite::toJSON(sB_json, pretty = TRUE)
      con = file("./json/gsDesign_Input_sB_test.json")
      writeLines(gsDesignInput, con) 
      close(con=con)
      
      gsDesignInputSchemaStr = readr::read_file("./json/gsDesign_Input_Schema_sB.json")
      gsDesignInputStr       = readr::read_file("./json/gsDesign_Input_sB_test.json")
      
      validFormat = jsonvalidate::json_validate(gsDesignInputSchemaStr, gsDesignInputStr, verbose = TRUE)
      cat("[sB] validFormat = ", validFormat,"\n")
    }
    
    
    if(validFormat){
      
      put_params_in_queue(qStruct$in_q, in_q_msg)
      qMsg$nCnt = qMsg$nCnt+1 
      s1 = paste0("(",qMsg$nCnt,") ",s1)
      qAllMsg$tmpStr = paste0(qAllMsg$tmpStr, s1,"<br>")
      
    }
    
    dT = as.data.frame(dT)
    
    
    output$allMsgStr <- renderText({
      (qAllMsg$tmpStr)
    })
    
    #output$paramsTable <- renderDataTable({ 
    #  dT 
    #})
    
  
    
  })
  
  ################ run part #################
  observeEvent(input$runAll,{
    
    updateActionButton(session,"done",
                       label = "Running...",
                       icon = icon("hourglass-half"))
  
    for(i in 1:qMsg$nCnt){
      
      paramsList = get_params_from_queue(qStruct$in_q)
      
      cat("** runEngine"," **\n")
      
      fName = paramsList$functions
      if(fName == 'nB'){
        p1 = paramsList$parameters$p1
        p2 = paramsList$parameters$p2
        alpha = paramsList$parameters$alpha
        beta = paramsList$parameters$beta 
        delta0 = paramsList$parameters$delta0
        ratio = paramsList$parameters$ratio
        sided = paramsList$parameters$sided
        scale = paramsList$parameters$scale
        n = paramsList$parameters$n
        #cat("p1: ",p1," p2: ", p2, " alpha: ",alpha, " beta: ", beta,"\n")
        #cat("delta0: ",delta0, " ratio: ", ratio, " sided: ",sided, " scale: ",scale," n:",n,"\n")
        
        if(n == -1){
          
          nResult = gsDesign::nBinomial(p1=p1, p2=p2, alpha=alpha, beta=beta, delta0=delta0, 
                            ratio=ratio, sided=sided, scale=scale, n=NULL)
          
          q_out_msg = list(fName,ceiling(nResult))
          
          
        }else{
          
          pwResult = gsDesign::nBinomial(p1=p1, p2=p2, alpha=alpha, beta=beta, delta0=delta0, 
                              ratio=ratio, sided=sided, scale=scale, n=n)
          q_out_msg = list('power',pwResult)
        }
        
      }
      if(fName == 'tB'){
        x1 = paramsList$parameters$x1
        x2 = paramsList$parameters$x2
        n1 = paramsList$parameters$n1
        n2 = paramsList$parameters$n2 
        delta0 = paramsList$parameters$delta0
        chisq = paramsList$parameters$chisq 
        adj = paramsList$parameters$adj 
        scale = paramsList$parameters$scale
        tol = paramsList$parameters$tol
        
        testResult = testBinomial(x1=x1, x2=x2, n1=n1, n2=n2,
                                    delta0=delta0, chisq=chisq, adj=adj,
                                    scale=scale, tol=tol)
        q_out_msg = list(fName,testResult)
        
      }
      if(fName == 'sB'){
        p1 = paramsList$parameters$p1
        p2 = paramsList$parameters$p2
        n1 = paramsList$parameters$n1
        n2 = paramsList$parameters$n2 
        delta0 = paramsList$parameters$delta0
        nsim = paramsList$parameters$nsim
        chisq = paramsList$parameters$chisq 
        adj = paramsList$parameters$adj 
        scale = paramsList$parameters$scale
        
        simResults = simBinomial(p1=p1, p2=p2, n1=n1, n2=n2, nsim=nsim, 
                            delta0=delta0, chisq=chisq, adj=adj, scale=scale)
        
        q_out_msg = list(fName,simResults)
      }
      
      put_params_in_queue(qStruct$out_q, q_out_msg)
    }
    updateActionButton(session, "done",
                       label = "Done",
                       icon = icon("hourglass-end"))
  })
  
  
}

##################################################
# Run the app
gsDesign_app = shinyApp(ui, server)
#runApp(gsDesign_app)
runApp(gsDesign_app, host="0.0.0.0", port = 29579, launch.browser = TRUE)



