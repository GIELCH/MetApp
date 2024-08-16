library(shiny)

labelMandatory = function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

mandatoryCSS = ".mandatory_star { color: red; }"

###
# Launcher UI
###
launcherUI = function(id){
  ns = NS(id)
  fluidPage(
    h4("Required :"),
    p("-Rscript added to PATH. Otherwise, edit the path in the input area below."),
    p("-a TSV file (see Input tab)"),
    p("-where to save the output (a suffix with the date and time of run is automatically attached to the directory name)"),
    p("-a config file (see TEMPLATE_config.R or build it using Configuration tab)"),
    
    br(),
    fluidRow(column(3, labelMandatory(tags$b("Path to input file (.tsv)")), br(),
                    shinyFilesButton(ns("input"), "Select input", 
                                     "Path to input file", multiple = F), br(),
                    textOutput(ns("select_input"))),
             column(3, labelMandatory(tags$b("Path to save the output")),br(),
                    shinyDirButton(ns("outdir"), "Select output", 
                                   "Path to save the output"),
                    textOutput(ns("select_outdir"))),
             column(3, tags$b("Path to config file (.R)"), br(),
                    shinyFilesButton(ns("config"), "Select config file", 
                                     "Path to config file", multiple = F),
                    textOutput(ns("select_config")))
    ),
    
    textInput(ns("command"), label="", width="100%"),
    rclipboardSetup(),
    fluidRow(column(2, uiOutput(ns("clip"))), 
             column(1, actionButton(ns("launch"), label="Go !", icon = icon("play"), class = "btn-info"))),
    hr(),
    tableOutput(ns("jobs")),
    div(uiOutput(ns("logFile"), style="padding:10px"),
        style="background-color:lightgrey; width: 60%"),
    
  )
  
}

###
# Launcher server logic
###
launcherMod = function(input, output, session, homePath, launcherPath, defaultRscript) {
  
  input_data = reactiveValues(infile = "", outdir = "", config = "")
  jobs = reactiveVal(readr::read_csv2("jobs.csv"))
  
  logPath = reactiveVal("")
  
  # INPUT file button
  shinyFileChoose(input, "input", roots = c(home = launcherPath))
  
  observeEvent(input$input, { 
    validate(need( length(unlist(input$input[1])) > 1, "" ))
    input_data$infile = paste( unlist(input$input[1]), collapse = "/" )
    output$select_input = renderText( input_data$infile )
  })
  
  
  # OUT PATH directory button
  shinyDirChoose(input, "outdir", roots = c(home = homePath))
  
  observeEvent(input$outdir, { 
    validate(need( length(unlist(input$outdir[1])) > 1, "" ))
    input_data$outdir = paste(unlist(input$outdir[1]), collapse = "/") 
    output$select_outdir = renderText( input_data$outdir )
  })
  
  # OUT PATH directory button
  shinyFileChoose(input, "config", roots = c(home = launcherPath))
  
  observeEvent(input$config, { 
    validate(need( length(unlist(input$config[1])) > 1, "" ))
    input_data$config = paste(unlist(input$config[1]), collapse = "/")
    output$select_config = renderText( input_data$config )
  })
  
  # Update command field
  observe({
    updateTextInput(session, "command", value = paste0(defaultRscript, " MetIDfyR.R -i ", launcherPath, input_data$infile, 
                                                      " -o ", homePath, input_data$outdir,
                                                      # Optional parameters
                                                      ifelse(input_data$config != "", paste0(" -c ", launcherPath, input_data$config), "")) )
  })
  
  observe({
    # Check mandatory fields are filled, enable submit if TRUE
    mandatoryFilled = input_data$infile != "" & input_data$outdir != ""
    shinyjs::toggleState(id = "launch", condition = mandatoryFilled)
  })
  
  # Add clipboard buttons
  output$clip = renderUI({
    rclipButton("clipbtn", "Copy command", input$command, icon=icon("clipboard"))
  })
  
  # Function to launch MetIdfyR run
  launchMetIDfyR = function(command, logPath, launcherPath){
    
    # Launch MetIDfyR from the package directory
    setwd(launcherPath)
    
    system(paste0(command, " > ", logPath))
    
  }
  
  current_job = eventReactive(input$launch, {
    jobs_table = readr::read_csv2("jobs.csv")
    logPath(tempfile("log_", tmpdir = "/srv/shiny-server/MetApp/"))
    message(paste0("Running MetIDfyR ", basename(logPath())))
    
    # Update table of jobs
    jobs_table = jobs_table %>% add_row(token = basename(logPath()), status="in progress")
    readr::write_csv2(jobs_table, file = "jobs.csv")
    jobs(jobs_table)
      
    x = callr::r_bg(
      func = launchMetIDfyR, args = list(input$command, logPath(), launcherPath),
      supervise = TRUE
    )
    return(x)
  })
  
  logContent = reactive({
    # while the job is active refresh every 5 seconds
    if(current_job()$is_alive()){
      invalidateLater(millis = 5000, session = session)
      
    }else{
      logFile = readLines(logPath(), warn=F)
      jobs_table = readr::read_csv2("jobs.csv")
      
      if(any(grep("END OF", logFile))){
        jobs_table$status[which(jobs_table$token == basename(logPath()))] = "finished"
        readr::write_csv2(jobs_table, file = "jobs.csv")
        jobs(jobs_table)
  
      }else if(any(grep("Error", logFile))){
        jobs_table$status[which(jobs_table$token == basename(logPath()))] = "error"
        readr::write_csv2(jobs_table, file = "jobs.csv")
        jobs(jobs_table)
      }
    }
    # if the log file is created then read it
    if(file.exists(logPath())){
      
      # Read log file and replace \n by 'p' tag
      logFile = readLines(logPath(), warn=F)
      splitText = stringi::stri_split(str = logFile, regex = '\\n')
      replacedText = lapply(splitText, p)
      return(replacedText)
    }
    
  })
  
  output$logFile = renderUI({
    logContent()
  })
  
  output$jobs = renderTable({
    jobs()
  })
}