library(shiny)

###
# Visualization UI
###
visualizationUI = function(id){
  ns = NS(id)
  
  fluidPage(
    h4("Here you can visualize the metabolites output produced by MetIDfyR"),
    
    fluidRow(column(5, tags$b("Comment area"),
                    textAreaInput(ns("comment"), "", height = "100%", width = "200%", resize = "both"))
    ), 
    
    br(), br(),
    h4(textOutput(ns("header"))),
    shinyDirButton(ns("dir"), "Select directory", "Upload"),
    downloadButton(ns("report"), "Save report"), br(),
    
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(ns("columns"),"Select Columns"),
        width = 2
      ),
      
      mainPanel(
        DT::DTOutput(ns('table'))
      )
    )
    
    ,
    
    hr(),
    
    # Add tabsets 
    tabsetPanel(type = "tabs", id = ns("polarity"),
                tabPanel(title = "Positive", value = "POS", 
                         tags$br(),
                         fluidRow(column(4, selectInput(ns("POS"), "Select a metabolite", choices = NULL)),
                                  column(4, selectInput(ns("POS_peak"), "Select a peak", choices = NULL))),
                         fluidRow(column(8, imageOutput(ns("figure_POS"))),
                                  column(4, tableOutput(ns("table_POS"))))
                ),
                
                tabPanel(title = "Negative", value = "NEG",
                         tags$br(),
                         fluidRow(column(4, selectInput(ns("NEG"), "Select a metabolite", choices = NULL)),
                                  column(4, selectInput(ns("NEG_peak"), "Select a peak", choices = NULL))),
                         fluidRow(column(8, imageOutput(ns("figure_NEG"))),
                                  column(4, tableOutput(ns("table_NEG"))))
                )
    )
  )
}

###
# Visualization server logic
###

visualizationMod = function(input, output, session, homePath){
  shinyDirChoose(input, "dir", roots = c(home = homePath))
  dir = reactive(input$dir)
  path = reactive({ 
    home = homePath
    if(length(unlist(dir()[1])) > 1){
      
      file.path(home, paste(unlist(dir()$path[-1]), collapse = "/"))
    }else{
      file.path()
    }
  })
  
  output$header = renderText({ 
    validate(need(!is.null(FullTable()), "Select the output folder containing the file \"out_...tsv\" :"))
    paste0("Currently open: ", DrugInterest() ) 
  })
  
  # Disable report button at launch
  shinyjs::disable("report")
  
  FullTable = reactiveVal(NULL)
  DisplayTable = reactiveVal(NULL)
  
  DrugInterest = reactiveVal()
  
  # Observe the path selected
  observeEvent(path(), {
    validate(need( length(path()) > 0, "Choose a directory"))
    
    # Read the result table and update colnames
    total_table = readr::read_tsv(list.files(path(), pattern = "out.*\\.tsv$", full.names = T))
    num_col = names(Filter(is.numeric, total_table))
    total_table[num_col] = total_table %>% Filter(f=is.numeric) %>% round(3)
    total_table$intensity = scales::scientific(total_table$intensity)
    total_table = plyr::rename(total_table, c("name"="Name", "formula"="Formula", "polarity"="Polarity", "adduct"="Adduct", "mz"="m/z", 
                                              "diff"="Change", "rt"="RT", "intensity"="Intensity", "abscore"="iAScore", 
                                              "dotp_ms2"="MS2 dotp", "common_ms2_peak"="MS2p", "mono_ppm"="Delta m/z (ppm)",
                                             "index_peak"="Index", "nb_transfo"="Nb Transfo", "score"="Score",
                                             "rintensity"="Relative intensity", "transfo"="Transformation"))
    # Store full table, name of the target
    FullTable(total_table %>%
      arrange(desc(Score)))
    DrugInterest(unique(FullTable()$Name))
    
    # Update the list with colnames of the full table and select some
    updateCheckboxGroupInput(session, "columns", choices = names(FullTable()),
                             selected = c("Formula", "Polarity", "Change", "m/z", "RT", "MS2p", "iAScore",
                                         "MS2 dotp", "Delta m/z (ppm)", "Intensity", "Score"))
    
    # Init the table to display with selected columns
    DisplayTable(FullTable()[, input$columns])
  })
  
  # When the full table is loaded 
  # Activate the report button if there is some row selected
  observe({ 
    req(!is.null(FullTable()))
    shinyjs::toggleState(id = "report", condition = length(input$table_rows_selected)) 
  })
  
  # Report download
  output$report = downloadHandler(
    filename = "report.pdf",
    
    content = function(file){
      rmarkdown::render("report_template.Rmd", output_file = file , quiet = T)
    }
  )
  
  # Display the table
  output$table <- DT::renderDT({
    req(!is.null(DisplayTable()))
    
    isolate(DisplayTable())
  })
  
  # Observe change in columns selection and update the DisplayTable
  observeEvent(input$columns, {
    req(!is.null(DisplayTable()))
    
    DisplayTable(FullTable()[, input$columns])
  })
  
  # Use proxy to update isolate DisplayTable
  proxy = dataTableProxy("table")
  
  observe({
    req(!is.null(DisplayTable()))
    
    replaceData(proxy, DisplayTable())
  })
  
  #Observe click event on table rows
  observeEvent(input$table_cell_clicked, {
    req(length(input$table_cell_clicked) > 0, !is.null(FullTable()))
    
    data = FullTable()[input$table_cell_clicked$row,]
    
    form = data$Formula
    peak = data$Index
    find_figure = list.files(path(), pattern = paste0(form, "_", peak), recursive = T, full.names = T)
    
    polar = ifelse(data$Polarity=="Positive mode", "POS", "NEG")
    updateTabsetPanel(session, "polarity", selected = polar)
    
    updateSelectInput(session, input$polarity, selected = form )
    observeEvent(input[[input$polarity]], {
      
      updateSelectInput(session, paste0(input$polarity, "_peak"), selected = peak)
    })
    
  })
  
  ### POS ###
  
  filesname_pos = reactive({ 
    validate(need(length(path()), ""))
    list.files(path(), recursive = T, pattern = ".*_POS.svg|.*plus.*svg|.*_POS.png|.*plus.*png")
  })
  
  observe({
    req(!is.null(FullTable()))
    validate(need(any(FullTable()$Polarity == "Positive mode"), ""))
    metabolites = unique(lapply(basename(filesname_pos()), function(x) strsplit(x, '_')[[1]][1]))
    
    updateSelectInput(session, "POS", choices = metabolites )
  })
  
  #Update the chromatogram peaks list
  observeEvent(input$POS, {
    
    current_metabolite_file = list.files(path(), recursive = T, pattern = paste0("^", input$POS, "_.*_POS|^", input$POS,"_.*plus"))
    peaks = unique(lapply(basename(current_metabolite_file), function(x) strsplit(x, '_')[[1]][2]))
    
    updateSelectInput(session, "POS_peak", choices = peaks )
    
  })
  
  #Update the POS page image
  output$figure_POS = renderImage({
    req(input$POS, input$POS_peak, length(path()) > 0)
    validate(need(any(FullTable()$Polarity == "Positive mode"), ""))
    filename = grep(paste0(input$POS, "_", input$POS_peak, "_"), filesname_pos(), value = T)
    
    list(src = file.path(path(), filename),
         alt = "Missing figure in pos",
         width = 800,
         height = 800)
  }, deleteFile = F)
  
  
  output$table_POS <- renderTable({
    validate(need(any(FullTable()$Polarity == "Positive mode"), ""))
    tabsname_pos = list.files(path(), recursive = T, 
                              pattern = paste0(input$POS, "_", input$POS_peak, ".*_POS.tsv|",
                                               input$POS,"_", input$POS_peak, "_.*plus.*tsv"))
    validate(need( length(path()) > 0, "Select a directory"),
             need( length(tabsname_pos) == 1, "No MS2 table to show"))
    table=readr::read_tsv(file.path(path(), tabsname_pos))
    table
  })
  
  
  ### NEG ###
  
  filesname_neg = reactive({
    validate(need(any(FullTable()$Polarity == "Negative mode"), ""))
    list.files(path(), recursive = T, pattern = ".*_NEG.png|.*minus.*png|.*_NEG.svg|.*minus.*svg")
  })
  
  observe({
    metabolites = unique(lapply(basename(filesname_neg()), function(x) strsplit(x, '_')[[1]][1]))
    
    updateSelectInput(session, "NEG", choices = metabolites )
    
  })
  
  
  #Update the chromatogram peaks list
  observeEvent(input$NEG, {
    
    current_metabolite_file = list.files(path(), recursive = T, pattern = paste0("^", input$NEG,".*_NEG|^", input$NEG, "_.*minus"))
    peaks = unique(lapply(basename(current_metabolite_file), function(x) strsplit(x, '_')[[1]][2]))
    
    updateSelectInput(session, "NEG_peak", choices = peaks )
    
  })
  
  #Update the NEG page image
  
  output$figure_NEG = renderImage({
    req(input$NEG, input$NEG_peak, length(path()) > 0)
    validate(need(any(FullTable()$Polarity == "Negative mode"), ""))
    filename = grep(paste0(input$NEG, "_", input$NEG_peak, "_"), filesname_neg(), value=T)
    
    list(src = file.path(path(), filename),
         alt = "Missing figure in NEG",
         width = 800,
         height = 800)
  }, deleteFile = F)
  
  
  output$table_NEG <- renderTable({
    validate(need(any(FullTable()$Polarity == "Negative mode"), ""))
    tabsname_neg = list.files(path(), recursive = T, 
                              pattern = paste0(input$NEG, "_", input$NEG_peak, ".*_NEG.tsv|",
                                               input$NEG,"_", input$NEG_peak, "_.*minus.*tsv"))
    validate(need( length(path()) > 0, "Select a directory"),
             need( length(tabsname_neg) == 1, "No MS2 table to show"))
    table=readr::read_tsv(file.path(path(), tabsname_neg))
    table
  })
  
}