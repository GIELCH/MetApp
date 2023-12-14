#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

jobs=list()

if(.Platform$OS.type == "windows"){
  homePath = normalizePath("/")
  defaultRscript = "Rscript.exe"
}else{
  homePath = normalizePath("~")
  defaultRscript = "Rscript"
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # # #
  ### Launcher Panel
  # # # 
  callModule(titlePanelMod, id = "t1")
  input_data = reactiveValues(Rscript = "", infile = "", outdir = "", config = "")
  
  # Rscript path
  shinyFileChoose(input, "Rscript", roots = c(home = homePath))
  
  observeEvent(input$Rscript, { 
    validate(need( length(unlist(input$Rscript[1])) > 1, "" ))
    input_data$Rscript = paste( unlist(input$Rscript[1]), collapse = "/" )
    output$select_Rscript = renderText( input_data$Rscript )
  })
  
  # INPUT file button
  shinyFileChoose(input, "input", roots = c(home = homePath))
  
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
  shinyFileChoose(input, "config", roots = c(home = homePath))
  
  observeEvent(input$config, { 
    validate(need( length(unlist(input$config[1])) > 1, "" ))
    input_data$config = paste(unlist(input$config[1]), collapse = "/")
    output$select_config = renderText( input_data$config )
  })
  
  # Update command field
  observe({
    rscript_path = ifelse(input_data$Rscript != "", input_data$Rscript, defaultRscript) 
    updateTextInput(session, "command", 
                    value= paste0(rscript_path, " MetIDfyR.R -i ~", input_data$infile, 
                                  " -o ~", input_data$outdir,
                                  # Optional parameters
                                  ifelse(input_data$config != "", paste0(" -c ~", input_data$config), "")) )
  })
  
  observe({
    # Check mandatory fields are filled, enable submit if TRUE
    mandatoryFilled <- input_data$infile != "" & input_data$outdir != "" 
    shinyjs::toggleState(id = "launch", condition = mandatoryFilled)
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    rclipButton("clipbtn", "Copy command", input$command, icon=icon("clipboard"))
  })
  
  observeEvent(input$launch, {
    
    token <- UUIDgenerate()
    message(paste0("Running MetIDfyR ", token))
    # the if statement is to avoid rerunning a job again
    if(is.null(jobs[[token]])){
      appPath = getwd()
      setwd("../")
      write(paste0("nohup ", input$command, " > ~/", input_data$outdir, "/run.log &" ), file = "command.sh")
      
      # call the job in the background session
      jobs[[token]] <<- callr::r_bg(system("bash command.sh"), args = list(token = token))
      
      setwd(appPath)
    }
    
  })
  
  # # #
  ### Input Panel
  # # #
  
  callModule(titlePanelMod, id = "t2")
  # Display template to save input TSV for the compound
  tsv_template = as.data.frame(readr::read_tsv("input/TEMPLATE_start_mlc.tsv", show_col_types = FALSE))
  output$mlcTSV = DT::renderDataTable(tsv_template, selection = 'none', rownames = FALSE, editable=T, server=F)
  proxy = dataTableProxy("mlcTSV")
  
  # Update the table when a cell is edited
  observeEvent(input$mlcTSV_cell_edit, {
    info = input$mlcTSV_cell_edit
    i = info$row
    j = info$col + 1
    v = info$value
    tsv_template[i, j] <<- DT:::coerceValue(v, tsv_template[i, j])
  })
  
  # Add new row in table 
  observeEvent(input$add, {
    proxy %>% addRow(as.data.frame(readr::read_tsv("input/TEMPLATE_start_mlc.tsv")))
  })
  
  # Save the input TSV in the input directory
  observeEvent(input$save, {
    
    readr::write_tsv(tsv_template, paste0("input/", input$tsvname, ".tsv"))
    
    shinyalert(
      title = "MetApp",
      text = paste0("You saved ", input$tsvname, ".tsv in the input directory."),
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # # #
  ### Configuration Panel
  # # #
  
  callModule(titlePanelMod, id = "t3")
  configFields = c("lib_perso", "bool_phase_1", "bool_phase_2", "cores", "nb_transformation", "min_peak_intensity", "mz_ppm", "rt_windows", 
                   "nb_scan", "wdw_mz_ms2", "minimum_mz")
  
  fieldsAll = c("bool_phase_1", "bool_phase_2", "cores", "nb_transformation", "min_peak_intensity", "mz_ppm", 
                "rt_windows", "nb_scan", "wdw_mz_ms2", "minimum_mz")
  fieldsSelect = c("lib_perso", "ms2_reference")
  
  
  # MS2 REFERENCE file button
  shinyFileChoose(input, "ms2_reference", roots = c(home = homePath))
  ms2_tsv = reactive(input$ms2_reference)
  observeEvent(input$ms2_reference, { 
    validate(need( length(unlist(ms2_tsv()[1])) > 1, "" ))
    output$select_ms2_ref = renderText( paste( unlist(ms2_tsv()[1]), collapse = "/" ) )
  })
  
  # R LIBRARY directory button
  shinyDirChoose(input, "lib_perso", roots = c(home = homePath))
  libperso = reactive(input$lib_perso)
  observeEvent(input$lib_perso, { 
    validate(need( length(unlist(libperso()[1])) > 1, "" ))
    output$select_lib_perso = renderText( paste(unlist(libperso()[1]), collapse = "/") )
  })
  
  observe({
    # Check mandatory fields are filled, enable submit if TRUE
    mandatoryFilled <- input$config_name != "" 
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Store data from the form
  formData = reactive({
    config = sapply(fieldsSelect, function(x) paste(unlist(input[[x]][1]), collapse = "/"))  # Generate path
    config = c(config, sapply(fieldsAll, function(x) input[[x]]))  # Extract parameters values
    t(config)
  }) 
  
  # When submit is pressed
  observeEvent(input$submit, {
    
    tmp_form = tibble::as_tibble(formData())
    
    file = paste0("input/", input$config_name, ".R")
    
    myForm = as.data.frame(cbind(c(configFields, "list_transfo"), 
                                 c(tmp_form[configFields], "\"input/list_transfo.tsv\"")))
    
    write(x="#Config file for MetIDfyR", file)
    for(i in 1:nrow(myForm)){
      if( myForm[i,1] == "lib_perso" & length(unlist(libperso()[1])) > 1 ){
        content = paste0("\"", myForm[i,2], "\"")
      }else content = myForm[i,2]
      
      write(paste0(myForm[i,1], " = ", ifelse(content=="", NA, content)), file , append=TRUE)
    }
    
    updateTextInput(session, "config_name", value = "", placeholder = "config.R")
    
    shinyalert(
      title = "MetApp",
      text = "The file is saved in the input directory.",
      html = FALSE,
      type = "success",
      showConfirmButton = TRUE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
    
  })
    
  
  observeEvent(input$resetbut, {
    shinyjs::reset("form")
    output$select_input = renderText({})
    output$select_outdir = renderText({})
    output$select_lib_perso = renderText({})
    output$select_ms2_ref = renderText({})
  })
  
  
  # # #
  ### Visualization panel
  # # #

  
  callModule(titlePanelMod, id = "t4")
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
  
  #Update the metabolites list when the output folder change
  Product = reactive({
    validate(need( length(path()) > 0, "Choose a directory"))
    total_table = readr::read_tsv(list.files(path(), pattern = "out.*\\.tsv$", full.names = T))
    num_col = names(Filter(is.numeric, total_table))
    total_table[num_col] = total_table %>% Filter(f=is.numeric) %>% round(3)
    total_table$intensity = scales::scientific(total_table$intensity)
    total_table = plyr::rename(total_table, c("name"="Name", "formula"="Formula", "polarity"="Polarity", "adduct"="Adduct", "mz"="m/z", 
                                         "diff"="Change", "rt"="RT", "intensity"="Intensity", "abscore"="iAScore", 
                                         "dotp_ms2"="MS2 dotp", "common_ms2_peak"="MS2p", "mono_ppm"="Delta m/z (ppm)",
                                         "index_peak"="Index", "nb_transfo"="Nb Transfo", "score"="Score"))
    total_table %>%
      arrange(desc(Score))
  })
  
  output$header = renderText({ 
    if(length(path()) > 0){
      paste0("Currently open: ", unique(Product()$Name) ) 
    }else{
      return("Select the output folder containing the file \"out_...tsv\" :")
    }
  })
  
  observe({ 
    shinyjs::toggleState(id = "report", condition = length(path()) & length(input$table_rows_selected)) 
    
    output$report = downloadHandler(
      filename = "report.pdf",
      
      content = function(file){
        rmarkdown::render("report_template.Rmd", output_file = file , quiet = T)
      }
    )
    updateCheckboxGroupInput(session, "columns", choices = names(Product()),
                             selected =c("Formula", "Polarity", "Change", "m/z", "RT", "MS2p", "iAScore",
                                         "MS2 dotp", "Delta m/z (ppm)", "Intensity", "Score"))
  })
  
  #Print table
  output$table <- DT::renderDataTable({
    need(length(path()) > 0, "")
    datatable(Product()[, input$columns], rownames = F, selection = "multiple", 
              options = list(scrollX = TRUE, searchHighlight = T))
  })
  
  #Observe click event on table rows
  observeEvent(input$table_cell_clicked, {
    need(Product(), "")
    if(length(input$table_cell_clicked) > 0){
      data = Product()[input$table_cell_clicked$row, ]
      
      form = data$Formula
      peak = data$Index
      find_figure = list.files(path(), pattern = paste0(form, "_", peak), recursive = T, full.names = T)
      
      polar = ifelse(data$Polarity=="Positive mode", "POS", "NEG")
      updateTabsetPanel(session, "polarity", selected = polar)
      
      updateSelectInput(session, input$polarity, selected = form )
      observeEvent(input[[input$polarity]], {
        
        updateSelectInput(session, paste0(input$polarity, "_peak"), selected = peak)
      })
      
    }
  })
  
  ### POS ###
  
  filesname_pos = reactive({ 
    need(length(path()), "")
    list.files(path(), recursive = T, pattern = ".*_POS.svg|.*plus.*svg|.*_POS.png|.*plus.*png")
  })
  
  observe({
    validate(need(any(Product()$Polarity == "Positive mode"), ""))
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
    validate(need(any(Product()$Polarity == "Positive mode"), ""))
    filename = grep(paste0(input$POS, "_", input$POS_peak, "_"), filesname_pos(), value = T)
    
    list(src = file.path(path(), filename),
         alt = "Missing figure in pos",
         width = 800,
         height = 800)
  }, deleteFile = F)
  
  
  output$table_POS <- renderTable({
    validate(need(any(Product()$Polarity == "Positive mode"), ""))
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
    validate(need(any(Product()$Polarity == "Negative mode"), ""))
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
    validate(need(any(Product()$Polarity == "Negative mode"), ""))
    filename = grep(paste0(input$NEG, "_", input$NEG_peak, "_"), filesname_neg(), value=T)
    
    list(src = file.path(path(), filename),
         alt = "Missing figure in NEG",
         width = 800,
         height = 800)
  }, deleteFile = F)
  
  
  output$table_NEG <- renderTable({
    validate(need(any(Product()$Polarity == "Negative mode"), ""))
    tabsname_neg = list.files(path(), recursive = T, 
                              pattern = paste0(input$NEG, "_", input$NEG_peak, ".*_NEG.tsv|",
                                               input$NEG,"_", input$NEG_peak, "_.*minus.*tsv"))
    validate(need( length(path()) > 0, "Select a directory"),
             need( length(tabsname_neg) == 1, "No MS2 table to show"))
    table=readr::read_tsv(file.path(path(), tabsname_neg))
    table
  })
    
  
})
  
