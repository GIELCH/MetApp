#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

launcherPath = "/usr/MetIDfyR/"

if(.Platform$OS.type == "windows"){
  homePath = normalizePath("/")
  defaultRscript = "Rscript.exe"
}else{
  homePath = normalizePath("~/")
  defaultRscript = "Rscript"
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # # #
  ### Launcher Panel
  # # # 
  
  callModule(titlePanelMod, id = "t1")
  callModule(launcherMod, id = "launcher", homePath, launcherPath, 
             defaultRscript)
  
  # # #
  ### Input Panel
  # # #
  
  callModule(titlePanelMod, id = "t2")
  # Display template to save input TSV for the compound
  output$mlcTSV = renderRHandsontable({
    template = readr::read_tsv(paste0(launcherPath,"/input/TEMPLATE_start_mlc.tsv"), 
                               show_col_types = FALSE)
    rhandsontable(template, rowHeaders = F)
  })
  
  # Save the input TSV in the input directory
  observeEvent(input$save, {
    
    filled_input = hot_to_r(input$mlcTSV)
    readr::write_tsv(filled_input, paste0(launcherPath, "/input/", input$tsvname, ".tsv"))
    
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
    
    file = paste0(launcherPath, "input/", input$config_name, ".R")
    
    myForm = as.data.frame(cbind(c(configFields, "list_transfo"), 
                                 c(tmp_form[configFields], paste0("\"", launcherPath, "input/list_transfo.tsv\""))))
    
    write(x="#Config file for MetIDfyR", file)
    for(i in 1:nrow(myForm)){
      if( myForm[i,1] == "lib_perso" & length(unlist(libperso()[1])) > 1 ){
        content = paste0("\"", myForm[i,2], "\"")
      }else content = myForm[i,2]
      
      write(paste0(myForm[i,1], " = ", content), file , append=TRUE)
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
  callModule(visualizationMod, id = "visual", homePath = "/datas/output_MetIDfyR/")
    
  
})
  
