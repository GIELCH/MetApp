#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load("shiny", "shinythemes", "shinyFiles", "DT", "shinyjs", "parallel", 
               "shinyalert", "dplyr", "rclipboard","rmarkdown", "rsvg", "rhandsontable")

source("modules/titlePanelMod.R")
source("modules/launcherMod.R")
source("modules/visualizationMod.R")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

mandatoryCSS <- ".mandatory_star { color: red; }"

#### UI ####

navbarPage("MetApp for MetIDfyR",
           theme = shinytheme("yeti"),
           
           
# Launcher for MetIDfyR
           tabPanel("Launcher",
                    
                    titlePanelUI(id = "t1", name = "MetIDfyR launcher"),
                    
                    launcherUI(id="launcher")
                    
                    ),
           
           
# Input file creation
           tabPanel("Input",
                    titlePanelUI(id="t2", "Input file generation"),
                    h4("Fill the following table and save it if you don't have an input TSV."),
                    p("The file will be saved in the input directory."),
                    p("Leave the cell EMPTY if there's nothing to put in."),
                    rHandsontableOutput('mlcTSV'),
                    br(),
                    textInput("tsvname", "Name of the input TSV"),
                    actionButton("save", "Save tsv", class="btn-info")
                    ),
# Configuration file creation
           tabPanel("Configuration",
                    shinyjs::useShinyjs(),
                    shinyjs::inlineCSS(mandatoryCSS),
                    
                    titlePanelUI(id="t3", "Configuration"),
                    h4("Please enter your parameters for MetIDfyR"),
                    h4("Responses will be saved as a R file in the input directory."),
                    fluidRow(id = "form",
                             column(6,
                                    textInput("config_name", "Name of the config file to save", placeholder = "config"),
                                    
                                    selectInput("bool_phase_1", "Include phase I transformations", choices = c("TRUE", "FALSE")),
                                    selectInput("bool_phase_2", "Include phase II transformations", choices = c("FALSE", "TRUE")),
                                    
                                    tags$b("Path to personal R library if required"), br(),
                                    shinyDirButton("lib_perso", "Select library", "Path to personal R library if required"),
                                    textOutput("select_lib_perso"),
                                    
                                    selectInput("nb_transformation", "Number of transformations to perform", choices = c(1:6)),
                                    selectInput("cores", "Number of cores to use", choices = c(2:detectCores()-1))
                                    
                             ),
                             column(6,
                                    textInput("min_peak_intensity", "Minimum intensity to consider a peak", value="5e5"),
                                    
                                    textInput("mz_ppm", "M/Z tolerance in ppm", value = "10"),
                                    
                                    textInput("rt_windows", "Retention time windows to consider a chromatogram peak", value="5"),
                                    
                                    selectInput("nb_scan", "Minimum number of scan", choices = c(1:10)),
                                    
                                    textInput("wdw_mz_ms2", "Quadrupole selection window width (m/z)", value="5"),
                                    
                                    textInput("minimum_mz", "Minimum MZ", value="200")
                             )
                        
                    ),
                    
                    actionButton("submit", "Save config", class = "btn-info"),
                    actionButton("resetbut", "Reset")
           ),
           
# Visualization of data
           tabPanel("Visualization",
                    # Application title
                    titlePanelUI(id="t4", "Data visualization"),
                    visualizationUI(id = "visual")
           )
           
           
           # tabPanel("Contact",
           #          h3("About MetIDfyR"),
           #          h3("GitHub link")
           # )
           
           
)
