#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinysky)
library(shinythemes)
library(RCurl)
library(TropFishR)

buildUrl <- function(session, path) {
  port <- session$clientData$url_port
  host <- session$clientData$url_hostname
  protocol <- session$clientData$url_protocol
  
  url <- paste0(protocol, "//", host, ":", port, "/", path)
  return (url);
}

source("assets/tropFishR/run_elefan_ga.R")
source("assets/tropFishR/run_elefan_sa.R")
source("assets/tropFishR/run_elefan.R")

source("assets/cmsy/CmsyFunction.R")

pdf(NULL)
set.seed(1)
d <- data(package = "TropFishR")
parallel <- FALSE

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  busyIndicator(wait = 1000),
  tags$head(tags$style(
    type="text/css",
    ".tab-content { height:6000px"
  )),
  tags$head(tags$style(
    type="text/css",
    ".shiny-download-link{color: white !important;} a.shiny-bound-output{background-color: #157ab5 !important;border-color: #afafaf !important;} .sidePanelCustomHeader {text-decoration: underline;font-size: 1.2em;} .sidePanelCustomHeaderTop {color: #0677b7; text-decoration: none;font-size: 1.25em;} hr {border-top: 1px solid #000 !important;} .btn{background-color: #006fca !important;color: white !important;} .shinysky-busy-indicator{z-index : 100} .pheader_elefan {color: #0677b7; font-size: 1.25em;} .sidePanelCustomHeaderTop2 {font-size: 0.9em;} .elefan_info{font-size: 1.2em;} #renderCmsyDlmToolsAnalysisChart {max-width: 100%; width: 100%; height: auto !important;} #renderCmsyDlmToolsManagementChart {max-width: 100%; width: 100%; height: auto !important;}"
  )),
  navbarPage("Stock Assessment",
             tabPanel("CMSY", 
                      sidebarLayout(
                        sidebarPanel(
                          useShinyjs(),
                          tags$label(class="sidePanelCustomHeaderTop", checked=NA,
                                     tags$p("CMSY - Catch Maximum, Sustainable Yield")
                          ),
                          tags$label(class="sidePanelCustomHeaderTop2", checked=NA,
                                     tags$p("The CMSY method for data-limited stock assessment. Described in Froese, R., Demirel, N., Coro, G., Kleisner, K. M., Winker, H. (2016). Estimating fisheries reference points from catch and resilience. Fish and Fisheries.")
                          ),
                          tags$label(class="sidePanelCustomHeaderTop2", checked=NA,
                                     tags$a(href="http://onlinelibrary.wiley.com/doi/10.1111/faf.12190/full", target="_blank", "Click here to read the paper.")
                          ),
                          hr(),
                          textInput("vreUsername", "VRE Username", "enrico.anello"),
                          textInput("vreToken", "VRE Token", "5b0f903a-3cb1-4424-a2bd-2700c9f1d4ed"),
                          
                          checkboxInput("cmsyDlmtools", "Run CMSY for DLMTools", TRUE),
                          checkboxInput("cmsyLegacy", "Run legacy CMSY", FALSE),
                          fileInput("file1", "Choose Stock CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          uiOutput("fill"),
                          textInput("region", "Region", "Mediterranean"),
                          textInput("subregion", "SubRegion", "Adriatic sea"),
                          
                          textInput("group", "Group", "Plankton feeders"),
                          textInput("name", "Name", "Sand smelt in Adriatic Sea"),
                          textInput("englishName", "English Name", "Big scale sand smelt"),
                          textInput("scientificName", "Scientific Name", "Atherina boyeri"),
                          textInput("source", "Source", "-"),
                          
                          numericInput("minOfYear", "Min of year", 1970, min = 1900, max = 2020, step=1),
                          numericInput("maxOfYear", "Max of year", 2014, min = 1900, max = 2020, step=1),
                          numericInput("startYear", "Start year", 1970, min = 1900, max = 2020, step=1),
                          numericInput("endYear", "End year", 2014, min = 1900, max = 2020, step=1),
                          
                          textInput("flim", "F limit", "NA"),
                          textInput("fpa", "FPA", "NA"),
                          textInput("blim", "B Limit", "NA"),
                          textInput("bpa", "BPA", "NA"),
                          textInput("bmsy", "B-MSY", "NA"),
                          textInput("fmsy", "F-MSY", "NA"),
                          textInput("msy", "MSY", "NA"),
                          textInput("msyBTrigger", "MSY Trigger", "NA"),
                          textInput("b40", "B 40", "NA"),
                          textInput("m", "M", "NA"),
                          textInput("fofl", "FOFL", "NA"),
                          textInput("last_f", "Last F", "NA"),
                          textInput("resiliance", "Resiliance", "Medium"),
                          textInput("r.low", "R Low", "NA"),
                          textInput("r.hi", "R Hi", "NA"),
                          
                          numericInput("stb.low", "Stb Low", 0.2, min = 0, max = 10, step=0.1),
                          numericInput("stb.hi", "Stb Hi", 0.6, min = 0, max = 10, step=0.1),
                          
                          textInput("int.yr", "Int Yr", "NA"),
                          textInput("intb.low", "Intb low", "NA"),
                          textInput("intb.hi", "Intb hi", "NA"),
                          
                          numericInput("endb.low", "Endb Low", 0.01, min = 0, max = 10, step=0.01),
                          numericInput("endb.hi", "Endb Hi", 0.4, min = 0, max = 10, step=0.1),
                          
                          textInput("q.start", "Q Start", "NA"),
                          textInput("q.end", "Q End", "NA"),
                          textInput("btype", "BType", "None"),
                          
                          checkboxInput("force.cmsy", "Force cmsy", FALSE),
                          
                          textInput("comments", "Comments", "landings"),
                          
                          actionButton("go_cmsy", "Go")
                        ),
                        mainPanel(
                          #htmlOutput("renderInfo"),
                          uiOutput("downloadCmsyReportButton"),
                          br(),
                          htmlOutput("titleDlmTools"),
                          br(),
                          htmlOutput("renderDlmtoolsLog"),
                          br(),
                          htmlOutput("renderCmsyDlmToolsInfo"),
                          htmlOutput("titleDlmToolsManagementChart"),
                          imageOutput("renderCmsyDlmToolsManagementChart"),
                          htmlOutput("renderDlmToolsSpace1"),
                          htmlOutput("titleDlmToolsAnalisysChart"),
                          imageOutput("renderCmsyDlmToolsAnalysisChart"),
                          htmlOutput("renderDlmToolsSpace2"),
                          htmlOutput("titleLegacy"),
                          br(),
                          htmlOutput("renderLegacyLog"),
                          br(),
                          htmlOutput("renderCmsyLegacyInfo"),
                          htmlOutput("titleLegacyManagementChart"),
                          imageOutput("renderCmsyLegacyManagementChart"),
                          htmlOutput("renderLegacySpace"),
                          htmlOutput("titleLegacyAnalisysChart"),
                          imageOutput("renderCmsyLegacyAnalysisChart")
                          
                        )
                      )
             ),
             tabPanel("Elefan by TropFishR", navbarPage("ELEFAN Algoritmhs by TropFishR",
                                                        tabPanel("ELEFAN_GA",
                                                                 sidebarLayout(
                                                                   sidebarPanel(
                                                                     shinyjs::useShinyjs(),
                                                                     tags$label(class="sidePanelCustomHeaderTop", checked=NA,
                                                                                tags$p("Electronic LEngth Frequency ANalysis with genetic algorithm used for estimating growth parameters.")
                                                                     ),
                                                                     tags$label(class="sidePanelCustomHeaderTop2", checked=NA,
                                                                                tags$p("The dataset used is the synLFQ7 - Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by ELEFAN, ELEFAN_SA, or ELEFAN_GA.")
                                                                     ),
                                                                     hr(),
                                                                     #selectInput("ELEFAN_GA_dataset", "Select Dataset", sort(unique(d$results[, "Item"])), selected = "synLFQ7"),
                                                                     #numericInput("ELEFAN_GA_binSize", "Bin size", 4, min = 1, max = 20, step=1),
                                                                     checkboxInput("ELEFAN_GA_seasonalised", "Seasonalised", FALSE),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Low Par Parameters")
                                                                     ),
                                                                     numericInput("ELEFAN_GA_lowPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_GA_lowPar_K", "Curving coefficient", 0.01, min = 0, max = 1, step=0.01),
                                                                     numericInput("ELEFAN_GA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_lowPar_C", "Amplitude of growth oscillation", 0, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_lowPar_ts", "Summer point", 0, min = 0, max = 1, step=0.1),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Up Par Parameters")
                                                                     ),
                                                                     numericInput("ELEFAN_GA_upPar_Linf", "Length of infinity in CM", 129, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_GA_upPar_K", "Curving coefficient", 1, min = 0, max = 1, step=0.01),
                                                                     numericInput("ELEFAN_GA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 1, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_upPar_C", "Amplitude of growth oscillation", 1, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_upPar_ts", "Summer point", 1, min = 0, max = 1, step=0.1),
                                                                     hr(),
                                                                     numericInput("ELEFAN_GA_popSize", "Population size", 50, min = 0, max = 10000, step=1),
                                                                     numericInput("ELEFAN_GA_maxiter", "Maximum number of iterations to run before the GA search is halted", 10, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_GA_run", "Number of consecutive generations without any improvement in the best fitness value before the GA is stopped", 100, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_GA_pmutation", "Probability of mutation in a parent chromosome. Usually mutation occurs with a small probability", 0.1, min = 0.1, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_pcrossover", "Probability of crossover between pairs of chromosomes. Typically this is a large value", 0.8, min = 0.1, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_GA_elitism", "Number of best fitness individuals to survive at each generation", 5, min = 0, max = 100, step=1),
                                                                     numericInput("ELEFAN_GA_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1),
                                                                     checkboxInput("ELEFAN_GA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
                                                                     
                                                                     actionButton("go_ga", "Run ELEFAN_GA")
                                                                   ),
                                                                   
                                                                   # Show a plot of the generated distribution
                                                                   mainPanel(
                                                                     uiOutput("downloadReport_ga"),
                                                                     br(),
                                                                     htmlOutput("titlePlot1_elefan_ga"),
                                                                     plotOutput("plot_ga_1"),
                                                                     htmlOutput("titlePlot2_elefan_ga"),
                                                                     plotOutput("plot_ga_2"),
                                                                     htmlOutput("titleResultsOfTheComputation_elefan_ga"),
                                                                     plotOutput("plot_ga_5"),
                                                                     htmlOutput("titlePlot3_elefan_ga"),
                                                                     plotOutput("plot_ga_3"),
                                                                     htmlOutput("titlePlot4_elefan_ga"),
                                                                     plotOutput("plot_ga_4"),
                                                                     htmlOutput("rnMax_ga"),
                                                                     htmlOutput("par_ga"),
                                                                     br(),br()
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("ELEFAN_SA",
                                                                 sidebarLayout(
                                                                   sidebarPanel(
                                                                     shinyjs::useShinyjs(),
                                                                     tags$label(class="sidePanelCustomHeaderTop", checked=NA,
                                                                                tags$p("Electronic LEngth Frequency ANalysis with simulated annealing for estimating growth parameters.")
                                                                     ),
                                                                     tags$label(class="sidePanelCustomHeaderTop2", checked=NA,
                                                                                tags$p("The dataset used is the synLFQ7 - Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by ELEFAN, ELEFAN_SA, or ELEFAN_GA.")
                                                                     ),
                                                                     hr(),
                                                                     #selectInput("ELEFAN_SA_dataset", "Select Dataset", sort(unique(d$results[, "Item"])), selected = "synLFQ7"),
                                                                     #numericInput("ELEFAN_SA_binSize", "Bin size", 4, min = 1, max = 20, step=1),
                                                                     checkboxInput("ELEFAN_SA_seasonalised", "Seasonalised", FALSE),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Init Par Parameters")
                                                                     ),
                                                                     numericInput("ELEFAN_SA_initPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_SA_initPar_K", "Curving coefficient", 0.5, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_SA_initPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0.5, min = 0, max = 1, step=0.01),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Low Par Parameters")
                                                                     ),
                                                                     numericInput("ELEFAN_SA_lowPar_Linf", "Length of infinity in CM", 119, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_SA_lowPar_K", "Curving coefficient", 0.01, min = 0, max = 1, step=0.01),
                                                                     numericInput("ELEFAN_SA_lowPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 0, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_SA_lowPar_C", "Amplitude of growth oscillation", 0, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_SA_lowPar_ts", "Summer point", 0, min = 0, max = 1, step=0.1),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Up Par Parameters")
                                                                     ),
                                                                     numericInput("ELEFAN_SA_upPar_Linf", "Length of infinity in CM", 129, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_SA_upPar_K", "Curving coefficient", 1, min = 0, max = 1, step=0.01),
                                                                     numericInput("ELEFAN_SA_upPar_t_anchor", "Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month", 1, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_SA_upPar_C", "Amplitude of growth oscillation", 1, min = 0, max = 1, step=0.1),
                                                                     numericInput("ELEFAN_SA_upPar_ts", "Summer point", 1, min = 0, max = 1, step=0.1),
                                                                     hr(),
                                                                     numericInput("ELEFAN_SA_SA_time", "Maximum running time in seconds", 60, min = 0, max = 10000, step=1),
                                                                     numericInput("ELEFAN_SA_SA_temp", "Initial value for temperature", 100000, min = 1, max = 10000000, step=100),
                                                                     numericInput("ELEFAN_SA_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1),
                                                                     numericInput("ELEFAN_SA_agemax", "Maximum age of species", 1, min = 0, max = 100, step=1),
                                                                     checkboxInput("ELEFAN_SA_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
                                                                     
                                                                     actionButton("go_sa", "Run ELEFAN_SA")
                                                                   ),
                                                                   
                                                                   # Show a plot of the generated distribution
                                                                   mainPanel(
                                                                     uiOutput("downloadReport_sa"),
                                                                     br(),
                                                                     htmlOutput("titlePlot1_elefan_sa"),
                                                                     plotOutput("plot_sa_1"),
                                                                     htmlOutput("titlePlot2_elefan_sa"),
                                                                     plotOutput("plot_sa_2"),
                                                                     htmlOutput("titleResultsOfTheComputation_elefan_sa"),
                                                                     plotOutput("plot_sa_5"),
                                                                     htmlOutput("titlePlot3_elefan_sa"),
                                                                     plotOutput("plot_sa_3"),
                                                                     htmlOutput("titlePlot4_elefan_sa"),
                                                                     plotOutput("plot_sa_4"),
                                                                     htmlOutput("rnMax_sa"),
                                                                     htmlOutput("par_sa"),
                                                                     br(),br()
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("ELEFAN",
                                                                 sidebarLayout(
                                                                   sidebarPanel(
                                                                     shinyjs::useShinyjs(),
                                                                     tags$label(class="sidePanelCustomHeaderTop", checked=NA,
                                                                                tags$p("Electronic LEngth Frequency ANalysis for estimating growth parameter.")
                                                                     ),
                                                                     tags$label(class="sidePanelCustomHeaderTop2", checked=NA,
                                                                                tags$p("The dataset used is the synLFQ7 - Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by ELEFAN, ELEFAN_SA, or ELEFAN_GA.")
                                                                     ),
                                                                     hr(),
                                                                     #selectInput("ELEFAN_dataset", "Select Dataset", sort(unique(d$results[, "Item"])), selected = "synLFQ7"),
                                                                     #numericInput("ELEFAN_binSize", "Bin size", 4, min = 1, max = 20, step=1),
                                                                     numericInput("ELEFAN_Linf_fix", "Linf: if used the K-Scan method is applied with a fixed Linf value (i.e. varying K only)", NA, min = 1, max = 1000, step=1),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("Linf range - Numeric vector with potential Linf values. Default is the last length class plus/minus 5 cm")
                                                                     ),
                                                                     numericInput("ELEFAN_Linf_range_from", "Linf sequence from", NULL, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_Linf_range_to", "Linf sequence to", NULL, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_Linf_range_by", "Linf increment sequence by", 1, min = 1, max = 1000, step=1),
                                                                     hr(),
                                                                     tags$label(class="sidePanelCustomHeader", checked=NA,
                                                                                tags$p("K values for which the score of growth functions should be calculated (by default: exp(seq(log(0.1),log(10),length.out = 100)))")
                                                                     ),
                                                                     numericInput("ELEFAN_K_Range_from", "Linf sequence from", NULL, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_K_Range_to", "Linf sequence to", NULL, min = 1, max = 1000, step=1),
                                                                     numericInput("ELEFAN_K_Range_by", "Linf increment sequence by", 1, min = 1, max = 1000, step=1),
                                                                     hr(),
                                                                     numericInput("ELEFAN_C", "Growth oscillation amplitude", 0, min = 0, max = 100, step=1),
                                                                     numericInput("ELEFAN_ts", "Onset of the first oscillation relative to summer point", 0, min = 0, max = 100, step=1),
                                                                     numericInput("ELEFAN_MA", "Number indicating over how many length classes the moving average should be performed", 5, min = 0, max = 100, step=1),
                                                                     checkboxInput("ELEFAN_addl.sqrt", "Additional squareroot transformation of positive values according to Brey et al. (1988)", FALSE),
                                                                     numericInput("ELEFAN_agemax", "Maximum age of species", NULL, min = 0, max = 100, step=1),
                                                                     checkboxInput("ELEFAN_contour", "if checked in combination with response surface analysis, contour lines are displayed rather than the score as text in each field of the score plot", FALSE),
                                                                     
                                                                     actionButton("go", "Run ELEFAN")
                                                                   ),
                                                                   
                                                                   # Show a plot of the generated distribution
                                                                   mainPanel(
                                                                     uiOutput("downloadReport"),
                                                                     br(),
                                                                     htmlOutput("titlePlot1_elefan"),
                                                                     plotOutput("plot_1"),
                                                                     htmlOutput("titlePlot2_elefan"),
                                                                     plotOutput("plot_2"),
                                                                     htmlOutput("titleResultsOfTheComputation_elefan"),
                                                                     plotOutput("plot_5"),
                                                                     htmlOutput("titlePlot3_elefan"),
                                                                     plotOutput("plot_3"),
                                                                     htmlOutput("titlePlot4_elefan"),
                                                                     plotOutput("plot_4"),
                                                                     htmlOutput("rnMax"),
                                                                     htmlOutput("par"),
                                                                     br(),br()
                                                                   )
                                                                 )
                                                        ),
                                                        tabPanel("Information on the dataset", htmlOutput("datasetInfo"))
             ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  cmsy <- reactiveValues()
  output$fill <- renderUI({
    inFile1 <- input$file1
    
    if (is.null(inFile1)) {
      return(NULL)
    }
    a <- read.csv(inFile1$datapath)
    
    selectInput("stock", "Select a stock", sort(unique(a$Stock)))
  })
  
  output$downloadCmsyReportButton <- renderUI({
    if (!is.null(cmsy$dlmTools) || !is.null(cmsy$legacy)) {
      downloadButton("downloadCmsyReport", label = "Download Report")
    }
  })
  
  output$downloadCmsyReport <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("cmsy_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "cmsyReport.Rmd")
      file.copy("assets/cmsy/cmsyReport.Rmd", tempReport, overwrite = TRUE)
      
      
      if (!is.null(cmsy$dlmTools$analisysChartUrl)) {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(cmsy$dlmTools$analisysChartUrl, fileAnalisysChart)
        cmsy$dlmTools$analisysChart <- fileAnalisysChart
      }
      if (!is.null(cmsy$dlmTools$analisysChartUrl)) {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(cmsy$dlmTools$managementChartUrl, fileManagementChart)
        cmsy$dlmTools$managementChart <- fileManagementChart
      }
      if (!is.null(cmsy$legacy$analisysChartUrl)) {
        fileAnalisysChart <- tempfile(fileext=".jpg")
        download.file(cmsy$legacy$analisysChartUrl, fileAnalisysChart)
        cmsy$legacy$analisysChart <- fileAnalisysChart
      }
      if (!is.null(cmsy$legacy$analisysChartUrl)) {
        fileManagementChart <- tempfile(fileext=".jpg")
        download.file(cmsy$legacy$managementChartUrl, fileManagementChart)
        cmsy$legacy$managementChart <- fileManagementChart
      }
      
      # Set up parameters to pass to Rmd document
      params <- list(cmsy = cmsy)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  output$renderDlmtoolsSpace1 <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderDlmtoolsSpace2 <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderLegacySpace <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        space <- "<br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/><br/>"
        space
      } else { "" }
    } else { "" }
  })
  
  output$renderDlmtoolsLog <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        log <- paste0("<a href='", cmsy$dlmTools$log, "'>Download the log of the computation</a>")
        log
      } else { "" }
    } else { "" }
  })
  output$renderLegacyLog <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        log <- paste0("<a href='", cmsy$legacy$log, "'>Download the log of the computation</a>")
        log
      } else { "" }
    } else { "" }
  })
  
  output$titleDlmTools <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h1> CMSY For DLM Tools - Results</h1>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleDlmToolsManagementChart <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h2> Management Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleDlmToolsAnalisysChart <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        title <- "<h2> Analysis Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$renderCmsyDlmToolsInfo <- renderText({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        cmsy$dlmTools$text <- gsub("\n\r", "<br/>", cmsy$dlmTools$text)
        cmsy$dlmTools$text <- gsub("\n", "<br/>", cmsy$dlmTools$text)
        cmsy$dlmTools$text <- gsub("----------------------------------------------------------", "", cmsy$dlmTools$text)
        cmsy$dlmTools$text
      } else {  "" }
    } else {  "" }
  })
  output$renderCmsyDlmToolsManagementChart <- renderImage({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        w1 <- session$clientData$output_renderCmsyDlmToolsManagementChart_width
        h1 <- (w1*3)/4
        list(src = cmsy$dlmTools$managementChart,
             contentType = 'image/jpg',
             width = w1,
             height = h1)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  output$renderCmsyDlmToolsAnalysisChart <- renderImage({
    if ("dlmTools" %in% names(cmsy)) {
      if (!is.null(cmsy$dlmTools)) {
        w2 <- session$clientData$output_renderCmsyDlmToolsAnalysisChart_width
        h2 <- (w2*3)/4
        list(src = cmsy$dlmTools$analisysChart,
             contentType = 'image/jpg',
             width = w2,
             height = h2)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  
  #--------------------------------------------------------------------------
  
  output$titleLegacy <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h1> Legacy CMSY - Results</h1>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleLegacyManagementChart <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h2> Management Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$titleLegacyAnalisysChart <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        title <- "<h2> Analisys Charts </h2>"
        title
      } else {  "" }
    } else {  "" }
  })
  
  output$renderCmsyLegacyInfo <- renderText({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        cmsy$legacy$text <- gsub("\n\r", "<br/>", cmsy$legacy$text)
        cmsy$legacy$text <- gsub("\n", "<br/>", cmsy$legacy$text)
        cmsy$legacy$text <- gsub("----------------------------------------------------------", "", cmsy$legacy$text)
        cmsy$legacy$text
      } else {  "" }
    } else {  "" }
  })
  output$renderCmsyLegacyManagementChart <- renderImage({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        w3 <- session$clientData$output_renderCmsyLegacyManagementChart_width
        h3 <- (w3*3)/4
        list(src = cmsy$legacy$managementChart,
             contentType = 'image/jpg',
             width = w3,
             height = h3)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  output$renderCmsyLegacyAnalysisChart <- renderImage({
    if ("legacy" %in% names(cmsy)) {
      if (!is.null(cmsy$legacy)) {
        w4 <- session$clientData$output_renderCmsyLegacyManagementChart_width
        h4 <- (w4*3)/4
        list(src = cmsy$legacy$analisysChart,
             contentType = 'image/jpg',
             width = w4,
             height = h4)
      } else {
        list(src = "NULL")
      }
    } else {
      list(src = "NULL")
    }
  })
  
  #--------------------------------------------------------------------------
  
  observeEvent(input$go_cmsy, {
    infile1 <- input$file1
    
    if (is.null(infile1)) {
      return(NULL)
    }
    
    shinyjs::runjs("window.scrollTo(0, 0);")
    
    inputCsvFile <- infile1$datapath
    
    templateFileDlmTools <- "assets/cmsy/cmsyForDlmToolsTemplate.xml"
    templateFileLegacy <- "assets/cmsy/cmsyLegacyTemplate.xml"
    
    cmsy$dlmTools <- list()
    cmsy$legacy <- list()
    if (input$cmsyDlmtools) {
      cat(file=stderr(),inputCsvFile, "\n")
      cat(file=stderr(), templateFileDlmTools, "\n")      
      ret <- runCmsy(input$region,input$subregion,input$stock,input$group,input$name,input$englishName,input$scientificName,input$source,input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, input$vreUsername, input$vreToken, inputCsvFile, templateFileDlmTools)
      for(i in 1:nrow(ret)) {
        row <- ret[i,]
        if (row$description == "estimates") {
          contents <- getURL(row$url)
          cmsy$dlmTools$textRaw <- contents
          contents <- gsub("\n\r", "<br/>", contents)
          contents <- gsub("\n", "<br/>", contents)
          contents <- gsub("----------------------------------------------------------", "", contents)
          cmsy$dlmTools$text <- contents
        }
        if (row$description == "analysis_charts") {
          fileAnalisysChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileAnalisysChart)
          cmsy$dlmTools$analisysChart <- fileAnalisysChart
          cmsy$dlmTools$analisysChartUrl <- row$url
        }
        if (row$description == "management_charts") {
          fileManagementChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileManagementChart)
          cmsy$dlmTools$managementChart <- fileManagementChart
          cmsy$dlmTools$managementChartUrl <- row$url
        }
        if (row$description == "Log of the computation") {
          cmsy$dlmTools$log <- row$url
        }
      }
    } else {
      cmsy$dlmTools <- NULL
    }
    if (input$cmsyLegacy) {
      ret <- runCmsy(input$region,input$subregion,input$stock,input$group,input$name,input$englishName,input$scientificName,input$source,input$minOfYear,input$maxOfYear,input$startYear,input$endYear,input$flim,input$fpa,input$blim,input$bpa,input$bmsy,input$fmsy,input$msy,input$msyBTrigger,input$b40,input$m,input$fofl,input$last_f,input$resiliance,input$r.low,input$r.hi,input$stb.low,input$stb.hi,input$int.yr,input$intb.low,input$intb.hi,input$endb.low,input$endb.hi,input$q.start,input$q.end,input$btype,input$force.cmsy,input$comments, input$vreUsername, input$vreToken, inputCsvFile, templateFileLegacy)
      for(i in 1:nrow(ret)) {
        row <- ret[i,]
        if (row$description == "estimates") {
          contents <- getURL(row$url)
          cmsy$legacy$textRaw <- contents
          contents <- gsub("\n\r", "<br/>", contents)
          contents <- gsub("\n", "<br/>", contents)
          contents <- gsub("----------------------------------------------------------", "", contents)
          cmsy$legacy$text <- contents
        }
        if (row$description == "analysis_charts") {
          fileAnalisysChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileAnalisysChart)
          cmsy$legacy$analisysChart <- fileAnalisysChart
          cmsy$legacy$analisysChartUrl <- row$url
        }
        if (row$description == "management_charts") {
          fileManagementChart <- tempfile(fileext=".jpg")
          download.file(row$url, fileManagementChart)
          cmsy$legacy$managementChart <- fileManagementChart
          cmsy$legacy$managementChartUrl <- row$url
        }
        if (row$description == "Log of the computation") {
          cmsy$legacy$log <- row$url
        }
      }
    } else {
      cmsy$legacy <- NULL
    }
    
    
    #ret = runCMSY(inFile1$datapath, inFile2$datapath, input$stocks, input$uncert, input$sigmaR, input$kv_pairs, input$ni, input$ni, input$ni, input$nab, T, F, NA, F)
    
  })
 
  elefan_ga <- reactiveValues()
  elefan_sa <- reactiveValues()
  elefan <- reactiveValues()
  
  
  observeEvent(input$go_ga, {
    shinyjs::runjs("window.scrollTo(0, 0);")
    #ds <- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    res <- run_elefan_ga(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                         low_par = list(Linf = input$ELEFAN_GA_lowPar_Linf, K = input$ELEFAN_GA_lowPar_K, t_anchor = input$ELEFAN_GA_lowPar_t_anchor, C = input$ELEFAN_GA_lowPar_C, ts = input$ELEFAN_GA_lowPar_ts),
                         up_par = list(Linf = input$ELEFAN_GA_upPar_Linf, K = input$ELEFAN_GA_upPar_K, t_anchor = input$ELEFAN_GA_upPar_t_anchor, C = input$ELEFAN_GA_upPar_C, ts = input$ELEFAN_GA_upPar_ts),
                         popSize = input$ELEFAN_GA_popSize, maxiter = input$ELEFAN_GA_maxiter, run = input$ELEFAN_GA_run, pmutation = input$ELEFAN_GA_pmutation, pcrossover = input$ELEFAN_GA_pcrossover,
                         elitism = input$ELEFAN_GA_elitism, MA = input$ELEFAN_GA_MA, addl.sqrt = input$ELEFAN_GA_addl.sqrt)
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      elefan_ga$results <- res
    }
  })
  
  output$plot_ga_1 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_ga_2 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_ga_3 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_ga_4 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_ga_5 <- renderPlot({
    if ('results' %in% names(elefan_ga)) {
      plot(elefan_ga$results$data)
    }
  })
  
  output$rnMax_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan_ga$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par_ga <- renderText({
    if ("results" %in% names(elefan_ga)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan_ga$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan_ga$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan_ga$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan_ga$data$results$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan_ga$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan_ga$results$data$par$phiL)
      title
    } else {  "" }
  })
  
  output$downloadReport_ga <- renderUI({
    if ("results" %in% names(elefan_ga)) {
      downloadButton('createElefanGAReport', 'Download Report')
    }
  })
  output$createElefanGAReport <- downloadHandler(
    filename = paste("ElefanGA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_ga.Rmd")
      file.copy("assets/tropFishR/elefan_ga.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_ga)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  
  
  observeEvent(input$go_sa, {
    shinyjs::runjs("window.scrollTo(0, 0);")
    #ds <<- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    res <- run_elefan_sa(ds,binSize =  4, seasonalised = input$ELEFAN_GA_seasonalised, 
                         init_par = list(Linf = input$ELEFAN_SA_initPar_Linf, K = input$ELEFAN_SA_initPar_K, t_anchor = input$ELEFAN_SA_initPar_t_anchor),
                         low_par = list(Linf = as.numeric(input$ELEFAN_SA_lowPar_Linf), K = as.numeric(input$ELEFAN_SA_lowPar_K), t_anchor = as.numeric(input$ELEFAN_SA_lowPar_t_anchor), C = as.numeric(input$ELEFAN_SA_lowPar_C), ts = as.numeric(input$ELEFAN_SA_lowPar_ts)),
                         up_par = list(Linf = as.numeric(input$ELEFAN_SA_upPar_Linf), K = as.numeric(input$ELEFAN_SA_upPar_K), t_anchor = as.numeric(input$ELEFAN_SA_upPar_t_anchor), C = as.numeric(input$ELEFAN_SA_upPar_C), ts = as.numeric(input$ELEFAN_SA_upPar_ts)),
                         SA_time = input$ELEFAN_SA_SA_time, SA_temp = input$ELEFAN_SA_SA_temp, MA = input$ELEFAN_SA_MA, addl.sqrt = input$ELEFAN_SA_addl.sqrt,
                         agemax = input$ELEFAN_SA_agemax)
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      elefan_sa$results <- res
    }
  })
  
  output$plot_sa_1 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_sa_2 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_sa_3 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_sa_4 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_sa_5 <- renderPlot({
    if ('results' %in% names(elefan_sa)) {
      plot(elefan_sa$results$data)
    }
  })
  
  output$rnMax_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan_sa$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par_sa <- renderText({
    if ("results" %in% names(elefan_sa)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan_sa$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan_sa$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan_sa$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan_sa$results$data$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan_sa$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan_sa$results$data$par$phiL)
      title
    } else {  "" }
  })
  output$downloadReport_sa <- renderUI({
    if ("results" %in% names(elefan_sa)) {
      downloadButton('createElefanSAReport', 'Download Report')
    }
  })
  output$createElefanSAReport <- downloadHandler(
    filename = paste("ElefanSA_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan_sa.Rmd")
      file.copy("assets/tropFishR/elefan_sa.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan_sa)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  
  observeEvent(input$go, {
    shinyjs::runjs("window.scrollTo(0, 0);")
    #ds <<- lfqModify(get(input$ELEFAN_GA_dataset, asNamespace('TropFishR')), bin_size = input$ELEFAN_GA_binSize)
    ds <- lfqModify(get('synLFQ7', asNamespace('TropFishR')), bin_size = 4)
    
    elefan_linf_range <- NA
    if (!is.na(input$ELEFAN_Linf_range_from) && !is.na(input$ELEFAN_Linf_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_Linf_range_from, to = input$ELEFAN_Linf_range_to, by = input$ELEFAN_Linf_range_by)
    }
    
    elefan_k_range <- exp(seq(log(0.1), log(10), length.out=100))
    if (!is.na(input$ELEFAN_K_Range_from) && !is.na(input$ELEFAN_K_range_to)) {
      elefan_linf_range <- seq(from = input$ELEFAN_K_Range_from, to = input$ELEFAN_K_range_to, by = input$ELEFAN_K_range_by)
    }
    
    
    elefan_agemax <- input$ELEFAN_agemax 
    if (is.na(input$ELEFAN_agemax)) {
      elefan_agemax <- NULL
    }
    res <- run_elefan(ds, binSize = 4, Linf_fix = input$ELEFAN_Linf_fix, Linf_range = elefan_linf_range, K_range = elefan_k_range,
                      C = input$ELEFAN_C, ts = input$ELEFAN_ts, MA = input$ELEFAN_MA, addl.sqrt = input$ELEFAN_addl.sqrt,
                      agemax = elefan_agemax, contour = input$ELEFAN_contour)
    if ('error' %in% names(res)) {
      showModal(modalDialog(
        title = "Error",
        res$error,
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      elefan$results <- res
    }
  })
  
  output$plot_1 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot1, Fname = "catch", date.axis = "modern")
    }
  })
  output$plot_2 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot2, Fname = "rcounts", date.axis = "modern")
    }
  })
  output$plot_3 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot3, mark = TRUE)
      mtext("(a)", side = 3, at = -1, line = 0.6)
    }
  })
  output$plot_4 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$plot4, type = "Isopleth", xaxis1 = "FM", mark = TRUE, contour = 6)
      mtext("(b)", side = 3, at = -0.1, line = 0.6)
    }
  })
  output$plot_5 <- renderPlot({
    if ('results' %in% names(elefan)) {
      plot(elefan$results$data)
    }
  })
  
  output$rnMax <- renderText({
    if ("results" %in% names(elefan)) {
      title <- paste0("<strong>Highest value of fitness function:</strong>&nbsp;", elefan$results$data$Rn_max)
      title
    } else {  "" }
  })
  output$par <- renderText({
    if ("results" %in% names(elefan)) {
      title <- "<hr>"
      title <- paste0(title, "<strong>Length infinity in cm:</strong>&nbsp;", elefan$results$data$par$Linf)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Curving coefficient:</strong>&nbsp;", elefan$results$data$par$K)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Time point anchoring growth curves in year-length coordinate system, corrsponds to peak spawning month:</strong>&nbsp;", elefan$results$data$par$t_anchor)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Amplitude of growth oscillation:</strong>&nbsp;", elefan$results$data$par$C)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Summer point of oscillation (ts = WP - 0.5):</strong>&nbsp;", elefan$results$data$par$ts)
      title <- paste0(title, "<br/>")
      title <- paste0(title, "<strong>Growth performance index defined as phiL = log10(K) + 2 * log10(Linf):</strong>&nbsp;", elefan$results$data$par$phiL)
      title
    } else {  "" }
  })
  
  output$downloadReport <- renderUI({
    if ("results" %in% names(elefan)) {
      downloadButton('createElefanReport', 'Download Report')
    }
  })
  output$createElefanReport <- downloadHandler(
    filename = paste("Elefan_report_",format(Sys.time(), "%Y%m%d%H%M%s"),".pdf",sep=""),
    content = function(file) {
      tempReport <- file.path(tempdir(), "elefan.Rmd")
      file.copy("assets/tropFishR/elefan.Rmd", tempReport, overwrite = TRUE)
      params <- list(elefan = elefan)
      rmarkdown::render(tempReport, output_file = file, params = params)
    }
  )
  
  
  output$titlePlot1_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  
  output$titleResultsOfTheComputation_elefan <- renderText({
    if ('results' %in% names(elefan)) {
      txt <- "<h2>Results of the ELEFAN computation</h2>"
      txt
    }
  })
  
  output$titlePlot1_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  
  output$titleResultsOfTheComputation_elefan_ga <- renderText({
    if ('results' %in% names(elefan_ga)) {
      txt <- "<h2>Results of the ELEFAN_GA computation</h2>"
      txt
    }
  })
  
  output$titlePlot1_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Raw LFQ data</p>"
      txt
    }
  })
  output$titlePlot2_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Reconstructed LFQ data</p>"
      txt
    }
  })
  output$titlePlot3_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F</p>"
      txt
    }
  })
  output$titlePlot4_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<p class=\"pheader_elefan\">Thompson and Bell model with changes in F and Lc</p>"
      txt
    }
  })
  
  output$titleResultsOfTheComputation_elefan_sa <- renderText({
    if ('results' %in% names(elefan_sa)) {
      txt <- "<h2>Results of the ELEFAN_SA computation</h2>"
      txt
    }
  })
  
  
  output$datasetInfo <- renderText({
    text <- "<span class=\"elefan_info\">The dataset used by this example is the <b>synLFQ7</b></span><br><br>"
    text <- paste0(text, "<span class=\"elefan_info\">Synthetic length-frequency data as generated by the function lfqGen from the fishdynr package (Taylor 2016). Can be used by <b>ELEFAN</b>, <b>ELEFAN_SA</b>, or <b>ELEFAN_GA</b>. <br>The data is generated with the following von Bertalanffy growth parameters:</span>")
    text <- paste0(text, "<ul style=\"margin-top: 10px;\">")
    text <- paste0(text, "<li>K = 0.2 +/- 0.1 (CV)</li>")
    text <- paste0(text, "<li>Linf = 123 +/- 0.05 (CV)</li>")
    text <- paste0(text, "<li>C = 0.3</li>")
    text <- paste0(text, "<li>ts = 0</li>")
    text <- paste0(text, "<li>t_anchor between 0.16 and 0.34 (Time when yearly recruitment pulse occurs; e.g. 0 = Jan 1, 0.25 = Apr 1, 0.5 = Jul 1, 0.75 = Oct 1; repro_wt = c(0, 0, 0.2, 1, 0.6, 0, 0, 0, 0, 0, 0, 0))</li>")
    text <- paste0(text, "</ul>")
    text
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

