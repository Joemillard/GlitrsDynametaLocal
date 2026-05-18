# Dynameta shiny app ui script

# Load packages
library(bslib) # for themes to customise appearance of shiny app (bs_theme bs_add_variables font_link)
library(DT) # for interactive tables
library(leaflet) # for mapping (leafletOutput)
library(shiny)
library(shinycssloaders) # for loading symbols (while models run) (withSpinner)
library(shinydisconnect) # for displaying nice error message if whole shiny app disconnects (disconnectMessage)
library(shinyjs) # for enabling and disabling download button (useShinyjs hidden)
library(stringr) # for wrangling text
library(tm)
# library(webshot) # for downloading map

meta_analysis_outputs <- readRDS("../shiny_data/meta_analysis_outputs.rds")
insect_orders <- as.character(unique(meta_analysis_outputs$order)) # for the image view

main_content <- function(){
  shiny::navbarPage(
    # uiOutput("background"), # this adds a picture background to the public engagement tab.
    #   IMPORTANT: If this is enabled, the footer on the public engagement tab must also be enabled.
    #   This is because the images are mostly CC-BY and therefore attribution MUST be given.
    
    # Add custom JavaScript to trigger a click event on a specific tab
    tags$script('
     $(document).ready(function(){
       $("#tabs a[data-value=login]").tab("show"); // Open Tab 1 on page load
     });
   '),
    
    # ===============================================================================================================================
    # ===============================================================================================================================
    
    # Applying UKCEH theme manually by taking code from https://raw.githubusercontent.com/NERC-CEH/UKCEH_shiny_theming/main/theme_elements.R
    # bs_theme for high level theming
    # bs_add_variables for low level theming (a 'theme' is the first argument for this function)
    theme = bslib::bs_add_variables(bslib::bs_theme(
      version = 5, # updated to 5 by RM for popovers
      bg = "#fff",
      fg = "#292C2F",
      primary = "#0483A4",
      secondary = "#EAEFEC",
      success = "#37a635",
      info = "#34b8c7",
      warning = "#F49633",
      base_font = bslib::font_link(family = "Montserrat",href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap"))
    ),
    
    
    # Add favicon - show UKCEH logo in tab
    tags$head(tags$link(rel="shortcut icon", href="https://brandroom.ceh.ac.uk/themes/custom/ceh/favicon.ico")),
    
    # Add a custom error message if the whole app disconnects / fails
    # Using header makes it apply to all tabs
    header = shinydisconnect::disconnectMessage(text = "An error has occured, please reload the page and try again.",
                                                refresh = "", # Don't include a refresh button
                                                width = "full", # Message should take up full width of screen
                                                size = 30, # Size 30 writing
                                                background = "#0483A4", # Blue background
                                                colour = "white", # White writing
                                                overlayColour = "grey", # Covers the app and draws attention to message
                                                overlayOpacity = 0.8), # Nearly full opaque
    
    # ===============================================================================================================================
    # ===============================================================================================================================
    
    
    tabsetPanel(
      id = "tabs",
      # ==============================================================
      # Intro tab
      # ==============================================================
      shiny::tabPanel("Introduction",
                      useShinyjs(), # set up shinyjs
                      
                      tags$head(
                        tags$style(
                          HTML("#tabs > li:nth-child(6) > a,
          #tabs > li:nth-child(7) > a,
          #tabs > li:nth-child(8) > a {
            color: #4CAF3A; /* Change this to the desired color */
          }
                             
                                       #tabs > li > a {
            color: #5F95D1; /* Add any other default styles here */
            /* Add any other default styles here */
          }")
                        )
                      ),
                      
                      # ----------------------------------------------------------------------------------------------------------------------
                      column(10, offset = 1,
                             # Pre-amble
                             p(h4(tags$b("Introduction"))),
                             
                             tags$hr(),
                             
                             p(tags$a(href="https://glitrs.ceh.ac.uk/", "GLiTRS", target = "_blank"), "Dynameta is designed for the ingestion and interactive meta-analyses of insect biodiversity effect sizes, oriented around the effect of anthropogenic threats
                  (based on the ", tags$a(href="https://www.iucnredlist.org/resources/threat-classification-scheme", "IUCN threats classification scheme", .noWS = "outside", target = "_blank"),
                               ") on biodiversity. If you encounter any issues or bugs while using GLiTRS Dynameta, please raise this with Joe Millard."),
                             
                             p("The platform is split into 5 main pages:"),
                             
                             tags$ol(
                               tags$li("Use this 'Introduction' to familiarise yourself with the site and its layout."),
                               tags$li("Use 'Summary' to explore our findings in an engaging way."),
                               tags$li("Use 'Explore data' to access the data behind the project, visualise its geographic distribution, and run custom meta-analytic models investigating the effect of anthropogenic threats on insect biodiversity."),
                               tags$li("Use 'Read more' for any documentation on the appropriate approach for carrying out a GLiTRS meta-analysis.")
                               
                               
                             ),
                             
                             tags$hr(),
                             
                             # ----------------------------------------------------------------------------------------------------------------------
                             
                             ### User choice of what data to analyse - sample or their own
                             
                             # ----------------------------------------------------------------------------------------------------------------------
                             
                             
                             # Citation
                             p("For any publications using GLiTRS Dynameta, please cite both our original software article and the database publication:"),
                             p("- Millard, J., Skinner, G., Bladon, A. J., Cooke, R., Outhwaite, C. L., Rodger, J. G., Barnes, L. A., Isip, J., Keum, J., Raw, C., Wenban-Smith, E., Dicks, L. V., Hui, C., Jones, J. I., Woodcock, B., Isaac, N. J., & Purvis, A. (2025). A Multithreat Meta‐Analytic Database for Understanding Insect Biodiversity Change. Diversity and Distributions. DOI:",
                               tags$a(href = "https://doi.org/10.1111/ddi.70025", "https://doi.org/10.1111/ddi.70025", target = "_blank")),
                             p("- Skinner, G., Cooke, R., Junghyuk, K., Purvis, A., Raw, C., Woodcock, B.A., Millard, J. (2023). Dynameta: a dynamic platform for ecological meta-analyses in R Shiny. SoftwareX. DOI:",
                               tags$a(href = "https://doi.org/10.1016/j.softx.2023.101439", "https://doi.org/10.1016/j.softx.2023.101439", target = "_blank"))
                             # ----------------------------------------------------------------------------------------------------------------------
                      )                
      ),
      
      # ==============================================================
      # Public engagement tab
      # ==============================================================
      shiny::tabPanel("Summary",
                      column(10, offset = 1,
                             # Title to show at top of tab
                             p(h4(tags$b("How do human activities affect insects?"))),
                             
                             
                             #tags$br(),
                             
                             # The container that gets the background
                             div(id = "tab-content-area",
                                 
                                 selectInput("chosen_threat",
                                             label = "Choose a threat:",
                                             choices = c("2 Agriculture and Aquaculture", "8 Invasive & other problematic species, genes & diseases", "9 Pollution"),
                                             selected = NULL),
                                 
                                 p(htmlOutput("threat_explanation")),
                                 
                                 h5(htmlOutput("overall_effect")),
                                 
                                 p(""),
                                 
                                 p("Click the insects below to find out how they are each affected by human activities."),
                             ),
                          
                          fluidRow(
                            tagList(
                              lapply(insect_orders, function(insect) {
                                display_id <- paste0(insect, "_display")
                                summary_id    <- paste0(insect, "_summary")
                                # Use the 'insect' string to determine which output to call
                                conditionalPanel(
                                  condition = paste0("output['", insect, "_display'] == 'TRUE'"),
                                  #condition = sprintf("output.%s == 'TRUE'", display_id),
                                  column(2,
                                         popover(
                                           tags$div(
                                             uiOutput(insect),
                                             tags$div(
                                               textOutput(paste0(insect, "_name")),
                                               style = "font-size: 0.8em; font-weight: bold; margin-top: 0px;"
                                             ),
                                             style = "width: 135px; height: 135px; display: flex; flex-direction: column; align-items: center; justify-content: center;"
                                           ),
                                           tags$div(
                                             htmlOutput(paste0(insect, "_summary"))
                                           ),
                                           # p(""), #line break
                                           # tags$div(
                                           #   htmlOutput(paste0(insect, "_threat_info"))
                                           # ),
                                           p(""),
                                           tags$div(
                                             htmlOutput(paste0(insect, "_upper"))
                                           ),
                                           tags$div(
                                             htmlOutput(paste0(insect, "_lower"))
                                           ),
                                           p(""),
                                           tags$div(
                                             htmlOutput(paste0(insect, "_n"))
                                           ),
                                           id = paste0("popover_", insect),
                                           placement = "right"
                                         )
                                  )
                                )
                              })
                            )),
                          
                          tags$br(),
                          p(tags$a(href="https://www.royensoc.co.uk/understanding-insects/classification-of-insects/", "Click here to find out more about insect Orders", target="_blank")),
                          tags$br(),
                          tags$br(),
                          p("The colour of the icons represents how they are affected by the threat: red indicates a decrease and blue indicates an increase in sites with the threat compared to sites without. Grey insect orders are not significantly affected by the threat."),
                          p("The information displayed above does not distinguish between insects that are harmful vs. those that are beneficial to humans. For example, the order Lepidoptera (butterflies and moths) includes species 
                            and moths whose caterpillars are major crop pests, as well as species which are important pollinators. Hence, viewing the effect of a threat on a 
                            whole insect Order may hide important differences within that Order in terms of the insects' effect on people and ecosystems. Please also note that this does not represent the entirety of scientific knowledge, but only what the GLiTRS project has found and synthesised."),
                          p("Scientific notes:"),
                          p("Please note that this only uses abundance data with logRR errors at the moment. Best and worst case scenarios correspond to upper and lower 95% confidence intervals, respectively, calculated automatically by the function metafor::rma.mv"),
                          
                      ),
                      #div(class = "custom-footer", textOutput("attribution"))
                      #                       )
                      
                      
      ), # close public engagement tab
      
      # ==============================================================
      # Meta-analysis dropdown
      # ==============================================================
      
      navbarMenu("Explore data",
                 shiny::tabPanel("Overview",
                             column(10,offset = 1,
                                 p(h4(tags$b("Overview"))),
                                 
                                 tags$hr(),
                                 
                                 h5(p("Disclaimer")),
                                 
                                 p("Due to the limited amount of data in the GLiTRS Dynameta database 
                                   at this time, and the risk of multiple testing issues, this tool 
                                   is best used for hypothesis generation rather than meta-analysis 
                                   for publication at this time. If the results are to be published 
                                   as they stand, it is very important to formulate, and ideally 
                                   pre-register, the research question prior to interacting with the 
                                   dynamic meta-analysis tool, to avoid (founded or unfounded) 
                                   accusations of p-hacking (i.e. manipulating the search strings 
                                   until results meet expectations and/or statistical significance). 
                                   On the contrary, if the results are used only for hypothesis 
                                   generation, this may in some circumstances be valid as long as it is declared 
                                   as such (i.e. 'We searched for trends within the GLiTRS Dyameta 
                                   database, to identify fields in which further study might be fruitful')."),
                                 
                                 h5(p("The GLiTRS Dynameta data")),
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     5, # width of this column within the row (each row has to sum to 12 - includes offsets)
                                     
                                     # add table legend for overview table
                                     shiny::htmlOutput("table_legend_overview"),
                                     
                                     # add sample size overview table
                                     shinycssloaders::withSpinner(shiny::tableOutput("sample_sizes_overview"), type = 8),
                                     
                                     
                                     
                                   ),
                                   
                                   column(
                                     7,
                                     
                                     # Add map of where data comes from
                                     shinycssloaders::withSpinner(leaflet::leafletOutput("map"), type = 8),
                                     
                                     # Add map figure legend
                                     shiny::htmlOutput("map_figure_legend"),
                                     
                                     tags$br(),
                                     
                                   #   # Add download button for leaflet map
                                   #   shiny::downloadButton(outputId = "download_map",
                                   #                         label = "Download map (.png)",
                                   #                         style='font-size:125%; color: #000; background-color: #eaefec; padding: 12px')
                                   #   
                                   )
                                 ),
                                 
                                 
                                 
                                 # link to code
                                 h5(shiny::icon("github", lib = "font-awesome", "fa-2x"), # add-in github icon
                                      tags$a(href="https://github.com/gls21/Dynameta", "View original Dynameta source code.", target = "_blank"),
                                      "")
                             )
                 ),# close overview tab
                 tabPanel("View effect sizes",
                      column(10, offset = 1,
                          tags$head(
                            tags$style(HTML(".dataTables_scrollHead, .dataTables_scrollBody { font-size: 14px; }"))
                          ),
                          
                          # Text to explain what the tab is for
                          p(h4(tags$b("View effect sizes"))),
                          
                          tags$hr(),
                          
                          # Include table legend for references table
                          shiny::htmlOutput("references_table_legend"),
                          
                          tabsetPanel(
                            
                            
                            
                            tabPanel("GLiTRS", h5(shinycssloaders::withSpinner(DT::DTOutput("references_table"), type = 8))),
                            tabPanel("Prior", h5(shinycssloaders::withSpinner(DT::DTOutput("prior_references_table"), type = 8)))),
                          
                          tags$br(),
                          
                          
                          # Add download button
                          shiny::downloadButton(outputId = "download_references_table",
                                                label = "Download 'GLiTRS' (.csv)",
                                                style = 'font-size:100%; color: #000; background-color: #eaefec; padding: 8px'),
                          
                          # Add download button
                          shiny::downloadButton(outputId = "download_prior_references_table",
                                                label = "Download 'Prior' (.csv)",
                                                style='font-size:100%; color: #000; background-color: #eaefec; padding: 8px'),
                          
                          tags$br(),
                          tags$br(),
                          
                          tags$br(),
                          tags$br()
                          
                      )
                          
                 ), # close view data tab
                 shiny::tabPanel("Run meta-analyses",
                             
                             column(10, offset = 1,
                                 
                                 # ******* Add code chunk ui1 here for subgroup-analysis *****************************************************************************
                                 
                                 # ----------------------------------------------------------------------------------------------------------------------
                                 
                                 # Title to show at top of tab
                                 p(h4(tags$b("Run meta-analyses"))),
                                 
                                 tags$hr(),
                                 
                                 p("Use this tab to investigate how different anthropogenic threats impact insect biodiversity. These models are multilevel meta-analytic models, run using the ", tags$a(href="https://www.metafor-project.org/doku.php/metafor", "metafor", target = "_blank"), " package. They account for the non-independence
                  of the data by specifying Paper_ID and Observation_ID as nested random effects. The effect size used to compare biodiversity is, for some of the data, the log transformed Ratio Of Means (ROM) (also known as the log response ratio), and for the rest, Hedges' D (aka Hedges' g).
                  The log response ratio quantifies proportionate change between treatments, while Hedges' D quantifies the difference between the means in terms of their standard deviations, with a correction for small sample size."),
                                 
                                 # ******* Add code chunk ui2 here for subgroup-analysis *****************************************************************************
                                 
                                 
                                 
                                 # -----------------------------------------------------------------------------------------------------------------------
                                 
                                 # ===========================================================================================================
                                 
                                 ### Running custom models
                                 
                                 # ===========================================================================================================
                                 
                                 # ******* Add code chunk ui3 here for subgroup-analysis *****************************************************************************
                                 
                                 p("Based on your research question, below you can filter the data by threat, taxonomic order, and biodiversity metric. Once you have made your selections, click 'Run custom model'.
                                                        The model will then run in real-time (this may take a while), with the results presented as a forest plot."),
                                 
                                 p("Note that users should be conscious of the dangers of multiple testing, as described ", tags$a(href="https://drmattg.github.io/Uncertain_Ecologist/Dynamic_Meta_analysis.html", "here", target = "_blank"), " by Dr Matthew J Grainger (who is unaffiliated with this project), when using GLiTRS Dynameta to run dynamic meta-analyses. Please see our publication in SoftwareX, entitled 'Dynameta: a dynamic platform for ecological meta-analyses in R Shiny', for more details."),
                                 
                                 tags$br(),
                                 
                                 # --------------------------------------------------------------------------------------------------
                                 
                                 # User inputs on what model to run and a button to run the model
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     3,
                                     
                                     h5(shiny::uiOutput("reactive_iucn_threat_category"))
                                     
                                   ),
                                   
                                   # shiny::column(
                                   #   2,
                                   #   
                                   #   h4(shiny::uiOutput("reactive_location"))
                                   #   
                                   # ),
                                   
                                   shiny::column(
                                     3,
                                     
                                     h5(shiny::uiOutput("reactive_taxa_order"))
                                     
                                   ),
                                   
                                   shiny::column(
                                     3,
                                     
                                     h5(shiny::uiOutput("reactive_biodiversity_metric_category"))
                                     
                                   ),
                                   
                                   shiny::column(
                                     3,
                                     
                                     h5(shiny::uiOutput("reactive_effect_size_category"))
                                     
                                   )
                                   
                                 ),
                                 
                                 tags$br(),
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     12,
                                     
                                     # include action button to run model once inputs have been selected
                                     shiny::actionButton("run_custom_model", "Run custom model", style='font-size:125%; color: #000; background-color: #eaefec; padding: 12px')
                                   )
                                   
                                 ),
                                 
                                 tags$br(),
                                 
                                 # --------------------------------------------------------------------------------------------------
                                 
                                 # Graph and table produced based on the custom model run
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     12,
                                     
                                     # This will make the stop error messages grey (rather than red) if the model doesn't run
                                     tags$head(tags$style(".shiny-output-error{color: grey;}")),
                                     
                                     conditionalPanel(
                                       condition = "output.small_data_warning_display",
                                       textOutput("small_data_warning")
                                     ),
                                     
                                     conditionalPanel(
                                       condition = "output.big_data_graph",
                                       textOutput("big_data_disclaimer"),
                                       shinycssloaders::withSpinner(shiny::plotOutput("custom_model_figure_big", width = 800, height = 300), type = 8)
                                     ),
                                     conditionalPanel(
                                       condition = "!output.big_data_graph",
                                       shinycssloaders::withSpinner(shiny::plotOutput("custom_model_figure_small", width = 1500, height = 1000), type = 8)
                                     )
                                     
                                   )
                                 ),
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     12,
                                     
                                     # add custom model figure legend
                                     shiny::htmlOutput("custom_model_figure_legend")
                                     
                                   )
                                   
                                 ),
                                 
                                 tags$br(),
                                 tags$hr(),
                                 
                                 # --------------------------------------------------------------------------------------------------
                                 
                                 # Add buttons for downloading custom model results
                                 
                                 p("Use this section to download the results."),
                                 
                                 p(tags$ul(
                                   tags$li("Click 'Download R custom model summary' to download a .txt file containing the output of the summary() function
                                                                applied to the custom model object. This provides a results summary of the model fitting."),
                                   tags$li("Click 'Download R custom model object' to download a .rds file containing the model object.
                                                                This has additional attributes attached, which specify the date and time the model was run,
                                                                the filters that were applied, and the R session information. Once downloaded,
                                                                use the readRDS() and attributes() functions to load the model object and view its attributes.
                                                                By downloading, it allows the same analysis to be repeated at a later date (perhaps after more data has become available)."),
                                   tags$li("Click 'Download forest plot' to download a .png file of your forest plot.")
                                 )),
                                 
                                 shiny::fluidRow(
                                   
                                   shiny::column(
                                     4,
                                     
                                     shinyjs::useShinyjs(), # so can enable and disable the download buttons
                                     
                                     # download button for downloading model output
                                     shiny::downloadButton(outputId = "download_custom_model_output",
                                                           label = "Download R custom model summary",
                                                           style='font-size:100%; color: #000; background-color: #eaefec; padding: 8px')
                                     
                                   ),
                                   
                                   shiny::column(
                                     4,
                                     
                                     shinyjs::useShinyjs(), # so can enable and disable the download buttons
                                     
                                     # download button for downloading model object in rds file
                                     shiny::downloadButton(outputId = "download_custom_model_object",
                                                           label = "Download R custom model object",
                                                           style='font-size:100%; color: #000; background-color: #eaefec; padding: 8px')
                                     
                                   ),
                                   
                                   shiny::column(
                                     4,
                                     
                                     shinyjs::useShinyjs(), # so can enable and disable the download buttons
                                     
                                     # download button for downloading forest plot
                                     shiny::downloadButton(outputId = "download_forest_plot",
                                                           label = "Download forest plot",
                                                           style='font-size:100%; color: #000; background-color: #eaefec; padding: 8px')
                                     
                                   )
                                   
                                 ),
                                 
                                 tags$br(),
                                 tags$br()
                                 
                                 # ******* Add code chunk ui4 here for subgroup-analysis *****************************************************************************
                             )
                                                 
                 ), # close modelling tab
      ),
     
      # ==============================================================
      # Resources tab
      # ==============================================================
      
      shiny::tabPanel("Read more",
                      
                  column(10, offset = 1,
                      
                      # Text to explain what the tab is for
                      p(h4(tags$b("Read more"))),
                      
                      tags$hr(),
                      
                      p("All meta-analyses performed by the GLiTRS team were carried out in line with our protocol, preregistered on the Open Science Framework (OSF)", tags$a(href="https://osf.io/mw7xq/?view_only=", "here", target = "_blank"), ". The OSF project contains: 1) a detailed guidance document; 2) a skeleton spreadheet to complete; 3) a skeleton protocol to complete; 4) a spot check spreadsheet to complete; and 5) a set of prior contributor protocols."),
                      
                      
                      p("Please refer to and follow the guidance developed by communities of practice
                       when conducting your own evidence syntheses using this resource:",
                           tags$ul(
                             tags$li(tags$a(href="https://environmentalevidence.org/", "Collaboration for Environmental Evidence (CEE)", target = "_blank"),
                                     " - For conducting environmental evidence syntheses. See ",
                                     tags$a(href="https://environmentalevidence.org/information-for-authors", "here", target = "_blank"),
                                     "for full guidance document."),
                             tags$li(tags$a(href="http://www.prisma-statement.org/", "PRISMA", target = "_blank"),
                                     " - For reporting systematic reviews and meta-analysis
                                (developed for medical field but see ecology specific version below)."),
                             tags$li(tags$a(href="https://www.prisma-statement.org/ecoevo", "PRISMA Extension for Ecology and Evolution", target = "_blank")),
                             tags$li(tags$a(href="https://www.roses-reporting.com/", "ROSES", target = "_blank"),
                                     " - For reporting systematic reviews and meta-analysis (developed for environmental research)."),
                             tags$li(tags$a(href="https://training.cochrane.org/handbook/current", "Cochrane Handbook for Systematic Reviews of Interventions", target = "_blank"),
                                     " - For conducting systematic reviews (developed for medical field)."),
                             tags$li(tags$a(href="https://www.campbellcollaboration.org/research-resources/training-courses.html", "Campbell Collaboration", target = "_blank"),
                                     " - For conducting evidence syntheses (developed for social sciences field)."),
                             tags$li(tags$a(href = "https://drmattg.github.io/Uncertain_Ecologist/Dynamic_Meta_analysis.html", "Dynamic Meta-analysis: When Transparency Meets Multiplicity by Dr Matthew J Grainger", target = "_blank"),
                                     " - Blog on the dangers of multiple testing in dynamic meta-analysis")
                           )),
                      
                      p("The following are useful guides for conducting meta-analyses:",
                           tags$ul(
                             tags$li(tags$a(href="https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/", "Doing Meta-Analysis with R: A Hands-On Guide", target = "_blank")),
                             tags$li(tags$a(href="http://www.metafor-project.org/doku.php/tips", "The metafor package tips and notes", target = "_blank"))
                           )),
                      
                      tags$br(),
                      
                      p("The data used here come from seven meta-analyses, four from pre-existing literature and three which were carried out by GLiTRS contributors."),
                      p("Prior meta-analyses:"),
                      tags$li("Gallego-Zamorano, J., M. M. de Jonge, K. Runge, et al. 2023. “Context-Dependent Responses of Terrestrial Invertebrates to Anthropogenic Nitrogen Enrichment: A Meta-Analysis.” Global Change Biology 29, no. 14: 4161–4173. ", tags$a(href = "https://doi.org/10.1111/gcb.16717", "https://doi.org/10.1111/gcb.16717", target = "_blank")),
                      tags$li("Liang, H., Y. D. He, P. Theodorou, and C. F. Yang. 2023. “The Effects of Urbanization on Pollinators and Pollination: A Meta-Analysis.” Ecology Letters 26, no. 9: 1629–1642. ", tags$a(href = "https://doi.org/10.1111/ele.14277", "https://doi.org/10.1111/ele.14277", target = "_blank")),
                      tags$li("Nessel, M. P., T. Konnovitch, G. Q. Romero, and A. L. González. 2023. “Decline of Insects and Arachnids Driven by Nutrient Enrichment: A Meta-Analysis.” Ecology 104, no. 2: e3897. ", tags$a(href = "https://doi.org/10.1002/ecy.3897", "https://doi.org/10.1002/ecy.3897", target = "_blank")),
                      tags$li("Wang, J., C. Ding, J. Heino, et al. 2020. “What Explains the Variation in Dam Impacts on Riverine Macroinvertebrates? A Global Quantitative Synthesis.” Environmental Research Letters 15, no. 12: 124028. ", tags$a(href = "https://doi.org/10.1088/1748-9326/abc4fc", "https://doi.org/10.1088/1748-9326/abc4fc", target = "_blank"), tags$br(), "Please note that we have only used a small subset of data from this paper. This is because we found a bug in the function they used to impute the standard deviation where it was not found in the literature, so we have only used data on which they did not use this function."),
                      tags$br(),
                      p("GLiTRS meta-analyses:"),
                      tags$li("Barnes, L. A., Wenban‐Smith, E., Skinner, G., Dicks, L. V., Millard, J., & Bladon, A. J. (2025). Differing Impacts of Livestock Farming and Ranching on Aquatic Insect Biodiversity: A Global Meta‐Analysis. Global Change Biology, 31(9). ", tags$a(href = "https://doi.org/10.1111/gcb.70513", "https://doi.org/10.1111/gcb.70513", target = "_blank")),
                      tags$li("Skinner, G. L. V., Cooke, R., Roy, H. E., Isaac, N. J. B., Outhwaite, C. L., Rodger, J., & Millard, J. (2026). Meta-analysis reveals negative but highly variable impacts of invasive alien species across terrestrial insect orders. Nature Communications, 17(1).", tags$a(href = "https://doi.org/10.1038/s41467-025-67925-9", "https://doi.org/10.1038/s41467-025-67925-9", target = "_blank")),
                      tags$li("And one dataset published in Skinner, G., Cooke, R., Keum, J., Purvis, A., Raw, C., Woodcock, B. A., & Millard, J. (2023). Dynameta: A dynamic platform for ecological meta-analyses in R Shiny. SoftwareX, 23, 101439.", tags$a(href = "https://doi.org/10.1016/j.softx.2023.101439", "https://doi.org/10.1016/j.softx.2023.101439", target = "_blank")),
                      tags$br(),
                      tags$br()
                  )
                      
      )  # close resources tab
      
      
      # ===============================================================================================================================
      # ===============================================================================================================================

    )
  )}
# ===============================================================================================================================
# ===============================================================================================================================

ui <- fluidPage(
  
  theme = bslib::bs_add_variables(bslib::bs_theme(
    version = 5, # updated to bootstrap 5 by RM in order to use tooltips
    bg = "#fff",
    fg = "#292C2F",
    primary = "#0483A4",
    secondary = "#EAEFEC",
    success = "#37a635",
    info = "#34b8c7",
    warning = "#F49633",
    base_font = bslib::font_link(family = "Montserrat",href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap"))
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "icon-recolor.css")
  ),
  
  shiny::titlePanel(
    div(
      tags$b("GLiTRS Dynameta"),
      img(
        src= "https://www.ceh.ac.uk/sites/default/files/images/theme/ukceh_logo_long_720x170_rgb.png",
        style= "height: 60px; vertical-align: middle; margin-left: 10px;"
      )
      
    ),
    
    windowTitle = "GLiTRS Dynameta | UK Centre for Ecology & Hydrology"),

    main_content()
  
)
