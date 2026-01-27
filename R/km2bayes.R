#' Interactive Tool for Kaplan-Meier Digitization and Bayesian Modeling
#'
#' @description
#' Launches a Shiny application that facilitates the extraction of individual patient data (IPD)
#' from Kaplan-Meier images and fits Bayesian cure models using the `bayescores` algorithm.
#'
#' The tool relies on manual digitization via `SurvdigitizeR` OR automated extraction via Google Opal (Experimental),
#' and allows for:
#' \itemize{
#'    \item Image pre-processing (brightness, contrast, borders) to improve visibility.
#'    \item Manual point extraction (point-and-click).
#'    \item Google Opal vector import.
#'    \item Interactive risk table editing.
#'    \item Bayesian model fitting (standard or historical priors).
#'    \item Comprehensive diagnostics and plotting (densities, survival curves, MCMC draws).
#' }
#'
#' @return A Shiny application object.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(survival, except = c(cluster))
#' @rawNamespace import(future, except = c(cluster))
#' @importFrom graphics legend lines
#' @importFrom stats plogis time vcov
#' @import bslib
#' @import stringr
#' @import dplyr
#' @import purrr
#' @import tibble
#' @import ggplot2
#' @import magick
#' @import survminer
#' @import SurvdigitizeR
#' @import bayescores
#' @import flexsurv
#' @import flexsurvcure
#' @import rstan
#' @import writexl
#' @import rhandsontable
#' @importFrom here here
#' @importFrom utils write.csv head globalVariables
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par rect
#' @importFrom stats median coef cor
#'
#' @export
km2bayes <- function() {

  # Increase upload size
  options(shiny.maxRequestSize = 30*1024^2)


  # ==============================================================================
  # 3. USER INTERFACE (UI)
  # ==============================================================================
  css_blue_metal <- "
  :root{
    --bm-bg: #F4F7FC; --bm-fg: #0B1F3A; --bm-muted: #5A6C80;
    --bm-primary: #1E5AA8; --bm-primary-2: #2B74C7; --bm-accent: #8FB3D9;
    --bm-card: #FFFFFF; --bm-card-2: #F2F6FD; --bm-border: #D7E1EE;
    --bm-shadow: 0 14px 32px rgba(11,31,58,0.15);
    --bm-shadow-soft: 0 8px 20px rgba(11,31,58,0.10);
    --bm-radius: 16px;
  }
  body{ background: radial-gradient(900px 600px at 20% 0%, rgba(143,179,217,0.25) 0%, rgba(244,247,252,0) 55%), radial-gradient(900px 600px at 90% 20%, rgba(30,90,168,0.15) 0%, rgba(244,247,252,0) 55%), var(--bm-bg); color: var(--bm-fg); }
  .bslib-sidebar-layout > .sidebar, .sidebar{ background: linear-gradient(180deg, #0D1E34 0%, #102845 55%, #0D1E34 100%); border-right: 1px solid rgba(255,255,255,0.08); box-shadow: 10px 0 30px rgba(11,31,58,0.12); }
  .bslib-sidebar-layout > .sidebar *, .sidebar *{ color: #EAF2FF; }
  .bslib-sidebar-layout > .sidebar .text-primary, .sidebar .text-primary{ color: #BFD6F3 !important; }
  .bslib-sidebar-layout > .sidebar hr, .sidebar hr{ border-color: rgba(234,242,255,0.12); opacity: 1; }
  .bslib-sidebar-layout > .sidebar .form-control, .sidebar .form-control, .bslib-sidebar-layout > .sidebar .form-select, .sidebar .form-select{ background: rgba(255,255,255,0.07); border: 1px solid rgba(255,255,255,0.14); color: #EAF2FF; }
  .bslib-sidebar-layout > .sidebar label, .sidebar label{ color: rgba(234,242,255,0.90); }
  .btn-primary{ background: linear-gradient(180deg, var(--bm-primary) 0%, var(--bm-primary-2) 100%); border: 1px solid rgba(255,255,255,0.15); box-shadow: var(--bm-shadow-soft); }
  .btn-primary:hover{ filter: brightness(1.03); }
  .btn-outline-danger{ border-color: rgba(255,255,255,0.30) !important; color: rgba(255,255,255,0.95) !important; }
  .btn-outline-danger:hover{ background: rgba(220,53,69,0.18) !important; border-color: rgba(255,255,255,0.35) !important; }
  .btn-opal { background: linear-gradient(180deg, #5e35b1 0%, #7e57c2 100%); border: 1px solid rgba(255,255,255,0.15); color: white !important; box-shadow: var(--bm-shadow-soft); }
  .btn-opal:hover { filter: brightness(1.1); }
  .btn-success, .btn-info, .btn-secondary, .btn-warning{ border: 1px solid rgba(255,255,255,0.14); box-shadow: var(--bm-shadow-soft); }
  .nav-underline{ border-bottom: 1px solid var(--bm-border); margin-bottom: 14px; }
  .nav-underline .nav-link{ color: var(--bm-muted); font-weight: 600; padding: 10px 14px; }
  .nav-underline .nav-link.active{ color: var(--bm-primary); border-bottom-color: var(--bm-primary); }
  .card{ border-radius: var(--bm-radius); border: 1px solid var(--bm-border); background: var(--bm-card); box-shadow: var(--bm-shadow); overflow: hidden; }
  .card-header{ background: linear-gradient(180deg, var(--bm-card-2) 0%, #FFFFFF 100%); border-bottom: 1px solid var(--bm-border); font-weight: 700; color: var(--bm-fg); }
  .card-footer{ background: linear-gradient(180deg, #FFFFFF 0%, var(--bm-card-2) 100%); border-top: 1px solid var(--bm-border); }
  #original_image_output img{ width: 100%; max-height: 420px; object-fit: contain; border-radius: 12px; border: 1px solid var(--bm-border); background: #FFFFFF; }
  .container-fluid{ padding-left: 10px; padding-right: 10px; }
  .accordion-item { background-color: rgba(255, 255, 255, 0.05); border: 1px solid rgba(255, 255, 255, 0.1); }
  .accordion-button { background-color: rgba(255, 255, 255, 0.1); color: #EAF2FF; }
  .accordion-button:not(.collapsed) { background-color: rgba(30, 90, 168, 0.4); color: #FFFFFF; }
  .accordion-body { background-color: transparent; }

  /* METRICS TABLE STYLE - 3 DIFFERENTIATED COLUMNS */
  .metrics-table-container table {
    width: 100%;
    border-collapse: separate;
    border-spacing: 0;
  }
  .metrics-table-container table th:nth-child(1),
  .metrics-table-container table th:nth-child(2) {
    background: linear-gradient(180deg, #E3F2FD 0%, #BBDEFB 100%) !important;
    border-bottom: 3px solid #1E5AA8 !important;
  }
  .metrics-table-container table th:nth-child(3),
  .metrics-table-container table th:nth-child(4) {
    background: linear-gradient(180deg, #E8F5E9 0%, #C8E6C9 100%) !important;
    border-bottom: 3px solid #2E7D32 !important;
  }
  /* Updated to Forensic Red/Orange for Correlations */
  .metrics-table-container table th:nth-child(5),
  .metrics-table-container table th:nth-child(6) {
    background: linear-gradient(180deg, #FFEBEE 0%, #FFCDD2 100%) !important;
    border-bottom: 3px solid #D32F2F !important;
  }
  .metrics-table-container table td:nth-child(1),
  .metrics-table-container table td:nth-child(2) {
    background-color: rgba(227, 242, 253, 0.3);
  }
  .metrics-table-container table td:nth-child(3),
  .metrics-table-container table td:nth-child(4) {
    background-color: rgba(232, 245, 233, 0.3);
  }
  .metrics-table-container table td:nth-child(5),
  .metrics-table-container table td:nth-child(6) {
    background-color: rgba(255, 235, 238, 0.3);
  }
  .metrics-table-container table td:nth-child(2),
  .metrics-table-container table td:nth-child(4),
  .metrics-table-container table td:nth-child(6) {
    border-right: 3px solid var(--bm-border);
  }
  .metrics-table-container table td:nth-child(1),
  .metrics-table-container table td:nth-child(3),
  .metrics-table-container table td:nth-child(5) {
    font-weight: 600;
    color: #0B1F3A;
  }

/* FIX: text visible in selectInput (tail_assumption) inside sidebar */
.bslib-sidebar-layout > .sidebar .form-select,
.sidebar .form-select{
  background-color: #FFFFFF !important;
  color: #0B1F3A !important;
}

.bslib-sidebar-layout > .sidebar .form-select option,
.sidebar .form-select option{
  background-color: #FFFFFF !important;
  color: #0B1F3A !important;
}

/* FIX REAL: selectInput (selectize) in sidebar: text and dropdown legible */
.bslib-sidebar-layout > .sidebar .selectize-control.single .selectize-input,
.sidebar .selectize-control.single .selectize-input{
  background-color: #FFFFFF !important;
  color: #0B1F3A !important;
}

.bslib-sidebar-layout > .sidebar .selectize-control.single .selectize-input input,
.sidebar .selectize-control.single .selectize-input input{
  color: #0B1F3A !important;
}

.bslib-sidebar-layout > .sidebar .selectize-dropdown,
.sidebar .selectize-dropdown{
  background-color: #FFFFFF !important;
  color: #0B1F3A !important;
}

.bslib-sidebar-layout > .sidebar .selectize-dropdown .option,
.sidebar .selectize-dropdown .option{
  color: #0B1F3A !important;
}

.bslib-sidebar-layout > .sidebar .selectize-dropdown .active,
.sidebar .selectize-dropdown .active{
  background-color: #E6F0FF !important;
  color: #0B1F3A !important;
}

/* FIX: selected value in selectize (item text) too light */
.bslib-sidebar-layout > .sidebar .selectize-control.single .selectize-input .item,
.sidebar .selectize-control.single .selectize-input .item{
  color: #0B1F3A !important;
  opacity: 1 !important;
}

/* GEMINI BUTTON - SILVER GRAY */
.btn-gemini {
  background: linear-gradient(180deg, #9E9E9E 0%, #757575 50%, #616161 100%);
  border: 1px solid rgba(255,255,255,0.25);
  color: white !important;
  box-shadow: 0 4px 15px rgba(97,97,97,0.4);
  font-weight: 600;
}
.btn-gemini:hover {
  filter: brightness(1.15);
  box-shadow: 0 6px 20px rgba(97,97,97,0.5);
}

/* IPD DOWNLOAD BUTTON - ELEGANT SMALL */
.btn-ipd-download {
  background: linear-gradient(180deg, #43A047 0%, #2E7D32 100%);
  border: 1px solid rgba(255,255,255,0.2);
  color: white !important;
  padding: 6px 12px;
  font-size: 0.85rem;
  border-radius: 8px;
  box-shadow: 0 3px 10px rgba(46,125,50,0.3);
}
.btn-ipd-download:hover {
  filter: brightness(1.1);
  box-shadow: 0 4px 14px rgba(46,125,50,0.4);
}
"

ui <- shiny::tagList(
  shiny::tags$head(shiny::tags$style(shiny::HTML(css_blue_metal))),
  bslib::page_sidebar(
    title = "KM2bayes: Instability metrics",
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", bg = "#F4F7FC", fg = "#0B1F3A", primary = "#1E5AA8"),

    sidebar = bslib::sidebar(
      width = 360,
      shiny::h6("Digitization and instability analysis tool.", style = "color: rgba(234,242,255,0.78); font-size: 0.90rem; margin-bottom: 18px;"),

      # ==============================================================================
      # SIDEBAR: EXTRACTION
      # ==============================================================================
      shiny::conditionalPanel(
        condition = "input.main_nav === 'Data Extraction' || input.main_nav === 'Stability metrics'",
        shiny::h5("Extraction Mode", class = "mb-3 text-primary"),

        # GEMINI PREPROCESSING BUTTON (MOVED UP)
        shiny::actionButton("open_gemini_modal", "Preprocess with Gemini", class = "btn-gemini w-100 mb-2", icon = shiny::icon("wand-magic-sparkles")),

        shiny::fileInput("img_upload", "1. Upload KM Image", accept = c("image/png", "image/jpeg", ".jpg", ".png")),

        # STEP 2: IMPORT
        shiny::actionButton("open_import_data", "2. Import Data (Text/Opal)", class = "btn-info w-100 mb-3", icon = shiny::icon("file-import")),

        shiny::div(
          style = "border: 1px solid rgba(255,255,255,0.2); padding: 10px; border-radius: 8px; background: rgba(0,0,0,0.15); margin-bottom: 15px;",
          shiny::h6("Axis Configuration", style = "color: #BFD6F3; font-weight: bold; margin-bottom: 10px;"),
          bslib::layout_columns(col_widths=c(4,4,4), shiny::numericInput("man_x_start","X Start",0), shiny::numericInput("man_x_end","X End",100), shiny::numericInput("man_x_inc","X Inc",20)),
          bslib::layout_columns(col_widths=c(4,4,4), shiny::numericInput("man_y_start","Y Start",0), shiny::numericInput("man_y_end","Y End",1), shiny::numericInput("man_y_inc","Y Inc",0.2,step=0.1)),
          bslib::layout_columns(col_widths=c(6,6), shiny::numericInput("man_num_curves","N Curves",2,min=1,max=5), shiny::checkboxInput("man_y_vert","Y Text Vert.",FALSE)),

          bslib::accordion(open=FALSE, bslib::accordion_panel("Image Settings",
                                                              shiny::numericInput("man_border","Border (px)",0,min=0,step=10),
                                                              shiny::sliderInput("man_brightness","Brightness (%)",50,150,130,5),
                                                              shiny::sliderInput("man_contrast","Contrast (%)",50,150,100,5),
                                                              shiny::numericInput("man_bg_light","BG Lightness",0.3,step=0.1),
                                                              # REMOVED Text Sens (unused)
                                                              shiny::checkboxInput("man_censoring","Detect Censoring",FALSE),
                                                              shiny::checkboxInput("man_enhance","Enhance Channels",FALSE)
          )),
          # STEP 3: DIGITIZE
          shiny::actionButton("run_manual_dig", "3. Digitize (Point & Click)", class="btn-primary w-100 mt-2", icon=shiny::icon("mouse-pointer")),
          shiny::div(style="height:8px;"),
          shiny::actionButton("open_opal_modal", "Google Opal (Exp)", class="btn-opal w-100", icon=shiny::icon("cloud-upload-alt"))
        ),

        # STEP 4: RECONSTRUCT
        shiny::actionButton("apply_edits","4. Analyze / Reconstruct", class="btn-primary w-100 mb-3", icon=shiny::icon("dna")),

        shiny::actionButton("reset_all", "Reset All", class="btn-outline-danger w-100 mb-2", icon=shiny::icon("trash-alt"))
      ),

      # ==============================================================================
      # SIDEBAR: BAYESIAN MODEL
      # ==============================================================================
      shiny::conditionalPanel(
        condition = "input.main_nav === 'Bayesian Model'",
        shiny::h5("Bayesian Settings", class = "mb-3 text-primary"),
        shiny::numericInput("iter", "Iterations:", value = 2000, min = 500),
        shiny::numericInput("chains", "Chains:", value = 4, min = 1, max = 4),
        shiny::numericInput("warmup", "Warmup:", value = 1000, min = 100),
        shiny::hr(),
        shiny::checkboxInput("shared_shape", "Shared Shape", value = TRUE),
        shiny::selectInput(
          "tail_assumption", "Tail Assumption:",
          choices = c("neutral", "immature_skeptical", "biologically_null", "supportive", "optimistic"),
          selected = "neutral"
        ),
        shiny::checkboxInput("use_historical", "Use Historical Prior", value = FALSE),
        shiny::conditionalPanel(
          condition = "input.use_historical == true",
          shiny::numericInput("hist_mean", "Hist. Mean:", 0),
          shiny::numericInput("hist_sd", "Hist. SD:", 1)
        ),
        shiny::actionButton("run_model", "Run Fit", class = "btn-primary w-100 mt-3", icon = shiny::icon("play")),
        shiny::hr(),
        shiny::downloadButton("dl_model_rds", "Download Model", class = "btn-secondary w-100 btn-sm"),
        shiny::downloadButton("dl_draws_csv", "MCMC Draws (CSV)", class = "btn-warning w-100 btn-sm mt-2")
      )
    ),

    bslib::navset_underline(
      id = "main_nav",

      # ==============================================================================
      # TAB 1: DATA EXTRACTION
      # ==============================================================================
      bslib::nav_panel("Data Extraction",
                       shiny::div(class="container-fluid py-3",
                                  bslib::layout_columns(col_widths=c(7,5),
                                                        bslib::card(
                                                          bslib::card_header(
                                                            shiny::div(class="d-flex justify-content-between align-items-center",
                                                                       shiny::span("Reconstructed Curves"),
                                                                       shiny::downloadButton("dl_ipd_excel", NULL, class="btn-ipd-download", icon=shiny::icon("file-excel"))
                                                            )
                                                          ),
                                                          shiny::plotOutput("km_plot_output", height="520px"),
                                                          bslib::card_footer(shiny::actionButton("switch_arms","Switch Arms", class="btn-outline-secondary btn-sm", icon=shiny::icon("exchange-alt")))
                                                        ),
                                                        bslib::card(bslib::card_header("Original Image"), shiny::imageOutput("original_image_output", height="420px"), style="background-color: rgba(255,255,255,0.55);")
                                  ),
                                  bslib::layout_columns(col_widths=c(6,6),
                                                        bslib::card(
                                                          bslib::card_header(shiny::div(class="d-flex justify-content-between align-items-center", shiny::span("Data Correction"))),
                                                          rhandsontable::rHandsontableOutput("hot_risk_table"), shiny::div(style="height:10px"), rhandsontable::rHandsontableOutput("hot_y_axis")
                                                        )
                                  )
                       )
      ),

      # ==============================================================================
      # TAB 2: FORENSIC ANALYSIS
      # ==============================================================================
      bslib::nav_panel("Stability metrics",
                       shiny::div(class="container-fluid py-3",
                                  # INSTRUCTIONS BOX
                                  bslib::card(
                                    bslib::card_header("Instructions for Bayesian Model Specification"),
                                    shiny::div(style = "background-color: #FFF8E1; padding: 15px; border-left: 5px solid #FFC107;",
                                               shiny::tags$ol(
                                                 shiny::tags$li(shiny::strong("Check Digitization Accuracy:"), " Review the calibration plot below. If the Mixture Cure model (dashed red) deviates substantially from the Kaplan-Meier curve (solid black), the digitization may need refinement."),
                                                 # UPDATED TEXT HERE: TAIL ASSUMPTION
                                                 shiny::tags$li(shiny::strong("Check Stability Metrics:"), " Use the Instability Check table and Interpretation Suggestions to decide on 'Tail Assumption' setting in the Bayesian model (e.g., 'immature_skeptical' for AFT-only models)."),
                                                 # UPDATED TEXT HERE: THRESHOLD AND MAE
                                                 shiny::tags$li(shiny::strong("Check Calibration for Shared Shape:"), " If the MAE (Mean Absolute Error) between arms differs substantially (> 3.0x), consider ", shiny::strong("unchecking 'Shared Shape'"), " in the Bayesian Model settings to allow each arm its own shape parameter.")
                                               )
                                    )
                                  ),

                                  # CALIBRATION PLOT (MOVED UP)
                                  bslib::card(bslib::card_header("Calibration: KM vs Mixture Cure"),
                                              shiny::plotOutput("calib_plot_output", height="450px")),

                                  bslib::card(
                                    bslib::card_header("Instability Check"),
                                    shiny::div(class = "metrics-table-container", style = "overflow-x: auto; max-height: 600px; overflow-y: auto;",
                                               shiny::div(class = "d-flex justify-content-center gap-3 mb-2",
                                                          shiny::span(style="background: linear-gradient(90deg, #E3F2FD, #BBDEFB); padding: 4px 12px; border-radius: 4px; font-weight: 600; border-left: 4px solid #1E5AA8;", "Sample Info"),
                                                          shiny::span(style="background: linear-gradient(90deg, #E8F5E9, #C8E6C9); padding: 4px 12px; border-radius: 4px; font-weight: 600; border-left: 4px solid #2E7D32;", "Events"),
                                                          shiny::span(style="background: linear-gradient(90deg, #FFEBEE, #FFCDD2); padding: 4px 12px; border-radius: 4px; font-weight: 600; border-left: 4px solid #D32F2F;", "Instability Metrics")
                                               ),
                                               shiny::tableOutput("metrics_summary_table"))
                                  ),

                                  # MOVED WARNING UI HERE
                                  shiny::div(class = "mb-3", shiny::uiOutput("calibration_warning_ui")),

                                  # NEW INTERPRETATION LOGIC SECTION
                                  bslib::card(
                                    bslib::card_header("Interpretation Suggestions"),
                                    shiny::div(style = "background-color: #FFF; padding: 15px; border-left: 5px solid #FF9800;",
                                               shiny::uiOutput("Interpretation_ui")
                                    )
                                  ),

                                  bslib::layout_columns(col_widths=c(6,6),
                                                        bslib::card(bslib::card_header("Mixture Cure Model Details"), shiny::div(style = "overflow-y: auto; max-height: 600px;", shiny::verbatimTextOutput("cure_surv_output"))),
                                                        bslib::card(bslib::card_header("Kaplan-Meier / Cox Summary"), shiny::div(style = "overflow-y: auto; max-height: 600px;", shiny::verbatimTextOutput("survfit_output")))
                                  )
                       )
      ),

      # ==============================================================================
      # TAB 3: BAYESIAN MODEL
      # ==============================================================================
      bslib::nav_panel(
        "Bayesian Model",
        shiny::div(
          class = "container-fluid py-3",
          bslib::card(
            bslib::card_header("Model Summary"),
            shiny::verbatimTextOutput("model_summary"),
            style = "background: linear-gradient(180deg, rgba(46,125,106,0.06) 0%, rgba(255,255,255,1) 65%); max-height: 320px; overflow-y: auto;"
          ),
          bslib::card(
            bslib::card_header("Model Visualizations"),
            bslib::navset_card_tab(
              bslib::nav_panel(
                "Posterior Densities",
                shiny::div(class = "text-end mb-2", shiny::downloadButton("dl_plot_dens", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_densities", height = "420px")
              ),
              bslib::nav_panel(
                "Correlated Densities",
                shiny::div(class = "text-end mb-2", shiny::downloadButton("dl_plot_corr", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_correlated", height = "620px")
              ),
              bslib::nav_panel(
                "Model Fit",
                shiny::div(class = "text-end mb-2", shiny::downloadButton("dl_plot_fit", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_model_fit", height = "620px")
              ),
              bslib::nav_panel(
                "Diagnostics",
                shiny::div(class = "text-end mb-2",
                           # Removed dl_draws_csv_2 per user request
                           shiny::downloadButton("dl_plot_diag", "Download Diagnostic Plot (PDF)", class = "btn-xs btn-outline-danger")
                ),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::div(shiny::h6("Convergence Table"), shiny::verbatimTextOutput("text_diagnostics_table")),
                  shiny::div(shiny::h6("Diagnostic Plot"), shiny::plotOutput("plot_diagnostics", height = "260px"))
                )
              ),
              bslib::nav_panel("Stan Code", shiny::verbatimTextOutput("repro_code"))
            )
          )
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  survival <- NULL
  curve <- survival <- Time <- NULL
  seed_val <- 555; adapt_delta_val <- 0.99
  temp_dir <- tempdir(); shiny::addResourcePath("temp_img", temp_dir)

  # AUTHENTICATION REMOVED

  vals <- shiny::reactiveValues(
    final_ipd=NULL, fit_obj=NULL, cox_obj=NULL,
    risk_table_editable=NULL, y_axis_editable=NULL,
    manual_raw_data=NULL, mode="manual",
    original_img_path=NULL, processed_img_path=NULL,
    analysis_results_full=NULL, analysis_summary_view=NULL,
    cure_model_obj=NULL, master_data_loaded=NULL, rmst_details=NULL,
    curve_mapping=NULL, calib_data=NULL,
    model_fit_obj=NULL, code_text=NULL,
    interpretation_html=NULL,
    calibration_warning_html=NULL
  )

  # ==============================================================================
  # GEMINI PREPROCESSING MODAL
  # ==============================================================================
  shiny::observeEvent(input$open_gemini_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Image Preprocessing with Gemini",
      size = "l",
      shiny::div(
        shiny::p("Use Google Gemini to preprocess your KM plot image and extract numerical data."),
        shiny::a("Open Google Gemini", href="https://gemini.google.com/app", target="_blank", class="btn btn-outline-primary w-100 mb-3", icon=shiny::icon("external-link-alt")),
        shiny::hr(),
        shiny::h6("Suggested Prompt:", style="font-weight: bold; margin-bottom: 10px;"),
        shiny::actionButton("copy_gemini_prompt", "Show Prompt to Copy", class="btn-secondary w-100 mb-3", icon=shiny::icon("copy")),
        shiny::helpText("Upload your KM plot image to Gemini and paste the prompt above to get a cleaned image and extracted data.")
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  shiny::observeEvent(input$copy_gemini_prompt, {
    prompt_text <- "Process this clinical trial plot image to perform two specific tasks. First, create a modified version of the image that cleans the plot. Preserve strictly the main data curves (including step-lines and censorship ticks), the X and Y axis lines, and the numeric values (tick labels) on both axes. Change the curve colors to dark red and dark blue for improved contrast. Remove completely all text content (including titles, axis names/labels, legends, hazard ratios, and p-values), the entire 'Numbers at risk' table below the X-axis, and any annotation markers such as arrows, brackets, or dashed/solid reference lines indicating medians or milestones that are not part of the main data curves. The final visual output should be a clean plot showing only the axis geometry, the axis numbers, and the data curves on a white background. Second, extract the numerical data from the original image into exactly 4 lines of plain text separated by line breaks. The format must be: Line 1 Y-axis values, Line 2 X-axis values, Line 3 Top row of the 'Numbers at risk' table, and Line 4 Bottom row of the 'Numbers at risk' table. Output ONLY numbers separated by spaces for each line. Do not include labels, commas, or any additional text."

    shiny::showModal(shiny::modalDialog(
      title = "Gemini Prompt",
      shiny::textAreaInput("gemini_prompt_display", label=NULL, height="250px", value=prompt_text, width="100%"),
      shiny::helpText("Copy this prompt and paste it in Gemini along with your KM plot image."),
      footer = shiny::modalButton("Close")
    ))
  })

  # 1. Image & Magick
  shiny::observeEvent(input$img_upload, { shiny::req(input$img_upload); vals$original_img_path <- input$img_upload$datapath; vals$processed_img_path <- input$img_upload$datapath })
  shiny::observe({
    shiny::req(vals$original_img_path)
    input$man_border; input$man_brightness; input$man_contrast
    tryCatch({
      img <- magick::image_read(vals$original_img_path)
      if(!is.null(input$man_border) && input$man_border>0) img <- magick::image_border(img, "white", paste0(input$man_border,"x",input$man_border))
      if(!is.null(input$man_brightness)) img <- magick::image_modulate(img, brightness=input$man_brightness)
      if(!is.null(input$man_contrast)) img <- magick::image_contrast(img, sharpen=(input$man_contrast-100)/50)
      tmp <- file.path(temp_dir, paste0("p_", as.integer(Sys.time()), ".png"))
      magick::image_write(img, tmp); vals$processed_img_path <- tmp
    }, error=function(e) warning(e))
  })

  shiny::observeEvent(input$reset_all, { session$reload() })

  # ==============================================================================
  # HELPER FUNCTION: MAP CURVES TO RISK GROUPS
  # ==============================================================================
  map_curves_to_risk_groups <- function(raw_data, risk_table) {
    curves <- unique(raw_data$curve)
    surv_avg <- sapply(curves, function(c) {
      mean(raw_data$survival[raw_data$curve == c], na.rm = TRUE)
    })
    if(surv_avg[1] > surv_avg[2]) {
      mapping <- list(curve_to_G1 = curves[1], curve_to_G2 = curves[2])
    } else {
      mapping <- list(curve_to_G1 = curves[2], curve_to_G2 = curves[1])
    }
    return(mapping)
  }

  # 2. Digitization
  # FIX 1: Added shiny::req() to validate ALL numeric inputs BEFORE using them
  # FIX 2: Changed notification to duration=NULL so it persists
  shiny::observeEvent(input$run_manual_dig, {
    shiny::req(vals$original_img_path)
    # FIX: Validate all numeric inputs before proceeding
    shiny::req(input$man_x_start, input$man_x_end, input$man_x_inc)
    shiny::req(input$man_y_start, input$man_y_end, input$man_y_inc)
    shiny::req(input$man_num_curves)
    shiny::req(input$man_x_inc > 0)  # Prevent division by zero or negative increment
    shiny::req(input$man_y_inc > 0)  # Prevent division by zero or negative increment

    vals$mode <- "manual"
    # FIX: Persistent notification
    id_dig <- shiny::showNotification("Starting digitization... Please wait.", type="message", duration=NULL)
    if(is.null(vals$risk_table_editable)) {
      vt <- seq(input$man_x_start, input$man_x_end, by=input$man_x_inc)
      vals$risk_table_editable <- tibble::tibble(Time=as.numeric(vt), N_Risk_G1=NA_real_, N_Risk_G2=NA_real_)
    }
    if(is.null(vals$y_axis_editable)) vals$y_axis_editable <- data.frame(Y_Values=seq(input$man_y_start, input$man_y_end, by=input$man_y_inc))

    tryCatch({
      # Removed word_sensitivity from args (unused)
      raw <- SurvdigitizeR::survival_digitize(img_path=vals$processed_img_path, bg_lightness=input$man_bg_light, attempt_OCR=FALSE, num_curves=input$man_num_curves, censoring=input$man_censoring, x_start=input$man_x_start, x_end=input$man_x_end, x_increment=input$man_x_inc, y_start=input$man_y_start, y_end=input$man_y_end, y_increment=input$man_y_inc, y_text_vertical=input$man_y_vert, enhance=input$man_enhance)
      if(max(raw$St, na.rm=T)>1.5) raw$survival <- raw$St/100 else raw$survival <- raw$St
      vals$manual_raw_data <- raw

      if(!is.null(vals$risk_table_editable)) {
        vals$curve_mapping <- map_curves_to_risk_groups(raw, vals$risk_table_editable)
        msg <- sprintf("Curves mapped: Curve %d -> G1 (Exp), Curve %d -> G2 (Ctrl)",
                       vals$curve_mapping$curve_to_G1, vals$curve_mapping$curve_to_G2)
        shiny::showNotification(msg, type="message", duration=8)
      }

      shiny::removeNotification(id=id_dig)
      shiny::showNotification("Digitization complete. Review Risk Table.", type="warning", duration=10)
    }, error=function(e) { shiny::removeNotification(id=id_dig); shiny::showNotification(e$message, type="error") })
  })

  # --- Opal ---
  shiny::observeEvent(input$open_opal_modal, { shiny::showModal(shiny::modalDialog(title="Google Opal", shiny::div(shiny::p("Use external link:"), shiny::a("Go to Opal", href="https://opal.google/?flow=drive:/1Nx_vaKxIgPLYisxM5xVy3WDjTb_so-fl&shared&mode=app", target="_blank", class="btn btn-outline-primary mb-3"), shiny::textAreaInput("opal_input_text", "Opal Code:", height="300px"), shiny::actionButton("process_opal", "Process", class="btn-success")), footer=shiny::modalButton("Close"))) })
  shiny::observeEvent(input$process_opal, {
    shiny::req(input$opal_input_text); txt <- input$opal_input_text
    # FIX: Persistent notification
    id_opal <- shiny::showNotification("Processing Opal data... Please wait.", type="message", duration=NULL)
    tryCatch({
      env <- new.env(); eval(parse(text=paste(grep("<- *c\\(|= *c\\(", strsplit(txt, "\n")[[1]], value=T), collapse="\n")), envir=env)
      getv <- function(x) { for(n in x) if(exists(n, env)) return(get(n, env)); stop() }
      a1x <- getv(c("arm1_x","a1_x")); a1y <- getv(c("arm1_y","a1_y")); a2x <- getv(c("arm2_x","a2_x")); a2y <- getv(c("arm2_y","a2_y"))
      rt <- getv(c("x_axis_ticks","risk_time")); r1 <- getv(c("n_risk_arm1","risk_n_arm1")); r2 <- getv(c("n_risk_arm2","risk_n_arm2")); yt <- getv(c("y_axis_ticks","y_ticks"))
      ml <- min(length(rt), length(r1), length(r2))
      vals$risk_table_editable <- tibble::tibble(Time=as.numeric(rt[1:ml]), N_Risk_G1=as.numeric(r1[1:ml]), N_Risk_G2=as.numeric(r2[1:ml]))
      vals$y_axis_editable <- data.frame(Y_Values=as.numeric(yt))
      sc <- if(max(c(a1y, a2y), na.rm=T)>1.5) 100 else 1
      vals$manual_raw_data <- rbind(data.frame(time=a1x, survival=a1y/sc, curve=1, St=a1y/sc*100), data.frame(time=a2x, survival=a2y/sc, curve=2, St=a2y/sc*100))
      vals$mode <- "opal"
      vals$curve_mapping <- map_curves_to_risk_groups(vals$manual_raw_data, vals$risk_table_editable)
      shiny::removeNotification(id=id_opal)
      shiny::removeModal(); shiny::showNotification("Opal data ready. Review table.", type="warning")
    }, error=function(e) { shiny::removeNotification(id=id_opal); shiny::showNotification("Opal Error", type="error") })
  })

  # --- Import Text ---
  shiny::observeEvent(input$open_import_data, {
    shiny::showModal(shiny::modalDialog(title="Import Text",
                                        shiny::p("Paste 1-4 rows with numbers. Order: 1) Y-axis (asc), 2) X times (asc), 3) N at Risk G1 (desc), 4) N at Risk G2 (desc). Auto-separation if needed."),
                                        shiny::a("Try extraction with Google Opal", href="https://opal.google/?flow=drive:/1RqWw_DnEXd0ir0yEUsNPtDAFlwo3y20H&shared&mode=app", target="_blank", class="btn btn-outline-info w-100 mb-2", icon=shiny::icon("external-link-alt")),
                                        shiny::actionButton("show_llm_prompt", "View Prompt for LLM (ChatGPT/Claude)", class="btn-secondary w-100 mb-3", icon=shiny::icon("robot")),
                                        shiny::textAreaInput("import_raw_text", "Paste data here:", rows=6, placeholder="0.0 0.2 ...\n0 12 ...\n100 80 ...\n100 90 ..."),
                                        footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("process_import_text", "Import", class="btn-primary"))))
  })

  shiny::observeEvent(input$show_llm_prompt, {
    shiny::showModal(shiny::modalDialog(title = "Extraction Prompt", shiny::textAreaInput("llm_prompt_copy", label=NULL, height="150px", value="Extract the numerical data from this image into exactly 4 lines of plain text separated by line breaks: 1) Y-axis values, 2) X-axis values, 3) Top row of 'Numbers at risk' table, 4) Bottom row of 'Numbers at risk' table. Only numbers separated by spaces, no labels, commas, or additional text."), footer = shiny::modalButton("Close")))
  })

  # FIX 3: Added persistent notification to process_import_text
  shiny::observeEvent(input$process_import_text, {
    shiny::req(input$import_raw_text); txt <- input$import_raw_text
    # FIX: Persistent notification so user knows it's working
    id_import <- shiny::showNotification("Processing imported data... Please wait.", type="message", duration=NULL)
    tryCatch({
      lns <- strsplit(txt, "\n")[[1]]; vl <- list(); for(l in lns) if(grepl("[0-9]", l)) vl[[length(vl)+1]] <- as.numeric(unlist(strsplit(trimws(l), "\\s+")))

      separar_auto <- function(nums) {
        nums <- nums[!is.na(nums)]
        if(length(nums) < 10) stop("Too few numbers for automatic separation.")
        cortes <- c(1)
        estado <- "y_asc"
        for(i in 2:length(nums)) {
          diff_val <- nums[i] - nums[i-1]
          if(estado == "y_asc") {
            if(diff_val < 0) { cortes <- c(cortes, i); estado <- "x_asc" }
          } else if(estado == "x_asc") {
            if(diff_val > 50) { cortes <- c(cortes, i); estado <- "g1_desc" }
          } else if(estado == "g1_desc") {
            if(diff_val > 50) { cortes <- c(cortes, i); estado <- "g2_desc" }
          }
        }
        if(length(cortes) != 4) stop("Could not detect 4 segments. Verify order: Y(asc), X(asc), G1(desc), G2(desc).")
        vy <- nums[cortes[1]:(cortes[2]-1)]
        vx <- nums[cortes[2]:(cortes[3]-1)]
        vr1 <- nums[cortes[3]:(cortes[4]-1)]
        vr2 <- nums[cortes[4]:length(nums)]
        if(!all(diff(vy) >= 0)) stop("Y-axis must be ascending.")
        if(!all(diff(vx) >= 0)) stop("X times must be ascending.")
        if(!all(diff(vr1) <= 0)) stop("G1 must be descending (n at risk decreases).")
        if(!all(diff(vr2) <= 0)) stop("G2 must be descending (n at risk decreases).")
        return(list(vy=vy, vx=vx, vr1=vr1, vr2=vr2))
      }

      if(length(vl) >= 4) {
        vy <- vl[[1]]; vx <- vl[[2]]; vr1 <- vl[[3]]; vr2 <- vl[[4]]
      } else {
        nums <- unlist(vl)
        res <- separar_auto(nums)
        vy <- res$vy; vx <- res$vx; vr1 <- res$vr1; vr2 <- res$vr2
        shiny::showNotification(paste0("Detected ", length(vl), " row(s). Automatic separation applied."), type="message", duration=5)
      }

      ml <- max(length(vx), length(vr1), length(vr2)); pad <- function(x,n) c(x, rep(NA, n-length(x)))
      vals$risk_table_editable <- tibble::tibble(Time=pad(vx,ml), N_Risk_G1=pad(vr1,ml), N_Risk_G2=pad(vr2,ml))
      shiny::updateNumericInput(session, "man_x_start", value=min(vx,na.rm=T)); shiny::updateNumericInput(session, "man_x_end", value=max(vx,na.rm=T))
      shiny::updateNumericInput(session, "man_x_inc", value=if(length(vx)>1) (max(vx)-min(vx))/(length(vx)-1) else 10)
      shiny::updateNumericInput(session, "man_y_start", value=min(vy,na.rm=T)); shiny::updateNumericInput(session, "man_y_end", value=max(vy,na.rm=T))
      shiny::updateNumericInput(session, "man_y_inc", value=if(length(vy)>1) (max(vy)-min(vy))/(length(vy)-1) else 0.1)
      vals$y_axis_editable <- data.frame(Y_Values=vy)
      shiny::removeNotification(id=id_import)
      shiny::removeModal()
      if(!is.null(vals$manual_raw_data)) {
        vals$curve_mapping <- map_curves_to_risk_groups(vals$manual_raw_data, vals$risk_table_editable)
        shiny::showNotification("Data imported. Running analysis...", type="message")
        vals$run_analysis_flag <- Sys.time()
      } else {
        shiny::showNotification("Data imported. Digitize the image to continue.", type="warning")
      }
    }, error=function(e) { shiny::removeNotification(id=id_import); shiny::showNotification(e$message, type="error") })
  })

  output$hot_risk_table <- rhandsontable::renderRHandsontable({ shiny::req(vals$risk_table_editable); rhandsontable::rhandsontable(vals$risk_table_editable, stretchH="all", height=450) %>% rhandsontable::hot_context_menu(allowRowEdit=TRUE, allowColEdit=FALSE) })
  output$hot_y_axis <- rhandsontable::renderRHandsontable({ shiny::req(vals$y_axis_editable); rhandsontable::rhandsontable(vals$y_axis_editable, stretchH="all", height=150) })

  # ==============================================================================
  # SWITCH ARMS BUTTON
  # ==============================================================================
  shiny::observeEvent(input$switch_arms, {
    shiny::req(vals$risk_table_editable)
    temp <- vals$risk_table_editable$N_Risk_G1
    vals$risk_table_editable$N_Risk_G1 <- vals$risk_table_editable$N_Risk_G2
    vals$risk_table_editable$N_Risk_G2 <- temp
    if(!is.null(vals$curve_mapping)) {
      temp_map <- vals$curve_mapping$curve_to_G1
      vals$curve_mapping$curve_to_G1 <- vals$curve_mapping$curve_to_G2
      vals$curve_mapping$curve_to_G2 <- temp_map
    }
    if(!is.null(vals$final_ipd)) {
      vals$run_analysis_flag <- Sys.time()
    }
    shiny::showNotification("Arms switched. G1 <-> G2", type="warning", duration=4)
  })

  # ==============================================================================
  # CORE ANALYSIS
  # ==============================================================================
  run_core_analysis <- function() {
    Time <- N_Risk_G1 <- time_tick <- nrisk <- N_Risk_G2 <- survival <- NULL
    shiny::req(vals$risk_table_editable, vals$y_axis_editable)
    # UX FIX: persistent notification until finished
    id_cal <- shiny::showNotification("Calculating Metrics... Please wait.", type="message", duration=NULL)

    tryCatch({
      if(is.null(vals$manual_raw_data)) stop("Missing curve data.")

      rt <- vals$risk_table_editable; vy <- vals$y_axis_editable$Y_Values

      if(is.null(vals$curve_mapping)) {
        vals$curve_mapping <- map_curves_to_risk_groups(vals$manual_raw_data, rt)
        msg <- sprintf("Auto-mapping: Curve %d -> G1, Curve %d -> G2",
                       vals$curve_mapping$curve_to_G1, vals$curve_mapping$curve_to_G2)
        shiny::showNotification(msg, type="message", duration=5)
      }

      limpiar <- function(x) as.numeric(gsub("[^0-9.]", "", as.character(x)))

      check_mono <- function(x) {
        x_clean <- as.numeric(x[!is.na(x)])
        if(length(x_clean) < 2) return(TRUE)
        if(any(diff(x_clean) > 0)) return(FALSE)
        return(TRUE)
      }
      if(!check_mono(rt$N_Risk_G1)) stop("Error: N_Risk_G1 increases over time.")
      if(!check_mono(rt$N_Risk_G2)) stop("Error: N_Risk_G2 increases over time.")

      df_c1 <- rt %>% dplyr::transmute(
        time_tick = as.numeric(Time),
        nrisk = limpiar(N_Risk_G1),
        curve = vals$curve_mapping$curve_to_G1
      ) %>% dplyr::filter(!is.na(time_tick), !is.na(nrisk))

      df_c2 <- rt %>% dplyr::transmute(
        time_tick = as.numeric(Time),
        nrisk = limpiar(N_Risk_G2),
        curve = vals$curve_mapping$curve_to_G2
      ) %>% dplyr::filter(!is.na(time_tick), !is.na(nrisk))

      nrisk_all <- dplyr::bind_rows(df_c1, df_c2)

      ipd_list <- list()
      for (cid in unique(nrisk_all$curve)) {
        km <- subset(vals$manual_raw_data, curve == cid)
        nr <- subset(nrisk_all, curve == cid)
        if (nrow(nr) >= 2 && nrow(km) > 0) {
          ipd_rec <- bayescores::reconstruct_ipd(km, nr)$ipd
          if(cid == vals$curve_mapping$curve_to_G1) {
            ipd_rec$arm <- "Group 1"
          } else {
            ipd_rec$arm <- "Group 2"
          }
          ipd_list[[length(ipd_list) + 1]] <- ipd_rec
        }
      }

      if (length(ipd_list) > 0) {
        final <- dplyr::bind_rows(ipd_list)
        mod_init <- coxph(Surv(time, status) ~ arm, final)
        hr_init <- exp(coef(mod_init)[1])
        if(!is.na(hr_init) && hr_init > 1) {
          final$arm <- ifelse(final$arm == "Group 1", "Group 2", "Group 1")
          temp_map <- vals$curve_mapping$curve_to_G1
          vals$curve_mapping$curve_to_G1 <- vals$curve_mapping$curve_to_G2
          vals$curve_mapping$curve_to_G2 <- temp_map
          temp_risk <- vals$risk_table_editable$N_Risk_G1
          vals$risk_table_editable$N_Risk_G1 <- vals$risk_table_editable$N_Risk_G2
          vals$risk_table_editable$N_Risk_G2 <- temp_risk
          mod_init <- coxph(Surv(time, status) ~ arm, final)
          shiny::showNotification("HR > 1 detected. Arms swapped automatically.", type="warning", duration=6)
        }
        vals$final_ipd <- final
        vals$fit_obj <- survfit(Surv(time, status) ~ arm, final)
        vals$cox_obj <- mod_init
      } else {
        stop("Could not reconstruct curves.")
      }

      data <- vals$final_ipd; arms <- levels(factor(data$arm)); ac <- arms[1]; ae <- arms[2]
      mt1 <- max(data$time[data$arm==ac]); mt2 <- max(data$time[data$arm==ae]); tau <- min(mt1, mt2)

      n_ctrl <- sum(data$arm==ac); n_exp <- sum(data$arm==ae)
      ev_ctrl <- sum(data$status[data$arm==ac]); ev_exp <- sum(data$status[data$arm==ae])
      cens_ctrl <- n_ctrl - ev_ctrl; cens_exp <- n_exp - ev_exp

      sf <- median(data$time); data$ts <- data$time/sf
      fc <- flexsurvcure(Surv(ts, status)~arm, data=data, anc=list(scale=~arm), dist="weibull", link="logistic", mixture=TRUE)
      vals$cure_model_obj <- fc

      # Calc Reverse KM
      rev_km <- survfit(Surv(time, 1-status) ~ 1, data=data)
      median_follow_up <- summary(rev_km)$table["median"]
      if(is.na(median_follow_up)) median_follow_up <- max(data$time) # Fallback if undefined

      # Calc Control Median Survival
      km_ctrl <- survfit(Surv(time, status) ~ 1, data=data[data$arm==ac,])
      median_surv_ctrl <- summary(km_ctrl)$table["median"]
      if(is.na(median_surv_ctrl)) median_surv_ctrl <- max(data$time[data$arm==ac]) # Fallback

      maturity_idx <- median_follow_up / median_surv_ctrl

      fc_aic <- tryCatch({
        flexsurvcure(Surv(time, status)~arm, data=data, anc=list(scale=~arm), dist="weibull", link="logistic", mixture=TRUE)
      }, error = function(e) NULL)

      res <- fc$res.t
      rn <- rownames(res)

      theta_logit <- res["theta", 1]
      p_theta <- c(plogis(res["theta", 1]), plogis(res["theta", 2]), plogis(res["theta", 3]), res["theta", 4], NA_real_)
      p_shape <- c(exp(res["shape", 1]), exp(res["shape", 2]), exp(res["shape", 3]), res["shape", 4], exp(res["shape", 1]))
      p_scale <- c(exp(res["scale", 1]), exp(res["scale", 2]), exp(res["scale", 3]), res["scale", 4], exp(res["scale", 1]))

      arm_row <- rn[!rn %in% c("theta", "shape", "scale") & !grepl("^scale\\(", rn)]
      if(length(arm_row) > 0) {
        p_arm <- c(res[arm_row[1], 1], res[arm_row[1], 2], res[arm_row[1], 3], res[arm_row[1], 4], exp(res[arm_row[1], 1]))
      } else {
        p_arm <- rep(NA, 5)
      }

      scale_arm_row <- rn[grepl("^scale\\(", rn)]
      if(length(scale_arm_row) > 0) {
        p_sc_arm <- c(res[scale_arm_row[1], 1], res[scale_arm_row[1], 2], res[scale_arm_row[1], 3], res[scale_arm_row[1], 4], exp(res[scale_arm_row[1], 1]))
      } else {
        p_sc_arm <- rep(NA, 5)
      }

      # Pearson from Vcov
      vc <- vcov(fc)
      ith <- grep("arm", rownames(vc)); isc <- grep("scale\\(arm", rownames(vc))
      idx1 <- ith[!ith %in% isc]; idx2 <- isc
      pearson <- NA
      if(length(idx1)>0 && length(idx2)>0) {
        cv <- vc[idx1[1], idx2[1]]; v1 <- vc[idx1[1], idx1[1]]; v2 <- vc[idx2[1], idx2[1]]
        if(!is.na(v1) && !is.na(v2) && v1>0 && v2>0) pearson <- cv/(sqrt(v1)*sqrt(v2))
      }

      # Parametric Bootstrap for Spearman/Kendall
      spearman <- NA; kendall <- NA
      tryCatch({
        if(length(idx1)>0 && length(idx2)>0) {
          mu_vec <- coef(fc)[c(idx1[1], idx2[1])]
          sigma_mat <- vc[c(idx1[1], idx2[1]), c(idx1[1], idx2[1])]
          # Simulate using Cholesky to avoid extra dependencies if possible
          n_sim <- 1000
          L <- chol(sigma_mat)
          Z <- matrix(stats::rnorm(n_sim * 2), n_sim, 2)
          sim_draws <- Z %*% L + matrix(rep(mu_vec, each=n_sim), nrow=n_sim)
          spearman <- cor(sim_draws[,1], sim_draws[,2], method="spearman")
          kendall <- cor(sim_draws[,1], sim_draws[,2], method="kendall")
        }
      }, error=function(e) NULL)

      rate_c <- p_theta[1]
      rate_e <- if(!is.na(theta_logit) && !is.na(p_arm[1])) plogis(theta_logit + p_arm[1]) else NA

      mae_cure <- NA
      mae_arm1 <- NA
      mae_arm2 <- NA
      mae_ratio <- NA
      tryCatch({
        arms_levels <- levels(factor(data$arm))
        arm_vals <- c(arms_levels[1], arms_levels[2])
        times_grid <- sort(unique(c(0, seq(0, tau, length.out = 101), tau)))
        times_grid <- times_grid[times_grid >= 0]
        km_fit <- survfit(Surv(time, status) ~ arm, data = data)
        s_km <- summary(km_fit, times = times_grid, extend = TRUE)
        KM_S <- matrix(NA, nrow = length(times_grid), ncol = 2)
        for(k in seq_along(times_grid)) {
          idx1 <- which(s_km$time == times_grid[k] & s_km$strata == paste0("arm=", arm_vals[1]))
          idx2 <- which(s_km$time == times_grid[k] & s_km$strata == paste0("arm=", arm_vals[2]))
          if(length(idx1) > 0) KM_S[k, 1] <- s_km$surv[idx1[1]]
          if(length(idx2) > 0) KM_S[k, 2] <- s_km$surv[idx2[1]]
        }
        KM_S[is.na(KM_S)] <- 1
        if(!is.null(fc_aic)) {
          CURE_S <- cbind(
            summary(fc_aic, newdata = data.frame(arm = arm_vals[1]), type = "survival", t = times_grid)[[1]]$est,
            summary(fc_aic, newdata = data.frame(arm = arm_vals[2]), type = "survival", t = times_grid)[[1]]$est
          )
        } else {
          CURE_S <- matrix(NA, nrow = length(times_grid), ncol = 2)
        }
        mae_arm1 <- mean(abs(CURE_S[,1] - KM_S[,1]), na.rm = TRUE)
        mae_arm2 <- mean(abs(CURE_S[,2] - KM_S[,2]), na.rm = TRUE)
        mae_cure <- mean(abs(CURE_S - KM_S), na.rm = TRUE)

        if(!is.na(mae_arm1) && !is.na(mae_arm2) && mae_arm1 > 0 && mae_arm2 > 0) {
          mae_ratio <- max(mae_arm1, mae_arm2) / min(mae_arm1, mae_arm2)
        }

        vals$calib_data <- list(
          times = times_grid,
          km_fit = km_fit,
          KM_S = KM_S,
          CURE_S = CURE_S,
          tau = tau,
          arm_vals = arm_vals,
          mae_arm1 = mae_arm1,
          mae_arm2 = mae_arm2
        )
      }, error = function(e) {
        mae_cure <<- NA
        mae_ratio <<- NA
      })

      # CALIBRATION WARNING FOR SHARED SHAPE
      vals$calibration_warning_html <- NULL
      if(!is.na(mae_ratio)) {
        # CHANGED RATIO TO 3.0 AS REQUESTED
        if(mae_ratio > 3.0) {
          vals$calibration_warning_html <- shiny::HTML(paste0(
            "<div style='background-color: #FFEBEE; padding: 12px; border-left: 5px solid #D32F2F; margin-bottom: 10px;'>",
            "<strong style='color: #D32F2F;'>&#9888; Calibration Warning:</strong> ",
            "The Mean Absolute Error (MAE) differs substantially between arms (MAE Arm1 = ", round(mae_arm1, 4),
            ", MAE Arm2 = ", round(mae_arm2, 4), ", ratio = ", round(mae_ratio, 2), "x). ",
            "This suggests the Weibull shape parameter may differ between treatment groups. ",
            "<strong>Recommendation:</strong> Consider <strong>unchecking 'Shared Shape'</strong> in the Bayesian Model settings ",
            "to allow each arm to have its own shape parameter, which may improve model fit.",
            "</div>"
          ))
        }
      }

      fr <- data.frame(
        N_Total = nrow(data), N_Ctrl = n_ctrl, N_Exp = n_exp,
        Events_Total = sum(data$status), Events_Ctrl = ev_ctrl, Events_Exp = ev_exp,
        Censored_Total = nrow(data)-sum(data$status), Censored_Ctrl = cens_ctrl, Censored_Exp = cens_exp,
        Censoring_Rate_Global = 1-mean(data$status), Censoring_Rate_Ctrl = cens_ctrl/n_ctrl, Censoring_Rate_Exp = cens_exp/n_exp,
        Tau_Common = tau,
        Pearson_Correlation = pearson,
        Spearman_Correlation = spearman,
        Kendall_Correlation = kendall,
        Maturity_Index = maturity_idx,
        Median_FollowUp = median_follow_up,
        Median_Surv_Ctrl = median_surv_ctrl,
        MAE_Arm1 = if(!is.na(mae_arm1)) mae_arm1 else NA,
        MAE_Arm2 = if(!is.na(mae_arm2)) mae_arm2 else NA,
        MAE_Ratio = if(!is.na(mae_ratio)) mae_ratio else NA
      )

      vals$analysis_results_full <- fr

      # --- PREPARE SUMMARY TABLE (REPLACING CURE MODEL COLS WITH CORRELATION/MATURITY) ---
      vals_vec <- sapply(fr, function(x) {
        if(is.numeric(x)) as.character(round(x, 4)) else as.character(x)
      })

      # Block 1: Sample Info
      b1_m <- c("N_Total", "N_Ctrl", "N_Exp", "Tau_Common")
      b1_v <- vals_vec[b1_m]

      # Block 2: Events (Added MAE_Arm1/2 here as requested "middle table")
      b2_m <- c("Events_Total", "Censored_Total", "Censoring_Rate_Global", "Events_Ctrl", "MAE_Arm1", "MAE_Arm2")
      b2_v <- vals_vec[b2_m]

      # Block 3: Instability Metrics (Added MAE_Ratio here)
      b3_m <- c("Pearson_Correlation", "Spearman_Correlation", "Kendall_Correlation",
                "Maturity_Index", "Median_FollowUp", "Median_Surv_Ctrl", "MAE_Ratio")
      b3_v <- vals_vec[b3_m]

      # Pad to ensure same length
      len <- max(length(b1_m), length(b2_m), length(b3_m))
      pad <- function(x, l) c(x, rep("", l-length(x)))

      df_view <- data.frame(
        Metric_1 = pad(names(b1_v), len), Value_1 = pad(unname(b1_v), len),
        Metric_2 = pad(names(b2_v), len), Value_2 = pad(unname(b2_v), len),
        Metric_3 = pad(names(b3_v), len), Value_3 = pad(unname(b3_v), len)
      )
      colnames(df_view) <- c("Sample Info", "Value", "Events / Calib.", "Value", "Instability Metrics", "Value")
      vals$analysis_summary_view <- df_view

      # --- GENERATE INTERPRETATION HTML (5 DECISION SCENARIOS) ---
      m_val <- maturity_idx
      rho_val <- abs(pearson)
      if(is.na(rho_val)) rho_val <- 0

      # --- PLATEAU VISUAL CHECK (15% TAIL DROP) ---
      # Logic: Check drop from 0.85*tau to tau.
      plateau_visual_stable <- TRUE
      tryCatch({
        km_check <- vals$calib_data$km_fit
        t_start_check <- 0.85 * tau
        s_start <- summary(km_check, times = t_start_check)$surv
        s_end <- summary(km_check, times = tau)$surv
        # If mean survival drops more than 0.05 in the last 15% of the curve -> Unstable
        if(mean(s_start) - mean(s_end) > 0.05) plateau_visual_stable <- FALSE
      }, error=function(e) NULL)

      interp_scenario <- ""
      interp_criteria <- ""
      interp_interpretation <- ""
      interp_protocol <- ""

      # SCENARIO 3: Structural Non-Identifiability (check first - critical)
      # UPDATED THRESHOLD TO 0.7
      if(rho_val > 0.7) {
        interp_scenario <- "Scenario 3: Structural Non-Identifiability (Model Degeneracy)"
        interp_criteria <- paste0(
          "<ul>",
          "<li><b>Parametric Stability:</b> Critical Instability (|&rho;| = ", round(rho_val, 3), " > 0.7)</li>",
          "<li><b>Inference Quality:</b> Likely divergent chains, infinite/extreme Credible Intervals, or high R-hat values.</li>",
          "</ul>"
        )
        interp_interpretation <- "The likelihood surface contains a ridge where the Cure OR and TR are interchangeable (parameter collinearity). The data supports multiple conflicting explanations with equal probability."
        interp_protocol <- paste0(
          "<b>Protocol: Parsimonious Model Selection</b>",
          "<ul>",
          "<li>Simplify the model to a standard AFT (Accelerated Failure Time) formulation.</li>",
          "<li>Assume a single mechanism (time extension) to restore convergence and interpretability.</li>",
          "<li><span style='color: #D32F2F;'><b>Action:</b> Set 'Tail Assumption' to <b>'biologically_null'</b> in Bayesian settings.</span></li>",
          "</ul>"
        )
      }
      # SCENARIO 1: Artifactual Stability
      else if(m_val < 2.5 && rho_val < 0.4) {
        interp_scenario <- "Scenario 1: Artifactual Stability (Censoring-Induced Pseudo-Plateau)"
        interp_criteria <- paste0(
          "<ul>",
          "<li><b>Data Maturity:</b> Low (M = ", round(m_val, 2), " < 2.5)</li>",
          "<li><b>Parametric Stability:</b> High (|&rho;| = ", round(rho_val, 3), " < 0.4)</li>",
          "<li><b>Signal:</b> Apparent Cure signal may be present.</li>",
          "</ul>"
        )
        interp_interpretation <- "The apparent stability of the cure parameter is likely an artifact of the follow-up cutoff ('administrative censoring wall') rather than a biological plateau."
        interp_protocol <- paste0(
          "<b>Protocol: Regularized AFT Reduction</b>",
          "<ul>",
          "<li>Apply a Skeptical Prior (e.g., Laplacian centered at 0) to the cure parameter.</li>",
          "<li>If the cure signal vanishes under regularization, reclassify the benefit as Pure Survival Time Prolongation (TR).</li>",
          "<li><span style='color: #E65100;'><b>Action:</b> Consider setting 'Tail Assumption' to <b>'immature_skeptical'</b>.</span></li>",
          "</ul>"
        )
      }
      # SCENARIO 4: Validated Curative Signal
      else if(m_val >= 3.0 && rho_val < 0.4 && plateau_visual_stable) {
        interp_scenario <- "Scenario 4: Validated Curative Signal (True Plateau)"
        interp_criteria <- paste0(
          "<ul>",
          "<li><b>Data Maturity:</b> High (M = ", round(m_val, 2), " &ge; 3.0). Follow-up extends well into the plateau phase.</li>",
          "<li><b>Parametric Stability:</b> High (|&rho;| = ", round(rho_val, 3), " < 0.4)</li>",
          "<li><b>Visual Confirmation:</b> Plateau appears stable (L-shaped curve).</li>",
          "</ul>"
        )
        interp_interpretation <- "The model has successfully identified a subpopulation with near-zero hazard, distinct from the time-to-event process of the uncured population."
        interp_protocol <- paste0(
          "<b>Protocol: Full Mixture Model Reporting</b>",
          "<ul>",
          "<li>Report both efficacy dimensions with high confidence.</li>",
          "<li>Use the <b>Cure Odds Ratio (OR)</b> as the primary endpoint for long-term efficacy.</li>",
          "<li>Use the <b>Time Ratio (TR)</b> to quantify benefit for the non-cured population.</li>",
          "<li><span style='color: #2E7D32;'><b>Action:</b> 'Tail Assumption' can remain <b>'neutral'</b> or <b>'supportive'</b>.</span></li>",
          "</ul>"
        )
      }
      # SCENARIO 2: Confirmed Non-Curative Delay
      else if(m_val > 3.0 && !plateau_visual_stable) {
        interp_scenario <- "Scenario 2: Confirmed Non-Curative Delay (Pure TR Effect)"
        interp_criteria <- paste0(
          "<ul>",
          "<li><b>Data Maturity:</b> High (M = ", round(m_val, 2), " > 3.0)</li>",
          "<li><b>Visual Inspection:</b> Kaplan-Meier curves decline continuously (no visual plateau).</li>",
          "<li><b>Signal:</b> Cure OR is likely non-significant or negligible.</li>",
          "</ul>"
        )
        interp_interpretation <- "The treatment effect is identified as a pure temporal shift rather than a change in the susceptible fraction. The initial separation of curves has been 'diluted' over time."
        interp_protocol <- paste0(
          "<b>Protocol: Standard AFT Reporting</b>",
          "<ul>",
          "<li>Do not force regularization.</li>",
          "<li>Base inference exclusively on the <b>Time Ratio (TR)</b> parameter.</li>",
          "<li>Report: 'The therapy extends median survival by X%, with no evidence of a cured fraction.'</li>",
          "<li><span style='color: #1565C0;'><b>Action:</b> Set 'Tail Assumption' to <b>'immature_skeptical'</b>.</span></li>",
          "</ul>"
        )
      }
      # SCENARIO 5: Indeterminate Efficacy Signal (default/intermediate)
      else {
        interp_scenario <- "Scenario 5: Indeterminate Efficacy Signal (Provisional)"
        interp_criteria <- paste0(
          "<ul>",
          "<li><b>Data Maturity:</b> Intermediate (M = ", round(m_val, 2), "). Follow-up is ongoing; the 'tail' is forming but not fully stabilized.</li>",
          "<li><b>Parametric Stability:</b> Moderate Ambiguity (|&rho;| = ", round(rho_val, 3), ").</li>",
          "<li><b>Signal:</b> The Cure OR may suggest benefit, but Credible Intervals are likely wide or borderline.</li>",
          "</ul>"
        )
        interp_interpretation <- "The likelihood surface allows for a trade-off between a 'larger cure' and a 'longer delay.' While a benefit is likely present, the specific mechanism (Cure vs. Time) cannot be strictly resolved without further follow-up."
        interp_protocol <- paste0(
          "<b>Protocol: Provisional Reporting (Low Confidence)</b>",
          "<ul>",
          "<li>Report the estimated parameters but append a mandatory <b>'Provisional'</b> caveat.</li>",
          "<li>Explicitly state that the decomposition of benefit is contingent on future data maturation.</li>",
          "<li>Prioritize the <b>Time Ratio (TR)</b> as the safer metric until M &ge; 3.0.</li>",
          "<li><span style='color: #FF6F00;'><b>Action:</b> Consider <b>'immature_skeptical'</b> for conservative inference, or <b>'neutral'</b> with caution.</span></li>",
          "</ul>"
        )
      }

      html_content <- paste0(
        "<h5>", interp_scenario, "</h5>",
        "<hr>",
        "<p><b>Diagnostic Criteria:</b></p>",
        interp_criteria,
        "<p><b>Interpretation:</b> ", interp_interpretation, "</p>",
        "<hr>",
        interp_protocol
      )
      vals$interpretation_html <- shiny::HTML(html_content)

      shiny::removeNotification(id=id_cal)
      shiny::showNotification("Analysis Complete.", type="message")
    }, error = function(e) { shiny::removeNotification(id=id_cal); shiny::showNotification(paste("Error:", e$message), type="error") })
  }

  output$Interpretation_ui <- shiny::renderUI({
    shiny::req(vals$interpretation_html)
    vals$interpretation_html
  })

  output$calibration_warning_ui <- shiny::renderUI({
    vals$calibration_warning_html
  })

  shiny::observeEvent(input$apply_edits, {
    vals$risk_table_editable <- rhandsontable::hot_to_r(input$hot_risk_table)
    vals$y_axis_editable <- rhandsontable::hot_to_r(input$hot_y_axis)
    run_core_analysis()
  })

  shiny::observeEvent(vals$run_analysis_flag, { run_core_analysis() }, ignoreInit = TRUE)

  # ==============================================================================
  # BAYESIAN MODEL
  # ==============================================================================
  shiny::observeEvent(input$run_model, {
    shiny::req(vals$final_ipd)
    # UX FIX: Persistent notification
    id_mod <- shiny::showNotification("Compiling and fitting model... This may take several minutes.", type = "message", duration = NULL)
    tryCatch({
      hist_arg <- if (input$use_historical) {
        paste0("TRUE, params=c(", input$hist_mean, ",", input$hist_sd, ")")
      } else {
        paste0("FALSE, tail_assumption='", input$tail_assumption, "'") # Fixed typo: changed belief to tail_assumption
      }
      vals$code_text <- paste0(
        "library(bayescores)\nlibrary(rstan)\nipd <- readRDS('path/to/ipd.rds')\n",
        "fit <- fit_bayesian_cure_model(ipd, time_col='time', event_col='status', arm_col='arm', iter=",
        input$iter, ", chains=", input$chains, ", warmup=", input$warmup, ", shared_shape=",
        input$shared_shape, ", use_historical_prior=", hist_arg, ")\n",
        "diagnose_fit(fit$stan_fit)\nmodel_diagnostics(fit)\n",
        "# Plots\n",
        "bayescores::plot_densities(fit)\n",
        "bayescores::plot_correlated_densities(fit)"
      )
      vals$model_fit_obj <- bayescores::fit_bayesian_cure_model(
        vals$final_ipd,
        time_col = "time", event_col = "status", arm_col = "arm",
        iter = input$iter, chains = input$chains, warmup = input$warmup,
        seed = seed_val, adapt_delta = adapt_delta_val,
        shared_shape = input$shared_shape,
        use_historical_prior = input$use_historical,
        historical_prior_params = c(input$hist_mean, input$hist_sd),
        tail_assumption = input$tail_assumption
      )
      shiny::removeNotification(id = id_mod)
      shiny::showNotification("Model fitted!", type = "message")
    }, error = function(e) {
      shiny::removeNotification(id = id_mod)
      shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })

  output$model_summary <- shiny::renderPrint({
    shiny::req(vals$model_fit_obj)
    # FIX: Use correct argument based on user feedback
    if (exists("outcomes", asNamespace("bayescores"))) {
      bayescores::outcomes(vals$model_fit_obj, correlation_method = "pearson")
    } else {
      print(vals$model_fit_obj)
    }
  })

  output$plot_densities <- shiny::renderPlot({
    shiny::req(vals$model_fit_obj)
    p <- bayescores::plot_densities(vals$model_fit_obj)
    print(p + ggplot2::theme(aspect.ratio = 1))
  })

  output$plot_correlated <- shiny::renderPlot({
    shiny::req(vals$model_fit_obj)
    bayescores::plot_correlated_densities(vals$model_fit_obj) + ggplot2::theme(aspect.ratio = 1)
  })

  output$plot_model_fit <- shiny::renderPlot({
    shiny::req(vals$model_fit_obj)
    graphics::par(pty = "s")
    plot(vals$model_fit_obj)
  })

  output$text_diagnostics_table <- shiny::renderPrint({
    shiny::req(vals$model_fit_obj)
    tryCatch({
      if (exists("diagnose_fit", asNamespace("bayescores"))) {
        print(bayescores::diagnose_fit(vals$model_fit_obj$stan_fit))
      } else {
        print(rstan::monitor(vals$model_fit_obj$stan_fit, print = FALSE))
      }
    }, error = function(e) {
      print(rstan::check_hmc_diagnostics(vals$model_fit_obj$stan_fit))
    })
  })

  output$plot_diagnostics <- shiny::renderPlot({
    shiny::req(vals$model_fit_obj)
    tryCatch({
      bayescores::model_diagnostics(vals$model_fit_obj)
    }, error = function(e) {
      plot(vals$model_fit_obj$stan_fit, pars = c("lp__"))
    })
  })

  output$repro_code <- shiny::renderText({
    shiny::req(vals$code_text)
    vals$code_text
  })

  output$dl_model_rds <- shiny::downloadHandler(
    filename = function() "bayesian_model.rds",
    content = function(f) saveRDS(vals$model_fit_obj, f)
  )

  # CSV Download logic
  draws_csv_logic <- function(f) {
    shiny::req(vals$model_fit_obj)
    draws <- as.data.frame(vals$model_fit_obj$stan_fit)
    write.csv(draws, f, row.names = FALSE)
  }

  output$dl_draws_csv <- shiny::downloadHandler(filename = "mcmc_draws.csv", content = draws_csv_logic)

  output$dl_plot_dens <- shiny::downloadHandler(
    filename = "densities.pdf",
    content = function(f) {
      pdf(f, width = 10, height = 8, onefile=FALSE)
      p <- bayescores::plot_densities(vals$model_fit_obj)
      print(p + ggplot2::theme(aspect.ratio = 1))
      dev.off()
    }
  )

  output$dl_plot_corr <- shiny::downloadHandler(
    filename = "correlated.pdf",
    content = function(f) {
      pdf(f, width = 10, height = 8)
      print(bayescores::plot_correlated_densities(vals$model_fit_obj) + ggplot2::theme(aspect.ratio = 1))
      dev.off()
    }
  )

  output$dl_plot_fit <- shiny::downloadHandler(
    filename = "fit.pdf",
    content = function(f) {
      pdf(f, width = 10, height = 8)
      graphics::par(pty = "s")
      plot(vals$model_fit_obj)
      dev.off()
    }
  )

  # Updated: Download PLOT in Diagnostics tab - FIX to ensure PDF generation works
  output$dl_plot_diag <- shiny::downloadHandler(
    filename = "diagnostics.pdf",
    content = function(f) {
      shiny::req(vals$model_fit_obj)
      pdf(f, width = 10, height = 8)
      tryCatch({
        p_diag <- bayescores::model_diagnostics(vals$model_fit_obj)
        # Check if result is a ggplot/gtable object and explicitly print it
        if(!is.null(p_diag) && (inherits(p_diag, "ggplot") || inherits(p_diag, "gtable"))) {
          print(p_diag)
        }
      }, error = function(e) {
        # Fallback
        plot(vals$model_fit_obj$stan_fit, pars = c("lp__"))
      })
      dev.off()
    }
  )

  # ==============================================================================
  # ORIGINAL OUTPUTS
  # ==============================================================================
  output$metrics_summary_table <- shiny::renderTable({ shiny::req(vals$analysis_summary_view); vals$analysis_summary_view }, striped=TRUE, bordered=TRUE)

  output$survfit_output <- shiny::renderPrint({
    shiny::req(vals$fit_obj)
    cat("--- SURVFIT (KAPLAN-MEIER) ---\n\n")
    print(vals$fit_obj)
    if(!is.null(vals$cox_obj)) {
      cat("\n\n--- COX MODEL SUMMARY ---\n")
      print(summary(vals$cox_obj))
    }
  })

  output$cure_surv_output <- shiny::renderPrint({ shiny::req(vals$cure_model_obj); print(vals$cure_model_obj); cat("\nPEARSON CORRELATION:", vals$analysis_results_full$Pearson_Correlation) })

  output$calib_plot_output <- shiny::renderPlot({
    shiny::req(vals$calib_data)
    cd <- vals$calib_data
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("black", "gray60"),
         xlab = "Time", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Control (tau=", round(cd$tau, 1), ")"))
    lines(cd$times, cd$CURE_S[,1], lty = 3, lwd = 2, col = "red")
    legend("topright", legend = c("KM", "Cure"),
           lty = c(1, 3), lwd = 2, col = c("black", "red"), bty = "n", cex = 0.9)
    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("gray60", "black"),
         xlab = "Time", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Experimental (tau=", round(cd$tau, 1), ")"))
    lines(cd$times, cd$CURE_S[,2], lty = 3, lwd = 2, col = "red")
    legend("topright", legend = c("KM", "Cure"),
           lty = c(1, 3), lwd = 2, col = c("black", "red"), bty = "n", cex = 0.9)
  })

  output$original_image_output <- shiny::renderImage({
    if(vals$mode=="manual" && !is.null(vals$processed_img_path))
      list(src=vals$processed_img_path, height="100%")
    else
      list(src="", alt="")
  }, deleteFile=FALSE)

  output$km_plot_output <- shiny::renderPlot({
    if(!is.null(vals$fit_obj)) {
      survminer::ggsurvplot(vals$fit_obj, data=vals$final_ipd, pval=TRUE, risk.table=TRUE, risk.table.height=0.25)
    } else if(!is.null(vals$manual_raw_data)) {
      ggplot(vals$manual_raw_data, aes(x=time, y=survival, col=factor(curve))) +
        geom_step() +
        labs(title="Digitized Curves (Pre-Analysis)",
             subtitle=if(!is.null(vals$curve_mapping)) sprintf("Curve %d -> G1 (Exp), Curve %d -> G2 (Ctrl)",
                                                               vals$curve_mapping$curve_to_G1,
                                                               vals$curve_mapping$curve_to_G2) else NULL)
    }
  })

  output$dl_ipd_excel <- shiny::downloadHandler(
    filename="ipd.xlsx",
    content=function(f) writexl::write_xlsx(vals$final_ipd, f)
  )
}

return(shiny::shinyApp(ui, server))
}
