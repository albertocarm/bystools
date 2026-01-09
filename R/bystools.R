#' Bystools: Full Bayesian Pipeline (Interactive Shiny App)
#'
#' Launches an interactive Shiny application that supports end-to-end execution
#' of the Bayescores Full Bayesian Pipeline, including (i) digitalization of
#' Kaplan–Meier curves from supplementary materials and (ii) streamlined entry
#' of quality-of-life (QoL) and toxicity data.
#'
#' @import bsicons
#' @import tesseract
#' @import magick
#' @import stringr
#' @import ggplot2
#' @import dplyr
#' @import survminer
#' @import writexl
#' @import DT
#' @import rstan
#' @import promises
#' @import bslib
#' @import bayescores
#' @import future
#' @importFrom utils capture.output data str head
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics barplot curve
#' @importFrom stats setNames
#' @importFrom survival coxph survfit
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#'
#' @export
bystools <- function() {

  # --- RSTAN CONFIGURATION ---
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)

  # --- ASYNC CONFIGURATION ---
  future::plan(future::multisession)

  # --- DATASET DISCOVERY ---
  all_datasets <- data(package = "bystools")$results[, "Item"]
  eff_datasets <- all_datasets[!grepl("toxicity|tox", all_datasets, ignore.case = TRUE)]
  tox_datasets <- all_datasets[grepl("toxicity|tox", all_datasets, ignore.case = TRUE)]

  # --- GLOBAL HELPERS ---
  get_system_organ_class <- function(event_name) {
    evt <- tolower(event_name)
    if (any(str_detect(evt, "nausea|vomit|diarrh|constipat|abdom|colitis|stomatitis|dyspepsia|ascites|ileus|mucositis|pancreatitis|gastritis|esophagitis|enteritis|gastroduodenitis|abdominal pain"))) {
      return("Gastrointestinal disorders")
    } else if (any(str_detect(evt, "anemia|neutropen|thrombocyto|platelet|leukopen|white blood|lymphopen|hemoglo|pancyto|eosinophil|neutrophil"))) {
      return("Blood and lymphatic system disorders")
    } else if (any(str_detect(evt, "fatigue|astheni|pyrexia|fever|edema|appetite|weight|dehydrat|malaise|pain|chills|infusion|fatig|generic|immunomediated|edema"))) {
      return("General, metabolic, and other disorders")
    } else if (any(str_detect(evt, "rash|pruritus|alopecia|palmar|skin|erythema|dermatitis|dry skin|vitiligo|acne|hand-foot|mucosal infl"))) {
      return("Dermatologic disorders")
    } else if (any(str_detect(evt, "infect|sepsis|pneumonia|nasopharyng|urinary|candidiasis|influenza|covid|herpes|bacteremia"))) {
      return("Infections and infestations")
    } else if (any(str_detect(evt, "dyspnea|cough|pneumonitis|effusion|embolism|respiratory|hypoxia|nasal|epistaxis|interstitial lung disease"))) {
      return("Respiratory, thoracic and mediastinal disorders")
    } else if (any(str_detect(evt, "neuropathy|dizzin|headache|paresthesia|dysgeusia|neuro|insomnia|confus|sensory"))) {
      return("Nervous system disorders")
    } else if (any(str_detect(evt, "alt|ast|bilirubin|alkaline|liver|hepat|gamma-glutamyl|transaminase|aspartate|alanine|lipase|amylase"))) {
      return("Hepatobiliary disorders")
    } else if (any(str_detect(evt, "arthralgia|myalgia|back pain|bone|muscle|spasm|skeletal|extremity"))) {
      return("Musculoskeletal and connective tissue disorders")
    } else if (any(str_detect(evt, "hypokal|hyponat|hypergly|magnes|calci|glucose|creatinine|thyroid|hypothyr|hyperthyr|ts h|hyperglycemia"))) {
      return("Endocrine or Metabolic disorders")
    } else if (any(str_detect(evt, "hypertension|vascular|blood pressure|thrombosis"))) {
      return("Vascular disorders")
    } else if (any(str_detect(evt, "nephritis|renal|proteinuria|creatinine|kidney"))) {
      return("Renal and urinary disorders")
    } else { return("General, metabolic, and other disorders") }
  }

  toxicity_master_list <- c(
    "Fatigue", "Diarrhea", "Pruritus", "Rash", "Nausea", "Vomiting",
    "Decreased Appetite", "Asthenia", "Pyrexia", "Arthralgia",
    "Hypothyroidism", "Hyperthyroidism", "ALT increased", "AST increased",
    "Pneumonitis", "Colitis", "Hepatitis", "Nephritis", "Infusion-related reaction",
    "Anemia", "Neutropenia", "Thrombocytopenia", "Peripheral Neuropathy",
    "Dry Skin", "Stomatitis", "Dysgeusia", "Myalgia",
    "Constipation", "Alopecia", "Hypertension", "Infection", "Weight loss",
    "Dyspnea", "Headache", "Insomnia", "Proteinuria", "Hyperglycemia",
    "Abdominal pain", "Immunomediated toxicity", "Edema", "Thrombosis",
    "Interstitial lung disease"
  )

  # --- UI ---
  ui <- page_navbar(
    title = tags$div(
      style = "display: flex; align-items: center;",
      tags$span(
        style = "font-size: 1.8rem; font-weight: 700; letter-spacing: -0.5px;",
        tags$span(style = "color: #a8c5e2;", "bys"),
        tags$span(style = "color: #e8eef4;", "tools")
      )
    ),
    id = "main_nav",
    theme = bs_theme(version = 5, bootswatch = "flatly"),

    header = tagList(
      tags$head(
        tags$style(HTML("
          body { background-color: #f0f2f5; }
          .navbar {
            padding: 6px 24px !important;
            background: linear-gradient(135deg, #1a2942 0%, #2c3e50 100%) !important;
            border-bottom: none !important;
            flex-wrap: nowrap !important;
            box-shadow: 0 2px 12px rgba(26, 41, 66, 0.15);
          }
          .navbar-brand {
            margin-right: 250px !important;
          }
          .navbar-nav {
            flex-direction: row !important;
            gap: 8px;
            flex-wrap: nowrap !important;
          }
          .navbar .nav-link {
            font-size: 0.82rem !important;
            font-weight: 600 !important;
            color: rgba(255, 255, 255, 0.85) !important;
            padding: 10px 18px !important;
            border-radius: 8px !important;
            transition: all 0.25s ease;
            background: rgba(255, 255, 255, 0.08);
            border: 1px solid rgba(255, 255, 255, 0.12);
            white-space: nowrap;
            backdrop-filter: blur(4px);
          }
          .navbar .nav-link.active {
            color: #1a2942 !important;
            background: rgba(255, 255, 255, 0.95) !important;
            border-color: rgba(255, 255, 255, 0.95);
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.12);
          }
          .navbar .nav-link:hover:not(.active) {
            background: rgba(255, 255, 255, 0.18) !important;
            color: #ffffff !important;
            border-color: rgba(255, 255, 255, 0.25);
          }
          .card {
            box-shadow: 0 2px 8px rgba(26, 41, 66, 0.06);
            border: 1px solid rgba(26, 41, 66, 0.08);
            border-radius: 12px;
            margin-bottom: 20px;
            background: rgba(255, 255, 255, 0.98);
          }
          .result-container {
            background: rgba(248, 250, 252, 0.95) !important;
            border-left: 4px solid #1a2942 !important;
            padding: 20px;
            border-radius: 10px;
            box-shadow: inset 0 1px 3px rgba(26, 41, 66, 0.04);
          }
          .section-title {
            font-weight: 700;
            color: #1a2942;
            border-bottom: 2px solid #3d5a80;
            padding-bottom: 10px;
            margin-bottom: 18px;
            font-size: 1rem;
            letter-spacing: -0.3px;
          }
          .upload-box {
            border: 2px dashed rgba(61, 90, 128, 0.35);
            border-radius: 10px;
            padding: 22px;
            text-align: center;
            cursor: pointer;
            transition: all 0.25s ease;
            background: rgba(248, 250, 252, 0.6);
            position: relative;
          }
          .upload-box:hover {
            border-color: #3d5a80;
            background: rgba(61, 90, 128, 0.06);
            transform: translateY(-1px);
          }
          .upload-success {
            color: #1a5235;
            font-weight: 600;
            margin-top: 10px;
            font-size: 0.82em;
            background: rgba(212, 237, 218, 0.9);
            padding: 8px 12px;
            border-radius: 6px;
            display: inline-block;
          }
          .action-btn {
            font-weight: 600;
            border-radius: 8px;
            transition: all 0.25s ease;
            cursor: pointer;
            padding: 12px 22px;
            letter-spacing: 0.2px;
          }
          .action-btn:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 12px rgba(26, 41, 66, 0.18) !important;
          }
          .btn-success {
            background: linear-gradient(135deg, #1a5235 0%, #27ae60 100%) !important;
            border: none !important;
          }
          .btn-danger {
            background: linear-gradient(135deg, #1a2942 0%, #3d5a80 100%) !important;
            border: none !important;
          }
          .btn-warning {
            background: linear-gradient(135deg, #8b6914 0%, #d4a017 100%) !important;
            border: none !important;
            color: #fff !important;
          }
          .btn-info {
            background: linear-gradient(135deg, #2c5282 0%, #4299e1 100%) !important;
            border: none !important;
          }
          .btn-primary {
            background: linear-gradient(135deg, #1a2942 0%, #3d5a80 100%) !important;
            border: none !important;
          }
          .reset-btn {
            font-size: 0.78rem;
            border-radius: 6px;
            padding: 6px 14px;
            background: transparent;
            border: 1px solid rgba(26, 41, 66, 0.25);
            color: #4a5568;
          }
          .reset-btn:hover {
            background: rgba(220, 53, 69, 0.08) !important;
            border-color: rgba(220, 53, 69, 0.4) !important;
            color: #c53030 !important;
          }
          .library-loaded-indicator {
            background: rgba(212, 237, 218, 0.9);
            color: #1a5235;
            padding: 10px 14px;
            border-radius: 6px;
            margin-top: 10px;
            font-weight: 600;
            font-size: 0.82rem;
            border: 1px solid rgba(26, 82, 53, 0.15);
          }
          .option-card {
            border: 1px solid rgba(26, 41, 66, 0.1);
            border-radius: 10px;
            padding: 18px;
            background: rgba(255, 255, 255, 0.85);
            margin-bottom: 14px;
            transition: all 0.2s ease;
          }
          .option-card:hover {
            box-shadow: 0 2px 8px rgba(26, 41, 66, 0.08);
          }
          .option-card h6 {
            color: #1a2942;
            font-weight: 700;
            margin-bottom: 12px;
            font-size: 0.92rem;
          }
          .option-label {
            display: inline-block;
            color: white;
            font-weight: 700;
            padding: 3px 10px;
            border-radius: 5px;
            font-size: 0.68rem;
            margin-bottom: 10px;
            letter-spacing: 0.5px;
            text-transform: uppercase;
          }
          .option-label-a { background: linear-gradient(135deg, #1a5235 0%, #27ae60 100%); }
          .option-label-b { background: linear-gradient(135deg, #553c9a 0%, #805ad5 100%); }
          .option-label-c { background: linear-gradient(135deg, #8b6914 0%, #d4a017 100%); }
          .sidebar {
            background: rgba(248, 250, 252, 0.95) !important;
            border-right: 1px solid rgba(26, 41, 66, 0.08);
          }
          .stan-log-container {
            background: linear-gradient(135deg, #1a2942 0%, #0d1520 100%);
            color: #e2e8f0;
            font-family: 'SF Mono', 'Consolas', 'Monaco', monospace;
            font-size: 0.73rem;
            padding: 16px;
            border-radius: 10px;
            max-height: 400px;
            overflow-y: auto;
            white-space: pre-wrap;
            word-wrap: break-word;
            box-shadow: inset 0 2px 4px rgba(0, 0, 0, 0.2);
          }
          .stan-log-container pre {
            color: #e2e8f0 !important;
            margin: 0;
            background: transparent;
          }
          .stan-running-indicator {
            display: inline-block;
            width: 10px;
            height: 10px;
            background: linear-gradient(135deg, #48bb78 0%, #38a169 100%);
            border-radius: 50%;
            margin-right: 10px;
            animation: pulse 1.5s infinite;
            box-shadow: 0 0 8px rgba(72, 187, 120, 0.5);
          }
          @keyframes pulse {
            0% { opacity: 1; transform: scale(1); }
            50% { opacity: 0.5; transform: scale(0.95); }
            100% { opacity: 1; transform: scale(1); }
          }
          .pdf-download-btn {
            font-size: 0.73rem;
            padding: 5px 12px;
            border-radius: 6px;
            background: transparent;
            border: 1px solid rgba(26, 41, 66, 0.2);
            color: #4a5568;
          }
          .pdf-download-btn:hover {
            background: rgba(26, 41, 66, 0.06);
            border-color: rgba(26, 41, 66, 0.35);
          }
          .soc-weight-input {
            font-size: 0.8rem;
          }
          .soc-weight-input .form-label {
            font-size: 0.73rem;
            margin-bottom: 3px;
            color: #4a5568;
          }
          .form-control, .form-select {
            border-radius: 8px;
            border: 1px solid rgba(26, 41, 66, 0.15);
            transition: all 0.2s ease;
          }
          .form-control:focus, .form-select:focus {
            border-color: #3d5a80;
            box-shadow: 0 0 0 3px rgba(61, 90, 128, 0.15);
          }
          .param-box {
            background: rgba(248, 250, 252, 0.8);
            border: 1px solid rgba(26, 41, 66, 0.1);
          }
          hr {
            border-color: rgba(26, 41, 66, 0.1);
          }
          .text-muted {
            color: #718096 !important;
          }
          .small, small {
            color: #718096;
          }
          .navset-card-underline .nav-link {
            background: transparent !important;
            border: none !important;
            color: #4a5568 !important;
            padding: 8px 16px !important;
            font-weight: 600 !important;
          }
          .navset-card-underline .nav-link.active {
            color: #1a2942 !important;
            border-bottom: 2px solid #1a2942 !important;
            background: transparent !important;
          }
          .card .nav-tabs .nav-link,
          .navset-card-tab .nav-tabs .nav-link,
          .navset-card-tab .nav-link {
            background: #f8f9fa !important;
            border: 1px solid #dee2e6 !important;
            border-bottom: none !important;
            color: #495057 !important;
            padding: 10px 18px !important;
            font-weight: 600 !important;
            margin-right: 4px;
            border-radius: 8px 8px 0 0 !important;
          }
          .card .nav-tabs .nav-link.active,
          .navset-card-tab .nav-tabs .nav-link.active,
          .navset-card-tab .nav-link.active {
            color: #1a2942 !important;
            background: #ffffff !important;
            border-color: #dee2e6 !important;
            border-bottom: 1px solid #ffffff !important;
          }
          .card .nav-tabs .nav-link:hover:not(.active),
          .navset-card-tab .nav-tabs .nav-link:hover:not(.active),
          .navset-card-tab .nav-link:hover:not(.active) {
            color: #1a2942 !important;
            background: #e9ecef !important;
          }
        "))
      )
    ),

    nav_panel("1. Data",
              div(class = "container-fluid py-4", style = "max-width: 1600px;",
                  div(class = "d-flex justify-content-end mb-3",
                      actionButton("reset_tab1", "Reset Tab", class = "btn-outline-secondary btn-sm reset-btn", icon = icon("refresh"))),
                  layout_columns(col_widths = c(4, 8),
                                 div(
                                   card(
                                     h5("Load Survival Data", class="section-title"),
                                     p("Choose one of the following options to load your IPD data:", class="text-muted mb-3", style="font-size: 0.9rem;"),

                                     div(class = "option-card",
                                         span("OPTION A", class="option-label option-label-a"),
                                         h6("Internal Package Data"),
                                         p("Load pre-specified datasets included in the bystools library", class="small text-muted mb-2"),
                                         selectInput("pkg_eff_select", "Select Dataset:", choices = setNames(eff_datasets, toupper(eff_datasets))),
                                         actionButton("load_pkg_eff", "Load Dataset", class="btn-success action-btn w-100", icon = icon("database")),
                                         uiOutput("status_load_eff")
                                     ),

                                     div(class = "option-card",
                                         span("OPTION B", class="option-label option-label-b"),
                                         h6("Upload External File"),
                                         p("Load IPD data from an external RDS file", class="small text-muted mb-2"),
                                         fileInput("load_ipd_rds_input", "Upload RDS File", accept = ".rds"),
                                         uiOutput("status_load_ipd_rds")
                                     ),

                                     div(class = "option-card",
                                         span("OPTION C", class="option-label option-label-c"),
                                         h6("Digitize from Figure"),
                                         p("Reconstruct IPD from published Kaplan-Meier plots", class="small text-muted mb-2"),
                                         tags$label(`for` = "risk_image", class = "upload-box d-block mb-3",
                                                    icon("table"), div("Upload 'Number at Risk' Table"),
                                                    uiOutput("risk_file_status"),
                                                    div(style="display:none;", fileInput("risk_image", NULL, accept = c("image/png", "image/jpeg")))
                                         ),
                                         tags$label(`for` = "km_image", class = "upload-box d-block mb-3",
                                                    icon("chart-line"), div("Upload Kaplan-Meier Plot"),
                                                    uiOutput("km_file_status"),
                                                    div(style="display:none;", fileInput("km_image", NULL, accept = c("image/png", "image/jpeg")))
                                         ),
                                         div(class="param-box mb-3 p-3 bg-light rounded border",
                                             layout_columns(col_widths = c(4, 4, 4),
                                                            numericInput("x_start", "Start", 0),
                                                            numericInput("x_end", "End", 0),
                                                            numericInput("x_increment", "Incr.", 0)),
                                             layout_columns(col_widths = c(4, 4, 4),
                                                            numericInput("y_start", "Y Start", 0),
                                                            numericInput("y_end", "Y End", 100),
                                                            numericInput("y_increment", "Y Incr.", 10)),
                                             checkboxInput("y_vertical", "Y-Text is Vertical?", value = TRUE)
                                         ),
                                         actionButton("run_analysis", "Process & Reconstruct", class="btn-warning action-btn w-100", icon = icon("gears"))
                                     )
                                   )
                                 ),
                                 div(class="result-container h-100",
                                     navset_card_tab(full_screen = TRUE, title = "Output results",
                                                     nav_panel("Plot",
                                                               div(class="d-flex justify-content-between align-items-center mb-3",
                                                                   h5("KM Preview", class="m-0"),
                                                                   div(downloadButton("dl_km_pdf", "PDF", class="btn-sm btn-outline-secondary pdf-download-btn me-2"),
                                                                       actionButton("switch_arms", "Switch Arms (Exp/Ctrl)", class="btn-warning btn-sm", icon=icon("exchange-alt")))
                                                               ),
                                                               plotOutput("final_plot", height="550px"),
                                                               hr(),
                                                               layout_columns(col_widths = c(6, 6),
                                                                              card(card_header("Summary"), verbatimTextOutput("survfit_summary")),
                                                                              card(card_header("Cox Model"), verbatimTextOutput("coxph_summary")))
                                                     ),
                                                     nav_panel("Data",
                                                               div(class="mb-3",
                                                                   downloadButton("download_excel", "Excel", class="btn-sm btn-success me-2"),
                                                                   downloadButton("download_ipd_rds", "RDS", class="btn-sm btn-info")),
                                                               tableOutput("ipd_preview")),
                                                     nav_panel("Code", verbatimTextOutput("repro_code"))
                                     )
                                 )
                  )
              )
    ),

    nav_panel("2. Model",
              div(class = "container-fluid py-4", style = "max-width: 1600px;",
                  div(class = "d-flex justify-content-end mb-3",
                      actionButton("reset_tab2", "Reset Tab", class = "btn-outline-secondary btn-sm reset-btn", icon = icon("refresh"))),
                  layout_sidebar(
                    sidebar = sidebar(title = "Model Configuration", class = "bg-light",
                                      div(class = "option-card",
                                          span("OPTION A", class="option-label option-label-a"),
                                          h6("Fit Model from Loaded Data"),
                                          p("Run Bayesian model using IPD from Tab 1", class="small text-muted mb-2"),
                                          numericInput("iter", "Iterations:", value = 2000, min = 500),
                                          numericInput("chains", "Chains:", value = 2, min = 1, max = 4),
                                          checkboxInput("shared_shape", "Shared Shape (Weibull)", value = TRUE),
                                          checkboxInput("use_prior", "Use Historical Prior", value = TRUE),
                                          conditionalPanel(condition = "input.use_prior == true",
                                                           sliderInput("prior_p1", "Prior Param 1 (Mean logOR):", min = 0, max = 1, value = 0.51),
                                                           sliderInput("prior_p2", "Prior Param 2 (SD logOR):", min = 0, max = 1, value = 0.27)),
                                          conditionalPanel(condition = "input.use_prior == false",
                                                           selectInput("cure_belief", "Cure Belief (Expert):", choices = c("unknown", "unlikely", "very_unlikely"), selected = "unknown")),
                                          actionButton("run_model", "Run Bayesian Fit", class="btn-danger action-btn w-100 mt-2", icon=icon("play"))
                                      ),
                                      hr(),
                                      div(class = "option-card",
                                          span("OPTION B", class="option-label option-label-b"),
                                          h6("Load Existing Model"),
                                          p("Upload a previously saved model RDS file", class="small text-muted mb-2"),
                                          fileInput("load_model_rds_input", "Select Model (.rds)", accept = ".rds"),
                                          uiOutput("status_model_loaded")
                                      ),
                                      hr(),
                                      selectInput("corr_method", "Correlation Method:", choices = c("pearson", "spearman", "kendall"), selected = "pearson"),
                                      downloadButton("save_model_rds", "Save Model RDS", class="btn-info w-100 mt-2")
                    ),
                    div(class="result-container",
                        navset_card_underline(
                          nav_panel("Stan Log",
                                    div(class="mb-2 d-flex align-items-center",
                                        uiOutput("stan_running_status"),
                                        span("MCMC Sampling Output", style="font-weight: 600; color: #1a2942;"),
                                        actionButton("clear_stan_log", "Clear Log", class="btn-outline-secondary btn-sm ms-3", icon=icon("trash"))),
                                    div(class="stan-log-container", id="stan_log_box", uiOutput("stan_log_output"))),
                          nav_panel("Model Summary", verbatimTextOutput("outcomes_text")),
                          nav_panel("Posterior Densities",
                                    div(class="mb-2", downloadButton("dl_densities_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                    plotOutput("plot_densities_out", height="550px")),
                          nav_panel("MCMC Traceplots",
                                    div(class="mb-2", downloadButton("dl_traceplots_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                    plotOutput("plot_mcmc_diagnostics", height="550px")),
                          nav_panel("Stan Diagnostics", verbatimTextOutput("diagnose_fit_text")),
                          nav_panel("Correlated Densities",
                                    div(class="mb-2", downloadButton("dl_corr_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                    plotOutput("corr_plot", height="550px")),
                          nav_panel("Reproducible Code", verbatimTextOutput("code_tab2"))
                        )
                    )
                  )
              )
    ),

    nav_panel("3. Safety (Tox & QoL)",
              div(class = "container-fluid py-4",
                  div(class = "d-flex justify-content-end mb-3",
                      actionButton("reset_tab3", "Reset Tab", class = "btn-outline-secondary btn-sm reset-btn", icon = icon("refresh"))),
                  layout_columns(col_widths = c(8, 4),
                                 card(h5("Adverse Events Data Grid", class="section-title"),
                                      div(class="p-3",
                                          layout_columns(col_widths = c(4, 4, 4),
                                                         div(class = "option-card",
                                                             span("OPTION A", class="option-label option-label-a"),
                                                             h6("Library"),
                                                             selectInput("pkg_tox_select", "Select Dataset:", choices = setNames(tox_datasets, toupper(tox_datasets))),
                                                             actionButton("load_pkg_tox", "Load Library", class="btn-primary btn-sm"),
                                                             uiOutput("status_load_tox")),
                                                         div(class = "option-card",
                                                             span("OPTION B", class="option-label option-label-b"),
                                                             h6("Load RDS"),
                                                             fileInput("load_tox_rds_input", "Upload RDS", accept = ".rds"),
                                                             uiOutput("status_load_tox_rds")),
                                                         div(class = "option-card",
                                                             span("OPTION C", class="option-label option-label-c"),
                                                             h6("Manual"),
                                                             checkboxInput("show_manual_grid", "Show Manual Input Grid", value = FALSE),
                                                             p("Toggle to enter data manually", class="small text-muted"))
                                          ),
                                          hr(),
                                          conditionalPanel(
                                            condition = "input.show_manual_grid == true",
                                            fluidRow(column(6, numericInput("N_exp_manual", "Total Exp (N):", 0)), column(6, numericInput("N_ctrl_manual", "Total Ctrl (N):", 0))),
                                            div(style = "max-height: 500px; overflow-y: auto; margin-top:10px;",
                                                lapply(toxicity_master_list, function(tox) {
                                                  id_safe <- gsub(" ", "_", tox)
                                                  div(class = "tox-row",
                                                      fluidRow(
                                                        column(4, tags$p(tox, style="font-weight:700; margin:0;")),
                                                        column(2, numericInput(paste0("n_any_exp_", id_safe), "Any E", NA)),
                                                        column(2, numericInput(paste0("n_g34_exp_", id_safe), "G34 E", NA)),
                                                        column(2, numericInput(paste0("n_any_ctrl_", id_safe), "Any C", NA)),
                                                        column(2, numericInput(paste0("n_g34_ctrl_", id_safe), "G34 C", NA))
                                                      )
                                                  )
                                                })
                                            )
                                          )
                                      )
                                 ),
                                 card(h5("QoL & Mixing Control", class="section-title"),
                                      div(class="p-3",
                                          selectInput("qol_scenario", "Scenario (Expected Outcome):", choices = c("1: Significant Improvement (+2)"=1, "2: Stabilization (0)"=2, "3: No Difference (0)"=3, "4: Deterioration (-1/-2)"=4, "5: Insufficient Data"=5), selected = 2),
                                          selectInput("qol_strength", "Strength of Evidence:", choices = c("1: Very Low"=1, "2: Low"=2, "3: Moderate"=3, "4: High"=4, "5: Very High"=5), selected = 3),
                                          hr(),
                                          actionButton("process_tox_manual", "Process Safety Data", class="btn-success action-btn w-100"),
                                          downloadButton("save_tox_rds", "Save Safety RDS", class="btn-info w-100 mt-2"),
                                          hr(),
                                          div(class="result-container",
                                              navset_card_underline(
                                                nav_panel("QoL Plot",
                                                          div(class="mb-2", downloadButton("dl_qol_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                                          plotOutput("plot_qol_hist", height="180px")),
                                                nav_panel("Structure", verbatimTextOutput("tox_object_status")),
                                                nav_panel("Reproducible Code", verbatimTextOutput("code_tab3"))
                                              )
                                          )
                                      )
                                 )
                  )
              )
    ),

    nav_panel("4. Final Bayescore",
              div(class = "container-fluid py-4",
                  div(class = "d-flex justify-content-end mb-3",
                      actionButton("reset_tab4", "Reset Tab", class = "btn-outline-secondary btn-sm reset-btn", icon = icon("refresh"))),
                  layout_sidebar(
                    sidebar = sidebar(title = "Synthesis Calibration", width = 350,
                                      textInput("trial_name_input", "Trial Name:", value = "", placeholder = "e.g., MONARCH-3"),
                                      hr(),
                                      h6("Efficacy Calibration", style="font-weight:600; color:#1a2942;"),
                                      numericInput("util_cure_effect", "Cure Target (Eff):", 0.20),
                                      numericInput("util_cure_val", "Cure Utility:", 75),
                                      numericInput("util_tr_effect", "TR Target (Eff):", 1.25),
                                      numericInput("util_tr_val", "TR Utility:", 50),
                                      hr(),
                                      h6("SOC Toxicity Weights", style="font-weight:600; color:#1a2942;"),
                                      div(class="soc-weight-input",
                                          numericInput("soc_w_gastro", "Gastrointestinal:", value = 1.2, min = 0, max = 5, step = 0.1),
                                          numericInput("soc_w_blood", "Blood & Lymphatic:", value = 1.6, min = 0, max = 5, step = 0.1),
                                          numericInput("soc_w_general", "General/Metabolic:", value = 1.3, min = 0, max = 5, step = 0.1),
                                          numericInput("soc_w_derm", "Dermatologic:", value = 1.1, min = 0, max = 5, step = 0.1),
                                          numericInput("soc_w_infect", "Infections:", value = 1.6, min = 0, max = 5, step = 0.1),
                                          numericInput("soc_w_resp", "Respiratory:", value = 2.5, min = 0, max = 5, step = 0.1)
                                      ),
                                      hr(),
                                      actionButton("calc_final", "Calculate Bayescore", class="btn-success action-btn w-100", icon=icon("calculator"))
                    ),
                    div(class="result-container",
                        navset_card_underline(
                          nav_panel("Component Summary", verbatimTextOutput("final_summary_text")),
                          nav_panel("Posterior Utility Density",
                                    div(class="mb-2", downloadButton("dl_final_density_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                    plotOutput("plot_final_density", height="600px")),
                          nav_panel("Relative Contribution",
                                    div(class="mb-2", downloadButton("dl_final_donut_pdf", "Download PDF", class="btn-sm btn-outline-secondary pdf-download-btn")),
                                    plotOutput("plot_final_donut", height="600px")),
                          nav_panel("Full Pipeline Reproducible Code", verbatimTextOutput("code_tab4"))
                        )
                    )
                  )
              )
    )
  )

  # --- SERVER ---
  server <- function(input, output, session) {
    vals <- reactiveValues(
      final_ipd = NULL, nrisk_df = NULL, model_fit_obj = NULL, tox_trial_obj = NULL,
      final_scores_obj = NULL, plot_obj = NULL, fit_obj = NULL, cox_obj = NULL,
      code_tab2 = NULL, generated_code = NULL, code_tab3 = NULL, code_tab4 = NULL,
      tox_loaded_from_lib = FALSE, tox_loaded_from_rds = FALSE, ipd_loaded_from_rds = FALSE,
      stan_log = NULL, stan_running = FALSE, stan_log_file = NULL
    )

    # RESET BUTTONS
    observeEvent(input$reset_tab1, {
      vals$final_ipd <- NULL; vals$nrisk_df <- NULL; vals$plot_obj <- NULL; vals$fit_obj <- NULL; vals$cox_obj <- NULL; vals$generated_code <- NULL; vals$ipd_loaded_from_rds <- FALSE
      showNotification("Tab 1 Reset", type = "message")
    })
    observeEvent(input$reset_tab2, {
      vals$model_fit_obj <- NULL; vals$code_tab2 <- NULL; vals$stan_log <- NULL; vals$stan_running <- FALSE
      showNotification("Tab 2 Reset", type = "message")
    })
    observeEvent(input$reset_tab3, {
      vals$tox_trial_obj <- NULL; vals$code_tab3 <- NULL; vals$tox_loaded_from_lib <- FALSE; vals$tox_loaded_from_rds <- FALSE
      updateNumericInput(session, "N_exp_manual", value = 0); updateNumericInput(session, "N_ctrl_manual", value = 0)
      showNotification("Tab 3 Reset", type = "message")
    })
    observeEvent(input$reset_tab4, {
      vals$final_scores_obj <- NULL; vals$code_tab4 <- NULL
      showNotification("Tab 4 Reset", type = "message")
    })

    observeEvent(input$clear_stan_log, {
      vals$stan_log <- NULL
    })

    # Switch Arms functionality
    observeEvent(input$switch_arms, {
      req(vals$final_ipd)
      # Get unique arm levels
      arm_levels <- unique(vals$final_ipd$arm)
      if(length(arm_levels) == 2) {
        # Create mapping to swap arms
        vals$final_ipd$arm <- ifelse(vals$final_ipd$arm == arm_levels[1], arm_levels[2], arm_levels[1])
        # Regenerate plot and models
        fit <- survfit(Surv(time, status) ~ arm, data = vals$final_ipd)
        vals$plot_obj <- ggsurvplot(fit, data = vals$final_ipd, palette = "jco")$plot
        vals$fit_obj <- fit
        vals$cox_obj <- survival::coxph(Surv(time, status) ~ arm, data = vals$final_ipd)
        showNotification("Arms switched successfully", type = "message")
      }
    })

    # Status Indicators
    output$status_load_eff <- renderUI({ req(vals$final_ipd); span(icon("check-circle"), " Data Loaded", class="text-success small") })
    output$status_model_loaded <- renderUI({ req(vals$model_fit_obj); span(icon("check-circle"), " Model Ready", class="text-success small") })
    output$risk_file_status <- renderUI({ req(input$risk_image); div(class="upload-success", icon("check-circle"), span(paste(" Loaded:", input$risk_image$name))) })
    output$km_file_status <- renderUI({ req(input$km_image); div(class="upload-success", icon("check-circle"), span(paste(" Loaded:", input$km_image$name))) })
    output$status_load_tox <- renderUI({ req(vals$tox_loaded_from_lib); div(class="library-loaded-indicator", icon("check-circle"), " Library Data Loaded") })
    output$status_load_tox_rds <- renderUI({ req(vals$tox_loaded_from_rds); div(class="library-loaded-indicator", icon("check-circle"), " RDS Data Loaded") })
    output$status_load_ipd_rds <- renderUI({ req(vals$ipd_loaded_from_rds); div(class="library-loaded-indicator", icon("check-circle"), " IPD Data Loaded from RDS") })

    # Stan Running Status Indicator
    output$stan_running_status <- renderUI({
      if(vals$stan_running) {
        span(class="stan-running-indicator")
      } else {
        NULL
      }
    })

    # Stan Log Output with auto-refresh
    output$stan_log_output <- renderUI({
      # Auto-invalidate every 3 seconds while running
      if(vals$stan_running) {
        invalidateLater(3000, session)
      }

      # Read log file if exists
      if(!is.null(vals$stan_log_file) && file.exists(vals$stan_log_file)) {
        log_content <- tryCatch({
          paste(readLines(vals$stan_log_file, warn = FALSE), collapse = "\n")
        }, error = function(e) "")

        if(nchar(log_content) > 0) {
          # Format the log with colors
          log_formatted <- log_content
          log_formatted <- gsub("(Chain \\d+:)", "<span style='color:#63b3ed;'>\\1</span>", log_formatted)
          log_formatted <- gsub("(Iteration:\\s*\\d+\\s*/\\s*\\d+)", "<span style='color:#68d391;'>\\1</span>", log_formatted)
          log_formatted <- gsub("(SAMPLING FOR MODEL)", "<span style='color:#faf089;'>\\1</span>", log_formatted)
          log_formatted <- gsub("(Warmup)", "<span style='color:#fc8181;'>\\1</span>", log_formatted)
          log_formatted <- gsub("(Sampling)", "<span style='color:#68d391;'>\\1</span>", log_formatted)
          log_formatted <- gsub("(\\[\\s*\\d+%\\])", "<span style='color:#9ae6b4;'>\\1</span>", log_formatted)

          vals$stan_log <- log_formatted
        }
      }

      if(is.null(vals$stan_log) || vals$stan_log == "") {
        tags$span("No MCMC output yet. Run the Bayesian fit to see Stan sampling progress.", style="color: #a0aec0;")
      } else {
        # Add JS to scroll to bottom
        tagList(
          tags$pre(HTML(vals$stan_log), style="margin: 0;"),
          tags$script(HTML("
            var logBox = document.getElementById('stan_log_box');
            if(logBox) logBox.scrollTop = logBox.scrollHeight;
          "))
        )
      }
    })

    # 1. DATA LOADING - OPTION B: External RDS
    observeEvent(input$load_ipd_rds_input, {
      req(input$load_ipd_rds_input)
      tryCatch({
        vals$final_ipd <- readRDS(input$load_ipd_rds_input$datapath)
        vals$ipd_loaded_from_rds <- TRUE
        fit <- survfit(Surv(time, status) ~ arm, data = vals$final_ipd)
        vals$plot_obj <- ggsurvplot(fit, data = vals$final_ipd, palette = "jco")$plot
        vals$fit_obj <- fit
        vals$cox_obj <- survival::coxph(Surv(time, status) ~ arm, data = vals$final_ipd)
        showNotification("IPD Data Loaded from RDS", type = "message")
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    # 1. EFFICACY RECONSTRUCTION
    observeEvent(input$risk_image, {
      req(input$risk_image)
      img <- image_read(input$risk_image$datapath) %>% image_convert(type = 'Grayscale') %>% image_modulate(brightness = 120) %>% image_contrast(sharpen = 2)
      text_raw <- ocr(img, engine = tesseract("eng"))
      lines <- str_trim(str_split(text_raw, "\n")[[1]]); lines <- lines[lines != ""]
      number_lines <- lines[grep("[0-9]", lines)]
      if(length(number_lines) < 2) return()
      get_nums <- function(s) as.numeric(unlist(str_extract_all(s, "\\d+")))
      times <- get_nums(number_lines[1])
      if(length(times) > 1) {
        updateNumericInput(session, "x_start", value = min(times)); updateNumericInput(session, "x_end", value = max(times))
        diffs <- diff(times); common_diff <- as.numeric(names(sort(table(diffs), decreasing=TRUE)[1]))
        updateNumericInput(session, "x_increment", value = common_diff)
      }
      df_list <- list()
      for(i in 2:length(number_lines)) {
        nums <- get_nums(number_lines[i]); len <- min(length(times), length(nums))
        if(len > 0) df_list[[i-1]] <- data.frame(time_tick = times[1:len], nrisk = nums[1:len], curve = as.integer(i-1))
      }
      vals$nrisk_df <- do.call(rbind, df_list)
    })

    observeEvent(input$run_analysis, {
      req(input$km_image, vals$nrisk_df)
      showNotification("Running Reconstruction...", type = "message", id = "process_notif")
      on.exit(removeNotification(id = "process_notif"))
      tryCatch({
        plot_km_cm <- SurvdigitizeR::survival_digitize(img_path = input$km_image$datapath, num_curves = max(vals$nrisk_df$curve), x_start = input$x_start, x_end = input$x_end, x_increment = input$x_increment, y_start = input$y_start, y_end = input$y_end, y_increment = input$y_increment, y_text_vertical = input$y_vertical)
        nrisk_tbl_all <- vals$nrisk_df; curves <- unique(nrisk_tbl_all$curve); res_list <- list()
        for(c_id in curves) {
          km_sub <- subset(plot_km_cm, curve == c_id); risk_sub <- subset(nrisk_tbl_all, curve == c_id)
          res <- bayescores::reconstruct_ipd(plot_km = km_sub, nrisk_tbl = risk_sub)
          res_list[[c_id]] <- res$ipd %>% mutate(arm = paste("Group", c_id))
        }
        vals$final_ipd <- do.call(rbind, res_list)
        fit <- survfit(Surv(time, status) ~ arm, data = vals$final_ipd)
        vals$plot_obj <- ggsurvplot(fit, data = vals$final_ipd, palette = "jco", xlim = c(input$x_start, input$x_end))$plot
        vals$fit_obj <- fit
        vals$cox_obj <- survival::coxph(Surv(time, status) ~ arm, data = vals$final_ipd)

        # GENERATE REPRO CODE TAB 1
        risk_df_str <- paste0("structure(list(",
                              paste(names(vals$nrisk_df), lapply(vals$nrisk_df, function(x) paste0("c(", paste(x, collapse=","), ")")), sep="=", collapse=", "),
                              "), class='data.frame', row.names=c(NA, -", nrow(vals$nrisk_df), "L))")

        vals$generated_code <- paste0(
          "library(ggplot2)\nlibrary(dplyr)\nlibrary(survival)\nlibrary(survminer)\nlibrary(SurvdigitizeR)\nlibrary(bayescores)\n\n",
          "# --- 1. SETUP DATA ---\nimg_path <- '", input$km_image$name, "'\n\n",
          "nrisk_tbl_all <- ", risk_df_str, "\n\n",
          "# --- 2. DIGITIZE KM CURVE ---\nplot_km_digitized <- survival_digitize(\n  img_path = img_path,\n  num_curves = max(nrisk_tbl_all$curve),\n  x_start = ", input$x_start, ", x_end = ", input$x_end, ", x_increment = ", input$x_increment, ",\n  y_start = ", input$y_start, ", y_end = ", input$y_end, ", y_increment = ", input$y_increment, ",\n  y_text_vertical = ", input$y_vertical, "\n)\n\n",
          "# --- 3. RECONSTRUCT IPD ---\ncurves <- unique(nrisk_tbl_all$curve)\nipd_list <- list()\n\nfor(c_id in curves) {\n  km_sub <- subset(plot_km_digitized, curve == c_id)\n  risk_sub <- subset(nrisk_tbl_all, curve == c_id)\n  res_arm <- reconstruct_ipd(plot_km = km_sub, nrisk_tbl = risk_sub)\n  ipd_list[[c_id]] <- res_arm$ipd %>% mutate(curve = c_id)\n}\n\n",
          "final_ipd <- bind_rows(ipd_list) %>% mutate(curve = factor(curve, labels = paste('Group', curves)))\n\n",
          "# --- 4. VALIDATION ---\nfit <- survfit(Surv(time, status) ~ curve, data = final_ipd)\nggsurvplot(fit, data = final_ipd, palette = 'jco', risk.table = TRUE)\nsummary(survival::coxph(Surv(time, status) ~ curve, data = final_ipd))"
        )
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    # 2. MODEL LOGIC
    observeEvent(input$load_model_rds_input, {
      req(input$load_model_rds_input)
      vals$model_fit_obj <- readRDS(input$load_model_rds_input$datapath)
      showNotification("Model Loaded Successfully", type = "message")
    })

    observeEvent(input$run_model, {
      req(vals$final_ipd)

      # Create temp file for Stan output
      stan_log_file <- tempfile(fileext = ".txt")
      vals$stan_log_file <- stan_log_file
      vals$stan_running <- TRUE

      # Initialize log file with header
      initial_log <- paste0(
        format(Sys.time(), "%H:%M:%S"), " Starting MCMC sampling...\n",
        "Iterations: ", input$iter, " | Chains: ", input$chains, "\n",
        "-------------------------------------------\n\n",
        "NOTE: Detailed sampling progress is displayed in the R console.\n",
        "You can continue working in other tabs and return here later.\n",
        "This panel will update automatically when sampling completes.\n\n",
        "-------------------------------------------\n"
      )
      writeLines(initial_log, stan_log_file)
      vals$stan_log <- initial_log

      # Store parameters for use in future
      iter_val <- input$iter
      chains_val <- input$chains
      shared_shape_val <- input$shared_shape
      use_prior_val <- input$use_prior
      prior_p1_val <- input$prior_p1
      prior_p2_val <- input$prior_p2
      cure_belief_val <- input$cure_belief
      ipd_data <- vals$final_ipd
      log_file_path <- stan_log_file

      showNotification("MCMC sampling started. Check 'Stan Log' tab for progress...", type = "message", duration = 5)

      # Run model asynchronously using promises pipe operators
      model_future <- future::future({
        # Open file connection for appending
        log_con <- file(log_file_path, open = "at")

        tryCatch({
          # Capture output using withCallingHandlers
          result <- withCallingHandlers({
            if(use_prior_val) {
              fit_bayesian_cure_model(
                data = ipd_data,
                time_col = "time",
                event_col = "status",
                arm_col = "arm",
                iter = iter_val,
                chains = chains_val,
                shared_shape = shared_shape_val,
                use_historical_prior = TRUE,
                historical_prior_params = c(prior_p1_val, prior_p2_val)
              )
            } else {
              fit_bayesian_cure_model(
                data = ipd_data,
                time_col = "time",
                event_col = "status",
                arm_col = "arm",
                iter = iter_val,
                chains = chains_val,
                shared_shape = shared_shape_val,
                use_historical_prior = FALSE,
                cure_belief = cure_belief_val
              )
            }
          }, message = function(m) {
            cat(m$message, file = log_con, append = TRUE)
          })

          cat("\n-------------------------------------------\n", file = log_con, append = TRUE)
          cat(format(Sys.time(), "%H:%M:%S"), " MCMC sampling completed!\n", file = log_con, append = TRUE)

          result
        }, finally = {
          close(log_con)
        })
      }, seed = TRUE)

      # Handle promise resolution
      promises::then(model_future,
                     onFulfilled = function(result) {
                       vals$model_fit_obj <- result
                       vals$stan_running <- FALSE

                       vals$code_tab2 <- paste0(
                         "library(bayescores)\nlibrary(rstan)\noptions(mc.cores = parallel::detectCores())\nrstan::rstan_options(auto_write = TRUE)\n\n",
                         "fit_model <- fit_bayesian_cure_model(\n  data = final_ipd,\n  time_col = 'time', event_col = 'status', arm_col = 'curve',\n",
                         "  iter = ", iter_val, ", chains = ", chains_val, ", shared_shape = ", shared_shape_val, ",\n",
                         if(use_prior_val) paste0("  use_historical_prior = TRUE, historical_prior_params = c(", prior_p1_val, ",", prior_p2_val, ")\n") else paste0("  use_historical_prior = FALSE, cure_belief = '", cure_belief_val, "'\n"),
                         ")\n\nprint(outcomes(fit_model))\nplot_densities(fit_model)"
                       )

                       showNotification("Bayesian model fitting completed!", type = "message")
                     },
                     onRejected = function(error) {
                       vals$stan_running <- FALSE
                       showNotification(paste("Error:", error$message), type = "error")
                     }
      )

      # Return NULL to not block
      NULL
    })

    # 3. SAFETY LOGIC
    observeEvent(input$load_pkg_tox, {
      req(input$pkg_tox_select)
      tryCatch({
        data(list = input$pkg_tox_select, package = "bystools", envir = environment())
        vals$tox_trial_obj <- get(input$pkg_tox_select, envir = environment())
        vals$tox_loaded_from_lib <- TRUE
        vals$tox_loaded_from_rds <- FALSE
        if(!is.null(vals$tox_trial_obj$N_patients)) {
          updateNumericInput(session, "N_exp_manual", value = vals$tox_trial_obj$N_patients["Experimental"])
          updateNumericInput(session, "N_ctrl_manual", value = vals$tox_trial_obj$N_patients["Control"])
        }
        showNotification("Library Data Loaded Successfully", type = "message")
      }, error = function(e) showNotification(e$message, type = "error"))
    })

    observeEvent(input$load_tox_rds_input, {
      req(input$load_tox_rds_input)
      vals$tox_trial_obj <- readRDS(input$load_tox_rds_input$datapath)
      vals$tox_loaded_from_rds <- TRUE
      vals$tox_loaded_from_lib <- FALSE
      updateNumericInput(session, "N_exp_manual", value = vals$tox_trial_obj$N_patients["Experimental"])
      updateNumericInput(session, "N_ctrl_manual", value = vals$tox_trial_obj$N_patients["Control"])
      showNotification("Safety RDS Loaded", type = "message")
    })

    observeEvent(input$process_tox_manual, {
      req(input$N_exp_manual > 0, input$N_ctrl_manual > 0)
      results <- lapply(toxicity_master_list, function(tox) {
        id_safe <- gsub(" ", "_", tox)
        na_e <- input[[paste0("n_any_exp_", id_safe)]]; ng_e <- input[[paste0("n_g34_exp_", id_safe)]]
        na_c <- input[[paste0("n_any_ctrl_", id_safe)]]; ng_c <- input[[paste0("n_g34_ctrl_", id_safe)]]
        if(!all(is.na(c(na_e, ng_e, na_c, ng_c)))) {
          data.frame(EventName = tox, SystemOrganClass = get_system_organ_class(tox),
                     Exp_Any_n = na_e, Exp_G34_n = ng_e,
                     Ctrl_Any_n = na_c, Ctrl_G34_n = ng_c)
        }
      })
      scenario <- as.numeric(input$qol_scenario)
      # QoL probabilities: P(-2), P(-1), P(0), P(+1), P(+2)
      # Scenario 1: Significant Improvement -> high probability on +2, +1
      # Scenario 2: Stabilization -> centered on 0
      # Scenario 3: No Difference -> centered on 0
      # Scenario 4: Deterioration -> high probability on -2, -1
      # Scenario 5: Insufficient Data -> uniform
      base_prob <- switch(scenario,
                          c(0, 0, 0.1, 0.2, 0.7),       # 1: Significant Improvement (+2)
                          c(0, 0.1, 0.7, 0.1, 0.1),     # 2: Stabilization (0)
                          c(0.05, 0.15, 0.6, 0.15, 0.05), # 3: No Difference (0)
                          c(0.6, 0.2, 0.1, 0.1, 0),     # 4: Deterioration (-2/-1)
                          rep(0.2, 5)                    # 5: Insufficient Data (uniform)
      )
      names(base_prob) <- c("P(-2)", "P(-1)", "P(0)", "P(+1)", "P(+2)")
      vals$tox_trial_obj <- list(toxicity = bind_rows(results), N_patients = c(Experimental = input$N_exp_manual, Control = input$N_ctrl_manual), qol = base_prob)
      showNotification("Safety Object Updated", type = "message")
    })

    # GENERATE REPRODUCIBLE CODE TAB 3
    observe({
      req(vals$tox_trial_obj)
      tox_df <- vals$tox_trial_obj$toxicity
      n_exp <- vals$tox_trial_obj$N_patients["Experimental"]
      n_ctrl <- vals$tox_trial_obj$N_patients["Control"]
      qol_vec <- vals$tox_trial_obj$qol

      tox_df_str <- paste0(capture.output(dput(tox_df)), collapse = "\n")

      vals$code_tab3 <- paste0(
        "# --- REPRODUCIBLE SAFETY DATA (TAB 3) ---\n",
        "library(dplyr)\n\n",
        "# 1. Structure exactly as required\n",
        "toxicity_trial <- list(\n",
        "  toxicity = ", tox_df_str, ",\n",
        "  N_patients = c(Experimental = ", n_exp, ", Control = ", n_ctrl, "),\n",
        "  qol = c(`P(-2)` = ", qol_vec[1], ", `P(-1)` = ", qol_vec[2], ", `P(0)` = ", qol_vec[3], ", `P(+1)` = ", qol_vec[4], ",\n",
        "          `P(+2)` = ", qol_vec[5], ")\n",
        ")\n\n",
        "# --- Ready for use in calculate_toxicity_analysis() ---"
      )
    })

    # 4. FINAL SYNTHESIS
    observeEvent(input$calc_final, {
      req(vals$model_fit_obj, vals$tox_trial_obj)
      efficacy_draws <- get_bayescores_draws(fit = vals$model_fit_obj, shrinkage_method = "none")

      # Build SOC weights from inputs
      soc_weights <- c(
        "Gastrointestinal disorders" = input$soc_w_gastro,
        "Blood and lymphatic system disorders" = input$soc_w_blood,
        "General, metabolic, and other disorders" = input$soc_w_general,
        "Dermatologic disorders" = input$soc_w_derm,
        "Infections and infestations" = input$soc_w_infect,
        "Respiratory, thoracic and mediastinal disorders" = input$soc_w_resp
      )

      tox_out <- calculate_toxicity_analysis(trial_data = vals$tox_trial_obj, n_simulations = 2000, soc_weights = soc_weights)

      my_calib <- list(efficacy = list(cure_utility_target = list(effect_value = input$util_cure_effect, utility_value = input$util_cure_val),
                                       tr_utility_target = list(effect_value = input$util_tr_effect, utility_value = input$util_tr_val)))

      vals$final_scores_obj <- get_bayescores(
        efficacy_inputs = efficacy_draws,
        qol_scores = sample_qol_scores(prob_vector = vals$tox_trial_obj$qol, n_samples = 2000),
        toxicity_scores = tox_out$toxicity_effect_vector,
        calibration_args = my_calib
      )

      # FULL PIPELINE REPRODUCIBLE CODE
      trial_name_code <- if(input$trial_name_input != "") paste0("'", input$trial_name_input, "'") else "NULL"
      vals$code_tab4 <- paste0(
        "library(dplyr)\nlibrary(bayescores)\n\n",
        "# 1. Safety Structure\ntoxicity_trial <- list(\n  toxicity = ", paste0(capture.output(dput(vals$tox_trial_obj$toxicity)), collapse="\n"), ",\n",
        "  N_patients = c(Experimental = ", input$N_exp_manual, ", Control = ", input$N_ctrl_manual, "),\n",
        "  qol = c('P(-2)'=", vals$tox_trial_obj$qol[1], ",'P(-1)'=", vals$tox_trial_obj$qol[2], ",'P(0)'=", vals$tox_trial_obj$qol[3], ",'P(+1)'=", vals$tox_trial_obj$qol[4], ",'P(+2)'=", vals$tox_trial_obj$qol[5], ")\n)\n\n",
        "# 2. SOC Weights\nsoc_weights <- c(\n",
        "  'Gastrointestinal disorders' = ", input$soc_w_gastro, ",\n",
        "  'Blood and lymphatic system disorders' = ", input$soc_w_blood, ",\n",
        "  'General, metabolic, and other disorders' = ", input$soc_w_general, ",\n",
        "  'Dermatologic disorders' = ", input$soc_w_derm, ",\n",
        "  'Infections and infestations' = ", input$soc_w_infect, ",\n",
        "  'Respiratory, thoracic and mediastinal disorders' = ", input$soc_w_resp, "\n)\n\n",
        "# 3. Calibration\nmy_calibration <- list(efficacy = list(\n  cure_utility_target = list(effect_value = ", input$util_cure_effect, ", utility_value = ", input$util_cure_val, "),\n",
        "  tr_utility_target = list(effect_value = ", input$util_tr_effect, ", utility_value = ", input$util_tr_val, ")\n))\n\n",
        "# 4. Analysis\ntox_out <- calculate_toxicity_analysis(trial_data = toxicity_trial, n_simulations = 2000, soc_weights = soc_weights)\n",
        "qol_draws <- sample_qol_scores(prob_vector = toxicity_trial$qol, n_samples = 2000)\n",
        "eff_draws <- get_bayescores_draws(fit = my_fitted_model, shrinkage_method = 'none')\n\n",
        "final_scores <- get_bayescores(efficacy_inputs = eff_draws, qol_scores = qol_draws, toxicity_scores = tox_out$toxicity_effect_vector, calibration_args = my_calibration)\n",
        "print(final_scores$component_summary)\nplot_utility_donut(final_scores, trial_name = ", trial_name_code, ")"
      )
    })

    # RENDERS
    output$final_plot <- renderPlot({ req(vals$plot_obj); vals$plot_obj })
    output$survfit_summary <- renderPrint({ req(vals$fit_obj); print(vals$fit_obj) })
    output$coxph_summary <- renderPrint({ req(vals$cox_obj); summary(vals$cox_obj) })
    output$ipd_preview <- renderTable({ req(vals$final_ipd); head(vals$final_ipd, 15) })
    output$repro_code <- renderText({ req(vals$generated_code); vals$generated_code })
    output$outcomes_text <- renderPrint({ req(vals$model_fit_obj); outcomes(vals$model_fit_obj) })
    output$plot_densities_out <- renderPlot({ req(vals$model_fit_obj); plot_densities(vals$model_fit_obj) })
    output$plot_mcmc_diagnostics <- renderPlot({ req(vals$model_fit_obj); model_diagnostics(vals$model_fit_obj) })
    output$diagnose_fit_text <- renderPrint({ req(vals$model_fit_obj); diagnose_fit(vals$model_fit_obj$stan_fit) })
    output$corr_plot <- renderPlot({ req(vals$model_fit_obj); plot_correlated_densities(vals$model_fit_obj, correlation_method = input$corr_method) })
    output$code_tab2 <- renderText({ req(vals$code_tab2); vals$code_tab2 })
    output$tox_object_status <- renderPrint({ req(vals$tox_trial_obj); str(vals$tox_trial_obj) })
    output$plot_qol_hist <- renderPlot({ req(vals$tox_trial_obj); graphics::barplot(vals$tox_trial_obj$qol, names.arg=names(vals$tox_trial_obj$qol), col="steelblue", main="QoL Distribution") })
    output$code_tab3 <- renderText({ req(vals$code_tab3); vals$code_tab3 })
    output$final_summary_text <- renderPrint({ req(vals$final_scores_obj); vals$final_scores_obj$component_summary })
    output$plot_final_density <- renderPlot({ req(vals$final_scores_obj); plot_final_utility_density(vals$final_scores_obj) })
    output$plot_final_donut <- renderPlot({
      req(vals$final_scores_obj)
      trial_name_val <- if(input$trial_name_input != "") input$trial_name_input else NULL
      plot_utility_donut(vals$final_scores_obj, trial_name = trial_name_val)
    })
    output$code_tab4 <- renderText({ req(vals$code_tab4); vals$code_tab4 })

    # PDF DOWNLOADS
    output$dl_km_pdf <- downloadHandler(
      filename = "km_plot.pdf",
      content = function(file) { pdf(file, width = 10, height = 7); print(vals$plot_obj); dev.off() }
    )
    output$dl_densities_pdf <- downloadHandler(
      filename = "posterior_densities.pdf",
      content = function(file) { pdf(file, width = 10, height = 7); print(plot_densities(vals$model_fit_obj)); dev.off() }
    )
    output$dl_traceplots_pdf <- downloadHandler(
      filename = "mcmc_traceplots.pdf",
      content = function(file) { pdf(file, width = 10, height = 7); print(model_diagnostics(vals$model_fit_obj)); dev.off() }
    )
    output$dl_corr_pdf <- downloadHandler(
      filename = "correlated_densities.pdf",
      content = function(file) { pdf(file, width = 10, height = 7); print(plot_correlated_densities(vals$model_fit_obj, correlation_method = input$corr_method)); dev.off() }
    )
    output$dl_qol_pdf <- downloadHandler(
      filename = "qol_distribution.pdf",
      content = function(file) { pdf(file, width = 8, height = 5); graphics::barplot(vals$tox_trial_obj$qol, names.arg=names(vals$tox_trial_obj$qol), col="steelblue", main="QoL Distribution"); dev.off() }
    )
    output$dl_final_density_pdf <- downloadHandler(
      filename = "final_utility_density.pdf",
      content = function(file) { pdf(file, width = 10, height = 7); print(plot_final_utility_density(vals$final_scores_obj)); dev.off() }
    )
    output$dl_final_donut_pdf <- downloadHandler(
      filename = "utility_donut.pdf",
      content = function(file) {
        trial_name_val <- if(input$trial_name_input != "") input$trial_name_input else NULL
        pdf(file, width = 10, height = 7); print(plot_utility_donut(vals$final_scores_obj, trial_name = trial_name_val)); dev.off()
      }
    )

    # OTHER DOWNLOADS
    output$download_excel <- downloadHandler(filename = "ipd_reconstructed.xlsx", content = function(f) writexl::write_xlsx(vals$final_ipd, f))
    output$download_ipd_rds <- downloadHandler(filename = "ipd_data.rds", content = function(f) saveRDS(vals$final_ipd, f))
    output$save_model_rds <- downloadHandler(filename = "bayesian_fit.rds", content = function(f) saveRDS(vals$model_fit_obj, f))
    output$save_tox_rds <- downloadHandler(filename = "safety_data.rds", content = function(f) saveRDS(vals$tox_trial_obj, f))
  }

  shinyApp(ui, server)
}
