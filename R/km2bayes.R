#' Interactive Tool for Kaplan-Meier Digitization and Bayesian Modeling
#'
#' @description
#' Launches a Shiny application that facilitates the extraction of individual patient data (IPD)
#' from Kaplan-Meier images and fits Bayesian cure models using the `bayescores` algorithm.
#'
#' The tool relies on manual digitization via `SurvdigitizeR` OR automated extraction via Google Opal (Experimental),
#' and allows for:
#' \itemize{
#'   \item Image pre-processing (brightness, contrast, borders) to improve visibility.
#'   \item Manual point extraction (point-and-click).
#'   \item Google Opal vector import.
#'   \item Interactive risk table editing.
#'   \item Bayesian model fitting (standard or historical priors).
#'   \item Comprehensive diagnostics and plotting (densities, survival curves, MCMC draws).
#' }
#'
#' @return A Shiny application object.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(survival, except = c(cluster))
#' @rawNamespace import(future, except = c(cluster))
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
#' @import rstan
#' @import writexl
#' @import rhandsontable
#' @importFrom here here
#' @importFrom utils write.csv head globalVariables
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par rect
#' @importFrom stats median coef
#'
#' @export
km2bayes <- function() {

  # CSS Styles
  css_blue_metal <- "
  :root{
    --bm-bg: #F4F7FC;
    --bm-fg: #0B1F3A;
    --bm-muted: #5A6C80;

    --bm-primary: #1E5AA8;
    --bm-primary-2: #2B74C7;
    --bm-accent: #8FB3D9;

    --bm-card: #FFFFFF;
    --bm-card-2: #F2F6FD;
    --bm-border: #D7E1EE;

    --bm-shadow: 0 10px 28px rgba(11,31,58,0.08);
    --bm-shadow-soft: 0 6px 18px rgba(11,31,58,0.06);
    --bm-radius: 16px;
  }

  body{
    background: radial-gradient(900px 600px at 20% 0%, rgba(143,179,217,0.25) 0%, rgba(244,247,252,0) 55%),
                radial-gradient(900px 600px at 90% 20%, rgba(30,90,168,0.15) 0%, rgba(244,247,252,0) 55%),
                var(--bm-bg);
    color: var(--bm-fg);
  }

  /* Sidebar (bslib page_sidebar) */
  .bslib-sidebar-layout > .sidebar,
  .sidebar{
    background: linear-gradient(180deg, #0D1E34 0%, #102845 55%, #0D1E34 100%);
    border-right: 1px solid rgba(255,255,255,0.08);
    box-shadow: 10px 0 30px rgba(11,31,58,0.12);
  }
  .bslib-sidebar-layout > .sidebar *,
  .sidebar *{
    color: #EAF2FF;
  }
  .bslib-sidebar-layout > .sidebar .text-primary,
  .sidebar .text-primary{
    color: #BFD6F3 !important;
  }
  .bslib-sidebar-layout > .sidebar hr,
  .sidebar hr{
    border-color: rgba(234,242,255,0.12);
    opacity: 1;
  }

  /* Inputs sidebar */
  .bslib-sidebar-layout > .sidebar .form-control,
  .bslib-sidebar-layout > .sidebar .form-select,
  .sidebar .form-control,
  .sidebar .form-select{
    background: rgba(255,255,255,0.07);
    border: 1px solid rgba(255,255,255,0.14);
    color: #EAF2FF;
  }
  .bslib-sidebar-layout > .sidebar .form-control:focus,
  .bslib-sidebar-layout > .sidebar .form-select:focus,
  .sidebar .form-control:focus,
  .sidebar .form-select:focus{
    box-shadow: 0 0 0 .2rem rgba(143,179,217,0.25);
    border-color: rgba(143,179,217,0.60);
  }
  .bslib-sidebar-layout > .sidebar label,
  .sidebar label{
    color: rgba(234,242,255,0.90);
  }

  /* Buttons: metal blue */
  .btn-primary{
    background: linear-gradient(180deg, var(--bm-primary) 0%, var(--bm-primary-2) 100%);
    border: 1px solid rgba(255,255,255,0.15);
    box-shadow: var(--bm-shadow-soft);
  }
  .btn-primary:hover{ filter: brightness(1.03); }

  .btn-outline-danger{
    border-color: rgba(255,255,255,0.30) !important;
    color: rgba(255,255,255,0.95) !important;
  }
  .btn-outline-danger:hover{
    background: rgba(220,53,69,0.18) !important;
    border-color: rgba(255,255,255,0.35) !important;
  }

  /* NEW: Opal Button Style */
  .btn-opal {
    background: linear-gradient(180deg, #5e35b1 0%, #7e57c2 100%);
    border: 1px solid rgba(255,255,255,0.15);
    color: white !important;
    box-shadow: var(--bm-shadow-soft);
  }
  .btn-opal:hover { filter: brightness(1.1); }

  .btn-success, .btn-info, .btn-secondary, .btn-warning{
    border: 1px solid rgba(255,255,255,0.14);
    box-shadow: var(--bm-shadow-soft);
  }

  /* Tabs underline */
  .nav-underline{
    border-bottom: 1px solid var(--bm-border);
    margin-bottom: 14px;
  }
  .nav-underline .nav-link{
    color: var(--bm-muted);
    font-weight: 600;
    padding: 10px 14px;
  }
  .nav-underline .nav-link.active{
    color: var(--bm-primary);
    border-bottom-color: var(--bm-primary);
  }

  /* Cards */
  .card{
    border-radius: var(--bm-radius);
    border: 1px solid var(--bm-border);
    background: var(--bm-card);
    box-shadow: var(--bm-shadow);
    overflow: hidden;
  }
  .card-header{
    background: linear-gradient(180deg, var(--bm-card-2) 0%, #FFFFFF 100%);
    border-bottom: 1px solid var(--bm-border);
    font-weight: 700;
    color: var(--bm-fg);
  }
  .card-footer{
    background: linear-gradient(180deg, #FFFFFF 0%, var(--bm-card-2) 100%);
    border-top: 1px solid var(--bm-border);
  }

  /* Imagen */
  #original_image_output img{
    width: 100%;
    max-height: 420px;
    object-fit: contain;
    border-radius: 12px;
    border: 1px solid var(--bm-border);
    background: #FFFFFF;
  }

  /* Compact spacing */
  .container-fluid{
    padding-left: 10px;
    padding-right: 10px;
  }

  /* Accordion transparency for Sidebar */
  .accordion-item {
    background-color: rgba(255, 255, 255, 0.05);
    border: 1px solid rgba(255, 255, 255, 0.1);
  }
  .accordion-button {
    background-color: rgba(255, 255, 255, 0.1);
    color: #EAF2FF;
  }
  .accordion-button:not(.collapsed) {
    background-color: rgba(30, 90, 168, 0.4);
    color: #FFFFFF;
  }
  .accordion-body {
    background-color: transparent;
  }


/* FIX: text visible in selectInput (cure_belief) inside sidebar */
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
  "

# --- UI Definition ---
ui <- shiny::tagList(
  shiny::tags$head(shiny::tags$style(shiny::HTML(css_blue_metal))),

  bslib::page_sidebar(
    title = "BayesCores Support Tool",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      bg = "#F4F7FC",
      fg = "#0B1F3A",
      primary = "#1E5AA8",
      secondary = "#5A6C80",
      success = "#2E7D6A",
      info = "#2B74C7",
      danger = "#C0392B",
      base_font = "Segoe UI, system-ui, Roboto, Helvetica Neue, Arial, sans-serif",
      heading_font = "Segoe UI Semibold, Segoe UI, system-ui, Roboto, Arial, sans-serif",
      code_font = "Cascadia Mono, Consolas, Menlo, Monaco, monospace"
    ),

    # ---------------- SIDEBAR ----------------
    sidebar = bslib::sidebar(
      width = 360,
      shiny::h6(
        "Support tool for digitizing data for the BayesCores algorithm and Bayesian modeling.",
        style = "color: rgba(234,242,255,0.78); font-size: 0.90rem; line-height: 1.25; margin-bottom: 18px;"
      ),

      shiny::conditionalPanel(
        condition = "input.main_nav === 'Data Extraction'",
        shiny::h5("Extraction Mode", class = "mb-3 text-primary"),
        shiny::fileInput("img_upload", "Upload KM Image (Manual Mode)", accept = c("image/png", "image/jpeg", ".jpg", ".png")),

        # --- MANUAL EXTRACTION INPUTS ---
        shiny::div(
          style = "border: 1px solid rgba(255,255,255,0.2); padding: 10px; border-radius: 8px; background: rgba(0,0,0,0.15); margin-bottom: 15px;",
          shiny::h6("Manual Configuration", style = "color: #BFD6F3; font-weight: bold; margin-bottom: 10px;"),

          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::numericInput("man_x_start", "X Start", value = 0),
            shiny::numericInput("man_x_end", "X End", value = 100),
            shiny::numericInput("man_x_inc", "X Inc", value = 20)
          ),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::numericInput("man_y_start", "Y Start", value = 0),
            shiny::numericInput("man_y_end", "Y End", value = 1),
            shiny::numericInput("man_y_inc", "Y Inc", value = 0.2, step = 0.1)
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput("man_num_curves", "N Curves", value = 2, min = 1, max = 5),
            # Default value FALSE
            shiny::checkboxInput("man_y_vert", "Y Text Vert.", value = FALSE)
          ),

          # Advanced Hidden Params
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              "Advanced Visual Params",
              shiny::h6("Image Pre-processing", style="color:#BFD6F3; font-size:0.85rem;"),
              shiny::numericInput("man_border", "Add Border (px)", value = 0, min=0, step=10),
              shiny::sliderInput("man_brightness", "Brightness (%)", min = 50, max = 150, value = 100, step = 5),
              shiny::sliderInput("man_contrast", "Contrast (%)", min = 50, max = 150, value = 100, step = 5),
              shiny::hr(style="border-color: rgba(255,255,255,0.2);"),
              shiny::h6("Digitizer Tuning", style="color:#BFD6F3; font-size:0.85rem;"),
              shiny::numericInput("man_bg_light", "BG Lightness", value = 0.3, step = 0.1, min = 0, max = 1),
              shiny::numericInput("man_word_sens", "Word Sens.", value = 30),
              shiny::numericInput("man_neighbors", "Neighbors", value = 20),
              shiny::numericInput("man_impute", "Impute Size", value = 0),
              shiny::checkboxInput("man_censoring", "Censoring Marks", value = FALSE),
              shiny::checkboxInput("man_enhance", "Enhance Channels", value = FALSE),
              shiny::checkboxInput("man_line_cens", "Line Censoring", value = FALSE)
            )
          ),

          shiny::actionButton("run_manual_dig", "Run Manual Digitization", class = "btn-primary w-100 mt-2", icon = shiny::icon("mouse-pointer")),

          # --- NEW: GOOGLE OPAL BUTTON ---
          shiny::div(style = "height: 8px;"),
          shiny::actionButton("open_opal_modal", "Google Opal (Experimental)", class = "btn-opal w-100", icon = shiny::icon("cloud-upload-alt"))
        ),

        shiny::actionButton("reset_all", "Reset / Start Over", class = "btn-outline-danger w-100 mb-2", icon = shiny::icon("trash-alt")),
        shiny::hr(),
        shiny::h6("Downloads", style = "color: rgba(234,242,255,0.78);"),
        shiny::downloadButton("dl_ipd_excel", "Excel", class = "btn-success w-100 mb-2 btn-sm"),
        shiny::downloadButton("dl_ipd_rds", "RDS", class = "btn-info w-100 btn-sm")
      ),

      shiny::conditionalPanel(
        condition = "input.main_nav === 'Bayesian Model'",
        shiny::h5("Bayesian Settings", class = "mb-3 text-primary"),
        shiny::numericInput("iter", "Iterations:", value = 2000, min = 500),
        shiny::numericInput("chains", "Chains:", value = 4, min = 1, max = 4),
        shiny::numericInput("warmup", "Warmup:", value = 1000, min = 100),
        shiny::hr(),
        shiny::checkboxInput("shared_shape", "Shared Shape", value = TRUE),
        shiny::selectInput(
          "cure_belief", "Cure Belief:",
          choices = c("unknown", "unlikely", "very_unlikely", "optimistic", "mild_optimistic"),
          selected = "unknown"
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
        # NEW: Button for MCMC Draws
        shiny::downloadButton("dl_draws_csv", "MCMC Draws (CSV)", class = "btn-warning w-100 btn-sm mt-2")
      )
    ),

    # ---------------- MAIN PANEL ----------------
    bslib::navset_underline(
      id = "main_nav",

      # TABS: 1. DATA EXTRACTION
      bslib::nav_panel(
        "Data Extraction",
        shiny::div(
          class = "container-fluid py-3",
          bslib::layout_columns(
            col_widths = c(7, 5),
            bslib::card(
              # Dynamic Header handled by content, here static text
              bslib::card_header("Plot Visualization (Digitized Check / Reconstructed)"),
              shiny::plotOutput("km_plot_output", height = "420px"),
              bslib::card_footer(
                shiny::div(
                  style = "display:flex; justify-content:flex-end; align-items:center;",
                  shiny::actionButton(
                    "switch_arms", "Switch Arms",
                    class = "btn-outline-secondary btn-sm",
                    icon = shiny::icon("exchange-alt"),
                    style = "width: 160px;"
                  )
                )
              )
            ),
            bslib::card(
              bslib::card_header("Original Image"),
              shiny::imageOutput("original_image_output", height = "420px"),
              style = "background-color: rgba(255,255,255,0.55);"
            )
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            bslib::card(
              bslib::card_header("Interactive Data Correction"),
              style = "background: linear-gradient(180deg, rgba(143,179,217,0.12) 0%, rgba(255,255,255,1) 60%);",
              shiny::h6("Risk Table (Editable)", style = "margin-top: 2px; color: #0B1F3A; font-weight: 700;"),
              rhandsontable::rHandsontableOutput("hot_risk_table"),
              shiny::div(style = "height:10px"),
              shiny::h6("Y-Axis Ticks (Editable)", style = "margin-top: 2px; color: #0B1F3A; font-weight: 700;"),
              rhandsontable::rHandsontableOutput("hot_y_axis"),
              shiny::hr(),
              # Renamed Button
              shiny::actionButton("apply_edits", "Generate IPD from Table & Curves", class = "btn-primary w-100", icon = shiny::icon("sync")),
              shiny::helpText("Right-click tables to add/remove rows. In Manual Mode, fill the Risk Table manually and press Apply.")
            ),
            shiny::div(
              # REMOVED: Debug Card
              # REMOVED: IPD Preview Card
              bslib::card(
                bslib::card_header("Survival Analysis Summary"),
                shiny::div(style = "overflow-y: auto; max-height: 480px;", shiny::verbatimTextOutput("survival_summary_output"))
              )
            )
          ),
          bslib::card(
            bslib::card_header("Reproducible Extraction Code"),
            shiny::verbatimTextOutput("extraction_repro_code"),
            full_screen = TRUE
          )
        )
      ),

      # TABS: 2. BAYESIAN MODEL
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
                shiny::div(class = "text-end mb-2", shiny::downloadButton("dl_plot_diag", "Download Diagnostic Plot", class = "btn-xs btn-outline-danger")),
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
# SERVER (DO NOT TOUCH LOGIC)
# ==============================================================================

server <- function(input, output, session) {
  seed_val <- 555; adapt_delta_val <- 0.99
  temp_dir <- tempdir()
  shiny::addResourcePath("temp_img", temp_dir)

  vals <- shiny::reactiveValues(
    final_ipd = NULL, fit_obj = NULL, cox_obj = NULL, survdiff_obj = NULL, model_fit_obj = NULL,
    code_text = NULL, extraction_code = NULL, debug_data = NULL, original_img_path = NULL,
    processed_img_path = NULL, # Path for image with magick adjustments
    curve_colors = NULL, x_breaks = NULL, y_breaks = NULL,
    risk_table_editable = NULL, y_axis_editable = NULL, extracted_params = NULL,
    manual_raw_data = NULL, # New: saves manual digitization
    mode = "manual"
  )

  # --- STOP ---
  shiny::onStop(function() {
    cat("App stopped.\n")
  })

  # 1. Load image and set initial path
  shiny::observeEvent(input$img_upload, {
    shiny::req(input$img_upload)
    vals$original_img_path <- input$img_upload$datapath
    vals$processed_img_path <- input$img_upload$datapath
  })

  # 2. Observer to apply Magick in real time (Brightness, Contrast, Border)
  shiny::observe({
    shiny::req(vals$original_img_path)
    # Trigger with any visual change or new image
    input$man_border
    input$man_brightness
    input$man_contrast

    tryCatch({
      img <- magick::image_read(vals$original_img_path)

      # Apply border if > 0
      if(!is.null(input$man_border) && input$man_border > 0) {
        geom <- paste0(input$man_border, "x", input$man_border)
        img <- magick::image_border(img, "white", geom)
      }

      # Apply brightness/contrast
      if(!is.null(input$man_brightness) && !is.null(input$man_contrast)) {
        img <- magick::image_modulate(img, brightness = input$man_brightness, saturation = 100, hue = 100)
        img <- magick::image_contrast(img, sharpen = (input$man_contrast - 100)/50) # Approx mapping
      }

      # Save temporarily
      tmp_proc <- file.path(temp_dir, paste0("proc_view_", as.integer(Sys.time()), ".png"))
      magick::image_write(img, tmp_proc, format = "png")
      vals$processed_img_path <- tmp_proc

    }, error = function(e){
      warning("Magick processing failed")
    })
  })

  # --- RESET ALL (START OVER): reliable hard reset ---
  shiny::observeEvent(input$reset_all, {
    # Reset reactive values manually to ensure clean state for new image
    vals$final_ipd <- NULL
    vals$fit_obj <- NULL
    vals$cox_obj <- NULL
    vals$survdiff_obj <- NULL
    vals$model_fit_obj <- NULL
    vals$code_text <- NULL
    vals$extraction_code <- NULL
    vals$debug_data <- NULL
    vals$original_img_path <- NULL
    vals$processed_img_path <- NULL
    vals$curve_colors <- NULL
    vals$x_breaks <- NULL
    vals$y_breaks <- NULL
    vals$risk_table_editable <- NULL
    vals$y_axis_editable <- NULL
    vals$extracted_params <- NULL
    vals$manual_raw_data <- NULL
    vals$mode <- "manual"

    session$reload()
  })

  # ==============================================================================
  # MANUAL MODE: Click & Digitize with SurvdigitizeR (Main Thread)
  # ==============================================================================
  shiny::observeEvent(input$run_manual_dig, {
    # FIX: Ensure processed_img_path is refreshed if inputs changed but observe hasn't fired yet
    shiny::req(vals$original_img_path)

    # Force refresh of processed image
    img <- magick::image_read(vals$original_img_path)
    if(!is.null(input$man_border) && input$man_border > 0) {
      geom <- paste0(input$man_border, "x", input$man_border)
      img <- magick::image_border(img, "white", geom)
    }
    if(!is.null(input$man_brightness) && !is.null(input$man_contrast)) {
      img <- magick::image_modulate(img, brightness = input$man_brightness, saturation = 100, hue = 100)
      img <- magick::image_contrast(img, sharpen = (input$man_contrast - 100)/50)
    }
    tmp_proc <- file.path(temp_dir, paste0("proc_view_run_", as.integer(Sys.time()), ".png"))
    magick::image_write(img, tmp_proc, format = "png")
    vals$processed_img_path <- tmp_proc

    vals$mode <- "manual"

    shiny::showNotification("Starting Manual Digitization. Check for popup window...", type = "message", duration = 5)

    # Generate time sequence for empty table
    t_start <- input$man_x_start
    t_end <- input$man_x_end
    t_inc <- input$man_x_inc

    if(is.na(t_inc) || t_inc <= 0) t_inc <- (t_end - t_start)/5
    vec_times <- seq(t_start, t_end, by = t_inc)

    # Create empty table (Grid) - NAMES IN ENGLISH
    empty_risk <- tibble::tibble(
      Time = as.numeric(vec_times),
      N_Risk_G1 = rep(NA_real_, length(vec_times)),
      N_Risk_G2 = rep(NA_real_, length(vec_times))
    )

    vals$risk_table_editable <- empty_risk

    # FIX: Y values without multiplying by 100
    y_seq <- seq(input$man_y_start, input$man_y_end, by=input$man_y_inc)
    vals$y_axis_editable <- data.frame(Y_Values = y_seq)

    # Save manual parameters
    vals$extracted_params <- list(
      x_start = input$man_x_start, x_end = input$man_x_end, x_inc = input$man_x_inc,
      y_start = input$man_y_start, y_end = input$man_y_end, y_inc = input$man_y_inc
    )

    # Run survival_digitize with manual arguments (WITHOUT attempt_OCR)
    # Use vals$processed_img_path to include added borders/brightness
    tryCatch({
      raw <- SurvdigitizeR::survival_digitize(
        img_path = vals$processed_img_path,
        bg_lightness = input$man_bg_light,
        attempt_OCR = FALSE, # Explicitly FALSE or omitted
        word_sensitivity = input$man_word_sens,
        num_curves = input$man_num_curves,
        censoring = input$man_censoring,
        x_start = input$man_x_start,
        x_end = input$man_x_end,
        x_increment = input$man_x_inc,
        y_start = input$man_y_start,
        y_end = input$man_y_end,
        y_increment = input$man_y_inc,
        y_text_vertical = input$man_y_vert,
        nr_neighbors = input$man_neighbors,
        enhance = input$man_enhance,
        impute_size = input$man_impute,
        line_censoring = input$man_line_cens
      )

      # Adjust Y scale if necessary (SurvdigitizeR sometimes returns 0-100)
      if (max(raw$St, na.rm = TRUE) > 1.5) raw$survival <- raw$St / 100 else raw$survival <- raw$St

      vals$manual_raw_data <- raw
      vals$debug_data <- list(manual = TRUE, params = vals$extracted_params)

      shiny::showNotification("Digitization complete! Fill Risk Table and click 'Generate IPD'.", type = "warning", duration = 10)

    }, error = function(e) {
      shiny::showNotification(paste("Error in manual digitization:", e$message), type = "error")
    })
  })

  # ==============================================================================
  # GOOGLE OPAL MODE (EXPERIMENTAL)
  # ==============================================================================

  shiny::observeEvent(input$open_opal_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Google Opal Extraction (Experimental)",
      size = "l",
      shiny::div(
        shiny::p("Please proceed to the experimental Google Opal app using the following link. No image upload is required here."),
        shiny::a("Go to Google Opal (External Link)", href="https://opal.google/?flow=drive:/1Nx_vaKxIgPLYisxM5xVy3WDjTb_so-fl&shared&mode=app", target="_blank", class="btn btn-outline-primary mb-3"),
        shiny::p("INSTRUCTIONS:"),
        shiny::tags$ul(
          shiny::tags$li("Load your KM image into Google Opal."),
          shiny::tags$li("Ensure you extract the full KM curves, risk numbers (n-risk), and axis ticks unmodified."),
          shiny::tags$li("Copy the R code generated by Opal (vectors like `a1_x`, `risk_n_arm1`, etc.) and paste it into the box below."),
          shiny::tags$li("The logic requires the structure of the R vectors to be preserved.")
        ),
        shiny::textAreaInput("opal_input_text", "Paste R Code extracted from Google Opal:", height = "300px", placeholder = "a1_x <- c(0, 1.2, ...)\n..."),
        shiny::div(class = "text-end",
                   shiny::actionButton("process_opal", "Process Opal Data", class = "btn-success", icon = shiny::icon("check"))
        )
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  shiny::observeEvent(input$process_opal, {
    shiny::req(input$opal_input_text)
    txt <- input$opal_input_text

    shiny::showNotification("Processing Opal Data...", type = "message")

    tryCatch({
      # --- 1. PRE-CLEANING: Filter only lines that look like R assignments ---
      # This removes "--- PART A ---", "# Comments", "ARM 1...", etc.
      all_lines <- strsplit(txt, "\n")[[1]]
      # Keep lines that have "<- c(" or "= c("
      valid_lines <- all_lines[grepl("<- *c\\(|= *c\\(", all_lines)]
      clean_txt <- paste(valid_lines, collapse = "\n")

      # --- 2. EVALUATION ---
      # Evaluate the cleaned text in a safe environment
      eval_env <- new.env()
      eval(parse(text = clean_txt), envir = eval_env)

      # --- 3. FLEXIBLE GETTER (Aliases) ---
      # This handles if Opal outputs "arm1_x" but app expects "a1_x"
      get_val <- function(candidates, env) {
        for(nm in candidates) {
          if(exists(nm, envir = env)) return(get(nm, envir = env))
        }
        stop(paste("Missing variable. Looked for:", paste(candidates, collapse=" or ")))
      }

      # Extract vectors with fallback names
      a1_x <- get_val(c("arm1_x", "a1_x"), eval_env)
      a1_y <- get_val(c("arm1_y", "a1_y"), eval_env)
      a2_x <- get_val(c("arm2_x", "a2_x"), eval_env)
      a2_y <- get_val(c("arm2_y", "a2_y"), eval_env)

      # Risk table mappings (x_axis_ticks usually = risk_time)
      risk_time   <- get_val(c("x_axis_ticks", "risk_time"), eval_env)
      risk_n_arm1 <- get_val(c("n_risk_arm1", "risk_n_arm1"), eval_env)
      risk_n_arm2 <- get_val(c("n_risk_arm2", "risk_n_arm2"), eval_env)
      y_ticks     <- get_val(c("y_axis_ticks", "y_ticks"), eval_env)

      # --- ROBUSTNESS FIX: Truncate mismatched lengths WITHIN arms ---
      len1_x <- length(a1_x); len1_y <- length(a1_y)
      len2_x <- length(a2_x); len2_y <- length(a2_y)

      if(len1_x != len1_y) {
        min_1 <- min(len1_x, len1_y)
        a1_x <- a1_x[1:min_1]
        a1_y <- a1_y[1:min_1]
        shiny::showNotification(paste("Warning: Arm 1 vectors length mismatch. Truncated to", min_1), type = "warning")
      }

      if(len2_x != len2_y) {
        min_2 <- min(len2_x, len2_y)
        a2_x <- a2_x[1:min_2]
        a2_y <- a2_y[1:min_2]
        shiny::showNotification(paste("Warning: Arm 2 vectors length mismatch. Truncated to", min_2), type = "warning")
      }
      # -------------------------------------------------------------

      # 1. Create Risk Table (Editable)
      # Ensure lengths match
      min_len <- min(length(risk_time), length(risk_n_arm1), length(risk_n_arm2))
      opal_risk <- tibble::tibble(
        Time = as.numeric(risk_time[1:min_len]),
        N_Risk_G1 = as.numeric(risk_n_arm1[1:min_len]),
        N_Risk_G2 = as.numeric(risk_n_arm2[1:min_len])
      )
      vals$risk_table_editable <- opal_risk

      # 2. Create Y Axis (Editable)
      vals$y_axis_editable <- data.frame(Y_Values = as.numeric(y_ticks))

      # 3. Reconstruct 'manual_raw_data' structure for downstream logic
      # Normalize Y to 0-1
      max_y <- max(c(a1_y, a2_y), na.rm=TRUE)
      # Usually Opal gives 0-100, checking scaling
      scale_fac <- if(max_y > 1.5) 100 else 1

      df_c1 <- data.frame(time = a1_x, survival = a1_y / scale_fac, curve = 1)
      df_c2 <- data.frame(time = a2_x, survival = a2_y / scale_fac, curve = 2)

      # Combine
      raw_sim <- rbind(df_c1, df_c2)
      # SurvdigitizeR uses 'St' as well
      raw_sim$St <- raw_sim$survival * 100

      vals$manual_raw_data <- raw_sim

      # 4. Set Params for Plot axes (Auto-calculate from vectors)
      x_min <- min(c(a1_x, a2_x), na.rm=TRUE)
      x_max <- max(c(a1_x, a2_x), na.rm=TRUE)

      y_min <- min(y_ticks, na.rm=TRUE)
      y_max <- max(y_ticks, na.rm=TRUE)

      # Update Breaks for Plot
      vals$x_breaks <- risk_time
      vals$y_breaks <- y_ticks

      # Update Internal Params just in case
      vals$extracted_params <- list(
        x_start = x_min, x_end = x_max, x_inc = risk_time[2] - risk_time[1], # approx
        y_start = y_min / scale_fac, y_end = y_max / scale_fac, y_inc = (y_ticks[2] - y_ticks[1]) / scale_fac
      )

      vals$mode <- "opal"

      shiny::removeModal()
      shiny::showNotification("Opal data processed successfully. Please review the tables and 'Generate IPD'.", type = "warning", duration = 8)

    }, error = function(e) {
      shiny::showNotification(paste("Error parsing Opal data. Check format.", e$message), type = "error")
    })
  })

  # Render Editables - FIX: stretchH to avoid row cutting
  output$hot_risk_table <- rhandsontable::renderRHandsontable({
    shiny::req(vals$risk_table_editable)
    # Increase height for better visibility and working scroll
    rhandsontable::rhandsontable(vals$risk_table_editable, stretchH = "all", height = 450) %>%
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) %>%
      rhandsontable::hot_cols(colWidths = 100)
  })

  output$hot_y_axis <- rhandsontable::renderRHandsontable({
    shiny::req(vals$y_axis_editable)
    # Reduce height of Y-axis table
    rhandsontable::rhandsontable(vals$y_axis_editable, stretchH = "all", height = 150) %>%
      rhandsontable::hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
  })

  # Apply Edits - CENTRAL CALCULATION BUTTON
  shiny::observeEvent(input$apply_edits, {
    # We need the table and a loaded image OR Opal data
    # Relaxing the image requirement for Opal mode
    shiny::req(input$hot_risk_table)
    if(vals$mode == "manual") shiny::req(vals$original_img_path)

    new_risk <- rhandsontable::hot_to_r(input$hot_risk_table)
    new_y_df <- rhandsontable::hot_to_r(input$hot_y_axis)
    new_y <- as.numeric(new_y_df$Y_Values)

    vals$risk_table_editable <- new_risk
    vals$y_axis_editable <- new_y_df

    shiny::showNotification("Re-calculating IPD...", type = "message", id = "ext_recalc", duration = NULL)

    # Common Logic for both Manual and Opal (since Opal populates manual_raw_data)
    tryCatch({
      if(is.null(vals$manual_raw_data)) stop("No digitization data found. Run Manual Digitization or Opal Import first.")

      # Update breaks for plot
      vals$x_breaks <- new_risk$Time
      vals$y_breaks <- new_y

      reconstruir_ipd_manual(vals$manual_raw_data, new_risk)

      shiny::removeNotification(id = "ext_recalc")
      shiny::showNotification("IPD Reconstructed", type = "message")
    }, error = function(e){
      shiny::removeNotification(id = "ext_recalc")
      shiny::showNotification(paste("Error rebuilding:", e$message), type = "error")
    })
  })

  # ==============================================================================
  # Core Logic: FINAL RECONSTRUCTION (Shared / Manual)
  # ==============================================================================

  # Helper function to convert table and raw data into IPD (Final part of the process)
  reconstruir_ipd_manual <- function(raw_data, tabla_riesgo) {
    limpiar <- function(x) as.numeric(gsub("[^0-9.]", "", as.character(x)))

    # --- 1. MONOTONICITY CHECK (NRISK CANNOT INCREASE) ---
    check_mono <- function(x) {
      x_clean <- as.numeric(x[!is.na(x)])
      if(length(x_clean) < 2) return(TRUE)
      # diff(x) must be <= 0 (decreasing or equal). If any diff > 0, error.
      if(any(diff(x_clean) > 0)) return(FALSE)
      return(TRUE)
    }

    if(!check_mono(tabla_riesgo$N_Risk_G1)) stop("Error: N_Risk_G1 increases over time. Numbers at risk must be monotonically decreasing.")
    if(!check_mono(tabla_riesgo$N_Risk_G2)) stop("Error: N_Risk_G2 increases over time. Numbers at risk must be monotonically decreasing.")

    # Use of ENGLISH names
    df_c1 <- tabla_riesgo %>%
      dplyr::transmute(time_tick = as.numeric(Time), nrisk = limpiar(N_Risk_G1), curve = 1) %>%
      dplyr::filter(!is.na(time_tick), !is.na(nrisk))

    df_c2 <- tabla_riesgo %>%
      dplyr::transmute(time_tick = as.numeric(Time), nrisk = limpiar(N_Risk_G2), curve = 2) %>%
      dplyr::filter(!is.na(time_tick), !is.na(nrisk))

    nrisk_all <- dplyr::bind_rows(df_c1, df_c2)

    ipd_list <- list()
    for (cid in unique(nrisk_all$curve)) {
      km <- subset(raw_data, curve == cid)
      nr <- subset(nrisk_all, curve == cid)
      if (nrow(nr) >= 2 && nrow(km) > 0) {
        ipd_list[[cid]] <- bayescores::reconstruct_ipd(km, nr)$ipd %>% dplyr::mutate(curve = cid)
      }
    }

    if (length(ipd_list) > 0) {
      final <- dplyr::bind_rows(ipd_list) %>% dplyr::mutate(arm = paste("Group", curve))

      # Initial model fit to check direction
      mod_init <- coxph(Surv(time, status) ~ arm, final)

      # --- 2. AUTOMATIC ARM SWITCH IF COEF > 0 ---
      # If coef is positive, it means the reference group is 'better' than the other (HR > 1).
      # Invert labels to force HR < 1 (negative coef).
      coef_val <- tryCatch(coef(mod_init)[1], error=function(e) 0)

      if(!is.na(coef_val) && coef_val > 0) {
        final$arm <- ifelse(final$arm == "Group 1", "TEMP", "Group 1")
        final$arm <- ifelse(final$arm == "TEMP", "Group 2", final$arm)
        # Re-adjust objects with new arm
        mod_init <- coxph(Surv(time, status) ~ arm, final)
        shiny::showNotification("Arms switched automatically (positive coefficient detected).", type="warning", duration=5)
      }

      vals$final_ipd <- final
      vals$fit_obj <- survfit(Surv(time, status) ~ arm, final)
      vals$cox_obj <- mod_init
      vals$survdiff_obj <- survdiff(Surv(time, status) ~ arm, final)

      # Default colors
      vals$curve_colors <- c("#1f77b4", "#ff7f0e")
    } else {
      stop("Could not reconstruct curves. Check Risk Table data.")
    }
  }

  # --------------------------------------------------------------------------
  # OUTPUTS
  # --------------------------------------------------------------------------
  output$original_image_output <- shiny::renderImage({
    # If using manual mode, we have a processed path. If Opal, we might not have one.
    if(vals$mode == "manual") {
      shiny::req(vals$processed_img_path)
      list(src = vals$processed_img_path, height = "100%", width = "auto", alt = "Orig")
    } else {
      # Return blank or placeholder for Opal if no image loaded
      list(src = "", alt = "No Image in Opal Mode")
    }
  }, deleteFile = FALSE)

  output$extraction_repro_code <- shiny::renderText({
    shiny::req(vals$extracted_params, vals$risk_table_editable)
    p <- vals$extracted_params
    rt <- vals$risk_table_editable

    # --- 3. REPRODUCIBLE CODE WITH FULL VECTORS ---
    # Filter empty or null rows for cleaning
    rt_clean <- rt %>% dplyr::filter(!is.na(Time))

    vec_t <- paste0("c(", paste(rt_clean$Time, collapse=", "), ")")
    vec_r1 <- paste0("c(", paste(rt_clean$N_Risk_G1, collapse=", "), ")")
    vec_r2 <- paste0("c(", paste(rt_clean$N_Risk_G2, collapse=", "), ")")

    paste0(
      "library(SurvdigitizeR)\nlibrary(bayescores)\nlibrary(tidyverse)\n\n# INSERT PATH HERE\nimg_path <- 'plot.png'\n\n",
      "x_start <- ", p$x_start, "; x_end <- ", p$x_end, "; x_increment <- ", p$x_inc, "\n",
      "y_start <- ", p$y_start, "; y_end <- ", p$y_end, "; y_increment <- ", p$y_inc, "\n\n",
      "# 1. Digitize\nraw_data <- SurvdigitizeR::survival_digitize(img_path, num_curves=2, x_start=x_start, x_end=x_end, x_increment=x_increment, y_start=y_start, y_end=y_end, y_increment=y_increment, censoring=TRUE)\n",
      "if(max(raw_data$St, na.rm=T) > 1.5) raw_data$survival <- raw_data$St/100 else raw_data$survival <- raw_data$St\n\n",
      "# 2. Reconstruct IPD (Vectors from Manual Table)\n",
      "times <- ", vec_t, "\n",
      "n_risk_g1 <- ", vec_r1, "\n",
      "n_risk_g2 <- ", vec_r2, "\n\n",
      "n_risk_df <- bind_rows(\n",
      "  tibble(time_tick = times, nrisk = n_risk_g1, curve = 1),\n",
      "  tibble(time_tick = times, nrisk = n_risk_g2, curve = 2)\n",
      ") %>% filter(!is.na(nrisk))\n\n",
      "ipd_list <- list()\n",
      "for(cid in 1:2) {\n",
      "   km <- subset(raw_data, curve == cid)\n",
      "   nr <- subset(n_risk_df, curve == cid)\n",
      "   if(nrow(nr) > 0) ipd_list[[cid]] <- bayescores::reconstruct_ipd(km, nr)$ipd %>% mutate(curve=cid)\n",
      "}\n",
      "final_ipd <- bind_rows(ipd_list) %>% mutate(arm=paste('Group', curve))\n"
    )
  })

  # Updated Plot Output (Dual State)
  output$km_plot_output <- shiny::renderPlot({
    # State 2: IPD Reconstructed (Final Plot)
    if (!is.null(vals$final_ipd) && !is.null(vals$fit_obj)) {
      plot_colors <- if(!is.null(vals$curve_colors)) vals$curve_colors else c("#1f77b4", "#ff7f0e")
      x_br <- if (!is.null(vals$x_breaks)) vals$x_breaks else waiver()
      # If in manual mode, respect original breaks
      y_br <- if (!is.null(vals$y_breaks)) vals$y_breaks else waiver()

      survminer::ggsurvplot(
        vals$fit_obj, data = vals$final_ipd, palette = plot_colors,
        pval = TRUE, pval.coord = c(0, 0.1),
        risk.table = TRUE, risk.table.col = "strata", risk.table.height = 0.25, risk.table.y.text = FALSE,
        break.x.by = if (length(x_br) > 1) diff(x_br)[1] else NULL,
        break.y.by = if (length(y_br) > 1) diff(y_br)[1] else NULL,
        ggtheme = ggplot2::theme_classic()
      )
    }
    # State 1: Digitized Raw Data (Check Plot)
    else if (!is.null(vals$manual_raw_data)) {
      # Use raw digitized points from SurvdigitizeR output
      ggplot2::ggplot(vals$manual_raw_data, aes(x=time, y=survival, color=as.factor(curve))) +
        ggplot2::geom_step(linewidth=1.2) +
        ggplot2::theme_minimal() +
        ggplot2::labs(title="Digitized Curves Check", subtitle="Review captured curves before applying risk table", y="Survival Probability", x="Time", color="Curve ID") +
        ggplot2::theme(legend.position="bottom")
    }
  })

  output$ipd_table_output <- shiny::renderTable({
    shiny::req(vals$final_ipd)
    head(vals$final_ipd, 20)
  })

  output$survival_summary_output <- shiny::renderPrint({
    shiny::req(vals$cox_obj)
    cat("COX MODEL:\n")
    print(summary(vals$cox_obj))
    cat("\nLOG-RANK:\n")
    print(vals$survdiff_obj)
  })

  output$dl_ipd_excel <- shiny::downloadHandler(
    filename = function() "ipd.xlsx",
    content = function(f) writexl::write_xlsx(vals$final_ipd, f)
  )

  output$dl_ipd_rds <- shiny::downloadHandler(
    filename = function() "ipd.rds",
    content = function(f) saveRDS(vals$final_ipd, f)
  )

  shiny::observeEvent(input$run_model, {
    shiny::req(vals$final_ipd)
    shiny::showNotification("Compiling and fitting model...", type = "message", id = "mod_start", duration = NULL)

    tryCatch({
      hist_arg <- if (input$use_historical) {
        paste0("TRUE, params=c(", input$hist_mean, ",", input$hist_sd, ")")
      } else {
        paste0("FALSE, belief='", input$cure_belief, "'")
      }

      # Added Density Plots to reproducible code
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
        cure_belief = input$cure_belief
      )

      shiny::removeNotification(id = "mod_start")
      shiny::showNotification("Model fitted!", type = "message")
    }, error = function(e) {
      shiny::removeNotification(id = "mod_start")
      shiny::showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })

  output$model_summary <- shiny::renderPrint({
    shiny::req(vals$model_fit_obj)
    if (exists("outcomes", asNamespace("bayescores"))) bayescores::outcomes(vals$model_fit_obj) else print(vals$model_fit_obj)
  })

  output$plot_densities <- shiny::renderPlot({
    shiny::req(vals$model_fit_obj)
    bayescores::plot_densities(vals$model_fit_obj) + ggplot2::theme(aspect.ratio = 1)
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

  # NEW: CSV Download for Draws
  output$dl_draws_csv <- shiny::downloadHandler(
    filename = function() "mcmc_draws.csv",
    content = function(f) {
      shiny::req(vals$model_fit_obj)
      # Extract draws (sims)
      # Usually rstan fit objects can be coerced to data.frame
      draws <- as.data.frame(vals$model_fit_obj$stan_fit)
      write.csv(draws, f, row.names = FALSE)
    }
  )

  output$dl_plot_dens <- shiny::downloadHandler(
    filename = "densities.pdf",
    content = function(f) {
      pdf(f, width = 10, height = 8)
      # Capture the plot object first to avoid auto-printing if any
      p <- bayescores::plot_densities(vals$model_fit_obj)
      # Add theme and print explicitly
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

  output$dl_plot_diag <- shiny::downloadHandler(
    filename = "diagnostics.pdf",
    content = function(f) {
      shiny::req(vals$model_fit_obj)
      pdf(f, width = 10, height = 8)
      tryCatch({
        bayescores::model_diagnostics(vals$model_fit_obj)
      }, error = function(e) {
        plot(vals$model_fit_obj$stan_fit, pars = c("lp__"))
      })
      dev.off()
    }
  )
}

shiny::shinyApp(ui, server)
}

# Add globalVariables to avoid R CMD check notes about non-standard evaluation
utils::globalVariables(c("Time", "N_Risk_G1", "N_Risk_G2", "time_tick", "nrisk", "time", "survival", "curve", "status", "arm", "cluster", "dataTableOutput", "renderDataTable"))
