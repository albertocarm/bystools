#' KM2bayes Pro — guided BayeScores with a clean two-choice entry and accuracy bench
#'
#' @description
#' The guided flagship app. It offers a clean two-choice landing
#' (\strong{upload a dataset} on the left vs. \strong{digitize a figure} on the
#' right); the workspace appears once something is loaded, instead of a permanently
#' split screen. It also includes an \strong{Accuracy bench} where you paste the
#' published hazard ratio (with CI) and medians and the app reports the error.
#'
#' Launches a Shiny application that extracts individual patient data (IPD) from
#' Kaplan-Meier images and fits Bayesian cure models using the `bayescores` algorithm.
#'
#' Image cleaning is available through three paths:
#' \itemize{
#'    \item \strong{Automatic engine:} the bundled Python digitizer
#'      (`inst/python/kmdig.py`) produces a clean plot and the numbers-at-risk, which
#'      are pre-loaded into the editable grid. If Python is not available the app shows
#'      installation instructions.
#'    \item \strong{LLM:} use any vision LLM with the provided prompt to obtain a
#'      cleaned image and the four data lines.
#'    \item \strong{Manual:} clean the image in any editor and digitize point-and-click,
#'      or paste the numbers directly.
#' }
#'
#' @return A Shiny application object.
#'
#' @rawNamespace import(shiny, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @rawNamespace import(survival, except = c(cluster))
#' @rawNamespace import(future, except = c(cluster))
#' @importFrom graphics legend lines
#' @importFrom stats plogis time vcov AIC
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
#' @importFrom utils write.csv head globalVariables read.csv data
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par rect
#' @importFrom stats median coef cor
#' @importFrom tools file_ext
#' @importFrom stats approx na.omit relevel
#' @importFrom graphics mtext
#'
#' @export
km2bayesPro <- function() {

  # Increase upload size
  options(shiny.maxRequestSize = 30*1024^2)

  # Stan
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())

  # Select the dedicated Python environment for the automatic engine if present.
  try({
    if (requireNamespace("reticulate", quietly = TRUE) &&
        isTRUE(reticulate::virtualenv_exists("r-bystools")))
      reticulate::use_virtualenv("r-bystools", required = FALSE)
  }, silent = TRUE)

  # ==============================================================================
  # 3. USER INTERFACE (UI)
  # ==============================================================================
  css_modern <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
  :root{
    --bm-bg: #F5F7FB; --bm-fg: #1A2233; --bm-muted: #67748A;
    --bm-primary: #4F46E5; --bm-primary-2: #7C3AED; --bm-accent: #06B6D4;
    --bm-accent-soft: #EEF0FF;
    --bm-card: #FFFFFF; --bm-card-2: #F3F5FF; --bm-border: #E4E8F2;
    --bm-shadow: 0 18px 40px rgba(31,41,75,0.10);
    --bm-shadow-soft: 0 8px 22px rgba(31,41,75,0.08);
    --bm-radius: 18px;
  }
  html{ font-size: 17px; }
  body{
    font-family: 'Inter', system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif;
    background:
      radial-gradient(1000px 700px at 12% -5%, rgba(79,70,229,0.10) 0%, rgba(245,247,251,0) 55%),
      radial-gradient(1000px 700px at 95% 10%, rgba(124,58,237,0.10) 0%, rgba(245,247,251,0) 55%),
      var(--bm-bg);
    color: var(--bm-fg);
    font-size: 1rem;
    letter-spacing: 0.1px;
  }
  h1,h2,h3,h4,h5,h6{ font-weight: 800; letter-spacing: -0.2px; }

  /* APP TITLE BAR */
  .navbar, .bslib-page-title, .navbar-brand{ font-weight: 800; }

  /* Sidebar */
  .bslib-sidebar-layout > .sidebar, .sidebar{
    background: linear-gradient(180deg, #FFFFFF 0%, #F3F5FF 100%);
    border-right: 1px solid var(--bm-border);
    box-shadow: 14px 0 38px rgba(31,41,75,0.06);
  }
  .bslib-sidebar-layout > .sidebar, .sidebar{ color: var(--bm-fg); }
  .bslib-sidebar-layout > .sidebar .text-primary, .sidebar .text-primary{ color: var(--bm-primary) !important; font-weight: 800; }
  .bslib-sidebar-layout > .sidebar hr, .sidebar hr{ border-color: var(--bm-border); opacity: 1; }
  .bslib-sidebar-layout > .sidebar .form-control, .sidebar .form-control,
  .bslib-sidebar-layout > .sidebar .form-select, .sidebar .form-select{
    background: #FFFFFF; border: 1px solid var(--bm-border); color: var(--bm-fg); border-radius: 10px;
  }
  .bslib-sidebar-layout > .sidebar label, .sidebar label{ color: var(--bm-fg); font-weight: 600; }

  /* BUTTONS */
  .btn{ border-radius: 12px; font-weight: 650; }
  .btn-primary{ background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-primary-2) 100%); border: none; box-shadow: var(--bm-shadow-soft); color: #fff; }
  .btn-primary:hover{ filter: brightness(1.06); }
  .btn-outline-danger{ border-color: rgba(220,53,69,0.45) !important; color: #D32F2F !important; }
  .btn-outline-danger:hover{ background: rgba(220,53,69,0.10) !important; }
  .btn-outline-secondary{ border-color: var(--bm-border) !important; color: var(--bm-muted) !important; }
  .btn-info{ background: linear-gradient(135deg, #06B6D4 0%, #0EA5E9 100%); border: none; color: #fff !important; box-shadow: var(--bm-shadow-soft); }
  .btn-success{ background: linear-gradient(135deg, #16A34A 0%, #22C55E 100%); border: none; color: #fff !important; }
  .btn-warning{ background: linear-gradient(135deg, #F59E0B 0%, #FBBF24 100%); border: none; color: #3a2a00 !important; }
  .btn-secondary{ background: #EEF1F8; border: 1px solid var(--bm-border); color: var(--bm-fg) !important; }
  .btn-opal{ background: linear-gradient(135deg, #7C3AED 0%, #A855F7 100%); border: none; color: #fff !important; box-shadow: var(--bm-shadow-soft); }
  .btn-opal:hover{ filter: brightness(1.08); }

  .btn-auto{ background: linear-gradient(135deg, #4F46E5 0%, #06B6D4 100%); border: none; color: #fff !important; box-shadow: 0 6px 18px rgba(79,70,229,0.35); font-weight: 700; }
  .btn-auto:hover{ filter: brightness(1.08); }
  .btn-llm{ background: linear-gradient(135deg, #0F172A 0%, #334155 100%); border: none; color: #fff !important; box-shadow: var(--bm-shadow-soft); font-weight: 650; }
  .btn-llm:hover{ filter: brightness(1.2); }

  /* Top navigation */
  .nav-underline{ border-bottom: 1px solid var(--bm-border); margin-bottom: 14px; }
  .nav-underline .nav-link{ color: var(--bm-muted); font-weight: 650; padding: 12px 18px; font-size: 1.02rem; }
  .nav-underline .nav-link.active{ color: var(--bm-primary); border-bottom-color: var(--bm-primary); }

  /* Card tabs */
  .nav-tabs { border-bottom: 1px solid var(--bm-border); }
  .nav-tabs .nav-link {
    color: var(--bm-muted) !important;
    font-weight: 650;
    border: none;
    border-bottom: 2px solid transparent;
  }
  .nav-tabs .nav-link:hover {
    color: var(--bm-primary-2) !important;
    border-color: transparent;
  }
  .nav-tabs .nav-link.active {
    color: var(--bm-primary) !important;
    background-color: transparent !important;
    border-color: transparent;
    border-bottom: 2px solid var(--bm-primary);
  }

  .card{ border-radius: var(--bm-radius); border: 1px solid var(--bm-border); background: var(--bm-card); box-shadow: var(--bm-shadow); overflow: hidden; }
  .card-header{ background: linear-gradient(180deg, var(--bm-card-2) 0%, #FFFFFF 100%); border-bottom: 1px solid var(--bm-border); font-weight: 750; color: var(--bm-fg); font-size: 1.05rem; }
  .card-footer{ background: linear-gradient(180deg, #FFFFFF 0%, var(--bm-card-2) 100%); border-top: 1px solid var(--bm-border); }
  #original_image_output img{ width: 100%; max-height: 420px; object-fit: contain; border-radius: 14px; border: 1px solid var(--bm-border); background: #FFFFFF; }
  #clean_image_output img{ width: 100%; max-height: 520px; object-fit: contain; display: block; border-radius: 14px; border: 1px solid var(--bm-border); background: #FFFFFF; }
  #clean_image_output{ width: 100%; height: 520px; }
  .container-fluid{ padding-left: 10px; padding-right: 10px; }
  .accordion-item { background-color: #FFFFFF; border: 1px solid var(--bm-border); border-radius: 12px; }
  .accordion-button { background-color: var(--bm-accent-soft); color: var(--bm-fg); font-weight: 650; border-radius: 12px; }
  .accordion-button:not(.collapsed) { background-color: rgba(79,70,229,0.12); color: var(--bm-primary); }
  .accordion-body { background-color: transparent; }

  /* Sidebar config panel */
  .km2-config-box{ border: 1px solid var(--bm-border) !important; background: #FFFFFF !important; border-radius: 14px; box-shadow: var(--bm-shadow-soft); }
  .km2-config-box h6{ color: var(--bm-primary) !important; }
  .km2-sep{ color: var(--bm-muted) !important; }

  /* Metrics table */
  .metrics-table-container table {
    width: 100%;
    border-collapse: separate;
    border-spacing: 0;
  }
  .metrics-table-container table th{ color: var(--bm-fg) !important; font-weight: 750; }
  .metrics-table-container table th:nth-child(1),
  .metrics-table-container table th:nth-child(2) {
    background: var(--bm-accent-soft) !important;
    border-bottom: 2px solid var(--bm-primary) !important;
  }
  .metrics-table-container table th:nth-child(3),
  .metrics-table-container table th:nth-child(4) {
    background: #F3EEFF !important;
    border-bottom: 2px solid var(--bm-primary-2) !important;
  }
  /* Correlation columns */
  .metrics-table-container table th:nth-child(5),
  .metrics-table-container table th:nth-child(6) {
    background: #E6FAFF !important;
    border-bottom: 2px solid var(--bm-accent) !important;
  }
  .metrics-table-container table td:nth-child(1),
  .metrics-table-container table td:nth-child(2) {
    background-color: rgba(79, 70, 229, 0.05);
  }
  .metrics-table-container table td:nth-child(3),
  .metrics-table-container table td:nth-child(4) {
    background-color: rgba(124, 58, 237, 0.05);
  }
  .metrics-table-container table td:nth-child(5),
  .metrics-table-container table td:nth-child(6) {
    background-color: rgba(6, 182, 212, 0.06);
  }
  .metrics-table-container table td:nth-child(2),
  .metrics-table-container table td:nth-child(4),
  .metrics-table-container table td:nth-child(6) {
    border-right: 2px solid var(--bm-border);
  }
  .metrics-table-container table td:nth-child(1),
  .metrics-table-container table td:nth-child(3),
  .metrics-table-container table td:nth-child(5) {
    font-weight: 600;
    color: var(--bm-fg);
  }

/* Sidebar select inputs */
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

/* Sidebar selectize inputs */
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

/* Sidebar selectize selected item */
.bslib-sidebar-layout > .sidebar .selectize-control.single .selectize-input .item,
.sidebar .selectize-control.single .selectize-input .item{
  color: #0B1F3A !important;
  opacity: 1 !important;
}

/* IPD DOWNLOAD BUTTON */
.btn-ipd-download {
  background-color: rgba(30, 90, 168, 0.1);
  border: 1px solid rgba(30, 90, 168, 0.3);
  color: #1E5AA8 !important;
  padding: 4px 10px;
  font-size: 0.85rem;
  border-radius: 6px;
  font-weight: 600;
}
.btn-ipd-download:hover {
  background-color: rgba(30, 90, 168, 0.2);
  border-color: rgba(30, 90, 168, 0.5);
}

/* Upload dropzone (browse + drag&drop + paste) */
.km2-dropzone{
  position: relative;
  border: 2px dashed #C7CCEA;
  border-radius: 16px;
  background: linear-gradient(180deg, #FFFFFF 0%, #F6F7FF 100%);
  padding: 22px 16px;
  text-align: center;
  cursor: pointer;
  outline: none;
  transition: border-color .15s ease, box-shadow .15s ease, background .15s ease;
}
.km2-dropzone:hover{ border-color: var(--bm-primary); background: #F2F3FF; }
.km2-dropzone:focus{ border-color: var(--bm-primary); box-shadow: 0 0 0 4px rgba(79,70,229,0.18); }
.km2-dropzone.drag{ border-color: var(--bm-primary); background: #EAECFF; box-shadow: 0 0 0 4px rgba(79,70,229,0.22); }
.km2-dz-icon{ font-size: 1.9rem; color: var(--bm-primary); margin-bottom: 6px; }
.km2-dz-title{ font-weight: 800; color: var(--bm-fg); font-size: 0.98rem; }
.km2-dz-sub{ color: var(--bm-muted); font-size: 0.78rem; margin-top: 3px; line-height: 1.35; }
.km2-dz-or{ display: block; color: var(--bm-muted); font-weight: 700; font-size: 0.72rem; letter-spacing: .1em; margin: 12px 0 9px; }
.km2-dz-btn{
  display: inline-block; padding: 7px 20px; border-radius: 10px;
  background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-primary-2) 100%);
  color: #fff; font-weight: 700; font-size: 0.85rem; box-shadow: var(--bm-shadow-soft);
}
.km2-dropzone:hover .km2-dz-btn{ filter: brightness(1.07); }
.km2-dz-filename{ margin-top: 10px; font-size: 0.8rem; font-weight: 600; color: var(--bm-primary); min-height: 1em; word-break: break-all; }
.km2-dz-filename:empty{ display: none; }
/* keep the native Shiny file input present (for upload binding) but out of sight */
.km2-dz-fileinput{ position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0); border: 0; }

/* Confirmation banner after auto-digitization */
.km2-confirm-banner{
  display: flex; align-items: center; gap: 16px;
  background: linear-gradient(135deg, #EEF0FF 0%, #F3EEFF 60%, #E6FAFF 100%);
  border: 1px solid var(--bm-border); border-left: 6px solid var(--bm-primary);
  border-radius: var(--bm-radius); box-shadow: var(--bm-shadow-soft);
  padding: 16px 18px; margin-bottom: 16px;
  animation: km2-pop .25s ease;
}
@keyframes km2-pop{ from{ opacity: 0; transform: translateY(-6px);} to{ opacity: 1; transform: none;} }
.km2-cb-icon{
  flex: 0 0 auto; width: 46px; height: 46px; border-radius: 13px;
  display: flex; align-items: center; justify-content: center; font-size: 1.3rem; color: #fff;
  background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-accent) 100%);
  box-shadow: 0 6px 16px rgba(79,70,229,0.35);
}
.km2-cb-body{ flex: 1 1 auto; min-width: 200px; }
.km2-cb-title{ font-weight: 800; font-size: 1.05rem; color: var(--bm-fg); }
.km2-cb-text{ color: var(--bm-muted); font-size: 0.9rem; line-height: 1.4; margin-top: 2px; }
.km2-cb-actions{ flex: 0 0 auto; display: flex; flex-direction: column; gap: 8px; min-width: 210px; }
.km2-cb-actions .btn{ width: 100%; font-weight: 700; }
.km2-cb-actions .btn-success{ box-shadow: 0 6px 16px rgba(22,163,74,0.30); }

/* Fallback box highlight when revealed */
.km2-fallback{ border-color: var(--bm-primary) !important; box-shadow: 0 0 0 3px rgba(79,70,229,0.10), var(--bm-shadow-soft) !important; }
.km2-fallback h6{ color: var(--bm-primary) !important; }

/* Landing */
.km2-hero{ max-width: 980px; margin: 18px auto 8px; padding: 8px 12px 24px; }
.km2-hero-head{ text-align: center; margin-bottom: 26px; }
.km2-hero-title{ font-weight: 800; font-size: 2.0rem; margin-bottom: 6px;
  background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-primary-2) 60%, var(--bm-accent) 100%);
  -webkit-background-clip: text; background-clip: text; -webkit-text-fill-color: transparent; }
.km2-hero-sub{ color: var(--bm-muted); font-size: 1.02rem; max-width: 620px; margin: 0 auto; }
.km2-choices{ display: grid; grid-template-columns: 1fr 1fr; gap: 22px; }
@media (max-width: 760px){ .km2-choices{ grid-template-columns: 1fr; } }
.km2-choice{ position: relative; border: 1px solid var(--bm-border); border-radius: var(--bm-radius);
  background: var(--bm-card); box-shadow: var(--bm-shadow-soft); padding: 30px 26px 26px;
  text-align: center; cursor: pointer; outline: none;
  transition: transform .15s ease, box-shadow .15s ease, border-color .15s ease; }
.km2-choice:hover{ transform: translateY(-4px); box-shadow: var(--bm-shadow); border-color: var(--bm-primary); }
.km2-choice:focus{ box-shadow: 0 0 0 4px rgba(79,70,229,0.18), var(--bm-shadow); }
.km2-choice::before{ content:''; position:absolute; inset:0 0 auto 0; height:5px; border-radius: var(--bm-radius) var(--bm-radius) 0 0; }
.km2-choice-data::before{ background: linear-gradient(90deg, #16A34A, #22C55E); }
.km2-choice-fig::before{ background: linear-gradient(90deg, var(--bm-primary), var(--bm-accent)); }
.km2-choice-icon{ width: 64px; height: 64px; margin: 6px auto 14px; border-radius: 18px;
  display: flex; align-items: center; justify-content: center; font-size: 1.7rem; color: #fff; }
.km2-choice-data .km2-choice-icon{ background: linear-gradient(135deg, #16A34A 0%, #22C55E 100%); box-shadow: 0 8px 18px rgba(22,163,74,0.30); }
.km2-choice-fig .km2-choice-icon{ background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-accent) 100%); box-shadow: 0 8px 18px rgba(79,70,229,0.30); }
.km2-choice-title{ font-weight: 800; font-size: 1.22rem; color: var(--bm-fg); margin-bottom: 6px; }
.km2-choice-sub{ color: var(--bm-muted); font-size: 0.92rem; line-height: 1.45; min-height: 2.6em; margin-bottom: 10px; }
.km2-choice-formats{ font-size: 0.76rem; letter-spacing: .04em; color: var(--bm-muted); font-weight: 700; margin-bottom: 16px; }
.km2-choice-btn{ display: inline-block; padding: 9px 26px; border-radius: 11px; color: #fff; font-weight: 700; font-size: 0.92rem; }
.km2-choice-data .km2-choice-btn{ background: linear-gradient(135deg, #16A34A 0%, #22C55E 100%); }
.km2-choice-fig .km2-choice-btn{ background: linear-gradient(135deg, var(--bm-primary) 0%, var(--bm-primary-2) 100%); }
.km2-hero-secondary{ text-align: center; margin-top: 30px; }
.km2-hero-or{ display: block; color: var(--bm-muted); font-weight: 800; font-size: 0.72rem; letter-spacing: .12em; margin-bottom: 12px; }
.km2-hero-note{ color: var(--bm-muted); font-size: 0.84rem; margin-top: 12px; }

/* Accuracy bench interpretation legend */
.km2-acc-legend{ display: flex; flex-wrap: wrap; gap: 8px; margin-top: 12px; }
.km2-acc-band{ font-size: 0.82rem; color: var(--bm-fg); background: #FFFFFF;
  border: 1px solid var(--bm-border); border-left-width: 4px; border-radius: 8px; padding: 4px 10px; }
"

paste_js <- "
$(function(){
  // Paste an image from the clipboard (Ctrl+V / Cmd+V) anywhere, unless typing.
  $(document).on('paste', function(e){
    var oe = e.originalEvent || e;
    var cd = oe.clipboardData || window.clipboardData;
    if(!cd) return;
    var ae = document.activeElement;
    var tag = (ae && ae.tagName ? ae.tagName.toLowerCase() : '');
    if(tag === 'input' || tag === 'textarea' || (ae && ae.isContentEditable)) return;
    var items = cd.items || [];
    for(var i = 0; i < items.length; i++){
      if(items[i].kind === 'file' && items[i].type.indexOf('image') === 0){
        var blob = items[i].getAsFile();
        var reader = new FileReader();
        reader.onload = function(ev){
          if(window.Shiny && Shiny.setInputValue)
            Shiny.setInputValue('pasted_image', ev.target.result, {priority: 'event'});
        };
        reader.readAsDataURL(blob);
        e.preventDefault();
        return;
      }
    }
  });
});
"

# Landing: each big choice card opens its own hidden file picker (browse + drop).
choices_js <- "
$(function(){
  function wire(card){
    var fi = card.querySelector('input[type=file]');
    if(!fi) return;
    card.addEventListener('click', function(e){ if(e.target !== fi) fi.click(); });
    card.addEventListener('keydown', function(e){
      if((e.key === 'Enter' || e.key === ' ')){ e.preventDefault(); fi.click(); }
    });
    ['dragenter','dragover'].forEach(function(ev){
      card.addEventListener(ev, function(e){ e.preventDefault(); e.stopPropagation(); card.classList.add('drag'); });
    });
    ['dragleave','dragend','drop'].forEach(function(ev){
      card.addEventListener(ev, function(e){ e.preventDefault(); e.stopPropagation(); card.classList.remove('drag'); });
    });
    card.addEventListener('drop', function(e){
      var files = e.dataTransfer && e.dataTransfer.files;
      if(files && files.length){
        try { var dt = new DataTransfer(); dt.items.add(files[0]); fi.files = dt.files; } catch(err){}
        fi.dispatchEvent(new Event('change', {bubbles: true}));
      }
    });
  }
  var ds = document.getElementById('km2_choice_ds');
  var img = document.getElementById('km2_choice_img');
  if(ds) wire(ds);
  if(img) wire(img);
});
"

ui <- shiny::tagList(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(css_modern)),
    shiny::tags$script(shiny::HTML(paste_js)),
    shiny::tags$script(shiny::HTML(choices_js))
  ),
  bslib::page_sidebar(
    title = "KM2bayes Pro · guided BayeScores with accuracy bench",
    theme = bslib::bs_theme(
      version = 5, bootswatch = "flatly",
      bg = "#F5F7FB", fg = "#1A2233", primary = "#4F46E5", secondary = "#7C3AED",
      base_font = bslib::font_google("Inter"),
      heading_font = bslib::font_google("Inter"),
      "font-size-base" = "1.02rem"
    ),

    sidebar = bslib::sidebar(
      id = "main_sidebar",
      width = 380,
      open = "closed",
      shiny::h6("Digitization and instability analysis tool.", style = "color: var(--bm-muted); font-size: 0.95rem; margin-bottom: 18px;"),

      # ==============================================================================
      # SIDEBAR: EXTRACTION
      # ==============================================================================
      shiny::conditionalPanel(
        condition = "input.main_nav === 'Data Extraction' || input.main_nav === 'Stability metrics'",
        # At landing the entry lives in the main hero, so the sidebar stays out
        # of the way — just a slim hint.
        shiny::conditionalPanel(
          condition = "output.has_input != true",
          shiny::div(style = "color: var(--bm-muted); font-size: 0.92rem; padding: 6px 2px;",
                     shiny::icon("arrow-right"), shiny::HTML("&nbsp; Pick how to start on the right."))
        ),

        # Everything below is workspace control, shown only after something loads.
        shiny::conditionalPanel(
          condition = "output.has_input == true",
        shiny::h5("Settings", class = "mb-3 text-primary"),

        # ---- FALLBACK PATHS: hidden until auto-digitization is questioned ----
        shiny::conditionalPanel(
          condition = "output.show_fallback == true || output.is_image_mode != true",
          shiny::div(
            class = "km2-config-box km2-fallback",
            style = "padding: 14px; margin-bottom: 15px;",
            shiny::h6(shiny::HTML("<i class='fa fa-life-ring'></i>&nbsp; Other ways in"), style = "font-weight: 800; margin-bottom: 2px;"),
            shiny::helpText("Use these only if auto-digitization can't read the figure well."),
            shiny::actionButton("open_llm_modal", "Preprocess with an LLM", class = "btn-llm w-100 mb-2", icon = shiny::icon("robot")),
            shiny::actionButton("open_import_data", "Import Data (Text / LLM)", class = "btn-info w-100 mb-2", icon = shiny::icon("file-import")),
            shiny::hr(style = "margin:10px 0;"),
            shiny::h6("Axis Configuration", style = "font-weight: 800; margin-bottom: 10px;"),
            bslib::layout_columns(col_widths=c(4,4,4), shiny::numericInput("man_x_start","X Start",0), shiny::numericInput("man_x_end","X End",100), shiny::numericInput("man_x_inc","X Inc",20)),
            bslib::layout_columns(col_widths=c(4,4,4), shiny::numericInput("man_y_start","Y Start",0), shiny::numericInput("man_y_end","Y End",1), shiny::numericInput("man_y_inc","Y Inc",0.2,step=0.1)),
            bslib::layout_columns(col_widths=c(6,6), shiny::numericInput("man_num_curves","N Curves",2,min=1,max=5), shiny::checkboxInput("man_y_vert","Y Text Vert.",FALSE)),
            bslib::accordion(open=FALSE, bslib::accordion_panel("Image Settings",
                                                                shiny::numericInput("man_border","Border (px)",0,min=0,step=10),
                                                                shiny::sliderInput("man_brightness","Brightness (%)",50,150,130,5),
                                                                shiny::sliderInput("man_contrast","Contrast (%)",50,150,100,5),
                                                                shiny::numericInput("man_bg_light","BG Lightness",0.3,step=0.1),
                                                                shiny::checkboxInput("man_censoring","Detect Censoring",FALSE),
                                                                shiny::checkboxInput("man_enhance","Enhance Channels",FALSE)
            )),
            shiny::actionButton("run_manual_dig", "Digitize manually (point & click)", class="btn-primary w-100 mt-2", icon=shiny::icon("mouse-pointer")),
            shiny::actionButton("apply_edits","Analyze / Reconstruct", class="btn-primary w-100 mt-2", icon=shiny::icon("dna"))
          )
        ),

        shiny::actionButton("reset_all", "Reset All", class="btn-outline-danger w-100 mb-2", icon=shiny::icon("trash-alt"))
        )
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

        # Load saved model
        shiny::div(style="text-align: center; margin: 10px 0; color: #aaa; font-weight: bold; font-size: 0.8rem;", "- OR -"),
        shiny::fileInput("upload_model_rds", "Load Saved Model (.rds)", accept = ".rds"),

        shiny::hr(),
        shiny::downloadButton("dl_model_rds", "Download Model", class = "btn-secondary w-100 btn-sm")
      )
    ),

    bslib::navset_underline(
      id = "main_nav",

      # ==============================================================================
      # TAB 1: DATA EXTRACTION
      # ==============================================================================
      bslib::nav_panel("Data Extraction",
                       shiny::div(class="container-fluid py-3",

                                  # Landing: shown until a dataset or figure is loaded.
                                  shiny::conditionalPanel(
                                    condition = "output.has_input != true",
                                    shiny::div(class = "km2-hero",
                                      shiny::div(class = "km2-hero-head",
                                        shiny::h2(class = "km2-hero-title", "Start your analysis"),
                                        shiny::p(class = "km2-hero-sub", "Pick one. Bring your own patient data, or reconstruct it from a published Kaplan–Meier figure.")
                                      ),
                                      shiny::div(class = "km2-choices",
                                        # Dataset path
                                        shiny::div(id = "km2_choice_ds", class = "km2-choice km2-choice-data", tabindex = "0", role = "button",
                                          shiny::div(class = "km2-choice-icon", shiny::icon("table")),
                                          shiny::div(class = "km2-choice-title", "I have the data"),
                                          shiny::div(class = "km2-choice-sub", "Upload individual patient data and skip digitization entirely."),
                                          shiny::div(class = "km2-choice-formats", ".csv · .xlsx · .rds · .rda"),
                                          shiny::span(class = "km2-choice-btn", "Upload dataset"),
                                          shiny::div(class = "km2-dz-fileinput",
                                            shiny::fileInput("up_dataset", label = NULL, accept = c(".csv", ".xlsx", ".rds", ".rda"))
                                          )
                                        ),
                                        # Figure path
                                        shiny::div(id = "km2_choice_img", class = "km2-choice km2-choice-fig", tabindex = "0", role = "button",
                                          shiny::div(class = "km2-choice-icon", shiny::icon("chart-line")),
                                          shiny::div(class = "km2-choice-title", "I have a figure"),
                                          shiny::div(class = "km2-choice-sub", "Upload a two-arm KM image and we digitize the curves and numbers at risk."),
                                          shiny::div(class = "km2-choice-formats", ".png · .jpg · paste with Ctrl+V"),
                                          shiny::span(class = "km2-choice-btn", "Upload figure"),
                                          shiny::div(class = "km2-dz-fileinput",
                                            shiny::fileInput("up_image", label = NULL, accept = c("image/png", "image/jpeg", ".png", ".jpg"))
                                          )
                                        )
                                      ),
                                      # ---- Secondary options below ----
                                      shiny::div(class = "km2-hero-secondary",
                                        shiny::span(class = "km2-hero-or", "OR TRY A BUILT-IN EXAMPLE"),
                                        shiny::div(style = "max-width: 460px; margin: 0 auto;",
                                          shiny::selectInput("example_dataset_hero", label = NULL, choices = c("Select an example dataset…" = ""), width = "100%")
                                        ),
                                        shiny::p(class = "km2-hero-note",
                                                 shiny::HTML("Need LLM cleaning or manual point-and-click? Those advanced paths live in the <b>sidebar</b> on the left."))
                                      )
                                    )
                                  ),

                                  # Workspace: appears once a dataset or figure is loaded.
                                  shiny::conditionalPanel(
                                    condition = "output.has_input == true",
                                  # Prompt to digitize once an image is loaded.
                                  shiny::conditionalPanel(
                                    condition = "output.is_image_mode == true && output.auto_done != true && output.has_fit != true",
                                    shiny::div(class = "km2-confirm-banner",
                                      shiny::div(class = "km2-cb-icon", shiny::icon("wand-magic-sparkles")),
                                      shiny::div(class = "km2-cb-body",
                                        shiny::div(class = "km2-cb-title", "Figure loaded"),
                                        shiny::div(class = "km2-cb-text",
                                                   shiny::HTML("Two-arm Kaplan&ndash;Meier figure. We'll extract both curves and the numbers at risk, then let you review and confirm before anything is fit."))
                                      ),
                                      shiny::div(class = "km2-cb-actions",
                                        shiny::actionButton("run_auto_preprocess", shiny::HTML("<b>Auto-digitize this figure</b>"), class = "btn-auto", icon = shiny::icon("wand-magic-sparkles"))
                                      )
                                    )
                                  ),
                                  # ---- Confirmation banner shown right after auto-digitization ----
                                  shiny::conditionalPanel(
                                    condition = "output.auto_done == true && output.has_fit != true",
                                    shiny::div(class = "km2-confirm-banner",
                                      shiny::div(class = "km2-cb-icon", shiny::icon("circle-check")),
                                      shiny::div(class = "km2-cb-body",
                                        shiny::div(class = "km2-cb-title", "Curves read — review the numbers at risk"),
                                        shiny::div(class = "km2-cb-text",
                                                   shiny::HTML("Check the <b>Numbers at Risk</b> in the grid below and <b>edit any cell by hand</b> if needed. When they look right, hit <b>Confirm &amp; Analyze</b>. Want the LLM or manual point-and-click instead? Open the extra tools."))
                                      ),
                                      shiny::div(class = "km2-cb-actions",
                                        shiny::actionButton("confirm_analyze", "Confirm & Analyze", class = "btn-success", icon = shiny::icon("circle-check")),
                                        shiny::actionButton("report_problem", "Other tools (LLM / manual)", class = "btn-outline-secondary", icon = shiny::icon("screwdriver-wrench"))
                                      )
                                    )
                                  ),
                                  bslib::layout_columns(col_widths=c(7,5),
                                                        bslib::card(
                                                          bslib::card_header(
                                                            shiny::div(class="d-flex justify-content-between align-items-center",
                                                                       shiny::uiOutput("left_panel_title", inline = TRUE)
                                                            )
                                                          ),
                                                          shiny::conditionalPanel("output.has_fit == true",
                                                                                  shiny::plotOutput("km_plot_output", height="520px")),
                                                          shiny::conditionalPanel("output.has_fit != true",
                                                                                  shiny::imageOutput("clean_image_output", height="520px")),
                                                          bslib::card_footer(
                                                            shiny::div(class="d-flex justify-content-between align-items-center",
                                                              shiny::actionButton("switch_arms","Switch Arms", class="btn-outline-secondary btn-sm", icon=shiny::icon("exchange-alt")),
                                                              shiny::conditionalPanel("output.has_fit == true",
                                                                shiny::downloadButton("dl_composite_png", "Download comparison PNG", class="btn-secondary btn-sm", icon=shiny::icon("images"))
                                                              )
                                                            )
                                                          )
                                                        ),
                                                        shiny::div(
                                                          shiny::conditionalPanel(
                                                            condition = "output.show_secondary_image == true",
                                                            bslib::card(bslib::card_header(shiny::uiOutput("right_panel_title", inline = TRUE)), shiny::imageOutput("original_image_output", height="420px"), style="background-color: rgba(255,255,255,0.55); margin-bottom: 10px;")
                                                          ),
                                                          shiny::conditionalPanel(
                                                            condition = "output.show_dataset_preview == true",
                                                            bslib::card(
                                                              bslib::card_header(
                                                                shiny::div(class="d-flex justify-content-between align-items-center",
                                                                           shiny::span("Dataset Preview"),
                                                                           shiny::div(
                                                                             shiny::downloadButton("dl_ipd_excel", "Excel (.xlsx)", class="btn-ipd-download", icon=shiny::icon("file-excel")),
                                                                             shiny::downloadButton("dl_ipd_rda", "R Data (.rda)", class="btn-ipd-download", icon=shiny::icon("file-code"), style="margin-left: 5px;")
                                                                           )
                                                                )
                                                              ),
                                                              shiny::div(style = "overflow-x: auto;", shiny::tableOutput("head_ipd_table"))
                                                            )
                                                          )
                                                        )
                                  ),
                                  shiny::conditionalPanel(
                                    condition = "output.show_risk_grid == true",
                                    bslib::layout_columns(col_widths=6,
                                                          bslib::card(
                                                            bslib::card_header(shiny::div(class="d-flex justify-content-between align-items-center",
                                                              shiny::span("Data Correction"),
                                                              shiny::conditionalPanel(
                                                                condition = "output.has_fit == true",
                                                                shiny::actionButton("apply_grid_edits", "Apply edits & re-analyze", class="btn-primary btn-sm", icon=shiny::icon("rotate"))
                                                              )
                                                            )),
                                                            rhandsontable::rHandsontableOutput("hot_risk_table"), shiny::div(style="height:10px"), rhandsontable::rHandsontableOutput("hot_y_axis")
                                                          )
                                    )
                                  )
                                  )
                       )
      ),

      # ==============================================================================
      # How it works: a plain-language guide to the whole workflow.
      # ==============================================================================
      bslib::nav_panel("How it works",
        shiny::div(class = "container-fluid py-3", style = "max-width: 900px; margin: 0 auto;",

          bslib::card(
            bslib::card_header("What this tool does"),
            bslib::card_body(shiny::HTML(
              "<p>KM2bayes Pro takes a two-arm survival comparison and turns it into
               patient-level data, then fits a Bayesian cure model on top of it. You can
               start from your own trial dataset or from a published Kaplan-Meier figure.
               Everything after that first step - the accuracy check, the stability
               read-out, the model - runs on the same reconstructed data, so the quality
               of the input is what really decides the quality of the answer.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("Best case: start from the data"),
            bslib::card_body(shiny::HTML(
              "<p>If you are the sponsor, a collaborator, or you otherwise hold the
               individual patient data, use the left option on the home screen and upload
               it. This is always the better route: there is nothing to reconstruct and
               nothing to approximate, so the curves, the hazard ratio and the medians are
               exact.</p>
               <p>Three columns are enough: <b>time</b>, <b>event</b> (1 for the event,
               0 for censored) and <b>arm</b>. CSV, Excel, RDS and RDA all work, and the
               column names can be in your own wording (time / months / os, status /
               dead, group / treatment, and so on). When you upload data the app skips
               digitization entirely and goes straight to the analysis.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("Reading it from a figure"),
            bslib::card_body(shiny::HTML(
              "<p>When you only have the published figure, upload a clean image of a
               two-arm Kaplan-Meier plot that also shows the numbers-at-risk table
               underneath. The engine reads both curves and the at-risk counts, and from
               those it rebuilds the individual patients with the standard Guyot method.
               The numbers at risk are what pin down how many events happen and when, so a
               figure without them can only be guessed at.</p>
               <p>It works best with two curves in clearly different colours, legible axis
               numbers, and as little clutter as possible over the plotting area.
               Confidence bands are fine. In-plot legends, annotation lines and low
               resolution make it harder.</p>
               <p>After you press <b>Auto-digitize</b>, check the result before you trust
               it. The reconstructed curves are drawn on top of your image so you can see
               whether they follow the real lines, and the numbers at risk are pre-filled
               in an editable grid. Fix any cell the reader got wrong, then press
               <b>Confirm and Analyze</b>.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("When the figure does not read cleanly"),
            bslib::card_body(shiny::HTML(
              "<p>Some figures are hard for an automatic reader: a grey curve, a busy
               panel, two very similar colours, an in-plot legend. Two fallbacks cover
               those cases.</p>
               <ul>
                 <li><b>Manual point-and-click.</b> You place the points along each curve
                 yourself. Slower, but it always works and you stay in control.</li>
                 <li><b>LLM preprocess.</b> Open the prompt, hand your figure to any vision
                 model, and it returns a cleaned version with the curves recoloured and the
                 legend and annotations stripped out, plus the four data rows. Upload that
                 cleaned image and the automatic reader handles it easily. This is usually
                 the quickest route for a difficult figure.</li>
               </ul>
               <p>Neither is a trick to rescue a bad reconstruction. They are just two ways
               to hand the engine a clean input.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("Accuracy bench: can I trust this reconstruction?"),
            bslib::card_body(shiny::HTML(
              "<p>This tab answers exactly that. Paste the hazard ratio with its confidence
               interval, and the medians, as the paper reports them. The app compares them
               against what your reconstruction produces and shows the error live.</p>
               <p>A hazard ratio within <b>5%</b> of the published value is as good as
               indistinguishable; within <b>10%</b> is fine for most uses; beyond that,
               revise the numbers at risk or re-digitize before you rely on it. If a median
               was not reached, leave it blank and the hazard-ratio check still works. Run
               this before you lean on anything downstream.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("Stability metrics: how much can I believe?"),
            bslib::card_body(shiny::HTML(
              "<p>The stability tab tells you how mature the data is and how far you can
               push the model. It reports the follow-up relative to the median, the number
               of events, and a few instability measures, and it plots the fitted cure
               model against the Kaplan-Meier curve.</p>
               <p>Short follow-up or few events means the tail of the curve, and therefore
               the cure fraction, is poorly determined. The read-out flags that and points
               you to a matching tail assumption for the Bayesian step, and it helps you
               decide whether the two arms should share one shape or get a shape each.
               Think of it as the check that stops you over-reading an immature curve.</p>"
            ))
          ),

          bslib::card(
            bslib::card_header("The Bayesian model and how to read it"),
            bslib::card_body(shiny::HTML(
              "<p>The model is a <b>mixture cure model</b>. It splits each arm into a
               fraction of patients who are effectively cured (their risk of the event
               flattens out) and the rest, whose survival follows a Weibull distribution.
               Fitting it the Bayesian way means every quantity comes back as a full
               posterior, so you get honest uncertainty instead of a single point.</p>
               <p>What to look at:</p>
               <ul>
                 <li><b>Cure fraction.</b> The estimated share left long-term event-free in
                 each arm, with its credible interval. This is the number the whole model
                 exists to produce.</li>
                 <li><b>Posterior densities.</b> The plausible range for each parameter.
                 Narrow means confident, wide means uncertain, and the overlap between arms
                 tells you how separable they really are.</li>
                 <li><b>Model fit.</b> The predicted curve over the Kaplan-Meier. Close
                 tracking means the model describes your data; a gap in the tail sends you
                 back to the tail assumption and the stability tab.</li>
                 <li><b>Diagnostics.</b> Convergence (R-hat near 1, no divergences). If
                 these look off the estimates are not settled yet, so rerun with more
                 iterations.</li>
               </ul>
               <p>The <b>tail assumption</b> you set on the left is your prior belief about
               long-term behaviour. On mature data it barely moves the result; on immature
               data it does the heavy lifting, which is precisely why the stability tab is
               there to guide the choice.</p>"
            ))
          )
        )
      ),

      # ==============================================================================
      # Accuracy bench: compare the reconstruction against published values.
      # ==============================================================================
      bslib::nav_panel("Accuracy bench",
        shiny::div(class = "container-fluid py-3",
          shiny::conditionalPanel(
            condition = "output.has_fit != true",
            bslib::card(bslib::card_body(
              shiny::div(style = "text-align:center; color: var(--bm-muted); padding: 30px;",
                shiny::icon("flask"), shiny::HTML("&nbsp; Load data or digitize a figure and run the analysis first. Then come back here to score the reconstruction against the published numbers.")
              )
            ))
          ),
          shiny::conditionalPanel(
            condition = "output.has_fit == true",
            bslib::layout_columns(col_widths = c(5, 7),
              bslib::card(
                bslib::card_header("Published (real) values"),
                bslib::card_body(
                  shiny::helpText("Paste the values reported in the paper. Leave blank what you don't have."),
                  shiny::h6("Hazard ratio", style = "font-weight:800; margin-top:4px;"),
                  bslib::layout_columns(col_widths = c(4,4,4),
                    shiny::numericInput("real_hr", "HR", value = NA, min = 0, step = 0.01),
                    shiny::numericInput("real_hr_lo", "CI low", value = NA, min = 0, step = 0.01),
                    shiny::numericInput("real_hr_hi", "CI high", value = NA, min = 0, step = 0.01)
                  ),
                  shiny::hr(),
                  shiny::h6("Median survival", style = "font-weight:800;"),
                  bslib::layout_columns(col_widths = c(6,6),
                    shiny::numericInput("real_med1", "Median · higher-survival arm", value = NA, min = 0, step = 0.1),
                    shiny::numericInput("real_med2", "Median · lower-survival arm", value = NA, min = 0, step = 0.1)
                  ),
                  shiny::helpText(shiny::HTML("Enter each published median by the <b>arm's survival</b>, not by the paper's group order — the tool auto-swaps arms internally, so it compares each median against the reconstructed arm with matching survival. Leave a median blank if it was <b>not reached (NR)</b> — common for high-survival curves that stay above 50% (e.g. adjuvant DFS). The HR comparison still works without it. Updates live as you type."))
                )
              ),
              bslib::card(
                bslib::card_header("Reconstructed vs real"),
                bslib::card_body(
                  shiny::div(class = "metrics-table-container", shiny::tableOutput("accuracy_table")),
                  shiny::div(style = "margin-top:10px;", shiny::uiOutput("accuracy_verdict")),
                  shiny::div(class = "km2-acc-legend",
                    shiny::span(class = "km2-acc-band", style = "border-color:#16A34A;",
                                shiny::tags$b(style="color:#16A34A;", "≤ 5%"), " Excellent"),
                    shiny::span(class = "km2-acc-band", style = "border-color:#F59E0B;",
                                shiny::tags$b(style="color:#B45309;", "5–10%"), " Acceptable"),
                    shiny::span(class = "km2-acc-band", style = "border-color:#D32F2F;",
                                shiny::tags$b(style="color:#D32F2F;", "> 10%"), " Poor")
                  ),
                  shiny::helpText(shiny::HTML("Bands apply to the <b>HR relative error</b>. A reconstruction within <b>10%</b> of the published HR is considered acceptable for digitized IPD; under 5% is essentially indistinguishable. The 95% CI check is a secondary sanity test."))
                )
              )
            )
          )
        )
      ),

      # ==============================================================================
      # TAB 2: FORENSIC ANALYSIS
      # ==============================================================================
      bslib::nav_panel("Stability metrics",
                       shiny::div(class="container-fluid py-3",
                                  shiny::div(class = "d-flex justify-content-end mb-2",
                                             shiny::downloadButton("dl_stability_html", "Download stability report (.html)", class = "btn-sm btn-outline-secondary")),
                                  # INSTRUCTIONS BOX
                                  bslib::card(
                                    bslib::card_header("Instructions for Bayesian Model Specification"),
                                    shiny::div(style = "background-color: var(--bm-accent-soft); padding: 15px; border-left: 5px solid var(--bm-primary); color: var(--bm-fg);",
                                               shiny::tags$ol(
                                                 shiny::tags$li(shiny::strong("Check Digitization Accuracy:"), " Review the calibration plot below. If the Mixture Cure model (dashed red) deviates substantially from the Kaplan-Meier curve (solid black), the digitization may need refinement."),
                                                 shiny::tags$li(shiny::strong("Check Stability Metrics:"), " Use the Instability Check table and Interpretation Suggestions to decide on 'Tail Assumption' setting in the Bayesian model (e.g., 'immature_skeptical' for AFT-only models)."),
                                                 shiny::tags$li(shiny::strong("Check Calibration for Shared Shape:"), " Compare MAE and AIC. If MAE differs substantially (> 3.0x) ", shiny::strong("OR"), " if the Free Shape AIC is lower by > 10 points, consider ", shiny::strong("unchecking 'Shared Shape'"), " to allow independent shape parameters. ", shiny::strong("Crucially, examine the 2x2 calibration plots below."), " There must be clear visual evidence that the 'Free Shape' model fits better (blue line vs red line). Be cautious of over-parameterization; only enable Free Shape if the visual improvement is distinct and justifies the added complexity.")
                                               )
                                    ),
                                  ),

                                  # HESSIAN WARNING UI
                                  shiny::div(class = "mb-3", shiny::uiOutput("hessian_warning_ui")),

                                  bslib::card(
                                    bslib::card_header("Instability Check"),
                                    shiny::div(class = "metrics-table-container", style = "overflow-x: auto; max-height: 600px; overflow-y: auto;",
                                               shiny::div(class = "d-flex justify-content-center gap-3 mb-2",
                                                          shiny::span(style="background: var(--bm-accent-soft); color: var(--bm-fg); padding: 4px 12px; border-radius: 8px; font-weight: 600; border-left: 4px solid var(--bm-primary);", "Sample Info"),
                                                          shiny::span(style="background: #F3EEFF; color: var(--bm-fg); padding: 4px 12px; border-radius: 8px; font-weight: 600; border-left: 4px solid var(--bm-primary-2);", "Events"),
                                                          shiny::span(style="background: #E6FAFF; color: var(--bm-fg); padding: 4px 12px; border-radius: 8px; font-weight: 600; border-left: 4px solid var(--bm-accent);", "Instability Metrics")
                                               ),
                                               shiny::tableOutput("metrics_summary_table"))
                                  ),

                                  shiny::div(class = "mb-3", shiny::uiOutput("calibration_warning_ui")),

                                  bslib::card(bslib::card_header("Calibration: KM vs Mixture Cure (Shared vs Free)"),
                                              shiny::plotOutput("calib_plot_output", height="600px")),

                                  bslib::card(
                                    bslib::card_header("Interpretation Suggestions"),
                                    shiny::div(style = "background-color: #FBFAFF; padding: 15px; border-left: 5px solid var(--bm-primary-2); color: var(--bm-fg);",
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
            bslib::card_header(
              shiny::div(class="d-flex justify-content-between align-items-center",
                shiny::span("Model Summary"),
                shiny::downloadButton("dl_model_summary_md", "Download .md", class="btn-xs btn-outline-secondary")
              )
            ),
            shiny::verbatimTextOutput("model_summary"),
            style = "background: linear-gradient(180deg, rgba(30,90,168,0.06) 0%, rgba(255,255,255,1) 65%); max-height: 320px; overflow-y: auto;"
          ),
          bslib::card(
            bslib::card_header("Model Visualizations"),
            bslib::navset_card_tab(
              bslib::nav_panel(
                "Posterior Densities",
                shiny::div(class = "text-end mb-2", shiny::actionButton("btn_open_pdf_dens", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_densities", height = "420px")
              ),
              bslib::nav_panel(
                "Correlated Densities",
                shiny::div(class = "text-end mb-2", shiny::actionButton("btn_open_pdf_corr", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_correlated", height = "620px")
              ),
              bslib::nav_panel(
                "Model Fit",
                shiny::div(class = "text-end mb-2", shiny::actionButton("btn_open_pdf_fit", "PDF", class = "btn-xs btn-outline-secondary")),
                shiny::plotOutput("plot_model_fit", height = "620px"),
                shiny::div(style="text-align: center; color: #666; font-size: 0.9rem; margin-top: 10px;",
                           "Black solid line: Kaplan-Meier | Red dashed line: Bayesian Model Prediction")
              ),
              bslib::nav_panel(
                "Diagnostics",
                shiny::div(class = "d-flex justify-content-end gap-2 mb-2",
                           shiny::downloadButton("dl_draws_csv", "MCMC Draws (CSV)", class = "btn-xs btn-warning"),
                           shiny::actionButton("btn_open_pdf_diag", "Download Diagnostic Plot (PDF)", class = "btn-xs btn-outline-danger")
                ),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  shiny::div(shiny::h6("Convergence Table"), shiny::verbatimTextOutput("text_diagnostics_table")),
                  shiny::div(shiny::h6("Diagnostic Plot"), shiny::plotOutput("plot_diagnostics", height = "260px"))
                )
              ),
              bslib::nav_panel("R Code (Reproduction)",
                shiny::div(class = "text-end mb-2", shiny::downloadButton("dl_repro_code", "Download .R", class = "btn-xs btn-outline-secondary")),
                shiny::verbatimTextOutput("repro_code"))
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

  vals <- shiny::reactiveValues(
    final_ipd=NULL, fit_obj=NULL, cox_obj=NULL,
    risk_table_editable=NULL, y_axis_editable=NULL,
    manual_raw_data=NULL, mode="manual",
    original_img_path=NULL, processed_img_path=NULL, overlay_img_path=NULL,
    auto_clean_path=NULL,
    analysis_results_full=NULL, analysis_summary_view=NULL,
    cure_model_obj=NULL, master_data_loaded=NULL, rmst_details=NULL,
    curve_mapping=NULL, calib_data=NULL,
    model_fit_obj=NULL, code_text=NULL,
    interpretation_html=NULL,
    reco_tail=NULL, reco_shared_shape=NULL,
    auto_done=FALSE, show_fallback=FALSE,
    calibration_warning_html=NULL,
    hessian_warning_shared=FALSE,
    hessian_warning_free=FALSE,
    hessian_warning_html=NULL
  )

  # ==============================================================================
  # IMAGE PRE-PROCESSING
  #   Path A: automatic engine (bundled Python digitizer via reticulate)
  #   Path B: any vision LLM (generic prompt, no vendor lock-in)
  #   Both are optional; the editable grid always stays open for rectification.
  # ==============================================================================

  # LLM prompt: clean the image and extract the four data lines
  llm_prompt_text <- paste0(
    "Process this clinical trial plot image to perform two specific tasks. ",
    "First, create a modified version of the image that cleans the plot. ",
    "Preserve strictly the main data curves (including step-lines and censorship ticks), ",
    "the X and Y axis lines, and the numeric values (tick labels) on both axes. ",
    "Change the curve colors to dark red and dark blue for improved contrast. ",
    "Remove completely all text content (including titles, axis names/labels, legends, ",
    "hazard ratios, and p-values), the entire 'Numbers at risk' table below the X-axis, ",
    "and any annotation markers such as arrows, brackets, or dashed/solid reference lines ",
    "indicating medians or milestones that are not part of the main data curves. ",
    "The final visual output should be a clean plot showing only the axis geometry, ",
    "the axis numbers, and the data curves on a white background. ",
    "Second, extract the numerical data from the original image into exactly 4 lines of plain text. ",
    "Line 1: Y-axis values. Line 2: X-axis values. Line 3: Top row of the 'Numbers at risk' table. ",
    "Line 4: Bottom row of the 'Numbers at risk' table. ",
    "Output ONLY numbers separated by spaces for each line. ",
    "Do not include labels, commas, or any additional text. ",
    "Crucially, do not repeat the data. ",
    "Stop your output immediately after extracting the final number from the bottom row."
  )

  # Expose whether we are in image mode (controls the pre-processing buttons in the sidebar)
  output$is_image_mode <- shiny::reactive({
    !is.null(vals$mode) && vals$mode == "manual" && !is.null(vals$original_img_path)
  })
  shiny::outputOptions(output, "is_image_mode", suspendWhenHidden = FALSE)

  # Flow state: the confirmation banner and the (hidden) fallback panel.
  output$auto_done <- shiny::reactive({ isTRUE(vals$auto_done) })
  shiny::outputOptions(output, "auto_done", suspendWhenHidden = FALSE)
  output$show_fallback <- shiny::reactive({ isTRUE(vals$show_fallback) })
  shiny::outputOptions(output, "show_fallback", suspendWhenHidden = FALSE)

  # Confirm & Analyze: accept the auto-loaded numbers and run the whole pipeline.
  shiny::observeEvent(input$confirm_analyze, {
    vals$auto_done <- FALSE
    if (!is.null(input$hot_risk_table)) {
      rt <- rhandsontable::hot_to_r(input$hot_risk_table)
      for (col in c("N_Risk_G1", "N_Risk_G2"))
        if (!is.null(rt[[col]])) rt[[col]] <- round(as.numeric(rt[[col]]))
      vals$risk_table_editable <- rt
    }
    if (!is.null(input$hot_y_axis)) vals$y_axis_editable <- rhandsontable::hot_to_r(input$hot_y_axis)
    run_core_analysis()
  })

  # Report a problem: reveal the LLM and manual fallback options.
  shiny::observeEvent(input$report_problem, {
    vals$show_fallback <- TRUE
    vals$auto_done <- FALSE
    bslib::sidebar_toggle("main_sidebar", open = TRUE)
    shiny::showNotification(
      "Opened the tools sidebar on the left — LLM, manual point-and-click, axis settings and import are there.",
      type = "message", duration = 8)
  })

  # ---- Path B: LLM modal ----
  shiny::observeEvent(input$open_llm_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Image Preprocessing with an LLM",
      size = "l",
      shiny::div(
        shiny::p("Use any vision-capable LLM to clean your KM plot and extract the numbers. Open your preferred tool, upload the image, and paste the prompt below."),
        shiny::div(class = "d-flex gap-2 mb-3",
          shiny::a("ChatGPT", href="https://chatgpt.com/", target="_blank", class="btn btn-outline-primary w-100"),
          shiny::a("Claude", href="https://claude.ai/", target="_blank", class="btn btn-outline-primary w-100"),
          shiny::a("Gemini", href="https://gemini.google.com/app", target="_blank", class="btn btn-outline-primary w-100")
        ),
        shiny::hr(),
        shiny::h6("Suggested prompt:", style="font-weight: 800; margin-bottom: 10px;"),
        shiny::textAreaInput("llm_prompt_display", label=NULL, height="260px", value=llm_prompt_text, width="100%"),
        shiny::helpText("Save the cleaned image, upload it here, and paste the 4 returned lines into 'Import Data (Text/LLM)'.")
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  # ---- Automatic engine ----

  # Locate the bundled Python digitizer
  kmdig_script_path <- function() {
    p <- system.file("python", "kmdig3.py", package = "bystools")
    if (nzchar(p) && file.exists(p)) return(p)
    cand <- c(here::here("inst", "python", "kmdig3.py"), file.path("inst", "python", "kmdig3.py"))
    cand <- cand[file.exists(cand)]
    if (length(cand)) cand[1] else ""
  }

  # Show install help listing ONLY what is actually missing.
  #   reticulate_missing : TRUE if the reticulate R package itself is absent
  #   missing_modules    : character vector of missing Python modules (cv2, ...)
  #   tesseract_missing  : TRUE if the Tesseract OCR engine is not reachable
  show_python_install_modal <- function(reticulate_missing = FALSE,
                                        missing_modules = character(0),
                                        tesseract_missing = TRUE) {
    pip_map <- c(cv2 = "opencv-python-headless", numpy = "numpy",
                 pytesseract = "pytesseract", sklearn = "scikit-learn")
    steps <- list()

    if (reticulate_missing) {
      steps <- c(steps, list(
        shiny::h6("Install the R bridge (reticulate)", style = "font-weight: 800;"),
        shiny::tags$pre("install.packages('reticulate')\nreticulate::install_miniconda()")
      ))
    }

    if (length(missing_modules)) {
      pkgs <- unname(pip_map[missing_modules]); pkgs <- pkgs[!is.na(pkgs)]
      cmd <- paste0("reticulate::py_install(c(",
                    paste(sprintf("'%s'", pkgs), collapse = ", "),
                    "), pip = TRUE, ignore_installed = TRUE)")
      steps <- c(steps, list(
        shiny::h6(paste0("Install the Python libraries (missing: ",
                         paste(missing_modules, collapse = ", "), ")"),
                  style = "font-weight: 800;"),
        shiny::div(
          style = "background:#EEF0FF; border-left:4px solid #4F46E5; padding:10px 14px; border-radius:8px;",
          shiny::tags$b("Recommended (one command):"),
          shiny::tags$pre("bystools::km2_setup_python()"),
          shiny::tags$small("Sets up an isolated Python environment. Restart R afterwards.")
        ),
        shiny::tags$div(style = "margin-top:6px;", shiny::tags$small("Manual alternative:")),
        shiny::tags$pre(cmd)
      ))
    }

    if (tesseract_missing) {
      steps <- c(steps, list(
        shiny::h6("Install the Tesseract OCR engine", style = "font-weight: 800;"),
        shiny::div(
          style = "background:#E8FBF3; border-left:4px solid #16A34A; padding:10px 14px; border-radius:8px; margin-bottom:8px;",
          shiny::tags$b("Recommended (downloads and launches the installer):"),
          shiny::tags$pre("bystools::km2_install_tesseract()"),
          shiny::tags$small("Click through the installer with default settings.")
        ),
        shiny::div(
          style = "background:#EEF0FF; border-left:4px solid #4F46E5; padding:10px 14px; border-radius:8px;",
          shiny::tags$b("Manual installation:"),
          shiny::tags$ol(
            style = "margin-bottom:6px;",
            shiny::tags$li(shiny::a("Open the Tesseract download page", href = "https://digi.bib.uni-mannheim.de/tesseract/", target = "_blank"),
                           " and click the newest file named ", shiny::tags$code("tesseract-ocr-w64-setup-5.x.x.exe"), " (64-bit)."),
            shiny::tags$li("Double-click the file you downloaded. If Windows shows ", shiny::tags$i("“Windows protected your PC”"),
                           ", click ", shiny::tags$b("More info"), " then ", shiny::tags$b("Run anyway"), " (it is a trusted university build)."),
            shiny::tags$li("In the installer press ", shiny::tags$b("Next / I Agree / Next"), " on every screen. ",
                           shiny::tags$b("Do not change the install folder"), "."),
            shiny::tags$li("Press ", shiny::tags$b("Install"), ", then ", shiny::tags$b("Finish"), "."),
            shiny::tags$li("Restart R and press ", shiny::tags$b("Auto-clean"), " again.")
          ),
          shiny::tags$small(shiny::HTML("Default folder is <code>C:\\Program Files\\Tesseract-OCR</code>; the app auto-detects it, so you never need to edit the Windows PATH."))
        ),
        shiny::tags$div(style = "margin-top:6px;",
          shiny::tags$small(shiny::HTML("macOS: <code>brew install tesseract</code> &nbsp;|&nbsp; Linux: <code>sudo apt-get install tesseract-ocr</code>"))
        )
      ))
    }

    shiny::showModal(shiny::modalDialog(
      title = "Automatic engine: install only what's missing",
      size = "l",
      shiny::div(
        shiny::div(
          style = "background:#E8FBF3; border-left:4px solid #16A34A; padding:12px 16px; border-radius:10px; margin-bottom:14px;",
          shiny::tags$b("Install everything in one command, then restart R."),
          shiny::tags$pre("bystools::km2_install_all()"),
          shiny::tags$small("Installs the R packages, the Python engine and Tesseract. The steps below are the manual alternative.")
        ),
        shiny::p("Anything already installed is not listed. After installing, restart R and press the button again."),
        shiny::tagList(steps)
      ),
      footer = shiny::modalButton("Close")
    ))
  }

  shiny::observeEvent(input$run_auto_preprocess, {
    shiny::req(vals$original_img_path)
    vals$auto_done <- FALSE                      # banner reappears only when this run finishes
    vals$auto_clean_path <- NULL

    # 1. reticulate present?
    if (!requireNamespace("reticulate", quietly = TRUE)) {
      show_python_install_modal(reticulate_missing = TRUE)
      return()
    }
    # 2. select a Python interpreter that has the required modules. kmdig.py is run
    #    as a separate process with this interpreter, independent of the Python the
    #    R session is bound to.
    py <- NULL
    if (isTRUE(tryCatch(.km2_env_ready("r-bystools"), error = function(e) FALSE))) {
      py <- .km2_env_python("r-bystools")
    } else {
      # dedicated env not ready: check whether the session interpreter has the modules
      needed <- c("cv2", "numpy", "pytesseract", "sklearn")
      sess_missing <- tryCatch(
        needed[!vapply(needed, function(m) reticulate::py_module_available(m), logical(1))],
        error = function(e) needed
      )
      if (length(sess_missing)) {
        show_python_install_modal(missing_modules = sess_missing, tesseract_missing = TRUE)
        return()
      }
      py <- tryCatch(reticulate::py_exe(), error = function(e) NULL)  # session env has them
    }
    if (is.null(py) || !file.exists(py)) {
      show_python_install_modal(missing_modules = c("cv2", "numpy", "pytesseract", "sklearn"),
                                tesseract_missing = TRUE)
      return()
    }
    # 3. script present?
    script <- kmdig_script_path()
    if (!nzchar(script)) {
      shiny::showNotification("kmdig.py not found in the package (inst/python).", type = "error")
      return()
    }

    id_auto <- shiny::showNotification("Auto-cleaning image with the digitizer engine... please wait.", type = "message", duration = NULL)
    tryCatch({
      prefix <- file.path(temp_dir, paste0("auto_", as.integer(Sys.time())))
      # Run kmdig3.py in a separate process with the env's own python. The "N Curves"
      # control drives the number of colour clusters, so 3+ arm plots work too.
      ncurves <- tryCatch(max(1L, as.integer(input$man_num_curves)), error = function(e) 2L)
      if (is.na(ncurves)) ncurves <- 2L
      out <- suppressWarnings(system2(
        py, args = shQuote(c(script, vals$original_img_path, prefix, as.character(ncurves))),
        stdout = TRUE, stderr = TRUE
      ))
      meta_file <- paste0(prefix, "_meta.json")
      clean_png <- paste0(prefix, "_clean.png")
      if (!file.exists(meta_file)) {
        # propagate the python error so it can be routed below
        stop(paste(out, collapse = "\n"))
      }
      meta <- jsonlite::fromJSON(meta_file, simplifyVector = FALSE)

      # Two-arm studies only: reject 3+ arm figures up front.
      if (isTRUE(meta$too_many_arms)) {
        shiny::removeNotification(id = id_auto)
        vals$auto_done <- FALSE; vals$show_fallback <- TRUE
        shiny::showNotification(
          sprintf("This figure looks like a %s-arm study. KM2bayes Pro supports two-arm studies only — please use a 2-arm figure, or try the other options in the sidebar.",
                  tryCatch(as.integer(meta$arms_detected), error = function(e) 3L)),
          type = "error", duration = 12)
        return()
      }

      if (file.exists(clean_png)) {
        vals$processed_img_path <- clean_png
        vals$auto_clean_path <- clean_png
        vals$mode <- "manual"
      }

      # Prefer the engine's own separated curves. kmdig3 already splits the two arms
      # (by hue and luminance) and scores each trace; re-digitizing the clean image
      # would re-merge same-hue arms (dark vs light purple) into one. So when the
      # self-check passes, load the engine's curves directly and skip the re-digitize.
      # If the check fails, leave curves unset so the manual/SurvdigitizeR path runs.
      csv_file <- paste0(prefix, ".csv")
      vals$manual_raw_data <- NULL
      if (isTRUE(tryCatch(isTRUE(meta$fit_ok), error = function(e) FALSE)) &&
          file.exists(csv_file)) {
        cur <- tryCatch(utils::read.csv(csv_file), error = function(e) NULL)
        if (!is.null(cur) && all(c("x_val", "y_val", "curve") %in% names(cur))) {
          cur <- cur[!is.na(cur$x_val) & cur$x_val >= 0, , drop = FALSE]
          vals$manual_raw_data <- data.frame(
            time     = as.numeric(cur$x_val),
            St       = as.numeric(cur$y_val),
            survival = as.numeric(cur$y_val),
            curve    = as.integer(gsub("[^0-9]", "", cur$curve))
          )
          vals$curve_mapping <- NULL
        }
      }
      overlay_png <- paste0(prefix, "_overlay.png")
      vals$overlay_img_path <- if (file.exists(overlay_png)) overlay_png else NULL

      # --- Pre-load Y axis (infer a regular grid from the OCR ticks) ---
      yt <- tryCatch(sort(unique(as.numeric(vapply(meta$ycal$ticks, function(t) t[[1]], numeric(1))))),
                     error = function(e) NULL)
      ax_y <- .km2_infer_axis(yt, zero_start = TRUE)
      if (!is.null(ax_y)) {
        vals$y_axis_editable <- data.frame(Y_Values = ax_y$seq)
        shiny::updateNumericInput(session, "man_y_start", value = ax_y$start)
        shiny::updateNumericInput(session, "man_y_end",   value = ax_y$end)
        shiny::updateNumericInput(session, "man_y_inc",   value = ax_y$inc)
      }

      # --- Pre-load X axis (infer a regular grid from the OCR ticks) ---
      xt <- tryCatch(sort(unique(as.numeric(vapply(meta$xcal$ticks, function(t) t[[1]], numeric(1))))),
                     error = function(e) NULL)
      ax_x <- .km2_infer_axis(xt, zero_start = TRUE)
      if (!is.null(ax_x)) {
        shiny::updateNumericInput(session, "man_x_start", value = ax_x$start)
        shiny::updateNumericInput(session, "man_x_end",   value = ax_x$end)
        shiny::updateNumericInput(session, "man_x_inc",   value = ax_x$inc)
      }

      # --- Pre-load numbers at risk (validated rows only) ---
      # Build the editable numbers-at-risk grid with the shared, unit-tested helper
      # (see tests/harness_risk_table.R), so the app runs exactly the verified path.
      n_rows <- 0L
      vals$risk_table_editable <- NULL
      rt <- .km2_build_risk_table(meta, ax_x)
      if (!is.null(rt)) {
        n_rows <- rt$n_rows
        vals$risk_table_editable <- rt$table
        if (!is.null(rt$ax)) {
          shiny::updateNumericInput(session, "man_x_start", value = rt$ax$start)
          shiny::updateNumericInput(session, "man_x_inc",   value = rt$ax$inc)
          shiny::updateNumericInput(session, "man_x_end",
                                    value = rt$ax$start + rt$ax$inc * (nrow(rt$table) - 1))
        }
      }
      if (is.null(vals$risk_table_editable)) {
        vt <- tryCatch(seq(input$man_x_start, input$man_x_end, by = max(input$man_x_inc, 1)),
                       error = function(e) numeric(0))
        vals$risk_table_editable <- tibble::tibble(
          Time = as.numeric(vt), N_Risk_G1 = NA_real_, N_Risk_G2 = NA_real_
        )
      }

      shiny::removeNotification(id = id_auto)
      n_curves <- tryCatch(as.integer(meta$n_curves), error = function(e) NA)

      # Second check: does each reconstructed curve actually follow its own detected
      # pixels? The engine scores every curve by how often the trace strays from the
      # real curve pixels (censor ticks included). A high score means a plateau or a
      # mis-trace, so we warn and open the manual / LLM fallbacks rather than let a
      # bad reconstruction pass silently.
      offmask <- tryCatch(unlist(meta$curve_offmask), error = function(e) numeric(0))
      fit_ok  <- tryCatch(isTRUE(meta$fit_ok), error = function(e) TRUE)
      if (length(offmask) && !fit_ok) fit_ok <- FALSE
      vals$fit_unreliable <- !fit_ok

      # Show the confirmation banner; if the read looks shaky, also surface fallbacks.
      vals$auto_done <- TRUE
      if (n_rows < 2 || !fit_ok) vals$show_fallback <- TRUE
      msg <- paste0(
        "Auto-digitization done",
        if (!is.na(n_curves)) paste0(" (", n_curves, " curve(s) detected)") else "",
        if (n_rows >= 2) ". Review the numbers at risk in the grid, then Confirm & Analyze."
        else if (n_rows == 1) ". Only one numbers-at-risk row read - fill the red cells, or use the other options."
        else ". Numbers-at-risk not read reliably - fill the red cells, or use the other options."
      )
      shiny::showNotification(msg, type = if (n_rows >= 2) "message" else "warning", duration = 12)
      if (!fit_ok) {
        pct <- if (length(offmask)) round(100 * max(offmask, na.rm = TRUE)) else NA
        shiny::showNotification(
          shiny::HTML(paste0(
            "<b>Heads up:</b> a reconstructed curve does not follow the detected curve ",
            "pixels well",
            if (!is.na(pct)) paste0(" (about ", pct, "% of it strays off the line)") else "",
            ". Compare the red trace with your figure on the left. If it looks wrong, ",
            "use the manual point-and-click or the LLM path (both are open in the sidebar).")),
          type = "warning", duration = NULL)
      }
    }, error = function(e) {
      shiny::removeNotification(id = id_auto)
      # route OCR-engine failures to the Tesseract install help
      if (grepl("tesseract", conditionMessage(e), ignore.case = TRUE)) {
        show_python_install_modal(missing_modules = character(0), tesseract_missing = TRUE)
      } else {
        shiny::showNotification(paste0("Automatic engine failed: ", conditionMessage(e),
                                       " — you can still use the LLM, Paint, or manual paths."),
                                type = "error", duration = 12)
      }
    })
  })

  # --- INFO UPLOAD MODAL ---
  shiny::observeEvent(input$upload_info_btn, {
    shiny::showModal(shiny::modalDialog(
      title = "File Format Requirements",
      shiny::tags$ul(
        shiny::tags$li(shiny::strong("Image:"), " Clean plot with axes, curves, and numbers. No titles or legends inside the plot area if possible."),
        shiny::tags$li(shiny::strong("Dataset:"), " Files: .xlsx, .csv, .rds, .rda"),
        shiny::tags$li("Must contain 3 columns:",
                       shiny::tags$ul(
                         shiny::tags$li("Time (numeric)"),
                         shiny::tags$li("Event/Status (0/1 or TRUE/FALSE)"),
                         shiny::tags$li("Arm/Group (categorical)")
                       )
        )
      ),
      footer = shiny::modalButton("Close")
    ))
  })

  # 0. Populate Example Datasets (landing selector only)
  shiny::observe({
    tryCatch({
      ds <- utils::data(package = "bayescores")$results[, "Item"]
      ds_clean <- ds[!grepl("toxicity", ds, ignore.case = TRUE)]
      shiny::updateSelectInput(session, "example_dataset_hero",
                               choices = c("Select an example dataset…" = "", ds_clean))
    }, error = function(e) warning("Could not list bayescores datasets"))
  })

  # OBSERVER FOR EXAMPLE DATASETS (driven by the landing selector)
  shiny::observeEvent(input$example_dataset_hero, {
    shiny::req(input$example_dataset_hero)
    if (input$example_dataset_hero == "") return()
    sel <- input$example_dataset_hero

    tryCatch({
      env <- new.env()
      utils::data(list = sel, package = "bayescores", envir = env)
      df <- env[[ls(env)[1]]]

      ns <- tolower(names(df))
      t_vars <- c("time", "tiempo", "t", "os", "pfs", "months", "days", "sem", "weeks", "futime", "ttfs")
      e_vars <- c("event", "status", "censor", "dead", "death", "evento", "estado", "outcome", "fustat")
      a_vars <- c("arm", "group", "treatment", "trt", "strat", "strata", "curve", "rama", "grupo")

      t_idx <- which(ns %in% t_vars)[1]
      e_idx <- which(ns %in% e_vars)[1]
      a_idx <- which(ns %in% a_vars)[1]

      if (is.na(t_idx) || is.na(e_idx) || is.na(a_idx)) {
        if(ncol(df) >= 3) {
          t_idx <- 1; e_idx <- 2; a_idx <- 3
          shiny::showNotification("Columns inferred by position (1=Time, 2=Event, 3=Arm).", type="warning")
        } else {
          stop("Could not identify columns.")
        }
      }

      clean_df <- data.frame(
        time = as.numeric(df[[t_idx]]),
        status = as.numeric(df[[e_idx]]),
        arm = as.factor(df[[a_idx]])
      )
      clean_df <- na.omit(clean_df)

      vals$final_ipd <- clean_df
      vals$fit_obj <- survfit(Surv(time, status) ~ arm, data = clean_df)
      vals$cox_obj <- tryCatch(coxph(Surv(time, status) ~ arm, data = clean_df), error=function(e) NULL)
      vals$mode <- "dataset"
      vals$processed_img_path <- NULL

      shiny::showNotification(paste("Example dataset", sel, "loaded."), type="message")
      vals$run_analysis_flag <- Sys.time()

    }, error = function(e) {
      shiny::showNotification(paste("Error loading example:", e$message), type="error")
    })
  })

  # Paste an image directly from the clipboard (same surface as Browse). The JS
  # listener sends a data URL; we decode it to a temp file and load it as a manual
  # image, exactly like a PNG/JPG upload.
  shiny::observeEvent(input$pasted_image, {
    du <- input$pasted_image
    shiny::req(is.character(du), length(du) == 1, grepl("^data:image/", du))
    tryCatch({
      head_b64 <- strsplit(du, ",", fixed = TRUE)[[1]]
      if (length(head_b64) < 2) stop("Empty clipboard image.")
      b64 <- head_b64[length(head_b64)]
      ext <- if (grepl("image/jpe?g", head_b64[1])) "jpg" else "png"
      raw <- jsonlite::base64_dec(b64)                 # jsonlite is already a hard dependency
      f <- file.path(temp_dir, paste0("paste_", as.integer(Sys.time()), ".", ext))
      writeBin(as.raw(raw), f)

      vals$original_img_path  <- f
      vals$overlay_img_path   <- NULL
      vals$processed_img_path <- f
      vals$auto_clean_path    <- NULL
      vals$mode               <- "manual"
      vals$auto_done <- FALSE; vals$show_fallback <- FALSE
      shiny::updateSelectInput(session, "example_dataset_hero", selected = "")
      shiny::showNotification("Image pasted from clipboard.", type = "message")
    }, error = function(e) {
      shiny::showNotification(paste("Could not read pasted image:", conditionMessage(e)), type = "error")
    })
  })

  # Shared upload handler: auto-detects image vs dataset by extension, so every
  # entry point (the landing's two choices, the sidebar dropzone) routes here.
  handle_upload <- function(datapath, fname) {
    vals$original_img_path <- datapath
    vals$overlay_img_path <- NULL
    vals$auto_clean_path <- NULL
    vals$auto_done <- FALSE; vals$show_fallback <- FALSE

    ext <- tolower(tools::file_ext(fname))

    if (ext %in% c("png", "jpg", "jpeg")) {
      vals$mode <- "manual"
      vals$processed_img_path <- datapath
      shiny::updateSelectInput(session, "example_dataset_hero", selected = "")

    } else {
      tryCatch({
        df <- NULL
        if (ext == "rds") {
          df <- readRDS(vals$original_img_path)
        } else if (ext == "rda") {
          env <- new.env()
          load(vals$original_img_path, envir = env)
          df <- env[[ls(env)[1]]]
        } else if (ext %in% c("xlsx", "xls")) {
          if (requireNamespace("readxl", quietly = TRUE)) {
            df <- readxl::read_excel(vals$original_img_path)
          } else {
            stop("readxl package required for Excel files.")
          }
        } else if (ext == "csv") {
          df <- utils::read.csv(vals$original_img_path)
        }

        if (!is.data.frame(df)) stop("Loaded object is not a data frame.")

        ns <- tolower(names(df))
        t_vars <- c("time", "tiempo", "t", "os", "pfs", "months", "days", "sem", "weeks", "futime", "ttfs")
        e_vars <- c("event", "status", "censor", "dead", "death", "evento", "estado", "outcome", "fustat")
        a_vars <- c("arm", "group", "treatment", "trt", "strat", "strata", "curve", "rama", "grupo")

        t_idx <- which(ns %in% t_vars)[1]
        e_idx <- which(ns %in% e_vars)[1]
        a_idx <- which(ns %in% a_vars)[1]

        if (is.na(t_idx) || is.na(e_idx) || is.na(a_idx)) {
          stop("Could not identify columns 'time', 'event', and 'arm' (or synonyms). Please rename your columns.")
        }

        clean_df <- data.frame(
          time = as.numeric(df[[t_idx]]),
          status = as.numeric(df[[e_idx]]),
          arm = as.factor(df[[a_idx]])
        )

        if (any(is.na(clean_df$time))) warning("NA in time column.")
        clean_df <- na.omit(clean_df)

        vals$final_ipd <- clean_df
        vals$fit_obj <- survfit(Surv(time, status) ~ arm, data = clean_df)
        vals$cox_obj <- tryCatch(coxph(Surv(time, status) ~ arm, data = clean_df), error=function(e) NULL)
        vals$mode <- "dataset"
        vals$processed_img_path <- NULL
        shiny::updateSelectInput(session, "example_dataset_hero", selected = "")

        shiny::showNotification("Dataset loaded successfully. Running analysis...", type="message")
        vals$run_analysis_flag <- Sys.time()

      }, error = function(e) {
        shiny::showNotification(paste("Error loading dataset:", e$message), type="error", duration=10)
      })
    }
  }

  # Both landing upload controls route through the same handler.
  shiny::observeEvent(input$up_image, {
    shiny::req(input$up_image)
    handle_upload(input$up_image$datapath, input$up_image$name)
  })
  shiny::observeEvent(input$up_dataset, {
    shiny::req(input$up_dataset)
    handle_upload(input$up_dataset$datapath, input$up_dataset$name)
  })

  # Whether any input has been loaded; toggles the landing versus the workspace.
  output$has_input <- shiny::reactive({
    !is.null(vals$original_img_path) || !is.null(vals$final_ipd) ||
      !is.null(vals$manual_raw_data) || (!is.null(vals$mode) && vals$mode == "dataset")
  })
  shiny::outputOptions(output, "has_input", suspendWhenHidden = FALSE)

  shiny::observe({
    shiny::req(vals$original_img_path)
    shiny::req(vals$mode == "manual")
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
    # single-curve fallback
    if(length(curves) < 2) return(list(curve_to_G1 = curves[1], curve_to_G2 = curves[1]))

    # map by survival area and risk-table totals
    # 1. Which curve has better survival? (average survival area)
    surv_avg <- sapply(curves, function(c) mean(raw_data$survival[raw_data$curve == c], na.rm = TRUE))
    curve_best <- curves[which.max(surv_avg)]
    curve_worst <- curves[which.min(surv_avg)]

    # 2. Which risk row has slower decline? (total sum)
    limpiar <- function(x) as.numeric(gsub("[^0-9.]", "", as.character(x)))
    sum_r1 <- sum(limpiar(risk_table$N_Risk_G1), na.rm = TRUE)
    sum_r2 <- sum(limpiar(risk_table$N_Risk_G2), na.rm = TRUE)

    # 3. Assign best-survival curve to the row with more cumulative patients
    if (sum_r1 >= sum_r2) {
      list(curve_to_G1 = curve_best, curve_to_G2 = curve_worst)
    } else {
      list(curve_to_G1 = curve_worst, curve_to_G2 = curve_best)
    }
  }

  # ==============================================================================
  # HELPER FUNCTION: FIT FLEXSURVCURE WITH HESSIAN CHECK
  # ==============================================================================
  fit_flexsurvcure_with_check <- function(formula, data, anc, dist, link, mixture) {
    hessian_warning <- FALSE
    model <- tryCatch({
      withCallingHandlers(
        flexsurvcure::flexsurvcure(formula, data = data, anc = anc,
                                   dist = dist, link = link, mixture = mixture),
        warning = function(w) {
          if (grepl("Hessian is not positive definite", w$message, ignore.case = TRUE)) {
            hessian_warning <<- TRUE
          }
          invokeRestart("muffleWarning")
        }
      )
    }, error = function(e) {
      return(NULL)
    })

    return(list(model = model, hessian_warning = hessian_warning))
  }

  # ==============================================================================
  # reconstruct_ipd wrapper guaranteeing interior points in each risk-table interval
  # ==============================================================================
  reconstruct_ipd_safe <- function(km_df, nr_df) {
    # Normalize: remove curve column so reconstruct_ipd assigns curve=1L internally
    km_df <- km_df[, c("time", "St"), drop = FALSE]
    nr_df <- nr_df[, c("time_tick", "nrisk"), drop = FALSE]

    # Safety net: if the digitized curve runs past the last numbers-at-risk time,
    # carry the last count forward to the curve's end so the tail is reconstructed
    # instead of being cut off where an incomplete at-risk row happens to stop.
    t_last  <- suppressWarnings(max(nr_df$time_tick, na.rm = TRUE))
    t_curve <- suppressWarnings(max(km_df$time, na.rm = TRUE))
    if (is.finite(t_curve) && is.finite(t_last) && t_curve > t_last + 1e-6) {
      last_n <- nr_df$nrisk[which.max(nr_df$time_tick)]
      nr_df  <- rbind(nr_df, data.frame(time_tick = t_curve, nrisk = last_n))
      nr_df  <- nr_df[order(nr_df$time_tick), , drop = FALSE]
    }

    eps   <- 1e-6
    ticks <- sort(nr_df$time_tick)
    extra <- list()
    for (i in seq_len(length(ticks) - 1)) {
      t_lo <- ticks[i]; t_hi <- ticks[i + 1]
      interior <- km_df$time[km_df$time > t_lo + eps & km_df$time < t_hi - eps]
      if (length(interior) == 0) {
        t_mid  <- (t_lo + t_hi) / 2
        St_mid <- approx(km_df$time, km_df$St, xout = t_mid,
                         method = "constant", rule = 2)$y
        extra[[length(extra) + 1]] <- data.frame(time = t_mid, St = St_mid)
      }
    }
    if (length(extra) > 0) {
      km_df <- rbind(km_df, do.call(rbind, extra))
      km_df <- km_df[order(km_df$time), , drop = FALSE]
    }
    bayescores::reconstruct_ipd(km_df, nr_df)
  }

  # Digitize the current (clean or raw) image into curve points with SurvdigitizeR.
  # Shared by the manual digitize action and the automatic path (called during
  # analysis when no curve data exists yet).
  digitize_current_image <- function(img_path = vals$processed_img_path) {
    raw <- .km2_survdigitize_robust(img_path=img_path, x_start=input$man_x_start, x_end=input$man_x_end, x_increment=input$man_x_inc, y_start=input$man_y_start, y_end=input$man_y_end, y_increment=input$man_y_inc, num_curves=input$man_num_curves, censoring=input$man_censoring, bg_lightness=input$man_bg_light, enhance=input$man_enhance, y_text_vertical=input$man_y_vert)
    if (max(raw$St, na.rm=TRUE) > 1.5) raw$survival <- raw$St/100 else raw$survival <- raw$St
    raw
  }

  shiny::observeEvent(input$run_manual_dig, {
    shiny::req(vals$original_img_path)
    shiny::req(input$man_x_start, input$man_x_end, input$man_x_inc)
    shiny::req(input$man_y_start, input$man_y_end, input$man_y_inc)
    shiny::req(input$man_num_curves)
    shiny::req(input$man_x_inc > 0)
    shiny::req(input$man_y_inc > 0)

    vals$mode <- "manual"
    id_dig <- shiny::showNotification("Starting digitization... Please wait.", type="message", duration=NULL)
    if(is.null(vals$risk_table_editable)) {
      vt <- seq(input$man_x_start, input$man_x_end, by=input$man_x_inc)
      vals$risk_table_editable <- tibble::tibble(Time=as.numeric(vt), N_Risk_G1=NA_real_, N_Risk_G2=NA_real_)
    }
    if(is.null(vals$y_axis_editable)) vals$y_axis_editable <- data.frame(Y_Values=seq(input$man_y_start, input$man_y_end, by=input$man_y_inc))

    tryCatch({
      raw <- digitize_current_image()
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

  # --- Import Text ---
  shiny::observeEvent(input$open_import_data, {
    shiny::showModal(shiny::modalDialog(title="Import Text",
                                        shiny::p("Paste 1-4 rows with numbers. Order: 1) Y-axis (asc), 2) X times (asc), 3) N at Risk G1 (desc), 4) N at Risk G2 (desc). Auto-separation if needed."),
                                        shiny::actionButton("show_llm_prompt", "View Prompt for LLM (ChatGPT/Claude)", class="btn-secondary w-100 mb-3", icon=shiny::icon("robot")),
                                        shiny::textAreaInput("import_raw_text", "Paste data here:", rows=6, placeholder="0.0 0.2 ...\n0 12 ...\n100 80 ...\n100 90 ..."),
                                        footer=shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton("process_import_text", "Import", class="btn-primary"))))
  })

  shiny::observeEvent(input$show_llm_prompt, {
    extract_only_prompt <- paste0(
      "Extract the numerical data from this image into exactly 4 lines of plain text. ",
      "Line 1: Y-axis values. Line 2: X-axis values. Line 3: Top row of the 'Numbers at risk' table. ",
      "Line 4: Bottom row of the 'Numbers at risk' table. ",
      "Output ONLY numbers separated by spaces for each line. Do not include labels, commas, or any additional text. ",
      "Crucially, do not repeat the data. Stop your output immediately after extracting the final number from the bottom row."
    )
    shiny::showModal(shiny::modalDialog(title = "Extraction Prompt (any LLM)", shiny::textAreaInput("llm_prompt_copy", label=NULL, height="180px", value=extract_only_prompt), footer = shiny::modalButton("Close")))
  })

  shiny::observeEvent(input$process_import_text, {
    shiny::req(input$import_raw_text); txt <- input$import_raw_text
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

  # Renderer that highlights empty/NA cells in red so missing values are obvious.
  hot_red_empty <- "function(instance, td, row, col, prop, value, cellProperties) {
    Handsontable.renderers.NumericRenderer.apply(this, arguments);
    if (value === null || value === '' || value === undefined || (typeof value === 'number' && isNaN(value))) {
      td.style.background = '#FFD6D6';
    }
    return td;
  }"

  output$hot_risk_table <- rhandsontable::renderRHandsontable({
    shiny::req(vals$risk_table_editable)
    rhandsontable::rhandsontable(vals$risk_table_editable, stretchH="all", height=450) %>%
      rhandsontable::hot_context_menu(allowRowEdit=TRUE, allowColEdit=FALSE) %>%
      rhandsontable::hot_col("Time", renderer = hot_red_empty) %>%
      # Numbers at risk are patient counts: always integers, never decimals.
      rhandsontable::hot_col("N_Risk_G1", type = "numeric", format = "0", renderer = hot_red_empty) %>%
      rhandsontable::hot_col("N_Risk_G2", type = "numeric", format = "0", renderer = hot_red_empty)
  })
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

    if (is.null(vals$mode) || vals$mode != "dataset") {
      shiny::req(vals$risk_table_editable, vals$y_axis_editable)
    }

    id_cal <- shiny::showNotification("Calculating Metrics... Please wait.", type="message", duration=NULL)

    # Reset Hessian warnings at start of analysis
    vals$hessian_warning_shared <- FALSE
    vals$hessian_warning_free <- FALSE
    vals$hessian_warning_html <- NULL

    tryCatch({

      if (is.null(vals$mode) || vals$mode != "dataset") {
        # Automatic path: no manual digitize was run, so digitize the cleaned image
        # here before reconstruction.
        if (is.null(vals$manual_raw_data)) {
          src <- if (!is.null(vals$auto_clean_path) && file.exists(vals$auto_clean_path))
                   vals$auto_clean_path else vals$processed_img_path
          shiny::req(src)
          id_ad <- shiny::showNotification("Digitizing the cleaned image... Please wait.", type="message", duration=NULL)
          on.exit(shiny::removeNotification(id_ad), add = TRUE)
          vals$manual_raw_data <- digitize_current_image(src)
          vals$curve_mapping <- NULL                     # remap arms after fresh digitization
        }
        if (is.null(vals$manual_raw_data)) stop("Missing curve data.")

        rt <- vals$risk_table_editable; vy <- vals$y_axis_editable$Y_Values

        # enforce monotonic non-increasing risk counts
        fix_mono <- function(x) {
          x <- as.numeric(x); if (length(x) < 2) return(x)
          for (i in 2:length(x)) {
            if (!is.na(x[i]) && !is.na(x[i-1]) && x[i] > x[i-1]) x[i] <- x[i-1]
          }
          x
        }
        rt$N_Risk_G1 <- fix_mono(rt$N_Risk_G1)
        rt$N_Risk_G2 <- fix_mono(rt$N_Risk_G2)

        if(is.null(vals$curve_mapping)) {
          vals$curve_mapping <- map_curves_to_risk_groups(vals$manual_raw_data, rt)
          msg <- sprintf("Auto-mapping: Curve %d -> G1, Curve %d -> G2",
                         vals$curve_mapping$curve_to_G1, vals$curve_mapping$curve_to_G2)
          shiny::showNotification(msg, type="message", duration=5)
        }

        # coerce to numeric for matching
        vals$curve_mapping$curve_to_G1 <- as.numeric(vals$curve_mapping$curve_to_G1)
        vals$curve_mapping$curve_to_G2 <- as.numeric(vals$curve_mapping$curve_to_G2)
        vals$manual_raw_data$curve <- as.numeric(vals$manual_raw_data$curve)

        limpiar <- function(x) as.numeric(gsub("[^0-9.]", "", as.character(x)))

        # guard against both arms mapping to the same curve
        if (identical(vals$curve_mapping$curve_to_G1, vals$curve_mapping$curve_to_G2)) {
          stop("Both arms point to the same curve. Check curve mapping.")
        }

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

          # drop zero-survival and zero-nrisk rows
          km <- km[km$survival > 0, ]
          nr <- nr[nr$nrisk > 0, ]
          # remove duplicate time ticks
          nr <- dplyr::distinct(nr, time_tick, .keep_all = TRUE)

          # ensure St column exists
          if (!"St" %in% names(km) && "survival" %in% names(km)) km$St <- km$survival

          if (nrow(nr) >= 2 && nrow(km) > 0) {
            ipd_rec <- reconstruct_ipd_safe(km[, c("time", "St"), drop = FALSE], nr)$ipd
            if(cid == vals$curve_mapping$curve_to_G1) {
              ipd_rec$arm <- "Group 1"
            } else {
              ipd_rec$arm <- "Group 2"
            }
            ipd_list[[length(ipd_list) + 1]] <- ipd_rec
          }
        }

        if (length(ipd_list) == 0) stop("Could not reconstruct curves.")

        final <- dplyr::bind_rows(ipd_list)
        # explicit factor levels
        final$arm <- factor(final$arm, levels = c("Group 1", "Group 2"))

        mod_init <- coxph(Surv(time, status) ~ arm, final)
        hr_init <- exp(coef(mod_init)[1])
        if(!is.na(hr_init) && hr_init > 1) {
          final$arm <- factor(ifelse(final$arm == "Group 1", "Group 2", "Group 1"),
                              levels = c("Group 1", "Group 2"))
          temp_map <- vals$curve_mapping$curve_to_G1
          vals$curve_mapping$curve_to_G1 <- vals$curve_mapping$curve_to_G2
          vals$curve_mapping$curve_to_G2 <- temp_map
          # isolate risk-table swap inside reactive context
          isolate({
            temp_risk <- vals$risk_table_editable$N_Risk_G1
            vals$risk_table_editable$N_Risk_G1 <- vals$risk_table_editable$N_Risk_G2
            vals$risk_table_editable$N_Risk_G2 <- temp_risk
          })
          mod_init <- coxph(Surv(time, status) ~ arm, final)
          shiny::showNotification("HR > 1 detected. Arms swapped automatically.", type="warning", duration=6)
        }
        vals$final_ipd <- final
        vals$fit_obj <- survfit(Surv(time, status) ~ arm, final)
        vals$cox_obj <- mod_init
      } else {
        # --- DATASET MODE ---
        shiny::req(vals$final_ipd)

        tryCatch({
          tmp_cox <- coxph(Surv(time, status) ~ arm, data = vals$final_ipd)
          if(!is.null(tmp_cox) && !is.na(coef(tmp_cox)[1])) {
            hr_val <- exp(coef(tmp_cox)[1])
            if(hr_val > 1) {
              f_levels <- levels(factor(vals$final_ipd$arm))
              if(length(f_levels) == 2) {
                vals$final_ipd$arm <- relevel(factor(vals$final_ipd$arm), ref = f_levels[2])
                vals$fit_obj <- survfit(Surv(time, status) ~ arm, data = vals$final_ipd)
                vals$cox_obj <- coxph(Surv(time, status) ~ arm, data = vals$final_ipd)
                shiny::showNotification("Auto-detection: Arms swapped based on survival (HR > 1 corrected).", type="warning", duration=6)
              }
            }
          }
        }, error = function(e) NULL)
      }

      # ================================================================
      # Metrics calculation
      # ================================================================
      data <- vals$final_ipd; arms <- levels(factor(data$arm)); ac <- arms[1]; ae <- arms[2]
      mt1 <- max(data$time[data$arm==ac]); mt2 <- max(data$time[data$arm==ae]); tau <- min(mt1, mt2)

      n_ctrl <- sum(data$arm==ac); n_exp <- sum(data$arm==ae)
      ev_ctrl <- sum(data$status[data$arm==ac]); ev_exp <- sum(data$status[data$arm==ae])
      cens_ctrl <- n_ctrl - ev_ctrl; cens_exp <- n_exp - ev_exp

      sf <- median(data$time); data$ts <- data$time/sf

      # --- FIT SHARED SHAPE MODEL WITH HESSIAN CHECK ---
      fc_result <- fit_flexsurvcure_with_check(
        formula = Surv(ts, status) ~ arm,
        data = data,
        anc = list(scale = ~arm),
        dist = "weibull",
        link = "logistic",
        mixture = TRUE
      )
      fc <- fc_result$model
      vals$hessian_warning_shared <- fc_result$hessian_warning
      vals$cure_model_obj <- fc

      # --- FIT FREE SHAPE MODEL WITH HESSIAN CHECK ---
      fc_free_result <- fit_flexsurvcure_with_check(
        formula = Surv(ts, status) ~ arm,
        data = data,
        anc = list(shape = ~arm, scale = ~arm),
        dist = "weibull",
        link = "logistic",
        mixture = TRUE
      )
      fc_free <- fc_free_result$model
      vals$hessian_warning_free <- fc_free_result$hessian_warning

      aic_shared <- tryCatch(if(!is.null(fc)) AIC(fc) else NA, error=function(e) NA)
      aic_free <- tryCatch(if(!is.null(fc_free)) AIC(fc_free) else NA, error=function(e) NA)

      # Calc Reverse KM
      rev_km <- survfit(Surv(time, 1-status) ~ 1, data=data)
      median_follow_up <- summary(rev_km)$table["median"]
      if(is.na(median_follow_up)) median_follow_up <- max(data$time)

      # Calc Control Median Survival
      km_ctrl <- survfit(Surv(time, status) ~ 1, data=data[data$arm==ac,])
      median_surv_ctrl <- summary(km_ctrl)$table["median"]
      if(is.na(median_surv_ctrl)) median_surv_ctrl <- max(data$time[data$arm==ac])

      maturity_idx <- median_follow_up / median_surv_ctrl

      # Initialize correlation variables
      pearson <- NA
      vcov_valid <- FALSE

      if (!is.null(fc) && !vals$hessian_warning_shared) {
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

        vc <- tryCatch(vcov(fc), error = function(e) NULL)

        if (!is.null(vc)) {
          eigenvalues <- tryCatch(eigen(vc)$values, error = function(e) NULL)
          if (!is.null(eigenvalues) && all(eigenvalues > 0)) {
            vcov_valid <- TRUE

            ith <- grep("arm", rownames(vc)); isc <- grep("scale\\(arm", rownames(vc))
            idx1 <- ith[!ith %in% isc]; idx2 <- isc

            if(length(idx1)>0 && length(idx2)>0) {
              cv <- vc[idx1[1], idx2[1]]; v1 <- vc[idx1[1], idx1[1]]; v2 <- vc[idx2[1], idx2[1]]
              if(!is.na(v1) && !is.na(v2) && v1>0 && v2>0) {
                pearson <- cv/(sqrt(v1)*sqrt(v2))

                if (!is.na(pearson) && abs(pearson) > 1) {
                  pearson <- NA
                  vcov_valid <- FALSE
                }
              }
            }
          }
        }

        rate_c <- p_theta[1]
        rate_e <- if(!is.na(theta_logit) && !is.na(p_arm[1])) plogis(theta_logit + p_arm[1]) else NA
      } else {
        theta_logit <- NA
        p_theta <- rep(NA, 5)
        p_shape <- rep(NA, 5)
        p_scale <- rep(NA, 5)
        p_arm <- rep(NA, 5)
        p_sc_arm <- rep(NA, 5)
        rate_c <- NA
        rate_e <- NA
      }

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

        if(!is.null(fc)) {
          CURE_S <- cbind(
            summary(fc, newdata = data.frame(arm = arm_vals[1]), type = "survival", t = times_grid/sf)[[1]]$est,
            summary(fc, newdata = data.frame(arm = arm_vals[2]), type = "survival", t = times_grid/sf)[[1]]$est
          )
        } else {
          CURE_S <- matrix(NA, nrow = length(times_grid), ncol = 2)
        }

        if(!is.null(fc_free) && !inherits(fc_free, "try-error")) {
          CURE_S_FREE <- cbind(
            summary(fc_free, newdata = data.frame(arm = arm_vals[1]), type = "survival", t = times_grid/sf)[[1]]$est,
            summary(fc_free, newdata = data.frame(arm = arm_vals[2]), type = "survival", t = times_grid/sf)[[1]]$est
          )
        } else {
          CURE_S_FREE <- matrix(NA, nrow = length(times_grid), ncol = 2)
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
          CURE_S_FREE = CURE_S_FREE,
          tau = tau,
          arm_vals = arm_vals,
          mae_arm1 = mae_arm1,
          mae_arm2 = mae_arm2
        )
      }, error = function(e) {
        mae_cure <<- NA
        mae_ratio <<- NA
      })

      # ==============================================================================
      # HESSIAN WARNING HTML GENERATION
      # ==============================================================================
      if (vals$hessian_warning_shared || vals$hessian_warning_free || !vcov_valid) {
        warning_parts <- c()

        if (vals$hessian_warning_shared) {
          warning_parts <- c(warning_parts, "<li><b>Shared Shape Model:</b> Hessian is not positive definite. The frequentist optimizer may have encountered numerical instabilities. This does not necessarily affect the Bayesian model, which uses a different estimation approach (HMC sampling with regularizing priors).</li>")
        }

        if (vals$hessian_warning_free) {
          warning_parts <- c(warning_parts, "<li><b>Free Shape Model:</b> Hessian is not positive definite. The frequentist optimizer may have encountered numerical instabilities. This does not necessarily affect the Bayesian model, which uses a different estimation approach (HMC sampling with regularizing priors).</li>")
        }

        if (!vcov_valid && !vals$hessian_warning_shared) {
          warning_parts <- c(warning_parts, "<li><b>Variance-Covariance Matrix:</b> Invalid or not positive definite (negative eigenvalues detected).</li>")
        }

        vals$hessian_warning_html <- shiny::HTML(paste0(
          "<div style='background-color: #FFF3E0; padding: 15px; border-left: 5px solid #E65100; margin-bottom: 15px;'>",
          "<h6 style='color: #E65100; margin-top: 0;'><strong>&#9888; Convergence Warning</strong></h6>",
          "<p>The mixture cure model optimization encountered issues:</p>",
          "<ul style='margin-bottom: 10px;'>",
          paste(warning_parts, collapse = ""),
          "</ul>",
          "<p><strong>Implications:</strong></p>",
          "<ul>",
          "<li>The <b>Pearson correlation</b> displayed below <span style='color: #D32F2F;'><b>may not be reliable</b></span>.</li>",
          "<li>Parameter estimates and confidence intervals from flexsurvcure should be interpreted with caution.</li>",
          "<li>This reflects numerical instability in the frequentist optimizer, not necessarily a problem for the Bayesian model (which uses HMC sampling with regularizing priors).</li>",
          "</ul>",
          "<p><strong>Recommendations:</strong></p>",
          "<ul>",
          "<li>Consider using <b>'immature_skeptical'</b> or <b>'biologically_null'</b> tail assumptions to simplify the model.</li>",
          "<li>Review the data for sufficient events in both arms.</li>",
          "<li>The Bayesian model may still converge if appropriate priors are used.</li>",
          "</ul>",
          "</div>"
        ))
      }

      # CALIBRATION WARNING LOGIC
      vals$calibration_warning_html <- NULL

      recommend_non_shared <- FALSE
      reasons_non_shared <- c()

      diff_aic <- NA
      if(!is.na(aic_shared) && !is.na(aic_free)) {
        diff_aic <- aic_shared - aic_free
      }

      if(!is.na(mae_ratio) && mae_ratio > 3.0) {
        recommend_non_shared <- TRUE
        reasons_non_shared <- c(reasons_non_shared, paste0("High MAE imbalance (Ratio = ", round(mae_ratio, 2), "x)"))
      }

      if(!is.na(diff_aic)) {
        if(diff_aic > 10) {
          recommend_non_shared <- TRUE
          reasons_non_shared <- c(reasons_non_shared, paste0("Strong statistical evidence (Delta AIC = ", round(diff_aic, 1), " > 10)"))
        }
      }

      if(recommend_non_shared) {
        vals$calibration_warning_html <- shiny::HTML(paste0(
          "<div style='background-color: #FFEBEE; padding: 12px; border-left: 5px solid #D32F2F; margin-bottom: 10px;'>",
          "<strong style='color: #D32F2F;'>&#9888; Calibration Warning:</strong> ",
          "Evidence suggests the Weibull shape parameter differs between treatment groups. ",
          "Reasons: ", paste(reasons_non_shared, collapse = ", "), ". ",
          "<strong>Recommendation:</strong> Consider <strong>unchecking 'Shared Shape'</strong> in the Bayesian Model settings ",
          "to allow each arm to have its own shape parameter, which may improve model fit.",
          "</div>"
        ))
      }

      # Build results data frame
      fr <- data.frame(
        N_Total = nrow(data), N_Ctrl = n_ctrl, N_Exp = n_exp,
        Events_Total = sum(data$status), Events_Ctrl = ev_ctrl, Events_Exp = ev_exp,
        Censored_Total = nrow(data)-sum(data$status), Censored_Ctrl = cens_ctrl, Censored_Exp = cens_exp,
        Censoring_Rate_Global = 1-mean(data$status), Censoring_Rate_Ctrl = cens_ctrl/n_ctrl, Censoring_Rate_Exp = cens_exp/n_exp,
        Tau_Common = tau,
        Pearson_Correlation = if(vcov_valid) pearson else NA,
        Maturity_Index = maturity_idx,
        Median_FollowUp = median_follow_up,
        Median_Surv_Ctrl = median_surv_ctrl,
        MAE_Arm1 = if(!is.na(mae_arm1)) mae_arm1 else NA,
        MAE_Arm2 = if(!is.na(mae_arm2)) mae_arm2 else NA,
        MAE_Ratio = if(!is.na(mae_ratio)) mae_ratio else NA,
        AIC_Shared = if(!is.na(aic_shared)) aic_shared else NA,
        AIC_Free_Shape = if(!is.na(aic_free)) aic_free else NA
      )

      vals$analysis_results_full <- fr

      vals_vec <- sapply(fr, function(x) {
        if(is.numeric(x)) as.character(round(x, 4)) else as.character(x)
      })

      if (!vcov_valid || vals$hessian_warning_shared) {
        if (!is.na(fr$Pearson_Correlation)) vals_vec["Pearson_Correlation"] <- paste0(vals_vec["Pearson_Correlation"], " *")
      }

      b1_m <- c("N_Total", "N_Ctrl", "N_Exp", "Tau_Common")
      b1_v <- vals_vec[b1_m]

      b2_m <- c("Events_Total", "Censored_Total", "MAE_Arm1", "MAE_Arm2", "AIC_Shared", "AIC_Free_Shape")
      b2_v <- vals_vec[b2_m]

      b3_m <- c("Pearson_Correlation", "Maturity_Index", "Median_FollowUp",
                "Median_Surv_Ctrl", "MAE_Ratio")
      b3_v <- vals_vec[b3_m]

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

      plateau_visual_stable <- TRUE
      tryCatch({
        km_check <- vals$calib_data$km_fit
        t_start_check <- 0.85 * tau
        s_start <- summary(km_check, times = t_start_check)$surv
        s_end <- summary(km_check, times = tau)$surv
        if(mean(s_start) - mean(s_end) > 0.05) plateau_visual_stable <- FALSE
      }, error=function(e) NULL)

      interp_scenario <- ""
      interp_criteria <- ""
      interp_interpretation <- ""
      interp_protocol <- ""
      reco_tail <- "neutral"   # tail assumption recommended by the active scenario

      if(rho_val > 0.7) {
        reco_tail <- "immature_skeptical"
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
          "<li><span style='color: #D32F2F;'><b>Action:</b> Set 'Tail Assumption' to either <b>'biologically_null'</b> or <b>'immature_skeptical'</b> in the Bayesian Model settings. Choose <b>'biologically_null'</b> in scenarios such as chemotherapy for incurable cancer where it is well established that treatment predominantly extends life without producing cures. Choose <b>'immature_skeptical'</b> in scenarios such as adjuvant therapy with short follow-up where it is expected that treatment could produce cures but the data are insufficient (e.g., due to short follow-up time) to determine whether a therapy can produce cures.</span></li>",
          "</ul>"
        )
      }
      else if(m_val < 2.5 && rho_val < 0.4) {
        reco_tail <- "immature_skeptical"
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
      else if(m_val >= 3.0 && rho_val < 0.4 && plateau_visual_stable) {
        reco_tail <- "neutral"
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
      else if(m_val > 3.0 && !plateau_visual_stable) {
        reco_tail <- "immature_skeptical"
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
      else {
        reco_tail <- "immature_skeptical"
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

      aic_conclusion <- ""
      if(!is.na(aic_shared) && !is.na(aic_free)) {
        daic <- aic_shared - aic_free
        if(daic > 10) {
          aic_conclusion <- paste0("<div style='margin-top: 10px; padding: 10px; background-color: #E6FAFF; border-left: 4px solid var(--bm-accent); color: var(--bm-fg);'><b>Model Structure Recommendation:</b> Strong statistical evidence favors <b>Free Shape</b> (Delta AIC = ", round(daic, 1), " > 10). Uncheck 'Shared Shape' if visual fit confirms improvement.</div>")
        } else {
          aic_conclusion <- paste0("<div style='margin-top: 10px; padding: 10px; background-color: var(--bm-accent-soft); border-left: 4px solid var(--bm-primary); color: var(--bm-fg);'><b>Model Structure Recommendation:</b> Evidence supports <b>Shared Shape</b> (Parsimony holds, Delta AIC <= 10).</div>")
        }
      }

      # --- PRE-CONFIGURE THE BAYESIAN MODEL FROM THESE RECOMMENDATIONS ---
      # Tail assumption comes from the active scenario; Shared Shape follows the
      # Model Structure Recommendation (keep Shared unless Delta AIC > 10 favours
      # Free Shape). The user can still override before running the fit.
      reco_shared <- !(!is.na(diff_aic) && diff_aic > 10)
      vals$reco_tail <- reco_tail
      vals$reco_shared_shape <- reco_shared
      shiny::updateSelectInput(session, "tail_assumption", selected = reco_tail)
      shiny::updateCheckboxInput(session, "shared_shape", value = reco_shared)

      preset_note <- paste0(
        "<div style='margin-top: 10px; padding: 10px; background-color: #F3EEFF; border-left: 4px solid var(--bm-primary-2); color: var(--bm-fg);'>",
        "<b>Applied to the Bayesian Model:</b> Tail Assumption set to <b>'", reco_tail,
        "'</b> and Shared Shape <b>", if (reco_shared) "enabled" else "disabled",
        "</b>. Adjust these in the Bayesian Model tab if you disagree.</div>"
      )

      html_content <- paste0(
        "<h5>", interp_scenario, "</h5>",
        "<hr>",
        "<p><b>Diagnostic Criteria:</b></p>",
        interp_criteria,
        "<p><b>Interpretation:</b> ", interp_interpretation, "</p>",
        "<hr>",
        interp_protocol,
        aic_conclusion,
        preset_note
      )
      vals$interpretation_html <- shiny::HTML(html_content)

      shiny::removeNotification(id=id_cal)
      shiny::showNotification(
        sprintf("Analysis complete. Bayesian model pre-set: tail = '%s', shared shape = %s.",
                reco_tail, reco_shared),
        type = "message", duration = 8)
    }, error = function(e) { shiny::removeNotification(id=id_cal); shiny::showNotification(paste("Error:", e$message), type="error") })
  }

  output$Interpretation_ui <- shiny::renderUI({
    shiny::req(vals$interpretation_html)
    vals$interpretation_html
  })

  output$calibration_warning_ui <- shiny::renderUI({
    vals$calibration_warning_html
  })

  output$hessian_warning_ui <- shiny::renderUI({
    vals$hessian_warning_html
  })

  output$head_ipd_table <- shiny::renderTable({
    shiny::req(vals$final_ipd)
    head(vals$final_ipd)
  })

  apply_grid_edits_and_reanalyze <- function() {
    tryCatch({
      if (!is.null(input$hot_risk_table)) {
        rt <- rhandsontable::hot_to_r(input$hot_risk_table)
        # Patient counts must stay integer even if a decimal is typed/pasted by hand.
        for (col in c("N_Risk_G1", "N_Risk_G2"))
          if (!is.null(rt[[col]])) rt[[col]] <- round(as.numeric(rt[[col]]))
        vals$risk_table_editable <- rt
      }
      if (!is.null(input$hot_y_axis)) vals$y_axis_editable <- rhandsontable::hot_to_r(input$hot_y_axis)
      run_core_analysis()
    }, error = function(e)
      shiny::showNotification(paste("Could not apply edits:", conditionMessage(e)), type = "error"))
  }

  shiny::observeEvent(input$apply_edits, { apply_grid_edits_and_reanalyze() })
  shiny::observeEvent(input$apply_grid_edits, { apply_grid_edits_and_reanalyze() })

  shiny::observeEvent(vals$run_analysis_flag, { run_core_analysis() }, ignoreInit = TRUE)

  # ==============================================================================
  # BAYESIAN MODEL
  # ==============================================================================

  shiny::observeEvent(input$upload_model_rds, {
    shiny::req(input$upload_model_rds)
    id_load <- shiny::showNotification("Loading model... Please wait.", type="message", duration=NULL)
    tryCatch({
      loaded_model <- readRDS(input$upload_model_rds$datapath)
      if (!is.list(loaded_model) || is.null(loaded_model$stan_fit)) {
        stop("The file does not appear to be a valid bayescores model object.")
      }
      vals$model_fit_obj <- loaded_model
      shiny::removeNotification(id_load)
      shiny::showNotification("Model loaded successfully! Visualizations updated.", type="message")
    }, error = function(e) {
      shiny::removeNotification(id_load)
      shiny::showNotification(paste("Error loading model:", e$message), type="error", duration=5)
    })
  })

  shiny::observeEvent(input$run_model, {
    shiny::req(vals$final_ipd)
    id_mod <- shiny::showNotification("Compiling and fitting model... This may take several minutes.", type = "message", duration = NULL)
    tryCatch({
      hist_arg <- if (input$use_historical) {
        paste0("TRUE, params=c(", input$hist_mean, ",", input$hist_sd, ")")
      } else {
        paste0("FALSE, tail_assumption='", input$tail_assumption, "'")
      }

      vals$code_text <- paste0(
        "library(readxl)\nlibrary(survival)\nlibrary(flexsurvcure)\nlibrary(bayescores)\nlibrary(rstan)\nlibrary(dplyr)\n\n",
        "# ------------------------------------------------------------------\n",
        "# 1. LOAD DATA\n",
        "# ------------------------------------------------------------------\n",
        "# Please define the path to your downloaded 'ipd.xlsx' file\n",
        "file_path <- \"path/to/your/ipd.xlsx\" # <--- EDIT THIS PATH\n",
        "ipd <- readxl::read_excel(file_path)\n\n",

        "# ------------------------------------------------------------------\n",
        "# 2. STABILITY METRICS & FORENSIC ANALYSIS (Replication)\n",
        "# ------------------------------------------------------------------\n",
        "# Data Prep\n",
        "sf <- median(ipd$time)\n",
        "ipd$ts <- ipd$time/sf\n",
        "arms <- levels(factor(ipd$arm)); ac <- arms[1]; ae <- arms[2]\n\n",

        "# Kaplan-Meier & Cox\n",
        "km_fit <- survfit(Surv(time, status) ~ arm, data=ipd)\n",
        "print(km_fit)\n\n",

        "# Mixture Cure Models (Frequentist for AIC/Correlation)\n",
        "# Model A: Shared Shape (Parsimonious)\n",
        "fc_shared <- flexsurvcure(Surv(ts, status)~arm, data=ipd, \n",
        "                                      anc=list(scale=~arm), dist=\"weibull\", link=\"logistic\", mixture=TRUE)\n",
        "# Model B: Free Shape (Complex)\n",
        "fc_free <- try(flexsurvcure(Surv(ts, status)~arm, data=ipd, \n",
        "                               anc=list(shape=~arm, scale=~arm), dist=\"weibull\", link=\"logistic\", mixture=TRUE), silent=TRUE)\n\n",

        "# Calculate AICs\n",
        "aic_shared <- AIC(fc_shared)\n",
        "aic_free <- if(!inherits(fc_free, \"try-error\")) AIC(fc_free) else NA\n",
        "cat(\"AIC Shared:\", aic_shared, \"\\nAIC Free:\", aic_free, \"\\n\")\n",
        "if(!is.na(aic_free) && (aic_shared - aic_free > 4)) cat(\"Suggestion: Consider Free Shape (Delta AIC > 4)\\n\")\n\n",

        "# Extract Pearson Correlation (Parameter Stability)\n",
        "vc <- vcov(fc_shared)\n",
        "ith <- grep(\"arm\", rownames(vc)); isc <- grep(\"scale\\\\(arm\", rownames(vc))\n",
        "idx1 <- ith[!ith %in% isc]; idx2 <- isc\n",
        "if(length(idx1)>0 && length(idx2)>0) {\n",
        "  pearson <- vc[idx1[1], idx2[1]] / (sqrt(vc[idx1[1], idx1[1]]) * sqrt(vc[idx2[1], idx2[1]]))\n",
        "  cat(\"Pearson Correlation:\", pearson, \"\\n\")\n",
        "} else { cat(\"Correlation not calculated.\\n\") }\n\n",

        "# Calculate Maturity Index\n",
        "rev_km <- survfit(Surv(time, 1-status) ~ 1, data=ipd)\n",
        "med_fu <- summary(rev_km)$table[\"median\"]\n",
        "if(is.na(med_fu)) med_fu <- max(ipd$time)\n",
        "km_ctrl <- survfit(Surv(time, status) ~ 1, data=ipd[ipd$arm==ac,])\n",
        "med_surv_ctrl <- summary(km_ctrl)$table[\"median\"]\n",
        "if(is.na(med_surv_ctrl)) med_surv_ctrl <- max(ipd$time[ipd$arm==ac])\n",
        "maturity_idx <- med_fu / med_surv_ctrl\n",
        "cat(\"Maturity Index:\", maturity_idx, \"\\n\")\n\n",

        "# ------------------------------------------------------------------\n",
        "# 3. BAYESIAN MODEL FITTING\n",
        "# ------------------------------------------------------------------\n",
        "fit <- fit_bayesian_cure_model(ipd, \n",
        "                               time_col='time', event_col='status', arm_col='arm', \n",
        "                               iter=", input$iter, ", chains=", input$chains, ", warmup=", input$warmup, ", \n",
        "                               shared_shape=", input$shared_shape, ", \n",
        "                               use_historical_prior=", hist_arg, ")\n\n",

        "# ------------------------------------------------------------------\n",
        "# 4. DIAGNOSTICS & PLOTS\n",
        "# ------------------------------------------------------------------\n",
        "diagnose_fit(fit$stan_fit)\n",
        "model_diagnostics(fit)\n",
        "bayescores::plot_densities(fit)\n",
        "bayescores::plot_correlated_densities(fit)\n",
        "plot(fit)\n"
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

  draws_csv_logic <- function(f) {
    shiny::req(vals$model_fit_obj)
    draws <- as.data.frame(vals$model_fit_obj$stan_fit)
    write.csv(draws, f, row.names = FALSE)
  }

  output$dl_draws_csv <- shiny::downloadHandler(filename = "mcmc_draws.csv", content = draws_csv_logic)

  # ---- Reproducible R code (.R) ----
  output$dl_repro_code <- shiny::downloadHandler(
    filename = function() "bayesian_model_reproduction.R",
    content  = function(f) { shiny::req(vals$code_text); writeLines(vals$code_text, f) }
  )

  # ---- Model summary (.md) ----
  model_summary_text <- function() {
    shiny::req(vals$model_fit_obj)
    paste(utils::capture.output({
      if (exists("outcomes", asNamespace("bayescores")))
        bayescores::outcomes(vals$model_fit_obj, correlation_method = "pearson")
      else print(vals$model_fit_obj)
    }), collapse = "\n")
  }
  output$dl_model_summary_md <- shiny::downloadHandler(
    filename = function() "model_summary.md",
    content  = function(f) {
      writeLines(c("# Bayesian model summary", "", "```text", model_summary_text(), "```"), f)
    }
  )

  # ---- Stability metrics report (.html) ----
  # Self-contained HTML that mirrors the app: same violet palette, the styled
  # Instability Check table, the interpretation boxes and the text summaries.
  # Generic for any study: nothing here is keyed to specific arm names.
  .html_escape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  .df_to_html <- function(d) {
    d  <- as.data.frame(lapply(d, as.character), stringsAsFactors = FALSE, check.names = FALSE)
    th <- paste0("<th>", vapply(names(d), .html_escape, character(1)), "</th>", collapse = "")
    rows <- apply(d, 1, function(r)
      paste0("<tr>", paste0("<td>", vapply(unname(r), .html_escape, character(1)), "</td>", collapse = ""), "</tr>"))
    paste0("<table><thead><tr>", th, "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")
  }
  .stability_css <- paste0(
    ":root{--bm-bg:#F5F7FB;--bm-fg:#1A2233;--bm-muted:#67748A;--bm-primary:#4F46E5;",
    "--bm-primary-2:#7C3AED;--bm-accent:#06B6D4;--bm-accent-soft:#EEF0FF;--bm-card:#FFFFFF;--bm-border:#E4E8F2;}",
    "*{box-sizing:border-box;}body{font-family:'Inter',system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;",
    "background:var(--bm-bg);color:var(--bm-fg);margin:0;padding:24px;}",
    "h2{font-weight:800;letter-spacing:-.2px;margin:0 0 16px;}",
    ".card{background:var(--bm-card);border:1px solid var(--bm-border);border-radius:18px;",
    "box-shadow:0 8px 22px rgba(31,41,75,.08);margin-bottom:18px;overflow:hidden;}",
    ".card-header{background:linear-gradient(180deg,#F3F5FF 0%,#FFFFFF 100%);border-bottom:1px solid var(--bm-border);padding:12px 16px;font-weight:750;}",
    ".card-body{padding:14px 16px;}",
    "pre{background:rgba(15,23,42,.05);padding:12px;border-radius:10px;overflow-x:auto;",
    "font-family:ui-monospace,Consolas,monospace;font-size:12.5px;white-space:pre;}",
    ".badges{display:flex;justify-content:center;gap:12px;margin-bottom:12px;}",
    ".badges span{padding:4px 12px;border-radius:8px;font-weight:600;font-size:13px;color:var(--bm-fg);}",
    ".badges .b1{background:var(--bm-accent-soft);border-left:4px solid var(--bm-primary);}",
    ".badges .b2{background:#F3EEFF;border-left:4px solid var(--bm-primary-2);}",
    ".badges .b3{background:#E6FAFF;border-left:4px solid var(--bm-accent);}",
    "table{width:100%;border-collapse:separate;border-spacing:0;font-size:13px;}",
    ".mtc th{color:var(--bm-fg);font-weight:750;text-align:left;padding:8px 10px;}",
    ".mtc td{padding:7px 10px;}",
    ".mtc th:nth-child(1),.mtc th:nth-child(2){background:var(--bm-accent-soft);border-bottom:2px solid var(--bm-primary);}",
    ".mtc th:nth-child(3),.mtc th:nth-child(4){background:#F3EEFF;border-bottom:2px solid var(--bm-primary-2);}",
    ".mtc th:nth-child(5),.mtc th:nth-child(6){background:#E6FAFF;border-bottom:2px solid var(--bm-accent);}",
    ".mtc td:nth-child(1),.mtc td:nth-child(2){background:rgba(79,70,229,.05);}",
    ".mtc td:nth-child(3),.mtc td:nth-child(4){background:rgba(124,58,237,.05);}",
    ".mtc td:nth-child(5),.mtc td:nth-child(6){background:rgba(6,182,212,.06);}",
    ".mtc td:nth-child(2),.mtc td:nth-child(4){border-right:2px solid var(--bm-border);}",
    ".mtc td:nth-child(2),.mtc td:nth-child(4),.mtc td:nth-child(6){text-align:right;}",
    ".mtc td:nth-child(1),.mtc td:nth-child(3),.mtc td:nth-child(5){font-weight:600;}"
  )
  stability_html <- function() {
    shiny::req(vals$analysis_summary_view)
    card <- function(title, body, body_style = "")
      paste0("<div class='card'><div class='card-header'>", title, "</div>",
             "<div class='card-body' style='", body_style, "'>", body, "</div></div>")
    pre_card <- function(title, txt)
      card(title, paste0("<pre>", .html_escape(paste(txt, collapse = "\n")), "</pre>"))

    parts <- card("Instability Check", paste0(
      "<div class='badges'><span class='b1'>Sample Info</span>",
      "<span class='b2'>Events</span><span class='b3'>Instability Metrics</span></div>",
      "<div class='mtc'>", .df_to_html(vals$analysis_summary_view), "</div>"))

    if (!is.null(vals$hessian_warning_html))
      parts <- c(parts, card("Warning", as.character(vals$hessian_warning_html)))
    if (!is.null(vals$calibration_warning_html))
      parts <- c(parts, card("Calibration Warning", as.character(vals$calibration_warning_html)))
    if (!is.null(vals$interpretation_html))
      parts <- c(parts, card("Interpretation Suggestions", as.character(vals$interpretation_html),
                             "border-left:5px solid var(--bm-primary-2);background:#FBFAFF;"))
    if (!is.null(vals$fit_obj)) {
      km <- utils::capture.output({
        cat("--- SURVFIT (KAPLAN-MEIER) ---\n\n"); print(vals$fit_obj)
        if (!is.null(vals$cox_obj)) { cat("\n\n--- COX MODEL SUMMARY ---\n"); print(summary(vals$cox_obj)) }
      })
      parts <- c(parts, pre_card("Kaplan-Meier / Cox Summary", km))
    }
    if (!is.null(vals$cure_model_obj))
      parts <- c(parts, pre_card("Mixture Cure Model Details", utils::capture.output(print(vals$cure_model_obj))))

    paste0("<!doctype html><html lang='en'><head><meta charset='utf-8'>",
           "<meta name='viewport' content='width=device-width, initial-scale=1'>",
           "<title>Stability metrics report</title>",
           "<link href='https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700;800&display=swap' rel='stylesheet'>",
           "<style>", .stability_css, "</style></head><body>",
           "<h2>Stability metrics report</h2>", paste(parts, collapse = "\n"),
           "</body></html>")
  }
  output$dl_stability_html <- shiny::downloadHandler(
    filename    = function() "stability_metrics.html",
    contentType = "text/html",
    content     = function(f) writeLines(stability_html(), f)
  )

  # ==============================================================================
  # PDF DOWNLOAD MODALS AND HANDLERS
  # ==============================================================================

  pdf_modal_ui <- function(id_dl_btn) {
    shiny::modalDialog(
      title = "PDF Export Settings",
      shiny::numericInput("pdf_w", "Width (inches)", 10, min=1),
      shiny::numericInput("pdf_h", "Height (inches)", 8, min=1),
      shiny::selectInput("pdf_p", "Paper Size", c("a4", "letter", "legal", "us", "special")),
      shiny::numericInput("pdf_pt", "Pointsize", 12, min=1),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::downloadButton(id_dl_btn, "Download PDF", class = "btn-primary")
      )
    )
  }

  shiny::observeEvent(input$btn_open_pdf_dens, { shiny::showModal(pdf_modal_ui("dl_plot_dens_confirm")) })
  shiny::observeEvent(input$btn_open_pdf_corr, { shiny::showModal(pdf_modal_ui("dl_plot_corr_confirm")) })
  shiny::observeEvent(input$btn_open_pdf_fit, { shiny::showModal(pdf_modal_ui("dl_plot_fit_confirm")) })
  shiny::observeEvent(input$btn_open_pdf_diag, { shiny::showModal(pdf_modal_ui("dl_plot_diag_confirm")) })

  output$dl_plot_dens_confirm <- shiny::downloadHandler(
    filename = "densities.pdf",
    content = function(f) {
      pdf(f, width = input$pdf_w, height = input$pdf_h, paper = input$pdf_p, pointsize = input$pdf_pt, onefile=FALSE)
      p <- bayescores::plot_densities(vals$model_fit_obj)
      print(p + ggplot2::theme(aspect.ratio = 1))
      dev.off()
    }
  )

  output$dl_plot_corr_confirm <- shiny::downloadHandler(
    filename = "correlated.pdf",
    content = function(f) {
      pdf(f, width = input$pdf_w, height = input$pdf_h, paper = input$pdf_p, pointsize = input$pdf_pt)
      print(bayescores::plot_correlated_densities(vals$model_fit_obj) + ggplot2::theme(aspect.ratio = 1))
      dev.off()
    }
  )

  output$dl_plot_fit_confirm <- shiny::downloadHandler(
    filename = "fit.pdf",
    content = function(f) {
      pdf(f, width = input$pdf_w, height = input$pdf_h, paper = input$pdf_p, pointsize = input$pdf_pt)
      graphics::par(pty = "s")
      plot(vals$model_fit_obj)
      dev.off()
    }
  )

  output$dl_plot_diag_confirm <- shiny::downloadHandler(
    filename = "diagnostics.pdf",
    content = function(f) {
      shiny::req(vals$model_fit_obj)
      pdf(f, width = input$pdf_w, height = input$pdf_h, paper = input$pdf_p, pointsize = input$pdf_pt)
      tryCatch({
        p_diag <- bayescores::model_diagnostics(vals$model_fit_obj)
        if(!is.null(p_diag) && (inherits(p_diag, "ggplot") || inherits(p_diag, "gtable"))) {
          print(p_diag)
        }
      }, error = function(e) {
        plot(vals$model_fit_obj$stan_fit, pars = c("lp__"))
      })
      dev.off()
    }
  )

  # ==============================================================================
  # Outputs
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

  output$cure_surv_output <- shiny::renderPrint({
    shiny::req(vals$cure_model_obj)
    print(vals$cure_model_obj)
    cat("\n")
    if (vals$hessian_warning_shared) {
      cat("WARNING: Hessian not positive definite - vcov may be invalid\n")
    }
    cat("PEARSON CORRELATION:", vals$analysis_results_full$Pearson_Correlation)
    if (vals$hessian_warning_shared) {
      cat(" (UNRELIABLE)")
    }
    cat("\n")
  })

  # CALIBRATION PLOT (2x2 GRID)
  output$calib_plot_output <- shiny::renderPlot({
    shiny::req(vals$calib_data)
    cd <- vals$calib_data

    par(mfrow = c(2, 2), mar = c(3, 3, 2, 1), oma = c(0, 0, 2, 0))

    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("black", "gray60"),
         xlab = "", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Shared - Control"))
    lines(cd$times, cd$CURE_S[,1], lty = 3, lwd = 2, col = "red")
    legend("topright", legend = c("KM", "Shared"), lty = c(1, 3), lwd = 2, col = c("black", "red"), bty = "n", cex = 0.8)

    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("gray60", "black"),
         xlab = "", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Shared - Experimental"))
    lines(cd$times, cd$CURE_S[,2], lty = 3, lwd = 2, col = "red")
    legend("topright", legend = c("KM", "Shared"), lty = c(1, 3), lwd = 2, col = c("black", "red"), bty = "n", cex = 0.8)

    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("black", "gray60"),
         xlab = "Time", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Free - Control"))
    if(!all(is.na(cd$CURE_S_FREE))) lines(cd$times, cd$CURE_S_FREE[,1], lty = 3, lwd = 2, col = "blue")
    legend("topright", legend = c("KM", "Free"), lty = c(1, 3), lwd = 2, col = c("black", "blue"), bty = "n", cex = 0.8)

    plot(cd$km_fit, conf.int = FALSE, lwd = 2, col = c("gray60", "black"),
         xlab = "Time", ylab = "S(t)", xlim = c(0, cd$tau),
         main = paste0("Free - Experimental"))
    if(!all(is.na(cd$CURE_S_FREE))) lines(cd$times, cd$CURE_S_FREE[,2], lty = 3, lwd = 2, col = "blue")
    legend("topright", legend = c("KM", "Free"), lty = c(1, 3), lwd = 2, col = c("black", "blue"), bty = "n", cex = 0.8)

    mtext("Calibration: Shared Shape (Red) vs Free Shape (Blue)", outer = TRUE, cex = 1.2)
  })

  output$has_fit <- shiny::reactive({ !is.null(vals$fit_obj) })
  shiny::outputOptions(output, "has_fit", suspendWhenHidden = FALSE)

  # ==============================================================================
  # Accuracy bench: reconstructed HR/CI/medians versus published values.
  # ==============================================================================
  recon_metrics <- shiny::reactive({
    shiny::req(vals$cox_obj, vals$fit_obj)
    ci <- tryCatch(summary(vals$cox_obj)$conf.int, error = function(e) NULL)
    hr <- if (!is.null(ci)) as.numeric(ci[1, 1]) else NA_real_
    lo <- if (!is.null(ci)) as.numeric(ci[1, 3]) else NA_real_
    hi <- if (!is.null(ci)) as.numeric(ci[1, 4]) else NA_real_
    tb <- tryCatch(summary(vals$fit_obj)$table, error = function(e) NULL)
    meds <- c(NA_real_, NA_real_)
    if (!is.null(tb)) {
      if (is.matrix(tb) && "median" %in% colnames(tb)) {
        mc <- as.numeric(tb[, "median"]); k <- min(2, length(mc)); meds[seq_len(k)] <- mc[seq_len(k)]
      } else if (!is.null(names(tb)) && "median" %in% names(tb)) {
        meds[1] <- as.numeric(tb["median"])
      }
    }
    surv_key <- function(m) if (is.na(m)) Inf else m
    if (surv_key(meds[1]) >= surv_key(meds[2])) {
      med_hi <- meds[1]; med_lo <- meds[2]
    } else {
      med_hi <- meds[2]; med_lo <- meds[1]
    }
    list(hr = hr, lo = lo, hi = hi, med_hi = med_hi, med_lo = med_lo)
  })

  fmt_err <- function(recon, real) {
    if (is.null(real) || is.na(real) || is.null(recon) || is.na(recon)) return(c("—", "—"))
    abserr <- abs(recon - real)
    relerr <- if (real != 0) 100 * abserr / abs(real) else NA_real_
    c(sprintf("%.3g", abserr), if (is.na(relerr)) "—" else sprintf("%.1f%%", relerr))
  }

  output$accuracy_table <- shiny::renderTable({
    rm <- recon_metrics()
    rows <- list()
    rows[[length(rows)+1]] <- {
      e <- fmt_err(rm$hr, input$real_hr)
      data.frame(Metric = "Hazard ratio",
                 Reconstructed = if (is.na(rm$hr)) "—" else sprintf("%.3f", rm$hr),
                 Real = if (is.null(input$real_hr) || is.na(input$real_hr)) "—" else sprintf("%.3f", input$real_hr),
                 `Abs. error` = e[1], `Rel. error` = e[2], check.names = FALSE)
    }
    rows[[length(rows)+1]] <- data.frame(
      Metric = "HR 95% CI",
      Reconstructed = if (is.na(rm$lo)) "—" else sprintf("[%.3f, %.3f]", rm$lo, rm$hi),
      Real = if (is.null(input$real_hr_lo) || is.na(input$real_hr_lo)) "—" else sprintf("[%.3f, %.3f]", input$real_hr_lo, input$real_hr_hi),
      `Abs. error` = "—", `Rel. error` = "—", check.names = FALSE)
    rows[[length(rows)+1]] <- { e <- fmt_err(rm$med_hi, input$real_med1)
      data.frame(Metric = "Median · higher-survival arm",
                 Reconstructed = if (is.na(rm$med_hi)) "NR" else sprintf("%.2f", rm$med_hi),
                 Real = if (is.null(input$real_med1) || is.na(input$real_med1)) "—" else sprintf("%.2f", input$real_med1),
                 `Abs. error` = e[1], `Rel. error` = e[2], check.names = FALSE) }
    rows[[length(rows)+1]] <- { e <- fmt_err(rm$med_lo, input$real_med2)
      data.frame(Metric = "Median · lower-survival arm",
                 Reconstructed = if (is.na(rm$med_lo)) "NR" else sprintf("%.2f", rm$med_lo),
                 Real = if (is.null(input$real_med2) || is.na(input$real_med2)) "—" else sprintf("%.2f", input$real_med2),
                 `Abs. error` = e[1], `Rel. error` = e[2], check.names = FALSE) }
    do.call(rbind, rows)
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lcccc")

  output$accuracy_verdict <- shiny::renderUI({
    rm <- recon_metrics()
    if (is.null(input$real_hr) || is.na(input$real_hr) || is.na(rm$hr)) {
      return(shiny::helpText("Enter at least the real HR to get a verdict. Note: arms may be flipped relative to the paper; compare the HR on the same reference arm."))
    }
    rel <- 100 * abs(rm$hr - input$real_hr) / abs(input$real_hr)
    if (!is.finite(rel)) rel <- Inf
    in_ci <- isTRUE(!is.null(input$real_hr_lo) && !is.na(input$real_hr_lo) &&
      !is.null(input$real_hr_hi) && !is.na(input$real_hr_hi) &&
      rm$hr >= input$real_hr_lo && rm$hr <= input$real_hr_hi)
    col <- if (rel <= 5) "#16A34A" else if (rel <= 10) "#F59E0B" else "#D32F2F"
    lab <- if (rel <= 5) "Excellent" else if (rel <= 10) "Acceptable" else "Poor"
    msg <- if (rel <= 5) "well within tolerance"
           else if (rel <= 10) "within the 10% acceptable band"
           else "beyond the 10% acceptable band — revise the numbers at risk or re-digitize"
    shiny::div(style = sprintf("border-left:5px solid %s; background:#FBFAFF; padding:10px 14px; border-radius:8px;", col),
      shiny::HTML(sprintf("<b style='color:%s'>%s</b> — reconstructed HR is %.1f%% from the published HR (%s)%s.",
                          col, lab, rel, msg,
                          if (in_ci) "; it also falls inside the published 95% CI" else "")))
  })

  # TRUE once auto-clean has produced a reconstructed image distinct from the upload.
  has_clean <- shiny::reactive({
    !is.null(vals$processed_img_path) && !is.null(vals$original_img_path) &&
      !identical(vals$processed_img_path, vals$original_img_path) &&
      file.exists(vals$processed_img_path)
  })

  # Show the second image panel only once there are reconstructed CURVES to compare
  # against the original. The kmdig "clean" PNG is a near-duplicate of the upload, so
  # showing it next to the original is pointless — we only show the original beside
  # the fitted curves (a genuinely different, useful comparison).
  output$show_secondary_image <- shiny::reactive({
    !is.null(vals$mode) && vals$mode == "manual" && !is.null(vals$fit_obj)
  })
  shiny::outputOptions(output, "show_secondary_image", suspendWhenHidden = FALSE)

  # Dataset preview only once there is reconstructed/loaded IPD to preview.
  output$show_dataset_preview <- shiny::reactive({ !is.null(vals$final_ipd) })
  shiny::outputOptions(output, "show_dataset_preview", suspendWhenHidden = FALSE)

  # Numbers-at-risk grid only when there is a table to correct (image/manual mode).
  output$show_risk_grid <- shiny::reactive({
    !is.null(vals$risk_table_editable) && !is.null(vals$mode) && vals$mode != "dataset"
  })
  shiny::outputOptions(output, "show_risk_grid", suspendWhenHidden = FALSE)

  output$left_panel_title <- shiny::renderUI({
    shiny::span(if (!is.null(vals$fit_obj)) "Reconstructed Curves" else "Original Image")
  })
  output$right_panel_title <- shiny::renderUI({
    # After reconstruction the left panel shows the curves, so the right panel
    # returns to the original for comparison.
    shiny::span(if (!is.null(vals$fit_obj)) "Original Image"
                else if (has_clean()) "Reconstructed (clean)"
                else "Original Image")
  })

  # Left panel before reconstruction: keep the ORIGINAL upload in place so it stays
  # available to compare against the reconstruction shown on the right.
  output$clean_image_output <- shiny::renderImage({
    if (!is.null(vals$mode) && vals$mode == "manual" &&
        !is.null(vals$original_img_path) && file.exists(vals$original_img_path))
      list(src = vals$original_img_path, height = "100%")
    else
      list(src = "", alt = "")
  }, deleteFile = FALSE)

  # Right panel:
  #   - after reconstruction (step 4): the ORIGINAL, to compare with the curves now
  #     shown on the left;
  #   - after auto-clean but before reconstruction: the reconstructed (clean) image;
  #   - otherwise: mirror the original upload.
  output$original_image_output <- shiny::renderImage({
    if (!is.null(vals$mode) && vals$mode == "manual") {
      img <- if (!is.null(vals$fit_obj)) vals$original_img_path
             else if (has_clean()) vals$processed_img_path
             else vals$original_img_path
      if (!is.null(img) && file.exists(img)) return(list(src = img, height = "100%"))
    }
    list(src = "", alt = "")
  }, deleteFile = FALSE)

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

  output$dl_ipd_rda <- shiny::downloadHandler(
    filename="ipd.rda",
    content=function(f) {
      ipd <- vals$final_ipd
      save(ipd, file = f)
    }
  )

  # Reconstructed KM curves (ggplot) with the Cox model on top: HR, 95% CI and p.
  make_km_ggplot <- function() {
    shiny::req(vals$fit_obj, vals$final_ipd)
    # Generic group labels so the figure is study-agnostic (never the arm names).
    k    <- tryCatch(length(vals$fit_obj$strata), error = function(e) 0L)
    if (is.null(k) || is.na(k) || k < 1) k <- 1L
    labs <- if (k == 2) c("Group 1", "Group 2") else paste("Group", seq_len(k))
    p <- survminer::ggsurvplot(
      vals$fit_obj, data = vals$final_ipd,
      conf.int = FALSE, risk.table = FALSE, censor = TRUE,
      palette = if (k == 2) c("#1F6FB2", "#9AA0A6") else NULL,
      legend.labs = labs,
      xlab = "Time", ylab = "Survival probability",
      legend.title = "Group",
      ggtheme = ggplot2::theme_minimal(base_size = 13)
    )$plot
    cox <- vals$cox_obj
    if (!is.null(cox)) {
      sm  <- summary(cox)
      hr  <- sm$conf.int[1, 1]; lo <- sm$conf.int[1, 3]; hi <- sm$conf.int[1, 4]
      pv  <- sm$coefficients[1, 5]
      plab <- if (is.finite(pv) && pv < 0.001) "p < 0.001" else sprintf("p = %.3f", pv)
      lab <- sprintf("Cox HR %.2f (95%% CI %.2f–%.2f)\n%s", hr, lo, hi, plab)
      p <- p + ggplot2::annotate("text", x = -Inf, y = -Inf, label = lab,
                                 hjust = -0.05, vjust = -0.7, size = 4.3, fontface = "bold")
    }
    p
  }

  # Download a side-by-side PNG: the image as pasted (left) and the reconstructed
  # KM curves with the Cox HR/CI/p annotation (right).
  output$dl_composite_png <- shiny::downloadHandler(
    filename = function() paste0("km_comparison_", as.integer(Sys.time()), ".png"),
    content = function(file) {
      shiny::req(vals$fit_obj, vals$original_img_path)
      if (!file.exists(vals$original_img_path)) stop("Original image not available.")
      rp <- tempfile(fileext = ".png")
      ggplot2::ggsave(rp, plot = make_km_ggplot(), width = 7, height = 6, dpi = 150, bg = "white")

      left  <- magick::image_read(vals$original_img_path)
      right <- magick::image_read(rp)
      h     <- max(magick::image_info(left)$height, magick::image_info(right)$height)
      left  <- magick::image_resize(left,  paste0("x", h))
      right <- magick::image_resize(right, paste0("x", h))
      gap   <- magick::image_blank(24, h, color = "white")
      comp  <- magick::image_append(c(left, gap, right), stack = FALSE)
      magick::image_write(magick::image_background(comp, "white"), path = file, format = "png")
    }
  )
}

return(shiny::shinyApp(ui, server))
}
