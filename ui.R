suppressPackageStartupMessages({
  library(quanteda)
  library(shiny)

  library(shinyBS)
  library(shinyjs)
  library(shinyWidgets)
  library(markdown)
})

# Detect web/Docker deployment (hide GPU option in these environments)
is_web <- tryCatch({
  TextAnalysisR::check_web_deployment()
}, error = function(e) FALSE)

ui <- fluidPage(
  useShinyjs(),
  shinybusy::add_busy_spinner(
    spin = "fading-circle",
    position = "full-page",
    color = "#337ab7",
    height = "100px",
    width = "100px"
  ),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$meta(name = "description", content = "TextAnalysisR: Comprehensive text mining and semantic analysis toolkit"),
    tags$meta(name = "keywords", content = "text mining, topic modeling, semantic analysis, R Shiny"),

    # CSP temporarily disabled for testing
    # tags$meta(
    #   `http-equiv` = "Content-Security-Policy",
    #   content = paste(
    #     "default-src 'self';",
    #     "script-src 'self' 'unsafe-inline' 'unsafe-eval' https://cdn.jsdelivr.net https://cdn.plot.ly https://translate.google.com http://translate.google.com https://translate.googleapis.com https://translate.googleusercontent.com https://translate-pa.googleapis.com;",
    #     "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com https://translate.googleapis.com https://translate.googleusercontent.com https://www.gstatic.com https://use.fontawesome.com https://cdnjs.cloudflare.com https://ka-f.fontawesome.com;",
    #     "font-src 'self' https://fonts.gstatic.com https://use.fontawesome.com https://cdnjs.cloudflare.com https://ka-f.fontawesome.com data:;",
    #     "img-src 'self' data: https: http://translate.google.com https://www.gstatic.com https://translate.google.com;",
    #     "connect-src 'self' https://translate.googleapis.com https://translate-pa.googleapis.com http://translate.googleapis.com;",
    #     "frame-src 'self' http: https: https://translate.google.com https://translate.googleusercontent.com;"
    #   )
    # ),
    tags$meta(`http-equiv` = "X-Content-Type-Options", content = "nosniff"),
    tags$meta(`http-equiv` = "X-XSS-Protection", content = "1; mode=block"),
    tags$meta(`http-equiv` = "Referrer-Policy", content = "no-referrer-when-downgrade"),

    tags$meta(name = "theme-color", content = "#337ab7"),

    # Font Awesome icons
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),

    # Google Translate API (external library - must remain here)
    tags$script(src = "https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit")
  ),

  tags$div(
    class = "top-right-controls",
    tags$a(
      id = "tts_toggle",
      href = "#",
      tabindex = "2",
      style = "cursor: pointer; text-decoration: none; color: #475569; font-size: 26px; user-select: none;",
      `aria-label` = "Text to speech",
      title = "Text to Speech (Alt+S)",
      tags$i(class = "fa fa-volume-up", id = "tts_icon", `aria-hidden` = "true", style = "pointer-events: none;")
    ),
    tags$a(
      id = "dark_mode_toggle",
      href = "javascript:void(0);",
      onclick = "toggleDarkMode()",
      tabindex = "3",
      style = "cursor: pointer; text-decoration: none; color: #475569; font-size: 30px;",
      `aria-label` = "Toggle dark mode",
      title = "Toggle dark mode",
      tags$i(class = "fa fa-moon", `aria-hidden` = "true")
    ),
    tags$div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$a(
        id = "translate_icon",
        href = "#",
        tabindex = "4",
        style = "cursor: pointer; text-decoration: none; color: #475569; font-size: 26px; user-select: none;",
        `aria-label` = "Select language",
        title = "Select Language",
        tags$i(class = "fa fa-globe", `aria-hidden` = "true", style = "pointer-events: none;")
      ),
      tags$div(
        id = "google_translate_element",
        style = "min-width: 120px; min-height: 30px; display: none;"
      )
    )
  ),
  includeCSS("www/styles.css"),
  includeScript("www/script.js"),

  tags$a(
    href = "#main-content",
    class = "skip-link",
    tabindex = "1",
    "Skip to main content"
  ),

  tags$div(
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    id = "status_region",
    class = "sr-only"
  ),

  titlePanel("TextAnalysisR",
    windowTitle = "TextAnalysisR: Text Mining and Semantic Analysis Toolkit"
  ),
  tags$main(
    id = "main-content",
    role = "main",
    tabindex = "-1",
    navbarPage(
      "",
      id = "main_navbar",
      tabPanel(
        "Home",
        fluidRow(
          column(
            width = 3,
            wellPanel(
              style = "padding: 0; border: none; box-shadow: none; background: transparent;",
              tags$ul(
                class = "nav nav-pills nav-stacked",
                id = "home_nav_menu",
                tags$li(tags$a(href = "#about-tab", "data-toggle" = "tab", "About")),
                tags$li(tags$a(href = "#semantic-tab", "data-toggle" = "tab", "Semantic Analysis")),
                tags$li(tags$a(href = "#lexicon-tab", "data-toggle" = "tab", "Sentiment Lexicons")),
                tags$li(tags$a(href = "#cyber-tab", "data-toggle" = "tab", "Cybersecurity")),
                tags$li(tags$a(href = "#access-tab", "data-toggle" = "tab", "Web Accessibility")),
                tags$li(tags$a(href = "#resources-tab", "data-toggle" = "tab", "Resources")),
                tags$li(tags$a(href = "#support-tab", "data-toggle" = "tab", "Support"))
              )
            )
          ),
          column(
            width = 9,
            tags$div(
              class = "tab-content",
              tags$div(id = "about-tab", class = "tab-pane", div(id = "about-content", class = "markdown-content", uiOutput("about_content"))),
              tags$div(id = "semantic-tab", class = "tab-pane", div(id = "installation-semantic-content", class = "markdown-content", uiOutput("installation_semantic_content"))),
              tags$div(id = "lexicon-tab", class = "tab-pane", div(id = "installation-lexical-content", class = "markdown-content", uiOutput("installation_lexical_content"))),
              tags$div(id = "cyber-tab", class = "tab-pane", div(id = "cybersecurity-content", class = "markdown-content", uiOutput("cybersecurity_content"))),
              tags$div(id = "access-tab", class = "tab-pane", div(id = "web-accessibility-content", class = "markdown-content", uiOutput("web_accessibility_content"))),
              tags$div(id = "resources-tab", class = "tab-pane", div(id = "links-content", class = "markdown-content", uiOutput("links_content"))),
              tags$div(id = "support-tab", class = "tab-pane", div(id = "support-content", class = "markdown-content", uiOutput("support_content")))
            )
          )
        )
      ),
    tabPanel(
      "Upload",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          selectizeInput(
            "dataset_choice",
            "Dataset",
            choices = c(
              "Select a dataset" = "",
              "Upload an Example Dataset",
              "Upload Your File",
              "Copy and Paste Text"
            ),
            selected = "",
            options = list(placeholder = "Select dataset", loadThrottle = 0)
          ),
          fileInput("file", "File upload",
            multiple = FALSE,
            accept = c(".xlsx", ".xls", ".xlsm", ".csv", ".pdf", ".docx", ".txt")
          ),
          conditionalPanel(
            condition = "input.dataset_choice == 'Upload Your File'",
            uiOutput("pdf_status_indicator")
          ),
          conditionalPanel(
            condition = "input.dataset_choice == 'Upload Your File'",
            uiOutput("multimodal_options_ui")
          ),
          conditionalPanel(
            condition = "input.dataset_choice == 'Copy and Paste Text'",
            tags$div(
              class = "text-input-white-placeholder",
              textAreaInput("text_input", "Text input", "",
                rows = 10, placeholder = "Paste your text here...

Supports:
• Plain text
• Tabular data (Excel, CSV, web tables)"
              )
            )
          ),
          tags$div(
            class = "limits-info-box status-step-purple",
            style = "margin-top: 15px;",
            tags$i(class = "fa fa-info-circle limits-icon status-icon status-icon-purple"),
            tags$strong("Limits:", class = "limits-title"), " Max 100MB file upload, 50MB paste. Optimal: 1K-5K documents"
          )
        ),
        mainPanel(
          width = 9,
          DT::dataTableOutput("data_table")
        )
      )
    ),
    tabPanel(
      "Preprocess",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          conditionalPanel(
            condition = "input.conditioned == 1",
            tags$h5(
              HTML("<strong>Select columns</strong> <a href='https://tidyr.tidyverse.org/reference/unite.html' target='_blank' style='font-size: 16px;'>Source</a>"),
              tags$span("REQUIRED", style = "background-color: #dc3545; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            div(class = "checkbox-margin", checkboxGroupInput("show_vars",
              label = NULL,
              choices = NULL
            )),
            actionButton("apply", "Apply", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.conditioned == 2",
            tags$h5(
              HTML("<strong>Segment corpus into tokens</strong> <a href='https://quanteda.io/reference/tokens.html' target='_blank' style='font-size: 16px;'>Source</a>"),
              tags$span("OPTIONAL", style = "background-color: #6c757d; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            tags$div(
              class = "warning-box status-sidebar-warning",
              style = "padding: 0 12px; margin-top: 0;",
              tags$div(
                style = "margin: 0; padding: 0;",
                checkboxInput("math_mode",
                  HTML("<strong>Math Mode:</strong> Keep numbers, symbols, and punctuation"),
                  value = FALSE)
              )
            ),
            div(class = "checkbox-margin", checkboxGroupInput("segment_options",
              label = NULL,
              choices = list(
                "Convert to lowercase" = "lowercase",
                "Remove punctuation" = "remove_punct",
                "Remove symbols" = "remove_symbols",
                "Remove numbers" = "remove_numbers",
                "Remove URLs" = "remove_url",
                "Remove separators" = "remove_separators",
                "Split hyphens" = "split_hyphens",
                "Split tags" = "split_tags",
                "Include document variables" = "include_docvars",
                "Keep acronyms" = "keep_acronyms",
                "Keep padding" = "padding"
              ),
              selected = c("lowercase", "remove_punct", "remove_symbols", "remove_numbers", "remove_url",
                          "remove_separators", "split_hyphens", "split_tags", "include_docvars")
            )),
            numericInput("min_char",
              label = "Minimum characters per token",
              value = 2,
              min = 1,
              max = 10,
              step = 1),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("preprocess", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("skip_segment", "Skip", class = "btn-secondary btn-block")
              )
            )
          ),
          conditionalPanel(
            condition = "input.conditioned == 4",
            tags$h5(
              HTML("<strong>Detect multi-words</strong> <a href='https://www.tidytextmining.com/ngrams' target='_blank' style='font-size: 16px;'>Source</a>"),
              tags$span("OPTIONAL", style = "background-color: #6c757d; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            checkboxGroupInput(
              "ngram_sizes",
              "N-gram sizes",
              choices = list(
                "Bigrams (2 words)" = "2",
                "Trigrams (3 words)" = "3",
                "4-grams (4 words)" = "4",
                "5-grams (5 words)" = "5"
              ),
              selected = c("2", "3")
            ),
            sliderInput(
              "ngram_min_count",
              "Minimum frequency",
              min = 2,
              max = 20,
              value = 3,
              step = 1
            ),
            sliderInput(
              "ngram_min_lambda",
              "Minimum lambda (collocation strength)",
              min = 0,
              max = 10,
              value = 3,
              step = 0.5
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("detect_ngrams", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("skip_ngram_detection", "Skip", class = "btn-secondary btn-block")
              )
            ),
            tags$hr(),
            tags$h5(strong("Compound selected n-grams"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            selectizeInput(
              "multi_word_expressions",
              label = "N-grams to compound",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = TRUE,
                placeholder = "Select from detected n-grams"
              )
            ),
            div(class = "checkbox-margin", checkboxGroupInput("stopword_options",
              label = "Clean stopwords from compound edges",
              choices = list(
                "Remove leading stopwords" = "leading_stopwords",
                "Remove trailing stopwords" = "trailing_stopwords"
              ),
              selected = c("leading_stopwords", "trailing_stopwords")
            )),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("dictionary", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("skip_dictionary", "Skip", class = "btn-secondary btn-block")
              )
            )
          ),
          conditionalPanel(
            condition = "input.conditioned == 5",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(
                HTML("<strong>Document-feature matrix</strong> <a href='https://quanteda.io/reference/dfm.html' target='_blank' style='font-size: 16px;'>Source</a>"),
                tags$span("REQUIRED", style = "background-color: #dc3545; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
                style = "color: #0c1f4a; margin: 0;"
              ),
              actionLink("showDFMInfo", tags$i(class = "fas fa-info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Click for DFM guide")
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("dfm_btn", "Process", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                downloadButton("download_preprocessing_report", "Report", class = "btn-secondary btn-block")
              )
            )
          ),
          conditionalPanel(
            condition = "input.conditioned == 3",
            tags$h5(
              strong("Remove stopwords"),
              tags$span("OPTIONAL", style = "background-color: #6c757d; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            selectizeInput(
              "common_words",
              label = "Top 10 frequent words pre-selected",
              choices = NULL,
              multiple = TRUE,
              options = list(
                create = TRUE,
                placeholder = "Type to add more or modify"
              )
            ),
            div(
              class = "stopwords-container",
              style = "max-height: 400px; overflow-y: auto;",
              selectizeInput(
                "custom_stopwords",
                label = HTML("<strong>Predefined stopwords</strong> <a href='https://search.r-project.org/CRAN/refmans/stopwords/html/stopwords.html' target='_blank' style='font-size: 16px;'>Source</a>"),
                choices = stopwords::stopwords("en", source = "snowball"),
                selected = stopwords::stopwords("en", source = "snowball"),
                multiple = TRUE,
                options = list(create = TRUE)
              )
            ),
            br(),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("remove", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("skip_stopwords", "Skip", class = "btn-secondary btn-block")
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "conditioned",
            tabPanel(
              "1. Unite Texts",
              value = 1,
              conditionalPanel(
                condition = "output.has_united_table_results",
                br(),
                DT::dataTableOutput("united_table")
              ),
              conditionalPanel(
                condition = "!output.has_united_table_results",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-columns", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Select columns and click ",
                      tags$strong("'Apply'", style = "color: #0c1f4a;"),
                      " to unite text columns",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "2. Segment Texts",
              value = 2,
              conditionalPanel(
                condition = "output.has_preprocess_results",
                shiny::verbatimTextOutput("dict_print_preprocess")
              ),
              conditionalPanel(
                condition = "!output.has_preprocess_results",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-cut", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Configure options and click ",
                      tags$strong("'Apply'", style = "color: #0c1f4a;"),
                      " to tokenize texts",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "3. Remove Stopwords",
              value = 3,
              conditionalPanel(
                condition = "output.step_3_outdated == true",
                div(
                  class = "status-outdated",
                  tags$i(class = "fa fa-exclamation-triangle status-icon-lg status-icon-warning"),
                  tags$span(
                    tags$strong("Outdated Results: "),
                    "Based on previous Step 2 settings. Click ",
                    tags$strong("Apply"),
                    " to update with latest Step 2 output."
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_stopword_results",
                br(),
                div(
                  style = "margin-bottom: 20px; overflow: visible;",
                  plotly::plotlyOutput("stopword_plot", height = 500, width = "100%")
                ),
                br(),
                DT::dataTableOutput("stopword_table")
              ),
              conditionalPanel(
                condition = "!output.has_stopword_results",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-filter", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Select stopwords and click ",
                      tags$strong("'Apply'", style = "color: #0c1f4a;"),
                      " to remove common words",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "4. Multi-Word Dictionary",
              value = 4,
              br(),
              tabsetPanel(
                id = "multiword_subtabs",
                tabPanel(
                  "Detected N-grams",
                  conditionalPanel(
                    condition = "output.has_ngram_detection_results",
                    br(),
                    uiOutput("ngram_detection_plot_uiOutput"),
                    br(),
                    DT::dataTableOutput("ngram_detection_table")
                  ),
                  conditionalPanel(
                    condition = "!output.has_ngram_detection_results",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 400px; margin: 0 auto;",
                        tags$i(class = "fa fa-search", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                        tags$p(
                          "Configure settings and click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " to detect multi-words",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  "Construct Multi-Words",
                  conditionalPanel(
                    condition = "output.step_4_outdated == true",
                    div(
                      class = "status-outdated",
                      tags$i(class = "fa fa-exclamation-triangle status-icon-lg status-icon-warning"),
                      tags$span(
                        tags$strong("Outdated Results: "),
                        "Based on previous Step 3 settings. Click ",
                        tags$strong("Apply"),
                        " to update with latest Step 3 output."
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.has_dictionary_results",
                    br(),
                    uiOutput("selected_ngrams_plot_uiOutput"),
                    br(),
                    DT::dataTableOutput("dictionary_table")
                  ),
                  conditionalPanel(
                    condition = "!output.has_dictionary_results",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 400px; margin: 0 auto;",
                        tags$i(class = "fa fa-link", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                        tags$p(
                          "Select n-grams and click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " to compound multi-words",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  )
                )
              )
            ),
            tabPanel(
              "5. Document-Feature Matrix",
              value = 5,
              conditionalPanel(
                condition = "output.step_5_outdated == true",
                div(
                  class = "status-outdated",
                  tags$i(class = "fa fa-exclamation-triangle status-icon-lg status-icon-warning"),
                  tags$span(
                    tags$strong("Outdated Results: "),
                    "Based on previous Step 4 settings. Click ",
                    tags$strong("Process"),
                    " to update with latest Step 4 output."
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_dfm_results",
                br(),
                div(
                  style = "margin-bottom: 20px; overflow: visible;",
                  plotly::plotlyOutput("dfm_plot", height = 500, width = "100%")
                ),
                br(),
                DT::dataTableOutput("dfm_table")
              ),
              conditionalPanel(
                condition = "!output.has_dfm_results",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-table", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Click ",
                      tags$strong("'Process'", style = "color: #0c1f4a;"),
                      " to create document-feature matrix",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Lexical Analysis",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          conditionalPanel(
            condition = "input.conditioned2 == 1 && input.linguistic_subtabs == 'lemmas'",
            tags$h5(
              HTML("<strong>Linguistic Analysis</strong> <a href='https://spacy.io/usage/linguistic-features#lemmatization' target='_blank' style='font-size: 16px;'>Source</a>"),
              tags$span("OPTIONAL", style = "background-color: #6c757d; color: white; padding: 2px 8px; border-radius: 3px; font-size: 13px; margin-left: 8px;"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("lemma", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("skip", "Skip", class = "btn-secondary btn-block")
              )
            )
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 1 && input.linguistic_subtabs == 'pos'",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(HTML("<strong>Part-of-Speech Configuration</strong> <a href='https://universaldependencies.org/u/pos/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showPOSInfo", icon("info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Click for POS tags guide")
            ),
            selectizeInput(
              "pos_filter",
              "Filter by POS tag",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "All POS tags (optional)"
              )
            ),
            sliderInput(
              "pos_top_n",
              "Top tags to display",
              value = 15,
              min = 1,
              max = 20,
              step = 1
            ),
            uiOutput("spacy_mode_ui"),
            checkboxInput(
              "include_dependency",
              "Show dependency relations",
              value = FALSE
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("apply_pos", "Apply", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                actionButton("generate_pos_report", "Report", class = "btn-primary btn-block", icon = icon("file-alt"))
              )
            )
          ),
          # Morphological Features sidebar
          conditionalPanel(
            condition = "input.conditioned2 == 1 && input.linguistic_subtabs == 'morphology'",
            div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(HTML("<strong>Morphology Analysis</strong>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showMorphInfo", icon("info-circle"),
                         style = "color: #337ab7; font-size: 16px;",
                         title = "Click for morphology guide")
            ),
            tags$p(
              "Run POS tagging first, then select features to analyze.",
              style = "font-size: 13px; color: #64748B; margin-bottom: 10px;"
            ),
            uiOutput("morph_status_ui"),
            checkboxGroupInput(
              "morph_features",
              NULL,
              choices = c(
                "Number (Sing/Plur)" = "Number",
                "Tense (Past/Pres/Fut)" = "Tense",
                "Verb Form (Fin/Inf/Part)" = "VerbForm",
                "Person (1/2/3)" = "Person",
                "Case (Nom/Acc/Gen)" = "Case",
                "Mood (Ind/Imp/Sub)" = "Mood",
                "Aspect (Perf/Imp/Prog)" = "Aspect"
              ),
              selected = c("Number", "Tense", "VerbForm", "Person")
            ),
            actionButton(
              "analyze_morphology",
              "Analyze Morphology",
              class = "btn-primary btn-block",
              icon = icon("language")
            )
          ),
          # Dependency Parsing sidebar
          conditionalPanel(
            condition = "input.conditioned2 == 1 && input.linguistic_subtabs == 'dependencies'",
            div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(HTML("<strong>Dependency Parsing</strong> <a href='https://universaldependencies.org/u/dep/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showDepInfo", icon("info-circle"),
                         style = "color: #337ab7; font-size: 16px;",
                         title = "Click for dependency relations guide")
            ),
            tags$p(
              "Dependency parsing shows grammatical relationships between words in a sentence.",
              style = "font-size: 13px; color: #64748B; margin-bottom: 10px;"
            )
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 2",
            tags$h5(strong("Word Frequency by Variable"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            selectizeInput(
              "continuous_var_3",
              "Continuous variable",
              choices = NULL,
              multiple = FALSE
            ),
            selectizeInput(
              "type_terms",
              "Terms",
              choices = NULL,
              options = list(maxItems = 20, multiple = TRUE, placeholder = "Select or type terms")
            ),
            tags$p(
              "Track term frequencies over the continuous variable.",
              style = "font-size: 13px; color: #64748B; margin-top: -5px; margin-bottom: 10px;"
            ),
            sliderInput(
              "height_line_con_var_plot",
              "Plot height",
              value = 500,
              min = 200,
              max = 2000,
              step = 50
            ),
            sliderInput(
              "width_line_con_var_plot",
              "Plot width",
              value = 1000,
              min = 500,
              max = 2000,
              step = 50
            ),
            actionButton("plot_term", "Plot Terms", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 4",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(HTML("<strong>Lexical Diversity Analysis</strong> <a href='https://github.com/quanteda/quanteda.textstats' target='_blank' style='font-size: 16px; color: #337ab7;'>Source</a>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showLexDivMetricsInfo", icon("info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Click for detailed metric descriptions")
            ),
            selectizeInput(
              "lexdiv_doc_id_var",
              "Document ID variable",
              choices = NULL,
              selected = "",
              options = list(
                allowEmptyOption = TRUE,
                persist = TRUE,
                placeholder = "Optional"
              )
            ),
            br(),
            checkboxGroupInput(
              "lexdiv_metrics",
              "Metrics to calculate",
              choices = c(
                "MTLD (Most Recommended)" = "MTLD",
                "MATTR (Recommended)" = "MATTR",
                "MSTTR (Mean Segmental TTR)" = "MSTTR",
                "TTR (Type-Token Ratio)" = "TTR",
                "CTTR (Corrected TTR)" = "CTTR",
                "Herdan's C" = "C",
                "Guiraud's R" = "R",
                "Maas" = "Maas",
                "Yule's K" = "K",
                "Simpson's D" = "D"
              ),
              selected = c("MTLD", "MATTR", "TTR")
            ),
            actionButton("run_lexdiv_analysis", "Analyze", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 5",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(HTML("<strong>Readability Analysis</strong> <a href='https://github.com/quanteda/quanteda.textstats' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showReadabilityMetricsInfo", icon("info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Click for detailed metric descriptions")
            ),
            selectizeInput(
              "readability_doc_id_var",
              "Document ID variable",
              choices = NULL,
              selected = "",
              options = list(
                allowEmptyOption = TRUE,
                persist = TRUE,
                placeholder = "Optional"
              )
            ),
            br(),
            checkboxGroupInput(
              "readability_metrics",
              "Metrics to calculate",
              choices = c(
                "Flesch Reading Ease" = "flesch",
                "Flesch-Kincaid Grade" = "flesch_kincaid",
                "Gunning Fog" = "gunning_fog",
                "SMOG" = "smog",
                "Automated Readability Index" = "ari",
                "Coleman-Liau" = "coleman_liau"
              ),
              selected = c("flesch", "flesch_kincaid", "gunning_fog")
            ),
            tags$hr(),
            tags$h5(strong("Visualization Options"), style = "color: #0c1f4a; margin-top: 10px; margin-bottom: 10px;"),
            radioButtons(
              "readability_view_type",
              "View:",
              choices = c(
                "Overall Distribution" = "distribution",
                "Group Comparison" = "group",
                "Top Documents" = "document"
              ),
              selected = "distribution"
            ),
            conditionalPanel(
              condition = "input.readability_view_type == 'group'",
              selectizeInput(
                "readability_group_var",
                "Group by:",
                choices = NULL,
                selected = NULL,
                multiple = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.readability_view_type == 'document'",
              sliderInput(
                "readability_top_n",
                "Number of documents",
                value = 15,
                min = 1,
                max = 30,
                step = 1
              )
            ),
            actionButton("run_readability_analysis", "Analyze", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 3",
            tags$h5(HTML("<strong>Keyword Extraction</strong> <a href='https://quanteda.io/reference/dfm_tfidf.html' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            conditionalPanel(
              condition = "input.keywords_subtabs == 'tfidf'",
              sliderInput(
                "tfidf_top_n",
                "Number of keywords",
                value = 20,
                min = 1,
                max = 50,
                step = 1
              ),
              checkboxInput(
                "tfidf_normalize",
                "Normalize TF-IDF scores",
                value = TRUE
              ),
              tags$p(
                "Scales scores to 0-1 range.",
                style = "font-size: 11px; color: #64748B; margin-top: -5px; margin-bottom: 10px;"
              )
            ),
            conditionalPanel(
              condition = "input.keywords_subtabs == 'textrank'",
              sliderInput(
                "textrank_top_n",
                "Number of keywords",
                value = 15,
                min = 1,
                max = 30,
                step = 1
              ),
              selectizeInput(
                "tfidf_group_var",
                "Compare groups (for keyness)",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Select grouping variable")
              ),
              tags$p(
                "Finds words used significantly more in one group vs others.",
                style = "font-size: 11px; color: #64748B; margin-top: -5px; margin-bottom: 10px;"
              )
            ),
            conditionalPanel(
              condition = "input.keywords_subtabs == 'comparison'",
              sliderInput(
                "comparison_top_n",
                "Keywords to display",
                value = 10,
                min = 1,
                max = 20,
                step = 1
              )
            ),
            actionButton("run_keyword_extraction", "Extract", class = "btn-primary btn-block")
          ),
          # Log Odds Ratio Analysis sidebar
          conditionalPanel(
            condition = "input.conditioned2 == 6",
            tags$h5(HTML("<strong>Log Odds Ratio Analysis</strong>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$p(
              "Compare word frequencies between categories to identify distinctive terms.",
              style = "font-size: 13px; color: #64748B; margin-bottom: 10px;"
            ),
            selectizeInput(
              "log_odds_group_var",
              "Grouping variable",
              choices = NULL,
              multiple = FALSE,
              options = list(placeholder = "Select category variable")
            ),
            radioButtons(
              "log_odds_comparison_mode",
              "Comparison mode",
              choices = c(
                "Binary (2 categories)" = "binary",
                "Pairwise (all pairs)" = "pairwise"
              ),
              selected = "binary"
            ),
            conditionalPanel(
              condition = "input.log_odds_comparison_mode == 'binary'",
              selectizeInput(
                "log_odds_reference",
                "Reference category",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "First category")
              )
            ),
            sliderInput(
              "log_odds_top_n",
              "Top terms per direction",
              value = 10,
              min = 1,
              max = 25,
              step = 1
            ),
            sliderInput(
              "log_odds_min_count",
              "Minimum word count",
              value = 5,
              min = 1,
              max = 20,
              step = 1
            ),
            actionButton("run_log_odds", "Calculate", class = "btn-primary btn-block", icon = icon("balance-scale"))
          ),
          # Lexical Dispersion sidebar
          conditionalPanel(
            condition = "input.conditioned2 == 7",
            tags$h5(HTML("<strong>Lexical Dispersion</strong>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$p(
              "Visualize where selected terms appear across documents in your corpus.",
              style = "font-size: 13px; color: #64748B; margin-bottom: 10px;"
            ),
            selectizeInput(
              "dispersion_terms",
              "Terms",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Type or select terms...",
                maxItems = 10,
                create = TRUE
              )
            ),
            radioButtons(
              "dispersion_scale",
              "Position scale",
              choices = c(
                "Relative (0-1)" = "relative",
                "Absolute (token position)" = "absolute"
              ),
              selected = "relative"
            ),
            checkboxInput(
              "dispersion_show_metrics",
              "Show dispersion metrics",
              value = TRUE
            ),
            actionButton("run_dispersion", "Analyze", class = "btn-primary btn-block", icon = icon("chart-line"))
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 1 && (input.linguistic_subtabs == 'ner' || input.linguistic_subtabs == 'dependencies')",
            selectizeInput(
              "lemma_doc_id_var",
              "Document ID variable",
              choices = NULL,
              selected = "",
              options = list(
                allowEmptyOption = TRUE,
                persist = TRUE,
                placeholder = "Optional"
              )
            )
          ),
          conditionalPanel(
            condition = "input.conditioned2 == 1 && input.linguistic_subtabs == 'ner'",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(
                HTML("<strong>Filter by entity type</strong> <a href='https://spacy.io/usage/linguistic-features#named-entities' target='_blank' style='font-size: 16px; color: #337ab7;'>Source</a>"),
                style = "color: #0c1f4a; margin: 0;"
              ),
              actionLink("showNERInfo", icon("info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Click for coding guide")
            ),
            selectizeInput(
              "entity_type_filter",
              NULL,
              choices = c("All Types" = "", "PERSON", "ORG", "GPE", "DATE", "MONEY", "CARDINAL", "ORDINAL", "PERCENT", "PRODUCT", "EVENT", "WORK_OF_ART", "LAW", "LANGUAGE", "LOC", "FAC", "NORP", "TIME", "QUANTITY", "CONCEPT", "THEME", "CODE", "CATEGORY", "CUSTOM"),
              selected = "",
              multiple = TRUE,
              options = list(placeholder = "All entity types")
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("apply_ner_filter", "Show Entities",
                         class = "btn-primary btn-block",
                         icon = icon("eye"))
            ),
            tags$hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            tags$div(
              style = "margin-bottom: 15px;",
              tags$h5(
                HTML("<strong>Custom Entity Types</strong>"),
                style = "color: #0c1f4a; margin-bottom: 10px;"
              ),
              tags$p(
                "Define custom entity types with colors. Edit cells to use these types.",
                style = "font-size: 13px; color: #64748B; margin-bottom: 10px;"
              ),
              div(
                style = "display: flex; gap: 8px; align-items: flex-end; margin-bottom: 10px;",
                div(
                  style = "flex: 2;",
                  textInput("custom_entity_name", NULL, placeholder = "Entity name")
                ),
                div(
                  style = "flex: 1;",
                  if (requireNamespace("colourpicker", quietly = TRUE)) {
                    colourpicker::colourInput("custom_entity_color", NULL, value = "#9C27B0", showColour = "background")
                  } else {
                    textInput("custom_entity_color", NULL, value = "#9C27B0", placeholder = "#HEX color")
                  }
                ),
                actionButton("add_custom_entity_type", icon("plus"),
                           class = "btn-primary btn-sm",
                           style = "height: 34px;",
                           title = "Add custom entity type")
              ),
              uiOutput("custom_entity_list")
            ),
            tags$hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
              tags$h5(
                HTML("<strong>Custom Entity Coding</strong>"),
                style = "color: #0c1f4a; margin-bottom: 10px;"
              )
            ),
            div(
              style = "display: flex; gap: 5px; margin-bottom: 10px;",
              actionButton("tab_type", "Type",
                         class = "btn-sm",
                         style = "flex: 1; font-size: 16px;"),
              actionButton("tab_upload", "Upload",
                         class = "btn-sm",
                         style = "flex: 1; font-size: 16px;")
            ),
            conditionalPanel(
              condition = "input.coding_mode == 'type' || typeof input.coding_mode == 'undefined'",
              textAreaInput(
                "batch_entity_text",
                "Define variables:",
                placeholder = "gender: female, male, women, men\nexplicit instruction: systematic, direct, guided practice",
                rows = 8,
                resize = "vertical"
              ),
              checkboxInput(
                "batch_include_lemmas",
                "Match word forms (e.g., 'run' finds running, ran, runs)",
                value = TRUE
              ),
              shinyjs::hidden(
                actionButton("add_batch_entities", "", style = "display: none;")
              )
            ),
            conditionalPanel(
              condition = "input.coding_mode == 'upload'",
                fileInput(
                  "codebook_upload",
                  "Upload codebook (CSV/Excel):",
                  accept = c(".csv", ".xlsx", ".xls"),
                  buttonLabel = "Browse",
                  placeholder = "No file selected"
                ),
                tags$p("Expected columns: construct_label, operational_definition",
                       style = "font-size: 16px; color: #6c757d; margin-top: -10px;"),
                tags$details(
                  tags$summary("View example format", style = "cursor: pointer; color: #337ab7; font-size: 16px;"),
                  tags$pre(
                    style = "background-color: #f8f9fa; padding: 8px; font-size: 16px; margin-top: 5px; line-height: 1.5;",
                    "construct_label,operational_definition\ngender,\"female, male, women, men\"\nexplicit instruction,\"systematic, direct, guided practice\"\nreading comprehension,\"comprehension, understanding, strategies\""
                  )
                ),
              shinyjs::hidden(
                actionButton("process_codebook", "", style = "display: none;")
              )
            ),
            tags$hr(style = "margin: 15px 0; border-color: #dee2e6;"),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                conditionalPanel(
                  condition = "input.coding_mode == 'type' || typeof input.coding_mode == 'undefined'",
                  actionButton("add_batch_entities_bottom", "Code",
                             class = "btn-info btn-block",
                             icon = icon("search-plus"))
                ),
                conditionalPanel(
                  condition = "input.coding_mode == 'upload'",
                  actionButton("process_codebook_bottom", "Process",
                             class = "btn-info btn-block",
                             icon = icon("play"))
                )
              ),
              div(
                style = "flex: 1;",
                downloadButton("export_entities", "Export",
                           class = "btn-primary btn-block",
                           icon = icon("download"))
              )
            )
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "conditioned2",
            tabPanel(
              "1. Linguistic Annotation",
              value = 1,
              br(),
              tabsetPanel(
                id = "linguistic_subtabs",
                tabPanel(
                  "Word Forms (Lemmas)",
                  value = "lemmas",
                  br(),
                  conditionalPanel(
                    condition = "output.lemma_ready == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 500px; margin: 0 auto;",
                        tags$i(class = "fa fa-info-circle", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " to run spaCy linguistic analysis, or ",
                          tags$strong("'Skip'", style = "color: #0c1f4a;"),
                          " to use standard tokenization.",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.lemma_ready == true",
                    div(
                      style = "margin-bottom: 20px; overflow: visible;",
                      plotly::plotlyOutput("lemma_plot", height = 500, width = "100%")
                    ),
                    br(),
                    DT::dataTableOutput("lemma_table")
                  )
                ),
                tabPanel(
                  "Part-of-Speech Tags",
                  value = "pos",
                  br(),
                  conditionalPanel(
                    condition = "output.pos_ready == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 500px; margin: 0 auto;",
                        tags$i(class = "fa fa-info-circle", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "First, click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " on the Word Forms tab to run spaCy analysis. Then configure and click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " here to view POS tags.",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.pos_ready == true",
                    uiOutput("pos_plot_uiOutput"),
                    br(),
                    DT::dataTableOutput("pos_table")
                  )
                ),
                tabPanel(
                  "Morphological Features",
                  value = "morphology",
                  br(),
                  conditionalPanel(
                    condition = "output.morph_ready == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 500px; margin: 0 auto;",
                        tags$i(class = "fa fa-info-circle", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "First, run POS tagging on the Word Forms tab. Then select morphological features in the sidebar and click ",
                          tags$strong("'Analyze Morphology'", style = "color: #0c1f4a;"),
                          " to extract features.",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.morph_ready == true",
                    tabsetPanel(
                      id = "morph_subtabs",
                      tabPanel(
                        "Feature Distribution",
                        br(),
                        uiOutput("morph_warning_ui"),
                        fluidRow(
                          column(6, plotly::plotlyOutput("morph_number_plot", height = "300px")),
                          column(6, plotly::plotlyOutput("morph_tense_plot", height = "300px"))
                        ),
                        fluidRow(
                          column(6, plotly::plotlyOutput("morph_verbform_plot", height = "300px")),
                          column(6, plotly::plotlyOutput("morph_person_plot", height = "300px"))
                        )
                      ),
                      tabPanel(
                        "Additional Features",
                        br(),
                        conditionalPanel(
                          condition = "output.has_additional_features == true",
                          fluidRow(
                            conditionalPanel(
                              condition = "output.has_case_data == true",
                              column(4, plotly::plotlyOutput("morph_case_plot", height = "300px"))
                            ),
                            conditionalPanel(
                              condition = "output.has_mood_data == true",
                              column(4, plotly::plotlyOutput("morph_mood_plot", height = "300px"))
                            ),
                            conditionalPanel(
                              condition = "output.has_aspect_data == true",
                              column(4, plotly::plotlyOutput("morph_aspect_plot", height = "300px"))
                            )
                          )
                        ),
                        conditionalPanel(
                          condition = "output.has_additional_features == false",
                          div(style = "padding: 60px 40px; text-align: center;",
                            div(style = "max-width: 400px; margin: 0 auto;",
                              tags$i(class = "fa fa-language", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", `aria-hidden` = "true"),
                              tags$p("Select Case, Mood, or Aspect checkboxes from the sidebar to display additional morphological features.",
                                style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;")
                            )
                          )
                        )
                      ),
                      tabPanel(
                        "Summary Table",
                        br(),
                        DT::dataTableOutput("morph_summary_table")
                      )
                    )
                  )
                ),
                tabPanel(
                  "Dependency Parsing",
                  value = "dependencies",
                  br(),
                  conditionalPanel(
                    condition = "output.dependencies_ready == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 500px; margin: 0 auto;",
                        tags$i(class = "fa fa-info-circle", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " on the Word Forms tab to run spaCy analysis and extract dependency parsing data.",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.dependencies_ready == true",
                    # Document selector for displaCy visualization
                    div(
                      style = "margin-bottom: 15px;",
                      selectInput(
                        "dep_viz_doc",
                        "Select document to visualize dependencies:",
                        choices = NULL,
                        width = "100%"
                      )
                    ),
                    # Download button for Dependency Parsing
                    div(
                      style = "margin-bottom: 15px; text-align: right;",
                      downloadButton("download_dep_plot", "Download Plot", class = "btn-secondary btn-sm")
                    ),
                    # displaCy dependency tree visualization
                    div(
                      style = "background: white; padding: 15px; border: 1px solid #dee2e6;
                               border-radius: 4px; margin-bottom: 20px; overflow-x: auto; overflow-y: hidden;",
                      # Wrapper to remove whitespace above the tree
                      div(
                        style = "margin-top: -100px; padding-top: 0;",
                        uiOutput("dep_displacy_html")
                      )
                    ),
                    # Existing full parse table
                    DT::dataTableOutput("spacy_full_table")
                  )
                ),
                tabPanel(
                  "Named Entity Recognition",
                  value = "ner",
                  br(),
                  conditionalPanel(
                    condition = "output.ner_ready == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 500px; margin: 0 auto;",
                        tags$i(class = "fa fa-info-circle", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Apply'", style = "color: #0c1f4a;"),
                          " on the Word Forms tab to run spaCy analysis and extract named entities.",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.ner_ready == true",
                    # Document selector for displaCy visualization
                    div(
                      style = "margin-bottom: 15px;",
                      selectInput(
                        "ner_viz_doc",
                        "Select document to visualize entities:",
                        choices = NULL,
                        width = "100%"
                      )
                    ),
                    # displaCy entity visualization
                    div(
                      style = "background: white; padding: 15px; border: 1px solid #dee2e6;
                               border-radius: 4px; margin-bottom: 20px; overflow-x: auto;",
                      uiOutput("ner_displacy_html")
                    ),
                    # Download button for NER
                    div(
                      style = "margin-bottom: 15px; text-align: right;",
                      downloadButton("download_ner_plot", "Download Plot", class = "btn-secondary btn-sm")
                    ),
                    # Existing frequency plot
                    uiOutput("entity_plot_uiOutput"),
                    br(),
                    DT::dataTableOutput("entity_table")
                  )
                )
              )
            ),
            tabPanel(
              "2. Frequency Trends",
              value = 2,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot height"),
                    class = "plot-dimensions-text"
                  ),
                  value = 2,
                  style = "success",
                  p(strong("Height of the plot")),
                  sliderInput(
                    inputId = "height_frequency_trends",
                    post = " px",
                    label = "height",
                    min = 200,
                    max = 2000,
                    step = 50,
                    value = 500
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_frequency_plot == true",
                uiOutput("line_con_var_plot_uiOutput")
              ),
              conditionalPanel(
                condition = "output.has_frequency_plot == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-chart-line", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Select terms, continuous variable, and click ",
                      tags$strong("'Plot Terms'", style = "color: #0c1f4a;"),
                      " to analyze frequency trends",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "3. Keywords",
              value = 3,
              br(),
              tabsetPanel(
                id = "keywords_subtabs",
                tabPanel(
                  "TF-IDF",
                  value = "tfidf",
                  uiOutput("keywords_tfidf_uiOutput")
                ),
                tabPanel(
                  "Statistical Keyness",
                  value = "textrank",
                  uiOutput("keywords_textrank_uiOutput")
                ),
                tabPanel(
                  "Comparison",
                  value = "comparison",
                  uiOutput("keywords_comparison_uiOutput")
                )
              )
            ),
            tabPanel(
              "4. Lexical Diversity",
              value = 4,
              br(),
              uiOutput("lexical_diversity_uiOutput")
            ),
            tabPanel(
              "5. Readability",
              value = 5,
              br(),
              uiOutput("readability_results_uiOutput")
            ),
            tabPanel(
              "6. Log Odds Ratio",
              value = 6,
              br(),
              conditionalPanel(
                condition = "output.log_odds_ready == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 500px; margin: 0 auto;",
                    tags$i(class = "fa fa-balance-scale", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Select a grouping variable and click ",
                      tags$strong("'Calculate'", style = "color: #0c1f4a;"),
                      " to compare word usage between categories.",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    ),

                  )
                )
              ),
              conditionalPanel(
                condition = "output.log_odds_ready == true",
                uiOutput("log_odds_plot_uiOutput"),
                br(),
                DT::dataTableOutput("log_odds_table")
              )
            ),
            tabPanel(
              "7. Lexical Dispersion",
              value = 7,
              br(),
              conditionalPanel(
                condition = "output.dispersion_ready == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 500px; margin: 0 auto;",
                    tags$i(class = "fa fa-chart-line", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Select terms in the sidebar and click ",
                      tags$strong("'Analyze'", style = "color: #0c1f4a;"),
                      " to visualize their dispersion across documents.",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    ),

                  )
                )
              ),
              conditionalPanel(
                condition = "output.dispersion_ready == true",
                uiOutput("dispersion_plot_uiOutput"),
                conditionalPanel(
                  condition = "input.dispersion_show_metrics == true",
                  br(),
                  div(
                    style = "margin-top: 20px;",
                    tags$h5("Dispersion Metrics", style = "color: #0c1f4a; margin-bottom: 10px;"),
                    DT::dataTableOutput("dispersion_metrics_table")
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Semantic Analysis",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",
          # Category variable - always visible
          div(
            style = "margin-bottom: 15px; padding-bottom: 15px; border-bottom: 1px solid #e2e8f0;",
            selectizeInput(
              "doc_category_var",
              "Category variable",
              choices = NULL,
              selected = "",
              options = list(
                allowEmptyOption = TRUE,
                persist = TRUE,
                placeholder = "Optional - enables category-based analysis"
              )
            )
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'summary'",
            tags$h5(strong("Configure document metadata"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            div(class = "checkbox-margin",
              selectizeInput(
                "doc_id_var",
                "Document ID variable",
                choices = NULL,
                selected = "",
                options = list(
                  allowEmptyOption = TRUE,
                  persist = TRUE,
                  placeholder = "Optional"
                )
              )
            ),
            br(),
            div(
              style = "margin-bottom: 15px;",
              actionButton("process_documents", "Process", class = "btn-primary btn-block")
            )
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs != 'summary'",
            div(
              style = "margin-bottom: 8px;",
              uiOutput("semantic_feature_space_selector")
            )
          ),

          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'similarity'",
            tags$h5(HTML("<strong>Document Similarity</strong> <a href='https://www.sbert.net/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            conditionalPanel(
              condition = "input.semantic_feature_space == 'embeddings'",
              div(
                selectInput(
                  "embedding_model",
                  "Embedding model",
                  choices = c(
                    "all-MiniLM-L6-v2" = "all-MiniLM-L6-v2",
                    "all-mpnet-base-v2" = "all-mpnet-base-v2"
                  ),
                  selected = "all-MiniLM-L6-v2"
                )
              )
            ),
            conditionalPanel(
              condition = "input.semantic_feature_space == 'words'"
            ),
            conditionalPanel(
              condition = "input.semantic_feature_space == 'ngrams'"
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("calculate_similarity", "Calculate", class = "btn-primary btn-block")
            ),
            div(
              style = "margin-bottom: 8px;",
              selectizeInput(
                "heatmap_category_filter",
                "Filter by category",
                choices = c("All Categories" = "all"),
                selected = "all"
              )
            ),
            tags$div(
              id = "words_similarity_ready_status",
              class = "status-sidebar-success",
              style = "display: none;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-success"),
              tags$span("Words similarity data calculated and ready")
            ),
            tags$div(
              id = "ngrams_similarity_ready_status",
              class = "status-sidebar-success",
              style = "display: none;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-success"),
              tags$span("N-grams similarity data calculated and ready")
            ),
            tags$div(
              id = "topics_similarity_ready_status",
              class = "status-sidebar-success",
              style = "display: none;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-success"),
              tags$span("Topics similarity data calculated and ready")
            ),
            tags$div(
              id = "embeddings_similarity_ready_status",
              class = "status-sidebar-success",
              style = "display: none;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-success"),
              tags$span("Embeddings similarity data calculated and ready")
            ),
            actionButton("visualize_similarity", "Visualize Results", class = "btn-primary btn-block", icon = icon("chart-bar"))
          ),

          # Comparative Analysis Controls (new subtab)
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'comparative'",
            tags$label(HTML("<strong>Comparative Analysis</strong>"), style = "font-weight: 700; margin-bottom: 8px; display: block;"),
            tags$div(
              class = "status-main-info",
              "Compare reference category against others to identify unique content, gaps, and cross-category opportunities."
            ),
            uiOutput("gap_reference_category_ui"),
            sliderInput(
              "gap_unique_threshold",
              "Unique threshold",
              min = 0.3, max = 0.9, value = 0.6, step = 0.05
            ),
            sliderInput(
              "gap_cross_policy_max",
              "Cross-category max",
              min = 0.6, max = 1.0, value = 0.8, step = 0.05
            ),
            actionButton("run_gap_analysis", "Run Comparative Analysis", class = "btn-info btn-block", icon = icon("search-minus")),
            tags$div(
              id = "gap_analysis_ready_status",
              class = "status-sidebar-success",
              style = "display: none; margin-top: 10px;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-success"),
              tags$span("Comparative analysis complete")
            )
          ),

          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'search'",
            selectInput(
              "search_method",
              "Search method:",
              choices = c(
                "Keyword" = "keyword",
                "Words" = "words",
                "N-grams" = "ngrams",
                "Embeddings" = "embeddings",
                "RAG Q&A (LLM)" = "rag"
              ),
              selected = "keyword"
            ),
            conditionalPanel(
              condition = "input.search_method != 'keyword' && input.search_method != 'rag'",
              uiOutput("search_method_status_message")
            ),
            conditionalPanel(
              condition = "input.search_method == 'rag'",
              # Provider selection
              radioButtons(
                "rag_provider",
                "Provider:",
                choices = c(
                  "Local (Ollama - Free, Private)" = "ollama",
                  "OpenAI (API Key Required)" = "openai",
                  "Gemini (API Key Required)" = "gemini"
                ),
                selected = "ollama"
              ),
              # Ollama model selection
              conditionalPanel(
                condition = "input.rag_provider == 'ollama'",
                selectInput(
                  "rag_ollama_model",
                  "Ollama Model:",
                  choices = c("phi3:mini", "llama3", "mistral", "nomic-embed-text"),
                  selected = "phi3:mini"
                ),
                tags$p(
                  style = "font-size: 12px; color: #666; margin-top: 5px;",
                  "Requires Ollama. Get it from ",
                  tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
                )
              ),
              # OpenAI settings
              conditionalPanel(
                condition = "input.rag_provider == 'openai'",
                selectInput(
                  "rag_openai_model",
                  "OpenAI Model:",
                  choices = c("gpt-4o-mini", "gpt-4o", "gpt-3.5-turbo"),
                  selected = "gpt-4o-mini"
                ),
                passwordInput(
                  "rag_openai_api_key",
                  "API Key:",
                  placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
                )
              ),
              # Gemini settings
              conditionalPanel(
                condition = "input.rag_provider == 'gemini'",
                selectInput(
                  "rag_gemini_model",
                  "Gemini Model:",
                  choices = c("gemini-2.0-flash", "gemini-1.5-flash", "gemini-1.5-pro"),
                  selected = "gemini-2.0-flash"
                ),
                passwordInput(
                  "rag_gemini_api_key",
                  "API Key:",
                  placeholder = "Enter key (or set GEMINI_API_KEY in .Renviron)"
                )
              )
            ),
            div(
              uiOutput("search_query_input"),
              sliderInput(
                "semantic_search_top_k",
                "Results to return",
                value = 5,
                min = 1,
                max = 20,
                step = 1
              ),
              br(),
              div(
                style = "margin-bottom: 15px;",
                actionButton("run_semantic_search", "Search", class = "btn-primary btn-block", icon = icon("search"))
              )
            )
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'document_clustering'",
            tags$h5(HTML("<strong>Document Clustering</strong> <a href='https://scikit-learn.org/stable/modules/clustering.html' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("document_clustering_status"),

            wellPanel(
              style = "padding: 12px; margin-bottom: 15px;",
              selectInput(
                "semantic_dimred_method",
                "Step 1: Dimensionality reduction",
                choices = c(
                  "PCA" = "PCA",
                  "t-SNE" = "t-SNE",
                  "UMAP" = "UMAP"
                ),
                selected = "UMAP"
              ),
              conditionalPanel(
                condition = "input.semantic_dimred_method == 'PCA'",
                sliderInput(
                  "semantic_pca_dims",
                  "Components",
                  value = 50,
                  min = 10,
                  max = 200,
                  step = 10
                )
              ),
              conditionalPanel(
                condition = "input.semantic_dimred_method == 't-SNE'",
                sliderInput(
                  "semantic_tsne_perplexity",
                  "Perplexity",
                  value = 30,
                  min = 5,
                  max = 100,
                  step = 5
                ),
                sliderInput(
                  "semantic_tsne_max_iter",
                  "Max iterations",
                  value = 1000,
                  min = 250,
                  max = 5000,
                  step = 250
                )
              ),
              conditionalPanel(
                condition = "input.semantic_dimred_method == 'UMAP'",
                sliderInput(
                  "semantic_umap_neighbors",
                  "Neighbors",
                  value = 15,
                  min = 5,
                  max = 50,
                  step = 1
                ),
                sliderInput(
                  "semantic_umap_min_dist",
                  "Min distance",
                  value = 0.1,
                  min = 0.01,
                  max = 0.5,
                  step = 0.01
                )
              ),
              div(
                style = "margin-top: 15px;",
                actionButton("run_dimensionality_reduction", "Reduce Dimensionality", class = "btn-primary btn-block", icon = icon("compress-arrows-alt"))
              )
            ),

            wellPanel(
              style = "padding: 12px; margin-bottom: 15px;",
              selectInput(
                "document_clustering_method",
                "Step 2: Clustering (optional)",
                choices = c(
                  "None" = "none",
                  "K-means" = "kmeans",
                  "Hierarchical" = "hierarchical",
                  "DBSCAN" = "dbscan",
                  "HDBSCAN" = "hdbscan"
                ),
                selected = "none"
              ),
              conditionalPanel(
                condition = "input.document_clustering_method == 'kmeans'",
                numericInput(
                  "kmeans_k",
                  "Number of clusters",
                  value = 5,
                  min = 2,
                  max = 20
                )
              ),
              conditionalPanel(
                condition = "input.document_clustering_method == 'hierarchical'",
                numericInput(
                  "hclust_k",
                  "Number of clusters",
                  value = 5,
                  min = 2,
                  max = 20
                )
              ),
              conditionalPanel(
                condition = "input.document_clustering_method == 'dbscan'",
                numericInput(
                  "dbscan_eps",
                  "Epsilon (neighborhood size)",
                  value = 0.5,
                  min = 0.1,
                  max = 2,
                  step = 0.1
                ),
                numericInput(
                  "dbscan_minPts",
                  "Min points",
                  value = 5,
                  min = 2,
                  max = 20
                )
              ),
              conditionalPanel(
                condition = "input.document_clustering_method == 'hdbscan'",
                numericInput(
                  "hdbscan_min_cluster_size",
                  "Min cluster size",
                  value = 5,
                  min = 2,
                  max = 50
                )
              ),
              conditionalPanel(
                condition = "input.document_clustering_method != 'none'",
                div(
                  style = "margin-top: 15px;",
                  actionButton("apply_clustering", "Apply Clustering", class = "btn-primary btn-block", icon = icon("project-diagram"))
                )
              )
            )
          ),
          conditionalPanel(
            condition = "false",
            tags$div(
              style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px;",
              tags$h5(HTML("<strong>Find Document Groups</strong> <a href='https://scikit-learn.org/stable/modules/clustering.html' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin: 0;"),
              actionLink("showClusteringInfo", icon("info-circle"),
                        style = "color: #337ab7; font-size: 16px;",
                        title = "Learn about document grouping")
            ),

            uiOutput("clustering_warning"),

            wellPanel(
              style = "padding: 15px; margin-bottom: 15px;",
              tags$h5(strong("Configuration"), style = "color: #0c1f4a; margin-bottom: 10px;"),
              radioButtons(
                "clustering_approach",
                "Discovery mode",
                choices = list(
                  "Automatic" = "auto",
                  "Manual" = "manual",
                  "Advanced" = "advanced"
                ),
                selected = "auto"
              ),

              conditionalPanel(
                condition = "input.clustering_approach == 'manual'",
                numericInput(
                  "manual_n_clusters",
                  "Number of groups",
                  value = 4,
                  min = 2,
                  max = 15
                )
              ),

              conditionalPanel(
                condition = "input.clustering_approach == 'advanced'",
                selectInput(
                  "semantic_cluster_method",
                  "Algorithm",
                  choices = c(
                    "K-means" = "kmeans",
                    "Hierarchical" = "hierarchical",
                    "DBSCAN (Density-based)" = "dbscan",
                    "Neural Topic Model" = "neural",
                    "Semantic Topic Modeling (All-in-one)" = "semantic_unified"
                  ),
                  selected = "kmeans"
                ),
                tags$p(
                  tags$i(class = "fa fa-info-circle", style = "margin-right: 5px;"),
                  tags$strong("Note:"), " Neural and Semantic models use transformer embeddings for advanced analysis.",
                  style = "font-size: 16px; color: #6b7280; margin-top: 5px;"
                ),
                conditionalPanel(
                  condition = "input.semantic_cluster_method == 'kmeans'",
                  numericInput(
                    "kmeans_n_clusters",
                    "Number of clusters",
                    value = 0,
                    min = 0,
                    max = 20,
                    step = 1
                  )
                ),
                conditionalPanel(
                  condition = "input.semantic_cluster_method == 'neural'",
                  numericInput(
                    "neural_n_topics",
                    "Number of topics",
                    value = 10,
                    min = 2,
                    max = 50,
                    step = 1
                  ),
                  numericInput(
                    "neural_hidden_size",
                    "Hidden layer size",
                    value = 100,
                    min = 50,
                    max = 500,
                    step = 50
                  )
                ),
                conditionalPanel(
                  condition = "input.semantic_cluster_method == 'semantic_unified'",
                  tags$h5("Unified Semantic Topic Modeling", style = "color: #10B981; font-weight: bold; margin-bottom: 10px;"),
                  tags$p("Combines multiple approaches for robust topic modeling",
                        style = "font-size: 16px; color: #6b7280; margin-bottom: 10px;"),
                  selectInput(
                    "unified_method",
                    "Base method",
                    choices = c(
                      "Embedding + Clustering" = "embedding_clustering",
                      "Similarity-based Grouping" = "similarity_grouping"
                    ),
                    selected = "embedding_clustering"
                  ),
                  numericInput(
                    "unified_n_topics",
                    "Number of topics",
                    value = 10,
                    min = 2,
                    max = 30,
                    step = 1
                  ),
                  sliderInput(
                    "unified_min_topic_size",
                    "Minimum topic size",
                    value = 3,
                    min = 2,
                    max = 10,
                    step = 1
                  )
                )
              )
            ),

            tags$details(
              style = "margin-top: 15px; margin-bottom: 15px;",
              tags$summary(
                style = "cursor: pointer; color: #495057; font-weight: 600;",
                "Advanced Settings"
              ),
              div(
                style = "margin-top: 10px; padding: 10px; border-radius: 4px;",
                sliderInput(
                  "semantic_cluster_seed",
                  "Random seed",
                  value = 123,
                  min = 1,
                  max = 1000,
                  step = 1
                ),
                selectInput(
                  "semantic_cluster_table_view",
                  "Table view",
                  choices = c(
                    "Cluster Summary" = "summary",
                    "Document Details" = "details"
                  ),
                  selected = "summary"
                )
              )
            ),

            div(
              style = "margin-bottom: 15px;",
              actionButton(
                "run_semantic_analysis_clustering",
                "Discover",
                class = "btn-primary btn-block",
                icon = icon("magnifying-glass-chart")
              )
            )
          ),
            conditionalPanel(
              condition = "output.has_clustering_results",
            wellPanel(
              style = "padding: 12px; margin-top: 15px; background-color: #f8f9fa;",
            tags$h5(strong("AI-Powered Cluster Labels"), style = "color: #0c1f4a; margin-bottom: 15px;"),
            uiOutput("cluster_labeling_status"),

            radioButtons(
              "cluster_label_provider",
              "AI Provider:",
              choices = c(
                "Local (Ollama - Free, Private)" = "ollama",
                "OpenAI (API Key Required)" = "openai",
                "Gemini (API Key Required)" = "gemini"
              ),
              selected = "ollama",
              inline = TRUE
            ),

            conditionalPanel(
              condition = "input.cluster_label_provider == 'ollama'",
              selectInput(
                "cluster_ollama_model",
                "Ollama Model:",
                choices = c("phi3:mini", "llama3", "mistral", "gemma2"),
                selected = "phi3:mini"
              ),
              tags$p(
                style = "font-size: 12px; color: #666;",
                "Requires Ollama. Get it from ",
                tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
              )
            ),

            conditionalPanel(
              condition = "input.cluster_label_provider == 'openai'",
              selectInput(
                "cluster_openai_model",
                "OpenAI Model:",
                choices = c("gpt-4o-mini", "gpt-4o", "gpt-4-turbo", "gpt-3.5-turbo"),
                selected = "gpt-4o-mini"
              ),
              passwordInput(
                "cluster_openai_api_key",
                "API Key:",
                placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
              )
            ),

            conditionalPanel(
              condition = "input.cluster_label_provider == 'gemini'",
              selectInput(
                "cluster_gemini_model",
                "Gemini Model:",
                choices = c("gemini-2.0-flash", "gemini-1.5-flash", "gemini-1.5-pro"),
                selected = "gemini-2.0-flash"
              ),
              passwordInput(
                "cluster_gemini_api_key",
                "API Key:",
                placeholder = "Enter key (or set GEMINI_API_KEY in .Renviron)"
              )
            ),

            numericInput(
              "top_terms_for_labels",
              "Top terms per cluster:",
              value = 10,
              min = 5,
              max = 20,
              step = 1
            ),

            actionButton(
              "generate_cluster_labels",
              "Generate Labels with AI",
              class = "btn-primary btn-block",
              icon = icon("wand-magic-sparkles")
            )
            )
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'sentiment'",
            tags$h5(HTML("<strong>Sentiment & Emotion Analysis</strong> <a href='https://github.com/juliasilge/tidytext' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),

            uiOutput("sentiment_status_message"),
            conditionalPanel(
              condition = "input.sentiment_subtabs == 'overall'",
              # Method selector
              radioButtons(
                "sentiment_method",
                "Analysis method",
                choices = c(
                  "Lexicon-based (Fast)" = "lexicon",
                  "Neural Network (Accurate)" = "neural",
                  "LLM-based (Explanations)" = "llm"
                ),
                selected = "lexicon",
                inline = TRUE
              ),
              # Lexicon-based options
              conditionalPanel(
                condition = "input.sentiment_method == 'lexicon'",
                selectInput(
                  "sentiment_lexicon",
                  "Sentiment lexicon",
                  choices = c(
                    "AFINN" = "afinn",
                    "Bing" = "bing",
                    "NRC" = "nrc"
                  ),
                  selected = "bing"
                ),
                actionButton("run_sentiment_analysis", "Analyze Sentiment", class = "btn-primary btn-block")
              ),
              # Neural/Embedding-based options
              conditionalPanel(
                condition = "input.sentiment_method == 'neural'",
                selectInput(
                  "neural_sentiment_model",
                  "Sentiment model",
                  choices = c(
                    "DistilBERT SST-2 (Fast)" = "distilbert-base-uncased-finetuned-sst-2-english",
                    "RoBERTa Sentiment (Accurate)" = "cardiffnlp/twitter-roberta-base-sentiment",
                    "BERT Multilingual" = "nlptown/bert-base-multilingual-uncased-sentiment"
                  ),
                  selected = "distilbert-base-uncased-finetuned-sst-2-english"
                ),
                actionButton("run_neural_sentiment", "Analyze Sentiment", class = "btn-primary btn-block")
              ),
              # LLM-based options
              conditionalPanel(
                condition = "input.sentiment_method == 'llm'",
                selectInput(
                  "llm_sentiment_provider",
                  "LLM Provider",
                  choices = c(
                    "Ollama (Local)" = "ollama",
                    "OpenAI" = "openai",
                    "Gemini" = "gemini"
                  ),
                  selected = "ollama"
                ),
                uiOutput("llm_sentiment_model_ui"),
                checkboxInput(
                  "llm_sentiment_explanation",
                  "Include sentiment explanations",
                  value = TRUE
                ),
                numericInput(
                  "llm_sentiment_batch_size",
                  "Batch size",
                  value = 5,
                  min = 1,
                  max = 20,
                  step = 1
                ),
                actionButton("run_llm_sentiment", "Analyze with LLM", class = "btn-primary btn-block", icon = icon("robot"))
              )
            ),
            conditionalPanel(
              condition = "input.sentiment_subtabs == 'category'",
              selectizeInput(
                "sentiment_category_var",
                "Category variable",
                choices = NULL,
                multiple = FALSE
              ),
              selectInput(
                "category_plot_type",
                "Plot type",
                choices = c(
                  "Bar Chart" = "bar",
                  "Box Plot" = "box",
                  "Violin Plot" = "violin"
                ),
                selected = "bar"
              )
            ),
            conditionalPanel(
              condition = "input.sentiment_subtabs == 'document'",
              selectizeInput(
                "sentiment_doc_id_var",
                "Document ID variable",
                choices = NULL,
                selected = "",
                options = list(
                  allowEmptyOption = TRUE,
                  persist = TRUE,
                  placeholder = "Optional"
                )
              ),
              sliderInput(
                "sentiment_top_docs",
                "Documents to show",
                value = 10,
                min = 1,
                max = 50,
                step = 1
              )
            ),
            conditionalPanel(
              condition = "input.sentiment_subtabs == 'emotion'",
              uiOutput("emotion_ready_indicator"),
              uiOutput("emotion_warning_message"),
              checkboxInput(
                "normalize_emotions",
                "Normalize emotion scores",
                value = FALSE
              ),
              selectizeInput(
                "emotion_group_var",
                "Group by variable",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Optional")
              )
            )
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'cooccurrence'",
            tags$h5(HTML("<strong>Word Co-occurrence Networks</strong> <a href='https://igraph.org/r/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            selectizeInput(
              "doc_var_co_occurrence",
              "Categorical variable",
              choices = NULL,
              multiple = FALSE,
              options = list(placeholder = "Optional")
            ),
            div(
              id = "global_cooccur_params", class = "global-params",
              sliderInput(
                "co_occurence_number_global",
                "Minimum co-occurrences",
                value = 10,
                min = 2,
                max = 150,
                step = 1
              ),
              sliderInput(
                "top_node_n_co_occurrence_global",
                "Top N nodes",
                value = 20,
                min = 0,
                max = 150,
                step = 1
              ),
              sliderInput(
                "node_label_size_cooccur",
                "Node label size",
                value = 22,
                min = 12,
                max = 40,
                step = 2
              ),
              selectInput(
                "community_method_cooccur",
                "Community detection",
                choices = c("Leiden (recommended)" = "leiden",
                            "Louvain" = "louvain"),
                selected = "leiden"
              ),
              selectInput(
                "node_size_cooccur",
                "Node size by",
                choices = c(
                  "Degree (connections)" = "degree",
                  "Betweenness (bridging)" = "betweenness",
                  "Frequency (word count)" = "frequency"
                ),
                selected = "degree"
              ),
              selectInput(
                "node_color_cooccur",
                "Node color by",
                choices = c(
                  "Community" = "community",
                  "Frequency (word count)" = "frequency"
                ),
                selected = "community"
              ),
              sliderInput(
                "nrows_co_occurrence",
                "Row numbers",
                value = 1,
                min = 1,
                max = 10,
                step = 1
              ),
                              div(
                  class = "category-toggle",
                  div(style = "margin-left: 15px; font-size: 16px;", checkboxInput("use_category_cooccur",
                    "Use category-specific analysis",
                    value = FALSE
                  ))
                )
            ),
            conditionalPanel(
              condition = "input.use_category_cooccur == true && input.doc_var_co_occurrence != 'None' && input.doc_var_co_occurrence != ''",
              tags$div(
                class = "status-main-warning",
                "Analyze top categories by document count for faster processing and clearer visualization."
              ),
              uiOutput("top_n_selector_cooccur"),
              uiOutput("category_selector_cooccur"),
              uiOutput("category_cooccur_controls")
            ),
            actionButton("plot_word_co_occurrence_network", "Plot Network", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.semantic_analysis_tabs == 'correlation'",
            tags$h5(HTML("<strong>Word Correlation Networks</strong> <a href='https://igraph.org/r/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            selectizeInput(
              "doc_var_correlation",
              "Categorical variable",
              choices = NULL,
              multiple = FALSE,
              options = list(placeholder = "Optional")
            ),
            div(
              id = "global_corr_params", class = "global-params",
              sliderInput(
                "common_term_n_global",
                "Common terms",
                value = 20,
                min = 5,
                max = 150,
                step = 1
              ),
              sliderInput(
                "corr_n_global",
                "Minimum correlation",
                value = 0.4,
                min = 0,
                max = 1,
                step = 0.1
              ),
              sliderInput(
                "top_node_n_correlation_global",
                "Top N nodes",
                value = 20,
                min = 0,
                max = 150,
                step = 1
              ),
              sliderInput(
                "node_label_size_corr",
                "Node label size",
                value = 22,
                min = 12,
                max = 40,
                step = 2
              ),
              selectInput(
                "community_method_corr",
                "Community detection",
                choices = c("Leiden (recommended)" = "leiden",
                            "Louvain" = "louvain"),
                selected = "leiden"
              ),
              selectInput(
                "node_size_corr",
                "Node size by",
                choices = c(
                  "Degree (connections)" = "degree",
                  "Betweenness (bridging)" = "betweenness",
                  "Frequency (word count)" = "frequency"
                ),
                selected = "degree"
              ),
              selectInput(
                "node_color_corr",
                "Node color by",
                choices = c(
                  "Community" = "community",
                  "Frequency (word count)" = "frequency"
                ),
                selected = "community"
              ),
              sliderInput(
                "nrows_correlation",
                "Row numbers",
                value = 1,
                min = 1,
                max = 10,
                step = 1
              ),
                              div(
                  class = "category-toggle",
                  div(style = "margin-left: 15px; font-size: 16px;", checkboxInput("use_category_corr",
                    "Use category-specific analysis",
                    value = FALSE
                  ))
                )
            ),
            conditionalPanel(
              condition = "input.use_category_corr == true && input.doc_var_correlation != 'None' && input.doc_var_correlation != ''",
              tags$div(
                class = "status-main-warning",
                tags$i(class = "fa fa-info-circle status-icon status-icon-warning"),
                "Analyze top categories by document count for faster processing and clearer visualization."
              ),
              uiOutput("top_n_selector_corr"),
              uiOutput("category_selector_corr"),
              uiOutput("category_corr_controls")
            ),
            actionButton("plot_word_correlation_network", "Plot Network", class = "btn-primary btn-block")
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "semantic_analysis_tabs",
            tabPanel(
              "1. Setup",
              value = "summary",
              br(),
              conditionalPanel(
                condition = "output.has_documents == true",
                DT::dataTableOutput("document_summary_table"),
                hr(),
                h4("Embedding Configuration", style = "margin-top: 20px;"),
                uiOutput("embedding_status_ui"),
                br(),
                fluidRow(
                  column(6,
                    selectInput(
                      "embedding_provider_setup",
                      "Embedding Provider:",
                      choices = c(
                        "Auto-detect (Recommended)" = "auto",
                        "Ollama (Local)" = "ollama",
                        "OpenAI" = "openai",
                        "Gemini" = "gemini",
                        "Sentence Transformers" = "sentence_transformers"
                      ),
                      selected = "auto"
                    )
                  ),
                  column(6,
                    selectInput(
                      "embedding_model_setup",
                      "Embedding Model:",
                      choices = c(
                        "Default (Auto)" = "",
                        "all-MiniLM-L6-v2" = "all-MiniLM-L6-v2",
                        "all-mpnet-base-v2" = "all-mpnet-base-v2",
                        "nomic-embed-text" = "nomic-embed-text",
                        "mxbai-embed-large" = "mxbai-embed-large"
                      ),
                      selected = ""
                    )
                  )
                ),
                actionButton(
                  "generate_embeddings",
                  "Generate Embeddings",
                  class = "btn-primary",
                  icon = icon("brain")
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-upload", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Load data and process documents in the ",
                      tags$strong("1. Setup", style = "color: #0c1f4a;"),
                      " tab first",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "2. Word Co-occurrence",
              value = "cooccurrence",
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 4,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_word_co_occurrence_network_plot",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 700
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_word_co_occurrence_network_plot",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_cooccurrence_plot == true",
                tabsetPanel(
                  id = "word_co_occur_subTab",
                  tabPanel(
                    "Plot",
                    uiOutput("word_co_occurrence_network_plot_uiOutput")
                  ),
                  tabPanel(
                    "Table",
                    uiOutput("word_co_occurrence_network_table_uiOutput")
                  ),
                  tabPanel(
                    "Summary",
                    uiOutput("word_co_occurrence_network_summary_uiOutput")
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_cooccurrence_plot == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Configure settings and click ",
                      tags$strong("'Plot Network'", style = "color: #0c1f4a;"),
                      " to visualize word co-occurrence",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "3. Word Correlation",
              value = "correlation",
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 4,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_word_correlation_network_plot",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 700
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_word_correlation_network_plot",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_correlation_plot == true",
                tabsetPanel(
                  id = "word_correlation_subTab",
                  tabPanel(
                    "Plot",
                    uiOutput("word_correlation_network_plot_uiOutput")
                  ),
                  tabPanel(
                    "Table",
                    uiOutput("word_correlation_network_table_uiOutput")
                  ),
                  tabPanel(
                    "Summary",
                    uiOutput("word_correlation_network_summary_uiOutput")
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_correlation_plot == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-share-alt", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Configure settings and click ",
                      tags$strong("'Plot Network'", style = "color: #0c1f4a;"),
                      " to visualize word correlation",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "4. Document Similarity",
              value = "similarity",
              conditionalPanel(
                condition = "output.has_documents == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-cog", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Process documents in the ",
                      tags$strong("1. Setup", style = "color: #0c1f4a;"),
                      " tab first",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_similarity_calculation == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-calculator", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Configure settings and click ",
                      tags$strong("'Calculate'", style = "color: #0c1f4a;"),
                      " to begin analysis",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_similarity_calculation == true && output.has_similarity_analysis == false",
                verbatimTextOutput("similarity_calculation_summary")
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_similarity_analysis == true",
                plotly::plotlyOutput("semantic_similarity_plot",
                  height = "auto", width = "100%"
                ),
                br(),
                DT::dataTableOutput("semantic_similarity_stats")
              )
            ),
            tabPanel(
              "5. Comparative Analysis",
              value = "comparative",
              br(),
              conditionalPanel(
                condition = "output.has_documents == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 500px; margin: 0 auto;",
                    tags$i(class = "fa fa-balance-scale", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Upload data to enable Comparative Analysis",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_gap_analysis == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 500px; margin: 0 auto;",
                    tags$i(class = "fa fa-balance-scale", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Run Comparative Analysis from the sidebar to compare categories and identify unique content, gaps, and cross-category opportunities.",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_gap_analysis == true",
                tags$h5(HTML("<strong>Comparative Analysis Results</strong>"), style = "color: #0c1f4a; margin-bottom: 15px;"),
                uiOutput("gap_reference_category_label"),
                tabsetPanel(
                  id = "gap_analysis_tabs",
                  tabPanel(
                    "Summary",
                    br(),
                    DT::dataTableOutput("gap_summary_stats")
                  ),
                  tabPanel(
                    "Heatmap",
                    br(),
                    tags$p("Cross-category similarity heatmap comparing reference documents against other categories.",
                           style = "color: #64748B; font-size: 16px; margin-bottom: 10px;"),
                    plotly::plotlyOutput("gap_cross_category_heatmap", height = "600px")
                  ),
                  tabPanel(
                    "Unique (Reference)",
                    br(),
                    tags$p("Reference items with low similarity to all comparison categories (distinctive content).",
                           style = "color: #64748B; font-size: 16px; margin-bottom: 10px;"),
                    DT::dataTableOutput("gap_unique_items")
                  ),
                  tabPanel(
                    "Missing (Comparison)",
                    br(),
                    tags$p("Comparison category items not well-covered by reference category (content gaps).",
                           style = "color: #64748B; font-size: 16px; margin-bottom: 10px;"),
                    DT::dataTableOutput("gap_missing_items")
                  ),
                  tabPanel(
                    "Cross-Category",
                    br(),
                    tags$p("Items with moderate similarity - potential for cross-category learning or transfer.",
                           style = "color: #64748B; font-size: 16px; margin-bottom: 10px;"),
                    DT::dataTableOutput("gap_cross_policy")
                  )
                )
              )
            ),
            tabPanel(
              "6. Semantic Search",
              value = "search",
              conditionalPanel(
                condition = "output.has_documents == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-cog", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Process documents in the ",
                      tags$strong("1. Setup", style = "color: #0c1f4a;"),
                      " tab first",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_search_results == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-search", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Enter a search query and click ",
                      tags$strong("'Search'", style = "color: #0c1f4a;"),
                      " to see results",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_documents == true && output.has_search_results == true",
                br(),
                uiOutput("search_method_indicator"),
                br(),
                DT::dataTableOutput("semantic_search_results")
              )
            ),
            tabPanel(
              "7. Sentiment & Emotion",
              value = "sentiment",
              br(),
              tabsetPanel(
                id = "sentiment_subtabs",
                tabPanel(
                  "Overall Sentiment",
                  value = "overall",
                  uiOutput("sentiment_overall_uiOutput")
                ),
                tabPanel(
                  "By Category",
                  value = "category",
                  uiOutput("sentiment_category_uiOutput")
                ),
                tabPanel(
                  "Document-Level",
                  value = "document",
                  uiOutput("sentiment_document_uiOutput")
                ),
                tabPanel(
                  "Emotion",
                  value = "emotion",
                  uiOutput("sentiment_emotion_uiOutput")
                )
              )
            ),
            tabPanel(
              "8. Document Clustering",
              value = "document_clustering",
              br(),
              tabsetPanel(
                id = "document_clustering_tabs",
                tabPanel(
                  "Visualization",
                  value = "viz",
                  conditionalPanel(
                    condition = "output.analysis_run == true",
                    plotly::plotlyOutput("semantic_space_plot", height = "600px", width = "100%"),
                    br(),
                    div(
                      style = "border: 1px solid #dee2e6; padding: 12px; margin-bottom: 15px; border-radius: 4px; background: white;",
                      fluidRow(
                        column(6,
                          uiOutput("space_feature_info")
                        ),
                        column(6,
                          uiOutput("space_quality_metrics")
                        )
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.analysis_run == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 400px; margin: 0 auto;",
                        tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Reduce Dimensionality'", style = "color: #0c1f4a;"),
                          " to generate visualization",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  "Group Analysis",
                  value = "groups",
                  br(),
                  conditionalPanel(
                    condition = "output.has_clusters == true",
                    DT::dataTableOutput("document_groups_table"),
                    tags$h5("Explore Groups", style = "color: #0c1f4a; margin-top: 15px;"),
                    fluidRow(
                      column(4,
                        selectInput(
                          "selected_document_group",
                          "Group",
                          choices = NULL,
                          width = "100%"
                        )
                      ),
                      column(8,
                        uiOutput("document_group_summary")
                      )
                    ),
                    br(),
                    fluidRow(
                      column(6,
                        tags$h5("Top Terms", style = "color: #0c1f4a; margin-bottom: 10px;"),
                        plotly::plotlyOutput("group_terms_plot", height = "300px")
                      ),
                      column(6,
                        tags$h5("Sample Documents", style = "color: #0c1f4a; margin-bottom: 10px;"),
                        DT::dataTableOutput("group_sample_docs")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "output.has_clusters == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 400px; margin: 0 auto;",
                        tags$i(class = "fa fa-layer-group", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Reduce Dimensionality'", style = "color: #0c1f4a;"),
                          " then optionally ",
                          tags$strong("'Apply Clustering'", style = "color: #0c1f4a;"),
                          " to create document groups",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  )
                ),
                tabPanel(
                  "Labels",
                  value = "labels",
                  br(),
                  conditionalPanel(
                    condition = "output.has_clusters == true",
                    fluidRow(
                      column(12,
                        wellPanel(
                          style = "padding: 15px; margin-bottom: 20px;",
                          tags$h5(strong("Label Generation"), style = "color: #0c1f4a; margin-bottom: 15px;"),
                          fluidRow(
                            column(4,
                              selectInput(
                                "label_method",
                                "Method:",
                                choices = c(
                                  "Top Terms (TF-IDF)" = "tfidf",
                                  "Representative Terms" = "representative",
                                  "Most Frequent Terms" = "frequent"
                                ),
                                selected = "tfidf"
                              )
                            ),
                            column(4,
                              numericInput(
                                "n_label_terms",
                                "Terms per label:",
                                value = 3,
                                min = 1,
                                max = 5
                              )
                            ),
                            column(4,
                              actionButton(
                                "generate_labels",
                                "Generate Labels",
                                class = "btn-primary",
                                icon = icon("tag"),
                                style = "margin-top: 25px;"
                              )
                            )
                          )
                        )
                      )
                    ),
                    plotly::plotlyOutput("labeled_space_plot", height = "500px"),
                    br(),
                    DT::dataTableOutput("labeled_groups_table")
                  ),
                  conditionalPanel(
                    condition = "output.has_clusters == false",
                    div(
                      style = "padding: 60px 40px; text-align: center;",
                      div(
                        style = "max-width: 400px; margin: 0 auto;",
                        tags$i(class = "fa fa-tags", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                        tags$p(
                          "Click ",
                          tags$strong("'Reduce Dimensionality'", style = "color: #0c1f4a;"),
                          " then ",
                          tags$strong("'Apply Clustering'", style = "color: #0c1f4a;"),
                          " to create groups for labeling",
                          style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    tabPanel(
      "Topic Modeling",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          class = "sidebar-panel",

          tags$h5(HTML("<strong>Topic modeling approach</strong>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
          tags$div(
            id = "topic_modeling_path",
            class = "shiny-input-radiogroup shiny-input-container",
            role = "radiogroup",
            tags$div(
              class = "radio",
              style = "display: flex; align-items: center; margin-bottom: 8px;",
              tags$label(
                style = "margin-bottom: 0; display: flex; align-items: center; width: 100%;",
                tags$input(
                  type = "radio",
                  name = "topic_modeling_path",
                  value = "probability",
                  checked = "checked"
                ),
                tags$span("Structural Topic Model (STM)", style = "margin-left: 5px;"),
                actionLink("showSTMInfo", icon("info-circle"),
                          style = "color: #337ab7; font-size: 16px; margin-left: 8px;",
                          title = "Learn about STM")
              )
            ),
            tags$div(
              class = "radio",
              style = "display: flex; align-items: center; margin-bottom: 8px;",
              tags$label(
                style = "margin-bottom: 0; display: flex; align-items: center; width: 100%;",
                tags$input(
                  type = "radio",
                  name = "topic_modeling_path",
                  value = "embedding"
                ),
                tags$span("Embedding-based Topics (UMAP+HDBSCAN)", style = "margin-left: 5px;"),
                actionLink("showEmbeddingTopicsInfo", icon("info-circle"),
                          style = "color: #337ab7; font-size: 16px; margin-left: 8px;",
                          title = "Learn about Embedding-based Topics")
              )
            ),
            tags$div(
              class = "radio",
              style = "display: flex; align-items: center; margin-bottom: 8px;",
              tags$label(
                style = "margin-bottom: 0; display: flex; align-items: center; width: 100%;",
                tags$input(
                  type = "radio",
                  name = "topic_modeling_path",
                  value = "hybrid"
                ),
                tags$span("Hybrid (STM + Embeddings)", style = "margin-left: 5px;"),
                actionLink("showHybridTopicsInfo", icon("info-circle"),
                          style = "color: #337ab7; font-size: 16px; margin-left: 8px;",
                          title = "Learn about Hybrid Topics")
              )
            )
          ),
          hr(),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 4 && input.searchKSubtabs != 'ai_rec'",
            tags$h5(
              HTML("<strong>Evaluate optimal topic number (K)</strong> <a href='https://github.com/bstewart/stm' target='_blank' style='font-size: 16px;'>Source</a>"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            sliderInput(
              "stm_K_range",
              "Range of topic numbers",
              value = c(5, 10),
              min = 2,
              max = 50
            ),
            selectizeInput(
              "stm_categorical_var",
              "Categorical covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectizeInput(
              "stm_continuous_var",
              "Continuous covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectInput("stm_init_type_search", "Initialization type",
              choices = c("Spectral", "LDA", "Random", "Custom"),
              selected = "Spectral"
            ),
            radioButtons("stm_gamma_prior_search", "Gamma prior",
              choices = c("Pooled", "L1"),
              selected = "Pooled"
            ),
            radioButtons("stm_kappa_prior_search", "Kappa prior",
              choices = c("L1", "Jeffreys"),
              selected = "L1"
            ),
            numericInput("stm_max_em_its_search", "Max EM iterations",
              value = 500, min = 100, max = 2000, step = 50
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("stm_search", "Search K", class = "btn-primary btn-block")
            )
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 5",
            tags$h5(strong("Structural topic model"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("stm_K_number_uiOutput"),
            selectInput(
              "stm_topic_measure",
              "Topic term measure",
              choices = c(
                "FREX" = "frex",
                "Lift" = "lift",
                "Score" = "score",
                "Probability" = "beta"
              ),
              selected = "frex"
            ),
            conditionalPanel(
              condition = "input.stm_topic_measure == 'frex'",
              sliderInput(
                "stm_top_term_number_frex",
                "Top terms (FREX)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.stm_topic_measure == 'lift'",
              sliderInput(
                "stm_top_term_number_lift",
                "Top terms (Lift)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.stm_topic_measure == 'score'",
              sliderInput(
                "stm_top_term_number_score",
                "Top terms (Score)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.stm_topic_measure == 'beta'",
              sliderInput(
                "stm_top_term_number_beta",
                "Top terms (Probability)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            sliderInput(
              "stm_ncol_top_terms",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            selectizeInput(
              "stm_categorical_var_2",
              "Categorical covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectizeInput(
              "stm_continuous_var_2",
              "Continuous covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectInput("stm_init_type_K", "Initialization type",
              choices = c("Spectral", "LDA", "Random", "Custom"),
              selected = "Spectral"
            ),
            radioButtons("stm_gamma_prior_K", "Gamma prior",
              choices = c("Pooled", "L1"),
              selected = "Pooled"
            ),
            radioButtons("stm_kappa_prior_K", "Kappa prior",
              choices = c("L1", "Jeffreys"),
              selected = "L1"
            ),
            numericInput("stm_max_em_its_K", "Max EM iterations",
              value = 500, min = 100, max = 2000, step = 50
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("stm_run", "Display", class = "btn-primary btn-block")
            ),
            tags$hr(),
            tags$h5(strong("Generate topic labels using AI"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            sliderInput(
              "stm_top_term_number_labeling",
              "Top terms for labeling",
              value = 7,
              min = 3,
              max = 15
            ),
            # Provider selection
            radioButtons(
              "stm_label_provider",
              "AI Provider:",
              choices = c(
                "Local (Ollama - Free, Private)" = "ollama",
                "OpenAI (API Key Required)" = "openai",
                "Gemini (API Key Required)" = "gemini"
              ),
              selected = "ollama",
              inline = TRUE
            ),
            # Ollama model selection
            conditionalPanel(
              condition = "input.stm_label_provider == 'ollama'",
              selectInput(
                "stm_label_ollama_model",
                "Ollama Model:",
                choices = c("phi3:mini", "llama3", "mistral", "gemma2"),
                selected = "phi3:mini"
              ),
              tags$p(
                style = "font-size: 12px; color: #666;",
                "Requires Ollama. Get it from ",
                tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
              )
            ),
            # OpenAI settings
            conditionalPanel(
              condition = "input.stm_label_provider == 'openai'",
              selectInput(
                "stm_label_openai_model",
                "OpenAI Model:",
                choices = c("gpt-4o-mini", "gpt-4o", "gpt-4-turbo", "gpt-3.5-turbo"),
                selected = "gpt-4o-mini"
              ),
              passwordInput(
                "stm_label_openai_api_key",
                "API Key:",
                placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
              )
            ),
            # Gemini settings
            conditionalPanel(
              condition = "input.stm_label_provider == 'gemini'",
              selectInput(
                "stm_label_gemini_model",
                "Gemini Model:",
                choices = c("gemini-2.0-flash", "gemini-1.5-flash", "gemini-1.5-pro"),
                selected = "gemini-2.0-flash"
              ),
              passwordInput(
                "stm_label_gemini_api_key",
                "API Key:",
                placeholder = "Enter key (or set GEMINI_API_KEY in .Renviron)"
              )
            ),
            textAreaInput(
              "stm_system_prompt",
              "System prompt",
              value = "
You are a highly skilled data scientist specializing in generating concise and descriptive topic labels based on provided top terms for each topic.
Each topic consists of a list of terms ordered from most to least significant (by beta scores).

Your objective is to create precise labels that capture the essence of each topic by following these guidelines:

1. Use Person-First Language
   - Prioritize respectful and inclusive language.
   - Avoid terms that may be considered offensive or stigmatizing.
   - For example, use 'students with learning disabilities' instead of 'disabled students'.
   - Use 'students with visual impairments' instead of 'impaired students'
   - Use 'students with blindness' instead of 'blind students'.

1. Analyze Top Terms' Significance
   - Primary Focus: Emphasize high beta-score terms as they strongly define the topic.
   - Secondary Consideration: Include lower-scoring terms if they add essential context.

2. Synthesize the Topic Label
   - Clarity: Make sure the label is clear and easily understandable.
   - Conciseness: Aim for a short phrase of about 5-7 words.
   - Relevance: Reflect the collective meaning of the most influential terms.
   - Intelligent interpretation: Use your understanding to create meaningful labels that capture the topic's essence.

3. Maintain Consistency
   - Capitalize the first word of all topic labels.
   - Keep formatting and terminology uniform across all labels.
   - Avoid ambiguity or generic wording that does not fit the provided top terms.

4. Adhere to Style Guidelines
   - Capitalization: Use title case for labels.
   - Avoid Jargon: Maintain accessibility; only use technical terms if absolutely necessary.
   - Uniqueness: Ensure each label is distinct and does not overlap significantly with others.

5. Handle Edge Cases
   - Conflicting Top Terms: If the terms suggest different directions, prioritize those with higher beta scores.
   - Low-Scoring Terms: Include them only if they add meaningful context.

6. Iterative Improvement
   - If the generated label is insufficiently representative, re-check term significance and revise accordingly.
   - Always adhere to these guidelines.

Example
----------
Top Terms (highest to lowest beta score):
virtual manipulatives (.035)
manipulatives (.022)
mathematical (.014)
app (.013)
solving (.013)
learning disability (.012)
algebra (.012)
area (.011)
tool (.010)
concrete manipulatives (.010)

Generated Topic Label:
Mathematical learning tools for students with disabilities

Focus on incorporating the most significant keywords while following the guidelines above to produce a concise, descriptive topic label.
",
              rows = 10
            ),
            textAreaInput(
              "stm_user_prompt",
              "User prompt:",
              value = "You have a topic with keywords listed from most to least significant: [terms will be inserted here]. Please create a concise and descriptive label (5-7 words) that: 1. Reflects the collective meaning of these keywords. 2. Gives higher priority to the most significant terms. 3. Adheres to the style guidelines provided in the system message.",
              rows = 5
            ),
            sliderInput(
              "stm_temperature",
              "Temperature (creativity level)",
              min = 0, max = 1, value = 0.5, step = 0.1
            ),
            actionButton("topic_generate_labels", HTML('<i class="fas fa-wand-magic-sparkles"></i> Generate Labels'), class = "btn-primary btn-block"),
            tags$hr(),
            textInput(
              "stm_label_topics",
              "Manually label topics",
              value = "",
              placeholder = "Type labels, use comma to separate"
            )
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 6",
            uiOutput("topic_number_uiOutput"),
            sliderInput(
              "stm_top_term_number_2",
              "Top terms per topic",
              value = 5,
              min = 1,
              max = 20
            ),
            actionButton("stm_display", "Display", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 7",
            tags$h5(strong("Explore example documents"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("quote_topic_number_uiOutput"),
            selectInput("stm_topic_texts", "Example quotes to display", choices = NULL),
            actionButton("stm_quote", "Quote", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 8",
            tags$h5(
              strong("Estimate Covariate Effects on Topics"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            tags$div(
              class = "status-step-purple",
              style = "margin-bottom: 15px;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              "Choose covariates in the Word-Topic tab"
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("stm_effect", "Estimate", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                downloadButton("stm_effect_download_table", class = "btn-secondary btn-block")
              )
            )
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 4 && input.searchKSubtabs == 'ai_rec'",
            tags$h5(strong("AI Configuration for K Selection"), style = "color: #0c1f4a; margin-bottom: 10px;"),

            # AI Provider selection
            radioButtons(
              "k_rec_provider",
              "AI Provider:",
              choices = c(
                "Local (Ollama - Free, Private)" = "ollama",
                "OpenAI (API Key Required)" = "openai",
                "Gemini (API Key Required)" = "gemini"
              ),
              selected = "ollama",
              inline = TRUE
            ),

            # Ollama-specific options
            conditionalPanel(
              condition = "input.k_rec_provider == 'ollama'",
              selectInput(
                "k_rec_ollama_model",
                "Ollama Model:",
                choices = c("phi3:mini", "llama3", "mistral", "gemma2"),
                selected = "phi3:mini"
              ),
              tags$p(
                style = "font-size: 12px; color: #666;",
                "Requires Ollama. Get it from ",
                tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
              )
            ),

            # OpenAI-specific options
            conditionalPanel(
              condition = "input.k_rec_provider == 'openai'",
              selectInput(
                "k_rec_openai_model",
                "OpenAI Model:",
                choices = c(
                  "gpt-4o-mini" = "gpt-4o-mini",
                  "gpt-4o" = "gpt-4o",
                  "gpt-4-turbo" = "gpt-4-turbo",
                  "gpt-3.5-turbo" = "gpt-3.5-turbo"
                ),
                selected = "gpt-4o-mini"
              ),
              passwordInput(
                "k_rec_openai_api_key",
                "API Key:",
                placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
              )
            ),

            # Gemini-specific options
            conditionalPanel(
              condition = "input.k_rec_provider == 'gemini'",
              selectInput(
                "k_rec_gemini_model",
                "Gemini Model:",
                choices = c(
                  "gemini-2.0-flash" = "gemini-2.0-flash",
                  "gemini-1.5-flash" = "gemini-1.5-flash",
                  "gemini-1.5-pro" = "gemini-1.5-pro"
                ),
                selected = "gemini-2.0-flash"
              ),
              passwordInput(
                "k_rec_gemini_api_key",
                "API Key:",
                placeholder = "Set GEMINI_API_KEY in .Renviron"
              )
            ),

            textAreaInput(
              "ai_system_search",
              "System prompt",
              value = "You are an expert in Structural Topic Modeling (STM) and topic model selection. Analyze the provided metrics to recommend the optimal number of topics (K).\n\nEvaluation criteria in order of importance:\n1. **Held-out likelihood**: Higher values indicate better generalization (most important for model selection)\n2. **Semantic coherence**: Higher values mean topics are more interpretable\n3. **Exclusivity**: Higher values mean topics are more distinct\n4. **Residuals**: Lower values indicate better model fit\n5. **Overall score**: A balanced metric combining all factors\n\nBest practice: Look for K values where:\n- Held-out likelihood plateaus or peaks\n- Semantic coherence and exclusivity are both reasonably high (upper-right quadrant)\n- There's an \"elbow\" in the residuals plot\n- The rate of improvement diminishes (diminishing returns)\n\nProvide a specific K recommendation with brief justification based on these metrics.",
              rows = 8
            ),
            textAreaInput(
              "ai_user_search",
              "User prompt",
              value = "Based on the Search K diagnostic plots and metrics table, what is the optimal number of topics (K)?\n\nKey observations:\n- Models with high coherence but low exclusivity may be too general\n- Models with high exclusivity but low coherence may be too specific\n- The ideal model balances both metrics while maximizing held-out likelihood\n- Consider practical interpretability: too few topics may oversimplify, too many may fragment meaningful themes\n\nPlease recommend a specific K value and explain which metrics most influenced your decision.",
              rows = 6
            ),
            actionButton(
              "generate_k_recommendation",
              HTML('<i class="fas fa-wand-magic-sparkles"></i> Generate Recommendation'),
              class = "btn-primary btn-block"
            )
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 9",
            tags$h5(strong("Plot Topic Effects by Categorical Covariates"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$div(
              class = "status-step-purple",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              tags$strong("Step 1:"), " Select categorical covariates in Word-Topic tab"
            ),
            tags$div(
              class = "status-step-blue",
              tags$i(class = "fa fa-check-circle status-icon status-icon-info"),
              tags$strong("Step 2:"), " Estimate coefficients in Estimated Effects tab"
            ),
            selectizeInput(
              "stm_effect_cat_btn",
              "Categorical covariate",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "stm_ncol_cat",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("stm_display_cat", "Display", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'probability' && input.conditioned3 == 10",
            tags$h5(strong("Plot Topic Effects by Continuous Covariates"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$div(
              class = "status-step-purple",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              tags$strong("Step 1:"), " Select continuous covariates in Word-Topic tab"
            ),
            tags$div(
              class = "status-step-blue",
              tags$i(class = "fa fa-check-circle status-icon status-icon-info"),
              tags$strong("Step 2:"), " Estimate coefficients in Estimated Effects tab"
            ),
            selectizeInput(
              "stm_effect_con_btn",
              "Continuous covariate",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "stm_ncol_con",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("stm_display_con", "Display", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.conditioned3 == 'ai_content'",
            tags$h5(HTML("<strong>Generate content using AI</strong>"), style = "color: #0c1f4a; margin-bottom: 15px;"),
            tags$div(
              class = "status-main-info",
              tags$i(class = "fas fa-robot status-icon status-icon-info"),
              "Generate survey items, research questions, or other content from topic terms."
            ),
            selectInput(
              "content_type",
              "Content type",
              choices = c(
                "Survey Item" = "survey_item",
                "Research Question" = "research_question",
                "Theme Description" = "theme_description",
                "Policy Recommendation" = "policy_recommendation",
                "Interview Question" = "interview_question",
                "Custom" = "custom"
              ),
              selected = "survey_item"
            ),
            sliderInput(
              "content_top_terms",
              "Top terms for content",
              value = 7,
              min = 3,
              max = 15
            ),
            sliderInput(
              "content_max_tokens",
              "Max tokens",
              min = 50, max = 500, value = 150, step = 25
            ),
            # Provider selection
            radioButtons(
              "content_provider",
              "AI Provider:",
              choices = c(
                "Local (Ollama - Free, Private)" = "ollama",
                "OpenAI (API Key Required)" = "openai",
                "Gemini (API Key Required)" = "gemini"
              ),
              selected = "ollama",
              inline = TRUE
            ),
            # Ollama model selection
            conditionalPanel(
              condition = "input.content_provider == 'ollama'",
              selectInput(
                "content_ollama_model",
                "Ollama Model:",
                choices = c("phi3:mini", "llama3", "mistral", "gemma2"),
                selected = "phi3:mini"
              ),
              tags$p(
                style = "font-size: 12px; color: #666;",
                "Requires Ollama. Get it from ",
                tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
              )
            ),
            # OpenAI settings
            conditionalPanel(
              condition = "input.content_provider == 'openai'",
              selectInput(
                "content_openai_model",
                "OpenAI Model:",
                choices = c("gpt-4o-mini", "gpt-4o", "gpt-4-turbo", "gpt-3.5-turbo"),
                selected = "gpt-4o-mini"
              ),
              passwordInput(
                "content_openai_api_key",
                "API Key:",
                placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
              )
            ),
            # Gemini settings
            conditionalPanel(
              condition = "input.content_provider == 'gemini'",
              selectInput(
                "content_gemini_model",
                "Gemini Model:",
                choices = c("gemini-2.0-flash", "gemini-1.5-flash", "gemini-1.5-pro"),
                selected = "gemini-2.0-flash"
              ),
              passwordInput(
                "content_gemini_api_key",
                "API Key:",
                placeholder = "Enter key (or set GEMINI_API_KEY in .Renviron)"
              )
            ),
            textAreaInput(
              "content_system_prompt",
              "System prompt",
              value = "",
              rows = 8
            ),
            textAreaInput(
              "content_user_prompt",
              "User prompt (use {terms} placeholder)",
              value = "",
              rows = 5
            ),
            sliderInput(
              "content_temperature",
              "Temperature (creativity level)",
              min = 0, max = 1, value = 0.5, step = 0.1
            ),
            actionButton(
              "generate_topic_content",
              HTML('<i class="fas fa-pen-fancy"></i> Generate Content'),
              class = "btn-primary btn-block",
              style = "margin-top: 15px;"
            )
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'embedding' && input.conditioned3 == 4",
            tags$h5(HTML("<strong>Configure Embedding-based Topic Modeling</strong> <a href='https://maartengr.github.io/BERTopic/' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("embedding_topic_python_status"),
            selectInput(
              "embedding_model_name",
              "Embedding model:",
              choices = c(
                "all-MiniLM-L6-v2" = "all-MiniLM-L6-v2",
                "all-mpnet-base-v2" = "all-mpnet-base-v2"
              ),
              selected = "all-MiniLM-L6-v2"
            ),

            selectInput(
              "embedding_method",
              "Topic discovery method:",
              choices = c(
                "UMAP + HDBSCAN" = "umap_hdbscan",
                "Embedding Clustering" = "embedding_clustering",
                "Hierarchical Semantic" = "hierarchical_semantic"
              ),
              selected = "umap_hdbscan"
            ),

            conditionalPanel(
              condition = "input.embedding_method == 'umap_hdbscan'",
              sliderInput(
                "embedding_umap_neighbors",
                "Neighbors:",
                value = 15,
                min = 5,
                max = 50
              ),
              sliderInput(
                "embedding_umap_min_dist",
                "Min distance (0.0 = tight clusters for topic modeling):",
                value = 0.0,
                min = 0.0,
                max = 0.99,
                step = 0.01
              ),
              sliderInput(
                "embedding_min_topic_size",
                "Min cluster size:",
                value = 10,
                min = 2,
                max = 50
              ),
              selectInput(
                "embedding_cluster_selection",
                "Cluster selection method:",
                choices = c(
                  "EOM (default, broader topics)" = "eom",
                  "Leaf (finer-grained topics)" = "leaf"
                ),
                selected = "eom"
              ),
              checkboxInput(
                "embedding_reduce_outliers",
                "Reduce outliers",
                value = FALSE
              )
            ),

            conditionalPanel(
              condition = "input.embedding_method != 'umap_hdbscan'",
              numericInput(
                "embedding_n_topics",
                "Number of topics",
                value = 10,
                min = 2,
                max = 50
              )
            ),

            selectInput(
              "embedding_topic_representation",
              "Topic representation",
              choices = c(
                "c-TF-IDF (BERTopic style)" = "c-tfidf",
                "Standard TF-IDF" = "tfidf",
                "Maximal Marginal Relevance" = "mmr",
                "Term Frequency" = "frequency"
              ),
              selected = "c-tfidf"
            ),

            sliderInput(
              "embedding_topic_diversity",
              "Topic diversity",
              value = 0.5,
              min = 0,
              max = 1,
              step = 0.1
            ),

            actionButton("embedding_run", "Run Model", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'embedding' && input.conditioned3 == 5",
            uiOutput("embedding_topics_info"),
            actionButton("embedding_display", "Display", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'embedding' && input.conditioned3 == 6",
            selectInput(
              "embedding_viz_type",
              "Visualization type",
              choices = c(
                "Topic Distribution" = "distribution",
                "Document Clusters" = "documents",
                "Topic Similarity Heatmap" = "heatmap",
                "Intertopic Distance" = "distance"
              ),
              selected = "distribution"
            )
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'embedding' && input.conditioned3 == 7",
            sliderInput(
              "embedding_quote_number",
              "Quotes to display",
              value = 3,
              min = 1,
              max = 10
            ),
            selectInput(
              "embedding_quote_topic",
              "Topic",
              choices = NULL
            ),
            actionButton("embedding_quote", "Quote", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 4",
            tags$h5(HTML("<strong>Evaluate optimal topic number (K) for hybrid model</strong> <a href='https://github.com/bstewart/stm' target='_blank' style='font-size: 16px;'>Source</a>"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            sliderInput(
              "hybrid_K_range",
              "Range of topic numbers",
              value = c(5, 10),
              min = 2,
              max = 50
            ),
            selectizeInput(
              "hybrid_categorical_var",
              "Categorical covariate(s)",
              choices = NULL,
              multiple = TRUE
            ),
            selectizeInput(
              "hybrid_continuous_var",
              "Continuous covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectInput("hybrid_init_type", "Initialization type",
              choices = c("Spectral", "LDA", "Random", "Custom"),
              selected = "Spectral"
            ),
            radioButtons("hybrid_gamma_prior", "Gamma prior",
              choices = c("Pooled", "L1"),
              selected = "Pooled"
            ),
            radioButtons("hybrid_kappa_prior", "Kappa prior",
              choices = c("L1", "Jeffreys"),
              selected = "L1"
            ),
            numericInput("hybrid_max_em_its", "Max EM iterations",
              value = 500, min = 100, max = 2000, step = 50
            ),
            selectInput(
              "hybrid_embedding_model",
              "Embedding model",
              choices = c(
                "all-MiniLM-L6-v2" = "all-MiniLM-L6-v2",
                "all-mpnet-base-v2" = "all-mpnet-base-v2"
              ),
              selected = "all-MiniLM-L6-v2"
            ),
            sliderInput(
              "hybrid_n_embedding_dims",
              "Embedding dimensions for STM",
              value = 5,
              min = 3,
              max = 15
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("hybrid_search", "Search K", class = "btn-primary btn-block")
            )
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 5",
            tags$h5(strong("Estimate hybrid topic model"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("hybrid_K_number_uiOutput"),
            selectInput(
              "hybrid_topic_measure",
              "Topic term measure",
              choices = c(
                "FREX" = "frex",
                "Lift" = "lift",
                "Score" = "score",
                "Probability" = "beta"
              ),
              selected = "frex"
            ),
            conditionalPanel(
              condition = "input.hybrid_topic_measure == 'frex'",
              sliderInput(
                "hybrid_top_term_number_frex",
                "Top terms (FREX)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.hybrid_topic_measure == 'lift'",
              sliderInput(
                "hybrid_top_term_number_lift",
                "Top terms (Lift)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.hybrid_topic_measure == 'score'",
              sliderInput(
                "hybrid_top_term_number_score",
                "Top terms (Score)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            conditionalPanel(
              condition = "input.hybrid_topic_measure == 'beta'",
              sliderInput(
                "hybrid_top_term_number_beta",
                "Top terms (Probability)",
                value = 5,
                min = 0,
                max = 50
              )
            ),
            sliderInput(
              "hybrid_ncol_top_terms",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            selectizeInput(
              "hybrid_categorical_var_2",
              "Categorical covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectizeInput(
              "hybrid_continuous_var_2",
              "Continuous covariate(s)",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Optional")
            ),
            selectInput("hybrid_init_type_K", "Initialization type",
              choices = c("Spectral", "LDA", "Random", "Custom"),
              selected = "Spectral"
            ),
            radioButtons("hybrid_gamma_prior_K", "Gamma prior",
              choices = c("Pooled", "L1"),
              selected = "Pooled"
            ),
            radioButtons("hybrid_kappa_prior_K", "Kappa prior",
              choices = c("L1", "Jeffreys"),
              selected = "L1"
            ),
            numericInput("hybrid_max_em_its_K", "Max EM iterations",
              value = 500, min = 100, max = 2000, step = 50
            ),
            div(
              style = "margin-bottom: 15px;",
              actionButton("hybrid_run", "Display", class = "btn-primary btn-block")
            ),
            tags$hr(),
            tags$h5(strong("Generate topic labels using AI"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            sliderInput(
              "hybrid_top_term_number_labeling",
              "Top terms for labeling",
              value = 7,
              min = 3,
              max = 15
            ),
            # Provider selection
            radioButtons(
              "hybrid_label_provider",
              "AI Provider:",
              choices = c(
                "Local (Ollama - Free, Private)" = "ollama",
                "OpenAI (API Key Required)" = "openai",
                "Gemini (API Key Required)" = "gemini"
              ),
              selected = "ollama",
              inline = TRUE
            ),
            # Ollama model selection
            conditionalPanel(
              condition = "input.hybrid_label_provider == 'ollama'",
              selectInput(
                "hybrid_label_ollama_model",
                "Ollama Model:",
                choices = c("phi3:mini", "llama3", "mistral", "gemma2"),
                selected = "phi3:mini"
              ),
              tags$p(
                style = "font-size: 12px; color: #666;",
                "Requires Ollama. Get it from ",
                tags$a(href = "https://ollama.com", target = "_blank", "ollama.com")
              )
            ),
            # OpenAI settings
            conditionalPanel(
              condition = "input.hybrid_label_provider == 'openai'",
              selectInput(
                "hybrid_label_openai_model",
                "OpenAI Model:",
                choices = c("gpt-4o-mini", "gpt-4o", "gpt-4-turbo", "gpt-3.5-turbo"),
                selected = "gpt-4o-mini"
              ),
              passwordInput(
                "hybrid_label_openai_api_key",
                "API Key:",
                placeholder = "sk-... (or set OPENAI_API_KEY in .Renviron)"
              )
            ),
            # Gemini settings
            conditionalPanel(
              condition = "input.hybrid_label_provider == 'gemini'",
              selectInput(
                "hybrid_label_gemini_model",
                "Gemini Model:",
                choices = c("gemini-2.0-flash", "gemini-1.5-flash", "gemini-1.5-pro"),
                selected = "gemini-2.0-flash"
              ),
              passwordInput(
                "hybrid_label_gemini_api_key",
                "API Key:",
                placeholder = "Enter key (or set GEMINI_API_KEY in .Renviron)"
              )
            ),
            textAreaInput(
              "hybrid_system_prompt",
              "System prompt",
              value = "You are a highly skilled data scientist specializing in generating concise and descriptive topic labels for hybrid topic models that combine STM probabilistic topics with semantic embeddings. Each topic consists of terms ordered from most to least significant. Create precise labels (5-7 words) that capture the essence of each topic using person-first language and avoiding stigmatizing terms.",
              rows = 5
            ),
            textAreaInput(
              "hybrid_user_prompt",
              "User prompt",
              value = "You have a hybrid topic model topic with keywords listed from most to least significant: [terms will be inserted here]. Please create a concise and descriptive label (5-7 words) that reflects the collective meaning of these keywords, giving higher priority to the most significant terms.",
              rows = 3
            ),
            sliderInput(
              "hybrid_temperature",
              "Temperature (creativity level)",
              min = 0, max = 1, value = 0.5, step = 0.1
            ),
            actionButton("hybrid_generate_labels", HTML('<i class="fas fa-wand-magic-sparkles"></i> Generate Labels'), class = "btn-primary btn-block"),
            tags$hr(),
            textInput(
              "hybrid_label_topics",
              "Manually label topics",
              value = "",
              placeholder = "Type labels, use comma to separate"
            )
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 6",
            uiOutput("hybrid_topic_number_uiOutput"),
            sliderInput(
              "hybrid_top_term_number_2",
              "Top terms per topic",
              value = 5,
              min = 1,
              max = 20
            ),
            actionButton("hybrid_display", "Display", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 8",
            tags$h5(
              strong("Estimate Covariate Effects on Topics"),
              style = "color: #0c1f4a; margin-bottom: 10px;"
            ),
            tags$div(
              class = "status-step-purple",
              style = "margin-bottom: 15px;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              "Choose covariates in the Word-Topic tab"
            ),
            div(
              style = "display: flex; gap: 10px; margin-bottom: 15px;",
              div(
                style = "flex: 1;",
                actionButton("hybrid_effect", "Estimate", class = "btn-primary btn-block")
              ),
              div(
                style = "flex: 1;",
                downloadButton("hybrid_effect_download_table", class = "btn-secondary btn-block")
              )
            )
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 7",
            tags$h5(strong("Explore example documents for each topic."), style = "color: #0c1f4a; margin-bottom: 10px;"),
            uiOutput("hybrid_quote_topic_number_uiOutput"),
            selectInput("hybrid_topic_texts", "Example quotes to display", choices = NULL),
            actionButton("hybrid_quote", "Quote", class = "btn-primary btn-block")
          ),

          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 9",
            tags$h5(strong("Plot Topic Effects by Categorical Covariates"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$div(
              class = "status-step-purple",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              tags$strong("Step 1:"), " Select categorical covariates in Word-Topic tab"
            ),
            tags$div(
              class = "status-step-blue",
              style = "margin-bottom: 10px;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-info"),
              tags$strong("Step 2:"), " Estimate coefficients in Estimated Effects tab"
            ),
            tags$div(
              class = "status-step-green",
              tags$i(class = "fa fa-info-circle status-icon status-icon-green-alt"),
              "Hybrid model includes both metadata and embedding dimension covariates"
            ),
            selectizeInput(
              "hybrid_effect_cat_btn",
              "Categorical covariate",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "hybrid_ncol_cat",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("hybrid_display_cat", "Display", class = "btn-primary btn-block")
          ),
          conditionalPanel(
            condition = "input.topic_modeling_path == 'hybrid' && input.conditioned3 == 10",
            tags$h5(strong("Plot Topic Effects by Continuous Covariates"), style = "color: #0c1f4a; margin-bottom: 10px;"),
            tags$div(
              class = "status-step-purple",
              tags$i(class = "fa fa-check-circle status-icon status-icon-purple"),
              tags$strong("Step 1:"), " Select continuous covariates in Word-Topic tab"
            ),
            tags$div(
              class = "status-step-blue",
              style = "margin-bottom: 10px;",
              tags$i(class = "fa fa-check-circle status-icon status-icon-info"),
              tags$strong("Step 2:"), " Estimate coefficients in Estimated Effects tab"
            ),
            tags$div(
              class = "status-step-green",
              tags$i(class = "fa fa-info-circle status-icon status-icon-green-alt"),
              "Hybrid model includes both metadata and embedding dimension covariates"
            ),
            selectizeInput(
              "hybrid_effect_con_btn",
              "Continuous covariate",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "hybrid_ncol_con",
              "Column numbers",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("hybrid_display_con", "Display", class = "btn-primary btn-block")
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            id = "conditioned3",
            tabPanel(
              "1. Model Configuration",
              value = 4,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 4,
                  style = "success",
                  p(strong("Dimensions of the plots")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_search_k",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 600
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_search_k",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_search_k_results == true",
                tabsetPanel(
                  id = "searchKSubtabs",
                  tabPanel(
                    "Diagnostic Plots",
                    value = "diagnostic",
                    br(),
                    uiOutput("topic_search_message"),
                    br(),
                    uiOutput("quality_metrics_plot_uiOutput")
                  ),
                  tabPanel(
                    "Quality Comparison",
                    value = "quality",
                    br(),
                    wellPanel(
                      style = "background-color: #f0f8ff; border: 1px solid #4682b4;",
                      p("Overall Score = Coherence(z) + Exclusivity(z) - Residual(z) + Heldout(z)",
                        style = "font-family: monospace; font-size: 16px; font-weight: bold;"),
                      tags$div(
                        style = "font-size: 16px; margin-top: 10px; line-height: 1.6;",
                        p(tags$b("Coherence:"), " How interpretable topics are based on co-occurring words", style = "margin: 5px 0;"),
                        p(tags$b("Exclusivity:"), " How distinctive topics are from each other", style = "margin: 5px 0;"),
                        p(tags$b("Residual:"), " Model fit to data (lower is better)", style = "margin: 5px 0;"),
                        p(tags$b("Heldout:"), " Model's ability to generalize to new data", style = "margin: 5px 0;")
                      )
                    ),
                    br(),
                    DT::dataTableOutput("quality_summary_table")
                  ),
                  tabPanel(
                    "Coherence vs Exclusivity",
                    value = "comparison",
                    br(),
                    uiOutput("model_comparison_plot_uiOutput")
                  ),
                  tabPanel(
                    "AI Recommendation",
                    value = "ai_rec",
                    br(),
                    uiOutput("ai_recommendation_output")
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_search_k_results == false && input.topic_modeling_path == 'probability'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-search-plus", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Configure K range and click ",
                      tags$strong("'Search K'", style = "color: #0c1f4a;"),
                      " to find optimal topic numbers",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.topic_modeling_path == 'embedding'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-cogs", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Configure settings and click ",
                      tags$strong("'Run Model'", style = "color: #0c1f4a;"),
                      " to discover topics",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "input.topic_modeling_path == 'hybrid' && output.has_hybrid_search_k_results == true",
                tabsetPanel(
                  id = "hybridSearchKSubtabs",
                  tabPanel(
                    "Diagnostic Plots",
                    value = "diagnostic",
                    br(),
                    uiOutput("hybrid_topic_search_message"),
                    br(),
                    uiOutput("hybrid_quality_metrics_plot_uiOutput")
                  ),
                  tabPanel(
                    "Quality Comparison",
                    value = "quality",
                    br(),
                    wellPanel(
                      style = "background-color: #f0f8ff; border: 1px solid #4682b4;",
                      p("Hybrid model combines STM probabilistic topics with semantic embeddings",
                        style = "font-family: monospace; font-size: 16px; font-weight: bold;"),
                      tags$div(
                        style = "font-size: 16px; margin-top: 10px; line-height: 1.6;",
                        p(tags$b("STM Metrics:"), " Based on statistical topic modeling", style = "margin: 5px 0;"),
                        p(tags$b("Coherence:"), " How interpretable topics are", style = "margin: 5px 0;"),
                        p(tags$b("Exclusivity:"), " How distinctive topics are", style = "margin: 5px 0;"),
                        p(tags$b("Heldout Likelihood:"), " Generalization performance", style = "margin: 5px 0;")
                      )
                    ),
                    br(),
                    DT::dataTableOutput("hybrid_quality_summary_table")
                  ),
                  tabPanel(
                    "Coherence vs Exclusivity",
                    value = "comparison",
                    br(),
                    uiOutput("hybrid_model_comparison_plot_uiOutput")
                  )
                )
              ),
              conditionalPanel(
                condition = "input.topic_modeling_path == 'hybrid' && output.has_hybrid_search_k_results == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-search-plus", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Configure K range and click ",
                      tags$strong("'Search K'", style = "color: #0c1f4a;"),
                      " to find optimal topic numbers",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "2. Word-Topic",
              value = 5,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 1,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 1000
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_word_topic_results == true",
                uiOutput("topic_term_message"),
                uiOutput("topic_term_plot_uiOutput"),
                br(),
                uiOutput("topic_term_table_uiOutput"),
                br()
              ),
              conditionalPanel(
                condition = "output.has_word_topic_results == false && input.topic_modeling_path == 'probability'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Search K, and then click ",
                      tags$strong("'Display'", style = "color: #0c1f4a;"),
                      " to view word-topic distributions",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_word_topic_results == false && input.topic_modeling_path == 'embedding'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Run model, then click ",
                      tags$strong("'Display'", style = "color: #0c1f4a;"),
                      " to view topic keywords",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_word_topic_results == false && input.topic_modeling_path == 'hybrid'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-project-diagram", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Search K, and then click ",
                      tags$strong("'Display'", style = "color: #0c1f4a;"),
                      " to view word-topic distributions",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "3. Content Generation",
              value = "ai_content",
              div(
                style = "padding: 20px;",
                
                conditionalPanel(
                  condition = "output.has_generated_content == true",
                  DT::dataTableOutput("generated_content_table")
                ),
                conditionalPanel(
                  condition = "output.has_generated_content == false",
                  div(
                    style = "padding: 60px 40px; text-align: center;",
                    tags$i(class = "fas fa-file-alt", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Run topic modeling first, then configure settings in the sidebar and click ",
                      tags$strong("'Generate Content'", style = "color: #0c1f4a;"),
                      " to create content from your topics.",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),

            tabPanel(
              "4. Document-Topic",
              value = 6,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 1,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_topic_prevalence",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 500
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_topic_prevalence",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_document_topic_results == true",
                uiOutput("topic_prevalence_plot_uiOutput"),
                br(),
                uiOutput("topic_prevalence_table_uiOutput")
              ),
              conditionalPanel(
                condition = "output.has_document_topic_results == false && input.topic_modeling_path == 'probability'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-file-alt", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Complete ",
                      tags$strong("Word-Topic tab", style = "color: #0c1f4a;"),
                      " to view document-topic distributions",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_document_topic_results == false && input.topic_modeling_path == 'embedding'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-file-alt", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Complete ",
                      tags$strong("Word-Topic tab", style = "color: #0c1f4a;"),
                      " to view document-topic distributions",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              ),
              conditionalPanel(
                condition = "output.has_document_topic_results == false && input.topic_modeling_path == 'hybrid'",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-file-alt", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Complete ",
                      tags$strong("Word-Topic tab", style = "color: #0c1f4a;"),
                      " to view document-topic distributions enhanced with semantic embeddings",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "5. Quotes",
              value = 7,
              conditionalPanel(
                condition = "output.has_quotes == true",
                DT::dataTableOutput("quote_table")
              ),
              conditionalPanel(
                condition = "output.has_quotes == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-quote-right", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Run topic model and select a topic to view ",
                      tags$strong("representative quotes", style = "color: #0c1f4a;"),
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "5. Estimated Effects",
              value = 8,
              conditionalPanel(
                condition = "output.has_effect_estimates == true",
                DT::dataTableOutput("effect_table")
              ),
              conditionalPanel(
                condition = "output.has_effect_estimates == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-calculator", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
                    tags$p(
                      "Click ",
                      tags$strong("'Estimate'", style = "color: #0c1f4a;"),
                      " button to generate effect estimates",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "6. Categorical Covariates",
              value = 9,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 1,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_cat_plot",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 500
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_cat_plot",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_categorical_plot == true",
                uiOutput("cat_plot_uiOutput"),
                br(),
                uiOutput("cat_table_uiOutput")
              ),
              conditionalPanel(
                condition = "output.has_categorical_plot == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-chart-bar", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Estimate effects, select categorical covariate, then click ",
                      tags$strong("'Display'", style = "color: #0c1f4a;"),
                      " to visualize topic prevalence by categories",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            ),
            tabPanel(
              "7. Continuous Covariates",
              value = 10,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    class = "plot-dimensions-text"
                  ),
                  value = 1,
                  style = "success",
                  p(strong("Dimensions of the plot")),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "height_con_plot",
                      post = " px",
                      label = "height",
                      min = 200,
                      max = 4000,
                      step = 5,
                      value = 500
                    )
                  ),
                  div(
                    style = "display:inline-block",
                    sliderInput(
                      inputId = "width_con_plot",
                      post = " px",
                      label = "width",
                      min = 500,
                      max = 3000,
                      step = 5,
                      value = 1000
                    )
                  )
                )
              ),
              tags$style(
                HTML(
                  ".plot-container {
                                max-height: 4000px;
                                max-width: 3000px;
                                overflow: auto; }"
                )
              ),
              conditionalPanel(
                condition = "output.has_continuous_plot == true",
                uiOutput("con_plot_uiOutput"),
                br(),
                uiOutput("con_table_uiOutput")
              ),
              conditionalPanel(
                condition = "output.has_continuous_plot == false",
                div(
                  style = "padding: 60px 40px; text-align: center;",
                  div(
                    style = "max-width: 400px; margin: 0 auto;",
                    tags$i(class = "fa fa-chart-line", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;", "aria-hidden" = "true"),
                    tags$p(
                      "Estimate effects, select continuous covariate, then click ",
                      tags$strong("'Display'", style = "color: #0c1f4a;"),
                      " to visualize topic prevalence trends",
                      style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  )
)
