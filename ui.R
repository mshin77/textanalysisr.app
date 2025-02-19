suppressPackageStartupMessages({
  library(quanteda)
  library(shiny)
  library(shinyBS)
  library(shinyjs)
  library(markdown)
})

ui <- fluidPage(
  useShinyjs(),

  tags$head(tags$style(
    HTML(
      ".nav.nav-pills.nav-stacked > .active > a,
         .nav.nav-pills.nav-stacked > .active > a:hover {background-color: #0c1f4a;}"
    )
  ),
  absolutePanel(
    top = 10,
    right = 10,
    HTML(
      '<div id="google_translate_element"></div>
      <script type="text/javascript">
        function googleTranslateElementInit() {
          new google.translate.TranslateElement({
            pageLanguage: "en",
            includedLanguages: "af,sq,am,ar,hy,az,eu,be,bn,bs,bg,ca,ceb,zh-CN,zh-TW,co,hr,cs,da,nl,en,eo,et,fil,fi,fr,fy,gl,ka,de,el,gu,ht,ha,haw,iw,hi,hmn,hu,is,ig,id,ga,it,ja,jw,kn,kk,km,rw,ko,ku,ky,lo,la,lv,lt,lb,mk,mg,ms,ml,mt,mi,mr,mn,my,ne,no,ps,fa,pl,pt,pa,ro,ru,sm,gd,sr,st,sn,sd,si,sk,sl,so,es,su,sw,sv,tg,ta,tt,te,th,tr,tk,uk,ur,ug,uz,vi,cy,xh,yi,yo,zu"
          }, "google_translate_element");
        }
      </script>
      <script type="text/javascript" src="https://translate.google.com/translate_a/element.js?cb=googleTranslateElementInit"></script>'
    )
  )),
  includeCSS("css/styles.css"),
  titlePanel("TextAnalysisR"),
  navbarPage(
    "",
    tabPanel("About",
             includeMarkdown("markdown/README.md")
    ),
    tabPanel("Upload",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 tags$h5(strong("Upload a file or use the dataset.")),
                 selectizeInput("dataset_choice", "Choose Dataset:",
                                choices = c("Select a dataset" = "",
                                            "Upload an Example Dataset",
                                            "Upload Your File",
                                            "Copy and Paste Text"),
                                selected = "",
                                options = list(placeholder = 'Select a dataset')
                 ),
                 fileInput("file", "Upload Your File",
                           multiple = TRUE,
                           accept = c(".xlsx", ".xls", ".xlsm", ".csv", ".pdf", ".docx", ".txt")),
                 textAreaInput("text_input", "Copy and Paste Text", "",
                               rows = 10, placeholder = "Paste your text here...")
               ),
               mainPanel(width = 9,
                         DT::dataTableOutput("data_table")
               )
             )
    ),
    tabPanel("Preprocess",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # Step 1
                 conditionalPanel(
                   condition = "input.conditioned == 1",
                   checkboxGroupInput("show_vars",
                                      label = HTML("<h5><strong>Select one or multiple columns to unite.</strong></h5>"),
                                      choices = NULL),
                   actionButton("apply", "Apply", icon = icon("check")),
                   downloadButton("download_table", "Download")
                 ),
                 # Step 2
                 conditionalPanel(
                   condition = "input.conditioned == 2",
                   conditionalPanel(
                     condition = "input.conditioned == 2",
                     tags$h5(
                       strong("Segment a corpus into tokens."),
                       tags$a(
                         href = "https://quanteda.io",
                         target = "_blank",
                         "Source")),
                     checkboxInput("remove_punct", "Remove punctuation", value = TRUE),
                     checkboxInput("remove_symbols", "Remove symbols", value = TRUE),
                     checkboxInput("remove_numbers", "Remove numbers", value = TRUE),
                     checkboxInput("remove_url", "Remove URLs", value = TRUE),
                     checkboxInput("remove_separators", "Remove separators", value = TRUE),
                     checkboxInput("split_hyphens", "Split hyphens", value = TRUE),
                     checkboxInput("split_tags", "Split tags", value = TRUE),
                     checkboxInput("include_docvars", "Include document variables", value = TRUE),
                     checkboxInput("keep_acronyms", "Keep acronyms", value = FALSE),
                     checkboxInput("padding", "Keep padding", value = FALSE),
                     actionButton("preprocess", "Apply", icon = icon("check"))
                   ),
                 ),
                 # Step 3
                 conditionalPanel(
                   condition = "input.conditioned == 3",
                   selectizeInput(
                     "multi_word_expressions",
                     label = HTML("<h5><strong>Construct a multi-word dictionary.</strong></h5>"),
                     choices = NULL,
                     multiple = TRUE,
                     options = list(create = TRUE)
                   ),
                   actionButton("dictionary", "Apply", icon = icon("check"))
                 ),
                 # Step 4
                 conditionalPanel(
                   condition = "input.conditioned == 4",
                   tags$h5(
                     strong("Construct a document-feature matrix.")),
                   actionButton("dfm_btn", "Process", icon = icon("file-alt"))
                 ),
                 # Step 5
                 conditionalPanel(
                   condition = "input.conditioned == 5",
                   selectizeInput(
                     "common_words",
                     label = HTML("<h5><strong>Remove common words in your dataset.</strong></h5>"),
                     choices = NULL,
                     multiple = TRUE,
                     options = list(create = TRUE)
                   ),
                   checkboxInput("leading_stopwords", "Remove leading stopwords", TRUE),
                   checkboxInput("trailing_stopwords", "Remove trailing stopwords", TRUE),
                   div(style = "max-height: 200px; overflow-y: auto;",
                       selectizeInput(
                         "custom_stopwords",
                         label = HTML("<h5><strong>Remove predefined stopwords.</strong><a href='https://search.r-project.org/CRAN/refmans/stopwords/html/stopwords.html'> Source</a></h5>"),
                         choices = stopwords::stopwords("en", source = "snowball"),
                         selected = stopwords::stopwords("en", source = "snowball"),
                         multiple = TRUE,
                         options = list(create = TRUE)
                       )
                   ),
                   br(),
                   actionButton("remove", "Apply", icon = icon("check"))
                 ),
                 # Step 6
                 conditionalPanel(
                   condition = "input.conditioned == 6",
                   tags$h5(
                     strong("Lemmatize tokens.")
                   ),
                   actionButton("lemma", "Apply", icon = icon("check")),
                   actionButton("skip", "Skip", icon = icon("step-forward"))
                 )
               ),
               mainPanel(
                 width = 9,
                 tabsetPanel(
                   id = "conditioned",
                   tabPanel(
                     "1. Unite Texts",
                     value = 1,
                     DT::dataTableOutput("united_table")
                   ),
                   tabPanel(
                     "2. Segment Texts",
                     value = 2,
                     shinycssloaders::withSpinner(shiny::verbatimTextOutput("dict_print_preprocess"))
                   ),
                   tabPanel(
                     "3. Multi-Word Dictionary",
                     value = 3,
                     shinycssloaders::withSpinner(shiny::verbatimTextOutput("dict_print_dictionary"))
                   ),
                   tabPanel(
                     "4. Document-Feature Matrix",
                     value = 4,
                     shinycssloaders::withSpinner(plotly::plotlyOutput("dfm_plot", height = 500, width = 1000)),
                     br(),
                     DT::dataTableOutput("dfm_table")
                   ),
                   tabPanel(
                     "5. Remove Stopwords",
                     value = 5,
                     shinycssloaders::withSpinner(plotly::plotlyOutput("stopword_plot", height = 500, width = 1000)),
                     br(),
                     DT::dataTableOutput("stopword_table")
                   ),
                   tabPanel(
                     "6. Lemmatize",
                     value = 6,
                     shinycssloaders::withSpinner(plotly::plotlyOutput("lemma_plot", height = 500, width = 1000)),
                     br(),
                     DT::dataTableOutput("lemma_table")
                   )
                 )
               )
             )),
    tabPanel("Word Analysis",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 # Step 1
                 conditionalPanel(
                   condition = "input.conditioned2 == 1",
                   tags$h5(strong("Visualize word co-occurrence networks.")),
                   selectizeInput(
                     "doc_var_co_occurrence",
                     "Select a categorical variable (optional).",
                     choices = NULL,
                     multiple = FALSE),
                   sliderInput(
                     "co_occurence_number",
                     "Minimum co-occurrence numbers",
                     value = 5,
                     min = 0,
                     max = 100
                   ),
                   sliderInput(
                     "top_node_n_co_occurrence",
                     "Select top N nodes",
                     value = 30,
                     min = 0,
                     max = 100
                   ),
                   sliderInput(
                     "nrows_co_occurrence",
                     "Choose row numbers.",
                     value = 1,
                     min = 1,
                     max = 10
                   ),
                   actionButton("plot_word_co_occurrence_network", "Plot", icon = icon("search"))
                 ),
                 # Step 2
                 conditionalPanel(
                   condition = "input.conditioned2 == 2",
                   tags$h5(strong("Visualize word correlation networks.")),
                   selectizeInput(
                     "doc_var_correlation",
                     "Select a categorical variable (optional).",
                     choices = NULL,
                     multiple = FALSE),
                   sliderInput(
                     "common_term_n",
                     "Number of common terms",
                     value = 5,
                     min = 0,
                     max = 100
                   ),
                   sliderInput(
                     "corr_n",
                     "Minimum correlation betweewn pairwise words",
                     value = 0.3,
                     min = 0,
                     max = 1,
                     step = 0.1
                   ),
                   sliderInput(
                     "top_node_n_correlation",
                     "Select top N nodes",
                     value = 30,
                     min = 0,
                     max = 100
                   ),
                   sliderInput(
                     "nrows_correlation",
                     "Choose row numbers.",
                     value = 1,
                     min = 1,
                     max = 10
                   ),
                   actionButton("plot_word_correlation_network", "Plot", icon = icon("search"))
                 ),
                 # Step 3
                 conditionalPanel(
                   condition = "input.conditioned2 == 3",
                   tags$h5(strong("Display word frequency distributions over a continuous variable.")),
                   selectizeInput(
                     "continuous_var_3",
                     "Select a continuous variable.",
                     choices = NULL,
                     multiple = FALSE
                   ),
                   selectizeInput(
                     "type_terms",
                     "Select or type terms.",
                     choices = NULL,
                     options = list(maxItems = 20, multiple = TRUE)
                   ),
                   actionButton("plot_term", "Plot", icon = icon("play"))
                 )
               ),

               mainPanel(
                 width = 9,
                 tabsetPanel(
                   id = "conditioned2",
                   tabPanel(
                     "1. Word Co-occurrence Networks",
                     value = 1,
                     bsCollapse(
                       open = 0,
                       bsCollapsePanel(
                         p(strong("Click to set plot dimensions"),
                           style = "font-size: 15px;"),
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
                     tabsetPanel(
                       id = "word_co_occur_subTab",
                       tabPanel(
                         "Plot",
                         shinycssloaders::withSpinner(uiOutput("word_co_occurrence_network_plot_uiOutput"))
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
                   tabPanel(
                     "2. Word Correlation Networks",
                     value = 2,
                     bsCollapse(
                       open = 0,
                       bsCollapsePanel(
                         p(strong("Click to set plot dimensions"),
                           style = "font-size: 15px;"),
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
                     tabsetPanel(
                       id = "word_correlation_subTab",
                       tabPanel(
                         "Plot",
                         shinycssloaders::withSpinner(uiOutput("word_correlation_network_plot_uiOutput"))
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
                   tabPanel(
                     "3. Word Frequency Distribution",
                     value = 3,
                     bsCollapse(
                       open = 0,
                       bsCollapsePanel(
                         p(strong("Click to set plot dimensions"),
                           style = "font-size: 15px;"),
                         value = 3,
                         style = "success",
                         p(strong("Dimensions of the plot")),
                         div(
                           style = "display:inline-block",
                           sliderInput(
                             inputId = "height_line_con_var_plot",
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
                             inputId = "width_line_con_var_plot",
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
                     shinycssloaders::withSpinner(uiOutput("line_con_var_plot_uiOutput"))
                   )
                 )
               )
             )),
    tabPanel(
      "Structural Topic Model",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # Step 1
          conditionalPanel(
            condition = "input.conditioned3 == 4",
            tags$h5(
              strong("Evaluate optimal number of topics (K)."),
              tags$a(
                href = "https://www.structuraltopicmodel.com",
                target = "_blank",
                "Source")),
            sliderInput(
              "K_range",
              "Range of topic numbers",
              value = c(5, 20),
              min = 0,
              max = 50
            ),
            selectizeInput(
              "categorical_var",
              "Select categorical covariate(s).",
              choices = NULL,
              multiple = TRUE
            ),
            selectizeInput(
              "continuous_var",
              "Select continuous covariate(s).",
              choices = NULL,
              multiple = TRUE
            ),
            selectInput("init_type_search", "Initialization Type",
                        choices = c("Spectral", "LDA", "Random", "Custom"),
                        selected = "Spectral"),
            radioButtons("gamma_prior_search", "Gamma Prior",
                         choices = c("Pooled", "L1"),
                         selected = "L1"),
            radioButtons("kappa_prior_search", "Kappa Prior",
                         choices = c("L1", "Jeffreys"),
                         selected = "L1"),
            numericInput("max_em_its_search", "Max EM Iterations",
                         value = 500, min = 100, max = 2000, step = 50),
            actionButton("search", "Search K", icon = icon("search")),
            downloadButton("search_download_table", "Download")
          ),
          # Step 2
          conditionalPanel(
            condition = "input.conditioned3 == 5",
            tags$h5(
              strong("Estimate the structural topic model.")),
            uiOutput('K_number_uiOutput'),
            selectizeInput(
              "categorical_var_2",
              "Select categorical covariate(s).",
              choices = NULL,
              multiple = TRUE
            ),
            selectizeInput(
              "continuous_var_2",
              "Select continuous covariate(s).",
              choices = NULL,
              multiple = TRUE
            ),
            sliderInput(
              "top_term_number_1",
              "Display the highest per-topic-per-word probabilities.",
              value = 5,
              min = 0,
              max = 50
            ),
            sliderInput(
              "ncol_top_terms",
              "Choose column numbers.",
              value = 2,
              min = 1,
              max = 10
            ),
            selectInput("init_type_K", "Initialization Type",
                        choices = c("Spectral", "LDA", "Random", "Custom"),
                        selected = "Spectral"),
            radioButtons("gamma_prior_K", "Gamma Prior",
                         choices = c("Pooled", "L1"),
                         selected = "L1"),
            radioButtons("kappa_prior_K", "Kappa Prior",
                         choices = c("L1", "Jeffreys"),
                         selected = "L1"),
            numericInput("max_em_its_K", "Max EM Iterations",
                         value = 500, min = 100, max = 2000, step = 50),
            actionButton("run", "Display", icon = icon("play")),
            tags$hr(),
            tags$h5(strong("Generate topic labels using OpenAI's API.")),
            helpText(
              strong("Get your "),
              tags$a(href = "https://platform.openai.com/api-keys", "API key", target = "_blank"),
              strong(" and store it securely (.env file). You may also enter it in the web app.")
            ),
            selectInput(
              "model",
              "Select Model:",
              choices = c("gpt-3.5-turbo", "gpt-4-turbo", "gpt-4"),
              selected = "gpt-3.5-turbo"
            ),
            actionButton("generate_labels", "Generate", icon = icon("tags")),
            tags$hr(),

            textInput(
              "label_topics",
              "Manually label topics. Use a comma for the next topic:",
              value = "",
              placeholder = "Type labels here"
            ),
            downloadButton("topic_download_table", "Download")
          ),
          # Step 3
          conditionalPanel(
            condition = "input.conditioned3 == 6",
            tags$h5(
              strong("Display the highest per-document-per-topic probabilities.")
            ),
            uiOutput("topic_number_uiOutput"),
            sliderInput(
              "top_term_number_2",
              "Choose the top term numbers to display",
              value = 5,
              min = 0,
              max = 50
            ),
            actionButton("display", "Display", icon = icon("file-alt"))
          ),
          # Step 4
          conditionalPanel(
            condition = "input.conditioned3 == 7",
            tags$h5(strong("Explore example documents for each topic.")),
            uiOutput("quote_topic_number_uiOutput"),
            selectInput("topic_texts", "Example quotes to display", choices = NULL),
            actionButton("quote", "Quote", icon = icon("file-alt"))
          ),
          # Step 5
          conditionalPanel(
            condition = "input.conditioned3 == 8",
            tags$h5(
              strong(
                "Display relationships between document-level covariates and topical proportions."
              )),
            helpText(strong("Choose covariate(s) in the '2. Word-Topic' tab first")
            ),
            actionButton("effect", "Estimate", icon = icon("table")),
            downloadButton("effect_download_table", "Download")
          ),
          # Step 6
          conditionalPanel(
            condition = "input.conditioned3 == 9",
            tags$h5(
              strong(
                "Plot topic prevalence effects by categorical covariates."
              )),
            helpText(strong("Select categorical covariate(s) in the '2. Word-Topic' tab.")),
            helpText(strong("Then, estimate coefficients first in the '5. Estimated Effects' tab.")),
            selectizeInput(
              "effect_cat_btn",
              "Select a categorical covariate.",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "ncol_cat",
              "Choose column numbers.",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("display_cat", "Display", icon = icon("file-alt"))
          ),
          # Step 7
          conditionalPanel(
            condition = "input.conditioned3 == 10",
            tags$h5(
              strong(
                "Plot topic prevalence effects by continuous covariates."
              )),
            helpText(strong("Select continuous covariate(s) in the '2. Word-Topic' tab.")),
            helpText(strong("Then, estimate coefficients first in the '5. Estimated Effects' tab.")),
            selectizeInput(
              "effect_con_btn",
              "Select a continuous covariate.",
              choices = NULL,
              multiple = FALSE
            ),
            sliderInput(
              "ncol_con",
              "Choose column numbers.",
              value = 2,
              min = 1,
              max = 10
            ),
            actionButton("display_con", "Display", icon = icon("file-alt"))
          )
        ),

        mainPanel(
          width = 9,
          tabsetPanel(
            id = "conditioned3",
            tabPanel(
              "1. Search K ",
              value = 4,
              uiOutput("topic_search_message"),
              shinycssloaders::withSpinner(plotly::plotlyOutput("search_K_plot"))
            ),
            tabPanel(
              "2. Word-Topic",
              value = 5,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    style = "font-size: 15px;"),
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
              uiOutput("topic_term_message"),
              shinycssloaders::withSpinner(uiOutput("topic_term_plot_uiOutput")),
              br(),
              shinycssloaders::withSpinner(uiOutput("topic_term_table_uiOutput"))
            ),
            tabPanel(
              "3. Document-Topic",
              value = 6,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    style = "font-size: 15px;"),
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
              uiOutput("topic_prevalence_plot_uiOutput"),
              br(),
              uiOutput("topic_prevalence_table_uiOutput")
            ),
            tabPanel(
              "4. Quotes", value = 7,
              DT::dataTableOutput("quote_table")
            ),
            tabPanel(
              "5. Estimated Effects",
              value = 8,
              DT::dataTableOutput("effect_table")
            ),
            tabPanel(
              "6. Categorical Variables",
              value = 9,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    style = "font-size: 15px;"),
                  value = 2,
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
              shinycssloaders::withSpinner(uiOutput("cat_plot_uiOutput")),
              br(),
              uiOutput("cat_table_uiOutput")
            ),
            tabPanel(
              "7. Continuous Variables",
              value = 10,
              bsCollapse(
                open = 0,
                bsCollapsePanel(
                  p(strong("Click to set plot dimensions"),
                    style = "font-size: 15px;"),
                  value = 3,
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
              shinycssloaders::withSpinner(uiOutput("con_plot_uiOutput")),
              br(),
              uiOutput("con_table_uiOutput")
            )
          )

        )
      )
    )

  )
)
