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
                         selectInput(
                           "dataset_choice",
                           helpText(
                             strong("Upload a file or use the dataset."),
                             br(),
                             tags$ul(
                               tags$li("When uploading an Excel file, please upload one sheet."),
                               tags$li("Wait until your file is fully processed and displayed on the right.")
                             )
                           ),
                           selected = " ",
                           choices = c(" ", "Upload an Example Dataset", "Upload Your File")
                         ),
                         fileInput(
                             "file",
                             "Upload a file",
                             multiple = TRUE,
                             accept = c(".xlsx, .xls, .xlsm, .csv")
                         )
                     ),
                     mainPanel(width = 9,
                               DT::dataTableOutput("data_table"))
                 )),
        tabPanel("Preprocess",
                 sidebarLayout(
                     sidebarPanel(
                         width = 3,
                         # Step 1
                         conditionalPanel(
                             condition = "input.conditioned == 1",
                             checkboxGroupInput("show_vars", "Select one or multiple columns."),
                             actionButton("apply", "Apply", icon = icon("table")),
                             downloadButton("download_table", "Download")
                         ),
                         # Step 2
                         conditionalPanel(
                             condition = "input.conditioned == 2",
                             helpText(strong('Segment a corpus into tokens (words).')),
                             actionButton("preprocess", "Apply", icon = icon("table"))
                         ),
                         # Step 3
                         conditionalPanel(
                             condition = "input.conditioned == 3",
                             helpText(strong(
                                 'Apply researcher-developed dictionaries.'
                             )),
                             actionButton("dictionary", "Apply", icon = icon("table"))
                         ),
                         # Step 4
                         conditionalPanel(
                             condition = "input.conditioned == 4",
                             helpText(strong(
                                 'Use the default stopword list from',
                                 a("quanteda", href = "https://quanteda.io"),
                                 '.'
                             )),
                             actionButton("stopword", "Apply", icon = icon("table"))
                         ),
                         # Step 5
                         conditionalPanel(
                             condition = "input.conditioned == 5",
                             helpText(strong("Construct a document-feature matrix.")),
                             actionButton("dfm_btn", "Process", icon = icon("file-alt"))
                         ),
                         # Step 6
                         conditionalPanel(
                             condition = "input.conditioned == 6",
                             selectizeInput(
                                 "remove.var",
                                 "Remove common words.",
                                 choices = NULL,
                                 options = list(maxItems = 20)
                             ),
                             actionButton("remove", "Remove", icon = icon("minus-circle"))
                         )
                     ),

                     mainPanel(
                         width = 9,
                         tabsetPanel(
                             id = "conditioned",
                             tabPanel("1. Unite texts", value = 1,
                                      DT::dataTableOutput("step1_table")),
                             tabPanel(
                                 "2. Preprocess",
                                 value = 2,
                                 shiny::verbatimTextOutput("step2_print_preprocess")
                             ),
                             tabPanel(
                                 "3. Dictionary",
                                 value = 3,
                                 shiny::verbatimTextOutput("step2_print_dictionary")
                             ),
                             tabPanel(
                                 "4. Stopword",
                                 value = 4,
                                 shiny::verbatimTextOutput("step2_print_stopword")
                             ),
                             tabPanel(
                               "5. Document-feature matrix",
                               value = 5,
                               plotly::plotlyOutput("step3_plot", height = 500, width = 1000),
                               br(),
                               DT::dataTableOutput("step3_table")
                             ),
                             tabPanel(
                                 "6. Remove common words",
                                 value = 6,
                                 plotly::plotlyOutput("step4_plot", height = 500, width = 1000),
                                 br(),
                                 DT::dataTableOutput("step4_table")
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
                        condition = "input.conditioned2 == 1",
                        helpText(strong(
                            "Please don't close your screen while running the model."
                        )),
                        sliderInput(
                            "K_range_1",
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
                        actionButton("search", "Search K", icon = icon("search"))
                    ),
                    # Step 2
                    conditionalPanel(
                        condition = "input.conditioned2 == 2",
                        helpText(strong(
                            "Display highest word probabilities for each topic."
                        )),
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
                        tags$hr(),
                        sliderInput(
                            "top_term_number_1",
                            "Display the highest per-topic-per-word probabilities.",
                            value = 5,
                            min = 0,
                            max = 10
                        ),
                        actionButton("run", "Display", icon = icon("play")),
                        tags$hr(),
                        textInput(
                            "label_topics",
                            "Label topics. Use a comma for the next topic.",
                            value = "",
                            placeholder = "Type labels here"
                        )
                    ),
                    # Step 3
                    conditionalPanel(
                        condition = "input.conditioned2 == 3",
                        helpText(
                            strong("Display the highest per-document-per-topic probabilities.")
                        ),
                        uiOutput("topic_number_uiOutput"),
                        sliderInput(
                            "top_term_number_2",
                            "Choose the top term numbers to display",
                            value = 5,
                            min = 0,
                            max = 10
                        ),
                        actionButton("display", "Display", icon = icon("file-alt"))
                    ),
                    # Step 4
                    conditionalPanel(
                        condition = "input.conditioned2 == 4",
                        helpText(strong("Explore example documents for each topic.")),
                        uiOutput("quote_topic_number_uiOutput"),
                        selectInput("topic_texts", "Example quotes to display", choices = NULL),
                        actionButton("quote", "Quote", icon = icon("file-alt"))
                    ),
                    # Step 5
                    conditionalPanel(
                        condition = "input.conditioned2 == 5",
                        helpText(
                            strong(
                                "Display relationships between document-level covariates and topical proportions."
                            ),
                            tags$hr(),
                            strong("Choose covariate(s) in the '2. Word-Topic' tab first")
                        ),
                        actionButton("effect", "Estimate", icon = icon("table")),
                        downloadButton("effect_download_table", "Download")
                    ),
                    # Step 6
                    conditionalPanel(
                      condition = "input.conditioned2 == 6",
                      helpText(
                        strong(
                          "Plot topic prevalence effects by categorical covariates."
                        ),
                        br(),
                        tags$ul(
                          tags$li("Select categorical covariate(s) in the '2. Word-Topic' tab."),
                          tags$li("Then, estimate coefficients first in the '5. Estimated Effects' tab.")
                        )
                      ),
                      selectizeInput(
                        "effect_cat_btn",
                        "Select a categorical covariate.",
                        choices = NULL,
                        multiple = TRUE
                      ),
                      actionButton("display_cat", "Display", icon = icon("file-alt"))
                    ),
                    # Step 7
                    conditionalPanel(
                      condition = "input.conditioned2 == 7",
                      helpText(
                        strong(
                          "Plot topic prevalence effects by continuous covariates."
                        ),
                        br(),
                        tags$ul(
                          tags$li("Select continuous covariate(s) in the '2. Word-Topic' tab."),
                          tags$li("Then, estimate coefficients first in the '5. Estimated Effects' tab.")
                        )
                      ),
                      selectizeInput(
                        "effect_con_btn",
                        "Select a continuous covariate.",
                        choices = NULL,
                        multiple = TRUE
                      ),
                      actionButton("display_con", "Display", icon = icon("file-alt"))
                    )
                ),

                mainPanel(
                  width = 9,
                  tabsetPanel(
                    id = "conditioned2",
                    tabPanel(
                      "1. Search K ",
                      value = 1,
                      shinycssloaders::withSpinner(plotly::plotlyOutput("search_K_plot"))
                    ),
                    tabPanel(
                      "2. Word-Topic",
                      value = 2,
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
                      shinycssloaders::withSpinner(uiOutput("topic_term_plot_uiOutput"))
                    ),
                    tabPanel(
                      "3. Document-Topic",
                      value = 3,
                      plotly::plotlyOutput(
                        "topic_by_prevalence_plot2",
                        height = 500,
                        width = 1000
                      ),
                      br(),
                      DT::dataTableOutput("topic_by_prevalence_table")
                    ),
                    tabPanel("4. Quotes", value = 4,
                             DT::dataTableOutput("quote_table")),
                    tabPanel(
                      "5. Estimated Effects",
                      value = 5,
                      DT::dataTableOutput("effect_table")
                    ),
                    tabPanel(
                      "6. Categorical Variables",
                      value = 6,
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
                      value = 7,
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
        ),
      tabPanel("Word Networks",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   # Step 1
                   conditionalPanel(
                     condition = "input.conditioned3 == 10",
                     helpText(strong("Visualize word co-occurrence network.")),
                     sliderInput(
                       "co_occurence_number_init",
                       "Minimum co-occurrence numbers",
                       value = 5,
                       min = 0,
                       max = 100
                     ),
                     actionButton("plot_word_co_occurrence_network", "Plot", icon = icon("search"))
                   ),
                   # Step 2
                   conditionalPanel(
                     condition = "input.conditioned3 == 11",
                     helpText(strong("Visualize word correlation network.")),
                     sliderInput(
                       "co_occurence_number",
                       "Minimum co-occurence numbers",
                       value = 5,
                       min = 0,
                       max = 100
                     ),
                     sliderInput(
                       "correlation_value",
                       "Minimum correlation betweewn pairwise words",
                       value = 0.3,
                       min = 0,
                       max = 1,
                       step = 0.1
                     ),
                     actionButton("plot_word_correlation_network", "Plot", icon = icon("search"))
                   ),
                   # Step 3
                   conditionalPanel(
                     condition = "input.conditioned3 == 12",
                     helpText(strong("Display changes in frequency over time. If you haven't already, run the Structural Topic Model first to extract topic-document probabilities.")),
                     selectizeInput(
                       "continuous_var_3",
                       "Select a time-related variable.",
                       choices = NULL,
                       multiple = TRUE
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
                     id = "conditioned3",
                     tabPanel(
                       "1. Word Co-Occurrence",
                       value = 10,
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
                       shinycssloaders::withSpinner(uiOutput("word_co_occurrence_network_plot_uiOutput"))
                     ),
                     tabPanel(
                       "2. Word Correlation Network",
                       value = 11,
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
                       shinycssloaders::withSpinner(uiOutput("word_correlation_network_plot_uiOutput"))
                     ),
                     tabPanel(
                       "3. Term Frequency Over Time",
                       value = 12,
                       bsCollapse(
                         open = 0,
                         bsCollapsePanel(
                           p(strong("Click to set plot dimensions"),
                             style = "font-size: 15px;"),
                           value = 5,
                           style = "success",
                           p(strong("Dimensions of the plot")),
                           div(
                             style = "display:inline-block",
                             sliderInput(
                               inputId = "height_line_year_plot,",
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
                               inputId = "width_line_year_plot,",
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
                       shinycssloaders::withSpinner(uiOutput("line_year_plot_uiOutput"))
                     )
                   )
                 )
               ))
    )
)
