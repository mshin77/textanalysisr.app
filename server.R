suppressPackageStartupMessages({
  library(dplyr)
  library(quanteda)
  library(quanteda.textstats)
  library(shiny)
  library(stm)
  library(tidyr)
  library(tidytext)
  library(plotly)
  library(stringr)
  library(DT)
  library(htmltools)
  library(tibble)
  library(openxlsx)
  library(shinyjs)
  library(shinybusy)
  library(TextAnalysisR)
  library(tools)
  library(proxy)
  library(cluster)
  library(visNetwork)
  library(digest)
})

options(shiny.maxRequestSize = 100 * 1024^2)
options(shiny.timeout = 300)

options(digits = 4)
options(scipen = 999)

server <- shinyServer(function(input, output, session) {
  options(shiny.error = function() {
    show_error_notification("An unexpected error occurred. Please try again or contact support.")
  })

  onStop(function() {
    gc(verbose = FALSE)
  })

  user_requests <- reactiveVal(list())

  `%||%` <- function(a, b) if (is.null(a)) b else a

  ################################################################################
  # DEPLOYMENT & FEATURE DETECTION
  ################################################################################

  is_web <- TextAnalysisR::check_web_deployment()
  feature_status <- reactive({
    TextAnalysisR::get_feature_status()
  })

  # Lazy spaCy initialization - only when needed

  spacy_initialized <- reactiveVal(FALSE)

  ensure_spacy <- function() {
    if (!spacy_initialized()) {
      tryCatch({
        suppressMessages(spacyr::spacy_initialize(model = "en_core_web_sm"))
        spacy_initialized(TRUE)
        TRUE
      }, error = function(e) {
        FALSE
      })
    } else {
      TRUE
    }
  }

  output$web_deployment_banner <- renderUI({
    TextAnalysisR::show_web_banner()
  })

  ################################################################################
  # HOME TAB
  ################################################################################

  output$about_content <- renderUI({
    includeMarkdown("markdown/about.md")
  })

  output$installation_semantic_content <- renderUI({
    includeMarkdown("markdown/installation_semantic.md")
  })

  output$installation_lexical_content <- renderUI({
    includeMarkdown("markdown/installation_lexical.md")
  })

  output$links_content <- renderUI({
    includeMarkdown("markdown/links.md")
  })

  output$cybersecurity_content <- renderUI({
    includeMarkdown("markdown/cybersecurity.md")
  })

  output$web_accessibility_content <- renderUI({
    includeMarkdown("markdown/web_accessibility.md")
  })

  output$support_content <- renderUI({
    includeMarkdown("markdown/support.md")
  })

  # Lazy-loaded Python availability check (deferred until file upload UI is shown)
  python_check_result <- reactiveVal(NULL)

  observe({
    req(input$dataset_choice == "Upload Your File")
    if (is.null(python_check_result())) {
      result <- tryCatch({
        env_check <- TextAnalysisR::check_python_env()
        env_check$available
      }, error = function(e) FALSE)
      python_check_result(result)
    }
  })

  # PDF Processing Status Indicator
  output$pdf_status_indicator <- renderUI({
    req(input$dataset_choice == "Upload Your File")
    python_available <- python_check_result()
    if (is.null(python_available)) return(NULL)

    if (python_available) {
      tags$div(
        style = "margin-top: -10px; margin-bottom: 10px; padding: 8px 12px; background-color: #D1FAE5; border: 1px solid #34D399; border-radius: 4px; font-size: 16px;",
        tags$i(class = "fa fa-check-circle", style = "color: #337ab7; margin-right: 5px;"),
        tags$strong("PDF Enhanced Mode:", style = "color: #047857;"),
        tags$span(" Python available for better table extraction", style = "color: #065F46;")
      )
    } else {
      tags$div(
        style = "margin-top: -10px; margin-bottom: 10px; padding: 8px 12px; background-color: #FEF3C7; border: 1px solid #FCD34D; border-radius: 4px; font-size: 16px;",
        tags$i(class = "fa fa-info-circle", style = "color: #D97706; margin-right: 5px;"),
        tags$strong("PDF Basic Mode:", style = "color: #B45309;"),
        tags$span(" R fallback (text only). ", style = "color: #92400E;"),
        tags$a(
          "Setup Python",
          href = "#",
          onclick = "Shiny.setInputValue('show_python_setup', Math.random()); return false;",
          style = "color: #B45309; text-decoration: underline;"
        )
      )
    }
  })

  # Multimodal PDF Options UI (conditional on Python availability)
  output$multimodal_options_ui <- renderUI({
    req(input$dataset_choice == "Upload Your File")
    python_available <- python_check_result()
    if (is.null(python_available)) return(NULL)

    if (is_web) {
      return(tags$div(
        class = "pdf-info-box",
        style = "margin-top: -10px; margin-bottom: 15px; padding: 12px; background-color: #FEF3C7; border: 1px solid #FCD34D; border-radius: 4px;",
        tags$div(
          style = "font-size: 16px; color: #92400E;",
          tags$i(class = "fa fa-info-circle", style = "margin-right: 5px;"),
          "Multimodal PDF extraction available in R package version only."
        )
      ))
    }

    if (!python_available) {
      return(tags$div(
        class = "pdf-info-box",
        style = "margin-top: -10px; margin-bottom: 15px; padding: 12px; background-color: #E0F2FE; border: 1px solid #BAE6FD; border-radius: 4px;",
        tags$div(
          style = "margin-bottom: 10px;",
          tags$strong("PDF with Charts/Diagrams?", style = "color: #0c1f4a; font-size: 16px;")
        ),
        tags$div(
          style = "font-size: 16px; color: #64748B;",
          "Multimodal extraction requires Python. ",
          tags$a(
            "Setup Python",
            href = "#",
            onclick = "Shiny.setInputValue('show_python_setup', Math.random()); return false;",
            style = "color: #337ab7; text-decoration: underline;"
          )
        )
      ))
    }

    tags$div(
      class = "pdf-info-box",
      style = "margin-top: -10px; margin-bottom: 15px; padding: 12px; background-color: #E0F2FE; border: 1px solid #BAE6FD; border-radius: 4px;",
      tags$div(
        style = "margin-bottom: 10px;",
        tags$strong("PDF with Charts/Diagrams?", style = "color: #0c1f4a; font-size: 16px;")
      ),
      checkboxInput("enable_multimodal",
        "Enable image/chart extraction",
        value = FALSE
      ),
      conditionalPanel(
        condition = "input.enable_multimodal == true",
        radioButtons("vision_provider",
          "Vision provider:",
          choices = c(
            "Local (Ollama - Free, Private)" = "ollama",
            "Cloud (OpenAI - Requires API Key)" = "openai"
          ),
          selected = "ollama",
          inline = FALSE
        ),
        conditionalPanel(
          condition = "input.vision_provider == 'ollama'",
          selectInput("ollama_vision_model",
            "Ollama model:",
            choices = c("llava", "bakllava", "llava-phi3"),
            selected = "llava"
          ),
          tags$div(
            class = "ollama-help-text",
            style = "font-size: 16px; color: #64748B; margin-top: -8px; margin-bottom: 8px;",
            HTML("First time? Run: <code>ollama pull llava</code>")
          )
        ),
        conditionalPanel(
          condition = "input.vision_provider == 'openai'",
          passwordInput("openai_api_key",
            "OpenAI API Key:",
            placeholder = "sk-..."
          ),
          selectInput("openai_vision_model",
            "OpenAI model:",
            choices = c("gpt-4o", "gpt-4-vision-preview"),
            selected = "gpt-4o"
          )
        )
      )
    )
  })

  # Show Python setup instructions modal
  observeEvent(input$show_python_setup, {
    output$modal_python_setup_output <- renderPrint({
      cat("Run in R console:\n\n")
      cat("TextAnalysisR::setup_python_env()\n\n")
      cat("Then restart the app.")
    })

    showModal(modalDialog(
      title = "Python Setup",
      verbatimTextOutput("modal_python_setup_output"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  create_error_plot <- function(message, color = "#6c757d") {
    TextAnalysisR::create_empty_plot_message(message, color = color)
  }

  truncate_text_with_ellipsis <- function(text, max_chars = 50) {
    text <- as.character(text)
    if (nchar(text) <= max_chars) {
      return(text)
    }
    paste0(substr(text, 1, max_chars), "...")
  }

  truncate_text_to_words <- function(text, max_words = 150) {
    text <- as.character(text)
    words <- strsplit(text, "\\s+")[[1]]

    if (length(words) > max_words) {
      truncated_text <- paste(words[1:max_words], collapse = " ")
      return(paste0(truncated_text, "..."))
    } else {
      return(text)
    }
  }

  wrap_long_text <- function(text, chars_per_line = 50, max_lines = 3) {
    text <- as.character(text)

    if (nchar(text) <= chars_per_line) return(text)

    if (!grepl(" ", text) && nchar(text) > chars_per_line) {
      lines <- character()
      remaining_text <- text
      while (nchar(remaining_text) > chars_per_line && length(lines) < max_lines - 1) {
        lines <- c(lines, substr(remaining_text, 1, chars_per_line))
        remaining_text <- substr(remaining_text, chars_per_line + 1, nchar(remaining_text))
      }
      if (nchar(remaining_text) > 0) {
        if (nchar(remaining_text) > chars_per_line) {
          lines <- c(lines, paste0(substr(remaining_text, 1, chars_per_line - 3), "..."))
        } else {
          lines <- c(lines, remaining_text)
        }
      }
      return(paste(lines, collapse = "\n"))
    }

    words <- strsplit(text, " ")[[1]]
    lines <- character()
    current_line <- ""

    for (word in words) {
      if (length(lines) >= max_lines - 1 && current_line != "") {
        lines <- c(lines, paste0(current_line, "..."))
        break
      }

      if (current_line == "") {
        test_line <- word
      } else {
        test_line <- paste(current_line, word)
      }

      if (nchar(test_line) <= chars_per_line) {
        current_line <- test_line
      } else {
        if (current_line != "") {
          lines <- c(lines, current_line)
          current_line <- word
        } else {
          if (length(lines) < max_lines - 1) {
            lines <- c(lines, paste0(substr(word, 1, chars_per_line - 3), "..."))
            current_line <- ""
          }
        }
      }
    }

    if (nchar(current_line) > 0) {
      if (length(lines) < max_lines) {
        lines <- c(lines, current_line)
      }
    }

    paste(lines[1:min(length(lines), max_lines)], collapse = "\n")
  }

  wrap_text_for_tooltip <- function(text, max_words = 150, chars_per_line = 50, max_lines = 3) {
    text <- as.character(text)

    if (nchar(text) > 150) {
      text_to_use <- substr(text, 1, 150)
      needs_ellipsis <- TRUE
    } else {
      text_to_use <- text
      needs_ellipsis <- FALSE
    }

    if (nchar(text_to_use) <= chars_per_line) {
      return(text_to_use)
    }

    result <- ""
    lines_created <- 0
    current_pos <- 1

    while (current_pos <= nchar(text_to_use) && lines_created < max_lines) {
      end_pos <- min(current_pos + chars_per_line - 1, nchar(text_to_use))
      line_text <- substr(text_to_use, current_pos, end_pos)

      if (end_pos < nchar(text_to_use) && lines_created < max_lines - 1) {
        last_space <- regexpr(" [^ ]*$", line_text)
        if (last_space > 0) {
          line_text <- substr(line_text, 1, last_space - 1)
          end_pos <- current_pos + last_space - 2
        }
      }

      if (result == "") {
        result <- line_text
      } else {
        result <- paste0(result, "\n", line_text)
      }

      lines_created <- lines_created + 1
      current_pos <- end_pos + 2
    }

    if (needs_ellipsis || current_pos <= nchar(text_to_use)) {
      result <- paste0(result, "...")
    }

    return(result)
  }

  stm_model_trigger <- reactiveVal(0)

  documents_data_reactive <- reactiveValues(
    has_dates = FALSE,
    date_column = NULL
  )

  ################################################################################
  # UPLOAD TAB
  ################################################################################

  observeEvent(input$dataset_choice, {
    updateCheckboxGroupInput(session, "show_vars", choices = character(0), selected = character(0))

    try(removeNotification("loadingFile"), silent = TRUE)

    if (input$dataset_choice == "Upload an Example Dataset") {
      shinyjs::disable("file")
      shinyjs::disable("text_input")
      shinyjs::reset("file")
      updateTextAreaInput(session, "text_input", value = "")
    } else if (input$dataset_choice == "Copy and Paste Text") {
      shinyjs::disable("file")
      shinyjs::enable("text_input")
      shinyjs::reset("file")
    } else if (input$dataset_choice == "Upload Your File") {
      shinyjs::enable("file")
      shinyjs::disable("text_input")
      updateTextAreaInput(session, "text_input", value = "")
    } else {
      shinyjs::disable("file")
      shinyjs::disable("text_input")
      shinyjs::reset("file")
      updateTextAreaInput(session, "text_input", value = "")
    }
  })

  file_validated <- reactiveVal(NULL)

  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      TextAnalysisR:::check_rate_limit(session$token, user_requests, max_requests = 100, window_seconds = 3600)

      file_data <- input$file[1, ]
      file_info_check <- list(name = file_data$name, size = file_data$size, datapath = file_data$datapath)
      TextAnalysisR:::validate_file_upload(file_info_check)

      file_validated(TRUE)

    }, error = function(e) {
      if (grepl("Rate limit", e$message)) {
        TextAnalysisR:::log_security_event("RATE_LIMIT_EXCEEDED", "File upload attempt", session, "WARNING")
      } else {
        TextAnalysisR:::log_security_event("FILE_UPLOAD_REJECTED",
          paste("Reason:", e$message), session, "WARNING")
      }
      showNotification(paste("Upload failed:", e$message), type = "error", duration = 10)
      file_validated(FALSE)
    })
  })

  mydata <- reactive({
    req(input$dataset_choice)

    tryCatch(
      {
        if (input$dataset_choice == "Upload an Example Dataset") {
          example_data <- NULL

          tryCatch(
            {
              example_data <- TextAnalysisR::SpecialEduTech
            },
            error = function(e) {
              tryCatch(
                {
                  data("SpecialEduTech", package = "TextAnalysisR", envir = environment())
                  if (exists("SpecialEduTech")) {
                    example_data <- SpecialEduTech
                  }
                },
                error = function(e) {
                  tryCatch(
                    {
                      example_data <- get("SpecialEduTech", envir = asNamespace("TextAnalysisR"))
                    },
                    error = function(e) {
                      library(TextAnalysisR)
                      example_data <- SpecialEduTech
                    }
                  )
                }
              )
            }
          )



          return(example_data)
        } else if (input$dataset_choice == "Copy and Paste Text") {
          if (is.null(input$text_input) || is.na(input$text_input) ||
              nchar(trimws(input$text_input)) == 0) {
            return(NULL)
          }

          text_content <- trimws(input$text_input)
          text_content <- TextAnalysisR:::sanitize_text_input(text_content)

          text_size_mb <- nchar(text_content) / (1024^2) * 2
          if (text_size_mb > 50) {
            showModal(modalDialog(
              title = tags$div(
                style = "color: #f59e0b;",
                icon("exclamation-triangle"),
                " Content Too Large"
              ),
              paste("The pasted content is approximately", round(text_size_mb, 2), "MB. Please paste content smaller than 50MB for optimal performance."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }

          if (nchar(text_content) < 10) {
            showModal(modalDialog(
              title = tags$div(
                style = "color: #f59e0b;",
                icon("exclamation-triangle"),
                " Text Too Short"
              ),
              "Please enter at least 10 characters of text to analyze.",
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }


          if (text_size_mb > 5 || nchar(text_content) > 100000) {
            show_loading_notification("Processing large content...", id = "loadingText")
          }


          tryCatch(
            {
              text_lines <- strsplit(text_content, "\n")[[1]]
              text_lines <- text_lines[text_lines != ""]

              if (length(text_lines) < 2) {
                stop("Not tabular data")
              }

              first_line <- text_lines[1]
              second_line <- text_lines[2]

              delimiters <- c("\t", ",", ";", "|")
              detected_delimiter <- NULL

              for (delim in delimiters) {
                if (grepl(delim, first_line) && grepl(delim, second_line)) {
                  delim_count_1 <- length(strsplit(first_line, delim, fixed = TRUE)[[1]])
                  delim_count_2 <- length(strsplit(second_line, delim, fixed = TRUE)[[1]])

                  if (delim_count_1 > 1 && delim_count_1 == delim_count_2) {
                    detected_delimiter <- delim
                    break
                  }
                }
              }

              if (is.null(detected_delimiter) && grepl("\\s{2,}", first_line) && grepl("\\s{2,}", second_line)) {
                first_fields <- strsplit(trimws(first_line), "\\s{2,}")[[1]]
                second_fields <- strsplit(trimws(second_line), "\\s{2,}")[[1]]

                if (length(first_fields) > 1 && length(first_fields) == length(second_fields)) {
                  text_lines <- sapply(text_lines, function(x) {
                    fields <- strsplit(trimws(x), "\\s{2,}")[[1]]
                    paste(fields, collapse = "\t")
                  })
                  detected_delimiter <- "\t"
                }
              }

              if (!is.null(detected_delimiter)) {
                lines_with_delim <- text_lines[sapply(text_lines, function(x) grepl(detected_delimiter, x))]

                if (length(lines_with_delim) >= 2) {
                  csv_content <- paste(lines_with_delim, collapse = "\n")

                  if (detected_delimiter == "\t") {
                    data <- read.delim(textConnection(csv_content), stringsAsFactors = FALSE, check.names = FALSE, quote = "\"", fill = TRUE, header = TRUE)
                  } else {
                    data <- read.csv(textConnection(csv_content), sep = detected_delimiter, stringsAsFactors = FALSE, check.names = FALSE, quote = "\"", fill = TRUE, header = TRUE)
                  }

                  col_names <- names(data)
                  col_names <- trimws(col_names)
                  col_names <- make.names(col_names, unique = TRUE)
                  names(data) <- col_names

                  has_meaningful_headers <- any(nchar(col_names) > 2 & !grepl("^[XV]\\d+$", col_names))

                  if (has_meaningful_headers) {
                    if (!"text" %in% names(data) && !"category" %in% names(data)) {
                      if (ncol(data) == 1) {
                        data$category <- rep("Custom", nrow(data))
                      }
                    }
                  } else {
                    if (!"text" %in% names(data)) {
                      if (ncol(data) >= 1) {
                        names(data)[1] <- "text"
                      }
                    }

                    if (ncol(data) >= 2 && !"category" %in% names(data)) {
                      names(data)[2] <- "category"
                    } else if (ncol(data) == 1) {
                      data$category <- rep("Custom", nrow(data))
                    }
                  }

                  text_col <- if ("text" %in% names(data)) "text" else names(data)[1]
                  data <- data[!is.na(data[[text_col]]) & nchar(trimws(data[[text_col]])) > 0, ]

                  if (nrow(data) > 0) {
                    try(removeNotification("loadingText"), silent = TRUE)
                    show_completion_notification(paste("Successfully parsed tabular data with", ncol(data), "columns and", nrow(data), "rows"))
                    return(data)
                  }
                }
              }

              stop("Not tabular data")
            },
            error = function(e) {
              result <- tryCatch(
                {
                  suppressWarnings(TextAnalysisR::import_files(input$dataset_choice, text_input = text_content))
                },
                error = function(e2) {
                  text_lines <- strsplit(text_content, "\n")[[1]]
                  text_lines <- text_lines[text_lines != ""]

                  if (length(text_lines) == 0) {
                    text_lines <- strsplit(text_content, "[.!?]+")[[1]]
                    text_lines <- trimws(text_lines[text_lines != ""])
                  }

                  if (length(text_lines) == 0) {
                    text_lines <- text_content
                  }

                  example_data <- data.frame(
                    text = text_lines,
                    category = rep("Custom", length(text_lines)),
                    stringsAsFactors = FALSE
                  )

                  try(removeNotification("loadingText"), silent = TRUE)
                  show_completion_notification(paste("Successfully processed", nrow(example_data), "text documents"))
                  return(example_data)
                }
              )
              return(result)
            }
          )
        } else if (input$dataset_choice == "Upload Your File") {
          if (is.null(input$file)) {
            return(NULL)
          }

          if (!isTRUE(file_validated())) {
            return(NULL)
          }

          if (!is.data.frame(input$file) || nrow(input$file) == 0) {
            stop("Invalid file upload data")
          }

          file_data <- input$file[1, ]

          required_cols <- c("name", "size", "type", "datapath")
          missing_cols <- setdiff(required_cols, names(file_data))
          if (length(missing_cols) > 0) {
            stop("Missing file information: ", paste(missing_cols, collapse = ", "))
          }

          file_size_mb <- file_data$size / (1024^2)
          if (file_size_mb > 100) {
            showModal(modalDialog(
              title = tags$div(
                style = "color: #f59e0b;",
                icon("exclamation-triangle"),
                " File Too Large"
              ),
              paste("The uploaded file is", round(file_size_mb, 2), "MB. Please upload a file smaller than 100MB."),
              easyClose = TRUE,
              footer = modalButton("Close")
            ))
            return(NULL)
          }

          if (file_size_mb > 50) {
            show_loading_notification("Processing large file...", id = "loadingFile")
          }

          if (!file.exists(file_data$datapath)) {
            stop("File not found")
          }

          file_info <- data.frame(filepath = file_data$datapath, stringsAsFactors = FALSE)

          if (!file.exists(file_data$datapath)) {
            stop("File path does not exist: ", file_data$datapath)
          }

          if (file.info(file_data$datapath)$size == 0) {
            stop("File is empty")
          }

          if (!requireNamespace("TextAnalysisR", quietly = TRUE)) {
            stop("TextAnalysisR package is not available")
          }

          if (!exists("import_files", where = asNamespace("TextAnalysisR"))) {
            stop("TextAnalysisR::import_files function not found")
          }

          file_extension <- tolower(tools::file_ext(file_data$name))

          if (file_extension == "pdf") {
            # Use unified PDF processing function
            use_multimodal <- isTRUE(input$enable_multimodal)

            # Check multimodal feature availability
            if (use_multimodal && !TextAnalysisR::check_feature("python")) {
              showNotification("Multimodal PDF requires Python. Using standard extraction.", type = "warning")
              use_multimodal <- FALSE
            }

            # Show loading notification
            loading_msg <- if (use_multimodal) {
              "Extracting text and analyzing images/charts..."
            } else {
              "Processing PDF file..."
            }
            show_loading_notification(loading_msg, id = "processingPDF")

            # Get settings
            vision_provider <- input$vision_provider %||% "ollama"
            api_key <- if (vision_provider == "openai") input$openai_api_key else NULL
            vision_model <- if (vision_provider == "ollama") {
              input$ollama_vision_model %||% "llava"
            } else {
              input$openai_vision_model %||% "gpt-4o"
            }

            # Process PDF with unified function
            pdf_result <- tryCatch({
              TextAnalysisR::process_pdf_unified(
                file_path = file_data$datapath,
                use_multimodal = use_multimodal,
                vision_provider = vision_provider,
                vision_model = vision_model,
                api_key = api_key,
                describe_images = TRUE
              )
            }, error = function(e) {
              list(
                success = FALSE,
                type = "error",
                method = "none",
                message = paste("PDF processing error:", e$message)
              )
            })

            try(removeNotification("processingPDF"), silent = TRUE)

            # Handle result
            if (!pdf_result$success) {
              error_type <- pdf_result$type %||% "error"

              if (error_type == "prerequisite_error") {
                showModal(modalDialog(
                  title = tags$div(
                    style = "color: #f59e0b;",
                    icon("exclamation-triangle"),
                    " Multimodal Extraction - Setup Required"
                  ),
                  tags$div(
                    style = "white-space: pre-wrap; font-family: monospace; font-size: 16px;",
                    pdf_result$message
                  ),
                  easyClose = TRUE,
                  footer = modalButton("Close")
                ))
              } else if (error_type == "extraction_error") {
                showModal(modalDialog(
                  title = tags$div(
                    style = "color: #dc2626;",
                    icon("times-circle"),
                    " Multimodal Extraction Error"
                  ),
                  tags$div(
                    style = "white-space: pre-wrap; font-family: monospace; font-size: 16px;",
                    pdf_result$message
                  ),
                  easyClose = TRUE,
                  footer = modalButton("Close")
                ))
              } else {
                showModal(modalDialog(
                  title = tags$div(
                    style = "color: #dc2626;",
                    icon("times-circle"),
                    " PDF Processing Failed"
                  ),
                  pdf_result$message,
                  easyClose = TRUE,
                  footer = modalButton("Close")
                ))
              }
              return(NULL)
            }

            # Show success notification with method info
            if (pdf_result$type == "multimodal") {
              showNotification(
                HTML(paste0(
                  "✓ Multimodal extraction: ", pdf_result$num_images, " images<br>",
                  "Provider: ", pdf_result$vision_provider
                )),
                type = "message",
                duration = 8
              )
            } else {
              method_label <- switch(pdf_result$method,
                "python" = "Python (Enhanced)",
                "r" = "R (Basic)",
                "Unknown"
              )
              showNotification(
                paste0("✓ ", method_label, ": ", pdf_result$message),
                type = "message",
                duration = 5
              )
            }

            # Prepare result data frame
            result <- pdf_result$data
            if (!"category" %in% names(result)) {
              result$category <- rep("Uploaded_PDF", nrow(result))
            }

          } else {
            result <- tryCatch(
              {
                suppressWarnings(suppressMessages({
                  TextAnalysisR::import_files(input$dataset_choice, file_info = file_info)
                }))
              },
              error = function(e) {
                if (file_extension %in% c("csv", "txt")) {
                  if (file_extension == "csv") {
                    data <- read.csv(file_data$datapath, stringsAsFactors = FALSE, quote = "\"", fill = TRUE)
                  } else {
                    lines <- readLines(file_data$datapath)
                    data <- data.frame(
                      text = lines[lines != ""],
                      category = rep("Uploaded", length(lines[lines != ""])),
                      stringsAsFactors = FALSE
                    )
                  }

                  if (!"text" %in% names(data)) {
                    if (ncol(data) >= 1) {
                      names(data)[1] <- "text"
                    }
                    if (ncol(data) >= 2 && !"category" %in% names(data)) {
                      names(data)[2] <- "category"
                    }
                  }

                  return(data)
                } else {
                  stop("Unsupported file format. Please upload CSV, TXT, or PDF files.")
                }
              }
            )
          }

          if (is.null(result)) {
            stop("Unable to process file")
          }

          if (!is.data.frame(result)) {
            stop("File processing did not return a valid data frame")
          }

          if (nrow(result) == 0) {
            stop("File processing returned an empty data frame")
          }

          try(removeNotification("loadingFile"), silent = TRUE)

          return(result)
        } else {
          return(NULL)
        }
      },
      error = function(e) {
        try(removeNotification("loadingFile"), silent = TRUE)

        cat("=== FILE PROCESSING ERROR DEBUG ===\n")
        cat("Error message:", e$message, "\n")
        cat("Error class:", class(e), "\n")
        if (!is.null(e$call)) cat("Error call:", deparse(e$call), "\n")
        if (exists("input") && !is.null(input$file)) {
          cat("File name:", input$file$name, "\n")
          cat("File size:", input$file$size, "bytes\n")
          cat("File type:", input$file$type, "\n")
          cat("File datapath:", input$file$datapath, "\n")
          cat("File exists:", file.exists(input$file$datapath), "\n")
        }
        cat("=== END DEBUG ===\n")

        error_msg <- if (nzchar(e$message)) {
          e$message
        } else {
          "Unknown error occurred while processing the file. Check console for details."
        }

        showModal(modalDialog(
          title = tags$div(
            style = "color: #dc2626;",
            icon("times-circle"),
            " Error"
          ),
          error_msg,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

        return(NULL)
      }
    )
  })

  output$data_table <- DT::renderDataTable(
    {
      req(mydata())
      mydata()
    },
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      width = "100%",
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print")
    )
  )

  output$data_loaded <- reactive({
    !is.null(mydata())
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)


  colnames <- reactive({
    data <- mydata()
    if (is.data.frame(data)) {
      return(names(data))
    } else {
      return(NULL)
    }
  })

  observeEvent(mydata(), {
    data <- mydata()

    if (is.null(data)) {
      return()
    }

    col_names <- colnames()
    if (!is.null(col_names)) {
      updateCheckboxGroupInput(
        session,
        "show_vars",
        choices = col_names,
        selected = ""
      )

      updateSelectizeInput(
        session,
        "doc_date_var",
        choices = c("None" = "", col_names),
        selected = ""
      )

      updateSelectizeInput(
        session,
        "lemma_doc_id_var",
        choices = c("None" = "", col_names),
        selected = ""
      )

      if (length(col_names) > 2) {
        showNotification(paste("Detected", length(col_names), "columns available for selection"), type = "message", duration = 3)
      }
    }
  })


  listed_vars <- eventReactive(input$show_vars, {
    input$show_vars
  })

  united_tbl <- eventReactive(input$apply, {
    if (is.null(listed_vars()) || length(listed_vars()) == 0) {
      showNotification("Please select at least one column to unite", type = "error")
      return(NULL)
    }

    req(mydata())

    tryCatch(
      {
        united_data <- TextAnalysisR::unite_cols(
          mydata(),
          listed_vars = listed_vars()
        )

        show_completion_notification(paste("Successfully united", length(listed_vars()), "columns into 'united_texts' while keeping original columns"))
        return(united_data)
      },
      error = function(e) {
        show_error_notification(paste("Error uniting columns:", e$message))
        return(NULL)
      }
    )
  })

  output$has_united_table_results <- reactive({
    !is.null(input$apply) && input$apply > 0 && !is.null(united_tbl())
  })
  outputOptions(output, "has_united_table_results", suspendWhenHidden = FALSE)

  output$united_table <- DT::renderDataTable(
    {
      req(input$apply)
      united_tbl()
    },
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      scrollX = TRUE,
      width = "100%",
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print")
    )
  )

  ################################################################################
  # PREPROCESS TAB
  ################################################################################

  preprocessed_skip <- reactiveVal(NULL)
  processed_tokens <- reactiveVal(NULL)
  stopwords_applied <- reactiveVal(FALSE)
  dictionary_applied <- reactiveVal(FALSE)
  final_tokens <- reactiveVal(NULL)
  lemmatized_tokens <- reactiveVal(NULL)
  lemma_applied <- reactiveVal(FALSE)
  last_clicked <- reactiveVal(NULL)
  math_mode_used <- reactiveVal(FALSE)
  compound_stats <- reactiveVal(NULL)
  dfm_tokens <- reactiveVal(NULL)

  step_2_version <- reactiveVal(0)
  step_3_version <- reactiveVal(0)
  step_4_version <- reactiveVal(0)
  step_3_based_on <- reactiveVal(0)
  step_4_based_on <- reactiveVal(0)
  step_5_based_on <- reactiveVal(0)

  output$step_3_outdated <- reactive({
    stopwords_applied() && step_3_based_on() < step_2_version() && step_2_version() > 0
  })
  outputOptions(output, "step_3_outdated", suspendWhenHidden = FALSE)

  output$step_4_outdated <- reactive({
    dictionary_applied() && step_4_based_on() < step_3_version() && step_3_version() > 0
  })
  outputOptions(output, "step_4_outdated", suspendWhenHidden = FALSE)

  output$step_5_outdated <- reactive({
    (!is.null(input$dfm_btn) && input$dfm_btn > 0) &&
      step_5_based_on() < step_4_version() && step_4_version() > 0
  })
  outputOptions(output, "step_5_outdated", suspendWhenHidden = FALSE)

  preprocessed_init <- eventReactive(input$preprocess, {
    req(united_tbl())

    preprocessed_skip(NULL)
    processed_tokens(NULL)
    dictionary_applied(FALSE)
    final_tokens(NULL)
    lemmatized_tokens(NULL)
    last_clicked(NULL)

    show_loading_notification("Processing text preprocessing...", id = "loadingPreprocess")

    tryCatch({
      raw_output <- capture.output(
        {
          segment_options <- input$segment_options %||% character(0)
          math_mode_enabled <- isTRUE(input$math_mode)

          if (math_mode_enabled) {
            segment_options <- setdiff(segment_options, c("remove_numbers", "remove_symbols"))
            math_mode_used(TRUE)
          } else {
            math_mode_used(FALSE)
          }

          toks_processed <- TextAnalysisR::prep_texts(
            united_tbl(),
            text_field = "united_texts",
            min_char = input$min_char %||% 2,
            lowercase = "lowercase" %in% segment_options,
            remove_punct = "remove_punct" %in% segment_options && !math_mode_enabled,
            remove_symbols = "remove_symbols" %in% segment_options && !math_mode_enabled,
            remove_numbers = "remove_numbers" %in% segment_options && !math_mode_enabled,
            remove_url = "remove_url" %in% segment_options,
            remove_separators = "remove_separators" %in% segment_options,
            split_hyphens = "split_hyphens" %in% segment_options,
            split_tags = "split_tags" %in% segment_options,
            include_docvars = "include_docvars" %in% segment_options,
            keep_acronyms = "keep_acronyms" %in% segment_options,
            padding = "padding" %in% segment_options,
            verbose = TRUE
          )
        },
        type = "message"
      )

      cleaned_output <- sapply(raw_output, function(line) {
        line <- gsub("\\033\\[[0-9;]*[A-Za-z]", "", line)
        line <- gsub("[[:cntrl:]]", "", line)
        line <- gsub("[gG]+[0-9]*;", "", line)
        line <- gsub("^[gG]+", "", line)
        line <- gsub("[gG]+$", "", line)
        line <- gsub("\\d{10,}", "", line)
        line <- gsub("Processed .* documents.*", "", line)
        line <- gsub(".*total tokens.*", "", line)
        trimws(line)
      })
      cleaned_output <- cleaned_output[nzchar(cleaned_output)]

      num_docs <- quanteda::ndoc(toks_processed)
      total_tokens <- sum(quanteda::ntoken(toks_processed))
      avg_tokens <- round(mean(quanteda::ntoken(toks_processed)), 1)
      num_types <- length(unique(unlist(as.list(toks_processed))))
      min_char_used <- input$min_char %||% 2

      preprocessing_applied <- c()

      if (math_mode_enabled) {
        preprocessing_applied <- c(preprocessing_applied, "  MATH MODE ENABLED: Numbers, symbols, and punctuation preserved")
      }

      if ("remove_punct" %in% segment_options && !math_mode_enabled) preprocessing_applied <- c(preprocessing_applied, "  - Punctuation removed")
      if ("remove_symbols" %in% segment_options && !math_mode_enabled) preprocessing_applied <- c(preprocessing_applied, "  - Symbols removed")
      if ("remove_numbers" %in% segment_options && !math_mode_enabled) preprocessing_applied <- c(preprocessing_applied, "  - Numbers removed")
      if ("remove_url" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - URLs removed")
      if ("remove_separators" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - Separators removed")
      if ("split_hyphens" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - Hyphens split")
      if ("split_tags" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - Tags split")
      if ("keep_acronyms" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - Acronyms kept")
      if ("padding" %in% segment_options) preprocessing_applied <- c(preprocessing_applied, "  - Padding applied")
      preprocessing_applied <- c(preprocessing_applied, paste0("  - Minimum characters: ", min_char_used))

      final_output <- c(
        paste("Documents:", num_docs),
        paste("Total tokens:", format(total_tokens, big.mark = ",")),
        paste("Unique tokens:", format(num_types, big.mark = ",")),
        paste("Average tokens per document:", avg_tokens),
        "",
        "Preprocessing Options Applied:",
        preprocessing_applied
      )

      try(removeNotification("loadingPreprocess"), silent = TRUE)
      show_completion_notification(paste("Text preprocessing completed for", quanteda::ndoc(toks_processed), "documents"))

      output$modal_pre_init_verbose_output <- renderPrint({
        cat(paste(final_output, collapse = "\n"))
      })

      step_2_version(step_2_version() + 1)

      return(toks_processed)
    }, error = function(e) {
      try(removeNotification("loadingPreprocess"), silent = TRUE)
      show_error_notification(paste("Error in preprocessing:", e$message), duration = 10)
      return(NULL)
    })
  })

  observeEvent(input$preprocess, {
    result <- preprocessed_init()
    req(!is.null(result))

    Sys.sleep(0.1)

    shiny::showModal(
      shiny::modalDialog(
        title = "Segmentation Complete",
        shiny::verbatimTextOutput("modal_pre_init_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  output$has_preprocess_results <- reactive({
    (!is.null(input$preprocess) && input$preprocess > 0) || (!is.null(input$skip_segment) && input$skip_segment > 0)
  })
  outputOptions(output, "has_preprocess_results", suspendWhenHidden = FALSE)

  output$dict_print_preprocess <- renderPrint({
    req(input$preprocess > 0 | input$skip_segment > 0)
    req(preprocessed_combined())
    preprocessed_combined() %>% glimpse()
  })

  observeEvent(input$skip_segment, {
    req(united_tbl())

    processed_tokens(NULL)
    dictionary_applied(FALSE)
    final_tokens(NULL)
    lemmatized_tokens(NULL)
    last_clicked(NULL)

    show_loading_notification("Skipping segmentation - using basic tokenization...", id = "loadingSkipSegment")

    tryCatch({
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )

      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }

      preprocessed_skip(toks)

      num_docs <- quanteda::ndoc(toks)
      total_tokens <- sum(quanteda::ntoken(toks))
      avg_tokens <- round(mean(quanteda::ntoken(toks)), 1)
      num_types <- length(unique(unlist(as.list(toks))))

      final_output <- c(
        paste("Documents:", num_docs),
        paste("Total tokens:", format(total_tokens, big.mark = ",")),
        paste("Unique tokens:", format(num_types, big.mark = ",")),
        paste("Average tokens per document:", avg_tokens),
        "",
        "Segmentation Skipped - Text Split Into Words Only:",
        "  - All punctuation kept",
        "  - All symbols kept",
        "  - All numbers kept",
        "  - All URLs kept",
        "  - All separators kept",
        "  - No minimum character filtering",
        "  - Hyphens not split",
        "  - No cleaning or preprocessing applied"
      )

      try(removeNotification("loadingSkipSegment"), silent = TRUE)
      show_completion_notification(paste("Basic tokenization completed for", num_docs, "documents"))

      step_2_version(step_2_version() + 1)

      output$modal_skip_segment_output <- renderPrint({
        cat(paste(final_output, collapse = "\n"))
      })

      shiny::showModal(
        shiny::modalDialog(
          title = "Segmentation Skipped",
          shiny::verbatimTextOutput("modal_skip_segment_output"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    }, error = function(e) {
      try(removeNotification("loadingSkipSegment"), silent = TRUE)
      show_error_notification(paste("Error in basic tokenization:", e$message), duration = 10)
    })
  })

  preprocessed_combined <- reactive({
    if (!is.null(preprocessed_skip())) {
      return(preprocessed_skip())
    } else if (!is.null(try(preprocessed_init(), silent = TRUE)) && !inherits(try(preprocessed_init(), silent = TRUE), "try-error")) {
      return(preprocessed_init())
    } else if (!is.null(united_tbl())) {
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      return(toks)
    }
    return(NULL)
  })

  observe({
    req(preprocessed_combined())

    tryCatch({
      dfm_temp <- quanteda::dfm(preprocessed_combined())

      freq_df <- quanteda.textstats::textstat_frequency(dfm_temp)

      top_100_words <- freq_df$feature[1:min(100, nrow(freq_df))]

      top_10_words <- freq_df$feature[1:min(10, nrow(freq_df))]

      updateSelectizeInput(
        session,
        "common_words",
        choices = top_100_words,
        selected = top_10_words,
        server = FALSE
      )
    }, error = function(e) {
      message("Error updating common_words: ", e$message)
    })
  })



  custom_dict_terms <- reactive({
    req(input$custom_dict)
    ext <- tools::file_ext(input$custom_dict$name)
    if (tolower(ext) == "csv") {
      terms <- read.csv(input$custom_dict$datapath, stringsAsFactors = FALSE)[[1]]
    } else {
      terms <- readLines(input$custom_dict$datapath, warn = FALSE)
    }
    terms <- trimws(terms)
    terms <- terms[nzchar(terms)]
    terms
  })


  use_custom_dict <- reactive({
    isTRUE(input$use_custom_dict)
  })

  ngram_stats <- reactiveValues(
    full_stats = NULL,
    collocation_list = NULL
  )

  output$dynamic_ngram_checkboxes <- renderUI({
    ngram_counts <- list("2" = 0, "3" = 0, "4" = 0, "5" = 0)

    if (!is.null(ngram_stats$full_stats) && nrow(ngram_stats$full_stats) > 0) {
      stats_df <- ngram_stats$full_stats
      stats_df$ngram_length <- sapply(strsplit(stats_df$collocation, " "), length)
      counts_table <- table(stats_df$ngram_length)

      for (n in names(counts_table)) {
        ngram_counts[[n]] <- counts_table[[n]]
      }
    }

    choices <- list()
    choices[[paste0("Bigrams (", ngram_counts[["2"]], " n-grams)")]] <- 2
    choices[[paste0("Trigrams (", ngram_counts[["3"]], " n-grams)")]] <- 3
    choices[[paste0("4-grams (", ngram_counts[["4"]], " n-grams)")]] <- 4
    choices[[paste0("5-grams (", ngram_counts[["5"]], " n-grams)")]] <- 5

    selected <- if (!is.null(input$ngram_sizes)) {
      input$ngram_sizes
    } else {
      c(2, 3, 4, 5)
    }

    checkboxGroupInput(
      "ngram_sizes",
      label = HTML("<h5><strong>Select n-gram types:</strong></h5>"),
      choices = choices,
      selected = selected
    )
  })

  ngram_stats <- reactiveVal(NULL)
  top_20_preselected <- reactiveVal(NULL)

  output$has_ngram_detection_results <- reactive({
    !is.null(ngram_stats()) && nrow(ngram_stats()) > 0
  })
  outputOptions(output, "has_ngram_detection_results", suspendWhenHidden = FALSE)

  observeEvent(input$detect_ngrams, {
    req(preprocessed_combined())

    compound_stats(NULL)

    ngram_sizes <- as.integer(input$ngram_sizes)
    if (is.null(ngram_sizes) || length(ngram_sizes) == 0) {
      showNotification("Please select at least one n-gram size", type = "warning")
      return()
    }

    tstat <- quanteda.textstats::textstat_collocations(
      preprocessed_combined(),
      size = ngram_sizes,
      min_count = input$ngram_min_count
    )

    tstat_filtered <- tstat %>%
      dplyr::filter(lambda >= input$ngram_min_lambda) %>%
      dplyr::arrange(desc(count), desc(lambda))

    tryCatch({
      patterns <- lapply(tstat_filtered$collocation, function(x) {
        unlist(strsplit(x, "\\s+"))
      })

      temp_compound <- quanteda::tokens_compound(
        preprocessed_combined(),
        pattern = patterns,
        concatenator = "_",
        case_insensitive = TRUE,
        join = FALSE
      )
      temp_dfm <- quanteda::dfm(temp_compound)
      temp_freq <- quanteda.textstats::textstat_frequency(temp_dfm)

      actual_counts <- sapply(tstat_filtered$collocation, function(ngram) {
        ngram_underscore <- tolower(gsub(" ", "_", ngram))
        matched <- temp_freq %>%
          dplyr::filter(tolower(feature) == ngram_underscore)

        if (nrow(matched) > 0) {
          return(sum(matched$frequency))
        } else {
          return(tstat_filtered$count[tstat_filtered$collocation == ngram])
        }
      })

      tstat_filtered$count <- actual_counts
    }, error = function(e) {
      message("Could not calculate actual compound counts: ", e$message)
    })

    ngram_stats(tstat_filtered)

    top_ngrams <- tstat_filtered$collocation[1:min(100, nrow(tstat_filtered))]
    suggested_ngrams <- tstat_filtered$collocation[1:min(20, nrow(tstat_filtered))]

    top_20_preselected(suggested_ngrams)

    updateSelectizeInput(
      session,
      "multi_word_expressions",
      choices = top_ngrams,
      selected = suggested_ngrams,
      options = list(create = TRUE)
    )

    ngram_breakdown <- tstat_filtered %>%
      dplyr::mutate(ngram_type = dplyr::case_when(
        length == 2 ~ "Bigrams",
        length == 3 ~ "Trigrams",
        length == 4 ~ "4-grams",
        length == 5 ~ "5-grams",
        TRUE ~ paste0(length, "-grams")
      )) %>%
      dplyr::group_by(ngram_type) %>%
      dplyr::summarise(count = dplyr::n(), .groups = 'drop') %>%
      dplyr::arrange(ngram_type)

    total_found <- nrow(tstat_filtered)
    top_selected <- min(20, total_found)

    modal_output <- c(
      paste("Total n-grams found:", total_found),
      paste("Top n-grams pre-selected:", top_selected),
      "",
      "Breakdown by n-gram size:"
    )

    for (i in 1:nrow(ngram_breakdown)) {
      modal_output <- c(modal_output,
        paste0("  ", ngram_breakdown$ngram_type[i], ": ", ngram_breakdown$count[i])
      )
    }

    output$modal_ngram_detection <- renderPrint({
      cat(paste(modal_output, collapse = "\n"))
    })

    Sys.sleep(0.1)

    shiny::showModal(
      shiny::modalDialog(
        title = "N-gram Detection Complete",
        shiny::verbatimTextOutput("modal_ngram_detection"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  observeEvent(input$skip_ngram_detection, {
    req(preprocessed_combined())

    ngram_stats(data.frame(
      collocation = character(0),
      count = numeric(0),
      lambda = numeric(0),
      z = numeric(0),
      stringsAsFactors = FALSE
    ))

    updateSelectizeInput(
      session,
      "multi_word_expressions",
      choices = NULL,
      selected = NULL
    )

    showNotification("Skipping n-gram detection - no multi-word expressions will be detected", type = "message", duration = 3)
  })

  observeEvent(input$dictionary, {
    toks_source <- if (!is.null(final_tokens())) {
      showNotification("Using stopword-filtered tokens", type = "message", duration = 3)
      final_tokens()
    } else if (!is.null(preprocessed_combined()) &&
               (!is.null(try(preprocessed_init(), silent = TRUE)) || !is.null(preprocessed_skip()))) {
      preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      showNotification("Creating tokens from united text...", type = "message", duration = 2)
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      toks
    } else {
      NULL
    }

    if (is.null(toks_source)) {
      showNotification("Please complete Step 1 (Unite Text) first", type = "warning", duration = 5)
      return(NULL)
    }

    lemmatized_tokens(NULL)
    last_clicked(NULL)

    if (is.null(input$multi_word_expressions) || length(input$multi_word_expressions) == 0) {
      processed_tokens(toks_source)
      dictionary_applied(TRUE)
      compound_stats(NULL)
      step_4_version(step_4_version() + 1)
      step_4_based_on(step_3_version())
    } else {
      # Convert multi-word expressions to character vectors for pattern matching
      patterns <- lapply(input$multi_word_expressions, function(x) {
        unlist(strsplit(x, "\\s+"))
      })

      toks_compound <- quanteda::tokens_compound(
        toks_source,
        pattern = patterns,
        concatenator = "_",
        case_insensitive = TRUE,
        join = FALSE
      )

      # Clean stopwords from compound edges using standard English stopwords
      all_stopwords <- tolower(stopwords::stopwords("en", source = "snowball"))

      clean_leading <- "leading_stopwords" %in% (input$stopword_options %||% character(0))
      clean_trailing <- "trailing_stopwords" %in% (input$stopword_options %||% character(0))

      if (clean_leading || clean_trailing) {
        docvars_backup <- quanteda::docvars(toks_compound)

        toks_list <- lapply(as.list(toks_compound), function(token_vector) {
          vapply(token_vector, function(tok) {
            if (grepl("_", tok, fixed = TRUE)) {
              parts <- unlist(strsplit(tok, "_", fixed = TRUE))

              # Keep cleaning edges until no more stopwords at edges
              changed <- TRUE
              while (changed && length(parts) > 0) {
                changed <- FALSE

                # Remove leading stopwords
                if (clean_leading && length(parts) > 0 && tolower(parts[1]) %in% all_stopwords) {
                  parts <- parts[-1]
                  changed <- TRUE
                }

                # Remove trailing stopwords
                if (clean_trailing && length(parts) > 0 && tolower(parts[length(parts)]) %in% all_stopwords) {
                  parts <- parts[-length(parts)]
                  changed <- TRUE
                }
              }

              if (length(parts) == 0) {
                return("")
              }
              paste(parts, collapse = "_")
            } else {
              tok
            }
          }, character(1), USE.NAMES = FALSE)
        })

        toks_compound <- quanteda::as.tokens(toks_list)
        toks_compound <- quanteda::tokens_remove(toks_compound, "")

        if (!is.null(docvars_backup) && nrow(docvars_backup) > 0) {
          quanteda::docvars(toks_compound) <- docvars_backup
        }
      }

      processed_tokens(toks_compound)
      dictionary_applied(TRUE)
      step_4_version(step_4_version() + 1)
      step_4_based_on(step_3_version())

      temp_dfm <- quanteda::dfm(toks_compound)
      temp_freq <- quanteda.textstats::textstat_frequency(temp_dfm)

      selected_ngrams_normalized <- tolower(gsub(" ", "_", input$multi_word_expressions))

      compound_freq <- temp_freq %>%
        dplyr::filter(tolower(feature) %in% selected_ngrams_normalized) %>%
        dplyr::group_by(feature) %>%
        dplyr::summarise(
          frequency = sum(frequency),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          Size = stringr::str_count(feature, "_") + 1
        )

      compound_stats(compound_freq)
    }
  })

  observeEvent(input$skip_dictionary, {
    toks_source <- if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      toks
    } else {
      NULL
    }

    if (is.null(toks_source)) {
      showNotification("Please complete Step 1 (Unite Text) first", type = "warning", duration = 5)
      return(NULL)
    }

    lemmatized_tokens(NULL)
    last_clicked(NULL)
    compound_stats(NULL)

    showNotification("Skipping multi-word dictionary...", type = "message", duration = 3)

    processed_tokens(toks_source)
    dictionary_applied(TRUE)
    step_4_version(step_4_version() + 1)
    step_4_based_on(step_3_version())

    num_docs <- quanteda::ndoc(toks_source)
    total_tokens <- sum(quanteda::ntoken(toks_source))
    num_types <- length(unique(unlist(as.list(toks_source))))

    final_output <- c(
      paste("Documents:", num_docs),
      paste("Total tokens:", format(total_tokens, big.mark = ",")),
      paste("Unique tokens:", format(num_types, big.mark = ",")),
      "",
      "Multi-Word Dictionary Skipped:",
      "  - No multi-word expressions compounded",
      "  - All tokens kept as single words",
      "  - Tokens passed through unchanged"
    )

    output$modal_skip_dictionary_output <- renderPrint({
      cat(paste(final_output, collapse = "\n"))
    })

    shiny::showModal(
      shiny::modalDialog(
        title = "Multi-Word Dictionary Skipped",
        shiny::verbatimTextOutput("modal_skip_dictionary_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  output$dict_print_dictionary <- renderPrint({
    req(processed_tokens())
    processed_tokens() %>% glimpse()
  })

  output$ngram_detection_plot <- plotly::renderPlotly({
    req(ngram_stats())
    TextAnalysisR::plot_ngram_frequency(
      ngram_data = ngram_stats(),
      top_n = 30,
      selected = input$multi_word_expressions,
      title = "N-gram Frequency"
    )
  })

  output$ngram_detection_plot_uiOutput <- renderUI({
    div(
      style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotly::plotlyOutput("ngram_detection_plot", height = 500, width = "100%")
    )
  })

  output$ngram_detection_table <- DT::renderDataTable({
    req(ngram_stats())
    tstat <- ngram_stats()

    tstat %>%
      dplyr::mutate(
        selected = ifelse(collocation %in% input$multi_word_expressions, "Selected", ""),
        length = stringr::str_count(collocation, "\\S+")
      ) %>%
      dplyr::select(
        Selected = selected,
        `N-gram` = collocation,
        Size = length,
        Frequency = count,
        Lambda = lambda,
        `Z-score` = z
      ) %>%
      dplyr::mutate(
        Lambda = round(Lambda, 2),
        `Z-score` = round(`Z-score`, 2)
      )
  },
  rownames = FALSE,
  extensions = 'Buttons',
  filter = 'top',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 25,
    order = list(list(4, 'desc')),
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

  output$has_dictionary_results <- reactive({
    dictionary_applied()
  })
  outputOptions(output, "has_dictionary_results", suspendWhenHidden = FALSE)

  dfm_dictionary <- reactive({
    req(input$dictionary)
    req(processed_tokens())
    dfm_obj <- quanteda::dfm(processed_tokens())
    dfm_obj
  })

  observeEvent(dfm_dictionary(), {
    req(dfm_dictionary())

    isolate({
      dfm_obj <- dfm_dictionary()
      doc_count <- quanteda::ndoc(dfm_obj)
      feature_count <- quanteda::nfeat(dfm_obj)

      freq_all <- quanteda.textstats::textstat_frequency(dfm_obj)
      mwe_count <- sum(stringr::str_detect(freq_all$feature, "_"))
      single_word_count <- feature_count - mwe_count

      selected_mwe <- input$multi_word_expressions
      selected_count <- length(selected_mwe)

      combined_output <- c(
        paste("Selected n-grams:", selected_count),
        paste("Compounded multi-word expressions:", mwe_count),
        paste("Single words:", single_word_count),
        paste("Total features:", format(feature_count, big.mark = ",")),
        paste("Documents:", doc_count)
      )

      output$modal_mwe_output <- renderPrint({
        cat(paste(combined_output, collapse = "\n"))
      })

      Sys.sleep(0.1)

      shiny::showModal(
        shiny::modalDialog(
          title = "Multi-Word Compounding Complete",
          shiny::verbatimTextOutput("modal_mwe_output"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
    })
  })

  output$dictionary_plot <- plotly::renderPlotly({
    req(input$dictionary)
    req(dfm_dictionary())

    freq_all <- quanteda.textstats::textstat_frequency(dfm_dictionary())

    selected_ngrams <- input$multi_word_expressions
    selected_ngrams_normalized <- tolower(gsub(" ", "_", selected_ngrams))

    top_20_original <- if (!is.null(top_20_preselected())) {
      tolower(gsub(" ", "_", top_20_preselected()))
    } else {
      character(0)
    }

    freq_mwe <- freq_all %>%
      dplyr::filter(tolower(feature) %in% selected_ngrams_normalized) %>%
      dplyr::mutate(
        source = ifelse(tolower(feature) %in% top_20_original, "Top 20", "Manual")
      ) %>%
      dplyr::arrange(desc(frequency))

    TextAnalysisR::plot_mwe_frequency(
      mwe_data = freq_mwe,
      title = "Multi-Word Expression Frequency After Compounding",
      color_by_source = TRUE,
      primary_color = "#10B981",
      secondary_color = "#A855F7"
    )
  })

  output$selected_ngrams_plot_uiOutput <- renderUI({
    div(
      style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotly::plotlyOutput("selected_ngrams_plot", height = 500, width = "100%")
    )
  })

  output$selected_ngrams_plot <- plotly::renderPlotly({
    req(input$dictionary)
    req(input$multi_word_expressions)

    if (!is.null(compound_stats())) {
      selected_ngrams <- compound_stats() %>%
        dplyr::rename(
          collocation = feature,
          count = frequency
        ) %>%
        dplyr::arrange(dplyr::desc(count))
    } else {
      req(ngram_stats())
      tstat <- ngram_stats()

      selected_ngrams <- tstat %>%
        dplyr::filter(collocation %in% input$multi_word_expressions) %>%
        dplyr::mutate(
          collocation = gsub(" ", "_", collocation)
        ) %>%
        dplyr::arrange(dplyr::desc(count))
    }

    TextAnalysisR::plot_ngram_frequency(
      ngram_data = selected_ngrams,
      title = "Multi-Word Frequency",
      highlight_color = "#10B981",
      default_color = "#10B981",
      show_stats = FALSE
    )
  })

  output$dictionary_plot_uiOutput <- renderUI({
    div(
      style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotly::plotlyOutput("dictionary_plot", height = 500, width = "100%")
    )
  })

  output$dictionary_table <- DT::renderDataTable({
    req(input$dictionary)
    req(input$multi_word_expressions)

    if (!is.null(compound_stats())) {
      compound_stats() %>%
        dplyr::arrange(desc(frequency)) %>%
        dplyr::select(
          `Multi-Word` = feature,
          Size,
          Frequency = frequency
        )
    } else {
      req(ngram_stats())
      tstat <- ngram_stats()

      tstat %>%
        dplyr::filter(collocation %in% input$multi_word_expressions) %>%
        dplyr::mutate(
          collocation = gsub(" ", "_", collocation),
          Size = stringr::str_count(collocation, "_") + 1
        ) %>%
        dplyr::arrange(desc(count)) %>%
        dplyr::select(
          `Multi-Word` = collocation,
          Size,
          Frequency = count
        )
    }
  },
  rownames = FALSE,
  extensions = 'Buttons',
  filter = 'top',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 25,
    order = list(list(2, 'desc')),
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))



  dfm_init <- eventReactive(input$dfm_btn, {
    tokens_to_use <- if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      toks
    } else {
      stop("No tokens available. Please complete Step 1 (Unite Text) first.")
    }

    dfm_tokens(tokens_to_use)

    dfm_object <- tokens_to_use %>% quanteda::dfm()

    doc_count     <- quanteda::ndoc(dfm_object)
    feature_count <- quanteda::nfeat(dfm_object)

    token_count <- length(unique(unlist(as.list(tokens_to_use))))

    avg_doc_length <- mean(quanteda::ntoken(tokens_to_use))
    total_tokens <- sum(quanteda::ntoken(tokens_to_use))

    source_info <- if (!is.null(processed_tokens())) {
      "Tokens from: Step 4 (Multi-Word Dictionary)"
    } else if (!is.null(final_tokens())) {
      "Tokens from: Step 3 (Remove Stopwords)"
    } else if (!is.null(preprocessed_combined())) {
      "Tokens from: Step 2 (Segment Texts)"
    } else {
      "Tokens from: Step 1 (Unite Text) - Basic tokenization"
    }

    note_lines <- c(
      "",
      source_info,
      "",
      "DFM Processing Applied:",
      "  - Converted to lowercase (case normalization)",
      "  - Empty tokens removed",
      "  - Duplicate features after lowercasing merged"
    )

    if (token_count > feature_count) {
      note_lines <- c(
        note_lines,
        "",
        paste0("Note: ", format(token_count - feature_count, big.mark = ","),
               " unique tokens were normalized/merged into ",
               format(feature_count, big.mark = ","), " features")
      )
    }

    basic_metrics <- c(
      "",
      "DFM Summary:",
      paste0("  - Total tokens: ", format(total_tokens, big.mark = ",")),
      paste0("  - Average document length: ", round(avg_doc_length, 1), " tokens"),
      "",
      "Note: For readability and lexical diversity metrics (TTR, Flesch-Kincaid, etc.),",
      "   use the 'Lexical Analysis' tab → '4. Readability'"
    )

    combined_output <- c(
      paste("Documents:", doc_count),
      paste("Features:", format(feature_count, big.mark = ",")),
      note_lines,
      basic_metrics
    )

    output$modal_dfm_verbose_output <- renderPrint({
      cat(paste(combined_output, collapse = "\n"))
    })

    Sys.sleep(0.1)

    shiny::showModal(
      shiny::modalDialog(
        title = "DFM Construction Complete",
        shiny::verbatimTextOutput("modal_dfm_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )

    step_5_based_on(step_4_version())

    dfm_object
  })

  output$has_dfm_results <- reactive({
    if (is.null(input$dfm_btn) || input$dfm_btn == 0) {
      return(FALSE)
    }

    dfm_obj <- tryCatch({
      dfm_init()
    }, error = function(e) {
      NULL
    })

    return(!is.null(dfm_obj))
  })
  outputOptions(output, "has_dfm_results", suspendWhenHidden = FALSE)

  output$dfm_plot <- plotly::renderPlotly({
    req(dfm_init())
    dfm_init() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$dfm_table <- DT::renderDataTable({
    req(dfm_init())
    quanteda.textstats::textstat_frequency(dfm_init())
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "100%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
  )

  output$download_preprocessing_report <- downloadHandler(
    filename = function() {
      paste0("preprocessing_report_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      tryCatch({
        if (is.null(united_tbl())) {
          error_msg <- c(
            "================================================================================",
            "PREPROCESSING REPORT NOT AVAILABLE",
            "================================================================================",
            "",
            "No preprocessing data available to generate report.",
            "",
            "Required Steps:",
            "  1. Complete Step 1: Unite Texts",
            "  2. Complete Step 5: Document-Feature Matrix (DFM)",
            "",
            "Optional Steps (can be skipped):",
            "  - Step 2: Segment Texts",
            "  - Step 3: Remove Stopwords",
            "  - Step 4: Multi-Word Dictionary",
            "",
            paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            "================================================================================"
          )
          writeLines(paste(error_msg, collapse = "\n"), file)
          return()
        }

        report_lines <- c(
          "================================================================================",
          "TEXT PREPROCESSING REPORT",
          "================================================================================",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("TextAnalysisR Version: 0.0.3"),
          "",
          "================================================================================",
          "STEP 1: TEXT UNIFICATION",
          "================================================================================",
          ""
        )

      if (!is.null(united_tbl())) {
        text_cols <- if (!is.null(input$show_vars) && length(input$show_vars) > 0) {
          paste(input$show_vars, collapse = ", ")
        } else {
          "Not specified"
        }
        report_lines <- c(report_lines,
          paste("Documents processed:", nrow(united_tbl())),
          paste("Text columns combined:", text_cols),
          ""
        )
      }

      report_lines <- c(report_lines,
        "================================================================================",
        "STEP 2: TOKENIZATION",
        "================================================================================",
        ""
      )

      if (!is.null(input$preprocessing_preset) && input$preprocessing_preset != "custom") {
        report_lines <- c(report_lines,
          paste("Preset used:", switch(input$preprocessing_preset,
            "topic" = "Topic Modeling (Standard)",
            "sentiment" = "Sentiment Analysis",
            "keywords" = "Keyword Extraction",
            "ner" = "Named Entity Recognition",
            "similarity" = "Document Similarity",
            "custom" = "Custom (Manual Setup)")),
          ""
        )
      }

      segment_opts <- input$segment_options %||% character(0)
      report_lines <- c(report_lines,
        paste("Status:", if (!is.null(preprocessed_init()) || !is.null(preprocessed_skip())) "Applied" else "Skipped"),
        paste("Tokenization type:", if (!is.null(preprocessed_init())) "Advanced" else if (!is.null(preprocessed_skip())) "Basic" else "Not performed"),
        "",
        "Options selected:",
        paste("  - Convert to lowercase:", if ("lowercase" %in% segment_opts) "Yes" else "No"),
        paste("  - Remove punctuation:", if ("remove_punct" %in% segment_opts) "Yes" else "No"),
        paste("  - Remove symbols:", if ("remove_symbols" %in% segment_opts) "Yes" else "No"),
        paste("  - Remove numbers:", if ("remove_numbers" %in% segment_opts) "Yes" else "No"),
        paste("  - Remove URLs:", if ("remove_url" %in% segment_opts) "Yes" else "No"),
        paste("  - Remove separators:", if ("remove_separators" %in% segment_opts) "Yes" else "No"),
        paste("  - Split hyphens:", if ("split_hyphens" %in% segment_opts) "Yes" else "No"),
        paste("  - Split tags:", if ("split_tags" %in% segment_opts) "Yes" else "No"),
        paste("  - Include document variables:", if ("include_docvars" %in% segment_opts) "Yes" else "No"),
        paste("  - Keep acronyms:", if ("keep_acronyms" %in% segment_opts) "Yes" else "No"),
        paste("  - Keep padding:", if ("padding" %in% segment_opts) "Yes" else "No"),
        paste("  - Minimum characters per token:", input$min_char %||% 2),
        paste("  - Math Mode:", if (isTRUE(input$math_mode)) "Enabled" else "Disabled"),
        ""
      )

      report_lines <- c(report_lines,
        "================================================================================",
        "STEP 3: STOPWORD REMOVAL",
        "================================================================================",
        ""
      )

      if (!is.null(input$remove) && input$remove > 0) {
        common_count <- length(input$common_words %||% character(0))
        custom_count <- length(input$custom_stopwords %||% character(0))

        report_lines <- c(report_lines,
          "Status: Applied",
          paste("  - Common words removed:", common_count),
          paste("  - Predefined stopwords removed:", custom_count),
          ""
        )
      } else if (!is.null(input$skip_stopwords) && input$skip_stopwords > 0) {
        report_lines <- c(report_lines, "Status: Skipped", "")
      } else {
        report_lines <- c(report_lines, "Status: Not yet performed", "")
      }

      report_lines <- c(report_lines,
        "================================================================================",
        "STEP 4: MULTI-WORD EXPRESSIONS",
        "================================================================================",
        ""
      )

      if (!is.null(input$detect_ngrams) && input$detect_ngrams > 0) {
        report_lines <- c(report_lines,
          "N-gram detection: Applied",
          paste("  - N-gram sizes:", paste(input$ngram_sizes %||% character(0), collapse = ", ")),
          paste("  - Minimum frequency:", input$ngram_min_count %||% 3),
          paste("  - Minimum lambda:", input$ngram_min_lambda %||% 3),
          ""
        )
      } else {
        report_lines <- c(report_lines, "N-gram detection: Skipped", "")
      }

      if (!is.null(input$dictionary) && input$dictionary > 0) {
        mwe_count <- length(input$multi_word_expressions %||% character(0))
        stopword_opts <- input$stopword_options %||% character(0)
        report_lines <- c(report_lines,
          "N-gram compounding: Applied",
          paste("  - Multi-word expressions compounded:", mwe_count),
          paste("  - Remove leading stopwords from MWE:", if ("leading_stopwords" %in% stopword_opts) "Yes" else "No"),
          paste("  - Remove trailing stopwords from MWE:", if ("trailing_stopwords" %in% stopword_opts) "Yes" else "No"),
          ""
        )
      } else {
        report_lines <- c(report_lines, "N-gram compounding: Skipped", "")
      }

      report_lines <- c(report_lines,
        "================================================================================",
        "STEP 5: DOCUMENT-FEATURE MATRIX",
        "================================================================================",
        ""
      )

      if (!is.null(dfm_init())) {
        dfm_obj <- dfm_init()
        report_lines <- c(report_lines,
          "Status: Created",
          paste("  - Documents:", quanteda::ndoc(dfm_obj)),
          paste("  - Features (unique tokens):", quanteda::nfeat(dfm_obj)),
          paste("  - Sparsity:", paste0(round(quanteda::sparsity(dfm_obj) * 100, 2), "%")),
          paste("  - Total tokens:", sum(quanteda::ntoken(dfm_obj))),
          paste("  - Average tokens per document:", round(mean(quanteda::ntoken(dfm_obj)), 2)),
          ""
        )
      } else {
        report_lines <- c(report_lines, "Status: Not yet created", "")
      }

      report_lines <- c(report_lines,
        "================================================================================",
        "FINAL STATISTICS",
        "================================================================================",
        ""
      )

      final_dfm <- tryCatch({
        if (!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip", "remove", "skip_stopwords"))) {
          if (!is.null(last_clicked()) && last_clicked() == "lemma") {
            dfm_lemma()
          } else if (!is.null(last_clicked()) && last_clicked() == "remove") {
            dfm_outcome()
          } else if (!is.null(final_tokens())) {
            quanteda::dfm(final_tokens())
          } else {
            NULL
          }
        } else if (!is.null(dfm_init())) {
          dfm_init()
        } else {
          NULL
        }
      }, error = function(e) NULL)

      if (!is.null(final_dfm)) {
        report_lines <- c(report_lines,
          paste("Final documents:", quanteda::ndoc(final_dfm)),
          paste("Final unique tokens:", quanteda::nfeat(final_dfm)),
          paste("Final total tokens:", sum(quanteda::ntoken(final_dfm))),
          paste("Final average tokens/document:", round(mean(quanteda::ntoken(final_dfm)), 2)),
          paste("Final sparsity:", paste0(round(quanteda::sparsity(final_dfm) * 100, 2), "%")),
          ""
        )
      }

      report_lines <- c(report_lines,
        "================================================================================",
        "SOFTWARE INFORMATION",
        "================================================================================",
        "",
        paste("TextAnalysisR:", packageVersion("TextAnalysisR")),
        paste("quanteda:", packageVersion("quanteda")),
        paste("R version:", R.version.string),
        paste("Platform:", R.version$platform),
        "",
        "================================================================================",
        "CITATION",
        "================================================================================",
        "",
        "If you use this preprocessing workflow in your research, please cite:",
        "",
        "Shin, M. (2025). TextAnalysisR: Text mining workflow tool (Version 0.0.3)",
        "  [R package]. https://github.com/mshin77/TextAnalysisR",
        "",
        "================================================================================",
        "END OF REPORT",
        "================================================================================"
      )

        writeLines(report_lines, file)
      }, error = function(e) {
        error_msg <- c(
          "================================================================================",
          "ERROR GENERATING PREPROCESSING REPORT",
          "================================================================================",
          "",
          paste("Error:", e$message),
          "",
          "Please ensure that:",
          "  - Text unification (Step 1) has been completed",
          "  - At least some preprocessing steps have been performed",
          "",
          "If you continue to experience issues, please:",
          "  1. Complete Step 1 (Unite Texts)",
          "  2. Complete at least Step 2 (Segment Texts)",
          "  3. Try generating the report again",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "================================================================================"
        )
        writeLines(paste(error_msg, collapse = "\n"), file)
      })
    }
  )

  # Step 5: Remove common words and stopwords.

  tstat_freq_dfm_init <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_init())
    tstat_freq_n_100 <- head(tstat_freq, 100)
    tstat_freq_n_100$feature
  })

  observe({
    updateSelectizeInput(
      session,
      "common_words",
      choices  = tstat_freq_dfm_init(),
      selected = head(tstat_freq_dfm_init(), 20),
      options  = list(create = TRUE),
      server = TRUE
    )
  })

  observeEvent(input$remove, {
    toks_source <- if (!is.null(processed_tokens())) {
      showNotification("Using multi-word compounded tokens", type = "message", duration = 3)
      processed_tokens()
    } else if (!is.null(preprocessed_combined()) &&
               (!is.null(try(preprocessed_init(), silent = TRUE)) || !is.null(preprocessed_skip()))) {
      preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      showNotification("Creating tokens from united text...", type = "message", duration = 2)
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      toks
    } else {
      NULL
    }

    if (is.null(toks_source)) {
      showNotification("Please complete Step 1 (Unite Text) first", type = "warning", duration = 5)
      return(NULL)
    }

    lemmatized_tokens(NULL)

    toks <- toks_source

    if (!is.null(input$common_words) && length(input$common_words) > 0) {
      toks <- quanteda::tokens_remove(toks, pattern = input$common_words, verbose = FALSE)
    }

    if (!is.null(input$custom_stopwords) && length(input$custom_stopwords) > 0) {
      toks <- quanteda::tokens_remove(toks, pattern = input$custom_stopwords, verbose = FALSE)
    }

    final_tokens(toks)
    stopwords_applied(TRUE)
    step_3_version(step_3_version() + 1)
    step_3_based_on(step_2_version())
    last_clicked("remove")
  })

  dfm_outcome <- reactive({
    req(input$remove > 0 | input$skip_stopwords > 0)

    tokens_obj <- if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else {
      NULL
    }

    req(tokens_obj)
    dfm_obj <- quanteda::dfm(tokens_obj)

    if (!is.null(quanteda::docvars(dfm_init()))) {
      quanteda::docvars(dfm_obj) <- quanteda::docvars(dfm_init())
    }

    doc_count <- quanteda::ndoc(dfm_obj)
    feature_count_before <- quanteda::nfeat(dfm_init())
    feature_count_after <- quanteda::nfeat(dfm_obj)
    features_removed <- feature_count_before - feature_count_after
    percent_reduction <- round((features_removed / feature_count_before) * 100, 1)

    combined_output <- c(
      paste("Documents:", doc_count),
      paste("Features before:", format(feature_count_before, big.mark = ",")),
      paste("Features after:", format(feature_count_after, big.mark = ",")),
      paste("Reduction:", format(features_removed, big.mark = ","), "features removed", paste0("(", percent_reduction, "%)"))
    )

    output$modal_dfm_verbose_output_stopwords <- renderPrint({
      cat(paste(combined_output, collapse = "\n"))
    })

    shiny::showModal(
      shiny::modalDialog(
        title = "Stopwords Removal Complete",
        shiny::verbatimTextOutput("modal_dfm_verbose_output_stopwords"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )

    dfm_obj
  })

  output$has_stopword_results <- reactive({
    stopwords_applied()
  })
  outputOptions(output, "has_stopword_results", suspendWhenHidden = FALSE)

  output$stopword_plot <- plotly::renderPlotly({
    tokens_for_plot <- if (!is.null(input$remove) && input$remove > 0) {
      req(final_tokens())
      final_tokens()
    } else {
      req(preprocessed_combined())
      preprocessed_combined()
    }

    dfm_for_plot <- quanteda::dfm(tokens_for_plot)

    dfm_for_plot %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$stopword_table <- DT::renderDataTable({
    tokens_for_table <- if (!is.null(input$remove) && input$remove > 0) {
      req(final_tokens())
      final_tokens()
    } else {
      req(preprocessed_combined())
      preprocessed_combined()
    }

    dfm_for_table <- quanteda::dfm(tokens_for_table)

    quanteda.textstats::textstat_frequency(dfm_for_table)
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "100%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

  spacyr_processed <- reactiveVal(NULL)
  lemma_cache_key <- reactiveVal(NULL)

  observeEvent(input$lemma, {
    tokens_to_use <- NULL

    if (!is.null(final_tokens())) {
      tokens_to_use <- final_tokens()
    } else if (!is.null(processed_tokens())) {
      tokens_to_use <- processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      tokens_to_use <- preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      showNotification("Creating tokens from united text...", type = "message", duration = 2)
      toks <- quanteda::tokens(united_tbl()$united_texts, what = "word")
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      tokens_to_use <- toks
      final_tokens(toks)
    }

    req(tokens_to_use)

    # Create cache key based on token content
    current_cache_key <- digest::digest(as.list(tokens_to_use), algo = "md5")

    # Check if we already have cached results for these tokens
    if (!is.null(lemma_cache_key()) && !is.null(lemmatized_tokens()) &&
        lemma_cache_key() == current_cache_key) {
      showNotification("Using cached lemmatization results", type = "message", duration = 2)
      lemma_applied(TRUE)
      last_clicked("lemma")
      return(NULL)
    }

    show_loading_notification("Running spaCy linguistic analysis...", id = "loadingLemma")
    shinybusy::show_spinner()

    tryCatch({
      spacy_status <- tryCatch({
        spacyr::spacy_initialized()
      }, error = function(e) {
        FALSE
      })

      if (!spacy_status) {
        tryCatch({
          suppressMessages(spacyr::spacy_initialize(model = "en_core_web_sm"))
        }, error = function(e) {
          error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
            e$message
          } else {
            as.character(e)
          }
          try(removeNotification("loadingLemma"), silent = TRUE)
          shinybusy::hide_spinner()
          showModal(modalDialog(
            title = "spaCy Initialization Failed",
            tags$p("Could not initialize spaCy. Please ensure:"),
            tags$ul(
              tags$li("Python is installed"),
              tags$li("spaCy is installed in Python"),
              tags$li("The 'en_core_web_sm' model is downloaded")
            ),
            tags$p("Run in R console:"),
            tags$pre("spacyr::spacy_install()"),
            tags$p(tags$strong("Error details:")),
            tags$pre(style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px;", error_msg),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          lemmatized_tokens(tokens_to_use)
          lemma_applied(TRUE)
          last_clicked("lemma")
          return(NULL)
        })
      }

      texts <- sapply(tokens_to_use, function(x) paste(x, collapse = " "))

      # Only extract lemma for performance - other features extracted separately when needed
      parsed <- spacyr::spacy_parse(
        texts,
        pos = FALSE,
        tag = FALSE,
        lemma = TRUE,
        entity = FALSE,
        dependency = FALSE
      )

      spacyr_processed(parsed)

      lemma_list <- split(parsed$lemma, parsed$doc_id)
      toks_lemma <- quanteda::as.tokens(lemma_list)

      original_docvars <- quanteda::docvars(tokens_to_use)
      if (!is.null(original_docvars) && nrow(original_docvars) > 0) {
        original_docnames <- quanteda::docnames(tokens_to_use)
        lemma_docnames <- quanteda::docnames(toks_lemma)

        if (quanteda::ndoc(toks_lemma) == nrow(original_docvars)) {
          quanteda::docvars(toks_lemma) <- original_docvars
        } else {
          dropped_docs <- setdiff(original_docnames, lemma_docnames)
          if (length(dropped_docs) > 0) {
            matched_indices <- match(lemma_docnames, original_docnames)
            matched_docvars <- original_docvars[matched_indices, , drop = FALSE]
            quanteda::docvars(toks_lemma) <- matched_docvars

            showNotification(
              paste0("Note: ", length(dropped_docs), " document(s) were dropped during lemmatization (likely empty after processing). ",
                     "Original: ", nrow(original_docvars), ", Remaining: ", quanteda::ndoc(toks_lemma)),
              type = "message",
              duration = 8,
              closeButton = TRUE
            )
          }
        }
      }

      lemmatized_tokens(toks_lemma)
      lemma_cache_key(current_cache_key)  # Cache the result
      lemma_applied(TRUE)
      last_clicked("lemma")

      try(removeNotification("loadingLemma"), silent = TRUE)
      shinybusy::hide_spinner()
      show_completion_notification("Linguistic analysis complete", duration = 3)

    }, error = function(e) {
      try(removeNotification("loadingLemma"), silent = TRUE)
      shinybusy::hide_spinner()
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        e$message
      } else {
        as.character(e)
      }
      showNotification(
        paste("Error in spaCy processing:", error_msg),
        type = "error",
        duration = NULL,
        closeButton = TRUE
      )
      lemmatized_tokens(tokens_to_use)
      lemma_applied(TRUE)
      last_clicked("lemma")
    })
  })

  final_tokens_or_lemma <- reactive({
    if (!is.null(last_clicked())) {
      if (last_clicked() == "lemma") {
        return(lemmatized_tokens())
      } else if (last_clicked() == "skip") {
        return(final_tokens())
      }
    }
    return(NULL)
  })

  observeEvent(input$skip, {
    tokens_to_use <- NULL

    if (!is.null(final_tokens())) {
      tokens_to_use <- final_tokens()
    } else if (!is.null(processed_tokens())) {
      tokens_to_use <- processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      tokens_to_use <- preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      toks <- quanteda::tokens(united_tbl()$united_texts, what = "word")
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      tokens_to_use <- toks
      final_tokens(toks)
    }

    req(tokens_to_use)
    lemmatized_tokens(tokens_to_use)
    lemma_applied(TRUE)
    last_clicked("skip")
  })

  observeEvent(input$skip_stopwords, {
    toks_source <- if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else if (!is.null(united_tbl())) {
      toks <- quanteda::tokens(
        united_tbl()$united_texts,
        what = "word"
      )
      other_cols <- united_tbl() %>% dplyr::select(-united_texts)
      if (ncol(other_cols) > 0) {
        quanteda::docvars(toks) <- other_cols
      }
      toks
    } else {
      NULL
    }

    if (is.null(toks_source)) {
      showNotification("Please complete Step 1 (Unite Text) first", type = "warning", duration = 5)
      return(NULL)
    }

    lemmatized_tokens(NULL)

    final_tokens(toks_source)
    stopwords_applied(TRUE)
    step_3_version(step_3_version() + 1)
    step_3_based_on(step_2_version())
    last_clicked("skip_stopwords")
  })

  observeEvent(input$reset_pipeline, {
    showModal(modalDialog(
      title = tags$div(
        style = "color: #f59e0b;",
        icon("exclamation-triangle"),
        " Reset All Preprocessing?"
      ),
      tags$p("This will clear all preprocessing steps (Steps 2-5) but keep your united text (Step 1).", style = "font-size: 16px;"),
      tags$p("You'll need to re-run any preprocessing steps you want to apply.", style = "font-size: 16px; margin-top: 10px;"),
      tags$p(tags$strong("This cannot be undone."), style = "font-size: 16px; color: #dc3545; margin-top: 10px;"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset_pipeline", "Reset All", class = "btn-warning")
      )
    ))
  })

  observeEvent(input$confirm_reset_pipeline, {
    preprocessed_skip(NULL)
    processed_tokens(NULL)
    final_tokens(NULL)
    lemmatized_tokens(NULL)
    dfm_tokens(NULL)
    stopwords_applied(FALSE)
    dictionary_applied(FALSE)
    lemma_applied(FALSE)
    last_clicked(NULL)
    math_mode_used(FALSE)
    compound_stats(NULL)

    step_2_version(0)
    step_3_version(0)
    step_4_version(0)
    step_3_based_on(0)
    step_4_based_on(0)
    step_5_based_on(0)

    removeModal()

    showNotification(
      "All preprocessing reset. Your united text is preserved. Start from Step 2 (Segment Texts) or Step 3 (Remove Stopwords).",
      type = "warning",
      duration = 8,
      closeButton = TRUE
    )
  })

  dfm_lemma <- reactive({
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(final_tokens_or_lemma())

    dfm_obj <- quanteda::dfm(final_tokens_or_lemma())

    tryCatch({
      tokens_source <- final_tokens_or_lemma()
      if (!is.null(quanteda::docvars(tokens_source)) && nrow(quanteda::docvars(tokens_source)) > 0) {
        quanteda::docvars(dfm_obj) <- quanteda::docvars(tokens_source)
      } else {
        source_dfm <- NULL

        if (!is.null(dfm_outcome()) && inherits(dfm_outcome(), "dfm")) {
          source_dfm <- dfm_outcome()
        } else if (!is.null(dfm_init()) && inherits(dfm_init(), "dfm")) {
          source_dfm <- dfm_init()
        }

        if (!is.null(source_dfm)) {
          source_docvars <- quanteda::docvars(source_dfm)
          if (!is.null(source_docvars) && nrow(source_docvars) > 0) {
            source_docnames <- quanteda::docnames(source_dfm)
            lemma_docnames <- quanteda::docnames(dfm_obj)

            if (quanteda::ndoc(dfm_obj) == nrow(source_docvars)) {
              quanteda::docvars(dfm_obj) <- source_docvars
            } else {
              matched_indices <- match(lemma_docnames, source_docnames)
              if (any(is.na(matched_indices))) {
                warning("Some documents could not be matched when transferring variables")
              }
              matched_docvars <- source_docvars[matched_indices, , drop = FALSE]
              quanteda::docvars(dfm_obj) <- matched_docvars
            }
          }
        }
      }
    }, error = function(e) {
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        e$message
      } else {
        as.character(e)
      }
      message("Could not transfer document variables to lemma DFM: ", error_msg)
    })

    dfm_obj
  })

  observeEvent(last_clicked(), {
    req(last_clicked() %in% c("lemma", "skip"))
    req(dfm_lemma())

    dfm_obj <- dfm_lemma()

    doc_count <- quanteda::ndoc(dfm_obj)

    if (last_clicked() == "lemma") {
      dfm_before <- tryCatch(dfm_outcome(), error = function(e) {
        tryCatch(dfm_init(), error = function(e) NULL)
      })

      if (!is.null(dfm_before)) {
        feature_count_before <- quanteda::nfeat(dfm_before)
        feature_count_after <- quanteda::nfeat(dfm_obj)
        features_reduced <- feature_count_before - feature_count_after
        percent_reduction <- round((features_reduced / feature_count_before) * 100, 1)

        combined_output <- c(
          paste("Documents:", doc_count),
          paste("Features before:", format(feature_count_before, big.mark = ",")),
          paste("Features after:", format(feature_count_after, big.mark = ",")),
          paste("Reduction:", format(features_reduced, big.mark = ","), "features removed", paste0("(", percent_reduction, "%)"))
        )
      } else {
        feature_count <- quanteda::nfeat(dfm_obj)
        combined_output <- c(
          paste("Documents:", doc_count),
          paste("Features:", format(feature_count, big.mark = ","))
        )
      }
      action_type <- "Lemmatization Complete"
    } else {
      feature_count <- quanteda::nfeat(dfm_obj)
      combined_output <- c(
        paste("Documents:", doc_count),
        paste("Features:", format(feature_count, big.mark = ","))
      )
      action_type <- "Lemmatization Skipped"
    }

    output$modal_lemma_verbose_output <- renderPrint({
      cat(paste(combined_output, collapse = "\n"))
    })

    shiny::showModal(
      shiny::modalDialog(
        title = action_type,
        shiny::verbatimTextOutput("modal_lemma_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
  })

  output$lemma_plot <- plotly::renderPlotly({
    req(dfm_lemma())
    dfm_lemma() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$lemma_table <- DT::renderDataTable({
    req(dfm_lemma())
    quanteda.textstats::textstat_frequency(dfm_lemma())
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "100%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

  dfm_final <- reactive({
    if (!is.null(last_clicked())) {
      if (last_clicked() %in% c("lemma", "skip")) {
        return(dfm_lemma())
      } else if (last_clicked() == "remove") {
        return(dfm_outcome())
      } else if (last_clicked() == "skip_stopwords") {
        tokens_obj <- if (!is.null(final_tokens())) {
          final_tokens()
        } else if (!is.null(processed_tokens())) {
          processed_tokens()
        } else if (!is.null(preprocessed_combined())) {
          preprocessed_combined()
        } else {
          NULL
        }

        req(tokens_obj)
        dfm_obj <- quanteda::dfm(tokens_obj)
        if (!is.null(quanteda::docvars(dfm_init()))) {
          quanteda::docvars(dfm_obj) <- quanteda::docvars(dfm_init())
        }
        return(dfm_obj)
      }
    }
    if (!is.null(dfm_outcome())) {
      return(dfm_outcome())
    }
    if (!is.null(dfm_init())) {
      return(dfm_init())
    }
    return(NULL)
  })

  observe({
    req(spacyr_processed())

    parsed <- spacyr_processed()
    pos_choices <- unique(parsed$pos)

    updateSelectizeInput(
      session,
      "pos_filter",
      choices = pos_choices,
      selected = NULL
    )
  })

  observe({
    tokens_obj <- if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(lemmatized_tokens())) {
      lemmatized_tokens()
    } else if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else {
      NULL
    }

    req(tokens_obj)

    doc_vars <- quanteda::docvars(tokens_obj)
    if (!is.null(doc_vars)) {
      var_names <- names(doc_vars)
      updateSelectizeInput(
        session,
        "lemma_doc_id_var",
        choices = c("None" = "", var_names),
        selected = ""
      )
    }
  })

  observe({
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(spacyr_processed())

    parsed <- spacyr_processed()

    # Check if entity column exists (spacy_parse may be called with entity = FALSE)
    if (!"entity" %in% names(parsed)) {
      # No entity data available - only use custom entities if any
      if (nrow(custom_entities()) > 0) {
        entity_counts <- custom_entities() %>%
          dplyr::count(`Construct`, name = "n") %>%
          dplyr::rename(entity = `Construct`) %>%
          dplyr::arrange(dplyr::desc(n))

        ordered_entities <- entity_counts$entity

        updateSelectizeInput(
          session,
          "entity_type_filter",
          choices = c("All Types" = "", ordered_entities),
          selected = ""
        )
      }
      return()
    }

    entity_counts <- parsed %>%
      dplyr::filter(!is.na(entity), entity != "") %>%
      dplyr::mutate(entity_clean = gsub("_[BI]$", "", entity)) %>%
      dplyr::count(entity_clean, sort = TRUE, name = "n") %>%
      dplyr::rename(entity = entity_clean)

    if (nrow(custom_entities()) > 0) {
      custom_counts <- custom_entities() %>%
        dplyr::count(`Construct`, name = "n") %>%
        dplyr::rename(entity = `Construct`)

      entity_counts <- dplyr::bind_rows(entity_counts, custom_counts) %>%
        dplyr::group_by(entity) %>%
        dplyr::summarise(n = sum(n), .groups = "drop") %>%
        dplyr::arrange(desc(n))
    }

    ordered_entities <- entity_counts$entity

    updateSelectizeInput(
      session,
      "entity_type_filter",
      choices = c("All Types" = "", ordered_entities),
      selected = ""
    )
  })

  output$lemma_ready <- reactive({
    lemma_applied()
  })
  outputOptions(output, "lemma_ready", suspendWhenHidden = FALSE)

  output$pos_ready <- reactive({
    (!is.null(final_tokens()) || !is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")) && pos_applied() > 0
  })
  outputOptions(output, "pos_ready", suspendWhenHidden = FALSE)

  output$ner_ready <- reactive({
    (!is.null(final_tokens()) || !is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")) && !is.null(spacyr_processed())
  })
  outputOptions(output, "ner_ready", suspendWhenHidden = FALSE)

  output$dependencies_ready <- reactive({
    (!is.null(final_tokens()) || !is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")) && !is.null(spacyr_processed())
  })
  outputOptions(output, "dependencies_ready", suspendWhenHidden = FALSE)

  pos_applied <- reactiveVal(0)

  observeEvent(input$apply_pos, {
    tokens_to_use <- TextAnalysisR::get_available_tokens(
      final_tokens = final_tokens(),
      processed_tokens = processed_tokens(),
      preprocessed_tokens = preprocessed_combined(),
      united_tbl = united_tbl()
    )
    req(tokens_to_use)

    # Check if spacyr_processed already has pos and tag columns
    current_parsed <- spacyr_processed()
    if (!is.null(current_parsed) && all(c("pos", "tag") %in% names(current_parsed))) {
      pos_applied(pos_applied() + 1)
      return()
    }

    showNotification("Extracting POS tags...", type = "message", duration = 3)

    include_dep <- isTRUE(input$include_dependency)
    
    tryCatch({
      parsed <- TextAnalysisR::extract_pos_tags(
        tokens_to_use,
        include_dependency = include_dep
      )
      spacyr_processed(parsed)
      pos_applied(pos_applied() + 1)
      
      msg <- if (include_dep) "POS tagging with dependency parsing completed!" else "POS tagging completed!"
      showNotification(msg, type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })

  observeEvent(input$generate_pos_report, {
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(pos_applied() > 0)
    req(spacyr_processed())

    parsed <- spacyr_processed()

    # Check if pos and tag columns exist
    if (!all(c("pos", "tag") %in% names(parsed))) {
      showNotification("POS data not available. Click 'Apply' to extract POS tags.", type = "warning")
      return()
    }

    if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
      parsed <- parsed %>%
        dplyr::filter(pos %in% input$pos_filter)
    }

    pos_summary <- parsed %>%
      dplyr::count(pos, tag, sort = TRUE) %>%
      dplyr::rename(
        `POS (Universal)` = pos,
        `Tag (Detailed)` = tag,
        Frequency = n
      )

    total_tokens <- nrow(parsed)
    unique_pos <- length(unique(parsed$pos))
    top_pos <- pos_summary %>% dplyr::slice_head(n = 5)

    report_lines <- c(
      "===========================================",
      "PART-OF-SPEECH ANALYSIS REPORT",
      "===========================================",
      "",
      paste("Total Tokens Analyzed:", format(total_tokens, big.mark = ",")),
      paste("Unique POS Tags Found:", unique_pos),
      "",
      "Top 5 Most Frequent POS Tags:",
      "-------------------------------------------"
    )

    for (i in 1:min(5, nrow(top_pos))) {
      report_lines <- c(report_lines,
        sprintf("%d. %s (%s): %s tokens",
          i,
          top_pos$`POS (Universal)`[i],
          top_pos$`Tag (Detailed)`[i],
          format(top_pos$Frequency[i], big.mark = ",")
        )
      )
    }

    if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
      report_lines <- c(report_lines,
        "",
        "Active Filters:",
        paste("  - POS Tags:", paste(input$pos_filter, collapse = ", "))
      )
    }

    report_lines <- c(report_lines,
      "",
      "===========================================",
      paste("Generated:", Sys.time()),
      "==========================================="
    )

    output$modal_pos_report <- renderPrint({
      cat(paste(report_lines, collapse = "\n"))
    })

    showModal(
      modalDialog(
        title = "Part-of-Speech Analysis Report",
        verbatimTextOutput("modal_pos_report"),
        footer = tagList(
          downloadButton("download_pos_report", "Report"),
          modalButton("Close")
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
  })

  output$download_pos_report <- downloadHandler(
    filename = function() {
      paste0("POS_Analysis_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      tryCatch({
        req(spacyr_processed())

        parsed <- spacyr_processed()

        if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
          parsed <- parsed %>%
            dplyr::filter(pos %in% input$pos_filter)
        }

        pos_summary <- parsed %>%
          dplyr::count(pos, tag, sort = TRUE) %>%
          dplyr::rename(
            `POS (Universal)` = pos,
            `Tag (Detailed)` = tag,
            Frequency = n
          )

        total_tokens <- nrow(parsed)
        unique_pos <- length(unique(parsed$pos))
        top_pos <- pos_summary %>% dplyr::slice_head(n = 5)

        report_lines <- c(
          "===========================================",
          "PART-OF-SPEECH ANALYSIS REPORT",
          "===========================================",
          "",
          paste("Total Tokens Analyzed:", format(total_tokens, big.mark = ",")),
          paste("Unique POS Tags Found:", unique_pos),
          "",
          "Top 5 Most Frequent POS Tags:",
          "-------------------------------------------"
        )

        for (i in 1:min(5, nrow(top_pos))) {
          report_lines <- c(report_lines,
            sprintf("%d. %s (%s): %s tokens",
              i,
              top_pos$`POS (Universal)`[i],
              top_pos$`Tag (Detailed)`[i],
              format(top_pos$Frequency[i], big.mark = ",")
            )
          )
        }

        if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
          report_lines <- c(report_lines,
            "",
            "Active Filters:",
            paste("  - POS Tags:", paste(input$pos_filter, collapse = ", "))
          )
        }

        report_lines <- c(report_lines,
          "",
          "===========================================",
          paste("Generated:", Sys.time()),
          "===========================================",
          "",
          "",
          "DETAILED POS TAG FREQUENCY TABLE:",
          "==========================================="
        )

        writeLines(paste(report_lines, collapse = "\n"), file)
        write.table(pos_summary, file, append = TRUE, sep = "\t", row.names = FALSE, quote = FALSE)
      }, error = function(e) {
        error_msg <- c(
          "===========================================",
          "ERROR GENERATING POS REPORT",
          "===========================================",
          "",
          paste("Error:", e$message),
          "",
          "Please ensure that:",
          "  - POS tagging has been applied",
          "  - SpaCy processing is complete",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "==========================================="
        )
        writeLines(paste(error_msg, collapse = "\n"), file)
      })
    }
  )

  observeEvent(input$generate_dfm_report, {
    req(dfm_init())

    dfm_obj <- dfm_init()

    tokens_to_use <- if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else {
      NULL
    }

    doc_count <- quanteda::ndoc(dfm_obj)
    feature_count <- quanteda::nfeat(dfm_obj)
    total_tokens <- sum(quanteda::ntoken(dfm_obj))
    avg_doc_length <- mean(quanteda::ntoken(dfm_obj))
    sparsity <- quanteda::sparsity(dfm_obj)

    source_info <- if (!is.null(processed_tokens())) {
      "Step 4 (Multi-Word Dictionary)"
    } else if (!is.null(final_tokens())) {
      "Step 3 (Remove Stopwords)"
    } else if (!is.null(preprocessed_combined())) {
      "Step 2 (Segment Texts)"
    } else {
      "Step 1 (Unite Text)"
    }

    freq_table <- quanteda.textstats::textstat_frequency(dfm_obj)
    top_features <- head(freq_table, 20)

    report_lines <- c(
      "================================================================================",
      "DOCUMENT-FEATURE MATRIX (DFM) REPORT",
      "================================================================================",
      "",
      paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("TextAnalysisR Version:", packageVersion("TextAnalysisR")),
      "",
      "================================================================================",
      "DFM SUMMARY",
      "================================================================================",
      "",
      paste("Documents:", format(doc_count, big.mark = ",")),
      paste("Features (unique tokens):", format(feature_count, big.mark = ",")),
      paste("Total tokens:", format(total_tokens, big.mark = ",")),
      paste("Average tokens per document:", round(avg_doc_length, 2)),
      paste("Sparsity:", paste0(round(sparsity * 100, 2), "%")),
      "",
      "================================================================================",
      "SOURCE INFORMATION",
      "================================================================================",
      "",
      paste("Tokens source:", source_info),
      "",
      "DFM Processing Applied:",
      "  - Converted to lowercase (case normalization)",
      "  - Empty tokens removed",
      "  - Duplicate features after lowercasing merged",
      "",
      "================================================================================",
      "TOP 20 MOST FREQUENT FEATURES",
      "================================================================================",
      ""
    )

    for (i in 1:min(20, nrow(top_features))) {
      report_lines <- c(report_lines,
        sprintf("%2d. %-20s: %s (%.2f%%)",
          i,
          top_features$feature[i],
          format(top_features$frequency[i], big.mark = ","),
          (top_features$frequency[i] / total_tokens * 100)
        )
      )
    }

    report_lines <- c(report_lines,
      "",
      "================================================================================",
      paste("End of Report - Generated:", Sys.time()),
      "================================================================================"
    )

    output$modal_dfm_report <- renderPrint({
      cat(paste(report_lines, collapse = "\n"))
    })

    showModal(
      modalDialog(
        title = "Document-Feature Matrix Report",
        verbatimTextOutput("modal_dfm_report"),
        footer = tagList(
          downloadButton("download_dfm_report", "Download"),
          modalButton("Close")
        ),
        easyClose = TRUE,
        size = "l"
      )
    )
  })

  output$download_dfm_report <- downloadHandler(
    filename = function() {
      paste0("DFM_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
    },
    content = function(file) {
      tryCatch({
        req(dfm_init())

        dfm_obj <- dfm_init()

        doc_count <- quanteda::ndoc(dfm_obj)
        feature_count <- quanteda::nfeat(dfm_obj)
        total_tokens <- sum(quanteda::ntoken(dfm_obj))
        avg_doc_length <- mean(quanteda::ntoken(dfm_obj))
        sparsity <- quanteda::sparsity(dfm_obj)

        source_info <- if (!is.null(processed_tokens())) {
          "Step 4 (Multi-Word Dictionary)"
        } else if (!is.null(final_tokens())) {
          "Step 3 (Remove Stopwords)"
        } else if (!is.null(preprocessed_combined())) {
          "Step 2 (Segment Texts)"
        } else {
          "Step 1 (Unite Text)"
        }

        freq_table <- quanteda.textstats::textstat_frequency(dfm_obj)
        top_features <- head(freq_table, 20)

        report_lines <- c(
          "================================================================================",
          "DOCUMENT-FEATURE MATRIX (DFM) REPORT",
          "================================================================================",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          paste("TextAnalysisR Version:", packageVersion("TextAnalysisR")),
          "",
          "================================================================================",
          "DFM SUMMARY",
          "================================================================================",
          "",
          paste("Documents:", format(doc_count, big.mark = ",")),
          paste("Features (unique tokens):", format(feature_count, big.mark = ",")),
          paste("Total tokens:", format(total_tokens, big.mark = ",")),
          paste("Average tokens per document:", round(avg_doc_length, 2)),
          paste("Sparsity:", paste0(round(sparsity * 100, 2), "%")),
          "",
          "================================================================================",
          "SOURCE INFORMATION",
          "================================================================================",
          "",
          paste("Tokens source:", source_info),
          "",
          "DFM Processing Applied:",
          "  - Converted to lowercase (case normalization)",
          "  - Empty tokens removed",
          "  - Duplicate features after lowercasing merged",
          "",
          "================================================================================",
          "TOP 20 MOST FREQUENT FEATURES",
          "================================================================================",
          ""
        )

        for (i in 1:min(20, nrow(top_features))) {
          report_lines <- c(report_lines,
            sprintf("%2d. %-20s: %s (%.2f%%)",
              i,
              top_features$feature[i],
              format(top_features$frequency[i], big.mark = ","),
              (top_features$frequency[i] / total_tokens * 100)
            )
          )
        }

        report_lines <- c(report_lines,
          "",
          "================================================================================",
          "FULL FEATURE FREQUENCY TABLE",
          "================================================================================",
          ""
        )

        writeLines(paste(report_lines, collapse = "\n"), file)
        write.table(freq_table, file, append = TRUE, sep = "\t", row.names = FALSE, quote = FALSE)
      }, error = function(e) {
        error_msg <- c(
          "================================================================================",
          "ERROR GENERATING DFM REPORT",
          "================================================================================",
          "",
          paste("Error:", e$message),
          "",
          "Please ensure that:",
          "  - Document-Feature Matrix has been created",
          "  - Data processing is complete",
          "",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "================================================================================"
        )
        writeLines(paste(error_msg, collapse = "\n"), file)
      })
    }
  )

  output$pos_plot <- plotly::renderPlotly({
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(pos_applied() > 0)

    if (is.null(spacyr_processed())) {
      return(TextAnalysisR::create_empty_plot_message(
        "POS tags only available when 'Apply' is clicked (not 'Skip')"
      ))
    }

    parsed <- spacyr_processed()

    # Check if pos column exists
    if (!"pos" %in% names(parsed)) {
      return(TextAnalysisR::create_empty_plot_message(
        "POS data not available. Click 'Apply' to extract POS tags."
      ))
    }

    pos_data <- parsed

    if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
      pos_data <- pos_data %>%
        dplyr::filter(pos %in% input$pos_filter)
    }

    top_n <- if (!is.null(input$pos_top_n)) input$pos_top_n else 20

    pos_freq <- pos_data %>%
      dplyr::count(pos, sort = TRUE) %>%
      dplyr::slice_head(n = top_n)

    TextAnalysisR::plot_pos_frequencies(
      pos_data = pos_freq,
      top_n = top_n,
      title = "Part-of-Speech Tag Frequency"
    )
  })

  output$pos_plot_uiOutput <- renderUI({
    div(
      style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotly::plotlyOutput("pos_plot", height = 500, width = "100%")
    )
  })

  output$pos_table <- DT::renderDataTable({
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(pos_applied() > 0)
    req(spacyr_processed())

    parsed <- spacyr_processed()

    # Check if pos and tag columns exist
    req(all(c("pos", "tag") %in% names(parsed)))

    if (!is.null(input$pos_filter) && length(input$pos_filter) > 0) {
      parsed <- parsed %>%
        dplyr::filter(pos %in% input$pos_filter)
    }

    pos_summary <- parsed %>%
      dplyr::count(pos, tag, sort = TRUE) %>%
      dplyr::rename(
        `POS (Universal)` = pos,
        `Tag (Detailed)` = tag,
        Frequency = n
      )

    pos_summary
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

  # Extract entities when NER filter is applied
  observeEvent(input$apply_ner_filter, {
    tokens_to_use <- TextAnalysisR::get_available_tokens(
      final_tokens = final_tokens(),
      processed_tokens = processed_tokens(),
      preprocessed_tokens = preprocessed_combined(),
      united_tbl = united_tbl()
    )
    req(tokens_to_use)

    # Check if spacyr_processed already has entity column
    current_parsed <- spacyr_processed()
    if (!is.null(current_parsed) && "entity" %in% names(current_parsed)) {
      return()
    }

    showNotification("Extracting named entities...", type = "message", duration = 3)

    tryCatch({
      parsed <- TextAnalysisR::extract_named_entities(tokens_to_use)
      spacyr_processed(parsed)
      showNotification("Named entity extraction completed!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
    })
  })

  output$entity_plot <- plotly::renderPlotly({
    req(input$apply_ner_filter)
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))

    if (is.null(spacyr_processed())) {
      return(TextAnalysisR::create_empty_plot_message(
        "Named entities only available when 'Apply' is clicked (not 'Skip')"
      ))
    }

    custom_entities()

    parsed <- spacyr_processed()

    # Check if entity column exists
    if (!"entity" %in% names(parsed)) {
      # Use only custom entities if available
      if (nrow(custom_entities()) > 0) {
        entity_data <- custom_entities() %>%
          dplyr::count(`Construct`, name = "n") %>%
          dplyr::rename(entity = `Construct`)

        if (!is.null(input$entity_type_filter) && length(input$entity_type_filter) > 0 && input$entity_type_filter[1] != "") {
          entity_data <- entity_data %>%
            dplyr::filter(entity %in% input$entity_type_filter)
        }

        entity_data <- entity_data %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = 20)
      } else {
        return(TextAnalysisR::create_empty_plot_message(
          "No named entity data available. Entity extraction was not enabled during processing."
        ))
      }
    } else {
      entity_data <- parsed %>%
        dplyr::filter(!is.na(entity), entity != "") %>%
        dplyr::mutate(entity_clean = gsub("_[BI]$", "", entity))

      if (!is.null(input$entity_type_filter) && length(input$entity_type_filter) > 0 && input$entity_type_filter[1] != "") {
        entity_data <- entity_data %>%
          dplyr::filter(entity_clean %in% input$entity_type_filter)
      }

      entity_data <- entity_data %>%
        dplyr::count(entity_clean, sort = TRUE, name = "n") %>%
        dplyr::rename(entity = entity_clean) %>%
        dplyr::slice_head(n = 20)

      if (nrow(custom_entities()) > 0) {
        custom_counts <- custom_entities() %>%
          dplyr::count(`Construct`, name = "n") %>%
          dplyr::rename(entity = `Construct`)

        if (!is.null(input$entity_type_filter) && length(input$entity_type_filter) > 0 && input$entity_type_filter[1] != "") {
          custom_counts <- custom_counts %>%
            dplyr::filter(entity %in% input$entity_type_filter)
        }

        entity_data <- dplyr::bind_rows(entity_data, custom_counts) %>%
          dplyr::group_by(entity) %>%
          dplyr::summarise(n = sum(n), .groups = "drop") %>%
          dplyr::arrange(dplyr::desc(n)) %>%
          dplyr::slice_head(n = 20)
      }
    }

    TextAnalysisR::plot_entity_frequencies(
      entity_data = entity_data,
      title = "Named Entity Type Frequency"
    )
  })

  custom_entities <- reactiveVal(data.frame())

  output$entity_plot_uiOutput <- renderUI({
    div(
      style = "display: flex; justify-content: center; margin-bottom: 20px;",
      plotly::plotlyOutput("entity_plot", height = 500, width = "100%")
    )
  })

  output$entity_table <- DT::renderDataTable({
    req(input$apply_ner_filter)
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(spacyr_processed())

    custom_entities()

    parsed <- spacyr_processed()

    # Check if entity column exists
    if (!"entity" %in% names(parsed)) {
      return(NULL)
    }

    entity_detail <- parsed %>%
      dplyr::filter(!is.na(entity), entity != "") %>%
      dplyr::mutate(entity_clean = gsub("_[BI]$", "", entity)) %>%
      dplyr::group_by(doc_id, entity_clean, token) %>%
      dplyr::summarise(
        Frequency = dplyr::n(),
        First_Sentence = dplyr::first(sentence_id),
        Context = paste(unique(substring(token, 1, 100)), collapse = "; "),
        .groups = "drop"
      ) %>%
      dplyr::rename(entity = entity_clean)

    tokens_obj <- if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(lemmatized_tokens())) {
      lemmatized_tokens()
    } else if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else {
      NULL
    }

    req(tokens_obj)
    all_doc_ids <- quanteda::docnames(tokens_obj)
    doc_number_mapping <- data.frame(
      doc_id = all_doc_ids,
      Document = paste0("Doc ", seq_along(all_doc_ids)),
      stringsAsFactors = FALSE
    )

    entity_detail <- entity_detail %>%
      dplyr::left_join(doc_number_mapping, by = "doc_id")

    if (!is.null(input$lemma_doc_id_var) && input$lemma_doc_id_var != "") {
      doc_vars <- quanteda::docvars(tokens_obj)
      if (!is.null(doc_vars) && input$lemma_doc_id_var %in% names(doc_vars)) {
        doc_id_mapping <- data.frame(
          doc_id = all_doc_ids,
          `Document ID` = doc_vars[[input$lemma_doc_id_var]],
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        entity_detail <- entity_detail %>%
          dplyr::left_join(doc_id_mapping, by = "doc_id")
      }
    }

    if (!is.null(input$entity_type_filter) && length(input$entity_type_filter) > 0 && input$entity_type_filter[1] != "") {
      entity_detail <- entity_detail %>%
        dplyr::filter(entity %in% input$entity_type_filter)
    }

    entity_detail <- entity_detail %>%
      dplyr::arrange(entity, desc(Frequency)) %>%
      dplyr::mutate(
        Custom_Label = "",
        Notes = "",
        Keep = TRUE
      )

    if (!is.null(input$lemma_doc_id_var) && input$lemma_doc_id_var != "") {
      entity_detail <- entity_detail %>%
        dplyr::select(
          Document,
          `Document ID`,
          `Construct` = entity,
          `Operational Definition` = token,
          Frequency,
          `First Sentence` = First_Sentence,
          Notes,
          Keep
        )
    } else {
      entity_detail <- entity_detail %>%
        dplyr::select(
          Document,
          `Construct` = entity,
          `Operational Definition` = token,
          Frequency,
          `First Sentence` = First_Sentence,
          Notes,
          Keep
        )
    }

    if (nrow(custom_entities()) > 0) {
      custom_df <- custom_entities()

      if (!is.null(input$entity_type_filter) && length(input$entity_type_filter) > 0 && input$entity_type_filter[1] != "") {
        custom_df <- custom_df %>%
          dplyr::filter(`Construct` %in% input$entity_type_filter)
      }

      if (!is.null(input$lemma_doc_id_var) && input$lemma_doc_id_var != "") {
        custom_df <- custom_df %>%
          dplyr::mutate(`Document ID` = NA) %>%
          dplyr::select(Document, `Document ID`, `Construct`, `Operational Definition`, Frequency, `First Sentence`, Notes, Keep)
      } else {
        custom_df <- custom_df %>%
          dplyr::select(Document, `Construct`, `Operational Definition`, Frequency, `First Sentence`, Notes, Keep)
      }

      entity_detail <- dplyr::bind_rows(entity_detail, custom_df) %>%
        dplyr::arrange(`Construct`, desc(Frequency))
    }

    entity_detail
  },
  editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5))),
  rownames = FALSE,
  extensions = 'Buttons',
  filter = 'top',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
    pageLength = 25,
    columnDefs = list(
      list(targets = c(4, 5, 6), className = 'dt-center'),
      list(targets = 6, className = 'dt-center', render = DT::JS(
        "function(data, type, row, meta) {",
        "  return data ? '<span style=\"color: green;\">Yes</span>' : '<span style=\"color: red;\">No</span>';",
        "}"
      ))
    )
  ),
  selection = 'multiple'
  )

  observeEvent(input$add_custom_entity, {
    req(input$custom_entity_text)
    req(input$custom_construct_label)

    construct_label <- input$custom_construct_label
    search_terms <- strsplit(input$custom_entity_text, "\n")[[1]]
    search_terms <- trimws(search_terms)
    search_terms <- search_terms[search_terms != ""]

    use_regex <- isTRUE(input$use_regex)

    if (!is.null(spacyr_processed())) {
      parsed <- spacyr_processed()

      search_terms_lower <- tolower(search_terms)

      if (use_regex) {
        matching_tokens <- parsed %>%
          dplyr::mutate(token_lower = tolower(token)) %>%
          dplyr::filter(token_lower %in% search_terms_lower | lemma %in% search_terms_lower)
      } else {
        matching_tokens <- parsed %>%
          dplyr::mutate(token_lower = tolower(token)) %>%
          dplyr::filter(token_lower %in% search_terms_lower)
      }

      if (nrow(matching_tokens) > 0) {
        entity_summary <- matching_tokens %>%
          dplyr::group_by(doc_id) %>%
          dplyr::summarise(
            doc_frequency = dplyr::n(),
            first_sentence = dplyr::first(sentence_id),
            .groups = "drop"
          )

        tokens_obj <- if (!is.null(final_tokens())) {
          final_tokens()
        } else if (!is.null(lemmatized_tokens())) {
          lemmatized_tokens()
        } else if (!is.null(processed_tokens())) {
          processed_tokens()
        } else if (!is.null(preprocessed_combined())) {
          preprocessed_combined()
        } else {
          NULL
        }

        if (is.null(tokens_obj)) {
          showNotification("No tokens available. Please complete preprocessing first.", type = "error")
          return()
        }

        all_doc_ids <- quanteda::docnames(tokens_obj)
        doc_number_mapping <- data.frame(
          doc_id = all_doc_ids,
          Document = paste0("Doc ", seq_along(all_doc_ids)),
          stringsAsFactors = FALSE
        )

        entity_summary <- entity_summary %>%
          dplyr::left_join(doc_number_mapping, by = "doc_id")

        total_frequency <- sum(entity_summary$doc_frequency)
        doc_list <- paste(entity_summary$Document, collapse = ", ")

        operational_def <- paste(search_terms, collapse = ", ")

        new_entity <- data.frame(
          `Construct` = construct_label,
          `Operational Definition` = operational_def,
          Document = doc_list,
          Frequency = total_frequency,
          `First Sentence` = entity_summary$first_sentence[1],
          Notes = paste0("Found in ", nrow(entity_summary), " document(s)", if (use_regex) " (with lemmas)" else " (exact match)"),
          Keep = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        showNotification(
          paste0("Construct '", construct_label, "': Found ", total_frequency, " occurrence(s) across ", nrow(entity_summary), " document(s)"),
          type = "message",
          duration = 5
        )
      } else {
        operational_def <- paste(search_terms, collapse = ", ")

        new_entity <- data.frame(
          `Construct` = construct_label,
          `Operational Definition` = operational_def,
          Document = "Not found",
          Frequency = 0,
          `First Sentence` = NA,
          Notes = "Not found in documents",
          Keep = TRUE,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        showNotification(
          paste0("Construct '", construct_label, "' not found in documents"),
          type = "warning",
          duration = 5
        )
      }
    } else {
      operational_def <- paste(search_terms, collapse = ", ")

      new_entity <- data.frame(
        `Construct` = construct_label,
        `Operational Definition` = operational_def,
        Document = "Manual",
        Frequency = 0,
        `First Sentence` = NA,
        Notes = "Added before text processing",
        Keep = TRUE,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      showNotification(
        paste0("Construct '", construct_label, "' added (text not yet processed)"),
        type = "message",
        duration = 3
      )
    }

    current_custom <- custom_entities()

    if (nrow(current_custom) == 0) {
      custom_entities(new_entity)
    } else {
      existing_idx <- which(current_custom$Construct == construct_label)
      if (length(existing_idx) > 0) {
        current_custom[existing_idx, ] <- new_entity
        custom_entities(current_custom)
        showNotification(
          paste0("Updated existing construct: ", construct_label),
          type = "message",
          duration = 3
        )
      } else {
        custom_entities(dplyr::bind_rows(current_custom, new_entity))
      }
    }

    updateTextInput(session, "custom_construct_label", value = "")
    updateTextAreaInput(session, "custom_entity_text", value = "")
  })

  observeEvent(input$add_batch_entities, {
    req(input$batch_entity_text)

    sanitized_input <- TextAnalysisR:::sanitize_text_input(input$batch_entity_text)
    lines <- strsplit(sanitized_input, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]

    if (length(lines) == 0) {
      showNotification("No constructs entered. Please enter at least one construct.", type = "warning", duration = 3)
      return()
    }

    use_regex <- isTRUE(input$batch_use_regex)
    all_new_entities <- list()
    found_count <- 0
    not_found_count <- 0
    total_occurrences <- 0

    withProgress(message = 'Processing batch constructs...', value = 0, {
      for (i in seq_along(lines)) {
        parts <- strsplit(lines[i], "[:|\t]")[[1]]
        if (length(parts) != 2) {
          showNotification(
            paste0("Skipping line ", i, ": Invalid format. Use 'label: term1, term2, term3'"),
            type = "warning",
            duration = 3
          )
          next
        }

        construct_label <- trimws(parts[1])
        terms_str <- trimws(parts[2])
        search_terms <- strsplit(terms_str, ",")[[1]]
        search_terms <- trimws(search_terms)
        search_terms <- search_terms[search_terms != ""]

        incProgress(1/length(lines), detail = paste("Processing:", construct_label))

        if (!is.null(spacyr_processed())) {
          parsed <- spacyr_processed()

          search_terms_lower <- tolower(search_terms)

          if (use_regex && "lemma" %in% names(parsed)) {
            # Include lemmas - match both token and lemma (both lowercased)
            matching_tokens <- parsed %>%
              dplyr::mutate(
                token_lower = tolower(token),
                lemma_lower = tolower(lemma)
              ) %>%
              dplyr::filter(token_lower %in% search_terms_lower | lemma_lower %in% search_terms_lower)
          } else {
            # Exact token match only
            matching_tokens <- parsed %>%
              dplyr::mutate(token_lower = tolower(token)) %>%
              dplyr::filter(token_lower %in% search_terms_lower)
          }

          if (nrow(matching_tokens) > 0) {
            entity_summary <- matching_tokens %>%
              dplyr::group_by(doc_id) %>%
              dplyr::summarise(
                doc_frequency = dplyr::n(),
                first_sentence = dplyr::first(sentence_id),
                .groups = "drop"
              )

            tokens_obj <- if (!is.null(final_tokens())) {
              final_tokens()
            } else if (!is.null(lemmatized_tokens())) {
              lemmatized_tokens()
            } else if (!is.null(processed_tokens())) {
              processed_tokens()
            } else if (!is.null(preprocessed_combined())) {
              preprocessed_combined()
            } else {
              NULL
            }

            if (is.null(tokens_obj)) {
              showNotification("No tokens available. Please complete preprocessing first.", type = "error")
              next
            }

            all_doc_ids <- quanteda::docnames(tokens_obj)
            doc_number_mapping <- data.frame(
              doc_id = all_doc_ids,
              Document = paste0("Doc ", seq_along(all_doc_ids)),
              stringsAsFactors = FALSE
            )

            entity_summary <- entity_summary %>%
              dplyr::left_join(doc_number_mapping, by = "doc_id")

            total_frequency <- sum(entity_summary$doc_frequency)
            doc_list <- paste(entity_summary$Document, collapse = ", ")

            operational_def <- paste(search_terms, collapse = ", ")

            new_entity <- data.frame(
              `Construct` = construct_label,
              `Operational Definition` = operational_def,
              Document = doc_list,
              Frequency = total_frequency,
              `First Sentence` = entity_summary$first_sentence[1],
              Notes = paste0("Found in ", nrow(entity_summary), " document(s)", if (use_regex) " (with lemmas)" else " (exact match)"),
              Keep = TRUE,
              check.names = FALSE,
              stringsAsFactors = FALSE
            )

            found_count <- found_count + 1
            total_occurrences <- total_occurrences + total_frequency
          } else {
            operational_def <- paste(search_terms, collapse = ", ")

            new_entity <- data.frame(
              `Construct` = construct_label,
              `Operational Definition` = operational_def,
              Document = "Not found",
              Frequency = 0,
              `First Sentence` = NA,
              Notes = "Not found in documents",
              Keep = TRUE,
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
            not_found_count <- not_found_count + 1
          }
        } else {
          operational_def <- paste(search_terms, collapse = ", ")

          new_entity <- data.frame(
            `Construct` = construct_label,
            `Operational Definition` = operational_def,
            Document = "Manual",
            Frequency = 0,
            `First Sentence` = NA,
            Notes = "Added before text processing",
            Keep = TRUE,
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          not_found_count <- not_found_count + 1
        }

        all_new_entities[[i]] <- new_entity
      }
    })

    current_custom <- custom_entities()
    new_entities_df <- dplyr::bind_rows(all_new_entities)

    if (nrow(current_custom) == 0) {
      custom_entities(new_entities_df)
    } else {
      for (i in seq_len(nrow(new_entities_df))) {
        new_construct <- new_entities_df$Construct[i]
        existing_idx <- which(current_custom$Construct == new_construct)

        if (length(existing_idx) > 0) {
          current_custom[existing_idx, ] <- new_entities_df[i, ]
        } else {
          current_custom <- dplyr::bind_rows(current_custom, new_entities_df[i, ])
        }
      }
      custom_entities(current_custom)
    }

    showNotification(
      paste0("Batch coding complete: ", length(lines), " constructs processed. ",
             found_count, " found (", total_occurrences, " total occurrences), ",
             not_found_count, " not found."),
      type = "message",
      duration = 7
    )

    updateTextAreaInput(session, "batch_entity_text", value = "")
  })

  observeEvent(input$process_codebook, {
    req(input$codebook_upload)

    tryCatch({
      file_path <- input$codebook_upload$datapath
      file_ext <- tools::file_ext(input$codebook_upload$name)

      if (file_ext == "csv") {
        codebook <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        codebook <- readxl::read_excel(file_path)
      } else {
        showNotification("Unsupported file format. Use CSV or Excel.", type = "error", duration = 5)
        return()
      }

      required_cols <- c("construct_label", "operational_definition")
      if (!all(required_cols %in% names(codebook))) {
        showNotification(
          paste0("Missing required columns. Expected: ", paste(required_cols, collapse = ", ")),
          type = "error",
          duration = 5
        )
        return()
      }

      all_new_entities <- list()
      found_count <- 0
      not_found_count <- 0
      total_occurrences <- 0

      withProgress(message = 'Processing codebook...', value = 0, {
        for (i in seq_len(nrow(codebook))) {
          construct_label <- codebook$construct_label[i]
          operational_def <- codebook$operational_definition[i]

          search_terms <- strsplit(operational_def, ",")[[1]]
          search_terms <- trimws(search_terms)
          search_terms <- search_terms[search_terms != ""]

          incProgress(1/nrow(codebook), detail = paste("Processing:", construct_label))

          if (!is.null(spacyr_processed())) {
            parsed <- spacyr_processed()

            search_terms_lower <- tolower(search_terms)
            matching_tokens <- parsed %>%
              dplyr::mutate(token_lower = tolower(token)) %>%
              dplyr::filter(token_lower %in% search_terms_lower | lemma %in% search_terms_lower)

            if (nrow(matching_tokens) > 0) {
              entity_summary <- matching_tokens %>%
                dplyr::group_by(doc_id) %>%
                dplyr::summarise(
                  doc_frequency = dplyr::n(),
                  first_sentence = dplyr::first(sentence_id),
                  .groups = "drop"
                )

              tokens_obj <- if (!is.null(final_tokens())) {
                final_tokens()
              } else if (!is.null(lemmatized_tokens())) {
                lemmatized_tokens()
              } else if (!is.null(processed_tokens())) {
                processed_tokens()
              } else if (!is.null(preprocessed_combined())) {
                preprocessed_combined()
              } else {
                NULL
              }

              if (is.null(tokens_obj)) {
                showNotification("No tokens available. Please complete preprocessing first.", type = "error")
                next
              }

              all_doc_ids <- quanteda::docnames(tokens_obj)
              doc_number_mapping <- data.frame(
                doc_id = all_doc_ids,
                Document = paste0("Doc ", seq_along(all_doc_ids)),
                stringsAsFactors = FALSE
              )

              entity_summary <- entity_summary %>%
                dplyr::left_join(doc_number_mapping, by = "doc_id")

              total_frequency <- sum(entity_summary$doc_frequency)
              doc_list <- paste(entity_summary$Document, collapse = ", ")

              new_entity <- data.frame(
                `Construct` = construct_label,
                `Operational Definition` = operational_def,
                Document = doc_list,
                Frequency = total_frequency,
                `First Sentence` = entity_summary$first_sentence[1],
                Notes = paste0("Found in ", nrow(entity_summary), " document(s)"),
                Keep = TRUE,
                check.names = FALSE,
                stringsAsFactors = FALSE
              )

              found_count <- found_count + 1
              total_occurrences <- total_occurrences + total_frequency
            } else {
              new_entity <- data.frame(
                `Construct` = construct_label,
                `Operational Definition` = operational_def,
                Document = "Not found",
                Frequency = 0,
                `First Sentence` = NA,
                Notes = "Not found in documents",
                Keep = TRUE,
                check.names = FALSE,
                stringsAsFactors = FALSE
              )
              not_found_count <- not_found_count + 1
            }
          } else {
            new_entity <- data.frame(
              `Construct` = construct_label,
              `Operational Definition` = operational_def,
              Document = "Manual",
              Frequency = 0,
              `First Sentence` = NA,
              Notes = "Added before text processing",
              Keep = TRUE,
              check.names = FALSE,
              stringsAsFactors = FALSE
            )
            not_found_count <- not_found_count + 1
          }

          all_new_entities[[i]] <- new_entity
        }
      })

      current_custom <- custom_entities()
      new_entities_df <- dplyr::bind_rows(all_new_entities)

      if (nrow(current_custom) == 0) {
        custom_entities(new_entities_df)
      } else {
        for (i in seq_len(nrow(new_entities_df))) {
          new_construct <- new_entities_df$Construct[i]
          existing_idx <- which(current_custom$Construct == new_construct)

          if (length(existing_idx) > 0) {
            current_custom[existing_idx, ] <- new_entities_df[i, ]
          } else {
            current_custom <- dplyr::bind_rows(current_custom, new_entities_df[i, ])
          }
        }
        custom_entities(current_custom)
      }

      showNotification(
        paste0("Codebook processed: ", nrow(codebook), " terms. ",
               found_count, " found (", total_occurrences, " total occurrences), ",
               not_found_count, " not found."),
        type = "message",
        duration = 7
      )

    }, error = function(e) {
      showNotification(
        paste0("Error processing codebook: ", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  coding_mode <- reactiveVal("type")

  observeEvent(input$tab_type, {
    coding_mode("type")
    session$sendCustomMessage("setCodingMode", "type")
  })

  observeEvent(input$tab_upload, {
    coding_mode("upload")
    session$sendCustomMessage("setCodingMode", "upload")
  })

  observeEvent(input$add_batch_entities_bottom, {
    shinyjs::click("add_batch_entities")
  })

  observeEvent(input$process_codebook_bottom, {
    shinyjs::click("process_codebook")
  })

  observeEvent(input$entity_table_cell_edit, {
    info <- input$entity_table_cell_edit

    showNotification(
      "Cell updated successfully. Note: Edits are for display only and will be included in exports.",
      type = "message",
      duration = 3
    )
  })

  output$export_entities <- downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0("entities_export_", timestamp, ".xlsx")
    },
    content = function(file) {
      parsed <- spacyr_processed()

      # Check if entity column exists
      if (is.null(parsed) || !"entity" %in% names(parsed)) {
        if (!is.null(custom_entities()) && nrow(custom_entities()) > 0) {
          # Export only custom entities
          custom_export <- custom_entities() %>%
            dplyr::select(
              `Construct`,
              `Operational Definition`,
              Frequency,
              Document,
              Notes,
              Keep
            )
          openxlsx::write.xlsx(custom_export, file = file)
        } else {
          # Create empty file with message
          openxlsx::write.xlsx(
            data.frame(Message = "No entity data available. Run entity extraction first."),
            file = file
          )
        }
        return()
      }

      entity_export <- parsed %>%
        dplyr::filter(!is.na(entity), entity != "") %>%
        dplyr::mutate(entity_clean = gsub("_[BI]$", "", entity)) %>%
        dplyr::select(
          Document = doc_id,
          Sentence = sentence_id,
          Token_ID = token_id,
          Entity_Text = token,
          Entity_Type = entity_clean,
          Lemma = lemma,
          POS = pos
        )

      if (!is.null(custom_entities()) && nrow(custom_entities()) > 0) {
        custom_export <- custom_entities() %>%
          dplyr::select(
            `Construct`,
            `Operational Definition`,
            Frequency,
            Document,
            Notes,
            Keep
          )

        openxlsx::write.xlsx(
          list(
            "Detected_Entities" = entity_export,
            "Custom_Entities" = custom_export
          ),
          file = file
        )
      } else {
        openxlsx::write.xlsx(entity_export, file = file)
      }
    }
  )

  output$spacyr_full_table <- DT::renderDataTable({
    req(!is.null(final_tokens()) || (!is.null(last_clicked()) && last_clicked() %in% c("lemma", "skip")))
    req(spacyr_processed())

    parsed <- spacyr_processed()

    tokens_obj <- if (!is.null(final_tokens())) {
      final_tokens()
    } else if (!is.null(lemmatized_tokens())) {
      lemmatized_tokens()
    } else if (!is.null(processed_tokens())) {
      processed_tokens()
    } else if (!is.null(preprocessed_combined())) {
      preprocessed_combined()
    } else {
      NULL
    }

    req(tokens_obj)
    all_doc_ids <- quanteda::docnames(tokens_obj)
    doc_number_mapping <- data.frame(
      doc_id = all_doc_ids,
      Document = paste0("Doc ", seq_along(all_doc_ids)),
      stringsAsFactors = FALSE
    )

    # Clean entity column if present (remove _B/_I suffixes)
    if ("entity" %in% names(parsed)) {
      parsed <- parsed %>% dplyr::mutate(entity = gsub("_[BI]$", "", entity))
    }

    # Select available columns - any_of() gracefully handles missing ones
    result <- parsed %>%
      dplyr::select(dplyr::any_of(c("doc_id", "sentence_id", "token_id", "token",
                                     "lemma", "pos", "tag", "entity",
                                     "head_token_id", "dep_rel"))) %>%
      dplyr::left_join(doc_number_mapping, by = "doc_id")

    # Add Document ID if selected
    if (!is.null(input$lemma_doc_id_var) && input$lemma_doc_id_var != "") {
      doc_vars <- quanteda::docvars(tokens_obj)
      if (!is.null(doc_vars) && input$lemma_doc_id_var %in% names(doc_vars)) {
        doc_id_mapping <- data.frame(
          doc_id = all_doc_ids,
          `Document ID` = doc_vars[[input$lemma_doc_id_var]],
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        result <- result %>% dplyr::left_join(doc_id_mapping, by = "doc_id")
      }
    }

    # Final column ordering - any_of() handles missing columns
    final_cols <- c("Document", "Document ID", "sentence_id", "token_id", "token",
                    "lemma", "pos", "tag", "entity", "head_token_id", "dep_rel")
    result %>% dplyr::select(dplyr::any_of(final_cols))
  },
  rownames = FALSE,
  extensions = 'Buttons',
  filter = 'top',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 25,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))



  colnames_cat <- reactive({
    req(mydata())
    categorical <- mydata() %>%
      dplyr::select(dplyr::where(is.character)) %>%
      dplyr::select(dplyr::where(~ dplyr::n_distinct(.) <= (0.2 * nrow(mydata()))))
    names(categorical)
  })

  observe({
    req(colnames_cat())
    updateSelectizeInput(session,
                         "doc_var_co_occurrence",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None"
    )
  })

  observe({
    req(colnames_cat())
    updateSelectizeInput(session,
                         "sentiment_group_var",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None"
    )
    updateSelectizeInput(session,
                         "readability_group_var",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None"
    )
    updateSelectizeInput(session,
                         "keyword_group_var",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None"
    )
    updateSelectizeInput(session,
                         "tfidf_group_var",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None"
    )
  })

  output$top_n_selector_cooccur <- renderUI({
    req(input$doc_var_co_occurrence, input$doc_var_co_occurrence != "None")

    cat_levels <- unique(mydata()[[input$doc_var_co_occurrence]])
    cat_levels <- cat_levels[!is.na(cat_levels)]
    n_cats <- length(cat_levels)

    if (n_cats == 0) return(NULL)

    sliderInput(
      "top_n_cooccur",
      "Top categories to analyze",
      value = min(3, n_cats),
      min = 1,
      max = min(20, n_cats),
      step = 1
    )
  })

  output$category_selector_cooccur <- renderUI({
    req(input$doc_var_co_occurrence, input$doc_var_co_occurrence != "None")
    req(input$top_n_cooccur)

    cat_levels <- unique(mydata()[[input$doc_var_co_occurrence]])
    cat_levels <- cat_levels[!is.na(cat_levels)]

    doc_counts <- sapply(cat_levels, function(level) {
      sum(mydata()[[input$doc_var_co_occurrence]] == level, na.rm = TRUE)
    })

    sorted_cats <- names(sort(doc_counts, decreasing = TRUE))
    top_n <- min(input$top_n_cooccur, length(sorted_cats))
    top_cats <- head(sorted_cats, top_n)

    if (length(top_cats) == 0) {
      return(tags$div(
        style = "background-color: #FEE2E2; border: 1px solid #DC2626; color: #7F1D1D; border-radius: 4px; padding: 10px; margin: 10px 0; font-size: 16px;",
        "No categories available for analysis."
      ))
    }

    choices <- setNames(sorted_cats, paste0(sorted_cats, " (", doc_counts[sorted_cats], " docs)"))

    tags$div(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 5px;",
        tags$label("Selected categories (ranked by document count):", style = "font-weight: bold; font-size: 16px;"),
        tags$div(
          style = "font-size: 16px;",
          actionLink("select_all_cooccur", "Select All", style = "margin-right: 10px; color: #337ab7;"),
          "|",
          actionLink("select_none_cooccur", "Clear All", style = "margin-left: 10px; color: #337ab7;")
        )
      ),
      tags$div(
        style = "background-color: #F8F9FA; border: 1px solid #DEE2E6; border-radius: 4px; padding: 10px; max-height: 300px; overflow-y: auto; font-size: 16px;",
        checkboxGroupInput(
          "selected_categories_cooccur",
          NULL,
          choices = choices,
          selected = top_cats
        )
      ),
      tags$div(
        style = "margin-top: 10px; padding: 8px; background-color: #E0F2FE; border-radius: 4px; font-size: 16px;",
        tags$strong(textOutput("cooccur_selection_summary", inline = TRUE))
      )
    )
  })

  output$cooccur_selection_summary <- renderText({
    n_selected <- length(input$selected_categories_cooccur %||% character(0))
    if (n_selected == 0) {
      "No categories selected. Please select at least one category to analyze."
    } else if (n_selected == 1) {
      paste("1 category selected for analysis")
    } else {
      paste(n_selected, "categories selected for analysis")
    }
  })

  observeEvent(input$select_all_cooccur, {
    req(input$doc_var_co_occurrence, input$doc_var_co_occurrence != "None")
    cat_levels <- unique(mydata()[[input$doc_var_co_occurrence]])
    cat_levels <- cat_levels[!is.na(cat_levels)]
    doc_counts <- sapply(cat_levels, function(level) {
      sum(mydata()[[input$doc_var_co_occurrence]] == level, na.rm = TRUE)
    })
    sorted_cats <- names(sort(doc_counts, decreasing = TRUE))
    updateCheckboxGroupInput(session, "selected_categories_cooccur", selected = sorted_cats)
  })

  observeEvent(input$select_none_cooccur, {
    updateCheckboxGroupInput(session, "selected_categories_cooccur", selected = character(0))
  })

  output$category_cooccur_controls <- renderUI({
    req(input$doc_var_co_occurrence, input$doc_var_co_occurrence != "None")

    cat_levels <- unique(mydata()[[input$doc_var_co_occurrence]])
    cat_levels <- cat_levels[!is.na(cat_levels)]

    if (length(cat_levels) == 0) {
      return(NULL)
    }

    controls <- lapply(cat_levels, function(level) {
      level_id <- make.names(paste(input$doc_var_co_occurrence, level, sep = "_"))

      div(
        class = "category-controls",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          tags$h6(strong(paste("Category:", level)), style = "margin: 0; font-weight: bold;"),
          actionButton(
            paste0("reset_cooccur_", level_id),
            "Reset to Global",
            class = "btn-sm btn-outline-secondary",
            style = "font-size: 11px; padding: 2px 8px;"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          sliderInput(
            paste0("co_occur_", level_id),
            "Minimum co-occurrences",
            value = min(isolate(input$co_occurence_number_global) %||% 5, 150),
            min = 0,
            max = 150,
            step = 1,
            width = "100%"
          ),
          sliderInput(
            paste0("top_nodes_", level_id),
            "Top N nodes",
            value = min(isolate(input$top_node_n_co_occurrence_global) %||% 30, 150),
            min = 0,
            max = 150,
            step = 1,
            width = "100%"
          )
        )
      )
    })

    tagList(controls)
  })

  observeEvent(input$doc_var_co_occurrence,
               {
                 req(input$doc_var_co_occurrence != "None")

                 cat_levels <- unique(mydata()[[input$doc_var_co_occurrence]])
                 cat_levels <- cat_levels[!is.na(cat_levels)]

                 for (level in cat_levels) {
                   level_id <- make.names(paste(input$doc_var_co_occurrence, level, sep = "_"))
                   reset_button_id <- paste0("reset_cooccur_", level_id)

                   local({
                     level_id_local <- level_id
                     reset_button_id_local <- reset_button_id

                     observeEvent(input[[reset_button_id_local]],
                                  {
                                    updateSliderInput(session, paste0("co_occur_", level_id_local),
                                                      value = input$co_occurence_number_global
                                    )
                                    updateSliderInput(session, paste0("top_nodes_", level_id_local),
                                                      value = input$top_node_n_co_occurrence_global
                                    )
                                  },
                                  ignoreInit = TRUE
                     )
                   })
                 }
               },
               ignoreInit = TRUE
  )

  ################################################################################
  # LEXICAL ANALYSIS TAB
  ################################################################################

  get_cooccur_params <- reactive({
    doc_var_input <- as.character(input$doc_var_co_occurrence)
    doc_var <- if (doc_var_input == "None") NULL else doc_var_input

    if (isTRUE(input$use_category_cooccur) && !is.null(doc_var)) {
      selected_cats <- input$selected_categories_cooccur

      if (is.null(selected_cats) || length(selected_cats) == 0) {
        cat_levels <- unique(mydata()[[doc_var]])
        cat_levels <- cat_levels[!is.na(cat_levels)]
      } else {
        cat_levels <- selected_cats
      }

      params_list <- list()
      for (level in cat_levels) {
        level_id <- make.names(paste(doc_var, level, sep = "_"))
        params_list[[as.character(level)]] <- list(
          co_occur_n = input[[paste0("co_occur_", level_id)]] %||% input$co_occurence_number_global,
          top_node_n = input[[paste0("top_nodes_", level_id)]] %||% input$top_node_n_co_occurrence_global
        )
      }
      return(list(
        doc_var = doc_var,
        use_category_specific = TRUE,
        category_params = params_list,
        nrows = input$nrows_co_occurrence,
        selected_categories = cat_levels
      ))
    } else {
      return(list(
        doc_var = doc_var,
        use_category_specific = FALSE,
        co_occur_n = input$co_occurence_number_global,
        top_node_n = input$top_node_n_co_occurrence_global,
        nrows = input$nrows_co_occurrence
      ))
    }
  })

  get_corr_params <- reactive({
    doc_var_input <- as.character(input$doc_var_correlation)
    doc_var <- if (doc_var_input == "None") NULL else doc_var_input

    if (isTRUE(input$use_category_corr) && !is.null(doc_var)) {
      cat_levels <- unique(mydata()[[doc_var]])
      cat_levels <- cat_levels[!is.na(cat_levels)]

      params_list <- list()
      for (level in cat_levels) {
        level_id <- make.names(paste(doc_var, level, sep = "_"))
        params_list[[as.character(level)]] <- list(
          common_term_n = input[[paste0("common_terms_", level_id)]] %||% input$common_term_n_global,
          corr_n = input[[paste0("min_corr_", level_id)]] %||% input$corr_n_global,
          top_node_n = input[[paste0("top_nodes_corr_", level_id)]] %||% input$top_node_n_correlation_global
        )
      }
      return(list(
        doc_var = doc_var,
        use_category_specific = TRUE,
        category_params = params_list,
        nrows = input$nrows_correlation
      ))
    } else {
      return(list(
        doc_var = doc_var,
        use_category_specific = FALSE,
        common_term_n = input$common_term_n_global,
        corr_n = input$corr_n_global,
        top_node_n = input$top_node_n_correlation_global,
        nrows = input$nrows_correlation
      ))
    }
  })

  cooccur_network_data <- reactiveVal(NULL)

  observeEvent(input$plot_word_co_occurrence_network, {
    params <- get_cooccur_params()
    dfm_to_use <- try(dfm_final(), silent = TRUE)
    if (is.null(dfm_to_use) || inherits(dfm_to_use, "try-error")) {
      dfm_to_use <- dfm_init()
    }
    req(dfm_to_use)

    show_loading_notification("Computing co-occurrence network...", id = "cooccur_network_loading")
    shinybusy::show_spinner()

    withProgress(message = 'Computing co-occurrence network...', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Computing network...")

        feature_type <- input$semantic_feature_space %||% "words"
        ngram_range <- if (feature_type == "ngrams") as.numeric(input$semantic_ngram_range %||% "2") else 2

        texts_df <- tryCatch(united_tbl(), error = function(e) NULL)
        texts_vec <- if (feature_type == "ngrams" && !is.null(texts_df) && "text" %in% names(texts_df)) {
          texts_df$text
        } else {
          NULL
        }

        embeddings_mat <- if (feature_type == "embeddings") embeddings_cache$embeddings else NULL

        # Category-specific network analysis
        if (isTRUE(params$use_category_specific) && !is.null(params$doc_var) &&
            params$doc_var %in% names(dfm_to_use@docvars)) {

          categories <- params$selected_categories
          results_list <- list()
          n_cats <- length(categories)

          for (i in seq_along(categories)) {
            cat_name <- categories[i]
            cat_params <- params$category_params[[cat_name]]

            incProgress(0.1 + (0.7 * i / n_cats), detail = paste("Processing", cat_name, "..."))

            # Get document indices for this category
            doc_indices <- which(dfm_to_use@docvars[[params$doc_var]] == cat_name)

            # Subset DFM by category
            dfm_subset <- tryCatch({
              if (length(doc_indices) > 0) {
                quanteda::dfm_subset(dfm_to_use, dfm_to_use@docvars[[params$doc_var]] == cat_name)
              } else {
                NULL
              }
            }, error = function(e) NULL)

            if (is.null(dfm_subset) || quanteda::ndoc(dfm_subset) < 2) {
              showNotification(
                paste("Category", cat_name, "has too few documents. Skipping."),
                type = "warning", duration = 3
              )
              next
            }

            cat_result <- tryCatch({
              TextAnalysisR::semantic_cooccurrence_network(
                dfm_object = dfm_subset,
                doc_var = NULL,
                co_occur_n = cat_params$co_occur_n %||% input$co_occurence_number_global,
                top_node_n = cat_params$top_node_n %||% input$top_node_n_co_occurrence_global,
                node_label_size = input$node_label_size_cooccur %||% 22,
                pattern = NULL,
                showlegend = TRUE,
                seed = 2025,
                feature_type = feature_type,
                ngram_range = ngram_range,
                texts = if (!is.null(texts_vec)) texts_vec[doc_indices] else NULL,
                embeddings = embeddings_mat,
                embedding_sim_threshold = input$embedding_sim_threshold %||% 0.5,
                community_method = input$community_method_cooccur %||% "leiden"
              )
            }, error = function(e) {
              message("Error processing category ", cat_name, ": ", e$message)
              NULL
            })

            if (!is.null(cat_result)) {
              results_list[[cat_name]] <- cat_result
            }
          }

          incProgress(0.9, detail = "Finalizing...")

          if (length(results_list) == 0) {
            showNotification(
              "No networks generated for any category. Try adjusting thresholds.",
              type = "warning", duration = 5
            )
            cooccur_network_data(NULL)
          } else {
            cooccur_network_data(list(
              mode = "category_specific",
              categories = names(results_list),
              results = results_list
            ))
            showNotification(
              paste("Successfully computed networks for", length(results_list), "categories"),
              type = "message", duration = 3
            )
          }

        } else {
          # Single network mode (existing behavior)
          result <- TextAnalysisR::semantic_cooccurrence_network(
            dfm_object = dfm_to_use,
            doc_var = params$doc_var,
            co_occur_n = params$co_occur_n %||% input$co_occurence_number_global,
            top_node_n = params$top_node_n %||% input$top_node_n_co_occurrence_global,
            node_label_size = input$node_label_size_cooccur %||% 22,
            pattern = NULL,
            showlegend = TRUE,
            seed = 2025,
            feature_type = feature_type,
            ngram_range = ngram_range,
            texts = texts_vec,
            embeddings = embeddings_mat,
            embedding_sim_threshold = input$embedding_sim_threshold %||% 0.5,
            community_method = input$community_method_cooccur %||% "leiden"
          )

          incProgress(0.9, detail = "Finalizing...")

          if (is.null(result)) {
            showNotification(
              "No co-occurrence network generated. Try lowering thresholds.",
              type = "warning", duration = 5
            )
            cooccur_network_data(NULL)
          } else {
            cooccur_network_data(list(
              mode = "single",
              result = result
            ))
            showNotification(
              "Successfully computed co-occurrence network",
              type = "message", duration = 3
            )
          }
        }

        remove_notification_by_id("cooccur_network_loading")
        shinybusy::hide_spinner()

      }, error = function(e) {
        remove_notification_by_id("cooccur_network_loading")
        shinybusy::hide_spinner()
        showNotification(paste("Error:", e$message), type = "error")
        cooccur_network_data(NULL)
      })
    })

  output$word_co_occurrence_network_plot <- visNetwork::renderVisNetwork({
    data <- cooccur_network_data()
    req(data)

    # Handle both single and category-specific modes
    if (!is.null(data$mode) && data$mode == "single") {
      data$result$plot
    } else if (!is.null(data$mode) && data$mode == "category_specific") {
      # For category mode, show first category by default
      if (length(data$results) > 0) {
        data$results[[1]]$plot
      } else {
        NULL
      }
    } else {
      # Legacy format (direct result)
      data$plot
    }
  })

  output$word_co_occurrence_network_plot_uiOutput <- renderUI({
    data <- cooccur_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Click 'Plot Network' to generate co-occurrence network visualization.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Handle category-specific mode with tabs
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        tabPanel(
          cat_name,
          tags$div(
            style = "margin-top: 15px;",
            tags$div(
              style = "background-color: #E0F2FE; border-radius: 4px; padding: 12px; margin-bottom: 15px; font-size: 16px;",
              paste0("Network: ", cat_data$stats$nodes, " nodes, ", cat_data$stats$edges, " edges | ",
                     "Modularity: ", ifelse(is.null(cat_data$stats$modularity) || is.na(cat_data$stats$modularity), "N/A", cat_data$stats$modularity), " | ",
                     "Clustering: ", ifelse(is.null(cat_data$stats$global_clustering) || is.na(cat_data$stats$global_clustering), "N/A", cat_data$stats$global_clustering))
            ),
            visNetwork::visNetworkOutput(paste0("cooccur_net_", make.names(cat_name)),
                                         height = paste0(input$height_word_co_occurrence_network_plot, "px"),
                                         width = paste0(input$width_word_co_occurrence_network_plot, "px"))
          )
        )
      })
      do.call(tabsetPanel, c(list(id = "cooccur_category_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      tags$div(
        tags$div(
          style = "background-color: #E0F2FE; border-radius: 4px; padding: 12px; margin-bottom: 15px; font-size: 16px;",
          paste0("Network: ", result_data$stats$nodes, " nodes, ", result_data$stats$edges, " edges | ",
                 "Modularity: ", ifelse(is.null(result_data$stats$modularity) || is.na(result_data$stats$modularity), "N/A", result_data$stats$modularity), " | ",
                 "Clustering: ", ifelse(is.null(result_data$stats$global_clustering) || is.na(result_data$stats$global_clustering), "N/A", result_data$stats$global_clustering))
        ),
        visNetwork::visNetworkOutput("word_co_occurrence_network_plot",
                                     height = paste0(input$height_word_co_occurrence_network_plot, "px"),
                                     width = paste0(input$width_word_co_occurrence_network_plot, "px"))
      )
    }
  })

  # Dynamic observers for category-specific network plots
  observe({
    data <- cooccur_network_data()
    req(!is.null(data$mode) && data$mode == "category_specific")

    for (cat_name in names(data$results)) {
      local({
        cn <- cat_name
        output_id <- paste0("cooccur_net_", make.names(cn))
        output[[output_id]] <- visNetwork::renderVisNetwork({
          data$results[[cn]]$plot
        })
      })
    }
  })

  output$word_co_occurrence_network_table_uiOutput <- renderUI({
    data <- cooccur_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Network centrality table will appear after generating the network.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Handle category-specific mode
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        tabPanel(
          cat_name,
          DT::datatable(
            cat_data$table,
            rownames = FALSE,
            extensions = 'Buttons',
            options = list(
              scrollX = TRUE,
              pageLength = 25,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
          ) %>%
            DT::formatStyle(columns = 1:ncol(cat_data$table), fontSize = "16px")
        )
      })
      do.call(tabsetPanel, c(list(id = "cooccur_table_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      DT::datatable(
        result_data$table,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      ) %>%
        DT::formatStyle(columns = 1:ncol(result_data$table), fontSize = "16px")
    }
  })

  output$word_co_occurrence_network_summary_uiOutput <- renderUI({
    data <- cooccur_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Network statistics will appear after generating the network.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Helper function to safely format values
    format_stat <- function(x) {
      if (is.null(x) || is.na(x) || is.nan(x)) return("N/A")
      if (is.numeric(x)) {
        if (x == round(x)) return(as.character(as.integer(x)))
        return(as.character(x))
      }
      as.character(x)
    }

    # Handle category-specific mode
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        stats_df <- data.frame(
          Metric = c("Nodes", "Edges", "Edge Density", "Diameter",
                     "Global Clustering", "Avg Local Clustering",
                     "Modularity", "Assortativity", "Avg Path Length"),
          Value = c(
            format_stat(cat_data$stats$nodes),
            format_stat(cat_data$stats$edges),
            format_stat(cat_data$stats$density),
            format_stat(cat_data$stats$diameter),
            format_stat(cat_data$stats$global_clustering),
            format_stat(cat_data$stats$avg_local_clustering),
            format_stat(cat_data$stats$modularity),
            format_stat(cat_data$stats$assortativity),
            format_stat(cat_data$stats$avg_path_length)
          ),
          Description = c(
            "Total unique words in network",
            "Total connections between words",
            "Proportion of possible edges present",
            "Longest shortest path in network",
            "Overall network clustering tendency",
            "Average of local clustering coefficients",
            "Quality of community structure",
            "Tendency of similar nodes to connect",
            "Average distance between nodes"
          ),
          stringsAsFactors = FALSE
        )
        tabPanel(
          cat_name,
          DT::datatable(
            stats_df,
            rownames = FALSE,
            extensions = 'Buttons',
            options = list(
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            )
          ) %>%
            DT::formatStyle(columns = 1:ncol(stats_df), fontSize = "16px")
        )
      })
      do.call(tabsetPanel, c(list(id = "cooccur_stats_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      stats_df <- data.frame(
        Metric = c("Nodes", "Edges", "Edge Density", "Diameter",
                   "Global Clustering", "Avg Local Clustering",
                   "Modularity", "Assortativity", "Avg Path Length"),
        Value = c(
          format_stat(result_data$stats$nodes),
          format_stat(result_data$stats$edges),
          format_stat(result_data$stats$density),
          format_stat(result_data$stats$diameter),
          format_stat(result_data$stats$global_clustering),
          format_stat(result_data$stats$avg_local_clustering),
          format_stat(result_data$stats$modularity),
          format_stat(result_data$stats$assortativity),
          format_stat(result_data$stats$avg_path_length)
        ),
        Description = c(
          "Total unique words in network",
          "Total connections between words",
          "Proportion of possible edges present",
          "Longest shortest path in network",
          "Overall network clustering tendency",
          "Average of local clustering coefficients",
          "Quality of community structure",
          "Tendency of similar nodes to connect",
          "Average distance between nodes"
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        stats_df,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        DT::formatStyle(columns = 1:ncol(stats_df), fontSize = "16px")
    }
  })

  })


  observeEvent(input$select_none_corr, {
    updateCheckboxGroupInput(session, "selected_categories_corr", selected = character(0))
  })

  output$category_corr_controls <- renderUI({
    req(input$doc_var_correlation, input$doc_var_correlation != "None")

    cat_levels <- unique(mydata()[[input$doc_var_correlation]])
    cat_levels <- cat_levels[!is.na(cat_levels)]

    if (length(cat_levels) == 0) {
      return(NULL)
    }

    controls <- lapply(cat_levels, function(level) {
      level_id <- make.names(paste(input$doc_var_correlation, level, sep = "_"))

      div(
        class = "category-controls",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          tags$h6(strong(paste("Category:", level)), style = "margin: 0; font-weight: bold;"),
          actionButton(
            paste0("reset_corr_", level_id),
            "Reset to Global",
            class = "btn-sm btn-outline-secondary",
            style = "font-size: 11px; padding: 2px 8px;"
          )
        ),
        div(
          style = "display: flex; flex-direction: column; gap: 15px;",
          sliderInput(
            paste0("common_terms_", level_id),
            "Common terms",
            value = min(isolate(input$common_term_n_global) %||% 5, 150),
            min = 0,
            max = 150,
            step = 1,
            width = "100%"
          ),
          sliderInput(
            paste0("min_corr_", level_id),
            "Minimum correlation",
            value = isolate(input$corr_n_global) %||% 0.3,
            min = 0,
            max = 1,
            step = 0.1,
            width = "100%"
          ),
          sliderInput(
            paste0("top_nodes_corr_", level_id),
            "Top N nodes",
            value = min(isolate(input$top_node_n_correlation_global) %||% 30, 150),
            min = 0,
            max = 150,
            step = 1,
            width = "100%"
          )
        )
      )
    })

    tagList(controls)
  })

  observeEvent(input$doc_var_correlation,
               {
                 req(input$doc_var_correlation != "None")

                 cat_levels <- unique(mydata()[[input$doc_var_correlation]])
                 cat_levels <- cat_levels[!is.na(cat_levels)]

                 for (level in cat_levels) {
                   level_id <- make.names(paste(input$doc_var_correlation, level, sep = "_"))
                   reset_button_id <- paste0("reset_corr_", level_id)

                   local({
                     level_id_local <- level_id
                     reset_button_id_local <- reset_button_id

                     observeEvent(input[[reset_button_id_local]],
                                  {
                                    updateSliderInput(session, paste0("common_terms_", level_id_local),
                                                      value = input$common_term_n_global
                                    )
                                    updateSliderInput(session, paste0("min_corr_", level_id_local),
                                                      value = input$corr_n_global
                                    )
                                    updateSliderInput(session, paste0("top_nodes_corr_", level_id_local),
                                                      value = input$top_node_n_correlation_global
                                    )
                                  },
                                  ignoreInit = TRUE
                     )
                   })
                 }
               },
               ignoreInit = TRUE
  )

  corr_network_data <- reactiveVal(NULL)

  observeEvent(input$plot_word_correlation_network, {
    params <- get_corr_params()
    dfm_to_use <- try(dfm_outcome(), silent = TRUE)
    if (is.null(dfm_to_use) || inherits(dfm_to_use, "try-error")) {
      dfm_to_use <- try(dfm_final(), silent = TRUE)
    }
    if (is.null(dfm_to_use) || inherits(dfm_to_use, "try-error")) {
      dfm_to_use <- dfm_init()
    }
    req(dfm_to_use)

    show_loading_notification("Computing correlation network...", id = "corr_network_loading")
    shinybusy::show_spinner()

    withProgress(message = 'Computing correlation network...', value = 0, {
      tryCatch({
        incProgress(0.1, detail = "Computing network...")

        feature_type <- input$semantic_feature_space %||% "words"
        ngram_range <- if (feature_type == "ngrams") as.numeric(input$semantic_ngram_range %||% "2") else 2

        texts_df <- tryCatch(united_tbl(), error = function(e) NULL)
        texts_vec <- if (feature_type == "ngrams" && !is.null(texts_df) && "text" %in% names(texts_df)) {
          texts_df$text
        } else {
          NULL
        }

        embeddings_mat <- if (feature_type == "embeddings") embeddings_cache$embeddings else NULL

        # Category-specific network analysis
        if (isTRUE(params$use_category_specific) && !is.null(params$doc_var) &&
            params$doc_var %in% names(dfm_to_use@docvars)) {

          categories <- names(params$category_params)
          results_list <- list()
          n_cats <- length(categories)

          for (i in seq_along(categories)) {
            cat_name <- categories[i]
            cat_params <- params$category_params[[cat_name]]

            incProgress(0.1 + (0.7 * i / n_cats), detail = paste("Processing", cat_name, "..."))

            # Get document indices for this category
            doc_indices <- which(dfm_to_use@docvars[[params$doc_var]] == cat_name)

            # Subset DFM by category
            dfm_subset <- tryCatch({
              if (length(doc_indices) > 0) {
                quanteda::dfm_subset(dfm_to_use, dfm_to_use@docvars[[params$doc_var]] == cat_name)
              } else {
                NULL
              }
            }, error = function(e) NULL)

            if (is.null(dfm_subset) || quanteda::ndoc(dfm_subset) < 2) {
              showNotification(
                paste("Category", cat_name, "has too few documents. Skipping."),
                type = "warning", duration = 3
              )
              next
            }

            cat_result <- tryCatch({
              TextAnalysisR::semantic_correlation_network(
                dfm_object = dfm_subset,
                doc_var = NULL,
                common_term_n = cat_params$common_term_n %||% input$common_term_n_global,
                corr_n = cat_params$corr_n %||% input$corr_n_global,
                top_node_n = cat_params$top_node_n %||% input$top_node_n_correlation_global,
                node_label_size = input$node_label_size_corr %||% 22,
                pattern = NULL,
                showlegend = TRUE,
                seed = 2025,
                feature_type = feature_type,
                ngram_range = ngram_range,
                texts = if (!is.null(texts_vec)) texts_vec[doc_indices] else NULL,
                embeddings = embeddings_mat,
                embedding_sim_threshold = input$embedding_sim_threshold %||% 0.5,
                community_method = input$community_method_corr %||% "leiden"
              )
            }, error = function(e) {
              message("Error processing category ", cat_name, ": ", e$message)
              NULL
            })

            if (!is.null(cat_result)) {
              results_list[[cat_name]] <- cat_result
            }
          }

          incProgress(0.9, detail = "Finalizing...")

          if (length(results_list) == 0) {
            showNotification(
              "No networks generated for any category. Try adjusting thresholds.",
              type = "warning", duration = 5
            )
            corr_network_data(NULL)
          } else {
            corr_network_data(list(
              mode = "category_specific",
              categories = names(results_list),
              results = results_list
            ))
            showNotification(
              paste("Successfully computed networks for", length(results_list), "categories"),
              type = "message", duration = 3
            )
          }

        } else {
          # Single network mode (existing behavior)
          result <- TextAnalysisR::semantic_correlation_network(
            dfm_object = dfm_to_use,
            doc_var = params$doc_var,
            common_term_n = params$common_term_n %||% input$common_term_n_global,
            corr_n = params$corr_n %||% input$corr_n_global,
            top_node_n = params$top_node_n %||% input$top_node_n_correlation_global,
            node_label_size = input$node_label_size_corr %||% 22,
            pattern = NULL,
            showlegend = TRUE,
            seed = 2025,
            feature_type = feature_type,
            ngram_range = ngram_range,
            texts = texts_vec,
            embeddings = embeddings_mat,
            embedding_sim_threshold = input$embedding_sim_threshold %||% 0.5,
            community_method = input$community_method_corr %||% "leiden"
          )

          incProgress(0.9, detail = "Finalizing...")

          if (is.null(result)) {
            showNotification(
              "No correlation network generated. Try lowering thresholds.",
              type = "warning", duration = 5
            )
            corr_network_data(NULL)
          } else {
            corr_network_data(list(
              mode = "single",
              result = result
            ))
            showNotification(
              "Successfully computed correlation network",
              type = "message", duration = 3
            )
          }
        }

        remove_notification_by_id("corr_network_loading")
        shinybusy::hide_spinner()

      }, error = function(e) {
        remove_notification_by_id("corr_network_loading")
        shinybusy::hide_spinner()
        showNotification(paste("Error:", e$message), type = "error")
        corr_network_data(NULL)
      })
    })
  })


  output$word_correlation_network_plot <- visNetwork::renderVisNetwork({
    data <- corr_network_data()
    req(data)

    # Handle both single and category-specific modes
    if (!is.null(data$mode) && data$mode == "single") {
      data$result$plot
    } else if (!is.null(data$mode) && data$mode == "category_specific") {
      # For category mode, show first category by default
      if (length(data$results) > 0) {
        data$results[[1]]$plot
      } else {
        NULL
      }
    } else {
      # Legacy format (direct result)
      data$plot
    }
  })

  output$word_correlation_network_plot_uiOutput <- renderUI({
    data <- corr_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Click 'Plot Network' to generate correlation network visualization.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Handle category-specific mode with tabs
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        tabPanel(
          cat_name,
          tags$div(
            style = "margin-top: 15px;",
            tags$div(
              style = "background-color: #E0F2FE; border-radius: 4px; padding: 12px; margin-bottom: 15px; font-size: 16px;",
              paste0("Network: ", cat_data$stats$nodes, " nodes, ", cat_data$stats$edges, " edges | ",
                     "Modularity: ", ifelse(is.null(cat_data$stats$modularity) || is.na(cat_data$stats$modularity), "N/A", cat_data$stats$modularity), " | ",
                     "Clustering: ", ifelse(is.null(cat_data$stats$global_clustering) || is.na(cat_data$stats$global_clustering), "N/A", cat_data$stats$global_clustering))
            ),
            visNetwork::visNetworkOutput(paste0("corr_net_", make.names(cat_name)),
                                         height = paste0(input$height_word_correlation_network_plot, "px"),
                                         width = paste0(input$width_word_correlation_network_plot, "px"))
          )
        )
      })
      do.call(tabsetPanel, c(list(id = "corr_category_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      tags$div(
        tags$div(
          style = "background-color: #E0F2FE; border-radius: 4px; padding: 12px; margin-bottom: 15px; font-size: 16px;",
          paste0("Network: ", result_data$stats$nodes, " nodes, ", result_data$stats$edges, " edges | ",
                 "Modularity: ", ifelse(is.null(result_data$stats$modularity) || is.na(result_data$stats$modularity), "N/A", result_data$stats$modularity), " | ",
                 "Clustering: ", ifelse(is.null(result_data$stats$global_clustering) || is.na(result_data$stats$global_clustering), "N/A", result_data$stats$global_clustering))
        ),
        visNetwork::visNetworkOutput("word_correlation_network_plot",
                                     height = paste0(input$height_word_correlation_network_plot, "px"),
                                     width = paste0(input$width_word_correlation_network_plot, "px"))
      )
    }
  })

  # Dynamic observers for category-specific correlation network plots
  observe({
    data <- corr_network_data()
    req(!is.null(data$mode) && data$mode == "category_specific")

    for (cat_name in names(data$results)) {
      local({
        cn <- cat_name
        output_id <- paste0("corr_net_", make.names(cn))
        output[[output_id]] <- visNetwork::renderVisNetwork({
          data$results[[cn]]$plot
        })
      })
    }
  })

  output$word_correlation_network_table_uiOutput <- renderUI({
    data <- corr_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Network centrality table will appear after generating the network.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Handle category-specific mode
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        tabPanel(
          cat_name,
          DT::datatable(
            cat_data$table,
            rownames = FALSE,
            extensions = 'Buttons',
            options = list(
              scrollX = TRUE,
              pageLength = 25,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
          ) %>%
            DT::formatStyle(columns = 1:ncol(cat_data$table), fontSize = "16px")
        )
      })
      do.call(tabsetPanel, c(list(id = "corr_table_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      DT::datatable(
        result_data$table,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        )
      ) %>%
        DT::formatStyle(columns = 1:ncol(result_data$table), fontSize = "16px")
    }
  })

  output$word_correlation_network_summary_uiOutput <- renderUI({
    data <- corr_network_data()

    if (is.null(data)) {
      return(tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 500px; margin: 0 auto;",
          tags$p(
            "Network statistics will appear after generating the network.",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      ))
    }

    # Helper function to safely format values
    format_stat <- function(x) {
      if (is.null(x) || is.na(x) || is.nan(x)) return("N/A")
      if (is.numeric(x)) {
        if (x == round(x)) return(as.character(as.integer(x)))
        return(as.character(x))
      }
      as.character(x)
    }

    # Handle category-specific mode
    if (!is.null(data$mode) && data$mode == "category_specific") {
      tabs <- lapply(names(data$results), function(cat_name) {
        cat_data <- data$results[[cat_name]]
        stats_df <- data.frame(
          Metric = c("Nodes", "Edges", "Edge Density", "Diameter",
                     "Global Clustering", "Avg Local Clustering",
                     "Modularity", "Assortativity", "Avg Path Length"),
          Value = c(
            format_stat(cat_data$stats$nodes),
            format_stat(cat_data$stats$edges),
            format_stat(cat_data$stats$density),
            format_stat(cat_data$stats$diameter),
            format_stat(cat_data$stats$global_clustering),
            format_stat(cat_data$stats$avg_local_clustering),
            format_stat(cat_data$stats$modularity),
            format_stat(cat_data$stats$assortativity),
            format_stat(cat_data$stats$avg_path_length)
          ),
          Description = c(
            "Total unique words in network",
            "Total connections between words",
            "Proportion of possible edges present",
            "Longest shortest path in network",
            "Overall network clustering tendency",
            "Average of local clustering coefficients",
            "Quality of community structure",
            "Tendency of similar nodes to connect",
            "Average distance between nodes"
          ),
          stringsAsFactors = FALSE
        )
        tabPanel(
          cat_name,
          DT::datatable(
            stats_df,
            rownames = FALSE,
            extensions = 'Buttons',
            options = list(
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            )
          ) %>%
            DT::formatStyle(columns = 1:ncol(stats_df), fontSize = "16px")
        )
      })
      do.call(tabsetPanel, c(list(id = "corr_stats_tabs"), tabs))
    } else {
      # Single mode
      result_data <- if (!is.null(data$mode) && data$mode == "single") data$result else data

      stats_df <- data.frame(
        Metric = c("Nodes", "Edges", "Edge Density", "Diameter",
                   "Global Clustering", "Avg Local Clustering",
                   "Modularity", "Assortativity", "Avg Path Length"),
        Value = c(
          format_stat(result_data$stats$nodes),
          format_stat(result_data$stats$edges),
          format_stat(result_data$stats$density),
          format_stat(result_data$stats$diameter),
          format_stat(result_data$stats$global_clustering),
          format_stat(result_data$stats$avg_local_clustering),
          format_stat(result_data$stats$modularity),
          format_stat(result_data$stats$assortativity),
          format_stat(result_data$stats$avg_path_length)
        ),
        Description = c(
          "Total unique words in network",
          "Total connections between words",
          "Proportion of possible edges present",
          "Longest shortest path in network",
          "Overall network clustering tendency",
          "Average of local clustering coefficients",
          "Quality of community structure",
          "Tendency of similar nodes to connect",
          "Average distance between nodes"
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        stats_df,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        DT::formatStyle(columns = 1:ncol(stats_df), fontSize = "16px")
    }
  })


  colnames_con <- reactive({
    req(mydata())
    continuous <- mydata() %>% dplyr::select(dplyr::where(is.numeric))
    names(continuous)
  })

  observe({
    updateSelectInput(session,
                      "continuous_var_3",
                      choices = colnames_con(),
                      selected = NULL
    )
  })

  tstat_freq_over_con_var <- reactive({
    dfm_to_use <- NULL

    tryCatch({
      dfm_to_use <- dfm_outcome()
    }, error = function(e) {
      tryCatch({
        dfm_to_use <<- dfm_final()
      }, error = function(e2) {
        tryCatch({
          dfm_to_use <<- dfm_init()
        }, error = function(e3) {
          dfm_to_use <<- NULL
        })
      })
    })

    req(dfm_to_use)

    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_to_use)
    tstat_freq_n_100 <- head(tstat_freq, 100)
    tstat_freq_n_100$feature
  })

  observe({
    req(tstat_freq_over_con_var())
    updateSelectizeInput(
      session,
      "type_terms",
      choices = tstat_freq_over_con_var(),
      options = list(create = TRUE),
      selected = ""
    )
  })


  observeEvent(input$plot_term, {
    suppressWarnings({
      tryCatch(
        {
        if (!requireNamespace("htmltools", quietly = TRUE) ||
            !requireNamespace("splines", quietly = TRUE) ||
            !requireNamespace("broom", quietly = TRUE)) {
          stop(
            "The 'htmltools', 'splines', and 'broom' packages are required for this functionality. ",
            "Please install them using install.packages(c('htmltools', 'splines', 'broom'))."
          )
        }

        vm <- isolate(input$type_terms)
        if (!is.null(vm)) {
          dfm_to_use <- NULL

          tryCatch({
            dfm_to_use <- dfm_outcome()
          }, error = function(e) {
            tryCatch({
              dfm_to_use <<- dfm_final()
            }, error = function(e2) {
              tryCatch({
                dfm_to_use <<- dfm_init()
              }, error = function(e3) {
                dfm_to_use <<- NULL
              })
            })
          })

          req(dfm_to_use)

          dfm_outcome_obj <- dfm_to_use
          dfm_td <- tidytext::tidy(dfm_to_use)

          dfm_outcome_obj@docvars$document <- dfm_outcome_obj@docvars$docname_

          dfm_td <- dfm_td %>%
            left_join(dfm_outcome_obj@docvars,
                      by = c("document" = "document")
            )

          output$line_con_var_plot <- plotly::renderPlotly({
            suppressWarnings({
              req(input$continuous_var_3)
              req(vm)

              con_var_term_counts <- dfm_td %>%
                tibble::as_tibble() %>%
                group_by(!!rlang::sym(input$continuous_var_3), term) %>%
                summarise(word_frequency = sum(count), .groups = "drop")

              TextAnalysisR::plot_term_trends_continuous(
                term_data = con_var_term_counts,
                continuous_var = input$continuous_var_3,
                terms = vm,
                height = input$height_frequency_trends
              )
            })
          })

          output$significance_results_table <- renderUI({
            req(input$continuous_var_3)
            req(vm)

            con_var_term_counts <- dfm_td %>%
              tibble::as_tibble() %>%
              group_by(!!rlang::sym(input$continuous_var_3), term) %>%
              summarise(word_frequency = sum(count), .groups = "drop")

            significance_results <- con_var_term_counts %>%
              mutate(word = term) %>%
              filter(word %in% vm) %>%
              group_by(word) %>%
              group_modify(~ {
                continuous_var <- if (is.null(input$continuous_var_3) ||
                                      length(input$continuous_var_3) == 0) {
                  stop("No continuous variable selected.")
                } else {
                  input$continuous_var_3[1]
                }

                tryCatch({
                  TextAnalysisR:::validate_column_name(continuous_var)
                }, error = function(e) {
                  TextAnalysisR:::log_security_event(
                    "INVALID_COLUMN_NAME",
                    paste("Invalid column name attempted:", continuous_var),
                    session,
                    "WARNING"
                  )
                  stop("Invalid column name format")
                })

                df <- .x %>%
                  dplyr::mutate(
                    word_frequency = as.numeric(word_frequency),
                    !!continuous_var := as.numeric(!!rlang::sym(continuous_var))
                  ) %>%
                  dplyr::filter(is.finite(word_frequency) &
                                  is.finite(!!rlang::sym(continuous_var)))

              if (length(unique(df$word_frequency)) <= 1) {
                return(tibble::tibble(
                  term = NA, estimate = NA, std.error = NA,
                  statistic = NA, p.value = NA,
                  `odds ratio` = NA, var.diag = NA,
                  `std.error (odds ratio)` = NA,
                  model_type = "Insufficient data"
                ))
              }

              if (length(unique(df[[continuous_var]])) <= 1) {
                return(tibble::tibble(
                  term = NA, estimate = NA, std.error = NA,
                  statistic = NA, p.value = NA,
                  `odds ratio` = NA, var.diag = NA,
                  `std.error (odds ratio)` = NA,
                  model_type = "Insufficient data"
                ))
              }

              if (nrow(df) < 2) {
                return(tibble::tibble(
                  term = NA, estimate = NA, std.error = NA,
                  statistic = NA, p.value = NA,
                  `odds ratio` = NA, var.diag = NA,
                  `std.error (odds ratio)` = NA,
                  model_type = "Insufficient data"
                ))
              }

              formula_simple <- as.formula(paste0("word_frequency ~ ", continuous_var))

              mean_count <- mean(df$word_frequency, na.rm = TRUE)
              var_count <- var(df$word_frequency, na.rm = TRUE)
              dispersion_ratio <- ifelse(mean_count != 0, var_count / mean_count, NA)
              prop_zero <- mean(df$word_frequency == 0, na.rm = TRUE)

              model <- NULL

              if (prop_zero > 0.5) {
                model <- tryCatch(
                  pscl::zeroinfl(formula_simple, data = df, dist = "negbin", link = "logit"),
                  error = function(e) {
                    return(NULL)
                  }
                )
                if (!is.null(model)) {
                  model_type <- "Zero-Inflated Negative Binomial"
                }
              }

              if (is.null(model)) {
                model <- tryCatch(
                  MASS::glm.nb(formula_simple, data = df, control = glm.control(maxit = 200)),
                  error = function(e) {
                    return(NULL)
                  }
                )
                if (is.null(model)) {
                  model <- glm(formula_simple, family = poisson(link = "log"), data = df)
                  model_type <- "Poisson"
                } else {
                  model_type <- "Negative Binomial"
                }
              }

              tidy_result <- broom::tidy(model) %>%
                dplyr::mutate(
                  `odds ratio` = exp(estimate),
                  var.diag = diag(vcov(model)),
                  `std.error (odds ratio)` = ifelse(var.diag >= 0,
                                                    sqrt(`odds ratio`^2 * var.diag),
                                                    NA),
                  model_type = model_type
                )

              return(tidy_result)
            }) %>%
            ungroup() %>%
            dplyr::select(
              word, model_type, term, estimate, std.error,
              `odds ratio`, `std.error (odds ratio)`, statistic, p.value
            ) %>%
            rename(
              logit = estimate,
              `z-statistic` = statistic
            )

            if (nrow(significance_results) > 0) {
              tables <- significance_results %>%
                mutate(word = factor(word, levels = vm)) %>%
                arrange(word) %>%
                group_by(word) %>%
                group_map(~ {

                  # Round all numeric columns to 3 decimal places
                  clean_data <- .x %>%
                    dplyr::mutate(
                      dplyr::across(where(is.numeric), ~ round(.x, 3))
                    )

                  htmltools::tagList(
                    htmltools::tags$div(
                      style = "margin-top: 20px; margin-bottom: 20px;",
                      htmltools::tags$p(
                        .y$word,
                        style = "font-weight: bold; text-align: center; font-size: 16px; font-family: 'Roboto', sans-serif; color: #0c1f4a;"
                      )
                    ),
                    DT::datatable(
                      clean_data,
                      rownames = FALSE,
                      extensions = "Buttons",
                      options = list(
                        scrollX = TRUE,
                        width = "100%",
                        dom = "Bfrtip",
                        buttons = c("copy", "csv", "excel", "pdf", "print"),
                        digits = 3
                      )
                    ) %>%
                      DT::formatStyle(
                        columns = names(clean_data),
                        `font-size` = "16px"
                      )
                  )
                })

              htmltools::tagList(tables)
            } else {
              htmltools::tagList(
                htmltools::tags$p("No significant results to display.")
              )
            }
          })
        }
      },
      error = function(e) {
        warning_message <- if (!is.null(e$message) && nchar(e$message) > 0) {
          e$message
        } else {
          "An error occurred while generating the term plot. Please check your data and try again."
        }
        print(paste("Term plot error:", warning_message))
        shiny::showModal(shiny::modalDialog(
          title = tags$div(
            style = "color: #dc2626;",
            icon("times-circle"),
            " Error Generating Term Plot"
          ),
          p(warning_message),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        NULL
      }
    )
    })
  })

  output$line_con_var_plot_uiOutput <- renderUI({
    req(input$plot_term > 0, input$continuous_var_3, input$type_terms)
    htmltools::tagList(
      plotly::plotlyOutput(
        "line_con_var_plot",
        height = input$height_frequency_trends,
        width = "100%"
      ),
      htmltools::tags$div(
        style = "margin-top: 20px; width: 100%;",
        uiOutput("significance_results_table")
      )
    )
  })

  sentiment_results <- reactiveValues(
    analyzed = FALSE,
    data = NULL,
    original_data = NULL,
    document_sentiment = NULL,
    emotion_scores = NULL,
    summary = NULL,
    grouped = NULL
  )

  observe({
    req(colnames_cat())
    categorical_vars <- colnames_cat()

    updateSelectizeInput(
      session,
      "sentiment_category_var",
      choices = c("None" = "None", categorical_vars),
      selected = "None"
    )

    updateSelectizeInput(
      session,
      "emotion_group_var",
      choices = c("None" = "None", categorical_vars),
      selected = "None"
    )
  })

  observe({
    req(united_tbl())

    col_names <- c("None" = "", names(united_tbl()))

    updateSelectizeInput(
      session,
      "sentiment_doc_id_var",
      choices = col_names,
      selected = ""
    )

    updateSelectizeInput(
      session,
      "readability_doc_id_var",
      choices = col_names,
      selected = ""
    )

    updateSelectizeInput(
      session,
      "lexdiv_doc_id_var",
      choices = col_names,
      selected = ""
    )
  })

  observeEvent(input$sentiment_category_var, {
    if (sentiment_results$analyzed && !is.null(input$sentiment_category_var) &&
        input$sentiment_category_var != "None") {

      doc_sentiment <- sentiment_results$document_data
      texts_df <- sentiment_results$original_data

      if (!is.null(texts_df) && input$sentiment_category_var %in% names(texts_df) &&
          !is.null(doc_sentiment) && "sentiment" %in% names(doc_sentiment)) {

        # Create row index for joining
        texts_df_with_idx <- texts_df %>%
          mutate(row_idx = row_number(),
                 doc_name = paste0("text", row_idx))

        doc_sentiment_with_group <- doc_sentiment %>%
          left_join(
            texts_df_with_idx %>% select(doc_name, !!rlang::sym(input$sentiment_category_var)),
            by = c("document" = "doc_name")
          ) %>%
          rename(category_var = !!rlang::sym(input$sentiment_category_var))

        grouped_sentiment <- doc_sentiment_with_group %>%
          filter(!is.na(category_var)) %>%
          group_by(category_var, sentiment) %>%
          summarize(n = n(), .groups = "drop") %>%
          group_by(category_var) %>%
          mutate(proportion = n / sum(n))

        sentiment_results$data <- doc_sentiment_with_group
        sentiment_results$grouped <- grouped_sentiment
      }


      current_plot_type <- input$category_plot_type %||% "bar"
      updateSelectInput(session, "category_plot_type",
                       choices = c("Bar Chart" = "bar",
                                  "Box Plot" = "box",
                                  "Violin Plot" = "violin"),
                       selected = current_plot_type)

    } else if (input$sentiment_category_var == "None") {

      sentiment_results$grouped <- NULL
      sentiment_results$data <- sentiment_results$document_data

      current_plot_type <- input$category_plot_type %||% "bar"
      updateSelectInput(session, "category_plot_type",
                       choices = c("Bar Chart" = "bar",
                                  "Box Plot" = "box",
                                  "Violin Plot" = "violin"),
                       selected = current_plot_type)
    }
  })

  observeEvent(input$emotion_group_var, {
    if (sentiment_results$analyzed && !is.null(input$emotion_group_var) &&
        input$emotion_group_var != "None") {

      doc_sentiment <- sentiment_results$data
      texts_df <- united_tbl()
      lexicon_name <- input$sentiment_lexicon %||% "bing"

      if (!is.null(texts_df) && input$emotion_group_var %in% names(texts_df) && lexicon_name == "nrc") {
        if (!"emotion_group_var" %in% names(doc_sentiment)) {
          doc_sentiment <- doc_sentiment %>%
            left_join(
              texts_df %>%
                mutate(document = row_number()) %>%
                select(document, emotion_group_var = !!sym(input$emotion_group_var)),
              by = "document"
            )
        }

        emotion_cols <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")
        available_emotions <- intersect(emotion_cols, names(doc_sentiment))

        if (length(available_emotions) > 0) {
          emotion_data_grouped <- doc_sentiment %>%
            select(document, emotion_group_var, all_of(available_emotions)) %>%
            pivot_longer(cols = all_of(available_emotions), names_to = "emotion", values_to = "score") %>%
            group_by(emotion_group_var, emotion) %>%
            summarize(total_score = sum(score, na.rm = TRUE), .groups = "drop")

          sentiment_results$emotion_scores_grouped <- emotion_data_grouped
          sentiment_results$data <- doc_sentiment
        }
      }
    } else if (sentiment_results$analyzed) {
      sentiment_results$emotion_scores_grouped <- NULL
    }
  })

  observeEvent(input$sentiment_setup_help, {
    showModal(modalDialog(
      title = "Sentiment Analysis Setup",
      size = "m",
      tags$div(
        tags$h4("First-time Setup", style = "color: #0c1f4a;"),
        tags$p("To use sentiment analysis, you need to install the textdata package and download the lexicons:"),
        tags$br(),
        tags$h5("Step 1: Install textdata package"),
        tags$pre("install.packages('textdata')"),
        tags$br(),
        tags$h5("Step 2: Download lexicons"),
        tags$p("Run these commands in your R console. You'll be prompted to confirm the download:"),
        tags$pre("library(tidytext)\nget_sentiments('bing')\nget_sentiments('afinn')\nget_sentiments('nrc')"),
        tags$br(),
        tags$div(
          style = "background-color: #EFF6FF; border: 1px solid #3B82F6; padding: 10px; border-radius: 4px;",
          tags$strong("Lexicon Descriptions:"),
          tags$ul(
            tags$li(tags$strong("AFINN:"), " Numeric sentiment scores from -5 (negative) to +5 (positive)"),
            tags$li(tags$strong("Bing:"), " Binary positive/negative classification"),
            tags$li(tags$strong("NRC:"), " Emotions (joy, fear, anger, etc.) plus positive/negative")
          )
        ),
        tags$br(),
        tags$p("After downloading, the lexicons will be cached locally and available for all future sessions.")
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })


  observeEvent(input$sentiment_lexicon, {
    if (sentiment_results$analyzed) {
      sentiment_results$analyzed <- FALSE
      sentiment_results$data <- NULL
      sentiment_results$document_data <- NULL
      sentiment_results$emotion_scores <- NULL
      sentiment_results$summary <- NULL
      sentiment_results$grouped <- NULL

      updateSelectizeInput(session, "sentiment_category_var", selected = "")
      updateSelectizeInput(session, "emotion_group_var", selected = "")

      showNotification(

        "Lexicon changed. Please click 'Analyze Sentiment' to run analysis with the new lexicon.",
        type = "message",
        duration = 5
      )
    }
  }, ignoreInit = TRUE)

  observeEvent(input$run_sentiment_analysis, {
    show_loading_notification("Running sentiment analysis...", id = "sentiment_loading")

    tryCatch({
      dfm_obj <- get_available_dfm()

      if (is.null(dfm_obj)) {
        remove_notification_by_id("sentiment_loading")
        show_no_dfm_notification()
        return()
      }

      texts_df <- tryCatch(united_tbl(), error = function(e) NULL)

      if (is.null(texts_df)) {
        remove_notification_by_id("sentiment_loading")
        showNotification("No text data available. Please complete document processing first.", type = "error", duration = 7)
        return()
      }

      lexicon_name <- input$sentiment_lexicon %||% "bing"
      feature_type <- input$semantic_feature_space %||% "words"
      ngram_range <- if (feature_type == "ngrams") as.numeric(input$semantic_ngram_range %||% "2") else 2

      if (feature_type == "embeddings") {
        if (!TextAnalysisR::check_feature("embeddings")) {
          remove_notification_by_id("sentiment_loading")
          showNotification("Embedding-based sentiment requires Python. Please use lexicon method.", type = "warning")
          return()
        }

        if (!"text" %in% names(texts_df)) {
          remove_notification_by_id("sentiment_loading")
          showNotification("Text column not found. Please unite text columns first.", type = "error")
          return()
        }

        texts_vec <- texts_df$text
        doc_names <- if ("doc_id" %in% names(texts_df)) texts_df$doc_id else quanteda::docnames(dfm_obj)

        sentiment_analysis_results <- tryCatch({
          TextAnalysisR::sentiment_embedding_analysis(
            texts = texts_vec,
            embeddings = embeddings_cache$embeddings,
            model_name = "distilbert-base-uncased-finetuned-sst-2-english",
            doc_names = doc_names,
            use_gpu = FALSE
          )
        }, error = function(e) {
          remove_notification_by_id("sentiment_loading")
          show_error_notification(
            paste0(
              "Embedding-based sentiment error: ", e$message, ". ",
              "Please ensure Python transformers library is installed. See Setup > Installation."
            )
          )
          return(NULL)
        })

        if (is.null(sentiment_analysis_results)) {
          return()
        }

      } else {
        tryCatch({
          test_lexicon <- tidytext::get_sentiments(lexicon_name)
        }, error = function(e) {
          if (grepl("textdata", e$message)) {
            stop(paste0("The textdata package is required to download sentiment lexicons.\n",
                       "Please install it with: install.packages('textdata')\n",
                       "After installation, run: tidytext::get_sentiments('", lexicon_name, "')\n",
                       "You will be prompted to download the lexicon on first use."))
          } else if (grepl("lexicon", e$message)) {
            stop(paste0("The ", lexicon_name, " lexicon needs to be downloaded first.\n",
                       "Run: tidytext::get_sentiments('", lexicon_name, "')\n",
                       "You will be prompted to confirm the download."))
          } else {
            stop(e$message)
          }
        })

        texts_vec <- if (feature_type == "ngrams" && "text" %in% names(texts_df)) {
          texts_df$text
        } else {
          NULL
        }

        sentiment_analysis_results <- TextAnalysisR::sentiment_lexicon_analysis(
          dfm_object = dfm_obj,
          lexicon = lexicon_name,
          texts_df = texts_df,
          feature_type = feature_type,
          ngram_range = ngram_range,
          texts = texts_vec
        )
      }

      doc_sentiment <- sentiment_analysis_results$document_sentiment
      emotion_data <- sentiment_analysis_results$emotion_scores
      summary_stats <- sentiment_analysis_results$summary_stats
      docs_analyzed <- summary_stats$documents_analyzed

      sentiment_results$analyzed <- TRUE
      sentiment_results$data <- doc_sentiment
      sentiment_results$document_data <- doc_sentiment
      sentiment_results$original_data <- texts_df
      sentiment_results$emotion_scores <- emotion_data
      sentiment_results$summary <- summary_stats
      sentiment_results$grouped <- NULL

      updateSliderInput(
        session,
        "sentiment_top_docs",
        max = docs_analyzed,
        value = min(10, docs_analyzed),
        label = paste0("Analyzed documents to show (", docs_analyzed, " available)")
      )

      remove_notification_by_id("sentiment_loading")
      show_completion_notification("Sentiment analysis complete!")
    }, error = function(e) {
      remove_notification_by_id("sentiment_loading")
      show_error_notification(paste("Error in sentiment analysis:", e$message))
    })
  })

  output$sentiment_distribution_plot <- plotly::renderPlotly({
    if (!sentiment_results$analyzed) {
      return(plot_error("Click 'Analyze Sentiment' to generate results"))
    }
    plot_sentiment_distribution(sentiment_results$data)
  })

  output$sentiment_by_category_plot <- plotly::renderPlotly({
    if (!sentiment_results$analyzed) {
      return(plot_error("Run sentiment analysis to see results"))
    } else if (!is.null(input$sentiment_category_var) && input$sentiment_category_var != "None" && !is.null(sentiment_results$grouped)) {
      plot_type <- if(is.null(input$category_plot_type)) "bar" else input$category_plot_type

      if (plot_type == "bar") {
        plot_sentiment_by_category(
          sentiment_results$data,
          category_var = "category_var",
          plot_type = "stacked",
          title = paste("Sentiment by", input$sentiment_category_var)
        )
      } else if (plot_type == "box") {
        TextAnalysisR::plot_sentiment_boxplot(
          sentiment_results$data,
          category_var = "category_var",
          title = paste("Sentiment Score by", input$sentiment_category_var)
        )
      } else {
        TextAnalysisR::plot_sentiment_violin(
          sentiment_results$data,
          category_var = "category_var",
          title = paste("Sentiment Score by", input$sentiment_category_var)
        )
      }
    } else {
      plot_error("Select a category variable to see analysis")
    }
  })

  output$sentiment_category_table <- renderDT({
    if (sentiment_results$analyzed && !is.null(input$sentiment_category_var) &&
        input$sentiment_category_var != "None" && !is.null(sentiment_results$grouped)) {
      datatable(
        sentiment_results$grouped %>%
          pivot_wider(names_from = sentiment, values_from = proportion, values_fill = 0) %>%
          mutate(across(where(is.numeric), ~ round(., 3))),
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    } else {
      return(NULL)
    }
  })

  output$sentiment_summary_table <- renderDT({
    if (!sentiment_results$analyzed) {
      return(NULL)
    } else {
      coverage_data <- data.frame(
        Metric = c("Total Documents in Dataset",
                   "Documents with Sentiment Words",
                   "Documents without Sentiment Words",
                   "Coverage Percentage"),
        Value = c(
          sentiment_results$summary$total_documents,
          paste0(sentiment_results$summary$documents_analyzed,
                 " (", round((sentiment_results$summary$documents_analyzed / sentiment_results$summary$total_documents) * 100, 1), "%)"),
          paste0(sentiment_results$summary$documents_without_sentiment,
                 " (", round((sentiment_results$summary$documents_without_sentiment / sentiment_results$summary$total_documents) * 100, 1), "%)"),
          paste0(sentiment_results$summary$coverage_percentage, "%")
        )
      )

      sentiment_data <- data.frame(
        Metric = c("Sentiment Distribution (of analyzed):",
                   "  Positive Documents",
                   "  Negative Documents",
                   "  Neutral Documents",
                   "  Average Sentiment Score"),
        Value = c("",
                  paste0(sentiment_results$summary$positive_docs,
                         " (", round((sentiment_results$summary$positive_docs / sentiment_results$summary$documents_analyzed) * 100, 1), "%)"),
                  paste0(sentiment_results$summary$negative_docs,
                         " (", round((sentiment_results$summary$negative_docs / sentiment_results$summary$documents_analyzed) * 100, 1), "%)"),
                  paste0(sentiment_results$summary$neutral_docs,
                         " (", round((sentiment_results$summary$neutral_docs / sentiment_results$summary$documents_analyzed) * 100, 1), "%)"),
                  round(sentiment_results$summary$avg_sentiment_score, 3)
        )
      )

      combined_data <- rbind(coverage_data, sentiment_data)

      datatable(combined_data,
                options = list(
                  dom = 'Bt',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 15,
                  scrollX = TRUE
                ),
                rownames = FALSE) %>%
        DT::formatStyle(
          'Metric',
          target = 'row',
          backgroundColor = styleEqual(
            c("Sentiment Distribution (of analyzed):"),
            c("#f8f9fa")
          ),
          fontWeight = styleEqual(
            c("Sentiment Distribution (of analyzed):"),
            c("bold")
          ),
          color = styleEqual(
            c("Sentiment Distribution (of analyzed):"),
            c("#0c1f4a")
          )
        ) %>%
        DT::formatStyle(columns = names(combined_data), `font-size` = "16px")
    }
  })

  output$document_sentiment_plot <- plotly::renderPlotly({
    if (!sentiment_results$analyzed) {
      return(plot_error("Run sentiment analysis to see results"))
    }

    doc_ids <- NULL
    text_preview <- NULL

    if (!is.null(input$sentiment_doc_id_var) && input$sentiment_doc_id_var != "" &&
        input$sentiment_doc_id_var != "None" && input$sentiment_doc_id_var %in% names(sentiment_results$original_data)) {
      doc_ids <- as.character(sentiment_results$original_data[[input$sentiment_doc_id_var]])
    }

    text_col <- input$sentiment_text_var %||% "text"
    if (text_col %in% names(sentiment_results$original_data)) {
      doc_texts <- sentiment_results$original_data[[text_col]]
      text_preview <- sapply(doc_texts, wrap_text_for_tooltip, USE.NAMES = FALSE)
    }

    plot_document_sentiment_trajectory(
      sentiment_data = sentiment_results$data,
      top_n = input$sentiment_top_docs,
      doc_ids = doc_ids,
      text_preview = text_preview
    )
  })

  output$document_sentiment_table <- renderDT({
    if (!sentiment_results$analyzed) {
      return(NULL)
    } else if (nrow(sentiment_results$data) == 0) {
      datatable(
        data.frame(Message = "No documents contained sentiment words from the selected lexicon."),
        options = list(dom = 't', pageLength = 1),
        rownames = FALSE
      )
    } else {
      doc_table <- sentiment_results$data %>%
        arrange(document) %>%
        mutate(
          Document = paste("Doc", document),
          Score = round(sentiment_score, 2)
        )

      if (!is.null(input$sentiment_doc_id_var) && input$sentiment_doc_id_var != "" &&
          input$sentiment_doc_id_var != "None" && input$sentiment_doc_id_var %in% names(sentiment_results$original_data)) {
        doc_ids <- as.character(sentiment_results$original_data[[input$sentiment_doc_id_var]])
        doc_table$`Document ID` <- doc_ids[doc_table$document]

        doc_table <- doc_table %>%
          select(Document, `Document ID`, Sentiment = sentiment, Score)
      } else {
        doc_table <- doc_table %>%
          select(Document, Sentiment = sentiment, Score)
      }

      datatable(doc_table,
                options = list(
                  pageLength = 10,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  scrollX = TRUE,
                  lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
                ),
                rownames = FALSE) %>%
        formatStyle("Sentiment",
                   backgroundColor = styleEqual(
                     c("positive", "negative", "neutral"),
                     c("#D1FAE5", "#FEE2E2", "#F3F4F6")
                   ))
    }
  })

  output$emotion_radar_plot <- plotly::renderPlotly({
    if (!sentiment_results$analyzed) {
      return(plot_error("Run sentiment analysis to see results"))
    }

    if (is.null(sentiment_results$emotion_scores)) {
      return(plot_error("Select 'NRC' lexicon to see emotion analysis"))
    }

    normalize <- !is.null(input$normalize_emotions) && input$normalize_emotions

    if (!is.null(sentiment_results$emotion_scores_grouped) &&
        !is.null(input$emotion_group_var) && input$emotion_group_var != "None") {
      plot_emotion_radar(
        emotion_data = sentiment_results$emotion_scores_grouped,
        group_var = "emotion_group_var",
        normalize = normalize,
        title = paste("Emotion Analysis by", input$emotion_group_var)
      )
    } else {
      plot_emotion_radar(
        emotion_data = sentiment_results$emotion_scores,
        normalize = normalize
      )
    }
  })

  output$emotion_scores_table <- renderDT({
    if (!sentiment_results$analyzed) {
      return(NULL)
    } else if (!is.null(sentiment_results$emotion_scores_grouped) &&
               !is.null(input$emotion_group_var) && input$emotion_group_var != "None") {
      grouped_data <- sentiment_results$emotion_scores_grouped

      if (!is.null(input$normalize_emotions) && input$normalize_emotions) {
        grouped_data <- grouped_data %>%
          group_by(emotion_group_var) %>%
          mutate(
            total_score = if (max(total_score, na.rm = TRUE) > 0) {
              round((total_score / max(total_score, na.rm = TRUE)) * 100, 2)
            } else {
              total_score
            }
          ) %>%
          ungroup()
      }

      emotion_table <- grouped_data %>%
        arrange(emotion_group_var, desc(total_score)) %>%
        mutate(
          Category = emotion_group_var,
          Emotion = str_to_title(emotion),
          Score = total_score
        ) %>%
        select(Category, Emotion, Score) %>%
        pivot_wider(names_from = Emotion, values_from = Score, values_fill = 0)

      datatable(emotion_table,
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 10,
                  scrollX = TRUE
                ),
                rownames = FALSE)
    } else if (!is.null(sentiment_results$emotion_scores)) {
      emotion_data <- sentiment_results$emotion_scores

      if (!is.null(input$normalize_emotions) && input$normalize_emotions) {
        max_score <- max(emotion_data$total_score, na.rm = TRUE)
        if (max_score > 0) {
          emotion_data <- emotion_data %>%
            mutate(total_score = round((total_score / max_score) * 100, 2))
        }
      }

      emotion_table <- emotion_data %>%
        arrange(desc(total_score)) %>%
        mutate(
          Emotion = str_to_title(emotion),
          Score = total_score,
          Percentage = round((total_score / sum(total_score)) * 100, 1)
        ) %>%
        select(Emotion, Score, Percentage)

      datatable(emotion_table,
                options = list(
                  dom = 'Bt',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 10,
                  scrollX = TRUE
                ),
                rownames = FALSE) %>%
        formatStyle("Score",
                   background = styleColorBar(emotion_table$Score, "#E0E7FF"),
                   backgroundSize = '98% 80%',
                   backgroundRepeat = 'no-repeat',
                   backgroundPosition = 'center')
    } else {
      datatable(
        data.frame(Message = "Select NRC lexicon to see emotion scores"),
        options = list(dom = 't', pageLength = 1),
        rownames = FALSE
      )
    }
  })

  output$sentiment_overall_uiOutput <- renderUI({
    if (sentiment_results$analyzed) {
      tagList(
        br(),
        tags$h5(strong("Overall Sentiment Distribution"), style = "color: #0c1f4a;"),
        fluidRow(
          column(12,
            plotly::plotlyOutput("sentiment_distribution_plot", height = "400px", width = "100%")
          )
        ),
        br(),
        tags$h5(strong("Summary Statistics"), style = "color: #0c1f4a;"),
        DT::dataTableOutput("sentiment_summary_table")
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-chart-bar", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Configure settings and click ",
            tags$strong("'Analyze Sentiment'", style = "color: #0c1f4a;"),
            " to generate results",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  output$sentiment_category_uiOutput <- renderUI({
    if (sentiment_results$analyzed && !is.null(input$sentiment_category_var) && input$sentiment_category_var != "None") {
      tagList(
        br(),
        tags$h5(strong("Sentiment by Category"), style = "color: #0c1f4a;"),
        fluidRow(
          column(12,
            plotly::plotlyOutput("sentiment_by_category_plot", height = "400px", width = "100%")
          )
        ),
        br(),
        tags$h5(strong("Category Comparison Table"), style = "color: #0c1f4a;"),
        DT::dataTableOutput("sentiment_category_table")
      )
    } else if (sentiment_results$analyzed) {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-layer-group", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Select a category variable in the sidebar to see sentiment analysis by category",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-arrow-left", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Run sentiment analysis from the ",
            tags$strong("Overall Sentiment", style = "color: #0c1f4a;"),
            " tab first",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  output$sentiment_document_uiOutput <- renderUI({
    if (sentiment_results$analyzed) {
      tagList(
        br(),
        plotly::plotlyOutput("document_sentiment_plot", height = "400px", width = "100%"),
        br(),
        DT::dataTableOutput("document_sentiment_table")
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-arrow-left", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Run sentiment analysis from the ",
            tags$strong("Overall Sentiment", style = "color: #0c1f4a;"),
            " tab first",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  output$emotion_ready_indicator <- renderUI({
    if (sentiment_results$analyzed && !is.null(sentiment_results$emotion_scores)) {
      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), "Emotion analysis data is available."
      )
    } else {
      NULL
    }
  })

  output$emotion_warning_message <- renderUI({
    if (!sentiment_results$analyzed || is.null(sentiment_results$emotion_scores)) {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; padding: 10px; margin-bottom: 15px; border-radius: 4px;",
        tags$i(class = "fa fa-exclamation-triangle", style = "color: #F59E0B; margin-right: 5px;"),
        tags$strong("Important:", style = "color: #92400E;"),
        tags$span(" To see emotion analysis, you must:", style = "color: #92400E;"),
        tags$ol(
          style = "margin-top: 8px; margin-bottom: 0; padding-left: 20px; color: #92400E;",
          tags$li("Go to 'Overall Sentiment' tab"),
          tags$li("Select 'NRC' lexicon"),
          tags$li("Click 'Analyze Sentiment'")
        )
      )
    } else {
      NULL
    }
  })

  output$sentiment_emotion_uiOutput <- renderUI({
    if (sentiment_results$analyzed && !is.null(sentiment_results$emotion_scores)) {
      tagList(
        br(),
        fluidRow(
          column(12,
            plotly::plotlyOutput("emotion_radar_plot", height = "500px", width = "100%")
          )
        ),
        br(),
        fluidRow(
          column(12,
            DT::dataTableOutput("emotion_scores_table")
          )
        )
      )
    } else if (sentiment_results$analyzed) {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-book", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "See ",
            tags$strong("NRC Lexicon", style = "color: #0c1f4a;"),
            " setup instructions in the sidebar",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-arrow-left", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Run sentiment analysis from the ",
            tags$strong("Overall Sentiment", style = "color: #0c1f4a;"),
            " tab first",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  observeEvent(input$showDFMInfo, {
    show_guide_modal("dfm_guide", "Document-Feature Matrix (DFM) Guide")
  })

  observeEvent(input$showPOSInfo, {
    show_guide_modal("pos_guide", "Part-of-Speech (POS) Tags Guide")
  })

  observeEvent(input$showNERInfo, {
    show_guide_modal("ner_guide", "Named Entity Recognition (NER) Types Guide")
  })

  observeEvent(input$showReadabilityMetricsInfo, {
    show_guide_modal("readability_metrics_guide", "Readability Metrics Guide")
  })

  observeEvent(input$showLexDivMetricsInfo, {
    show_guide_modal("lexical_diversity_guide", "Lexical Diversity Metrics Guide")
  })

  observeEvent(input$showSTMInfo, {
    show_guide_modal("stm_guide", "Structural Topic Model (STM) Guide")
  })

  observeEvent(input$showEmbeddingTopicsInfo, {
    show_guide_modal("embedding_topics_guide", "Embedding-based Topic Modeling Guide")
  })

  observeEvent(input$showHybridTopicsInfo, {
    show_guide_modal("hybrid_topics_guide", "Hybrid Topic Modeling Guide")
  })

  readability_results <- reactiveValues(
    analyzed = FALSE,
    data = NULL,
    metrics_data = NULL,
    comparison_data = NULL
  )

  observe({
    req(colnames_cat())
    updateSelectizeInput(
      session,
      "readability_group_var",
      choices = c("None" = "None", colnames_cat()),
      selected = "None"
    )
  })

  output$readability_metric_selector_ui <- renderUI({
    if (!readability_results$analyzed) {
      return(NULL)
    }

    scores_data <- readability_results$metrics_data
    metric_names <- names(scores_data)[names(scores_data) != "Document"]
    metric_names <- metric_names[!is.na(metric_names) & metric_names != ""]

    if (length(metric_names) == 0) {
      return(NULL)
    }

    metric_labels <- c(
      "flesch" = "Flesch Reading Ease",
      "flesch_kincaid" = "Flesch-Kincaid Grade",
      "gunning_fog" = "Gunning Fog",
      "smog" = "SMOG",
      "ari" = "Automated Readability Index",
      "coleman_liau" = "Coleman-Liau"
    )

    display_names <- sapply(metric_names, function(m) {
      if (m %in% names(metric_labels)) {
        metric_labels[[m]]
      } else {
        m
      }
    }, USE.NAMES = FALSE)

    metric_choices <- setNames(metric_names, display_names)
    metric_choices <- metric_choices[!is.na(names(metric_choices)) & !is.na(metric_choices)]

    if (length(metric_choices) == 0) {
      return(NULL)
    }

    selectInput(
      "selected_readability_metric",
      "Select metric to visualize:",
      choices = metric_choices,
      selected = metric_names[1]
    )
  })

  observeEvent(input$run_readability_analysis, {
    req(united_tbl())

    dfm_to_use <- NULL

    tryCatch({
      dfm_to_use <- dfm_final()
    }, error = function(e) {
      tryCatch({
        dfm_to_use <<- dfm_init()
      }, error = function(e2) {
        dfm_to_use <<- NULL
      })
    })

    if (is.null(dfm_to_use)) {
      output$dfm_required_message <- renderPrint({
        cat(get_dfm_setup_instructions("readability analysis"), sep = "\n")
      })

      show_dfm_instructions_modal("dfm_required_message")
      return(NULL)
    }

    show_loading_notification("Running readability analysis...", id = "readability_loading")

    tryCatch({
      texts <- united_tbl()$united_texts
      doc_names <- paste0("Doc ", seq_len(nrow(united_tbl())))

      selected_metrics <- input$readability_metrics

      if(is.null(selected_metrics) || length(selected_metrics) == 0) {
        selected_metrics <- c("flesch", "flesch_kincaid", "gunning_fog")
      }

      selected_metrics <- unname(as.character(selected_metrics))

      readability_scores <- TextAnalysisR::calculate_text_readability(
        texts = texts,
        metrics = selected_metrics,
        include_lexical_diversity = FALSE,
        include_sentence_stats = FALSE,
        dfm_for_lexdiv = dfm_to_use,
        doc_names = doc_names
      )

      corp <- quanteda::corpus(texts)
      quanteda::docnames(corp) <- doc_names

      readability_results$analyzed <- TRUE
      readability_results$metrics_data <- readability_scores
      readability_results$original_data <- united_tbl()
      readability_results$data <- list(
        scores = readability_scores,
        corpus = corp,
        metrics = selected_metrics,
        warning = "Note: Traditional readability formulas (Flesch, FOG, etc.) measure surface-level features but don't capture semantic complexity, cohesion, or domain familiarity. TTR is sensitive to text length; MTLD is more reliable for lexical diversity. Modern best practice (2025) combines traditional metrics with lexical diversity (MTLD), syntactic complexity, and semantic coherence measures. Use these as complementary indicators, not absolute measures of comprehension."
      )

      remove_notification_by_id("readability_loading")
      show_completion_notification("Readability analysis complete!", duration = 5)
    }, error = function(e) {
      remove_notification_by_id("readability_loading")
      show_error_notification(paste("Error in readability analysis:", e$message))
    })
  })

  output$readability_plot <- plotly::renderPlotly({
    if (!readability_results$analyzed) {
      return(plot_error("Click 'Analyze' button to generate results"))
    }

    scores_data <- readability_results$metrics_data
    selected_metric <- input$selected_readability_metric
    view_type <- input$readability_view_type %||% "distribution"

    if (is.null(selected_metric) || is.na(selected_metric)) {
      metric_names <- names(scores_data)[names(scores_data) != "Document"]
      metric_names <- metric_names[!is.na(metric_names)]
      if (length(metric_names) == 0) {
        return(plot_error("No valid metrics available"))
      }
      selected_metric <- metric_names[1]
    }

    if (is.na(selected_metric) || !selected_metric %in% names(scores_data)) {
      return(plot_error("Selected metric not available"))
    }

    if(view_type == "group") {
      group_var <- input$readability_group_var

      if(is.null(group_var) || group_var == "" || group_var == "None" ||
         !group_var %in% names(readability_results$original_data)) {
        return(plot_error("Please select a group variable in 'Group by'"))
      }

      group_vals <- as.character(readability_results$original_data[[group_var]])
      scores_data[[group_var]] <- group_vals

      plot_readability_by_group(scores_data, selected_metric, group_var)

    } else if(view_type == "document") {
      top_n <- input$readability_top_n %||% 15
      plot_top_readability_documents(scores_data, selected_metric, top_n = top_n)

    } else {
      plot_readability_distribution(scores_data, selected_metric)
    }
  })

  output$readability_metrics_table <- renderDT({
    if (!readability_results$analyzed) {
      return(NULL)
    } else {
      scores_data <- readability_results$metrics_data
      view_type <- input$readability_view_type %||% "distribution"

      if (!is.null(input$readability_doc_id_var) && input$readability_doc_id_var != "" &&
          input$readability_doc_id_var != "None" && input$readability_doc_id_var %in% names(readability_results$original_data)) {
        doc_ids <- as.character(readability_results$original_data[[input$readability_doc_id_var]])
        scores_data$`Document ID` <- doc_ids
      }

      if (view_type == "group") {
        group_var <- input$readability_group_var
        if (!is.null(group_var) && group_var != "" && group_var != "None" &&
            group_var %in% names(readability_results$original_data)) {
          group_vals <- as.character(readability_results$original_data[[group_var]])
          scores_data[[group_var]] <- group_vals

          if ("Document ID" %in% names(scores_data)) {
            scores_data <- scores_data %>%
              select(Document, `Document ID`, !!sym(group_var), everything()) %>%
              arrange(!!sym(group_var))
          } else {
            scores_data <- scores_data %>%
              select(Document, !!sym(group_var), everything()) %>%
              arrange(!!sym(group_var))
          }
        }
      } else if ("Document ID" %in% names(scores_data)) {
        scores_data <- scores_data %>%
          select(Document, `Document ID`, everything())
      }

      datatable(
        scores_data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = names(scores_data)[!names(scores_data) %in% c("Document", "Document ID", input$readability_group_var)], digits = 2)
    }
  })

  output$readability_results_uiOutput <- renderUI({
    if (readability_results$analyzed) {
      tagList(
        uiOutput("readability_metric_selector_ui"),
        br(),
        plotly::plotlyOutput("readability_plot", height = "500px", width = "100%"),
        br(),
        DT::dataTableOutput("readability_metrics_table")
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-book-reader", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Configure settings and click ",
            tags$strong("'Analyze'", style = "color: #0c1f4a;"),
            " button to generate results",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  lexical_diversity_results <- reactiveValues(
    analyzed = FALSE,
    data = NULL,
    summary = NULL,
    selected_metric = "TTR",
    original_data = NULL
  )

  output$lexical_diversity_uiOutput <- renderUI({
    if (lexical_diversity_results$analyzed) {
      # Get available metrics from data
      available_metrics <- setdiff(names(lexical_diversity_results$data), "document")

      # Create choices with display labels
      metric_labels <- c(
        "MTLD" = "MTLD (Most Recommended)",
        "MATTR" = "MATTR (Recommended)",
        "MSTTR" = "MSTTR (Mean Segmental TTR)",
        "TTR" = "TTR (Type-Token Ratio)",
        "CTTR" = "CTTR (Corrected TTR)",
        "C" = "Herdan's C",
        "R" = "Guiraud's R",
        "Maas" = "Maas",
        "K" = "Yule's K",
        "D" = "Simpson's D",
        "U" = "Dugast's U",
        "S" = "Summer's S",
        "I" = "Yule's I",
        "Vm" = "Herdan's Vm"
      )

      # Filter to only available metrics
      metric_choices <- available_metrics
      names(metric_choices) <- sapply(available_metrics, function(m) {
        if (m %in% names(metric_labels)) metric_labels[[m]] else m
      })

      tagList(
        fluidRow(
          column(12,
            selectInput(
              "lexdiv_metric_select",
              "Select Metric to Visualize:",
              choices = metric_choices,
              selected = lexical_diversity_results$selected_metric
            )
          )
        ),
        br(),
        plotly::plotlyOutput("lexical_diversity_plot", height = "500px", width = "100%"),
        br(),
        DT::dataTableOutput("lexical_diversity_table")
      )
    } else {
      tagList(
        tags$div(
          style = "padding: 60px 40px; text-align: center;",
          tags$div(
            style = "max-width: 500px; margin: 0 auto;",
            tags$i(class = "fa fa-chart-line", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
            tags$p(
              "Click ",
              tags$strong("'Analyze'", style = "color: #0c1f4a;"),
              " in the sidebar to calculate lexical diversity metrics",
              style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
            )
          )
        )
      )
    }
  })

  observeEvent(input$run_lexdiv_analysis, {
    tokens_to_use <- NULL

    tryCatch({
      tokens_to_use <- lemmatized_tokens()
    }, error = function(e) {
      tryCatch({
        tokens_to_use <<- processed_tokens()
      }, error = function(e2) {
        tryCatch({
          tokens_to_use <<- final_tokens()
        }, error = function(e3) {
          tryCatch({
            tokens_to_use <<- dfm_tokens()
          }, error = function(e4) {
            tryCatch({
              tokens_to_use <<- preprocessed_combined()
            }, error = function(e5) {
              tokens_to_use <<- NULL
            })
          })
        })
      })
    })

    if (is.null(tokens_to_use)) {
      output$lexdiv_required_message <- renderPrint({
        cat("Lexical Diversity Analysis requires tokens.\n\n",
            "Minimum Required Steps:\n",
            "  1. Go to 'Preprocessing' tab\n",
            "  2. Upload data and unite text columns (Step 1)\n",
            "  3. Create DFM (Step 6: Document-Feature Matrix)\n\n",
            "Optional Steps (for advanced analysis):\n",
            "  • Step 2: Segment Texts (Preprocess)\n",
            "  • Step 3: Remove Stopwords\n",
            "  • Step 4: Multi-Words\n",
            "  • Step 5: Word Forms (Lemmas)\n\n",
            "Then return to Lexical Analysis → Lexical Diversity and click 'Analyze'", sep = "")
      })
      show_dfm_instructions_modal("lexdiv_required_message")
      return(NULL)
    }

    show_loading_notification("Calculating lexical diversity metrics...", id = "lexdiv_loading")

    tryCatch({
      # Get selected metrics from checkboxGroup, default to all if none selected
      selected_metrics <- input$lexdiv_metrics
      if (is.null(selected_metrics) || length(selected_metrics) == 0) {
        selected_metrics <- "all"
      }

      # Get texts for average sentence length calculation
      texts_for_lexdiv <- NULL
      tryCatch({
        text_col <- input$text_column
        if (!is.null(text_col) && text_col %in% names(united_tbl())) {
          texts_for_lexdiv <- as.character(united_tbl()[[text_col]])
        }
      }, error = function(e) {
        texts_for_lexdiv <- NULL
      })

      result <- TextAnalysisR::lexical_diversity_analysis(
        x = tokens_to_use,
        measures = selected_metrics,
        texts = texts_for_lexdiv
      )

      lexical_diversity_results$analyzed <- TRUE
      lexical_diversity_results$data <- result$lexical_diversity
      lexical_diversity_results$summary <- result$summary_stats
      lexical_diversity_results$original_data <- united_tbl()

      # Set selected_metric to first available metric from selection
      available_metrics <- setdiff(names(result$lexical_diversity), "document")
      if (length(available_metrics) > 0) {
        lexical_diversity_results$selected_metric <- available_metrics[1]
      } else {
        lexical_diversity_results$selected_metric <- "TTR"
      }

      remove_notification_by_id("lexdiv_loading")
      show_completion_notification("Lexical diversity analysis completed successfully!")

    }, error = function(e) {
      remove_notification_by_id("lexdiv_loading")
      show_error_notification(paste0("Error analyzing lexical diversity: ", e$message))
    })
  })

  output$lexical_diversity_plot <- plotly::renderPlotly({
    req(lexical_diversity_results$analyzed)
    req(lexical_diversity_results$data)

    metric <- input$lexdiv_metric_select %||% lexical_diversity_results$selected_metric

    if (!(metric %in% names(lexical_diversity_results$data))) {
      return(create_error_plot("Selected metric not available"))
    }

    tryCatch({
      TextAnalysisR::plot_lexical_diversity_distribution(
        lexdiv_data = lexical_diversity_results$data,
        metric = metric
      )
    }, error = function(e) {
      create_error_plot(paste("Error creating plot:", e$message))
    })
  })

  output$lexical_diversity_table <- DT::renderDataTable({
    req(lexical_diversity_results$analyzed)
    req(lexical_diversity_results$data)

    lex_data <- lexical_diversity_results$data

    # Add Document ID if selected
    if (!is.null(input$lexdiv_doc_id_var) && input$lexdiv_doc_id_var != "" &&
        input$lexdiv_doc_id_var != "None" && !is.null(lexical_diversity_results$original_data) &&
        input$lexdiv_doc_id_var %in% names(lexical_diversity_results$original_data)) {
      doc_ids <- as.character(lexical_diversity_results$original_data[[input$lexdiv_doc_id_var]])
      lex_data$`Document ID` <- doc_ids
      # Reorder columns to put Document ID after document
      col_order <- c("document", "Document ID", setdiff(names(lex_data), c("document", "Document ID")))
      lex_data <- lex_data[, col_order, drop = FALSE]
    }

    # Rename 'document' column to 'Document' for consistency with Readability
    if ("document" %in% names(lex_data)) {
      names(lex_data)[names(lex_data) == "document"] <- "Document"
    }

    numeric_cols <- names(lex_data)[!names(lex_data) %in% c("Document", "Document ID")]

    datatable(
      lex_data,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = numeric_cols, digits = 3)
  })

  keyword_results <- reactiveValues(
    analyzed = FALSE,
    data = NULL,
    tfidf_data = NULL,
    keyness_data = NULL
  )

  observeEvent(input$run_keyword_extraction, {
    dfm_to_use <- NULL

    tryCatch({
      dfm_to_use <- dfm_final()
    }, error = function(e) {
      tryCatch({
        dfm_to_use <<- dfm_init()
      }, error = function(e2) {
        dfm_to_use <<- NULL
      })
    })

    if (is.null(dfm_to_use)) {
      output$keywords_extraction_required_message <- renderPrint({
        cat(get_dfm_setup_instructions("keyword extraction"), sep = "\n")
      })

      show_dfm_instructions_modal("keywords_extraction_required_message")
      return(NULL)
    }

    show_loading_notification("Extracting keywords using TF-IDF and statistical keyness...", id = "keyword_loading")

    tryCatch({
      dfm_obj <- dfm_to_use

      tfidf_df <- extract_keywords_tfidf(
        dfm_obj,
        top_n = input$tfidf_top_n %||% 20,
        normalize = isTRUE(input$tfidf_normalize)
      )

      if(quanteda::ndoc(dfm_obj) > 1) {
        group_var <- input$tfidf_group_var
        keyness_top_n <- input$textrank_top_n %||% 15

        if (!is.null(group_var) && group_var != "None" && group_var != "") {
          dfm_docvars <- quanteda::docvars(dfm_obj)

          if (group_var %in% names(dfm_docvars)) {
            group_values <- dfm_docvars[[group_var]]
            unique_groups <- unique(group_values[!is.na(group_values)])

            if (length(unique_groups) >= 2) {
              target_docs <- which(group_values == unique_groups[1])
              keyness_df <- extract_keywords_keyness(dfm_obj, target = target_docs, top_n = keyness_top_n)
            } else {
              keyness_df <- data.frame(Keyword = character(), Keyness_Score = numeric())
            }
          } else {
            target_docs <- 1:ceiling(quanteda::ndoc(dfm_obj)/2)
            keyness_df <- extract_keywords_keyness(dfm_obj, target = target_docs, top_n = keyness_top_n)
          }
        } else {
          target_docs <- 1:ceiling(quanteda::ndoc(dfm_obj)/2)
          keyness_df <- extract_keywords_keyness(dfm_obj, target = target_docs, top_n = keyness_top_n)
        }
      } else {
        keyness_df <- data.frame(Keyword = character(), Keyness_Score = numeric())
      }

      keyword_results$analyzed <- TRUE
      keyword_results$tfidf_data <- tfidf_df
      keyword_results$keyness_data <- keyness_df
      keyword_results$normalized <- isTRUE(input$tfidf_normalize)
      keyword_results$group_var <- if (!is.null(input$tfidf_group_var) && input$tfidf_group_var != "None") input$tfidf_group_var else NULL
      keyword_results$data <- list(
        tfidf = tfidf_df,
        keyness = keyness_df,
        dfm = dfm_obj,
        normalized = isTRUE(input$tfidf_normalize),
        group_var = if (!is.null(input$tfidf_group_var) && input$tfidf_group_var != "None") input$tfidf_group_var else NULL,
        note = "TF-IDF identifies important terms based on term frequency and inverse document frequency, balancing term prominence with distinctiveness across documents. For multi-document collections, keyness analysis (log-likelihood ratio G²) compares document subsets to identify statistically significant keywords. These lexical-statistical methods are efficient, interpretable, and well-established in corpus linguistics research."
      )

      remove_notification_by_id("keyword_loading")
      show_completion_notification("Keyword extraction complete!", duration = 5)
    }, error = function(e) {
      remove_notification_by_id("keyword_loading")
      show_error_notification(paste("Error in keyword extraction:", e$message))
    })
  })

  output$tfidf_keywords_plot <- plotly::renderPlotly({
    if (!keyword_results$analyzed) {
      return(plot_error("Click 'Extract' button to generate results"))
    }
    plot_tfidf_keywords(
      keyword_results$tfidf_data,
      normalized = isTRUE(keyword_results$normalized)
    )
  })

  output$tfidf_keywords_table <- renderDT({
    if (!keyword_results$analyzed) {
      return(NULL)
    } else {
      tfidf_data_desc <- keyword_results$tfidf_data[order(keyword_results$tfidf_data$TF_IDF_Score, decreasing = TRUE), ]
      datatable(
        tfidf_data_desc,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          order = list(),
          scrollX = TRUE,
          width = "100%"
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = "TF_IDF_Score", digits = 4) %>%
        DT::formatStyle(columns = names(tfidf_data_desc), `font-size` = "16px")
    }
  })

  output$textrank_keywords_plot <- plotly::renderPlotly({
    if (!keyword_results$analyzed) {
      return(plot_error("Run keyword extraction to see results"))
    }
    plot_keyness_keywords(
      keyword_results$keyness_data,
      group_label = keyword_results$group_var
    )
  })

  output$textrank_keywords_table <- renderDT({
    if (!keyword_results$analyzed) {
      return(NULL)
    } else {
      if(nrow(keyword_results$keyness_data) == 0) {
        datatable(
          data.frame(Message = "Keyness analysis requires multiple documents for comparison."),
          options = list(dom = 't', pageLength = 1),
          rownames = FALSE
        )
      } else {
        keyness_data_desc <- keyword_results$keyness_data[order(abs(keyword_results$keyness_data$Keyness_Score), decreasing = TRUE), ]
        datatable(
          keyness_data_desc,
          options = list(
            pageLength = 20,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            order = list(),
            scrollX = TRUE,
            width = "100%"
          ),
          rownames = FALSE
        ) %>%
          formatRound(columns = "Keyness_Score", digits = 2) %>%
          DT::formatStyle(columns = names(keyness_data_desc), `font-size` = "16px")
      }
    }
  })

  output$keywords_tfidf_uiOutput <- renderUI({
    if (keyword_results$analyzed) {
      tagList(
        br(),
        plotly::plotlyOutput("tfidf_keywords_plot", height = "500px", width = "100%"),
        br(),
        DT::dataTableOutput("tfidf_keywords_table")
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-key", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Configure settings and click ",
            tags$strong("'Extract'", style = "color: #0c1f4a;"),
            " button to generate results",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  output$keywords_textrank_uiOutput <- renderUI({
    if (keyword_results$analyzed) {
      tagList(
        br(),
        plotly::plotlyOutput("textrank_keywords_plot", height = "500px", width = "100%"),
        br(),
        DT::dataTableOutput("textrank_keywords_table")
      )
    } else{
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-key", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Configure settings and click ",
            tags$strong("'Extract'", style = "color: #0c1f4a;"),
            " button to generate results",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })

  output$keyword_trends_plot <- plotly::renderPlotly({
    if (!keyword_results$analyzed) {
      return(NULL)
    }
    plot_keyword_comparison(
      keyword_results$tfidf_data,
      top_n = input$comparison_top_n %||% 10,
      normalized = isTRUE(keyword_results$normalized)
    )
  })

  output$keyword_comparison_table <- renderDT({
    if (!keyword_results$analyzed) {
      return(NULL)
    } else {
      tfidf_df_sorted <- keyword_results$tfidf_data[order(keyword_results$tfidf_data$TF_IDF_Score, decreasing = TRUE), ]

      datatable(
        tfidf_df_sorted,
        options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          order = list(),
          scrollX = TRUE,
          width = "100%"
        ),
        rownames = FALSE
      ) %>%
        formatRound(columns = "TF_IDF_Score", digits = 4) %>%
        DT::formatStyle(columns = names(tfidf_df_sorted), `font-size` = "16px")
    }
  })

  output$keywords_comparison_uiOutput <- renderUI({
    if (keyword_results$analyzed) {
      tagList(
        br(),
        plotly::plotlyOutput("keyword_trends_plot", height = "500px", width = "100%"),
        br(),
        DT::dataTableOutput("keyword_comparison_table")
      )
    } else {
      tags$div(
        style = "padding: 60px 40px; text-align: center;",
        tags$div(
          style = "max-width: 400px; margin: 0 auto;",
          tags$i(class = "fa fa-key", style = "font-size: 48px; color: #CBD5E1; margin-bottom: 20px; display: block;"),
          tags$p(
            "Configure settings and click ",
            tags$strong("'Extract'", style = "color: #0c1f4a;"),
            " button to generate results",
            style = "font-size: 18px; font-weight: 400; line-height: 1.7; color: #64748B; margin: 0;"
          )
        )
      )
    }
  })



  processed_documents <- eventReactive(input$process_documents, {
    req(united_tbl())

    tryCatch(
      {
        data <- united_tbl()

        if (!"united_texts" %in% names(data)) {
          show_unite_texts_required_notification()
          return(NULL)
        }

        doc_summary <- data %>%
          mutate(
            combined_text = trimws(as.character(united_texts)),
            doc_length = nchar(combined_text),
            word_count = ifelse(
              nchar(combined_text) > 0,
              lengths(strsplit(combined_text, "\\s+")),
              0
            )
          )

        doc_summary <- doc_summary %>%
          filter(nchar(combined_text) > 0)

        if (nrow(doc_summary) == 0) {
          showNotification("No valid documents found after processing.", type = "warning")
          return(NULL)
        }

        if (is.null(input$doc_id_var) || input$doc_id_var == "" || input$doc_id_var == "None" ||
            !input$doc_id_var %in% names(data)) {
          doc_summary$doc_id <- paste0("Doc_", seq_len(nrow(doc_summary)))
          id_source <- "Generated automatically"
        } else {
          original_ids <- as.character(doc_summary[[input$doc_id_var]])
          has_missing <- any(is.na(original_ids)) || any(nchar(trimws(original_ids)) == 0)
          has_duplicates <- any(duplicated(original_ids[!is.na(original_ids)]))

          if (has_missing || has_duplicates) {
            doc_summary$doc_id <- paste0("Doc_", seq_len(nrow(doc_summary)))
            id_source <- paste(
              "Generated (original had",
              ifelse(has_missing, "missing values", ""),
              ifelse(has_missing && has_duplicates, " and ", ""),
              ifelse(has_duplicates, "duplicates", "")
            )
            showNotification(
              paste("Document ID column", input$doc_id_var, "was selected."),
              type = "message", duration = 5
            )
          } else {
            doc_summary$doc_id <- original_ids
            id_source <- paste("From column:", input$doc_id_var)
          }
        }

        if (!is.null(input$doc_category_var) && input$doc_category_var != "" && input$doc_category_var != "None" &&
            input$doc_category_var %in% names(data)) {
          doc_summary$category <- as.character(doc_summary[[input$doc_category_var]])
          doc_summary$category <- ifelse(is.na(doc_summary$category), "No Category", doc_summary$category)
        } else {
          doc_summary$category <- "No Category"
        }

        if (!is.null(input$doc_date_var) && input$doc_date_var != "" && input$doc_date_var != "None" &&
            input$doc_date_var %in% names(data)) {
          doc_summary$date <- as.Date(doc_summary[[input$doc_date_var]])
          documents_data_reactive$has_dates <- TRUE
          documents_data_reactive$date_column <- input$doc_date_var
        } else {
          documents_data_reactive$has_dates <- FALSE
          documents_data_reactive$date_column <- NULL
        }

        showNotification(
          paste("Successfully processed", nrow(doc_summary), "documents using united texts.", id_source),
          type = "message", duration = 5
        )

        return(doc_summary)
      },
      error = function(e) {
        showNotification(
          paste("Error processing documents:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )
  })

  observeEvent(input$process_documents, {
    req(processed_documents())
    updateTabsetPanel(session, "semantic_analysis_tabs", selected = "summary")
  })

  document_display_data <- reactive({
    req(united_tbl())

    input$doc_id_var
    input$doc_category_var

    original_data <- united_tbl()

    has_dfm <- !is.null(tryCatch(dfm_outcome(), error = function(e) NULL))

    if (has_dfm) {
      dfm_doc_names <- quanteda::docnames(dfm_outcome())
      n_docs <- length(dfm_doc_names)

      docs_data <- data.frame(
        dfm_doc_id = dfm_doc_names,
        document_number = paste0("Doc ", seq_len(n_docs)),
        stringsAsFactors = FALSE
      )
    } else {
      n_docs <- nrow(original_data)
      docs_data <- data.frame(
        document_number = paste0("Doc ", seq_len(n_docs)),
        stringsAsFactors = FALSE
      )
    }

    if (!is.null(input$doc_id_var) && input$doc_id_var != "" && input$doc_id_var != "None" &&
        input$doc_id_var %in% names(original_data)) {
      if (nrow(original_data) == n_docs) {
        docs_data$document_id_display <- as.character(original_data[[input$doc_id_var]])
      } else if (has_dfm) {
        matching_indices <- match(dfm_doc_names, rownames(original_data))
        docs_data$document_id_display <- as.character(original_data[[input$doc_id_var]][matching_indices])
      } else {
        docs_data$document_id_display <- as.character(original_data[[input$doc_id_var]])
      }

      docs_data$document_id_display <- ifelse(is.na(docs_data$document_id_display),
                                              docs_data$document_number,
                                              docs_data$document_id_display
      )
    } else {
      docs_data$document_id_display <- docs_data$document_number
    }

    if (!is.null(input$doc_category_var) && input$doc_category_var != "" && input$doc_category_var != "None" &&
        input$doc_category_var %in% names(original_data)) {
      if (nrow(original_data) == n_docs) {
        docs_data$category_display <- as.character(original_data[[input$doc_category_var]])
      } else if (has_dfm) {
        matching_indices <- match(dfm_doc_names, rownames(original_data))
        docs_data$category_display <- as.character(original_data[[input$doc_category_var]][matching_indices])
      } else {
        docs_data$category_display <- as.character(original_data[[input$doc_category_var]])
      }

      docs_data$category_display <- ifelse(is.na(docs_data$category_display), "No Category", docs_data$category_display)
    } else {
      docs_data$category_display <- "Document"
    }

    proc_docs_available <- tryCatch({
      !is.null(processed_documents())
    }, error = function(e) {
      FALSE
    })

    if (proc_docs_available) {
      proc_docs <- processed_documents()
      if (!is.null(proc_docs) && nrow(proc_docs) == n_docs) {
        docs_data$combined_text <- proc_docs$combined_text
        docs_data$word_count <- proc_docs$word_count
        docs_data$doc_length <- proc_docs$doc_length
        if (!"united_texts" %in% names(docs_data)) {
          docs_data$united_texts <- proc_docs$united_texts
        }
        if (!"doc_id" %in% names(docs_data)) {
          docs_data$doc_id <- proc_docs$doc_id
        }
        if (!"category" %in% names(docs_data)) {
          docs_data$category <- proc_docs$category
        }
      }
    }

    if (!"combined_text" %in% names(docs_data)) {
      if (!is.null(original_data) && "united_texts" %in% names(original_data)) {
        docs_data$combined_text <- as.character(original_data$united_texts)
        docs_data$word_count <- ifelse(
          nchar(docs_data$combined_text) > 0,
          lengths(strsplit(docs_data$combined_text, "\\s+")),
          0
        )
        docs_data$doc_length <- nchar(docs_data$combined_text)
      } else {
        docs_data$combined_text <- paste("Document", seq_len(n_docs), "text content")
        docs_data$word_count <- rep(NA, n_docs)
        docs_data$doc_length <- rep(NA, n_docs)
      }
    }

    if (!"united_texts" %in% names(docs_data)) {
      docs_data$united_texts <- docs_data$combined_text
    }

    names(docs_data) <- make.names(names(docs_data), unique = TRUE)

    return(docs_data)
  })

  output$document_summary_table <- DT::renderDataTable({
    req(document_display_data())

    docs_data <- document_display_data()

    show_doc_id <- !is.null(input$doc_id_var) && input$doc_id_var != "" && input$doc_id_var != "None"
    show_category <- !is.null(input$doc_category_var) && input$doc_category_var != "" && input$doc_category_var != "None"

    base_cols <- c("document_number", "word_count", "doc_length", "united_texts")
    col_names <- c("Document", "Word Count", "Character Count", "United Text")

    if (show_doc_id) {
      base_cols <- c(base_cols[1], "document_id_display", base_cols[2:length(base_cols)])
      col_names <- c(col_names[1], "Document ID", col_names[2:length(col_names)])
    }

    if (show_category) {
      insert_pos <- if (show_doc_id) 3 else 2
      base_cols <- c(base_cols[1:(insert_pos - 1)], "category_display", base_cols[insert_pos:length(base_cols)])
      col_names <- c(col_names[1:(insert_pos - 1)], "Category", col_names[insert_pos:length(col_names)])
    }

    available_cols <- names(docs_data)
    existing_cols <- base_cols[base_cols %in% available_cols]
    final_col_names <- col_names[1:length(existing_cols)]

    summary_data <- docs_data %>%
      dplyr::select(all_of(existing_cols))

    names(summary_data) <- final_col_names

    if ("Document ID" %in% names(summary_data)) {
      bold_column <- "Document ID"
    } else {
      bold_column <- "Document"
    }

    n_cols <- length(existing_cols)
    text_col_index <- n_cols - 1

    column_defs <- list(
      list(width = "80px", targets = 0),
      list(width = "400px", targets = text_col_index)
    )

    if (n_cols > 2) {
      middle_cols <- 1:(text_col_index - 1)
      column_defs <- append(column_defs, list(list(width = "120px", targets = middle_cols)), 1)
    }

    dt_table <- DT::datatable(
      summary_data,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        pageLength = 10,
        columnDefs = column_defs
      )
    )

    if (bold_column %in% names(summary_data)) {
      dt_table <- dt_table %>%
        DT::formatStyle(
          bold_column,
          fontWeight = "bold"
        )
    }

    return(dt_table)
  })



  base_similarity_results <- reactive({
    req(input$calculate_similarities)
    req(processed_documents())

    current_category_filter <- input$heatmap_category_filter %||% "all"
    docs_data <- get_filtered_docs_data(current_category_filter)

    if (is.null(docs_data) || nrow(docs_data) == 0) {
      showNotification("No documents to analyze. Please process documents first.", type = "error")
      return(NULL)
    }

    if (nrow(docs_data) < 2) {
      showNotification("Need at least 2 documents for similarity analysis.", type = "error")
      return(NULL)
    }

    texts <- docs_data$combined_text

    if (any(nchar(trimws(texts)) == 0)) {
      showNotification("Some documents are empty. Please check text data.", type = "warning")
      texts <- texts[nchar(trimws(texts)) > 0]
      docs_data <- docs_data[nchar(trimws(docs_data$combined_text)) > 0, ]
    }

    current_model <- input$embedding_model %||% "all-MiniLM-L6-v2"
    current_texts_hash <- digest::digest(texts, algo = "md5")

    use_cached <- !is.null(embeddings_cache$embeddings) &&
                  !is.null(embeddings_cache$model) &&
                  !is.null(embeddings_cache$texts_hash) &&
                  embeddings_cache$model == current_model &&
                  embeddings_cache$texts_hash == current_texts_hash &&
                  identical(embeddings_cache$category_filter, current_category_filter)

    if (use_cached) {
      result <- list(
        similarity_matrix = NULL,
        method_used = "embeddings (cached)",
        embeddings = embeddings_cache$embeddings,
        diagnostics = list()
      )

      if (requireNamespace("reticulate", quietly = TRUE)) {
        tryCatch({
          sklearn_metrics <- reticulate::import("sklearn.metrics.pairwise")
          result$similarity_matrix <- sklearn_metrics$cosine_similarity(result$embeddings)
          result$similarity_matrix <- as.matrix(result$similarity_matrix)
        }, error = function(e) {
          result$similarity_matrix <<- NULL
        })
      }
    } else {
      result <- TextAnalysisR::calculate_similarity_robust(
        texts = texts,
        method = "embeddings",
        embedding_model = current_model,
        min_word_length = 3,
        doc_names = docs_data$doc_id
      )

      if (result$method_used == "embeddings" && !is.null(result$embeddings)) {
        embeddings_cache$embeddings <- result$embeddings
        embeddings_cache$model <- current_model
        embeddings_cache$texts_hash <- current_texts_hash
        embeddings_cache$category_filter <- current_category_filter
        embeddings_cache$timestamp <- Sys.time()
      }

      if (result$method_used == "jaccard") {
        showNotification(
          "Embeddings failed. Using Jaccard similarity fallback.",
          type = "warning",
          duration = 5
        )
      }
    }

    similarity_matrix <- result$similarity_matrix
    n_docs <- nrow(similarity_matrix)

    shiny::showModal(shiny::modalDialog(
      title = "Similarity Analysis",
      div(
        style = "text-align: center; padding: 20px;",
        p("Analysis completed successfully!"),
        p(paste(n_docs, "documents processed")),
        p(paste("Method:", result$method_used)),
        p("Rendering visualization..."),
        br(),
        div(class = "progress progress-striped active",
            div(class = "progress-bar", style = "width: 100%")
        )
      ),
      footer = NULL,
      easyClose = FALSE,
      size = "m"
    ))

    return(list(
      similarity_matrix = similarity_matrix,
      docs_data = docs_data,
      embeddings = result$embeddings,
      method = result$method_used
    ))
  })

  similarity_results <- reactive({
    req(base_similarity_results())

    results <- base_similarity_results()
    return(results)
  })

  output$document_similarity_heatmap <- plotly::renderPlotly({
    req(input$calculate_similarities)
    req(similarity_results())

    tryCatch(
      {
        results <- similarity_results()

        if (is.null(results) || is.null(results$similarity_matrix)) {
          return(create_error_plot("No similarity data available"))
        }

        sim_matrix <- results$similarity_matrix
        docs_data <- results$docs_data
        method <- results$method %||% "unknown"

        if (nrow(sim_matrix) == 0 || ncol(sim_matrix) == 0) {
          return(create_error_plot("Empty similarity matrix"))
        }

        x_labels <- docs_data$document_number
        y_labels <- docs_data$document_number

        hover_text <- matrix("", nrow = nrow(sim_matrix), ncol = ncol(sim_matrix))
        for (i in 1:nrow(sim_matrix)) {
          for (j in 1:ncol(sim_matrix)) {
            hover_text[i, j] <- paste0(
              "Document: ", docs_data$document_id_display[i], "<br>",
              "Category: ", docs_data$category_display[i], "<br>",
              "Document: ", docs_data$document_id_display[j], "<br>",
              "Category: ", docs_data$category_display[j], "<br>",
              "Similarity: ", round(sim_matrix[i, j], 3)
            )
          }
        }

        if (!is.null(input$heatmap_category_filter) && input$heatmap_category_filter != "all") {
          title_text <- paste("Document Similarity Heatmap:", input$heatmap_category_filter)
        } else {
          title_text <- "Document Similarity Heatmap"
        }

        annotations <- list()
        if (!is.null(input$heatmap_category_filter) && input$heatmap_category_filter != "all" &&
            length(unique(docs_data$category_display)) > 1) {
          category_groups <- split(seq_len(nrow(docs_data)), docs_data$category_display)

          for (cat_name in names(category_groups)) {
            indices <- category_groups[[cat_name]]
            if (length(indices) > 0) {
              if (min(indices) > 1) {
                annotations <- c(annotations, list(
                  list(
                    type = "line",
                    x0 = min(indices) - 0.5,
                    x1 = min(indices) - 0.5,
                    y0 = 0.5,
                    y1 = nrow(sim_matrix) + 0.5,
                    xref = "x",
                    yref = "y",
                    line = list(color = "red", width = 2, dash = "dash")
                  )
                ))
              }

              if (min(indices) > 1) {
                annotations <- c(annotations, list(
                  list(
                    type = "line",
                    x0 = 0.5,
                    x1 = ncol(sim_matrix) + 0.5,
                    y0 = min(indices) - 0.5,
                    y1 = min(indices) - 0.5,
                    xref = "x",
                    yref = "y",
                    line = list(color = "red", width = 2, dash = "dash")
                  )
                ))
              }

              annotations <- c(annotations, list(
                list(
                  text = cat_name,
                  x = mean(indices),
                  y = -0.5,
                  xref = "x",
                  yref = "y",
                  showarrow = FALSE,
                  font = list(size = 16, color = "red"),
                  bgcolor = "rgba(255,255,255,0.8)",
                  bordercolor = "red",
                  borderwidth = 1
                )
              ))
            }
          }
        }

        p <- plotly::plot_ly() %>%
          plotly::add_heatmap(
            z = sim_matrix,
            x = x_labels,
            y = y_labels,
            colorscale = "Viridis",
            text = hover_text,
            hovertemplate = "%{text}<extra></extra>",
            hoverlabel = TextAnalysisR::get_plotly_hover_config(),
            showscale = TRUE
          ) %>%
          plotly::layout(
            title = list(
              text = title_text,
              font = list(size = 18, color = "#0c1f4a", family = "Roboto"),
              x = 0.5,
              xref = "paper",
              xanchor = "center",
              y = 0.98,
              yref = "paper",
              yanchor = "top"
            ),
            xaxis = list(
              title = list(text = "Documents"),
              tickangle = -45,
              tickfont = list(size = 16, color = "#3B3B3B", family = "Roboto, sans-serif"),
              titlefont = list(size = 16, color = "#0c1f4a", family = "Roboto, sans-serif")
            ),
            yaxis = list(
              title = list(text = "Documents"),
              tickfont = list(size = 16, color = "#3B3B3B", family = "Roboto, sans-serif"),
              titlefont = list(size = 16, color = "#0c1f4a", family = "Roboto, sans-serif")
            ),
            margin = list(b = 100, l = 100),
            annotations = annotations
          )

        tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})

        return(p)
      },
      error = function(e) {
        return(TextAnalysisR::create_empty_plot_message(
          paste("Error creating heatmap:", e$message),
          color = "#EF4444"
        ))
      }
    )
  })



  output$similarity_stats_table <- DT::renderDataTable({
    req(input$calculate_similarities)
    req(similarity_results())

    tryCatch(
      {
        results <- similarity_results()

        if (is.null(results) || is.null(results$similarity_matrix)) {
          return(NULL)
        }

        sim_matrix <- results$similarity_matrix
        docs_data <- results$docs_data
        method <- results$method %||% "unknown"

        if (nrow(sim_matrix) == 0 || ncol(sim_matrix) == 0) {
          return(NULL)
        }

        off_diagonal <- sim_matrix[upper.tri(sim_matrix) | lower.tri(sim_matrix)]

        if (length(off_diagonal) == 0) {
          off_diagonal <- c(0)
        }

        mean_sim <- mean(off_diagonal, na.rm = TRUE)
        median_sim <- median(off_diagonal, na.rm = TRUE)
        min_sim <- min(off_diagonal, na.rm = TRUE)
        max_sim <- max(off_diagonal, na.rm = TRUE)
        sd_sim <- sd(off_diagonal, na.rm = TRUE)
        pairs_above <- sum(off_diagonal >= input$similarity_threshold, na.rm = TRUE)

        stats_df <- data.frame(
          Metric = c(
            "Analysis Method",
            "Total Documents",
            "Total Pairs",
            "Mean Similarity",
            "Median Similarity",
            "Min Similarity",
            "Max Similarity",
            "Std Dev Similarity",
            "Pairs Above Threshold",
            "Total Categories"
          ),
          Value = c(
            ifelse(method == "sentence_transformers", paste("Embeddings (", input$embedding_model %||% "all-MiniLM-L6-v2", ")", sep = ""), "Basic Text Similarity"),
            nrow(sim_matrix),
            length(off_diagonal),
            mean_sim,
            median_sim,
            min_sim,
            max_sim,
            sd_sim,
            pairs_above,
            length(unique(docs_data$category_display))
          )
        )

        DT::datatable(
          stats_df,
          rownames = FALSE,
          options = list(
            dom = "t",
            ordering = FALSE
          )
        ) %>%
          DT::formatStyle(
            "Metric",
            fontWeight = "bold"
          )
      },
      error = function(e) {
        DT::datatable(
          data.frame(Error = paste("Error calculating statistics:", e$message)),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        )
      }
    )
  })

  values <- reactiveValues(semantic_step = 1)

  performance_metrics <- reactiveValues(
    operation_times = list(),
    memory_usage = list(),
    cache_hit_rates = list(),
    last_cleanup = Sys.time()
  )

  cache_manager <- reactiveValues(
    max_cache_size = 500,
    cache_entries = list(),
    last_accessed = list(),
    cache_stats = list(
      hits = 0,
      misses = 0,
      evictions = 0
    )
  )

  memory_monitor <- function(operation = "general") {
    tryCatch({
      if (requireNamespace("pryr", quietly = TRUE)) {
        mem_usage <- pryr::mem_used()
        performance_metrics$memory_usage[[operation]] <- mem_usage

        if (mem_usage > 1e9) {
          gc()
          if (reticulate::py_available()) {
            tryCatch({
              reticulate::py_run_string("import gc; gc.collect()")
            }, error = function(e) {})
          }
          performance_metrics$last_cleanup <- Sys.time()
          showNotification("Memory usage high - performed cleanup", type = "warning", duration = 3)
        }
      }
    }, error = function(e) {
      gc()
    })
  }

  manage_cache <- function(key, data, size_mb = 10) {
    current_time <- Sys.time()

    cache_manager$last_accessed[[key]] <- current_time

    total_size <- sum(sapply(cache_manager$cache_entries, function(x) x$size))
    if (total_size + size_mb > cache_manager$max_cache_size) {
      oldest_key <- names(cache_manager$last_accessed)[which.min(cache_manager$last_accessed)]
      if (!is.null(oldest_key) && oldest_key != key) {
        cache_manager$cache_entries[[oldest_key]] <- NULL
        cache_manager$last_accessed[[oldest_key]] <- NULL
        cache_manager$cache_stats$evictions <- cache_manager$cache_stats$evictions + 1
      }
    }

    cache_manager$cache_entries[[key]] <- list(
      data = data,
      size = size_mb,
      timestamp = current_time
    )
  }

  get_cached_data <- function(key) {
    if (!is.null(cache_manager$cache_entries[[key]])) {
      cache_manager$cache_stats$hits <- cache_manager$cache_stats$hits + 1
      cache_manager$last_accessed[[key]] <- Sys.time()
      return(cache_manager$cache_entries[[key]]$data)
    } else {
      cache_manager$cache_stats$misses <- cache_manager$cache_stats$misses + 1
      return(NULL)
    }
  }

  progress_tracker <- function(total_steps, current_step, operation, id = "progress") {
    progress <- (current_step / total_steps) * 100
    showNotification(
      paste(operation, ":", round(progress, 1), "%"),
      type = "message",
      duration = NULL,
      id = id
    )
  }

  time_operation <- function(operation_name, operation_func) {
    start_time <- Sys.time()
    result <- operation_func()
    end_time <- Sys.time()

    execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    performance_metrics$operation_times[[operation_name]] <- execution_time

    return(result)
  }

  comparison_results <- reactiveValues(
    results = list(),
    metrics = data.frame(),
    calculation_status = list(),
    correlations = data.frame(),
    update_trigger = 0,
    last_dfm_hash = NULL,
    last_data_hash = NULL,
    cached_plots = list(),
    cached_stats = list(),
    last_doc_id_var = NULL,
    last_doc_category_var = NULL,
    config_changed = FALSE,
    calculated_doc_id_var = NULL,
    calculated_doc_category_var = NULL,
    gap_analysis = NULL
  )

  embeddings_cache <- reactiveValues(
    embeddings = NULL,
    model = NULL,
    texts_hash = NULL,
    category_filter = NULL,
    timestamp = NULL
  )

  observeEvent(input$doc_id_var, {
    if (!is.null(comparison_results$calculated_doc_id_var) &&
        comparison_results$calculated_doc_id_var != input$doc_id_var &&
        length(comparison_results$results) > 0) {
      comparison_results$config_changed <- TRUE
      comparison_results$update_trigger <- isolate(comparison_results$update_trigger) + 1

      showNotification(
        "Document ID variable changed. Please recalculate similarity data to reflect the new configuration.",
        type = "warning",
        duration = 7
      )
    }
    comparison_results$last_doc_id_var <- input$doc_id_var
  }, ignoreNULL = FALSE)

  observeEvent(input$doc_category_var, {
    if (!is.null(comparison_results$calculated_doc_category_var) &&
        comparison_results$calculated_doc_category_var != input$doc_category_var &&
        length(comparison_results$results) > 0) {
      comparison_results$config_changed <- TRUE
      comparison_results$update_trigger <- isolate(comparison_results$update_trigger) + 1

      showNotification(
        "Category variable changed. Please recalculate similarity data to reflect the new configuration.",
        type = "warning",
        duration = 7
      )
    }
    comparison_results$last_doc_category_var <- input$doc_category_var
  }, ignoreNULL = FALSE)

  clear_embeddings_cache <- function() {
    embeddings_cache$embeddings <- NULL
    embeddings_cache$model <- NULL
    embeddings_cache$texts_hash <- NULL
    embeddings_cache$category_filter <- NULL
    embeddings_cache$timestamp <- NULL
    embeddings_cache$by_category <- list()
    showNotification("Embedding cache cleared", type = "message")
  }

  output$cache_management_ui <- renderUI({
    tagList(
      if (!is.null(embeddings_cache$embeddings)) {
        cache_info <- paste(
          "Cached:", nrow(embeddings_cache$embeddings), "documents",
          "| Model:", embeddings_cache$model %||% "Unknown",
          "| Time:", format(embeddings_cache$timestamp %||% Sys.time(), "%H:%M:%S")
        )

        tags$div(
          style = "background: #f0f9ff; border: 1px solid #0ea5e9; padding: 8px; border-radius: 4px; margin: 8px 0;",
          tags$small(cache_info)
        )
      },

      if (length(performance_metrics$operation_times) > 0) {
        latest_ops <- tail(performance_metrics$operation_times, 3)
        perf_info <- paste(
          "Recent operations:",
          paste(names(latest_ops), round(unlist(latest_ops), 2), "s", collapse = " | ")
        )

        tags$div(
          style = "background: #fef3c7; border: 1px solid #f59e0b; padding: 8px; border-radius: 4px; margin: 8px 0;",
          tags$small(perf_info)
        )
      },

      if (cache_manager$cache_stats$hits > 0 || cache_manager$cache_stats$misses > 0) {
        hit_rate <- cache_manager$cache_stats$hits / (cache_manager$cache_stats$hits + cache_manager$cache_stats$misses) * 100
        cache_stats_info <- paste(
          "Cache hit rate:", round(hit_rate, 1), "%",
          "| Hits:", cache_manager$cache_stats$hits,
          "| Misses:", cache_manager$cache_stats$misses,
          "| Evictions:", cache_manager$cache_stats$evictions
        )

        tags$div(
          style = "background: #ecfdf5; border: 1px solid #10b981; padding: 8px; border-radius: 4px; margin: 8px 0;",
          tags$small(cache_stats_info)
        )
      },

      if (length(performance_metrics$memory_usage) > 0) {
        latest_mem <- tail(performance_metrics$memory_usage, 1)[[1]]
        if (!is.null(latest_mem)) {
          mem_mb <- round(latest_mem / 1e6, 1)
          mem_info <- paste("Memory usage:", mem_mb, "MB")

          tags$div(
            style = "background: #fef2f2; border: 1px solid #ef4444; padding: 8px; border-radius: 4px; margin: 8px 0;",
            tags$small(mem_info)
          )
        }
      },

      div(
        style = "display: flex; gap: 10px; margin-top: 10px;",
        actionButton("clear_embeddings_cache", "Clear Embeddings",
                     class = "btn-sm btn-outline-secondary"),
        actionButton("clear_all_cache", "Clear All Cache",
                     class = "btn-sm btn-outline-warning"),
        actionButton("show_performance", "Performance Report",
                     class = "btn-sm btn-outline-info")
      )
    )
  })

  observeEvent(input$clear_embeddings_cache, {
    clear_embeddings_cache()
  })

  observeEvent(input$clear_all_cache, {
    clear_embeddings_cache()
    cache_manager$cache_entries <- list()
    cache_manager$last_accessed <- list()
    cache_manager$cache_stats$hits <- 0
    cache_manager$cache_stats$misses <- 0
    cache_manager$cache_stats$evictions <- 0
    showNotification("All caches cleared", type = "message")
  })

  observeEvent(input$show_performance, {
    if (length(performance_metrics$operation_times) > 0) {
      perf_data <- data.frame(
        Operation = names(performance_metrics$operation_times),
        Time_Seconds = unlist(performance_metrics$operation_times),
        stringsAsFactors = FALSE
      )

      showModal(modalDialog(
        title = "Performance Report",
        renderTable(perf_data, rownames = FALSE),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else {
      showNotification("No performance data available yet", type = "message")
    }
  })

  get_embedding_model_display_name <- function(model_id) {
    model_names <- list(
      "all-MiniLM-L6-v2" = "all-MiniLM-L6-v2 (Fast, 90MB)",
      "all-mpnet-base-v2" = "all-mpnet-base-v2 (Best Quality, 420MB)"
    )
    return(model_names[[model_id]] %||% model_id)
  }

  get_filtered_docs_data <- function(category_filter = NULL) {
    docs_data <- document_display_data()

    if (is.null(docs_data) || nrow(docs_data) == 0) {
      return(NULL)
    }

    cache_key <- paste(category_filter %||% "all", nrow(docs_data), sep = "_")

    if (!is.null(embeddings_cache$by_category[[cache_key]])) {
      return(embeddings_cache$by_category[[cache_key]])
    }

    if (!is.null(category_filter) && category_filter != "all") {
      filter_indices <- which(docs_data$category_display == category_filter)
      if (length(filter_indices) > 0) {
        docs_data <- docs_data[filter_indices, , drop = FALSE]
      }
    }

    embeddings_cache$by_category[[cache_key]] <- docs_data

    return(docs_data)
  }

  embedding_model_cache <- reactiveValues(
    model = NULL,
    model_name = NULL,
    last_used = NULL
  )

  cleanup_embedding_memory <- function() {
    if (requireNamespace("reticulate", quietly = TRUE)) {
      tryCatch({
        gc()

        if (reticulate::py_available()) {
          tryCatch({
            reticulate::py_run_string("import gc; gc.collect()")
          }, error = function(e) {

          })
        }
      }, error = function(e) {
      })
    }
  }

  calculate_enhanced_similarity <- function(texts, method = "cosine", use_embeddings = FALSE, embedding_model = "all-MiniLM-L6-v2") {
    memory_monitor("similarity_analysis")

    package_result <- tryCatch({
      TextAnalysisR::calculate_document_similarity(
        texts = texts,
        document_feature_type = if(use_embeddings) "embeddings" else "words",
        similarity_method = method,
        use_embeddings = use_embeddings,
        embedding_model = embedding_model,
        calculate_metrics = TRUE,
        verbose = FALSE
      )
    }, error = function(e) NULL)

    if (!is.null(package_result)) {
      return(package_result)
    }

    if (is.null(texts) || length(texts) == 0) {
      showNotification("No texts provided for similarity calculation", type = "error")
      return(NULL)
    }

    valid_texts <- texts[nchar(trimws(texts)) > 0]
    if (length(valid_texts) < 2) {
      showNotification("Need at least 2 non-empty texts for similarity calculation", type = "error")
      return(NULL)
    }

    tryCatch({
      if (use_embeddings && requireNamespace("reticulate", quietly = TRUE)) {
        current_texts_hash <- digest::digest(valid_texts, algo = "md5")

        if (!is.null(embeddings_cache$embeddings) &&
            !is.null(embeddings_cache$model) &&
            !is.null(embeddings_cache$texts_hash) &&
            embeddings_cache$model == embedding_model &&
            embeddings_cache$texts_hash == current_texts_hash) {

          similarity_matrix <- sklearn_metrics$cosine_similarity(embeddings_cache$embeddings)
          similarity_matrix <- as.matrix(similarity_matrix)

          return(list(
            similarity_matrix = similarity_matrix,
            embeddings = embeddings_cache$embeddings,
            method = "embedding_cosine",
            model_name = embedding_model,
            n_docs = length(valid_texts)
          ))
        }

        python_available <- tryCatch({
          reticulate::py_config()
          TRUE
        }, error = function(e) FALSE)

        if (!python_available) {
          showNotification("Python not available. Falling back to traditional similarity.", type = "warning")
          use_embeddings <- FALSE
        } else {
          tryCatch({
            sentence_transformers <- reticulate::import("sentence_transformers")
            sklearn_metrics <- reticulate::import("sklearn.metrics.pairwise")
          }, error = function(e) {
            showNotification("Required Python libraries not found. Falling back to traditional similarity.", type = "warning")
            use_embeddings <- FALSE
          })
        }

        if (use_embeddings) {
          n_docs <- length(valid_texts)

          if (is.null(embedding_model_cache$model) ||
              embedding_model_cache$model_name != embedding_model ||
              is.null(embedding_model_cache$last_used) ||
              as.numeric(difftime(Sys.time(), embedding_model_cache$last_used, units = "mins")) > 30) {

            embedding_model_cache$model <- sentence_transformers$SentenceTransformer(embedding_model)
            embedding_model_cache$model_name <- embedding_model
            embedding_model_cache$last_used <- Sys.time()
          }

          model <- embedding_model_cache$model

          avg_text_length <- mean(nchar(valid_texts))
          if (n_docs > 100 || avg_text_length > 5000) {
            batch_size <- 20
          } else if (n_docs > 50 || avg_text_length > 2000) {
            batch_size <- 40
          } else {
            batch_size <- n_docs
          }

          embeddings_list <- list()
          total_batches <- ceiling(n_docs / batch_size)
          successful_batches <- 0
          failed_batches <- 0

          showNotification(
            HTML(paste0(
              "Generating embeddings...<br>",
              "Processing ", n_docs, " documents in ", total_batches, " batch",
              ifelse(total_batches > 1, "es", ""), "<br>",
              "Using model: ", get_embedding_model_display_name(embedding_model)
            )),
            type = "message",
            duration = NULL,
            id = "embedding_progress"
          )

          for (i in seq(1, n_docs, by = batch_size)) {
            end_idx <- min(i + batch_size - 1, n_docs)
            batch_texts <- valid_texts[i:end_idx]
            current_batch <- ceiling(i / batch_size)

            progress_pct <- round((current_batch - 1) / total_batches * 100)
            shiny::showNotification(
              HTML(paste0(
                "Processing batch ", current_batch, " of ", total_batches, "<br>",
                "Progress: ", progress_pct, "% complete"
              )),
              type = "message",
              duration = 2,
              id = "batch_progress"
            )

            batch_embeddings <- tryCatch({
              model$encode(batch_texts, show_progress_bar = FALSE)
            }, error = function(e) {
              failed_batches <- failed_batches + 1
              showNotification(
                paste("Error in batch", current_batch, ":", e$message),
                type = "warning",
                duration = 5
              )
              return(NULL)
            })

            if (!is.null(batch_embeddings)) {
              embeddings_list[[length(embeddings_list) + 1]] <- batch_embeddings
              successful_batches <- successful_batches + 1
            }
          }

          try(removeNotification("batch_progress"), silent = TRUE)
          try(removeNotification("embedding_progress"), silent = TRUE)

          if (length(embeddings_list) == 0) {
            showNotification("Failed to generate embeddings - all batches failed", type = "error")
            return(NULL)
          }

          if (failed_batches > 0) {
            showNotification(
              HTML(paste0(
                "⚠ Embeddings generated with issues<br>",
                "Completed: ", successful_batches, " batches<br>",
                "Failed: ", failed_batches, " batches"
              )),
              type = "warning",
              duration = 5
            )
          } else {
            showNotification(
              HTML(paste0(
                "Embeddings generated successfully<br>",
                "Processed all ", n_docs, " documents"
              )),
              type = "message",
              duration = 3
            )
          }

          embeddings <- do.call(rbind, embeddings_list)

          embeddings_cache$embeddings <- embeddings
          embeddings_cache$model <- embedding_model
          embeddings_cache$texts_hash <- current_texts_hash
          embeddings_cache$timestamp <- Sys.time()

          if (n_docs > 50) {
            gc()
            if (reticulate::py_available()) {
              tryCatch({
                reticulate::py_run_string("import gc; gc.collect()")
              }, error = function(e) {

              })
            }
          }

          similarity_matrix <- sklearn_metrics$cosine_similarity(embeddings)
          similarity_matrix <- as.matrix(similarity_matrix)

          doc_names <- if (exists("dfm_outcome") && !is.null(dfm_outcome())) {
            dfm_doc_names <- quanteda::docnames(dfm_outcome())
            if (length(dfm_doc_names) == nrow(similarity_matrix)) dfm_doc_names else paste0("doc_", seq_len(nrow(similarity_matrix)))
          } else {
            paste0("doc_", seq_len(nrow(similarity_matrix)))
          }

          rownames(similarity_matrix) <- doc_names
          colnames(similarity_matrix) <- doc_names

          return(list(
            similarity_matrix = similarity_matrix,
            embeddings = embeddings,
            method = "embedding_cosine",
            model_name = embedding_model,
            n_docs = length(valid_texts)
          ))
        }
      }

      showNotification("Using traditional text similarity analysis...", type = "message", duration = 3)

      valid_texts_clean <- valid_texts
      temp_tbl <- tibble::tibble(united_texts = valid_texts_clean)
      tokens <- TextAnalysisR::prep_texts(
        temp_tbl,
        text_field = "united_texts",
        remove_stopwords = TRUE,
        verbose = FALSE
      )

      dfm <- quanteda::dfm(tokens)
      dfm <- quanteda::dfm_trim(dfm, min_termfreq = 2, min_docfreq = 1)

      if (quanteda::nfeat(dfm) == 0) {
        showNotification("No features remaining after preprocessing. Using basic similarity.", type = "warning")
        similarity_matrix <- outer(valid_texts, valid_texts, function(x, y) {
          mapply(function(a, b) {
            if (nchar(a) == 0 || nchar(b) == 0) {
              return(0)
            }
            common_chars <- sum(utf8ToInt(a) %in% utf8ToInt(b))
            max(common_chars / max(nchar(a), nchar(b)), 0)
          }, x, y)
        })
      } else {
        similarity_matrix <- as.matrix(quanteda.textstats::textstat_simil(dfm, method = method))
      }

      return(list(
        similarity_matrix = similarity_matrix,
        embeddings = NULL,
        method = paste0("traditional_", method),
        dfm = if (exists("dfm")) dfm else NULL,
        n_docs = length(valid_texts)
      ))
    }, error = function(e) {
      showNotification(paste("Similarity calculation error:", e$message), type = "error")
      NULL
    })
  }


  semantic_search <- function(query, documents, embeddings = NULL, top_k = 5, use_embeddings = FALSE, search_method = "keyword") {
    if (is.null(query) || nchar(trimws(query)) == 0) {
      showNotification("Search query cannot be empty", type = "warning")
      return(NULL)
    }

    if (is.null(documents) || length(documents) == 0) {
      showNotification("No documents available for search", type = "error")
      return(NULL)
    }

    top_k <- max(1, min(top_k, length(documents), 50))

    if (!use_embeddings || search_method == "keyword") {
      return(keyword_search(query, documents, top_k))
    }

    if (is.null(embeddings)) {
      if (!is.null(embeddings_cache$embeddings)) {
        embeddings <- embeddings_cache$embeddings
      } else {
        showNotification("Embeddings not available for semantic search", type = "error")
        return(NULL)
      }
    }

    if (nrow(embeddings) != length(documents)) {
      showNotification("Embeddings and documents count mismatch", type = "error")
      return(NULL)
    }

    memory_monitor("semantic_search")

    tryCatch({
      if (!requireNamespace("reticulate", quietly = TRUE)) {
        showNotification("Python/reticulate not available for semantic search", type = "error")
        return(NULL)
      }

      if (!reticulate::py_available()) {
        showNotification("Python not available. Please check Python installation.", type = "error")
        return(NULL)
      }

      sentence_transformers <- tryCatch({
        reticulate::import("sentence_transformers")
      }, error = function(e) {
        showNotification("sentence_transformers not available. Please install: pip install sentence-transformers", type = "error")
        return(NULL)
      })

      sklearn_metrics <- tryCatch({
        reticulate::import("sklearn.metrics.pairwise")
      }, error = function(e) {
        showNotification("sklearn not available. Please install: pip install scikit-learn", type = "error")
        return(NULL)
      })

      if (is.null(sentence_transformers) || is.null(sklearn_metrics)) {
        return(NULL)
      }

      model_name <- embeddings_cache$model %||% "all-MiniLM-L6-v2"

      if (!is.null(embedding_model_cache$model) &&
          embedding_model_cache$model_name == model_name &&
          !is.null(embedding_model_cache$last_used) &&
          as.numeric(difftime(Sys.time(), embedding_model_cache$last_used, units = "mins")) < 30) {
        model <- embedding_model_cache$model
      } else {
        showNotification(paste("Loading", model_name, "model..."), type = "message", duration = 2)
        model <- sentence_transformers$SentenceTransformer(model_name)
        embedding_model_cache$model <- model
        embedding_model_cache$model_name <- model_name
        embedding_model_cache$last_used <- Sys.time()
      }

      query_embedding <- tryCatch({
        embedding <- model$encode(query, show_progress_bar = FALSE)
        if (!is.null(embedding)) {
          numpy <- reticulate::import("numpy")
          numpy$reshape(embedding, list(1L, -1L))
        } else {
          NULL
        }
      }, error = function(e) {
        showNotification(paste("Error encoding query:", e$message), type = "error")
        return(NULL)
      })

      if (is.null(query_embedding)) {
        return(NULL)
      }

      similarities <- tryCatch({
        sim_matrix <- sklearn_metrics$cosine_similarity(query_embedding, embeddings)
        as.vector(sim_matrix)
      }, error = function(e) {
        showNotification(paste("Error calculating similarities:", e$message), type = "error")
        return(NULL)
      })

      if (is.null(similarities)) {
        return(NULL)
      }

      if (length(similarities) != length(documents)) {
        showNotification("Similarity calculation error: dimension mismatch", type = "error")
        return(NULL)
      }

      similarities[is.na(similarities) | is.infinite(similarities)] <- 0

      if (length(similarities) == 0) {
        showNotification("No valid similarities calculated", type = "error")
        return(NULL)
      }

      top_indices <- order(similarities, decreasing = TRUE)[1:min(top_k, length(similarities))]

      valid_indices <- top_indices[top_indices <= length(documents) & top_indices > 0]
      if (length(valid_indices) == 0) {
        showNotification("No valid document indices found", type = "error")
        return(NULL)
      }

      results <- data.frame(
        Document_Index = valid_indices,
        Similarity_Score = similarities[valid_indices],
        Document_Text = documents[valid_indices],
        stringsAsFactors = FALSE
      )

      results <- results[results$Similarity_Score > 0.1, , drop = FALSE]

      if (nrow(results) == 0) {
        showNotification("No documents found with sufficient similarity", type = "message")
        return(NULL)
      }

      return(results)

    }, error = function(e) {
      showNotification(paste("Semantic search error:", e$message), type = "error")
      return(NULL)
    })
  }

  ################################################################################
  # SEMANTIC ANALYSIS TAB
  ################################################################################

  perform_similarity_search <- function(query, documents, top_k = 5, search_method = "keyword", similarity_matrix = NULL, ngram_size = NULL) {
    if (is.null(query) || nchar(trimws(query)) == 0) {
      showNotification("Search query cannot be empty", type = "warning")
      return(NULL)
    }

    if (is.null(documents) || length(documents) == 0) {
      showNotification("No documents available for search", type = "error")
      return(NULL)
    }

    top_k <- max(1, min(top_k, length(documents), 50))

    if (search_method == "keyword") {
      return(keyword_search(query, documents, top_k))
    }

    if (search_method %in% c("words", "ngrams", "embeddings")) {
      if (is.null(similarity_matrix)) {
        showNotification(paste("No similarity matrix available for", search_method, "method"), type = "error")
        return(NULL)
      }

      if (search_method == "embeddings") {
        return(semantic_search(query, documents, embeddings_cache$embeddings, top_k, TRUE, "semantic"))
      } else {
        query_lower <- tolower(trimws(query))
        query_terms <- unlist(strsplit(query_lower, "\\s+"))

        doc_scores <- sapply(seq_along(documents), function(i) {
          doc_lower <- tolower(documents[i])

          term_score <- sum(sapply(query_terms, function(term) {
            stringr::str_count(doc_lower, paste0("\\b", term, "\\b"))
          }))

          if (!is.null(similarity_matrix) && i <= nrow(similarity_matrix)) {
            context_score <- mean(similarity_matrix[i, ], na.rm = TRUE)
          } else {
            context_score <- 0
          }

          combined_score <- (0.7 * term_score / max(length(query_terms), 1)) + (0.3 * context_score)

          return(combined_score)
        })

        doc_scores[is.na(doc_scores) | is.infinite(doc_scores)] <- 0

        if (all(doc_scores == 0)) {
          if (!is.null(similarity_matrix)) {
            avg_similarities <- rowMeans(similarity_matrix, na.rm = TRUE)
            top_indices <- order(avg_similarities, decreasing = TRUE)[1:min(top_k, length(documents))]

            results <- data.frame(
              Document_Index = top_indices,
              Similarity_Score = avg_similarities[top_indices],
              Document_Text = documents[top_indices],
              stringsAsFactors = FALSE
            )

            return(results[results$Similarity_Score > 0, , drop = FALSE])
          } else {
            showNotification("No matching documents found", type = "message")
            return(NULL)
          }
        }

        positive_indices <- which(doc_scores > 0)
        if (length(positive_indices) == 0) {
          showNotification("No relevant documents found", type = "message")
          return(NULL)
        }

        top_indices <- positive_indices[order(doc_scores[positive_indices], decreasing = TRUE)]
        top_indices <- top_indices[1:min(top_k, length(top_indices))]

        results <- data.frame(
          Document_Index = top_indices,
          Similarity_Score = doc_scores[top_indices],
          Document_Text = documents[top_indices],
          stringsAsFactors = FALSE
        )

        if (max(results$Similarity_Score) > 0) {
          results$Similarity_Score <- results$Similarity_Score / max(results$Similarity_Score)
        }

        return(results)
      }
    }

    showNotification(paste("Unknown search method:", search_method), type = "error")
    return(NULL)
  }

  document_ranking <- function(query_doc, candidate_docs, method = "cosine") {
    if (length(candidate_docs) < 2) {
      return(data.frame(Document = candidate_docs, Rank = 1, stringsAsFactors = FALSE))
    }

    tryCatch({
      all_docs <- c(query_doc, candidate_docs)
      similarity_result <- calculate_enhanced_similarity(all_docs, method = method, use_embeddings = TRUE)

      if (is.null(similarity_result)) {
        return(NULL)
      }

      similarities <- similarity_result$similarity_matrix[1, -1]

      ranking <- data.frame(
        Document = candidate_docs,
        Similarity = similarities,
        stringsAsFactors = FALSE
      )

      ranking <- ranking[order(ranking$Similarity, decreasing = TRUE), ]
      ranking$Rank <- 1:nrow(ranking)

      return(ranking)
    }, error = function(e) {
      showNotification(paste("Document ranking error:", e$message), type = "error")
      return(NULL)
    })
  }

  keyword_search <- function(query, documents, top_k = 5) {
    if (is.null(query) || length(documents) == 0) {
      return(NULL)
    }

    tryCatch({
      query_terms <- tolower(unlist(strsplit(query, "\\s+")))
      query_terms <- query_terms[nchar(query_terms) > 2]

      if (length(query_terms) == 0) {
        return(NULL)
      }

      doc_term_freq <- sapply(documents, function(doc) {
        doc_lower <- tolower(doc)
        term_counts <- sapply(query_terms, function(term) {
          matches <- gregexpr(term, doc_lower, fixed = TRUE)[[1]]
          if (matches[1] == -1) return(0)
          length(matches)
        })
        sum(term_counts)
      })

      doc_lengths <- sapply(documents, function(doc) {
        nchar(doc)
      })

      idf <- log(length(documents) / (sum(doc_term_freq > 0) + 1))

      tf_idf_scores <- (doc_term_freq / (doc_lengths + 1)) * idf

      if (max(tf_idf_scores) > 0) {
        scores <- tf_idf_scores / max(tf_idf_scores)
      } else {
        scores <- tf_idf_scores
      }

      top_indices <- order(scores, decreasing = TRUE)[1:min(top_k, length(scores))]
      top_indices <- top_indices[scores[top_indices] > 0]

      if (length(top_indices) == 0) {
        return(NULL)
      }

      results <- data.frame(
        Document_Index = top_indices,
        Similarity_Score = scores[top_indices],
        Document_Text = documents[top_indices],
        stringsAsFactors = FALSE
      )

      return(results)
    }, error = function(e) {
      showNotification(paste("Fallback search error:", e$message), type = "error")
      return(NULL)
    })
  }

  find_optimal_parameters <- function(data_matrix, method = "silhouette", range = c(2, 10)) {
    if (nrow(data_matrix) < max(range)) {
      showNotification("Not enough data points for the specified range", type = "warning")
      return(NULL)
    }

    tryCatch({
      k_values <- seq(range[1], range[2])
      scores <- numeric(length(k_values))

      for (i in seq_along(k_values)) {
        k <- k_values[i]

        if (method == "silhouette") {
          if (requireNamespace("cluster", quietly = TRUE)) {
            set.seed(123)
            km_result <- kmeans(data_matrix, centers = k, nstart = 10)
            sil <- cluster::silhouette(km_result$cluster, dist(data_matrix))
            scores[i] <- mean(sil[, 3])
          }
        } else if (method == "elbow") {
          set.seed(123)
          km_result <- kmeans(data_matrix, centers = k, nstart = 10)
          scores[i] <- km_result$tot.withinss
        } else if (method == "gap") {
          if (requireNamespace("cluster", quietly = TRUE)) {
            set.seed(123)
            gap_result <- cluster::clusGap(data_matrix, FUN = kmeans, nstart = 10, K.max = k)
            scores[i] <- gap_result$Tab[gap_result$Tab[, "k"] == k, "gap"]
          }
        }
      }

      if (method == "silhouette") {
        optimal_k <- k_values[which.max(scores)]
        optimal_score <- max(scores)
      } else if (method == "elbow") {
        diffs <- diff(scores)
        optimal_k <- k_values[which.min(diffs) + 1]
        optimal_score <- scores[which.min(diffs) + 1]
      } else if (method == "gap") {
        optimal_k <- k_values[which.max(scores)]
        optimal_score <- max(scores)
      }

      return(list(
        optimal_k = optimal_k,
        optimal_score = optimal_score,
        k_values = k_values,
        scores = scores,
        method = method
      ))
    }, error = function(e) {
      showNotification(paste("Parameter optimization error:", e$message), type = "error")
      return(NULL)
    })
  }

  calculate_comprehensive_metrics <- function(similarity_matrix, labels = NULL, method_info = NULL) {
    off_diagonal <- similarity_matrix[upper.tri(similarity_matrix) | lower.tri(similarity_matrix)]

    metrics <- list(
      mean_similarity = mean(off_diagonal, na.rm = TRUE),
      median_similarity = median(off_diagonal, na.rm = TRUE),
      std_similarity = sd(off_diagonal, na.rm = TRUE),
      min_similarity = min(off_diagonal, na.rm = TRUE),
      max_similarity = max(off_diagonal, na.rm = TRUE),
      similarity_range = paste(round(min(off_diagonal, na.rm = TRUE), 3), "to", round(max(off_diagonal, na.rm = TRUE), 3)),
      sparsity = sum(off_diagonal < 0.1, na.rm = TRUE) / length(off_diagonal),
      connectivity = sum(off_diagonal > 0.3, na.rm = TRUE) / length(off_diagonal),
      skewness = ifelse(requireNamespace("moments", quietly = TRUE), moments::skewness(off_diagonal, na.rm = TRUE), NA),
      kurtosis = ifelse(requireNamespace("moments", quietly = TRUE), moments::kurtosis(off_diagonal, na.rm = TRUE), NA)
    )



    if (!is.null(method_info)) {
      metrics$method <- method_info$method
      metrics$model_name <- method_info$model_name %||% "N/A"
    }

    metrics
  }

  get_available_dfm <- function() {
    dfm_lemma_result <- tryCatch(dfm_lemma(), error = function(e) NULL)
    dfm_outcome_result <- tryCatch(dfm_outcome(), error = function(e) NULL)
    dfm_final_result <- tryCatch(dfm_final(), error = function(e) NULL)
    dfm_init_result <- tryCatch(dfm_init(), error = function(e) NULL)

    return(TextAnalysisR::get_available_dfm(
      dfm_lemma = dfm_lemma_result,
      dfm_outcome = dfm_outcome_result,
      dfm_final = dfm_final_result,
      dfm_init = dfm_init_result
    ))
  }

  get_available_tokens <- function() {
    final_tokens_result <- tryCatch(final_tokens(), error = function(e) NULL)
    if (!is.null(final_tokens_result)) {
      return(final_tokens_result)
    }

    preprocessed_result <- tryCatch(preprocessed_combined(), error = function(e) NULL)
    if (!is.null(preprocessed_result)) {
      return(preprocessed_result)
    }

    return(NULL)
  }

  semantic_feature_matrix <- reactive({
    tryCatch({
      dfm_to_use <- get_available_dfm()
      if (is.null(dfm_to_use)) {
        return(NULL)
      }

      feature_space <- input$semantic_feature_space %||% "words"

      if (feature_space == "ngrams") {
        input$semantic_ngram_range
      }

      switch(feature_space,
             "words" = as.matrix(dfm_to_use),
             "ngrams" = {
               tokens_check <- get_available_tokens()
               if (is.null(tokens_check)) return(NULL)

               n_value <- as.numeric(input$semantic_ngram_range)
               tokens_ngrams <- quanteda::tokens_ngrams(tokens_check, n = n_value, concatenator = "_")
               as.matrix(quanteda::dfm(tokens_ngrams))
             },
             "embeddings" = {
               if (!is.null(embeddings_cache$embeddings)) {
                 cached_filter <- embeddings_cache$category_filter

                 if (is.null(cached_filter) || cached_filter == "all") {
                   embeddings_cache$embeddings
                 } else {
                   return(NULL)
                 }
               } else {
                 return(NULL)
               }
             },
             as.matrix(dfm_to_use)
      )
    }, error = function(e) {
      cat("Error in semantic_feature_matrix:", e$message, "\n")
      return(NULL)
    })
  })

  observe({
    updateNumericInput(session, "semantic_step_hidden", value = values$semantic_step)
  })

  observeEvent(input$semantic_next_step, {
    if (values$semantic_step < 2) {
      values$semantic_step <- values$semantic_step + 1
    }
  })

  observeEvent(input$semantic_prev_step, {
    if (values$semantic_step > 1) {
      values$semantic_step <- values$semantic_step - 1
    }
  })

  observe({
    req(united_tbl())
    col_names <- c("None" = "None", names(united_tbl()))
    current_doc_id <- isolate(input$doc_id_var)
    current_category <- isolate(input$doc_category_var)

    if (is.null(current_doc_id) || current_doc_id == "") current_doc_id <- "None"
    if (is.null(current_category) || current_category == "") current_category <- "None"

    updateSelectizeInput(
      session,
      "doc_id_var",
      choices = col_names,
      selected = current_doc_id,
      options = list(
        allowEmptyOption = TRUE,
        persist = TRUE
      )
    )
    updateSelectizeInput(
      session,
      "doc_category_var",
      choices = c("None" = "None", colnames_cat()),
      selected = current_category,
      options = list(
        allowEmptyOption = TRUE,
        persist = TRUE
      )
    )
  })

  output$semantic_feature_space_selector <- renderUI({
    input$semantic_analysis_tabs

    if (input$semantic_analysis_tabs == "summary") {
      return(NULL)
    }

    choices <- c(
      "Words" = "words",
      "N-grams" = "ngrams",
      "Embeddings" = "embeddings"
    )

    label_content <- if (input$semantic_analysis_tabs == "similarity") {
      tags$div(
        style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 5px;",
        tags$label("Feature Space:", style = "margin: 0;"),
        actionLink("showSemanticMethodsInfo", icon("info-circle"),
                  style = "color: #337ab7; font-size: 16px;",
                  title = "Click for detailed method descriptions")
      )
    } else {
      tags$label("Feature Space:", style = "margin-bottom: 5px;")
    }

    tagList(
      label_content,
      selectInput(
        "semantic_feature_space",
        label = NULL,
        choices = choices,
        selected = "words"
      ),
      conditionalPanel(
        condition = "input.semantic_feature_space == 'ngrams'",
        selectInput(
          "semantic_ngram_range",
          "N-gram range:",
          choices = c(
            "Bigrams (2 words)" = "2",
            "Trigrams (3 words)" = "3",
            "4-grams (4 words)" = "4",
            "5-grams (5 words)" = "5"
          ),
          selected = "2"
        )
      ),
      conditionalPanel(
        condition = "input.semantic_feature_space == 'embeddings'",
        sliderInput(
          "embedding_sim_threshold",
          "Document similarity threshold:",
          min = 0.3,
          max = 0.9,
          value = 0.5,
          step = 0.05
        ),
        tags$div(
          style = "font-size: 14px; color: #64748B; margin-top: -8px; margin-bottom: 8px;",
          "Higher values = fewer, stronger document connections"
        )
      )
    )
  })

  output$embedding_status_ui <- renderUI({
    if (!is.null(embeddings_cache$embeddings)) {
      tags$div(
        style = "background: #d1fae5; border: 1px solid #10b981; padding: 12px; border-radius: 4px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          tags$i(class = "fa fa-check-circle", style = "color: #10b981; font-size: 18px;"),
          tags$strong("Embeddings Ready", style = "color: #065f46;")
        ),
        tags$div(
          style = "color: #047857; font-size: 16px;",
          paste(
            nrow(embeddings_cache$embeddings), "documents |",
            embeddings_cache$model %||% "Unknown model", "|",
            format(embeddings_cache$timestamp %||% Sys.time(), "%Y-%m-%d %H:%M:%S")
          )
        )
      )
    } else {
      tags$div(
        style = "background: #fef3c7; border: 1px solid #f59e0b; padding: 12px; border-radius: 4px;",
        tags$div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
          tags$i(class = "fa fa-info-circle", style = "color: #d97706; font-size: 18px;"),
          tags$strong("No Embeddings Generated", style = "color: #92400e;")
        ),
        tags$p(
          "Generate embeddings to enable advanced semantic analyses.",
          style = "color: #78350f; margin: 0; font-size: 16px;"
        )
      )
    }
  })

  observeEvent(input$generate_embeddings, {
    texts_df <- tryCatch(united_tbl(), error = function(e) NULL)

    if (is.null(texts_df) || !"text" %in% names(texts_df)) {
      showNotification(
        "Please unite text columns first before generating embeddings.",
        type = "error",
        duration = 5
      )
      return()
    }

    texts_vec <- texts_df$text

    if (length(texts_vec) == 0) {
      showNotification("No texts available for embedding generation.", type = "error")
      return()
    }

    model_name <- input$embedding_model_setup %||% "all-MiniLM-L6-v2"

    loading_id <- show_loading_notification("Generating embeddings... This may take a moment.")

    tryCatch({
      embeddings <- TextAnalysisR::generate_embeddings(
        texts = texts_vec,
        model = model_name,
        verbose = FALSE
      )

      embeddings_cache$embeddings <- embeddings
      embeddings_cache$model <- model_name
      embeddings_cache$texts_hash <- digest::digest(texts_vec, algo = "md5")
      embeddings_cache$timestamp <- Sys.time()

      remove_notification_by_id(loading_id)

      show_completion_notification(
        paste0(
          "Embeddings generated successfully! ",
          nrow(embeddings), " documents processed with ",
          model_name, " model."
        )
      )

    }, error = function(e) {
      remove_notification_by_id(loading_id)

      show_error_notification(
        paste0(
          "Error generating embeddings: ", e$message, ". ",
          "Please ensure Python and sentence-transformers are installed. ",
          "See Setup > Installation guide for help."
        )
      )
    })
  })

  observeEvent(input$semantic_configure_docs, {
    req(united_tbl())
    show_completion_notification("Documents configured successfully!")
    values$semantic_step <- 2
  })


  validate_semantic_prerequisites <- function() {
    tryCatch({
      dfm_check <- get_available_dfm()

      if (is.null(dfm_check)) {
        show_dfm_required_modal()
        return(FALSE)
      }

      ndoc_check <- tryCatch({
        quanteda::ndoc(dfm_check)
      }, error = function(e) {
        0
      })

      if (ndoc_check == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "No Documents Found",
          p("The document-feature matrix has no documents."),
          p("Please ensure you have uploaded data and completed preprocessing."),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return(FALSE)
      }

      return(TRUE)
    }, error = function(e) {
      shiny::showModal(shiny::modalDialog(
        title = "Validation Error",
        p("An error occurred during validation."),
        p(paste("Error:", e$message)),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return(FALSE)
    })
  }

  validate_feature_type <- function(feature_type) {
    return(TRUE)
  }

  get_document_data <- function() {
    if (exists("document_display_data") && !is.null(document_display_data())) {
      return(document_display_data())
    }
    return(NULL)
  }

  run_semantic_analysis_with_validation <- function(analysis_type, analysis_function) {
    if (!validate_semantic_prerequisites()) {
      return()
    }

    show_loading_notification(paste("Running", analysis_type, "analysis..."),
                              id = paste0("loading", gsub(" ", "", analysis_type))
    )

    tryCatch(
      {
        result <- analysis_function()
        remove_notification_by_id(paste0("loading", gsub(" ", "", analysis_type)))
        show_completion_notification(paste(analysis_type, "analysis completed!"))
        return(result)
      },
      error = function(e) {
        remove_notification_by_id(paste0("loading", gsub(" ", "", analysis_type)))
        showNotification(paste("Error in", tolower(analysis_type), "analysis:", e$message), type = "error")
        return(NULL)
      }
    )
  }

  prepare_semantic_data <- function(feature_type) {
    processed_docs_null <- tryCatch({
      is.null(processed_documents())
    }, error = function(e) {
      TRUE
    })

    if (processed_docs_null) {
      shiny::showModal(shiny::modalDialog(
        title = "Document Configuration Required",
        p("Please run Document Configuration first."),
        p("Configure documents in the Document Configuration section before proceeding."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return(NULL)
    }

    if (feature_type == "embeddings") {
      docs_data <- processed_documents()
      if (!"combined_text" %in% names(docs_data)) {
        showNotification("Document text data not available for embeddings.", type = "error")
        return(NULL)
      }
      return(list(
        texts = docs_data$combined_text,
        category_labels = if ("category" %in% names(docs_data)) docs_data$category else NULL
      ))
    } else {
      docs_data_available <- !is.null(get_document_data())
      if (!docs_data_available) {
        showNotification("Document data not available.", type = "error")
        return(NULL)
      }
      docs_data <- get_document_data()
      return(list(
        texts = docs_data$combined_text,
        category_labels = if ("category_display" %in% names(docs_data)) docs_data$category_display else NULL
      ))
    }
  }

  run_similarity_analysis <- function(feature_type, similarity_method = "cosine", embedding_model = "all-MiniLM-L6-v2") {
    start_time <- Sys.time()

    cache_key <- generate_cache_key(feature_type)
    if (is_cached(feature_type, cache_key)) {
      showNotification(paste("Using cached results for", feature_type), type = "message")
      calculation_state$in_progress <- FALSE
      shinyjs::enable("calculate_similarity")
      shinyjs::enable("visualize_similarity")
      return(comparison_results$results[[feature_type]])
    }

    data_prep <- prepare_semantic_data(feature_type)
    if (is.null(data_prep)) {
      calculation_state$in_progress <- FALSE
      similarity_analysis_triggered(FALSE)
      shinyjs::enable("calculate_similarity")
      shinyjs::enable("visualize_similarity")
      removeNotification("similarityProgress")
      return(NULL)
    }

    similarity_result <- switch(feature_type,
                                "embeddings" = calculate_enhanced_similarity(data_prep$texts, use_embeddings = TRUE, embedding_model = embedding_model),
                                "words" = calculate_words_similarity(),
                                "ngrams" = calculate_ngrams_similarity(),
                                "topics" = calculate_topics_similarity(),
                                NULL
    )

    if (is.null(similarity_result)) {
      removeNotification("similarityProgress")
      calculation_state$in_progress <- FALSE
      similarity_analysis_triggered(FALSE)
      shinyjs::enable("calculate_similarity")
      shinyjs::enable("visualize_similarity")
      return(NULL)
    }

    store_similarity_results(feature_type, similarity_result, data_prep$category_labels, start_time, cache_key)

    execution_time <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
    n_docs <- if (!is.null(similarity_result$n_docs)) similarity_result$n_docs else nrow(similarity_result$similarity_matrix)

    removeNotification("similarityProgress")

    calculation_state$in_progress <- FALSE
    shinyjs::enable("calculate_similarity")
    shinyjs::enable("visualize_similarity")

    if (feature_type == "embeddings") {
      model_display_name <- get_embedding_model_display_name(embedding_model)
      showNotification(
        paste(tools::toTitleCase(feature_type), "analysis completed in", execution_time, "seconds using", model_display_name, "-", n_docs, "documents processed"),
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste(tools::toTitleCase(feature_type), "analysis completed in", execution_time, "seconds -", n_docs, "documents processed"),
        type = "message",
        duration = 5
      )
    }

    return(similarity_result)
  }

  generate_cache_key <- function(feature_type) {
    dfm_data <- get_available_dfm()
    if (is.null(dfm_data)) {
      return(NULL)
    }

    content_hash <- digest::digest(list(
      ndoc = quanteda::ndoc(dfm_data),
      nfeat = quanteda::nfeat(dfm_data),
      features = quanteda::featnames(dfm_data)[1:min(100, quanteda::nfeat(dfm_data))],
      matrix_sample = as.matrix(dfm_data)[1:min(10, quanteda::ndoc(dfm_data)), 1:min(100, quanteda::nfeat(dfm_data))]
    ), algo = "md5")

    base_key <- paste(feature_type, content_hash, sep = "_")

    if (feature_type == "ngrams") {
      n_value <- as.numeric(input$semantic_ngram_range %||% "2")
      base_key <- paste(base_key, "n", n_value, sep = "_")
    }

    if (feature_type == "embeddings") {
      model_name <- input$embedding_model %||% "all-MiniLM-L6-v2"
      base_key <- paste(base_key, "model", model_name, sep = "_")
    }

    if (!is.null(input$heatmap_category_filter) && input$heatmap_category_filter != "All") {
      base_key <- paste(base_key, "cat", digest::digest(input$heatmap_category_filter, algo = "md5"), sep = "_")
    }

    if (!is.null(input$doc_id_var)) {
      base_key <- paste(base_key, "docid", digest::digest(input$doc_id_var, algo = "md5"), sep = "_")
    }

    if (!is.null(input$doc_category_var)) {
      base_key <- paste(base_key, "doccat", digest::digest(input$doc_category_var, algo = "md5"), sep = "_")
    }

    base_key
  }

  is_cached <- function(feature_type, cache_key) {
    !is.null(comparison_results$results[[feature_type]]) &&
      !is.null(comparison_results$results[[feature_type]]$cache_key) &&
      comparison_results$results[[feature_type]]$cache_key == cache_key
  }

  calculate_words_similarity <- function() {
    tryCatch(
      {
        dfm_data <- get_available_dfm()
        if (is.null(dfm_data)) {
          shiny::showModal(shiny::modalDialog(
            title = "Preprocessing Required",
            p("Please complete preprocessing steps first."),
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          return(NULL)
        }

        feature_matrix <- as.matrix(dfm_data)
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)

        list(similarity_matrix = similarity_matrix, method = "words_cosine", embeddings = NULL)
      },
      error = function(e) {
        showNotification(paste("✗ Words similarity error:", e$message), type = "error", duration = 5)
        NULL
      }
    )
  }

  calculate_ngrams_similarity <- function() {
    tryCatch(
      {
        tokens_data <- get_available_tokens()
        if (is.null(tokens_data)) {
          shiny::showModal(shiny::modalDialog(
            title = "Preprocessing Required",
            p("Please complete preprocessing (at least Step 4: DFM) to generate tokens."),
            easyClose = TRUE,
            footer = shiny::modalButton("OK")
          ))
          return(NULL)
        }

        n_value <- as.numeric(input$semantic_ngram_range %||% "2")

        tokens_ngrams <- quanteda::tokens_ngrams(tokens_data, n = n_value, concatenator = "_")
        dfm_ngrams <- quanteda::dfm(tokens_ngrams)
        feature_matrix <- as.matrix(dfm_ngrams)
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)

        list(similarity_matrix = similarity_matrix, method = "ngrams_cosine", embeddings = NULL)
      },
      error = function(e) {
        showNotification(paste("✗ N-grams similarity error:", e$message), type = "error", duration = 5)
        NULL
      }
    )
  }

  calculate_topics_similarity <- function() {
    tryCatch(
      {
        if (is.null(stm_K_number()$theta)) {
          return(NULL)
        }
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(stm_K_number()$theta)
        list(similarity_matrix = similarity_matrix, method = "topics_cosine", embeddings = NULL)
      },
      error = function(e) {
        showNotification(paste("Topics similarity error:", e$message), type = "error")
        NULL
      }
    )
  }

  store_similarity_results <- function(feature_type, similarity_result, category_labels, start_time, cache_key) {
    metrics <- calculate_comprehensive_metrics(similarity_result$similarity_matrix, category_labels, similarity_result)

    comparison_results$results[[feature_type]] <- list(
      similarity_matrix = similarity_result$similarity_matrix,
      feature_type = feature_type,
      method = similarity_result$method,
      timestamp = Sys.time(),
      embeddings = similarity_result$embeddings,
      cache_key = cache_key,
      ngram_range = if (feature_type == "ngrams") input$semantic_ngram_range else NULL,
      processing_time = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      metrics = metrics
    )

    comparison_results$calculation_status[[feature_type]] <- TRUE
    comparison_results$config_changed <- FALSE
    comparison_results$calculated_doc_id_var <- input$doc_id_var
    comparison_results$calculated_doc_category_var <- input$doc_category_var

    calculation_state$in_progress <- FALSE

    update_similarity_status_indicators()
  }
  calculate_feature_correlations <- function() {
    if (is.null(comparison_results$results) || length(comparison_results$results) < 2) {
      return()
    }

    feature_types <- names(comparison_results$results)
    correlations_list <- list()

    for (i in 1:(length(feature_types) - 1)) {
      for (j in (i + 1):length(feature_types)) {
        type1 <- feature_types[i]
        type2 <- feature_types[j]

        if (!is.null(comparison_results$results[[type1]]$similarity_matrix) &&
            !is.null(comparison_results$results[[type2]]$similarity_matrix)) {
          sim1 <- comparison_results$results[[type1]]$similarity_matrix[upper.tri(comparison_results$results[[type1]]$similarity_matrix)]
          sim2 <- comparison_results$results[[type2]]$similarity_matrix[upper.tri(comparison_results$results[[type2]]$similarity_matrix)]

          min_length <- min(length(sim1), length(sim2))
          if (min_length > 1) {
            sim1 <- sim1[1:min_length]
            sim2 <- sim2[1:min_length]

            correlation_value <- cor(sim1, sim2, method = "pearson", use = "complete.obs")

            if (!is.na(correlation_value)) {
              correlations_list[[length(correlations_list) + 1]] <- data.frame(
                Feature_Type_1 = type1,
                Feature_Type_2 = type2,
                Correlation = round(correlation_value, 4),
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
    }

    if (length(correlations_list) > 0) {
      comparison_results$correlations <- do.call(rbind, correlations_list)
      showNotification(paste("Calculated correlations between", length(correlations_list), "feature type pairs"), type = "message", duration = 3)
    }
  }

  similarity_analysis_triggered <- reactiveVal(FALSE)
  similarity_visualization_ready <- reactiveVal(FALSE)

  calculation_state <- reactiveValues(
    in_progress = FALSE,
    visualization_shown = list()
  )

  search_results_reactive <- reactiveValues(
    results = NULL,
    method_used = NULL
  )

  update_similarity_status_indicators <- function() {
    shinyjs::runjs("
      $('#words_similarity_ready_status').hide();
      $('#ngrams_similarity_ready_status').hide();
      $('#topics_similarity_ready_status').hide();
      $('#embeddings_similarity_ready_status').hide();
    ")

    if (is.null(input$main_tabs) || input$main_tabs != "semantic") return()
    if (is.null(input$semantic_analysis_tabs) || input$semantic_analysis_tabs != "similarity") return()
    if (is.null(input$semantic_feature_space)) return()

    current_feature <- input$semantic_feature_space

    if (!is.null(comparison_results$results[[current_feature]]) &&
        !is.null(comparison_results$calculation_status[[current_feature]]) &&
        comparison_results$calculation_status[[current_feature]]) {

      if (current_feature == "words") {
        shinyjs::runjs("$('#words_similarity_ready_status').show();")
      } else if (current_feature == "ngrams") {
        shinyjs::runjs("$('#ngrams_similarity_ready_status').show();")
      } else if (current_feature == "topics") {
        shinyjs::runjs("$('#topics_similarity_ready_status').show();")
      } else if (current_feature == "embeddings") {
        shinyjs::runjs("$('#embeddings_similarity_ready_status').show();")
      }
    }
  }

  observeEvent(input$semantic_feature_space, {
    update_similarity_status_indicators()

    if (!is.null(input$semantic_feature_space)) {
      feature_type <- input$semantic_feature_space

      similarity_analysis_triggered(FALSE)
      calculation_state$visualization_shown[[feature_type]] <- FALSE

      if (!is.null(comparison_results$calculation_status[[feature_type]]) &&
          comparison_results$calculation_status[[feature_type]]) {
        showNotification(
          paste(tools::toTitleCase(feature_type), "similarity data available. Click 'Visualize Results' to display."),
          type = "message",
          duration = 3
        )
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(input$semantic_analysis_tabs, {
    if (input$semantic_analysis_tabs == "similarity") {
      update_similarity_status_indicators()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$calculate_similarity, {
    tryCatch({
      if (!validate_semantic_prerequisites()) {
        return()
      }

      processed_docs_null <- tryCatch({
        is.null(processed_documents())
      }, error = function(e) {
        TRUE
      })

      if (processed_docs_null) {
        shiny::showModal(shiny::modalDialog(
          title = "Document Configuration Required",
          p("Please click 'Process' button in the Setup tab first."),
          p("This will prepare your documents for similarity analysis."),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return()
      }

      feature_type <- input$semantic_feature_space %||% "words"

      if (!validate_feature_type(feature_type)) {
        return()
      }

      cache_key <- tryCatch({
        generate_cache_key(feature_type)
      }, error = function(e) {
        stop(e)
      })

      is_cached_result <- tryCatch({
        is_cached(feature_type, cache_key)
      }, error = function(e) {
        FALSE
      })

      if (is_cached_result) {
        showNotification(
          paste(tools::toTitleCase(feature_type), "results already available - displaying cached results"),
          type = "message",
          duration = 3
        )
        comparison_results$calculation_status[[feature_type]] <- TRUE
        comparison_results$config_changed <- FALSE
        comparison_results$calculated_doc_id_var <- input$doc_id_var
        comparison_results$calculated_doc_category_var <- input$doc_category_var
        return()
      }

      shinyjs::hide("words_similarity_ready_status")
      shinyjs::hide("ngrams_similarity_ready_status")
      shinyjs::hide("topics_similarity_ready_status")
      shinyjs::hide("embeddings_similarity_ready_status")
      shinyjs::disable("calculate_similarity")
      shinyjs::disable("visualize_similarity")

      calculation_state$in_progress <- TRUE
      similarity_analysis_triggered(FALSE)
      similarity_visualization_ready(FALSE)
      calculation_state$visualization_shown[[feature_type]] <- FALSE

      shinybusy::show_spinner()

      showNotification(
        HTML(paste("Calculating", feature_type, "similarity...<br>This may take a few moments.")),
        type = "message",
        duration = NULL,
        id = "similarityProgress"
      )

      run_similarity_analysis(
        feature_type = feature_type,
        similarity_method = "cosine",
        embedding_model = input$embedding_model %||% "all-MiniLM-L6-v2"
      )

      calculation_state$in_progress <- FALSE
      shinybusy::hide_spinner()

      if (!is.null(comparison_results$results[[feature_type]])) {
        comparison_results$calculation_status[[feature_type]] <- TRUE
        comparison_results$config_changed <- FALSE
        comparison_results$calculated_doc_id_var <- input$doc_id_var
        comparison_results$calculated_doc_category_var <- input$doc_category_var

        n_docs <- nrow(comparison_results$results[[feature_type]]$similarity_matrix)

        final_output <- c(
          paste("Feature type:", tools::toTitleCase(feature_type)),
          paste("Documents processed:", n_docs),
          paste("Similarity method:", comparison_results$results[[feature_type]]$method),
          paste("Cache status:", if(is_cached_result) "Using cached results" else "Newly calculated"),
          "",
          "Calculation successful",
          "",
          "Click 'Visualize Results' to display the similarity heatmap."
        )

        output$modal_similarity_output <- renderPrint({
          cat(paste(final_output, collapse = "\n"))
        })

        shiny::showModal(
          shiny::modalDialog(
            title = "Similarity Calculation Complete",
            shiny::verbatimTextOutput("modal_similarity_output"),
            easyClose = TRUE,
            footer = shiny::modalButton("Close")
          )
        )
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Calculation Completed",
          tags$p("Calculation completed but no results were generated."),
          easyClose = FALSE,
          footer = shiny::modalButton("Close")
        ))
      }

      shinyjs::enable("calculate_similarity")
      shinyjs::enable("visualize_similarity")

    }, error = function(e) {
      calculation_state$in_progress <- FALSE
      shinybusy::hide_spinner()
      shinyjs::enable("calculate_similarity")
      shinyjs::enable("visualize_similarity")

      shiny::showModal(shiny::modalDialog(
        title = "Calculation Error",
        tags$p(tags$strong("Error:"), e$message, style = "color: #EF4444;"),
        easyClose = FALSE,
        footer = shiny::modalButton("Close")
      ))
    })
  })

  observeEvent(input$visualize_similarity, {
    feature_type <- input$semantic_feature_space %||% "words"

    if (is.null(comparison_results$calculation_status[[feature_type]]) ||
        !comparison_results$calculation_status[[feature_type]]) {
      showNotification(
        paste("⚠ Please calculate", feature_type, "similarity first"),
        type = "warning",
        duration = 5
      )
      return()
    }

    if (is.null(comparison_results$results[[feature_type]])) {
      showNotification(
        "✗ No similarity data found. Please calculate similarity first.",
        type = "error",
        duration = 5
      )
      return()
    }

    if (!is.null(comparison_results$config_changed) && comparison_results$config_changed) {
      warning_output <- c(
        "Document ID or category variable has changed.",
        "",
        "The current similarity results were calculated with different settings.",
        "",
        "Please click 'Calculate' again to recalculate similarity with the updated configuration."
      )

      output$modal_config_warning_output <- renderPrint({
        cat(paste(warning_output, collapse = "\n"))
      })

      shiny::showModal(
        shiny::modalDialog(
          title = "Configuration Changed",
          shiny::verbatimTextOutput("modal_config_warning_output"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
      return()
    }

    docs_count <- if (!is.null(comparison_results$results[[feature_type]]$similarity_matrix)) {
      nrow(comparison_results$results[[feature_type]]$similarity_matrix)
    } else {
      0
    }

    shinybusy::show_spinner()

    notification_id <- showNotification(
      paste("Rendering", feature_type, "similarity visualization for", docs_count, "documents..."),
      type = "message",
      duration = NULL,
      closeButton = FALSE
    )

    on.exit({
      removeNotification(notification_id)
      shinybusy::hide_spinner()
    }, add = TRUE)

    similarity_analysis_triggered(TRUE)
    calculation_state$visualization_shown[[feature_type]] <- TRUE
  })

  observeEvent(input$run_semantic_search, {
    tryCatch({
      TextAnalysisR:::check_rate_limit(session$token, user_requests, max_requests = 100, window_seconds = 3600)
    }, error = function(e) {
      TextAnalysisR:::log_security_event(
        "RATE_LIMIT_EXCEEDED",
        "Semantic search rate limit exceeded",
        session,
        "WARNING"
      )
      TextAnalysisR:::show_error_notification(
        "You have made too many search requests. Please wait before trying again."
      )
      return()
    })

    query <- TextAnalysisR:::sanitize_text_input(input$semantic_search_query)
    if (is.null(query) || nchar(trimws(query)) == 0) {
      showNotification("Please enter a search query", type = "warning")
      return()
    }

    if (nchar(trimws(query)) > 1000) {
      showNotification("Search query too long. Please use a shorter query.", type = "warning")
      return()
    }

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) == 0) {
      showNotification("No documents available for search. Please complete document configuration first.", type = "error")
      return()
    }

    if (!"combined_text" %in% names(docs_data)) {
      shiny::showModal(shiny::modalDialog(
        title = "Preprocessing Required",
        p("Document text not available."),
        p("Please complete preprocessing steps first."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    if (all(grepl("^Document \\d+ text content$", docs_data$combined_text))) {
      original_data <- united_tbl()
      if (!is.null(original_data) && "united_texts" %in% names(original_data)) {
        docs_data$combined_text <- as.character(original_data$united_texts)
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Document Processing Required",
          p("Please click 'Process' button in the Setup tab first to process your documents."),
          p("This will prepare the documents for search."),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return()
      }
    }

    if (any(grepl("^Document \\d+ text content$", docs_data$combined_text))) {
      shiny::showModal(shiny::modalDialog(
        title = "Document Processing Required",
        p("Some documents have not been processed yet."),
        p("Please click 'Process' button in the Setup tab first."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    valid_docs <- docs_data$combined_text[nchar(trimws(docs_data$combined_text)) > 0]
    if (length(valid_docs) == 0) {
      showNotification("No documents with valid text content found.", type = "error", duration = 5)
      return()
    }

    search_method <- input$search_method %||% "keyword"

    if (search_method == "rag") {
      if (!TextAnalysisR::check_feature("langgraph")) {
        if (is_web) {
          showNotification("RAG search not available in web version. Use R package.", type = "warning")
        } else {
          showModal(modalDialog(
            title = "Python Environment Required",
            HTML("<p>RAG Q&A requires Python environment with LangGraph.</p>
                 <p><strong>Setup:</strong></p>
                 <ol>
                   <li>Run: <code>TextAnalysisR::setup_python_env()</code></li>
                   <li>Install Ollama and pull a model: <code>ollama pull llama3</code></li>
                   <li>Restart the app</li>
                 </ol>"),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
        return()
      }

      python_check <- tryCatch({
        TextAnalysisR::check_python_env()
      }, error = function(e) list(available = FALSE))

      if (!python_check$available) {
        showModal(modalDialog(
          title = "Python Environment Required",
          HTML("<p>RAG Q&A requires Python environment with LangGraph.</p>
               <p><strong>Setup:</strong></p>
               <ol>
                 <li>Run: <code>TextAnalysisR::setup_python_env()</code></li>
                 <li>Install Ollama and pull a model: <code>ollama pull llama3</code></li>
                 <li>Restart the app</li>
               </ol>"),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }

      showNotification("Generating answer with LLM...", type = "message",
                       duration = NULL, id = "ragProgress")

      rag_result <- tryCatch({
        TextAnalysisR::run_rag_search(
          query = query,
          documents = valid_docs,
          ollama_model = input$rag_llm_model %||% "llama3",
          top_k = input$semantic_search_top_k %||% 5
        )
      }, error = function(e) {
        list(success = FALSE, error = e$message)
      })

      try(removeNotification("ragProgress"), silent = TRUE)

      if (rag_result$success) {
        search_results_reactive$rag_answer <- rag_result$answer
        search_results_reactive$rag_confidence <- rag_result$confidence
        search_results_reactive$rag_sources <- rag_result$sources
        search_results_reactive$method_used <- "rag"

        showNotification(
          paste0("Answer generated (Confidence: ",
                 round(rag_result$confidence * 100), "%)"),
          type = "message", duration = 5
        )
      } else {
        showNotification(
          paste("RAG search failed:", rag_result$error),
          type = "error", duration = 8
        )
      }
      return()
    }

    if (search_method != "keyword") {
      similarity_data <- comparison_results$results[[search_method]]

      if (is.null(similarity_data)) {
        method_label <- if (search_method == "ngrams") {
          "N-grams"
        } else {
          tools::toTitleCase(search_method)
        }

        shiny::showModal(shiny::modalDialog(
          title = "Similarity Data Required",
          tags$div(
            style = "padding: 10px;",
            tags$p(
              paste0(method_label, " similarity data has not been calculated yet."),
              style = "margin-bottom: 15px;"
            ),
            tags$p(
              tags$strong("To use similarity search:"),
              style = "margin-bottom: 10px;"
            ),
            tags$ol(
              tags$li("Go to the 'Document Similarity' tab"),
              tags$li(paste("Select feature space:", method_label)),
              tags$li("Click 'Calculate' button"),
              tags$li("Return here to search")
            ),
            tags$hr(style = "margin: 15px 0;"),
            tags$p(
              tags$i(class = "fa fa-info-circle", style = "margin-right: 5px; color: #337ab7;"),
              "Tip: Use 'Keyword' search method for simple text matching without similarity calculation.",
              style = "color: #64748B; font-size: 16px; font-style: italic;"
            )
          ),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        return()
      }

      ngram_size <- if (search_method == "ngrams") {
        similarity_data$ngram_range %||% "2"
      } else {
        NULL
      }

      if (search_method == "embeddings" && is.null(embeddings_cache$embeddings)) {
        showNotification("Embeddings cache is empty. Please recalculate embeddings.", type = "error")
        return()
      }

      ngram_label <- if (!is.null(ngram_size)) {
        switch(ngram_size,
          "2" = "bigram",
          "3" = "trigram",
          paste0(ngram_size, "-gram")
        )
      } else {
        NULL
      }

      method_label <- switch(search_method,
        "words" = "Words similarity search",
        "ngrams" = paste0(ngram_label, " similarity search"),
        "topics" = "Topics similarity search",
        "embeddings" = "Embeddings similarity search",
        search_method
      )
      showNotification(paste("Performing", method_label, "..."), type = "message", duration = NULL, id = "searchProgress")
    } else {
      showNotification("Performing keyword search...", type = "message", duration = NULL, id = "searchProgress")
    }

    search_results <- tryCatch({
      time_operation("semantic_search", function() {
        perform_similarity_search(
          query = query,
          documents = valid_docs,
          top_k = input$semantic_search_top_k %||% 5,
          search_method = search_method,
          similarity_matrix = if (search_method != "keyword") similarity_data$similarity_matrix else NULL,
          ngram_size = if (search_method == "ngrams") as.numeric(ngram_size) else NULL
        )
      })
    }, error = function(e) {
      showNotification(paste("Search operation failed:", e$message), type = "error")
      return(NULL)
    })

    try(removeNotification("searchProgress"), silent = TRUE)

    if (!is.null(search_results) && nrow(search_results) > 0) {
      search_results_reactive$results <- search_results
      search_results_reactive$method_used <- search_method
      search_results_reactive$ngram_size <- if (search_method == "ngrams") ngram_size else NULL
      ngram_label <- if (search_method == "ngrams") {
        switch(ngram_size,
          "2" = "Bigram",
          "3" = "Trigram",
          paste0(ngram_size, "-gram")
        )
      } else { NULL }

      method_label <- switch(search_method,
        "keyword" = "Keyword Search",
        "words" = "Words Similarity",
        "ngrams" = paste0(ngram_label, " Similarity"),
        "topics" = "Topics Similarity",
        "embeddings" = "Embeddings Similarity",
        search_method
      )
      showNotification(paste("Found", nrow(search_results), "relevant documents using", method_label), type = "message", duration = 3)
    } else {
      search_results_reactive$results <- NULL
      search_results_reactive$method_used <- search_method
      search_results_reactive$ngram_size <- if (search_method == "ngrams") ngram_size else NULL
      showNotification("No relevant documents found for query", type = "message", duration = 3)
    }
  })

  similarity_analysis_reactive <- reactive({
    req(similarity_analysis_triggered())

    input$heatmap_category_filter

    if (input$semantic_feature_space != "embeddings") {
      search_results_reactive$results <- NULL
    }

    if (!validate_semantic_prerequisites()) {
      return(NULL)
    }

    processed_docs_null <- tryCatch({
      is.null(processed_documents())
    }, error = function(e) {
      TRUE
    })

    if (processed_docs_null) {
      return(NULL)
    }

    feature_type <- input$semantic_feature_space %||% "words"
    if (!validate_feature_type(feature_type)) {
      return(NULL)
    }

    cache_key <- generate_cache_key(feature_type)
    if (is_cached(feature_type, cache_key)) {
      return(comparison_results$results[[feature_type]])
    }

    return(NULL)
  })

  observeEvent(input$clear_cache, {
    embedding_model_cache$model <- NULL
    embedding_model_cache$model_name <- NULL
    embedding_model_cache$last_used <- NULL

    showNotification("Cache cleared. Models will be reloaded as needed.", type = "message")
  })

  observeEvent(input$semantic_ngram_range, {
    ngram_label <- switch(input$semantic_ngram_range,
      "2" = "Bigram",
      "3" = "Trigram",
      "4" = "4-gram",
      "5" = "5-gram",
      paste0(input$semantic_ngram_range, "-gram")
    )

    messages <- c()

    if (!is.null(comparison_results$results[["ngrams"]])) {
      comparison_results$results[["ngrams"]] <- NULL
      comparison_results$calculation_status[["ngrams"]] <- FALSE
      similarity_analysis_triggered(FALSE)
      calculation_state$visualization_shown[["ngrams"]] <- FALSE

      if (!is.null(comparison_results$cached_plots)) {
        plot_keys_to_remove <- grep("^ngrams_", names(comparison_results$cached_plots), value = TRUE)
        for (key in plot_keys_to_remove) {
          comparison_results$cached_plots[[key]] <- NULL
        }
      }

      if (!is.null(comparison_results$cached_stats)) {
        stats_keys_to_remove <- grep("^stats_ngrams_", names(comparison_results$cached_stats), value = TRUE)
        for (key in stats_keys_to_remove) {
          comparison_results$cached_stats[[key]] <- NULL
        }
      }

      messages <- c(messages, "N-gram similarity data cleared")
    }

    if (!is.null(dimred_results$last_params) && dimred_results$last_params$feature_space == "ngrams") {
      dimred_results$pca <- NULL
      dimred_results$tsne <- NULL
      dimred_results$umap <- NULL
      dimred_results$last_method <- NULL
      dimred_results$last_params <- NULL
      messages <- c(messages, "dimensionality reduction results cleared")
    }

    if (!is.null(comparison_results$clustering) && comparison_results$clustering$feature_space == "ngrams") {
      comparison_results$clustering <- NULL
      messages <- c(messages, "clustering results cleared")
    }

    if (isTRUE(document_clustering_results$analysis_run) &&
        !is.null(document_clustering_results$feature_space) &&
        document_clustering_results$feature_space == "ngrams") {
      document_clustering_results$coordinates <- NULL
      document_clustering_results$clusters <- NULL
      document_clustering_results$dimred_complete <- FALSE
      document_clustering_results$analysis_run <- FALSE
      document_clustering_results$labels <- NULL
      messages <- c(messages, "document clustering results cleared")
    }

    if (!is.null(search_results_reactive$method_used) && search_results_reactive$method_used == "ngrams") {
      search_results_reactive$results <- NULL
      search_results_reactive$method_used <- NULL
      search_results_reactive$ngram_size <- NULL
      messages <- c(messages, "search results cleared")
    }

    advanced_results$ai_labels <- NULL
    advanced_results$cross_validation <- NULL

    if (length(messages) > 0) {
      showNotification(
        paste0("⚠ N-gram range changed to ", ngram_label, ". The following data was cleared: ", paste(messages, collapse = ", "), ". Please re-run analyses as needed."),
        type = "warning",
        duration = 10
      )
    }
  })

  observeEvent(input$embedding_model, {
    if (!is.null(comparison_results$results[["embeddings"]])) {
      comparison_results$results[["embeddings"]] <- NULL
      comparison_results$calculation_status[["embeddings"]] <- FALSE
      clear_embeddings_cache()
      similarity_analysis_triggered(FALSE)
      calculation_state$visualization_shown[["embeddings"]] <- FALSE
      showNotification("⚠ Embedding model changed. Please recalculate embeddings similarity.", type = "warning", duration = 5)
      shinyjs::hide("similarity_status")
    }
  })

  observe({
    dfm_available <- get_available_dfm()

    if (!is.null(dfm_available)) {
      current_dfm_hash <- digest::digest(list(
        ndoc = quanteda::ndoc(dfm_available),
        nfeat = quanteda::nfeat(dfm_available),
        features_sample = quanteda::featnames(dfm_available)[1:min(100, quanteda::nfeat(dfm_available))],
        matrix_sample = as.matrix(dfm_available)[1:min(10, quanteda::ndoc(dfm_available)), 1:min(100, quanteda::nfeat(dfm_available))]
      ), algo = "md5")

      if (!is.null(comparison_results$last_dfm_hash) &&
          comparison_results$last_dfm_hash != current_dfm_hash) {
        if (length(comparison_results$results) > 0) {
          comparison_results$results <- list()
          comparison_results$calculation_status <- list()
          clear_embeddings_cache()
          similarity_analysis_triggered(FALSE)
          similarity_visualization_ready(FALSE)
          shinyjs::hide("similarity_status")
          showNotification("Document matrix updated. All similarity caches cleared.", type = "message", duration = 2)
        }
      } else if (!is.null(comparison_results$last_dfm_hash) &&
                 comparison_results$last_dfm_hash == current_dfm_hash) {
        if (length(comparison_results$results) > 0) {
          showNotification("Data unchanged. Previous analysis results are still available.", type = "message", duration = 3)
        }
      }

      comparison_results$last_dfm_hash <- current_dfm_hash
    }
  })

  observeEvent(input$semantic_feature_space, {
    if (input$semantic_feature_space != "embeddings") {
      search_results_reactive$results <- NULL
    }

    dfm_check <- get_available_dfm()
    if (input$semantic_analysis_tabs == "similarity" && !is.null(dfm_check) && quanteda::ndoc(dfm_check) > 0) {
      feature_type <- input$semantic_feature_space

    }
  })


  document_clustering_results <- reactiveValues(
    coordinates = NULL,
    clusters = NULL,
    method = NULL,
    clustering_method = NULL,
    analysis_run = FALSE,
    quality_metrics = list(),
    labels = NULL,
    dimred_complete = FALSE,
    feature_matrix = NULL
  )

  dimred_results <- reactiveValues(
    pca = NULL,
    tsne = NULL,
    umap = NULL,
    last_method = NULL,
    last_params = NULL,
    cached_plots = list()
  )

  dimred_analysis_triggered <- reactiveVal(FALSE)

  observeEvent(input$generate_labels, {
    req(document_clustering_results$clusters)

    showNotification("Generating cluster labels...", type = "message", id = "labelGen")

    tryCatch({
      feature_matrix <- document_clustering_results$feature_matrix

      if (is.null(feature_matrix)) {
        remove_notification_by_id("labelGen")
        showNotification("Feature matrix not available. Please run dimensionality reduction first.", type = "error")
        return()
      }

      clusters <- document_clustering_results$clusters
      method <- input$label_method %||% "tfidf"
      n_terms <- input$n_label_terms %||% 3

      unique_clusters <- sort(unique(clusters[clusters > 0]))
      labels <- list()

      for (cluster_id in unique_clusters) {
        cluster_docs <- which(clusters == cluster_id)
        cluster_features <- feature_matrix[cluster_docs, , drop = FALSE]

        if (ncol(cluster_features) <= 2) {
          labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
          next
        }

        if (method == "tfidf") {
          tf <- colSums(cluster_features)
          doc_freq <- colSums(feature_matrix > 0)
          doc_freq[doc_freq == 0] <- 1
          idf <- log(nrow(feature_matrix) / doc_freq)
          tfidf <- tf * idf
          tfidf <- tfidf[!is.na(tfidf) & !is.infinite(tfidf)]
          if (length(tfidf) == 0) {
            labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
            next
          }
          top_terms <- names(head(sort(tfidf, decreasing = TRUE), n_terms))
        } else if (method == "representative") {
          cluster_mean <- colMeans(cluster_features)
          overall_mean <- colMeans(feature_matrix)
          diff <- cluster_mean - overall_mean
          diff <- diff[!is.na(diff)]
          if (length(diff) == 0) {
            labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
            next
          }
          top_terms <- names(head(sort(diff, decreasing = TRUE), n_terms))
        } else {
          term_freq <- colSums(cluster_features)
          term_freq <- term_freq[term_freq > 0]
          if (length(term_freq) == 0) {
            labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
            next
          }
          top_terms <- names(head(sort(term_freq, decreasing = TRUE), n_terms))
        }

        if (length(top_terms) == 0) {
          labels[[as.character(cluster_id)]] <- paste("Cluster", cluster_id)
        } else {
          labels[[as.character(cluster_id)]] <- paste(top_terms, collapse = ", ")
        }
      }

      document_clustering_results$labels <- labels
      remove_notification_by_id("labelGen")
      show_completion_notification("Labels generated successfully!")

    }, error = function(e) {
      remove_notification_by_id("labelGen")
      show_error_notification(paste("Error generating labels:", e$message))
    })
  })

  observeEvent(input$run_dimensionality_reduction, {
    show_loading_notification("Reducing dimensionality...", id = "loadingDimRed")

    tryCatch({
      dfm_check <- get_available_dfm()

      if (is.null(dfm_check) || quanteda::ndoc(dfm_check) == 0) {
        remove_notification_by_id("loadingDimRed")
        showNotification("Please process documents in the Setup tab first.", type = "error", duration = 5)
        return()
      }

      feature_matrix <- semantic_feature_matrix()
      if (is.null(feature_matrix)) {
        remove_notification_by_id("loadingDimRed")

        feature_space <- input$semantic_feature_space %||% "words"
        if (feature_space == "embeddings") {
          showNotification("No embeddings available. Please calculate embeddings similarity in the Document Similarity tab first.", type = "error", duration = 7)
        } else if (feature_space == "ngrams") {
          showNotification("N-grams require completed preprocessing. Please complete preprocessing steps (including tokens) first.", type = "error", duration = 7)
        } else {
          show_no_feature_matrix_notification()
        }
        return()
      }

      if (nrow(feature_matrix) < 2) {
        remove_notification_by_id("loadingDimRed")
        showNotification("At least 2 documents are required for dimensionality reduction.", type = "error", duration = 5)
        return()
      }

      dup_rows <- duplicated(feature_matrix)
      if (any(dup_rows)) {
        dup_count <- sum(dup_rows)
        feature_matrix <- feature_matrix[!dup_rows, , drop = FALSE]
        showNotification(
          paste("Removed", dup_count, "duplicate document(s) for dimensionality reduction."),
          type = "warning",
          duration = 5
        )
      }

      if (nrow(feature_matrix) < 2) {
        remove_notification_by_id("loadingDimRed")
        showNotification("After removing duplicates, less than 2 unique documents remain.", type = "error", duration = 5)
        return()
      }

      method <- input$semantic_dimred_method %||% "UMAP"
      coords <- NULL

      if (method == "PCA") {
        pca_result <- prcomp(feature_matrix, center = TRUE, scale. = TRUE)
        coords <- pca_result$x[, 1:2]
      } else if (method == "t-SNE") {
        set.seed(123)
        tsne_result <- Rtsne::Rtsne(feature_matrix,
                                    dims = 2,
                                    perplexity = input$semantic_tsne_perplexity %||% 30,
                                    max_iter = input$semantic_tsne_max_iter %||% 1000,
                                    check_duplicates = FALSE)
        coords <- tsne_result$Y
      } else if (method == "UMAP") {
        set.seed(123)
        umap_result <- umap::umap(feature_matrix,
                                  n_neighbors = input$semantic_umap_neighbors %||% 15,
                                  min_dist = input$semantic_umap_min_dist %||% 0.1)
        coords <- umap_result$layout
      }

      document_clustering_results$coordinates <- coords
      document_clustering_results$method <- method
      document_clustering_results$feature_matrix <- feature_matrix
      document_clustering_results$dimred_complete <- TRUE
      document_clustering_results$clusters <- NULL
      document_clustering_results$clustering_method <- "none"
      document_clustering_results$quality_metrics <- list()
      document_clustering_results$analysis_run <- TRUE
      document_clustering_results$feature_space <- input$semantic_feature_space %||% "words"
      document_clustering_results$ngram_range <- if (document_clustering_results$feature_space == "ngrams") input$semantic_ngram_range else NULL

      remove_notification_by_id("loadingDimRed")
      show_completion_notification("Dimensionality reduction complete! You can now apply clustering if desired.", duration = 5)

    }, error = function(e) {
      cat("ERROR in dimensionality reduction:", e$message, "\n")
      print(e)
      remove_notification_by_id("loadingDimRed")
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        paste("Dimensionality reduction error:", e$message)
      } else {
        "An error occurred during dimensionality reduction. Please ensure documents are processed and all required parameters are set."
      }
      showNotification(error_msg, type = "error", duration = 5)
    })
  })

  observeEvent(input$apply_clustering, {
    show_loading_notification("Applying clustering...", id = "loadingClustering")

    tryCatch({
      if (!isTRUE(document_clustering_results$dimred_complete)) {
        remove_notification_by_id("loadingClustering")
        showNotification("Please run dimensionality reduction first (Step 1).", type = "error", duration = 5)
        return()
      }

      coords <- document_clustering_results$coordinates
      if (is.null(coords) || nrow(coords) < 2) {
        remove_notification_by_id("loadingClustering")
        showNotification("Valid dimensionality reduction results not found. Please run Step 1 first.", type = "error", duration = 5)
        return()
      }

      clustering_method <- input$document_clustering_method %||% "none"

      if (clustering_method == "none") {
        remove_notification_by_id("loadingClustering")
        showNotification("Please select a clustering method other than 'None'.", type = "warning", duration = 5)
        return()
      }

      clusters <- NULL

      if (clustering_method == "kmeans") {
        k <- input$kmeans_k %||% 5
        km_result <- kmeans(coords, centers = k, nstart = 25)
        clusters <- km_result$cluster
      } else if (clustering_method == "hierarchical") {
        k <- input$hclust_k %||% 5
        hc_result <- hclust(dist(coords))
        clusters <- cutree(hc_result, k = k)
      } else if (clustering_method == "dbscan") {
        eps <- input$dbscan_eps %||% 0.5
        minPts <- input$dbscan_minPts %||% 5
        dbscan_result <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
        clusters <- dbscan_result$cluster
      } else if (clustering_method == "hdbscan") {
        min_size <- input$hdbscan_min_cluster_size %||% 5
        hdbscan_result <- dbscan::hdbscan(coords, minPts = min_size)
        clusters <- hdbscan_result$cluster
      }

      quality_metrics <- list()
      if (!is.null(clusters)) {
        if (length(unique(clusters)) > 1) {
          sil <- cluster::silhouette(clusters, dist(coords))
          quality_metrics$silhouette <- mean(sil[, 3])
        }
        quality_metrics$n_clusters <- length(unique(clusters[clusters > 0]))
        if (min(clusters) == 0) {
          quality_metrics$outlier_pct <- sum(clusters == 0) / length(clusters) * 100
        }
      }

      document_clustering_results$clusters <- clusters
      document_clustering_results$clustering_method <- clustering_method
      document_clustering_results$quality_metrics <- quality_metrics
      document_clustering_results$analysis_run <- TRUE

      remove_notification_by_id("loadingClustering")
      show_completion_notification("Clustering applied successfully!", duration = 5)

    }, error = function(e) {
      cat("ERROR in clustering:", e$message, "\n")
      print(e)
      remove_notification_by_id("loadingClustering")
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        paste("Clustering error:", e$message)
      } else {
        "An error occurred during clustering. Please check your parameters and try again."
      }
      showNotification(error_msg, type = "error", duration = 5)
    })
  })

  observe({
    if (isTRUE(document_clustering_results$dimred_complete)) {
      shinyjs::enable("apply_clustering")
    } else {
      shinyjs::disable("apply_clustering")
    }
  })

  observeEvent(input$run_document_clustering_analysis, {
    show_loading_notification("Running document clustering analysis...", id = "loadingDocClustering")

    tryCatch({
      dfm_check <- get_available_dfm()

      if (is.null(dfm_check) || quanteda::ndoc(dfm_check) == 0) {
        remove_notification_by_id("loadingDocClustering")
        showNotification("Please process documents in the Setup tab first.", type = "error", duration = 5)
        return()
      }

      feature_matrix <- semantic_feature_matrix()
      if (is.null(feature_matrix)) {
        remove_notification_by_id("loadingDocClustering")

        feature_space <- input$semantic_feature_space %||% "words"
        if (feature_space == "embeddings") {
          showNotification("No embeddings available. Please calculate embeddings similarity in the Document Similarity tab first.", type = "error", duration = 7)
        } else if (feature_space == "ngrams") {
          showNotification("N-grams require completed preprocessing. Please complete preprocessing steps (including tokens) first.", type = "error", duration = 7)
        } else {
          show_no_feature_matrix_notification()
        }
        return()
      }

      if (nrow(feature_matrix) < 2) {
        remove_notification_by_id("loadingDocClustering")
        showNotification("At least 2 documents are required for clustering analysis.", type = "error", duration = 5)
        return()
      }

      dup_rows <- duplicated(feature_matrix)
      if (any(dup_rows)) {
        dup_count <- sum(dup_rows)
        feature_matrix <- feature_matrix[!dup_rows, , drop = FALSE]
        showNotification(
          paste("Removed", dup_count, "duplicate document(s) for dimensionality reduction."),
          type = "warning",
          duration = 5
        )
      }

      if (nrow(feature_matrix) < 2) {
        remove_notification_by_id("loadingDocClustering")
        showNotification("After removing duplicates, less than 2 unique documents remain.", type = "error", duration = 5)
        return()
      }

      method <- input$semantic_dimred_method %||% "UMAP"
      coords <- NULL

      if (method == "PCA") {
        pca_result <- prcomp(feature_matrix, center = TRUE, scale. = TRUE)
        coords <- pca_result$x[, 1:2]
      } else if (method == "t-SNE") {
        set.seed(123)
        tsne_result <- Rtsne::Rtsne(feature_matrix,
                                    dims = 2,
                                    perplexity = input$semantic_tsne_perplexity %||% 30,
                                    max_iter = input$semantic_tsne_max_iter %||% 1000,
                                    check_duplicates = FALSE)
        coords <- tsne_result$Y
      } else if (method == "UMAP") {
        set.seed(123)
        umap_result <- umap::umap(feature_matrix,
                                  n_neighbors = input$semantic_umap_neighbors %||% 15,
                                  min_dist = input$semantic_umap_min_dist %||% 0.1)
        coords <- umap_result$layout
      }

      clusters <- NULL
      clustering_method <- input$document_clustering_method %||% "none"

      if (clustering_method != "none") {
        if (clustering_method == "kmeans") {
          k <- input$kmeans_k %||% 5
          km_result <- kmeans(coords, centers = k, nstart = 25)
          clusters <- km_result$cluster
        } else if (clustering_method == "hierarchical") {
          k <- input$hclust_k %||% 5
          hc_result <- hclust(dist(coords))
          clusters <- cutree(hc_result, k = k)
        } else if (clustering_method == "dbscan") {
          eps <- input$dbscan_eps %||% 0.5
          minPts <- input$dbscan_minPts %||% 5
          dbscan_result <- dbscan::dbscan(coords, eps = eps, minPts = minPts)
          clusters <- dbscan_result$cluster
        } else if (clustering_method == "hdbscan") {
          min_size <- input$hdbscan_min_cluster_size %||% 5
          hdbscan_result <- dbscan::hdbscan(coords, minPts = min_size)
          clusters <- hdbscan_result$cluster
        }
      }

      quality_metrics <- list()
      if (!is.null(clusters)) {
        if (length(unique(clusters)) > 1) {
          sil <- cluster::silhouette(clusters, dist(coords))
          quality_metrics$silhouette <- mean(sil[, 3])
        }
        quality_metrics$n_clusters <- length(unique(clusters[clusters > 0]))
        if (min(clusters) == 0) {
          quality_metrics$outlier_pct <- sum(clusters == 0) / length(clusters) * 100
        }
      }

      document_clustering_results$coordinates <- coords
      document_clustering_results$clusters <- clusters
      document_clustering_results$method <- method
      document_clustering_results$clustering_method <- clustering_method
      document_clustering_results$quality_metrics <- quality_metrics
      document_clustering_results$analysis_run <- TRUE

      remove_notification_by_id("loadingDocClustering")
      show_completion_notification("Document clustering analysis complete!")

    }, error = function(e) {
      cat("ERROR in document clustering:", e$message, "\n")
      print(e)
      remove_notification_by_id("loadingDocClustering")
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        paste("Clustering error:", e$message)
      } else {
        "An error occurred during clustering analysis. Please ensure documents are processed and all required parameters are set."
      }
      showNotification(error_msg, type = "error", duration = 5)
    })
  })

  observeEvent(input$run_semantic_analysis_dimred, {
    show_loading_notification("Running dimensionality reduction...", id = "loadingDimRed")

    dimred_results$cached_plots <- list()
    dimred_results$just_ran_analysis <- TRUE
    dimred_results$calculating <- TRUE

    dfm_check <- get_available_dfm()
    if (is.null(dfm_check) || quanteda::ndoc(dfm_check) == 0) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      showNotification(
        "No document-feature matrix (DFM) found. Please complete preprocessing and create the DFM before running semantic analysis.",
        type = "error"
      )
      return()
    }

    dfm_available <- get_available_dfm()
    if (is.null(dfm_available)) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      showNotification("Please complete the preprocessing steps and create a DFM first.", type = "error")
      return()
    }

    processed_docs_null <- tryCatch({
      is.null(processed_documents())
    }, error = function(e) {
      TRUE
    })

    if (processed_docs_null) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      shiny::showModal(shiny::modalDialog(
        title = "Document Configuration Required",
        p("Please run Document Configuration first."),
        p("Configure documents in the Document Configuration section before proceeding."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    feature_type <- input$semantic_feature_space %||% "words"
    if (is.null(comparison_results$calculation_status[[feature_type]]) ||
        !comparison_results$calculation_status[[feature_type]]) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      showNotification(
        paste("⚠ Please calculate", feature_type, "similarity for this method first"),
        type = "warning",
        duration = 5
      )
      return()
    }

    feature_matrix <- semantic_feature_matrix()
    if (is.null(feature_matrix)) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      showNotification("No feature matrix available. Please process documents first.", type = "error")
      return()
    }

    n_docs <- nrow(feature_matrix)

    if (n_docs < 2) {
      dimred_results$calculating <- FALSE
      try(removeNotification("loadingDimRed"), silent = TRUE)
      showNotification("Need at least 2 documents for dimensionality reduction", type = "error")
      return()
    }

    current_params <- list(
      method = input$semantic_dimred_method,
      pca_dims = input$semantic_pca_dims,
      tsne_perplexity = input$semantic_tsne_perplexity,
      tsne_max_iter = input$semantic_tsne_max_iter,
      umap_neighbors = input$semantic_umap_neighbors,
      umap_min_dist = input$semantic_umap_min_dist,
      seed = input$semantic_cluster_seed,
      feature_space = input$semantic_feature_space,
      ngram_range = if (input$semantic_feature_space == "ngrams") input$semantic_ngram_range else NULL
    )

    if (is.null(dimred_results$last_params) ||
        !identical(dimred_results$last_params, current_params)) {
      dimred_results$last_params <- current_params

      pca_dims <- min(input$semantic_pca_dims, ncol(feature_matrix), nrow(feature_matrix) - 1)

      col_vars <- apply(feature_matrix, 2, var, na.rm = TRUE)
      constant_cols <- which(col_vars == 0 | is.na(col_vars))

      if (length(constant_cols) > 0) {
        showNotification(paste("Removing", length(constant_cols), "constant/zero columns before PCA"), type = "message")
        feature_matrix <- feature_matrix[, -constant_cols, drop = FALSE]
        pca_dims <- min(pca_dims, ncol(feature_matrix), nrow(feature_matrix) - 1)
      }

      if (ncol(feature_matrix) < 2) {
        dimred_results$calculating <- FALSE
        try(removeNotification("loadingDimRed"), silent = TRUE)
        showNotification("Insufficient non-constant features for PCA analysis", type = "error")
        return()
      }

      pca_result <- prcomp(feature_matrix, scale. = TRUE, center = TRUE, rank. = pca_dims)
      dimred_results$pca <- pca_result

      if (requireNamespace("umap", quietly = TRUE)) {
        set.seed(input$semantic_cluster_seed)
        umap_config <- umap::umap.defaults
        safe_n_neighbors <- min(input$semantic_umap_neighbors, n_docs - 1, 15)
        umap_config$n_neighbors <- safe_n_neighbors
        umap_config$min_dist <- input$semantic_umap_min_dist
        umap_config$random_state <- input$semantic_cluster_seed
        umap_config$metric <- "cosine"
        umap_result <- umap::umap(pca_result$x, config = umap_config)
        dimred_results$umap <- umap_result
      }

      if (requireNamespace("Rtsne", quietly = TRUE)) {
        set.seed(input$semantic_cluster_seed)
        safe_perplexity <- min(input$semantic_tsne_perplexity %||% 30, floor((n_docs - 1) / 3))

        tsne_result <- tryCatch(
          {
            Rtsne::Rtsne(
              pca_result$x,
              dims = 2,
              perplexity = safe_perplexity,
              max_iter = input$semantic_tsne_max_iter %||% 1000,
              check_duplicates = FALSE,
              pca = FALSE,
              verbose = FALSE
            )
          },
          error = function(e) {
            showNotification(paste("t-SNE failed:", e$message), type = "warning")
            NULL
          }
        )
        dimred_results$tsne <- tsne_result
      }

      dimred_results$last_method <- input$semantic_dimred_method
    }

    dimred_results$calculating <- FALSE
    try(removeNotification("loadingDimRed"), silent = TRUE)
    show_completion_notification("Dimensionality reduction completed!")

    dimred_analysis_triggered(TRUE)
  })

  dimred_analysis_reactive <- reactive({
    req(dimred_analysis_triggered())

    if (!is.null(dimred_results$last_method)) {
      return(dimred_results)
    }

    return(NULL)
  })

  observeEvent(input$run_parameter_optimization, {
    dfm_check <- get_available_dfm()
    if (is.null(dfm_check) || quanteda::ndoc(dfm_check) == 0) {
      shiny::showModal(shiny::modalDialog(
        title = "Preprocessing Required",
        p("No document-feature matrix (DFM) found."),
        p("Please complete preprocessing steps first."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    feature_matrix <- semantic_feature_matrix()
    if (is.null(feature_matrix)) {
      showNotification("No feature matrix available. Please process documents first.", type = "error")
      return()
    }

    if (nrow(feature_matrix) < 3) {
      showNotification("Need at least 3 documents for parameter optimization", type = "error")
      return()
    }

    show_loading_notification("Running parameter optimization...", id = "optimizationProgress")

    opt_results <- time_operation("parameter_optimization", function() {
      find_optimal_parameters(
        data_matrix = feature_matrix,
        method = input$optimization_method %||% "silhouette",
        range = input$optimization_range %||% c(2, 10)
      )
    })

    if (!is.null(opt_results)) {
      optimization_results$results <- opt_results

      if (input$semantic_cluster_method == "kmeans") {
        updateSliderInput(session, "kmeans_n_clusters", value = opt_results$optimal_k)
      }

      showNotification(
        paste("Optimal k =", opt_results$optimal_k, "with score =", round(opt_results$optimal_score, 4)),
        type = "message"
      )
    }

    try(removeNotification("optimizationProgress"), silent = TRUE)
  })

  observeEvent(input$run_semantic_analysis_clustering, {
    dfm_check <- get_available_dfm()
    if (is.null(dfm_check) || quanteda::ndoc(dfm_check) == 0) {
      showNotification(
        "No document-feature matrix (DFM) found. Please complete preprocessing and create the DFM before running semantic analysis.",
        type = "error"
      )
      return()
    }

    dfm_check <- get_available_dfm()
    if (is.null(dfm_check)) {
      showNotification("Please complete the preprocessing steps and create a DFM first.", type = "error")
      return()
    }

    processed_docs_null <- tryCatch({
      is.null(processed_documents())
    }, error = function(e) {
      TRUE
    })

    if (processed_docs_null) {
      showNotification("Run Document Configuration first.", type = "error", duration = 10)
      return()
    }

    feature_space <- input$semantic_feature_space %||% "words"
    if (length(comparison_results$results) == 0 || is.null(comparison_results$results[[feature_space]])) {
      feature_label <- switch(feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        feature_space
      )
      showNotification(
        paste(feature_label, "similarity must be calculated first. Please go to Similarity Analysis tab and calculate similarity."),
        type = "warning",
        duration = 10
      )
      return()
    }

    show_loading_notification("Running clustering analysis...", id = "loadingClustering")
    comparison_results$clustering_calculating <- TRUE

    if (length(comparison_results$results) == 0) {
      feature_matrix <- semantic_feature_matrix()
      if (is.null(feature_matrix)) {
        comparison_results$clustering_calculating <- FALSE
        remove_notification_by_id("loadingClustering")
        showNotification("No feature matrix available. Please process documents first.", type = "error")
        return()
      }

      feature_space <- input$semantic_feature_space %||% "words"

      if (feature_space == "words") {
        dfm_check <- tryCatch(dfm_outcome(), error = function(e) NULL)
        if (is.null(dfm_check)) {
          comparison_results$clustering_calculating <- FALSE
          remove_notification_by_id("loadingClustering")
          showNotification("No DFM available. Please complete preprocessing first.", type = "error")
          return()
        }
        feature_matrix <- as.matrix(dfm_check)
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      } else if (feature_space == "embeddings") {
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      } else {
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      }
    } else {
      recent_result <- comparison_results$results[[length(comparison_results$results)]]
      similarity_matrix <- recent_result$similarity_matrix
    }

    if (is.null(similarity_matrix)) {
      comparison_results$clustering_calculating <- FALSE
      try(removeNotification("loadingClustering"), silent = TRUE)
      showNotification("No similarity matrix available. Please run similarity analysis first.", type = "error")
      return()
    }

    if (any(is.na(similarity_matrix)) || any(!is.finite(similarity_matrix))) {
      comparison_results$clustering_calculating <- FALSE
      try(removeNotification("loadingClustering"), silent = TRUE)
      showNotification("Similarity matrix contains missing or infinite values. Please check data.", type = "error")
      return()
    }

    if (nrow(similarity_matrix) < 2 || ncol(similarity_matrix) < 2) {
      comparison_results$clustering_calculating <- FALSE
      try(removeNotification("loadingClustering"), silent = TRUE)
      showNotification("Insufficient data for clustering analysis. Need at least 2 documents.", type = "error")
      return()
    }

    col_vars <- apply(similarity_matrix, 2, var, na.rm = TRUE)
    if (all(col_vars == 0 | is.na(col_vars))) {
      comparison_results$clustering_calculating <- FALSE
      try(removeNotification("loadingClustering"), silent = TRUE)
      showNotification("All features have zero variance. Cannot perform clustering.", type = "error")
      return()
    }

    cluster_method <- input$semantic_cluster_method

    has_dimred_results <- !is.null(dimred_results$last_method) &&
      dimred_results$last_params$feature_space == input$semantic_feature_space

    has_umap_dimred <- !is.null(dimred_results$umap) &&
      dimred_results$last_method == "UMAP" &&
      dimred_results$last_params$feature_space == input$semantic_feature_space

    has_pca_dimred <- !is.null(dimred_results$pca) &&
      dimred_results$last_method == "PCA" &&
      dimred_results$last_params$feature_space == input$semantic_feature_space

    has_tsne_dimred <- !is.null(dimred_results$tsne) &&
      dimred_results$last_method == "t-SNE" &&
      dimred_results$last_params$feature_space == input$semantic_feature_space

    method_display_names <- list(
      "dbscan" = if (has_dimred_results) paste("DBSCAN on", dimred_results$last_method) else "DBSCAN",
      "kmeans" = "K-means",
      "hierarchical" = "Hierarchical",
      "neural" = "Neural Topic Model",
      "semantic_unified" = "Unified Semantic Topic Modeling"
    )

    method_display_name <- method_display_names[[cluster_method]] %||% cluster_method

    existing_embedding <- NULL
    embedding_method <- NULL
    umap_params <- list()

    if (cluster_method == "dbscan" && has_dimred_results) {
      if (has_umap_dimred) {
        existing_embedding <- dimred_results$umap$layout
        embedding_method <- "UMAP"
      } else if (has_pca_dimred) {
        existing_embedding <- dimred_results$pca$x[, 1:2]
        embedding_method <- "PCA"
      } else if (has_tsne_dimred) {
        existing_embedding <- dimred_results$tsne$Y
        embedding_method <- "t-SNE"
      }
    }

    dbscan_params <- list(
      eps = input$dbscan_eps %||% 0,
      min_samples = input$dbscan_min_samples %||% 5
    )

    n_clusters <- if (cluster_method == "kmeans") {
      if (!is.null(input$kmeans_n_clusters) && input$kmeans_n_clusters > 0) {
        input$kmeans_n_clusters
      } else {
        NULL
      }
    } else {
      NULL
    }

    show_loading_notification(paste("Running", method_display_name, "clustering analysis..."), id = "loadingClustering")

    set.seed(input$semantic_cluster_seed)

    clustering_result <- tryCatch(
      {
        if (cluster_method == "semantic_unified") {
          texts <- document_display_data()$combined_text

          unified_result <- TextAnalysisR::fit_embedding_topics(
            texts = texts,
            method = input$unified_method,
            n_topics = input$unified_n_topics,
            embedding_model = input$embedding_model %||% "all-MiniLM-L6-v2",
            clustering_method = "kmeans",
            min_topic_size = input$unified_min_topic_size,
            seed = input$semantic_cluster_seed,
            verbose = FALSE
          )

          advanced_results$semantic_unified <- unified_result

          list(
            clusters = unified_result$topic_assignments,
            method = "semantic_unified",
            n_clusters = unified_result$n_topics,
            n_clusters_found = unified_result$n_topics,
            auto_detected = FALSE,
            detection_method = "Unified Semantic Modeling",
            topics = unified_result$topics,
            topic_keywords = unified_result$topic_keywords,
            embeddings = unified_result$embeddings,
            similarity_matrix = unified_result$similarity_matrix
          )
        } else if (cluster_method == "neural") {
          texts <- document_display_data()$combined_text

          neural_result <- TextAnalysisR::run_neural_topics_internal(
            texts = texts,
            n_topics = input$neural_n_topics,
            hidden_units = input$neural_hidden_size,
            embedding_model = input$embedding_model %||% "all-MiniLM-L6-v2",
            seed = input$semantic_cluster_seed
          )

          advanced_results$neural_topic_model <- neural_result

          list(
            clusters = neural_result$topic_assignments,
            method = "neural",
            n_clusters = neural_result$n_topics,
            n_clusters_found = neural_result$n_topics,
            auto_detected = FALSE,
            detection_method = "Neural Topic Model",
            topics = neural_result$topics,
            topic_keywords = neural_result$topic_keywords
          )
        } else if (cluster_method == "dbscan" && !is.null(existing_embedding)) {
          calculate_enhanced_clustering(
            similarity_matrix = similarity_matrix,
            method = cluster_method,
            n_clusters = n_clusters,
            umap_params = umap_params,
            dbscan_params = dbscan_params,
            existing_embedding = existing_embedding,
            embedding_method = embedding_method
          )
        } else {
          result <- TextAnalysisR::cluster_embeddings(
            data_matrix = similarity_matrix,
            method = if (cluster_method == "dbscan") "umap_dbscan" else cluster_method,
            n_clusters = n_clusters %||% 0,
            dbscan_eps = dbscan_params$eps,
            dbscan_min_samples = dbscan_params$min_samples,
            seed = input$semantic_cluster_seed,
            verbose = FALSE
          )

          if (!is.null(result)) {
            result$n_clusters_found <- result$n_clusters
            result$auto_detected <- result$auto_detected %||% (n_clusters %||% 0) == 0
            result$detection_method <- result$detection_method %||%
              if (result$auto_detected) "Automatic Detection" else "Manual"
          }
          result
        }
      },
      error = function(e) {
        error_msg <- e$message
        if (grepl("constant.*zero.*variance", error_msg, ignore.case = TRUE)) {
          showNotification("Clustering error: cannot rescale a constant/zero column to unit variance", type = "error")
        } else if (grepl("missing.*TRUE.*FALSE", error_msg, ignore.case = TRUE)) {
          showNotification("Clustering error: missing value where TRUE/FALSE needed", type = "error")
        } else {
          showNotification(paste("Clustering error:", error_msg), type = "error")
        }
        return(NULL)
      }
    )

    if (is.null(clustering_result)) {
      showNotification("Clustering analysis failed. Please try a different method.", type = "error")
      return()
    }

    clustering_result$similarity_matrix <- similarity_matrix
    clustering_result$feature_space <- input$semantic_feature_space
    clustering_result$ngram_range <- if (input$semantic_feature_space == "ngrams") input$semantic_ngram_range else NULL
    comparison_results$clustering <- clustering_result
    comparison_results$update_trigger <- isolate(comparison_results$update_trigger) + 1

    silhouette_text <- if (!is.null(clustering_result$silhouette) && !is.na(clustering_result$silhouette)) {
      paste("Silhouette:", round(clustering_result$silhouette, 3))
    } else {
      "Silhouette: N/A"
    }

    davies_bouldin_text <- if (!is.null(clustering_result$davies_bouldin) && !is.na(clustering_result$davies_bouldin)) {
      paste("Davies-Bouldin:", round(clustering_result$davies_bouldin, 3))
    } else {
      "Davies-Bouldin: N/A"
    }

    calinski_harabasz_text <- if (!is.null(clustering_result$calinski_harabasz) && !is.na(clustering_result$calinski_harabasz)) {
      paste("Calinski-Harabasz:", round(clustering_result$calinski_harabasz, 1))
    } else {
      "Calinski-Harabasz: N/A"
    }

    metrics_text <- paste(silhouette_text, davies_bouldin_text, calinski_harabasz_text, sep = " | ")

    auto_text <- if (!is.null(clustering_result$auto_detected) && clustering_result$auto_detected) {
      detection_method <- clustering_result$detection_method %||% "Advanced Analysis"
      if (cluster_method == "dbscan") {
        outlier_info <- if (!is.null(clustering_result$noise_ratio)) {
          paste("| Outlier:", round(clustering_result$noise_ratio * 100, 1), "%")
        } else {
          ""
        }
        paste("(", clustering_result$n_clusters, "clusters auto-detected via", detection_method, outlier_info, ")")
      } else {
        paste("(", clustering_result$n_clusters, "clusters auto-detected via", detection_method, ")")
      }
    } else {
      ""
    }

    comparison_results$clustering_calculating <- FALSE
    try(removeNotification("loadingClustering"), silent = TRUE)
    show_completion_notification(paste(method_display_name, "clustering completed!", auto_text, metrics_text))

    clustering_analysis_triggered(TRUE)
  })

  clustering_analysis_triggered <- reactiveVal(FALSE)

  optimization_results <- reactiveValues(results = NULL)

  clustering_analysis_reactive <- reactive({
    req(clustering_analysis_triggered())

    input$semantic_feature_space
    input$semantic_ngram_range
    input$semantic_cluster_method
    input$umap_n_neighbors
    input$umap_min_dist
    input$umap_n_components
    input$umap_metric
    input$dbscan_eps
    input$dbscan_min_samples
    input$kmeans_n_clusters
    input$semantic_cluster_seed
    input$outlier_reduction_method
    input$outlier_threshold

    dfm_check <- tryCatch(dfm_outcome(), error = function(e) {
      cat("clustering_analysis_reactive: Error getting dfm_outcome:", e$message, "\n")
      NULL
    })

    if (is.null(dfm_check)) {
      return(NULL)
    }

    if (quanteda::ndoc(dfm_check) == 0) {
      return(NULL)
    }

    processed_docs_null <- tryCatch({
      is.null(processed_documents())
    }, error = function(e) {
      TRUE
    })

    if (processed_docs_null) {
      return(NULL)
    }

    if (length(comparison_results$results) == 0) {
      feature_matrix <- semantic_feature_matrix()
      if (is.null(feature_matrix)) {
        return(NULL)
      }

      feature_space <- input$semantic_feature_space %||% "words"

      if (feature_space == "words") {
        dfm_check <- tryCatch(dfm_outcome(), error = function(e) NULL)
        if (is.null(dfm_check)) {
          return(NULL)
        }
        feature_matrix <- as.matrix(dfm_check)
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      } else if (feature_space == "embeddings") {
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      } else {
        similarity_matrix <- TextAnalysisR::calculate_cosine_similarity(feature_matrix)
      }
    } else {
      recent_result <- comparison_results$results[[length(comparison_results$results)]]
      similarity_matrix <- recent_result$similarity_matrix
    }

    if (is.null(similarity_matrix)) {
      return(NULL)
    }

    if (any(is.na(similarity_matrix)) || any(!is.finite(similarity_matrix))) {
      return(NULL)
    }

    if (nrow(similarity_matrix) < 2 || ncol(similarity_matrix) < 2) {
      return(NULL)
    }

    col_vars <- apply(similarity_matrix, 2, var, na.rm = TRUE)
    if (all(col_vars == 0 | is.na(col_vars))) {
      return(NULL)
    }

    cluster_method <- input$semantic_cluster_method

    existing_embedding <- NULL
    embedding_method <- NULL

    umap_params <- list(
      n_neighbors = input$umap_n_neighbors %||% 15,
      min_dist = input$umap_min_dist %||% 0.1,
      n_components = input$umap_n_components %||% 10,
      metric = input$umap_metric %||% "cosine"
    )

    dbscan_params <- list(
      eps = input$dbscan_eps %||% 0,
      min_samples = input$dbscan_min_samples %||% 5
    )

    n_clusters <- if (cluster_method == "kmeans") {
      if (!is.null(input$kmeans_n_clusters) && input$kmeans_n_clusters > 0) {
        input$kmeans_n_clusters
      } else {
        NULL
      }
    } else {
      NULL
    }

    set.seed(input$semantic_cluster_seed)

    clustering_result <- tryCatch(
      {
        calculate_enhanced_clustering(
          similarity_matrix = similarity_matrix,
          method = cluster_method,
          n_clusters = n_clusters,
          umap_params = umap_params,
          dbscan_params = dbscan_params,
          existing_embedding = existing_embedding,
          embedding_method = embedding_method
        )
      },
      error = function(e) {
        return(NULL)
      }
    )

    if (is.null(clustering_result)) {
      return(NULL)
    }

    clustering_result$similarity_matrix <- similarity_matrix
    clustering_result$feature_space <- input$semantic_feature_space
    clustering_result$ngram_range <- if (input$semantic_feature_space == "ngrams") input$semantic_ngram_range else NULL
    comparison_results$clustering <- clustering_result
    comparison_results$update_trigger <- isolate(comparison_results$update_trigger) + 1
  })

  reduce_outliers <- function(clustering_result, similarity_matrix, method = "reassign", threshold = 0.3) {
    tryCatch({
      outlier_indices <- which(clustering_result$clusters == 0)

      if (length(outlier_indices) == 0) {
        return(clustering_result)
      }

      result <- clustering_result

      if (method == "remove") {
        non_outlier_indices <- which(clustering_result$clusters != 0)

        if (length(non_outlier_indices) == 0) {
          showNotification("Cannot remove all documents - they are all outliers.", type = "error")
          return(NULL)
        }

        result$clusters <- clustering_result$clusters[non_outlier_indices]
        result$outliers_removed <- length(outlier_indices)
        result$outlier_reduction_method <- "remove"

        if (!is.null(similarity_matrix)) {
          result$similarity_matrix <- similarity_matrix[non_outlier_indices, non_outlier_indices]
        }

        if (!is.null(clustering_result$umap_embedding)) {
          result$umap_embedding <- clustering_result$umap_embedding[non_outlier_indices, ]
        }

        result$document_indices <- non_outlier_indices

      } else if (method == "reassign") {
        reassigned_count <- 0
        new_clusters <- clustering_result$clusters

        for (outlier_idx in outlier_indices) {
          outlier_similarities <- similarity_matrix[outlier_idx, ]
          non_outlier_indices <- which(clustering_result$clusters != 0)

          if (length(non_outlier_indices) > 0) {
            best_similarity <- max(outlier_similarities[non_outlier_indices], na.rm = TRUE)

            if (!is.na(best_similarity) && best_similarity >= threshold) {
              best_match_idx <- non_outlier_indices[which.max(outlier_similarities[non_outlier_indices])]
              new_clusters[outlier_idx] <- clustering_result$clusters[best_match_idx]
              reassigned_count <- reassigned_count + 1
            } else {
              cluster_centroids <- calculate_cluster_centroids(
                clustering_result$umap_embedding,
                clustering_result$clusters
              )

              if (!is.null(cluster_centroids)) {
                outlier_embedding <- clustering_result$umap_embedding[outlier_idx, ]
                centroid_distances <- apply(cluster_centroids, 1, function(centroid) {
                  sqrt(sum((outlier_embedding - centroid)^2))
                })

                nearest_cluster <- which.min(centroid_distances)
                new_clusters[outlier_idx] <- nearest_cluster
                reassigned_count <- reassigned_count + 1
              }
            }
          }
        }

        result$clusters <- new_clusters
        result$outliers_reassigned <- reassigned_count
        result$outlier_reduction_method <- "reassign"
        result$outlier_threshold <- threshold
      }

      return(result)

    }, error = function(e) {
      showNotification(paste("Error in outlier reduction:", e$message), type = "error")
      return(NULL)
    })
  }

  calculate_cluster_centroids <- function(embeddings, clusters) {
    tryCatch({
      unique_clusters <- unique(clusters[clusters > 0])
      if (length(unique_clusters) == 0) return(NULL)

      centroids <- matrix(0, nrow = length(unique_clusters), ncol = ncol(embeddings))

      for (i in seq_along(unique_clusters)) {
        cluster <- unique_clusters[i]
        cluster_indices <- which(clusters == cluster)
        if (length(cluster_indices) > 0) {
          centroids[i, ] <- colMeans(embeddings[cluster_indices, , drop = FALSE])
        }
      }

      return(centroids)
    }, error = function(e) {
      return(NULL)
    })
  }

  observeEvent(input$reduce_outliers, {
    if (!isTRUE(input$reduce_outliers)) {
      return()
    }

    if (is.null(comparison_results$clustering)) {
      showNotification("✗ No clustering results available. Please run topic discovery first.", type = "error", duration = 5)
      return()
    }

    clustering_result <- comparison_results$clustering
    cluster_method <- clustering_result$method %||% "unknown"

    if (cluster_method != "umap_dbscan") {
      showNotification("Outlier reduction is only available for UMAP + DBSCAN clustering.", type = "warning")
      return()
    }

    if (!any(clustering_result$clusters == 0)) {
      showNotification("No outliers found in the current clustering results.", type = "message")
      return()
    }

    method <- input$outlier_reduction_method
    threshold <- input$outlier_threshold

    if (method == "none") {
      showNotification("Please select an outlier reduction method.", type = "warning")
      return()
    }

    showNotification("Reducing outliers...", type = "message", duration = NULL, id = "reducingOutliers")

    similarity_matrix <- clustering_result$similarity_matrix
    if (is.null(similarity_matrix)) {
      showNotification("Similarity matrix not available. Please run clustering analysis again.", type = "error")
      return()
    }

    reduced_result <- reduce_outliers(clustering_result, similarity_matrix, method, threshold)

    if (is.null(reduced_result)) {
      showNotification("Outlier reduction failed.", type = "error")
      return()
    }

    comparison_results$clustering <- reduced_result

    comparison_results$update_trigger <- isolate(comparison_results$update_trigger) + 1

    if (method == "remove") {
      removed_count <- reduced_result$outliers_removed %||% 0
      showNotification(paste("Removed", removed_count, "outliers from clustering results."), type = "message")
    } else if (method == "reassign") {
      reassigned_count <- reduced_result$outliers_reassigned %||% 0
      total_outliers <- sum(clustering_result$clusters == 0)
      showNotification(paste("Reassigned", reassigned_count, "out of", total_outliers, "outliers to nearest clusters."), type = "message")
    }

    try(removeNotification("reducingOutliers"), silent = TRUE)
  })



  output$download_crossval_results <- downloadHandler(
    filename = function() {
      paste0("method_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (!is.null(advanced_results$cross_validation)) {
        results <- advanced_results$cross_validation$comparison_metrics
        write.csv(results, file, row.names = FALSE)
        showNotification("Comparison results downloaded", type = "message", duration = 3)
      } else {
        write.csv(data.frame(Message = "No comparison results available. Please run cross-validation first."), file)
      }
    }
  )

  output$download_temporal_data <- downloadHandler(
    filename = function() {
      paste0("temporal_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      if (!is.null(advanced_results$temporal)) {
        temporal_data <- advanced_results$temporal$temporal_metrics
        write.csv(temporal_data, file, row.names = FALSE)
        showNotification("Temporal data downloaded", type = "message", duration = 3)
      } else {
        write.csv(data.frame(Message = "No temporal analysis results available. Please run analysis first."), file)
      }
    }
  )

  get_similarity_data_for_plot <- function(feature_type) {
    if (!is.null(comparison_results$results[[feature_type]])) {
      return(list(
        similarity_matrix = comparison_results$results[[feature_type]]$similarity_matrix,
        method_name = comparison_results$results[[feature_type]]$method,
        has_run_analysis = TRUE
      ))
    }

    return(list(error = paste("No", feature_type, "similarity data available. Please calculate first.")))
  }

  apply_category_filter_to_similarity <- function(similarity_matrix, filter_value) {
    if (is.null(filter_value) || filter_value == "all") {
      return(list(matrix = similarity_matrix, docs_data = document_display_data()))
    }

    if (!exists("document_display_data") || is.null(document_display_data())) {
      return(list(matrix = similarity_matrix, docs_data = NULL))
    }

    docs_data <- document_display_data()
    filter_indices <- which(docs_data$category_display == filter_value)

    if (length(filter_indices) > 0) {
      filtered_matrix <- similarity_matrix[filter_indices, filter_indices, drop = FALSE]
      filtered_docs <- docs_data[filter_indices, , drop = FALSE]
      return(list(matrix = filtered_matrix, docs_data = filtered_docs))
    }

    return(list(matrix = similarity_matrix, docs_data = docs_data))
  }

  output$semantic_similarity_plot <- plotly::renderPlotly({
    calc_in_progress <- calculation_state$in_progress
    feature_type <- isolate(input$semantic_feature_space %||% "words")

    if (calc_in_progress) {
      return(NULL)
    }

    if (!is.null(comparison_results$config_changed) && comparison_results$config_changed) {
      if (!is.null(comparison_results$calculation_status[[feature_type]]) &&
          comparison_results$calculation_status[[feature_type]]) {
        return(create_error_plot(
          "Document ID or category has changed. Please click 'Calculate' and 'Visualize Results' again.",
          color = "#F59E0B"
        ))
      } else {
        return(NULL)
      }
    }

    isolate({
      ngram_component <- if (feature_type == "ngrams") {
        paste0("_ngram", input$semantic_ngram_range %||% "2")
      } else {
        ""
      }

      cached_plot_key <- paste0(
        feature_type, "_",
        input$heatmap_category_filter %||% "all", "_",
        digest::digest(input$doc_id_var %||% "", algo = "md5"), "_",
        digest::digest(input$doc_category_var %||% "", algo = "md5"),
        ngram_component
      )
    })

    if (!is.null(comparison_results$cached_plots[[cached_plot_key]]) &&
        similarity_analysis_triggered()) {
      return(comparison_results$cached_plots[[cached_plot_key]])
    }

    viz_shown <- calculation_state$visualization_shown[[feature_type]] %||% FALSE

    if (!similarity_analysis_triggered() && !viz_shown) {
      if (!is.null(comparison_results$calculation_status[[feature_type]]) &&
          comparison_results$calculation_status[[feature_type]]) {
        return(create_error_plot(
          "Click 'Visualize Results' to display similarity analysis",
          color = "#28a745"
        ))
      } else {
        return(NULL)
      }
    }

    isolate({

      docs_data <- document_display_data()
      if (is.null(docs_data) || nrow(docs_data) < 2) {
        return(create_error_plot(
          if (is.null(docs_data)) "Process documents in the Setup tab first" else "At least 2 documents required for similarity analysis",
          color = "#6c757d"
        ))
      }

      similarity_data <- get_similarity_data_for_plot(feature_type)
      if (!is.null(similarity_data$error)) {
        return(create_error_plot(similarity_data$error))
      }

      filtered_data <- apply_category_filter_to_similarity(
        similarity_data$similarity_matrix,
        input$heatmap_category_filter
      )

      plot <- TextAnalysisR::plot_similarity_heatmap(
        similarity_matrix = filtered_data$matrix,
        docs_data = filtered_data$docs_data,
        feature_type = feature_type,
        method_name = similarity_data$method_name,
        category_filter = input$heatmap_category_filter,
        doc_id_var = input$doc_id_var
      )

      comparison_results$cached_plots[[cached_plot_key]] <- plot

      return(plot)
    })
  })

  output$semantic_similarity_stats <- DT::renderDataTable({
    if (similarity_analysis_triggered()) {
      similarity_analysis_triggered()
    }

    if (!similarity_analysis_triggered()) {
      return(NULL)
    }

    if (!is.null(comparison_results$config_changed) && comparison_results$config_changed) {
      return(NULL)
    }

    feature_type <- isolate(input$semantic_feature_space %||% "words")

    ngram_component <- if (feature_type == "ngrams") {
      paste0("_ngram", isolate(input$semantic_ngram_range) %||% "2")
    } else {
      ""
    }

    stats_cache_key <- paste0("stats_", feature_type, "_", isolate(input$heatmap_category_filter) %||% "all", ngram_component)

    if (!is.null(comparison_results$cached_stats[[stats_cache_key]])) {
      return(comparison_results$cached_stats[[stats_cache_key]])
    }

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      return(NULL)
    }

    similarity_data <- get_similarity_data_for_plot(feature_type)
    if (!is.null(similarity_data$error)) {
      return(NULL)
    }

    filtered_data <- apply_category_filter_to_similarity(
      similarity_data$similarity_matrix,
      input$heatmap_category_filter
    )

    similarity_matrix <- filtered_data$matrix
    filtered_docs_data <- filtered_data$docs_data

    if (is.null(similarity_matrix) || nrow(similarity_matrix) < 2) {
      return(DT::datatable(
        data.frame(Message = "Need at least 2 documents for similarity analysis"),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    tryCatch(
      {
        category_labels <- NULL
        if (!is.null(filtered_docs_data) && nrow(filtered_docs_data) == nrow(similarity_matrix)) {
          if ("category_display" %in% names(filtered_docs_data)) {
            category_labels <- filtered_docs_data$category_display
          }
        } else if (!is.null(filtered_docs_data) && "category_display" %in% names(filtered_docs_data)) {
          n_docs <- nrow(similarity_matrix)
          if (nrow(filtered_docs_data) >= n_docs) {
            category_labels <- filtered_docs_data$category_display[1:n_docs]
          }
        }



        method_name <- if (feature_type == "embeddings") {
          paste("Embeddings (", input$embedding_model %||% "all-MiniLM-L6-v2", ")", sep = "")
        } else {
          paste(stringr::str_to_title(feature_type), "Features")
        }

        metrics <- calculate_comprehensive_metrics(
          similarity_matrix,
          category_labels,
          list(method = similarity_data$method_name, feature_type = feature_type)
        )

        stats_df <- data.frame(
          Metric = c(
            "Analysis Method",
            "Total Documents",
            "Total Pairs",
            "Mean Similarity",
            "Median Similarity",
            "Min Similarity",
            "Max Similarity",
            "Std Dev Similarity",
            "Similarity Range",
            "Sparsity",
            "Connectivity",
            "Total Categories"
          ),
          Value = c(
            method_name,
            nrow(similarity_matrix),
            length(metrics$mean_similarity * nrow(similarity_matrix) * (nrow(similarity_matrix) - 1) / 2),
            if (is.numeric(metrics$mean_similarity) && !is.na(metrics$mean_similarity)) round(metrics$mean_similarity, 3) else metrics$mean_similarity,
            if (is.numeric(metrics$median_similarity) && !is.na(metrics$median_similarity)) round(metrics$median_similarity, 3) else metrics$median_similarity,
            if (is.numeric(metrics$min_similarity) && !is.na(metrics$min_similarity)) round(metrics$min_similarity, 3) else metrics$min_similarity,
            if (is.numeric(metrics$max_similarity) && !is.na(metrics$max_similarity)) round(metrics$max_similarity, 3) else metrics$max_similarity,
            if (is.numeric(metrics$std_similarity) && !is.na(metrics$std_similarity)) round(metrics$std_similarity, 3) else metrics$std_similarity,
            if (is.numeric(metrics$similarity_range) && !is.na(metrics$similarity_range)) round(metrics$similarity_range, 3) else metrics$similarity_range,
            if (is.numeric(metrics$sparsity) && !is.na(metrics$sparsity)) round(metrics$sparsity, 3) else metrics$sparsity,
            if (is.numeric(metrics$connectivity) && !is.na(metrics$connectivity)) round(metrics$connectivity, 3) else metrics$connectivity,
            if (!is.null(category_labels)) length(unique(category_labels)) else "N/A"
          )
        )

        result_table <- DT::datatable(
          stats_df,
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            width = "100%",
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          )
        )

        comparison_results$cached_stats[[stats_cache_key]] <- result_table

        return(result_table)
      },
      error = function(e) {
        return(DT::datatable(
          data.frame(Error = paste("Error calculating statistics:", e$message)),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }
    )
  })

  # Contrastive Similarity Analysis Logic
  output$has_gap_analysis <- reactive({
    !is.null(comparison_results$gap_analysis)
  })
  outputOptions(output, "has_gap_analysis", suspendWhenHidden = FALSE)

  # Reference category selector UI
  output$gap_reference_category_ui <- renderUI({
    docs_data <- document_display_data()
    category_var <- input$doc_category_var

    # Check for category_display column (created by document_display_data)
    if (is.null(docs_data) || is.null(category_var) || category_var == "" || 
        !"category_display" %in% names(docs_data) || all(docs_data$category_display == "Document")) {
      return(NULL)
    }

    categories <- unique(docs_data$category_display)
    if (length(categories) < 2) {
      return(tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 8px 12px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-info-circle", style = "margin-right: 5px;"),
        "Need at least 2 categories."
      ))
    }

    selectInput(
      "gap_reference_category",
      "Reference category",
      choices = categories,
      selected = categories[1]
    )
  })

  # Reference category label for results
  output$gap_reference_category_label <- renderUI({
    req(comparison_results$gap_analysis)
    ref_cat <- comparison_results$gap_analysis$reference_category
    if (!is.null(ref_cat)) {
      tags$p(
        tags$strong("Reference: "), ref_cat,
        style = "color: #0c1f4a; margin-bottom: 10px; font-size: 16px;"
      )
    }
  })

  observeEvent(input$run_gap_analysis, {
    # Check if similarity analysis has been run
    if (!isTRUE(similarity_analysis_triggered())) {
      showNotification(
        "Please calculate document similarity first (click 'Calculate Similarity' button).",
        type = "warning",
        duration = 5
      )
      return(NULL)
    }

    feature_type <- isolate(input$semantic_feature_space %||% "words")
    similarity_data <- get_similarity_data_for_plot(feature_type)

    if (!is.null(similarity_data$error)) {
      showNotification(
        paste("Please calculate", feature_type, "similarity first."),
        type = "error",
        duration = 5
      )
      return(NULL)
    }

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      showNotification("Need at least 2 documents for contrastive analysis.", type = "error", duration = 5)
      return(NULL)
    }

    category_var <- isolate(input$doc_category_var)
    # Check for category_display column (created by document_display_data from doc_category_var)
    if (is.null(category_var) || category_var == "" || !"category_display" %in% names(docs_data) ||
        all(docs_data$category_display == "Document")) {
      showNotification("Please select a category variable for contrastive analysis.", type = "warning", duration = 5)
      return(NULL)
    }

    categories <- unique(docs_data$category_display)
    if (length(categories) < 2) {
      showNotification("Contrastive analysis requires at least 2 categories.", type = "warning", duration = 5)
      return(NULL)
    }

    tryCatch({
      shinyjs::runjs("$('#gap_analysis_ready_status').hide();")

      showNotification("Running contrastive analysis...", type = "message", duration = 2, id = "gap_progress")

      similarity_matrix <- similarity_data$similarity_matrix
      n_docs <- nrow(similarity_matrix)

      if (nrow(docs_data) != n_docs) {
        docs_data <- docs_data[1:n_docs, ]
      }

      doc_id_var <- isolate(input$doc_id_var) %||% "display_name"
      if (!doc_id_var %in% names(docs_data)) {
        doc_id_var <- names(docs_data)[1]
      }

      # Use selected reference category
      ref_category <- isolate(input$gap_reference_category)
      if (is.null(ref_category) || !ref_category %in% categories) {
        ref_category <- categories[1]
      }

      ref_indices <- which(docs_data$category_display == ref_category)
      other_indices <- which(docs_data$category_display != ref_category)

      if (length(ref_indices) == 0 || length(other_indices) == 0) {
        showNotification("Cannot perform analysis: need documents in both reference and comparison categories.", type = "error", duration = 5)
        return(NULL)
      }

      cross_sim_df <- tidyr::expand_grid(
        ref_idx = ref_indices,
        other_idx = other_indices
      ) %>%
        dplyr::mutate(
          ref_id = docs_data[[doc_id_var]][ref_idx],
          other_id = docs_data[[doc_id_var]][other_idx],
          other_category = docs_data$category_display[other_idx],
          similarity = purrr::map2_dbl(ref_idx, other_idx, ~similarity_matrix[.x, .y])
        ) %>%
        dplyr::select(-ref_idx, -other_idx)

      gap_result <- TextAnalysisR::analyze_similarity_gaps(
        similarity_data = cross_sim_df,
        ref_var = "ref_id",
        other_var = "other_id",
        similarity_var = "similarity",
        category_var = "other_category",
        unique_threshold = input$gap_unique_threshold %||% 0.6,
        cross_policy_min = input$gap_unique_threshold %||% 0.6,
        cross_policy_max = input$gap_cross_policy_max %||% 0.8
      )

      # Store reference category with results
      gap_result$reference_category <- ref_category

      comparison_results$gap_analysis <- gap_result

      removeNotification("gap_progress")
      shinyjs::runjs("$('#gap_analysis_ready_status').show();")
      showNotification("Contrastive analysis complete!", type = "message", duration = 3)

    }, error = function(e) {
      removeNotification("gap_progress")
      showNotification(paste("Contrastive analysis error:", e$message), type = "error", duration = 7)
    })
  })

  output$gap_summary_stats <- DT::renderDataTable({
    req(comparison_results$gap_analysis)
    gap_result <- comparison_results$gap_analysis

    # Get numeric columns for formatting
    numeric_cols <- names(gap_result$summary_stats)[sapply(gap_result$summary_stats, is.numeric)]

    DT::datatable(
      gap_result$summary_stats,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    ) %>% DT::formatRound(columns = numeric_cols, digits = 3)
  })

  output$gap_unique_items <- DT::renderDataTable({
    req(comparison_results$gap_analysis)
    gap_result <- comparison_results$gap_analysis

    if (is.null(gap_result$unique_items) || nrow(gap_result$unique_items) == 0) {
      return(DT::datatable(
        data.frame(Message = "No unique items found below the threshold."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    numeric_cols <- names(gap_result$unique_items)[sapply(gap_result$unique_items, is.numeric)]

    DT::datatable(
      gap_result$unique_items,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10
      )
    ) %>% DT::formatRound(columns = numeric_cols, digits = 3)
  })

  output$gap_missing_items <- DT::renderDataTable({
    req(comparison_results$gap_analysis)
    gap_result <- comparison_results$gap_analysis

    if (is.null(gap_result$missing_items) || nrow(gap_result$missing_items) == 0) {
      return(DT::datatable(
        data.frame(Message = "No missing items found below the threshold."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    numeric_cols <- names(gap_result$missing_items)[sapply(gap_result$missing_items, is.numeric)]

    DT::datatable(
      gap_result$missing_items,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10
      )
    ) %>% DT::formatRound(columns = numeric_cols, digits = 3)
  })

  output$gap_cross_policy <- DT::renderDataTable({
    req(comparison_results$gap_analysis)
    gap_result <- comparison_results$gap_analysis

    if (is.null(gap_result$cross_policy) || nrow(gap_result$cross_policy) == 0) {
      return(DT::datatable(
        data.frame(Message = "No cross-category opportunities found in the specified range."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }

    numeric_cols <- names(gap_result$cross_policy)[sapply(gap_result$cross_policy, is.numeric)]

    DT::datatable(
      gap_result$cross_policy,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 10
      )
    ) %>% DT::formatRound(columns = numeric_cols, digits = 3)
  })

  observeEvent(input$showDimRedInfo, {
    show_guide_modal("dimensionality_reduction_guide", "Dimensionality Reduction Guide")
  })

  observeEvent(input$showClusteringInfo, {
    show_guide_modal("clustering_guide", "Document Clustering Guide")
  })

  observeEvent(input$showSemanticMethodsInfo, {
    show_guide_modal("semantic_methods_guide", "Semantic Analysis Methods Guide")
  })

  output$semantic_method_description <- renderUI({
    method <- input$semantic_feature_space %||% "words"

    description <- switch(method,
      "words" = tagList(
        tags$h5(tags$strong("Words: Lexical Semantic Analysis"), style = "color: #0c1f4a; margin-top: 0;"),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("What it does:"), " Analyzes meaning through vocabulary usage. Documents using similar words are considered semantically similar."
        ),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("How it works:"), " Creates word frequency vectors (bag-of-words), then calculates cosine similarity between document vectors."
        ),
        tags$p(style = "margin-bottom: 0;",
          tags$strong("Best for:"), " Finding documents with shared terminology, technical vocabulary, or domain-specific language."
        )
      ),

      "ngrams" = tagList(
        tags$h5(tags$strong("N-grams: Phrasal Semantic Analysis"), style = "color: #0c1f4a; margin-top: 0;"),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("What it does:"), " Captures meaning through word sequences and phrases. Preserves local context that individual words miss."
        ),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("How it works:"), " Extracts consecutive word sequences (e.g., 'machine learning', 'New York'), then compares phrase patterns between documents."
        ),
        tags$p(style = "margin-bottom: 0;",
          tags$strong("Best for:"), " Detecting similar expressions, idioms, technical phrases, or writing styles."
        )
      ),

      "embeddings" = tagList(
        tags$h5(tags$strong("Embeddings: Deep Semantic Analysis"), style = "color: #0c1f4a; margin-top: 0;"),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("What it does:"), " Captures deep semantic meaning using AI. Understands context, synonyms, and complex relationships between concepts."
        ),
        tags$p(style = "margin-bottom: 8px;",
          tags$strong("How it works:"), " Uses transformer neural networks (MiniLM) to create dense vector representations that encode semantic meaning."
        ),
        tags$p(style = "margin-bottom: 0;",
          tags$strong("Best for:"), " Most accurate semantic matching, understanding paraphrases, detecting subtle meaning differences."
        )
      )
    )

    return(description)
  })

  output$dimred_warning <- renderUI({
    current_feature_space <- input$semantic_feature_space %||% "words"

    data_available <- !is.null(comparison_results$results[[current_feature_space]]) &&
                     !is.null(comparison_results$calculation_status[[current_feature_space]]) &&
                     comparison_results$calculation_status[[current_feature_space]] == TRUE

    config_outdated <- comparison_results$config_changed && data_available

    if (config_outdated) {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Configuration Changed:"), "Document configuration has been modified. Please recalculate similarity data in the Similarity Analysis tab to reflect the new settings."
      )
    } else if (data_available) {
      feature_label <- switch(current_feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        current_feature_space
      )

      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), paste(feature_label, "similarity data is available for dimensionality reduction.")
      )
    } else {
      feature_label <- switch(current_feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        current_feature_space
      )

      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-info-circle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Note:"), paste(feature_label, "similarity requires calculation first. Go to Similarity Analysis tab and calculate with '", feature_label, "' feature space.")
      )
    }
  })

  output$document_clustering_status <- renderUI({
    current_feature_space <- input$semantic_feature_space
    if (is.null(current_feature_space)) {
      current_feature_space <- "words"
    }

    data_available <- !is.null(comparison_results$results[[current_feature_space]]) &&
                     !is.null(comparison_results$results[[current_feature_space]]$similarity_matrix)

    if (current_feature_space == "embeddings") {
      data_available <- data_available && !is.null(embeddings_cache$embeddings)
    }

    ngram_range <- if (current_feature_space == "ngrams" && data_available) {
      comparison_results$results[[current_feature_space]]$ngram_range %||% NULL
    } else {
      NULL
    }

    feature_label <- if (current_feature_space == "ngrams" && !is.null(ngram_range)) {
      switch(ngram_range,
        "2" = "Bigrams",
        "3" = "Trigrams",
        "4" = "4-grams",
        "5" = "5-grams",
        paste0(ngram_range, "-grams")
      )
    } else {
      switch(current_feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        current_feature_space
      )
    }

    if (data_available) {
      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), paste(feature_label, "similarity data is available for document grouping.")
      )
    } else {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Calculate Required:"), paste(" Go to Document Similarity tab and calculate", feature_label, "similarity first.")
      )
    }
  })

  output$sentiment_status_message <- renderUI({
    current_feature_space <- input$semantic_feature_space
    if (is.null(current_feature_space)) {
      current_feature_space <- "words"
    }

    data_available <- !is.null(comparison_results$results[[current_feature_space]]) &&
                     !is.null(comparison_results$results[[current_feature_space]]$similarity_matrix)

    if (current_feature_space == "embeddings") {
      data_available <- data_available && !is.null(embeddings_cache$embeddings)
    }

    ngram_range <- if (current_feature_space == "ngrams" && data_available) {
      comparison_results$results[[current_feature_space]]$ngram_range %||% NULL
    } else {
      NULL
    }

    feature_label <- if (current_feature_space == "ngrams" && !is.null(ngram_range)) {
      switch(ngram_range,
        "2" = "Bigrams",
        "3" = "Trigrams",
        "4" = "4-grams",
        "5" = "5-grams",
        paste0(ngram_range, "-grams")
      )
    } else {
      switch(current_feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        current_feature_space
      )
    }

    if (data_available) {
      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), paste(feature_label, "similarity data is available for sentiment analysis.")
      )
    } else {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Calculate Required:"), paste(" Go to Document Similarity tab and calculate", feature_label, "similarity first.")
      )
    }
  })

  output$cluster_labeling_warning <- renderUI({
    current_feature_space <- input$semantic_feature_space %||% "words"

    data_available <- !is.null(comparison_results$results[[current_feature_space]]) &&
                     !is.null(comparison_results$calculation_status[[current_feature_space]]) &&
                     comparison_results$calculation_status[[current_feature_space]] == TRUE

    clustering_available <- !is.null(comparison_results$clustering)

    ngram_range <- if (current_feature_space == "ngrams" && data_available) {
      comparison_results$results[[current_feature_space]]$ngram_range %||% NULL
    } else {
      NULL
    }

    feature_label <- if (current_feature_space == "ngrams" && !is.null(ngram_range)) {
      switch(ngram_range,
        "2" = "Bigrams",
        "3" = "Trigrams",
        "4" = "4-grams",
        "5" = "5-grams",
        paste0(ngram_range, "-grams")
      )
    } else {
      switch(current_feature_space,
        "words" = "Words",
        "ngrams" = "N-grams",
        "embeddings" = "Embeddings",
        current_feature_space
      )
    }

    if (!data_available) {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-info-circle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Note:"), paste(feature_label, "similarity requires calculation first. Go to Document Similarity tab and calculate with '", feature_label, "' feature space.")
      )
    } else if (!clustering_available) {
      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-info-circle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Note:"), "Cluster labeling requires clustering first. Go to Document Groups tab and discover groups."
      )
    } else {
      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), "Clustering data is available for labeling."
      )
    }
  })

  output$search_query_input <- renderUI({
    method <- input$search_method %||% "keyword"

    if (method == "rag") {
      textAreaInput(
        "semantic_search_query",
        "Ask a question",
        placeholder = "E.g., How does assistive technology support learning?",
        rows = 3
      )
    } else {
      textAreaInput(
        "semantic_search_query",
        "Search query",
        placeholder = "Enter your search terms...",
        rows = 3
      )
    }
  })

  output$search_method_status_message <- renderUI({
    method <- input$search_method %||% "keyword"

    if (method == "keyword" || method == "rag") return(NULL)

    method_available <- !is.null(comparison_results$results[[method]]) &&
                       !is.null(comparison_results$results[[method]]$similarity_matrix)

    if (method == "embeddings") {
      method_available <- method_available && !is.null(embeddings_cache$embeddings)
    }

    ngram_size <- if (method == "ngrams" && method_available) {
      comparison_results$results[[method]]$ngram_range %||% "2"
    } else {
      NULL
    }

    ngram_term <- if (method == "ngrams" && !is.null(ngram_size)) {
      switch(ngram_size,
        "2" = "Bigrams",
        "3" = "Trigrams",
        "4" = "4-grams",
        "5" = "5-grams",
        paste0(ngram_size, "-grams")
      )
    } else { NULL }

    method_label <- switch(method,
      "words" = "Words similarity",
      "ngrams" = if (!is.null(ngram_term)) paste0(ngram_term, " similarity") else "N-grams similarity",
      "embeddings" = "Embeddings similarity",
      method
    )

    if (method_available) {
      tags$div(
        style = "background-color: #D1FAE5; border: 1px solid #10B981; color: #065F46; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-check-circle", style = "margin-right: 5px; color: #10B981;"),
        tags$strong("Ready:"), paste(method_label, "data is available for search.")
      )
    } else {
      feature_label <- if (method == "ngrams") {
        "N-grams"
      } else {
        tools::toTitleCase(method)
      }

      tags$div(
        style = "background-color: #FEF3C7; border: 1px solid #F59E0B; color: #92400E; padding: 4px 12px; margin-bottom: 10px; font-size: 16px; border-radius: 4px;",
        tags$i(class = "fa fa-exclamation-triangle", style = "margin-right: 5px; color: #F59E0B;"),
        tags$strong("Calculate Required:"), paste(" Go to Document Similarity tab and calculate", feature_label, "similarity first.")
      )
    }
  })


  output$search_method_indicator <- renderUI({
    if (!is.null(search_results_reactive$method_used)) {
      method_info <- switch(search_results_reactive$method_used,
        "keyword" = list(
          label = "Keyword Search (Exact matching)",
          icon = "fa-search",
          color = "#337ab7"
        ),
        "words" = list(
          label = "Words Similarity (Lexical matching)",
          icon = "fa-font",
          color = "#337ab7"
        ),
        "ngrams" = list(
          label = if (!is.null(search_results_reactive$ngram_size)) {
            ngram_name <- switch(search_results_reactive$ngram_size,
              "2" = "Bigrams",
              "3" = "Trigrams",
              "4" = "4-grams",
              "5" = "5-grams",
              paste0(search_results_reactive$ngram_size, "-grams")
            )
            paste0(ngram_name, " Similarity (Phrase matching)")
          } else {
            "N-grams Similarity (Phrase matching)"
          },
          icon = "fa-align-left",
          color = "#DC2626"
        ),
        "topics" = list(
          label = "Topics Similarity (Thematic matching)",
          icon = "fa-tags",
          color = "#EA580C"
        ),
        "embeddings" = list(
          label = "Embeddings Similarity (Deep semantic matching)",
          icon = "fa-brain",
          color = "#8B5CF6"
        ),
        "rag" = list(
          label = "RAG Q&A (LLM-powered answer generation)",
          icon = "fa-robot",
          color = "#10B981"
        ),
        list(
          label = search_results_reactive$method_used,
          icon = "fa-question",
          color = "#6B7280"
        )
      )

      method_label <- method_info$label
      icon_class <- method_info$icon
      color_style <- paste0("color: ", method_info$color, ";")

      tags$div(
        style = "padding: 10px; background: white; border-radius: 4px; border: 1px solid #e5e7eb;",
        tags$i(class = paste("fa", icon_class), style = paste(color_style, "margin-right: 8px;")),
        tags$strong("Method Used: ", style = "color: #374151;"),
        tags$span(method_label, style = color_style)
      )
    }
  })

  output$semantic_search_results <- DT::renderDataTable({

    if (search_results_reactive$method_used == "rag") {
      answer <- search_results_reactive$rag_answer
      confidence <- search_results_reactive$rag_confidence
      sources <- search_results_reactive$rag_sources

      rag_display <- data.frame(
        Field = c("Answer", "Confidence", "Source Documents"),
        Value = c(
          answer,
          paste0(round(confidence * 100), "%"),
          paste(paste0("Doc ", sources), collapse = ", ")
        ),
        stringsAsFactors = FALSE
      )

      return(DT::datatable(
        rag_display,
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE),
        escape = FALSE
      ))
    }

    results <- search_results_reactive$results

    if (!all(c("Document_Index", "Similarity_Score", "Document_Text") %in% names(results))) {
      return(TextAnalysisR::create_message_table("Error: Invalid search results format"))
    }

    if (nrow(results) == 0) {
      return(TextAnalysisR::create_message_table("No documents found matching query"))
    }

    tryCatch({
      display_results <- data.frame(
        Rank = 1:nrow(results),
        Similarity_Score = round(results$Similarity_Score, 4),
        Document_Text = sapply(results$Document_Text, function(text) {
          if (is.null(text) || is.na(text)) {
            return("No text available")
          }
          text <- as.character(text)
          if (nchar(text) > 200) {
            paste0(substr(text, 1, 200), "...")
          } else {
            text
          }
        }),
        stringsAsFactors = FALSE
      )

      return(DT::datatable(
        display_results,
        rownames = FALSE,
        caption = paste("Top", nrow(results), "most similar documents"),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      ))

    }, error = function(e) {
      return(DT::datatable(
        data.frame(
          Error = paste("Error displaying results:", e$message),
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    })
  })

  output$has_clusters <- reactive({
    !is.null(document_clustering_results$clusters)
  })
  outputOptions(output, "has_clusters", suspendWhenHidden = FALSE)

  output$has_clustering_results <- reactive({
    !is.null(comparison_results$clustering)
  })
  outputOptions(output, "has_clustering_results", suspendWhenHidden = FALSE)

  output$analysis_run <- reactive({
    isTRUE(document_clustering_results$analysis_run)
  })
  outputOptions(output, "analysis_run", suspendWhenHidden = FALSE)

  output$has_documents <- reactive({
    tryCatch({
      proc_docs <- processed_documents()
      !is.null(proc_docs) && nrow(proc_docs) > 0
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_documents", suspendWhenHidden = FALSE)

  output$has_similarity_calculation <- reactive({
    feature_type <- input$semantic_feature_space %||% "words"
    !is.null(comparison_results$calculation_status[[feature_type]]) &&
      comparison_results$calculation_status[[feature_type]]
  })
  outputOptions(output, "has_similarity_calculation", suspendWhenHidden = FALSE)

  output$has_similarity_analysis <- reactive({
    similarity_analysis_triggered()
  })
  outputOptions(output, "has_similarity_analysis", suspendWhenHidden = FALSE)

  output$similarity_calculation_summary <- renderPrint({
    feature_type <- input$semantic_feature_space %||% "words"

    if (is.null(comparison_results$results[[feature_type]])) {
      cat("No results available")
      return()
    }

    result_data <- comparison_results$results[[feature_type]]
    metrics <- result_data$metrics

    safe_print <- function(label, value, digits = NULL) {
      cat(label, ":")
      if (is.null(value) || length(value) == 0) {
        cat(" N/A\n")
      } else if (!is.null(digits) && is.numeric(value)) {
        cat(" ", round(value, digits), "\n")
      } else {
        cat(" ", value, "\n")
      }
    }

    cat("Feature Space:", tools::toTitleCase(feature_type), "\n")
    safe_print("Documents Analyzed", metrics$n_docs)
    safe_print("Mean Similarity", metrics$mean_similarity, 4)
    safe_print("Median Similarity", metrics$median_similarity, 4)

    if (!is.null(metrics$min_similarity) && !is.null(metrics$max_similarity) &&
        is.numeric(metrics$min_similarity) && is.numeric(metrics$max_similarity)) {
      cat("Similarity Range:", round(metrics$min_similarity, 3), "to", round(metrics$max_similarity, 3), "\n")
    } else {
      cat("Similarity Range: N/A\n")
    }

    safe_print("Standard Deviation", metrics$std_similarity, 4)
  })

  output$has_search_results <- reactive({
    !is.null(search_results_reactive$results)
  })
  outputOptions(output, "has_search_results", suspendWhenHidden = FALSE)

  output$has_cooccurrence_plot <- reactive({
    !is.null(input$plot_word_co_occurrence_network) && input$plot_word_co_occurrence_network > 0
  })
  outputOptions(output, "has_cooccurrence_plot", suspendWhenHidden = FALSE)

  output$has_correlation_plot <- reactive({
    !is.null(input$plot_word_correlation_network) && input$plot_word_correlation_network > 0
  })
  outputOptions(output, "has_correlation_plot", suspendWhenHidden = FALSE)

  output$has_frequency_plot <- reactive({
    !is.null(input$plot_term) && input$plot_term > 0 &&
      !is.null(input$continuous_var_3) && input$continuous_var_3 != "" &&
      !is.null(input$type_terms)
  })
  outputOptions(output, "has_frequency_plot", suspendWhenHidden = FALSE)

  output$has_quotes <- reactive({
    tryCatch({
      thoughts_data <- thoughts()
      !is.null(thoughts_data) && nrow(thoughts_data) > 0
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_quotes", suspendWhenHidden = FALSE)

  output$has_categorical_plot <- reactive({
    !is.null(input$effect_cat_btn) && input$effect_cat_btn != "" &&
      !is.null(input$display_cat)
  })
  outputOptions(output, "has_categorical_plot", suspendWhenHidden = FALSE)

  output$has_continuous_plot <- reactive({
    !is.null(input$effect_con_btn) && input$effect_con_btn != "" &&
      !is.null(input$display_con)
  })
  outputOptions(output, "has_continuous_plot", suspendWhenHidden = FALSE)

  output$has_search_k_results <- reactive({
    tryCatch({
      input$topic_modeling_path == "probability" &&
        !is.null(K_search()) && length(K_search()) > 0
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_search_k_results", suspendWhenHidden = FALSE)

  output$has_hybrid_search_k_results <- reactive({
    tryCatch({
      input$topic_modeling_path == "hybrid" &&
        !is.null(hybrid_K_search()) && length(hybrid_K_search()) > 0
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_hybrid_search_k_results", suspendWhenHidden = FALSE)

  output$hybrid_topic_search_message <- renderUI({
    search_result <- hybrid_K_search()
    if (!is.null(search_result) && !is.null(search_result$results)) {
      HTML(paste0(
        "<div style='background-color: #f0f8ff; border: 1px solid #4682b4; padding: 10px; border-radius: 5px;'>",
        "<h5><strong style='color: #2c5aa0;'>Hybrid Model Search K Results</strong></h5>",
        "<p><b>Searched K range:</b> ", min(search_result$results$K), " to ", max(search_result$results$K), "</p>",
        "<p><b>Total models evaluated:</b> ", nrow(search_result$results), "</p>",
        "<p style='color: #6b7280; font-size: 16px;'>",
        "Diagnostic metrics shown below are based on the STM component. ",
        "Select optimal K in the Word-Topic tab to run the full hybrid model with embeddings.",
        "</p>",
        "</div>"
      ))
    }
  })

  output$hybrid_quality_metrics_plot_uiOutput <- renderUI({
    search_result <- hybrid_K_search()
    if (!is.null(search_result) && !is.null(search_result$results)) {
      height_val <- input$height_search_k %||% 600
      plotly::plotlyOutput("hybrid_quality_metrics_plot", height = paste0(height_val, "px"), width = "100%")
    }
  })

  output$hybrid_quality_metrics_plot <- plotly::renderPlotly({
    req(hybrid_K_search())
    search_result <- hybrid_K_search()
    if (is.null(search_result$results)) return(NULL)

    width_val <- if (!is.null(input$width_search_k)) input$width_search_k else 1000
    height_val <- if (!is.null(input$height_search_k)) input$height_search_k else 800

    TextAnalysisR::plot_quality_metrics(
      search_results = search_result,
      title = "Hybrid Model Diagnostics by K (STM Component)",
      height = height_val,
      width = width_val
    )
  })

  output$hybrid_quality_summary_table <- DT::renderDataTable({
    req(hybrid_K_search())
    search_result <- hybrid_K_search()
    if (is.null(search_result$results)) return(NULL)

    df <- search_result$results
    df$heldout <- as.numeric(df$heldout)
    df$residual <- as.numeric(df$residual)
    df$semcoh <- as.numeric(df$semcoh)
    df$exclus <- as.numeric(df$exclus)

    df <- df %>%
      dplyr::mutate(
        heldout_z = scale(heldout)[,1],
        semcoh_z = scale(semcoh)[,1],
        exclus_z = scale(exclus)[,1],
        residual_z = -scale(residual)[,1],
        overall_score = round((semcoh_z + exclus_z + heldout_z + residual_z) / 4, 3)
      ) %>%
      dplyr::select(K, heldout, semcoh, exclus, residual, overall_score) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 3)))

    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$hybrid_model_comparison_plot_uiOutput <- renderUI({
    search_result <- hybrid_K_search()
    if (!is.null(search_result) && !is.null(search_result$results)) {
      height_val <- input$height_search_k %||% 600
      plotly::plotlyOutput("hybrid_model_comparison_plot", height = paste0(height_val, "px"), width = "100%")
    }
  })

  output$hybrid_model_comparison_plot <- plotly::renderPlotly({
    req(hybrid_K_search())
    search_result <- hybrid_K_search()
    if (is.null(search_result$results)) return(NULL)

    width_val <- if (!is.null(input$width_search_k)) input$width_search_k else 1000
    height_val <- if (!is.null(input$height_search_k)) input$height_search_k else 600

    TextAnalysisR::plot_model_comparison(
      search_results = search_result,
      title = "Hybrid Model: Coherence vs Exclusivity",
      height = height_val,
      width = width_val
    )
  })

  output$has_word_topic_results <- reactive({
    tryCatch({
      !is.null(stm_K_number())
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_word_topic_results", suspendWhenHidden = FALSE)

  output$has_document_topic_results <- reactive({
    tryCatch({
      !is.null(gamma_terms())
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_document_topic_results", suspendWhenHidden = FALSE)

  output$space_feature_info <- renderUI({
    if (!isTRUE(document_clustering_results$analysis_run)) return(NULL)

    feature_space <- input$semantic_feature_space %||% "words"
    method <- document_clustering_results$method %||% "UMAP"

    tags$div(
      tags$h6(strong("Analysis Configuration"), style = "color: #0c1f4a;"),
      tags$p(paste("Feature Space:", feature_space)),
      tags$p(paste("Reduction Method:", method))
    )
  })

  output$space_quality_metrics <- renderUI({
    if (!isTRUE(document_clustering_results$analysis_run)) return(NULL)

    metrics <- document_clustering_results$quality_metrics
    if (length(metrics) == 0) {
      return(tags$p("No clustering applied", style = "color: #6b7280;"))
    }

    tags$div(
      tags$h6(strong("Quality Metrics"), style = "color: #0c1f4a;"),
      if (!is.null(metrics$silhouette)) tags$p(paste("Silhouette Score:", round(metrics$silhouette, 3))),
      if (!is.null(metrics$n_clusters)) tags$p(paste("Number of Clusters:", metrics$n_clusters)),
      if (!is.null(metrics$outlier_pct)) tags$p(paste("Outliers:", round(metrics$outlier_pct, 1), "%"))
    )
  })

  output$document_groups_table <- DT::renderDataTable({
    if (!isTRUE(document_clustering_results$analysis_run)) return(NULL)

    docs_data <- document_display_data()
    if (is.null(docs_data)) return(NULL)

    n_docs <- nrow(document_clustering_results$coordinates)
    table_data <- docs_data[1:n_docs, c("document_id_display", "combined_text")]

    if (!is.null(document_clustering_results$clusters)) {
      table_data$cluster <- document_clustering_results$clusters
    }

    DT::datatable(table_data,
                 options = list(pageLength = 10, scrollX = TRUE))
  })

  observe({
    if (!is.null(document_clustering_results$clusters)) {
      unique_clusters <- sort(unique(document_clustering_results$clusters[document_clustering_results$clusters > 0]))
      updateSelectInput(session, "selected_document_group",
                       choices = setNames(unique_clusters, paste("Cluster", unique_clusters)))
    }
  })

  output$document_group_summary <- renderUI({
    req(input$selected_document_group)
    cluster_id <- as.numeric(input$selected_document_group)

    if (!is.null(document_clustering_results$clusters)) {
      n_docs <- sum(document_clustering_results$clusters == cluster_id)
      tags$p(paste("Contains", n_docs, "documents"))
    }
  })

  output$group_terms_plot <- plotly::renderPlotly({
    req(input$selected_document_group)
    cluster_id <- as.numeric(input$selected_document_group)

    if (!is.null(document_clustering_results$clusters) && !is.null(semantic_feature_matrix())) {
      cluster_docs <- which(document_clustering_results$clusters == cluster_id)
      feature_matrix <- semantic_feature_matrix()
      cluster_features <- feature_matrix[cluster_docs, , drop = FALSE]
      term_freq <- colMeans(cluster_features)

      TextAnalysisR::plot_cluster_terms(
        terms = term_freq,
        cluster_id = cluster_id,
        n_terms = 10
      )
    }
  })

  output$group_sample_docs <- DT::renderDataTable({
    req(input$selected_document_group)
    cluster_id <- as.numeric(input$selected_document_group)

    if (!is.null(document_clustering_results$clusters)) {
      docs_data <- document_display_data()
      cluster_docs <- which(document_clustering_results$clusters == cluster_id)

      sample_indices <- head(cluster_docs, 5)
      sample_data <- docs_data[sample_indices, c("document_id_display", "combined_text")]

      DT::datatable(sample_data,
                   options = list(pageLength = 5, scrollX = TRUE, dom = 't'))
    }
  })

  output$labeled_space_plot <- plotly::renderPlotly({
    if (!isTRUE(document_clustering_results$analysis_run) || is.null(document_clustering_results$clusters)) {
      return(create_error_plot("Generate labels first", color = "#6c757d"))
    }

    coords <- document_clustering_results$coordinates
    clusters <- document_clustering_results$clusters
    labels <- document_clustering_results$labels

    plot_data <- data.frame(
      x = coords[, 1],
      y = coords[, 2],
      cluster = as.factor(clusters)
    )

    if (!is.null(labels)) {
      plot_data$label <- labels[match(as.character(clusters), names(labels))]
      plot_data$hover_text <- paste0("Cluster ", clusters, ": ", plot_data$label)
    } else {
      plot_data$label <- paste("Cluster", clusters)
      plot_data$hover_text <- plot_data$label
    }

    plotly::plot_ly(plot_data, x = ~x, y = ~y,
                   color = ~cluster,
                   colors = "Set1",
                   type = "scatter",
                   mode = "markers",
                   marker = list(size = 8),
                   text = ~hover_text,
                   hoverinfo = "text") %>%
      plotly::layout(
        title = list(
          text = "Labeled Document Clustering",
          font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
          x = 0.5,
          xref = "paper",
          xanchor = "center"
        ),
        xaxis = list(
          title = list(text = "Dimension 1"),
          titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
          tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
        ),
        yaxis = list(
          title = list(text = "Dimension 2"),
          titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
          tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
        ),
        font = list(family = "Roboto, sans-serif", size = 16, color = "#3B3B3B"),
        hovermode = "closest",
        hoverlabel = list(
          align = "left",
          font = list(size = 15, family = "Roboto, sans-serif"),
          maxwidth = 300
        ),
        legend = list(
          font = list(size = 16, family = "Roboto, sans-serif")
        ),
        margin = list(l = 80, r = 40, t = 80, b = 60)
      ) %>%
      plotly::config(displayModeBar = TRUE)
  })

  output$labeled_groups_table <- DT::renderDataTable({
    if (!is.null(document_clustering_results$labels)) {
      cluster_ids <- names(document_clustering_results$labels)

      labels_df <- data.frame(
        Cluster = cluster_ids,
        Label = unlist(document_clustering_results$labels),
        Documents = sapply(cluster_ids, function(c) {
          sum(document_clustering_results$clusters == as.numeric(c))
        }),
        stringsAsFactors = FALSE
      )

      DT::datatable(labels_df,
                    options = list(pageLength = 10, dom = 'tip'),
                    rownames = FALSE)
    }
  })

  output$semantic_space_plot <- plotly::renderPlotly({

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      return(create_error_plot("Run Document Configuration first", color = "#dc3545"))
    }

    feature_matrix <- semantic_feature_matrix()
    if (is.null(feature_matrix)) {
      feature_space <- input$semantic_feature_space %||% "words"
      error_msg <- if (feature_space == "embeddings") {
        "No embeddings available. Calculate embeddings in Document Similarity first."
      } else if (feature_space == "ngrams") {
        "N-grams require completed preprocessing with tokens."
      } else {
        "No DFM available. Complete preprocessing (at least Step 4) first."
      }
      return(create_error_plot(error_msg, color = "#dc3545"))
    }

    coords <- document_clustering_results$coordinates
    if (is.null(coords)) {
      return(create_error_plot("No dimensionality reduction results available", color = "#dc3545"))
    }

    clusters <- document_clustering_results$clusters

    hover_text <- paste0(
      "<b>ID:</b> ", sapply(docs_data$document_id_display[1:nrow(coords)], wrap_long_text, USE.NAMES = FALSE), "\n",
      "<b>Text:</b>\n", sapply(docs_data$combined_text[1:nrow(coords)], wrap_text_for_tooltip, USE.NAMES = FALSE)
    )

    if (!is.null(clusters)) {
      hover_text <- paste0(hover_text, "\n<b>Cluster:</b> ", clusters)

      cluster_colors <- c(
        "#1e40af", "#b91c1c", "#337ab7", "#d97706", "#6b21a8",
        "#be185d", "#0f766e", "#c2410c", "#4338ca", "#65a30d"
      )
      cluster_indices <- as.numeric(as.factor(clusters))
      hover_bg_colors <- cluster_colors[(cluster_indices - 1) %% length(cluster_colors) + 1]

      hoverlabel_config <- list(
        bgcolor = hover_bg_colors,
        bordercolor = hover_bg_colors,
        font = list(family = "Roboto, sans-serif", size = 15, color = "#ffffff"),
        align = "left",
        namelength = -1,
        maxwidth = 450
      )
    } else {
      hoverlabel_config <- list(
        bgcolor = "#4338ca",
        bordercolor = "#4338ca",
        font = list(family = "Roboto, sans-serif", size = 15, color = "#ffffff"),
        align = "left",
        namelength = -1,
        maxwidth = 450
      )
    }

    method <- input$semantic_dimred_method %||% "UMAP"
    title_text <- paste("Document Clustering:", method, "Visualization")
    if (!is.null(clusters)) {
      title_text <- paste(title_text, "with", input$document_clustering_method, "Clustering")
    }

    plot_semantic_viz(
      plot_type = if (!is.null(clusters)) "clustering" else "dimensionality_reduction",
      coords = coords,
      clusters = clusters,
      hover_text = hover_text,
      hover_config = hoverlabel_config,
      title = title_text,
      height = 600,
      width = 800
    )
  })

  output$semantic_dimred_plot <- plotly::renderPlotly({
    if (!is.null(dimred_results$calculating) && dimred_results$calculating) {
      return(NULL)
    }

    dimred_triggered <- dimred_analysis_triggered()
    current_method <- input$semantic_dimred_method %||% "PCA"

    cache_key <- if (current_method == "PCA") {
      paste0("PCA_", input$semantic_pca_dims %||% 2)
    } else if (current_method == "t-SNE") {
      paste0("tSNE_", input$semantic_tsne_perplexity %||% 30, "_", input$semantic_tsne_max_iter %||% 1000)
    } else if (current_method == "UMAP") {
      paste0("UMAP_", input$semantic_umap_neighbors %||% 15, "_", input$semantic_umap_min_dist %||% 0.1)
    } else {
      current_method
    }

    if (dimred_triggered && !is.null(dimred_results$cached_plots[[cache_key]])) {
      return(dimred_results$cached_plots[[cache_key]])
    }

    if (dimred_triggered && is.null(dimred_results$cached_plots[[cache_key]]) && !is.null(dimred_results$last_method) && !isTRUE(dimred_results$just_ran_analysis)) {
      return(create_error_plot(
        paste("Parameters changed. Click 'Visualize' to update", current_method, "visualization"),
        color = "#ffc107"
      ))
    }

    if (!dimred_triggered) {
      return(create_error_plot(
        "Click 'Visualize' to run analysis",
        color = "#6c757d"
      ))
    }

    dimred_analysis_reactive()

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      return(create_error_plot(
        "Run Document Configuration first",
        color = "#dc3545"
      ))
    }

    feature_matrix_check <- semantic_feature_matrix()
    if (is.null(feature_matrix_check)) {
      return(create_error_plot(
        "No feature matrix available. Please process documents first.",
        color = "#dc3545"
      ))
    }

    if (is.null(dimred_results$last_method)) {
      return(create_error_plot(
        "Click 'Visualize' to run dimensionality reduction analysis",
        color = "#6c757d"
      ))
    }

    current_feature_space <- input$semantic_feature_space
    current_ngram_range <- if (current_feature_space == "ngrams") input$semantic_ngram_range else NULL

    if (is.null(dimred_results$last_params) ||
        dimred_results$last_params$feature_space != current_feature_space ||
        (current_feature_space == "ngrams" && dimred_results$last_params$ngram_range != current_ngram_range)) {
      return(create_error_plot(
        "Dimensionality reduction results are outdated due to feature space or N-gram range changes.<br>Please re-run dimensionality reduction analysis.",
        color = "#ffc107"
      ))
    }

    feature_matrix <- semantic_feature_matrix()
    n_docs <- nrow(feature_matrix)

    if (n_docs < 2) {
      return(create_error_plot(
        "Need at least 2 documents for visualization",
        color = "#dc3545"
      ))
    }

    if (exists("document_display_data") && !is.null(document_display_data()) && nrow(document_display_data()) > 0) {
      docs_data <- document_display_data()
      if (nrow(docs_data) >= n_docs) {
        docs_data <- docs_data[1:n_docs, ]
        doc_names <- docs_data$document_id_display
      } else {
        doc_names <- paste("Document", 1:n_docs)
      }
    } else {
      doc_names <- paste("Document", 1:n_docs)
    }

    method <- input$semantic_dimred_method
    coords <- NULL
    title_text <- ""
    x_lab <- ""
    y_lab <- ""

    if (method == "PCA" && !is.null(dimred_results$pca)) {
      pca_result <- dimred_results$pca
      coords <- data.frame(
        x = pca_result$x[, 1],
        y = pca_result$x[, 2],
        doc_id = doc_names
      )

      explained_var <- summary(pca_result)$importance[2, 1:2] * 100
      title_text <- paste("Document PCA Visualization")
      x_lab <- paste0("PC1 (", round(explained_var[1], 1), "% variance)")
      y_lab <- paste0("PC2 (", round(explained_var[2], 1), "% variance)")
    } else if (method == "t-SNE" && !is.null(dimred_results$tsne)) {
      tsne_result <- dimred_results$tsne
      coords <- data.frame(
        x = tsne_result$Y[, 1],
        y = tsne_result$Y[, 2],
        doc_id = doc_names
      )
      title_text <- "Document t-SNE Visualization"
      x_lab <- "t-SNE 1"
      y_lab <- "t-SNE 2"
    } else if (method == "UMAP" && !is.null(dimred_results$umap)) {
      umap_result <- dimred_results$umap
      coords <- data.frame(
        x = umap_result$layout[, 1],
        y = umap_result$layout[, 2],
        doc_id = doc_names
      )
      title_text <- "Document UMAP Visualization"
      x_lab <- "UMAP 1"
      y_lab <- "UMAP 2"
    }

    if (is.null(coords)) {
      return(create_error_plot(
        paste("No", method, "results available. Click 'Create' to generate."),
        color = "#ffc107"
      ))
    }

    docs_data <- NULL
    if (exists("document_display_data") && !is.null(document_display_data()) && nrow(document_display_data()) >= n_docs) {
      docs_data <- document_display_data()[1:n_docs, ]
    }

    hover_text <- if (!is.null(docs_data)) {
      doc_ids_wrapped <- sapply(docs_data$document_id_display, wrap_long_text, USE.NAMES = FALSE)
      if ("category_display" %in% names(docs_data)) {
        cats_wrapped <- sapply(docs_data$category_display, wrap_long_text, USE.NAMES = FALSE)
        paste0(
          "<b>Document:</b> ", doc_ids_wrapped, "<br>",
          "<b>Category:</b> ", cats_wrapped
        )
      } else {
        paste0("<b>Document:</b> ", doc_ids_wrapped)
      }
    } else {
      paste0("<b>Document:</b> ", coords$doc_id)
    }

    color_var <- if (!is.null(docs_data) && "category_display" %in% names(docs_data)) {
      docs_data$category_display
    } else {
      NULL
    }

    coords_matrix <- as.matrix(coords[, c("x", "y")])

    final_plot <- plot_semantic_viz(
      plot_type = "dimensionality_reduction",
      coords = coords_matrix,
      data_labels = doc_names,
      color_by = color_var,
      hover_text = hover_text,
      title = title_text,
      height = 600,
      width = 800
    )

    cache_key_store <- if (method == "PCA") {
      paste0("PCA_", input$semantic_pca_dims %||% 2)
    } else if (method == "t-SNE") {
      paste0("tSNE_", input$semantic_tsne_perplexity %||% 30, "_", input$semantic_tsne_max_iter %||% 1000)
    } else if (method == "UMAP") {
      paste0("UMAP_", input$semantic_umap_neighbors %||% 15, "_", input$semantic_umap_min_dist %||% 0.1)
    } else {
      method
    }

    dimred_results$cached_plots[[cache_key_store]] <- final_plot

    dimred_results$just_ran_analysis <- FALSE

    return(final_plot)
  })

  output$dimred_feature_space_info <- renderUI({
    input$semantic_dimred_method
    input$semantic_pca_dims
    input$semantic_tsne_perplexity
    input$semantic_tsne_max_iter
    input$semantic_umap_neighbors
    input$semantic_umap_min_dist
    input$semantic_cluster_seed

    if (is.null(input$run_semantic_analysis_dimred) || input$run_semantic_analysis_dimred == 0) {
      return(NULL)
    }

    if (is.null(dimred_results$last_method)) {
      return(NULL)
    }

    feature_space <- input$semantic_feature_space %||% "words"
    method <- input$semantic_dimred_method %||% "PCA"

    feature_space_display <- switch(feature_space,
                                    "words" = "Word Features",
                                    "ngrams" = "N-gram Features",
                                    "topics" = "Topic Features",
                                    "embeddings" = "Embedding Features",
                                    "Word Features"
    )

    method_info <- switch(method,
                          "PCA" = {
                            if (!is.null(dimred_results$pca)) {
                              pca_result <- dimred_results$pca
                              explained_var <- summary(pca_result)$importance[2, 1:2] * 100
                              paste("Principal Component Analysis | PC1:", round(explained_var[1], 1), "% | PC2:", round(explained_var[2], 1), "%")
                            } else {
                              "Principal Component Analysis"
                            }
                          },
                          "t-SNE" = {
                            perplexity <- input$semantic_tsne_perplexity %||% 30
                            max_iter <- input$semantic_tsne_max_iter %||% 1000
                            paste("t-SNE | Perplexity:", perplexity, "| Max Iterations:", max_iter)
                          },
                          "UMAP" = {
                            neighbors <- input$semantic_umap_neighbors %||% 15
                            min_dist <- input$semantic_umap_min_dist %||% 0.1
                            paste("UMAP | Neighbors:", neighbors, "| Min Distance:", min_dist)
                          },
                          method
    )

    htmltools::tags$div(
      style = "border: 1px solid #dee2e6; padding: 12px; margin-bottom: 15px; border-radius: 4px; background: white;",
      htmltools::tags$h5(strong("Feature Space Information"), style = "color: #0c1f4a; margin: 0 0 8px 0;"),
      htmltools::tags$table(
        style = "width: 100%; margin-top: 8px; margin-bottom: 8px; font-size: 16px;",
        htmltools::tags$tr(
          htmltools::tags$th("Property", style = "text-align: left; padding: 4px 20px 4px 0; color: #4b5563; font-weight: 500;"),
          htmltools::tags$th("Value", style = "text-align: left; padding: 4px 0; color: #4b5563; font-weight: 500;")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Feature Space", style = "padding: 4px 20px 4px 0;"),
          htmltools::tags$td(feature_space_display, style = "padding: 4px 0; font-weight: 600; color: #0c1f4a;")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Method", style = "padding: 4px 20px 4px 0;"),
          htmltools::tags$td(method_info, style = "padding: 4px 0; font-weight: 600; color: #0c1f4a;")
        )
      )
    )
  })

  output$semantic_cluster_plot <- plotly::renderPlotly({
    if (!is.null(comparison_results$clustering_calculating) && comparison_results$clustering_calculating) {
      return(NULL)
    }

    clustering_analysis_reactive()

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      return(create_error_plot(
        if (is.null(docs_data)) "Process documents in the Setup tab first" else "At least 2 documents required for clustering",
        color = "#6c757d"
      ))
    }

    if (!clustering_analysis_triggered()) {
      return(create_error_plot(
        "Select clustering method in the sidebar and click 'Apply' to begin",
        color = "#6c757d"
      ))
    }

    if (is.null(comparison_results$clustering)) {
      return(create_error_plot(
        "Click 'Apply' in the sidebar to run clustering analysis",
        color = "#6c757d"
      ))
    }

    clustering_result <- comparison_results$clustering

    current_feature_space <- input$semantic_feature_space
    current_ngram_range <- if (current_feature_space == "ngrams") input$semantic_ngram_range else NULL

    if (clustering_result$feature_space != current_feature_space ||
        (current_feature_space == "ngrams" && clustering_result$ngram_range != current_ngram_range)) {
      return(create_error_plot(
        "Feature space changed. Click 'Apply' to re-run clustering analysis",
        color = "#ffc107"
      ))
    }
    cluster_labels <- clustering_result$clusters
    cluster_method <- clustering_result$method %||% input$semantic_cluster_method
    n_clusters <- clustering_result$n_clusters_found %||% max(cluster_labels, na.rm = TRUE)
    silhouette_score <- clustering_result$silhouette

    if (!is.null(clustering_result$similarity_matrix)) {
      similarity_matrix <- clustering_result$similarity_matrix
    } else if (length(comparison_results$results) > 0) {
      recent_result <- comparison_results$results[[length(comparison_results$results)]]
      similarity_matrix <- recent_result$similarity_matrix
    } else {
      feature_matrix <- semantic_feature_matrix()
      if (is.null(feature_matrix)) {
        return(create_error_plot(
          "No feature matrix available. Please process documents first.",
          color = "#dc3545"
        ))
      }
      similarity_matrix <- calculate_cosine_similarity(feature_matrix)
    }


    if (!is.null(clustering_result$umap_embedding)) {
      umap_coords <- clustering_result$umap_embedding
      coords_x <- umap_coords[, 1]
      coords_y <- umap_coords[, 2]
      coord_method <- "UMAP"
    } else if (ncol(similarity_matrix) > 2) {
      dist_matrix <- as.dist(1 - similarity_matrix)
      mds_result <- cmdscale(dist_matrix, k = 2)
      coords_x <- mds_result[, 1]
      coords_y <- mds_result[, 2]
      coord_method <- "MDS"
    } else {
      coords_x <- similarity_matrix[, 1]
      coords_y <- if (ncol(similarity_matrix) > 1) similarity_matrix[, 2] else rep(0, nrow(similarity_matrix))
      coord_method <- "Direct"
    }

    n_docs <- length(cluster_labels)

    if (exists("document_display_data") && !is.null(document_display_data()) && nrow(document_display_data()) > 0) {
      docs_data <- document_display_data()

      if (!is.null(clustering_result$outlier_reduction_method) &&
          clustering_result$outlier_reduction_method == "remove" &&
          !is.null(clustering_result$document_indices)) {
        docs_data <- docs_data[clustering_result$document_indices, , drop = FALSE]
        doc_names <- docs_data$document_number
        doc_ids <- docs_data$document_id_display
        categories <- docs_data$category_display
      } else if (nrow(docs_data) >= n_docs) {
        docs_data <- docs_data[1:n_docs, ]
        doc_names <- docs_data$document_number
        doc_ids <- docs_data$document_id_display
        categories <- docs_data$category_display
      } else {
        doc_names <- paste("Doc", 1:n_docs)
        doc_ids <- paste("Document", 1:n_docs)
        categories <- rep("Unknown", n_docs)
      }
    } else {
      doc_names <- paste("Doc", 1:n_docs)
      doc_ids <- paste("Document", 1:n_docs)
      categories <- rep("Unknown", n_docs)
    }

    unique_clusters <- sort(unique(cluster_labels[cluster_labels != 0]))
    cluster_mapping <- setNames(1:length(unique_clusters), unique_clusters)

    cluster_names <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      ifelse(cluster_labels == 0, "Outlier", paste("Cluster", cluster_mapping[as.character(cluster_labels)]))
    } else {
      paste("Cluster", cluster_mapping[as.character(cluster_labels)])
    }

    outlier_info <- ""
    if (!is.null(clustering_result$outlier_reduction_method)) {
      if (clustering_result$outlier_reduction_method == "remove") {
        removed_count <- clustering_result$outliers_removed %||% 0
        outlier_info <- paste("| Outliers removed:", ifelse(is.null(removed_count) || is.na(removed_count), 0, removed_count))
      } else if (clustering_result$outlier_reduction_method == "reassign") {
        reassigned_count <- clustering_result$outliers_reassigned %||% 0
        outlier_info <- paste("| Outliers reassigned:", ifelse(is.null(reassigned_count) || is.na(reassigned_count), 0, reassigned_count))
      }
    }

    ordered_clusters <- unique(cluster_names[cluster_names != "Outlier"])
    ordered_clusters <- ordered_clusters[order(as.numeric(gsub("[^0-9]", "", ordered_clusters)))]
    if ("Outlier" %in% cluster_names) {
      ordered_clusters <- c(ordered_clusters, "Outlier")
    }
    cluster_names <- factor(cluster_names, levels = ordered_clusters)

    unique_cluster_names <- unique(cluster_names)

    outlier_names <- unique_cluster_names[unique_cluster_names == "Outlier"]
    regular_cluster_names <- unique_cluster_names[unique_cluster_names != "Outlier"]

    if (length(regular_cluster_names) > 0) {
      cluster_numbers <- as.numeric(gsub("Cluster ", "", regular_cluster_names))
      ordered_regular_names <- regular_cluster_names[order(cluster_numbers)]
    } else {
      ordered_regular_names <- character(0)
    }

    ordered_cluster_levels <- c(ordered_regular_names, outlier_names)

    cluster_stats <- table(cluster_names)
    unique_clusters <- length(unique(cluster_labels))

    actual_clusters <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      length(unique(cluster_labels[cluster_labels > 0]))
    } else {
      unique_clusters
    }

    outlier_points <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      sum(cluster_labels == 0)
    } else {
      0
    }

    if (outlier_points > 0) {
      cluster_info <- paste("Clusters found:", actual_clusters, "| Outlier points:", outlier_points, outlier_info)
    } else {
      cluster_info <- paste("Clusters found:", actual_clusters, outlier_info)
    }

    silhouette_info <- if (!is.null(silhouette_score) && !is.na(silhouette_score)) {
      paste("Silhouette:", round(silhouette_score, 3), "(higher is better)")
    } else {
      "Silhouette: N/A"
    }

    davies_bouldin_info <- if (!is.null(clustering_result$davies_bouldin) && !is.na(clustering_result$davies_bouldin)) {
      paste("Davies-Bouldin:", round(clustering_result$davies_bouldin, 3), "(lower is better)")
    } else {
      "Davies-Bouldin: N/A"
    }

    calinski_harabasz_info <- if (!is.null(clustering_result$calinski_harabasz) && !is.na(clustering_result$calinski_harabasz)) {
      paste("Calinski-Harabasz:", round(clustering_result$calinski_harabasz, 1), "(higher is better)")
    } else {
      "Calinski-Harabasz: N/A"
    }

    metrics_info <- paste(silhouette_info, davies_bouldin_info, calinski_harabasz_info, sep = " | ")


    dim_reduction_info <- if (cluster_method == "umap_dbscan") {
      umap_params <- clustering_result$umap_params
      if (!is.null(umap_params)) {
        paste(
          "Dimensionality Reduction:", coord_method,
          "| n_neighbors:", umap_params$n_neighbors,
          "| min_dist:", umap_params$min_dist,
          "| dimensions:", umap_params$n_components
        )
      } else {
        paste("Dimensionality Reduction:", coord_method)
      }
    } else {
      paste("Visualization:", coord_method, "projection")
    }

    unique_cluster_values <- sort(unique(cluster_labels))
    n_unique_clusters <- length(unique_cluster_values)

    if (n_unique_clusters <= 8) {
      base_colors <- RColorBrewer::brewer.pal(min(8, max(3, n_unique_clusters)), "Set2")
    } else if (n_unique_clusters <= 12) {
      base_colors <- RColorBrewer::brewer.pal(12, "Set3")
    } else {
      base_colors <- c(
        RColorBrewer::brewer.pal(8, "Set2"),
        RColorBrewer::brewer.pal(12, "Set3"),
        rainbow(max(0, n_unique_clusters - 20))
      )
    }

    if (length(base_colors) < n_unique_clusters) {
      base_colors <- c(base_colors, rainbow(n_unique_clusters - length(base_colors)))
    }
    base_colors <- base_colors[1:n_unique_clusters]

    cluster_factor_levels <- ordered_cluster_levels

    if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      outlier_indices <- which(cluster_factor_levels == "Outlier")
      regular_indices <- which(cluster_factor_levels != "Outlier")

      color_mapping <- character(length(cluster_factor_levels))
      names(color_mapping) <- cluster_factor_levels

      if (length(regular_indices) > 0) {
        color_mapping[regular_indices] <- base_colors[1:length(regular_indices)]
      }

      if (length(outlier_indices) > 0) {
        color_mapping[outlier_indices] <- "#808080"
      }
    } else {
      color_mapping <- setNames(base_colors[1:length(cluster_factor_levels)], cluster_factor_levels)
    }

    doc_ids_wrapped <- sapply(doc_ids, wrap_long_text, USE.NAMES = FALSE)
    cats_wrapped <- sapply(categories, wrap_long_text, USE.NAMES = FALSE)

    hover_text <- paste0(
      "<b>Document:</b> ", doc_ids_wrapped, "<br>",
      "<b>Category:</b> ", cats_wrapped, "<br>",
      "<b>Cluster:</b> ", cluster_names, "<br>"
    )

    coords_matrix <- cbind(coords_x, coords_y)

    plot_semantic_viz(
      plot_type = "clustering",
      coords = coords_matrix,
      clusters = cluster_labels,
      data_labels = doc_names,
      hover_text = hover_text,
      cluster_colors = color_mapping,
      title = paste("Document Clustering:", stringr::str_to_title(cluster_method)),
      height = 600,
      width = 800
    )
  })

  output$semantic_cluster_table <- DT::renderDataTable({
    clustering_analysis_reactive()

    docs_data <- document_display_data()
    if (is.null(docs_data) || nrow(docs_data) < 2) {
      return(NULL)
    }

    if (!clustering_analysis_triggered()) {
      return(NULL)
    }

    table_view <- input$semantic_cluster_table_view %||% "summary"

    if (is.null(comparison_results$clustering)) {
      return(NULL)
    }

    clustering_result <- comparison_results$clustering

    current_feature_space <- input$semantic_feature_space
    current_ngram_range <- if (current_feature_space == "ngrams") input$semantic_ngram_range else NULL

    if (clustering_result$feature_space != current_feature_space ||
        (current_feature_space == "ngrams" && clustering_result$ngram_range != current_ngram_range)) {
      return(NULL)
    }

    cluster_labels <- clustering_result$clusters
    cluster_method <- clustering_result$method %||% "unknown"
    n_clusters_found <- clustering_result$n_clusters_found %||% max(cluster_labels, na.rm = TRUE)
    actual_clusters <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      length(unique(cluster_labels[cluster_labels > 0]))
    } else {
      length(unique(cluster_labels))
    }
    silhouette_score <- clustering_result$silhouette

    n_docs <- length(cluster_labels)

    if (exists("document_display_data") && !is.null(document_display_data()) && nrow(document_display_data()) > 0) {
      docs_data <- document_display_data()

      if (!is.null(clustering_result$outlier_reduction_method) &&
          clustering_result$outlier_reduction_method == "remove" &&
          !is.null(clustering_result$document_indices)) {
        docs_data <- docs_data[clustering_result$document_indices, , drop = FALSE]
        doc_names <- docs_data$document_number
        doc_ids <- docs_data$document_id_display
        categories <- docs_data$category_display
      } else if (nrow(docs_data) >= n_docs) {
        docs_data <- docs_data[1:n_docs, ]
        doc_names <- docs_data$document_number
        doc_ids <- docs_data$document_id_display
        categories <- docs_data$category_display
      } else {
        doc_names <- paste("Doc", 1:n_docs)
        doc_ids <- paste("Document", 1:n_docs)
        categories <- rep("Unknown", n_docs)
      }
    } else {
      doc_names <- paste("Doc", 1:n_docs)
      doc_ids <- paste("Document", 1:n_docs)
      categories <- rep("Unknown", n_docs)
    }

    unique_clusters <- sort(unique(cluster_labels[cluster_labels != 0]))
    cluster_mapping <- setNames(1:length(unique_clusters), unique_clusters)

    cluster_names_table <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
      ifelse(cluster_labels == 0, "Outlier", paste("Cluster", cluster_mapping[as.character(cluster_labels)]))
    } else {
      paste("Cluster", cluster_mapping[as.character(cluster_labels)])
    }

    if (table_view == "details") {
      doc_cluster_df <- data.frame(
        Document = doc_names,
        Document_ID = doc_ids,
        Category = categories,
        Cluster = cluster_names_table,
        Cluster_ID = cluster_labels,
        stringsAsFactors = FALSE
      )

      table_out <- DT::datatable(
        doc_cluster_df,
        rownames = FALSE,
        caption = "Document Cluster Assignments",
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          dom = "frtip",
          searching = TRUE
        )
      ) %>%
        DT::formatStyle(
          "Cluster",
          fontWeight = "bold"
        ) %>%
        DT::formatStyle(
          "Document",
          fontSize = "90%"
        )
    } else {
      cluster_table <- table(cluster_labels)
      cluster_sizes <- as.numeric(cluster_table)
      cluster_percentages <- round(cluster_sizes / n_docs * 100, 1)

      cluster_summary_names <- if (cluster_method == "umap_dbscan" && any(cluster_labels == 0)) {
        cluster_ids <- as.numeric(names(cluster_table))
        ifelse(cluster_ids == 0, "Outlier", paste("Cluster", cluster_ids))
      } else {
        paste("Cluster", names(cluster_table))
      }

      cluster_summary <- data.frame(
        Cluster = cluster_summary_names,
        Size = cluster_sizes,
        Percentage = cluster_percentages,
        Method = stringr::str_to_title(cluster_method),
        stringsAsFactors = FALSE
      )

      if (cluster_method == "kmeans" && !is.null(clustering_result$tot.withinss)) {
        cluster_summary$Within_SS <- round(clustering_result$withinss, 2)
      }

      if (!is.null(clustering_result$outlier_reduction_method)) {
        if (clustering_result$outlier_reduction_method == "remove") {
          removed_count <- clustering_result$outliers_removed %||% 0
          cluster_summary$Outliers_Removed <- removed_count
        } else if (clustering_result$outlier_reduction_method == "reassign") {
          reassigned_count <- clustering_result$outliers_reassigned %||% 0
          cluster_summary$Outliers_Reassigned <- reassigned_count
        }
      }

      silhouette_text <- if (!is.null(silhouette_score) && !is.na(silhouette_score)) {
        paste("Silhouette:", round(silhouette_score, 3))
      } else {
        "Silhouette: N/A"
      }

      davies_bouldin_score <- clustering_result$davies_bouldin
      davies_bouldin_text <- if (!is.null(davies_bouldin_score) && !is.na(davies_bouldin_score)) {
        paste("Davies-Bouldin:", round(davies_bouldin_score, 3))
      } else {
        "Davies-Bouldin: N/A"
      }

      calinski_harabasz_score <- clustering_result$calinski_harabasz
      calinski_harabasz_text <- if (!is.null(calinski_harabasz_score) && !is.na(calinski_harabasz_score)) {
        paste("Calinski-Harabasz:", round(calinski_harabasz_score, 1))
      } else {
        "Calinski-Harabasz: N/A"
      }

      table_out <- DT::datatable(
        cluster_summary,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          width = "100%",
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      )
    }
    table_out
  })

  observe({
    comparison_results$clustering
    comparison_results$update_trigger

    if (!is.null(comparison_results$clustering)) {
      unique_clusters <- sort(unique(comparison_results$clustering$clusters[comparison_results$clustering$clusters > 0]))
      updateSelectInput(
        session,
        "selected_cluster_group",
        choices = setNames(unique_clusters, paste("Group", unique_clusters))
      )
    }
  })

  output$cluster_group_summary <- renderUI({
    req(input$selected_cluster_group)
    if (!is.null(comparison_results$clustering)) {
      cluster_id <- as.numeric(input$selected_cluster_group)
      cluster_docs <- which(comparison_results$clustering$clusters == cluster_id)
      n_docs <- length(cluster_docs)

      tags$div(
        style = "padding: 10px; background: #F8FAFC; border-radius: 4px;",
        tags$p(
          paste("This group contains", n_docs, "documents"),
          style = "margin: 0; color: #475569;"
        )
      )
    }
  })

  output$cluster_terms_plot <- plotly::renderPlotly({
    req(input$selected_cluster_group)

    dfm_check <- tryCatch(dfm_outcome(), error = function(e) NULL)

    if (!is.null(comparison_results$clustering) && !is.null(dfm_check)) {
      cluster_id <- as.numeric(input$selected_cluster_group)
      cluster_docs <- which(comparison_results$clustering$clusters == cluster_id)

      cluster_dfm <- dfm_check[cluster_docs, ]
      top_features <- topfeatures(cluster_dfm, 10)

      TextAnalysisR::plot_cluster_terms(
        terms = top_features,
        cluster_id = cluster_id,
        n_terms = 10,
        color = "#3B82F6"
      )
    }
  })

  output$cluster_sample_docs <- DT::renderDataTable({
    req(input$selected_cluster_group)
    if (!is.null(comparison_results$clustering)) {
      cluster_id <- as.numeric(input$selected_cluster_group)
      cluster_docs <- which(comparison_results$clustering$clusters == cluster_id)
      docs_data <- document_display_data()

      if (!is.null(docs_data) && nrow(docs_data) > 0) {
        sample_docs <- head(cluster_docs, 5)

        if (length(sample_docs) > 0 && max(sample_docs) <= nrow(docs_data)) {
          sample_data <- docs_data[sample_docs, ]

          display_cols <- c()
          if ("doc_id" %in% names(sample_data)) display_cols <- c(display_cols, "doc_id")
          if ("text" %in% names(sample_data)) {
            sample_data$text_preview <- substr(sample_data$text, 1, 100)
            display_cols <- c(display_cols, "text_preview")
          }

          if (length(display_cols) > 0) {
            DT::datatable(
              sample_data[, display_cols, drop = FALSE],
              options = list(
                dom = 't',
                pageLength = 5,
                ordering = FALSE
              ),
              rownames = FALSE
            )
          }
        }
      }
    }
  })

  output$clustering_summary_status <- renderUI({
    if (!is.null(comparison_results$clustering)) {
      n_clusters <- length(unique(comparison_results$clustering$clusters[comparison_results$clustering$clusters > 0]))
      n_outliers <- sum(comparison_results$clustering$clusters == 0)
      quality_score <- comparison_results$clustering$silhouette %||% NA

      div(
        style = "padding: 15px; background: #F0F9FF; border: 1px solid #3B82F6; margin-bottom: 20px; border-radius: 4px;",
        tags$h4(
          paste("Found", n_clusters, "document groups"),
          style = "margin-top: 0; color: #1E40AF;"
        ),
        if (!is.na(quality_score)) {
          tags$p(
            paste0("Quality score: ", round(quality_score * 100, 1), "%"),
            style = "margin-bottom: 5px; color: #64748B;"
          )
        },
        if (n_outliers > 0) {
          tags$small(
            paste("(", n_outliers, "outliers detected)"),
            style = "color: #94A3B8;"
          )
        }
      )
    }
  })

  output$clustering_quality_metrics <- renderUI({
    comparison_results$update_trigger

    if (is.null(comparison_results$clustering)) {
      return(NULL)
    }

    clustering_result <- comparison_results$clustering

    current_feature_space <- input$semantic_feature_space
    current_ngram_range <- if (current_feature_space == "ngrams") input$semantic_ngram_range else NULL

    if (clustering_result$feature_space != current_feature_space ||
        (current_feature_space == "ngrams" && clustering_result$ngram_range != current_ngram_range)) {
      return(htmltools::tags$div(
        htmltools::tags$p("Clustering results are outdated due to feature space or N-gram range changes. Please re-run clustering analysis.",
                          style = "color: orange; font-weight: bold;")
      ))
    }

    silhouette_score <- clustering_result$silhouette
    davies_bouldin_score <- clustering_result$davies_bouldin
    calinski_harabasz_score <- clustering_result$calinski_harabasz

    htmltools::tags$div(
      htmltools::tags$strong("Global Quality Metrics"),
      htmltools::tags$table(
        style = "width: auto; margin-top: 8px; margin-bottom: 8px;",
        htmltools::tags$tr(
          htmltools::tags$th("Metric", style = "padding-right: 16px; text-align: left;"),
          htmltools::tags$th("Value", style = "text-align: left;")
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Silhouette Score"),
          htmltools::tags$td(
            if (!is.null(silhouette_score) && !is.na(silhouette_score)) {
              round(silhouette_score, 3)
            } else {
              "N/A"
            },
            htmltools::tags$span(" (Higher is better)", style = "font-size: 0.9em; color: #888;")
          )
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Davies-Bouldin"),
          htmltools::tags$td(
            if (!is.null(davies_bouldin_score) && !is.na(davies_bouldin_score)) {
              round(davies_bouldin_score, 3)
            } else {
              "N/A"
            },
            htmltools::tags$span(" (Lower is better)", style = "font-size: 0.9em; color: #888;")
          )
        ),
        htmltools::tags$tr(
          htmltools::tags$td("Calinski-Harabasz"),
          htmltools::tags$td(
            if (!is.null(calinski_harabasz_score) && !is.na(calinski_harabasz_score)) {
              round(calinski_harabasz_score, 1)
            } else {
              "N/A"
            },
            htmltools::tags$span(" (Higher is better)", style = "font-size: 0.9em; color: #888;")
          )
        )
      )
    )
  })

  output$clustering_quality_metrics_compact <- renderUI({
    comparison_results$update_trigger

    if (is.null(comparison_results$clustering)) {
      return(htmltools::tags$div(
        style = "color: #6b7280; font-size: 16px;",
        "Run clustering to see quality metrics"
      ))
    }

    clustering_result <- comparison_results$clustering

    current_feature_space <- input$semantic_feature_space
    current_ngram_range <- if (current_feature_space == "ngrams") input$semantic_ngram_range else NULL

    if (clustering_result$feature_space != current_feature_space ||
        (current_feature_space == "ngrams" && clustering_result$ngram_range != current_ngram_range)) {
      return(htmltools::tags$div(
        style = "color: #dc2626; font-size: 16px;",
        icon("exclamation-triangle"),
        " Metrics outdated - please re-run clustering"
      ))
    }

    silhouette_score <- clustering_result$silhouette
    davies_bouldin_score <- clustering_result$davies_bouldin
    calinski_harabasz_score <- clustering_result$calinski_harabasz

    htmltools::tags$table(
      style = "width: 100%; margin-top: 8px; margin-bottom: 8px; font-size: 16px;",
      htmltools::tags$tr(
        htmltools::tags$th("Metric", style = "text-align: left; padding: 4px 20px 4px 0; color: #4b5563; font-weight: 500;"),
        htmltools::tags$th("Value", style = "text-align: left; padding: 4px 20px 4px 0; color: #4b5563; font-weight: 500;"),
        htmltools::tags$th("Interpretation", style = "text-align: left; padding: 4px 0; color: #4b5563; font-weight: 500;")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Silhouette Score", style = "padding: 4px 20px 4px 0;"),
        htmltools::tags$td(
          if (!is.null(silhouette_score) && !is.na(silhouette_score)) {
            round(silhouette_score, 3)
          } else {
            "N/A"
          },
          style = "padding: 4px 20px 4px 0; font-weight: 600; color: #0c1f4a;"
        ),
        htmltools::tags$td("Higher is better", style = "padding: 4px 0; color: #6b7280;")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Davies-Bouldin", style = "padding: 4px 20px 4px 0;"),
        htmltools::tags$td(
          if (!is.null(davies_bouldin_score) && !is.na(davies_bouldin_score)) {
            round(davies_bouldin_score, 3)
          } else {
            "N/A"
          },
          style = "padding: 4px 20px 4px 0; font-weight: 600; color: #0c1f4a;"
        ),
        htmltools::tags$td("Lower is better", style = "padding: 4px 0; color: #6b7280;")
      ),
      htmltools::tags$tr(
        htmltools::tags$td("Calinski-Harabasz", style = "padding: 4px 20px 4px 0;"),
        htmltools::tags$td(
          if (!is.null(calinski_harabasz_score) && !is.na(calinski_harabasz_score)) {
            round(calinski_harabasz_score, 1)
          } else {
            "N/A"
          },
          style = "padding: 4px 20px 4px 0; font-weight: 600; color: #0c1f4a;"
        ),
        htmltools::tags$td("Higher is better", style = "padding: 4px 0; color: #6b7280;")
      )
    )
  })


  output$clustering_warning <- renderUI({
    NULL
  })

  output$outlier_reduction_plot <- plotly::renderPlotly({
    input$outlier_reduction_method
    input$outlier_threshold

    if (is.null(input$reduce_outliers) || input$reduce_outliers == 0) {
      return(create_error_plot(
        "Click 'Reduce Outliers' to run outlier reduction",
        color = "#6c757d"
      ))
    }

    if (is.null(comparison_results$clustering)) {
      return(create_error_plot(
        "No clustering results available.<br>Please run clustering analysis first.",
        color = "#6c757d"
      ))
    }

    clustering_result <- comparison_results$clustering

    if (clustering_result$method != "umap_dbscan") {
      return(create_error_plot(
        "Outlier reduction is only available for UMAP-DBSCAN clustering method.",
        color = "#ffc107"
      ))
    }

    if (is.null(clustering_result$outlier_reduction_method)) {
      return(create_error_plot(
        "No outlier reduction results available.<br>Please run outlier reduction first.",
        color = "#6c757d"
      ))
    }

    cluster_labels <- clustering_result$clusters
    n_docs <- length(cluster_labels)

    if (!is.null(clustering_result$umap_embedding)) {
      umap_coords <- clustering_result$umap_embedding
      coords_x <- umap_coords[, 1]
      coords_y <- umap_coords[, 2]
      coord_method <- "UMAP"
    } else {
      return(create_error_plot(
        "No UMAP coordinates available for outlier reduction visualization.",
        color = "#6c757d"
      ))
    }

    docs_data <- document_display_data()
    if (!is.null(docs_data) && nrow(docs_data) >= n_docs) {
      docs_data <- docs_data[1:n_docs, ]
      doc_names <- docs_data$document_number
      doc_ids <- docs_data$document_id_display
      categories <- docs_data$category_display
    } else {
      doc_names <- paste("Doc", 1:n_docs)
      doc_ids <- paste("Document", 1:n_docs)
      categories <- rep("Unknown", n_docs)
    }

    unique_clusters <- sort(unique(cluster_labels[cluster_labels != 0]))
    cluster_mapping <- setNames(1:length(unique_clusters), unique_clusters)

    cluster_names <- if (any(cluster_labels == 0)) {
      ifelse(cluster_labels == 0, "Outlier", paste("Cluster", cluster_mapping[as.character(cluster_labels)]))
    } else {
      paste("Cluster", cluster_mapping[as.character(cluster_labels)])
    }

    plot_data <- data.frame(
      x = coords_x,
      y = coords_y,
      cluster = cluster_names,
      document = doc_names,
      document_id = doc_ids,
      category = categories,
      stringsAsFactors = FALSE
    )

    unique_cluster_names <- unique(cluster_names)
    n_clusters <- length(unique_cluster_names)

    if (n_clusters <= 8) {
      colors <- RColorBrewer::brewer.pal(max(n_clusters, 3), "Set2")[1:n_clusters]
    } else {
      colors <- c(RColorBrewer::brewer.pal(8, "Set2"), rainbow(n_clusters - 8))
    }
    names(colors) <- unique_cluster_names

    p <- plotly::plot_ly(
      data = plot_data,
      x = ~x,
      y = ~y,
      color = ~cluster,
      colors = colors,
      type = "scatter",
      mode = "markers",
      text = ~paste("Document:", document_id, "<br>Category:", category, "<br>Cluster:", cluster),
      hoverinfo = "text",
      marker = list(size = 10, opacity = 0.7)
    ) %>%
      plotly::layout(
        title = list(
          text = paste("Outlier Reduction Results -", stringr::str_to_title(clustering_result$outlier_reduction_method)),
          font = list(size = 18, color = "#0c1f4a", family = "Roboto"),
          x = 0.5,
          xref = "paper",
          xanchor = "center"
        ),
        xaxis = list(
          title = paste(coord_method, "1"),
          titlefont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
          tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
        ),
        yaxis = list(
          title = paste(coord_method, "2"),
          titlefont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
          tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
        ),
        showlegend = TRUE,
        legend = list(
          title = list(text = "Clusters", font = list(size = 16, color = "#3B3B3B", family = "Roboto, sans-serif")),
          font = list(size = 16, color = "#3B3B3B"),
          orientation = "v",
          x = 1.05,
          y = 0.5
        ),
        margin = list(t = 80, b = 50, l = 50, r = 150)
      )

    outlier_info <- ""
    if (clustering_result$outlier_reduction_method == "remove") {
      removed_count <- clustering_result$outliers_removed %||% 0
      outlier_info <- paste("Outliers removed:", removed_count)
    } else if (clustering_result$outlier_reduction_method == "reassign") {
      reassigned_count <- clustering_result$outliers_reassigned %||% 0
      outlier_info <- paste("Outliers reassigned:", reassigned_count)
    }

    if (nchar(outlier_info) > 0) {
      p <- p %>% plotly::add_annotations(
        text = outlier_info,
        xref = "paper",
        yref = "paper",
        x = 0.5,
        y = -0.1,
        showarrow = FALSE,
        font = list(size = 16, color = "#666")
      )
    }

    p
  })

  output$outlier_reduction_table <- DT::renderDataTable({
    input$outlier_reduction_method
    input$outlier_threshold

    if (is.null(input$reduce_outliers) || input$reduce_outliers == 0) {
      return(DT::datatable(
        data.frame(Message = "Click 'Reduce Outliers' to run outlier reduction"),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    if (is.null(comparison_results$clustering)) {
      return(DT::datatable(
        data.frame(Message = "No clustering results available. Please run clustering analysis first."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    clustering_result <- comparison_results$clustering

    if (clustering_result$method != "umap_dbscan") {
      return(DT::datatable(
        data.frame(Message = "Outlier reduction is only available for UMAP-DBSCAN clustering method."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    if (is.null(clustering_result$outlier_reduction_method)) {
      return(DT::datatable(
        data.frame(Message = "No outlier reduction results available. Please run outlier reduction first."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    cluster_labels <- clustering_result$clusters
    n_docs <- length(cluster_labels)

    cluster_table <- table(cluster_labels)
    cluster_sizes <- as.numeric(cluster_table)
    cluster_percentages <- round(cluster_sizes / n_docs * 100, 1)

    cluster_summary_names <- if (any(cluster_labels == 0)) {
      cluster_ids <- as.numeric(names(cluster_table))
      ifelse(cluster_ids == 0, "Outlier", paste("Cluster", cluster_ids))
    } else {
      paste("Cluster", names(cluster_table))
    }

    summary_data <- data.frame(
      Cluster = cluster_summary_names,
      Size = cluster_sizes,
      Percentage = cluster_percentages,
      Method = stringr::str_to_title(clustering_result$outlier_reduction_method),
      stringsAsFactors = FALSE
    )

    if (clustering_result$outlier_reduction_method == "remove") {
      removed_count <- clustering_result$outliers_removed %||% 0
      summary_data$Outliers_Removed <- removed_count
      summary_data$Total_Documents <- n_docs + removed_count
    } else if (clustering_result$outlier_reduction_method == "reassign") {
      reassigned_count <- clustering_result$outliers_reassigned %||% 0
      summary_data$Outliers_Reassigned <- reassigned_count
    }

    DT::datatable(
      summary_data,
      rownames = FALSE,
      caption = "Outlier Reduction Results",
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        width = "100%",
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf", "print")
      )
    )
  })



  clean_similarity_matrix <- function(similarity_matrix) {
    similarity_matrix[!is.finite(similarity_matrix)] <- 0

    if (nrow(similarity_matrix) == ncol(similarity_matrix)) {
      similarity_matrix <- (similarity_matrix + t(similarity_matrix)) / 2
    }

    if (nrow(similarity_matrix) == ncol(similarity_matrix)) {
      diag(similarity_matrix) <- 1
    }

    return(similarity_matrix)
  }

  renumber_clusters_sequentially <- function(clusters) {
    if (is.null(clusters) || length(clusters) == 0) {
      return(clusters)
    }

    unique_clusters <- sort(unique(clusters))
    cluster_mapping <- setNames(1:length(unique_clusters), unique_clusters)
    return(cluster_mapping[as.character(clusters)])
  }

  advanced_results <- reactiveValues(
    ai_labels = NULL,
    cross_validation = NULL,
    temporal_analysis = NULL,
    llm_enhanced = NULL,
    neural_topic_model = NULL
  )

  output$temporal_ready <- reactive({
    documents_data_reactive$has_dates
  })
  outputOptions(output, "temporal_ready", suspendWhenHidden = FALSE)

  observeEvent(input$run_temporal_analysis, {
    req(document_display_data())
    req(documents_data_reactive$has_dates)

    docs_data <- document_display_data()
    if (!"date" %in% names(docs_data)) {
      showNotification("No date variable found. Please configure dates in Document Configuration.", type = "error")
      return()
    }

    show_loading_notification("Running temporal semantic analysis...", id = "temporal_progress")

    tryCatch({
      embeddings <- if (!is.null(embeddings_cache$embeddings)) {
        embeddings_cache$embeddings
      } else {
        TextAnalysisR::generate_embeddings(
          texts = docs_data$combined_text,
          model = "all-MiniLM-L6-v2",
          verbose = FALSE
        )
      }

      temporal_result <- TextAnalysisR::fit_temporal_model(
        texts = docs_data$combined_text,
        dates = docs_data$date,
        time_windows = input$temporal_window,
        embeddings = embeddings,
        verbose = FALSE
      )

      advanced_results$temporal_analysis <- temporal_result

      remove_notification_by_id("temporal_progress")
      show_completion_notification("Temporal analysis completed successfully!", duration = 3)

    }, error = function(e) {
      remove_notification_by_id("temporal_progress")
      show_error_notification(paste("Temporal analysis error:", e$message))
    })
  })

  output$temporal_evolution_plot <- plotly::renderPlotly({
    req(advanced_results$temporal_analysis)

    result <- advanced_results$temporal_analysis
    if (!is.null(result$evolution_plot)) {
      result$evolution_plot
    } else if (!is.null(result$drift_over_time)) {
      plotly::plot_ly(
        x = names(result$drift_over_time),
        y = result$drift_over_time,
        type = "scatter",
        mode = "lines+markers",
        name = "Semantic Drift",
        marker = list(color = "#337ab7", size = 8),
        line = list(color = "#337ab7", width = 2),
        hovertemplate = "Period: %{x}<br>Drift Score: %{y:.3f}<extra></extra>"
      ) %>%
        plotly::layout(
          title = list(
            text = "Semantic Evolution Over Time",
            font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
            x = 0.5,
            xref = "paper",
            xanchor = "center"
          ),
          xaxis = list(
            title = list(text = "Time Period"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          yaxis = list(
            title = list(text = "Semantic Drift Score"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          margin = list(t = 60, b = 60, l = 80, r = 40),
          hoverlabel = list(
            font = list(size = 16, family = "Roboto, sans-serif"),
            align = "left"
          )
        )
    } else {
      TextAnalysisR::create_empty_plot_message("No temporal data to display")
    }
  })

  output$temporal_metrics_table <- DT::renderDataTable({
    req(advanced_results$temporal_analysis)

    result <- advanced_results$temporal_analysis
    if (!is.null(result$topic_stability)) {
      data.frame(
        Metric = c("Average Topic Stability", "Semantic Coherence", "Temporal Consistency"),
        Value = c(
          round(mean(result$topic_stability, na.rm = TRUE), 3),
          round(result$coherence %||% 0, 3),
          round(result$consistency %||% 0, 3)
        )
      )
    } else {
      data.frame(Metric = "No metrics available", Value = NA)
    }
  }, options = list(pageLength = 10, dom = 't'))

  observeEvent(input$run_cross_validation, {
    req(document_display_data())
    req(length(input$crossval_methods) >= 2)

    show_loading_notification("Running cross-validation analysis...", id = "crossval_progress")

    texts <- document_display_data()$combined_text

    tryCatch({
      semantic_results <- list()

      embeddings <- if (!is.null(embeddings_cache$embeddings)) {
        embeddings_cache$embeddings
      } else {
        TextAnalysisR::generate_embeddings(
          texts = texts,
          model = "all-MiniLM-L6-v2",
          verbose = FALSE
        )
      }

      for (method in input$crossval_methods) {
        if (method == "kmeans" || method == "hierarchical" || method == "dbscan") {
          result <- TextAnalysisR::cluster_embeddings(
            data_matrix = embeddings,
            method = method,
            n_clusters = 5,
            seed = 123,
            verbose = FALSE
          )
          semantic_results[[method]] <- result
        } else if (method == "bertopic") {
          result <- TextAnalysisR::fit_embedding_topics(
            texts = texts,
            method = "umap_hdbscan",
            n_topics = 10,
            min_topic_size = 10,
            embedding_model = "all-MiniLM-L6-v2",
            seed = 123,
            verbose = FALSE
          )
          semantic_results[[method]] <- result
        } else if (method == "stm" && !is.null(stm_K_number())) {
          semantic_results[[method]] <- list(
            model = stm_K_number(),
            n_topics = stm_K_number()$settings$dim$K
          )
        }
      }

      if (length(semantic_results) >= 2) {
        validation_result <- TextAnalysisR::validate_cross_models(
          semantic_results = semantic_results,
          stm_results = if ("stm" %in% names(semantic_results)) semantic_results$stm else NULL,
          verbose = FALSE
        )
        advanced_results$cross_validation <- validation_result
      }

      remove_notification_by_id("crossval_progress")
      show_completion_notification("Cross-validation completed successfully!", duration = 3)

    }, error = function(e) {
      remove_notification_by_id("crossval_progress")
      show_error_notification(paste("Cross-validation error:", e$message))
    })
  })

  output$crossval_comparison_plot <- plotly::renderPlotly({
    req(advanced_results$cross_validation)

    result <- advanced_results$cross_validation

    if (!is.null(result$comparison_metrics)) {
      metrics_df <- result$comparison_metrics

      plotly::plot_ly(
        data = metrics_df,
        x = ~Method,
        y = ~Score,
        type = "bar",
        color = ~Metric,
        text = ~paste("Score:", round(Score, 3)),
        textposition = "outside",
        hovertemplate = "Method: %{x}<br>Score: %{y:.3f}<extra></extra>"
      ) %>%
        plotly::layout(
          title = list(
            text = "Method Comparison Results",
            font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
            x = 0.5,
            xref = "paper",
            xanchor = "center"
          ),
          xaxis = list(
            title = list(text = "Analysis Method"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          yaxis = list(
            title = list(text = "Score"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          barmode = "group",
          legend = list(
            title = list(text = "Metric", font = list(size = 16, color = "#0c1f4a", family = "Roboto, sans-serif")),
            font = list(size = 16, color = "#3B3B3B", family = "Roboto, sans-serif")
          ),
          margin = list(t = 60, b = 60, l = 80, r = 40),
          hoverlabel = list(
            font = list(size = 16, family = "Roboto, sans-serif"),
            align = "left"
          )
        )
    } else {
      TextAnalysisR::create_empty_plot_message("No comparison data available")
    }
  })

  output$crossval_results_table <- DT::renderDataTable({
    req(advanced_results$cross_validation)

    result <- advanced_results$cross_validation

    if (!is.null(result$validation_metrics)) {
      result$validation_metrics
    } else if (!is.null(result$topic_cluster_correspondence)) {
      data.frame(
        Metric = "Topic-Cluster Correspondence",
        Value = round(result$topic_cluster_correspondence, 3)
      )
    } else {
      data.frame(
        Message = "Cross-validation results will appear here"
      )
    }
  }, options = list(pageLength = 10, scrollX = TRUE))

  ollama_available_cluster <- reactive({
    TextAnalysisR::check_ollama(verbose = FALSE)
  })

  ollama_models_cluster <- reactive({
    if (ollama_available_cluster()) {
      models <- TextAnalysisR::list_ollama_models(verbose = FALSE)
      if (!is.null(models) && length(models) > 0) {
        return(models)
      }
    }
    return(NULL)
  })

  output$ollama_status_cluster <- renderUI({
    if (ollama_available_cluster()) {
      models <- ollama_models_cluster()
      if (!is.null(models) && length(models) > 0) {
        tags$div(
          style = "background-color: #D1FAE5; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
          tags$small(
            style = "color: #065F46;",
            icon("check-circle"), " Ollama is available with ", length(models), " model(s)"
          )
        )
      } else {
        tags$div(
          style = "background-color: #FEF3C7; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
          tags$small(
            style = "color: #92400E;",
            icon("exclamation-triangle"), " Ollama running but no models found. Run: ollama pull phi3:mini"
          )
        )
      }
    } else {
      tags$div(
        style = "background-color: #FEE2E2; padding: 8px; border-radius: 4px; margin-bottom: 10px;",
        tags$small(
          style = "color: #991B1B;",
          icon("times-circle"), " Ollama not available. Install from: ",
          tags$a(href = "https://ollama.ai", target = "_blank", "ollama.ai")
        )
      )
    }
  })

  output$cluster_ollama_model_selector <- renderUI({
    models <- ollama_models_cluster()
    if (!is.null(models) && length(models) > 0) {
      selectInput(
        "cluster_ollama_model",
        "Ollama Model:",
        choices = models,
        selected = models[1]
      )
    } else {
      NULL
    }
  })

  output$cluster_labeling_status <- renderUI({
    provider <- input$cluster_label_provider
    if (is.null(provider)) provider <- "auto"

    if (provider == "auto") {
      if (ollama_available_cluster()) {
        tags$p(style = "color: #337ab7; font-size: 16px;", icon("info-circle"), " Using Ollama (local AI)")
      } else {
        tags$p(style = "color: #DC2626; font-size: 16px;", icon("info-circle"), " Will use OpenAI (requires API key)")
      }
    }
  })

  observeEvent(input$generate_cluster_labels, {
    if (is.null(comparison_results$clustering)) {
      showNotification("Please run clustering analysis first", type = "error")
      return()
    }

    provider <- if (is.null(input$cluster_label_provider)) "auto" else input$cluster_label_provider

    if (provider == "ollama" && !TextAnalysisR::check_feature("ollama")) {
      showNotification("Ollama not available. Install from ollama.com or use OpenAI.", type = "warning")
      return()
    }

    model <- if (provider == "ollama" || (provider == "auto" && ollama_available_cluster())) {
      if (!is.null(input$cluster_ollama_model)) {
        input$cluster_ollama_model
      } else {
        "phi3:mini"
      }
    } else {
      if (!is.null(input$cluster_openai_model)) {
        input$cluster_openai_model
      } else {
        "gpt-3.5-turbo"
      }
    }

    show_loading_notification(paste("Generating AI labels using", provider, "..."), id = "loadingAILabels")

    tryCatch({
      clustering_result <- comparison_results$clustering
      cluster_assignments <- clustering_result$clusters

      docs_data <- document_display_data()
      if (is.null(docs_data)) {
        showNotification("Document data not available", type = "error")
        return()
      }

      texts <- docs_data$combined_text[1:length(cluster_assignments)]

      cluster_keywords <- list()
      unique_clusters <- unique(cluster_assignments)

      for (cluster in unique_clusters) {
        if (cluster == 0) next
        cluster_docs <- which(cluster_assignments == cluster)
        cluster_texts <- texts[cluster_docs]

        if (length(cluster_texts) > 0) {
          temp_tbl <- tibble::tibble(united_texts = cluster_texts)
          tokens <- TextAnalysisR::prep_texts(
            temp_tbl,
            text_field = "united_texts",
            remove_stopwords = TRUE,
            verbose = FALSE
          )

          dfm <- quanteda::dfm(tokens)
          dfm <- quanteda::dfm_trim(dfm, min_termfreq = 2, min_docfreq = 1)

          if (quanteda::nfeat(dfm) > 0) {
            top_terms <- quanteda.textstats::textstat_frequency(dfm, n = 10)
            cluster_keywords[[as.character(cluster)]] <- top_terms$feature
          } else {
            cluster_keywords[[as.character(cluster)]] <- character(0)
          }
        }
      }

      ai_labels <- TextAnalysisR::generate_cluster_labels(
        cluster_keywords = cluster_keywords,
        provider = provider,
        model = model,
        temperature = 0.3,
        max_tokens = 50,
        verbose = FALSE
      )

      advanced_results$ai_labels <- ai_labels
      advanced_results$cluster_keywords <- cluster_keywords

      remove_notification_by_id("loadingAILabels")
      show_completion_notification("AI labels generated successfully!")

    }, error = function(e) {
      remove_notification_by_id("loadingAILabels")
      show_error_notification(paste("Error generating AI labels:", e$message))
    })
  })







  # "Structural Topic Model" page

  # Step 1. Search K.

  observe({
    updateSelectizeInput(session,
                         "categorical_var",
                         choices = colnames_cat(),
                         selected = ""
    )
  })

  observe({
    updateSelectInput(session,
                      "continuous_var",
                      choices = colnames_con(),
                      selected = ""
    )
  })

  K_range <- eventReactive(input$K_range, {
    seq(input$K_range[1], input$K_range[2])
  })

  # Cache for STM conversion to avoid redundant DFM->STM conversions
  stm_conversion_cache <- reactiveValues(
    result = NULL,
    dfm_hash = NULL,
    notification_shown = FALSE
  )

  out <- reactive({
    dfm_obj <- get_available_dfm()

    if (is.null(dfm_obj)) {
      return(NULL)
    }

    # Check cache using hash of DFM dimensions and feature names
    current_hash <- digest::digest(list(
      ndoc = quanteda::ndoc(dfm_obj),
      nfeat = quanteda::nfeat(dfm_obj),
      docnames = quanteda::docnames(dfm_obj)
    ), algo = "md5")

    # Return cached result if DFM hasn't changed
    if (!is.null(stm_conversion_cache$result) &&
        !is.null(stm_conversion_cache$dfm_hash) &&
        stm_conversion_cache$dfm_hash == current_hash) {
      return(stm_conversion_cache$result)
    }

    # Reset notification flag for new DFM
    stm_conversion_cache$notification_shown <- FALSE

    result <- suppressWarnings(quanteda::convert(dfm_obj, to = "stm"))

    original_docs <- quanteda::ndoc(dfm_obj)
    remaining_docs <- length(result$documents)
    dropped_docs <- original_docs - remaining_docs

    # Only show notification once per DFM change
    if (dropped_docs > 0 && !isTRUE(stm_conversion_cache$notification_shown)) {
      showNotification(
        paste0("Note: ", dropped_docs, " empty document(s) were automatically removed during STM conversion. ",
               "Original documents: ", original_docs, ", Remaining: ", remaining_docs),
        type = "message",
        duration = 8,
        closeButton = TRUE
      )
      stm_conversion_cache$notification_shown <- TRUE
    }

    # Update cache
    stm_conversion_cache$result <- result
    stm_conversion_cache$dfm_hash <- current_hash

    result
  })

  prevalence_formula_K_search <- reactive({
    categorical_var <- if (!is.null(input$categorical_var)) {
      trimws(unlist(strsplit(as.character(input$categorical_var), ",")))
    } else {
      NULL
    }
    continuous_var <- if (!is.null(input$continuous_var)) {
      trimws(unlist(strsplit(as.character(input$continuous_var), ",")))
    } else {
      NULL
    }

    if (!is.null(categorical_var) && length(categorical_var) > 0) {
      for (var in categorical_var) {
        tryCatch({
          TextAnalysisR:::validate_column_name(var)
        }, error = function(e) {
          TextAnalysisR:::log_security_event(
            "INVALID_COLUMN_NAME",
            paste("Invalid categorical variable name:", var),
            session,
            "WARNING"
          )
          stop("Invalid column name format in categorical variables")
        })
      }
    }
    if (!is.null(continuous_var) && length(continuous_var) > 0) {
      for (var in continuous_var) {
        tryCatch({
          TextAnalysisR:::validate_column_name(var)
        }, error = function(e) {
          TextAnalysisR:::log_security_event(
            "INVALID_COLUMN_NAME",
            paste("Invalid continuous variable name:", var),
            session,
            "WARNING"
          )
          stop("Invalid column name format in continuous variables")
        })
      }
    }

    terms <- c()
    if (!is.null(categorical_var) && length(categorical_var) > 0) {
      terms <- c(terms, categorical_var)
    }
    if (!is.null(continuous_var) && length(continuous_var) > 0) {
      for (var in continuous_var) {
        if (var %in% names(out()$meta)) {
          unique_values <- length(unique(out()$meta[[var]]))
          df <- max(3, min(4, unique_values - 1))
          terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
        } else {
          warning(paste("Variable", var, "not found in meta data"))
        }
      }
    }

    if (length(terms) > 0) {
      as.formula(paste("~", paste(terms, collapse = " + ")))
    } else {
      as.formula("~ 1")
    }
  })

  search_notification_shown <- reactiveVal(FALSE)

  # Cache for searchK results to avoid redundant computation
  searchK_cache <- reactiveValues(
    result = NULL,
    params_hash = NULL
  )

  K_search <- eventReactive(input$search, {
    tryCatch({
      TextAnalysisR:::check_rate_limit(session$token, user_requests, max_requests = 100, window_seconds = 3600)
    }, error = function(e) {
      TextAnalysisR:::log_security_event(
        "RATE_LIMIT_EXCEEDED",
        "K search rate limit exceeded",
        session,
        "WARNING"
      )
      TextAnalysisR:::show_error_notification(
        "You have made too many search requests. Please wait before trying again."
      )
      return(NULL)
    })

    dfm_obj <- get_available_dfm()

    if (is.null(dfm_obj)) {
      show_dfm_required_modal()
      return(NULL)
    }

    if (!is.null(input$K_range)) {
      if (input$K_range[1] < 2) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid K Range",
          p("The minimum number of topics (K) must be at least 2."),
          p("Topic modeling requires at least 2 topics to be meaningful."),
          p("Please adjust the range slider to start from 2 or higher."),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return(NULL)
      }
    }

    # Check cache for searchK results
    current_params_hash <- digest::digest(list(
      dfm_hash = stm_conversion_cache$dfm_hash,
      K_range = K_range(),
      init_type = input$init_type_search,
      prevalence = deparse(prevalence_formula_K_search()),
      gamma_prior = input$gamma_prior_search,
      kappa_prior = input$kappa_prior_search,
      max_em_its = input$max_em_its_search
    ), algo = "md5")

    if (!is.null(searchK_cache$result) &&
        !is.null(searchK_cache$params_hash) &&
        searchK_cache$params_hash == current_params_hash) {
      showNotification("Using cached searchK results", type = "message", duration = 2)
      return(searchK_cache$result)
    }

    showNotification(HTML("Searching for optimal K...<br>This may take several minutes."),
                     type = "message", duration = NULL, id = "search_k_notification")

    tryCatch(
      {
        result <- tryCatch({
          if (is.null(prevalence_formula_K_search())) {
            TextAnalysisR::find_optimal_k(
              dfm_object = dfm_obj,
              topic_range = K_range(),
              max.em.its = input$max_em_its_search,
              verbose = TRUE
            )
          } else {
            # Use multiple cores for faster searchK (leave 1 core free)
            n_cores <- max(1, parallel::detectCores() - 1)
            stm::searchK(
              data = out()$meta,
              documents = out()$documents,
              vocab = out()$vocab,
              init.type = input$init_type_search,
              K = K_range(),
              prevalence = prevalence_formula_K_search(),
              verbose = TRUE,
              gamma.prior = input$gamma_prior_search,
              sigma.prior = 0,
              kappa.prior = input$kappa_prior_search,
              max.em.its = input$max_em_its_search,
              emtol = 1e-04,  # Relaxed tolerance for faster convergence
              cores = n_cores,
              control = list(alpha = 1)
            )
          }
        }, error = function(search_error) {
          if (grepl("chol|decomposition|singular", search_error$message, ignore.case = TRUE)) {
            showNotification("Spectral initialization failed. Using LDA initialization instead...", type = "warning")
            n_cores <- max(1, parallel::detectCores() - 1)
            stm::searchK(
              data = out()$meta,
              documents = out()$documents,
              vocab = out()$vocab,
              init.type = "LDA",
              K = K_range(),
              prevalence = prevalence_formula_K_search(),
              verbose = TRUE,
              gamma.prior = input$gamma_prior_search,
              sigma.prior = 0,
              kappa.prior = input$kappa_prior_search,
              max.em.its = input$max_em_its_search,
              emtol = 1e-04,  # Relaxed tolerance for faster convergence
              cores = n_cores,
              control = list(alpha = 1)
            )
          } else {
            stop(search_error)
          }
        })

        remove_notification_by_id("search_k_notification")
        show_completion_notification("Search completed successfully!", duration = 3)
        search_notification_shown(FALSE)

        # Cache the result
        searchK_cache$result <- result
        searchK_cache$params_hash <- current_params_hash

        return(result)
      },
      error = function(e) {
        tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})
        warning_message <- paste(e$message)
        print(warning_message)

        error_title <- "Search Failed"
        error_msg <- if (grepl("chol.*decomposition failed", warning_message, ignore.case = TRUE)) {
          "Topic model fitting failed. Try reducing K range or simplifying covariates."
        } else if (grepl("convergence", warning_message, ignore.case = TRUE)) {
          "Model failed to converge. Try increasing max iterations or reducing K range."
        } else {
          "Topic model error. Check data quality and try simpler parameters."
        }

        shiny::showModal(shiny::modalDialog(
          title = error_title,
          error_msg,
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        NULL
      }
    )
  })

  output$topic_search_message <- renderUI({
    formula_text <- if (is.null(prevalence_formula_K_search())) {
      "No document-level covariates"
    } else {
      lines <- deparse(prevalence_formula_K_search())
      lines_no_indent <- sub("^\\s+", "", lines)
      paste(lines_no_indent, collapse = " ")
    }

    k_range_text <- if (!is.null(input$K_range)) {
      paste0(input$K_range[1], " to ", input$K_range[2])
    } else {
      "Not selected"
    }

    HTML(
      paste0(
        "<div style='background-color: #f0f8ff; border: 1px solid #4682b4; padding: 10px; border-radius: 5px; margin-bottom: 20px;'>",
        "<h5><strong style='color: #2c5aa0;'>Model Information</strong></h5>",
        "<p style='white-space: normal;'>",
        "<b>Prevalence formula = </b> ", formula_text,
        "</p>",
        "<p><b>Topic number (K) range = </b> ", k_range_text, "</p>",
        "</div>"
      )
    )
  })




  output$quality_metrics_plot <- plotly::renderPlotly({
    req(K_search())
    TextAnalysisR::plot_quality_metrics(
      K_search(),
      title = "Diagnostic Plots",
      height = input$height_search_k,
      width = input$width_search_k
    )
  })

  output$quality_metrics_plot_uiOutput <- renderUI({
    height_val <- input$height_search_k %||% 600
    div(
      style = "margin-bottom: 20px; overflow: hidden;",
      plotly::plotlyOutput(
        "quality_metrics_plot",
        height = paste0(height_val, "px"),
        width = "100%"
      )
    )
  })

  output$quality_summary_table <- DT::renderDataTable({
    req(K_search())
    search_result <- K_search()

    summary_data <- search_result$results

    for (col in names(summary_data)) {
      if (is.list(summary_data[[col]])) {
        summary_data[[col]] <- unlist(summary_data[[col]])
      }
    }

    if ("semcoh" %in% names(summary_data)) {
      summary_data$semcoh <- as.numeric(summary_data$semcoh)
    }
    if ("exclus" %in% names(summary_data)) {
      summary_data$exclus <- as.numeric(summary_data$exclus)
    }
    if ("residual" %in% names(summary_data)) {
      summary_data$residual <- as.numeric(summary_data$residual)
    }
    if ("lbound" %in% names(summary_data)) {
      summary_data$lbound <- as.numeric(summary_data$lbound)
    }
    if ("em.its" %in% names(summary_data)) {
      summary_data$em.its <- as.numeric(summary_data$em.its)
    }

    safe_scale <- function(x) {
      if (length(unique(x)) == 1 || all(is.na(x))) {
        return(rep(0, length(x)))
      }
      return(as.numeric(scale(x)))
    }

    if ("heldout" %in% names(summary_data)) {
      summary_data$heldout <- as.numeric(summary_data$heldout)
    }

    if ("semcoh" %in% names(summary_data) && "exclus" %in% names(summary_data)) {
      summary_data <- summary_data %>%
        mutate(
          overall_score = safe_scale(semcoh) + safe_scale(exclus) - safe_scale(residual) +
                         (if("heldout" %in% names(summary_data)) safe_scale(heldout) else 0),
          score_components = paste0(
            "Coh_z:", round(safe_scale(semcoh), 2),
            " Exc_z:", round(safe_scale(exclus), 2),
            " -Res_z:", round(-safe_scale(residual), 2),
            if("heldout" %in% names(summary_data)) paste0(" Held_z:", round(safe_scale(heldout), 2)) else ""
          )
        ) %>%
        rename(
          `Semantic Coherence` = semcoh,
          `Exclusivity` = exclus,
          `Residual` = residual,
          `Held-out Likelihood` = heldout,
          `Lower Bound` = lbound,
          `Convergence Iterations` = em.its,
          `Overall Score` = overall_score
        )
    } else if ("semcoh" %in% names(summary_data)) {
      summary_data <- summary_data %>%
        mutate(
          overall_score = safe_scale(semcoh) - safe_scale(residual) +
                         (if("heldout" %in% names(summary_data)) safe_scale(heldout) else 0)
        ) %>%
        rename(
          `Semantic Coherence` = semcoh,
          `Residual` = residual,
          `Held-out Likelihood` = heldout,
          `Lower Bound` = lbound,
          `Convergence Iterations` = em.its,
          `Overall Score` = overall_score
        )
    } else {
      summary_data <- summary_data %>%
        mutate(
          overall_score = -safe_scale(residual) +
                         (if("heldout" %in% names(summary_data)) safe_scale(heldout) else 0)
        ) %>%
        rename(
          `Residual` = residual,
          `Held-out Likelihood` = heldout,
          `Lower Bound` = lbound,
          `Convergence Iterations` = em.its,
          `Overall Score` = overall_score
        )
    }

    column_order <- c("K", "Semantic Coherence", "Exclusivity", "Residual",
                      "Held-out Likelihood", "Overall Score", "score_components",
                      "Lower Bound", "Convergence Iterations")

    existing_cols <- intersect(column_order, names(summary_data))
    remaining_cols <- setdiff(names(summary_data), existing_cols)
    remaining_cols <- remaining_cols[remaining_cols != "bound"]

    summary_data <- summary_data %>%
      select(all_of(existing_cols), all_of(remaining_cols)) %>%
      mutate(across(where(is.numeric), ~ round(., 4)))

    if ("Overall Score" %in% names(summary_data)) {
      summary_data <- summary_data %>% arrange(desc(`Overall Score`))
    }

    DT::datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "Quality metrics summary for different K values. Higher overall score indicates better model."
    ) %>%
      DT::formatStyle(
        'Overall Score',
        backgroundColor = DT::styleInterval(
          quantile(summary_data$`Overall Score`, probs = c(0.33, 0.67)),
          c('#ffcccc', '#ffffcc', '#ccffcc')
        )
      )
  })

  output$model_comparison_plot <- plotly::renderPlotly({
    req(K_search())
    TextAnalysisR::plot_model_comparison(
      K_search(),
      title = "Model Comparison",
      height = input$height_search_k,
      width = input$width_search_k
    )
  })

  output$model_comparison_plot_uiOutput <- renderUI({
    height_val <- input$height_search_k %||% 600
    div(
      style = "margin-bottom: 20px; overflow: hidden;",
      plotly::plotlyOutput(
        "model_comparison_plot",
        height = paste0(height_val, "px"),
        width = "100%"
      )
    )
  })

  output$show_ai_recommendation <- reactive({
    !is.null(K_search())
  })
  outputOptions(output, "show_ai_recommendation", suspendWhenHidden = FALSE)

  ai_recommendation <- reactiveVal(NULL)

  observeEvent(input$generate_k_recommendation, {
    req(K_search())

    if (file.exists(".env")) {
      dotenv::load_dot_env()
    }

    existing_key <- Sys.getenv("OPENAI_API_KEY")

    shiny::showModal(shiny::modalDialog(
      title = if (nzchar(existing_key)) "Confirm or Update API Key" else "OpenAI API Key Required",
      tagList(
        if (nzchar(existing_key)) {
          shiny::p("An API key is already configured. You can use it or replace it with a new one.")
        },
        tags$div(
          tags$label("API Key:", `for` = "openai_api_key_search"),
          tags$div(
            class = "input-group",
            style = "width: 100%;",
            shiny::passwordInput("openai_api_key_search",
                               label = NULL,
                               value = existing_key,
                               placeholder = "sk-...",
                               width = "100%"),
            tags$span(
              class = "input-group-btn",
              style = "vertical-align: top;",
              tags$button(
                id = "toggle_api_key_search_modal",
                type = "button",
                class = "btn btn-default",
                style = "height: 34px; border-left: none; border-radius: 0 4px 4px 0;",
                onclick = "
                  var input = document.getElementById('openai_api_key_search');
                  var icon = this.querySelector('i');
                  if (input.type === 'password') {
                    input.type = 'text';
                    icon.className = 'fa fa-eye-slash';
                    this.title = 'Hide API key';
                  } else {
                    input.type = 'password';
                    icon.className = 'fa fa-eye';
                    this.title = 'Show API key';
                  }
                ",
                title = "Show API key",
                tags$i(class = "fa fa-eye")
              )
            )
          ),
          tags$small("Tip: Click the eye icon to show/hide your API key",
                     style = "color: #666; display: block; margin-top: 5px;")
        )
      ),
      footer = tagList(
        shiny::actionButton("submit_api_key_search", "Submit", class = "btn-primary"),
        shiny::modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$submit_api_key_search, {
    req(nzchar(input$openai_api_key_search))

    if (!TextAnalysisR:::validate_api_key(input$openai_api_key_search, strict = TRUE)) {
      TextAnalysisR:::log_security_event(
        "INVALID_API_KEY_ATTEMPT",
        "Malformed API key submitted for semantic search",
        session,
        "WARNING"
      )
      TextAnalysisR:::show_error_notification(
        "Invalid API key format. OpenAI keys should start with 'sk-' and be 48+ characters."
      )
      return()
    }

    Sys.setenv(OPENAI_API_KEY = input$openai_api_key_search)
    TextAnalysisR:::log_security_event(
      "API_KEY_UPDATED",
      "Valid API key configured for semantic search",
      session,
      "INFO"
    )
    shiny::removeModal()

    search_result <- K_search()

    metrics_summary <- search_result$results

    for (col in names(metrics_summary)) {
      if (is.list(metrics_summary[[col]])) {
        metrics_summary[[col]] <- unlist(metrics_summary[[col]])
      }
    }

    metrics_summary <- metrics_summary %>%
      mutate(
        K = as.numeric(K),
        coherence = if ("semcoh" %in% names(.)) as.numeric(semcoh) else NA,
        exclusivity = if ("exclus" %in% names(.)) as.numeric(exclus) else NA,
        residual = if ("residual" %in% names(.)) as.numeric(residual) else NA,
        lbound = if ("lbound" %in% names(.)) as.numeric(lbound) else NA,
        heldout = if ("heldout" %in% names(.)) as.numeric(heldout) else NA
      )

    safe_scale <- function(x) {
      if (all(is.na(x)) || length(unique(x[!is.na(x)])) <= 1) {
        return(rep(0, length(x)))
      }
      return(as.numeric(scale(x)))
    }

    metrics_summary <- metrics_summary %>%
      mutate(
        coherence_z = safe_scale(coherence),
        exclusivity_z = safe_scale(exclusivity),
        residual_z = safe_scale(residual),
        heldout_z = safe_scale(heldout),
        lbound_z = safe_scale(lbound),
        overall_score = coherence_z + exclusivity_z - residual_z + heldout_z,
        coherence_exclusivity_product = coherence * exclusivity
      ) %>%
      arrange(desc(overall_score))

    optimal_candidates <- list()

    optimal_candidates$best_overall <- metrics_summary$K[which.max(metrics_summary$overall_score)]

    if (nrow(metrics_summary) > 2) {
      residual_diff <- diff(metrics_summary$residual)
      residual_diff2 <- diff(residual_diff)
      if (length(residual_diff2) > 0) {
        elbow_idx <- which.max(residual_diff2) + 2
        if (elbow_idx <= nrow(metrics_summary)) {
          optimal_candidates$residual_elbow <- metrics_summary$K[elbow_idx]
        }
      }
    }

    if (!all(is.na(metrics_summary$heldout))) {
      optimal_candidates$max_heldout <- metrics_summary$K[which.max(metrics_summary$heldout)]
    }

    if (!all(is.na(metrics_summary$coherence_exclusivity_product))) {
      optimal_candidates$best_balance <- metrics_summary$K[which.max(metrics_summary$coherence_exclusivity_product)]
    }

    metrics_display <- metrics_summary %>%
      select(K, coherence, exclusivity, residual, heldout, overall_score) %>%
      mutate(across(where(is.numeric), ~round(., 4)))

    metrics_text <- capture.output(print(metrics_display))
    metrics_string <- paste(metrics_text, collapse = "\n")

    top_k_values <- metrics_summary %>%
      arrange(desc(overall_score)) %>%
      head(3) %>%
      pull(K)

    analysis_context <- paste(
      "METRICS SUMMARY WITH OVERALL SCORES:\n", metrics_string,
      "\n\nOVERALL SCORE FORMULA:",
      "\nOverall Score = Coherence(z) + Exclusivity(z) - Residual(z) + Heldout(z)",
      "\n- Higher coherence = more interpretable topics",
      "\n- Higher exclusivity = more distinct topics",
      "\n- Lower residual = better model fit",
      "\n- Higher heldout = better generalization",
      "\n\nKEY FINDINGS:",
      "\n1. BEST OVERALL SCORE: K =", optimal_candidates$best_overall,
      " (This is the recommended K based on balanced metrics)",
      if (!is.null(optimal_candidates$residual_elbow))
        paste("\n2. Residual elbow point: K =", optimal_candidates$residual_elbow) else "",
      if (!is.null(optimal_candidates$max_heldout))
        paste("\n3. Maximum held-out likelihood: K =", optimal_candidates$max_heldout) else "",
      if (!is.null(optimal_candidates$best_balance))
        paste("\n4. Best coherence-exclusivity product: K =", optimal_candidates$best_balance) else "",
      "\n\nTop 3 K values by overall score:", paste(top_k_values, collapse = ", "),
      "\n\nBased on this analysis, please provide:",
      "\n1. Your specific K recommendation",
      "\n2. A brief justification based on the metrics",
      "\n3. Any important considerations for the user",
      "\n\nFormat your response as clear paragraphs without section headers."
    )

    shiny::showNotification(HTML("Generating AI recommendation...<br>This may take a few moments."), type = "message", duration = NULL, id = "ai_gen_notification")

    tryCatch({

      response <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))),
        httr::content_type_json(),
        encode = "json",
        body = list(
          model = input$ai_model_search,
          messages = list(
            list(role = "system", content = input$ai_system_search),
            list(role = "user", content = paste(
              input$ai_user_search,
              "\n\n",
              analysis_context
            ))
          ),
          temperature = 0.7,
          max_tokens = 500
        )
      )

      if (httr::status_code(response) == 200) {
        content <- httr::content(response)
        recommendation <- content$choices[[1]]$message$content

        recommended_k <- as.numeric(gsub(".*K\\s*=\\s*(\\d+).*", "\\1",
                                        regmatches(recommendation,
                                                  regexpr("K\\s*=\\s*\\d+", recommendation))))

        ai_recommendation(list(
          text = recommendation,
          recommended_k = if (length(recommended_k) > 0 && !is.na(recommended_k[1])) recommended_k[1] else NA,
          timestamp = Sys.time()
        ))

        tryCatch(removeNotification(id = "ai_gen_notification"), error = function(e) {})
        tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})
        show_completion_notification("AI recommendation generated successfully!", duration = 3)
      } else {
        tryCatch(removeNotification(id = "ai_gen_notification"), error = function(e) {})
        tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})
        show_error_notification("Error generating AI recommendation. Please check API key.")
      }
    }, error = function(e) {
      tryCatch(removeNotification(id = "ai_gen_notification"), error = function(e) {})
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        paste("AI recommendation error:", e$message)
      } else {
        "Error generating AI recommendation. Please check your API key and try again."
      }
      shiny::showNotification(error_msg, type = "error", duration = 5)
    })
  })


  output$ai_recommendation_output <- renderUI({
    req(ai_recommendation())
    rec <- ai_recommendation()

    process_markdown <- function(text) {
      lines <- strsplit(text, "\n")[[1]]
      lines <- lines[!grepl("^#{1,6}\\s+", lines)]
      text <- paste(lines, collapse = "\n")

      text <- gsub("### Trade-offs to Consider:?", "", text, ignore.case = TRUE)
      text <- gsub("## Trade-offs:?", "", text, ignore.case = TRUE)
      text <- gsub("Trade-offs to Consider:?", "", text, ignore.case = TRUE)

      text <- gsub("\\\\\\(\\s*", "", text)
      text <- gsub("\\s*\\\\\\)", "", text)
      text <- gsub("$$", "", text)
      text <- gsub("$", "", text)
      text <- gsub("K\\s*=\\s*(\\d+)", "<strong>K = \\1</strong>", text)
      text <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", text)
      text <- gsub("__(.+?)__", "<strong>\\1</strong>", text)
      text <- gsub("(?<!\\*)\\*([^\\*]+?)\\*(?!\\*)", "<em>\\1</em>", text, perl = TRUE)
      text <- gsub("(?<!_)_([^_]+?)_(?!_)", "<em>\\1</em>", text, perl = TRUE)
      text <- gsub("(\\d+)\\. ", "<br>\\1. ", text)

      paragraphs <- strsplit(text, "\n\n+")[[1]]
      paragraphs <- paragraphs[nchar(trimws(paragraphs)) > 0]
      paragraphs <- paste0("<p style='margin-bottom: 15px;'>", paragraphs, "</p>")
      text <- paste(paragraphs, collapse = "")

      text <- gsub("\n", "<br>", text)

      text <- gsub("^\\s+|\\s+$", "", text)

      return(HTML(text))
    }

    wellPanel(
      style = "background-color: #e8f4f8; border: 1px solid #0c1f4a;",
      h4("AI Recommendation", style = "color: #0c1f4a;"),
      if (!is.na(rec$recommended_k)) {
        tags$div(
          tags$h5(paste("Recommended K:", rec$recommended_k),
                 style = "color: #28a745; font-weight: bold;")
        )
      },
      tags$div(
        style = "font-family: 'Roboto', sans-serif; line-height: 1.6; font-size: 16px;",
        process_markdown(rec$text)
      ),
      tags$hr(),
      tags$small(paste("Generated at:", format(rec$timestamp, "%Y-%m-%d %H:%M:%S")))
    )
  })

  output$ai_recommendation_table <- DT::renderDataTable({
    req(K_search(), ai_recommendation())
    search_result <- K_search()
    rec <- ai_recommendation()

    if (!is.na(rec$recommended_k)) {
      summary_data <- search_result$results

      for (col in names(summary_data)) {
        if (is.list(summary_data[[col]])) {
          summary_data[[col]] <- unlist(summary_data[[col]])
        }
      }

      if ("semcoh" %in% names(summary_data)) {
        summary_data$semcoh <- as.numeric(summary_data$semcoh)
      }
      if ("exclus" %in% names(summary_data)) {
        summary_data$exclus <- as.numeric(summary_data$exclus)
      }
      if ("residual" %in% names(summary_data)) {
        summary_data$residual <- as.numeric(summary_data$residual)
      }
      if ("lbound" %in% names(summary_data)) {
        summary_data$lbound <- as.numeric(summary_data$lbound)
      }

      summary_data <- summary_data %>%
        mutate(
          Recommendation = ifelse(K == rec$recommended_k, "Recommended", ""),
          coherence = if ("semcoh" %in% names(.)) round(semcoh, 4) else NA,
          exclusivity = if ("exclus" %in% names(.)) round(exclus, 4) else NA,
          residual = if ("residual" %in% names(.)) round(residual, 4) else NA,
          lbound = if ("lbound" %in% names(.)) round(lbound, 4) else NA
        ) %>%
        select(K, Recommendation, any_of(c("coherence", "exclusivity", "residual", "lbound"))) %>%
        rename_with(~ case_when(
          . == "coherence" ~ "Coherence",
          . == "exclusivity" ~ "Exclusivity",
          . == "residual" ~ "Residual",
          . == "lbound" ~ "Lower Bound",
          TRUE ~ .
        ))

      DT::datatable(
        summary_data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = 'rtip'
        )
      ) %>%
        DT::formatStyle(
          'Recommendation',
          color = DT::styleEqual('Recommended', 'green'),
          fontWeight = DT::styleEqual('Recommended', 'bold')
        )
    } else {
      NULL
    }
  })

  ################################################################################
  # TOPIC MODELING TAB
  ################################################################################

  output$K_number_uiOutput <- renderUI({
    sliderInput(
      "K_number",
      "Choose K (number of topics).",
      value = 10,
      min = 0,
      max = 50
    )
  })

  K_number <- eventReactive(eventExpr = input$K_number, {
    input$K_number
  })

  # Hybrid model K number UI output
  output$hybrid_K_number_uiOutput <- renderUI({
    sliderInput(
      "hybrid_K_number",
      "Choose K (number of topics).",
      value = 10,
      min = 0,
      max = 50
    )
  })

  hybrid_K_number <- eventReactive(input$hybrid_K_number, {
    input$hybrid_K_number
  })

  observe({
    req(colnames_cat())
    updateSelectizeInput(session,
                         "categorical_var_2",
                         choices = colnames_cat(),
                         server = TRUE
    )
  })

  observe({
    updateSelectInput(session,
                      "continuous_var_2",
                      choices = colnames_con(),
                      selected = " "
    )
  })

  stm_K_number <- reactiveVal(NULL)
  previous_K_number <- reactiveVal(NULL)
  previous_categorical_var_2 <- reactiveVal(NULL)
  previous_continuous_var_2 <- reactiveVal(NULL)
  previous_slider_values <- reactiveVal(list(frex = 5, lift = 5, score = 5, beta = 5))
  prevalence_formula <- reactiveVal(NULL)
  output_message <- reactiveVal(NULL)

  prevalence_formula_K_n <- reactive({
    categorical_var <- if (!is.null(input$categorical_var_2)) {
      trimws(unlist(strsplit(as.character(input$categorical_var_2), ",")))
    } else {
      NULL
    }
    continuous_var <- if (!is.null(input$continuous_var_2)) {
      trimws(unlist(strsplit(as.character(input$continuous_var_2), ",")))
    } else {
      NULL
    }

    if (!is.null(categorical_var) && length(categorical_var) > 0) {
      for (var in categorical_var) {
        tryCatch({
          TextAnalysisR:::validate_column_name(var)
        }, error = function(e) {
          TextAnalysisR:::log_security_event(
            "INVALID_COLUMN_NAME",
            paste("Invalid categorical variable name:", var),
            session,
            "WARNING"
          )
          stop("Invalid column name format in categorical variables")
        })
      }
    }
    if (!is.null(continuous_var) && length(continuous_var) > 0) {
      for (var in continuous_var) {
        tryCatch({
          TextAnalysisR:::validate_column_name(var)
        }, error = function(e) {
          TextAnalysisR:::log_security_event(
            "INVALID_COLUMN_NAME",
            paste("Invalid continuous variable name:", var),
            session,
            "WARNING"
          )
          stop("Invalid column name format in continuous variables")
        })
      }
    }

    terms <- c()
    if (!is.null(categorical_var) && length(categorical_var) > 0) {
      terms <- c(terms, categorical_var)
    }
    if (!is.null(continuous_var) && length(continuous_var) > 0) {
      for (var in continuous_var) {
        if (var %in% names(out()$meta)) {
          unique_values <- length(unique(out()$meta[[var]]))
          df <- max(3, min(4, unique_values - 1))
          terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
        } else {
          warning(paste("Variable", var, "not found in meta data"))
        }
      }
    }

    if (length(terms) > 0) {
      as.formula(paste("~", paste(terms, collapse = " + ")))
    } else {
      as.formula("~ 1")
    }
  })

  output$topic_term_message <- renderUI({
    formula_text <- if (is.null(prevalence_formula_K_n())) {
      "No document-level covariates"
    } else {
      lines <- deparse(prevalence_formula_K_n())
      lines_no_indent <- sub("^\\s+", "", lines)
      paste(lines_no_indent, collapse = " ")
    }

    HTML(
      paste0(
        "<div style='background-color: #f0f8ff; border: 1px solid #4682b4; padding: 10px; border-radius: 5px;'>",
        "<h5><strong style='color: #2c5aa0;'>Model Information</strong></h5>",
        "<p style='white-space: normal;'>",
        "<b>Prevalence formula = </b> ", formula_text,
        "</p>",
        "<p><b>Topic number (K) = </b> ", input$K_number, "</p>",
        "</div>"
      )
    )
  })

  observeEvent(input$run, {
    dfm_obj <- get_available_dfm()

    if (is.null(dfm_obj)) {
      shiny::showModal(shiny::modalDialog(
        title = "Preprocessing Required",
        p("No document-feature matrix (DFM) found."),
        p("Please complete the required preprocessing steps:"),
        tags$div(
          style = "margin-left: 20px; margin-top: 10px;",
          tags$p(
            tags$strong(style = "color: #DC2626;", "Required:"),
            style = "margin-bottom: 5px;"
          ),
          tags$ul(
            tags$li(tags$strong("Step 1:"), " Unite Texts"),
            tags$li(tags$strong("Step 4:"), " Document-Feature Matrix (DFM)")
          ),
          tags$p(
            tags$strong(style = "color: #6B7280;", "Optional:"),
            " Steps 2, 3, 5, and 6",
            style = "margin-top: 10px; font-size: 12px;"
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    n_docs <- quanteda::ndoc(get_available_dfm())
    if (n_docs < 20) {
      shiny::showModal(shiny::modalDialog(
        title = tags$div(
          icon("exclamation-triangle"),
          " Insufficient Documents for Topic Modeling",
          style = "color: #DC2626;"
        ),
        tags$div(
          tags$p(
            paste("Topic modeling requires at least 20 documents for reliable results. Currently:", n_docs, "documents."),
            style = "margin-bottom: 10px;"
          ),
          tags$p(
            tags$strong("Recommendations:"),
            style = "margin-bottom: 5px; color: #0c1f4a;"
          ),
          tags$ul(
            tags$li("Upload a larger dataset with more documents"),
            tags$li("Topic modeling quality improves significantly with 50+ documents"),
            tags$li("For best results, use 100+ documents")
          )
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    showNotification(HTML(paste("Fitting STM model with K =", input$K_number, "topics...<br>This may take several minutes.")),
                     type = "message", duration = NULL, id = "stm_model_notification")

    stm_model_trigger(isolate(stm_model_trigger()) + 1)

    req(input$K_number)

    categorical_var <- if (!is.null(input$categorical_var_2)) {
      unlist(strsplit(as.character(input$categorical_var_2), ",\\s*"))
    } else {
      NULL
    }

    continuous_var <- if (!is.null(input$continuous_var_2)) {
      unlist(strsplit(as.character(input$continuous_var_2), ",\\s*"))
    } else {
      NULL
    }

    terms <- c()
    if (!is.null(categorical_var) && length(categorical_var) > 0) {
      terms <- c(terms, categorical_var)
    }
    if (!is.null(continuous_var) && length(continuous_var) > 0) {
      for (var in continuous_var) {
        unique_values <- length(unique(out()$meta[[var]]))
        df <- max(3, min(4, unique_values - 1))
        terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
      }
    }

    prevalence_formula(if (length(terms) > 0) {
      as.formula(paste("~", paste(terms, collapse = " + ")))
    } else {
      NULL
    })

    print(paste("Computing STM model with K =", input$K_number))

    K_num <- as.numeric(input$K_number)
    if (is.na(K_num)) {
      stop("K_number must be a numeric value")
    }

    if (is.null(out()$documents) || length(out()$documents) == 0) {
      stop("No documents found. Please check preprocessing steps.")
    }

    if (is.null(out()$vocab) || length(out()$vocab) == 0) {
      stop("No vocabulary found. Please check preprocessing steps.")
    }

    if (!is.list(out()$documents)) {
      stop("Documents must be in STM format. Please check the data conversion.")
    }

    doc_lengths <- sapply(out()$documents, function(d) {
      if (is.null(d) || length(d) == 0) return(0)
      if (is.matrix(d) && ncol(d) >= 2) return(sum(d[2,]))
      return(0)
    })

    if (all(doc_lengths == 0)) {
      stop("All documents are empty after preprocessing. Please check text preprocessing steps.")
    }

    tryCatch(
      {
        init_type_to_use <- input$init_type_K

        stm_result <- tryCatch({
          stm::stm(
            data = out()$meta,
            documents = out()$documents,
            vocab = out()$vocab,
            init.type = init_type_to_use,
            K = K_num,
            prevalence = prevalence_formula_K_n(),
            verbose = TRUE,
            gamma.prior = input$gamma_prior_K,
            sigma.prior = 0,
            kappa.prior = input$kappa_prior_K,
            max.em.its = input$max_em_its_K,
            emtol = 1e-04,
            control = list(alpha = 50/K_num)
          )
        }, error = function(init_error) {
          if (grepl("chol|decomposition|singular", init_error$message, ignore.case = TRUE)) {
            showNotification("Spectral initialization failed. Trying LDA initialization...", type = "warning")
            stm::stm(
              data = out()$meta,
              documents = out()$documents,
              vocab = out()$vocab,
              init.type = "LDA",
              K = K_num,
              prevalence = prevalence_formula_K_n(),
              verbose = TRUE,
              gamma.prior = input$gamma_prior_K,
              sigma.prior = 0,
              kappa.prior = input$kappa_prior_K,
              max.em.its = input$max_em_its_K,
              emtol = 1e-04,
              control = list(alpha = 50/K_num)
            )
          } else {
            stop(init_error)
          }
        })

        stm_K_number(stm_result)
        generated_labels(NULL)

        previous_K_number(input$K_number)
        previous_categorical_var_2(input$categorical_var_2)
        previous_continuous_var_2(input$continuous_var_2)
        previous_topic_measure(input$topic_measure)

        current_slider_values <- list(
          frex = input$top_term_number_frex %||% 5,
          lift = input$top_term_number_lift %||% 5,
          score = input$top_term_number_score %||% 5,
          beta = input$top_term_number_beta %||% 5
        )
        previous_slider_values(current_slider_values)

        output$output_message <- renderUI({
          HTML(paste0(
            "<div style='background-color: #f0f8ff; border: 1px solid #4682b4; padding: 10px; border-radius: 5px;'>",
            "<h5><strong style='color: #2c5aa0;'>Model Information</strong></h5>",
            "<p><b>Prevalence formula:</b> ",
            ifelse(is.null(prevalence_formula()), "No document-level covariates", deparse(prevalence_formula())),
            "</p>",
            "<p><b>Computing STM model with K =</b> ", input$K_number, "</p>",
            "</div>"
          ))
        })

        tryCatch(removeNotification(id = "stm_model_notification"), error = function(e) {})
        show_completion_notification("STM model completed successfully!", duration = 3)
      },
      error = function(e) {
        tryCatch(removeNotification(id = "stm_model_notification"), error = function(e) {})
        warning_message <- if (!is.null(e$message) && nchar(e$message) > 0) {
          e$message
        } else {
          "An error occurred while running the STM model. Please check your data and parameters."
        }
        print(paste("STM model error:", warning_message))
        shiny::showModal(shiny::modalDialog(
          title = tags$div(
            style = "color: #dc2626;",
            icon("times-circle"),
            " Error Running STM Model"
          ),
          p(warning_message),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        NULL
      }
    )
  })

  observeEvent(input$run_embedding, {
    if (!TextAnalysisR::require_feature("embeddings", session)) {
      return()
    }

    if (is.null(united_tbl())) {
      shiny::showModal(shiny::modalDialog(
        title = "Preprocessing Required",
        p("United text data not found."),
        p("Please complete Step 1 (Unite Texts) in the Preprocess tab."),
        tags$p(
          tags$strong("Note:"), " Embedding-based topic modeling only requires Step 1 (Unite Texts).",
          style = "margin-top: 10px; font-size: 12px; color: #6B7280;"
        ),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    show_loading_notification(HTML(paste("Running embedding-based topic modeling with", input$embedding_n_topics, "topics...<br>This may take several minutes.")),
                              id = "embedding_model_notification")

    tryCatch({
      texts <- united_tbl()$united_texts

      if (input$embedding_method == "umap_hdbscan") {
        embedding_result <- TextAnalysisR::fit_embedding_topics(
          texts = texts,
          method = input$embedding_method,
          embedding_model = input$embedding_model_name,
          umap_neighbors = input$umap_neighbors,
          umap_min_dist = input$umap_min_dist,
          min_topic_size = input$min_topic_size,
          reduce_outliers = input$reduce_outliers,
          diversity = input$topic_diversity,
          representation_method = input$topic_representation,
          seed = 123,
          verbose = TRUE
        )
      } else {
        embedding_result <- TextAnalysisR::fit_embedding_topics(
          texts = texts,
          method = input$embedding_method,
          n_topics = input$embedding_n_topics,
          embedding_model = input$embedding_model_name,
          representation_method = input$topic_representation,
          seed = 123,
          verbose = TRUE
        )
      }

      stm_K_number(embedding_result)

      tryCatch(removeNotification(id = "embedding_model_notification"), error = function(e) {})
      show_completion_notification("Embedding-based model completed successfully!", duration = 3)

    }, error = function(e) {
      tryCatch(removeNotification(id = "embedding_model_notification"), error = function(e) {})
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        e$message
      } else {
        "An error occurred while running the embedding model. Please check your data and parameters."
      }
      shiny::showModal(shiny::modalDialog(
        title = tags$div(
          style = "color: #dc2626;",
          icon("times-circle"),
          " Error Running Embedding Model"
        ),
        p(error_msg),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
  })

  hybrid_K_search <- eventReactive(input$search_hybrid, {
    tryCatch({
      TextAnalysisR:::check_rate_limit(session$token, user_requests, max_requests = 100, window_seconds = 3600)
    }, error = function(e) {
      TextAnalysisR:::log_security_event(
        "RATE_LIMIT_EXCEEDED",
        "Hybrid search rate limit exceeded",
        session,
        "WARNING"
      )
      TextAnalysisR:::show_error_notification(
        "You have made too many search requests. Please wait before trying again."
      )
      return(NULL)
    })

    dfm_obj <- get_available_dfm()

    if (is.null(dfm_obj)) {
      show_dfm_required_modal()
      return(NULL)
    }

    if (!is.null(input$hybrid_K_range)) {
      if (input$hybrid_K_range[1] < 2) {
        shiny::showModal(shiny::modalDialog(
          title = "Invalid K Range",
          p("The minimum number of topics (K) must be at least 2."),
          p("Please adjust the range slider to start from 2 or higher."),
          easyClose = TRUE,
          footer = shiny::modalButton("OK")
        ))
        return(NULL)
      }
    }

    showNotification(HTML("Searching for optimal K for hybrid model...<br>This may take several minutes."),
                     type = "message", duration = NULL, id = "hybrid_search_k_notification")

    tryCatch({
      categorical_var <- if (!is.null(input$hybrid_categorical_var)) unlist(strsplit(as.character(input$hybrid_categorical_var), ",\\s*")) else NULL
      continuous_var <- if (!is.null(input$hybrid_continuous_var)) unlist(strsplit(as.character(input$hybrid_continuous_var), ",\\s*")) else NULL

      terms <- c()
      if (!is.null(categorical_var) && length(categorical_var) > 0) {
        terms <- c(terms, categorical_var)
      }
      if (!is.null(continuous_var) && length(continuous_var) > 0) {
        for (var in continuous_var) {
          unique_values <- length(unique(out()$meta[[var]]))
          df <- max(3, min(4, unique_values - 1))
          terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
        }
      }

      prevalence_formula <- if (length(terms) > 0) {
        as.formula(paste("~", paste(terms, collapse = " + ")))
      } else {
        NULL
      }

      result <- tryCatch({
        if (is.null(prevalence_formula)) {
          TextAnalysisR::find_optimal_k(
            dfm_object = dfm_obj,
            topic_range = input$hybrid_K_range,
            max.em.its = input$hybrid_max_em_its,
            verbose = TRUE
          )
        } else {
          stm::searchK(
            data = out()$meta,
            documents = out()$documents,
            vocab = out()$vocab,
            init.type = input$hybrid_init_type,
            K = input$hybrid_K_range,
            prevalence = prevalence_formula,
            verbose = TRUE,
            gamma.prior = input$hybrid_gamma_prior,
            sigma.prior = 0,
            kappa.prior = input$hybrid_kappa_prior,
            max.em.its = input$hybrid_max_em_its,
            control = list(alpha = 1)
          )
        }
      }, error = function(search_error) {
        if (grepl("chol|decomposition|singular", search_error$message, ignore.case = TRUE)) {
          showNotification("Spectral initialization failed. Using LDA initialization...", type = "warning")
          stm::searchK(
            data = out()$meta,
            documents = out()$documents,
            vocab = out()$vocab,
            init.type = "LDA",
            K = input$hybrid_K_range,
            prevalence = prevalence_formula,
            verbose = TRUE,
            gamma.prior = input$hybrid_gamma_prior,
            sigma.prior = 0,
            kappa.prior = input$hybrid_kappa_prior,
            max.em.its = input$hybrid_max_em_its,
            control = list(alpha = 1)
          )
        } else {
          stop(search_error)
        }
      })

      removeNotification(id = "hybrid_search_k_notification")
      show_completion_notification("Hybrid search K completed successfully!", duration = 3)
      return(result)
    }, error = function(e) {
      tryCatch(removeNotification(id = "hybrid_search_k_notification"), error = function(e) {})
      error_msg <- paste("Error in hybrid search K:", e$message)
      shiny::showModal(shiny::modalDialog(
        title = "Hybrid Search K Failed",
        p(error_msg),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
      return(NULL)
    })
  })

  output$hybrid_K_number_uiOutput <- renderUI({
    search_result <- hybrid_K_search()
    if (!is.null(search_result) && !is.null(search_result$results)) {
      K_values <- search_result$results$K
      selectInput("hybrid_K_number",
                  "Select K (number of topics) based on search results:",
                  choices = K_values,
                  selected = K_values[length(K_values)])
    }
  })

  observeEvent(input$hybrid_display, {
    req(input$hybrid_K_number)
    dfm_obj <- get_available_dfm()

    if (is.null(united_tbl()) || is.null(dfm_obj)) {
      shiny::showModal(shiny::modalDialog(
        title = "Preprocessing Required",
        p("No document-feature matrix (DFM) found."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    show_loading_notification(HTML(paste("Running hybrid topic modeling with K =", input$hybrid_K_number, "topics...<br>This may take several minutes.")),
                              id = "hybrid_model_notification")

    tryCatch({
      texts <- united_tbl()$united_texts

      metadata <- NULL
      if (!is.null(input$hybrid_categorical_var_2) || !is.null(input$hybrid_continuous_var_2)) {
        metadata <- out()$meta
        selected_cols <- c(input$hybrid_categorical_var_2, input$hybrid_continuous_var_2)
        if (length(selected_cols) > 0) {
          metadata <- metadata[, selected_cols, drop = FALSE]
        }
      }

      prevalence_formula <- NULL
      if (!is.null(metadata) && ncol(metadata) > 0) {
        covariate_names <- names(metadata)
        for (var in covariate_names) {
          tryCatch({
            TextAnalysisR:::validate_column_name(var)
          }, error = function(e) {
            TextAnalysisR:::log_security_event(
              "INVALID_COLUMN_NAME",
              paste("Invalid metadata column name:", var),
              session,
              "WARNING"
            )
            stop("Invalid column name format in metadata")
          })
        }
        prevalence_formula <- as.formula(paste("~", paste(covariate_names, collapse = " + ")))
      }

      hybrid_result <- TextAnalysisR::fit_hybrid_model(
        texts = texts,
        metadata = metadata,
        n_topics_stm = as.numeric(input$hybrid_K_number),
        embedding_model = input$hybrid_embedding_model,
        stm_prevalence = prevalence_formula,
        stm_init_type = input$hybrid_init_type_K %||% "Spectral",
        alignment_method = "cosine",
        verbose = TRUE,
        seed = 123
      )

      stm_K_number(hybrid_result$stm_result$model)

      attr(stm_K_number(), "hybrid_result") <- hybrid_result

      tryCatch(removeNotification(id = "hybrid_model_notification"), error = function(e) {})
      show_completion_notification("Hybrid model completed successfully!", duration = 3)

      output$output_message <- renderUI({
        HTML(paste0(
          "<div style='background-color: #f0f8ff; border: 1px solid #4682b4; padding: 10px; border-radius: 5px;'>",
          "<h5><strong style='color: #2c5aa0;'>Hybrid Model Information</strong></h5>",
          "<p><b>STM Topics:</b> ", input$hybrid_K_number, "</p>",
          "<p><b>Embedding Model:</b> ", input$hybrid_embedding_model %||% "all-MiniLM-L6-v2", "</p>",
          "<p><b>Alignment Score:</b> ", round(hybrid_result$alignment$score, 3), "</p>",
          "<p><b>Method:</b> STM Hybrid</p>",
          "</div>"
        ))
      })

    }, error = function(e) {
      tryCatch(removeNotification(id = "hybrid_model_notification"), error = function(e) {})
      error_msg <- if (!is.null(e$message) && nchar(e$message) > 0) {
        e$message
      } else {
        "An error occurred while running the hybrid model. Please check your data and parameters."
      }
      shiny::showModal(shiny::modalDialog(
        title = "Error Running Hybrid Model",
        p(error_msg),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })
  })

  observeEvent(input$display_embedding_topics, {
    if (is.null(stm_K_number())) {
      showNotification("Please run the embedding model first.", type = "warning", duration = 3)
      return()
    }

    if ("topic_assignments" %in% names(stm_K_number())) {
      output$topic_prevalence_plot_uiOutput <- renderUI({
        tagList(
          div(
            style = "margin-bottom: 20px; overflow: hidden;",
            plotly::plotlyOutput("embedding_topic_plot", height = 500, width = "100%")
          ),
          br(),
          h4("Clustering Quality Metrics"),
          tableOutput("embedding_quality_metrics")
        )
      })


      output$embedding_topics_info <- renderUI({
        NULL
      })

      output$embedding_topic_plot <- plotly::renderPlotly({
        model <- stm_K_number()
        viz_type <- input$embedding_viz_type

        if (viz_type == "distribution") {
          topic_counts <- table(model$topic_assignments)
          topic_props <- prop.table(topic_counts)

          n_outliers <- sum(model$topic_assignments == -1)
          outlier_pct <- round(100 * n_outliers / length(model$topic_assignments), 1)

          plotly::plot_ly(
            x = names(topic_props),
            y = as.numeric(topic_props),
            type = "bar",
            marker = list(color = ifelse(names(topic_props) == "-1", "#EF4444", "#337ab7")),
            text = paste("Documents:", topic_counts),
            hovertemplate = "Topic %{x}<br>Proportion: %{y:.3f}<br>%{text}<extra></extra>"
          ) %>%
            plotly::layout(
              title = list(
                text = "Topic Distribution",
                font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
                x = 0.5,
                xref = "paper",
                xanchor = "center"
              ),
              xaxis = list(
                title = list(text = "Topic/Cluster (-1 = outliers)"),
                tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
              ),
              yaxis = list(
                title = list(text = "Proportion"),
                tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
              ),
              margin = list(t = 60, b = 60, l = 80, r = 40),
              hoverlabel = list(
                font = list(size = 16, family = "Roboto, sans-serif"),
                align = "left"
              )
            )

        } else if (viz_type == "documents" && "reduced_embeddings" %in% names(model)) {
          df <- data.frame(
            x = model$reduced_embeddings[, 1],
            y = model$reduced_embeddings[, 2],
            topic = as.factor(model$topic_assignments),
            text = paste("Topic:", model$topic_assignments)
          )

          plotly::plot_ly(df, x = ~x, y = ~y, color = ~topic,
                         type = "scatter", mode = "markers",
                         marker = list(size = 6, opacity = 0.7),
                         text = ~text,
                         hovertemplate = "%{text}<extra></extra>") %>%
            plotly::layout(
              title = list(
                text = "Document Embeddings by Topic",
                font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
                x = 0.5,
                xref = "paper",
                xanchor = "center"
              ),
              xaxis = list(
                title = list(text = "UMAP 1"),
                tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
              ),
              yaxis = list(
                title = list(text = "UMAP 2"),
                tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
              ),
              showlegend = TRUE,
              legend = list(
                title = list(text = "Topic", font = list(size = 16, color = "#0c1f4a", family = "Roboto, sans-serif")),
                font = list(size = 16, color = "#3B3B3B", family = "Roboto, sans-serif")
              ),
              margin = list(t = 60, b = 60, l = 80, r = 40),
              hoverlabel = list(
                font = list(size = 16, family = "Roboto, sans-serif"),
                align = "left"
              )
            )

        } else if (viz_type == "heatmap" && "embeddings" %in% names(model)) {
          topic_ids <- unique(model$topic_assignments[model$topic_assignments > 0])

          if (length(topic_ids) > 1) {
            topic_embeddings <- sapply(topic_ids, function(tid) {
              topic_docs <- which(model$topic_assignments == tid)
              colMeans(model$embeddings[topic_docs, , drop = FALSE])
            })

            similarity_matrix <- cor(topic_embeddings)

            plotly::plot_ly(
              z = similarity_matrix,
              x = paste("Topic", topic_ids),
              y = paste("Topic", topic_ids),
              type = "heatmap",
              colorscale = "Viridis",
              zmin = -1, zmax = 1,
              hovertemplate = "X: %{x}<br>Y: %{y}<br>Similarity: %{z:.3f}<extra></extra>"
            ) %>%
              plotly::layout(
                title = list(
                  text = "Topic Similarity Heatmap",
                  font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
                  x = 0.5,
                  xref = "paper",
                  xanchor = "center"
                ),
                xaxis = list(
                  title = list(text = ""),
                  tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
                ),
                yaxis = list(
                  title = list(text = ""),
                  tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif")
                ),
                margin = list(t = 60, b = 60, l = 100, r = 40),
                hoverlabel = list(
                  font = list(size = 16, family = "Roboto, sans-serif"),
                  align = "left"
                )
              )
          } else {
            TextAnalysisR::create_empty_plot_message(
              "Need at least 2 topics for similarity heatmap"
            )
          }

        } else if (viz_type == "distance") {
          if ("reduced_embeddings" %in% names(model)) {
            topic_ids <- unique(model$topic_assignments[model$topic_assignments > 0])

            topic_positions <- t(sapply(topic_ids, function(tid) {
              topic_docs <- which(model$topic_assignments == tid)
              colMeans(model$reduced_embeddings[topic_docs, 1:2, drop = FALSE])
            }))

            topic_sizes <- sapply(topic_ids, function(tid) {
              sum(model$topic_assignments == tid)
            })

            df <- data.frame(
              x = topic_positions[, 1],
              y = topic_positions[, 2],
              topic = paste("Topic", topic_ids),
              size = sqrt(topic_sizes) * 5,
              count = topic_sizes
            )

            plotly::plot_ly(df, x = ~x, y = ~y,
                           type = "scatter", mode = "markers+text",
                           marker = list(size = ~size, opacity = 0.6, color = "#337ab7"),
                           text = ~topic,
                           hovertext = ~paste("Documents:", count),
                           textposition = "middle center") %>%
              plotly::layout(
                title = list(
                  text = "Intertopic Distance Map",
                  font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
                  x = 0.5,
                  xref = "paper",
                  xanchor = "center"
                ),
                xaxis = list(
                  title = list(text = "PC1"),
                  tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                  titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
                ),
                yaxis = list(
                  title = list(text = "PC2"),
                  tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
                  titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
                ),
                showlegend = FALSE,
                margin = list(t = 60, b = 60, l = 80, r = 40),
                hoverlabel = list(
                  font = list(size = 16, family = "Roboto, sans-serif"),
                  align = "left"
                )
              )
          }
        }
      })

      output$embedding_quality_metrics <- renderTable({
        model <- stm_K_number()
        n_topics <- length(unique(model$topic_assignments[model$topic_assignments > 0]))
        n_outliers <- sum(model$topic_assignments == -1)

        metrics_df <- data.frame(
          Metric = c("Topics Discovered", "Total Documents", "Outlier Documents", "Outlier Percentage",
                     "Avg Documents/Topic", "Min Topic Size", "Max Topic Size"),
          Value = c(
            n_topics,
            length(model$topic_assignments),
            n_outliers,
            paste0(round(100 * n_outliers / length(model$topic_assignments), 1), "%"),
            round(mean(table(model$topic_assignments[model$topic_assignments > 0])), 1),
            min(table(model$topic_assignments[model$topic_assignments > 0])),
            max(table(model$topic_assignments[model$topic_assignments > 0]))
          )
        )
        metrics_df
      })

      output$topic_prevalence_table_uiOutput <- renderUI({
        DT::dataTableOutput("embedding_topic_table", width = "100%")
      })

      output$embedding_topic_table <- DT::renderDataTable({
        model <- stm_K_number()
        if ("topic_keywords" %in% names(model)) {
          topic_counts <- table(model$topic_assignments[model$topic_assignments > 0])

          topic_df <- data.frame(
            Topic = names(model$topic_keywords),
            `Top Keywords (c-TF-IDF)` = sapply(model$topic_keywords, function(x) paste(head(x, 7), collapse = ", ")),
            `Document Count` = as.numeric(topic_counts[names(model$topic_keywords)]),
            stringsAsFactors = FALSE
          )

          if ("topic_diversity" %in% names(model)) {
            topic_df$Diversity <- round(model$topic_diversity, 3)
          }

          DT::datatable(topic_df, options = list(pageLength = 15))
        }
      })
    }
  })

  observeEvent(input$embedding_quote, {
    if (is.null(stm_K_number())) {
      showNotification("Please run the embedding model first.", type = "warning", duration = 3)
      return()
    }

    selected_topic <- as.numeric(input$embedding_quote_topic)

    if (is.na(selected_topic)) {
      showNotification("Please select a topic first.", type = "warning")
      return()
    }

    model <- stm_K_number()
    texts <- united_tbl()$united_texts

    topic_docs <- which(model$topic_assignments == selected_topic)

    if (length(topic_docs) > 0) {
      if ("reduced_embeddings" %in% names(model)) {
        cluster_embeddings <- model$reduced_embeddings[topic_docs, , drop = FALSE]
        centroid <- colMeans(cluster_embeddings)

        distances <- apply(cluster_embeddings, 1, function(x) {
          sqrt(sum((x - centroid)^2))
        })

        closest_indices <- topic_docs[order(distances)[1:min(5, length(topic_docs))]]
      } else {
        closest_indices <- head(topic_docs, 5)
      }

      example_texts <- texts[closest_indices]

      output$quote_table <- DT::renderDataTable({
        data.frame(
          `Document ID` = closest_indices,
          `Example Text` = substr(example_texts, 1, 500),
          stringsAsFactors = FALSE
        )
      }, options = list(pageLength = 5))
    } else {
      showNotification("No documents found for this topic.", type = "warning")
    }
  })

  observeEvent(input$display_hybrid_topics, {
    if (is.null(stm_K_number()) || is.null(attr(stm_K_number(), "hybrid_result"))) {
      showNotification("Please run the hybrid model first.", type = "warning", duration = 3)
      return()
    }

    hybrid_result <- attr(stm_K_number(), "hybrid_result")

    output$topic_prevalence_plot_uiOutput <- renderUI({
      div(
        style = "margin-bottom: 20px; overflow: hidden;",
        plotly::plotlyOutput("hybrid_topic_plot", height = 500, width = "100%")
      )
    })

    output$hybrid_topic_plot <- plotly::renderPlotly({
      theta <- hybrid_result$stm_model$theta
      topic_props <- colMeans(theta)

      plotly::plot_ly(
        x = paste0("Topic ", 1:length(topic_props)),
        y = topic_props,
        type = "bar",
        marker = list(color = "#337ab7"),
        hovertemplate = "Topic %{x}<br>Proportion: %{y:.3f}<extra></extra>"
      ) %>%
        plotly::layout(
          title = list(
            text = "Topic Prevalence",
            font = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif"),
            x = 0.5,
            xref = "paper",
            xanchor = "center"
          ),
          xaxis = list(
            title = list(text = "Topic"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          yaxis = list(
            title = list(text = "Average Proportion"),
            tickfont = list(size = 18, color = "#3B3B3B", family = "Roboto, sans-serif"),
            titlefont = list(size = 18, color = "#0c1f4a", family = "Roboto, sans-serif")
          ),
          margin = list(t = 60, b = 60, l = 80, r = 40),
          hoverlabel = list(
            font = list(size = 16, family = "Roboto, sans-serif"),
            align = "left"
          )
        )
    })

    output$topic_prevalence_table_uiOutput <- renderUI({
      DT::dataTableOutput("hybrid_topic_table", width = "100%")
    })

    output$hybrid_topic_table <- DT::renderDataTable({
      if ("topic_keywords" %in% names(hybrid_result)) {
        topic_df <- data.frame(
          Topic = names(hybrid_result$topic_keywords),
          Keywords = sapply(hybrid_result$topic_keywords, function(x) paste(x, collapse = ", ")),
          Coherence = round(hybrid_result$quality_metrics$semantic_coherence, 3),
          Exclusivity = round(hybrid_result$quality_metrics$exclusivity, 3),
          stringsAsFactors = FALSE
        )
        DT::datatable(topic_df, options = list(pageLength = 10))
      }
    })
  })

  effect_hybrid_result <- eventReactive(input$effect_hybrid, {
    if (is.null(stm_K_number()) || is.null(attr(stm_K_number(), "hybrid_result"))) {
      showNotification("Please run the hybrid model first.", type = "warning", duration = 3)
      return(NULL)
    }

    hybrid_result <- attr(stm_K_number(), "hybrid_result")

    showNotification("Estimating effects for hybrid model...", type = "message", duration = NULL, id = "hybrid_effect_notification")

    tryCatch({
      categorical_var <- if (!is.null(input$hybrid_categorical_var_2)) unlist(strsplit(as.character(input$hybrid_categorical_var_2), ",\\s*")) else NULL
      continuous_var <- if (!is.null(input$hybrid_continuous_var_2)) unlist(strsplit(as.character(input$hybrid_continuous_var_2), ",\\s*")) else NULL

      terms <- c()
      if (!is.null(categorical_var) && length(categorical_var) > 0) {
        terms <- c(terms, categorical_var)
      }
      if (!is.null(continuous_var) && length(continuous_var) > 0) {
        for (var in continuous_var) {
          if (var %in% names(hybrid_result$metadata)) {
            unique_values <- length(unique(hybrid_result$metadata[[var]]))
            df <- max(3, min(4, unique_values - 1))
            terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
          }
        }
      }

      prevalence_formula <- if (length(terms) > 0) {
        as.formula(paste("~", paste(terms, collapse = " + ")))
      } else {
        hybrid_result$prevalence_formula
      }

      if (!is.null(prevalence_formula)) {
        effect_result <- stm::estimateEffect(
          formula = prevalence_formula,
          stmobj = hybrid_result$stm_result$model,
          metadata = hybrid_result$metadata,
          uncertainty = "Global"
        )

        tryCatch(removeNotification(id = "hybrid_effect_notification"), error = function(e) {})
        show_completion_notification("Hybrid model effects estimated successfully!", duration = 3)

        return(effect_result)
      } else {
        tryCatch(removeNotification(id = "hybrid_effect_notification"), error = function(e) {})
        showNotification("No covariates selected for effect estimation.", type = "warning", duration = 3)
        return(NULL)
      }

    }, error = function(e) {
      tryCatch(removeNotification(id = "hybrid_effect_notification"), error = function(e) {})
      showNotification(paste("Error estimating effects:", e$message), type = "error", duration = 5)
      return(NULL)
    })
  })

  observe({
    effect_result <- effect_hybrid_result()
    if (!is.null(effect_result)) {
      output$effect_table <- DT::renderDataTable({
        td <- tidytext::tidy(effect_result) %>%
          dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
        DT::datatable(td, options = list(pageLength = 15))
      })
    }
  })

  output$effect_hybrid_download_table <- downloadHandler(
    filename = function() {
      paste0("hybrid_effect_estimates_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (is.null(effect_hybrid_result())) {
        showNotification("No effect estimates available for download. Please run the effect estimation first.", type = "error")
        return()
      }
      td <- tidytext::tidy(effect_hybrid_result()) %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
      openxlsx::write.xlsx(td, file)
    }
  )

  observe({
    if (!is.null(stm_K_number()) && "topic_assignments" %in% names(stm_K_number())) {
      topics <- unique(stm_K_number()$topic_assignments[stm_K_number()$topic_assignments > 0])
      topic_choices <- setNames(topics, paste("Topic", topics))

      output$embedding_quote_topic_uiOutput <- renderUI({
        selectInput("embedding_quote_topic",
                   "Select Topic:",
                   choices = topic_choices)
      })
    }
  })

  observe({
    if (!is.null(colnames_cat())) {
      updateSelectizeInput(session, "hybrid_categorical_var",
                          choices = colnames_cat(),
                          selected = NULL)
    }
  })

  observe({
    if (!is.null(colnames_con())) {
      updateSelectizeInput(session, "hybrid_continuous_var",
                          choices = colnames_con(),
                          selected = NULL)
    }
  })

  output$text_output <- renderText({
    output_message()
  })

  generated_labels <- shiny::reactiveVal(NULL)

  previous_topic_measure <- reactiveVal(NULL)

  get_top_term_number <- reactive({
    measure <- if (is.null(input$topic_measure)) "frex" else input$topic_measure

    switch(measure,
           "frex" = input$top_term_number_frex %||% 5,
           "lift" = input$top_term_number_lift %||% 5,
           "score" = input$top_term_number_score %||% 5,
           "beta" = input$top_term_number_beta %||% 5,
           5
    )
  })

  stm_topic_terms <- reactive({
    if (is.null(stm_K_number())) {
      return(NULL)
    }

    measure <- if (is.null(input$topic_measure)) "frex" else input$topic_measure

    tryCatch(
      {
        current_top_terms <- get_top_term_number()

        if (measure == "beta") {
          beta_matrix <- exp(stm_K_number()$beta$logbeta[[1]])

          topic_terms <- data.frame()
          for (i in 1:nrow(beta_matrix)) {
            top_indices <- order(beta_matrix[i, ], decreasing = TRUE)[1:current_top_terms]
            top_probs <- beta_matrix[i, top_indices]
            top_terms <- stm_K_number()$vocab[top_indices]

            topic_data <- data.frame(
              topic = i,
              term = top_terms,
              beta = top_probs,
              stringsAsFactors = FALSE
            )
            topic_terms <- rbind(topic_terms, topic_data)
          }
        } else if (measure == "frex") {
          label_result <- stm::labelTopics(stm_K_number(), n = current_top_terms)
          frex_terms <- label_result$frex

          package_terms <- TextAnalysisR::get_topic_terms(
            stm_model = stm_K_number(),
            top_term_n = current_top_terms,
            verbose = FALSE
          )

          beta_matrix <- exp(stm_K_number()$beta$logbeta[[1]])

          topic_terms <- data.frame()
          for (i in 1:nrow(frex_terms)) {
            terms <- as.character(frex_terms[i, ])
            vocab_indices <- match(terms, stm_K_number()$vocab)
            term_probs <- beta_matrix[i, vocab_indices]

            topic_data <- data.frame(
              topic = i,
              term = terms,
              beta = term_probs,
              stringsAsFactors = FALSE
            )
            topic_terms <- rbind(topic_terms, topic_data)
          }
        } else if (measure == "lift") {
          label_result <- stm::labelTopics(stm_K_number(), n = current_top_terms)
          lift_terms <- label_result$lift

          beta_matrix <- exp(stm_K_number()$beta$logbeta[[1]])

          topic_terms <- data.frame()
          for (i in 1:nrow(lift_terms)) {
            terms <- as.character(lift_terms[i, ])
            vocab_indices <- match(terms, stm_K_number()$vocab)
            term_probs <- beta_matrix[i, vocab_indices]

            topic_data <- data.frame(
              topic = i,
              term = terms,
              beta = term_probs,
              stringsAsFactors = FALSE
            )
            topic_terms <- rbind(topic_terms, topic_data)
          }
        } else if (measure == "score") {
          label_result <- stm::labelTopics(stm_K_number(), n = current_top_terms)
          score_terms <- label_result$score

          beta_matrix <- exp(stm_K_number()$beta$logbeta[[1]])

          topic_terms <- data.frame()
          for (i in 1:nrow(score_terms)) {
            terms <- as.character(score_terms[i, ])
            vocab_indices <- match(terms, stm_K_number()$vocab)
            term_probs <- beta_matrix[i, vocab_indices]

            topic_data <- data.frame(
              topic = i,
              term = terms,
              beta = term_probs,
              stringsAsFactors = FALSE
            )
            topic_terms <- rbind(topic_terms, topic_data)
          }
        }

        return(topic_terms)
      },
      error = function(e) {
        showNotification(paste("Error extracting topic-term data:", e$message), type = "warning")
        return(NULL)
      }
    )
  })

  beta_td <- reactive({
    get_top_term_number()
    stm_topic_terms()
  })





  previous_system <- reactiveVal(NULL)
  previous_user <- reactiveVal(NULL)
  needs_label_generation <- reactiveVal(FALSE)

  # Content generation reactive values
  generated_content <- shiny::reactiveVal(NULL)
  needs_content_generation <- reactiveVal(FALSE)
  previous_content_type <- reactiveVal(NULL)

  # Update system and user prompts when content type changes
  shiny::observeEvent(input$content_type, {
    content_type <- input$content_type
    system_prompt <- TextAnalysisR::get_content_type_prompt(content_type)
    user_prompt <- TextAnalysisR::get_content_type_user_template(content_type)
    shiny::updateTextAreaInput(session, "content_system_prompt", value = system_prompt)
    shiny::updateTextAreaInput(session, "content_user_prompt", value = user_prompt)
  }, ignoreInit = FALSE)

  output$topic_summary_insights <- DT::renderDataTable({
    req(stm_K_number())

    topic_props <- colMeans(stm_K_number()$theta)
    n_topics <- length(topic_props)

    labels <- if (!is.null(generated_labels())) {
      gen_labels <- generated_labels()
      label_vec <- character(n_topics)
      for (i in 1:n_topics) {
        idx <- which(gen_labels$topic == i)
        if (length(idx) > 0) {
          label_vec[i] <- gen_labels$topic_label[idx[1]]
        } else {
          label_vec[i] <- paste("Topic", i)
        }
      }
      label_vec
    } else {
      paste("Topic", 1:n_topics)
    }

    if (!is.null(input$label_topics) && input$label_topics != "") {
      manual_labels <- trimws(strsplit(input$label_topics, ",")[[1]])
      for (i in seq_len(min(length(manual_labels), n_topics))) {
        if (manual_labels[i] != "") {
          labels[i] <- manual_labels[i]
        }
      }
    }

    label_result <- stm::labelTopics(stm_K_number(), n = 5)
    top_terms <- apply(label_result$frex, 1, paste, collapse = ", ")

    summary_df <- data.frame(
      Topic = 1:n_topics,
      Label = labels,
      `Top Terms` = top_terms,
      Proportion = round(topic_props * 100, 2),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      summary_df,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "Topic summary with labels and proportions"
    ) %>%
      DT::formatStyle(
        'Proportion',
        background = DT::styleColorBar(summary_df$Proportion, 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  output$generated_labels_table <- DT::renderDataTable({
    req(generated_labels())

    gen_labels <- generated_labels() %>%
      select(topic, topic_label) %>%
      rename(Topic = topic, `Generated Label` = topic_label)

    DT::datatable(
      gen_labels,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      caption = "AI-generated topic labels"
    )
  })


  shiny::observeEvent(input$topic_generate_labels, {
    if (!is.null(generated_labels()) && input$system == previous_system() && input$user == previous_user()) {
      shiny::showModal(shiny::modalDialog(
        title = "Notification",
        "Topic labels were already generated.",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    } else {
      shiny::req(beta_td())
      if (file.exists(".env")) {
        dotenv::load_dot_env()
      }

      existing_key <- Sys.getenv("OPENAI_API_KEY")
      needs_label_generation(TRUE)
      shiny::showModal(shiny::modalDialog(
        title = if (nzchar(existing_key)) "Confirm or Update API Key" else "OpenAI API Key Required",
        tagList(
          if (nzchar(existing_key)) {
            shiny::p("An API key is already configured. You can use it or replace it with a new one.")
          },
          tags$div(
            tags$label("API Key:", `for` = "openai_api_input"),
            tags$div(
              class = "input-group",
              style = "width: 100%;",
              shiny::passwordInput("openai_api_input",
                                 label = NULL,
                                 value = existing_key,
                                 placeholder = "sk-...",
                                 width = "100%"),
              tags$span(
                class = "input-group-btn",
                style = "vertical-align: top;",
                tags$button(
                  id = "toggle_api_key_modal",
                  type = "button",
                  class = "btn btn-default",
                  style = "height: 34px; border-left: none; border-radius: 0 4px 4px 0;",
                  onclick = "
                    var input = document.getElementById('openai_api_input');
                    var icon = this.querySelector('i');
                    if (input.type === 'password') {
                      input.type = 'text';
                      icon.className = 'fa fa-eye-slash';
                      this.title = 'Hide API key';
                    } else {
                      input.type = 'password';
                      icon.className = 'fa fa-eye';
                      this.title = 'Show API key';
                    }
                  ",
                  title = "Show API key",
                  tags$i(class = "fa fa-eye")
                )
              )
            ),
            tags$small("Tip: Click the eye icon to show/hide your API key",
                       style = "color: #666; display: block; margin-top: 5px;")
          )
        ),
        footer = tagList(
          shiny::actionButton("submit_api_key", "Submit", class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
      return()

      shiny::showNotification("Generating topic labels...", type = "message", duration = NULL, id = "label_gen_notification")

      top_topic_terms <- stm_topic_terms() %>%
        dplyr::group_by(topic) %>%
        dplyr::top_n(input$top_term_number_labeling, beta) %>%
        dplyr::ungroup()

      new_labels_td <- TextAnalysisR::generate_topic_labels(
        top_topic_terms,
        model = input$model,
        system = if(nzchar(input$system)) input$system else NULL,
        user = if(nzchar(input$user)) input$user else NULL,
        temperature = input$temperature,
        openai_api_key = api_key,
        verbose = FALSE
      )

      generated_labels(tibble::as_tibble(new_labels_td))

      previous_system(input$system)
      previous_user(input$user)

      tryCatch(removeNotification(id = "label_gen_notification"), error = function(e) {})
      tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})
      show_completion_notification("Topic labels generated successfully!", duration = 3)
    }
  })

  shiny::observeEvent(input$submit_api_key, {
    if (nzchar(input$openai_api_input)) {
      if (!TextAnalysisR:::validate_api_key(input$openai_api_input, strict = TRUE)) {
        TextAnalysisR:::log_security_event(
          "INVALID_API_KEY_ATTEMPT",
          "Malformed API key submitted for topic labeling",
          session,
          "WARNING"
        )
        TextAnalysisR:::show_error_notification(
          "Invalid API key format. OpenAI keys should start with 'sk-' and be 48+ characters."
        )
        return()
      }

      Sys.setenv(OPENAI_API_KEY = input$openai_api_input)
      TextAnalysisR:::log_security_event(
        "API_KEY_UPDATED",
        "Valid API key configured for topic labeling",
        session,
        "INFO"
      )
      shiny::removeModal()
      show_completion_notification("OpenAI API Key saved successfully for this session.", duration = 3)
      if (needs_label_generation()) {
        needs_label_generation(FALSE)
        shiny::isolate({

          shiny::showNotification("Generating topic labels...", type = "message", duration = NULL, id = "label_gen_notification")

          top_topic_terms <- stm_topic_terms() %>%
            dplyr::group_by(topic) %>%
            dplyr::top_n(input$top_term_number_labeling, beta) %>%
            dplyr::ungroup()

          new_labels_td <- TextAnalysisR::generate_topic_labels(
            top_topic_terms,
            model = input$model,
            system = if(nzchar(input$system)) input$system else NULL,
            user = if(nzchar(input$user)) input$user else NULL,
            temperature = input$temperature,
            openai_api_key = input$openai_api_input,
            verbose = FALSE
          )

          generated_labels(tibble::as_tibble(new_labels_td))

          previous_system(input$system)
          previous_user(input$user)

          tryCatch(removeNotification(id = "label_gen_notification"), error = function(e) {})
          tryCatch(removeNotification(id = "search_k_notification"), error = function(e) {})
          show_completion_notification("Topic labels generated successfully!", duration = 3)
        })
      }
    } else {
      shiny::showNotification("Please enter a valid API key.", type = "error", duration = 3)
    }
  })

  # Content generation from topics
  shiny::observeEvent(input$generate_topic_content, {
    # Check if content was already generated for same type
    if (!is.null(generated_content()) && input$content_type == previous_content_type()) {
      shiny::showModal(shiny::modalDialog(
        title = "Notification",
        "Content was already generated for this content type.",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    } else {
      shiny::req(beta_td())
      if (file.exists(".env")) {
        dotenv::load_dot_env()
      }

      existing_key <- Sys.getenv("OPENAI_API_KEY")
      needs_content_generation(TRUE)
      shiny::showModal(shiny::modalDialog(
        title = if (nzchar(existing_key)) "Confirm or Update API Key" else "OpenAI API Key Required",
        tagList(
          if (nzchar(existing_key)) {
            shiny::p("An API key is already configured. You can use it or replace it with a new one.")
          },
          tags$div(
            tags$label("API Key:", `for` = "content_api_input"),
            tags$div(
              class = "input-group",
              style = "width: 100%;",
              shiny::passwordInput("content_api_input",
                                 label = NULL,
                                 value = existing_key,
                                 placeholder = "sk-...",
                                 width = "100%"),
              tags$span(
                class = "input-group-btn",
                style = "vertical-align: top;",
                tags$button(
                  id = "toggle_content_api_key",
                  type = "button",
                  class = "btn btn-default",
                  style = "height: 34px; border-left: none; border-radius: 0 4px 4px 0;",
                  onclick = "
                    var input = document.getElementById('content_api_input');
                    var icon = this.querySelector('i');
                    if (input.type === 'password') {
                      input.type = 'text';
                      icon.className = 'fa fa-eye-slash';
                      this.title = 'Hide API key';
                    } else {
                      input.type = 'password';
                      icon.className = 'fa fa-eye';
                      this.title = 'Show API key';
                    }
                  ",
                  title = "Show API key",
                  tags$i(class = "fa fa-eye")
                )
              )
            ),
            tags$small("Tip: Click the eye icon to show/hide your API key",
                       style = "color: #666; display: block; margin-top: 5px;")
          )
        ),
        footer = tagList(
          shiny::actionButton("submit_content_api_key", "Submit", class = "btn-primary"),
          shiny::modalButton("Cancel")
        ),
        easyClose = TRUE
      ))
    }
  })

  shiny::observeEvent(input$submit_content_api_key, {
    if (nzchar(input$content_api_input)) {
      if (!TextAnalysisR:::validate_api_key(input$content_api_input, strict = TRUE)) {
        TextAnalysisR:::log_security_event(
          "INVALID_API_KEY_ATTEMPT",
          "Malformed API key submitted for content generation",
          session,
          "WARNING"
        )
        TextAnalysisR:::show_error_notification(
          "Invalid API key format. OpenAI keys should start with 'sk-' and be 48+ characters."
        )
        return()
      }

      Sys.setenv(OPENAI_API_KEY = input$content_api_input)
      TextAnalysisR:::log_security_event(
        "API_KEY_UPDATED",
        "Valid API key configured for content generation",
        session,
        "INFO"
      )
      shiny::removeModal()
      show_completion_notification("OpenAI API Key saved successfully for this session.", duration = 3)

      if (needs_content_generation()) {
        needs_content_generation(FALSE)
        shiny::isolate({
          content_type <- input$content_type
          content_type_label <- switch(content_type,
            "survey_item" = "Survey Items",
            "research_question" = "Research Questions",
            "theme_description" = "Theme Descriptions",
            "policy_recommendation" = "Policy Recommendations",
            "interview_question" = "Interview Questions",
            "custom" = "Custom Content"
          )

          shiny::showNotification(
            paste0("Generating ", content_type_label, "..."),
            type = "message", duration = NULL, id = "content_gen_notification"
          )

          top_topic_terms <- stm_topic_terms() %>%
            dplyr::group_by(topic) %>%
            dplyr::top_n(input$content_top_terms, beta) %>%
            dplyr::ungroup()

          # Use the system and user prompts from the UI (populated by content type or user-edited)
          system_prompt <- if (nzchar(input$content_system_prompt)) {
            input$content_system_prompt
          } else {
            NULL
          }

          user_prompt_template <- if (nzchar(input$content_user_prompt)) {
            input$content_user_prompt
          } else {
            NULL
          }

          # Use content-specific model and temperature
          new_content <- TextAnalysisR::generate_topic_content(
            topic_terms_df = top_topic_terms,
            content_type = content_type,
            topic_var = "topic",
            term_var = "term",
            weight_var = "beta",
            provider = "openai",
            model = input$content_model,
            temperature = input$content_temperature,
            system_prompt = system_prompt,
            user_prompt_template = user_prompt_template,
            max_tokens = input$content_max_tokens,
            api_key = input$content_api_input,
            verbose = FALSE
          )

          generated_content(tibble::as_tibble(new_content))
          previous_content_type(content_type)

          tryCatch(removeNotification(id = "content_gen_notification"), error = function(e) {})
          show_completion_notification(paste0(content_type_label, " generated successfully!"), duration = 3)

          # Show results in a modal
          output_var <- switch(content_type,
            "survey_item" = "survey_item",
            "research_question" = "research_question",
            "theme_description" = "theme_description",
            "policy_recommendation" = "policy_recommendation",
            "interview_question" = "interview_question",
            "custom" = "generated_content"
          )

          shiny::showModal(shiny::modalDialog(
            title = paste("Generated", content_type_label),
            size = "l",
            DT::renderDataTable({
              DT::datatable(
                new_content,
                options = list(
                  pageLength = 10,
                  scrollX = TRUE,
                  dom = 'Bfrtip',
                  buttons = list(
                    'copy',
                    list(extend = 'csv', title = 'Generated_Content'),
                    list(extend = 'excel', title = 'Generated_Content'),
                    list(extend = 'pdf', title = 'Generated_Content'),
                    'print'
                  )
                ),
                extensions = 'Buttons',
                rownames = FALSE
              )
            }),
            footer = tagList(
              shiny::modalButton("Close")
            ),
            easyClose = TRUE
          ))
        })
      }
    } else {
      shiny::showNotification("Please enter a valid API key.", type = "error", duration = 3)
    }
  })

  # Download handler for generated content
  output$download_generated_content <- downloadHandler(
    filename = function() {
      content_type <- isolate(input$content_type)
      paste0("generated_", content_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(generated_content())
      utils::write.csv(generated_content(), file, row.names = FALSE)
    }
  )

  # Output for checking if generated content exists (for AI Content tab)
  output$has_generated_content <- reactive({
    !is.null(generated_content()) && nrow(generated_content()) > 0
  })
  outputOptions(output, "has_generated_content", suspendWhenHidden = FALSE)

  # Render generated content table in AI Content tab (editable with export options)
  output$generated_content_table <- DT::renderDataTable({
    req(generated_content())
    DT::datatable(
      generated_content(),
      editable = TRUE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          'copy',
          list(extend = 'csv', title = 'Generated_Content'),
          list(extend = 'excel', title = 'Generated_Content'),
          list(extend = 'pdf', title = 'Generated_Content'),
          'print'
        )
      ),
      extensions = 'Buttons',
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = names(generated_content()), `font-size` = "16px")
  })

  # Handle edits to the generated content table
  shiny::observeEvent(input$generated_content_table_cell_edit, {
    info <- input$generated_content_table_cell_edit
    current_data <- generated_content()
    current_data[info$row, info$col + 1] <- info$value
    generated_content(current_data)
  })

  output$topic_term_plot <- plotly::renderPlotly({
    stm_model_trigger()
    shiny::req(beta_td())
    shiny::req(input$topic_measure)

    wrap_width <- if (is.null(input$width)) {
      30
    } else if (input$width > 1000) {
      35
    } else {
      25
    }

    manual_labels <- if (input$label_topics != "") {
      strsplit(input$label_topics, split = ",")[[1]]
    } else {
      NULL
    }

    new_labels_td <- generated_labels()

    topic_labels_td <- beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(get_top_term_number(), beta) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(topic, .keep_all = TRUE)

    topic_numbers <- sort(unique(topic_labels_td$topic))
    final_labels <- stats::setNames(paste("Topic", topic_numbers), topic_numbers)

    if (!is.null(new_labels_td) && nrow(new_labels_td) > 0) {
      for (i in seq_len(nrow(new_labels_td))) {
        topic_idx <- as.character(new_labels_td$topic[i])
        if (topic_idx %in% names(final_labels)) {
          final_labels[topic_idx] <- new_labels_td$topic_label[i]
        }
      }
    }

    if (!is.null(manual_labels) && length(manual_labels) > 0) {
      for (i in seq_len(min(length(manual_labels), length(topic_numbers)))) {
        if (manual_labels[i] != "") {
          final_labels[as.character(topic_numbers[i])] <- manual_labels[i]
        }
      }
    }

    final_labels <- sapply(final_labels, function(x) stringr::str_wrap(x, width = wrap_width))

    topic_term_data <- beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(get_top_term_number(), beta) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(topic = as.character(topic), term = as.character(term))

    num_topics <- length(unique(topic_term_data$topic))
    topic_colors <- if (num_topics <= 8) {
      RColorBrewer::brewer.pal(max(min(num_topics, 8), 3), "Set2")[1:num_topics]
    } else {
      c(
        RColorBrewer::brewer.pal(8, "Set2"),
        rainbow(num_topics - 8, start = 0.1, end = 0.9)
      )
    }

    measure <- if (is.null(input$topic_measure)) "frex" else input$topic_measure
    measure_label <- switch(measure,
                            "frex" = "FREX Score",
                            "lift" = "Lift Score",
                            "score" = "Score",
                            "beta" = "Probability"
    )

    plot_word_probability(
      top_topic_terms = topic_term_data,
      topic_label = final_labels,
      ncol = input$ncol_top_terms,
      height = input$height,
      width = input$width,
      ylab = measure_label,
      title = paste("Top Terms by", measure_label),
      colors = topic_colors,
      measure_label = measure_label
    )
  })

  output$topic_term_plot_uiOutput <- renderUI({
    div(
      style = "margin-bottom: 20px; overflow: hidden;",
      plotly::plotlyOutput(
        "topic_term_plot",
        height = 500,
        width = "100%"
      )
    )
  })

  topic_table_data <- reactive({
    req(beta_td())
    get_top_term_number()

    current_wrap_width <- if (is.null(input$width)) {
      30
    } else if (input$width > 1000) {
      35
    } else {
      25
    }

    topic_labels_td <- beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(get_top_term_number(), beta) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(topic, .keep_all = TRUE) %>%
      dplyr::mutate(topic_label = paste("Topic", topic)) %>%
      dplyr::mutate(topic_label = stringr::str_wrap(topic_label, width = current_wrap_width))

    manual_labels <- if (input$label_topics != "") {
      strsplit(input$label_topics, split = ",")[[1]]
    } else {
      NULL
    }

    new_labels_td <- generated_labels()
    topic_numbers <- sort(unique(topic_labels_td$topic))
    max_topics <- length(topic_numbers)

    final_labels <- stats::setNames(paste("Topic", topic_numbers), topic_numbers)

    if (!is.null(new_labels_td) && nrow(new_labels_td) > 0) {
      for (i in seq_len(nrow(new_labels_td))) {
        topic_idx <- as.character(new_labels_td$topic[i])
        if (topic_idx %in% names(final_labels)) {
          final_labels[topic_idx] <- new_labels_td$topic_label[i]
        }
      }
    }

    if (!is.null(manual_labels) && length(manual_labels) > 0) {
      for (i in seq_len(min(length(manual_labels), max_topics))) {
        if (manual_labels[i] != "") {
          final_labels[as.character(topic_numbers[i])] <- manual_labels[i]
        }
      }
    }

    topic_labels_td <- topic_labels_td %>%
      dplyr::mutate(topic_label = final_labels[as.character(topic)]) %>%
      dplyr::arrange(as.numeric(topic))

    topic_labels_td$topic_label <- stringr::str_wrap(topic_labels_td$topic_label, width = current_wrap_width)

    df <- beta_td() %>%
      dplyr::left_join(
        dplyr::distinct(topic_labels_td, topic, topic_label),
        by = "topic",
        relationship = "many-to-one"
      ) %>%
      dplyr::select(topic_label, topic, term, beta) %>%
      dplyr::mutate(across(where(is.numeric), ~ round(., 3)))

    df
  })

  output$topic_term_table <- DT::renderDataTable({
    if (is.null(topic_table_data()) || nrow(topic_table_data()) == 0) {
      return(DT::datatable(
        data.frame(Message = "No topic-term data available. Please run topic modeling first."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }
    topic_table_data() %>%
      DT::datatable(
        rownames = FALSE,
        width = input$width,
        extensions = "Buttons",
        colnames = c(
          "Topic Label", "Topic", "Term",
          if (is.null(input$topic_measure) || input$topic_measure == "beta") {
            "Probability"
          } else if (input$topic_measure == "frex") {
            "FREX Score"
          } else if (input$topic_measure == "lift") {
            "Lift Score"
          } else {
            "Value"
          }
        ),
        options = list(
          scrollX = TRUE,
          width = "100%",
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print")
        )
      )
  })

  output$topic_term_table_uiOutput <- renderUI({
    req(beta_td())
    htmltools::tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("topic_term_table", width = "100%")
    )
  })



  # Step 3: Display per-document-per topic probabilities by topics.

  output$topic_number_uiOutput <- renderUI({
    sliderInput(
      "topic_number",
      "Select topics:",
      value = K_number(),
      min = 0,
      step = 1,
      max = K_number()
    )
  })


  calculate_enhanced_clustering <- function(similarity_matrix, method = "kmeans", n_clusters = NULL,
                                            umap_params = list(), dbscan_params = list(),
                                            existing_embedding = NULL, embedding_method = NULL) {
    tryCatch({
      similarity_matrix <- clean_similarity_matrix(similarity_matrix)
      dist_matrix <- as.dist(1 - similarity_matrix)
      n_docs <- nrow(similarity_matrix)

      result <- switch(method,


                       "dbscan" = {
                         if (!requireNamespace("dbscan", quietly = TRUE)) {
                           showNotification("dbscan package required for this method", type = "error")
                           return(NULL)
                         }

                         if (!is.null(existing_embedding)) {
                           embedding <- existing_embedding
                           detection_method <- paste("DBSCAN on", embedding_method)
                         } else {
                           embedding <- similarity_matrix
                           detection_method <- "DBSCAN"
                         }

                         eps <- dbscan_params$eps %||% 0
                         min_samples <- dbscan_params$min_samples %||% 5

                         if (eps == 0) {
                           k_neighbor <- min_samples
                           if (ncol(embedding) > 2) {
                             dist_matrix <- as.matrix(dist(embedding))
                           } else {
                             dist_matrix <- as.matrix(dist(embedding))
                           }
                           k_distances <- apply(dist_matrix, 1, function(row) {
                             sorted_distances <- sort(row[row > 0])
                             if (length(sorted_distances) >= k_neighbor) {
                               return(sorted_distances[k_neighbor])
                             } else {
                               return(max(sorted_distances))
                             }
                           })
                           eps <- quantile(sort(k_distances), 0.2)
                         }

                         db <- dbscan::dbscan(embedding, eps = eps, minPts = min_samples)

                         list(
                           clusters = db$cluster,
                           umap_embedding = if (ncol(embedding) == 2) embedding else NULL,
                           eps = eps,
                           minPts = min_samples,
                           n_clusters_found = max(db$cluster, na.rm = TRUE),
                           auto_detected = TRUE,
                           detection_method = detection_method,
                           embedding_method = embedding_method
                         )
                       },

                       "kmeans" = {
                         if (is.null(n_clusters) || n_clusters == 0) {
                           max_k <- min(10, n_docs - 1)

                           if (max_k < 2) {
                             n_clusters <- 2
                           } else if (requireNamespace("cluster", quietly = TRUE)) {
                             sil_width <- numeric(max_k - 1)
                             for (k in 2:max_k) {
                               km_temp <- kmeans(similarity_matrix, centers = k, nstart = 10)
                               sil <- cluster::silhouette(km_temp$cluster, dist_matrix)
                               sil_width[k - 1] <- mean(sil[, 3])
                             }
                             n_clusters <- which.max(sil_width) + 1
                           } else {
                             n_clusters <- 3
                           }
                         }

                         km <- kmeans(similarity_matrix, centers = n_clusters, nstart = 25, iter.max = 300)

                         list(
                           clusters = km$cluster,
                           centers = km$centers,
                           withinss = km$withinss,
                           tot.withinss = km$tot.withinss,
                           betweenss = km$betweenss,
                           n_clusters_found = n_clusters,
                           auto_detected = is.null(n_clusters) || n_clusters == 0,
                           detection_method = "Silhouette Analysis"
                         )
                       },

                       "hierarchical" = {
                         hc <- hclust(dist_matrix, method = "ward.D2")

                         if (is.null(n_clusters) || n_clusters == 0) {
                           max_k <- min(10, n_docs - 1)

                           if (max_k < 2) {
                             n_clusters <- 2
                           } else if (requireNamespace("cluster", quietly = TRUE)) {
                             sil_width <- numeric(max_k - 1)
                             for (k in 2:max_k) {
                               clusters_temp <- cutree(hc, k = k)
                               sil <- cluster::silhouette(clusters_temp, dist_matrix)
                               sil_width[k - 1] <- mean(sil[, 3])
                             }
                             n_clusters <- which.max(sil_width) + 1
                           } else {
                             heights <- hc$height
                             n_merges <- length(heights)
                             height_diffs <- diff(heights)
                             if (length(height_diffs) > 0) {
                               biggest_jump <- which.max(height_diffs)
                               n_clusters <- n_merges - biggest_jump + 1
                               n_clusters <- max(2, min(n_clusters, max_k))
                             } else {
                               n_clusters <- 3
                             }
                           }
                         }

                         list(
                           clusters = cutree(hc, k = n_clusters),
                           hclust = hc,
                           height = hc$height,
                           n_clusters_found = n_clusters,
                           auto_detected = is.null(n_clusters) || n_clusters == 0,
                           detection_method = "Silhouette Analysis"
                         )
                       },

                       {
                         showNotification(paste("Unknown clustering method:", method, "- using k-means"), type = "warning")
                         km <- kmeans(similarity_matrix, centers = 3, nstart = 10)
                         list(
                           clusters = km$cluster,
                           centers = km$centers,
                           n_clusters_found = 3,
                           auto_detected = TRUE,
                           detection_method = "Default K-means"
                         )
                       }
      )

      if (!is.null(result)) {
        result$method <- method
        result$n_clusters <- result$n_clusters_found %||% n_clusters

        if (!is.null(result$clusters) && length(unique(result$clusters)) > 1) {
          eval_metrics <- TextAnalysisR::calculate_clustering_metrics(
            clusters = result$clusters,
            data_matrix = similarity_matrix,
            dist_matrix = dist_matrix
          )
          result$silhouette <- eval_metrics$silhouette
          result$davies_bouldin <- eval_metrics$davies_bouldin
          result$calinski_harabasz <- eval_metrics$calinski_harabasz
        } else {
          result$silhouette <- NA
          result$davies_bouldin <- NA
          result$calinski_harabasz <- NA
        }
      }

      return(result)
    }, error = function(e) {
      showNotification(paste("Clustering error:", e$message), type = "error")
      return(NULL)
    })
  }

  top_terms_selected <- reactive({
    topic_data <- topic_table_data()
    if (is.null(topic_data) || !is.data.frame(topic_data)) {
      return(NULL)
    }

    beta_data <- beta_td()
    if (is.null(beta_data) || !is.data.frame(beta_data)) {
      return(NULL)
    }

    topic_mapping <- topic_data %>%
      dplyr::select(topic, topic_label) %>%
      dplyr::distinct()

    beta_data %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(input$top_term_number_2, beta) %>%
      dplyr::arrange(beta) %>%
      dplyr::summarise(terms = paste(term, collapse = ", ")) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(topic_mapping, by = "topic")
  })

  gamma_terms <- reactive({
    dfm_obj <- get_available_dfm()

    if (is.null(stm_K_number()) || is.null(dfm_obj)) {
      return(NULL)
    }
    tryCatch(
      {
        package_result <- tryCatch({
          TextAnalysisR::calculate_topic_probability(
            stm_model = stm_K_number(),
            top_n = input$topic_number %||% 10,
            verbose = FALSE
          )
        }, error = function(e) NULL)

        topic_data <- topic_table_data()
        if (!is.data.frame(topic_data)) {
          showNotification("Topic data is not available. Please ensure topic modeling has been run.", type = "error")
          return(NULL)
        }

        top_terms <- top_terms_selected()
        if (!is.data.frame(top_terms)) {
          showNotification("Top terms data is not available.", type = "error")
          return(NULL)
        }

        topic_mapping <- topic_data %>%
          dplyr::select(topic, topic_label) %>%
          dplyr::distinct()

        if (is.null(package_result)) {
          gamma_td <- tidytext::tidy(
            stm_K_number(),
            matrix = "gamma",
            document_names = rownames(dfm_outcome())
          )

          gamma_td %>%
            dplyr::group_by(topic) %>%
            dplyr::summarise(gamma = mean(gamma)) %>%
            dplyr::arrange(desc(gamma)) %>%
            dplyr::left_join(top_terms, by = "topic") %>%
            dplyr::left_join(topic_mapping, by = "topic") %>%
            dplyr::mutate(topic = reorder(topic, gamma))
        } else {
          package_result %>%
            dplyr::left_join(top_terms, by = "topic") %>%
            dplyr::left_join(topic_mapping, by = "topic") %>%
            dplyr::mutate(topic = reorder(topic, gamma))
        }
      },
      error = function(e) {
        showNotification(paste("Error calculating topic prevalence:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  observeEvent(input$display, {
    gamma_data <- gamma_terms()
    if (is.null(gamma_data)) {
      shiny::showModal(shiny::modalDialog(
        title = "Topic Modeling Required",
        p("Please run the STM model in Step 2 (Topic Modeling) before viewing topic prevalence."),
        easyClose = TRUE,
        footer = shiny::modalButton("OK")
      ))
      return()
    }

    if (input$label_topics == "") {
      tn <- NULL
    } else {
      tn <- strsplit(input$label_topics, split = ",")[[1]]
    }


    observe({
      req(united_tbl())

      tryCatch(
        {
          col_names <- c("None" = "", names(united_tbl()))

          updateSelectizeInput(session,
                               "doc_id_var",
                               choices = col_names,
                               selected = ""
          )

          updateSelectizeInput(session,
                               "doc_category_var",
                               choices = col_names,
                               selected = ""
          )
        },
        error = function(e) {
          cat("Error updating document variables:", e$message, "\n")
        }
      )
    })

    observe({
      req(input$doc_category_var, input$doc_category_var != "", input$doc_category_var != "None")
      req(united_tbl())

      tryCatch(
        {
          if (input$doc_category_var %in% names(united_tbl())) {
            categories <- unique(united_tbl()[[input$doc_category_var]])
            categories <- categories[!is.na(categories)]

            if (length(categories) > 0) {
              all_choices <- c("All Categories" = "all", setNames(categories, categories))

              updateSelectizeInput(session,
                                   "heatmap_category_filter",
                                   choices = all_choices,
                                   selected = "all"
              )
            }
          }
        },
        error = function(e) {
          cat("Error updating category filter:", e$message, "\n")
        }
      )
    })

    output$topic_by_prevalence_plot2 <- plotly::renderPlotly({
      gamma_data <- gamma_terms()
      if (is.null(gamma_data)) {
        return(NULL)
      }

      topic_prevalence_data <- gamma_data %>%
        mutate(tt = as.numeric(topic))

      if (!is.null(tn)) {
        topic_labels <- tn[seq_along(topic_prevalence_data$tt)]
        topic_prevalence_data <- topic_prevalence_data %>%
          mutate(topic_label = ifelse(!is.na(topic_labels), topic_labels, paste("Topic", tt)))
      } else {
        topic_prevalence_data <- topic_prevalence_data %>%
          mutate(topic_label = paste("Topic", topic))
      }

      num_topics <- nrow(topic_prevalence_data)
      topic_colors <- if (num_topics <= 8) {
        RColorBrewer::brewer.pal(max(min(num_topics, 8), 3), "Set2")[1:num_topics]
      } else {
        c(
          RColorBrewer::brewer.pal(8, "Set2"),
          rainbow(num_topics - 8, start = 0.1, end = 0.9)
        )
      }

      plot_topic_probability(
        gamma_data = topic_prevalence_data,
        top_n = input$topic_number,
        height = input$height_topic_prevalence,
        width = input$width_topic_prevalence,
        topic_labels = TRUE,
        colors = topic_colors
      )
    })
  })

  output$topic_prevalence_plot_uiOutput <- renderUI({
    req(input$display)
    if (!is.null(gamma_terms())) {
      div(
        style = "margin-bottom: 20px; overflow: hidden;",
        plotly::plotlyOutput(
          "topic_by_prevalence_plot2",
          height = paste0(input$height_topic_prevalence, "px"),
          width = paste0(input$width_topic_prevalence, "px")
        )
      )
    }
  })


  observeEvent(input$display, {
    if (input$label_topics == "") {
      tn <- NULL
    } else {
      tn <- strsplit(input$label_topics, split = ",")[[1]]
    }

    output$topic_by_prevalence_table <- DT::renderDataTable({
      gamma_data <- gamma_terms()
      if (is.null(gamma_data)) {
        return(NULL)
      }

      topic_by_prevalence_table <- gamma_data %>%
        top_n(input$topic_number, gamma) %>%
        mutate(
          tt = as.numeric(topic),
          ord = tt
        ) %>%
        arrange(ord)

      if (!is.null(tn)) {
        topic_by_prevalence_table <- topic_by_prevalence_table %>%
          mutate(
            topic_label = ifelse(tt <= length(tn), tn[tt], paste("Topic", tt))
          )
      } else {
        topic_by_prevalence_table <- topic_by_prevalence_table %>%
          mutate(
            topic_label = paste("Topic", tt)
          )
      }

      topic_by_prevalence_table$topic_label <- factor(
        topic_by_prevalence_table$topic_label,
        levels = unique(topic_by_prevalence_table$topic_label)
      )

      topic_by_prevalence_table %>%
        select(topic_label, topic, gamma) %>%
        dplyr::arrange(as.numeric(topic)) %>%
        mutate(across(where(is.numeric), ~ round(., 3))) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            width = "100%",
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          )
        ) %>%
        DT::formatStyle(
          columns = c("topic", "gamma"),
          fontSize = "16px"
        )
    })

    output$topic_prevalence_table_uiOutput <- renderUI({
      if (!is.null(gamma_terms())) {
        htmltools::tags$div(
          style = "margin-top: 20px;",
          DT::dataTableOutput("topic_by_prevalence_table", width = "100%")
        )
      }
    })
  })

  # Step 4: Explore example documents for each topic (display quotes).

  print_K_number_from_1 <-
    eventReactive(eventExpr = input$K_number, {
      1:input$K_number
    })

  output$quote_topic_number_uiOutput <- renderUI({
    req(print_K_number_from_1())
    selectInput(
      "topic_number_quote",
      "Topic number",
      choices = print_K_number_from_1(),
      selected = " "
    )
  })


  colnames_cat_3 <- reactive({
    req(out(), mydata())
    names(mydata())
  })

  observe({
    req(colnames_cat_3())
    choices <- colnames_cat_3()
    if (length(choices) > 0) {
      updateSelectizeInput(session,
                           "topic_texts",
                           choices = choices,
                           selected = choices[1]
      )
    } else {
      updateSelectizeInput(session,
                           "topic_texts",
                           choices = character(0),
                           selected = NULL
      )
    }
  })


  thoughts <- eventReactive(eventExpr = input$quote, {
    if (is.null(stm_K_number()) || is.null(out()) || is.null(input$topic_texts) || is.null(mydata())) {
      return(NULL)
    }
    tryCatch(
      {
        pn <- input$topic_number_quote %>% as.numeric()
        tn <- input$topic_texts

        if (!is.null(out()$meta) && ncol(out()$meta) > 0) {
          if ("united_texts" %in% names(out()$meta)) {
            texts <- out()$meta$united_texts
          } else {
            text_cols <- names(out()$meta)[sapply(out()$meta, is.character)]
            if (length(text_cols) > 0) {
              texts <- out()$meta[[text_cols[1]]]
            } else {
              stop("No text column found in STM metadata")
            }
          }
        } else {
          stop("No metadata available from STM conversion")
        }

        if (length(texts) != length(out()$documents)) {
          stop(sprintf("Text-document mismatch: %d texts vs %d documents. This may be due to missing values in the data.",
                      length(texts), length(out()$documents)))
        }

        thoughts_result <- stm::findThoughts(
          stm_K_number(),
          texts = texts,
          n = 3,
          topics = pn
        )

        if (!is.null(thoughts_result$docs[[1]]) && length(thoughts_result$docs[[1]]) > 0) {
          doc_indices <- thoughts_result$index[[1]]

          selected_values <- NULL
          if (tn %in% names(out()$meta)) {
            selected_values <- out()$meta[[tn]][doc_indices]
          } else if (tn %in% names(mydata())) {
            doc_names <- rownames(out()$meta)
            if (!is.null(doc_names) && length(doc_names) > 0) {
              original_indices <- match(doc_names[doc_indices], rownames(mydata()))
              if (!any(is.na(original_indices))) {
                selected_values <- mydata()[[tn]][original_indices]
              }
            }
          }

          result_df <- data.frame(
            Document_Index = doc_indices,
            stringsAsFactors = FALSE
          )

          if (!is.null(selected_values)) {
            result_df[[tn]] <- selected_values
          }

          result_df$Text <- thoughts_result$docs[[1]]

          return(result_df)
        } else {
          data.frame(
            Document_Index = integer(0),
            Text = character(0),
            stringsAsFactors = FALSE
          )
        }
      },
      error = function(e) {
        showNotification(paste("Error finding topic thoughts:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  output$quote_table <- DT::renderDataTable({
    if (is.null(thoughts()) || nrow(thoughts()) == 0) {
      return(DT::datatable(
        data.frame(Message = "No topic thoughts available. Please run topic modeling and select valid parameters first."),
        rownames = FALSE,
        options = list(dom = "t", ordering = FALSE)
      ))
    }

    thoughts_data <- thoughts()

    if (ncol(thoughts_data) > 2) {
      column_defs <- list(
        list(width = "80px", targets = 0),
        list(width = "200px", targets = 1),
        list(width = "400px", targets = 2)
      )
    } else {
      column_defs <- list(
        list(width = "80px", targets = 0),
        list(width = "600px", targets = 1)
      )
    }

    DT::datatable(
      thoughts_data,
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        columnDefs = column_defs
      )
    )
  })

  # Hybrid model topic number UI output
  output$hybrid_topic_number_uiOutput <- renderUI({
    sliderInput(
      "hybrid_topic_number",
      "Select topics:",
      value = hybrid_K_number(),
      min = 0,
      step = 1,
      max = hybrid_K_number()
    )
  })

  # Hybrid model quote UI outputs
  hybrid_print_K_number_from_1 <- eventReactive(input$hybrid_K_number, {
    1:input$hybrid_K_number
  })

  output$hybrid_quote_topic_number_uiOutput <- renderUI({
    req(hybrid_print_K_number_from_1())
    selectInput(
      "hybrid_topic_number_quote",
      "Topic number",
      choices = hybrid_print_K_number_from_1(),
      selected = " "
    )
  })






  effect_stm_K_number <- eventReactive(eventExpr = input$effect, {
    if (is.null(stm_K_number()) || is.null(out())) {
      showNotification("No STM model available. Please run topic modeling first.", type = "warning")
      return(NULL)
    }

    tryCatch(
      {
        categorical_var <- if (!is.null(input$categorical_var_2)) unlist(strsplit(as.character(input$categorical_var_2), ",\\s*")) else NULL
        continuous_var <- if (!is.null(input$continuous_var_2)) unlist(strsplit(as.character(input$continuous_var_2), ",\\s*")) else NULL

        terms <- c()
        if (!is.null(categorical_var) && length(categorical_var) > 0) {
          terms <- c(terms, categorical_var)
        }
        if (!is.null(continuous_var) && length(continuous_var) > 0) {
          for (var in continuous_var) {
            if (var %in% names(out()$meta)) {
              unique_values <- length(unique(out()$meta[[var]]))
              df <- max(3, min(4, unique_values - 1))
              terms <- c(terms, paste0("s(", var, ", df = ", df, ")"))
            } else {
              warning("Variable not found in metadata: ", var)
            }
          }
        }

        prevalence_formula <- if (length(terms) > 0) {
          as.formula(paste("~", paste(terms, collapse = " + ")))
        } else {
          NULL
        }

        if (!is.null(prevalence_formula) && length(terms) > 0) {
          cat("Prevalence formula:", paste(deparse(prevalence_formula), collapse = " "), "\n")
          stmm <- stm::estimateEffect(
            formula = prevalence_formula,
            stmobj = stm_K_number(),
            metadata = out()$meta,
            documents = out()$documents,
            uncertainty = "Global",
            prior = 1e-5
          )
        } else {
          stmm <- NULL
        }
      },
      error = function(e) {
        showNotification(paste("Error estimating effects:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  output$has_effect_estimates <- reactive({
    tryCatch({
      effect_stm_K_number_td <- effect_stm_K_number()
      if (!is.null(effect_stm_K_number_td)) {
        td <- tidytext::tidy(effect_stm_K_number_td) %>%
          dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
        !is.null(td) && nrow(td) > 0
      } else {
        FALSE
      }
    }, error = function(e) {
      FALSE
    })
  })
  outputOptions(output, "has_effect_estimates", suspendWhenHidden = FALSE)

  output$effect_table <- DT::renderDataTable(
    {
      effect_stm_K_number_td <- effect_stm_K_number()
      td <- tidytext::tidy(effect_stm_K_number_td) %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
      td
    },
    rownames = FALSE
  )

  output$effect_download_table <- downloadHandler(
    filename = function() {
      paste0(ifelse(is.null(input$file) || input$file == "", "estimated regression data", input$file), ".xlsx")
    },
    content = function(file) {
      if (is.null(effect_stm_K_number())) {
        showNotification("No effect estimates available for download. Please run the effect estimation first.", type = "error")
        return()
      }
      td <- tidytext::tidy(effect_stm_K_number()) %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3)))
      openxlsx::write.xlsx(td, file)
    }
  )

  # Step 6: Plot topic prevalence effects by a categorical variable.

  observe({
    selected_cat <- input$categorical_var_2
    updateSelectizeInput(session,
                      "effect_cat_btn",
                      choices = selected_cat,
                      selected = if (!is.null(selected_cat) && length(selected_cat) > 0) selected_cat[1] else NULL
    )
  })

  effects_categorical_var <- reactive({
    if (is.null(effect_stm_K_number()) || is.null(input$effect_cat_btn)) {
      return(NULL)
    }
    tryCatch(
      {
        stminsights::get_effects(
          estimates = effect_stm_K_number(),
          variable = input$effect_cat_btn,
          type = "pointestimate"
        )
      },
      error = function(e) {
        showNotification(paste("Error calculating categorical effects:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  observeEvent(input$display_cat, {
    output$cat_plot <- plotly::renderPlotly({
      plot_topic_effects_categorical(
        effects_data = effects_categorical_var(),
        ncol = input$ncol_cat,
        height = input$height_cat_plot,
        width = input$width_cat_plot
      )
    })
  })

  output$cat_plot_uiOutput <- renderUI({
    req(input$effect_cat_btn)
    req(input$display_cat)
    plotly::plotlyOutput(
      "cat_plot",
      height = input$height_cat_plot,
      width = input$width_cat_plot
    )
  })

  observeEvent(input$display_cat, {
    req(input$effect_cat_btn)
    output$cat_table <- DT::renderDataTable({
      if (is.null(effects_categorical_var()) || nrow(effects_categorical_var()) == 0) {
        return(DT::datatable(
          data.frame(Message = "No categorical effects available. Please run the effect estimation first."),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }
      effects_categorical_var() %>%
        dplyr::select(topic, value, proportion, lower, upper) %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            width = "100%",
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          )
        )
    })
  })

  output$cat_table_uiOutput <- renderUI({
    req(input$effect_cat_btn)
    req(input$display_cat)
    htmltools::tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("cat_table", width = input$width_cat_plot)
    )
  })


  # Step 7: Plot topic prevalence effects by a continuous variable.

  observe({
    selected_con <- input$continuous_var_2
    updateSelectizeInput(session,
                         "effect_con_btn",
                         choices = selected_con,
                         selected = if (!is.null(selected_con) && length(selected_con) > 0) selected_con[1] else NULL
    )
  })

  effects_continuous_var <- reactive({
    if (is.null(effect_stm_K_number()) || is.null(input$effect_con_btn)) {
      return(NULL)
    }
    tryCatch(
      {
        stminsights::get_effects(
          estimates = effect_stm_K_number(),
          variable = input$effect_con_btn,
          type = "continuous"
        )
      },
      error = function(e) {
        showNotification(paste("Error calculating continuous effects:", e$message), type = "error")
        return(NULL)
      }
    )
  })

  observeEvent(input$display_con, {
    output$con_plot <- plotly::renderPlotly({
      req(effects_continuous_var())

      plot_topic_effects_continuous(
        effects_data = effects_continuous_var(),
        ncol = input$ncol_con,
        height = input$height_con_plot,
        width = input$width_con_plot
      )
    })
  })

  output$con_plot_uiOutput <- renderUI({
    req(input$effect_con_btn)
    req(input$display_con)
    plotly::plotlyOutput(
      "con_plot",
      height = input$height_con_plot,
      width = input$width_con_plot
    )
  })

  observeEvent(input$display_con, {
    req(input$effect_con_btn)
    output$con_table <- DT::renderDataTable({
      if (is.null(effects_continuous_var()) || nrow(effects_continuous_var()) == 0) {
        return(DT::datatable(
          data.frame(Message = "No continuous effects available. Please run the effect estimation first."),
          rownames = FALSE,
          options = list(dom = "t", ordering = FALSE)
        ))
      }
      effects_continuous_var() %>%
        dplyr::select(topic, value, proportion, lower, upper) %>%
        dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            scrollX = TRUE,
            width = "100%",
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          )
        )
    })
  })

  output$con_table_uiOutput <- renderUI({
    req(input$effect_con_btn)
    req(input$display_con)
    htmltools::tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("con_table", width = input$width_con_plot)
    )
  })

  colnames_all <- reactive({
    req(united_tbl())
    names(united_tbl())
  })

  colnames_cat_doc <- reactive({
    req(united_tbl())
    categorical <- united_tbl() %>%
      dplyr::select(dplyr::where(is.character)) %>%
      dplyr::select(dplyr::where(~ dplyr::n_distinct(.) <= (0.2 * nrow(united_tbl()))))
    names(categorical)
  })

  observe({
    req(united_tbl())
    updateSelectizeInput(session,
                         "doc_id_var",
                         choices = c("None" = "", colnames_all()),
                         selected = ""
    )
  })

  observe({
    req(united_tbl())
    updateSelectizeInput(session,
                         "doc_category_var",
                         choices = c("None" = "", colnames_cat_doc()),
                         selected = ""
    )
  })

  observe({
    req(input$doc_category_var, input$doc_category_var != "")
    req(united_tbl())

    if (input$doc_category_var %in% names(united_tbl())) {
      categories <- unique(united_tbl()[[input$doc_category_var]])
      categories <- categories[!is.na(categories)]

      all_choices <- c("All Categories" = "all", setNames(categories, categories))

      updateSelectizeInput(session,
                           "heatmap_category_filter",
                           choices = all_choices,
                           selected = "all"
      )
    }
  })


  session$onSessionEnded(function() {
    stopApp()
  })
})
