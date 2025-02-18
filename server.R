suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(quanteda)
  library(quanteda.textstats)
  library(shiny)
  library(stm)
  library(tidyr)
  library(tidytext)
  library(ggraph)
  library(widyr)
  library(markdown)
  library(tidygraph)
  library(plotly)
  library(rlang)
  library(broom)
  library(igraph)
})

server <- shinyServer(function(input, output, session) {
  suppressMessages(spacyr::spacy_initialize(model = "en_core_web_sm"))

  observeEvent(input$dataset_choice, {
    if (input$dataset_choice == "Upload an Example Dataset") {
      shinyjs::disable("file")
      shinyjs::disable("text_input")
    } else if (input$dataset_choice == "Copy and Paste Text") {
      shinyjs::disable("file")
      shinyjs::enable("text_input")
    } else if (input$dataset_choice == "Upload Your File") {
      shinyjs::enable("file")
      shinyjs::disable("text_input")
    } else {
      shinyjs::disable("file")
      shinyjs::disable("text_input")
    }
  })

  mydata <- reactive({
    req(input$dataset_choice)

    if (input$dataset_choice == "Upload an Example Dataset") {
      return(TextAnalysisR::SpecialEduTech)
    } else if (input$dataset_choice == "Copy and Paste Text") {
      req(input$text_input)
      return(TextAnalysisR::process_files(input$dataset_choice, text_input = input$text_input))
    } else if (input$dataset_choice == "Upload Your File") {
      req(input$file)
      file_info <- data.frame(filepath = input$file$datapath, stringsAsFactors = FALSE)
      return(TextAnalysisR::process_files(input$dataset_choice, file_info = file_info))
    } else {
      return(NULL)
    }
  })

  output$data_table <- DT::renderDataTable({
    req(mydata())
    DT::datatable(mydata(), rownames = FALSE)
  })

  # Step 1: Unite texts.

  colnames <- reactive(names(mydata()))

  observeEvent(mydata(), {
    req(mydata())

    updateCheckboxGroupInput(
      session,
      "show_vars",
      choices = colnames(),
      selected = ""
    )
  })

  listed_vars <- eventReactive(input$show_vars, {
    input$show_vars
  })

  united_tbl <- eventReactive(input$apply, {

    req(listed_vars())
    req(mydata())

    selected_vars <- mydata() %>% dplyr::select(all_of(unname(listed_vars())))

    united_texts_tbl <- selected_vars %>%
      tidyr::unite(col = "united_texts", sep = " ", remove = TRUE)

    docvar_tbl <- mydata()

    united_data <- dplyr::bind_cols(united_texts_tbl, docvar_tbl %>% dplyr::select(-all_of(unname(listed_vars()))))

    return(united_data)
  })

  output$united_table <- DT::renderDataTable({
    req(input$apply)
    united_tbl()
  },
  rownames = FALSE
  )

  output$download_table <- downloadHandler(
    filename = function() {
      if (is.null(input$file)) {
        "combined_data.xlsx"
      } else {
        paste0(tools::file_path_sans_ext(input$file$name), ".xlsx")
      }
    },
    content = function(file) {
      openxlsx::write.xlsx(united_tbl(), file)
    }
  )

  # Step 2: Preprocess data.

  preprocessed_init <- eventReactive(eventExpr = input$preprocess, {

    pre_init_verbose_output <- capture.output({
      toks_processed <- TextAnalysisR::preprocess_texts(
        united_tbl(),
        text_field = "united_texts",
        min_char = 2,
        remove_punct = input$remove_punct,
        remove_symbols = input$remove_symbols,
        remove_numbers = input$remove_numbers,
        remove_url = input$remove_url,
        remove_separators = input$remove_separators,
        split_hyphens = input$split_hyphens,
        split_tags = input$split_tags,
        include_docvars = input$include_docvars,
        keep_acronyms = input$keep_acronyms,
        padding = input$padding,
        verbose = TRUE
      )
    }, type = "message")

    shiny::showModal(
      shiny::modalDialog(
        title = "Preprocessing Steps",
        shiny::verbatimTextOutput("modal_pre_init_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )

    output$modal_pre_init_verbose_output <- renderPrint({
      cat(gsub("\n\n", "\n", paste(pre_init_verbose_output, collapse = "\n")))
    })

    return(toks_processed)
  })

  output$dict_print_preprocess <- renderPrint({
    req(input$preprocess)
    preprocessed_init() %>% glimpse()
  })

  # Step 3: Process dictionary.

  observeEvent(preprocessed_init(), {
    tstat_collocation <- TextAnalysisR::detect_multi_word_expressions(
      preprocessed_init(),
      size = 2:5,
      min_count = 2
    )
    tstat_collocation_top_500 <- head(tstat_collocation, 500)
    tstat_collocation_top_20 <- head(tstat_collocation, 20)
    updateSelectizeInput(
      session,
      "multi_word_expressions",
      choices = tstat_collocation_top_500,
      selected = tstat_collocation_top_20,
      options = list(create = TRUE)
    )
  })

  processed_tokens <- eventReactive(input$dictionary, {
    req(preprocessed_init())

    if (is.null(input$multi_word_expressions) || length(input$multi_word_expressions) == 0) {
      return(preprocessed_init())
    } else {
      custom_dict <- dictionary(list(custom = input$multi_word_expressions))

      dict_verbose_output <- capture.output({
        toks_compound <- quanteda::tokens_compound(
          preprocessed_init(),
          pattern = custom_dict,
          concatenator = "_",
          valuetype = "glob",
          window = 0,
          case_insensitive = TRUE,
          join = TRUE,
          keep_unigrams = FALSE,
          verbose = TRUE
        )
      }, type = "message")

      shiny::showModal(
        shiny::modalDialog(
          title = "Dictionary Preprocessing Steps",
          shiny::verbatimTextOutput("modal_dict_verbose_output"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )

      output$modal_dict_verbose_output <- renderPrint({
        cat(gsub("\n\n", "\n", paste(dict_verbose_output, collapse = "\n")))
      })

      return(toks_compound)
    }
  })

  output$dict_print_dictionary <- renderPrint({
    req(processed_tokens())
    processed_tokens() %>% glimpse()
  })

  # Step 4: Construct a document-feature matrix.

  dfm_init <- eventReactive(input$dfm_btn, {
    req(processed_tokens())

    dfm_verbose_output <- capture.output({
      dfm_object <- processed_tokens() %>% quanteda::dfm()
    }, type = "message")

    doc_count     <- quanteda::ndoc(dfm_object)
    feature_count <- quanteda::nfeat(dfm_object)
    summary_info  <- capture.output({
      str(dfm_object)
    })

    combined_output <- c(
      dfm_verbose_output,
      paste("Document count:", doc_count),
      paste("Feature count:", feature_count),
      "Summary of dfm_object:",
      summary_info
    )

    shiny::showModal(
      shiny::modalDialog(
        title = "DFM Construction Steps",
        shiny::verbatimTextOutput("modal_dfm_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )

    output$modal_dfm_verbose_output <- renderPrint({
      cat(gsub("\n\n", "\n", paste(combined_output, collapse = "\n")))
    })

    dfm_object
  })

  output$dfm_plot <- plotly::renderPlotly({
    dfm_init() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$dfm_table <- DT::renderDataTable({
    quanteda.textstats::textstat_frequency(dfm_init())
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "80%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
  )

  # Step 5: Remove common words and stopwords.

  tstat_freq_dfm_init <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_init())
    tstat_freq$feature
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

  final_tokens <- eventReactive(input$remove, {
    req(processed_tokens())
    toks <- processed_tokens()

    if (!is.null(input$common_words) && length(input$common_words) > 0) {
      toks <- quanteda::tokens_remove(toks, pattern = input$common_words, verbose = FALSE)
    }

    if (isTRUE(input$leading_stopwords)) {
      toks <- quanteda::tokens(
        lapply(toks, function(token_vector) {
          sapply(token_vector, function(tok) {
            parts <- unlist(strsplit(tok, "_"))
            if (length(parts) > 1 &&
                tolower(parts[1]) %in% c(tolower(input$common_words), tolower(input$custom_stopwords))) {
              parts <- parts[-1]
            }
            paste(parts, collapse = "_")
          })
        })
      )
    }

    if (isTRUE(input$trailing_stopwords)) {
      toks <- quanteda::tokens(
        lapply(toks, function(token_vector) {
          sapply(token_vector, function(tok) {
            parts <- unlist(strsplit(tok, "_"))
            if (length(parts) > 1 &&
                tolower(tail(parts, n = 1)) %in% c(tolower(input$common_words), tolower(input$custom_stopwords))) {
              parts <- head(parts, -1)
            }
            paste(parts, collapse = "_")
          })
        })
      )
    }

    if (!is.null(input$custom_stopwords) && length(input$custom_stopwords) > 0) {
      toks <- quanteda::tokens_remove(toks, pattern = input$custom_stopwords, verbose = FALSE)
    }

    toks
  })

  dfm_init_updated <- eventReactive(input$remove, {
    req(final_tokens())
    dfm_obj <- quanteda::dfm(final_tokens())

    if (!is.null(quanteda::docvars(dfm_init()))) {
      quanteda::docvars(dfm_obj) <- quanteda::docvars(dfm_init())
    }

    doc_count     <- quanteda::ndoc(dfm_obj)
    feature_count <- quanteda::nfeat(dfm_obj)
    summary_info  <- capture.output(str(dfm_obj))
    combined_output <- c(
      paste("Document count:", doc_count),
      paste("Feature count:", feature_count),
      "Summary of dfm_object:",
      summary_info
    )

    shiny::showModal(
      shiny::modalDialog(
        title = "DFM Construction Steps (Stopwords Removal)",
        shiny::verbatimTextOutput("modal_dfm_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
    output$modal_dfm_verbose_output <- renderPrint({
      cat(paste(combined_output, collapse = "\n"))
    })

    dfm_obj
  })

  output$stopword_plot <- plotly::renderPlotly({
    req(input$remove)
    dfm_init_updated() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$stopword_table <- DT::renderDataTable({
    req(input$remove)
    quanteda.textstats::textstat_frequency(dfm_init_updated())
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "80%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))

  # Step 6: Lemmatize options.

  dfm_outcome <- eventReactive(input$lemma, {
    req(final_tokens())
    toks <- final_tokens()

    if (!is.null(input$lemma) && length(input$lemma) > 0) {
      texts <- sapply(toks, paste, collapse = " ")
      parsed <- spacyr::spacy_parse(x = texts, lemma = TRUE, entity = FALSE, pos = FALSE)
      toks <- quanteda::as.tokens(parsed, use_lemma = TRUE)
    }

    if (!is.null(input$remainder_tokens) && length(input$remainder_tokens) > 0) {
      toks <- quanteda::tokens_remove(toks, pattern = input$remainder_tokens, verbose = FALSE)
    }

    dfm_obj <- quanteda::dfm(toks)

    doc_count     <- quanteda::ndoc(dfm_obj)
    feature_count <- quanteda::nfeat(dfm_obj)
    summary_info  <- capture.output(str(dfm_obj))
    combined_output <- c(
      paste("Document count:", doc_count),
      paste("Feature count:", feature_count),
      "Summary of dfm_object:",
      summary_info
    )

    shiny::showModal(
      shiny::modalDialog(
        title = "DFM Construction Steps (Stemming & Lemmatization)",
        shiny::verbatimTextOutput("modal_lemma_verbose_output"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )
    output$modal_lemma_verbose_output <- renderPrint({
      cat(paste(combined_output, collapse = "\n"))
    })

    return(dfm_obj)
  })

  observe({
    req(dfm_outcome())
    freq <- quanteda.textstats::textstat_frequency(dfm_outcome())
    updateSelectizeInput(
      session,
      "remainder_tokens",
      choices  = freq$feature,
      selected = head(freq$feature, 10),
      options  = list(create = TRUE),
      server = TRUE
    )
  })

  output$lemma_plot <- plotly::renderPlotly({
    req(input$lemma)
    dfm_outcome() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$lemma_table <- DT::renderDataTable({
    req(input$lemma)
    quanteda.textstats::textstat_frequency(dfm_outcome())
  },
  rownames = FALSE,
  extensions = 'Buttons',
  options = list(
    scrollX = TRUE,
    scrollY = "400px",
    width = "80%",
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))


  # "Word Networks" page

  # 1. Visualize word co-occurrence network.

  colnames_cat <- reactive({
    req(mydata())
    categorical <- mydata() %>% select(where(is.character)) %>%
      select(where(~ n_distinct(.) <= (0.2 * nrow(mydata(
      )))))
    names(categorical)
  })

  observe({
    updateSelectizeInput(session,
                         "doc_var_co_occurrence",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None")
  })

  word_co_occurrence_network_results <- reactive({
    req(input$plot_word_co_occurrence_network)
    doc_var_input <- as.character(input$doc_var_co_occurrence)
    doc_var <- if (doc_var_input == "None") NULL else doc_var_input
    co_occur_n <- floor(as.numeric(input$co_occurence_number))
    top_node_n <- as.numeric(input$top_node_n_co_occurrence)
    nrows <- as.numeric(input$nrows_co_occurrence)

    word_co_occurrence_message <- capture.output({
      result <- TextAnalysisR::word_co_occurrence_network(
        dfm_object = dfm_outcome(),
        doc_var = doc_var,
        co_occur_n = co_occur_n,
        top_node_n = top_node_n,
        nrows = nrows,
        width = input$width_word_co_occurrence_network_plot,
        height = input$height_word_co_occurrence_network_plot
      )
    }, type = "message")

    if (length(word_co_occurrence_message) > 0 && any(nzchar(word_co_occurrence_message))) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Word Co-occurrence Message",
          shiny::verbatimTextOutput("word_co_occurrence_text"),
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )

      output$word_co_occurrence_text <- renderPrint({
        cat(paste(word_co_occurrence_message, collapse = "\n"))
      })
    }

    return(result)
  })

  output$word_co_occurrence_network_plot_uiOutput <- renderUI({
    req(input$plot_word_co_occurrence_network)
    word_co_occurrence_network_results()$plot
  })

  output$word_co_occurrence_network_table_uiOutput <- renderUI({
    req(input$plot_word_co_occurrence_network)
    word_co_occurrence_network_results()$table
  })

  output$word_co_occurrence_network_summary_uiOutput <- renderUI({
    req(input$plot_word_co_occurrence_network)
    word_co_occurrence_network_results()$summary
  })

  # 2. Visualize word correlation network.

  observe({
    updateSelectizeInput(session,
                         "doc_var_correlation",
                         choices = c("None" = "None", colnames_cat()),
                         selected = "None")
  })

word_correlation_network_results <- reactive({

  req(input$plot_word_correlation_network)
  doc_var_input <- as.character(input$doc_var_correlation)
  doc_var <- if (doc_var_input == "None") NULL else doc_var_input
  common_term_n <- floor(as.numeric(input$common_term_n))
  corr_n <- as.numeric(input$corr_n)
  top_node_n <- as.numeric(input$top_node_n_correlation)
  nrows <- as.numeric(input$nrows_correlation)

  word_correlation_message <- capture.output({
    result <- TextAnalysisR::word_correlation_network(
    dfm_object = dfm_outcome(),
    doc_var = doc_var,
    common_term_n = common_term_n,
    corr_n = corr_n,
    top_node_n = top_node_n,
    nrows = nrows,
    width = input$width_word_correlation_network_plot,
    height = input$height_word_correlation_network_plot
  )
  }, type = "message")

  if (length(word_correlation_message) > 0 && any(nzchar(word_correlation_message))) {
    shiny::showModal(
      shiny::modalDialog(
        title = "Word Correlation Message",
        shiny::verbatimTextOutput("word_correlation_text"),
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      )
    )

    output$word_correlation_text <- renderPrint({
      cat(paste(word_correlation_message, collapse = "\n"))
    })
  }

  return(result)
})

output$word_correlation_network_plot_uiOutput <- renderUI({
  req(input$plot_word_correlation_network)
  word_correlation_network_results()$plot
})

output$word_correlation_network_table_uiOutput <- renderUI({
  req(input$plot_word_correlation_network)
  word_correlation_network_results()$table
})

output$word_correlation_network_summary_uiOutput <- renderUI({
  req(input$plot_word_correlation_network)
  word_correlation_network_results()$summary
})

  # 3. Display selected word frequency distributions over a continuous variable.

colnames_con <- reactive({
  req(mydata())
  continuous <- mydata() %>% select(where(is.numeric))
  names(continuous)
})

  observe({
    updateSelectInput(session,
                      "continuous_var_3",
                      choices = colnames_con(),
                      selected = NULL)
  })

  tstat_freq_over_con_var <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_outcome())
    tstat_freq$feature
  })

  observe({
    updateSelectizeInput(
      session,
      "type_terms",
      choices = tstat_freq_over_con_var(),
      options = list(create = TRUE),
      selected = ""
    )
  })

  observeEvent(input$type_terms, {
    invisible(capture.output(print(input$type_terms)))
  })

  observeEvent(input$continuous_var_3, {
    invisible(capture.output(print(input$continuous_var_3)))
  })

  observeEvent(input$plot_term, {
    tryCatch({
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
        dfm_outcome_obj <- dfm_outcome()
        dfm_td <- tidytext::tidy(dfm_outcome())

        dfm_outcome_obj@docvars$document <- dfm_outcome_obj@docvars$docname_

        dfm_td <- dfm_td %>%
          left_join(dfm_outcome_obj@docvars,
                    by = c("document" = "document"))

        con_var_term_counts <- dfm_td %>%
          tibble::as_tibble() %>%
          group_by(!!rlang::sym(input$continuous_var_3)) %>%
          mutate(word_frequency = n()) %>%
          ungroup()

        output$line_con_var_plot <- plotly::renderPlotly({

          req(input$continuous_var_3)
          req(vm)

          con_var_term_gg <- con_var_term_counts %>%
            mutate(term = factor(term, levels = vm)) %>%
            mutate(across(where(is.numeric), ~ round(., 3))) %>%
            filter(term %in% vm) %>%
            ggplot(aes(
              x = !!rlang::sym(input$continuous_var_3),
              y = word_frequency,
              group = term
            )) +
            geom_point(color = "#337ab7", alpha = 0.6, size = 1) +
            geom_line(color = "#337ab7", alpha = 0.6, linewidth = 0.5) +
            facet_wrap(~ term, scales = "free") +
            ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
            labs(y = "Word Frequency") +
            theme_minimal(base_size = 11) +
            theme(
              legend.position = "none",
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
              axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
              strip.text.x = element_text(size = 11, color = "#3B3B3B", face = "bold"),
              axis.text.x = element_text(size = 11, color = "#3B3B3B"),
              axis.text.y = element_text(size = 11, color = "#3B3B3B"),
              axis.title = element_text(size = 11, color = "#3B3B3B"),
              axis.title.x = element_text(margin = margin(t = 9)),
              axis.title.y = element_text(margin = margin(r = 11))
            )

          plotly::ggplotly(
            con_var_term_gg,
            height = input$height_line_con_var_plot,
            width = input$width_line_con_var_plot
          ) %>%
            plotly::layout(
              margin = list(l = 40, r = 150, t = 60, b = 40)
            )
        })

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

            df <- .x %>%
              dplyr::mutate(
                word_frequency = as.numeric(word_frequency),
                !!continuous_var := as.numeric(!!rlang::sym(continuous_var))
              ) %>%
              dplyr::filter(is.finite(word_frequency) &
                              is.finite(!!rlang::sym(continuous_var)))

            if (length(unique(df$word_frequency)) <= 1) {
              return(tibble::tibble(term = NA, estimate = NA, std.error = NA,
                                    statistic = NA, p.value = NA,
                                    `odds ratio` = NA, var.diag = NA,
                                    `std.error (odds ratio)` = NA,
                                    model_type = "Insufficient data"))
            }

            if (length(unique(df[[continuous_var]])) <= 1) {
              return(tibble::tibble(term = NA, estimate = NA, std.error = NA,
                                    statistic = NA, p.value = NA,
                                    `odds ratio` = NA, var.diag = NA,
                                    `std.error (odds ratio)` = NA,
                                    model_type = "Insufficient data"))
            }

            if (nrow(df) < 2) {
              return(tibble::tibble(term = NA, estimate = NA, std.error = NA,
                                    statistic = NA, p.value = NA,
                                    `odds ratio` = NA, var.diag = NA,
                                    `std.error (odds ratio)` = NA,
                                    model_type = "Insufficient data"))
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
                `std.error (odds ratio)` = sqrt(`odds ratio`^2 * var.diag),
                model_type = model_type
              )

            return(tidy_result)
          }) %>%
          ungroup() %>%
          dplyr::select(word, model_type, term, estimate, std.error,
                        `odds ratio`, `std.error (odds ratio)`, statistic, p.value) %>%
          rename(
            logit = estimate,
            `z-statistic` = statistic
          )

        output$significance_results_table <- renderUI({

          if (nrow(significance_results) > 0) {

            tables <- significance_results %>%
              mutate(word = factor(word, levels = vm)) %>%
              arrange(word) %>%
              group_by(word) %>%
              group_map(~ {
                htmltools::tagList(
                  htmltools::tags$div(
                    style = "margin-bottom: 20px;",
                    htmltools::tags$p(
                      .y$word,
                      style = "font-weight: bold; text-align: center; font-size: 11pt;"
                    )
                  ),
                  .x %>%
                    mutate_if(is.numeric, ~ round(., 3)) %>%
                    DT::datatable(
                      rownames = FALSE,
                      extensions = 'Buttons',
                      options = list(
                        scrollX = TRUE,
                        width = "80%",
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      )
                    ) %>%
                    DT::formatStyle(
                      columns = names(.x),
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
    }, error = function(e) {
      warning_message <- paste(e$message)
      print(warning_message)
      shiny::showModal(shiny::modalDialog(
        title = "Error",
        warning_message,
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
      NULL
    })
  })

  output$line_con_var_plot_uiOutput <- renderUI({
    req(input$plot_term > 0, input$continuous_var_3, input$type_terms)
    htmltools::tagList(
      plotly::plotlyOutput(
        "line_con_var_plot",
        height = input$height_line_con_var_plot,
        width = input$width_line_con_var_plot
      ),
      htmltools::tags$div(
        style = paste0("margin-top: 20px; width: ", input$width_line_con_var_plot, "px;"),
        uiOutput("significance_results_table")
      )
    )
  })


  # "Structural Topic Model" page

  # 1. Search K.

  observe({
    updateSelectizeInput(session,
                         "categorical_var",
                         choices = colnames_cat(),
                         selected = "")
  })

  observe({
    updateSelectInput(session,
                      "continuous_var",
                      choices = colnames_con(),
                      selected = "")
  })

  K_range <- eventReactive(input$K_range, {
    seq(input$K_range[1], input$K_range[2])
  })

  observeEvent(eventExpr = input$categorical_var, {
    invisible(capture.output(print(input$categorical_var)))
  })

  observeEvent(eventExpr = input$continuous_var, {
    invisible(capture.output(print(input$continuous_var)))
  })

  out <- reactive({
    quanteda::convert(dfm_outcome(), to = "stm")
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

K_search <- eventReactive(input$search, {
  tryCatch({
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
      max.em.its = input$max_em_its_search
    )
  }, error = function(e) {
    warning_message <- paste(e$message)
    print(warning_message)
    shiny::showModal(shiny::modalDialog(
      title = "Error",
      warning_message,
      easyClose = TRUE,
      footer = shiny::modalButton("Close")
    ))
    NULL
  })
})

  output$topic_search_message <- renderUI({
    formula_text <- if (is.null(prevalence_formula_K_search())) {
      "No document-level covariates"
    } else {
      lines <- deparse(prevalence_formula_K_search())
      lines_no_indent <- sub("^\\s+", "", lines)
      paste(lines_no_indent, collapse = " ")
    }

    HTML(
      paste0(
        "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;'>",
        "<h4 style='color: #333;'>Model Information</h4>",
        "<p style='white-space: normal;'>",
        "<b>Prevalence formula = </b> ", formula_text,
        "</p>",
        "<p><b>Topic number (K) range = </b> ", paste0(min(K_range()), " to ", max(K_range())), "</p>",
        "</div>"
      )
    )
  })

 output$search_K_plot <- plotly::renderPlotly({
  search_result <- K_search()

  if (is.null(search_result)) {
    return(NULL)
  }

  print(search_result$results)

  search_result$results$heldout <- as.numeric(search_result$results$heldout)
  search_result$results$residual <- as.numeric(search_result$results$residual)
  search_result$results$semcoh <- as.numeric(search_result$results$semcoh)
  search_result$results$lbound <- as.numeric(search_result$results$lbound)

  p1 <- plotly::plot_ly(
    data = search_result$results,
    x = ~K,
    y = ~heldout,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Held-out Likelihood:", round(heldout, 3)),
    hoverinfo = 'text',
    width = 800,
    height = 600
  )

  p2 <- plotly::plot_ly(
    data = search_result$results,
    x = ~K,
    y = ~residual,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Residuals:", round(residual, 3)),
    hoverinfo = 'text',
    width = 800,
    height = 600
  )

  p3 <- plotly::plot_ly(
    data = search_result$results,
    x = ~K,
    y = ~semcoh,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Semantic Coherence:", round(semcoh, 3)),
    hoverinfo = 'text',
    width = 800,
    height = 600
  )

  p4 <- plotly::plot_ly(
    data = search_result$results,
    x = ~K,
    y = ~lbound,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Lower Bound:", round(lbound, 3)),
    hoverinfo = 'text',
    width = 800,
    height = 600
  )

  plotly::subplot(p1, p2, p3, p4, nrows = 2, margin = 0.1) %>%
    plotly::layout(
      title = list(
        text = "Model Diagnostics by Number of Topics (K)",
        font = list(size = 16)
      ),
      showlegend = FALSE,
      margin = list(t = 100, b = 150, l = 50, r = 50),
      annotations = list(
        list(
          x = 0.25, y = 1.05, text = "Held-out Likelihood", showarrow = FALSE,
          xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom',
          font = list(size = 14)
        ),
        list(
          x = 0.75, y = 1.05, text = "Residuals", showarrow = FALSE,
          xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom',
          font = list(size = 14)
        ),
        list(
          x = 0.25, y = 0.5, text = "Semantic Coherence", showarrow = FALSE,
          xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom',
          font = list(size = 14)
        ),
        list(
          x = 0.75, y = 0.5, text = "Lower Bound", showarrow = FALSE,
          xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'bottom',
          font = list(size = 14)
        ),
        list(
          x = 0.5, y = -0.2, text = "Number of Topics (K)", showarrow = FALSE,
          xref = 'paper', yref = 'paper', xanchor = 'center', yanchor = 'top',
          font = list(size = 14)
        )
      ),
      yaxis = list(
        title = list(
          text = "Metric Value",
          font = list(size = 14)
        )
      )
    )
})

  output$search_download_table <- downloadHandler(
    filename = function() {
      "topic_number_search_results.xlsx"
    },
    content = function(file) {
      search_result <- K_search()
      td <- search_result$results %>%
        mutate_if(is.numeric, ~ round(., 3))
      openxlsx::write.xlsx(td, file)
    }
  )

  # 2. Step 2: Run a model and display highest word probabilities for each labeled topic.

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

  observe({
    updateSelectizeInput(session,
                         "categorical_var_2",
                         choices = colnames_cat(),
                         server = TRUE)
  })

  observe({
    updateSelectInput(session,
                      "continuous_var_2",
                      choices = colnames_con(),
                      selected = " ")
  })

  observeEvent(input$categorical_var_2, {
    shiny::updateActionButton(session, "run", label = "Run")
  })

  observeEvent(input$continuous_var_2, {
    shiny::updateActionButton(session, "run", label = "Run")
  })

  observeEvent(input$K_number, {
    shiny::updateActionButton(session, "run", label = "Run")
  })


  stm_K_number <- reactiveVal(NULL)
  previous_K_number <- reactiveVal(NULL)
  previous_categorical_var_2 <- reactiveVal(NULL)
  previous_continuous_var_2 <- reactiveVal(NULL)
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
        "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;'>",
        "<h4 style='color: #333;'>Model Information</h4>",
        "<p style='white-space: normal;'>",
        "<b>Prevalence formula = </b> ", formula_text,
        "</p>",
        "<p><b>Topic number (K) = </b> ", input$K_number, "</p>",
        "</div>"
      )
    )
  })


  observeEvent(input$run, {
    print("Run button clicked: Checking if STM model needs to be updated...")

    req(input$K_number)

    model_update_needed <- (
      is.null(stm_K_number()) ||
        input$K_number != previous_K_number() ||
        (!is.null(input$categorical_var_2) && !identical(input$categorical_var_2, previous_categorical_var_2())) ||
        (!is.null(input$continuous_var_2) && !identical(input$continuous_var_2, previous_continuous_var_2()))
    )

    if (isTRUE(model_update_needed)) {
      print("Model parameters changed: Running or updating STM model...")

      categorical_var <- if (!is.null(input$categorical_var_2))
        unlist(strsplit(as.character(input$categorical_var_2), ",\\s*")) else NULL

      continuous_var <- if (!is.null(input$continuous_var_2))
        unlist(strsplit(as.character(input$continuous_var_2), ",\\s*")) else NULL

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

      tryCatch({
        stm_K_number(stm::stm(
          data = out()$meta,
          documents = out()$documents,
          vocab = out()$vocab,
          init.type = input$init_type_K,
          K = K_num,
          prevalence = prevalence_formula_K_n(),
          verbose = TRUE,
          gamma.prior = input$gamma_prior_K,
          sigma.prior = 0,
          kappa.prior = input$kappa_prior_K,
          max.em.its = input$max_em_its_K
        ))

        generated_labels(NULL)

        previous_K_number(input$K_number)
        previous_categorical_var_2(input$categorical_var_2)
        previous_continuous_var_2(input$continuous_var_2)

        output$output_message <- renderUI({
          HTML(paste0(
            "<div style='border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f9f9f9;'>",
            "<h4 style='color: #333;'>Model Information</h4>",
            "<p><b>Prevalence formula:</b> ",
            ifelse(is.null(prevalence_formula()), "No document-level covariates", deparse(prevalence_formula())),
            "</p>",
            "<p><b>Computing STM model with K =</b> ", input$K_number, "</p>",
            "</div>"
          ))
        })
      }, error = function(e) {
        warning_message <- paste(e$message)
        print(warning_message)
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          warning_message,
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
        NULL
      })

    } else {
      print("No significant parameter changes detected. STM model remains unchanged.")
      shiny::removeModal()
      shiny::showModal(shiny::modalDialog(
        title = "Notification",
        "No significant parameter changes detected. STM model remains unchanged.",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    }
  })

  output$text_output <- renderText({
    output_message()
  })

  generated_labels <- shiny::reactiveVal(NULL)

  beta_td <- reactive({
    req(stm_K_number())
    tidytext::tidy(stm_K_number(), matrix = "beta")
  })

  previous_system <- reactiveVal(NULL)
  previous_user <- reactiveVal(NULL)

  shiny::observeEvent(input$generate_labels, {
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

      if (Sys.getenv("SHINY_PORT") == "" && Sys.getenv("OPENAI_API_KEY") == "") {
        shiny::showModal(shiny::modalDialog(
          title = "OpenAI API Key Required",
          tagList(
            shiny::p("Please enter your OpenAI API key:"),
            shiny::textInput("openai_api_input", label = NULL, value = "", placeholder = "Enter API key here")
          ),
          footer = tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("submit_api_key", "Submit")
          ),
          easyClose = TRUE
        ))
        generated_labels(NULL)
        return()
      }

      shiny::showModal(shiny::modalDialog(
        title = "Generating Topic Labels",
        "Please wait while topic labels are being generated...",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))

      shiny::withProgress(message = 'Generating topic labels...', value = 0, {
        top_topic_terms <- beta_td() %>%
          dplyr::group_by(topic) %>%
          dplyr::top_n(input$top_term_number_1, beta) %>%
          dplyr::ungroup()

        new_labels_td <- TextAnalysisR::generate_topic_labels(
          top_topic_terms,
          system = input$system,
          user = input$user
        )

        generated_labels(tibble::as_tibble(new_labels_td))
        incProgress(1)

        previous_system(input$system)
        previous_user(input$user)

        shiny::removeModal()
        shiny::showNotification("Topic labels generated successfully!", type = "message", duration = 3)
      })
    }
  })

  shiny::observeEvent(input$submit_api_key, {
    if (nzchar(input$openai_api_input)) {
      Sys.setenv(OPENAI_API_KEY = input$openai_api_input)
      shiny::removeModal()
      shiny::showNotification("OpenAI API Key saved successfully for this session.", type = "message", duration = 3)
    } else {
      shiny::showNotification("Please enter a valid API key.", type = "error", duration = 3)
    }
  })

  output$topic_term_plot <- plotly::renderPlotly({
    shiny::req(beta_td())

    wrap_width <- reactive({
      if (is.null(input$width)) {
        return(30)
      } else if (input$width > 1000) {
        return(35)
      } else {
        return(25)
      }
    })

    topic_labels_td <- beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(input$top_term_number_1, beta) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(topic, .keep_all = TRUE) %>%
      dplyr::mutate(topic_label = paste("Topic", topic))

    topic_labels_td <- topic_labels_td %>%
      dplyr::mutate(topic_label = stringr::str_wrap(topic_label, width = wrap_width()))

    manual_labels <- if (input$label_topics != '') {
      strsplit(input$label_topics, split = ',')[[1]]
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

    topic_labels_td$topic_label <- stringr::str_wrap(topic_labels_td$topic_label, width = wrap_width())

    topic_term_plot <- beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(input$top_term_number_1, beta) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(term = as.character(term)) %>%
      dplyr::left_join(dplyr::distinct(topic_labels_td, topic, topic_label), by = "topic", relationship = "many-to-one") %>%
      dplyr::mutate(
        topic_label = factor(topic_label, levels = unique(topic_labels_td$topic_label)),
        term = tidytext::reorder_within(term, beta, topic_label)
      ) %>%
      dplyr::arrange(topic_label, dplyr::desc(beta))

    topic_term_plot_gg <- ggplot2::ggplot(
      topic_term_plot,
      ggplot2::aes(term, beta, fill = topic_label, text = paste("Topic:", topic_label, "<br>Beta:", sprintf("%.3f", beta)))
    ) +
      ggplot2::geom_col(show.legend = FALSE, alpha = 0.8) +
      ggplot2::facet_wrap(~ topic_label, scales = "free", ncol = input$ncol_top_terms, strip.position = "top") +
      tidytext::scale_x_reordered() +
      ggplot2::scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
      ggplot2::coord_flip() +
      ggplot2::xlab(" ") +
      ggplot2::ylab("Word Probability") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
        axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
        strip.text.x = element_text(
          size = 11,
          face = "bold",
          lineheight = ifelse(input$width > 1000, 1.1, 1.5),
          margin = margin(l = 20, r = 20)
        ),
        panel.spacing.x = unit(ifelse(input$width > 1000, 2.2, 1.6), "lines"),
        panel.spacing.y = unit(ifelse(input$width > 1000, 2.2, 1.6), "lines"),
        axis.text.x = element_text(size = 11, color = "#3B3B3B", hjust = 1, margin = margin(r = 20)),
        axis.text.y = element_text(size = 11, color = "#3B3B3B", margin = margin(t = 20)),
        axis.title = element_text(size = 11, color = "#3B3B3B"),
        axis.title.x = element_text(margin = margin(t = 25)),
        axis.title.y = element_text(margin = margin(r = 25)),
        plot.margin = margin(t = 40, b = 40, l = 100, r = 40)
      )

    plotly::ggplotly(topic_term_plot_gg, height = input$height, width = input$width, tooltip = "text") %>%
      plotly::layout(
        title = list(font = 11),
        margin = list(t = 40, b = 40)
      )
  })

  output$topic_term_plot_uiOutput <- renderUI({
    shinycssloaders::withSpinner(plotly::plotlyOutput(
      "topic_term_plot",
      height = input$height,
      width = input$width
    ))
  })

topic_table_data <- reactive({
  req(beta_td())

  current_wrap_width <- if (is.null(input$width)) {
    30
  } else if (input$width > 1000) {
    35
  } else {
    25
  }

  topic_labels_td <- beta_td() %>%
    dplyr::group_by(topic) %>%
    dplyr::top_n(input$top_term_number_1, beta) %>%
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
    dplyr::mutate_if(is.numeric, ~ round(., 3))

  df
})

output$topic_term_table <- DT::renderDataTable({
  topic_table_data() %>%
    DT::datatable(
      rownames = FALSE,
      width = input$width,
      extensions = 'Buttons',
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        width = "80%",
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
})

  output$topic_term_table_uiOutput <- renderUI({
    req(beta_td())
    htmltools::tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("topic_term_table", width = input$width)
    )
  })

output$topic_download_table <- downloadHandler(
  filename = function() {
    "topic_term_table.xlsx"
  },
  content = function(file) {
    openxlsx::write.xlsx(topic_table_data(), file)
  }
)

  # Step 3: Display per-document-per topic probabilities by topics.

  output$topic_number_uiOutput <- renderUI({
    sliderInput(
      "topic_number",
      "Choose the topic numbers to display.",
      value = K_number(),
      min = 0,
      step = 1,
      max = K_number()
    )
  })

  top_terms_selected <- reactive({
    topic_mapping <- topic_table_data() %>%
      dplyr::select(topic, topic_label) %>%
      dplyr::distinct()

    beta_td() %>%
      dplyr::group_by(topic) %>%
      dplyr::top_n(input$top_term_number_2, beta) %>%
      dplyr::arrange(beta) %>%
      dplyr::summarise(terms = paste(term, collapse = ", ")) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(topic_mapping, by = "topic")
  })

  gamma_terms <- reactive({
    gamma_td <- tidytext::tidy(
      stm_K_number(),
      matrix = "gamma",
      document_names = rownames(dfm_outcome())
    )

    topic_mapping <- topic_table_data() %>%
      dplyr::select(topic, topic_label) %>%
      dplyr::distinct()

    gamma_td %>%
      dplyr::group_by(topic) %>%
      dplyr::summarise(gamma = mean(gamma)) %>%
      dplyr::arrange(desc(gamma)) %>%
      dplyr::left_join(top_terms_selected(), by = "topic") %>%
      dplyr::left_join(topic_mapping, by = "topic") %>%
      dplyr::mutate(topic = reorder(topic, gamma))
  })

  observeEvent(input$display, {
    gamma_terms()

    if (input$label_topics == '') {
      tn = NULL
    } else{
      tn = strsplit(input$label_topics, split = ',')[[1]]
    }

    output$topic_by_prevalence_plot2 <- plotly::renderPlotly({
      topic_by_prevalence_plot <- gamma_terms() %>%
        top_n(input$topic_number, gamma) %>%
        mutate(tt = as.numeric(topic)) %>%
        mutate(ord = topic) %>%
        mutate(topic = paste('Topic', topic)) %>%  arrange(ord)
      levelt = paste("Topic", topic_by_prevalence_plot$ord) %>% unique()
      topic_by_prevalence_plot$topic = factor(topic_by_prevalence_plot$topic,
                                              levels = levelt)
      if (!is.null(tn)) {
        reft  = 1:length(topic_by_prevalence_plot$tt)
        topic_by_prevalence_plot$topic =
          tn[reft]
        topic_by_prevalence_plot <-
          topic_by_prevalence_plot %>%
          mutate(topic = as.character(topic)) %>%
          mutate(topic = ifelse(!is.na(topic), topic, paste('Topic', tt)))
        topic_by_prevalence_plot$topic =
          factor(topic_by_prevalence_plot$topic,
                 levels = topic_by_prevalence_plot$topic)
      }

      tp <- topic_by_prevalence_plot %>%
        ggplot(aes(topic, gamma, label = terms, fill = topic, text = paste("Topic:", topic, "<br>Terms:", terms, "<br>Gamma:", sprintf("%.3f", gamma)))
        ) +
        geom_col(alpha = 0.8) +
        coord_flip() +
        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
        xlab(" ") +
        ylab("Topic Proportion") +
        theme_minimal(base_size = 10) +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
          axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
          strip.text.x = element_text(size = 10, color = "#3B3B3B"),
          axis.text.x = element_text(size = 10, color = "#3B3B3B"),
          axis.text.y = element_text(size = 10, color = "#3B3B3B"),
          axis.title = element_text(size = 10, color = "#3B3B3B"),
          axis.title.x = element_text(margin = margin(t = 9)),
          axis.title.y = element_text(margin = margin(r = 9))
        )
      tp %>% plotly::ggplotly(tooltip = "text") %>%
        plotly::layout(
          margin = list(t = 40, b = 40)
        )
    })
  })

  output$topic_prevalence_plot_uiOutput <- renderUI({
    plotly::plotlyOutput(
      "topic_by_prevalence_plot2",
      height = input$height_topic_prevalence,
      width = input$width_topic_prevalence
    )
  })

  observeEvent(input$display, {
    if (input$label_topics == '') {
      tn <- NULL
    } else {
      tn <- strsplit(input$label_topics, split = ',')[[1]]
    }

    output$topic_by_prevalence_table <- DT::renderDataTable({
      topic_by_prevalence_table <- gamma_terms() %>%
        top_n(input$topic_number, gamma) %>%
        mutate(
          tt = as.numeric(topic),
          ord = tt
        ) %>%
        arrange(ord)

      if (!is.null(tn)) {
        topic_by_prevalence_table <- topic_by_prevalence_table %>%
          mutate(
            topic_label = ifelse(tt <= length(tn), tn[tt], paste('Topic', tt))
          )
      } else {
        topic_by_prevalence_table <- topic_by_prevalence_table %>%
          mutate(
            topic_label = paste('Topic', tt)
          )
      }

      topic_by_prevalence_table$topic_label <- factor(
        topic_by_prevalence_table$topic_label,
        levels = unique(topic_by_prevalence_table$topic_label)
      )

      topic_by_prevalence_table %>%
        select(topic_label, topic, gamma) %>%
        dplyr::arrange(as.numeric(topic)) %>%
        mutate_if(is.numeric, ~ round(., 3)) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            scrollY = "400px",
            width = "80%",
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        ) %>%
        DT::formatStyle(
          columns = c("topic", "gamma"),
          fontSize = '16px'
        )
    })

    output$topic_prevalence_table_uiOutput <- renderUI({
      htmltools::tags$div(
        style = "margin-top: 20px;",
        DT::dataTableOutput("topic_by_prevalence_table", width = input$width_topic_prevalence)
      )
    })

  })

  # Step 4: Explore example documents for each topic (display quotes).

  print_K_number_from_1 <-
    eventReactive(eventExpr = input$K_number, {
      print(1:input$K_number)
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

  observeEvent(eventExpr = input$topic_number_quote, {
    invisible(capture.output(print(input$topic_number_quote)))
  })

  colnames_cat_3 <- reactive({
    categorical <- mydata() %>% select(where(is.character))
    names(categorical)
  })

  observe({
    updateSelectizeInput(session,
                         "topic_texts",
                         choices = colnames_cat_3(),
                         selected = " ")
  })

  observeEvent(eventExpr = input$topic_texts, {
    invisible(capture.output(print(input$topic_texts)))
  })

  thoughts <- eventReactive(eventExpr = input$quote, {
    pn <- input$topic_number_quote %>% as.numeric()
    tn <- input$topic_texts

    stm::findThoughts(
      stm_K_number(),
      texts = out()$meta[[tn]],
      n = 3,
      topics = pn
    )$docs[[1]]
  })

  output$quote_table <- DT::renderDataTable({
    thoughts() %>% tibble()
  },
  rownames = FALSE
  )

  # Step 5: Display estimates regressions based on a STM object.

  effect_stm_K_number <- eventReactive(eventExpr = input$effect, {
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
  })

  observe({
    effect_stm_K_number_td <- effect_stm_K_number()

    if (!is.null(effect_stm_K_number_td)) {
      td <- tidytext::tidy(effect_stm_K_number_td) %>%
        mutate_if(is.numeric, ~ round(., 3))

      output$effect_table <- DT::renderDataTable({ td },
                                                 rownames = FALSE
      )
    }
  })

  output$effect_download_table <- downloadHandler(
    filename = function() {
      paste0(ifelse(is.null(input$file) || input$file == "", "estimated regression data", input$file), ".xlsx")
    },
    content = function(file) {
      td <- tidytext::tidy(effect_stm_K_number()) %>%
        mutate_if(is.numeric, ~ round(., 3))
      openxlsx::write.xlsx(td, file)
    }
  )


  # Step 6: Plot topic prevalence effects by a categorical variable.

  observe({
    selected_cat <- input$categorical_var_2
    updateSelectInput(session,
                      "effect_cat_btn",
                      choices = selected_cat,
                      selected = if (!is.null(selected_cat) && length(selected_cat) > 0) selected_cat[1] else NULL)
  })

  effects_categorical_var <- reactive({
    stminsights::get_effects(
      estimates = effect_stm_K_number(),
      variable = input$effect_cat_btn,
      type = 'pointestimate'
    )
  })

  observeEvent(input$display_cat, {

    output$cat_plot <- plotly::renderPlotly({
      req(effects_categorical_var())

      effects_categorical_var_gg <- effects_categorical_var() %>%
        dplyr::mutate(topic_label = paste("Topic", topic)) %>%
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap(~ topic_label, ncol = input$ncol_cat, scales = "free") +
        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
        xlab("") +
        ylab("Topic proportion") +
        geom_errorbar(
          aes(ymin = lower, ymax = upper),
          width = 0.1,
          linewidth = 0.5,
          color = "#337ab7"
        ) +
        geom_point(color = "#337ab7", size = 1.5) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
          axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
          strip.text.x = element_text(size = 11, color = "#3B3B3B", margin = margin(b = 30, t = 15)),
          axis.text.x = element_text(size = 11, color = "#3B3B3B", hjust = 1, margin = margin(t = 20)),
          axis.text.y = element_text(size = 11, color = "#3B3B3B", margin = margin(r = 20)),
          axis.title = element_text(size = 11, color = "#3B3B3B"),
          axis.title.x = element_text(margin = margin(t = 25)),
          axis.title.y = element_text(margin = margin(r = 25)),
          plot.margin = margin(t = 40, b = 40)
        )

      plotly::ggplotly(effects_categorical_var_gg,
                       height = input$height_cat_plot,
                       width = input$width_cat_plot
      ) %>%
        plotly::layout(
          margin = list(t = 40, b = 40)
        )
    })
  })

  output$cat_plot_uiOutput <- renderUI({
    req(input$effect_cat_btn)
    req(input$display_cat)
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        "cat_plot",
        height = input$height_cat_plot,
        width = input$width_cat_plot
      )
    )
  })

  observeEvent(input$display_cat, {
    req(input$effect_cat_btn)
    output$cat_table <- DT::renderDataTable({
      req(effects_categorical_var())
      effects_categorical_var() %>%
        dplyr::select(topic, value, proportion, lower, upper) %>%
        mutate_if(is.numeric, ~ round(., 3)) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            scrollY = "400px",
            width = "80%",
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
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
    updateSelectInput(session,
                      "effect_con_btn",
                      choices = selected_con,
                      selected = if (!is.null(selected_con) && length(selected_con) > 0) selected_con[1] else NULL)
  })

  effects_continuous_var <- reactive({
    stminsights::get_effects(
      estimates = effect_stm_K_number(),
      variable = input$effect_con_btn,
      type = 'continuous'
    )
  })

  observeEvent(input$display_con, {

    output$con_plot <- plotly::renderPlotly({
      req(effects_continuous_var())

      effects_continuous_var_gg <- effects_continuous_var() %>%
        dplyr::mutate(topic_label = paste("Topic", topic)) %>%
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap(~ topic_label, ncol = input$ncol_con, scales = "free") +
        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#337ab7", alpha = 0.2) +
        geom_line(linewidth = 0.5, color = "#337ab7") +
        xlab("") +
        ylab("Topic proportion") +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
          axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
          strip.text.x = element_text(size = 11, color = "#3B3B3B", margin = margin(b = 30, t = 15)),
          axis.text.x = element_text(size = 11, color = "#3B3B3B", hjust = 1, margin = margin(t = 20)),
          axis.text.y = element_text(size = 11, color = "#3B3B3B", margin = margin(r = 20)),
          axis.title = element_text(size = 11, color = "#3B3B3B"),
          axis.title.x = element_text(margin = margin(t = 25)),
          axis.title.y = element_text(margin = margin(r = 25)),
          plot.margin = margin(t = 40, b = 40)
        )

      plotly::ggplotly(effects_continuous_var_gg,
                       height = input$height_con_plot,
                       width = input$width_con_plot
      ) %>%
        plotly::layout(
          margin = list(t = 40, b = 40)
        )
    })
  })

  output$con_plot_uiOutput <- renderUI({
    req(input$effect_con_btn)
    req(input$display_con)
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        "con_plot",
        height = input$height_con_plot,
        width = input$width_con_plot
      )
    )
  })

  observeEvent(input$display_con, {
    req(input$effect_con_btn)
    output$con_table <- DT::renderDataTable({
      req(effects_continuous_var())
      effects_continuous_var() %>%
        dplyr::select(topic, value, proportion, lower, upper) %>%
        mutate_if(is.numeric, ~ round(., 3)) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            scrollY = "400px",
            width = "80%",
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
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

  session$onSessionEnded(function() {
    spacyr::spacy_finalize()
    Sys.unsetenv("OPENAI_API_KEY")
    shiny::showNotification("OpenAI API Key has been removed from the environment.", type = "message", duration = 3)
    stopApp()
  })
})
