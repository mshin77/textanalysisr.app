
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(quanteda)
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

  observeEvent(input$dataset_choice, {
    if (input$dataset_choice == "Upload an Example Dataset") {
      shinyjs::disable("file")
    } else {
      shinyjs::enable("file")
    }
  })

  mydata <- reactive({
    if (input$dataset_choice == "Upload an Example Dataset") {
      data <- TextAnalysisR::SpecialEduTech
    } else {
      req(input$file)
      filename <- input$file$datapath
      tryCatch({
        if (grepl("*[.]xlsx$|[.]xls$|[.]xlsm$", filename)) {
          data <- as.data.frame(readxl::read_excel(filename))
        } else if (grepl("*[.]csv$|*", filename)) {
          data <- read.csv(filename)
        }
      })
    }
    return(data)
  })

  output$data_table <- DT::renderDataTable({
    req(mydata())
    DT::datatable(mydata(),
                  rownames = FALSE)
  })

  # "Preprocess" page
  # Step 1: Unite texts
  # Display checkbox

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

  # Select one or multiple columns for text data pre-processing
  united_tbl <- eventReactive(input$apply, {

    req(listed_vars())
    req(mydata())

    united_texts_tbl <- mydata() %>%
      dplyr::select(all_of(unname(listed_vars()))) %>%
      tidyr::unite(col = "united_texts", sep = " ", remove = TRUE)

    docvar_tbl <- mydata()

    dplyr::bind_cols(united_texts_tbl, docvar_tbl)
  })

  output$step1_table <- DT::renderDataTable({
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

  # Step 2
  # Preprocess data
  processed_tokens <-
    eventReactive(eventExpr = input$preprocess, {
      united_tbl() %>% TextAnalysisR::preprocess_texts()
    })

  output$step2_print_preprocess <- renderPrint({
    req(input$preprocess)
    processed_tokens() %>% glimpse()
  })

  # Step 3
  # Apply researcher-developed dictionaries
  tokens_dict <- eventReactive(input$dictionary, {
    dictionary_list_1 <- TextAnalysisR::dictionary_list_1
    dictionary_list_2 <- TextAnalysisR::dictionary_list_2

    tokens_dict_int <-
      processed_tokens() %>% quanteda::tokens_lookup(
        dictionary = dictionary(dictionary_list_1),
        valuetype = "glob",
        verbose = FALSE,
        exclusive = FALSE,
        capkeys = FALSE
      )

    tokens_dict_int %>% quanteda::tokens_lookup(
      dictionary = dictionary(dictionary_list_2),
      valuetype = "glob",
      verbose = FALSE,
      exclusive = FALSE,
      capkeys = FALSE
    )
  })

  output$step2_print_dictionary <- renderPrint({
    tokens_dict()
  })

  # Step 4
  # Remove researcher-developed stop words.

  stopwords_list <- TextAnalysisR::stopwords_list

  tokens_dict_no_stop <- eventReactive(input$stopword, {
    tokens_dict() %>% quanteda::tokens_remove(stopwords_list)
  })

  output$step2_print_stopword <- renderPrint({
    req(input$stopword)
    tokens_dict_no_stop() %>% glimpse()
  })

  # Step 5
  # Construct a document-feature matrix
  dfm_init <- eventReactive(eventExpr = input$dfm_btn, {
    dfm_init <- tokens_dict_no_stop() %>% quanteda::dfm()
  })

  output$step3_plot <- plotly::renderPlotly({
    dfm_init() %>% TextAnalysisR::plot_word_frequency(n = 20)
  })

  output$step3_table <- DT::renderDataTable({
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

  # Step 6
  # Display the most frequent words (top 20)
  top_frequent_word  <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_init())
    tstat_freq_n_20 <- utils::head(tstat_freq, 20)
    top_frequent_word <- tstat_freq_n_20$feature
  })

  observe({
    updateSelectizeInput(
      session,
      "remove.var",
      choices = top_frequent_word(),
      options = list(create = TRUE),
      selected = ""
    )
  })

  # Remove common words across documents
  dfm_outcome <- reactive({
    dictionary_list_1 <- TextAnalysisR::dictionary_list_1
    dictionary_list_2 <- TextAnalysisR::dictionary_list_2

    invisible(capture.output(print(input$remove)))

    rm <- isolate(input$remove.var)

    if (!is.null(rm)) {
      removed_processed_tokens <-
        quanteda::tokens_remove(tokens_dict_no_stop(), rm)

      removed_tokens_dict_int <- removed_processed_tokens %>%
        quanteda::tokens_lookup(
          dictionary = dictionary(dictionary_list_1),
          valuetype = "glob",
          verbose = FALSE,
          exclusive = FALSE,
          capkeys = FALSE
        )

      removed_tokens_dict <- removed_tokens_dict_int %>%
        quanteda::tokens_lookup(
          dictionary = dictionary(dictionary_list_2),
          valuetype = "glob",
          verbose = FALSE,
          exclusive = FALSE,
          capkeys = FALSE
        )

      removed_tokens_dict %>% quanteda::dfm()

    } else{
      dfm_init()
    }
  })

  output$step4_plot <- plotly::renderPlotly({

    req(input$remove)
    req(input$remove.var)

    dfm_outcome() %>% TextAnalysisR::plot_word_frequency(n = 20)

  })

  output$step4_table <- DT::renderDataTable({

    req(input$remove)
    req(input$remove.var)

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
  )
  )


  # "Structural Topic Model" page

  # 1. Search K

  colnames_cat <- reactive({
    categorical <- mydata() %>% select(where(is.character)) %>%
      select(where(~ n_distinct(.) <= (0.2 * nrow(mydata(
      )))))
    names(categorical)
  })

  observe({
    updateSelectizeInput(session,
                         "categorical_var",
                         choices = colnames_cat(),
                         selected = "")
  })

  colnames_con <- reactive({
    continuous <- mydata() %>% select(which(sapply(., is.numeric)))
    names(continuous)
  })

  observe({
    updateSelectInput(session,
                      "continuous_var",
                      choices = colnames_con(),
                      selected = "")
  })

  print_K_range_1 <- eventReactive(eventExpr = input$K_range_1, {
    print(input$K_range_1[1]:input$K_range_1[2])
  })

  observeEvent(eventExpr = input$categorical_var, {
    print(input$categorical_var)
  })

  observeEvent(eventExpr = input$continuous_var, {
    print(input$continuous_var)
  })

  out <- reactive({
    quanteda::convert(dfm_outcome(), to = "stm")
  })

  K_search <- eventReactive(eventExpr = input$search, {

    categorical_var <- if (!is.null(input$categorical_var)) as.character(input$categorical_var) else NULL
    continuous_var <- if (!is.null(input$continuous_var)) as.character(input$continuous_var) else NULL

    if (!is.null(categorical_var)) {
      categorical_var <- unlist(strsplit(categorical_var, ",\\s*"))
    }
    if (!is.null(continuous_var)) {
      continuous_var <- unlist(strsplit(continuous_var, ",\\s*"))
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

    prevalence_formula <- if (length(terms) > 0) {
      as.formula(paste("~", paste(terms, collapse = " + ")))
    } else {
      NULL
    }

    stm::searchK(
      data = out()$meta,
      documents = out()$documents,
      vocab = out()$vocab,
      max.em.its = 75,
      init.type = "Spectral",
      K = print_K_range_1(),
      prevalence = prevalence_formula,
      verbose = TRUE
    )
  })


  output$search_K_plot <- plotly::renderPlotly({

    search_result <- K_search()

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


  # 2. Step 2: Run a model and display highest word probabilities for each labeled topic
  output$K_number_uiOutput <- renderUI({
    sliderInput(
      "K_number",
      "Choose K (number of topics)",
      value = 10,
      min = 0,
      max = 50
    )
  })

  print_K_number <- eventReactive(eventExpr = input$K_number, {
    print(input$K_number)
  })

  colnames_cat_2 <- reactive({
    categorical_2 <- mydata() %>% select(where(is.character)) %>%
      select(where(~ n_distinct(.) <= (0.2 * nrow(mydata(
      )))))
    names(categorical_2)
  })

  observe({
    updateSelectizeInput(session,
                         "categorical_var_2",
                         choices = colnames_cat(),
                         server = TRUE)
  })

  colnames_con_2 <- reactive({
    continuous_2 <- mydata() %>% select(which(sapply(., is.numeric)))
    names(continuous_2)
  })

  observe({
    updateSelectInput(session,
                      "continuous_var_2",
                      choices = colnames_con(),
                      selected = " ")
  })

  observeEvent(eventExpr = input$categorical_var_2, {
    print(input$categorical_var_2)
  })

  observeEvent(eventExpr = input$continuous_var_2, {
    print(input$continuous_var_2)
  })

  stm_K_number <- eventReactive(eventExpr = input$run, {

    categorical_var <- if (!is.null(input$categorical_var_2)) unlist(strsplit(as.character(input$categorical_var_2), ",\\s*")) else NULL
    continuous_var <- if (!is.null(input$continuous_var_2)) unlist(strsplit(as.character(input$continuous_var_2), ",\\s*")) else NULL

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

    stm::stm(
      data = out()$meta,
      documents = out()$documents,
      vocab = out()$vocab,
      max.em.its = 75,
      init.type = "Spectral",
      K = print_K_number(),
      prevalence = prevalence_formula,
      verbose = TRUE
    )
  })



  # Display highest word probabilities for each topic

  # Tidy the word-topic combinations
  beta_td <- reactive({
    tidytext::tidy(stm_K_number(), document_names = rownames(dfm_outcome()))
  })

  observe({
    invisible(capture.output(print(input$label_topics)))
  })

  output$topic_term_plot <- plotly::renderPlotly({
    invisible(capture.output(print(input$top_term_number_1)))
    req(!is.na(stm_K_number()))

    if (input$label_topics == '') {
      tn = NULL
    } else {
      tn = strsplit(input$label_topics, split = ',')[[1]]
    }

    topic_term_plot <- beta_td() %>%
      group_by(topic) %>%
      top_n(input$top_term_number_1, beta) %>%
      ungroup() %>%
      mutate(
        ord = factor(topic, levels = c(min(topic):max(topic))),
        tt = as.numeric(topic),
        topic = paste("Topic", topic),
        term = reorder_within(term, beta, topic)
      ) %>% arrange(ord)

    levelt = paste("Topic", topic_term_plot$ord) %>% unique()
    topic_term_plot$topic = factor(topic_term_plot$topic,
                                   levels = levelt)
    if (!is.null(tn)) {
      topic_term_plot$topic = tn[topic_term_plot$tt]
      topic_term_plot <- topic_term_plot %>%
        mutate(topic = as.character(topic)) %>%
        mutate(topic = ifelse(!is.na(topic), topic, paste('Topic', tt)))
      topic_term_plot$topic =
        factor(topic_term_plot$topic, levels = topic_term_plot$topic %>% unique())
    }
    topic_term_plot$tt = NULL

    topic_term_plot_gg <- ggplot(
      topic_term_plot,
      aes(term, beta, fill = topic, text = paste("Topic:", topic, "<br>Beta:", sprintf("%.3f", beta)))
    ) +
      geom_col(show.legend = FALSE, alpha = 0.8) +
      facet_wrap(~ topic, scales = "free", ncol = 2, strip.position = "top") +
      scale_x_reordered() +
      scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
      coord_flip() +
      xlab(" ") +
      ylab("Word probability") +
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

    plotly::ggplotly(topic_term_plot_gg, height = input$height, width = input$width, tooltip = "text") %>%
      plotly::layout(
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



  # Step 3: Display per-document-per topic probabilities by topics

  output$topic_number_uiOutput <- renderUI({
    req(print_K_number())
    sliderInput(
      "topic_number",
      "Choose the topic numbers to display",
      value = print_K_number(),
      min = 0,
      step = 1,
      max = print_K_number()
    )
  })

  top_terms_selected <- reactive({
    beta_td() %>%
      arrange(beta) %>%
      group_by(topic) %>%
      top_n(input$top_term_number_2, beta) %>%
      arrange(beta) %>%
      select(topic, term) %>%
      summarise(terms = list(term)) %>%
      mutate(terms = purrr::map(terms, paste, collapse = ", ")) %>%
      unnest(cols = c(terms))
  })

  gamma_terms <- reactive({
    gamma_td = tidytext::tidy(
      stm_K_number(),
      matrix = "gamma",
      document_names = rownames(dfm_outcome())
    )
    gamma_td %>%
      group_by(topic) %>%
      summarise(gamma = mean(gamma)) %>%
      arrange(desc(gamma)) %>%
      left_join(top_terms_selected(), by = "topic") %>%
      mutate(topic = reorder(topic, gamma))
  })


  # Visualize per-document-per topic probabilities ----

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
        ylab("Topic proportion") +
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

  # Display topics ordered by prevalence

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
        select(topic = topic_label, gamma) %>%
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
  })


  # Step 4: Explore example documents for each topic (display quotes)
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

    td <- tidytext::tidy(effect_stm_K_number_td) %>%
      mutate_if(is.numeric, ~ round(., 3))

    output$effect_table <- DT::renderDataTable({ td
    },
    rownames = FALSE
    )
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

  observeEvent(eventExpr = input$effect_cat_btn, {
    # print(input$effect_cat_btn)
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
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap(~ topic, ncol = 2, scales = "free") +
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
    tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("cat_table")
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

  observeEvent(input$effect_con_btn, {
    # print(input$effect_con_btn)
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

      effects_continuous_var_gg <-  effects_continuous_var() %>%
        ggplot(aes(x = value, y = proportion)) +
        facet_wrap( ~ topic,
                    ncol = 2,
                    scales = "free") +
        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#337ab7", alpha = 0.2) +
        geom_line(linewidth = 0.5,
                  color = "#337ab7") +
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
    tags$div(
      style = "margin-top: 20px;",
      DT::dataTableOutput("con_table")
    )
  })

  # "Word Networks" page

  # 1. Visualize word co-occurrence network

  output$word_co_occurrence_network_plot <- renderPlotly({

    req(input$plot_word_co_occurrence_network, dfm_outcome())
    req(input$top_node_n_co_occurrence)

    top_node_n <- as.numeric(input$top_node_n_co_occurrence)

    dfm_td <- tidytext::tidy(dfm_outcome()) %>%
      tibble::as_tibble()

    co_occur_n <- floor(as.numeric(input$co_occurence_number_init))

    term_co_occur <- dfm_td %>%
      count(document, term) %>%
      widyr::pairwise_count(term, document, sort = TRUE) %>%
      filter(n >= co_occur_n)

    co_occur_graph <- igraph::graph_from_data_frame(term_co_occur, directed = FALSE)

    if (igraph::vcount(co_occur_graph) == 0) {
      showNotification("No co-occurrence relationships meet the threshold.", type = "error")
      return(NULL)
    }

    igraph::V(co_occur_graph)$degree <- igraph::degree(co_occur_graph)
    igraph::V(co_occur_graph)$betweenness <- igraph::betweenness(co_occur_graph)
    igraph::V(co_occur_graph)$closeness <- igraph::closeness(co_occur_graph)
    igraph::V(co_occur_graph)$eigenvector <- igraph::eigen_centrality(co_occur_graph)$vector
    igraph::V(co_occur_graph)$community <- igraph::cluster_leiden(co_occur_graph)$membership

    layout <- igraph::layout_with_fr(co_occur_graph)
    layout_df <- as.data.frame(layout)
    colnames(layout_df) <- c("x", "y")

    layout_df$label <- igraph::V(co_occur_graph)$name
    layout_df$degree <- igraph::V(co_occur_graph)$degree
    layout_df$betweenness <- igraph::V(co_occur_graph)$betweenness
    layout_df$closeness <- igraph::V(co_occur_graph)$closeness
    layout_df$eigenvector <- igraph::V(co_occur_graph)$eigenvector
    layout_df$community <- igraph::V(co_occur_graph)$community

    edge_data <- igraph::as_data_frame(co_occur_graph, what = "edges") %>%
      dplyr::mutate(
        x = layout_df$x[match(from, layout_df$label)],
        y = layout_df$y[match(from, layout_df$label)],
        xend = layout_df$x[match(to, layout_df$label)],
        yend = layout_df$y[match(to, layout_df$label)],
        cooccur_count = n
      ) %>%
      dplyr::select(from, to, x, y, xend, yend, cooccur_count)

    edge_data <- edge_data %>%
      dplyr::mutate(
        line_group = as.integer(cut(
          cooccur_count,
          breaks = unique(quantile(cooccur_count, probs = seq(0, 1, length.out = 6), na.rm = TRUE)),
          include.lowest = TRUE
        )),
        line_width = scales::rescale(line_group, to = c(1, 5)),
        alpha = scales::rescale(line_group, to = c(0.1, 0.3))
      )

    edge_group_labels <- edge_data %>%
      dplyr::group_by(line_group) %>%
      dplyr::summarise(
        min_count = min(cooccur_count, na.rm = TRUE),
        max_count = max(cooccur_count, na.rm = TRUE)
      ) %>%
      dplyr::mutate(label = paste0("Count: ", min_count, " - ", max_count)) %>%
      dplyr::pull(label)

    node_data <- layout_df %>%
      dplyr::mutate(
        degree_log = log1p(degree),
        size = scales::rescale(degree_log, to = c(12, 30)),
        text_size = scales::rescale(degree_log, to = c(14, 20)),
        alpha = scales::rescale(degree_log, to = c(0.2, 1)),
        hover_text = paste(
          "Word:", label,
          "<br>Degree:", degree,
          "<br>Betweenness:", round(betweenness, 2),
          "<br>Closeness:", round(closeness, 2),
          "<br>Eigenvector:", round(eigenvector, 2),
          "<br>Community:", community
        )
      )

    n_communities <- length(unique(node_data$community))
    if (n_communities >= 3 && n_communities <= 8) {
      palette <- RColorBrewer::brewer.pal(n_communities, "Set2")
    } else if (n_communities > 8) {
      palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_communities)
    } else if (n_communities > 0 && n_communities < 3) {
      palette <- RColorBrewer::brewer.pal(3, "Set2")[1:n_communities]
    } else {
      palette <- rep("#000000", n_communities)
    }

    node_data$community <- factor(node_data$community, levels = unique(node_data$community))
    community_levels <- levels(node_data$community)
    names(palette) <- community_levels
    node_data$color <- palette[as.character(node_data$community)]

    plot <- plotly::plot_ly(
      type = 'scatter',
      mode = 'markers',
      width = input$width_word_co_occurrence_network_plot,
      height = input$height_word_co_occurrence_network_plot
    )

    for (i in unique(edge_data$line_group)) {

      edge_subset <- edge_data %>% dplyr::filter(line_group == i)
      edge_label <- edge_group_labels[i]

      edge_subset <- edge_subset %>%
        dplyr::mutate(
          mid_x = (x + xend) / 2,
          mid_y = (y + yend) / 2
        )

      if (nrow(edge_subset) > 0) {
        plot <- plot %>%
          plotly::add_segments(
            data = edge_subset,
            x = ~x,
            y = ~y,
            xend = ~xend,
            yend = ~yend,
            line = list(
              color = '#5C5CFF',
              width = ~line_width
            ),
            hoverinfo = 'none',
            opacity = ~alpha,
            showlegend = TRUE,
            name = edge_label,
            legendgroup = "Edges"
          ) %>%

          plotly::add_trace(
            data = edge_subset,
            x = ~mid_x,
            y = ~mid_y,
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 0.1, color = '#e0f7ff', opacity = 0),
            text = ~paste0(
              "Co-occurrence: ", cooccur_count,
              "<br>Source: ", from,
              "<br>Target: ", to
            ),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
    }

    plot <- plot %>%
      plotly::layout(
        legend = list(
          title = list(text = "Co-occurrence"),
          orientation = "v",
          x = 1.1,
          y = 1,
          xanchor = "left",
          yanchor = "top"
        )
      )

    marker_params <- list(
      size = ~size,
      showscale = FALSE,
      line = list(width = 3, color = '#FFFFFF')
    )

    for (n in community_levels) {
      community_data <- node_data[node_data$community == n, ]

      plot <- plot %>%
        plotly::add_markers(
          data = community_data,
          x = ~x,
          y = ~y,
          marker = list(
            size = ~size,
            color = palette[n],
            showscale = FALSE,
            line = list(width = 3, color = '#FFFFFF')
          ),
          hoverinfo = 'text',
          text = ~hover_text,
          showlegend = TRUE,
          name = paste("Community", n),
          legendgroup = "Community"
        )
    }

    top_nodes <- head(node_data[order(-node_data$degree), ], top_node_n)

    annotations <- if (nrow(top_nodes) > 0) {
      lapply(1:nrow(top_nodes), function(i) {
        label <- top_nodes$label[i]
        text <- ifelse(!is.na(label) & label != "", label, "")

        list(
          x = top_nodes$x[i],
          y = top_nodes$y[i],
          text = text,
          xanchor = ifelse(!is.na(top_nodes$x[i]) & top_nodes$x[i] > 0, "left", "right"),
          yanchor = ifelse(!is.na(top_nodes$y[i]) & top_nodes$y[i] > 0, "bottom", "top"),
          xshift = ifelse(!is.na(top_nodes$x[i]) & top_nodes$x[i] > 0, 5, -5),
          yshift = ifelse(!is.na(top_nodes$y[i]) & top_nodes$y[i] > 0, 3, -3),
          showarrow = FALSE,
          font = list(size = top_nodes$text_size[i], color = 'black')
        )
      })
    } else {
      list()
    }

    word_co_occurrence_plotly <- plot %>%
      plotly::layout(
        dragmode = "pan",
        title = list(text = "Word Co-occurrence Network", font = list(size = 16)),
        showlegend = TRUE,
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(l = 40, r = 100, t = 60, b = 40),
        annotations = annotations
      )

    word_co_occurrence_plotly
  })

  output$word_co_occurrence_network_plot_uiOutput <- renderUI({

    req(input$plot_word_co_occurrence_network)
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        "word_co_occurrence_network_plot",
        width = input$width_word_co_occurrence_network_plot,
        height = input$height_word_co_occurrence_network_plot
      )
    )
  })

  # 2. Visualize word correlation network

  output$word_correlation_network_plot <- renderPlotly({

    req(input$plot_word_correlation_network, dfm_outcome())
    req(input$top_node_n_correlation)

    top_node_n <- as.numeric(input$top_node_n_correlation)

    dfm_td <- tidytext::tidy(dfm_outcome()) %>%
      tibble::as_tibble()

    co_occur_n <- floor(as.numeric(input$co_occurence_number))
    corr_n <- as.numeric(input$correlation_value)

    term_cor <- dfm_td %>%
      tibble::as_tibble() %>%
      group_by(term) %>%
      filter(n() >= co_occur_n) %>%
      widyr::pairwise_cor(term, document, sort = TRUE)

    term_cor_graph <- term_cor %>%
      filter(correlation > corr_n) %>%
      igraph::graph_from_data_frame(directed = FALSE)

    if (igraph::vcount(term_cor_graph) == 0) {
      showNotification("No correlation relationships meet the threshold.", type = "error")
      return(NULL)
    }

    igraph::V(term_cor_graph)$degree <- igraph::degree(term_cor_graph)
    igraph::V(term_cor_graph)$betweenness <- igraph::betweenness(term_cor_graph)
    igraph::V(term_cor_graph)$closeness <- igraph::closeness(term_cor_graph)
    igraph::V(term_cor_graph)$eigenvector <- igraph::eigen_centrality(term_cor_graph)$vector
    igraph::V(term_cor_graph)$community <- igraph::cluster_leiden(term_cor_graph)$membership

    layout <- igraph::layout_with_fr(term_cor_graph)
    layout_df <- as.data.frame(layout)
    colnames(layout_df) <- c("x", "y")

    layout_df$label <- igraph::V(term_cor_graph)$name
    layout_df$degree <- igraph::V(term_cor_graph)$degree
    layout_df$betweenness <- igraph::V(term_cor_graph)$betweenness
    layout_df$closeness <- igraph::V(term_cor_graph)$closeness
    layout_df$eigenvector <- igraph::V(term_cor_graph)$eigenvector
    layout_df$community <- igraph::V(term_cor_graph)$community

    edge_data <- igraph::as_data_frame(term_cor_graph, what = "edges") %>%
      dplyr::mutate(
        x = layout_df$x[match(from, layout_df$label)],
        y = layout_df$y[match(from, layout_df$label)],
        xend = layout_df$x[match(to, layout_df$label)],
        yend = layout_df$y[match(to, layout_df$label)],
        correlation = correlation
      ) %>%
      dplyr::select(from, to, x, y, xend, yend, correlation)

    edge_data <- edge_data %>%
      dplyr::mutate(
        line_group = as.integer(cut(
          correlation,
          breaks = unique(quantile(correlation, probs = seq(0, 1, length.out = 6), na.rm = TRUE)),
          include.lowest = TRUE
        )),
        line_width = scales::rescale(line_group, to = c(1, 5)),
        alpha = scales::rescale(line_group, to = c(0.1, 0.3))
      )

    edge_group_labels <- edge_data %>%
      dplyr::group_by(line_group) %>%
      dplyr::summarise(
        min_corr = min(correlation, na.rm = TRUE),
        max_corr = max(correlation, na.rm = TRUE)
      ) %>%
      dplyr::mutate(label = paste0("Correlation: ", round(min_corr, 2), " - ", round(max_corr, 2))) %>%
      dplyr::pull(label)

    node_data <- layout_df %>%
      dplyr::mutate(
        degree_log = log1p(degree),
        size = scales::rescale(degree_log, to = c(12, 30)),
        text_size = scales::rescale(degree_log, to = c(14, 20)),
        alpha = scales::rescale(degree_log, to = c(0.2, 1)),
        hover_text = paste(
          "Word:", label,
          "<br>Degree:", degree,
          "<br>Betweenness:", round(betweenness, 2),
          "<br>Closeness:", round(closeness, 2),
          "<br>Eigenvector:", round(eigenvector, 2),
          "<br>Community:", community
        )
      )

    n_communities <- length(unique(node_data$community))
    if (n_communities >= 3 && n_communities <= 8) {
      palette <- RColorBrewer::brewer.pal(n_communities, "Set2")
    } else if (n_communities > 8) {
      palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_communities)
    } else if (n_communities > 0 && n_communities < 3) {
      palette <- RColorBrewer::brewer.pal(3, "Set2")[1:n_communities]
    } else {
      palette <- rep("#000000", n_communities)
    }

    node_data$community <- factor(node_data$community, levels = unique(node_data$community))
    community_levels <- levels(node_data$community)
    names(palette) <- community_levels
    node_data$color <- palette[as.character(node_data$community)]

    plot <- plotly::plot_ly(
      type = 'scatter',
      mode = 'markers',
      width = input$width_word_correlation_network_plot,
      height = input$height_word_correlation_network_plot
    )

    for (i in unique(edge_data$line_group)) {

      edge_subset <- edge_data %>% dplyr::filter(line_group == i)
      edge_label <- edge_group_labels[i]

      edge_subset <- edge_subset %>%
        dplyr::mutate(
          mid_x = (x + xend) / 2,
          mid_y = (y + yend) / 2
        )

      if (nrow(edge_subset) > 0) {
        plot <- plot %>%
          plotly::add_segments(
            data = edge_subset,
            x = ~x,
            y = ~y,
            xend = ~xend,
            yend = ~yend,
            line = list(
              color = '#5C5CFF',
              width = ~line_width
            ),
            hoverinfo = 'none',
            opacity = ~alpha,
            showlegend = TRUE,
            name = edge_label,
            legendgroup = "Edges"
          ) %>%

          plotly::add_trace(
            data = edge_subset,
            x = ~mid_x,
            y = ~mid_y,
            type = 'scatter',
            mode = 'markers',
            marker = list(size = 0.1, color = '#e0f7ff', opacity = 0),
            text = ~paste0(
              "Correlation:", round(correlation, 2),
              "<br>Source: ", from,
              "<br>Target: ", to
            ),
            hoverinfo = 'text',
            showlegend = FALSE
          )
      }
    }

    plot <- plot %>%
      plotly::layout(
        legend = list(
          title = list(text = "Correlation"),
          orientation = "v",
          x = 1.1,
          y = 1,
          xanchor = "left",
          yanchor = "top"
        )
      )

    marker_params <- list(
      size = ~size,
      showscale = FALSE,
      line = list(width = 3, color = '#FFFFFF')
    )

    for (n in community_levels) {
      community_data <- node_data[node_data$community == n, ]

      plot <- plot %>%
        plotly::add_markers(
          data = community_data,
          x = ~x,
          y = ~y,
          marker = list(
            size = ~size,
            color = palette[n],
            showscale = FALSE,
            line = list(width = 3, color = '#FFFFFF')
          ),
          hoverinfo = 'text',
          text = ~hover_text,
          showlegend = TRUE,
          name = paste("Community", n),
          legendgroup = "Community"
        )
    }

    top_nodes <- head(node_data[order(-node_data$degree), ], top_node_n)

    annotations <- if (nrow(top_nodes) > 0) {
      lapply(1:nrow(top_nodes), function(i) {
        label <- top_nodes$label[i]
        text <- ifelse(!is.na(label) & label != "", label, "")

        list(
          x = top_nodes$x[i],
          y = top_nodes$y[i],
          text = text,
          xanchor = ifelse(!is.na(top_nodes$x[i]) & top_nodes$x[i] > 0, "left", "right"),
          yanchor = ifelse(!is.na(top_nodes$y[i]) & top_nodes$y[i] > 0, "bottom", "top"),
          xshift = ifelse(!is.na(top_nodes$x[i]) & top_nodes$x[i] > 0, 5, -5),
          yshift = ifelse(!is.na(top_nodes$y[i]) & top_nodes$y[i] > 0, 3, -3),
          showarrow = FALSE,
          font = list(size = top_nodes$text_size[i], color = 'black')
        )
      })
    } else {
      list()
    }

    word_correlation_plotly <- plot %>%
      plotly::layout(
        dragmode = "pan",
        title = list(text = "Word Correlation Network", font = list(size = 16)),
        showlegend = TRUE,
        xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        margin = list(l = 40, r = 100, t = 60, b = 40),
        annotations = annotations
      )

    word_correlation_plotly

  })

  output$word_correlation_network_plot_uiOutput <- renderUI({

    req(input$plot_word_correlation_network)
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        "word_correlation_network_plot",
        height = input$height_word_correlation_network_plot,
        width = input$width_word_correlation_network_plot
      )
    )

  })

  # 3. Display selected terms

  observe({
    selected_con <- input$continuous_var_2
    updateSelectInput(session,
                      "continuous_var_3",
                      choices = selected_con,
                      selected = if (!is.null(selected_con) && length(selected_con) > 0) selected_con[1] else NULL)
  })

  top_frequent_over_con_var <- reactive({
    tstat_freq <- quanteda.textstats::textstat_frequency(dfm_outcome())
    tstat_freq_n_20 <- head(tstat_freq, 20)
    tstat_freq_n_20$feature
  })

  observe({
    updateSelectizeInput(
      session,
      "type_terms",
      choices = top_frequent_over_con_var(),
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

    vm <- isolate(input$type_terms)
    if (!is.null(vm)) {
      dfm_outcome_obj <- dfm_outcome()
      dfm_td <- tidytext::tidy(dfm_outcome())
      gamma_td <-
        tidytext::tidy(
          stm_K_number(),
          matrix = "gamma",
          document_names = rownames(dfm_outcome())
        )
      dfm_outcome_obj@docvars$document <- dfm_outcome_obj@docvars$docname_

      dfm_gamma_td <- gamma_td %>%
        left_join(dfm_outcome_obj@docvars,
                  by = c("document" = "document")) %>%
        left_join(dfm_td, by = c("document" = "document"), relationship = "many-to-many")

      con_var_term_counts <- dfm_gamma_td %>%
        tibble::as_tibble() %>%
        group_by(!!rlang::sym(input$continuous_var_3)) %>%
        mutate(
          con_3_total = sum(count),
          term_proportion = count / con_3_total
        ) %>%
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
            y = term_proportion,
            group = term
          )) +
          geom_point(color = "#337ab7", alpha = 0.6, size = 1) +
          geom_line(color = "#337ab7", alpha = 0.6, linewidth = 0.5) +
          facet_wrap(~ term, scales = "free") +
          scale_y_continuous(labels = scales::percent_format()) +
          labs(y = "Term Proportion (%)") +
          theme_minimal(base_size = 11) +
          theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
            axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
            strip.text.x = element_text(size = 11, color = "#3B3B3B"),
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
        do(
          tidy(
            glm(
              cbind(count, con_3_total - count) ~ s(!!rlang::sym(input$continuous_var_3)),
              weights = con_3_total,
              family = binomial(link = "logit"),
              data = .
            )
          )
        ) %>%
        mutate(`odds ratio` = exp(estimate)) %>%
        rename(`logit` = estimate) %>%
        ungroup()

      output$significance_results_table <- renderUI({
        if (nrow(significance_results) > 0) {
          tables <- significance_results %>%
            mutate(word = factor(word, levels = vm)) %>%
            arrange(word) %>%
            group_by(word) %>%
            group_map(~ {
              tagList(
                tags$div(
                  style = "margin-bottom: 20px;",
                  tags$p(
                    style = "font-weight: bold; text-align: center;",
                    .y$word
                  )
                ),
                .x %>%
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
              )
            })
          tagList(tables)
        } else {
          tagList(
            tags$p("No significant results to display.")
          )
        }
      })


    }
  })

  output$line_con_var_plot_uiOutput <- renderUI({
    req(input$plot_term > 0, input$continuous_var_3, input$type_terms)
    tagList(
      plotly::plotlyOutput(
        "line_con_var_plot",
        height = input$height_line_con_var_plot,
        width = input$width_line_con_var_plot
      ),
      tags$div(
        style = "margin-top: 20px;",
        uiOutput("significance_results_table")
      )
    )
  })

  session$onSessionEnded(stopApp)

})
