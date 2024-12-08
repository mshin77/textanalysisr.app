

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
})

server <- shinyServer(function(input, output, session) {
    observe({
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


    output$data_table <-
        DT::renderDataTable(mydata(), rownames = FALSE)

    # "Preprocess" page
    # Step 1: Unite texts
    # Display checkbox
    colnames <- reactive(names(mydata()))

    observe({
        updateCheckboxGroupInput(session,
                                 "show_vars",
                                 choices = colnames(),
                                 selected = "")
    })

    listed_vars <- eventReactive(input$show_vars, {
        print(input$show_vars)
    })

    # Select one or multiple columns for text data pre-processing
    united_tbl <- eventReactive(input$apply, {
        united_texts_tbl <- mydata() %>%
            select(listed_vars()) %>%
            tidyr::unite(col = united_texts,
                         sep = " ",
                         remove = FALSE)
        docvar_tbl <- mydata()
        united_texts_tbl %>% dplyr::left_join(docvar_tbl)

    })

    # Print data table
    output$step1_table <- DT::renderDataTable(united_tbl(),
                                              rownames = FALSE)
    # Download data table as a csv file
    output$download_table <- downloadHandler(
        filename = function() {
            paste(input$file, ".csv", sep = "")
        },
        content = function(file) {
            utils::write.csv(united_tbl(), file, row.names = FALSE)
        }
    )

    # Step 2
    # Preprocess data
    processed_tokens <-
        eventReactive(eventExpr = input$preprocess, {
            united_tbl() %>% TextAnalysisR::preprocess_texts()
        })

    output$step2_print_preprocess <- renderPrint({
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
                verbose = TRUE,
                exclusive = FALSE,
                capkeys = FALSE
            )

        tokens_dict_int %>% quanteda::tokens_lookup(
            dictionary = dictionary(dictionary_list_2),
            valuetype = "glob",
            verbose = TRUE,
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
    })


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

        print(input$remove)

        rm <- isolate(input$remove.var)

        if (!is.null(rm)) {
            removed_processed_tokens <-
                quanteda::tokens_remove(tokens_dict_no_stop(), rm)

            removed_tokens_dict_int <- removed_processed_tokens %>%
                quanteda::tokens_lookup(
                    dictionary = dictionary(dictionary_list_1),
                    valuetype = "glob",
                    verbose = TRUE,
                    exclusive = FALSE,
                    capkeys = FALSE
                )

            removed_tokens_dict <- removed_tokens_dict_int %>%
                quanteda::tokens_lookup(
                    dictionary = dictionary(dictionary_list_2),
                    valuetype = "glob",
                    verbose = TRUE,
                    exclusive = FALSE,
                    capkeys = FALSE
                )

            removed_tokens_dict %>% quanteda::dfm()

        } else{
            dfm_init()
        }
    })

    observeEvent(input$remove, {
        if (!is.null(input$remove.var)) {
            output$step4_plot <- plotly::renderPlotly({
                dfm_outcome() %>% TextAnalysisR::plot_word_frequency(n = 20)
            })
        }
    })

    observeEvent(input$remove, {
        if (!is.null(input$remove.var)) {
            output$step4_table <- DT::renderDataTable({
                quanteda.textstats::textstat_frequency(dfm_outcome())
            })
        }
    })

    # "Structural topic model" page
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
        if (!is.null(input$categorical_var) &&
            !is.null(input$continuous_var)) {
            stm::searchK(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_range_1(),
                prevalence =  ~ eval(parse(text = input$categorical_var)) +
                    stm::s(eval(
                        parse(text = input$continuous_var)
                    )),
                verbose = TRUE
            )

        } else if (!is.null(input$categorical_var) &&
                   is.null(input$continuous_var)) {
            stm::searchK(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_range_1(),
                prevalence =  ~ eval(parse(text = input$categorical_var)),
                verbose = TRUE
            )

        } else if (is.null(input$categorical_var) &&
                   !is.null(input$continuous_var)) {
            stm::searchK(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_range_1(),
                prevalence =  ~ stm::s(eval(
                    parse(text = input$continuous_var)
                )),
                verbose = TRUE
            )

        } else {
            stm::searchK(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_range_1(),
                prevalence = NULL,
                verbose = TRUE
            )

        }
    })

    output$search_K_plot <- renderPlot({
        plot(K_search())
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
                          selected = "")
    })

    observeEvent(eventExpr = input$categorical_var_2, {
        print(input$categorical_var_2)
    })

    observeEvent(eventExpr = input$continuous_var_2, {
        print(input$continuous_var_2)
    })

    stm_K_number <- eventReactive(eventExpr = input$run, {
        if (!is.null(input$categorical_var_2) &&
            !is.null(input$continuous_var_2)) {
            stm::stm(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_number(),
                prevalence =  ~ eval(parse(text = input$categorical_var_2)) +
                    stm::s(eval(
                        parse(text = input$continuous_var_2)
                    )),
                verbose = TRUE
            )

        } else if (!is.null(input$categorical_var_2) &&
                   is.null(input$continuous_var_2)) {
            stm::stm(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_number(),
                prevalence =  ~ eval(parse(text = input$categorical_var_2)),
                verbose = TRUE
            )

        } else if (is.null(input$categorical_var_2) &&
                   !is.null(input$continuous_var_2)) {
            stm::stm(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_number(),
                prevalence =  ~ stm::s(eval(
                    parse(text = input$continuous_var_2)
                )),
                verbose = TRUE
            )

        } else {
            stm::stm(
                data = out()$meta,
                documents = out()$documents,
                vocab = out()$vocab,
                max.em.its = 75,
                init.type = "Spectral",
                K = print_K_number(),
                prevalence = NULL,
                verbose = TRUE
            )

        }

    })


    # Display highest word probabilities for each topic

    # Tidy the word-topic combinations
    beta_td <- reactive({
        tidytext::tidy(stm_K_number(), document_names = rownames(dfm_outcome()))
    })

    observe({
        print(input$label_topics)
    })

    output$topic_term_plot <- renderPlot({
        print(input$top_term_number_1)
        req(!is.na(stm_K_number()))

        if (input$label_topics == '') {
            tn = NULL
        } else{
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
        topic_term_plot %>%
            ggplot(aes(term, beta, fill = topic)) +
            geom_col(show.legend = FALSE, alpha = 0.8) +
            facet_wrap(~ topic, scales = "free", ncol = 3) +
            scale_x_reordered() +
            scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
            coord_flip() +
            xlab("") +
            ylab("Word probability") +
            theme_minimal(base_size = 14) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
                axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
                strip.text.x = element_text(size = 14, color = "#3B3B3B"),
                axis.text.x = element_text(size = 14, color = "#3B3B3B"),
                axis.text.y = element_text(size = 14, color = "#3B3B3B"),
                axis.title = element_text(size = 14, color = "#3B3B3B"),
                axis.title.x = element_text(margin = margin(t = 7)),
                axis.title.y = element_text(margin = margin(r = 7))
            )
    })

    output$topic_term_plot_uiOutput <- renderUI({
        shinycssloaders::withSpinner(plotOutput(
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

            topic_by_prevalence_plot %>%
                ggplot(aes(topic, gamma, label = terms, fill = topic)) +
                geom_col(alpha = 0.8) +
                coord_flip() +
                scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
                xlab("") +
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
        })

    })

    # Display topics ordered by prevalence
    observeEvent(input$display, {
        if (input$label_topics == '') {
            tn = NULL
        } else{
            tn = strsplit(input$label_topics, split = ',')[[1]]
        }

        output$topic_by_prevalence_table <- DT::renderDataTable({
            topic_by_prevalence_table <- gamma_terms() %>%
                top_n(input$topic_number, gamma) %>%
                mutate(tt = as.numeric(topic)) %>%
                mutate(ord = topic) %>%
                mutate(topic = paste('Topic', topic)) %>%  arrange(ord)
            levelt = paste("Topic", topic_by_prevalence_table$ord) %>% unique()

            topic_by_prevalence_table$topic = factor(topic_by_prevalence_table$topic,
                                                     levels = levelt)
            topic_by_prevalence_table %>%
                select(topic, gamma) %>%
                mutate_if(is.numeric, ~ round(., 3)) %>%
                DT::datatable(rownames = FALSE)
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
            selected = ""
        )
    })

    observeEvent(eventExpr = input$topic_number_quote, {
        print(input$topic_number_quote)
    })

    colnames_cat_3 <- reactive({
        categorical <- mydata() %>% select(where(is.character))
        names(categorical)
    })

    observe({
        updateSelectizeInput(session,
                             "topic_texts",
                             choices = colnames_cat_3(),
                             selected = "")
    })

    observeEvent(eventExpr = input$topic_texts, {
        print(input$topic_texts)
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
    })

    # Step 5: Display estimates regressions based on a STM object.

    effect_stm_K_number <- eventReactive(eventExpr = input$effect, {
        if (!is.null(input$categorical_var_2) &&
            !is.null(input$continuous_var_2)) {
            ff = paste0(
                '1:',
                input$K_number,
                ' ~ ',
                input$categorical_var_2,
                ' +',
                ' stm::s(',
                input$continuous_var_2,
                ')'
            )

            stmm = stm::estimateEffect(
                formula = eval(parse(text = ff)),
                stmobj = stm_K_number(),
                metadata = out()$meta,
                documents = out()$documents,
                uncertainty = "Global"
            )

        } else if (!is.null(input$categorical_var_2) &&
                   is.null(input$continuous_var_2)) {
            ff = paste0('1:',
                        input$K_number,
                        ' ~ ',
                        input$categorical_var_2)

            stmm = stm::estimateEffect(
                formula = eval(parse(text = ff)),
                stmobj = stm_K_number(),
                metadata = out()$meta,
                documents = out()$documents,
                uncertainty = "Global"
            )

        } else if (is.null(input$categorical_var_2) &&
                   !is.null(input$continuous_var_2)) {
            ff = paste0('1:',
                        input$K_number,
                        ' ~ ',
                        'stm::s(',
                        input$continuous_var_2,
                        ')')

            stmm = stm::estimateEffect(
                formula = eval(parse(text = ff)),
                stmobj = stm_K_number(),
                metadata = out()$meta,
                documents = out()$documents,
                uncertainty = "Global"
            )
        } else {
            stmm = NULL
        }


    })

    observe({
        effect_stm_K_number_td = effect_stm_K_number()

        td <-
            tidytext::tidy(effect_stm_K_number_td) %>% mutate_if(is.numeric, ~ round(., 3))

        output$effect_table <-
            DT::renderDataTable(td, rownames = FALSE)

    })

    # Download data table as a csv file
    output$effect_download_table <- downloadHandler(
        filename = function() {
            paste(input$file, ".csv", sep = "")
        },
        content = function(file) {
            utils::write.csv(effect_stm_K_number(), file, row.names = FALSE)
        }
    )


    # Step 6: Plot topic prevalence effects by a categorical variable.
    observe({
        updateSelectizeInput(session,
                             "effect_cat_btn",
                             choices = colnames_cat_2(),
                             selected = "")
    })

    observeEvent(eventExpr = input$effect_cat_btn, {
        print(input$effect_cat_btn)
    })

    effects_categorical_var <- reactive({
        stminsights::get_effects(
            estimates = effect_stm_K_number(),
            variable = input$effect_cat_btn,
            type = 'pointestimate'
        )
    })


    output$cat_plot <- renderPlot({
        observeEvent(input$display_cat, {
            if (!is.null(input$effect_cat_btn)) {
                output$cat_plot <-  renderPlot({
                    effects_categorical_var() %>%
                        ggplot(aes(x = value, y = proportion)) +
                        facet_wrap( ~ topic,
                                    ncol = 3,
                                    scales = "free") +
                        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
                        xlab("") +
                        ylab("Topic proportion") +
                        geom_errorbar(
                            aes(ymin = lower, ymax = upper),
                            width = 0.1,
                            size = 0.5,
                            color = "#337ab7"
                        ) +
                        geom_point(color = "#337ab7",
                                   size = 1.5) +
                        theme_minimal(base_size = 14) +
                        theme(
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            axis.ticks = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            strip.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.y = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title.x = element_text(margin = margin(t = 9)),
                            axis.title.y = element_text(margin = margin(r = 9))
                        )
                })

            } else {
                NULL
            }
        })


    })

    output$cat_plot_uiOutput <- renderUI({
        shinycssloaders::withSpinner(
            plotOutput(
                "cat_plot",
                height = input$height_cat_plot,
                width = input$width_cat_plot
            )
        )
    })



    observeEvent(input$display_cat, {
        if (!is.null(input$effect_cat_btn)) {
            output$cat_table <- DT::renderDataTable({
                effects_categorical_var() %>%
                    mutate_if(is.numeric, ~ round(., 3)) %>%
                    DT::datatable(rownames = FALSE)
            })

        }
    })

    # Step 7: Plot topic prevalence effects by a continuous variable.
    observe({
        updateSelectizeInput(session,
                             "effect_con_btn",
                             choices = colnames_con_2(),
                             selected = "")
    })

    observeEvent(input$effect_con_btn, {
        print(input$effect_con_btn)
    })

    effects_continuous_var <- reactive({
        stminsights::get_effects(
            estimates = effect_stm_K_number(),
            variable = input$effect_con_btn,
            type = 'continuous'
        )
    })

    output$con_plot <- renderPlot({
        observeEvent(input$display_con, {
            if (!is.null(input$effect_con_btn)) {
                output$con_plot <- renderPlot({
                    effects_continuous_var() %>%
                        ggplot(aes(x = value, y = proportion)) +
                        facet_wrap( ~ topic,
                                    ncol = 3,
                                    scales = "free") +
                        scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
                        geom_line(linewidth = 0.5,
                                  color = "#337ab7") +
                        xlab("") +
                        ylab("Topic proportion") +
                        theme_minimal(base_size = 14) +
                        theme(
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            axis.ticks = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            strip.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.y = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title.x = element_text(margin = margin(t = 9)),
                            axis.title.y = element_text(margin = margin(r = 9))
                        )
                })

            } else {
                NULL
            }
        })

    })

    output$con_plot_uiOutput <- renderUI({
        shinycssloaders::withSpinner(
            plotOutput(
                "con_plot",
                height = input$height_con_plot,
                width = input$width_con_plot
            )
        )
    })

    observeEvent(input$display_con, {
        if (!is.null(input$effect_con_btn)) {
            output$con_table <- DT::renderDataTable({
                effects_continuous_var() %>%
                    mutate_if(is.numeric, ~ round(., 3)) %>%
                    DT::datatable(rownames = FALSE)
            })

        }
    })

    # "Network analysis" page
    # 1. Hierarchical clustering
    hclust <- reactive({
        stm_dist <-
            textmineR::CalcHellingerDist(stm_K_number()$theta, by_rows = FALSE)
        stats::hclust(stats::as.dist(stm_dist), "ward.D2")
    })

    # create dendrogram
    observeEvent(input$plot_dendrogram, {
        output$dendro_plot <- renderPlot({
            ggdendro::ggdendrogram(hclust(), rotate = TRUE) +
                theme_classic(base_size = 14) +
                theme(
                    axis.line = element_line(linewidth = 0.1, color = "#3B3B3B"),
                    axis.text.x = element_text(size = 14, color = "#3B3B3B"),
                    axis.text.y = element_text(size = 14, color = "#3B3B3B"),
                    axis.title = element_text(size = 14, color = "#3B3B3B"),
                    plot.title = element_text(hjust = 1),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    axis.title.y = element_text(margin = margin(r = 10))
                ) +
                labs(title = NULL,
                     x = 'Distance',
                     y = 'Height')
        })
    })


    # 2. Visualize word co-occurrence network

    output$word_co_occurrence_network_plot <- renderPlot({
      observeEvent(input$plot_word_co_occurrence_network, {
        output$word_co_occurrence_network_plot <- renderPlot({

          dfm_td <- tidytext::tidy(dfm_outcome())

          co_occur_n <- floor(as.numeric(input$co_occurence_number_init))

          term_co_occur <- dfm_td %>%
            tibble::as_tibble() %>%
            widyr::pairwise_count(term, document, sort = TRUE) %>%
            filter(n >= co_occur_n)

          co_occur_graph <- igraph::graph_from_data_frame(term_co_occur, directed = FALSE)

          igraph::V(co_occur_graph)$centrality <- igraph::degree(co_occur_graph, mode = "out") / (igraph::vcount(co_occur_graph) - 1)

          layout <- ggraph::create_layout(co_occur_graph, layout = "fr")

          word_co_occurrence_network <- layout %>% ggraph() +
            geom_edge_link(aes(edge_alpha = n), edge_colour = "#b0aeae", edge_width = 1.5) +
            geom_node_point(aes(size = centrality, colour = centrality)) +
            geom_node_text(
              aes(label = name),
              repel = TRUE,
              check_overlap = FALSE,
              size = 5
            ) +
            scale_color_continuous(name = "Centrality",
                                   guide = 'legend',
                                   high = "#47a0ed",
                                   low = "#deebf7") +
            scale_size_continuous(
              name = "Centrality",
              guide = guide_legend(title.position = "top")
            ) +
            scale_edge_alpha_continuous(
              name = "Co-occurrence",
              labels = scales::number_format(accuracy = 1),
              guide = guide_legend(title.position = "top")
            ) +
            theme_void(base_size = 14) +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.text = element_text(size = 14)
            )

          word_co_occurrence_network
        })
      })
    })

    output$word_co_occurrence_network_plot_uiOutput <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
          "word_co_occurrence_network_plot",
          height = input$height_word_co_occurrence_network_plot,
          width = input$width_word_co_occurrence_network_plot
        )
      )
    })

    # 3. Visualize word correlation network

    output$word_network_plot <- renderPlot({
      observeEvent(input$plot_word_network, {
        output$word_network_plot <- renderPlot({

          dfm_td <- tidytext::tidy(dfm_outcome())

          co_occur_n <- floor(as.numeric(input$co_occurence_number))

          corr_n <- as.numeric(input$correlation_value)

          term_cor <- dfm_td %>% tibble::as_tibble() %>%
            group_by(term) %>%
            filter(n() >= co_occur_n) %>%
            widyr::pairwise_cor(term, document, sort = TRUE)

          word_network <- term_cor %>%
            filter(correlation > corr_n) %>%
            ggraph(layout = "fr") +
            geom_edge_link(
              aes(
                edge_alpha = correlation,
                edge_width = correlation
              ),
              edge_colour = "#47a0ed"
            ) +
            geom_node_point(size = 3, color = "white") +
            geom_node_text(
              aes(label = name),
              repel = TRUE,
              check_overlap = FALSE,
              size = 5
            ) +
            scale_edge_alpha_continuous(
              name = "Correlation",
              guide = guide_legend(title.position = "top")
            ) +
            scale_edge_width_continuous(
              name = "Correlation",
              guide = guide_legend(title.position = "top")
            ) +
            theme_void(base_size = 14) +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              legend.text = element_text(size = 14)
            )

          word_network
        })
      })
    })

    output$word_network_plot_uiOutput <- renderUI({
      shinycssloaders::withSpinner(
        plotOutput(
          "word_network_plot",
          height = input$height_word_network_plot,
          width = input$width_word_network_plot
        )
      )
    })


    # 4. Display selected terms that have changed in frequency over time

    observe({
        updateSelectizeInput(session,
                             "continuous_var_3",
                             choices = colnames_con_2(),
                             selected = "")
    })

    top_frequent_over_time  <- reactive({
        tstat_freq <- quanteda.textstats::textstat_frequency(dfm_outcome())
        tstat_freq_n_20 <- head(tstat_freq, 20)
        tstat_freq_n_20$feature
    })

    observe({
        updateSelectizeInput(
            session,
            "type_terms",
            choices = top_frequent_over_time(),
            options = list(create = TRUE),
            selected = ""
        )
    })

    observeEvent(input$type_terms, {
        print(input$type_terms)
    })

    observeEvent(input$continuous_var_3, {
        print(input$continuous_var_3)
    })



    output$line_year_plot <- renderPlot({
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
                dfm_outcome_obj@docvars$document <-
                    dfm_outcome_obj@docvars$docname_

                # Bind data by column
                dfm_gamma_td <- gamma_td %>%
                    left_join(dfm_outcome_obj@docvars,
                              by = c("document" = "document")) %>%
                    left_join(dfm_td, by = c("document" = "document"))

                year_term_counts <-
                    dfm_gamma_td %>% tibble::as_tibble() %>%
                    group_by(eval(parse(text = input$continuous_var_3))) %>%
                    mutate(con_3_total = sum(count),
                           percent = count / con_3_total) %>%
                    ungroup()

                output$line_year_plot <- renderPlot({
                    year_term_counts %>%
                        filter(term %in% vm) %>%
                        ggplot(aes(eval(
                            parse(text = input$continuous_var_3)
                        ), count / con_3_total)) +
                        geom_point(color = "#337ab7") +
                        geom_smooth(color = "#337ab7") +
                        facet_wrap(~ term, scales = "free_y") +
                        scale_y_continuous(labels = scales::percent_format()) +
                        labs(x = "", y = "") +
                        theme_minimal(base_size = 14) +
                        theme(
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            axis.ticks = element_line(
                                color = "#3B3B3B",
                                linewidth = 0.3
                            ),
                            strip.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.x = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.text.y = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title = element_text(
                                size = 14,
                                color = "#3B3B3B"
                            ),
                            axis.title.x = element_text(margin = margin(t = 9)),
                            axis.title.y = element_text(margin = margin(r = 9))
                        )
                })

                # Test the significance of word frequency over time
                year_term_counts %>%
                    mutate(word = term) %>%
                    filter(word %in% vm) %>%
                    group_by(word) %>%
                    do(tidy(glm(
                        cbind(count, con_3_total - count) ~ eval(parse(text = input$continuous_var_3)),
                        .,
                        family = "binomial"
                    ))) %>%
                    ungroup() %>%
                    filter(term == "eval(parse(text = input$continuous_var_3))") %>%
                    arrange(desc(abs(estimate)))
            }

        })

    })

    output$line_year_plot_uiOutput <- renderUI({
        shinycssloaders::withSpinner(
            plotOutput(
                "line_year_plot",
                height = input$height_line_year_plot,
                width = input$width_line_year_plot
            )
        )
    })

    session$onSessionEnded(stopApp)

})
