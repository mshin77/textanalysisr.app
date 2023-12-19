# Preprocess Text Data ----

#' @title Preprocess text data
#'
#' @name preprocess_texts
#'
#' @description
#' Preprocess text data by conducting the following functions:
#' construct a corpus; segment texts in a corpus into tokens; preprocess tokens;
#' convert the features of tokens to lowercase;
#' remove stopwords; specify the minimum length in characters for tokens (at least 2).
#'
#' @param data A data frame that contains text as data.
#' @param text_field A name of column that contains text data in a data frame.
#' @param ... Further arguments passed to \code{corpus}.
#'
#' @export
#' @return A tokens object output from \code{quanteda::tokens}.
#' The result is a list of tokenized and preprocessed text data.
#'
#' @examples
#' SpecialEduTech %>% preprocess_texts(text_field = "abstract")
#'
#' @import quanteda
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos

preprocess_texts <-
    function(data, text_field = "united_texts", ...) {

        # Construct a corpus
        corp <- quanteda::corpus(data, text_field = text_field, ...)

        # Segment texts in a corpus into tokens (words or sentences) by word boundaries
        toks <- quanteda::tokens(corp)

        # Preprocess tokens
        toks_clean <- quanteda::tokens(
            toks,
            what = "word",
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_numbers = TRUE,
            remove_url = TRUE,
            remove_separators = TRUE,
            split_hyphens = TRUE,
            split_tags = TRUE,
            include_docvars = TRUE,
            padding = FALSE,
            verbose = TRUE)

        # Convert the features of tokens to lowercase.
        toks_lower <- quanteda::tokens_tolower(toks_clean,
                                               keep_acronyms = FALSE)

        # Remove English stopwords.
        toks_lower_no_stop <- toks_lower %>%
            quanteda::tokens_remove(quanteda::stopwords("en"),
                                    valuetype = "glob",
                                    window = 0,
                                    verbose = TRUE,
                                    padding = TRUE)

        # Specify the minimum length in characters for tokens (at least 2).
        toks_lower_no_stop_adj <- toks_lower_no_stop %>%
            quanteda::tokens_select(min_nchar=2L,
                                    verbose = TRUE)

        return(toks_lower_no_stop_adj)
    }


#' @title Plot word frequency results.
#'
#' @name plot_word_frequency
#'
#' @description
#' Plot the frequently observed top n terms.
#'
#' @param data A document-feature matrix (dfm) object through the quanteda package.
#' @param n The number of top n features (terms or words).
#' @param ... Further arguments passed to \code{quanteda.textstats::textstat_frequency}.
#'
#' @export
#' @return A ggplot object output from \code{quanteda.textstats::textstat_frequency} and \code{ggplot2::ggplot}.
#' The result is a ggplot object representing the word frequency plot.
#'
#' @examples
#' if(requireNamespace("quanteda")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' dfm %>% plot_word_frequency(n = 20)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom ggplot2 ggplot geom_point coord_flip labs theme_bw
#'
plot_word_frequency <-
    function(data, n = 20, ...) {
        word_frequency_plot <- data %>%
            quanteda.textstats::textstat_frequency(n = n, ...) %>%
            ggplot(aes(x = stats::reorder(feature, frequency), y = frequency)) +
            geom_point(colour = "#5f7994", size = 1) +
            coord_flip() +
            labs(x = NULL, y = "Word frequency") +
            theme_bw(base_size = 12)
        return(word_frequency_plot)
    }


#' @title Extract frequently observed words
#'
#' @name extract_frequent_word
#'
#' @description
#' Extract frequently observed top n features (terms or words).
#'
#' @param data A document-feature matrix (dfm) object through the quanteda package.
#' @param n A number of top n features (terms or words) frequently observed.
#' @param ... Further arguments passed to \code{quanteda.textstats::textstat_frequency}.
#'
#' @export
#' @return A character (feature) Vector extracted from \code{quanteda.textstats::textstat_frequency}.
#' The result is a character vector of top frequent words.
#'
#' @examples
#' if(requireNamespace("quanteda")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' dfm %>% extract_frequent_word()
#' }
#'
#' @importFrom rlang := enquos
#'
extract_frequent_word <-
    function(data, n = 20, ...) {

        tstat_freq <- quanteda.textstats::textstat_frequency(data, ...)
        tstat_freq_n_20 <- utils::head(tstat_freq, n = n)
        top_frequency_word <- tstat_freq_n_20$feature

        return(extract_frequent_word)
    }


# Display text mining results from the structural topic model ----

#' @title Visualize a plot for highest word probabilities within each topic
#'
#' @name plot_topic_term
#'
#' @description
#' Visualize a plot for highest word probabilities within each topic.
#'
#' @param data A tidy data frame that includes term (word)-topic probabilities
#'                (probabilities of each word per each topic).
#' @param top_n A number of top n terms frequently observed in each document.
#' @param topic_names (Labeled) topic names
#' @param ... Further arguments passed to \code{dplyr::group_by}.
#'
#' @export
#' @return A ggplot object output from \code{stm::stm}, \code{tidytext::tidy}, and \code{ggplot2::ggplot}.
#' The result is a ggplot object representing the topic-term plot.
#'
#' @examples
#' if(requireNamespace("quanteda", "tidytext")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' data <- tidytext::tidy(stm_15, document_names = rownames(dfm), log = FALSE)
#' data %>% plot_topic_term(top_n = 10)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom tidytext scale_x_reordered reorder_within
#'
plot_topic_term <-
    function(data, top_n, topic_names = NULL, ...) {

        topic_term_plot <- data %>%
            group_by(topic, ...) %>%
            top_n(top_n, beta) %>%
            ungroup() %>%
            mutate(
                ord = factor(topic, levels = c(min(topic): max(topic))),
                tt = as.numeric(topic),
                topic = paste("Topic", topic),
                term = reorder_within(term, beta, topic)) %>%
            arrange(ord)
        levelt = paste("Topic", topic_term_plot$ord) %>% unique()
        topic_term_plot$topic = factor(topic_term_plot$topic,
                                       levels = levelt)
        if(!is.null(topic_names)){
            topic_term_plot$topic = topic_names[topic_term_plot$tt]
            topic_term_plot <- topic_term_plot %>%
                mutate(topic = as.character(topic)) %>%
                mutate(topic = ifelse(!is.na(topic), topic, paste('Topic',tt)))
            topic_term_plot$topic =
                factor(topic_term_plot$topic, levels = topic_term_plot$topic %>% unique())
        }
        topic_term_plot$tt = NULL
        topic_term_plot <- topic_term_plot %>%
            ggplot(aes(term, beta, fill = topic)) +
            geom_col(show.legend = FALSE, alpha = 0.8) +
            facet_wrap(~ topic, scales = "free", ncol = 3) +
            scale_x_reordered() +
            scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
            coord_flip() +
            xlab("") +
            ylab("Word probability") +
            theme_minimal(base_size = 12) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "#3B3B3B", size = 0.3),
                axis.ticks = element_line(color = "#3B3B3B", size = 0.3),
                strip.text.x = element_text(size = 12, color = "#3B3B3B"),
                axis.text.x = element_text(size = 12, color = "#3B3B3B"),
                axis.text.y = element_text(size = 12, color = "#3B3B3B"),
                axis.title = element_text(size = 12, color = "#3B3B3B"),
                axis.title.x = element_text(margin = margin(t = 7)),
                axis.title.y = element_text(margin = margin(r = 7)))

        return(topic_term_plot)
    }


#' @title Examine highest word probabilities for each topic
#'
#' @name examine_top_terms
#'
#' @description
#' Examine highest document-topic probabilities.
#'
#' @param data A tidy data frame that includes term-topic probabilities
#'                (probabilities of each word per each topic).
#' @param top_n A number of top n terms with highest term-topic probabilities in each document.
#' @param ... Further arguments passed to \code{dplyr::group_by}.
#'
#' @export
#' @return A tibble (data frame) object with a list of word probabilities from \code{tidytext::tidy}.
#' The result is a data frame containing word probabilities for each topic.
#'
#' @examples
#' if(requireNamespace("quanteda", "tidytext")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' data <- tidytext::tidy(stm_15, document_names = rownames(dfm), log = FALSE)
#' data %>% examine_top_terms(top_n = 5)
#' }
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom rlang := enquos
#' @importFrom tidyr unnest
#'
examine_top_terms <-
    function(data, top_n, ...) {
        top_terms <- data %>%
            arrange(beta) %>%
            group_by(topic, ...) %>%
            top_n(top_n, beta) %>%
            arrange(beta) %>%
            select(topic, term) %>%
            summarise(terms = list(term)) %>%
            mutate(terms = purrr::map(terms, paste, collapse = ", ")) %>%
            unnest(cols = c(terms))
        return(top_terms)
    }


#' @title Visualize a plot for document-topic probabilities
#'
#' @name plot_topic_probability
#'
#' @description
#' Visualize a plot for document-topic probabilities.
#'
#' @param data A tidy data frame that includes document-topic probabilities.
#'                 (probabilities of each topic per each document).
#' @param top_n A number of top n terms with highest document-topic probabilities.
#' @param topic_names Topic names
#' @param ... Further arguments passed.
#'
#' @export
#' @return A ggplot object output from \code{stm::stm}, \code{tidytext::tidy}, and \code{ggplot2::ggplot}.
#'
#' @examples
#' if(requireNamespace("quanteda", "tidytext")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' data <- tidytext::tidy(stm_15, matrix = "gamma", document_names = rownames(dfm), log = FALSE)
#' data %>% plot_topic_probability(top_n = 15)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_topic_probability <-
    function(data, top_n, topic_names = NULL, ...) {

        topic_by_prevalence_plot <- data %>%
            top_n(top_n, gamma) %>%
            mutate(tt = as.numeric(topic)) %>%
            mutate(ord = topic) %>%
            mutate(topic = paste('Topic',topic)) %>%  arrange(ord)
        levelt = paste("Topic", topic_by_prevalence_plot$ord) %>% unique()
        topic_by_prevalence_plot$topic = factor(topic_by_prevalence_plot$topic,
                                                levels = levelt)
        if(!is.null(topic_names)){
            reft  = 1:length(topic_by_prevalence_plot$tt)
            topic_by_prevalence_plot$topic =
                topic_names[reft]
            topic_by_prevalence_plot <- topic_by_prevalence_plot %>%
                mutate(topic = as.character(topic)) %>%
                mutate(topic = ifelse(!is.na(topic), topic, paste('Topic',tt)))
            topic_by_prevalence_plot$topic =
                factor(topic_by_prevalence_plot$topic, levels = topic_by_prevalence_plot$topic)
        }

        topic_by_prevalence_plot <- topic_by_prevalence_plot %>%
            ggplot(aes(topic, gamma, fill = topic)) +
            geom_col(show.legend = FALSE, alpha = 0.8) +
            coord_flip() +
            scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 2)) +
            xlab("") +
            ylab("Topic proportion") +
            theme_minimal(base_size = 12) +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "#3B3B3B", size = 0.3),
                axis.ticks = element_line(color = "#3B3B3B", size = 0.3),
                strip.text.x = element_text(size = 12, color = "#3B3B3B"),
                axis.text.x = element_text(size = 12, color = "#3B3B3B"),
                axis.text.y = element_text(size = 12, color = "#3B3B3B"),
                axis.title = element_text(size = 12, color = "#3B3B3B"),
                axis.title.x = element_text(margin = margin(t = 9)),
                axis.title.y = element_text(margin = margin(r = 9)))

        return(topic_by_prevalence_plot)
    }


#' @title Visualize a table for document-topic probabilities
#'
#' @name plot_topic_probability_table
#'
#' @description
#' Visualize a table for document-topic probabilities.
#'
#' @param data A tidy data frame that includes document-topic probabilities.
#'                 (probabilities of each topic per each document).
#' @param top_n A number of top n terms with highest document-topic probabilities.
#' @param topic_names Topic names
#' @param ... Further arguments passed.
#'
#' @export
#' @return A tibble (data frame) object with a list of topic probabilities from \code{tidytext::tidy}.
#' The result is a ggplot object representing the topic-term plot.
#'
#' @examples
#' if(requireNamespace("quanteda", "tidytext")){
#' dfm <- SpecialEduTech %>% preprocess_texts(text_field = "abstract") %>% quanteda::dfm()
#' data <- tidytext::tidy(stm_15, matrix = "gamma", document_names = rownames(dfm), log = FALSE)
#' data %>% plot_topic_probability_table(top_n = 15)
#' }
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#'
plot_topic_probability_table <-
    function(data, top_n, topic_names = NULL, ...) {

        topic_by_prevalence_table <- data %>%
            top_n(top_n, gamma) %>%
            mutate(tt = as.numeric(topic)) %>%
            mutate(ord = topic) %>%
            mutate(topic = paste('Topic',topic)) %>%  arrange(ord)
        levelt = paste("Topic", topic_by_prevalence_table$ord) %>% unique()
        topic_by_prevalence_table$topic = factor(topic_by_prevalence_table$topic,
                                                 levels = levelt)

        topic_by_prevalence_table <- topic_by_prevalence_table %>%
            select(topic, gamma)
        return(topic_by_prevalence_table)
    }
