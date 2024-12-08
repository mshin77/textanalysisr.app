
#' @title Preprocess Text Data
#'
#' @description
#' Preprocesses text data by:
#' - Constructing a corpus
#' - Tokenizing text into words
#' - Converting to lowercase
#' - Removing default English stopwords and optional custom stopwords
#' - Specifying a minimum token length.
#'
#' Typically used before constructing a dfm and fitting an STM model.
#'
#' @param df A data frame that contains text data.
#' @param text_field The name of the column containing text data.
#' @param custom_stopwords A character vector of additional stopwords to remove. Default is NULL.
#' @param min_char Minimum length in characters for tokens (default is 2).
#' @param ... Further arguments passed to \code{quanteda::corpus}.
#'
#' @return A \code{quanteda::tokens} object. The output is a list of tokens. Each token is a word in the text data.
#' The function removes punctuation, symbols, numbers, URLs, and stopwords. It also converts all words to lowercase.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- TextAnalysisR::SpecialEduTech
#'   tokens <- preprocess_texts(df, text_field = "abstract")
#'   tokens
#' }
preprocess_texts <-
  function(df, text_field = "united_texts", custom_stopwords = NULL, min_char = 2, ...) {

    corp <- quanteda::corpus(df, text_field = text_field, ...)
    toks <- quanteda::tokens(corp,
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
                             verbose = FALSE)
    toks_lower <- quanteda::tokens_tolower(toks, keep_acronyms = FALSE)

    all_stopwords <- c(quanteda::stopwords("en"), custom_stopwords)
    toks_lower_no_stop <- quanteda::tokens_remove(toks_lower,
                                                  all_stopwords,
                                                  valuetype = "glob",
                                                  window = 0,
                                                  verbose = FALSE,
                                                  padding = TRUE)

    toks_clean <- quanteda::tokens_select(toks_lower_no_stop,
                                          min_nchar = min_char,
                                          verbose = FALSE)

    return(toks_clean)
  }


#' @title Plot Word Frequency
#'
#' @description
#' Given a document-feature matrix (dfm), this function computes the most frequent terms
#' and creates a ggplot-based visualization of term frequencies.
#'
#' @param dfm_object A \code{quanteda} dfm object.
#' @param n The number of top features (terms or words) to display.
#' @param ... Further arguments passed to \code{quanteda.textstats::textstat_frequency}.
#'
#' @return A \code{ggplot} object visualizing the top terms by their frequency. The plot
#' shows each term on one axis and frequency on the other, with points representing their
#' observed frequencies.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   df <- TextAnalysisR::SpecialEduTech
#'   dfm_object <- df %>%
#'     preprocess_texts(text_field = "abstract") %>%
#'     quanteda::dfm()
#'   plot <- plot_word_frequency(dfm_object, n = 20)
#'   print(plot)
#' }
plot_word_frequency <-
  function(dfm_object, n = 20, ...) {
    word_freq <- quanteda.textstats::textstat_frequency(dfm_object, n = n, ...)
    word_frequency_plot <- ggplot(word_freq, aes(x = reorder(feature, frequency), y = frequency)) +
      geom_point(colour = "#5f7994", size = 1) +
      coord_flip() +
      labs(x = NULL, y = "Word frequency") +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "none",
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
    return(word_frequency_plot)
  }


#' @title Examine Highest Per-Term Per-Topic Probabilities
#'
#' @description
#' Given a tidy data frame of word-topic probabilities (beta values) from an STM model,
#' this function extracts the top terms for each topic.
#'
#' @param beta_td A tidy data frame from \code{tidytext::tidy(stm_model, matrix = "beta")},
#'    where \code{stm_model} is a fitted Structural Topic Model created using \code{stm::stm()}.
#' @param top_n The number of top terms per topic to return.
#' @param ... Further arguments passed to \code{dplyr::group_by}.
#'
#' @return A \code{tibble} containing the top \code{top_n} terms for each topic. The output includes
#' columns for \code{topic}, \code{term}, and \code{beta} values, restricted to the highest-probability terms.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- df %>%
#'  preprocess_texts(text_field = "abstract") %>%
#'  quanteda::dfm()
#' out <- quanteda::convert(dfm_object, to = "stm")
#' stm_model <- stm(out$documents,
#'                 out$vocab,
#'                 data = out$meta,
#'                 prevalence = ~
#'                   I((year >= 1980)*(year - 1980)) +
#'                   I((year >= 1990)*(year - 1990)) +
#'                   I((year >= 2000)*(year - 2000)) +
#'                   I((year >= 2010)*(year - 2010)),
#'                 max.em.its = 75,
#'                 init.type = 'Spectral',
#'                 K = 15,
#'                 verbose = FALSE)
#'
#' beta_td <- tidytext::tidy(stm_model, matrix="beta")
#' top_terms <- examine_top_terms(beta_td, top_n = 5)
#' head(top_terms)
#' }
examine_top_terms <-
  function(beta_td, top_n, ...) {
    topic_term <- beta_td %>%
      group_by(topic, ...) %>%
      top_n(top_n, beta) %>%
      ungroup()
    return(topic_term)
  }


#' @title Plot Topic Per-term Per-topic Probabilities
#'
#' @description
#' Given per-term per-topic probabilities (beta), this function creates a plot of the top terms in each topic.
#'
#' @param beta_td A tidy data frame from \code{tidytext::tidy(stm_model, matrix = "beta")},
#'    where \code{stm_model} is a fitted Structural Topic Model created using \code{stm::stm()}.
#' @param ncol The number of columns in the facet plot.
#' @param topic_names An optional character vector for labeling topics. If provided, must be the same length as the number of topics.
#' @param ... Further arguments passed to \code{dplyr::group_by}.
#'
#' @return A \code{ggplot} object showing a facet-wrapped chart of top terms for each topic,
#' ordered by their per-topic probability (beta). Each facet represents a topic.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Assume stm_model is a fitted STM model.
#'   beta_td <- tidytext::tidy(stm_model, matrix = "beta", log = FALSE)
#'   plot <- plot_topic_term(beta_td, ncol = 3)
#'   print(plot)
#' }
#'
#' @importFrom stats reorder
#' @importFrom numform ff_num
#'
plot_topic_term <-
  function(beta_td, ncol = 3, topic_names = NULL, ...) {

    topic_term <- beta_td %>%
      mutate(
        ord = factor(topic, levels = c(min(topic): max(topic))),
        tt = as.numeric(topic),
        topic = paste("Topic", topic),
        term = tidytext::reorder_within(term, beta, topic)
      ) %>%
      arrange(ord)

    levelt = paste("Topic", topic_term$ord) %>% unique()
    topic_term$topic = factor(topic_term$topic, levels = levelt)

    if(!is.null(topic_names)){
      topic_term$topic = topic_names[topic_term$tt]
      topic_term <- topic_term %>%
        mutate(topic = as.character(topic)) %>%
        mutate(topic = ifelse(!is.na(topic), topic, paste('Topic', tt)))
      topic_term$topic = factor(topic_term$topic, levels = unique(topic_term$topic))
    }

    topic_term$tt = NULL

    ggplot(topic_term, aes(term, beta, fill = topic)) +
      geom_col(show.legend = FALSE, alpha = 0.8) +
      facet_wrap(~ topic, scales = "free", ncol = ncol) +
      tidytext::scale_x_reordered() +
      scale_y_continuous(labels = ff_num(zero = 0, digits = 3)) +
      coord_flip() +
      xlab("") +
      ylab("Word probability") +
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
  }


#' @title Plot Per-Document Per-Topic Probabilities
#'
#' @description
#' Given a tidy data frame of per-document per-topic probabilities (gamma),
#' this function calculates the mean topic prevalence across documents and plots the top topics.
#'
#' @param gamma_td A tidy data frame derived from \code{tidytext::tidy(stm_model, matrix = "gamma")},
#'   where \code{stm_model} is a fitted Structural Topic Model created using \code{stm::stm()}.
#' @param top_n The number of topics to display, ordered by their mean prevalence.
#'
#' @return A \code{ggplot} object showing a bar plot of topic prevalence. Topics are ordered by their
#' mean gamma value (average prevalence across documents).
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Assume stm_model is a fitted STM model.
#'   gamma_td <- tidytext::tidy(stm_model, matrix="gamma")
#'   plot <- topic_probability_plot(gamma_td, top_n = 10)
#'   print(plot)
#' }
topic_probability_plot <-
  function(gamma_td, top_n = 10) {

    gamma_terms <- gamma_td %>%
      group_by(topic) %>%
      summarise(gamma = mean(gamma)) %>%
      arrange(desc(gamma)) %>%
      mutate(topic = reorder(topic, gamma)) %>%
      top_n(top_n, gamma)

    ggplot(gamma_terms, aes(topic, gamma, fill = topic)) +
      geom_col(alpha = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = ff_num(zero = 0, digits = 2)) +
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
  }


#' @title Create a Table for Per-Document Per-Topic Probabilities
#'
#' @description
#' Given a tidy data frame of per-document per-topic probabilities (gamma),
#' this function calculates the mean prevalence of each topic and returns a table of the top topics.
#'
#' @param gamma_td A tidy data frame derived from \code{tidytext::tidy(stm_model, matrix = "gamma")},
#'   where \code{stm_model} is a fitted Structural Topic Model created using \code{stm::stm()}.
#' @param top_n The number of topics to display, ordered by their mean prevalence.
#'
#' @return A \code{tibble} containing columns \code{topic} and \code{gamma}, where \code{topic}
#' is a factor representing each topic (relabeled with a "Topic X" format), and \code{gamma} is the
#' mean topic prevalence across all documents. Numeric values are rounded to three decimal places.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Assume stm_model is a fitted STM model.
#'   gamma_td <- tidytext::tidy(stm_model, matrix="gamma")
#'   table <- topic_probability_table(gamma_td, top_n = 10)
#'   print(table)
#' }
topic_probability_table <-
  function(gamma_td, top_n = 10) {

    gamma_terms <- gamma_td %>%
      group_by(topic) %>%
      summarise(gamma = mean(gamma)) %>%
      arrange(desc(gamma)) %>%
      mutate(topic = reorder(topic, gamma)) %>%
      top_n(top_n, gamma) %>%
      mutate(tt = as.numeric(topic)) %>%
      mutate(ord = topic) %>%
      mutate(topic = paste('Topic', topic)) %>%
      arrange(ord)

    levelt = paste("Topic", gamma_terms$ord) %>% unique()
    gamma_terms$topic = factor(gamma_terms$topic, levels = levelt)

    gamma_terms %>%
      select(topic, gamma) %>%
      mutate_if(is.numeric, ~ round(., 3))
  }


#' @title Plot a Hierarchical Clustering Dendrogram
#'
#' @description
#' Perform hierarchical clustering on STM model's document-topic probabilities
#' and plot a dendrogram.
#'
#' @param gamma_matrix A document-topic probability matrix (output of `stm_model$gamma`).
#' @param method Clustering method (default is "ward.D2").
#'
#' @return A ggplot2 dendrogram plot.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Assume stm_model is a fitted STM model.
#'   gamma_matrix <- stm_model$gamma
#'   hierarchical_clustering(gamma_matrix, method = "ward.D2")
#' }
#'
#' @importFrom textmineR CalcHellingerDist
#' @importFrom ggdendro ggdendrogram
#' @importFrom stats hclust
#'
hierarchical_clustering <- function(gamma_matrix, method = "ward.D2") {

  if (!is.matrix(gamma_matrix)) {
    stop("`gamma_matrix` must be a matrix. Ensure you are passing `stm_model$gamma`.")
  }

  stm_dist <- textmineR::CalcHellingerDist(gamma_matrix, by_rows = FALSE)

  hclust_res <- stats::hclust(stats::as.dist(stm_dist), method = method)

  ggdendro::ggdendrogram(hclust_res, rotate = TRUE) +
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
}



#' @title Plot a Word Co-occurrence Network
#'
#' @description
#' Visualize the co-occurrence relationships between terms in the corpus based on pairwise counts.
#'
#' @param dfm_td A tidy data frame of terms (output of \code{tidytext::tidy(dfm_object)}),
#'   where \code{dfm_object} is a document-feature matrix created using \code{quanteda::dfm()}
#'   after preprocessing text data.
#' @param term_col The column name for terms (default is "term").
#' @param doc_col The column name for document IDs (default is "document").
#' @param co_occur_n Minimum number of co-occurrences for filtering terms (default is 2).
#'
#' @return A `ggraph` object visualizing the word co-occurrence network.
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "text") %>%
#'  quanteda::dfm()
#' dfm_td <- tidytext::tidy(dfm_object)
#' plot_word_co_occurrence_network(dfm_td,
#'                                term_col = "abstract",
#'                                doc_col = "document",
#'                                co_occur_n = 5)
#' }
#'
#'
plot_word_co_occurrence_network <- function(dfm_td, term_col = "term", doc_col = "document", co_occur_n = 2) {
  if (!all(c(term_col, doc_col) %in% colnames(dfm_td))) {
    stop("The specified `term_col` or `doc_col` is not present in the data.")
  }

  term_co_occur <- dfm_td %>%
    tibble::as_tibble() %>%
    widyr::pairwise_count(!!rlang::sym(term_col), !!rlang::sym(doc_col), sort = TRUE) %>%
    dplyr::filter(n >= co_occur_n)

  co_occur_graph <- igraph::graph_from_data_frame(term_co_occur, directed = FALSE)

  igraph::V(co_occur_graph)$centrality <- igraph::degree(co_occur_graph, mode = "out") / (igraph::vcount(co_occur_graph) - 1)

  layout <- ggraph::create_layout(co_occur_graph, layout = "fr")

  layout %>%
    ggraph() +
    geom_edge_link(aes(edge_alpha = n), edge_colour = "#b0aeae", edge_width = 1.5) +
    geom_node_point(aes(size = centrality, colour = centrality)) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      check_overlap = FALSE,
      size = 5
    ) +
    scale_color_continuous(name = "Centrality", guide = 'legend', high = "#47a0ed", low = "#deebf7") +
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
}


#' @title Plot a Word Correlation Network
#'
#' @description
#' Visualize the correlation relationships between terms in the corpus based on pairwise correlations.
#'
#' @param dfm_td A tidy data frame of terms (output of \code{tidytext::tidy(dfm_object)}),
#'   where \code{dfm_object} is a document-feature matrix created using \code{quanteda::dfm()}
#'   after preprocessing text data.
#' @param term_col The column name for terms (default is "term").
#' @param doc_col The column name for document IDs (default is "document").
#' @param co_occur_n Minimum number of co-occurrences for filtering terms (default is 2).
#' @param correlation_threshold Minimum correlation value to include edges in the graph (default is 0.3).
#'
#' @return A `ggraph` object visualizing the word correlation network.
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "text") %>%
#'  quanteda::dfm()
#' dfm_td <- tidytext::tidy(dfm_object)
#' plot_word_correlation_network(dfm_td,
#'                              term_col = "abstract",
#'                              doc_col = "document",
#'                              correlation_threshold = 0.3)
#' }
#'
#'
plot_word_correlation_network <- function(dfm_td, term_col = "term", doc_col = "document", co_occur_n = 2, correlation_threshold = 0.3) {
  if (!all(c(term_col, doc_col) %in% colnames(dfm_td))) {
    stop("The specified `term_col` or `doc_col` is not present in the data.")
  }

  term_cor <- dfm_td %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!rlang::sym(term_col)) %>%
    dplyr::filter(dplyr::n() >= co_occur_n) %>%
    widyr::pairwise_cor(!!rlang::sym(term_col), !!rlang::sym(doc_col), sort = TRUE)

  filtered_correlations <- term_cor %>%
    dplyr::filter(correlation > correlation_threshold)

  term_graph <- igraph::graph_from_data_frame(filtered_correlations, directed = FALSE)

  layout <- ggraph::create_layout(term_graph, layout = "fr")

  layout %>%
    ggraph() +
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
}


#' @title Visualize Word Frequency Trends Over Time
#'
#' @description
#' Analyze and visualize the change in word frequencies over time or any continuous variable.
#'
#' @param dfm_object A document-feature matrix (\code{dfm}).
#' @param gamma_td A tidy data frame derived from \code{tidytext::tidy(stm_model, matrix = "gamma")},
#'   where \code{stm_model} is a fitted Structural Topic Model created using \code{stm::stm()}.
#' @param time_variable The name of the continuous variable to group by.
#' @param selected_terms A vector of selected terms to analyze.
#'
#' @return A ggplot2 line plot showing word frequency trends.
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "abstract") %>%
#'  quanteda::dfm()
#' word_frequency_trends(dfm_object, gamma_td,
#'                      time_variable = "year",
#'                      selected_terms = c("computer", "disability"))
#'}
#'
#' @importFrom stats glm
#'
#' @title Word Frequency Trends Over Time
#'
#' @description Analyze and visualize word frequency trends over time for a fixed term column.
#'
#' @param dfm_object A `quanteda` dfm object representing document-feature matrix.
#' @param gamma_td A tidy data frame of document-topic probabilities (gamma).
#' @param time_variable The column name for the time variable (e.g., "year").
#' @param selected_terms A vector of terms to analyze trends for.
#'
#' @return A ggplot object showing word frequency trends over time.
#' @export
#'
#' @examples
#' if (interactive()) {
#' # Assume stm_model is a fitted STM model.
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "abstract") %>%
#'  quanteda::dfm()
#' gamma_td <- tidytext::tidy(stm_model, matrix = "gamma")
#' word_frequency_trends(dfm_object,
#'                      gamma_td, time_variable = "year",
#'                      selected_terms = c("computer", "disability"))
#' }
#'
#' @importFrom stats glm
word_frequency_trends <- function(dfm_object, gamma_td, time_variable, selected_terms) {

  dfm_object@docvars$document <- dfm_object@docvars$docname_

  dfm_td <- tidytext::tidy(dfm_object)

  dfm_gamma_td <- gamma_td %>%
    left_join(dfm_object@docvars,
              by = c("document" = "document")) %>%
    left_join(dfm_td, by = c("document" = "document"))

  word_trends <- dfm_gamma_td %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!rlang::sym(time_variable)) %>%
    dplyr::mutate(total_count = sum(count), percent = count / total_count) %>%
    dplyr::ungroup()

  trend_data <- word_trends %>%
    dplyr::filter(term %in% selected_terms)

  significance <- trend_data %>%
    dplyr::group_by(term) %>%
    dplyr::do(tidy(glm(
      cbind(count, total_count - count) ~ eval(parse(text = time_variable)),
      .,
      family = "binomial"
    ))) %>%
    dplyr::ungroup()

  ggplot(trend_data, aes(x = !!rlang::sym(time_variable), y = percent, color = term)) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", se = FALSE) +
    facet_wrap(~ term, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "") +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
      strip.text = element_text(size = 14, color = "#3B3B3B"),
      axis.text = element_text(size = 14, color = "#3B3B3B"),
      axis.title = element_text(size = 14, color = "#3B3B3B"),
      axis.title.x = element_text(margin = margin(t = 9)),
      axis.title.y = element_text(margin = margin(r = 9))
    )
}

