
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
#'   tokens <- TextAnalysisR::preprocess_texts(df, text_field = "abstract")
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
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- df %>%
#'   TextAnalysisR::preprocess_texts(text_field = "abstract") %>%
#'   quanteda::dfm() %>%
#'   quanteda::dfm_trim(min_termfreq = 1, min_docfreq = 1) %>%
#'   .[quanteda::ntoken(.) > 0, ]
#' TextAnalysisR::plot_word_frequency(dfm_object, n = 20)
#' }
plot_word_frequency <-
  function(dfm_object, n = 20, ...) {
    word_freq <- quanteda.textstats::textstat_frequency(dfm_object, n = n, ...)
    word_frequency_plot <- ggplot(word_freq, aes(x = reorder(feature, frequency), y = frequency)) +
      geom_point(colour = "#5f7994", size = 1) +
      coord_flip() +
      labs(x = NULL, y = "Word frequency") +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#3B3B3B", linewidth = 0.3),
        axis.ticks = element_line(color = "#3B3B3B", linewidth = 0.3),
        strip.text.x = element_text(size = 11, color = "#3B3B3B"),
        axis.text.x = element_text(size = 11, color = "#3B3B3B"),
        axis.text.y = element_text(size = 11, color = "#3B3B3B"),
        axis.title = element_text(size = 11, color = "#3B3B3B"),
        axis.title.x = element_text(margin = margin(t = 9)),
        axis.title.y = element_text(margin = margin(r = 9))
      )
    return(word_frequency_plot)
  }


#' @title Evaluate Optimal Number of Topics
#'
#' @description
#' This function performs a search for the optimal number of topics (K) using \code{stm::searchK}
#' and visualizes diagnostics, including held-out likelihood, residuals, semantic coherence,
#' and lower bound metrics.
#'
#' @param dfm_object A quanteda document-feature matrix (dfm).
#' @param K_range A numeric vector specifying the range of topics (K) to search over.
#' @param max.em.its Maximum number of EM iterations (default: 75).
#' @param categorical_var An optional character string for a categorical variable in the metadata.
#' @param continuous_var An optional character string for a continuous variable in the metadata.
#' @param verbose Logical; if \code{TRUE}, prints progress information (default: TRUE).
#' @param ... Further arguments passed to \code{stm::searchK}.
#'
#' @return A Plotly object showing the diagnostics for the number of topics (K).
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  df <- TextAnalysisR::SpecialEduTech
#'  dfm_object <- df %>%
#'    TextAnalysisR::preprocess_texts(text_field = "abstract") %>%
#'    quanteda::dfm() %>%
#'    quanteda::dfm_trim(min_termfreq = 1, min_docfreq = 1) %>%
#'    .[quanteda::ntoken(.) > 0, ]
#'  TextAnalysisR::evaluate_optimal_topic_number(
#'    dfm_object = dfm_object,
#'    K_range = 5:30,
#'    max.em.its = 75,
#'    categorical_var = "reference_type",
#'    continuous_var = "year")
#' }
#'
#' @importFrom quanteda convert
#' @importFrom stm searchK
#' @importFrom plotly plot_ly subplot layout
evaluate_optimal_topic_number <- function(dfm_object, K_range, max.em.its = 75, categorical_var = NULL, continuous_var = NULL, verbose = TRUE, ...) {

  out <- quanteda::convert(dfm_object, to = "stm")
  if (!all(c("meta", "documents", "vocab") %in% names(out))) {
    stop("Conversion of dfm_outcome must result in 'meta', 'documents', and 'vocab'.")
  }

  meta <- out$meta
  documents <- out$documents
  vocab <- out$vocab

  prevalence_formula <- NULL

  if (!is.null(categorical_var) && !is.null(continuous_var)) {
    prevalence_formula <- reformulate(c(categorical_var, sprintf("s(%s)", continuous_var)))
  } else if (!is.null(categorical_var)) {
    prevalence_formula <- reformulate(categorical_var)
  } else if (!is.null(continuous_var)) {
    prevalence_formula <- reformulate(sprintf("s(%s)", continuous_var))
  }

  stm_model <- stm::searchK(
    data = meta,
    documents = documents,
    vocab = vocab,
    K = K_range,
    prevalence = prevalence_formula,
    max.em.its = max.em.its,
    init.type = "Spectral",
    verbose = verbose,
    ...
  )

  stm_model$results$heldout <- as.numeric(stm_model$results$heldout)
  stm_model$results$residual <- as.numeric(stm_model$results$residual)
  stm_model$results$semcoh <- as.numeric(stm_model$results$semcoh)
  stm_model$results$lbound <- as.numeric(stm_model$results$lbound)

  p1 <- plotly::plot_ly(
    data = stm_model$results,
    x = ~K,
    y = ~heldout,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Held-out Likelihood:", round(heldout, 3)),
    hoverinfo = 'text'
  )

  p2 <- plotly::plot_ly(
    data = stm_model$results,
    x = ~K,
    y = ~residual,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Residuals:", round(residual, 3)),
    hoverinfo = 'text'
  )

  p3 <- plotly::plot_ly(
    data = stm_model$results,
    x = ~K,
    y = ~semcoh,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Semantic Coherence:", round(semcoh, 3)),
    hoverinfo = 'text'
  )

  p4 <- plotly::plot_ly(
    data = stm_model$results,
    x = ~K,
    y = ~lbound,
    type = 'scatter',
    mode = 'lines+markers',
    text = ~paste("K:", K, "<br>Lower Bound:", round(lbound, 3)),
    hoverinfo = 'text'
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
}


#' @title Plot Highest Word Probabilities for Each Topic
#'
#' @description
#' This function provides a visualization of the top terms for each topic,
#' ordered by their word probability distribution for each topic (beta).
#'
#' @param dfm_object A quanteda document-feature matrix (dfm).
#' @param K_number The number of topics to display.
#' @param max.em.its Maximum number of EM iterations (default: 75).
#' @param categorical_var An optional character string for a categorical variable in the metadata.
#' @param continuous_var An optional character string for a continuous variable in the metadata.
#' @param verbose Logical; if \code{TRUE}, prints progress information (default: TRUE).
#' @param top_n The number of top terms to display for each topic.
#' @param ncol The number of columns in the facet plot.
#' @param topic_names An optional character vector for labeling topics. If provided, must be the same length as the number of topics.
#' @param height The height of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#' @param width The width of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#' @param ... Further arguments passed to \code{stm::searchK}.
#'
#' @return A \code{Plotly} object showing a facet-wrapped chart of top terms for each topic,
#' ordered by their per-topic probability (beta). Each facet represents a topic.
#'
#' @details
#' If \code{topic_names} is provided, it replaces the default "Topic \{n\}" labels with custom names.
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- df %>%
#'   TextAnalysisR::preprocess_texts(text_field = "abstract") %>%
#'   quanteda::dfm() %>%
#'   quanteda::dfm_trim(min_termfreq = 1, min_docfreq = 1) %>%
#'   .[quanteda::ntoken(.) > 0, ]
#' TextAnalysisR::plot_word_probabilities(
#'  dfm_object = dfm_object,
#'  K_number = 15,
#'  max.em.its = 75,
#'  categorical_var = "reference_type",
#'  continuous_var = "year",
#'  top_n = 10,
#'  ncol = 3,
#'  height = 2000,
#'  width = 1000,
#'  verbose = TRUE)
#' }
#'
#' @importFrom stats reorder
#' @importFrom numform ff_num
#' @importFrom plotly ggplotly layout
#' @importFrom tidytext reorder_within scale_x_reordered
plot_word_probabilities <- function(dfm_object, K_number, max.em.its = 75, categorical_var = NULL, continuous_var = NULL,
                            top_n = 10, ncol = 3, topic_names = NULL, height = 2000, width = 1000, verbose = TRUE, ...) {

  out <- quanteda::convert(dfm_object, to = "stm")
  if (!all(c("meta", "documents", "vocab") %in% names(out))) {
    stop("Conversion of dfm_outcome must result in 'meta', 'documents', and 'vocab'.")
  }

  meta <- out$meta
  documents <- out$documents
  vocab <- out$vocab

  prevalence_formula <- NULL

  if (!is.null(categorical_var) && !is.null(continuous_var)) {
    prevalence_formula <- reformulate(c(categorical_var, sprintf("s(%s)", continuous_var)))
  } else if (!is.null(categorical_var)) {
    prevalence_formula <- reformulate(categorical_var)
  } else if (!is.null(continuous_var)) {
    prevalence_formula <- reformulate(sprintf("s(%s)", continuous_var))
  }

  stm_model <- stm::stm(
    data = meta,
    documents = documents,
    vocab = vocab,
    K = K_number,
    prevalence = prevalence_formula,
    max.em.its = max.em.its,
    init.type = "Spectral",
    verbose = verbose,
    ...
  )

  beta_td <- tidytext::tidy(stm_model, matrix = "beta")

  topic_term_plot <- beta_td %>%
    group_by(topic) %>%
    slice_max(order_by = beta, n = top_n) %>%
    ungroup() %>%
    mutate(
      ord = factor(topic, levels = c(min(topic):max(topic))),
      tt = as.numeric(topic),
      topic = paste("Topic", topic),
      term = reorder_within(term, beta, topic)
    ) %>%
    arrange(ord) %>%
    ungroup()

  levelt = paste("Topic", topic_term_plot$ord) %>% unique()
  topic_term_plot$topic = factor(topic_term_plot$topic,
                                 levels = levelt)

  topic_term_plot_gg <- ggplot(
    topic_term_plot,
    aes(term, beta, fill = topic, text = paste("Topic:", topic, "<br>Beta:", sprintf("%.3f", beta)))
  ) +
    geom_col(show.legend = FALSE, alpha = 0.8) +
    facet_wrap(~ topic, scales = "free", ncol = ncol, strip.position = "top") +
    scale_x_reordered() +
    scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
    coord_flip() +
    xlab("") +
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

  plotly::ggplotly(topic_term_plot_gg, height = height, width = width, tooltip = "text") %>%
    plotly::layout(
      margin = list(t = 40, b = 40)
    )
}

#' @title Plot Mean Topic Prevalence Across Documents
#'
#' @description
#' This function calculates the mean topic prevalence across documents and plots the top topics.
#'
#' @param dfm_object A quanteda document-feature matrix (dfm).
#' @param K_number The number of topics to display.
#' @param max.em.its Maximum number of EM iterations (default: 75).
#' @param categorical_var An optional character string for a categorical variable in the metadata.
#' @param continuous_var An optional character string for a continuous variable in the metadata.
#' @param top_n The number of topics to display, ordered by their mean prevalence.
#' @param height The height of the resulting Plotly plot, in pixels. Defaults to \code{500}.
#' @param width The width of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#' @param verbose Logical; if \code{TRUE}, prints progress information (default: TRUE).
#' @param ... Further arguments passed to \code{stm::searchK}.
#'
#' @return A \code{ggplot} object showing a bar plot of topic prevalence. Topics are ordered by their
#' mean gamma value (average prevalence across documents).
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'  df <- TextAnalysisR::SpecialEduTech
#'  dfm_object <- df %>%
#'    TextAnalysisR::preprocess_texts(text_field = "abstract") %>%
#'    quanteda::dfm() %>%
#'    quanteda::dfm_trim(min_termfreq = 1, min_docfreq = 1) %>%
#'    .[quanteda::ntoken(.) > 0, ]
#' TextAnalysisR::plot_mean_topic_prevalence(
#'   dfm_object = dfm_object,
#'   K_number = 15,
#'   max.em.its = 75,
#'   categorical_var = "reference_type",
#'   continuous_var = "year",
#'   top_n = 15,
#'   height = 500,
#'   width = 1000,
#'   verbose = TRUE)
#' }
#'
#' @importFrom stats reorder
#' @importFrom numform ff_num
#' @importFrom plotly ggplotly layout
#' @importFrom tidytext reorder_within scale_x_reordered
plot_mean_topic_prevalence <- function(dfm_object, K_number, max.em.its = 75, categorical_var = NULL, continuous_var = NULL,
                                       top_n = 15, height = 500, width = 1000, verbose = TRUE, ...) {

  out <- quanteda::convert(dfm_object, to = "stm")
  if (!all(c("meta", "documents", "vocab") %in% names(out))) {
    stop("Conversion of dfm_outcome must result in 'meta', 'documents', and 'vocab'.")
  }

  meta <- out$meta
  documents <- out$documents
  vocab <- out$vocab

  prevalence_formula <- NULL

  if (!is.null(categorical_var) && !is.null(continuous_var)) {
    prevalence_formula <- reformulate(c(categorical_var, sprintf("s(%s)", continuous_var)))
  } else if (!is.null(categorical_var)) {
    prevalence_formula <- reformulate(categorical_var)
  } else if (!is.null(continuous_var)) {
    prevalence_formula <- reformulate(sprintf("s(%s)", continuous_var))
  }

  stm_model <- stm::stm(
    data = meta,
    documents = documents,
    vocab = vocab,
    K = K_number,
    prevalence = prevalence_formula,
    max.em.its = max.em.its,
    init.type = "Spectral",
    verbose = verbose,
    ...
  )

  gamma_td <- tidytext::tidy(stm_model, matrix = "gamma")
  beta_td <- tidytext::tidy(stm_model, matrix = "beta")

  # Get top terms for each topic
  top_terms <- beta_td %>%
    group_by(topic) %>%
    slice_max(order_by = beta, n = top_n) %>%
    summarise(terms = paste(term, collapse = ", "))

  gamma_terms <- gamma_td %>%
    group_by(topic) %>%
    summarise(gamma = mean(gamma)) %>%
    arrange(desc(gamma)) %>%
    mutate(topic = reorder(topic, gamma)) %>%
    top_n(top_n, gamma) %>%
    left_join(top_terms, by = "topic")  # Join with top terms

  gamma_term_gg <- ggplot(gamma_terms,
                          aes(topic, gamma, label = terms,
                              fill = topic,
                              text = paste("Topic:",
                                           topic, "<br>Terms:",
                                           terms, "<br>Gamma:",
                                           sprintf("%.3f", gamma)))) +
    geom_col(alpha = 0.8) +
    coord_flip() +
    scale_y_continuous(labels = numform::ff_num(zero = 0, digits = 3)) +
    xlab("") +
    ylab("Topic proportion") +
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
      axis.title.y = element_text(margin = margin(r = 9))
    )

  plotly::ggplotly(gamma_term_gg, tooltip = "text", height = height, width = width) %>%
    plotly::layout(
      margin = list(t = 40, b = 40)
    )
}

#' @title Plot a Word Co-occurrence Network
#'
#' @description
#' Visualize the co-occurrence relationships between terms in the corpus based on pairwise counts.
#'
#' @param dfm_object A quanteda document-feature matrix (dfm).
#' @param term_col The column name for terms (default is "term").
#' @param document The column name for document IDs (default is "document").
#' @param co_occur_n Minimum number of co-occurrences for filtering terms (default is 2).
#' @param height The height of the resulting Plotly plot, in pixels. Defaults to \code{700}.
#' @param width The width of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#'
#' @return A Plotly object visualizing the interactive word co-occurrence network.
#' @export
#'
#' @examples
#' if (interactive()) {
#'  df <- TextAnalysisR::SpecialEduTech
#'  dfm_object <- df %>%
#'    TextAnalysisR::preprocess_texts(text_field = "abstract") %>%
#'    quanteda::dfm() %>%
#'    quanteda::dfm_trim(min_termfreq = 1, min_docfreq = 1) %>%
#'    .[quanteda::ntoken(.) > 0, ]
#' TextAnalysisR::plot_word_co_occurrence_network(dfm_object,
#'                                term_col = "abstract",
#'                                document = "document",
#'                                co_occur_n = 5,
#'                                height = 700,
#'                                width = 1000)
#' }
#'
#'
plot_word_co_occurrence_network <- function(dfm_object,
                                            term_col = "term",
                                            document = "document",
                                            co_occur_n = 2,
                                            height = 700,
                                            width = 1000) {

  dfm_td <- tidytext::tidy(dfm_object)

  term_co_occur <- dfm_td %>%
    tibble::as_tibble() %>%
    widyr::pairwise_count(!!rlang::sym(term_col), !!rlang::sym(document), sort = TRUE) %>%
    dplyr::filter(n >= co_occur_n)

  co_occur_graph <- igraph::graph_from_data_frame(term_co_occur, directed = FALSE)

  igraph::V(co_occur_graph)$centrality <- igraph::degree(co_occur_graph, mode = "out") / (igraph::vcount(co_occur_graph) - 1)

  ggraph_gg <- layout %>%
    ggraph() +
    geom_edge_link(
      aes(edge_alpha = n, text = paste("Co-occurrence:", n)),
      edge_colour = "#b0aeae",
      edge_width = 1.5
    ) +
    geom_node_point(
      aes(size = centrality, colour = centrality, text = paste("Word:", name, "<br>Centrality:", round(centrality, 2))),
      show.legend = TRUE
    ) +
    geom_node_text(
      aes(label = name),
      repel = TRUE,
      check_overlap = FALSE,
      size = 5
    ) +
    scale_color_continuous(
      name = "Centrality",
      guide = 'legend',
      high = "#47a0ed",
      low = "#deebf7"
    ) +
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

  ggplotly(ggraph_gg, tooltip = "text", height = height, width = width)
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
#' @param height The height of the resulting Plotly plot, in pixels. Defaults to \code{700}.
#' @param width The width of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#'
#' @return A Plotly object visualizing the interactive word correlation network.
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "text") %>%
#'  quanteda::dfm()
#' dfm_td <- tidytext::tidy(dfm_object)
#' TextAnalysisR::plot_word_correlation_network(dfm_td,
#'                              term_col = "abstract",
#'                              doc_col = "document",
#'                              correlation_threshold = 0.3,
#'                              height = 700,
#'                              width = 1000)
#' }
#'
#'
plot_word_correlation_network <- function(dfm_td, term_col = "term",
                                          doc_col = "document", co_occur_n = 2,
                                          correlation_threshold = 0.3,
                                          height = 700, width = 1000) {
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

  ggraph_gg <- layout %>%
    ggraph() +
    geom_edge_link(
      aes(
        edge_alpha = correlation,
        edge_width = correlation,
        text = paste("Correlation:", round(correlation, 2))
      ),
      edge_colour = "#47a0ed"
    ) +
    geom_node_point(
      aes(size = centrality, text = paste("Term:", name, "<br>Centrality:", round(centrality, 2))),
      color = "white"
    ) +
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

  ggplotly(ggraph_gg, tooltip = "text", height = height, width = width)
}


#' @title Word Frequency Trends Over Time
#'
#' @description Analyze and visualize word frequency trends over time for a fixed term column.
#'
#' @param dfm_object A `quanteda` dfm object representing document-feature matrix.
#' @param gamma_td A tidy data frame of document-topic probabilities (gamma).
#' @param time_variable The column name for the time variable (e.g., "year").
#' @param selected_terms A vector of terms to analyze trends for.
#' @param height The height of the resulting Plotly plot, in pixels. Defaults to \code{700}.
#' @param width The width of the resulting Plotly plot, in pixels. Defaults to \code{1000}.
#'
#' @return A Plotly object showing interactive word frequency trends over time.
#' @export
#'
#' @examples
#' if (interactive()) {
#' df <- TextAnalysisR::SpecialEduTech
#' dfm_object <- preprocess_texts(df, text_field = "abstract") %>%
#'  quanteda::dfm()
#' out <- quanteda::convert(dfm_object, to = "stm")
#' stm_15 <- stm::stm(out$documents,
#'                   out$vocab,
#'                   data = out$meta,
#'                   prevalence = ~
#'                     I((year >= 1980)*(year - 1980)) +
#'                     I((year >= 1990)*(year - 1990)) +
#'                     I((year >= 2000)*(year - 2000)) +
#'                     I((year >= 2010)*(year - 2010)),
#'                   max.em.its = 75,
#'                   init.type = 'Spectral',
#'                   K = 15,
#'                   verbose = FALSE)
#' stm_model <- stm_15
#' gamma_td <- tidytext::tidy(stm_model, matrix = "gamma")
#' TextAnalysisR::word_frequency_trends(dfm_object,
#'                                     gamma_td, time_variable = "year",
#'                                     selected_terms = c("computer", "disability"),
#'                                     height = 700, width = 1000)
#' }
#'
#' @importFrom stats glm reformulate
#' @importFrom plotly ggplotly
word_frequency_trends <- function(dfm_object, gamma_td, time_variable, selected_terms, height = 700, width = 1000) {

  dfm_object@docvars$document <- dfm_object@docvars$docname_

  dfm_td <- tidytext::tidy(dfm_object)

  dfm_gamma_td <- gamma_td %>%
    left_join(dfm_object@docvars, by = c("document" = "document")) %>%
    left_join(dfm_td, by = c("document" = "document"))

  word_trends <- dfm_gamma_td %>%
    tibble::as_tibble() %>%
    dplyr::group_by(!!rlang::sym(time_variable)) %>%
    dplyr::mutate(
      total_count = sum(count),
      term_proportion = count / total_count
    ) %>%
    dplyr::ungroup()

  trend_data <- word_trends %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    dplyr::filter(term %in% selected_terms)

  significance <- trend_data %>%
    dplyr::group_by(term) %>%
    dplyr::do(tidy(glm(
      cbind(count, total_count - count) ~ !!rlang::sym(time_variable),
      data = .,
      family = "binomial"
    ))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
    dplyr::arrange(desc(abs(estimate)))

  year_term_gg <- ggplot(trend_data, aes(
    x = !!rlang::sym(time_variable),
    y = term_proportion,
    group = term
  )) +
    geom_point(size = 1, alpha = 0.6, color = "#636363") +
    geom_smooth(method = "loess", se = TRUE, color = "#337ab7", linewidth = 0.5, formula = y ~ x) +
    facet_wrap(~ term, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(x = "", y = "") +
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
      axis.title.y = element_text(margin = margin(r = 9))
    )

  plotly::ggplotly(year_term_gg, height = height, width = width) %>%
    plotly::layout(
      margin = list(l = 40, r = 150, t = 60, b = 40)
    )
}
