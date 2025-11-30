FROM rocker/shiny-verse:latest

LABEL maintainer="Mikyung Shin <shin.mikyung@gmail.com>"
LABEL description="Docker image for TextAnalysisR: Text Mining Workflow Tool (Version 0.0.3)"

# System dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    libglpk40 \
    libssl-dev \
    libsasl2-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpoppler-cpp-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN mirror
ENV CRAN_MIRROR=https://packagemanager.posit.co/cran/__linux__/jammy/latest

# Core dependencies
RUN install2.r -r ${CRAN_MIRROR} \
    remotes \
    devtools \
    Matrix \
    ggplot2 \
    dplyr \
    tidyr \
    tibble \
    rlang \
    magrittr

# Text analysis packages
RUN install2.r -r ${CRAN_MIRROR} \
    quanteda \
    quanteda.textstats \
    tidytext \
    stm \
    widyr

# Visualization packages
RUN install2.r -r ${CRAN_MIRROR} \
    plotly \
    ggraph \
    igraph \
    visNetwork \
    RColorBrewer \
    scales

# Shiny packages
RUN install2.r -r ${CRAN_MIRROR} \
    shiny \
    shinyjs \
    shinybusy \
    DT \
    htmltools \
    htmlwidgets

# Additional dependencies
RUN install2.r -r ${CRAN_MIRROR} \
    broom \
    numform \
    purrr \
    readxl \
    pdftools \
    jsonlite \
    httr \
    reticulate \
    syuzhet \
    textdata

# Install TextAnalysisR from GitHub
RUN R -e "remotes::install_github('mshin77/TextAnalysisR', dependencies = TRUE)"

# Expose Shiny port
EXPOSE 3838

# Run Shiny app
CMD ["R", "-e", "TextAnalysisR::run_app(host = '0.0.0.0', port = 3838)"]
