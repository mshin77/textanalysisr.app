FROM rocker/shiny-verse

LABEL maintainer="Mikyung Shin <shin.mikyung@gmail.com>"
LABEL description="Docker image for TextAnalysisR Shiny application"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev 
    
RUN sudo apt-get install -y r-cran-tidyverse
COPY Rprofile.site /etc/R
ENV _R_SHLIB_STRIP_=true
RUN install2.r -r 'https://packagemanager.rstudio.com/all/__linux__/focal/latest' remotes
COPY DESCRIPTION .
  
RUN R -e "install.packages('ggplot2', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages(c('numform', 'utils', 'purrr'), repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('plotly', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('dplyr', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('quanteda', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('stm', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('stminsights', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('tidyr', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('tidytext', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('textmineR', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('tibble', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages(c('stats', 'scales', 'readxl'), repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('ggraph', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('ggdendro', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('widyr', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('markdown', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('shiny', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('shinyjs', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('shinyBS', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('DT', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('shinycssloaders', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
RUN R -e "install.packages('quanteda.textstats', repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest')"
   
RUN rm -f DESCRIPTION

RUN
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app

COPY app .

RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]