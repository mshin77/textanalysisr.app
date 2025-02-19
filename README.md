<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" width=150px alt="TextAnalysisR Logo">

<!-- badges: start -->
<!-- badges: end -->

`TextAnalysisR` provides a supporting workflow tool for text mining
analysis. The web app incorporates
[quanteda](https://github.com/quanteda/quanteda) (text preprocessing),
[stm](https://github.com/bstewart/stm) (structural topic modeling),
[ggraph](https://github.com/thomasp85/ggraph), and
[widyr](https://github.com/juliasilge/widyr) (network analysis).
[tidytext](https://github.com/cran/tidytext) is implemented to tidy
non-tidy format objects. The R Shiny web app is available at
`TextAnalysisR::TextAnalysisR.app()` or <https://textanalysisr.org>.
Functions are provided for completing word-topic probabilities,
document-topic probabilities, estimated effects of covariates on topic
prevalence, and network analysis, similar to tasks available in the web
app.

## Installation

The development version from
[GitHub](https://github.com/mshin77/TextAnalysisR) with:

    install.packages("devtools")
    devtools::install_github("mshin77/TextAnalysisR")

## Load the TextAnalysisR Package

    library(TextAnalysisR)

## Alternatively, Launch and Browse the Shiny App

Access the web app at <https://www.textanalysisr.org>.

Launch and browser the TextAnalysisR.app on the local computer:

    TextAnalysisR.app()

## Citation

-   Shin, M. (2025). *TextAnalysisR: A text mining workflow tool* (R
    package version 0.0.2) \[Computer software\].
    <https://mshin77.github.io/TextAnalysisR>

-   Shin, M. (2025). *TextAnalysisR: A text mining workflow tool* \[Web
    application\]. <https://www.textanalysisr.org>

## References

-   Shin, M., Park, H., & Kang, E. (2024). Development of education
    policies and practices for students with learning disabilities in
    South Korea using Delphi surveys and topic modeling. *Learning
    Disability Quarterly*.
    [GitHub](https://github.com/mshin77/Korea-LD-policy)

-   Shin, M., Ok, M. W., Choo, S., Hossain, G., Bryant, D. P., &
    Kang, E. (2023). A content analysis of research on technology use
    for teaching mathematics to students with disabilities: word
    networks and topic modeling. *International Journal of STEM
    Education, 10*(1), 1-23.
    [GitHub](https://github.com/mshin77/math-tech-sped)
