# Auto-install missing packages
required_packages <- c(
    "shiny",
    "shinythemes",
    "DT",
    "shinydisconnect",
    "shiny.i18n",
    "shinyvalidate",
    "tidyr",
    "dplyr",
    "purrr",
    "readr",
    "stringr",
    "ggplot2",
    "broom",
    "cowplot",
    "multcompView",
    "ggstatsplot",
    "showtext",
    "PMCMRplus",
    "markdown",
    "Rcpp"
)

installed_pkgs <- installed.packages()[, "Package"]
missing_pkgs <- required_packages[!(required_packages %in% installed_pkgs)]

if (length(missing_pkgs) > 0) {
    message(
        "Installing missing packages: ",
        paste(missing_pkgs, collapse = ", ")
    )
    install.packages(missing_pkgs, repos = "https://cloud.r-project.org/")
}

# Source the C++ code globally so it's available to both server and UI (if needed)
# server.R currently sources it, but global is a good place too.
# However, sourceCpp implies loading Rcpp.
library(Rcpp)
# We won't source C++ here to avoid conflicts with server.R logic,
# but ensuring Rcpp is installed is the key.
