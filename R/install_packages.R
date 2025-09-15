#' Install required packages for Monte Carlo Option Pricing simulation
#' 
#' This script installs all necessary packages for the simulation.

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}

# Required packages
packages <- c(
  "plotly",
  "dplyr",
  "ggplot2",
  "reshape2",
  "knitr",
  "rmarkdown",
  "htmlwidgets"
)

# Install packages
cat("Installing required packages...\n")
for (pkg in packages) {
  install_if_missing(pkg)
}

cat("All packages installed successfully!\n")