#!/usr/bin/env Rscript
#'
#' Runner script for Monte Carlo Option Pricing simulation
#' 
#' Usage: Rscript run_simulation.R [--simulations N] [--steps M] [--price S] [--strike K]

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Default parameters
n_simulations <- 10000
n_steps <- 252
S0 <- 100
K <- 105

# Parse arguments
if (length(args) > 0) {
  for (i in seq_along(args)) {
    if (args[i] == "--simulations" && i < length(args)) {
      n_simulations <- as.numeric(args[i + 1])
    } else if (args[i] == "--steps" && i < length(args)) {
      n_steps <- as.numeric(args[i + 1])
    } else if (args[i] == "--price" && i < length(args)) {
      S0 <- as.numeric(args[i + 1])
    } else if (args[i] == "--strike" && i < length(args)) {
      K <- as.numeric(args[i + 1])
    } else if (args[i] == "--help") {
      cat("Usage: Rscript run_simulation.R [options]\n")
      cat("Options:\n")
      cat("  --simulations N   Number of simulations (default: 10000)\n")
      cat("  --steps M        Number of time steps (default: 252)\n")
      cat("  --price S        Initial stock price (default: 100)\n")
      cat("  --strike K       Strike price (default: 105)\n")
      cat("  --help           Show this help message\n")
      quit()
    }
  }
}

cat("Running Monte Carlo Option Pricing Simulation with parameters:\n")
cat("Simulations:", n_simulations, "\n")
cat("Time steps:", n_steps, "\n")
cat("Initial price:", S0, "\n")
cat("Strike price:", K, "\n\n")

# Source main script with modified parameters
source("R/main.R")