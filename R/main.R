#' Main Monte Carlo Option Pricing Simulation
#' 
#' This is the main script that runs the complete simulation.

# Load required packages and functions
source("R/install_packages.R")
source("R/functions.R")

# Set seed for reproducibility
set.seed(123)

# Simulation parameters
n_simulations <- 10000
n_steps <- 252
T <- 1
dt <- T / n_steps
S0 <- 100
K <- 105
r <- 0.05
sigma <- 0.2
mu <- 0.08

cat("Starting Monte Carlo Option Pricing Simulation...\n")
cat("Parameters:\n")
cat("- Simulations:", n_simulations, "\n")
cat("- Time steps:", n_steps, "\n")
cat("- Initial price:", S0, "\n")
cat("- Strike price:", K, "\n")
cat("- Risk-free rate:", r, "\n")
cat("- Volatility:", sigma, "\n")
cat("- Time to expiration:", T, "years\n\n")

# Generate price paths
cat("Generating price paths...\n")
price_paths <- generate_price_paths(n_simulations, n_steps, S0, mu, sigma, dt)

# Calculate option payoffs
final_prices <- price_paths[n_steps + 1, ]
call_payoffs <- pmax(final_prices - K, 0)
put_payoffs <- pmax(K - final_prices, 0)

# Discount to present value
call_price <- mean(call_payoffs) * exp(-r * T)
put_price <- mean(put_payoffs) * exp(-r * T)

# Black-Scholes comparison
bs_prices <- black_scholes(S0, K, r, sigma, T)
call_bs <- bs_prices$call
put_bs <- bs_prices$put

# Create results directory structure
dir.create("results/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("results/reports", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# Prepare data for visualization
time <- seq(0, T, length.out = n_steps + 1)
sample_paths <- price_paths[, sample(1:n_simulations, 100)]
path_data <- data.frame(
  Time = rep(time, ncol(sample_paths)),
  Price = as.vector(sample_paths),
  Path = rep(1:ncol(sample_paths), each = nrow(sample_paths))
)

# Create and save plots
cat("Creating visualizations...\n")
paths_plot <- create_paths_plot(path_data, S0, K, T, n_simulations)
htmlwidgets::saveWidget(paths_plot, "results/plots/price_paths.html", selfcontained = TRUE)

# Payoff distribution plot
payoff_data <- data.frame(Final_Price = final_prices)
payoff_plot <- ggplot(payoff_data, aes(x = Final_Price)) +
  geom_histogram(aes(y = ..density..), bins = 50, alpha = 0.7, fill = "lightblue") +
  geom_vline(xintercept = K, color = "red", linetype = "dashed", size = 1) +
  geom_density(color = "darkblue", size = 1) +
  labs(title = "Distribution of Final Stock Prices", x = "Final Stock Price", y = "Density") +
  theme_minimal()
ggsave("results/plots/payoff_distribution.png", payoff_plot, width = 10, height = 6)

# Convergence analysis
conv_plot <- convergence_analysis(call_payoffs, r, T, call_bs)
ggsave("results/plots/convergence_analysis.png", conv_plot, width = 10, height = 6)

# Save simulation data
simulation_data <- data.frame(
  Simulation = 1:n_simulations,
  Final_Price = final_prices,
  Call_Payoff = call_payoffs,
  Put_Payoff = put_payoffs,
  In_The_Money_Call = final_prices > K,
  In_The_Money_Put = final_prices < K
)
write.csv(simulation_data, "data/processed/simulation_data.csv", row.names = FALSE)

# Save results summary
results_summary <- paste(
  "MONTE CARLO OPTION PRICING RESULTS",
  "==================================",
  paste("Simulation date:", Sys.Date()),
  paste("Number of simulations:", n_simulations),
  paste("Number of time steps:", n_steps),
  paste("Initial stock price:", S0),
  paste("Strike price:", K),
  paste("Risk-free rate:", r),
  paste("Volatility:", sigma),
  paste("Time to expiration:", T, "years"),
  "",
  "CALL OPTION:",
  paste("Monte Carlo price:", round(call_price, 4)),
  paste("Black-Scholes price:", round(call_bs, 4)),
  paste("Absolute error:", round(abs(call_price - call_bs), 4)),
  paste("Relative error:", round(abs(call_price - call_bs)/call_bs * 100, 2), "%"),
  paste("Probability in-the-money:", round(mean(final_prices > K), 4)),
  "",
  "PUT OPTION:",
  paste("Monte Carlo price:", round(put_price, 4)),
  paste("Black-Scholes price:", round(put_bs, 4)),
  paste("Absolute error:", round(abs(put_price - put_bs), 4)),
  paste("Relative error:", round(abs(put_price - put_bs)/put_bs * 100, 2), "%"),
  paste("Probability in-the-money:", round(mean(final_prices < K), 4)),
  "",
  "STATISTICS:",
  paste("Mean final price:", round(mean(final_prices), 4)),
  paste("Std dev final price:", round(sd(final_prices), 4)),
  paste("Minimum final price:", round(min(final_prices), 4)),
  paste("Maximum final price:", round(max(final_prices), 4)),
  sep = "\n"
)

writeLines(results_summary, "results/reports/simulation_results.txt")

cat("Simulation completed successfully!\n")
cat("Results saved in 'results/' directory\n")