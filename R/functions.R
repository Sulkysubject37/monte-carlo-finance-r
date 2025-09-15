#' Monte Carlo Option Pricing Functions
#' 
#' This file contains all the core functions for the Monte Carlo simulation.

#' Generate stock price paths using Geometric Brownian Motion
#' 
#' @param n_simulations Number of simulations
#' @param n_steps Number of time steps
#' @param S0 Initial stock price
#' @param mu Expected return
#' @param sigma Volatility
#' @param dt Time step size
#' @return Matrix of price paths
generate_price_paths <- function(n_simulations, n_steps, S0, mu, sigma, dt) {
  price_paths <- matrix(NA, nrow = n_steps + 1, ncol = n_simulations)
  price_paths[1, ] <- S0
  
  for (i in 1:n_simulations) {
    for (t in 2:(n_steps + 1)) {
      Z <- rnorm(1)
      price_paths[t, i] <- price_paths[t-1, i] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * Z)
    }
  }
  
  return(price_paths)
}

#' Calculate Black-Scholes option prices
#' 
#' @param S0 Initial stock price
#' @param K Strike price
#' @param r Risk-free rate
#' @param sigma Volatility
#' @param T Time to expiration
#' @return List with call and put prices
black_scholes <- function(S0, K, r, sigma, T) {
  d1 <- (log(S0/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  call_price <- S0 * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  put_price <- K * exp(-r * T) * pnorm(-d2) - S0 * pnorm(-d1)
  
  return(list(call = call_price, put = put_price))
}

#' Create interactive price paths plot
#' 
#' @param path_data Data frame with price paths
#' @param S0 Initial stock price
#' @param K Strike price
#' @param T Time to expiration
#' @param n_simulations Number of simulations
#' @return plotly object
create_paths_plot <- function(path_data, S0, K, T, n_simulations) {
  plot_ly() %>%
    add_trace(
      data = path_data,
      x = ~Time, y = ~Price, group = ~Path,
      type = 'scatter',
      mode = 'lines',
      line = list(width = 1, color = 'rgba(0,100,200,0.3)'),
      name = 'Price Paths',
      hoverinfo = 'none'
    ) %>%
    add_trace(
      x = c(0, T), y = c(K, K),
      type = 'scatter',
      mode = 'lines',
      line = list(width = 3, color = 'red', dash = 'dash'),
      name = paste('Strike Price (', K, ')')
    ) %>%
    add_trace(
      x = c(0, T), y = c(S0, S0),
      type = 'scatter',
      mode = 'lines',
      line = list(width = 3, color = 'green', dash = 'dash'),
      name = paste('Initial Price (', S0, ')')
    ) %>%
    layout(
      title = list(
        text = paste("<b>Monte Carlo Option Pricing Simulation</b>",
                     "<br><span style='font-size:12px'>", n_simulations, "simulations | Strike:", K,
                     "| Initial:", S0, "</span>"),
        x = 0.1
      ),
      xaxis = list(title = "Time (Years)"),
      yaxis = list(title = "Stock Price"),
      legend = list(x = 0.1, y = 0.9),
      margin = list(l = 50, r = 50, b = 50, t = 80)
    )
}

#' Perform convergence analysis
#' 
#' @param call_payoffs Vector of call option payoffs
#' @param r Risk-free rate
#' @param T Time to expiration
#' @param call_bs Black-Scholes call price
#' @return ggplot object
convergence_analysis <- function(call_payoffs, r, T, call_bs) {
  n_points <- seq(100, length(call_payoffs), length.out = 20)
  call_prices <- sapply(n_points, function(n) {
    mean(call_payoffs[1:n]) * exp(-r * T)
  })
  
  conv_data <- data.frame(
    Simulations = n_points,
    Call_Price = call_prices,
    BS_Price = call_bs
  )
  
  ggplot(conv_data, aes(x = Simulations, y = Call_Price)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = call_bs, color = "red", linetype = "dashed") +
    labs(title = "Monte Carlo Convergence Analysis",
         x = "Number of Simulations", y = "Call Option Price") +
    theme_minimal()
}