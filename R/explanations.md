# Monte Carlo Option Pricing Explanation

## Overview

This simulation uses Monte Carlo methods to price European call and put options by simulating multiple possible future stock price paths.

## Methodology

### Geometric Brownian Motion

Stock prices are modeled using Geometric Brownian Motion (GBM):

\[ dS = \mu S dt + \sigma S dW \]

Where:
- \( S \) = stock price
- \( \mu \) = expected return
- \( \