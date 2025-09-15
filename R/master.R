# Install and load required packages
if (!require(plotly)) install.packages("plotly")
if (!require(dplyr)) install.packages("dplyr")
library(plotly)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Monte Carlo Simulation Parameters
n_points <- 10000
radius <- 1

# Generate 3D random points
x <- runif(n_points, -1, 1)
y <- runif(n_points, -1, 1)
z <- runif(n_points, -1, 1)

# Calculate distances and identify points inside sphere
distance <- sqrt(x^2 + y^2 + z^2)
is_inside <- distance < radius

# Create comprehensive data frame
simulation_data <- data.frame(
  x = x,
  y = y,
  z = z,
  distance = distance,
  inside_sphere = is_inside,
  point_type = ifelse(is_inside, "Inside Sphere", "Outside Sphere")
)

# Calculate volume estimation
volume_cube <- 8
fraction_inside <- mean(is_inside)
volume_sphere_estimate <- volume_cube * fraction_inside
volume_sphere_true <- (4/3) * pi * radius^3
error <- abs(volume_sphere_estimate - volume_sphere_true)

# Sample points for better visualization performance (optional)
sample_data <- simulation_data %>% 
  sample_n(2000)  # Reduce points for smoother interaction

# Create enhanced interactive 3D plot
plot_3d <- plot_ly() %>%
  
  # Add points inside sphere
  add_trace(
    data = sample_data %>% filter(inside_sphere == TRUE),
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 3,
      color = 'rgba(255, 0, 0, 0.6)',  # Red with transparency
      symbol = 'circle'
    ),
    name = 'Inside Sphere',
    text = ~paste("Distance:", round(distance, 3),
                  "<br>Position: (", round(x, 2), ",", round(y, 2), ",", round(z, 2), ")"),
    hoverinfo = 'text'
  ) %>%
  
  # Add points outside sphere
  add_trace(
    data = sample_data %>% filter(inside_sphere == FALSE),
    x = ~x, y = ~y, z = ~z,
    type = 'scatter3d',
    mode = 'markers',
    marker = list(
      size = 2,
      color = 'rgba(0, 0, 255, 0.4)',  # Blue with transparency
      symbol = 'circle'
    ),
    name = 'Outside Sphere',
    text = ~paste("Distance:", round(distance, 3),
                  "<br>Position: (", round(x, 2), ",", round(y, 2), ",", round(z, 2), ")"),
    hoverinfo = 'text'
  ) %>%
  
  # Layout configuration
  layout(
    title = list(
      text = paste("<b>3D Monte Carlo Sphere Volume Estimation</b>",
                   "<br><span style='font-size:12px'>Estimated Volume:", round(volume_sphere_estimate, 4),
                   "| True Volume:", round(volume_sphere_true, 4),
                   "| Error:", round(error, 4), "</span>"),
      x = 0.1
    ),
    scene = list(
      xaxis = list(title = "X Coordinate", range = c(-1.2, 1.2)),
      yaxis = list(title = "Y Coordinate", range = c(-1.2, 1.2)),
      zaxis = list(title = "Z Coordinate", range = c(-1.2, 1.2)),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.5)
      ),
      aspectmode = "cube"
    ),
    legend = list(
      x = 0.1,
      y = 0.9,
      bgcolor = "rgba(255,255,255,0.8)"
    ),
    margin = list(l = 0, r = 0, b = 0, t = 80)
  )

# Display the plot
plot_3d

# Save the interactive plot as HTML file
htmlwidgets::saveWidget(
  widget = plot_3d,
  file = "3d_monte_carlo_simulation.html",
  title = "3D Monte Carlo Sphere Volume Estimation",
  selfcontained = TRUE
)

# Save the data as CSV for future use
write.csv(simulation_data, "monte_carlo_simulation_data.csv", row.names = FALSE)

# Save a static PNG image (requires orca - install with: plotly::orca_install())
tryCatch({
  save_image(plot_3d, "3d_simulation_static.png", width = 1200, height = 800)
}, error = function(e) {
  message("To save static images, install orca: plotly::orca_install()")
})

# Print summary statistics
cat("=== MONTE CARLO SIMULATION RESULTS ===\n")
cat("Total points generated:", n_points, "\n")
cat("Points inside sphere:", sum(is_inside), "\n")
cat("Points outside sphere:", sum(!is_inside), "\n")
cat("Fraction inside sphere:", fraction_inside, "\n")
cat("Estimated sphere volume:", volume_sphere_estimate, "\n")
cat("True sphere volume:", volume_sphere_true, "\n")
cat("Absolute error:", error, "\n")
cat("Relative error:", round(error/volume_sphere_true * 100, 2), "%\n")

# Save results summary to text file
results_summary <- paste(
  "MONTE CARLO SIMULATION RESULTS",
  "==============================",
  paste("Total points generated:", n_points),
  paste("Points inside sphere:", sum(is_inside)),
  paste("Points outside sphere:", sum(!is_inside)),
  paste("Fraction inside sphere:", fraction_inside),
  paste("Estimated sphere volume:", volume_sphere_estimate),
  paste("True sphere volume:", volume_sphere_true),
  paste("Absolute error:", error),
  paste("Relative error:", round(error/volume_sphere_true * 100, 2), "%"),
  paste("Simulation date:", Sys.Date()),
  sep = "\n"
)

writeLines(results_summary, "simulation_results_summary.txt")