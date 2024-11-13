# Load model functions
source("Models/population_growth_model.R")
source("Models/predator_prey_model.R")
source("Models/disease_spread_model.R")

# Example simulation for the Population Growth Model
params_pg <- list(
  growth_rate = 0.1,
  carrying_capacity = 1000,
  initial_population = 50
)
time_span_pg <- seq(0, 50, by = 0.1)
pg_results <- simulate_population_growth(params_pg, time_span_pg)

# Plotting the results
plot_population_growth <- function(simulation_results) {
  library(ggplot2)
  ggplot(simulation_results, aes(x = time, y = N)) +
    geom_line(color = "blue") +
    labs(title = "Population Growth Over Time", x = "Time", y = "Population Size") +
    theme_minimal()
}

plot_population_growth(pg_results)
