library(ggplot2)
library(deSolve)

# Parameters for the Population Growth Model
params <- list(
  growth_rate = 0.1,
  carrying_capacity = 1000,
  initial_population = 50
)

# Time span for the simulation
time_span <- seq(0, 50, by = 0.1)

# Logistic Growth Function
logistic_growth <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN_dt <- r * N * (1 - N / K)
    list(c(dN_dt))
  })
}

# Simulation function without external pressures
simulate_population_growth <- function(params, time_span) {
  # Extract parameters
  r <- params$growth_rate
  K <- params$carrying_capacity
  N0 <- params$initial_population

  # Initial state
  state <- c(N = N0)
  parameters <- c(r = r, K = K)

  # Run the simulation
  out <- ode(y = state, times = time_span, func = logistic_growth, parms = parameters)
  results <- as.data.frame(out)
  return(results)
}

# Run the simulation
results <- simulate_population_growth(params, time_span)

# Plotting function
plot_population_growth <- function(simulation_results) {
  ggplot(simulation_results, aes(x = time, y = N)) +
    geom_line(color = "blue", linewidth = 1) +
    labs(title = "Population Growth Over Time", x = "Time", y = "Population linewidth") +
    theme_minimal()
}

# Plot the results
plot_population_growth(results)

