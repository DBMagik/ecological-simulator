# Parameters for the Population Growth Model
params <- list(
  growth_rate = 0.1,
  carrying_capacity = 1000,
  initial_population = 50
)

# Time span for the simulation
time_span <- seq(0, 50, by = 0.1)  # Simulate from time 0 to 50 in increments of 0.1

library(deSolve)  # It's better to load libraries at the top

# Exponential Growth Function
exponential_growth <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN_dt <- r * N
    list(c(dN_dt))
  })
}

# Logistic Growth Function
logistic_growth <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN_dt <- r * N * (1 - N / K)
    list(c(dN_dt))
  })
}

# Simulation Function
simulate_population_growth <- function(params, time_span, model = "exponential") {
  state <- c(N = params$initial_population)
  parameters <- list(r = params$growth_rate)

  if (model == "logistic") {
    parameters$K <- params$carrying_capacity
    model_func <- logistic_growth
  } else {
    model_func <- exponential_growth
  }

  out <- ode(y = state, times = time_span, func = model_func, parms = parameters)
  as.data.frame(out)
}

# Run the simulation
results <- simulate_population_growth(params, time_span, model = "logistic")

# Plotting Function
plot_population_growth <- function(simulation_results) {
  library(ggplot2)
  ggplot(simulation_results, aes(x = time, y = N)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Population Growth Over Time", x = "Time", y = "Population Size") +
    theme_minimal()
}

# Plot the results
plot_population_growth(results)
