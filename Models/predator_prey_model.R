# Load necessary libraries
library(deSolve)
library(ggplot2)
library(reshape2)

# Parameters for the Predator Prey Model ued in testing
params <- list(
  prey_birth_rate = 1.0,
  predation_rate = 0.1,
  predator_efficiency = 0.10,
  predator_death_rate = 0.4,
  initial_prey = 40,
  initial_predator = 9
)

time_span <- seq(0, 50, by = 0.1)

# Predator-Prey Model Function
predator_prey_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dPrey_dt <- a * Prey - b * Prey * Predator
    dPredator_dt <- e * b * Prey * Predator - m * Predator
    list(c(dPrey_dt, dPredator_dt))
  })
}

# Simulation Function
simulate_predator_prey <- function(params, time_span) {
  state <- c(Prey = params$initial_prey, Predator = params$initial_predator)
  parameters <- list(a = params$prey_birth_rate,
                     b = params$predation_rate,
                     e = params$predator_efficiency,
                     m = params$predator_death_rate)
  out <- ode(y = state, times = time_span, func = predator_prey_model, parms = parameters)
  as.data.frame(out)
}

# Plotting Function
plot_predator_prey <- function(simulation_results) {
  # Melt the data for plotting
  data_melted <- melt(simulation_results, id.vars = "time", variable.name = "Population", value.name = "linewidth")
  ggplot(data_melted, aes(x = time, y = linewidth, color = Population)) +
    geom_line(linewidth = 1) +
    labs(title = "Predator-Prey Dynamics Over Time", x = "Time", y = "Population Size") +
    theme_minimal()
}

# Run the simulation
results <- simulate_predator_prey(params, time_span)

# Plot the results
plot_predator_prey(results)
