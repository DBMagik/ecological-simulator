# Parameters for the Disease Spread Model
params <- list(
  infection_rate = 0.3,
  recovery_rate = 0.1,
  death_rate = 0.05,
  initial_susceptible = 990,
  initial_infected = 10,
  initial_recovered = 0
)

# Time span for the simulation
time_span <- seq(0, 160, by = 1)

# Load necessary libraries
library(deSolve)
library(reshape2)
library(ggplot2)

# Disease Spread Model Function
disease_spread_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS_dt <- -beta * S * I / N
    dI_dt <- beta * S * I / N - gamma * I - delta * I  # delta represents the death rate
    dR_dt <- gamma * I
    list(c(dS_dt, dI_dt, dR_dt))
  })
}


# Simulation Function
simulate_disease_spread <- function(params, time_span) {
  state <- c(S = params$initial_susceptible,
             I = params$initial_infected,
             R = params$initial_recovered)
  parameters <- list(beta = params$infection_rate,
                     gamma = params$recovery_rate,
                     delta = params$death_rate)  # New parameter
  out <- ode(y = state, times = time_span, func = disease_spread_model, parms = parameters)
  as.data.frame(out)
}


# Plotting Function
plot_disease_spread <- function(simulation_results) {
  # Melt the data for plotting
  data_melted <- melt(simulation_results, id.vars = "time", variable.name = "Compartment", value.name = "Count")
  ggplot(data_melted, aes(x = time, y = Count, color = Compartment)) +
    geom_line(linewidth = 1) +
    labs(title = "Disease Spread Over Time", x = "Time", y = "Number of Individuals") +
    theme_minimal()
}

# Run the simulation
results <- simulate_disease_spread(params, time_span)

# Plot the results
plot_disease_spread(results)
