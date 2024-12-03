# main.R

# Load necessary libraries
library(deSolve)
library(ggplot2)
library(reshape2)

# Source individual models
source("Models/population_growth_model.R")
source("Models/disease_spread_model.R")
source("Models/predator_prey_model.R")

# Define time span
time_span <- seq(0, 100, by = 0.1)

## --- Running Models Separately ---

### Population Growth Model
params_population <- list(
  growth_rate = 0.1,
  carrying_capacity = 1000,
  initial_population = 50
)
population_results <- simulate_population_growth(params_population, time_span)
plot_population_growth(population_results)

### Disease Spread Model
params_disease <- list(
  infection_rate = 0.3,
  recovery_rate = 0.1,
  initial_susceptible = 990,
  initial_infected = 10,
  initial_recovered = 0
)
disease_results <- simulate_disease_spread(params_disease, time_span)
plot_disease_spread(disease_results)

### Predator-Prey Model
params_predator_prey <- list(
  prey_birth_rate = 1.0,
  predation_rate = 0.1,
  predator_efficiency = 0.075,
  predator_death_rate = 1.5,
  initial_prey = 40,
  initial_predator = 9
)
predator_prey_results <- simulate_predator_prey(params_predator_prey, time_span)
plot_predator_prey(predator_prey_results)

## --- Combined Model ---

# Parameters for the Combined Model
params_combined <- list(
  # Population Growth Parameters
  r_prey = 0.5,
  K_prey = 1000,

  # Predator Parameters
  e = 0.075,
  m = 1.5,

  # Predation Parameters
  a = 0.01,

  # Disease Parameters (Prey)
  beta_prey = 0.3,
  gamma_prey = 0.1,

  # Initial Populations
  initial_S_prey = 990,
  initial_I_prey = 10,
  initial_R_prey = 0,
  initial_Predator = 20
)

# Combined Model Function
combined_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Total prey population
    N_prey <- S_prey + I_prey + R_prey

    # Prey Population Dynamics
    dS_prey_dt <- r_prey * S_prey * (1 - N_prey / K_prey) -
                  beta_prey * S_prey * I_prey / N_prey -
                  a * S_prey * Predator
    dI_prey_dt <- beta_prey * S_prey * I_prey / N_prey -
                  gamma_prey * I_prey -
                  a * I_prey * Predator
    dR_prey_dt <- gamma_prey * I_prey -
                  a * R_prey * Predator

    # Predator Population Dynamics
    dPredator_dt <- e * a * N_prey * Predator - m * Predator

    return(list(c(dS_prey_dt, dI_prey_dt, dR_prey_dt, dPredator_dt)))
  })
}

# Initial State
state_combined <- c(
  S_prey = params_combined$initial_S_prey,
  I_prey = params_combined$initial_I_prey,
  R_prey = params_combined$initial_R_prey,
  Predator = params_combined$initial_Predator
)

# Run Combined Simulation
combined_results <- ode(
  y = state_combined,
  times = time_span,
  func = combined_model,
  parms = params_combined
)

combined_results_df <- as.data.frame(combined_results)

# Plotting Combined Results

# Prey Dynamics
prey_data <- combined_results_df[, c("time", "S_prey", "I_prey", "R_prey")]
prey_melted <- melt(prey_data, id.vars = "time", variable.name = "Compartment", value.name = "Population")
plot_prey_combined <- ggplot(prey_melted, aes(x = time, y = Population, color = Compartment)) +
  geom_line(linewidth = 1) +
  labs(title = "Prey Dynamics in Combined Model", x = "Time", y = "Population linewidth") +
  theme_minimal()

# Predator Dynamics
plot_predator_combined <- ggplot(combined_results_df, aes(x = time, y = Predator)) +
  geom_line(color = "red", linewidth = 1) +
  labs(title = "Predator Dynamics in Combined Model", x = "Time", y = "Population linewidth") +
  theme_minimal()

# Combined Predator and Prey Populations
combined_results_df$Total_Prey <- combined_results_df$S_prey + combined_results_df$I_prey + combined_results_df$R_prey
plot_combined <- ggplot(combined_results_df, aes(x = time)) +
  geom_line(aes(y = Total_Prey, color = "Total Prey"), linewidth = 1) +
  geom_line(aes(y = Predator, color = "Predator"), linewidth = 1) +
  labs(title = "Predator and Total Prey Populations Over Time", x = "Time", y = "Population linewidth") +
  scale_color_manual(values = c("Total Prey" = "blue", "Predator" = "red")) +
  theme_minimal()

# Display Combined Plots
print(plot_prey_combined)
print(plot_predator_combined)
print(plot_combined)
