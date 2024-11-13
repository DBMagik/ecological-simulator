# Parameters for the Predator-Prey Model
params <- list(
  prey_birth_rate = 1.0,
  predation_rate = 0.1,
  predator_efficiency = 0.075,
  predator_death_rate = 1.5,
  initial_prey = 40,
  initial_predator = 9
)

time_span <- seq(0, 50, by = 0.1)


predator_prey_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dPrey_dt <- a * Prey - b * Prey * Predator
    dPredator_dt <- e * b * Prey * Predator - m * Predator
    list(c(dPrey_dt, dPredator_dt))
  })
}

simulate_predator_prey <- function(params, time_span) {
  library(deSolve)
  state <- c(Prey = params$initial_prey, Predator = params$initial_predator)
  parameters <- list(a = params$prey_birth_rate,
                     b = params$predation_rate,
                     e = params$predator_efficiency,
                     m = params$predator_death_rate)
  out <- ode(y = state, times = time_span, func = predator_prey_model, parms = parameters)
  as.data.frame(out)
}
