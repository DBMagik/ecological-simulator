# Parameters for the Disease Spread Model
params <- list(
  infection_rate = 0.3,
  recovery_rate = 0.1,
  initial_susceptible = 990,
  initial_infected = 10,
  initial_recovered = 0
)

time_span <- seq(0, 160, by = 1)

library(deSolve)


disease_spread_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    N <- S + I + R
    dS_dt <- -parameters$beta * S * I / N
    dI_dt <- parameters$beta * S * I / N - parameters$gamma * I
    dR_dt <- parameters$gamma * I
    list(c(dS_dt, dI_dt, dR_dt))
  })
}


simulate_disease_spread <- function(params, time_span) {
  library(deSolve)
  state <- c(S = params$initial_susceptible,
             I = params$initial_infected,
             R = params$initial_recovered)
  parameters <- list(beta = params$infection_rate,
                     gamma = params$recovery_rate)
  out <- ode(y = state, times = time_span, func = disease_spread_model, parms = parameters)
  as.data.frame(out)
}
