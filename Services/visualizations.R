plot_population_growth <- function(simulation_results) {
  library(ggplot2)
  ggplot(simulation_results, aes(x = time, y = N)) +
    geom_line(color = "darkgreen", size = 1) +
    theme_minimal() +
    labs(title = "Population Growth Over Time",
         x = "Time",
         y = "Population Size")
}
