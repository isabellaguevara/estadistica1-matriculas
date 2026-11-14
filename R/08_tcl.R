# Integrante 3 | Teorema del Límite Central
source(here::here("R", "00_setup.R"))
d <- cargar_datos()
x <- d$matriculados
mu <- mean(x)
sigma <- sd(x)
ns <- c(2, 5, 30, 100, 500)
B <- 1000

# Guardar medias simuladas y un histograma por tamaño muestral.
for (n in ns) {
  medias <- replicate(B, mean(sample(x, size = n, replace = TRUE)))
  sim <- tibble(media = medias)
  p <- ggplot(sim, aes(media)) +
    geom_histogram(aes(y = after_stat(density)), bins = 35) +
    stat_function(fun = dnorm, args = list(mean = mu, sd = sigma / sqrt(n)),
                  linewidth = 1) +
    labs(title = paste("TCL: distribución de medias, n =", n),
         x = "Media muestral de matriculados por fila", y = "Densidad") +
    theme_minimal()
  guardar_fig(paste0("tcl_n_", n), p)
}
