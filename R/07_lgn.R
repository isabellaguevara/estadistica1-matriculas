# Integrante 1 | Ley de los Grandes Números
source(here::here("R", "00_setup.R"))
d <- cargar_datos()

# La población de la simulación son las filas; variable: matriculados por grupo.
x <- d$matriculados
mu <- mean(x)
ns <- c(5, 10, 50, 100, 500, 1000, 5000)
B <- 1000

sim <- purrr::map_dfr(ns, function(n) {
  medias <- replicate(B, mean(sample(x, size = n, replace = TRUE)))
  tibble(n = n, media_muestral_promedio = mean(medias),
         error_absoluto = abs(mean(medias) - mu))
})
guardar_tabla(sim, "lgn_errores")

# Una trayectoria acumulada de una permutación aleatoria de las filas.
z <- sample(x)
trayectoria <- tibble(n = seq_along(z), media_acumulada = cumsum(z) / seq_along(z))
p <- ggplot(trayectoria, aes(n, media_acumulada)) +
  geom_line() + geom_hline(yintercept = mu, linetype = 2) +
  labs(title = "Convergencia de la media acumulada",
       x = "Número de filas incorporadas", y = "Media de matriculados por fila") +
  theme_minimal()
guardar_fig("lgn_media_acumulada", p)
