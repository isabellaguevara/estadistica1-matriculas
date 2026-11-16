# 07_lgn.R | Integrante 1 | Ley de los Grandes Números
source(here::here("R", "00_setup.R"))
d <- cargar_datos()   # base de análisis 2017-2020
set.seed(2026)        # semilla propia: no depende de lo ejecutado antes

# La población de la simulación son las filas; variable: matriculados por grupo.
x <- d$matriculados
N <- length(x)
mu <- mean(x)
sigma <- sqrt(mean((x - mu)^2))   # desviación estándar poblacional

ns <- c(5, 10, 50, 100, 500, 1000, 5000, 10000)
B <- 1000   # repeticiones por cada n

# Resumen de la población (sirve para completar las secciones 8 y 9)
poblacion <- tibble(
  N = N, mu = mu, sigma = sigma, mediana = median(x), maximo = max(x),
  asimetria = mean((x - mu)^3) / sigma^3
)
guardar_tabla(poblacion, "lgn_poblacion")
print(as.data.frame(poblacion))

# 1. Una muestra por cada n (muestreo con reemplazo: extracciones independientes)
una_muestra <- purrr::map_dfr(ns, function(n) {
  m <- mean(sample(x, size = n, replace = TRUE))
  tibble(n = n, media_muestral = m, mu = mu,
         error_abs = abs(m - mu),
         error_rel_pct = 100 * abs(m - mu) / mu)
})
guardar_tabla(una_muestra, "lgn_una_muestra")
print(as.data.frame(una_muestra))

# 2. B muestras por cada n: error promedio (promedio de |media - mu|)
repetido <- purrr::map_dfr(ns, function(n) {
  medias <- replicate(B, mean(sample(x, size = n, replace = TRUE)))
  err <- abs(medias - mu)
  tibble(n = n, B = B,
         error_abs_medio = mean(err),
         error_rel_medio_pct = 100 * mean(err) / mu,
         error_rel_p95_pct = 100 * unname(quantile(err, 0.95)) / mu)
})
guardar_tabla(repetido, "lgn_errores")
print(as.data.frame(repetido))

p_err <- ggplot(repetido, aes(n, error_rel_medio_pct)) +
  geom_line() + geom_point() +
  scale_x_log10(breaks = ns,
                labels = label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_log10(labels = label_number(decimal.mark = ",", suffix = " %")) +
  labs(title = "Error relativo medio de la media muestral según el tamaño de muestra",
       subtitle = paste0("Promedio de ", B, " muestras por cada n; escalas logarítmicas"),
       x = "Tamaño de muestra n (filas)",
       y = "Error relativo medio respecto de µ (%)",
       caption = "Fuente: MEN (SNIES), 2017-2020") +
  theme_minimal()
guardar_fig("lgn_error_relativo", p_err, 9, 5)

# 3. Media acumulada de una muestra aleatoria frente a n
z <- sample(x, size = max(ns), replace = TRUE)
trayectoria <- tibble(n = seq_along(z), media_acumulada = cumsum(z) / seq_along(z))

p_acum <- ggplot(trayectoria, aes(n, media_acumulada)) +
  geom_line() +
  geom_hline(yintercept = mu, linetype = 2, colour = "red") +
  scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Convergencia de la media acumulada hacia la media poblacional",
       subtitle = paste0("Línea punteada: µ = ", format(round(mu, 2), decimal.mark = ","), " matriculados por grupo"),
       x = "Número de extracciones n (escala logarítmica)",
       y = "Media acumulada de matriculados por grupo",
       caption = "Fuente: MEN (SNIES), 2017-2020") +
  theme_minimal()
guardar_fig("lgn_media_acumulada", p_acum, 9, 5)