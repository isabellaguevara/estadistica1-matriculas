# 07_lgn.R  | Responsable: Integrante 1
# Ley de los Grandes Números. Variable: matriculados por fila (todas las filas).
# Población = todas las filas de la base limpia; media poblacional = media de todas ellas.
source(here::here("R", "00_setup.R"))
datos <- cargar_datos()

pob <- datos$matriculados
mu  <- mean(pob)
cat("Media poblacional: ", mu, " (N = ", length(pob), " filas)\n", sep = "")

ns <- c(5, 10, 50, 100, 500, 1000, 5000, 10000)
ns <- ns[ns <= length(pob)]

# (a) Una muestra aleatoria por cada n
una_muestra <- tibble(n = ns) |>
  mutate(media_muestral = map_dbl(n, ~ mean(sample(pob, .x))),
         error_abs = abs(media_muestral - mu),
         error_rel = error_abs / mu)
print(una_muestra)
guardar_tabla(una_muestra, "lgn_una_muestra")

# (b) Error promedio sobre muchas repeticiones (muestra el patrón con claridad)
R_REP <- 300
repetido <- tibble(n = ns) |>
  mutate(error_abs_medio = map_dbl(n, \(k) mean(replicate(R_REP, abs(mean(sample(pob, k)) - mu)))),
         error_rel_medio = error_abs_medio / mu)
print(repetido)
guardar_tabla(repetido, "lgn_error_repetido")

# (c) Media acumulada de una sola permutación aleatoria
m <- min(10000, length(pob))
acum <- tibble(n = 1:m, media = cummean(sample(pob, m)))
ggplot(acum, aes(n, media)) +
  geom_line() +
  geom_hline(yintercept = mu, linetype = "dashed", colour = "red") +
  scale_x_log10(labels = label_comma()) +
  labs(title = "Ley de los Grandes Números: media muestral frente a n",
       subtitle = "Línea roja: media poblacional",
       x = "Tamaño de muestra n (escala logarítmica)",
       y = "Media muestral (matriculados por fila)") +
  theme_minimal()
guardar_fig("lgn_media_acumulada")

# (d) Error relativo medio en función de n
ggplot(repetido, aes(n, error_rel_medio)) +
  geom_line() + geom_point() +
  scale_x_log10(labels = label_comma()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "El error de la media muestral disminuye al aumentar n",
       subtitle = paste0("Promedio de ", R_REP, " muestras por cada n"),
       x = "Tamaño de muestra n (escala logarítmica)",
       y = "Error relativo medio") +
  theme_minimal()
guardar_fig("lgn_error_relativo")
