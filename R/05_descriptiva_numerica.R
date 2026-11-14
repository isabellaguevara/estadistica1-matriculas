# Integrante 3 | Pregunta 5: distribución del tamaño de los grupos
source(here::here("R", "00_setup.R"))
d <- cargar_datos()
x <- d$matriculados

resumen <- tibble(
  n = length(x), media = mean(x), mediana = median(x),
  desviacion = sd(x), minimo = min(x), q1 = quantile(x, .25),
  q3 = quantile(x, .75), IQR = IQR(x), maximo = max(x),
  asimetria_aprox = mean((x - mean(x))^3) / sd(x)^3
)
guardar_tabla(resumen, "resumen_matriculados")

p <- ggplot(d, aes(x = matriculados)) +
  geom_histogram(bins = 50) +
  labs(title = "Distribución de matriculados por fila",
       x = "Matriculados por grupo (personas)", y = "Frecuencia de filas") +
  theme_minimal()
guardar_fig("histograma_matriculados", p)
