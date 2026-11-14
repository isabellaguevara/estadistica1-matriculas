# Integrante 1 | Pregunta 1: evolución de la matrícula por periodo
source(here::here("R", "00_setup.R"))
datos <- cargar_datos()

serie <- datos |>
  group_by(ano, semestre) |>
  summarise(matriculados = sum(matriculados), .groups = "drop") |>
  arrange(ano, semestre) |>
  mutate(periodo = paste0(ano, "-S", semestre),
         variacion_pct = 100 * (matriculados / lag(matriculados) - 1))
guardar_tabla(serie, "matricula_por_periodo")

p <- ggplot(serie, aes(x = interaction(ano, semestre, lex.order = TRUE),
                       y = matriculados, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "Evolución de la matrícula por periodo",
       x = "Año-semestre", y = "Total de matriculados (personas)") +
  theme_minimal()
guardar_fig("evolucion_matricula", p, 10, 5)
