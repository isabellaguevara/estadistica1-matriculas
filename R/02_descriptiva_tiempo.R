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

datos <- datos |>
  mutate(grupo_ies = if_else(str_detect(ies, "SERVICIO NACIONAL"), "SENA", "Demás IES"))

serie_anual <- datos |>
  group_by(ano, grupo_ies) |>
  summarise(matriculados = sum(matriculados), .groups = "drop")
guardar_tabla(serie_anual, "matricula_anual_por_grupo")

p <- ggplot(serie_anual, aes(ano, matriculados, fill = grupo_ies)) +
  geom_col() +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Matrícula por año",
       subtitle = "El SENA reporta el semestre 2; las demás IES, el semestre 1",
       x = "Año", y = "Total de matriculados (personas)", fill = "Grupo") +
  theme_minimal()
guardar_fig("evolucion_matricula", p, 10, 5)
