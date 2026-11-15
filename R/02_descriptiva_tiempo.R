# 02_descriptiva_tiempo.R | Integrante 1 | Pregunta 1: evolución de la matrícula
source(here::here("R", "00_setup.R"))
datos <- cargar_datos()   # 2017-2020

datos <- datos |>
  mutate(grupo_ies = if_else(str_detect(ies, "SERVICIO NACIONAL"),
                             "SENA", "Demás IES"))

# Verificar que el patrón solo atrapa al SENA (debe imprimir una sola institución)
message("Instituciones clasificadas como SENA:")
print(datos |> filter(grupo_ies == "SENA") |> distinct(ies))

# Verificaciones de la nota metodológica
guardar_tabla(
  datos |> distinct(ano, semestre, grupo_ies) |> arrange(ano, grupo_ies),
  "verificacion_semestre_por_grupo"
)
ambos <- datos |> distinct(ano, semestre, cod_ies) |> count(ano, cod_ies) |> filter(n > 1)
if (nrow(ambos) > 0) {
  warning(nrow(ambos), " IES reportan en ambos semestres de un mismo año.")
}

serie_anual <- datos |>
  group_by(ano, grupo_ies) |>
  summarise(matriculados = sum(matriculados), .groups = "drop")
guardar_tabla(serie_anual, "matricula_anual_por_grupo")

tabla_anual <- serie_anual |>
  pivot_wider(names_from = grupo_ies, values_from = matriculados) |>
  arrange(ano) |>
  mutate(total = `Demás IES` + SENA,
         var_total_pct = 100 * (total / lag(total) - 1),
         var_demas_pct = 100 * (`Demás IES` / lag(`Demás IES`) - 1),
         var_sena_pct  = 100 * (SENA / lag(SENA) - 1),
         part_sena_pct = 100 * SENA / total)
guardar_tabla(tabla_anual, "matricula_anual")
print(as.data.frame(tabla_anual))

p <- ggplot(serie_anual, aes(ano, matriculados, fill = grupo_ies)) +
  geom_col() +
  scale_x_continuous(breaks = unique(serie_anual$ano)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(title = "Matrícula anual por grupo de instituciones, 2017-2020",
       subtitle = "El SENA reporta el semestre 2; las demás IES, el semestre 1",
       x = "Año", y = "Total de matriculados (personas)", fill = "Grupo",
       caption = "Fuente: MEN (SNIES)") +
  theme_minimal()
guardar_fig("evolucion_matricula", p, 10, 5)