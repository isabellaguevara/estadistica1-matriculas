# Integrante 2 | Preguntas 2 y 3: sector, metodología y género
source(here::here("R", "00_setup.R"))
corte <- cargar_corte()

# Ajustar nombres de etiqueta según las columnas creadas por los catálogos.
for (v in intersect(c("sector", "metodologia", "genero", "area", "nivel"), names(corte))) {
  guardar_tabla(tabla_freq(corte, !!rlang::sym(v)), paste0("frecuencia_", v))
}

# Ejemplo de gráfico ponderado por matriculados.
if ("sector" %in% names(corte)) {
  resumen <- corte |> group_by(sector) |> summarise(n = sum(matriculados), .groups = "drop")
  p <- ggplot(resumen, aes(x = sector, y = n)) +
    geom_col() + labs(title = "Matrícula por sector", x = "Sector",
                      y = "Matriculados (personas)") + theme_minimal()
  guardar_fig("matricula_sector", p)
}
