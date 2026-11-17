
# ============================================================
# ANÁLISIS EXTRA
# Evolución de la distribución y brecha de género por área
# Periodo: 2017-2020
# ============================================================

source(here::here("R", "00_setup.R"))

# ============================================================
# 1. CARGAR Y VERIFICAR LOS DATOS
# ============================================================

# Cargar todos los datos del periodo 2017-2020
datos <- cargar_datos()

variables <- c(
  "ano",
  "area",
  "genero",
  "matriculados"
)

faltantes <- setdiff(variables, names(datos))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan las siguientes variables:",
      paste(faltantes, collapse = ", ")
    )
  )
}

if (!is.numeric(datos$matriculados)) {
  stop("La variable matriculados debe ser numérica.")
}

# ============================================================
# 2. PREPARAR LOS DATOS
# ============================================================

# Excluir áreas sin clasificación y registros sin género.
# Se conservan únicamente las dos categorías de género analizadas.

datos_genero <- datos |>
  filter(
    !is.na(ano),
    !is.na(area),
    area != "Sin dato",
    !is.na(genero),
    genero %in% c("Femenino", "Masculino")
  )

# Comprobar las áreas disponibles
areas <- sort(unique(datos_genero$area))

cat("\nAños analizados:", sort(unique(datos_genero$ano)), "\n")
cat("Cantidad de áreas identificadas:", length(areas), "\n")
cat("\nÁreas incluidas:\n")
print(areas)

# ============================================================
# 3. CALCULAR LA PARTICIPACIÓN POR GÉNERO
# ============================================================

# Se suman los matriculados de cada combinación:
# año + área + género.

resumen_genero <- datos_genero |>
  group_by(ano, area, genero) |>
  summarise(
    matriculados = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::complete(
    ano,
    area,
    genero = c("Femenino", "Masculino"),
    fill = list(matriculados = 0)
  ) |>
  group_by(ano, area) |>
  mutate(
    total_area = sum(matriculados),
    porcentaje = ifelse(
      total_area > 0,
      matriculados / total_area * 100,
      NA_real_
    )
  ) |>
  ungroup() |>
  mutate(
    genero = factor(
      genero,
      levels = c("Femenino", "Masculino")
    )
  )

# ============================================================
# 4. GRÁFICAS INDIVIDUALES POR ÁREA
# ============================================================

for (i in seq_along(areas)) {
  
  area_actual <- areas[i]
  
  datos_area <- resumen_genero |>
    filter(area == area_actual)
  
  grafico <- ggplot(
    datos_area,
    aes(
      x = ano,
      y = porcentaje,
      color = genero,
      group = genero
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    scale_x_continuous(
      breaks = sort(unique(datos_area$ano))
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    labs(
      title = paste(
        "Evolución de la matrícula por género"
      ),
      subtitle = area_actual,
      x = "Año",
      y = "Porcentaje de matriculados",
      color = "Género"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  
  guardar_fig(
    paste0("evolucion_genero_area_", sprintf("%02d", i)),
    grafico
  )
}

# ============================================================
# 5. GRÁFICA GENERAL: TODAS LAS ÁREAS
# ============================================================

grafico_todas_areas <- ggplot(
  resumen_genero,
  aes(
    x = ano,
    y = porcentaje,
    color = genero,
    group = genero
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(
    ~ area,
    ncol = 2
  ) +
  scale_x_continuous(
    breaks = sort(unique(resumen_genero$ano))
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  labs(
    title = "Evolución de la matrícula por género y área",
    subtitle = "Comparación entre 2017 y 2020",
    x = "Año",
    y = "Porcentaje de matriculados",
    color = "Género"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom"
  )

guardar_fig(
  "evolucion_genero_todas_areas",
  grafico_todas_areas
)

# ============================================================
# 6. TABLA VISUAL: EVOLUCIÓN DE LA BRECHA POR ÁREA
# ============================================================

# Calcular porcentajes de mujeres y hombres por año y área

tabla_brechas <- resumen_genero |>
  select(ano, area, genero, porcentaje) |>
  tidyr::pivot_wider(
    names_from = genero,
    values_from = porcentaje
  ) |>
  mutate(
    brecha_pp = Femenino - Masculino,
    etiqueta = paste0(
      ifelse(brecha_pp > 0, "+", ""),
      formatC(
        brecha_pp,
        format = "f",
        digits = 2,
        decimal.mark = ","
      ),
      " pp"
    )
  )

# Crear tabla visual

grafico_brechas <- ggplot(
  tabla_brechas,
  aes(
    x = area,
    y = factor(ano),
    fill = brecha_pp
  )
) +
  geom_tile(
    color = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(label = etiqueta),
    size = 3.2,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#4C78A8",
    mid = "white",
    high = "#E76F51",
    midpoint = 0,
    name = "Brecha (pp)"
  ) +
  scale_x_discrete(
    labels = function(x) stringr::str_wrap(x, width = 20)
  ) +
  scale_y_discrete(
    limits = rev(as.character(sort(unique(tabla_brechas$ano))))
  ) +
  labs(
    title = "Evolución de la brecha de género por área",
    subtitle = "Diferencia entre la participación femenina y masculina, 2017-2020",
    x = "Área de conocimiento",
    y = "Año",
    caption = "Valores positivos favorecen a las mujeres; negativos, a los hombres."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 8
    ),
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid = element_blank()
  )

guardar_fig(
  "tabla_evolucion_brecha_genero",
  grafico_brechas
)

# Guardar también la tabla numérica
guardar_tabla(
  tabla_brechas,
  "evolucion_brecha_genero"
)


# ============================================================
# 7. FINALIZACIÓN
# ============================================================

message(
  "Análisis temporal de género por área finalizado."
)

message(
  "Se generaron ",
  length(areas),
  " gráficas individuales y una gráfica general."
)
