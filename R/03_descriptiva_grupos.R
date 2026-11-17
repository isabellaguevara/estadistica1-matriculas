
# ============================================================
# INTEGRANTE 2
# Preguntas 2 y 3: sector, metodología y género
# ============================================================

source(here::here("R", "00_setup.R"))

# Cargar datos del año de corte (2020)
corte <- cargar_corte()

# ============================================================
# 1. VERIFICACIÓN INICIAL
# ============================================================

variables <- c(
  "sector",
  "metodologia",
  "genero",
  "area",
  "nivel_formacion",
  "matriculados"
)

faltantes <- setdiff(variables, names(corte))

if (length(faltantes) > 0) {
  stop(
    paste(
      "Faltan las siguientes variables en la base:",
      paste(faltantes, collapse = ", ")
    )
  )
}

if (!is.numeric(corte$matriculados)) {
  stop("La variable matriculados debe ser numérica.")
}

cat("\nTotal de registros:", nrow(corte), "\n")
cat("Total de matriculados:", sum(corte$matriculados, na.rm = TRUE), "\n")


# ============================================================
# 2. PREGUNTA 2: DISTRIBUCIÓN POR SECTOR Y METODOLOGÍA
# ============================================================

# 2.1 Frecuencias por sector y metodología

for (v in c("sector", "metodologia")) {
  
  tabla <- corte |>
    filter(!is.na(.data[[v]])) |>
    group_by(categoria = .data[[v]]) |>
    summarise(
      frecuencia_absoluta = sum(matriculados, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      frecuencia_relativa = frecuencia_absoluta /
        sum(frecuencia_absoluta),
      porcentaje = frecuencia_relativa * 100
    ) |>
    arrange(desc(frecuencia_absoluta))
  
  guardar_tabla(tabla, paste0("frecuencia_", v))
}


# 2.2 Distribución conjunta: sector y metodología

tabla_sector_metodologia <- corte |>
  filter(
    !is.na(sector),
    !is.na(metodologia)
  ) |>
  group_by(sector, metodologia) |>
  summarise(
    matriculados = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(sector) |>
  mutate(
    porcentaje_dentro_sector =
      matriculados / sum(matriculados) * 100
  ) |>
  ungroup()

guardar_tabla(
  tabla_sector_metodologia,
  "sector_metodologia"
)


# 2.3 Gráfico de matrícula por sector

resumen_sector <- corte |>
  filter(!is.na(sector)) |>
  group_by(sector) |>
  summarise(
    matriculados = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  )

grafico_sector <- ggplot(
  resumen_sector,
  aes(x = sector, y = matriculados)
) +
  geom_col() +
  labs(
    title = "Distribución de la matrícula por sector",
    x = "Sector",
    y = "Cantidad de matriculados"
  ) +
  theme_minimal()

guardar_fig("matricula_sector", grafico_sector)


# 2.4 Gráfico de matrícula por metodología

resumen_metodologia <- corte |>
  filter(
    !is.na(metodologia),
    metodologia != "Código 4"
  ) |>
  group_by(metodologia) |>
  summarise(
    matriculados = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  )

grafico_metodologia <- ggplot(
  resumen_metodologia,
  aes(x = metodologia, y = matriculados)
) +
  geom_col() +
  labs(
    title = "Distribución de la matrícula por metodología",
    x = "Metodología",
    y = "Cantidad de matriculados"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

guardar_fig("matricula_metodologia", grafico_metodologia)


# ============================================================
# 3. PREGUNTA 3: DISTRIBUCIÓN POR GÉNERO
# ============================================================

# 3.1 Frecuencia general por género

tabla_genero <- corte |>
  filter(!is.na(genero)) |>
  group_by(genero) |>
  summarise(
    frecuencia_absoluta = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    frecuencia_relativa = frecuencia_absoluta /
      sum(frecuencia_absoluta),
    porcentaje = frecuencia_relativa * 100
  ) |>
  arrange(desc(frecuencia_absoluta))

guardar_tabla(tabla_genero, "frecuencia_genero")


# 3.2 Función para analizar género por otra variable

analizar_genero <- function(datos, variable) {
  
  resultado <- datos |>
    filter(
      !is.na(.data[[variable]]),
      !is.na(genero)
    ) |>
    group_by(
      categoria = .data[[variable]],
      genero
    ) |>
    summarise(
      matriculados = sum(matriculados, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(categoria) |>
    mutate(
      total_categoria = sum(matriculados),
      porcentaje = matriculados / total_categoria * 100
    ) |>
    ungroup()
  
  resultado
}


# 3.3 Brecha de género según área de conocimiento

# Se excluyen los registros sin clasificación de área.
genero_area <- analizar_genero(
  corte |> filter(area != "Sin dato"),
  "area"
)

guardar_tabla(genero_area, "genero_por_area")


# 3.4 Brecha de género según nivel de formación

genero_nivel_formacion <- analizar_genero(
  corte,
  "nivel_formacion"
)

guardar_tabla(
  genero_nivel_formacion,
  "genero_por_nivel_formacion"
)


# 3.5 Tamaño de cada nivel de formación

tabla_tamano_nivel <- corte |>
  filter(!is.na(nivel_formacion)) |>
  group_by(nivel_formacion) |>
  summarise(
    mujeres = sum(
      matriculados[genero == "Femenino"],
      na.rm = TRUE
    ),
    hombres = sum(
      matriculados[genero == "Masculino"],
      na.rm = TRUE
    ),
    total = sum(matriculados, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_total = total / sum(total) * 100
  ) |>
  arrange(desc(total))

guardar_tabla(
  tabla_tamano_nivel,
  "tamano_nivel_formacion"
)


# ============================================================
# 4. GRÁFICOS COMPARATIVOS DE GÉNERO
# ============================================================

# 4.1 Distribución de género por área

grafico_genero_area <- ggplot(
  genero_area,
  aes(x = categoria, y = porcentaje, fill = genero)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribución de matrícula por género y área",
    x = "Área de conocimiento",
    y = "Porcentaje de matriculados",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

guardar_fig("genero_por_area", grafico_genero_area)


# 4.2 Distribución de género por nivel de formación

grafico_genero_nivel <- ggplot(
  genero_nivel_formacion,
  aes(x = categoria, y = porcentaje, fill = genero)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Distribución de matrícula por género y nivel",
    x = "Nivel de formación",
    y = "Porcentaje de matriculados",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

guardar_fig("genero_por_nivel_formacion", grafico_genero_nivel)


# ============================================================
# 5. CÁLCULO DE LA BRECHA ENTRE MUJERES Y HOMBRES
# ============================================================

# Brecha = porcentaje femenino - porcentaje masculino.
# Se expresa en puntos porcentuales (pp).

calcular_brecha <- function(tabla) {
  
  femenino <- tabla |>
    filter(genero == "Femenino") |>
    select(
      categoria,
      porcentaje_femenino = porcentaje
    )
  
  masculino <- tabla |>
    filter(genero == "Masculino") |>
    select(
      categoria,
      porcentaje_masculino = porcentaje
    )
  
  brecha <- full_join(
    femenino,
    masculino,
    by = "categoria"
  ) |>
    mutate(
      brecha_pp = porcentaje_femenino -
        porcentaje_masculino
    )
  
  brecha
}


# 5.1 Brecha por área

brecha_area <- calcular_brecha(genero_area)

guardar_tabla(
  brecha_area,
  "brecha_genero_area"
)


# 5.2 Brecha por nivel de formación

brecha_nivel_formacion <- calcular_brecha(
  genero_nivel_formacion
)

guardar_tabla(
  brecha_nivel_formacion,
  "brecha_genero_nivel_formacion"
)


# ============================================================
# 6. FINALIZACIÓN
# ============================================================

message("Análisis descriptivo por grupos finalizado.")

