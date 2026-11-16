# 05_descriptiva_numerica.R | Responsable: Integrante 3
# Pregunta 5: tamaño de los grupos (matriculados por fila).
# Pregunta 4: concentración geográfica de la matrícula (departamento de oferta).
source(here::here("R", "00_setup.R"))

datos <- cargar_datos()   # 2017-2020: población de la LGN y del TCL (134.941 filas)
corte <- cargar_corte()   # año 2020: "fotografía" para la geografía

fmt_n <- scales::label_comma(big.mark = ".", decimal.mark = ",")
fuente_filas <- "Fuente: MEN (SNIES), 2017-2020; cálculos propios."
fuente_corte <- "Fuente: MEN (SNIES), 2020; cálculos propios."

# =====================================================================
# PARTE A. Pregunta 5: distribución del tamaño de los grupos
# Variable: matriculados por fila (cada fila es un grupo, no una persona).
# =====================================================================
x     <- datos$matriculados
N     <- length(x)
mu    <- mean(x)
sigma <- sqrt(mean((x - mu)^2))          # desviación POBLACIONAL (divisor N)
asimetria <- mean((x - mu)^3) / sigma^3  # coeficiente de asimetría (g1)
moda  <- as.numeric(names(which.max(table(x))))
q     <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
iqr   <- q[3] - q[1]
p_extra <- quantile(x, c(0.05, 0.10, 0.90, 0.95, 0.99), names = FALSE)

estadisticos <- tibble::tibble(
  estadistico = c("n (filas)", "Media", "Mediana", "Moda",
                  "Desviación estándar (poblacional)", "Varianza (poblacional)",
                  "Coeficiente de variación", "Mínimo", "Máximo", "Rango",
                  "Q1", "Q3", "IQR", "P5", "P10", "P90", "P95", "P99",
                  "Asimetría (g1)"),
  valor = c(N, mu, q[2], moda, sigma, sigma^2, sigma / mu, min(x), max(x),
            max(x) - min(x), q[1], q[3], iqr, p_extra, asimetria)
)
guardar_tabla(estadisticos, "numerica_estadisticos")
print(as.data.frame(estadisticos))

# Control de consistencia con la sección 8 del informe (Isabella).
coincide <- N == 134941 && round(mu, 2) == 71.43 && round(sigma, 2) == 217.49 &&
  q[2] == 17 && max(x) == 18464 && round(asimetria, 1) == 26.5
if (!coincide) {
  warning("Los estadísticos NO coinciden con la sección 8 (mu = 71,43; sigma = 217,49; ",
          "mediana = 17; máx = 18.464; asimetría = 26,5; n = 134.941). ",
          "Revise que está usando cargar_datos() y la base 2017-2020.")
}

# Valores atípicos: cercas de Tukey calculadas sobre la escala ORIGINAL.
cerca_inf <- q[1] - 1.5 * iqr
cerca_sup <- q[3] + 1.5 * iqr
atipico   <- x < cerca_inf | x > cerca_sup
atipicos <- tibble::tibble(
  cerca_inferior   = cerca_inf,
  cerca_superior   = cerca_sup,
  n_atipicos       = sum(atipico),
  pct_de_filas     = mean(atipico),
  pct_de_matriculados = sum(x[atipico]) / sum(x),
  n_extremos_3iqr  = sum(x > q[3] + 3 * iqr)
)
guardar_tabla(atipicos, "numerica_atipicos")
print(as.data.frame(atipicos))

# Histograma en escala logarítmica con media, mediana y cerca superior.
lineas <- tibble::tibble(
  nombre = c("Media", "Mediana", "Cerca superior de Tukey"),
  x = c(mu, q[2], cerca_sup)
)
p_hist <- ggplot2::ggplot(datos, ggplot2::aes(x = matriculados)) +
  ggplot2::geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  ggplot2::geom_vline(data = lineas, ggplot2::aes(xintercept = x, color = nombre),
                      linetype = "dashed", linewidth = 0.8) +
  ggplot2::scale_x_log10(labels = fmt_n) +
  ggplot2::scale_y_continuous(labels = fmt_n) +
  ggplot2::labs(
    title = "Distribución del número de matriculados por grupo, 2017-2020",
    subtitle = "Escala logarítmica en el eje horizontal",
    x = "Matriculados por grupo (personas, escala logarítmica)",
    y = "Número de grupos (filas)", color = NULL, caption = fuente_filas
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
guardar_fig("numerica_histograma_log", p_hist)

# =====================================================================
# PARTE B. Pregunta 4: concentración por departamento de oferta (corte 2020)
# OJO: departamento de OFERTA = depto_prog (no depto_ies).
# Se SUMA matriculados (personas); no se cuentan filas.
# =====================================================================
geo <- corte |>
  dplyr::filter(!is.na(depto_prog)) |>
  tabla_freq(depto_prog)
guardar_tabla(geo, "geo_departamento_corte")

k <- nrow(geo)
v <- sort(geo$f_abs)                                   # ascendente
gini <- 2 * sum(seq_len(k) * v) / (k * sum(v)) - (k + 1) / k
resumen_geo <- tibble::tibble(
  n_departamentos = k,
  matriculados_total = sum(geo$f_abs),
  top1 = geo$f_rel[1],
  top3 = sum(geo$f_rel[1:3]),
  top5 = sum(geo$f_rel[1:5]),
  hhi  = sum(geo$f_rel^2),     # 1/k = reparto perfecto; 1 = todo en un departamento
  gini = gini
)
guardar_tabla(resumen_geo, "geo_resumen_concentracion")
print(as.data.frame(resumen_geo))
print(as.data.frame(head(geo, 10)))

# Barras: 10 departamentos con más matrícula.
p_top <- geo |>
  dplyr::slice_head(n = 10) |>
  dplyr::mutate(depto_prog = forcats::fct_reorder(depto_prog, f_abs)) |>
  ggplot2::ggplot(ggplot2::aes(x = f_abs, y = depto_prog)) +
  ggplot2::geom_col(fill = "steelblue") +
  ggplot2::geom_text(
    ggplot2::aes(label = scales::percent(f_rel, accuracy = 0.1, decimal.mark = ",")),
    hjust = -0.1, size = 3
  ) +
  ggplot2::scale_x_continuous(labels = fmt_n, expand = ggplot2::expansion(mult = c(0, 0.15))) +
  ggplot2::labs(
    title = "Matrícula por departamento de oferta: 10 mayores, 2020",
    x = "Matriculados (personas)", y = "Departamento de oferta",
    caption = fuente_corte
  ) +
  ggplot2::theme_minimal()
guardar_fig("geo_top10_departamentos", p_top)

# Curva de Lorenz: cuanto más se aleja de la diagonal, más concentrada la matrícula.
lorenz <- tibble::tibble(p = (0:k) / k, L = c(0, cumsum(v) / sum(v)))
p_lorenz <- ggplot2::ggplot(lorenz, ggplot2::aes(x = p, y = L)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  ggplot2::geom_line(color = "firebrick", linewidth = 1) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(decimal.mark = ",")) +
  ggplot2::labs(
    title = "Curva de Lorenz de la matrícula por departamento, 2020",
    subtitle = paste0("Índice de Gini = ", format(round(gini, 3), decimal.mark = ",")),
    x = "Proporción acumulada de departamentos (de menor a mayor matrícula)",
    y = "Proporción acumulada de matriculados", caption = fuente_corte
  ) +
  ggplot2::theme_minimal()
guardar_fig("geo_lorenz", p_lorenz)

# Mapa coroplético. Necesita un archivo de departamentos de Colombia en
#   data/geo/departamentos.geojson  (o .shp ajustando ruta_geo)
# Fuentes: Geoportal DANE (MGN) o GADM (nivel 1). Ese archivo NO se sube al repo si es pesado.
dir.create(here::here("data", "geo"), showWarnings = FALSE, recursive = TRUE)
ruta_geo       <- here::here("data", "geo", "departamentos.geojson")
COL_NOMBRE_GEO <- "NAME_1"   # GADM: "NAME_1" | DANE-MGN: "DPTO_CNMBR" (revise names(mapa))

if (file.exists(ruta_geo) && requireNamespace("sf", quietly = TRUE)) {
  mapa <- sf::st_read(ruta_geo, quiet = TRUE)
  print(names(mapa))
  stopifnot(COL_NOMBRE_GEO %in% names(mapa))
  mapa <- dplyr::mutate(mapa, clave = stringr::str_remove_all(limpiar_nombres(.data[[COL_NOMBRE_GEO]]), "_"))
  geo_m <- dplyr::mutate(geo, clave = stringr::str_remove_all(limpiar_nombres(depto_prog), "_"))
  
  # Correcciones manuales si algún nombre no cruza (clave_matricula = clave_mapa):
  correcciones <- c(
    "archipielagodesanandresprovidenciaysantacatalina" = "sanandresyprovidencia"
  )
  geo_m <- dplyr::mutate(geo_m, clave = dplyr::coalesce(unname(correcciones[clave]), clave))
  
  cat("Departamentos de la matrícula SIN par en el mapa:\n")
  print(setdiff(geo_m$clave, mapa$clave))
  cat("Polígonos del mapa SIN datos:\n")
  print(setdiff(mapa$clave, geo_m$clave))
  
  mapa_datos <- dplyr::left_join(mapa, geo_m, by = "clave")
  p_mapa <- ggplot2::ggplot(mapa_datos) +
    ggplot2::geom_sf(ggplot2::aes(fill = f_rel), color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(
      trans = "sqrt", labels = scales::label_percent(accuracy = 1, decimal.mark = ","),
      na.value = "grey85", name = "Participación"
    ) +
    ggplot2::labs(
      title = "Participación de cada departamento en la matrícula, 2020",
      subtitle = "Departamento de oferta del programa; escala de color en raíz cuadrada",
      caption = fuente_corte
    ) +
    ggplot2::theme_void()
  guardar_fig("geo_mapa_departamentos", p_mapa, w = 7, h = 8)
} else {
  message("Mapa omitido: falta ", ruta_geo, " o el paquete 'sf' (install.packages('sf')).")
}

message("05_descriptiva_numerica.R finalizadoo.")