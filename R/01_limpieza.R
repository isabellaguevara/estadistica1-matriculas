# 01_limpieza.R | Responsable: Integrante 1
# Descarga/lectura, normalización, faltantes, validación, catálogos y corte.
source(here::here("R", "00_setup.R"))

TEXTOS_FALTANTES <- c(
  "", "NA", "N/A", "NULL", "SIN INFORMACION", "SIN DATO",
  "SIN DATOS", "NO APLICA", "NO REPORTADO", "NO DISPONIBLE"
)

ruta_raw <- here::here("data", "raw", "matricula_es.csv")
if (!file.exists(ruta_raw)) {
  url <- "https://www.datos.gov.co/api/views/5wck-szir/rows.csv?accessType=DOWNLOAD"
  dir.create(dirname(ruta_raw), recursive = TRUE, showWarnings = FALSE)
  options(timeout = 1800)
  estado <- tryCatch(
    utils::download.file(url, ruta_raw, mode = "wb", quiet = FALSE),
    error = function(e) stop("Falló la descarga: ", conditionMessage(e))
  )
  if (!identical(as.integer(estado), 0L) || !file.exists(ruta_raw)) {
    stop("No se descargó el archivo. Descárguelo manualmente desde datos.gov.co y guárdelo como data/raw/matricula_es.csv")
  }
}

crudo <- readr::read_csv(
  ruta_raw,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE, progress = FALSE
)
names(crudo) <- limpiar_nombres(names(crudo))
cat("Columnas detectadas:\n"); print(names(crudo))

# Se renombran las variables necesarias. Las columnas no listadas se conservan.
mapa <- c(
  cod_ies = "^codigo_de_la_institucion$|^codigo_institucion$",
  ies_padre = "^ies_padre$",
  ies = "^institucion_de_educacion_superior",
  principal_seccional = "^principal",
  id_sector = "^id_sector$",
  id_caracter = "^id_caracter$",
  depto_ies = "^departamento_de_domicilio",
  mpio_ies = "^municipio_de.*domicilio",
  snies = "^codigo_snies",
  programa = "^programa_academico",
  id_nivel = "^id_nivel$",
  id_nivel_formacion = "^id_nivel_formacion$",
  id_metodologia = "^id_metodologia$",
  id_area = "^id_area$",
  id_nucleo = "^id_nucleo$",
  nbc = "^nucleo_basico",
  depto_prog = "^departamento_de_oferta",
  mpio_prog = "^municipio_de_oferta",
  id_genero = "^id_genero$",
  ano = "^ano$",
  semestre = "^semestre$",
  matriculados = "^total_matriculados$"
)

nms <- names(crudo)
for (nuevo in names(mapa)) {
  hits <- nms[stringr::str_detect(nms, mapa[[nuevo]])]
  if (length(hits) > 0 && !nuevo %in% names(crudo)) {
    names(crudo)[match(hits[1], names(crudo))] <- nuevo
    nms <- names(crudo)
  }
}

obligatorias <- c("id_sector", "id_nivel", "id_metodologia",
                  "id_area", "id_genero", "ano", "semestre", "matriculados")
faltan <- setdiff(obligatorias, names(crudo))
if (length(faltan)) {
  stop("No se detectaron columnas obligatorias: ",
       paste(faltan, collapse = ", "),
       ". Revise los nombres impresos arriba y ajuste 'mapa'.")
}

datos <- crudo
n_filas_crudo <- nrow(datos)
n_vars_crudo <- ncol(datos)
rm(crudo)

es_placeholder <- function(x) {
  !is.na(x) & stringr::str_to_upper(stringr::str_squish(x)) %in% TEXTOS_FALTANTES
}
n_ph <- purrr::map_int(datos, ~ sum(es_placeholder(.x)))
datos <- datos |>
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    ~ dplyr::if_else(es_placeholder(.x), NA_character_, stringr::str_squish(.x))
  ))

# Unificar mayúsculas en variables de texto: hay variantes como "SECCIONAL" y
# "Seccional", o nombres de IES escritos de dos formas para un mismo código.
cols_texto <- intersect(
  c("ies", "principal_seccional", "depto_ies", "mpio_ies",
    "programa", "nbc", "depto_prog", "mpio_prog"),
  names(datos)
)
distintos_antes <- unname(purrr::map_int(datos[cols_texto], dplyr::n_distinct))
datos <- datos |>
  dplyr::mutate(dplyr::across(dplyr::all_of(cols_texto), stringr::str_to_upper))
distintos_despues <- unname(purrr::map_int(datos[cols_texto], dplyr::n_distinct))
guardar_tabla(
  tibble::tibble(variable = cols_texto,
                 distintos_antes = distintos_antes,
                 distintos_despues = distintos_despues),
  "normalizacion_texto"
)

matri_txt <- datos$matriculados
datos <- datos |>
  dplyr::mutate(
    matriculados = suppressWarnings(as.numeric(matriculados)),
    ano = suppressWarnings(as.integer(ano)),
    semestre = suppressWarnings(as.integer(semestre)),
    dplyr::across(dplyr::starts_with("id_"), ~ suppressWarnings(as.integer(.x)))
  )
n_no_num <- sum(!is.na(matri_txt) & is.na(datos$matriculados))
if (n_no_num > 0) warning(n_no_num, " valores de matriculados no numéricos quedaron como NA.")

faltantes <- tibble::tibble(
  variable = names(datos),
  n_na = purrr::map_int(datos, ~ sum(is.na(.x))),
  n_texto_sin_dato = unname(n_ph[names(datos)])
) |>
  dplyr::mutate(pct_na = n_na / nrow(datos)) |>
  dplyr::arrange(dplyr::desc(n_na))
guardar_tabla(faltantes, "faltantes")

clave <- intersect(c("snies", "depto_prog", "mpio_prog", "id_genero", "ano", "semestre"), names(datos))
n_duplicados <- if (length(clave)) sum(duplicated(datos[clave])) else NA_integer_
n_neg <- sum(datos$matriculados < 0, na.rm = TRUE)
n_cero <- sum(datos$matriculados == 0, na.rm = TRUE)

log_limpieza <- tibble::tibble(
  paso = "Datos leídos", filas = nrow(datos),
  matriculados = sum(datos$matriculados, na.rm = TRUE)
)
registrar <- function(paso, df) {
  log_limpieza <<- dplyr::bind_rows(
    log_limpieza,
    tibble::tibble(paso = paso, filas = nrow(df),
                   matriculados = sum(df$matriculados, na.rm = TRUE))
  )
}

# Criterio documentado: se excluyen filas sin conteo, sin periodo o con conteo negativo.
datos <- datos |> dplyr::filter(!is.na(matriculados)); registrar("Sin NA en matriculados", datos)
datos <- datos |> dplyr::filter(!is.na(ano), !is.na(semestre)); registrar("Con año y semestre", datos)
datos <- datos |> dplyr::filter(matriculados >= 0); registrar("Sin negativos", datos)
guardar_tabla(log_limpieza, "log_limpieza")

# Catálogos: deben tener columnas codigo, etiqueta, verificado.
# No se inventan etiquetas. Si no existe el catálogo se genera una plantilla.
vars_cat <- c(
  sector = "id_sector", caracter = "id_caracter", nivel = "id_nivel",
  nivel_formacion = "id_nivel_formacion", metodologia = "id_metodologia",
  area = "id_area", nucleo = "id_nucleo", genero = "id_genero"
)
for (etq in names(vars_cat)) {
  v <- unname(vars_cat[[etq]])
  if (!v %in% names(datos)) next
  ruta <- here::here("catalogos", paste0(v, ".csv"))
  if (!file.exists(ruta)) {
    plantilla <- datos |>
      dplyr::filter(!is.na(.data[[v]])) |>
      dplyr::distinct(codigo = .data[[v]]) |>
      dplyr::mutate(etiqueta = NA_character_, verificado = FALSE)
    readr::write_csv(plantilla, ruta)
    message("Plantilla creada: ", ruta, ". Complete las etiquetas con fuente oficial.")
  }
  cat_v <- readr::read_csv(
    ruta, show_col_types = FALSE,
    col_types = readr::cols(
      codigo = readr::col_integer(),
      etiqueta = readr::col_character(),
      verificado = readr::col_logical(),
      .default = readr::col_skip()
    )
  )
  if (anyDuplicated(cat_v$codigo)) stop("Hay códigos duplicados en ", ruta)
  sin_cat <- setdiff(unique(stats::na.omit(datos[[v]])), cat_v$codigo)
  if (length(sin_cat)) warning("Códigos sin catálogo en ", v, ": ", paste(sin_cat, collapse = ", "))
  datos <- datos |>
    dplyr::left_join(dplyr::transmute(cat_v, !!v := codigo, !!etq := etiqueta), by = v) |>
    dplyr::mutate(!!etq := dplyr::case_when(
      is.na(.data[[v]]) ~ "Sin dato",
      is.na(.data[[etq]]) ~ paste("Código", .data[[v]]),
      TRUE ~ .data[[etq]]
    ))
}

estructura <- tibble::tibble(
  n_filas_crudo = n_filas_crudo, n_vars_crudo = n_vars_crudo,
  n_filas_limpio = nrow(datos),
  ano_min = min(datos$ano, na.rm = TRUE),
  ano_max = max(datos$ano, na.rm = TRUE),
  n_periodos = dplyr::n_distinct(datos$ano, datos$semestre),
  n_ies = if ("cod_ies" %in% names(datos)) dplyr::n_distinct(datos$cod_ies) else NA_integer_,
  n_programas = if ("snies" %in% names(datos)) dplyr::n_distinct(datos$snies) else NA_integer_,
  n_duplicados_clave = n_duplicados, n_matri_cero = n_cero,
  n_matri_negativos_original = n_neg
)
guardar_tabla(estructura, "estructura")

diccionario <- tibble::tibble(
  variable = names(datos),
  clase = purrr::map_chr(datos, ~ class(.x)[1]),
  n_distintos = purrr::map_int(datos, dplyr::n_distinct),
  n_na = purrr::map_int(datos, ~ sum(is.na(.x))),
  ejemplos = purrr::map_chr(datos, ~ paste(head(unique(stats::na.omit(.x)), 3), collapse = " | "))
)
guardar_tabla(diccionario, "diccionario_variables")

totales <- datos |>
  dplyr::group_by(ano, semestre) |>
  dplyr::summarise(total = sum(matriculados), filas = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(ano, semestre) |>
  dplyr::mutate(razon_prev = total / dplyr::lag(total))
guardar_tabla(totales, "totales_periodo")

# ---- Corte y base de análisis (ANO_MIN y ANO_CORTE vienen de 00_setup.R) ----
corte <- dplyr::filter(datos, ano == ANO_CORTE)
analisis <- dplyr::filter(datos, ano >= ANO_MIN)
message("Corte: año ", ANO_CORTE, " | Matrícula (S1 + S2): ",
        scales::comma(sum(corte$matriculados)))

# Validación contra cifras oficiales (justifica la exclusión de 2015-2016)
validacion <- datos |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total_base = sum(matriculados), .groups = "drop") |>
  dplyr::left_join(oficial_snies, by = "ano") |>
  dplyr::mutate(diferencia = total_base - oficial_snies)
guardar_tabla(validacion, "validacion_oficial")
print(as.data.frame(validacion))
if (any(validacion$diferencia[validacion$ano >= ANO_MIN] != 0)) {
  warning("Los totales de la base de análisis no coinciden con el SNIES.")
}

# Estructura y faltantes de la base de análisis (la que usa el informe)
estructura_analisis <- tibble::tibble(
  n_filas = nrow(analisis),
  matriculados = sum(analisis$matriculados),
  n_ies = dplyr::n_distinct(analisis$cod_ies),
  n_programas = dplyr::n_distinct(analisis$snies),
  n_duplicados_clave = sum(duplicated(analisis[clave]))
)
guardar_tabla(estructura_analisis, "estructura_analisis")

faltantes_analisis <- tibble::tibble(
  variable = names(analisis),
  n_na = purrr::map_int(analisis, ~ sum(is.na(.x)))
) |>
  dplyr::mutate(pct_na = n_na / nrow(analisis)) |>
  dplyr::arrange(dplyr::desc(n_na))
guardar_tabla(faltantes_analisis, "faltantes_analisis")

# 2015-2016 se excluyen (registros repetidos; ver validacion_oficial y
# diagnostico_2015_2016 generado por 01b_diagnostico_2015_2016.R)
saveRDS(datos,    here::here("data", "processed", "matricula_limpia.rds"))
saveRDS(analisis, here::here("data", "processed", "matricula_analisis.rds"))
saveRDS(corte,    here::here("data", "processed", "matricula_corte.rds"))
message("Limpieza finalizada.")