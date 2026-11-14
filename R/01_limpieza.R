# 01_limpieza.R  | Responsable: Integrante 1
# Descarga, estructura, datos faltantes, limpieza y decodificación.
# Salidas: data/processed/*.rds y tablas en output/tablas/ (para el informe).
source(here::here("R", "00_setup.R"))

# ---- Parámetros -------------------------------------------------------------
# NULL = elegir automáticamente el último periodo "completo". O fijarlo a mano:
# PERIODO_CORTE <- c(ano = 2023, semestre = 2)
PERIODO_CORTE <- NULL
TEXTOS_FALTANTES <- c("", "NA", "N/A", "NULL", "SIN INFORMACION", "SIN INFORMACIÓN",
                      "SIN DATO", "SIN DATOS", "NO APLICA", "NO REPORTADO", "NO DISPONIBLE")

# ---- 1. Descarga (una sola vez) ---------------------------------------------
ruta_raw <- here("data", "raw", "matricula_es.csv")
if (!file.exists(ruta_raw)) {
  url <- "https://www.datos.gov.co/api/views/5wck-szir/rows.csv?accessType=DOWNLOAD"
  options(timeout = 1800)
  ok <- tryCatch({ download.file(url, ruta_raw, mode = "wb"); TRUE },
                 error = function(e) { message("Falló la descarga: ", conditionMessage(e)); FALSE })
  if (!ok) stop("Descargue el CSV a mano desde datos.gov.co (Exportar > CSV) y guárdelo en: ", ruta_raw)
}

# Se lee todo como texto para detectar valores tipo "Sin información" antes de convertir
crudo <- read_csv(ruta_raw, col_types = cols(.default = col_character()),
                  show_col_types = FALSE, progress = FALSE)
names(crudo) <- limpiar_nombres(names(crudo))
cat("Columnas del archivo:\n"); print(names(crudo))

# ---- 2. Nombres cortos (busca por patrón; tolera pequeñas diferencias) ------
mapa <- c(
  cod_ies             = "^codigo_de_la_institucion|^codigo_institucion",
  ies_padre           = "^ies_padre",
  ies                 = "^institucion_de_educacion_superior",
  principal_seccional = "^principal",
  id_sector           = "^id_sector$",
  id_caracter         = "^id_caracter$",
  depto_ies           = "^departamento_de_?domicilio",
  mpio_ies            = "^municipio_de_?domicilio",
  snies               = "snies",
  programa            = "^programa_academico",
  id_nivel            = "^id_nivel$",
  id_nivel_formacion  = "^id_nivel_formacion$",
  id_metodologia      = "metodolog",
  id_area             = "^id_area$",
  id_nucleo           = "^id_nucleo$",
  nbc                 = "nucleo_basico|^nbc",
  depto_prog          = "^departamento_(de_?oferta|del_?programa)",
  mpio_prog           = "^municipio_(de_?oferta|del_?programa)",
  id_genero           = "^id_genero$",
  ano                 = "^ano$",
  semestre            = "^semestre$",
  matriculados        = "total_matriculados|^matriculados"
)
nms <- names(crudo); usados <- character()
for (nuevo in names(mapa)) {
  hit <- setdiff(nms[str_detect(nms, mapa[[nuevo]])], usados)
  if (!length(hit)) { warning("No se encontró columna para '", nuevo, "'. Revise 'mapa'."); next }
  names(crudo)[names(crudo) == hit[1]] <- nuevo
  usados <- c(usados, hit[1])
}
obligatorias <- c("id_sector", "id_nivel", "id_metodologia", "id_area", "id_genero",
                  "ano", "semestre", "matriculados")
faltan <- setdiff(obligatorias, names(crudo))
if (length(faltan)) stop("Faltan columnas obligatorias: ", paste(faltan, collapse = ", "))
datos <- crudo
n_filas_crudo <- nrow(datos); n_vars_crudo <- ncol(datos)
rm(crudo)

# ---- 3. Textos de "sin dato" -> NA (se cuentan para el informe) --------------
es_placeholder <- function(x) !is.na(x) & str_to_upper(str_squish(x)) %in% TEXTOS_FALTANTES
n_ph <- map_int(datos, ~ sum(es_placeholder(.x)))
datos <- datos |> mutate(across(everything(), ~ if_else(es_placeholder(.x), NA_character_, str_squish(.x))))

# ---- 4. Tipos de variable ----------------------------------------------------
matri_txt <- datos$matriculados
datos <- datos |>
  mutate(matriculados = suppressWarnings(as.numeric(matriculados)),
         ano          = suppressWarnings(as.integer(ano)),
         semestre     = suppressWarnings(as.integer(semestre)),
         across(starts_with("id_"), ~ suppressWarnings(as.integer(.x))))
n_no_num <- sum(!is.na(matri_txt) & is.na(datos$matriculados))
if (n_no_num > 0) warning(n_no_num, " valores de 'Total Matriculados' no son numéricos (quedaron NA).")

# ---- 5. Datos faltantes (documentar en el informe) ---------------------------
faltantes <- tibble(
  variable = names(datos),
  n_na = map_int(datos, ~ sum(is.na(.x))),
  n_texto_sin_dato = unname(n_ph[names(datos)])
) |>
  mutate(pct_na = n_na / nrow(datos)) |>
  arrange(desc(n_na))
print(faltantes, n = Inf)
guardar_tabla(faltantes, "faltantes")

clave <- intersect(c("snies", "depto_prog", "mpio_prog", "id_genero", "ano", "semestre"), names(datos))
n_duplicados <- sum(duplicated(datos[clave]))
n_neg  <- sum(datos$matriculados < 0, na.rm = TRUE)
n_cero <- sum(datos$matriculados == 0, na.rm = TRUE)
cat("\nDuplicados por clave (", paste(clave, collapse = ", "), "): ", n_duplicados,
    "\nMatriculados negativos: ", n_neg, " | iguales a cero: ", n_cero, "\n", sep = "")

# ---- 6. Limpieza (cada paso queda registrado) --------------------------------
log_limpieza <- tibble(paso = "Datos crudos", filas = nrow(datos),
                       matriculados = sum(datos$matriculados, na.rm = TRUE))
registrar <- function(paso, df)
  log_limpieza <<- bind_rows(log_limpieza,
                             tibble(paso = paso, filas = nrow(df), matriculados = sum(df$matriculados)))

# Criterio: se eliminan filas sin conteo, sin periodo o con conteo negativo.
# Las filas con NA en variables categóricas se conservan (aparecen como "Sin dato").
datos <- datos |> filter(!is.na(matriculados)); registrar("Sin NA en Total Matriculados", datos)
datos <- datos |> filter(!is.na(ano), !is.na(semestre)); registrar("Sin NA en año/semestre", datos)
datos <- datos |> filter(matriculados >= 0); registrar("Sin matriculados negativos", datos)
print(log_limpieza); guardar_tabla(log_limpieza, "log_limpieza")

# ---- 7. Decodificar códigos (catálogos en catalogos/*.csv) ------------------
# Para cada variable Id_* hay un CSV (codigo, etiqueta, verificado). Si no existe, se genera
# una plantilla con los códigos presentes y un ejemplo para identificarlos; complétela con
# el diccionario del MEN/SNIES y suba el CSV al repositorio.
vars_cat <- c(sector = "id_sector", caracter = "id_caracter", nivel = "id_nivel",
              nivel_formacion = "id_nivel_formacion", metodologia = "id_metodologia",
              area = "id_area", genero = "id_genero")
col_ejemplo <- c(id_area = "nbc", id_nivel = "programa", id_nivel_formacion = "programa",
                 id_metodologia = "programa", id_caracter = "ies", id_sector = "ies")

for (etq in names(vars_cat)) {
  v <- vars_cat[[etq]]
  if (!v %in% names(datos)) next
  ruta <- here("catalogos", paste0(v, ".csv"))
  if (!file.exists(ruta)) {
    ej <- unname(col_ejemplo[v])
    plantilla <- datos |>
      filter(!is.na(.data[[v]])) |>
      group_by(codigo = .data[[v]]) |>
      summarise(n_filas = n(),
                ejemplo = if (!is.na(ej) && ej %in% names(datos)) first(.data[[ej]]) else NA_character_) |>
      mutate(etiqueta = NA_character_, verificado = FALSE) |>
      select(codigo, etiqueta, verificado, n_filas, ejemplo)
    write_csv(plantilla, ruta)
    message("Plantilla creada: ", ruta, "  -> complete la columna 'etiqueta'.")
  }
  cat_v <- read_csv(ruta, show_col_types = FALSE,
                    col_types = cols(codigo = col_integer(), etiqueta = col_character(),
                                     verificado = col_logical(), .default = col_character()))
  if (anyDuplicated(cat_v$codigo)) stop("Códigos repetidos en ", ruta)
  sin_cat <- setdiff(unique(na.omit(datos[[v]])), cat_v$codigo)
  if (length(sin_cat)) warning("Códigos de ", v, " sin etiqueta en el catálogo: ", paste(sin_cat, collapse = ", "))
  if (any(!cat_v$verificado, na.rm = TRUE) || any(is.na(cat_v$verificado)))
    message("Aviso: el catálogo ", v, " tiene etiquetas sin verificar con el diccionario oficial.")
  datos <- datos |>
    left_join(transmute(cat_v, !!v := codigo, !!etq := etiqueta), by = v) |>
    mutate(!!etq := case_when(is.na(.data[[v]]) ~ "Sin dato",
                              is.na(.data[[etq]]) ~ paste("Código", .data[[v]]),
                              TRUE ~ .data[[etq]]))
}

# ---- 8. Estructura y diccionario para la sección de descripción -------------
estructura <- tibble(
  n_filas_crudo = n_filas_crudo, n_vars_crudo = n_vars_crudo, n_filas_limpio = nrow(datos),
  ano_min = min(datos$ano), ano_max = max(datos$ano),
  n_periodos = nrow(distinct(datos, ano, semestre)),
  n_ies = if ("cod_ies" %in% names(datos)) n_distinct(datos$cod_ies) else NA_integer_,
  n_programas = if ("snies" %in% names(datos)) n_distinct(datos$snies) else NA_integer_,
  n_duplicados_clave = n_duplicados, n_matri_cero = n_cero
)
print(t(estructura)); guardar_tabla(estructura, "estructura")

diccionario <- tibble(
  variable = names(datos),
  clase = map_chr(datos, ~ class(.x)[1]),
  n_distintos = map_int(datos, n_distinct),
  n_na = map_int(datos, ~ sum(is.na(.x))),
  ejemplos = map_chr(datos, ~ paste(head(unique(na.omit(.x)), 3), collapse = " | "))
)
guardar_tabla(diccionario, "diccionario_variables")

# ---- 9. Corte de un solo periodo (la matrícula es un stock: no sumar semestres)
totales <- datos |>
  group_by(ano, semestre) |>
  summarise(total = sum(matriculados), filas = n()) |>
  ungroup() |>
  arrange(ano, semestre) |>
  mutate(razon_prev = total / lag(total))
guardar_tabla(totales, "totales_periodo")

if (is.null(PERIODO_CORTE)) {
  ult <- slice_tail(totales, n = 1)
  # Si el último periodo cae >10% frente al anterior, probablemente está incompleto
  if (!is.na(ult$razon_prev) && ult$razon_prev < 0.9) ult <- slice_tail(totales, n = 2) |> slice(1)
} else {
  ult <- filter(totales, ano == PERIODO_CORTE[["ano"]], semestre == PERIODO_CORTE[["semestre"]])
}
cat("\nPeriodo de corte: ", ult$ano, "-", ult$semestre, " (", comma(ult$total), " matriculados)\n", sep = "")
corte <- semi_join(datos, ult, by = c("ano", "semestre"))

saveRDS(datos, here("data", "processed", "matricula_limpia.rds"))
saveRDS(corte, here("data", "processed", "matricula_corte.rds"))
message("Listo: data/processed/matricula_limpia.rds y matricula_corte.rds")
