# 00_setup.R | Responsable: Integrante 1
# Paquetes, rutas, semilla y funciones compartidas.

# Abrir siempre estadistica1.Rproj desde RStudio.
# here() reconoce la raíz del proyecto (.Rproj, .git o .here).
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}

paquetes <- c("tidyverse", "scales", "stringi", "here")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]

if (length(faltantes) > 0) {
  install.packages(faltantes, repos = "https://cloud.r-project.org")
}

# Verificar que la instalación sí haya terminado.
no_instalados <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(no_instalados) > 0) {
  stop(
    "No se pudieron instalar estos paquetes: ",
    paste(no_instalados, collapse = ", "),
    ". Revise internet, permisos y repositorio CRAN."
  )
}

suppressPackageStartupMessages(
  invisible(lapply(paquetes, library, character.only = TRUE))
)

options(scipen = 999, dplyr.summarise.inform = FALSE)
set.seed(2026)

# Crear carpetas necesarias.
carpetas <- c(
  "data/raw", "data/processed",
  "output/figuras", "output/tablas", "catalogos"
)
for (d in carpetas) {
  dir.create(here::here(d), recursive = TRUE, showWarnings = FALSE)
}

# Normalizar nombres: sin tildes, minúsculas y guion bajo.
limpiar_nombres <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_remove_all("^_+|_+$")
}

guardar_fig <- function(nombre, p = ggplot2::last_plot(), w = 8, h = 5) {
  ggplot2::ggsave(
    filename = here::here("output", "figuras", paste0(nombre, ".png")),
    plot = p, width = w, height = h, dpi = 300
  )
}

guardar_tabla <- function(df, nombre) {
  readr::write_csv(df, here::here("output", "tablas", paste0(nombre, ".csv")))
  invisible(df)
}

# Frecuencias ponderadas: cada fila aporta matriculados estudiantes.
tabla_freq <- function(df, var, peso = matriculados) {
  df |>
    dplyr::count({{ var }}, wt = {{ peso }}, name = "f_abs") |>
    dplyr::arrange(dplyr::desc(f_abs)) |>
    dplyr::mutate(
      f_rel = f_abs / sum(f_abs),
      F_abs = cumsum(f_abs),
      F_rel = cumsum(f_rel)
    )
}

cargar_datos <- function() {
  readRDS(here::here("data", "processed", "matricula_limpia.rds"))
}
cargar_corte <- function() {
  readRDS(here::here("data", "processed", "matricula_corte.rds"))
}

message("00_setup.R ejecutado correctamente. Raíz: ", here::here())
