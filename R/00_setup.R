# 00_setup.R  | Responsable: Integrante 1
# Paquetes, rutas, semilla y funciones compartidas por todos los scripts.

paquetes <- c("tidyverse", "scales", "here")
nuevos <- setdiff(paquetes, rownames(installed.packages()))
if (length(nuevos)) install.packages(nuevos)
invisible(lapply(paquetes, library, character.only = TRUE))

options(scipen = 999, dplyr.summarise.inform = FALSE)
set.seed(2026)   # reproducibilidad de LGN y TCL

# here() toma como raíz la carpeta del repositorio (donde está .git o .here)
for (d in c("data/raw", "data/processed", "output/figuras", "output/tablas", "catalogos"))
  dir.create(here(d), recursive = TRUE, showWarnings = FALSE)

# Normaliza nombres de columna: sin tildes, minúsculas, con guion bajo
limpiar_nombres <- function(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_remove_all("^_+|_+$")
}

guardar_fig <- function(nombre, p = last_plot(), w = 8, h = 5) {
  ggsave(here("output", "figuras", paste0(nombre, ".png")), p,
         width = w, height = h, dpi = 300)
}

guardar_tabla <- function(df, nombre) {
  write_csv(df, here("output", "tablas", paste0(nombre, ".csv")))
  invisible(df)
}

# Tabla de frecuencias ponderada por matriculados:
# absoluta, relativa y acumuladas (ordenada de mayor a menor)
tabla_freq <- function(df, var, peso = matriculados) {
  df |>
    count({{ var }}, wt = {{ peso }}, name = "f_abs") |>
    arrange(desc(f_abs)) |>
    mutate(f_rel = f_abs / sum(f_abs),
           F_abs = cumsum(f_abs),
           F_rel = cumsum(f_rel))
}

cargar_datos <- function() readRDS(here("data", "processed", "matricula_limpia.rds"))
cargar_corte <- function() readRDS(here("data", "processed", "matricula_corte.rds"))
