# Proyecto Estadística I (2026-II, Grupo B1)

Matrícula de educación superior en Colombia (MEN, datos.gov.co: `5wck-szir`).

## Cómo ejecutar
Abrir R en la raíz del repositorio y ejecutar los scripts en orden:

    source("R/00_setup.R")      # paquetes, rutas, funciones
    source("R/01_limpieza.R")   # descarga, limpieza, genera data/processed/*.rds
    # luego 02 ... 08 (cada uno es independiente después de la limpieza)

## Responsables
| Script | Integrante |
|---|---|
| 00_setup, 01_limpieza, 02_descriptiva_tiempo, 07_lgn | 1 |
| 03_descriptiva_grupos, 04_probabilidad, informe final | 2 |
| 05_descriptiva_numerica, 06_bayes, 08_tcl | 3 |

## Reglas
- Trabajar en ramas y abrir pull requests; revisión cruzada.
- No subir datos (están en .gitignore).
- Probabilidades y frecuencias ponderadas por `matriculados`.
