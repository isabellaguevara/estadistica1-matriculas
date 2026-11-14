# Estadística I — MEN Matrícula

## Requisitos
- R y RStudio.
- Abrir `estadistica1.Rproj` (crear el archivo desde RStudio si aún no existe).
- Internet para descargar el CSV y para instalar paquetes.

## Orden de ejecución
1. `R/00_setup.R`
2. `R/01_limpieza.R`
3. Ejecutar los scripts asignados: `02` a `08`.

Los datos crudos y procesados están excluidos de Git mediante `.gitignore`.
Cada integrante debe descargar/obtener `data/raw/matricula_es.csv` localmente si la descarga automática falla.

## Importante
- Las filas son grupos agregados, no personas individuales.
- Para probabilidades y frecuencias de estudiantes, ponderar por `matriculados`.
- Para LGN y TCL, la unidad de análisis indicada en la guía son las filas y la variable es `matriculados` por grupo.
- No sumar semestres como si fueran personas distintas.
- Completar y verificar los catálogos `catalogos/id_*.csv` con el diccionario oficial MEN/SNIES antes de interpretar códigos.
- Revisar etiquetas de `sector`, `metodologia`, `area` y `genero` antes de ejecutar 04 y 06.

## Carpetas
- `R/`: scripts.
- `data/raw/`: CSV original (no versionado).
- `data/processed/`: RDS limpios (no versionados).
- `catalogos/`: diccionarios compartidos.
- `output/`: tablas y figuras generadas.
