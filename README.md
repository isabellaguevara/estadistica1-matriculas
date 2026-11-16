# Matrícula en educación superior en Colombia: análisis estadístico

Proyecto de **Estadística I** · Grupo B1 · 2026-II

Análisis estadístico de los datos de matrícula en educación superior publicados por el Ministerio de Educación Nacional (MEN): estadística descriptiva, probabilidad, teorema de Bayes y verificación por simulación de la Ley de los Grandes Números (LGN) y del Teorema del Límite Central (TCL).

## Equipo

| Rol | Integrante | Enfoque |
| --- | --- | --- |
| Integrante 1 | Isabella | Datos y tiempo |
| Integrante 2 | Juan Jose Jairo | Grupos y probabilidad |
| Integrante 3 | Andres Felipe Prada | Simulación y Bayes |

## Datos

- **Fuente:** conjunto `MEN_MATRICULA_ESTADISTICA_ES`, Portal de Datos Abiertos (datos.gov.co), identificador `5wck-szir`. Corresponde al SNIES, que las instituciones de educación superior (IES) reportan al MEN.
- **Fecha de descarga:** [completar]
- **Tamaño:** 236.920 registros y 26 variables (2015-2020).

Los datos **no están en el repositorio** (son grandes y GitHub rechaza archivos de más de 100 MB). Cada integrante debe descargarlos por su cuenta (ver [Cómo ejecutar](#cómo-ejecutar)).

### Decisiones sobre los datos

1. **Unidad de análisis.** Cada fila no es una persona: es un grupo definido por *programa × municipio de oferta × género × año × semestre*, y `Total Matriculados` es el número de personas del grupo. Las frecuencias, probabilidades y Bayes se calculan **ponderando por `Total Matriculados`**. La LGN y el TCL usan como población las filas (matriculados por grupo).
2. **La unidad de tiempo es el año.** Cada IES reporta un solo semestre por año: el SENA el semestre 2 y las demás instituciones el semestre 1. Comparar semestres entre sí produce un zigzag sin sentido; la matrícula anual es la suma de ambos semestres.
3. **Se analiza 2017-2020.** 2015 y 2016 contienen registros repetidos que inflan los conteos: el total de 2015 es exactamente el doble del oficial del SNIES y el de 2016 lo supera en 950.377 matriculados. Los totales de 2017-2020 coinciden exactamente con las cifras oficiales. El diagnóstico está en `R/01b_diagnostico_2015_2016.R`.
4. **Corte para probabilidades y Bayes:** año 2020 completo (semestre 1 + semestre 2).
5. **Las columnas `Id_*` son códigos.** Sus etiquetas se guardan en `catalogos/` y se toman del diccionario oficial del MEN/SNIES. No se asume el significado de un código sin verificarlo.

Los parámetros `ANO_MIN` (2017), `ANO_CORTE` (2020) y los totales oficiales de comparación están definidos una sola vez en `R/00_setup.R`.

## Estructura del repositorio

```
.
├── estadistica1.Rproj
├── R/                    # scripts (ver tabla más abajo)
├── catalogos/            # etiquetas de los códigos Id_* (codigo, etiqueta, verificado)
├── data/
│   ├── raw/              # matricula_es.csv (se descarga; NO se sube a Git)
│   └── processed/        # .rds generados por 01_limpieza (NO se suben a Git)
├── output/
│   ├── tablas/           # CSV con los resultados
│   └── figuras/          # PNG de los gráficos
└── README.md
```

## Requisitos

- R y RStudio.
- Paquetes: `tidyverse`, `scales`, `stringi`, `here`. `00_setup.R` los instala si faltan.
- `dplyr` ≥ 1.1.0 (los scripts usan `pick()`). Si falla: `update.packages("dplyr")`.

## Cómo ejecutar

1. Clonar el repositorio y **abrir siempre `estadistica1.Rproj`** desde RStudio (el paquete `here` usa el `.Rproj` para ubicar la raíz del proyecto).
2. Descargar el CSV desde la página del conjunto de datos en datos.gov.co y guardarlo como `data/raw/matricula_es.csv`. Si el archivo no existe, `01_limpieza.R` intenta descargarlo automáticamente.
3. Ejecutar en este orden:

   ```r
   source("R/00_setup.R")      # paquetes, rutas, semilla y parámetros
   source("R/01_limpieza.R")   # limpieza y generación de data/processed/
   ```

   Después, cada integrante corre sus propios scripts (tabla siguiente). `01b_diagnostico_2015_2016.R` es solo documentación y no es necesario para continuar.
4. Los datos limpios (`data/processed/`) tampoco se suben a Git, así que **cada integrante debe correr `00` y `01` en su computador**.

La semilla es fija (`set.seed(2026)` en `00_setup.R`) para que las simulaciones sean reproducibles.

### Catálogos de códigos

`01_limpieza.R` genera una plantilla `catalogos/<variable>.csv` para cada `Id_*` que no tenga catálogo. Se completan **una sola vez**, con el diccionario del MEN/SNIES, para que los tres decodifiquen igual:

- Columnas: `codigo`, `etiqueta`, `verificado`.
- Marcar `verificado = TRUE` solo cuando la etiqueta se confirmó con la fuente oficial.
- Mientras falte una etiqueta, el código aparece como «Código N» en los resultados.

## Scripts

| Script | Responsable | Contenido |
| --- | --- | --- |
| `00_setup.R` | Integrante 1 | Paquetes, rutas, semilla, parámetros y funciones compartidas |
| `01_limpieza.R` | Integrante 1 | Lectura, normalización, faltantes, validación contra cifras oficiales, catálogos y corte |
| `01b_diagnostico_2015_2016.R` | Integrante 1 | Diagnóstico de los registros repetidos de 2015-2016 |
| `02_descriptiva_tiempo.R` | Integrante 1 | Evolución de la matrícula (pregunta 1) |
| `03_descriptiva_grupos.R` | Integrante 2 | Sector, modalidad y género (preguntas 2 y 3) |
| `04_probabilidad.R` | Integrante 2 | Probabilidad de dos eventos |
| `05_descriptiva_numerica.R` | Integrante 3 | Tamaño de los grupos y geografía (preguntas 4 y 5) |
| `06_bayes.R` | Integrante 3 | Teorema de Bayes |
| `07_lgn.R` | Integrante 1 | Ley de los Grandes Números |
| `08_tcl.R` | Integrante 3 | Teorema del Límite Central |

### Funciones compartidas (`00_setup.R`)

- `cargar_datos()`: base de análisis 2017-2020 (la usan los scripts 02 a 08).
- `cargar_corte()`: corte del año 2020.
- `cargar_datos_completo()`: 2015-2020, solo para diagnóstico.
- `tabla_freq()`: frecuencias absolutas, relativas y acumuladas ponderadas por matriculados.
- `guardar_tabla()` y `guardar_fig()`: escriben en `output/tablas/` y `output/figuras/`.

## Preguntas de investigación

1. ¿Cómo ha cambiado la matrícula total anual entre 2017 y 2020 y qué variaciones notables aparecen?
2. ¿Cómo se reparte la matrícula entre sector oficial y privado, y entre modalidades de oferta?
3. ¿Existe una brecha de género en la matrícula y cambia según el área de conocimiento o el nivel de formación?
4. ¿Qué tan concentrada está la matrícula por departamento de oferta?
5. ¿Cómo se distribuye el número de matriculados por grupo y con qué frecuencia aparecen valores atípicos?

## Flujo de trabajo en Git

- **Nadie escribe directamente en `main`.**
- Cada integrante trabaja en su propia rama (`int1/...`, `int2/...`, `int3/...`) y abre un *pull request*.
- Revisión cruzada: el Integrante 1 revisa el trabajo del 3, el 2 revisa el del 1 y el 3 revisa el del 2. Cada revisor busca errores de ponderación, gráficos mal etiquetados e interpretaciones flojas.
- Cada integrante edita solo sus propios scripts, para evitar conflictos.

## Limitaciones

- Datos agregados: no hay información individual ni trayectorias.
- El único atributo de persona es el género (no hay edad, estrato ni ingresos).
- Dependen de lo que cada IES reporta; puede haber cambios metodológicos o de cobertura entre años.
- Programas e instituciones aparecen y desaparecen durante el periodo.
- Solo educación superior formal reportada al SNIES.
- 2015 y 2016 se excluyen por registros repetidos (ver arriba).
- En 2017-2020 hay 35 registros (0,03 %) que repiten la combinación programa × municipio × género × año × semestre sin ser idénticos; se conservan porque los totales anuales coinciden con el SNIES.

## Referencias

- Ministerio de Educación Nacional. *MEN_MATRICULA_ESTADISTICA_ES*. Portal de Datos Abiertos, datos.gov.co (`5wck-szir`).
- Ministerio de Educación Nacional (2022). Evolución de la matrícula en IES, SNIES-MEN, corte de mayo de 2022. <https://www.mineducacion.gov.co/1780/articles-415125_recurso_1.pdf>
