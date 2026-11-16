# 06_bayes.R | Responsable: Integrante 3
# Teorema de Bayes sobre el corte 2020 (N = 2.355.603 matriculados), ponderado.
#   A = "la persona matriculada está en el área de interés"
#   B = "la persona matriculada es mujer"
source(here::here("R", "00_setup.R"))

corte <- cargar_corte()
N <- sum(corte$matriculados)
stopifnot(N == 2355603)   # debe coincidir con el total oficial del SNIES 2020

# ---- PASO 1: verificar etiquetas ANTES de definir los eventos -------------
# Si aquí ve "Código 7" en vez de un nombre, falta completar catalogos/id_area.csv
# (o id_genero.csv) con el diccionario del MEN/SNIES.
cat("\nÁrea de conocimiento (corte 2020):\n")
print(as.data.frame(corte |> dplyr::group_by(id_area, area) |>
                      dplyr::summarise(matriculados = sum(matriculados), .groups = "drop") |>
                      dplyr::arrange(dplyr::desc(matriculados))))
cat("\nGénero (corte 2020):\n")
print(as.data.frame(corte |> dplyr::group_by(id_genero, genero) |>
                      dplyr::summarise(matriculados = sum(matriculados), .groups = "drop")))

# ---- PASO 2: definir los eventos (ajustar patrones según lo impreso) ------
AREA_PATRON   <- "INGENIER"   # área de interés: Ingeniería, arquitectura, urbanismo y afines
GENERO_PATRON <- "FEMEN"      # mujeres

corte <- corte |>
  dplyr::mutate(
    A = stringr::str_detect(tidyr::replace_na(area, ""),
                            stringr::regex(AREA_PATRON, ignore_case = TRUE)),
    B = stringr::str_detect(tidyr::replace_na(genero, ""),
                            stringr::regex(GENERO_PATRON, ignore_case = TRUE))
  )
stopifnot(any(corte$A), any(!corte$A), any(corte$B), any(!corte$B))

# Nota para el informe: las filas con área "Sin dato" (id_area faltante) quedan en Aᶜ.
cat("\nMatriculados con área 'Sin dato' (quedan en Aᶜ): ",
    sum(corte$matriculados[corte$area == "Sin dato"]), "\n")

# ---- PASO 3: conteos PONDERADOS (personas, no filas) ----------------------
w <- corte$matriculados
A <- corte$A
B <- corte$B

n_A    <- sum(w[A])
n_Ac   <- sum(w[!A])
n_B    <- sum(w[B])
n_AB   <- sum(w[A & B])
n_AcB  <- sum(w[!A & B])

# ---- PASO 4: probabilidades que exige la guía -----------------------------
P_A         <- n_A / N
P_Ac        <- 1 - P_A
P_B_dado_A  <- n_AB / n_A
P_B_dado_Ac <- n_AcB / n_Ac

# Probabilidad total y Bayes
P_B_total    <- P_B_dado_A * P_A + P_B_dado_Ac * P_Ac
P_A_dado_B   <- P_B_dado_A * P_A / P_B_total

# ---- PASO 5: verificación contra el cálculo directo -----------------------
P_B_directo       <- n_B / N
P_A_dado_B_directo <- n_AB / n_B
stopifnot(isTRUE(all.equal(P_B_total, P_B_directo)),
          isTRUE(all.equal(P_A_dado_B, P_A_dado_B_directo)))

resultados <- tibble::tibble(
  concepto = c("P(A)", "P(Aᶜ)", "P(B|A)", "P(B|Aᶜ)",
               "P(B) por probabilidad total", "P(B) directo",
               "P(A|B) por Bayes", "P(A|B) directo"),
  valor = c(P_A, P_Ac, P_B_dado_A, P_B_dado_Ac,
            P_B_total, P_B_directo, P_A_dado_B, P_A_dado_B_directo)
)
guardar_tabla(resultados, "bayes_resultados")
print(as.data.frame(resultados), digits = 6)

tabla_2x2 <- tibble::tibble(
  evento  = c("A (área de interés)", "Aᶜ (otras áreas)", "Total"),
  mujeres = c(n_AB, n_AcB, n_B),
  hombres = c(n_A - n_AB, n_Ac - n_AcB, N - n_B),
  total   = c(n_A, n_Ac, N)
)
guardar_tabla(tabla_2x2, "bayes_tabla_2x2")
print(as.data.frame(tabla_2x2))

# ---- PASO 6: frases para la interpretación del informe --------------------
pct <- function(p) format(round(100 * p, 1), decimal.mark = ",")
cat("\nInterpretación (en frecuencias naturales):\n")
cat("- De cada 100 matriculados, ", pct(P_A), " están en el área de interés.\n", sep = "")
cat("- De cada 100 matriculados EN esa área, ", pct(P_B_dado_A), " son mujeres: P(B|A).\n", sep = "")
cat("- De cada 100 matriculados en las demás áreas, ", pct(P_B_dado_Ac), " son mujeres: P(B|Aᶜ).\n", sep = "")
cat("- De cada 100 MUJERES matriculadas, ", pct(P_A_dado_B), " están en el área de interés: P(A|B).\n", sep = "")
cat("- P(B|A) y P(A|B) difieren porque el universo cambia: el primero mira dentro del área; el segundo, dentro de las mujeres.\n")

message("06_bayes.R finalizado.")