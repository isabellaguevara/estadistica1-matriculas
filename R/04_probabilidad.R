
# ============================================================
# INTEGRANTE 2
# Pregunta 6: Probabilidad de dos eventos sobre un único corte
# ============================================================

source(here::here("R", "00_setup.R"))

# Cargar datos del año de corte (2020)
d <- cargar_corte()


# ============================================================
# 1. VERIFICACIÓN Y DEFINICIÓN DE LOS EVENTOS
# ============================================================

stopifnot(
  all(c("area", "genero", "matriculados") %in% names(d))
)

# A: El matriculado pertenece al área de Ingeniería,
#    Arquitectura, Urbanismo y afines.
#
# B: El matriculado es de género femenino.

A <- d$area %in% "INGENIERÍA, ARQUITECTURA, URBANISMO Y AFINES"
B <- d$genero %in% "Femenino"

# Pesos y total de matriculados
w <- d$matriculados
N <- sum(w, na.rm = TRUE)


# ============================================================
# 2. CÁLCULO DE LAS PROBABILIDADES
# ============================================================

# Probabilidad de A
P_A <- sum(w[A], na.rm = TRUE) / N

# Probabilidad de B
P_B <- sum(w[B], na.rm = TRUE) / N

# Probabilidad de la intersección A y B
P_AB <- sum(w[A & B], na.rm = TRUE) / N

# Probabilidad de B dado A
P_B_dado_A <- P_AB / P_A

# Probabilidad del complemento de A
P_Ac <- 1 - P_A

# Probabilidad de B dado el complemento de A
P_B_dado_Ac <- sum(w[!A & B], na.rm = TRUE) /
  sum(w[!A], na.rm = TRUE)


# ============================================================
# 3. TABLA DE RESULTADOS
# ============================================================

resultados <- tibble(
  evento = c(
    "P(A)",
    "P(B)",
    "P(A y B)",
    "P(B|A)",
    "P(A complemento)",
    "P(B|A complemento)"
  ),
  descripcion = c(
    "Matriculado en Ingeniería y afines",
    "Matriculado de género femenino",
    "Mujer matriculada en Ingeniería y afines",
    "Mujer dado que estudia Ingeniería y afines",
    "Matriculado en otras áreas o sin dato de área",
    "Mujer dado que no pertenece a Ingeniería y afines"
  ),
  probabilidad = c(
    P_A,
    P_B,
    P_AB,
    P_B_dado_A,
    P_Ac,
    P_B_dado_Ac
  )
)

print(resultados)

guardar_tabla(resultados, "probabilidades")


# ============================================================
# 4. TABLA DE CONTINGENCIA
# ============================================================

tabla_eventos <- tibble(
  area = c(
    "Ingeniería y afines",
    "Otras áreas o sin dato"
  ),
  mujeres = c(
    sum(w[A & B], na.rm = TRUE),
    sum(w[!A & B], na.rm = TRUE)
  ),
  hombres = c(
    sum(w[A & d$genero %in% "Masculino"], na.rm = TRUE),
    sum(w[!A & d$genero %in% "Masculino"], na.rm = TRUE)
  )
) |>
  mutate(
    total = mujeres + hombres
  )

print(tabla_eventos)

guardar_tabla(
  tabla_eventos,
  "tabla_eventos_probabilidad"
)


# ============================================================
# 5. VERIFICACIONES
# ============================================================

stopifnot(
  abs(sum(tabla_eventos$total) - N) < 1e-8,
  P_A >= 0 && P_A <= 1,
  P_B >= 0 && P_B <= 1,
  P_AB >= 0 && P_AB <= 1,
  P_B_dado_A >= 0 && P_B_dado_A <= 1,
  P_B_dado_Ac >= 0 && P_B_dado_Ac <= 1
)

message("Análisis de probabilidad finalizado correctamente.")
