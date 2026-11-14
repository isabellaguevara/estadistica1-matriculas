# Integrante 2 | Probabilidad de dos eventos sobre un único corte
source(here::here("R", "00_setup.R"))
d <- cargar_corte()

# Revisar las etiquetas reales del catálogo antes de definir estos eventos.
# Cambiar "Oficial", "Virtual" y "A distancia" por etiquetas verificadas.
stopifnot(all(c("sector", "metodologia") %in% names(d)))
A <- d$sector == "Oficial"
B <- d$metodologia %in% c("Virtual", "A distancia")
w <- d$matriculados
N <- sum(w)

P_A <- sum(w[A], na.rm = TRUE) / N
P_B <- sum(w[B], na.rm = TRUE) / N
P_AB <- sum(w[A & B], na.rm = TRUE) / N
P_B_dado_A <- P_AB / P_A

resultados <- tibble(
  evento = c("P(A)", "P(B)", "P(A y B)", "P(B|A)"),
  probabilidad = c(P_A, P_B, P_AB, P_B_dado_A)
)
print(resultados)
guardar_tabla(resultados, "probabilidades")
