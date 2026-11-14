# Integrante 3 | Teorema de Bayes, ponderado por matriculados
source(here::here("R", "00_setup.R"))
d <- cargar_corte()

# Sustituir las etiquetas por las verificadas en los catálogos.
stopifnot(all(c("area", "genero") %in% names(d)))
A <- d$area == "Área elegida"
B <- d$genero == "Mujer"
w <- d$matriculados
N <- sum(w)

P_B_dado_A <- sum(w[A & B], na.rm = TRUE) / sum(w[A], na.rm = TRUE)
P_A <- sum(w[A], na.rm = TRUE) / N
P_B_dado_noA <- sum(w[!A & B], na.rm = TRUE) / sum(w[!A], na.rm = TRUE)
P_noA <- sum(w[!A], na.rm = TRUE) / N
bayes <- P_B_dado_A * P_A /
  (P_B_dado_A * P_A + P_B_dado_noA * P_noA)
directa <- sum(w[A & B], na.rm = TRUE) / sum(w[B], na.rm = TRUE)

resultados <- tibble(metodo = c("Bayes", "Cálculo directo"),
                     P_A_dado_B = c(bayes, directa))
print(resultados)
guardar_tabla(resultados, "teorema_bayes")
