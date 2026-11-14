# 02_descriptiva_tiempo.R  | Responsable: Integrante 1  | Pregunta 1
# Evolución de la matrícula en el tiempo. Cada periodo se compara con otros periodos;
# nunca se suman semestres (sería contar varias veces a la misma persona).
source(here::here("R", "00_setup.R"))
datos <- cargar_datos()

# Posición en el eje x: 2021 = primer semestre, 2021.5 = segundo
datos <- datos |> mutate(t = ano + (semestre - 1) / 2)

# --- Serie total por periodo -------------------------------------------------
serie <- datos |>
  group_by(ano, semestre, t) |>
  summarise(matriculados = sum(matriculados), filas = n()) |>
  ungroup() |>
  arrange(ano, semestre) |>
  mutate(periodo = sprintf("%d-%d", ano, semestre),
         var_periodo = matriculados / lag(matriculados) - 1)

# Variación interanual: mismo semestre del año anterior
previo <- serie |> transmute(ano = ano + 1L, semestre, prev = matriculados)
serie <- serie |>
  left_join(previo, by = c("ano", "semestre")) |>
  mutate(var_interanual = matriculados / prev - 1) |>
  select(-prev)
print(serie, n = Inf)
guardar_tabla(serie, "serie_matricula")

# Aviso: si el último periodo cae >10% frente al anterior, probablemente está incompleto
# y distorsiona la variación y el promedio anual. Considere excluirlo del análisis
# (p. ej. datos <- filter(datos, !(ano == 2024 & semestre == 2)) antes de armar la serie).
ult_p <- slice_tail(serie, n = 1)
if (!is.na(ult_p$var_periodo) && ult_p$var_periodo < -0.10)
  warning("El último periodo (", ult_p$periodo, ") cae ", percent(-ult_p$var_periodo, accuracy = 0.1),
          " frente al anterior: ¿está incompleto?")

# Promedio semestral por año (evita sumar semestres)
anual <- serie |>
  group_by(ano) |>
  summarise(promedio_semestral = mean(matriculados), n_semestres = n()) |>
  mutate(var_anual = promedio_semestral / lag(promedio_semestral) - 1)
print(anual, n = Inf)
guardar_tabla(anual, "matricula_anual")

cat("\nMayores caídas entre periodos consecutivos:\n")
print(slice_min(serie, var_periodo, n = 3) |> select(periodo, matriculados, var_periodo))
cat("\nMayores aumentos:\n")
print(slice_max(serie, var_periodo, n = 3) |> select(periodo, matriculados, var_periodo))

# --- Gráfico 1: serie total ---------------------------------------------------
ggplot(serie, aes(t, matriculados)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = unique(serie$ano)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Matrícula total en educación superior por periodo",
       subtitle = "Cada punto es un semestre (x.0 = primero, x.5 = segundo)",
       x = "Año", y = "Matriculados (personas)",
       caption = "Fuente: MEN (SNIES), datos.gov.co") +
  theme_minimal()
guardar_fig("serie_matricula")

# --- Gráfico 2: serie por sector ----------------------------------------------
por_sector <- datos |>
  group_by(t, sector) |>
  summarise(matriculados = sum(matriculados)) |>
  ungroup()
guardar_tabla(por_sector, "serie_por_sector")

ggplot(por_sector, aes(t, matriculados, colour = sector)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = unique(floor(por_sector$t))) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Matrícula por sector", x = "Año",
       y = "Matriculados (personas)", colour = "Sector",
       caption = "Fuente: MEN (SNIES), datos.gov.co") +
  theme_minimal()
guardar_fig("serie_por_sector")

# --- Gráfico 3: variación interanual ----------------------------------------
serie |>
  filter(!is.na(var_interanual)) |>
  ggplot(aes(t, var_interanual)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = unique(serie$ano)) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Variación interanual de la matrícula",
       subtitle = "Mismo semestre frente al año anterior",
       x = "Año", y = "Variación (%)") +
  theme_minimal()
guardar_fig("variacion_interanual")

# TODO (informe): interpretar tendencia, caídas/aumentos notables y posibles causas
# (sin afirmar causalidad que los datos no permiten).
