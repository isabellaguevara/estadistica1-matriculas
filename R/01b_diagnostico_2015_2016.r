# 01b_diagnostico_2015_2016.R | Integrante 1
# Documenta la causa de que 2015-2016 no coincidan con el SNIES.
# Es solo de documentación: los compañeros NO necesitan ejecutarlo.
# Ejecutar después de 01_limpieza.R
source(here::here("R", "00_setup.R"))
completo <- cargar_datos_completo()   # 2015-2020

# 1. Repetidos y totales por año (pick() no incluye 'ano', la agrupación)
d1 <- completo |>
  group_by(ano) |>
  summarise(
    filas = n(),
    total_base = sum(matriculados),
    rep_clave = sum(duplicated(pick(snies, depto_prog, mpio_prog, id_genero, semestre))),
    rep_clave_sin_sem = sum(duplicated(pick(snies, depto_prog, mpio_prog, id_genero))),
    rep_exactas = sum(duplicated(pick(everything()))),
    .groups = "drop"
  )

# 2. Total si se eliminan las filas exactamente repetidas
d2 <- completo |>
  distinct() |>
  group_by(ano) |>
  summarise(total_sin_exactas = sum(matriculados), .groups = "drop")

# 3. ¿Cuántas IES reportan en ambos semestres el mismo año?
d3 <- completo |>
  distinct(ano, semestre, cod_ies) |>
  count(ano, cod_ies) |>
  group_by(ano) |>
  summarise(n_ies = n(), n_ies_ambos_sem = sum(n == 2), .groups = "drop")

# 4. Totales por semestre
d4 <- completo |>
  group_by(ano, semestre) |>
  summarise(filas = n(), total = sum(matriculados),
            n_ies = n_distinct(cod_ies), .groups = "drop")

diag <- d1 |>
  left_join(d2, by = "ano") |>
  left_join(d3, by = "ano") |>
  left_join(oficial_snies, by = "ano") |>
  mutate(dif_base = total_base - oficial_snies,
         dif_sin_exactas = total_sin_exactas - oficial_snies)

print(as.data.frame(diag))
print(as.data.frame(d4))
guardar_tabla(diag, "diagnostico_2015_2016")
guardar_tabla(d4, "diagnostico_semestres")