# 08_tcl.R | Responsable: Integrante 3
# Verificación del Teorema del Límite Central.
# Variable: matriculados por grupo. Población: las 134.941 filas de 2017-2020
# (la misma de la LGN, sección 8). mu = 71,43; sigma = 217,49 (poblacional).
source(here::here("R", "00_setup.R"))   # fija set.seed(2026)

datos <- cargar_datos()
x     <- datos$matriculados
N     <- length(x)
mu    <- mean(x)
sigma <- sqrt(mean((x - mu)^2))          # poblacional (divisor N), NO sd(x)
g1    <- mean((x - mu)^3) / sigma^3      # asimetría de la población

stopifnot(N == 134941, round(mu, 2) == 71.43, round(sigma, 2) == 217.49)

NS <- c(2, 5, 30, 100, 500)   # tamaños pedidos por la guía
# NS <- c(NS, 5000)           # opcional: n grande para mostrar la convergencia lenta
B  <- 2000                    # muestras por cada n (la guía pide B >= 1000)

fuente <- "Fuente: MEN (SNIES), 2017-2020; cálculos propios."
fmt_n  <- scales::label_comma(big.mark = ".", decimal.mark = ",")

# ---- Simulación: B medias muestrales por cada n (con reemplazo) -----------
medias <- purrr::map_dfr(NS, function(n) {
  tibble::tibble(n = n, media = replicate(B, mean(sample(x, n, replace = TRUE))))
}) |>
  dplyr::mutate(n_lab = factor(paste0("n = ", n), levels = paste0("n = ", NS)))

# ---- Resumen: ¿se cumple sd = sigma/sqrt(n)? ¿y la forma? ----------------
resumen <- medias |>
  dplyr::group_by(n) |>
  dplyr::summarise(
    media_de_medias    = mean(media),
    sd_empirica        = sd(media),                       # dispersión de las B medias
    sd_teorica         = sigma / sqrt(first(n)),
    razon_emp_teo      = sd_empirica / sd_teorica,        # ~ 1 si se cumple
    asimetria_empirica = mean((media - mean(media))^3) / sqrt(mean((media - mean(media))^2))^3,
    asimetria_teorica  = g1 / sqrt(first(n)),             # la asimetría de la media cae como 1/sqrt(n)
    pct_dentro_2se     = mean(abs(media - mu) <= 2 * sigma / sqrt(first(n))),  # normal: 95,45 %
    .groups = "drop"
  )
guardar_tabla(resumen, "tcl_resumen")
print(as.data.frame(resumen), digits = 4)

# ---- Histogramas con la normal teórica N(mu, sigma/sqrt(n)) ---------------
curva <- purrr::map_dfr(NS, function(n) {
  se <- sigma / sqrt(n)
  xs <- seq(mu - 4 * se, mu + 4 * se, length.out = 400)
  tibble::tibble(n = n, x = xs, y = dnorm(xs, mu, se))
}) |>
  dplyr::mutate(n_lab = factor(paste0("n = ", n), levels = paste0("n = ", NS)))

p_hist <- ggplot2::ggplot(medias, ggplot2::aes(x = media)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                          bins = 40, fill = "steelblue", color = "white") +
  ggplot2::geom_line(data = curva, ggplot2::aes(x = x, y = y),
                     color = "firebrick", linewidth = 0.8) +
  ggplot2::geom_vline(xintercept = mu, linetype = "dashed") +
  ggplot2::facet_wrap(~ n_lab, scales = "free", ncol = 2) +
  ggplot2::scale_x_continuous(labels = fmt_n) +
  ggplot2::labs(
    title = "Distribución de la media muestral según el tamaño de muestra",
    subtitle = paste0(format(B, big.mark = "."), " muestras por n; curva roja: N(µ, σ/√n); línea punteada: µ = 71,43"),
    x = "Media muestral de matriculados por grupo (personas)",
    y = "Densidad", caption = fuente
  ) +
  ggplot2::theme_minimal()
guardar_fig("tcl_histogramas", p_hist, w = 10, h = 9)

# ---- Gráficos Q-Q: si los puntos siguen la recta, la media es ~ normal ----
p_qq <- ggplot2::ggplot(medias, ggplot2::aes(sample = media)) +
  ggplot2::stat_qq(alpha = 0.4, size = 0.8) +
  ggplot2::stat_qq_line(color = "firebrick") +
  ggplot2::facet_wrap(~ n_lab, scales = "free", ncol = 2) +
  ggplot2::labs(
    title = "Gráficos Q-Q de la media muestral frente a la normal",
    x = "Cuantiles teóricos de la normal", y = "Cuantiles de la media muestral (personas)",
    caption = fuente
  ) +
  ggplot2::theme_minimal()
guardar_fig("tcl_qq", p_qq, w = 10, h = 9)

# ---- La dispersión cae como sigma/sqrt(n) --------------------------------
p_sd <- ggplot2::ggplot(resumen, ggplot2::aes(x = n)) +
  ggplot2::geom_line(ggplot2::aes(y = sd_teorica, color = "Teórica σ/√n")) +
  ggplot2::geom_point(ggplot2::aes(y = sd_empirica, color = "Empírica")) +
  ggplot2::scale_x_log10() + ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "Desviación estándar de la media muestral: empírica vs. teórica",
    subtitle = "Escalas logarítmicas", x = "Tamaño de muestra n (filas)",
    y = "Desviación estándar de la media (personas)", color = NULL, caption = fuente
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
guardar_fig("tcl_dispersion", p_sd)

message("08_tcl.R finalizado. Revise output/tablas/tcl_resumen.csv y las figuras tcl_*.png")