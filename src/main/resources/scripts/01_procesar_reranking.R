if (Sys.info()["sysname"] == "Windows") {
  .libPaths(c("C:/Users/jobfm/Documents/R/win-library/4.1", .libPaths()))
}
# 01_procesar_reranking.R
# Pipeline analítico: Ingesta SBP + Cadenas de Markov + Visualizaciones Estocásticas, Concentración y Dinámica Temporal

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
  library(markovchain)
  library(ggplot2)
  library(jsonlite)
  library(base64enc)
})

# 1. Definición de argumentos
args <- commandArgs(trailingOnly = TRUE)
archivo_excel <- if (length(args) > 0) args[1] else "RE-RANKING-en-RE0026_.xlsx"

if (!file.exists(archivo_excel)) {
  stop(paste("No se encontró el archivo:", archivo_excel))
}

# 2. Diccionario de conversión de meses en español a fechas
normalizar_fecha_hoja <- function(nombre_hoja) {
  meses <- c(
    "ene" = "01", "enero" = "01", "feb" = "02", "febrero" = "02",
    "mar" = "03", "marz" = "03", "marzo" = "03", "abr" = "04", "abril" = "04",
    "may" = "05", "mayo" = "05", "jun" = "06", "junio" = "06",
    "jul" = "07", "julio" = "07", "ago" = "08", "agosto" = "08",
    "sep" = "09", "sept" = "09", "septiembre" = "09",
    "oct" = "10", "octubre" = "10", "nov" = "11", "noviembre" = "11",
    "dic" = "12", "diciembre" = "12"
  )

  texto <- tolower(str_trim(nombre_hoja))
  anio <- str_extract(texto, "\\d{4}")
  mes_str <- str_extract(texto, "^[a-z]+")

  mes_num <- meses[mes_str]
  if (is.na(mes_num) || is.na(anio)) return(as.Date("2020-01-01"))

  as.Date(paste0(anio, "-", mes_num, "-01"))
}

# 3. Función para procesar cada hoja mensual
procesar_hoja <- function(hoja) {
  raw_df <- read_excel(archivo_excel, sheet = hoja, col_names = FALSE)

  idx_header <- which(apply(raw_df, 1, function(r) any(str_detect(r, regex("BANCOS|TOTAL CARTERA", ignore_case = TRUE)))))
  if (length(idx_header) == 0) return(NULL)
  idx_header <- idx_header[1]

  df <- read_excel(archivo_excel, sheet = hoja, skip = idx_header - 1)

  col_names_estandar <- c("ranking", "banco", "total_cartera", "total_construccion",
                          "ponderacion", "vivienda_interino", "local_comercial_interino",
                          "infraestructura", "otras_construcciones")
  colnames(df) <- col_names_estandar[1:ncol(df)]

  fecha_mes <- normalizar_fecha_hoja(hoja)

  df_limpio <- df %>%
    filter(!is.na(banco),
           !str_detect(tolower(banco), "total|fuente|sistema|nota"),
           !is.na(total_construccion)) %>%
    mutate(
      fecha_corte = fecha_mes,
      ranking = suppressWarnings(as.numeric(ranking)),
      total_cartera = suppressWarnings(as.numeric(total_cartera)),
      total_construccion = suppressWarnings(as.numeric(total_construccion)),
      ponderacion = suppressWarnings(as.numeric(ponderacion)),
      vivienda_interino = suppressWarnings(as.numeric(vivienda_interino)),
      local_comercial_interino = suppressWarnings(as.numeric(local_comercial_interino)),
      infraestructura = suppressWarnings(as.numeric(infraestructura)),
      otras_construcciones = suppressWarnings(as.numeric(otras_construcciones))
    )

  return(df_limpio)
}

# 4. Consolidación de datos
hojas <- excel_sheets(archivo_excel)
datos_consolidados <- map_dfr(hojas, procesar_hoja) %>%
  arrange(banco, fecha_corte)

# 5. Modelado Estocástico: Clasificación en Estados para Cadenas de Markov
totales_mensuales <- datos_consolidados %>%
  group_by(fecha_corte) %>%
  summarise(saldo_sistema = sum(total_construccion, na.rm = TRUE))

datos_con_estados <- datos_consolidados %>%
  left_join(totales_mensuales, by = "fecha_corte") %>%
  mutate(
    cuota_mercado = ifelse(saldo_sistema > 0, (total_construccion / saldo_sistema) * 100, 0),
    estado_markov = case_when(
      cuota_mercado >= 15.0 ~ "TIER_1_ALTO",
      cuota_mercado >= 3.0  ~ "TIER_2_MEDIO",
      cuota_mercado > 0.0   ~ "TIER_3_BAJO",
      TRUE                  ~ "TIER_4_CERO"
    )
  )

# 6. Estimación de la Matriz de Transición de Markov
secuencias <- datos_con_estados %>%
  group_by(banco) %>%
  summarise(transiciones = list(estado_markov)) %>%
  pull(transiciones)

estados_posibles <- c("TIER_1_ALTO", "TIER_2_MEDIO", "TIER_3_BAJO", "TIER_4_CERO")
fit_markov <- markovchainFit(data = secuencias, possibleStates = estados_posibles, method = "mle")
matriz_probabilidades <- fit_markov$estimate@transitionMatrix

# Helper para exportar gráficas a Base64
guardar_grafico_base64 <- function(plot_obj, width = 8, height = 5.2) {
  img_temp <- tempfile(fileext = ".png")
  ggsave(img_temp, plot = plot_obj, width = width, height = height, dpi = 150)
  paste0("data:image/png;base64,", base64enc::base64encode(img_temp))
}

# 7.1 Gráfico 1: Heatmap de la Matriz de Transición
df_heatmap <- as.data.frame(as.table(matriz_probabilidades))
colnames(df_heatmap) <- c("Origen", "Destino", "Probabilidad")

p_heatmap <- ggplot(df_heatmap, aes(x = Destino, y = Origen, fill = Probabilidad)) +
  geom_tile(color = "#0f172a", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", Probabilidad)), color = "#0f172a", fontface = "bold", size = 4.2) +
  scale_fill_gradient(low = "#bae6fd", high = "#0284c7") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Matriz de Transición Estocástica (Cadenas de Markov)",
    subtitle = "Probabilidad mensual de migración de cuota de crédito en Construcción SBP",
    x = "Estado Futuro (t+1)",
    y = "Estado Actual (t)"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#0f172a"),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold")
  )

grafico_heatmap_base64 <- guardar_grafico_base64(p_heatmap, width = 8, height = 5.5)

# 7.2 Gráfico 2: Proyección y Convergencia Estocástica (P^t para t = 1, 2, 3, 6, 12, 24 meses)
horizontes <- c(1, 2, 3, 6, 12, 24)
df_proyeccion <- data.frame()
mc_obj <- fit_markov$estimate

for (h in horizontes) {
  P_h <- (mc_obj ^ h)@transitionMatrix
  for (i in rownames(P_h)) {
    for (j in colnames(P_h)) {
      df_proyeccion <- rbind(df_proyeccion, data.frame(
        Meses = h,
        Origen = i,
        Destino = j,
        Probabilidad = P_h[i, j]
      ))
    }
  }
}

p_proyeccion <- ggplot(df_proyeccion %>% filter(Origen == Destino),
                       aes(x = Meses, y = Probabilidad, color = Origen, group = Origen)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = horizontes) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("TIER_1_ALTO" = "#0284c7", "TIER_2_MEDIO" = "#10b981",
                                "TIER_3_BAJO" = "#f59e0b", "TIER_4_CERO" = "#ef4444")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Persistencia Estocástica y Convergencia en el Tiempo (P^t)",
    subtitle = "Probabilidad de permanencia en el mismo Tier a 1, 3, 6, 12 y 24 meses",
    x = "Horizonte Temporal (Meses)",
    y = "Probabilidad de Permanencia",
    color = "Nivel de Exposición"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#0f172a"),
    legend.position = "bottom"
  )

grafico_proyeccion_base64 <- guardar_grafico_base64(p_proyeccion, width = 8, height = 5)

# 7.3 Gráfico 3: Concentración de Cartera Bancaria (Top 10 Entidades)
fecha_reciente <- max(datos_con_estados$fecha_corte, na.rm = TRUE)
top_bancos <- datos_con_estados %>%
  filter(fecha_corte == fecha_reciente, total_construccion > 0) %>%
  arrange(desc(total_construccion)) %>%
  slice_head(n = 10) %>%
  mutate(
    banco_corto = str_trunc(banco, 26),
    saldo_millones = total_construccion / 1e6
  )

p_concentracion <- ggplot(top_bancos, aes(x = reorder(banco_corto, saldo_millones), y = saldo_millones, fill = estado_markov)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TIER_1_ALTO" = "#0284c7", "TIER_2_MEDIO" = "#10b981",
                               "TIER_3_BAJO" = "#f59e0b", "TIER_4_CERO" = "#ef4444")) +
  theme_minimal(base_size = 12) +
  labs(
    title = paste("Concentración del Crédito en Construcción (Corte:", format(fecha_reciente, "%m/%Y"), ")"),
    subtitle = "Top 10 Entidades Financieras en Panamá (Saldos en Millones USD)",
    x = NULL,
    y = "Saldo de Construcción (Millones USD)",
    fill = "Clasificación"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#0f172a"),
    legend.position = "bottom"
  )

grafico_concentracion_base64 <- guardar_grafico_base64(p_concentracion, width = 8.5, height = 5.2)

# 7.4 Gráfico 4: Evolución Temporal por Áreas Apiladas (Stacked Area Chart - 100% Cuota de Mercado)
evolucion_tiers <- datos_con_estados %>%
  group_by(fecha_corte, estado_markov) %>%
  summarise(saldo_tier = sum(total_construccion, na.rm = TRUE), .groups = "drop") %>%
  group_by(fecha_corte) %>%
  mutate(
    saldo_total_mes = sum(saldo_tier),
    proporcion = ifelse(saldo_total_mes > 0, saldo_tier / saldo_total_mes, 0)
  ) %>%
  ungroup() %>%
  mutate(estado_markov = factor(estado_markov, levels = rev(estados_posibles)))

p_area <- ggplot(evolucion_tiers, aes(x = fecha_corte, y = proporcion, fill = estado_markov)) +
  geom_area(alpha = 0.9, position = "stack") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  scale_fill_manual(values = c("TIER_1_ALTO" = "#0284c7", "TIER_2_MEDIO" = "#10b981",
                               "TIER_3_BAJO" = "#f59e0b", "TIER_4_CERO" = "#ef4444")) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Evolución Temporal de la Cuota de Mercado por Estratos (Stacked Area)",
    subtitle = "Participación histórica mensual del saldo crediticio según clasificación Markov",
    x = "Línea Temporal",
    y = "Participación Acumulada (% del Sistema)",
    fill = "Estrato"
  ) +
  theme(
    plot.title = element_text(face = "bold", color = "#0f172a"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


  # 7.5 Gráfico 5: Diagrama de Cajas (Boxplot) de Distribución de Saldos por Tier
  df_boxplot <- datos_con_estados %>%
    filter(total_construccion > 0) %>%
    mutate(saldo_millones = total_construccion / 1e6)

  p_boxplot <- ggplot(df_boxplot, aes(x = estado_markov, y = saldo_millones, fill = estado_markov)) +
    geom_boxplot(outlier.colour = "#ef4444", outlier.alpha = 0.6, alpha = 0.85) +
    scale_y_log10(labels = scales::dollar_format(suffix = "M", prefix = "$")) +
    scale_fill_manual(values = c("TIER_1_ALTO" = "#0284c7", "TIER_2_MEDIO" = "#10b981",
                                 "TIER_3_BAJO" = "#f59e0b", "TIER_4_CERO" = "#ef4444")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Distribución y Asimetría del Saldo por Estrato (Escala Log)",
      subtitle = "Dispersión, cuartiles y valores atípicos de exposición crediticia",
      x = "Estrato de Exposición",
      y = "Saldo Crédito Construcción (Log USD)",
      fill = "Estrato"
    ) +
    theme(
      plot.title = element_text(face = "bold", color = "#0f172a"),
      legend.position = "none"
    )

  grafico_boxplot_base64 <- guardar_grafico_base64(p_boxplot, width = 8.5, height = 5)

  # 7.6 Gráfico 6: Dispersión y Regresión Lineal (Cartera Total vs Cartera Construcción)
  df_scatter <- datos_con_estados %>%
    filter(total_cartera > 0, total_construccion > 0) %>%
    mutate(
      cartera_total_m = total_cartera / 1e6,
      construccion_m = total_construccion / 1e6
    )

  p_scatter <- ggplot(df_scatter, aes(x = cartera_total_m, y = construccion_m)) +
    geom_point(color = "#0284c7", alpha = 0.35, size = 2) +
    geom_smooth(method = "lm", color = "#0f172a", linewidth = 1.1, se = TRUE) +
    scale_x_log10(labels = scales::dollar_format(suffix = "M", prefix = "$")) +
    scale_y_log10(labels = scales::dollar_format(suffix = "M", prefix = "$")) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Correlación Bivariada: Cartera General vs. Sector Construcción",
      subtitle = "Ajuste por mínimos cuadrados ordinarios (MCO) en escala logarítmica",
      x = "Cartera Total del Banco (Log USD)",
      y = "Crédito en Construcción (Log USD)"
    ) +
    theme(
      plot.title = element_text(face = "bold", color = "#0f172a")
    )

grafico_area_base64 <- guardar_grafico_base64(p_area, width = 8.5, height = 5.2)

# 8. Salida JSON estructurada para consumo de Quarkus
salida_resultado <- list(
  status = "SUCCESS",
  total_registros_historicos = nrow(datos_con_estados),
  total_bancos_analizados = n_distinct(datos_con_estados$banco),
  estados = estados_posibles,
  matriz_transicion = as.data.frame(matriz_probabilidades),
  grafico_markov_base64 = grafico_heatmap_base64,
  grafico_proyeccion_base64 = grafico_proyeccion_base64,
  grafico_concentracion_base64 = grafico_concentracion_base64,
  grafico_area_base64 = grafico_area_base64,
  grafico_boxplot_base64 = grafico_boxplot_base64,
  grafico_scatter_base64 = grafico_scatter_base64
)

cat(toJSON(salida_resultado, auto_unbox = TRUE))