if (Sys.info()["sysname"] == "Windows") {
  .libPaths(c("C:/Users/jobfm/Documents/R/win-library/4.1", .libPaths()))
}
# 01_procesar_reranking.R
# Pipeline analítico: Ingesta SBP + Cadenas de Markov + Exportación JSON/Base64

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

# 1. Definición de argumentos (Permite ejecución dinámica desde Quarkus o manual)
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
# - TIER_1_ALTO: Cuota de mercado en construcción > 15%
# - TIER_2_MEDIO: Cuota entre 3% y 15%
# - TIER_3_BAJO: Cuota < 3% y > 0
# - TIER_4_CERO: Saldo 0 o nulo
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

# 7. Generación de Gráfico con ggplot2 (Mapa de calor de la matriz de transición)
df_heatmap <- as.data.frame(as.table(matriz_probabilidades))
colnames(df_heatmap) <- c("Origen", "Destino", "Probabilidad")

p_markov <- ggplot(df_heatmap, aes(x = Destino, y = Origen, fill = Probabilidad)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = round(Probabilidad, 3)), color = "black", fontface = "bold") +
  scale_fill_gradient(low = "#e0f2fe", high = "#0284c7") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Matriz de Transición Estocástica (Cadenas de Markov)",
    subtitle = "Probabilidad mensual de migración de cuota de crédito en Construcción SBP",
    x = "Estado Futuro (t+1)",
    y = "Estado Actual (t)"
  ) +
  theme(panel.grid = element_blank())

# Guardar y convertir a Base64
img_temp <- tempfile(fileext = ".png")
ggsave(img_temp, plot = p_markov, width = 8, height = 6, dpi = 150)
grafico_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(img_temp))

# 8. Salida JSON estructurada para consumo de Quarkus
salida_resultado <- list(
  status = "SUCCESS",
  total_registros_historicos = nrow(datos_con_estados),
  total_bancos_analizados = n_distinct(datos_con_estados$banco),
  estados = estados_posibles,
  matriz_transicion = as.data.frame(matriz_probabilidades),
  grafico_markov_base64 = grafico_base64
)

cat(toJSON(salida_resultado, auto_unbox = TRUE))
