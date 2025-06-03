
# Minería de Opinión - WhatsApp | Mauricio Ríos
# Librerías necesarias
library(syuzhet)
library(tidyverse)
library(lubridate)
library(readr)

# Leer el archivo de WhatsApp
archivo <- "chat.txt"
lineas <- readLines(archivo, encoding = "UTF-8")
mensajes <- lineas[grepl(" - .*?:", lineas)]

# Crear dataframe con fecha, autor y mensaje
df <- data.frame(
  fecha_hora = sub(" - .*", "", mensajes),
  autor = sub(".* - (.*?): .*", "\1", mensajes),
  mensaje = sub(".*?: ", "", mensajes),
  stringsAsFactors = FALSE
)

# Parsear fecha y extraer hora y día
df$fecha_hora <- parse_date_time(df$fecha_hora, orders = c("dmy HM", "dmy IMp", "dmy IMS p", "dmy, I:M p"), tz = "America/Argentina/Buenos_Aires")
df$Hora <- hour(df$fecha_hora)
df$`Nombre del día` <- weekdays(df$fecha_hora)
df$`Día de la semana` <- wday(df$fecha_hora, label = TRUE, abbr = FALSE, week_start = 1)

# Limpieza del mensaje
df$mensaje_limpio <- tolower(df$mensaje)
df$mensaje_limpio <- gsub("[[:punct:]]", "", df$mensaje_limpio)
df$mensaje_limpio <- gsub("[0-9]", "", df$mensaje_limpio)
df$mensaje_limpio <- gsub("[
]", " ", df$mensaje_limpio)
df$mensaje_limpio <- trimws(df$mensaje_limpio)

# Análisis NRC y sentimientos
emociones <- get_nrc_sentiment(df$mensaje_limpio)
df <- bind_cols(df, emociones)
df$sentimiento_general <- get_sentiment(df$mensaje_limpio)

# Clasificación textual del sentimiento
df <- df %>%
  mutate(
    Sentimiento = case_when(
      joy + trust + surprise + anticipation > anger + sadness + fear + negative ~ "Positivo",
      anger + sadness + fear + negative > joy + trust + surprise + anticipation ~ "Negativo",
      TRUE ~ "Neutro"
    )
  )

# Traducir nombres de columnas
df <- df %>%
  rename(
    ira = anger,
    anticipacion = anticipation,
    disgusto = disgust,
    miedo = fear,
    alegria = joy,
    tristeza = sadness,
    sorpresa = surprise,
    confianza = trust,
    negativo = negative,
    positivo = positive
  )

# Clasificación emociones avanzadas
df$`Emociones avanzadas` <- ifelse(
  rowSums(df[c("ira", "miedo", "disgusto", "tristeza", "negativo")]) > 0 |
  rowSums(df[c("alegria", "anticipacion", "sorpresa", "confianza", "positivo")]) > 0,
  "Con emociones avanzadas",
  "Sin emociones avanzadas"
)

# Puntajes sentimentales extra
df$syuzhet <- get_sentiment(df$mensaje_limpio, method = "syuzhet")
df$bing    <- get_sentiment(df$mensaje_limpio, method = "bing")
df$afinn   <- get_sentiment(df$mensaje_limpio, method = "afinn")
df$nrc     <- get_sentiment(df$mensaje_limpio, method = "nrc")

# Filtrar mensajes con emociones
emocion_cols <- c("ira", "anticipacion", "disgusto", "miedo", "alegria", "tristeza",
                  "sorpresa", "confianza", "negativo", "positivo")
df$suma_emociones <- rowSums(df[emocion_cols])
df_filtrado <- df %>% filter(suma_emociones > 0)

# Convertir a formato largo
df_largo <- df_filtrado %>%
  pivot_longer(
    cols = all_of(emocion_cols),
    names_to = "Emocion",
    values_to = "Valor"
  ) %>%
  filter(Valor > 0)

# Exportar CSV final
write_csv(df_largo, "emociones_completas_final.csv")
