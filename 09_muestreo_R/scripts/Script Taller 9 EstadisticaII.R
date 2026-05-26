########################################################################/
# Script Taller 9 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 26-05-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("sampling")

library(tidyverse)
library(haven)
library(sampling)


# Base de datos -----------------------------------------------------------

data <- readRDS("data/base_92_20112024.Rds")

data %>% glimpse()


# Preparación: recodificar valores perdidos --------------------------------

data <- data %>%
  mutate(
    democracia_19 = case_when(
      democracia_19 %in% c(-8, -9) ~ NA_real_,
      TRUE ~ democracia_19
    ),
    bienestar_2 = case_when(
      bienestar_2 %in% c(-8, -9) ~ NA_real_,
      TRUE ~ bienestar_2
    )
  )


# Valores de referencia ---------------------------------------------------

data %>%
  summarise(
    media_democracia = mean(democracia_19, na.rm = TRUE),
    media_satisf     = mean(bienestar_2,   na.rm = TRUE),
    n                = n()
  )


# Muestreo Aleatorio Simple (MAS) -----------------------------------------

set.seed(2026)

# Una muestra aleatoria simple de tamaño 100
muestra_mas <- data %>%
  sample_n(100)

# Estimamos el promedio en esta muestra
mean(muestra_mas$democracia_19, na.rm = TRUE)


# MAS con y sin reemplazo -------------------------------------------------

# Sin reemplazo: un caso solo puede aparecer una vez
muestra_sin <- data %>% sample_n(100, replace = FALSE)

# Con reemplazo: un caso puede ser seleccionado más de una vez
muestra_con <- data %>% sample_n(100, replace = TRUE)

# Verificamos duplicados
table(duplicated(muestra_sin$id_bu))
table(duplicated(muestra_con$id_bu))


# Distribución muestral: repetir 1000 veces --------------------------------

simulaciones <- map_dbl(1:1000, ~{
  muestra <- sample_n(data, 100)                    # tomamos una muestra
  mean(muestra$democracia_19, na.rm = TRUE)          # calculamos su media
})

mean(simulaciones)

# Histograma de medias muestrales
hist(simulaciones,
     probability = TRUE,              # escala de densidad (no frecuencia)
     main = "Distribución de 1000 medias muestrales (n = 100 c/u)", # título
     xlab = "Media democracia_19",    # etiqueta eje x
     col  = "darkgrey", border = "white") # color barras y bordes

curve(dnorm(x, mean = mean(simulaciones), sd = sd(simulaciones)), # curva normal teórica
      col = "red", lwd = 2, add = TRUE) # color rojo, grosor 2, sobre el histograma

abline(v = mean(simulaciones), col = "blue", lty = 2, lwd = 2) # línea vertical en la media


# Muestreo Estratificado --------------------------------------------------

# Revisamos la distribución del estrato
table(data$sexo)
prop.table(table(data$sexo))
# 1 = Hombre (39%), 2 = Mujer (61%)

# La base debe estar ordenada por la variable de estrato
data <- data %>% arrange(sexo)

# Definimos el tamaño proporcional: n = 150 → ~39% Hombre, ~61% Mujer
estratos <- strata(data,
                   stratanames = c("sexo"),  # variable de estrato
                   size = c(58, 92),         # nᵢ por estrato (proporcional a 150)
                   method = "srswor")        # muestreo sin reemplazo dentro de cada estrato

table(estratos$sexo)

# Extraemos los datos de la muestra
muestra_estrat <- getdata(data, estratos)

# Verificamos la distribución resultante
table(muestra_estrat$sexo)
prop.table(table(muestra_estrat$sexo))

# Estimamos la media de democracia_19 por estrato
muestra_estrat %>%
  group_by(sexo) %>%
  summarise(
    media_democracia = mean(democracia_19, na.rm = TRUE),
    n                = n()
  )


# Estratificado por dos variables -----------------------------------------

# Recodificamos GSE a 4 categorías (D y E juntos)
data <- data %>%
  mutate(gse_4cat = case_when(
    gse %in% c(4, 5) ~ 4,
    TRUE             ~ gse
  ))

# Ordenamos por ambas variables de estrato
data <- data %>% arrange(sexo, gse_4cat)

# 2 × 4 = 8 estratos, 10 casos por estrato
estratos_2 <- strata(data,
                     stratanames = c("sexo", "gse_4cat"),
                     size = rep(10, 8),
                     method = "srswor")

muestra_estrat_2 <- getdata(data, estratos_2)
table(muestra_estrat_2$sexo, muestra_estrat_2$gse_4cat)


# Muestreo por Conglomerados ----------------------------------------------

# Revisamos las macro-zonas disponibles como conglomerados
table(data$region_3)

# Seleccionamos 3 macro-zonas al azar
muestra_conglom <- cluster(data,
                           clustername = "region_3",  # variable de conglomerado
                           size = 3,                  # número de grupos a seleccionar
                           method = "srswor")

# Extraemos los datos de las zonas seleccionadas
muestra_conglom_final <- getdata(data, muestra_conglom)

# ¿Qué macro-zonas fueron seleccionadas?
unique(muestra_conglom_final$region_3)


# Conglomerados: segunda etapa --------------------------------------------

# Segunda etapa: 30 casos por macro-zona seleccionada
muestra_2etapas <- muestra_conglom_final %>%
  group_by(region_3) %>%
  slice_sample(n = 30) %>%
  ungroup()

table(muestra_2etapas$region_3)

# También podemos seleccionar una proporción en vez de un número fijo
muestra_2etapas_prop <- muestra_conglom_final %>%
  group_by(region_3) %>%
  slice_sample(prop = 0.50) %>%  # 50% de cada grupo
  ungroup()

table(muestra_2etapas_prop$region_3)


# Comparación entre tipos de muestreo -------------------------------------

# Valor de referencia (base CEP completa)
media_ref  <- mean(data$democracia_19, na.rm = TRUE)

# Media en muestra MAS (n = 100)
media_mas  <- mean(muestra_mas$democracia_19, na.rm = TRUE)

# Media en muestra estratificada
media_est  <- mean(muestra_estrat$democracia_19, na.rm = TRUE)

# Media en muestra conglomerados (dos etapas)
media_cong <- mean(muestra_2etapas$democracia_19, na.rm = TRUE)

tibble(
  tipo  = c("Base completa (ref.)", "MAS", "Estratificado", "Conglomerados"),
  media = round(c(media_ref, media_mas, media_est, media_cong), 3)
)
########################################################################
