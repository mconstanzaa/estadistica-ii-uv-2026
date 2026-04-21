########################################################################/
# Script Taller 7 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 21-04-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("tidyverse")
#install.packages("DescTools")

options(scipen = 999) # Evitamos notación científica

library(tidyverse)
library(DescTools)


# Base de datos -----------------------------------------------------------

load("data/data_elpi_selected_variables.RData")

data %>% glimpse()


# IC para medias: cálculo manual ------------------------------------------

data %>%
  drop_na(calculation) %>%             # eliminamos NA antes de calcular
  summarise(
    n     = n(),                        # tamaño muestral
    media = mean(calculation),          # media muestral (x̄)
    s     = sd(calculation),            # desviación estándar muestral (s)
    t     = qt(0.975, df = n - 1),      # valor crítico t para IC 95% (1 - α/2)
    lwr   = media - t * (s / sqrt(n)),  # límite inferior: x̄ - t × (s/√n)
    upr   = media + t * (s / sqrt(n))   # límite superior: x̄ + t × (s/√n)
  )


# IC para medias con MeanCI() ---------------------------------------------

MeanCI(data$calculation,
       conf.level = 0.95,  # nivel de confianza
       na.rm = TRUE)       # excluir NA


# IC agrupado por nivel educativo del cuidador/a --------------------------

data %>%
  drop_na(calculation, educ2012_rec) %>%          # eliminamos NA en ambas variables
  group_by(educ2012_rec) %>%                       # agrupamos por educación del cuidador/a
  summarise(
    n     = n(),
    media = MeanCI(calculation, conf.level = 0.95)[1],   # media
    lwr   = MeanCI(calculation, conf.level = 0.95)[2],   # límite inferior
    upr   = MeanCI(calculation, conf.level = 0.95)[3]    # límite superior
  )


# Visualización del IC por grupo ------------------------------------------

data_ic <- data %>%
  drop_na(calculation, educ2012_rec) %>%          # eliminamos NA en ambas variables
  group_by(educ2012_rec) %>%                       # agrupamos por educación del cuidador/a
  summarise(
    media = MeanCI(calculation, conf.level = 0.95)[1],  # media muestral por grupo
    lwr   = MeanCI(calculation, conf.level = 0.95)[2],  # límite inferior del IC 95%
    upr   = MeanCI(calculation, conf.level = 0.95)[3]   # límite superior del IC 95%
  )

ggplot(data_ic, aes(x = educ2012_rec, y = media)) +  # educación en x, media en y
  geom_point(size = 3, color = "#9467bd") +            # punto para la media de cada grupo
  geom_errorbar(aes(ymin = lwr, ymax = upr),           # barras de error con límites del IC
                width = 0.2, color = "#9467bd") +       # ancho de las barras horizontales
  labs(title = "IC 95% para puntaje en cálculo según educación del cuidador/a",
       x = "Nivel educativo del cuidador/a",           # etiqueta eje x
       y = "Puntaje en cálculo (estandarizado)") +     # etiqueta eje y
  theme_minimal() +                                     # estilo limpio sin fondo gris
  theme(axis.text.x = element_text(angle = 25,         # rota etiquetas del eje x
                                   hjust = 1))          # alinea etiquetas rotadas a la derecha


# IC para proporciones: cálculo manual ------------------------------------

table(data$indig_nino, exclude = FALSE)

data %>%
  drop_na(indig_nino) %>%                           # eliminamos NA antes de calcular
  summarise(
    n       = n(),                                   # tamaño muestral
    n_indig = sum(indig_nino == "Indigenous"),       # casos pertenecientes a pueblo originario
    p_hat   = n_indig / n,                           # proporción muestral (p̂)
    se      = sqrt(p_hat * (1 - p_hat) / n),         # error estándar
    lwr     = p_hat - 1.96 * se,                     # límite inferior (Z = 1.96)
    upr     = p_hat + 1.96 * se                      # límite superior
  )


# IC para proporciones con BinomCI() --------------------------------------

n_total <- sum(!is.na(data$indig_nino))                   # total sin NA
n_indig <- sum(data$indig_nino == "Indigenous",           # casos que cumplen condición
               na.rm = TRUE)

BinomCI(x = n_indig,
        n = n_total,
        conf.level = 0.95,    # nivel de confianza
        method = "wald")      # método de Wald: fórmula Z estándar


# IC de proporción agrupado por sexo --------------------------------------

data %>%
  drop_na(indig_nino, sex) %>%                                                      # eliminamos NA en ambas variables
  group_by(sex) %>%                                                                  # agrupamos por sexo
  summarise(
    n       = n(),                                                                   # tamaño muestral por grupo
    n_indig = sum(indig_nino == "Indigenous"),                                       # casos por grupo
    prop    = n_indig / n,                                                           # proporción por grupo (p̂)
    lwr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "lwr.ci"],  # límite inferior IC 95%
    upr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "upr.ci"]   # límite superior IC 95%
  )


# Visualización del IC de proporción --------------------------------------

data_ic_prop <- data %>%
  drop_na(indig_nino, sex) %>%                                                      # eliminamos NA en ambas variables
  group_by(sex) %>%                                                                  # agrupamos por sexo
  summarise(
    n       = n(),                                                                   # tamaño muestral por grupo
    n_indig = sum(indig_nino == "Indigenous"),                                       # casos por grupo
    prop    = n_indig / n,                                                           # proporción por grupo (p̂)
    lwr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "lwr.ci"],  # límite inferior IC 95%
    upr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "upr.ci"]   # límite superior IC 95%
  )

ggplot(data_ic_prop, aes(x = sex, y = prop)) +  # sexo en x, proporción en y
  geom_point(size = 3, color = "#9467bd") +       # punto para la proporción de cada grupo
  geom_errorbar(aes(ymin = lwr, ymax = upr),      # barras de error con límites del IC
                width = 0.15, color = "#9467bd") + # ancho de las barras horizontales
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # formato porcentaje
  labs(title = "IC 95% para proporción de niños/as de pueblo originario según sexo",
       x = "Sexo",                                # etiqueta eje x
       y = "Proporción estimada") +               # etiqueta eje y
  theme_minimal()                                  # estilo limpio sin fondo gris

########################################################################