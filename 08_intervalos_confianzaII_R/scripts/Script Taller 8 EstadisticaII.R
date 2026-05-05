########################################################################/
# Script Taller 8 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 05-05-2026
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


# Visualización del IC de proporción agrupado -----------------------------

data_ic_prop <- data %>%
  drop_na(indig_nino, sex) %>%
  group_by(sex) %>%
  summarise(
    n       = n(),
    n_indig = sum(indig_nino == "Indigenous"),
    prop    = n_indig / n,
    lwr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "lwr.ci"],
    upr     = BinomCI(n_indig, n, conf.level = 0.95, method = "wald")[, "upr.ci"]
  )

ggplot(data_ic_prop, aes(x = sex, y = prop)) +  # sexo en x, proporción en y
  geom_point(size = 3, color = "#9467bd") +       # punto para la proporción de cada grupo
  geom_errorbar(aes(ymin = lwr, ymax = upr),      # barras de error con límites del IC
                width = 0.15, color = "#9467bd") + # ancho de las barras horizontales
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # formato porcentaje
  labs(title = "IC 95% para proporción de niños/as de pueblo originario según sexo",
       x = "Sexo",
       y = "Proporción estimada") +
  theme_minimal()


# Comparación de niveles de confianza -------------------------------------

# IC al 90%
BinomCI(x = n_indig,
        n = n_total,
        conf.level = 0.90,
        method = "wald")

# IC al 95%
BinomCI(x = n_indig,
        n = n_total,
        conf.level = 0.95,
        method = "wald")

# IC al 99%
BinomCI(x = n_indig,
        n = n_total,
        conf.level = 0.99,
        method = "wald")

########################################################################
