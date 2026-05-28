########################################################################/
# Solución Ejercicio 3 Estadística II ---------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 21-04-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Preparación -------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("tidyverse")
#install.packages("DescTools")

library(tidyverse)
library(DescTools)

load("data/ELSOC_Wide_2016_2023.RData")

# Filtramos ola 2023
data_elsoc_2023 <- elsoc_wide_2016_2023 %>%
  filter(ola_w07 == 1)

dim(data_elsoc_2023)

# Recodificamos valores perdidos en ítems de cohesión barrial
data_elsoc_2023 <- data_elsoc_2023 %>%
  mutate(
    t02_01_w07 = case_when(
      t02_01_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE                          ~ t02_01_w07
    ),
    t02_02_w07 = case_when(
      t02_02_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE                          ~ t02_02_w07
    ),
    t02_03_w07 = case_when(
      t02_03_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE                          ~ t02_03_w07
    )
  )

# Verificamos recodificación
table(data_elsoc_2023$t02_01_w07, exclude = FALSE)

# Creamos índice de cohesión barrial
data_elsoc_2023 <- data_elsoc_2023 %>%
  mutate(
    media_cohesion_barrial = rowMeans(cbind(t02_01_w07,  # ítem 1: barrio ideal
                                            t02_02_w07,  # ítem 2: integración barrial
                                            t02_03_w07), # ítem 3: identificación barrial
                                      na.rm = TRUE)
  )

summary(data_elsoc_2023$media_cohesion_barrial)


# IC medias ---------------------------------------------------------------

# IC al 95% para la media de cohesión barrial
MeanCI(data_elsoc_2023$media_cohesion_barrial,
       conf.level = 0.95,  # nivel de confianza
       na.rm = TRUE)       # excluir NA

# Interpretación:
# Con un 95% de confianza, el intervalo [3.651; 3.712] contiene el verdadero
# promedio poblacional de cohesión barrial en Chile (ola 2023).
# La media muestral estimada es 3.682 (en una escala de 1 a 5), lo que sugiere
# un nivel moderadamente alto de cohesión barrial en la población.

# Recodificamos sexo con etiquetas
data_elsoc_2023 <- data_elsoc_2023 %>%
  mutate(sexo = factor(m0_sexo_w07,
                       levels = c(1, 2),
                       labels = c("Hombre", "Mujer")))

# IC agrupado por sexo
data_elsoc_2023 %>%
  drop_na(media_cohesion_barrial, sexo) %>%
  group_by(sexo) %>%
  summarise(
    n     = n(),
    media = MeanCI(media_cohesion_barrial, conf.level = 0.95)[1],  # media
    lwr   = MeanCI(media_cohesion_barrial, conf.level = 0.95)[2],  # límite inferior
    upr   = MeanCI(media_cohesion_barrial, conf.level = 0.95)[3]   # límite superior
  )

# Interpretación:
# Los hombres presentan una media de cohesión barrial más alta (~3.75) que las
# mujeres (~3.645). Los intervalos de confianza al 95% de ambos grupos no se
# superponen (Hombres: [3.70; 3.79] — Mujeres: [3.60; 3.69]), lo que indica
# que la diferencia entre sexos es estadísticamente distinguible con un 95%
# de confianza.

# Gráfico
data_ic <- data_elsoc_2023 %>%
  drop_na(media_cohesion_barrial, sexo) %>%
  group_by(sexo) %>%
  summarise(
    media = MeanCI(media_cohesion_barrial, conf.level = 0.95)[1],
    lwr   = MeanCI(media_cohesion_barrial, conf.level = 0.95)[2],
    upr   = MeanCI(media_cohesion_barrial, conf.level = 0.95)[3]
  )

ggplot(data_ic, aes(x = sexo, y = media)) +
  geom_point(size = 3, color = "#9467bd") +
  geom_errorbar(aes(ymin = lwr, ymax = upr),
                width = 0.15, color = "#9467bd") +
  labs(title = "IC 95% para cohesión barrial según sexo",
       x = "Sexo",
       y = "Media cohesión barrial") +
  theme_minimal()
########################################################################
