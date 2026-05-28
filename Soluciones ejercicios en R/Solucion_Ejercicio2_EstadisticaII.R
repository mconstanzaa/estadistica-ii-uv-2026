########################################################################/
# Solución Ejercicio 2 Estadística II --------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 31-03-2026
########################################################################/

rm(list=ls())


# Paquetes ----------------------------------------------------------------

library(dplyr)


# Base de datos -----------------------------------------------------------

load("data/ELSOC_Wide_2016_2023.RData")


# Exploración base de datos -----------------------------------------------

elsoc_wide_2016_2023 %>% glimpse()


# Mutate 1 ----------------------------------------------------------------

# Recodificamos -999 y -888 como NA en los ítems de identidad barrial
elsoc_wide_2016_2023 <- elsoc_wide_2016_2023 %>%
  mutate(
    t02_01_w07 = case_when(
      t02_01_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE ~ t02_01_w07
    ),
    t02_02_w07 = case_when(
      t02_02_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE ~ t02_02_w07
    ),
    t02_03_w07 = case_when(
      t02_03_w07 %in% c(-999, -888) ~ NA_real_,
      TRUE ~ t02_03_w07
    ),
  )

# Revisamos el resultado para una de las variables
table(elsoc_wide_2016_2023$t02_01_w07, exclude = FALSE)


# Mutate 2 ----------------------------------------------------------------

# Calculamos el promedio de los tres ítems de identidad barrial

# Opción 1: promedio "a mano" (suma y división)
elsoc_wide_2016_2023 <- elsoc_wide_2016_2023 %>%
  mutate(
    media_identidad_barrio = (t02_01_w07 + t02_02_w07 + t02_03_w07) / 3
  )

# Opción 2: usando rowMeans()
elsoc_wide_2016_2023 <- elsoc_wide_2016_2023 %>%
  mutate(
    media_identidad_barrio = rowMeans(
      cbind(t02_01_w07, t02_02_w07, t02_03_w07),
      na.rm = TRUE
    )
  )

summary(elsoc_wide_2016_2023$media_identidad_barrio)


# Mutate 3 ----------------------------------------------------------------

# Clasificamos la edad en tres grupos etarios
elsoc_wide_2016_2023 <- elsoc_wide_2016_2023 %>%
  mutate(
    grupo_edad = case_when(
      m0_edad_w07 %in% 18:39 ~ "Adulto/a joven",
      m0_edad_w07 %in% 40:59 ~ "Adulto/a",
      m0_edad_w07 >= 60      ~ "Adulto/a mayor",
      TRUE                   ~ NA_character_
    )
  )

table(elsoc_wide_2016_2023$grupo_edad, exclude = FALSE)

########################################################################
