########################################################################/
# Script Taller 4 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 31-03-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("dplyr")

library(dplyr) #manipulación de datos, también pueden ocupar library(tidyverse)


# Base de datos -----------------------------------------------------------

load("data/Base Evaluaciones ELPI III_reducida.Rdata")


# Exploración base de datos -----------------------------------------------

data %>% glimpse() # Vista previa 
data %>% head()    # Primera 6 observaciones
data %>% colnames() # Nombres columnas/variables


# Mutate ------------------------------------------------------------------


# Operaciones aritméticas -------------------------------------------------

# Suma
data <- data %>%
  mutate(suma_matematica = wm_pb_pa + wm_pb_fd + wm_pb_cc)

summary(data$suma_matematica)

# Promedio
data <- data %>%
  mutate(media_matematica = (wm_pb_pa + wm_pb_fd + wm_pb_cc) / 3)

summary(data$media_matematica)


# Valores perdidos --------------------------------------------------------

# na.rm = FALSE → si hay NA, el resultado es NA (comportamiento por defecto)
data <- data %>% 
  mutate(suma_mat_naF = rowSums(cbind(wm_pb_pa, wm_pb_fd, wm_pb_cc), 
                                na.rm = FALSE))

# na.rm = TRUE → suma lo disponible aunque falten datos
data <- data %>% 
  mutate(suma_mat_naT = rowSums(cbind(wm_pb_pa, wm_pb_fd, wm_pb_cc), 
                                na.rm = TRUE))

summary(data$suma_mat_naF)
summary(data$suma_mat_naT)


# Recodificar valores perdidos --------------------------------------------

data <- data %>%
  mutate(sexo = case_when(
    sexo == -999 ~ NA_real_,
    sexo == -888 ~ NA_real_,
    TRUE         ~ as.numeric(sexo)
  ))

table(data$sexo, exclude = FALSE)


# Recodificar una variable numérica en grupos -----------------------------

# Clasificamos la edad del niño en valores
summary(data$edad_mesesr)

data <- data %>% 
  mutate(grupo_edad = case_when(
    edad_mesesr < 72  ~ 1,
    edad_mesesr >= 72 ~ 2,
    TRUE              ~ NA_real_
  ))

table(data$grupo_edad, exclude = FALSE)


# Recodificar en múltiples categorías -------------------------------------

table(data$idregion)

data <- data %>%
  mutate(zona = case_when(
    idregion == 13             ~ "Metropolitana",
    idregion %in% c(1:4, 15)  ~ "Norte",
    idregion %in% c(5:7)      ~ "Centro",
    idregion %in% c(8:12, 14) ~ "Sur",
    TRUE                       ~ NA_character_
  ))

table(data$zona, exclude = FALSE)
########################################################################
