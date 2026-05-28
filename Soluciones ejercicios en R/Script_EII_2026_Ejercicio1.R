########################################################################/
# Script Solución Taller 2 Estadística II ----------------------------------
# M. Constanza Ayala (maria.ayala@uv.cl)
# 17-03-2026
########################################################################/

# Paquetes ----------------------------------------------------------------

library(dplyr)


# Base de datos -----------------------------------------------------------

load("data/ELSOC_Wide_2016_2023.RData")


# Exploración base de datos -----------------------------------------------

dim(elsoc_wide_2016_2023)     # Número de observaciones y variables
names(elsoc_wide_2016_2023)   # Nombre de las variables
str(elsoc_wide_2016_2023)     # Estructura y tipo de variables
head(elsoc_wide_2016_2023)    # Primeras 6 observaciones
tail(elsoc_wide_2016_2023)    # Últimas 6 observaciones

# Lógica fila, columna
elsoc_wide_2016_2023[, 2]     # Valores de la variable en la columna 2
elsoc_wide_2016_2023[2, ]     # Valores del caso 2 (fila 2)
elsoc_wide_2016_2023[2, 2]    # Valor del caso 2 en la columna 2

########################################################################/
