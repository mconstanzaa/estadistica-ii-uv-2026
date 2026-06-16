########################################################################/
# Script Taller 12 Estadística II -------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 16-06-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados
options(scipen=999) # Desactivamos notación científica


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("sjPlot")
#install.packages("DescTools")

library(tidyverse)  # Manipulación de datos y gráficos
library(haven)      # Importar bases de datos
library(sjPlot)     # Tablas de contingencia
library(DescTools)  # V de Cramer


# Base de datos -----------------------------------------------------------

data <- readRDS("data/base_92_20112024.Rds")

data %>% glimpse()


# Preparación: recodificar valores perdidos y etiquetar ------------------

# Valores no válidos: -8 (no sabe) y -9 (no contesta)
data <- data %>%
  mutate(mtf_45_h = case_when(
    mtf_45_h %in% c(-8, -9) ~ NA_real_,
    TRUE ~ mtf_45_h
  ))

# Etiquetas de las categorías
data$mtf_45_h <- factor(data$mtf_45_h,
                        levels = c(1:5),
                        labels = c("Muy de acuerdo",
                                   "De acuerdo",
                                   "Ni de acuerdo ni en desacuerdo",
                                   "En desacuerdo",
                                   "Muy en desacuerdo"))

data$sexo <- factor(data$sexo,
                    levels = c(1:2),
                    labels = c("Hombre", "Mujer"))


# Verificación de NA y reducción de la base -------------------------------

sum(is.na(data$mtf_45_h))
sum(is.na(data$sexo))

data <- data %>%
  drop_na(mtf_45_h)

dim(data) # Verificamos el tamaño resultante


# Descriptivos univariados ------------------------------------------------

# Variable sexo
data %>%
  group_by(Sexo = sexo) %>%
  summarise(
    N          = n(),
    Porcentaje = round(100 * n() / nrow(data), 1)
  )

# Variable homoparentalidad
data %>%
  group_by(Homoparentalidad = mtf_45_h) %>%
  summarise(
    N          = n(),
    Porcentaje = round(100 * n() / nrow(data), 1)
  )


# Tabla de contingencia ---------------------------------------------------

sjt.xtab(data$mtf_45_h, data$sexo,
         show.col.prc = TRUE,   # Porcentajes por columna, para filas ocupar show.row.prc
         digits       = 2,
         var.labels   = c("Actitud frente a la homoparentalidad", "Sexo"))


# Chi-cuadrado ------------------------------------------------------------

# Verificamos el supuesto: frecuencias esperadas >= 5 en al menos el 80% de las celdas
chisq.test(data$mtf_45_h, data$sexo)$expected

# Aplicamos el test
chisq.test(data$mtf_45_h, data$sexo)


# V de Cramer -------------------------------------------------------------

# Creamos la tabla de contingencia como objeto
tabla <- table(data$mtf_45_h, data$sexo)

# Calculamos V de Cramer con el paquete DescTools
CramerV(tabla)


# Residuos tipificados corregidos -----------------------------------------

chi <- chisq.test(tabla)  # Guardamos el resultado del test

# Extraemos los residuos tipificados corregidos
chi$stdres
########################################################################
