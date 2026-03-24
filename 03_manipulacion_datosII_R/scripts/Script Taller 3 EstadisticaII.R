########################################################################/
# Script Taller 3 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 24-03-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquete a utilizar ------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("dplyr")

library(dplyr) #manipulación de datos, también pueden ocupar library(tidyverse)


# Cargar base de datos ----------------------------------------------------

load("data/Base Evaluaciones ELPI III_reducida.Rdata")


# Ocupemos %>% ------------------------------------------------------------

# Exploremos los datos 
data %>% glimpse() # Vista previa 
data %>% head()    # Primera 6 observaciones
data %>% colnames() # Nombres columnas/variables


# filter() ----------------------------------------------------------------

# Filtramos vectores a partir de una condición 
##Vamos a ocupar la variable sexo 
##pero tenemos que agregarle etiquetas a los atributos para facilitar el trabajo
table(data$sexo)
data$sexo <-  factor(data$sexo,
                     levels = c(1:2), #indicamos los niveles
                     labels = c("Hombre", "Mujer"))# les ponemos etiquetas

##Ahora hacemos los filtros
#Hombre
data_hombre <- data %>% 
  filter(sexo=="Hombre") 
dim(data_hombre)
table(data_hombre$sexo)

#Mujer
data_mujer <- data %>% 
  filter(sexo=="Mujer")
dim(data_mujer)
table(data_mujer$sexo)


# Generamos dos condiciones
summary(data$wm_pb_pa)
data_mat1 <- data %>% 
  filter(wm_pb_pa >=36 & wm_pb_fd==36)
dim(data_mat1)

summary(data$wm_pb_fd)
data_mat2 <- data %>% 
  filter(wm_pb_pa >=36 | wm_pb_fd==36)
dim(data_mat2)


# select() ----------------------------------------------------------------

# Seleccionamos columnas/variables
data1 <- data %>% 
  select(1:3)
data1 %>% colnames()

data2 <- data %>% 
  select("folio", "wm_pb_pa")
data2 %>% colnames()

data3 <- data %>% 
  select("folio", "wm_pb_pa":"wm_pb_cc")
data3 %>% colnames()

data4 <- data %>% 
  select(1,edad_mesesr,sexo,idregion,wm_pb_pa:wm_pb_cc)
data4 %>% colnames()

#Eliminamos una columna
data5 <- data %>% 
  select(-sexo)
data5 %>% colnames()


# arrange() ---------------------------------------------------------------

# Ordenamos la BBDD a partir de uno o más vectores
head(data4, n=15)

data_arrange1 <- data4 %>% 
  arrange(wm_pb_pa)  # Por defecto el orden es creciente
head(data_arrange1, n=15)

data_arrange2 <- data4 %>% 
  arrange(desc(wm_pb_pa)) # Decreciente
head(data_arrange2, n=15)

data_arrange3 <- data4 %>% 
  arrange(wm_pb_pa,wm_pb_fd)  # Utilizando dos variables
head(data_arrange3, n=15)
########################################################################
