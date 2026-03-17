########################################################################/
# Script Taller 2 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 17-03-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquete a utilizar ------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("dplyr")

library(dplyr) #manipulación de datos, también pueden ocupar library(tidyverse)


# Trabajo con bases de datos ----------------------------------------------

head(mtcars) # Primeras 6 observaciones

mtcars[,2] #Valores de la variable en la columna 2
mtcars[2,] #Valores del caso 2 (fila 2)
mtcars[2,2] #Valor del caso 2 en la columna 2

mtcars$cyl #Valores de la variable "cyl" 


# Importar datos en R -----------------------------------------------------

load("data/Base Evaluaciones ELPI III_reducida.Rdata")


# Explorar la base de datos -----------------------------------------------

# Podemos ver los componentes del objeto 
dim(data)   # Observaciones y variables
names(data) # Nombre de nuestras variables 
str(data)   # Visor de nuestras variables
head(data) # Primeras 6 observaciones
tail(data) # Últimas 6 observaciones

# Lógica fila, columna
data[1] # primera columna del data frame
data[1,] #primera fila
data[,1] #primera columna
data[1,1] #elemento ubicado en la primera fila y primera columna
data[1:6,1:6] #filas de la 1 a la 6 y las columnas de la 1 a la 6
data[,c("sexo","nivel_edu_niño")] #todas las filas de las columnas


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
