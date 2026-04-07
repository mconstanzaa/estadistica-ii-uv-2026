########################################################################/
# Script Taller 5 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 06-04-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("tidyverse")

library(tidyverse) 


# Probabilidad empírica en R ----------------------------------------------

set.seed(2026)                                           # Fija la semilla para reproducibilidad
lanzamientos <- sample(c("cara", "sello"),               # Vector con los posibles resultados
                       size = 100,                       # Número de lanzamientos
                       replace = TRUE)                   # Con reemplazo (cada lanzamiento es independiente)

# Frecuencia observada
table(lanzamientos)

# Probabilidad empírica
prop.table(table(lanzamientos))


# Probabilidad condicional en R -------------------------------------------

set.seed(2026)
encuesta <- data.frame(
  sexo = sample(c("Mujer", "Hombre"), 1000, replace = TRUE),  # Sin prob → probabilidad igual (50/50)
  voto = sample(c("Candidato X", "Otro"), 1000, replace = TRUE,
                prob = c(0.55, 0.45))                          # Con prob → simula que X es más votado
)
tabla <- table(encuesta$sexo, encuesta$voto)                   # Tabla de contingencia sexo × voto
prop.table(tabla, margin = 1)                                  # Proporciones por fila (suma 1 en cada fila)


# Distribución normal en R ------------------------------------------------

tibble(x = seq(-4, 4, 0.01)) %>%        # Crea secuencia de valores Z entre -4 y 4
  mutate(densidad = dnorm(x,             # Calcula la densidad normal para cada valor de x
                          mean = 0,      # Media = 0 (distribución estándar)
                          sd = 1)) %>%   # Desviación estándar = 1
  ggplot(aes(x = x, y = densidad)) +    # Define los ejes del gráfico
  geom_line(color = "steelblue",        # Dibuja la curva
            linewidth = 1.2) +          # Grosor de la línea
  labs(title = "Distribución Normal Estándar (μ = 0, σ = 1)",
       x = "Valor Z", y = "Densidad") + # Títulos de ejes
  theme_minimal()                        # Estilo limpio sin fondo gris


# Puntaje Z en R ----------------------------------------------------------

# Ejemplo: ingreso mensual ~ Normal(μ = $650.000, σ = $400.000)
# ¿Cuál es el puntaje Z de una persona que gana $1.200.000?
mu <- 650000
sigma <- 400000
X <- 1200000

Z <- (X - mu) / sigma
Z

# Múltiples valores
casos <- tibble(
  variable  = c("Ingreso mensual", "Años de escolaridad", "Puntaje autoritarismo", "Horas de trabajo semanal"),
  X         = c(1200000,           8,                     65,                      38),
  mu        = c(650000,            11,                    50,                      45),
  sigma     = c(400000,            3,                     10,                      8)
)

casos <- casos %>%
  mutate(Z = (X - mu) / sigma)

casos


# Calcular probabilidades con `pnorm()` -----------------------------------

# P(Z < 1.375) → probabilidad de ganar menos de $1.200.000
pnorm(1.375)

# P(0 < Z < 1.375) → área entre la media y nuestro valor
pnorm(1.375) - pnorm(0)

# P(Z > 1.375) → probabilidad de ganar más de $1.200.000
pnorm(1.375, lower.tail = FALSE)

# P(ingreso entre $650.000 y $1.200.000) directamente
pnorm(1200000, mean = 650000, sd = 400000) - pnorm(650000, mean = 650000, sd = 400000)


# Prob. con valores Z negativos -------------------------------------------

# P(Z < -1) → probabilidad de tener menos de 8 años de escolaridad
pnorm(-1)

# P(-0.875 < Z < 1.375) → probabilidad de trabajar entre 38 y 50 horas semanales
pnorm(1.375) - pnorm(-0.875)

# P(Z > -0.875) → probabilidad de trabajar más de 38 horas semanales
pnorm(-0.875, lower.tail = FALSE)


# Hipótesis nula y alternativa --------------------------------------------


# Ejemplo 1: Género en cargos directivos (valor P > 0.05) -----------------

prop.test(x = 53, n = 120, p = 0.50, alternative = "two.sided")


# Visualizando valor p ----------------------------------------------------

# Calculamos el Z observado para nuestra proporción (44% vs 50% esperado)
z_obs <- (0.44 - 0.50) / sqrt(0.50 * 0.50 / 120)

tibble(x = seq(-4, 4, 0.01)) %>%
  mutate(y = dnorm(x)) %>%                              # Densidad normal estándar
  ggplot(aes(x = x, y = y)) +
  geom_line(color = "steelblue", linewidth = 1) +       # Curva normal
  geom_area(data = . %>% filter(x <= -abs(z_obs)),      # Área cola izquierda
            fill = "#E74C3C", alpha = 0.4) +
  geom_area(data = . %>% filter(x >= abs(z_obs)),       # Área cola derecha
            fill = "#E74C3C", alpha = 0.4) +
  geom_vline(xintercept = c(-abs(z_obs), abs(z_obs)),   # Líneas en Z observado
             linetype = "dashed", color = "#E74C3C") +
  labs(title = "Distribución normal bajo H₀ — Género en cargos directivos",
       subtitle = paste0("Área sombreada = valor P = ",
                         round(2 * pnorm(-abs(z_obs)), 3),  # Valor P de dos colas
                         " → No rechazamos H₀"),
       x = "Valor Z", y = "Densidad") +
  theme_minimal()


# Ejemplo 2: Pueblos originarios en el mercado laboral (valor P ≈  --------

prop.test(x = 18, n = 300, p = 0.128, alternative = "two.sided")


# Ejemplo 3: El nivel de significancia (α) --------------------------------

prop.test(x = 114, n = 200, p = 0.50, alternative = "two.sided")
########################################################################
