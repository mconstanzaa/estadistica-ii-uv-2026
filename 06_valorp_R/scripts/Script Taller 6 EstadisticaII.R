########################################################################/
# Script Taller 6 Estadística II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 14-04-2026
########################################################################/

rm(list=ls()) # Borramos todos los objetos que están cargados


# Paquetes ----------------------------------------------------------------

#Si no están descargados, instalamos los paquetes
#install.packages("tidyverse")

library(tidyverse)


# pnorm() y qnorm(): funciones inversas -----------------------------------

pnorm(1.96)   # → 0.975: el Z = 1.96 deja 97.5% del área a su izquierda
qnorm(0.975)  # → 1.96:  el área 97.5% corresponde al Z = 1.96


# Valores Z críticos con qnorm() ------------------------------------------

qnorm(0.950)   # α = 0.10 → cada cola = 5.0% → área izquierda = 95.0%
qnorm(0.975)   # α = 0.05 → cada cola = 2.5% → área izquierda = 97.5%
qnorm(0.995)   # α = 0.01 → cada cola = 0.5% → área izquierda = 99.5%


# Ejemplo 1: Género en cargos directivos (valor P > 0.05) -----------------

prop.test(x = 53,               # número de casos que cumplen la condición
          n = 120,              # total de casos en la muestra
          p = 0.50,             # proporción esperada bajo H₀
          alternative = "two.sided") # Hₐ: p ≠ 0.50


# Visualizando el valor P -------------------------------------------------

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


# Ejemplo 2: Pueblos originarios en el mercado laboral (valor P ≈ 0) ------

prop.test(x = 18,               # número de casos que cumplen la condición
          n = 300,              # total de casos en la muestra
          p = 0.128,            # proporción esperada bajo H₀ (Censo 2017)
          alternative = "two.sided") # Hₐ: p ≠ 0.128


# Ejemplo 3: El nivel de significancia (α) --------------------------------

prop.test(x = 114,              # número de casos que cumplen la condición
          n = 200,              # total de casos en la muestra
          p = 0.50,             # proporción esperada bajo H₀
          alternative = "two.sided") # Hₐ: p ≠ 0.50

########################################################################