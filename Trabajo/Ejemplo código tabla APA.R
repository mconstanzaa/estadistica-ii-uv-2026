# Tabla APA ---------------------------------------------------------------
library(tidyverse)
library(openxlsx)

data <- readRDS("data/base_92_20112024.Rds")

vars_num <- c("democracia_19", "bienestar_2")
vars_cat <- c("sexo", "gse")


# Numéricas
desc_num <- data %>%
  summarise(across(all_of(vars_num),
                   list(media = \(x) mean(x, na.rm = TRUE),
                        sd    = \(x) sd(x, na.rm = TRUE),
                        n     = \(x) sum(!is.na(x))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to      = c("Variable", ".value"),
               names_pattern = "^(.+)_(media|sd|n)$") %>%
  mutate(`Media / %`  = sprintf("%.2f", media),
         `Desv. Est.` = sprintf("%.2f", sd),
         n            = as.integer(n)) %>%
  select(Variable, n, `Media / %`, `Desv. Est.`)

# Categóricas
desc_cat <- data %>%
  select(all_of(vars_cat)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Categoria") %>%
  filter(!is.na(Categoria)) %>%
  count(Variable, Categoria, name = "n") %>%
  group_by(Variable) %>%
  mutate(`Media / %`  = paste0(round(100 * n / sum(n), 2), "%"),
         `Desv. Est.` = "",
         Variable      = paste0(Variable, ": ", Categoria)) %>%
  select(Variable, n, `Media / %`, `Desv. Est.`)

tabla <- bind_rows(desc_num, desc_cat)

# Exportar con formato APA
ruta <- 'outputs/tabla_descriptivos_apa.xlsx'

wb <- createWorkbook()
addWorksheet(wb, "Tabla 1")

nf <- nrow(tabla)
nc <- ncol(tabla)

# Estilos
s_titulo <- createStyle(fontSize = 12, fontName = "Times New Roman",
                        textDecoration = "bold")
s_nota   <- createStyle(fontSize = 12, fontName = "Times New Roman",
                        textDecoration = "italic")
s_header <- createStyle(fontSize = 12, fontName = "Times New Roman",
                        textDecoration = "bold",
                        border = "TopBottom", borderStyle = c("medium", "thin"))
s_cuerpo <- createStyle(fontSize = 12, fontName = "Times New Roman")
s_cierre <- createStyle(fontSize = 12, fontName = "Times New Roman",
                        border = "Bottom", borderStyle = "medium")

# Título e indicación (filas 1-2)
writeData(wb, "Tabla 1", "Tabla 1",                                           startRow = 1)
writeData(wb, "Tabla 1", "Descriptivos de variables numéricas y categóricas", startRow = 2)
addStyle(wb, "Tabla 1", s_titulo, rows = 1, cols = 1)
addStyle(wb, "Tabla 1", s_nota,   rows = 2, cols = 1)

# Encabezado (fila 3)
writeData(wb, "Tabla 1", tabla, startRow = 3)
addStyle(wb, "Tabla 1", s_header, rows = 3,          cols = 1:nc, gridExpand = TRUE)

# Cuerpo (filas 4 a penúltima)
addStyle(wb, "Tabla 1", s_cuerpo, rows = 4:(nf + 2), cols = 1:nc, gridExpand = TRUE)

# Línea de cierre (última fila de datos)
addStyle(wb, "Tabla 1", s_cierre, rows = nf + 3,     cols = 1:nc, gridExpand = TRUE)

# Nota al pie
writeData(wb, "Tabla 1", "Nota. n = tamaño muestral por categoría o variable.",
          startRow = nf + 5)
addStyle(wb, "Tabla 1", s_nota, rows = nf + 5, cols = 1)

setColWidths(wb, "Tabla 1", cols = 1:nc, widths = "auto")
saveWorkbook(wb, ruta, overwrite = TRUE)
