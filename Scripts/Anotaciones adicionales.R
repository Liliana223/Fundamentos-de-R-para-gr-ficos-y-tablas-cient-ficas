# Fundamentos de R para gráficos y tablas científicas
# Prof. Astrid Liliana Vargas Sanchez
# Anotaciones adicionales
# Tablas científicas y casos prácticos

# =========================
# Graficos en patchwork (del mismo tamaño)
# =========================

# Instalar si no lo tienes
# install.packages("patchwork")

library(ggplot2)
library(patchwork)

# Cargar datos
data(iris)

# Gráfica 1: Sepal Length vs Sepal Width
p1 <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  ggtitle("Sepal Length vs Sepal Width") +
  theme_minimal()

# Gráfica 2: Petal Length vs Petal Width
p2 <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  ggtitle("Petal Length vs Petal Width") +
  theme_minimal()

# Gráfica 3: Boxplot de Sepal Length por especie
p3 <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  ggtitle("Sepal Length por especie") +
  theme_minimal()

# Layout: 2 arriba, 1 abajo

(p1 | p2) / (p3 | plot_spacer())+
  plot_annotation(
    title = "Análisis exploratorio del dataset iris",
    subtitle = "Comparación de variables morfológicas por especie"
  )

# Layout: 2 arriba, 1 centrada abajo

(p1 | p2) /
  (plot_spacer() | p3 | plot_spacer())

# =========================
# Grafico con especies de leyenda en cursiva
# =========================

grafico1 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  scale_color_discrete(
    labels = c(
      expression(italic("setosa")), # Poner etiquetas en cursiva
      expression(italic("versicolor")),
      expression(italic("virginica"))
    )
  ) +
  theme_minimal()

grafico1 

# =========================
# Grafico con titulos y subtitulos en negrita y cursiva
# =========================

p1 +
  labs(
    title = "Análisis del dataset iris",
    subtitle = "Relación entre variables morfológicas",
    x = "Sepal Length",
    y = "Sepal Width"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),        # Título en negrita
    plot.subtitle = element_text(face = "plain"),    # Subtítulo normal
    axis.title.x = element_text(face = "italic"),    # Eje X en cursiva
    axis.title.y = element_text(face = "italic")     # Eje Y en cursiva
  )

# =========================
# Tabla con GT, Especies en cursiva
# =========================

library(dplyr)
library(gt)

# Resumen por especie
tabla <- iris %>%
  group_by(Species) %>%
  summarise(
    Sepal_Length = mean(Sepal.Length),
    Sepal_Width  = mean(Sepal.Width)
  )

# Crear tabla con gt (centrada)
tabla %>%
  gt() %>%
  tab_style( # aplica estilos
    style = cell_text(style = "italic"), # pone el texto en cursiva
    locations = cells_body(columns = Species) # solo afecta la columna de especies
  )

# Crear tabla con gt (alineada a la derecha)

tabla %>%
  mutate(Species = paste0("<i>", Species, "</i>")) %>%
  gt() %>%
  fmt_markdown(columns = Species)

# Crear tabla con gt (con encabezado)

tabla %>%
  gt() %>%
  tab_header(
    title = "Resumen del dataset iris",
    subtitle = "Promedio de variables por especie"
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = Species)
  )