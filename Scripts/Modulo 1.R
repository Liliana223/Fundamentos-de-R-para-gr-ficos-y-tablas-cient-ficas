# Fundamentos de R para gráficos y tablas científicas
# Prof. Astrid Liliana Vargas Sanchez
# Modulo 1
# Introducción a R y manipulaciòn de datos con dplyr

# -------------------------------
# Asignación de variables
# -------------------------------
valor_a <- 52 # Se sugiere no utilizar caracteres especiales, espacios, ni iniciar con numeros

# Evitar usar nombres de funciones o palabras reservadas. No uses nombres como mean, sum, data, etc., 
# para evitar sobrescribir funciones de R
# Puedes usar gion bajo en vez de espacio. Ejemplo: nombre_estudiante
# Puedes usar una combinación de mayusculas con minusculas. Ejemplo: nombreEstudiante
# Evita usar letras como c para variables, porque: c() es una función base de R que se usa para crear vectores:

valor_b = 45
print(valor_b) # Imprimir por pantalla el valor de la variable

56.8 -> valor_B # Es importante diferenciar entre mayusculas o minusculas
print(valor_B)   

# Usa letras minúsculas por consistencia. No es obligatorio, pero facilita la lectura si mantienes un estilo 
# coherente:

total_ventas <- 5480   # mejor que TotalVentas o TOTALVENTAS
      
valor_c<-2L # Numeros enteros. Se puede crear variables sin dejar espacios sin embargo se recomienda dejar espacios
print(valor_c)  # por legibilidad y estilo profesional

      
texto <- "Hola mundo" # Se sugiere usar nombres que indiquen claramente el contenido de la variable
print(texto)

logico <- TRUE
print(logico)

# -------------------------------
# Verificar el tipo de variable
# -------------------------------
print(class(valor_a))       # "numeric"
print(class(valor_b))       # "numeric"
print(class(valor_c))       # "integer"
print(class(texto))   # "character"
print(class(logico))  # "logical"

# -------------------------------
# Operadores aritméticos
# -------------------------------
suma <- valor_a + valor_b
resta <- valor_a - valor_c
producto <- valor_b * valor_b
division <- valor_b / valor_b
potencia <- valor_a^valor_b
modulo <- valor_b %% valor_c     # Residuo

# Mostrar resultados
print(paste("Suma:", suma)) # paste: unir (concatenar) varios elementos en una sola cadena de texto.
print(paste("Producto:", producto))

# -------------------------------
# Operadores comparativos
# -------------------------------
print(valor_a > valor_b)     # TRUE
print(valor_a == valor_b)    # FALSE

# -------------------------------
# Operadores lógicos
# -------------------------------
valor_x <- TRUE

valor_y <- FALSE

print(valor_x & valor_y)     # FALSE 
print(valor_x | valor_y)     # TRUE 

# ----------------------------
# Estructuras de Datos en R
# ----------------------------

# 1. VECTORES (con c())
vector_numerico <- c(5, 1, 8, 2, 14)

print(vector_numerico)

vector_caracter <- c("manzana", "pera", "banano")

print(vector_caracter)

vector_logico <- c(TRUE, FALSE, TRUE)

print(vector_logico)

# 1.1 Índices con corchetes. 1 DIMENSIÓN. 1 ÍNDICE 

vector_numerico[1]    # Resultado: 3
vector_caracter[3]    # Resultado: banano
vector_logico[1:3]    # Resultado: TRUE FALSE  TRUE (del primero al tercero)
vector_numerico[c(1, 5)]  # Resultado: 5 14 (elementos 1 y 5)


# 1.2. FACTORES (datos categóricos)

colores <- c("rojo", "azul", "verde", "rojo", "azul") # Crear un vector normal
colores <- factor(c("rojo", "azul", "verde", "rojo", "azul")) # Crear categorias
print(colores)
print(levels(colores)) #Niveles del factor o categorias

# 2. MATRICES (con matrix())
matriz <- matrix(1:9, nrow = 3, ncol = 3)
print(matriz)

# 2.1 Índices: [fila, columna] 2 DIMENSIONES. 2 ÍNDICES

matriz[1, 1]  # Resultado:1 (fila 1, columna 1)
matriz[2, 3]  # Resultado:8 (fila 2, columna 3)
matriz[ ,2]   # Resultado: toda la columna 2 → 4 5 6
matriz[3, ]   # Resultado: toda la fila 3 → 3 6 9

# 3. DATA FRAMES (tablas de datos)
df <- data.frame(
  nombre = c("Julieta", "Edgar", "Camilo"),
  edad = c(42, 34, 21),
  aprobado = c(TRUE, TRUE, FALSE)
)

print(df)

# 3.1 Índices: [fila, columna]

df[1, 2]  # Resultado: 42 (fila 1, columna 2: edad)
df[2, ]   # Resultado: segunda fila completa
df[ ,3]   # Resultado: tercera columna

# 3.2 Por nombre de columna

df$nombre        # Accede a la columna "nombre"
df$edad[1]       # Edad de la primera fila → 42
df[["aprobado"]] # Accede a la columna "aprobado"

# 3.3 Con subset() para condiciones:

subset(df, edad > 25)   # Filas con edad mayor a 25
df[df$aprobado == TRUE, ]  # Filas con aprobado TRUE


# 4. LISTAS (pueden contener diferentes tipos de datos)
lista <- list(nombre = "Karen", edad = 26, casada = FALSE, notas = c(4.5, 3.8,5.0))
print(lista)

# Crear una lista con varios tipos de datos

# a. Crear cada estructura individual
variable <- 12

vector <- c("A", "B", "C", "D")

matriz <- matrix(1:6, nrow = 2, ncol = 3)

df <- data.frame(
  nombre = c("Maria", "Carlos"),
  edad = c(20, 23)
)

# b. Crear la lista con todos los elementos
mi_lista <- list(
  numero = variable,
  letras = vector,
  numeros_matriz = matriz,
  personas = df
)

# Ver la lista completa
print(mi_lista)

# -------------------------------
# Funciones básicas
# -------------------------------
vector1 <- c(12, 5, 3, 10)
print(vector1)

print(length(vector1))          # Número de elementos
print(sum(vector1))             # Suma de elementos
print(sort(vector1))            # Ordena los elementos

# -------------------------------
# Conjuntos de datos
# -------------------------------
                                                                  
# Usamos el dataset iris que ya se encuentra en R
data(iris) 

# -------------------------------
# 1. Cargar datasets
# -------------------------------

# Establecer la libreria de trabajo
setwd("E:\\Documentos\\Lili\\Proyectos\\Fundamentos de R para gráficos y tablas científicas\\Scripts")

# Leer el archivo con read.csv (Base R)
dataset_iris_base <- read.csv("iris_extended.csv")

# Leer un dataset con la libreria readr

# Primero, asegúrate de tener el paquete instalado
# install.packages("readr")

# Cargar la libreria

library(tidyverse) # Todos los paquetes
library(readr)

# Usa read_table() para leer un archivo TXT
archivotxt <- read_table("datos_pacientes.txt")

# Verifica los primeros registros
head(archivotxt)

# Usa read_table() para leer un archivo TSV
archivotsv <- read_tsv("ejemplo_datos.tsv")

# Verifica los primeros registros
head(archivotsv)

# Usa read_csv() para leer un archivo CSV
datos <- read_csv("iris_extended.csv")

# Verifica los primeros registros
head(datos)

# Guardar un CSV en R 

write_csv(datos, "datos.csv")

# Usa read_excel para leer un archivo Excel

# Primero, asegúrate de tener el paquete instalado
# install.packages("readxl")

library(readxl)

datos_excel <- read_excel("archivo_excel.xlsx")

# =========================
# Uso de funciones principales de tidyr y dplyr
# =========================

# =========================
# Cargar librerías y los datos
# =========================
library(tidyr)
library(dplyr)
# Usa read_csv() para leer un archivo CSV
datos <- read_csv("iris_extended.csv")

# =========================
# Vista inicial
# =========================
head(datos)

# =========================
# pivot_longer()
# Convertir de formato ancho a largo
# =========================

datos_largos <- datos %>% #Opreador %>%. El pipe (%>%) permite encadenar funciones y leer el código como una secuencia de pasos.
  pivot_longer(
    cols = c(longitud_sepalo, ancho_sepalo, longitud_petalo, ancho_petalo),
    names_to = "variable",
    values_to = "valor"
  )

head(datos_largos)

# Ahora tienes una columna "variable" y otra "valor"

# =========================
# pivot_wider()
# Volver de formato largo a ancho
# =========================

datos_anchos <- datos_largos %>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )

head(datos_anchos)

# =========================
# separate()
# Separar una columna en varias
# =========================

# Ejemplo: crear una columna combinada
datos_sep <- datos %>%
  mutate(combinado = paste(longitud_sepalo, ancho_sepalo, sep = "_"))

# Separarla nuevamente
datos_sep <- datos_sep %>%
  separate(combinado, into = c("long_sep", "ancho_sep"), sep = "_")

head(datos_sep)

# =========================
# unite()
# Unir columnas
# =========================

datos_unidos <- datos %>%
  unite(
    "sepalo", #nombre de la nueva columna
    longitud_sepalo, ancho_sepalo, #columnas que se van a unir
    sep = "_" #separador entre valores
  )

head(datos_unidos)


# -------------------------------
# Limpieza de datos
# -------------------------------
# =========================
# Cargar librerías
# =========================
library(readr)
library(dplyr)
library(tidyr)

# =========================
# Cargar datos
# =========================
datos <- read_csv("iris_extended.csv")

# =========================
#  Explorar el dataset
# =========================
head(datos)      # ver primeras filas
str(datos)       # estructura

# =========================
# Revisar valores faltantes NA
# =========================

colSums(is.na(datos))

# =========================
#  Eliminar valores faltantes (opción 1). Estamos eliminando todas las filas que tienen al menos un valor faltante (NA).
# =========================

datos_limpios <- datos %>%
  drop_na()

# =========================
# Reemplazar valores faltantes (opción 2). En lugar de eliminar filas, estamos rellenando los valores faltantes (NA) con un valor específico.
# =========================

datos <- datos %>% 
  replace_na(list(
    area_petalo = 0,
    area_sepalo = 0
  ))

# =========================
# fill()
# Rellenar valores faltantes
# =========================

# Ejemplo (simulado)
datos_fill <- datos %>%
  fill(tipo_suelo, .direction = "down")

# =========================
#  Verificar duplicados. Estamos identificando filas repetidas en el dataset.
# =========================

duplicados <- duplicated(datos)
sum(duplicados) # Cuenta cuántos TRUE hay. Es decir: cuántas filas duplicadas existen

# Eliminar duplicados
datos <- datos %>%
  distinct() # Elimina filas completamente iguales


# =========================
# Filtrar datos erróneos
# =========================

# Ejemplo: eliminar valores negativos
datos <- datos %>%
  filter(
    longitud_sepalo > 0,
    ancho_sepalo > 0,
    longitud_petalo > 0,
    ancho_petalo > 0
  )

# =========================
# Crear nuevas variables (opcional)
# =========================

datos <- datos %>%
  mutate(
    relacion_area = area_sepalo...19 / area_petalo...20
    
  )

# =========================
# Convertir variables si es necesario
# =========================
datos <- datos %>%
  mutate(
    especie = as.factor(especie),
    tipo_suelo = as.factor(tipo_suelo)
  )

# =========================
# Seleccionar columnas relevantes
# =========================

datos <- datos %>%
  select(
    especie, tipo_suelo,
    longitud_sepalo, ancho_sepalo,
    longitud_petalo, ancho_petalo,
    relacion_area
  )

# =========================
#  Verificación final
# =========================

summary(datos)
head(datos)

# =========================
# Guardar datos limpios
# =========================

write_csv(datos, "datos_limpios.csv")
