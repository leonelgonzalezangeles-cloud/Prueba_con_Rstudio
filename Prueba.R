library(readxl)
library(writexl)
library(ggplot2)
library(dplyr)
library(stringr)
library(tidyr)

#------>Cargar el documento tipo xlsx<-------

#Se carga el documento "Base_para_prueba.xlsx" y se guarda en el objeto "datos"
datos <- read_excel("C:/Users/leo-1/Downloads/Base_para_prueba.xlsx")

View(datos)

#----->Eliminar Espacios en blanco<-----

#Columnas que requieren eliminar los espacios en blanco a un inicio y final
#Tambien espacios multiples en blanco

datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email, starts_with("asistencia")),
                ~str_squish(.)))

#Se eliminan todos los espacios entre sus caracteres de la columna "Email"
datos$Email <- gsub(" ", "", datos$Email)



#----->Eliminar caracteres erroneos y se cambia a formato Title Case<------

#Se limitan ciertos caracteres para eliminaran aquellos que se han mezclados entre los datos
# Como: * # $ % & = ? ! / \ , ; : " ' ( ) [ ] { }
datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email),
                ~str_replace_all(., "[\\*#\\$%&=\\?!/\\\\,;:\"'()\\[\\]{}]", "")))

# Se eliminarán los acentos de los nombre evitando haciendo exepciones con la pestaña de la"ñ"
datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email),
                ~ . %>%
                  str_replace_all("ñ", "_ene_") %>%
                  str_replace_all("Ñ", "_ENE_") %>%
                  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
                  str_replace_all("_ene_", "ñ") %>%
                  str_replace_all("_ENE_", "Ñ")
  ))


#Se cambia el formato a "Title Case" de las columnas que competen al nombre completo del usuario
datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`), str_to_title))




#------>Hallar inconsistencias en el nombre de los usuarios<---------

# Verificar si hay campos vacios en las columnas A:E
#  A ->Fuente
#  B ->Nombre
#  C ->Primer apellido
#  D ->Segundo apellido
#  E ->Email

#   ¿Hay campos vacios entre la columna A hasta la E?
any(is.na(datos[,1:5])) #False

#   ¿Cuantas celdas estan vacias?
sum(is.na(datos[,1:5]))#[1] 0

#  ¿Alguna celda tiene mas de una palabra en la columna "Primer apellido" o  "Segundo apellido"?
#  En caso de hallar este problema ¿Donde esta?
datos %>%
  select(`Primer apellido`, `Segundo apellido`) %>%
  filter(str_count(`Primer apellido`, "\\S+") > 1 |
           str_count(`Segundo apellido`, "\\S+") > 1)

#  Eliminar las letras solas entre las columnas de los apellidos
datos <- datos %>%
  mutate(
    `Primer apellido` = str_remove_all(`Primer apellido`, "\\b[A-Za-z]\\b"),
    `Segundo apellido` = str_remove_all(`Segundo apellido`, "\\b[A-Za-z]\\b")
  )

#  Quitar los espacios multiples en blanco y espacios en blanco a un inicio y final

datos <- datos %>%
  mutate(across(c(`Primer apellido`, `Segundo apellido`,),~str_squish(.)))


#Verificar si hay una inconsistencia entre las celdas correspondientes a nombre y apellidos
#concentrandose en la columna "Nombre"
# 1.-Verificar que no hayan valores repetidos en la celda "Nombre"
#    que esten en las columnas de "Primer apellido" y "Segundo apellido" 
#    Ej.  

#         Columna A: Nombre
#         Columna B: Primer apellido
#         Columna C: Segundo apellido


#         A2 Laura Gutierrez Salinas
#         B2 Gutierrez
#         C3 Salinas



#Se define la columna "nombre_original" para evitar modificar la columna "Nombre" por el momento.

#Se genera una columna llamada "apellidos_en_nombre" que será llenado con:
# False, si no hay valores identicos entre las columnas "Primer apellido" y "Segundo apellido" en la columna "Nombre".
# True, si hay valores identicos entre las columnas "Primer apellido" y "Segundo apellido" en la columna "Nombre"
datos <- datos %>%
  mutate(nombre_original = Nombre,
         apellidos_en_nombre =
           str_detect(nombre_original, fixed(`Primer apellido`)) |
           str_detect(nombre_original, fixed(`Segundo apellido`)))



#Se dependiendo de haberse hallado valores de apellidos similares en la columna "Nombres"
#Se distribuyen estos valores en las columnas de "Primer apellido" y "Segundo apellido"
datos <- datos %>%
  mutate(
    Nombre = ifelse(apellidos_en_nombre, word(nombre_original, 1), nombre_original),
    `Primer apellido` = ifelse(apellidos_en_nombre, word(nombre_original, 2), `Primer apellido`),
    `Segundo apellido` = ifelse(apellidos_en_nombre, word(nombre_original, 3), `Segundo apellido`)
  )




#------>Hallar valores repetidos<---------
# Puntos a tener en cuenta:
#     -Pueden haber nombres iguales pero no cuentas de correo iguales.
#     -El analisis de valores repetidos se hara a partir de los valores de "Email"
#     -Al hallar filas duplicadas, las asistencias se deben respetar y no eliminarse


# Se cambiaran los valores de NA o null por 0 en las columnas de asistencia
datos <- datos %>%
  mutate(across(starts_with("asistencia"),~ifelse(is.na(.), 0, .)))

# 
datos <- datos %>%
  unite("Nombre completo",
        Nombre,
        `Primer apellido`,
        `Segundo apellido`,
        sep = " ",
        remove = TRUE) #Elimina las columnas originales


# Se agrupan las filas conforme al dato de correo

datos_limpios <- datos %>%
  group_by(Email) %>% #agrupa por Email
  summarise(
    `Nombre completo` = first(`Nombre completo`), #Toma el primer dato de la agrupación
    asistencia_ses1 = max(asistencia_ses1),       #Tomara los valores mas altos
    asistencia_ses2 = max(asistencia_ses2),
    asistencia_ses3 = max(asistencia_ses3),
    asistencia_ses4 = max(asistencia_ses4),
    asistencia_ses5 = max(asistencia_ses5),
    .groups = "drop" #Elimina los grupos 
  )
View(datos_limpios)




#-------->Desempeño mas bajo<-----------
datos_limpios <- datos_limpios %>%
  mutate(across(starts_with("asistencia"), as.numeric))

datos_limpios <- datos_limpios %>%
  mutate(Desempeño = rowSums(across(starts_with("asistencia")))) #Suma los valores de las columnas de assistencias


datos_limpios %>%
  filter(Desempeño == min(desempeno))
