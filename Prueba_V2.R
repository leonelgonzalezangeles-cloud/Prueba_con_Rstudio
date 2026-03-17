library(readxl)  #Se carga para leer archivos .xlsx
library(writexl)
library(dplyr)
library(stringr)
library(tidyr) 
library(modeest) #Se carga para aplicar la funcion moda (mfv)



#---------------->Cargar el documento tipo xlsx<----------------

#  Se carga el documento "Base_para_prueba.xlsx" y se guarda en el objeto "datos"
datos <- read_excel("C:/Users/leo-1/Downloads/Base_para_prueba.xlsx")
View(datos)       #Se muestra "datos"

#  Se imprime el tamaño del dataframe
print(paste("Cantidad de filas: ", nrow(datos), ", Cantidad de columnas: ", ncol(datos) ))


#---------------->Eliminar Espacios en blanco<----------------

#  Todas las columnas requieren eliminar los espacios en blanco a un inicio y al final.
#  Tambien cambiar los espacios multiples entre los caracteres por uno solo


#  Se emplea la funcion "str_squish" para eliminar los espacios
datos <- datos %>%     
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email, starts_with("asistencia")),function(.)str_squish(.)))

#  Los datos de "Email" no requieren ningun espacio en blanco, por lo que serán eliminados
datos$Email <- gsub(" ", "", datos$Email)



#---------------->Eliminar caracteres erroneos<----------------

#  Se limitan ciertos caracteres para eliminaran aquellos que se han mezclados entre los datos
#  Simbolo como: * # $ % & = ? ! / \ , ; : " ' ( ) [ ] { }
datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email),function(.)str_replace_all(., "[\\*#\\$%&=\\?!/\\\\,;:\"'()\\[\\]{}]", "")))

#Todo el texto estara en minusculas
datos <- datos %>%
mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`,Email), str_to_lower)) # Se cambian todas las letras a minusculas en las columnas relacionadas


#  Se eliminarán los acentos de los nombre haciendo excepciones con la pestaña de la"ñ",
#  cambiando el formato donde solo existan letras de A-Z,sin caracteres especiales como: ´~¨ 

                
datos <- datos %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`, Email),
    function(.) 
      str_replace_all(
        iconv(                                             #cambia la codificacion de UTF-8 a ASCII//TRANSLIT
          str_replace_all(., "ñ", "_ene_"),                #Mascara para convertir ñ en "_ene_" 
          from = "UTF-8",
          to = "ASCII//TRANSLIT"                           #Formato al que se cambiara
        ),
        "_ene_",
        "ñ"
      )
  ))

#  Hay celdas vacis en la parte de asistencia por lo que se deben modificar por un valor que se pued atrabajar.
#  Por esta razopn se han cambiado a 0 los valores null o NA
#  Asi mismo, se cambia formato numerico las celdas de asistencia
datos <- datos %>%
  mutate(across(starts_with("asistencia"), ~replace_na(as.numeric(.), 0)))
    
#---------------->Hallar inconsistencias en el nombre de los usuarios<----------------

#  Verificar si hay campos vacios en las columnas A:E
#    A ->Fuente
#    B ->Nombre
#    C ->Primer apellido
#    D ->Segundo apellido
#    E ->Email

 if(any(is.na(datos[,1:5]))){                                             #¿Hay campos vacios entre la columna A hasta la E?
   print(paste("Hay",sum(is.na(datos[,1:5])), "celdas vacias"))            #Si hay celdas vacias, cuentalas
   } else {
    print("No hay celdas en blanco")                                     #De no haber, imprime mensaje "No hay valores repetidos" 
  }



#   Verificar si hay una inconsistencia entre las celdas correspondientes a nombre y apellidos
#   concentrandose en la columna "Nombre"

#    1.-Verificar que no hayan valores repetidos en la celda "Nombre"
#       que esten en las columnas de "Primer apellido" y "Segundo apellido" 
#       Ej.  

#         Columna A: Nombre
#         Columna B: Primer apellido
#         Columna C: Segundo apellido


#         A2 Laura Gutierrez Salinas
#         B2 Gutierrez
#         C3 Salinas


#  Se intenta verificar que no hayna valores inmersos de apellidos en la columna de Nombre.
#  Al usar str_detect se pretende tener una salida que dirá:
#     --False, si no hay valores identicos entre las columnas "Primer apellido" y "Segundo apellido" en la columna "Nombre".
#     --True, si hay valores identicos entre las columnas "Primer apellido" y "Segundo apellido" en la columna "Nombre"
#  A partir de esto, se podrá tomar accion eliminando los apellidos que esten inmersos en "Nombre" 
datos <- datos %>%
  mutate(Nombre= ifelse(str_detect(Nombre, fixed(`Primer apellido`)),str_remove(datos$Nombre, `Primer apellido`), Nombre))
  
          
datos <- datos %>%
  mutate(Nombre= ifelse(str_detect(Nombre, fixed(`Segundo apellido`)),str_remove(datos$Nombre, `Segundo apellido`), Nombre)          )


#  Al haberse eliminado ciertos caracetres de la columna Nombre, ahora se requiere eliminar los espacios en blanco
datos <- datos %>%     
  mutate(Nombre = str_squish(Nombre)) #str:squish elimina espacios al inicio, final y espacios multiples


#  Contar las palabras que hay en apellidos y si solo hay un caracter (Ej. X) o se halla un "Sin apellido".
#  Se debera intercambiar el segundo apellido por el primer apellido. Ya que no puede haber un nombre sin primer apellido.
#  Para el caso de hallar un solo caracter de un apellido, este se puede tomar como un abreviatura y se debe añadir un "." despues del caracter
#  Ej. 
#      Ramirez  -> R.
#      Ruiz -> R.

#Resguardo será la columna que guardara el valor de `Primer apellido` porque este dato se ocupara para el intercambio de posiciones entre Segundo y Primer apellido`

datos <- datos %>%
  mutate(
    `Primer apellido` = ifelse(str_length(`Primer apellido`) == 1 & str_detect(`Primer apellido`, "[a-z]"),    #Se evalua que haya solo un dato en la columna `Primer apellido` y qeu se trate de una letra
      paste(`Primer apellido`, ".", sep = ""),         #Si es TRUE, agrega un "." despues del caracter encontrado           
      `Primer apellido`                                #Si es FALSE, no hacer nada, solo respetar el mismo valor de Primer apellido
                              ),
    `Segundo apellido` = ifelse(str_length(`Segundo apellido`) == 1 & str_detect(`Segundo apellido`, "[a-z]"),    #Se evalua que haya solo un dato en la columna `Segundo apellido` y que se trate de una letra
                               paste(`Segundo apellido`, ".", sep = ""),         #Si es TRUE, agrega un "." despues del caracter encontrado           
                               `Segundo apellido`                                #Si es FALSE, no hacer nada, solo respetar el mismo valor de Segundo apellido
    )
    
         )


#  Se hara una validacion para determinar si hay letras antes y despues de cada palabra,
#  evitando caracterestes como "X." que puedan ser una abreviacion del apellido.
#  Ej.
#     Columna C: Primer apellido       Columna D: Segundo apellido

#     Columna C: Cuevas Y              Columna D: Sandoval     ->     Columna C: Cuevas              Columna D: Sandoval
#     Columna C: Gonzalez              Columna D: Y ac         ->     Columna C: Gonzalez              Columna D: Ac
#     Columna C: X.                    Columna D: Martin       ->     Columna C: X.                    Columna D: Martin

datos <- datos %>%
  mutate(across(
    c(Nombre, `Primer apellido`, `Segundo apellido`),
    ~ str_remove_all(., "\\b[A-Za-z]\\b(?!\\.)")
  ))




#---------------->Hallar valores repetidos<----------------
#---------------->Hacer homogeneas las asistencias<----------------
#  Puntos a tener en cuenta:
#     1.- Pueden haber nombres iguales pero no cuentas de correo iguales.

#     2.- Pueden haber filas con correos iguales pero celdas con valores erroneos en apellidos o nombre
#         Ej.
#           Email: luisangeleshernandez@gmail.com   Nombre: Luis   Primer apellido: Angeles
#           Email: luisangeleshernandez@gmail.com   Nombre: Luis   Primer apellido: Angel
#
#         De aqui la pregunta ¿Qué dato se debe considerar para eliminar valores repetidos?
#          -El correo nos proporciona informacion adicional para reconocer el valor correcto.
#          -Este mismo error puede verse involucrado entre las columnas "Nombre", "Primer apellido" y "Segundo apellido"

#     3.- Para validar los datos repetidos se deben homogenizar los grupos de filas para poder homogenizar las asistencias
#         Se tomarán los valores mas altos para cada sesión en cada agrupacion.
#         Ej.
#           --Sin homogenizar:
#
#           Asistencia_ses1 = 1    Asistencia_ses2 = 0   Asistencia_ses3 = 0 
#           Asistencia_ses1 = 0    Asistencia_ses2 = 0   Asistencia_ses3 = 1

#           --Homogenizados:
#
#           Asistencia_ses1 = 1    Asistencia_ses2 = 0   Asistencia_ses3 = 1  
#           Asistencia_ses1 = 1    Asistencia_ses2 = 0   Asistencia_ses3 = 1

#  Se haran 4 filtros para validación de datos duplicados:

#  1er filtro: 
#         Verificar datos coincidentes respecto de las columnas: "Nombre", "Primer apellido", "Segundo apellido" y "Email".
#         Tambien, se haran homogeneos a la par los registros de asistencias.          

#         En esta etapa se eliminaran las filas repetidas que coincidan completamente con las siguientes columnas:
#         Columna B: Nombre   
#         Columna C: Primer apellido    
#         Columna D: Segundo apellido
#         Columna E: Email    

  
filtro <- datos            #Se cargará el dataframe "datos" en uno nuevo llamado "filtro"
View(filtro)               #Se muestra el dataframe "filtro"


filtro <- filtro %>%
  group_by(Nombre, `Primer apellido`, `Segundo apellido`, Email) %>%     #Primera agrupación con todas las columnas involucradas
  mutate(across(starts_with("asistencia"), ~max(.x, na.rm = TRUE))) %>%  #Se hacen hmogeneos los valores de asistencia
  ungroup()                                                              #Se elimina la agrupácion

#        Se eliminan las filas repetidas  
filtro <- distinct(filtro, Nombre, `Primer apellido`, `Segundo apellido`, Email, .keep_all = TRUE)

#        ¿Cuantas filas se eliminaron?
x <- (nrow(datos)-nrow(filtro))
print(paste("Cantidad de filas eliminadas por estar repetidas :", x ))




#  2do filtro: 
#         Verificar datos coincidentes respecto de las columnas: Primer apellido", "Segundo apellido" y "Email".

#         En esta etapa se agruparan y posteriormente se eliminaran las filas repetidas que coincidan completamente con las siguientes columnas:
#         Columna C: Primer apellido    
#         Columna D: Segundo apellido
#         Columna E: Email 

#         Se excluyen en la agrupacion a la columna B (Nombre) por sospecha de hallar datos heterogeneos en la columna
#         que impida agrupar y asi unificar el registro del usuario

#         Pueden haber dos posibilidades para la busqueda de homogenizar las filas agrupadas:

#         A) Posibilidad de HALLAR un valor moda de la columna "Nombre" en la grupacion:
#            Ej. La moda de Nombre es: oscar 
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: julian   Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com

#         B) Posibilidad de NO HALLAR un valor moda de la columna "Nombre" en la agrupacion:
#            Ej. No existe moda en Nombre. 
#              Nombre: ocar     Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: julian   Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com

#         En el caso para el inciso A, se tomara como valor para unificar el grupo a la moda encontrada
#         En el caso para el inciso B, se debera acudir a hacer una extraccion del nombre en el correo,
#         porque se conoce quela posicion del nombre esta justo antes del de "Primer apellido.
#         Ya que se teme a que todos los datos esten erroneos, incompletos o que uno sea el indicado pero dentro del
#         proceso se tomarian caracteres que aseguren mas el objetivo de unificar el registro delusuario


filtro <- filtro %>%
  group_by(`Primer apellido`, `Segundo apellido`, Email) %>%     #Se agrupa  excluyendo "Nombre"
  summarise(                                                     #Summarise elimina filas repetidas
    Nombre = if(length(mfv(Nombre)) > 1){                        #Si la moda (mfv) nos regresa dos valores quiere decir que hay un empate
      str_extract(first(Email), paste0(".*(?=", first(`Primer apellido`), ")")) #Si hay un empate, se requiere extraer el dato de nombre del Email
    } else {
      mfv(Nombre)                                                #En caso de que la moda (mfv) retorne un solo valor, este será ocupado para homogenizar
    },
    across(starts_with("asistencia"), max, na.rm = TRUE),        #Se homogenizan las asistencias
    .groups = "drop"                                             #Se revierte la agrupacion al finalizar el proceso de unificar o resumir
  )
#        ¿Cuantas filas se eliminaron?            
y <- (nrow(datos)-nrow(filtro)-x)                                #Se guarda el valor de filas que se han eliminado y se le restan las del primer filtro
print(paste("Cantidad de filas eliminadas por estar repetidas :", y )) #Se muestra un mensaje para verificar si se unificaron algunos grupos




#  3er filtro: 
#         Verificar datos coincidentes respecto de las columnas: "Nombre", "Segundo apellido" y "Email".

#         En esta etapa se agruparan y posteriormente se eliminaran las filas repetidas que coincidan completamente con las siguientes columnas:
#         Columna B: Nombre    
#         Columna D: Segundo apellido
#         Columna E: Email 

#         Se excluyen en la agrupacion a la columna C (Primer apellido) por sospecha de hallar datos heterogeneos en la columna
#         que impida agrupar y asi unificar el registro del usuario

#         Pueden haber dos posibilidades para la busqueda de homogenizar las filas agrupadas:

#         A) Posibilidad de HALLAR un valor moda de la columna "Primer apellido" en la grupacion:
#            Ej. La moda de Primer apellido es: gomez 
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomes  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com

#         B) Posibilidad de NO HALLAR un valor moda de la columna "Primer apellido" en la agrupacion:
#            Ej. No existe moda en Primer apellido. 
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomes  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: cuevas Segundo apellido: cruz  Email: oscargomezcruz@gmail.com

#         En el caso para el inciso A, se tomara como valor para unificar el grupo a la moda encontrada.
#         En el caso para el inciso B, se debera acudir a hacer una extraccion del apellido en el correo,
#         que se encuentra entre "Nombre" y "Segundo apellido".Ya que se teme a que todos los datos esten erroneos, 
#         incompletos o que uno sea el indicado pero dentro del proceso se tomarian caracteres que aseguren mas el
#         objetivo de unificar correctamente el registro delusuario

filtro <- filtro %>%
  group_by(Nombre, `Segundo apellido`, Email) %>%               #Se agrupa excluyendo a Primer apellido
  summarise(
    `Primer apellido` = if(length(mfv(`Primer apellido`)) > 1){ #Se condiciona asi hay un solo valor de moda 
      str_extract(first(Email), paste0("(?<=", first(Nombre), ").*(?=", first(`Segundo apellido`), ")"))  #Se extraera el primer apellido de entre Nombre y Segundo apellido
    } else {
      mfv(`Primer apellido`)                                    #En caso de haber un solo resultado de moda, este se tomara como valor para homogenizar las celdas de la columna
    },
    across(starts_with("asistencia"), max, na.rm = TRUE),       #Se homogenizan los valores de asistencia
    .groups = "drop"                                            #Se deshace la agrupacion despues de resumir 
  )

#         ¿Cuantas filas se eliminaron?
z <- (nrow(datos)-nrow(filtro)-x-y)                             #Se calculan las filas eliminadas para saber si hubo algun cambio
print(paste("Cantidad de filas eliminadas por estar repetidas :", z))  #Se imprime un mensaje sobre las filas eliminadas




#  4to filtro: 
#         Verificar datos coincidentes respecto de las columnas: "Nombre", "Primer apellido" y "Email".

#         En esta etapa se agruparan y posteriormente se eliminaran las filas repetidas que coincidan completamente con las siguientes columnas:
#         Columna B: Nombre    
#         Columna C: Primer apellido
#         Columna E: Email 

#         Se excluyen en la agrupacion a la columna D (Segundo apellido) por sospecha de hallar datos heterogeneos en la columna
#         que impida agrupar y asi unificar el registro del usuario

#         Pueden haber dos posibilidades para la busqueda de homogenizar las filas agrupadas:

#         A) Posibilidad de HALLAR un valor moda de la columna "Segundo apellido" en la grupacion:
#            Ej. La moda de Primer apellido es: gomez 
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: crux  Email: oscargomezcruz@gmail.com

#         B) Posibilidad de NO HALLAR un valor moda de la columna "Segundo apellido" en la agrupacion:
#            Ej. No existe moda en Primer apellido. 
#              Nombre: oscar    Primer apellido: gomez  Segundo apellido: cruz  Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: gomes  Segundo apellido: cru   Email: oscargomezcruz@gmail.com
#              Nombre: oscar    Primer apellido: cuevas Segundo apellido: crux  Email: oscargomezcruz@gmail.com

#         En el caso para el inciso A, se tomara como valor para unificar el grupo a la moda encontrada.
#         En el caso para el inciso B, se debera acudir a hacer una extraccion del apellido en el correo,
#         que se encuentra entre "Segundo apellido" y "@". Ya que se teme a que todos los datos esten erroneos, 
#         incompletos o que uno sea el indicado pero dentro del proceso se tomarian caracteres que aseguren mas el
#         objetivo de unificar correctamente el registro delusuario



filtro <- filtro %>%
  group_by(Nombre, `Primer apellido`, Email) %>%               #Se hace la excepcion de "Segundo apellido" al agrupar
  summarise(                                                   
    `Segundo apellido` = if(length(mfv(`Segundo apellido`)) > 1){ #Se condiciona respecto de la cantidad de salidas de la moda
      str_extract(first(Email), paste0("(?<=", first(`Primer apellido`), ").*(?=@)")) #Si la moda es mas de una salida, entonces se requiere extraer el apellido del Email
    } else {
      mfv(`Segundo apellido`)                                  #En caso de haber moda, ese sera el dato que correspondera para toda la columna dentro de la agrupacion
    },
    across(starts_with("asistencia"), max, na.rm = TRUE),      #Se homogenizan los valores de asistencia en medio de la agrupacion 
    .groups = "drop"                                           #Se deshace la agrupacion
  )

#  ¿Cuantas filas se eliminaron?
p <- (nrow(datos)-nrow(filtro)-x-y-z)                          #Se calculan las filas eliminadas en el proceso
print(paste("Cantidad de filas eliminadas por estar repetidas :", p)) #Se muestra el valor de filas eliminadas



#---------------->Generar la columna de nombre completo<----------------

#  Se cambia el formato a "Title Case" de las columnas que competen a las columnas de nombre y apellidos
filtro <- filtro %>%
  mutate(across(c(Nombre, `Primer apellido`, `Segundo apellido`), str_to_title),    #STR_TO_TILTE modifica la capitalizacion del texto
         Email = str_to_lower(Email))                                                       #Se serciora que el correo este en minusculas

#  Unir las columnas nombre, primer apellido y segundo apellido
filtro <- filtro %>%
  unite("Nombre completo",                    #Unite requiere una columna la cual será el repositorio de la combinacion
        Nombre,
        `Primer apellido`,
        `Segundo apellido`,
        sep = " ",                            #Cada caracter combinado debe separarse con un espacio
       )
#  Se emplea la funcion "str_squish" para eliminar los espacios al inicio, final y espacios multiples
filtro <- filtro %>%     
  mutate(`Nombre completo` = str_squish(`Nombre completo`))



#-------->Desempeño mas bajo<-----------
#  Para hacer una suma de las asistencias, se requiere asegurar que las asistencias tengan un formato numerico
filtro <- filtro %>%
  mutate(across(starts_with("asistencia"), as.numeric)) #se cambia el formato a numerico a todas las filas que inicien con "asistencia"

#  Se genera una columna para conocer el procentaje de desempeño
filtro <- filtro %>%
  mutate(
    Desempeño = ifelse(
      rowSums(across(starts_with("asistencia"))) > 0,                       # Si la suma de asistencias es mayor a cero se realiza el calculo de porentaje
      (rowSums(across(starts_with("asistencia"))) / 5) * 100,               # Porentaje:  [(suma_de_asistencias)/(Total de asistencias)] *100
      0                                                                     #Si la suma de asistencias es 0, no se puede dividir. Por lo que se le da un valor de 0 
    )
  )


#---------------->Salida del proceso de limpieza<----------------

Datos_limpios <- filtro %>%
  arrange(`Nombre completo`)                           #Se ordenan los datos de manera alfabetica respecto de "Nombre completo"

View(Datos_limpios)                                    #Se visualizan los datos 


#  ¿Quienes tuvieron el menor desempeño?

Bajo_desempeño <-  Datos_limpios%>%
  arrange(`Nombre completo`) %>%
  filter(Desempeño == 0) 

View(Bajo_desempeño)

write_xlsx(
  list(
    Datos_limpios = Datos_limpios,
    Bajo_desempeño = Bajo_desempeño
  ),
  "Base_Limpia.xlsx"
)
