# Prueba_con_Rstudio

_En este proyecto se hizo una limpieza de datos usando Rstudio, a un conjunto de datos de 4 fuentes distintas. Donde se encontraron datos duplicados, incompletos, simbolos mezclados entre los datos e inconsistencias. La finalidad fue generar un archivo con usuarios unicos con el total de asistencias para determinar el desempeño de los usuarios registrados._

## Comenzando 🚀

_Estas instrucciones te permitirán obtener una copia del proyecto en funcionamiento en tu máquina local para propósitos de desarrollo y pruebas._

Mira **Deployment** para conocer como desplegar el proyecto.


### Pre-requisitos 📋

_Para poder ocupar el programa es necesario tener instaladas las siguientes librerias:_

```
readxl
writexl
dplyr
stringr
tidyr
modeest
```

_Dentro del archivo "Prueba_V2.R" ya viene el siguiente codigo para incluir las librerias necesarias. _
```
library(readxl)     # Esta libreria permite abrir un archivo .xlsx
library(writexl)    # Permite generear un archivo .xlsx
library(dplyr)      # Ayuda a manipular y transformar columnas y filas 
library(stringr)    # Permite manipular y limpiar texto en las celdas
library(tidyr)      # Puede manipular e identifica los formatos de los datos 
library(modeest)    # Esta libreria permite agregar la funcion moda (mfv)
```

### Instalación 🔧

_Para poder instalar los paquetes de las librerias y asi poder agregar las librerias al momento de correr el programa. Se debe usar la instruccion "install.packages("Nombre_de_la_libreria_o_paquete")" en la consola de Rstudio, se la enter y queda esperar a que finalice el proceso._

_Un ejemplo para la instlación del paquete "dplyr" y "stringr" es:_

```
install.packages("dplyr")

install.packages("stringr")

```

_Ahora solo queda descargar el programa y el archivo "Base_para_prueba.xlsx"_



## Ejecutando las pruebas ⚙️

_Para poder correr el programa correctamente, es necesario que cambies la ruta de la ubicacion del archivo "Base_para_prueba.xlsx" en la linea 13, de la sección de "Cargar el documento tipo xlsx"_

_El siguiente ejemplo muestra donde esta ubicado el archivo a trabajar con la ruta en mi propia computadora_

```
datos <- read_excel("C:/Users/leo-1/Downloads/Base_para_prueba.xlsx")
```


### Secciones y procesos que hay en el script 🔩

_En todo el script se encuentra comentado por secciones y sus procesos que se han realizado durante toda la limpieza del la base de datos._

_Las secciones se distingen por tener la siguiente estructura:_
```
#---------------->Cargar el documento tipo .xlsx<----------------
#---------------->Eliminar espacios en blanco<----------------
#---------------->Eliminar caracteres erroneos<----------------
#---------------->Hallar inconsistencias en el nombre de los usuarios<----------------
#---------------->Hacer homogeneas las asistencias<----------------
#---------------->Hallar valores repetidos<----------------
#---------------->Generar la columna de nombre completo<----------------
#---------------->Desempeño mas bajo<-----------
#---------------->Salida del proceso de limpieza<----------------
```

### Documento de salida 📦

_Al final se genera un documento "Base_Limpia.xlsx" con los datos limpios y dos hojas: "Datos_limpios" y "Bajo_desempeño"_

_En la hoja "Datos_limpios" se encuentran los registros unificados de los usuarios y sus respectivas asistencias. Por otro lado, en la hoja "Bajo desempeño" solo estan los registros de los usuarios con el menor desempeño_ 

_El documeto de salida se guardará en la carpeta de ubicacion del archivo "Prueba_V2.R"_




