### Taller integrador de calidad de datos

#Nombres: Ivonne Benítez, Salomé Cartagena, Sebastián Ortiz
#Fecha: 12 de novienbre de 2026

#1.----- Verificación de requisitos para completar el proyecto---

#install.packages(c('here','readr','dplyr','tidyr','ggplot2','knitr'))

getwd()               # Carpeta actual de trabajo
here::here()          # Raíz del proyecto .Rproj
file.exists(here::here('data_raw','encuesta_socioeconomica_2026.csv'))

#---------------- Etapa 0 — Preparación del proyecto y funciones comunes---------------- 

# Este comando se ejecuta desde Console; no cambie la ruta.
source(here::here('R', '00_configuracion.R'))

normalizar_ausencia(c('NA','', '0','450.00'))
a_numero(c('100.50','NA','USD 410','0'))

#====EJERCICIO: explique por qué no debemos sustituir todos los ceros 
#por NA y por qué se importan primero las claves como texto.

#Respuesta: los ceros no son NA necesariamente, puesto que dependerá de la naturaleza
#de la variable. Por ejemplo, para una variable de "ingresos", un cero indica que la
#persona no percibe remuneraciones de ningun tipo. Por otro lado, un NA significa que
#no se tiene respuesta para esa observación.


#---------------------- Etapa 1 — Importación y perfilado------------------------------ 

#1.1 Importar en modo conservador
source(here::here('R','00_configuracion.R'))
personas_raw <- leer_texto(
  here::here('data_raw','encuesta_socioeconomica_2026.csv')
)
#Si se deja que read_csv() importe automaticamente, se corre el riesgo de que algunas
#variables numericas se exporten como texto, o viceversa, y se pierda información en
#el camino.

#1.2 Comprender estructura y frecuencias
dim(personas_raw)                   # Número de filas y columnas
names(personas_raw)                 # Nombres de campos
head(personas_raw, 6)              # Primeras seis filas
personas_raw %>% count(zona)       # Tabla de frecuencia por zona


#1.3 Distinguir ausencia, cero y formato inválido
personas_raw %>% summarise(
  faltantes = sum(is.na(normalizar_ausencia(ingreso_mensual_usd))),
  ceros = sum(a_numero(ingreso_mensual_usd) == 0, na.rm = TRUE),
  negativos = sum(a_numero(ingreso_mensual_usd) < 0, na.rm = TRUE)
)

#1.4 Construir tabla de perfilado
source(here::here('R','01_importar_perfilar.R'))

#====EJERCICIO E1: Abra el CSV y explique cuáles variables 
#requieren prioridad de revisión.

#Respuesta: se deberá revisar con precaución la variable de id_hogar puesto que,
#pese a que solo son 5 ausencias, se deberá analizar el motivo, ya que la variable
#id_encuesta nos muestra que si existe el 100% de registro de encuestas. A su vez, se 
#podría revisar la variable ingreso_hogar_n ya que es la que tiene mas ausencias
#(1% aprox).

#-------------------------- Etapa 2 — Validación de variables-------------------------- 

#2.1. Crear una regla de dominio
edu_validas <- readr::read_csv(
  here::here('diccionario','catalogo_educacion.csv'),
  show_col_types = FALSE
)$nivel_educacion

x_edu <- normalizar_ausencia(personas$nivel_educacion)
alerta_educacion <- !is.na(x_edu) & !(x_edu %in% edu_validas)

#2.2. Transformar TRUE/FALSE en una tabla auditable
alertas_edu <- crear_alerta(
  personas, alerta_educacion, 'V07', 'nivel_educacion',
  'Categoría educativa fuera del catálogo literal'
)
head(alertas_edu)

source(here::here('R','02_validar_variables.R'))

rev_2 <- read_csv("resultados/tablas/02_alertas_variables.csv")

#====EJERCICIO E2: seleccionen tres reglas, identifiquen una fila y expliquen 
#si es error confirmado, advertencia o ausencia.

#Respuesta: 
#1) Regla: V02 (edad no interpretable o fuera del criterio didactico). 
    #Fila csv: 1953
    #Tipo de error: confirmado, puesto que son valores que no son lógicos respecto
    #a la naturaleza de la variable edad

#2) Regla: VO4 (Formato de ingreso no numérico)
    #Fila csv: 319
    #Tipo de error: confirmado, puesto que la observación dice "USD 410"

#3) Regla: V06 (ingreso no declarado)
    #Fila csv: 1690
    #Tipo de error: ausencia, ya que existe un NA donde debería estar el valor 
    #observado del ingreso mensual


#-------------------------- Etapa 3 — Consistencia lógica e integridad------------------ 

#3.1. Encontrar claves repetidas
duplicados <- personas %>%
  filter(!is.na(normalizar_ausencia(id_persona))) %>%
  add_count(periodo_referencia, id_persona,
            name = 'repeticiones') %>%
  filter(repeticiones > 1)

#3.2. Detectar claves huérfanas
huerfanos <- personas %>%
  filter(!is.na(normalizar_ausencia(id_hogar))) %>%
  anti_join(hogares,
            by = c('id_hogar', 'periodo_referencia'))

#3.3. Evaluar consistencia entre variables
# Regla didáctica: ocupado con cero horas declaradas
alerta_ocupado <- personas$condicion_actividad == 'Ocupado' &
  !is.na(personas$horas_n) & personas$horas_n == 0

source(here::here('R','03_integridad_consistencia.R'))


#====EJERCICIO E3: explicación de un duplicado y una referencia sin correspondencia.

#Respuesta:
# 1) En el archivo 03_alertas_integridad.csv existen 36 casos en los cuales
#el id_persona se repite. Esto muestra que se incumple la regla de unicidad. Se 
#debería validar si sucedió por errores en la carga de los formularios, puesto que
#no existe una id_encuesta repetida, lo que quiere decir que no se trata únicamente
#de una fila repetida.

#2) En el archivo 03_huerfanos.csv, se evidencian 10 personas con un identificador
#de hogar de H9999999999, que corresponde a la regla I02 (Hogar sin correspondencia 
#en catálogo, lo que muestra un problema de correspondencia, ya que este valor no 
#se encuentra en el catálogo de hogares

#------------------------ Etapa 4 — Depuración propuesta y bitácora-------------------- 
#4.1. Normalizar etiquetas conocidas
canon_zona <- c('urbana'='Urbana', 'rural'='Rural')
canonizar(c('URBANA','Rural ','99'),canon_zona)

#4.2. Registrar cambios con identificador estable
#4.3. Comprender la imputación antes de simularla

median(c(100, 300, 500, 9000))  # 400
mean(c(100, 300, 500, 9000))    # 2475

source(here::here('R','04_depuracion_bitacora.R'))

#====EJERCICIO E4: Compare tres normalizaciones y un caso que se mantiene 
#pendiente de revisión.

#Respuesta:
#Normalización 1: El caso de la variable provincia, donde se normaliza "PICHINCHA" o, 
# "Guayas ", a: "Pichincha", "Guayas"

#Normalización 2: El caso de la variable zona, donde se normaliza "URBANA", 
#"Rural ", a: "Urbana", "Rural"

#Normalización 3: El caso de la variable nivel_educacion, donde se normaliza "SUPERIOR", 
#"Secundaria ", a: "Superior", "Secundaria"

#Caso pendiente de revisión: el caso que no se encuentra propuesto en la bitacora y que
# está pendiente es el de los valores "H9999999999" en la variable id_hogar. 


#---------------------- Etapa 5 — Indicadores y tablas de seguimiento-----------------

#5.1. Completitud
#5.2. Validez
#5.3. Unicidad
#5.4. Consistencia
#5.5. Interpretar un ejemplo
#5.6. Comparación entre períodos

source(here::here('R','05_indicadores_tablas.R'))

#====EJERCICIO E5: Explique una diferencia en puntos porcentuales y compruebe el 
#numerador/denominador de un indicador.

#Respuesta;
#Se analiza el indicador de Validez, ya que los otros no presentan variación
#entre el original y la propuesta
#*Diferencia en puntos porcentuales**

#La validez pasa de 99,57% en el escenario original al 99,69% en propuesta. Es decir,
#0,12 puntos porcentuales. Esto representa una mejora pequeña en el cumplimiento de 
#los criterios de validez evaluados. 

#Comprobación del numerador y denominador**
  
#En la propuesta:
#Numerador: 11.917, correspondiente a las evaluaciones contabilizadas como válidas.
#Denominador: 11.954, correspondiente al total de evaluaciones consideradas para este indicador.

#El cálculo para la propuesta es: (11.917 ÷ 11.954) × 100 = 99,69%
  

#------------------------ Etapa 6 — Gráficos de seguimiento---------------------------

#Objetivo: transformar tablas en figuras con títulos, ejes, unidades y advertencias 
#correctas.

source(here::here('R','06_graficos_seguimiento.R'))

#====EJERCICIO E6: eleccione un gráfico y explique el denominador, una conclusión que 
#sí se justifica y una conclusión que sería indebida.

#Se selecciona el gráfico 06_pobreza.png que aborda "Composición según pobreza didáctica"

#Denominador: cada barra representa el total de registros de una combinación de 
#zona y período, y suma 100 %. Por ejemplo, para la categoría Rural en 
#noviembre de 2026, el denominador es 220 registros: 79 clasificados como pobres 
#y 141 como no pobres. Por tanto: 79 ÷ 220 × 100 = 35,91 % de pobreza didáctica.
  
#Conclusión justificada: entre los registros rurales analizados, la proporción 
#clasificada como pobre según el criterio didáctico aumenta de 18,58% a 35,91%, 
#una diferencia de 17,33 puntos porcentuales.

#Conclusión indebida: La pobreza oficial de la población rural a nivel nacional
#aumentó 17,33 puntos porcentuales. 



#-------------------------- Etapa 7 — Crear el archivo RDS---------------------------

#7.1. ¿Qué es un RDS y para qué sirve?
#7.2. Preparar el paquete de insumos
res <- readRDS(here::here('resultados','05_tablas.rds'))
paquete_reporte <- list(
  metadatos = list(titulo='Encuesta socioeconómica sintética'),
  tablas = res$tablas,
  alertas = res$alertas,
  bitacora = res$bitacora
)

saveRDS(paquete_reporte,
        here::here('resultados','07_paquete_reporte.rds'))

#7.3. Comprobar que el RDS se puede volver a leer
x <- readRDS(here::here('resultados','07_paquete_reporte.rds'))
names(x)
str(x, max.level = 2)
nrow(x$tablas$indicadores)

#====EJERCICIO E7: archivo físico resultados/07_paquete_reporte.rds, 
#captura de names(x) y comprobación de lectura. 

source(here::here('R','07_construir_rds_reporte.R'))


#------------------------ Etapa 8 — Crear el reporte HTML con Quarto-------------------

#Paso 8.1. Comprender el encabezado YAML
#Paso 8.2. Leer el RDS desde el documento
#Paso 8.3. Insertar una tabla y un gráfico
#Paso 8.4. Renderizar HTML y Word

#8_00 Verificación realizada: Revisión de insumos correcta. Ahora realice render de Quarto.



#EJERCICIO E8: HTML y DOCX (Word es opcional), más captura del informe con tabla y gráfico 
#visibles. Validación: los resultados del HTML deben coincidir con las tablas del RDS, 
#sin números redactados manualmente en el cuerpo del informe.











