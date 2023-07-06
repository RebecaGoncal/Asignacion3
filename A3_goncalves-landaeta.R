# Asiganción 3
# Integrantes: Rebeca Goncalves y Carlos Landaeta

# Librerias
library(tidyverse)
library(mice)
library(data.table)
library(haven)

# Traer la base de datos
personas <- read_sav("encovi_personas2017_ds.sav") 

#DEPURACION Y FORMATEO DE DATOS-------------------------------------------------

# Encabezados para la base de datos PERSONAS
new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar los encabezados en la base de datos PERSONAS
personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
# Convierte los identificadores a caracteres
mutate(id_hogar = str_pad(id_hogar, width = 4, "left", "0"),
         id_per = str_pad(id_per, width = 2, "left", "0"))

# Poner NA en los 98 y 99 para mejor entenimiento
personas[personas == 98 | personas == 99] <- NA

# Convertimos la variable trab_remun a dicotomicas
# Es decir poniendo 0 y 1 en la variable trab remunerado
personas <- personas %>%
  mutate(across(.cols = c("trab_remun"),
                .fns = function(x) ifelse(x == 2, yes = 0, no = x)))

# Crear una variable indicadora para identificar a las personas que deben tener ingresos laborales reportados
personas$ing_laboral_imp <- ifelse(personas$trab_remun == "1" & !is.na(personas$trab_remun) & ( personas$ing_laboral <= 0 |  is.na(personas$ing_laboral)), 1, 0)

# Convertir las variables haven_labelled a factores y luego a numéricas
personas <- personas %>%
  mutate_at(vars(-id_hogar, -id_per), as.numeric)

# GRUPOS DE OCUPACION ----------------------------------------------------------

# Definir los valores específicos para el Grupo 1
valores_grupo1 <- c("1", "2", "8", "9", "NA") # Ajusta los valores según tus necesidades

# Crear una nueva variable de grupos de ocupación
personas <- personas %>%
  mutate(grupo_cat_ocupa = ifelse(cat_ocupa %in% valores_grupo1, 1, 2))

# JUSTIFICACIÓN: Para reflejar el tamaño del grupo de donantes, se utilizó la variable "grupo_cat_ocupa" que fue generada previamente, nos pareció conveniente agrupar en dos grupos. 
# En un grupo a las personas con trabajos en el sector público, ayudante familiary remunerado o no ya que en general cuenta con un salario menor. El otro grupo aquellas personas 
# con trabajo en el sector privado con patrones o empleados y personas con trabajo propio ya que en general ganan más. Logramos con esta, imputar los ingresos
# laborales según su ocupación.

# GRUPOS DE TIPOS DE CIUDAD -----------------------------------------------------

# Definir los valores específicos para el Grupo 1
valores_grupo1_ciudad <- c("1", "4") 

# Crear una nueva variable de grupos de tipo de ciudad
personas <- personas %>%
  mutate(grupo_tipo_ciudad = ifelse(tipo_ciudad %in% valores_grupo1_ciudad, 1, 2))

# JUSTIFICACIÓN: Para reflejar el tamaño del grupo de donantes, se utilizó la variable "grupo_tipo_ciudad" que fue generada previamente, 
# nos pareció conveniente agrupar en dos grupos. En un grupo a las personas que viven en la Gran Caracas y en Ciudades Medianas donde la mayoria de las personas viven  
# ya ambos son relativamente semejantes entre ellas. El otro grupo aquellas personas que viven en ciudades del interior y pequeñas. Logramos con esta, imputar los ingresos
# laborales según el tipo de ciudad en la que vive.

# GRUPOS DE NIVEL EDUCATIVO -----------------------------------------------------

# Definir los valores específicos para el Grupo 1 y el Grupo 2
valores_grupo1_nivel_edu <- c("6", "7") 
valores_grupo2_nivel_edu <- c("5","4")

# Crear una nueva variable de grupos de nivel educativo
personas <- personas %>%
  mutate(grupo_nivel_edu = ifelse(nivel_edu %in% valores_grupo1_nivel_edu, 1, ifelse(nivel_edu %in% valores_grupo2_nivel_edu, 2, 3)))

# JUSTIFICACIÓN: Para reflejar el tamaño del grupo de donantes, se utilizó la variable "grupo_nivel_edu" que fue generada previamente, nos pareció conveniente agrupar en tres grupos. 
# En un grupo a las personas con un nivel educativo en el rango de Media y Técnico (TSU) ya que en este caso cuenta con un salario mayor. El otro grupo aquellas personas con un nivel educativo
# en el rango Universitario y Postgrado. Y un último grupo donde se encuentran las personas con nivel educativo en un rango de Preescolar, Primaria y Ninguna. Logramos con esta, imputar los ingresos
# laborales según su nivel educativo.

# TABLA CON AGRUPACIONES --------------------------------------------------------

h <- personas %>% select(cat_ocupa, grupo_cat_ocupa, tipo_ciudad, grupo_tipo_ciudad, nivel_edu, grupo_nivel_edu, sexo, grp_edad,ing_laboral, ing_laboral_imp)

# PROCESO DE IMPUTACION ----------------------------------------------------------

# Copiamos la base de datos original
imp <- personas

# Realizar la imputación
col_imputar <- c('ing_laboral')
col_donantes <- c("grupo_cat_ocupa", "grupo_tipo_ciudad", "grupo_nivel_edu", "grp_edad", "sexo")

imputacion <- mice(personas[, c(col_imputar, col_donantes, "ing_laboral_imp")], method = "pmm", maxit = 5, m = 5)

# Agrega los datos imputados a la base de datos IMP
imp[,"ing_laboral"] <- complete(imputacion,1)[,"ing_laboral"]

# Verificar los resultados)
summary(imp$ing_laboral)

# Generar la variable que refleja el tamaño del grupo de donantes que cumple con las condiciones
n_imp <- imp %>%
  filter(ing_laboral_imp == 0) %>%
  count(grupo_cat_ocupa, grupo_tipo_ciudad, grupo_nivel_edu, grp_edad, sexo)

imp %>%
  filter(ing_laboral_imp == 0 & grupo_cat_ocupa == 1 & grupo_tipo_ciudad == 1 & grupo_nivel_edu == 1 & sexo ==1 & grp_edad== 5)


