##Asiganción 3
#Traer la base de datos
personas <- read_sav("encovi_personas2017_ds.sav") 

# PERSONAS
new_names_pers <- c("id_hogar", "id_per", "parentesco", "edad", "sexo", 
                    "sit_conyu", "nivel_edu", "edu_ano_aprobado", "edu_sem_aprobado",
                    "tipo_edu", "sit_econo", "sector_eco", "cat_ocupa",
                    "trab_remun", "ing_laboral", "horas_trab", "ing_otro",
                    "ing_pension",
                    "pesop", "grp_edad", "anos_edu", "tipo_ciudad")

# Renombrar
personas <- personas %>%
  setnames(old = colnames(.), new = new_names_pers) %>%
  
  # Convierte los identificadores a caracteres
  mutate(id_hogar = str_pad(id_hogar, width = 4, "left", "0"),
         id_per = str_pad(id_per, width = 2, "left", "0"))

# Poner NA en los 98 y 99
personas[personas == 98 | personas == 99] <- NA

# Poniendo 0 y 1 en la variable trab remunerado
personas <- personas %>%
  mutate(across(.cols = c("trab_remun"),
                .fns = function(x) ifelse(x == 2, yes = 0, no = x)))


# Crear una variable indicadora para identificar a las personas que deben tener ingresos laborales reportados
personas$ing_laboral_imp <- ifelse(personas$trab_remun == "1" & !is.na(personas$trab_remun) & ( personas$ing_laboral <= 0 |  is.na(personas$ing_laboral)), 1, 0)


# Convertir las variables haven_labelled a factores y luego a numéricas
personas <- personas %>%
  mutate_at(vars(-id_hogar, -id_per), as.numeric)

# Real la imputación
imputacion <- mice(personas[personas$ing_laboral_imp == 1, ], method = "pmm", pred = quickpred(personas))

# Obtener los datos imputados
personas_imputados <- complete(imputacion)

# Verificar los resultados
summary(personas_imputados$ing_laboral)