library(tidyverse)
library(readr)


data_raw_23 <- read_delim("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/19075374-b180-48a0-aaaf-e0e44cd6816f/download/informacion-publica-dengue-zika-nacional-se-1-a-52-de-2023-2024-06-10.csv", 
                                                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


data_raw_24 <- read_delim("http://datos.salud.gob.ar/dataset/ceaa8e87-297e-4348-84b8-5c643e172500/resource/e57edd84-096d-4348-9137-2f5dae9c8f55/download/informacion-publica-dengue-zika-nacional-se-1-a-52-de-2024-2025-04-14.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


unique(data_raw_24$grupo_etario)

#Veo la cantidad de datos

View(data_raw_23 %>%
  filter(id_prov_indec_residencia == 30) %>%
  group_by(across(c(9,1))) %>%
  summarise(n= sum(cantidad)))

View(data_raw_24 %>%
       filter(id_prov_indec_residencia == 30) %>%
       group_by(sepi_min,id_depto_indec_residencia) %>%
       summarise(n= sum(cantidad)))

# Selecciono solo Entre Rios y proceso

data_raw_23 <- data_raw_23 %>% filter(id_prov_indec_residencia == 30) 


data_raw_24 <- data_raw_24 %>% filter(id_prov_indec_residencia == 30)


# Armo una combinacion de todos los departamentos y todas las semanas epi

# Suponé que estos son tus 12 grupos de edad


deptos <- c("008","015","021","028","035","042","049","056",
            "063","070","077","084","088","091","098","105","113")


# Crear todas las combinaciones posibles

combinaciones <- expand.grid(
  semana = 1:52,
  edad = 1:12,
  deptos = deptos,
  stringsAsFactors = FALSE
)

str(combinaciones)
str(data_raw_23)

# Armo el dataset de 2023

dengue_er_2023 <- combinaciones %>%
  left_join(data_raw_23 %>%
              select(sepi_min,id_grupo_etario,id_depto_indec_residencia,cantidad), by= c("semana"= "sepi_min","edad"="id_grupo_etario","deptos"="id_depto_indec_residencia"))%>%
    mutate(cantidad= if_else(is.na(cantidad),0,as.numeric(cantidad)),
           anio= 2023,
           evento="Dengue")
unique(dengue_er_2023$edad)

#Armo la edad agrupada

dengue_er_2023 <- dengue_er_2023 %>%
  mutate(gredad = case_when(edad <= 4 ~ '1.0-4',
                            edad == 5 ~ '2.5-9',
                            edad == 6 ~ '3.10-14',
                            edad == 7 ~ '4.15-19',
                            edad == 8 ~ '5.20-24',
                            edad == 9 ~ '6.25-34',
                            edad == 10 ~ '7.35-44',
                            edad == 11 ~ '8.45-64',
                            edad == 12 ~ '9.+ de 65')) %>%
  group_by(anio,semana,gredad,evento,deptos)%>%
  summarise(cantidad= sum(cantidad)) %>%
  mutate(LINK= paste0('30',deptos))

# Armo el dataset de 2024

dengue_er_2024 <- combinaciones %>%
  left_join(data_raw_24 %>%
              select(sepi_min,id_grupo_etario,id_depto_indec_residencia,cantidad), by= c("semana"= "sepi_min","edad"="id_grupo_etario","deptos"="id_depto_indec_residencia"))%>%
  mutate(cantidad= if_else(is.na(cantidad),0,as.numeric(cantidad)),
         anio= 2024,
         evento="Dengue")


#Armo la edad agrupada

dengue_er_2024 <- dengue_er_2024 %>%
  mutate(gredad = case_when(edad <= 4 ~ '1.0-4',
                            edad == 5 ~ '2.5-9',
                            edad == 6 ~ '3.10-14',
                            edad == 7 ~ '4.15-19',
                            edad == 8 ~ '5.20-24',
                            edad == 9 ~ '6.25-34',
                            edad == 10 ~ '7.35-44',
                            edad == 11 ~ '8.45-64',
                            edad == 12 ~ '9.+ de 65')) %>%
  group_by(anio,semana,gredad,evento,deptos)%>%
  summarise(cantidad= sum(cantidad)) %>%
  mutate(LINK= paste0('30',deptos))

#Adecuo la poblacion y creo el dataset final

pob_dengue <- read_delim(paste0("src/","proy_2010_2025.csv"),delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE,col_types = cols(VALOR = col_character()))


pob_dengue_er <- pob_dengue %>%
       filter((ANO >= 2023 & ANO <= 2024) & SEXO == 'Ambos Sexos' & GRUPEDAD != '18.TOTAL' & DEPARTAMENTO != 'TOTAL') %>%
       mutate(
         VALOR = as.numeric(gsub(",", ".", VALOR)),  # <-- convertir y manejar decimales si hay comas
         GREDAD_EPI = case_when(
           as.numeric(substr(GRUPEDAD, 1, 2)) >= 6 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 7  ~ '6.25-34',
           as.numeric(substr(GRUPEDAD, 1, 2)) >= 8 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 9  ~ '7.35-44',
           as.numeric(substr(GRUPEDAD, 1, 2)) >= 10 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 13 ~ '8.45-64',
           as.numeric(substr(GRUPEDAD, 1, 2)) >= 13 & as.numeric(substr(GRUPEDAD, 1, 2)) < 18  ~ '9.+ de 65',
           TRUE ~ GRUPEDAD
         )
       ) %>%
       group_by(ANO, LINK, DEPARTAMENTO, GREDAD_EPI) %>%
       summarise(POBLACION = sum(VALOR, na.rm = TRUE))



#Estandar 2010

pob_dengue_er_estandar <- pob_dengue %>%
  filter(ANO <= 2010 & SEXO == 'Ambos Sexos' & GRUPEDAD != '18.TOTAL' & DEPARTAMENTO == 'TOTAL') %>%
  mutate(
    VALOR = as.numeric(gsub(",", ".", VALOR)),  # <-- convertir y manejar decimales si hay comas
    GREDAD_EPI = case_when(
      as.numeric(substr(GRUPEDAD, 1, 2)) >= 6 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 7  ~ '6.25-34',
      as.numeric(substr(GRUPEDAD, 1, 2)) >= 8 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 9  ~ '7.35-44',
      as.numeric(substr(GRUPEDAD, 1, 2)) >= 10 & as.numeric(substr(GRUPEDAD, 1, 2)) <= 13 ~ '8.45-64',
      as.numeric(substr(GRUPEDAD, 1, 2)) >= 13 & as.numeric(substr(GRUPEDAD, 1, 2)) < 18  ~ '9.+ de 65',
      TRUE ~ GRUPEDAD
    )
  ) %>%
  group_by(ANO, LINK, DEPARTAMENTO, GREDAD_EPI) %>%
  summarise(POBLACION = sum(VALOR, na.rm = TRUE)) %>%
  ungroup()



#Joineo todo

pob_dengue_er$LINK <- as.character(pob_dengue_er$LINK)

dengue_er_2023 <- dengue_er_2023 %>%
  left_join(pob_dengue_er %>% filter(ANO== 2023) %>% ungroup() %>% select(LINK,GREDAD_EPI,POBLACION) , by= c("LINK", "gredad"="GREDAD_EPI"))


dengue_er_2024 <- dengue_er_2024 %>%
  left_join(pob_dengue_er %>% filter(ANO== 2024) %>% ungroup() %>% select(LINK,GREDAD_EPI,POBLACION) , by= c("LINK", "gredad"="GREDAD_EPI"))



#Guardo el archivo

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb,"Dengue Entre Rios 2023")
openxlsx::addWorksheet(wb,"Dengue Entre Rios 2024")
openxlsx::addWorksheet(wb,"Estandar 2010")

openxlsx::writeData(wb,"Dengue Entre Rios 2023",dengue_er_2023)
openxlsx::writeData(wb,"Dengue Entre Rios 2024",dengue_er_2024)
openxlsx::writeData(wb,"Estandar 2010",pob_dengue_er_estandar)

openxlsx::saveWorkbook(wb,"dengue_entre_rios.xlsx",overwrite = TRUE)

